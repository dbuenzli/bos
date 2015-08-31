(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Executable name. *)

let exec = if Array.length Sys.argv = 0 then "" else Sys.argv.(0)

(* Errors *)

let quote pp ppf v = Fmt.pf ppf "`%a'" pp v

let err_done = "Bos.OS.Arg.parse_opts or Bos.OS.Arg.parse already called"
let err_env v msg = R.msgf "environment variable %s: %s" v msg
let err_repeat n = R.msgf "option %a cannot be repeated" (quote Fmt.string) n
let err_need_argument n =
  R.msgf "option %a needs an argument" (quote Fmt.string) n

let err_dupe n n' =
  R.msgf "options %a and %a cannot be present at the same time"
    (quote Fmt.string) n (quote Fmt.string) n'

let err_unknown_opt ppf l =
  Fmt.pf ppf "unknown option %a." (quote Fmt.string) l

let err_too_many ppf l =
  Fmt.pf ppf "too many arguments, don't know what to do with %a"
    Fmt.(list ~sep:(Fmt.unit ",@ ") (quote Fmt.string)) l

(* Argument converters *)

type 'a parser = string -> ('a, Rresult.R.msg) Rresult.result
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

let err_invalid s kind =
  R.msgf "invalid value %a, expected %s" (quote Fmt.string) s kind

let converter kind k_of_string print =
  let parse s = match k_of_string s with
  | None -> Error (err_invalid s kind)
  | Some v -> Ok v
  in
  parse, print

let some ?(none = "") (parse, print) =
  let parse s = match parse s with Error _ as e -> e | Ok v -> Ok (Some v) in
  let print = Fmt.option ~none:Fmt.(const string none) print in
  parse, print

(* Parsing *)

type parse =
  | Done
  | Perror of R.msg
  | Line of string list

let raw_args = match Array.to_list Sys.argv with
| [] -> []
| cmd :: args -> args

let get_parse, set_parse =
  let parse = ref (Line raw_args) in
  (fun () -> !parse),
  (fun p -> parse := p)

(* Option names and values *)

let make_opt_names names =
  let opt n = if String.length n = 1 then strf "-%s" n else strf "--%s" n in
  List.map opt names

let is_short_opt n =
  if String.length n < 2 then false else
  n.[0] = '-' && n.[1] <> '-'

let is_long_opt n =
  if String.length n < 3 then false else
  n.[0] = '-' && n.[1] = '-' && n.[2] <> '-'

let is_opt n = is_short_opt n || is_long_opt n

let short_opt_arg n =
  if String.length n <= 2 then None else
  Some (String.with_pos_range ~stop:2 n,
        String.with_pos_range ~start:2 n)

let long_opt_arg n = String.cut ~sep:"=" n

let opt_arg n = if is_short_opt n then short_opt_arg n else long_opt_arg n

let partition_opt_pos l =
  let rec loop opts poss = function
  | "--" :: l -> List.rev opts, List.rev_append poss l
  | [] -> List.rev opts, List.rev poss
  | a :: l ->
      if is_opt a then loop (a :: opts) poss l else loop opts (a :: poss) l
  in
  loop [] [] l

(* Documentation *)

let sadly_undocumented = "sadly undocumented"

type doc_opt_kind =
  | Flag of string
  | Opt of string * string * unit Fmt.t (* pretty prints the absent value. *)

type opt_doc =
  { names : string list;
    env : string option;
    repeat : bool;
    kind : doc_opt_kind; }

let get_opt_docs, add_opt_doc =
  let docs = ref [] in
  (fun () -> !docs),
  (fun doc -> docs := doc :: !docs)


let pp_opt_doc ppf = function
| Flag d -> Fmt.text ppf d
| Opt (d, docv, _) ->
    let d =
      try
        let b = Buffer.create 244 in
        let subst = function "docv" -> docv | s -> strf "$(%s)" s in
        Buffer.add_substitute b subst d;
        Buffer.contents b
      with Not_found -> d
    in
    Fmt.text ppf d

let pp_opt_docs ppf opt_docs =
  let is_flag o = match o.kind with Flag _ -> true | _ -> false in
  let opt_docs = List.sort compare opt_docs in
  let pp_name = Fmt.(styled `Bold string) in
  let pp_var = Fmt.(styled `Underline string) in
  let pp_short var ppf name = Fmt.pf ppf "%a %a" pp_name name pp_var var in
  let pp_long var ppf name = Fmt.pf ppf "%a=%a" pp_name name pp_var var in
  let pp_absent ppf absent =
    let absent = strf "@[<h>%a@]" absent () in
    if absent <> "" then Fmt.pf ppf "@ (absent=%s)" absent
  in
  let pp_opt var ppf n =
    if is_short_opt n then pp_short var ppf n else pp_long var ppf n
  in
  let pp_env ppf var = match var with
  | None -> ()
  | Some var -> Fmt.pf ppf "@ (env=%a)" Fmt.(styled `Underline string) var
  in
  let pp_opts ppf o =
    match o.kind with
    | Flag _ -> Fmt.(list ~sep:(unit ",@ ") pp_name) ppf o.names;
    | Opt (_, var, absent) ->
        Fmt.(list ~sep:(unit ",@ ") (pp_opt var)) ppf o.names;
        pp_absent ppf absent;
  in
  let pp_opt_doc ppf o = match o.names with
  | [n] when is_short_opt n && o.env = None && is_flag o ->
      Fmt.pf ppf "@[@[%a@]  @[%a@]@]" pp_opts o pp_opt_doc o.kind
  | _ ->
      Fmt.pf ppf "@[<v4>@[%a%a@]@,@[%a@]@]"
        pp_opts o pp_env o.env pp_opt_doc o.kind
  in
  if opt_docs = [] then () else
  Fmt.pf ppf "@[<v>Options:@,@,  @[<v>%a@]@]"
    Fmt.(list ~sep:cut pp_opt_doc) opt_docs


(* Environment default parsing *)

let env_default var parser = match var with
| None -> Ok None
| Some var ->
    match Bos_os_env.var var with
    | None -> Ok None
    | Some s ->
        match parser s with
        | Ok v -> Ok (Some v)
        | Error (`Msg e) -> Error (err_env var e)

(* Flag queries *)

let rec rem_flag names rleft = function
| "--" :: _ -> None
| s :: ss when List.mem s names -> Some (s, List.rev_append rleft ss)
| s :: ss -> rem_flag names (s :: rleft) ss
| [] -> None

let flag ?(doc = sadly_undocumented) ?env names =
  let names = make_opt_names names in
  add_opt_doc { names; env; repeat = false; kind = Flag doc };
  match get_parse () with
  | Done -> invalid_arg err_done
  | Perror _ -> false
  | Line line ->
      match rem_flag names [] line with
      | None ->
          begin match env_default env Bos_os_env.bool with
          | Ok (Some v) -> v
          | Ok None -> false
          | Error e -> set_parse (Perror e); false
          end
      | Some (flag, rest) ->
          match rem_flag names [] rest with
          | None -> set_parse (Line rest); true
          | Some (flag', _) ->
              if flag = flag'
              then (set_parse @@ Perror (err_repeat flag); false)
              else (set_parse @@ Perror (err_dupe flag flag'); false)

let flag_all ?(doc = sadly_undocumented) ?env names =
  let names = make_opt_names names in
  add_opt_doc { names; env; repeat = true; kind = Flag doc };
  match get_parse () with
  | Done -> invalid_arg err_done
  | Perror _ -> 0
  | Line line ->
      let rec find acc line = match rem_flag names [] line with
      | Some (flag, rest) -> find (acc + 1) rest
      | None ->
          if acc <> 0 then (set_parse (Line line); acc) else
          match env_default env Bos_os_env.bool with
          | Ok (Some v) -> if v then 1 else 0
          | Ok None -> 0
          | Error e -> set_parse (Perror e); 0
      in
      find 0 line

(* Option queries *)

let rec rem_option names rleft = function
| "--" :: _ -> Ok None
| s :: ss ->
    begin match opt_arg s with
    | None ->
        if not (List.mem s names)
        then rem_option names (s :: rleft) ss
        else begin match ss with
        | [] -> Error (err_need_argument s)
        | "--" :: _ -> Error (err_need_argument s)
        | s' :: _ when is_opt s' -> Error (err_need_argument s)
        | arg :: ss -> Ok (Some (s, arg, List.rev_append rleft ss))
        end
    | Some (opt, arg) ->
        if not (List.mem opt names)
        then rem_option names (s :: rleft) ss
        else Ok (Some (opt, arg, List.rev_append rleft ss))
    end
| [] -> Ok None

let opt ?(docv = "VAL") ?(doc = sadly_undocumented) ?env names (parse, print)
    ~absent
  =
  let names = make_opt_names names in
  let opt = Opt (doc, docv, fun ppf () -> print ppf absent) in
  add_opt_doc { names; env; repeat = false; kind = opt };
  match get_parse () with
  | Done -> invalid_arg err_done
  | Perror _ -> absent
  | Line line ->
      match rem_option names [] line with
      | Error e -> set_parse (Perror e); absent
      | Ok None ->
          begin match env_default env parse with
          | Ok (Some v) -> v
          | Ok None -> absent
          | Error e -> set_parse (Perror e); absent
          end
      | Ok (Some (opt, arg, rest)) ->
          match rem_option names [] rest with
          | Ok None -> set_parse (Line rest);
              begin match parse arg with
              | Ok v -> v
              | Error e -> set_parse (Perror e); absent
              end
          | Ok (Some (opt', _, _)) ->
              if opt = opt'
              then (set_parse @@ Perror (err_repeat opt); absent)
              else (set_parse @@ Perror (err_dupe opt opt'); absent)
          | Error e -> (* well... *) set_parse (Perror e); absent

let opt_all ?(docv = "VAL") ?(doc = sadly_undocumented) ?env names
    (parse, print) ~absent
  =
  let names = make_opt_names names in
  let opt = Opt (doc, docv, fun ppf () -> Fmt.(list ~sep:sp print) ppf absent)in
  add_opt_doc { names; env; repeat = false; kind = opt };
  match get_parse () with
  | Done -> invalid_arg err_done
  | Perror _ -> absent
  | Line line ->
      let rec find acc line = match rem_option names [] line with
      | Error e -> set_parse (Perror e); absent
      | Ok (Some (_, arg, rest)) ->
          begin match parse arg with
          | Error e -> set_parse (Perror e); absent
          | Ok arg ->  find (arg :: acc) rest
          end
      | Ok None ->
          if acc <> [] then (set_parse (Line line); acc) else
          match env_default env parse with
          | Ok (Some v) -> [v]
          | Ok None -> absent
          | Error e -> set_parse (Perror e); absent
      in
      find [] line

(* Parsing *)

let get_pp_usage opt_docs ~pos = function
| Some u -> Fmt.(const string) u
| None ->
    fun ppf () ->
      if opt_docs <> []
      then Fmt.pf ppf "[%a]..." Fmt.(styled_unit `Underline "OPTION") ();
      if pos
      then Fmt.pf ppf " %a..." Fmt.(styled_unit `Underline "ARG") ()

let pp_usage ppf usage = Fmt.pf ppf "Usage: %s %a@." exec usage ()
let pp_usage_try_help ppf usage =
  pp_usage ppf usage;
  Fmt.pf ppf "Try %a for more information@."
    (quote Fmt.(suffix (unit " --help") string)) exec;
  ()

let parse_error ~usage msg =
  Fmt.epr "%s: %s@." exec msg;
  Fmt.epr "%a" pp_usage_try_help usage;
  exit 1

let maybe_help ~doc ~usage opt_docs =
  let rec find_help = function
  | "--" :: _ | [] -> false
  | s :: ss -> List.mem s ["-h"; "-help"; "--help" ] || find_help ss
  in
  if not (find_help raw_args) then () else
  begin
    Fmt.(pf stdout "%s - @[%a@]@." Bos_path.(base @@ v exec) text doc);
    Fmt.(pf stdout "%a" pp_usage usage);
    Fmt.(pf stdout "%a@." pp_opt_docs opt_docs);
    exit 0
  end

let parse_opts ?(doc = sadly_undocumented) ?usage () =
  let usage = get_pp_usage (get_opt_docs ()) ~pos:false usage in
  maybe_help ~doc ~usage (get_opt_docs ());
  match get_parse () with
  | Line [] -> ()
  | Line l ->
      let opts, poss = partition_opt_pos l in
      List.iter (fun o -> Fmt.epr "%s: @[%a@]@." exec err_unknown_opt o) opts;
      if poss <> [] then (Fmt.epr "%s: @[%a@]@." exec err_too_many poss);
      pp_usage_try_help Fmt.stderr usage;
      exit 1
  | Done -> invalid_arg err_done
  | Perror (`Msg e) -> parse_error ~usage e

let parse_pos_args parse ps =
  let rec loop acc = function
  | p :: ps -> parse p >>= fun p -> loop (p :: acc) ps
  | [] -> Ok (List.rev acc)
  in
  loop [] ps

let parse ?(doc = sadly_undocumented) ?usage ~pos:(parse, print) () =
  let usage = get_pp_usage (get_opt_docs ()) ~pos:true usage in
  maybe_help ~doc ~usage (get_opt_docs ());
  match get_parse () with
  | Done -> invalid_arg err_done
  | Perror (`Msg e) -> parse_error ~usage e
  | Line l ->
      let opts, poss = partition_opt_pos l in
      if opts <> [] then begin
        List.iter (fun o -> Fmt.epr "%s: @[%a@]@." exec err_unknown_opt o) opts;
        pp_usage_try_help Fmt.stderr usage;
        exit 1
      end;
      match parse_pos_args parse poss with
      | Error (`Msg e) -> parse_error ~usage e
      | Ok poss -> poss

(* Predefined argument converters *)

let string = (fun s -> Ok s), Fmt.string
let cmd = (fun s -> Ok (Bos_cmd.v s) (* FIXME use Cmd.of_string *)),Bos_cmd.pp
let path = converter "a path" Bos_path.of_string Bos_path.pp
let char = converter "a character" String.to_char Fmt.char
let bool = converter "`true' or `false'" String.to_bool Fmt.bool
let int = converter "an integer" String.to_int Fmt.int
let nativeint = converter "a native integer" String.to_nativeint Fmt.nativeint
let int32 = converter "a 32-bit integer" String.to_int32 Fmt.int32
let int64 = converter "a 64-bit integer" String.to_int64 Fmt.int64
let float = converter "a float" String.to_float Fmt.float

let enum enum =
  if enum = [] then invalid_arg "empty enumeration" else
  let parse s = try Ok (List.assoc s enum) with
  | Not_found ->
      let alts = List.map (fun (a, _) -> strf "%a" (quote Fmt.string) a) enum in
      Error (err_invalid s (strf "one of %s" (String.concat ~sep:", " alts)))
  in
  let print ppf v =
    let enum_inv = List.rev_map (fun (s, v) -> (v, s)) enum in
    let to_string v = try List.assoc v enum_inv with
    | Not_found ->
        invalid_arg "Bos.Arg.enum: incomplete enumeration for the type"
    in
    Fmt.(using to_string string) ppf v
  in
  parse, print

let parse_split ?(sep = ",") s parse =
  let rec loop acc = function
  | s :: ss -> parse s >>= fun v -> loop (v :: acc) ss
  | [] -> Ok (List.rev acc)
  in
  loop [] (String.cuts ~sep:"," s)

let list ?sep (parse, print) =
  let parse s = parse_split ?sep s parse in
  let print = Fmt.list ~sep:(Fmt.unit ",") print in
  parse, print

let array ?sep (parse, print) =
  let parse s = match parse_split ?sep s parse with
  | Error _ as e -> e
  | Ok l -> Ok (Array.of_list l)
  in
  let print = Fmt.array ~sep:(Fmt.unit ",") print in
  parse, print

let pair ?(sep = ",") (lparse, lprint) (rparse, rprint) =
  let parse s = match String.cut ~sep s with
  | None -> Error (err_invalid s (strf "a separator `%s' in the string" sep))
  | Some (l, r) ->
      lparse l >>= fun l ->
      rparse r >>= fun r ->
      Ok (l, r)
  in
  let print = Fmt.pair ~sep:Fmt.(const string sep) lprint rprint in
  parse, print

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
