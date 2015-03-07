(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult_infix
open Bos_prelude

type 'a result = ('a, R.msg) R.t

let path_str = Bos_path.to_string

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p

let file_exists ?err file =
  try
    let file = path_str file in
    let err_msg file = R.error_msgf "%s: no such file" file in
    let exists = Sys.file_exists file && not (Sys.is_directory file) in
    ret_exists ?err err_msg file exists
  with
  | Sys_error e -> R.error_msg e

let dir_exists ?err dir =
  try
    let dir = path_str dir in
    let err_msg file = R.error_msgf "%s: no such directory" dir in
    let exists = Sys.file_exists dir && Sys.is_directory dir in
    ret_exists ?err err_msg dir exists
  with Sys_error e -> R.error_msg e

module Path = struct

  (* Path operations *)

  let exists ?err p =
    try
      let p = path_str p in
      let err_msg p = R.error_msgf "%s: no such path" p in
      ret_exists ?err err_msg p (Sys.file_exists p)
    with Sys_error e -> R.error_msg e

  let err_move src dst =
    let src, dst = (path_str src), (path_str dst) in
    R.error_msgf "move %s to %s: destination exists" src dst

  let move ?(force = false) src dst =
    (if force then R.ok false else exists dst) >>= fun don't ->
    if don't then err_move src dst else
    try R.ok (Sys.rename (path_str src) (path_str dst)) with
    | Sys_error e -> R.error_msg e

  (* Matching paths *)

  let pats_of_path p =
    let buf = Buffer.create 255 in
    let parse_seg acc s =
      acc
      >>= fun acc -> Bos_pat.of_string ~buf s
      >>= fun pat -> R.ok (pat :: acc)
    in
    let parse_segs ss = List.fold_left parse_seg (R.ok []) ss in
    match Bos_path.to_segs p with
    | `Rel ss -> parse_segs ss >>= fun ss -> R.ok (".", List.rev ss)
    | `Abs ss -> parse_segs ss >>= fun ss -> R.ok ("/", List.rev ss)

  let match_segment ~env acc path seg = match acc with
  | Error _ as e -> e
  | Ok acc ->
      try
        let occs =
          if not (Sys.file_exists path) then [||] else
          if Sys.is_directory path then (Sys.readdir path) else
          [||]
        in
        let add_match acc f = match (Bos_pat.match_pat ~env 0 f seg) with
        | None -> acc
        | Some _ as m -> (strf "%s%s%s" path Filename.dir_sep f, m) :: acc
        in
        R.ok (Array.fold_left add_match acc occs)
      with Sys_error e ->
        let err _ = R.msgf "Unexpected error while matching `%s'" path in
        R.error_msg e |> R.reword_error_msg err

  let match_path ~env p =
    let rec loop acc = function
    | seg :: segs ->
        let add_seg acc (p, env) = match_segment ~env acc p seg in
        begin match acc with
        | Error _ as e -> e
        | Ok acc -> loop (List.fold_left add_seg (R.ok []) acc) segs
        end
    | [] -> acc
    in
    pats_of_path p >>= fun (root, segs) ->
    match segs with
    | [] -> R.ok []
    | segs -> loop (R.ok [root, env]) segs

  let matches p =
    let pathify acc (p, _) = (Bos_path.of_string p) :: acc in
    match_path ~env:None p >>| List.fold_left pathify []

  let unify ?(init = String.Map.empty) p =
    let env = Some init in
    let pathify acc (p, map) = match map with
    | None -> assert false
    | Some map -> (Bos_path.of_string p, map) :: acc
    in
    match_path ~env p >>| List.fold_left pathify []

  (* Folding over file system hierarchies *)

  type traverse = [`All | `None | `If of Bos_path.t -> bool result ]
  type elements = [ `Any | `Files | `Dirs | `Is of Bos_path.t -> bool result ]
  type 'a fold_error = Bos_path.t -> 'a result -> unit result

  let log_fold_error ~level =
    fun p -> function
    | Error (`Msg m) -> Bos_log.msg level "%s" m; R.ok ()
    | Ok _ -> assert false

  exception Fold_stop of R.msg

  let err_fun err f ~backup_value = (* handles path function errors in folds *)
    fun p -> match f p with
    | Ok v -> v
    | Error _ as e ->
        match err p e with
        | Ok () -> backup_value   (* use backup value and continue the fold. *)
        | Error m -> raise (Fold_stop m)                  (* the fold stops. *)

  let err_predicate_fun err p = err_fun err p ~backup_value:false

  let do_traverse_fun err = function
  | `All -> fun _ -> true
  | `None -> fun _ -> false
  | `If pred -> err_predicate_fun err pred

  let is_element_fun err = function
  | `Any -> fun _ -> true
  | `Files -> err_predicate_fun err file_exists
  | `Dirs -> err_predicate_fun err dir_exists
  | `Is pred -> err_predicate_fun err pred

  let is_dir_fun err =
    let is_dir p = try Ok (Sys.is_directory (path_str p)) with
    | Sys_error e -> R.error_msg e
    in
    err_predicate_fun err is_dir

  let readdir_fun err =
    let readdir d = try Ok (Sys.readdir (path_str d)) with
    | Sys_error e -> R.error_msg e
    in
    err_fun err readdir ~backup_value:[||]

  let fold
      ?(err = log_fold_error ~level:Bos_log.Error) ?(over = `Any)
      ?(traverse = `All) f acc paths
    =
    try
      let do_traverse = do_traverse_fun err traverse in
      let is_element = is_element_fun err over in
      let is_dir = is_dir_fun err in
      let readdir =  readdir_fun err in
      let process dir (acc, to_traverse) bname =
        let p = Bos_path.(dir / bname) in
        (if is_element p then (f acc p) else acc),
        (if is_dir p && do_traverse p then p :: to_traverse else to_traverse)
      in
      let rec loop acc = function
      | (d :: ds) :: up ->
          let childs = readdir d in
          let acc, to_traverse = Array.fold_left (process d) (acc, []) childs in
          loop acc (to_traverse :: ds :: up)
      | [] :: [] -> acc
      | [] :: up -> loop acc up
      | _ -> assert false
      in
      let init acc p = process (Bos_path.dirname p) acc (Bos_path.basename p) in
      let acc, to_traverse = List.fold_left init (acc, []) paths in
      Ok (loop acc (to_traverse :: []))
    with Fold_stop e -> Error e
end

(* File operations *)

module File = struct

  let apply f x ~finally y =
    let result = try f x with
    | e -> try finally y; raise e with _ -> raise e
    in
    finally y;
    result

  let is_dash = Bos_path.is_dash

  (* Files *)

  let dev_null = match Sys.os_type with
  (* Using Sys.os_type, because that's really for the driver. *)
  | "Win32" -> Bos_path.file "NUL"
  |  _ -> Bos_path.(root / "dev" / "null")

  let exists = file_exists

  let delete ?(maybe = false) file =
    exists file >>= fun exists ->
    if maybe && not exists then R.ok () else
    try R.ok (Sys.remove (path_str file)) with
    | Sys_error e -> R.error_msg e

  let temp ?dir suff =
    try
      let temp_dir = match dir with
      | None -> None
      | Some d -> Some (Bos_path.to_string d)
      in
      let f = Filename.temp_file ?temp_dir "bos" suff in
      let f = Bos_path.of_string f in
      at_exit (fun () -> ignore (delete f));
      R.ok f
    with Sys_error e -> R.error_msg e

  (* Input *)

  let with_inf f file v =
    try
      let ic = if is_dash file then stdin else open_in_bin (path_str file) in
      let close ic = if is_dash file then () else close_in ic in
      apply (f ic) v ~finally:close ic
    with
    | Sys_error e -> R.error_msg e

  let read file =
    let input ic () =
      let len = in_channel_length ic in
      let s = Bytes.create len in
      really_input ic s 0 len; R.ok s
    in
    with_inf input file ()

  let read_lines file = read file >>| (String.split ~sep:"\n")

  (* Output *)

  let with_outf f file v =
    try
      let oc = if is_dash file then stdout else open_out_bin (path_str file) in
      let close oc = if is_dash file then () else close_out oc in
      apply (f oc) v ~finally:close oc
    with
    | Sys_error e -> R.error_msg e

  let write file contents =
    let write oc contents = output_string oc contents; R.ok () in
    if is_dash file then with_outf write file contents else
    temp ~dir:(Bos_path.dirname file) "write"
    >>= fun tmpf -> with_outf write tmpf contents
    >>= fun () -> Path.move ~force:true tmpf file

  let write_lines file lines = write file (String.concat "\n" lines)

  let write_subst vars file contents =
    let write_subst oc contents =                     (* man that's ugly. *)
      let s = contents in
      let start = ref 0 in
      let last = ref 0 in
      let len = String.length s in
      while (!last < len - 4) do
        if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
        begin
          let start_subst = !last in
          let last_id = ref (!last + 2) in
          let stop = ref false in
          while (!last_id < len - 1 && not !stop) do
            if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
              if s.[!last_id] <> ' '
              then (incr last_id)
              else (stop := true; last := !last_id)
            end else begin
              let id_start = start_subst + 2 in
              let id = String.sub s (id_start) (!last_id - id_start) in
              try
                let subst = List.assoc id vars in
                Pervasives.output oc s !start (start_subst - !start);
                output_string oc subst;
                stop := true;
                start := !last_id + 2;
                last := !last_id + 2;
              with Not_found ->
                stop := true;
                last := !last_id
            end
          done
        end
      done;
      Pervasives.output oc s !start (len - !start); R.ok ()
    in
    if is_dash file then with_outf write_subst file contents else
    temp ~dir:(Bos_path.dirname file) "write"
    >>= fun tmpf -> with_outf write_subst tmpf contents
    >>= fun () -> Path.move ~force:true tmpf file
end

(* Directory operations *)

module Dir = struct
  let exists = dir_exists
  let current () =
    try R.ok (Bos_path.of_string (Sys.getcwd ())) with
    | Sys_error e -> R.error_msg e

  let set_current dir =
    try R.ok (Sys.chdir (path_str dir)) with
    | Sys_error e -> R.error_msg e

  let contents ?(path = true) dir =
    try
      let name = if path then Bos_path.add dir else Bos_path.file in
      let files = Sys.readdir (path_str dir) in
      let add_file acc f = name f :: acc in
      R.ok (Array.fold_left add_file [] files)
    with Sys_error e -> R.error_msg e

  (* Directory folding *)

  let fold_contents ?err ?over ?traverse f acc d =
    contents d >>= Path.fold ?err ?over ?traverse f acc

  let descendants ?err ?over ?traverse d =
    fold_contents ?err ?over ?traverse (fun acc p -> p :: acc) [] d
end

(* Commands *)

(* FIXME in these functions should [cmd] and [args] be quoted ? *)

module Cmd = struct
  let exists ?err cmd =
    try
      let null = path_str File.dev_null in
      (* Using Sys.os_type, because that's really for the driver. *)
      let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
      let err_msg cmd = R.error_msgf "%s: no such command" cmd in
      let exists = Sys.command (strf "%s %s 1>%s 2>%s" test cmd null null)= 0 in
      ret_exists ?err err_msg cmd exists
    with Sys_error e -> R.error_msg e

  let trace cmd = Bos_log.info ~header:"EXEC" "@[<2>%a@]" Bos_fmt.pp_text cmd
  let mk_cmd cmd args = String.concat " " (cmd :: args)

  let execute cmd = trace cmd; Sys.command cmd
  let exec_ret cmd args = execute (mk_cmd cmd args)
  let handle_ret cmd = match execute cmd with
  | 0 -> R.ok ()
  | c -> R.error_msgf "Exited with code: %d `%s'" c cmd

  let exec cmd args = handle_ret (mk_cmd cmd args)
  let exec_read ?(trim = true) cmd args =
    let cmd = mk_cmd cmd args in
    File.temp "cmd-read"
    >>= fun file -> handle_ret (strf "%s > %s" cmd (path_str file))
    >>= fun () -> File.read file
    >>= fun v -> R.ok (if trim then String.trim v else v)

  let exec_read_lines cmd args =
    exec_read cmd args >>| String.split ~sep:"\n"

  let exec_write cmd args file =
    let cmd = mk_cmd cmd args in
    File.temp "cmd-write"
    >>= fun tmpf -> handle_ret (strf "%s > %s" cmd (path_str tmpf))
    >>= fun () -> Path.move ~force:true tmpf file
end

(* Environment variables lookup *)

module Env = struct
  let var name = try Some (Sys.getenv name) with Not_found -> None

  let value ?(log = Bos_log.Error) name parse ~absent = match var name with
  | None -> absent
  | Some s ->
      match parse s with
      | Ok v -> v
      | Error (`Msg m) ->
          Bos_log.msg log "environment variable %s: %s" name m; absent

  let parser kind k_of_string =
    fun s -> match k_of_string s with
    | None -> R.error_msgf "could not parse %s value from %S" kind s
    | Some v -> Ok v

  let bool =
    let of_string s = match String.lowercase s with
    | "" | "false" | "no" | "n" | "0" -> Some false
    | "true" | "yes" | "y" | "1" -> Some true
    | _ -> None
    in
    parser "bool" of_string
end

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
