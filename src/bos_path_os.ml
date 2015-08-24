(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Existence and move *)

let exists path =
  try Ok (ignore @@ Unix.((stat path).st_kind); true) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp path (Unix.error_message e)

let must_exist path =
  try Ok (ignore @@ Unix.((stat path))) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> R.error_msgf "%s: No such path" path
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp path (Unix.error_message e)

let move ?(force = false) src dst =
  let rename src dst =
    try Ok (Unix.rename src dst) with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "move %a to %a: %s"
          Bos_path.pp src Bos_path.pp dst (Unix.error_message e)
  in
  if force then rename src dst else
  exists dst >>= function
  | false -> rename src dst
  | true ->
      R.error_msgf "move %a to %a: Destination exists"
        Bos_path.pp src Bos_path.pp dst

(* Status *)

let stat p = try Ok (Unix.stat p) with
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "stat %a: %s" Bos_path.pp p (Unix.error_message e)

let lstat p = try Ok (Unix.lstat p) with
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "lstat %a: %s" Bos_path.pp p (Unix.error_message e)

(* Matching paths *)

let pats_of_path p = failwith "TODO"
(*
   let buf = Buffer.create 255 in
   let parse_seg acc s =
   acc
   >>= fun acc -> Bos_pat.of_string ~buf s
   >>= fun pat -> R.ok (pat :: acc)
   in
   let parse_segs ss = List.fold_left parse_seg (R.ok []) ss in
   match Bos_path.segs p with
   | `Rel ss -> parse_segs ss >>= fun ss -> R.ok (".", List.rev ss)
   | `Abs ss -> parse_segs ss >>= fun ss -> R.ok ("/", List.rev ss)
*)

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
  let p = match Bos_path.of_string p with
  | None -> failwith "TODO"
  | Some p -> p
  in
  let pathify acc (p, _) = p :: acc in
  match_path ~env:None p >>| List.fold_left pathify []

let unify ?(init = String.Map.empty) p =
  let env = Some init in
  let pathify acc (p, map) = match map with
  | None -> assert false
  | Some map ->
      let p = match Bos_path.of_string p with
      | None -> failwith "TODO"
      | Some p -> p
      in
      (p, map) :: acc
  in
  match_path ~env p >>| List.fold_left pathify []

(* Folding over file system hierarchies *)

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p

let dir_exists ?err dir =
  try
    let err_msg file = R.error_msgf "%s: no such directory" dir in
    let exists = Sys.file_exists dir && Sys.is_directory dir in
    ret_exists ?err err_msg dir exists
  with Sys_error e -> R.error_msg e

type 'a res = ('a, R.msg) result
type traverse = [`All | `None | `If of Bos_path.t -> bool res ]
type elements = [ `Any | `Files | `Dirs | `Is of Bos_path.t -> bool res ]
type 'a fold_error = Bos_path.t -> 'a res -> unit res

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
| `Files -> err_predicate_fun err Bos_file.exists
| `Dirs -> err_predicate_fun err dir_exists
| `Is pred -> err_predicate_fun err pred

let is_dir_fun err =
  let is_dir p = try Ok (Sys.is_directory p) with
  | Sys_error e -> R.error_msg e
  in
  err_predicate_fun err is_dir

let readdir_fun err =
  let readdir d = try Ok (Sys.readdir d) with
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
    let process d (acc, to_traverse) bname =
      let p = Bos_path.(d / bname) in
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
    let init acc p = process (Bos_path.parent p) acc (Bos_path.base p) in
    let acc, to_traverse = List.fold_left init (acc, []) paths in
    Ok (loop acc (to_traverse :: []))
  with Fold_stop e -> Error e

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
