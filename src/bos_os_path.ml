(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let uerror = Unix.error_message

(* Existence and move *)

let exists path =
  try Ok (ignore @@ Unix.stat path; true) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp path (uerror e)

let must_exist path =
  try Ok (ignore @@ Unix.stat path) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> R.error_msgf "%s: No such path" path
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp path (uerror e)

let move ?(force = false) src dst =
  let rename src dst =
    try Ok (Unix.rename src dst) with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "move %a to %a: %s"
          Bos_path.pp src Bos_path.pp dst (uerror e)
  in
  if force then rename src dst else
  exists dst >>= function
  | false -> rename src dst
  | true ->
      R.error_msgf "move %a to %a: Destination exists"
        Bos_path.pp src Bos_path.pp dst

let rec stat p = try Ok (Unix.stat p) with
| Unix.Unix_error (Unix.EINTR, _, _) -> stat p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "stat %a: %s" Bos_path.pp p (uerror e)

(* Path links *)

let rec force_remove op target p =
  try match Unix.((stat p).st_kind) with
  | Unix.S_DIR -> Ok (Unix.rmdir p)
  | _ -> Ok (Unix.unlink p)
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> force_remove op target p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "force %s %a to %a: %s" op Bos_path.pp target Bos_path.pp p
        (uerror e)

let rec link ?(force = false) ~target p = try Ok (Unix.link target p) with
| Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~target p
| Unix.Unix_error (Unix.EEXIST, _, _) when force ->
    force_remove "link" target p >>= fun () -> link ~force ~target p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "link %a to %a: %s"
      Bos_path.pp target Bos_path.pp p (uerror e)

let rec symlink ?(force = false) ~target p = try Ok (Unix.symlink target p) with
| Unix.Unix_error (Unix.EINTR, _, _) -> symlink ~force ~target p
| Unix.Unix_error (Unix.EEXIST, _, _) when force ->
    force_remove "symlink" target p >>= fun () -> symlink ~force ~target p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "symlink %a to %a: %s"
      Bos_path.pp target Bos_path.pp p (uerror e)

let rec symlink_target p = try Ok (Unix.readlink p) with
| Unix.Unix_error (Unix.EINTR, _, _) -> symlink_target p
| Unix.Unix_error (Unix.EINVAL, _, _) ->
    R.error_msgf "target of %a: Not a symbolic link" Bos_path.pp p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "target of %a: %s" Bos_path.pp p (uerror e)

let rec symlink_stat p = try Ok (Unix.lstat p) with
| Unix.Unix_error (Unix.EINTR, _, _) -> symlink_stat p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "symlink stat %a: %s" Bos_path.pp p (uerror e)

(* Matching paths. *)

(* The following code is horribly messy mainly due to volume
   handling. Could certainly be improved. *)

let rec match_segment dotfiles ~env acc path seg =
  (* N.B. path can be empty, usually for relative patterns without volume. *)
  let rec readdir dh acc =
    match (try Some (Unix.readdir dh) with End_of_file -> None) with
    | None -> Ok acc
    | Some (".." | ".") -> readdir dh acc
    | Some e when String.length e > 1 && e.[0] = '.' && not dotfiles ->
        readdir dh acc
    | Some e ->
        match Bos_path.is_seg_valid e with
        | true ->
            begin match Bos_pat.match_pat ~env 0 e seg with
            | None -> readdir dh acc
            | Some _ as m ->
                let p = if path = "" then e else Bos_path.add_seg path e in
                readdir dh ((p, m) :: acc)
            end
        | false ->
            R.error_msgf
              "directory %a: cannot parse element to a path (%a)"
              Bos_path.pp path String.dump e
  in
  try
    let path = if path = "" then "." else path in
    let dh = Unix.opendir path in
    Bos_base.apply (readdir dh) acc ~finally:Unix.closedir dh
  with
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> Ok acc
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok acc
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      match_segment dotfiles ~env acc path seg
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "directory %a: %s" Bos_path.pp path (uerror e)

let match_path ?(dotfiles = false) ~env p =
  let err _ = R.msgf "Unexpected error while matching `%s'" p in
  let buf = Buffer.create 256 in
  let vol, start, segs =
    let vol, segs = Bos_path.split_volume p in
    match Bos_path.segs segs with
    | "" :: "" :: [] (* root *) ->  vol, Bos_path.dir_sep, []
    | "" :: ss -> vol, Bos_path.dir_sep, ss
    | ss -> vol, "", ss (* N.B. ss is non empty. *)
  in
  let rec match_segs acc = function
  | [] -> Ok acc
  | "" :: [] -> (* final empty segment "", keep only directories. *)
      let rec loop acc = function
      | [] -> Ok acc
      | (p, env) :: matches ->
          let r = try Ok (Unix.((stat p).st_kind = Unix.S_DIR)) with
          | Unix.Unix_error (e, _, _) -> R.error_msgf "%s: %s" p (uerror e)
          in
          match r with
          | Error _ as e -> e
          | Ok false -> loop acc matches
          | Ok true ->
              let acc' = (Bos_path.add_seg p "", env) :: acc in
              loop acc' matches
      in
      loop [] acc
  | (".." | "." as e) :: segs ->
      (* We simply add the segment to current matches. No need
         to test if the resulting path exists. We can always go up (root
         absorbs) or stay at the same level. *)
      let rec loop acc = function
      | [] -> acc
      | (p, env) :: matches ->
          let p = if p = vol then p ^ e (* C:.. *) else Bos_path.add_seg p e in
          loop ((p, env) :: acc) matches
      in
      match_segs (loop [] acc) segs
  | seg :: segs ->
      match Bos_pat.of_string ~buf seg with
      | Error _ as e -> e
      | Ok seg ->
          let rec loop acc = function
          | [] -> Ok acc
          | (p, env) :: matches ->
              match match_segment dotfiles ~env acc p seg with
              | Error _ as e -> e
              | Ok acc -> loop acc matches
          in
          match loop [] acc with
          | Error _ as e -> e
          | Ok acc -> match_segs acc segs
  in
  let start_exists vol start =
    let start = if start = "" then "." else start in
    exists (vol ^ start)
  in
  start_exists vol start >>= function
  | false -> Ok []
  | true ->
      let start = if start = "" then vol else vol ^ start in
      R.reword_error_msg err @@ match_segs [start, env] segs

let matches ?dotfiles p =
  let get_path acc (p, _) = p :: acc in
  match_path ?dotfiles ~env:None p >>| List.fold_left get_path []

let query ?dotfiles ?(init = String.Map.empty) p =
  let env = Some init in
  let unopt_map acc (p, map) = match map with
  | None -> assert false
  | Some map -> (p, map) :: acc
  in
  match_path ?dotfiles ~env p >>| List.fold_left unopt_map []

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
  | Error (`Msg m) -> (Bos_log.msg level "%s" @@ fun fmt -> fmt m); Ok ()
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
| `Files -> err_predicate_fun err Bos_os_file.exists
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
    ?(err = log_fold_error ~level:Logs.Error) ?(over = `Any)
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
