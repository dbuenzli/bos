(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let uerror = Unix.error_message

(* Existence, move information and mode *)

let exists path =
  try Ok (ignore @@ Unix.stat (Fpath.to_string path); true) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Fpath.pp path (uerror e)

let must_exist path =
  try Ok (ignore @@ Unix.stat (Fpath.to_string path); path) with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      R.error_msgf "%a: No such path" Fpath.pp path
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Fpath.pp path (uerror e)

let move ?(force = false) src dst =
  let rename src dst =
    try Ok (Unix.rename (Fpath.to_string src) (Fpath.to_string dst)) with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "move %a to %a: %s"
          Fpath.pp src Fpath.pp dst (uerror e)
  in
  if force then rename src dst else
  exists dst >>= function
  | false -> rename src dst
  | true ->
      R.error_msgf "move %a to %a: Destination exists"
        Fpath.pp src Fpath.pp dst

let rec stat p = try Ok (Unix.stat (Fpath.to_string p)) with
| Unix.Unix_error (Unix.EINTR, _, _) -> stat p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "stat %a: %s" Fpath.pp p (uerror e)

module Mode = struct
  type t = int

  let rec get p = try Ok (Unix.((stat (Fpath.to_string p)).st_perm)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> get p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "get mode %a: %s" Fpath.pp p (uerror e)

  let rec set p m = try Ok (Unix.chmod (Fpath.to_string p) m) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> set p m
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "set mode %a: %s" Fpath.pp p (uerror e)
end

(* Path links *)

let rec force_remove op target p =
  let sp = Fpath.to_string p in
  try match Unix.((stat sp).st_kind) with
  | Unix.S_DIR -> Ok (Unix.rmdir sp)
  | _ -> Ok (Unix.unlink sp)
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> force_remove op target p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "force %s %a to %a: %s" op Fpath.pp target Fpath.pp p
        (uerror e)

let rec link ?(force = false) ~target p =
  try Ok (Unix.link (Fpath.to_string target) (Fpath.to_string p)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~target p
  | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
      force_remove "link" target p >>= fun () -> link ~force ~target p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "link %a to %a: %s"
        Fpath.pp target Fpath.pp p (uerror e)

let rec symlink ?(force = false) ~target p =
  try Ok (Unix.symlink (Fpath.to_string target) (Fpath.to_string p)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> symlink ~force ~target p
  | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
      force_remove "symlink" target p >>= fun () -> symlink ~force ~target p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "symlink %a to %a: %s"
        Fpath.pp target Fpath.pp p (uerror e)

let rec symlink_target p =
  try
    let l = Unix.readlink (Fpath.to_string p) in
    match Fpath.of_string l with
    | Ok l -> Ok l
    | Error _ ->
        R.error_msgf "target of %a: could not read a path from %a"
          Fpath.pp p String.dump l
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_target p
  | Unix.Unix_error (Unix.EINVAL, _, _) ->
      R.error_msgf "target of %a: Not a symbolic link" Fpath.pp p
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "target of %a: %s" Fpath.pp p (uerror e)

let rec symlink_stat p = try Ok (Unix.lstat (Fpath.to_string p)) with
| Unix.Unix_error (Unix.EINTR, _, _) -> symlink_stat p
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "symlink stat %a: %s" Fpath.pp p (uerror e)

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
        match Fpath.is_seg e with
        | true ->
            begin match Bos_pat.match_pat ~env 0 e seg with
            | None -> readdir dh acc
            | Some _ as m ->
                let p =
                  if path = "" then e else
                  Fpath.(to_string (add_seg (Fpath.v path) e))
                in
                readdir dh ((p, m) :: acc)
            end
        | false ->
            R.error_msgf
              "directory %a: cannot parse element to a path (%a)"
              Fpath.pp (Fpath.v path) String.dump e
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
      R.error_msgf "directory %a: %s" Fpath.pp (Fpath.v path) (uerror e)

let match_path ?(dotfiles = false) ~env p =
  let err _ =
    R.msgf "Unexpected error while matching `%a'" Fpath.pp p
  in
  let vol, start, segs =
    let vol, segs = Fpath.split_volume p in
    match Fpath.segs segs with
    | "" :: "" :: [] (* root *) ->  vol, Fpath.dir_sep, []
    | "" :: ss -> vol, Fpath.dir_sep, ss
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
              let acc' = Fpath.(to_string (add_seg (v p) ""), env) :: acc in
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
          let p =
            if p = vol then p ^ e (* C:.. *) else
            Fpath.(to_string (add_seg (v p) e))
          in
          loop ((p, env) :: acc) matches
      in
      match_segs (loop [] acc) segs
  | seg :: segs ->
      match Bos_pat.of_string seg with
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
    exists (Fpath.v (vol ^ start))
  in
  start_exists vol start >>= function
  | false -> Ok []
  | true ->
      let start = if start = "" then vol else vol ^ start in
      R.reword_error_msg err @@ match_segs [start, env] segs

let matches ?dotfiles p =
  let get_path acc (p, _) = (Fpath.v p) :: acc in
  match_path ?dotfiles ~env:None p >>| List.fold_left get_path []

let query ?dotfiles ?(init = String.Map.empty) p =
  let env = Some init in
  let unopt_map acc (p, map) = match map with
  | None -> assert false
  | Some map -> (Fpath.v p, map) :: acc
  in
  match_path ?dotfiles ~env p >>| List.fold_left unopt_map []

(* Folding over file system hierarchies *)

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p

let dir_exists ?err dir =
  try
    let err_msg dir = R.error_msgf "%a: no such directory" Fpath.pp dir in
    let exists =
      let dir = Fpath.to_string dir in
      Sys.file_exists dir && Sys.is_directory dir
    in
    ret_exists ?err err_msg dir exists
  with Sys_error e -> R.error_msg e

type 'a res = ('a, R.msg) result
type traverse = [ `Any | `None | `Sat of Fpath.t -> bool res ]
type elements = [ `Any | `Files | `Dirs | `Sat of Fpath.t -> bool res ]
type 'a fold_error = Fpath.t -> 'a res -> unit res

let log_fold_error ~level =
  fun p -> function
  | Error (`Msg e) -> Bos_log.msg level (fun m -> m "%s" e); Ok ()
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
| `Any -> fun _ -> true
| `None -> fun _ -> false
| `Sat sat -> err_predicate_fun err sat

let is_element_fun err = function
| `Any -> err_predicate_fun err exists
| `Files -> err_predicate_fun err Bos_os_file.exists
| `Dirs -> err_predicate_fun err dir_exists
| `Sat sat -> err_predicate_fun err sat

let is_dir_fun err =
  let is_dir p = try Ok (Sys.is_directory (Fpath.to_string p)) with
  | Sys_error e -> R.error_msg e
  in
  err_predicate_fun err is_dir

let readdir_fun err =
  let readdir d = try Ok (Sys.readdir (Fpath.to_string d)) with
  | Sys_error e -> R.error_msg e
  in
  err_fun err readdir ~backup_value:[||]

let fold
    ?(err = log_fold_error ~level:Logs.Error)
    ?(dotfiles = false)
    ?(elements = `Any) ?(traverse = `Any)
    f acc paths
  =
  try
    let do_traverse = do_traverse_fun err traverse in
    let is_element = is_element_fun err elements in
    let is_dir = is_dir_fun err in
    let readdir =  readdir_fun err in
    let process d (acc, to_traverse as acc') bname =
      if not dotfiles && String.is_prefix "." bname then acc' else
      let p = Fpath.(d / bname) in
      (if is_element p then (f p acc) else acc),
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
    let init acc p =
      if Fpath.(is_current_dir p || is_parent_dir p)
      then process p acc ""
      else process (Fpath.parent p) acc Fpath.(to_string (base p))
    in
    let acc, to_traverse = List.fold_left init (acc, []) paths in
    (Ok (loop acc (to_traverse :: [])))
  with Fold_stop (`Msg _ as e) -> Error e

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
