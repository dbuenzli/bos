(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let uerror = Unix.error_message

(* Existence, creation, deletion, contents *)

let rec exists dir =
  try Ok (Unix.((stat dir).st_kind = S_DIR)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "directory %a exists: %s" Bos_path.pp dir (uerror e)

let rec must_exist dir =
  try
    match Unix.((stat dir).st_kind) with
    | Unix.S_DIR -> Ok ()
    | _ ->
        R.error_msgf "directory %a must exist: Not a directory" Bos_path.pp dir
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      R.error_msgf "directory %a must exist: No such directory" Bos_path.pp dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "directory %a must exist: %s" Bos_path.pp dir (uerror e)

let create ?(path = true) ?(mode = 0o755) dir =
  let rec chmod dir mode = try Ok (Unix.chmod dir mode) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> chmod dir mode
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "create directory %a: %s" Bos_path.pp dir (uerror e)
  in
  let rec mkdir d mode = try Ok (Unix.mkdir d mode) with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
      if d = dir then
        R.error_msgf "create directory %a: %s" Bos_path.pp d (uerror e)
      else
        R.error_msgf "create directory %a: %a: %s"
          Bos_path.pp dir Bos_path.pp d (uerror e)
  in
  exists dir >>= function
  | true -> chmod dir mode
  | false ->
      if not path then mkdir dir mode else
      let rec dirs_to_create p acc =
        exists p >>= function
        | true -> Ok acc
        | false -> dirs_to_create (Bos_path.parent p) (p :: acc)
      in
      let rec create_them dirs () = match dirs with
      | dir :: dirs -> mkdir dir mode >>= create_them dirs
      | [] -> Ok ()
      in
      dirs_to_create dir [] >>= fun dirs -> create_them dirs ()

let rec contents ?(rel = false) dir =
  let rec readdir dh acc =
    match (try Some (Unix.readdir dh) with End_of_file -> None) with
    | None -> Ok acc
    | Some (".." | ".") -> readdir dh acc
    | Some f ->
        match Bos_path.of_string f with
        | Some f ->
            readdir dh ((if rel then f else Bos_path.(dir // f)) :: acc)
        | None ->
            R.error_msgf
              "director %a contents: cannot parse element to a path (%a)"
              Bos_path.pp dir String.dump f
  in
  try
    let dh = Unix.opendir dir in
    Bos_base.apply (readdir dh) [] ~finally:Unix.closedir dh
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> contents ~rel dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "directory %a contents: %s" Bos_path.pp dir (uerror e)

let rec delete_files to_rmdir dirs = match dirs with
| [] -> Ok to_rmdir
| dir :: todo ->
    let rec delete_dir_files dh dirs =
      match (try Some (Unix.readdir dh) with End_of_file -> None) with
      | None -> Ok dirs
      | Some (".." | ".") -> delete_dir_files dh dirs
      | Some file ->
          let rec try_unlink file =
            try (Unix.unlink file; Ok dirs) with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok dirs
            | Unix.Unix_error (Unix.EPERM, _, _) -> Ok (file :: dirs)
            | Unix.Unix_error (Unix.EINTR, _, _) -> try_unlink file
            | Unix.Unix_error (e, _, _) ->
                R.error_msgf "%a: %s" Bos_path.pp file (uerror e)
          in
          match try_unlink Bos_path.(dir / file) with
          | Ok dirs -> delete_dir_files dh dirs
          | Error _ as e -> e
    in
    try
      let dh = Unix.opendir dir in
      match Bos_base.apply (delete_dir_files dh) [] ~finally:Unix.closedir dh
      with
      | Ok dirs -> delete_files (dir :: to_rmdir) (List.rev_append dirs todo)
      | Error _ as e -> e
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> delete_files to_rmdir todo
    | Unix.Unix_error (Unix.EINTR, _, _) -> delete_files to_rmdir dirs
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "%a: %s" Bos_path.pp dir (uerror e)

let rec delete_dirs = function
| [] -> Ok ()
| dir :: dirs ->
    let rec rmdir dir = try Ok (Unix.rmdir dir) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> rmdir dir
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "%a: %s" Bos_path.pp dir (uerror e)
    in
    match rmdir dir with
    | Ok () -> delete_dirs dirs
    | Error _ as e -> e

let delete ?must_exist:(must = false) ?(recurse = false) dir =
  let rec must_exist dir =
    try
      match Unix.((stat dir).st_kind) with
      | Unix.S_DIR -> Ok ()
      | _ -> R.error_msg "Not a directory"
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
    | Unix.Unix_error (Unix.ENOENT, _, _) -> R.error_msg "No such directory"
    | Unix.Unix_error (e, _, _) -> R.error_msgf "%s" (uerror e)
  in
  let delete recurse dir =
    if not recurse then
      let rec rmdir dir = try Ok (Unix.rmdir dir) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
      | Unix.Unix_error (Unix.EINTR, _, _) -> rmdir dir
      | Unix.Unix_error (e, _, _) -> R.error_msgf "%s" (uerror e)
      in
      rmdir dir
    else
    delete_files [] [dir] >>= fun rmdirs ->
    delete_dirs rmdirs
  in
  match
    if must then must_exist dir >>= fun () -> delete recurse dir else
    delete recurse dir
  with
  | Ok _ as r -> r
  | Error (`Msg msg) ->
      R.error_msgf "delete directory %a: %s" Bos_path.pp dir msg

(* Current working directory *)

let rec current () =
  try
    let p = Unix.getcwd () in
    match Bos_path.of_string p with
    | Some dir -> Ok dir
    | None ->
        R.error_msgf
          "get current working directory: cannot parse it to a path (%a)"
          String.dump p
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "get current working directory: %s" (uerror e)

let rec set_current dir = try Ok (Unix.chdir dir) with
| Unix.Unix_error (Unix.EINTR, _, _) -> set_current dir
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "set current working directory: %s" (uerror e)

let with_current dir f v =
  current () >>= fun old ->
  try
    set_current dir >>= fun () ->
    let r = f v in
    match set_current old with
    | Ok () -> r
    | Error _ as e ->
        match r with
        | Ok _ -> e
        | Error _ as e -> e (* communicate error from [f], TODO bos log *)
  with
  | exn -> ignore (set_current old) (* TODO bos log *); raise exn

(* Directory folding *)

let contents_fold ?err ?over ?traverse f acc d =
  contents d >>= Bos_path_os.fold ?err ?over ?traverse f acc

let descendants ?err ?over ?traverse d =
  contents_fold ?err ?over ?traverse (fun acc p -> p :: acc) [] d

(* Temporary directories *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let delete_tmp dir = ignore (delete ~recurse:true dir)
let tmps = ref Bos_path.Set.empty
let tmps_add file = tmps := Bos_path.Set.add file !tmps
let tmps_rem file = delete_tmp file; tmps := Bos_path.Set.remove file !tmps
let delete_tmps () = Bos_path.Set.iter delete_tmp !tmps
let () = at_exit delete_tmps

let default_tmp_mode = 0o700

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_tmp.default_dir () | Some d -> d in
  let err () =
    R.error_msgf "create temporary directory %s in %a: \
                  too many failing attempts"
      (strf pat "XXXXXX") Bos_path.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let dir = Bos_tmp.rand_path dir pat in
    try Ok (Unix.mkdir dir mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "create temporary directory %s in %a: %s"
          (strf pat "XXXXXX") Bos_path.pp dir (uerror e)
  in
  match loop 10000 with
  | Ok dir as r -> tmps_add dir; r
  | Error _ as e -> e

let with_tmp ?mode ?dir pat f v =
  tmp ?mode ?dir pat >>= fun dir ->
  try
    let r = f dir v in
    tmps_rem dir;
    r
  with e -> tmps_rem dir; raise e

(* Default temporary directory *)

let default_tmp = Bos_tmp.default_dir
let set_default_tmp = Bos_tmp.set_default_dir

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
