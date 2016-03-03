(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let uerror = Unix.error_message

(* Existence, creation, deletion, contents *)

let rec exists dir =
  try Ok (Unix.((stat @@ Fpath.to_string dir).st_kind = S_DIR)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a exists: %s" Fpath.pp dir (uerror e)

let rec must_exist dir =
  try
    match Unix.((stat @@ Fpath.to_string dir).st_kind) with
    | Unix.S_DIR -> Ok dir
    | _ -> R.error_msgf "%a must exist: Not a directory" Fpath.pp dir
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      R.error_msgf "%a must exist: No such directory" Fpath.pp dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a must exist: %s" Fpath.pp dir (uerror e)

let create ?(path = true) ?(mode = 0o755) dir =
  let rec mkdir d mode = try Ok (Unix.mkdir (Fpath.to_string d) mode) with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
      if d = dir
      then R.error_msgf "create directory %a: %s" Fpath.pp d (uerror e)
      else R.error_msgf "create directory %a: %a: %s"
             Fpath.pp dir Fpath.pp d (uerror e)
  in
  exists dir >>= function
  | true -> Ok true
  | false ->
      match path with
      | false -> mkdir dir mode >>= fun () -> Ok false
      | true ->
          let rec dirs_to_create p acc = exists p >>= function
          | true -> Ok acc
          | false -> dirs_to_create (Fpath.parent p) (p :: acc)
          in
          let rec create_them dirs () = match dirs with
          | dir :: dirs -> mkdir dir mode >>= create_them dirs
          | [] -> Ok ()
          in
          dirs_to_create dir []
          >>= fun dirs -> create_them dirs ()
          >>= fun () -> Ok false

let rec contents ?(rel = false) dir =
  let rec readdir dh acc =
    match (try Some (Unix.readdir dh) with End_of_file -> None) with
    | None -> Ok acc
    | Some (".." | ".") -> readdir dh acc
    | Some f ->
        match Fpath.of_string f with
        | Some f ->
            readdir dh ((if rel then f else Fpath.(dir // f)) :: acc)
        | None ->
            R.error_msgf
              "%a directory contents: cannot parse element to a path (%a)"
              Fpath.pp dir String.dump f
  in
  try
    let dh = Unix.opendir (Fpath.to_string dir) in
    Bos_base.apply (readdir dh) [] ~finally:Unix.closedir dh
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> contents ~rel dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a directory contents: %s" Fpath.pp dir (uerror e)

let fold_contents ?err ?dotfiles ?elements ?traverse f acc d =
  contents d >>= Bos_os_path.fold ?err ?dotfiles ?elements ?traverse f acc

let rec delete_files to_rmdir dirs = match dirs with
| [] -> Ok to_rmdir
| dir :: todo ->
    let rec delete_dir_files dh dirs =
      match (try Some (Unix.readdir dh) with End_of_file -> None) with
      | None -> Ok dirs
      | Some (".." | ".") -> delete_dir_files dh dirs
      | Some file ->
          let rec try_unlink file =
            try (Unix.unlink (Fpath.to_string file); Ok dirs) with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok dirs
            | Unix.Unix_error (Unix.EPERM, _, _) -> Ok (file :: dirs)
            | Unix.Unix_error (Unix.EINTR, _, _) -> try_unlink file
            | Unix.Unix_error (e, _, _) ->
                R.error_msgf "%a: %s" Fpath.pp file (uerror e)
          in
          match try_unlink Fpath.(dir / file) with
          | Ok dirs -> delete_dir_files dh dirs
          | Error _ as e -> e
    in
    try
      let dh = Unix.opendir (Fpath.to_string dir) in
      match Bos_base.apply (delete_dir_files dh) [] ~finally:Unix.closedir dh
      with
      | Ok dirs -> delete_files (dir :: to_rmdir) (List.rev_append dirs todo)
      | Error _ as e -> e
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> delete_files to_rmdir todo
    | Unix.Unix_error (Unix.EINTR, _, _) -> delete_files to_rmdir dirs
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "%a: %s" Fpath.pp dir (uerror e)

let rec delete_dirs = function
| [] -> Ok ()
| dir :: dirs ->
    let rec rmdir dir = try Ok (Unix.rmdir (Fpath.to_string dir)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> rmdir dir
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "%a: %s" Fpath.pp dir (uerror e)
    in
    match rmdir dir with
    | Ok () -> delete_dirs dirs
    | Error _ as e -> e

let delete ?must_exist:(must = false) ?(recurse = false) dir =
  let rec must_exist dir =
    try
      match Unix.((stat (Fpath.to_string dir)).st_kind) with
      | Unix.S_DIR -> Ok ()
      | _ -> R.error_msg "Not a directory"
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
    | Unix.Unix_error (Unix.ENOENT, _, _) -> R.error_msg "No such directory"
    | Unix.Unix_error (e, _, _) -> R.error_msgf "%s" (uerror e)
  in
  let delete recurse dir =
    if not recurse then
      let rec rmdir dir = try Ok (Unix.rmdir (Fpath.to_string dir)) with
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
      R.error_msgf "delete directory %a: %s" Fpath.pp dir msg

(* Current working directory *)

let rec current () =
  try
    let p = Unix.getcwd () in
    match Fpath.of_string p with
    | Some dir ->
        if Fpath.is_abs dir then Ok dir else
        R.error_msgf "getcwd(3) returned a relative path: (%a)" Fpath.pp dir
    | None ->
        R.error_msgf
          "get current working directory: cannot parse it to a path (%a)"
          String.dump p
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "get current working directory: %s" (uerror e)

let rec set_current dir = try Ok (Unix.chdir (Fpath.to_string dir)) with
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

let user () =
  let debug err = Bos_log.debug (fun m -> m "OS.Dir.user: %s" err) in
  let env_var_fallback () =
    Bos_os_env.(parse "HOME" (some path) ~absent:None) >>= function
    | Some p -> Ok p
    | None -> R.error_msgf "cannot determine user home directory: \
                            HOME environment variable is undefined"
  in
  if Sys.os_type = "Win32" then env_var_fallback () else
  try
    let uid = Unix.getuid () in
    let home = (Unix.getpwuid uid).Unix.pw_dir in
    match Fpath.of_string home with
    | Some p -> Ok p
    | None ->
        debug (strf "could not parse path (%a) from passwd entry"
                 String.dump home);
        env_var_fallback ()
  with
  | Unix.Unix_error (e, _, _) -> (* should not happen *)
      debug (uerror e); env_var_fallback ()
  | Not_found ->
      env_var_fallback ()

(* Temporary directories *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let delete_tmp dir = ignore (delete ~recurse:true dir)
let tmps = ref Fpath.Set.empty
let tmps_add file = tmps := Fpath.Set.add file !tmps
let tmps_rem file = delete_tmp file; tmps := Fpath.Set.remove file !tmps
let delete_tmps () = Fpath.Set.iter delete_tmp !tmps
let () = at_exit delete_tmps

let default_tmp_mode = 0o700

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
  let err () =
    R.error_msgf "create temporary directory %s in %a: \
                  too many failing attempts"
      (strf pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let dir = Bos_os_tmp.rand_path dir pat in
    try Ok (Unix.mkdir (Fpath.to_string dir) mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "create temporary directory %s in %a: %s"
          (strf pat "XXXXXX") Fpath.pp dir (uerror e)
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

let default_tmp = Bos_os_tmp.default_dir
let set_default_tmp = Bos_os_tmp.set_default_dir

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