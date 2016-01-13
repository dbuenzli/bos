(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Error messages *)

let err_invalid_input = "input no longer valid, did it escape its scope ?"
let err_invalid_output = "output no longer valid, did it escape its scope ?"
let uerror = Unix.error_message

(* Famous file paths *)

let dev_null = Fpath.v (if Sys.os_type = "Win32" then "NUL" else "/dev/null")
let dash = Fpath.v "-"
let is_dash = Fpath.equal dash

(* Existence and deletion *)

let rec exists file =
  try Ok (Unix.((stat @@ Fpath.to_string file).st_kind = S_REG)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> exists file
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "file %a exists: %s" Fpath.pp file (uerror e)

let rec must_exist file =
  try
    match Unix.((stat @@ Fpath.to_string file).st_kind) with
    | Unix.S_REG -> Ok ()
    | _ -> R.error_msgf "file %a must exist: Not a file" Fpath.pp file
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist file
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      R.error_msgf "file %a must exist: No such file" Fpath.pp file
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "file %a must exist: %s" Fpath.pp file (uerror e)

let delete ?(must_exist = false) file =
  let rec unlink file = try Ok (Unix.unlink @@ Fpath.to_string file) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unlink file
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      if not must_exist then Ok () else
      R.error_msgf "delete file %a: No such file" Fpath.pp file
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "delete file %a: %s" Fpath.pp file (uerror e)
  in
  unlink file

let rec truncate p size =
  try Ok (Unix.truncate (Fpath.to_string p) size) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "truncate file %a: %s" Fpath.pp p (uerror e)

(* Input *)

type input = unit -> (bytes * int * int) option

let with_input file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin (Fpath.to_string file)
    in
    let ic_valid = ref true in
    let close ic =
      ic_valid := false; if is_dash file then () else close_in ic
    in
    let bsize = 65536 (* IO_BUFFER_SIZE *) in
    let b = Bytes.create bsize in
    let input () =
      if not !ic_valid then invalid_arg err_invalid_input else
      let rc = input ic b 0 bsize in
      if rc = 0 then None else Some (b, 0, rc)
    in
    Ok (Bos_base.apply (f input) v ~finally:close ic)
  with
  | Sys_error e -> R.error_msg e

let with_ic file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin (Fpath.to_string file)
    in
    let close ic = if is_dash file then () else close_in ic in
    Ok (Bos_base.apply (f ic) v ~finally:close ic)
  with
  | End_of_file -> R.error_msgf "%a: unexpected end of file" Fpath.pp file
  | Sys_error e -> R.error_msg e

let read file =
  let input ic () =
    let len = in_channel_length ic in
    if len <= Sys.max_string_length then begin
      let s = Bytes.create len in
      really_input ic s 0 len;
      Ok (Bytes.unsafe_to_string s)
    end else begin
      R.error_msgf "read %a: file too large (%a, max supported size: %a)"
        Fpath.pp file Fmt.byte_size len Fmt.byte_size Sys.max_string_length
    end
  in
  match with_ic file input () with
  | Ok (Ok _ as v) -> v
  | Ok (Error _ as e) -> e
  | Error _ as e -> e


let fold_lines f acc file =
  let input ic acc =
    let rec loop acc =
      match try Some (input_line ic) with End_of_file -> None with
      | None -> acc
      | Some line -> loop (f acc line)
    in
    loop acc
  in
  with_ic file input acc

let read_lines file = fold_lines (fun acc l -> l :: acc) [] file >>| List.rev

(* Temporary files *)

type tmp_name_pat = (string -> string, Format.formatter, unit, string) format4

let rec unlink_tmp file = try Unix.unlink (Fpath.to_string file) with
| Unix.Unix_error (Unix.EINTR, _, _) -> unlink_tmp file
| Unix.Unix_error (e, _, _) -> ()

let tmps = ref Fpath.Set.empty
let tmps_add file = tmps := Fpath.Set.add file !tmps
let tmps_rem file = unlink_tmp file; tmps := Fpath.Set.remove file !tmps
let unlink_tmps () = Fpath.Set.iter unlink_tmp !tmps

let () = at_exit unlink_tmps

let create_tmp_path mode dir pat =
  let err () =
    R.error_msgf "create temporary file %s in %a: too many failing attempts"
      (strf pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err () else
    let file = Bos_os_tmp.rand_path dir pat in
    let sfile = Fpath.to_string file in
    try Ok (file, Unix.(openfile sfile [O_WRONLY; O_CREAT; O_EXCL] mode)) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "create temporary file %a: %s" Fpath.pp file (uerror e)
  in
  loop 10000

let default_tmp_mode = 0o600

let tmp ?(mode = default_tmp_mode) ?dir pat =
  let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
  create_tmp_path mode dir pat >>= fun (file, fd) ->
  let rec close fd = try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd
  | Unix.Unix_error (e, _, _) -> () (* TODO bos log *)
  in
  close fd; tmps_add file; Ok file

let with_tmp_oc ?(mode = default_tmp_mode) ?dir pat f v =
  try
    let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    let oc = Unix.out_channel_of_descr fd in
    let delete_close oc = tmps_rem file; close_out oc in
    tmps_add file;
    Ok (Bos_base.apply (f file oc) v ~finally:delete_close oc)
  with Sys_error e -> R.error_msg e

let with_tmp_output ?(mode = default_tmp_mode) ?dir pat f v =
  try
    let dir = match dir with None -> Bos_os_tmp.default_dir () | Some d -> d in
    create_tmp_path mode dir pat >>= fun (file, fd) ->
    let oc = Unix.out_channel_of_descr fd in
    let oc_valid = ref true in
    let delete_close oc = oc_valid := false; tmps_rem file; close_out oc in
    let output b =
      if not !oc_valid then invalid_arg err_invalid_output else
      match b with
      | Some (b, pos, len) -> output oc b pos len
      | None -> flush oc
    in
    tmps_add file;
    Ok (Bos_base.apply (f file output) v ~finally:delete_close oc)
  with Sys_error e -> R.error_msg e

(* Output *)

type output = (bytes * int * int) option -> unit

let default_mode = 0o622

let rec rename src dst =
  try Unix.rename (Fpath.to_string src) (Fpath.to_string dst); Ok () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> rename src dst
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "rename %a to %a: %s"
        Fpath.pp src Fpath.pp dst (uerror e)

let stdout_with_output f v =
  try
    let output_valid = ref true in
    let close () = output_valid := false in
    let output b =
      if not !output_valid then invalid_arg err_invalid_output else
      match b with
      | Some (b, pos, len) -> output stdout b pos len
      | None -> flush stdout
    in
    Ok (Bos_base.apply (f output) v ~finally:close ())
  with Sys_error e -> R.error_msg e

let with_output ?(mode = default_mode) file f v =
  if is_dash file then stdout_with_output f v else
  let do_write tmp tmp_out v =
    let r = f tmp_out v in
    match rename tmp file with
    | Error _ as e -> e
    | Ok () -> Ok r
  in
  match with_tmp_output ~mode ~dir:(Fpath.parent file) "bos-%s.tmp" do_write v
  with
  | Ok (Ok _ as r) -> r
  | Ok (Error _ as e) -> e
  | Error _ as e -> e

let with_oc ?(mode = default_mode) file f v =
  if is_dash file
  then Ok (Bos_base.apply (f stdout) v ~finally:(fun () -> ()) ())
  else
  let do_write tmp tmp_oc v =
    let r = f tmp_oc v in
    match rename tmp file with
    | Error _ as e -> e
    | Ok () -> Ok r
  in
  match with_tmp_oc ~mode ~dir:(Fpath.parent file) "bos-%s.tmp" do_write v
  with
  | Ok (Ok _ as r) -> r
  | Ok (Error _ as e) -> e
  | Error _ as e -> e

let write ?mode file contents =
  let write oc contents = output_string oc contents; () in
  with_oc ?mode file write contents

let writef ?mode file fmt = (* FIXME avoid the kstrf  *)
  Fmt.kstrf (fun content -> write ?mode file content) fmt

let write_lines ?mode file lines =
  let rec write oc = function
  | [] -> ()
  | l :: ls ->
      output_string oc l;
      if ls <> [] then (output_char oc '\n'; write oc ls) else ()
  in
  with_oc ?mode file write lines

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
