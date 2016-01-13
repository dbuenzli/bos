(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Command existence *)

let err_empty_line = "empty command line"

let exists cmd =
  try
    let cmd = List.hd (Bos_cmd.to_list cmd) in
    let null = Fpath.to_string (Bos_os_file.dev_null) in
    let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
    Ok (Sys.command (strf "%s %s 1>%s 2>%s" test cmd null null) = 0)
  with
  | Sys_error e -> R.error_msg e
  | Failure _ -> invalid_arg err_empty_line

let must_exist cmd =
  exists cmd >>= function
  | false -> R.error_msgf "%s: no such command" (List.hd (Bos_cmd.to_list cmd))
  | true -> Ok ()

(* FIXME in these functions [cmd] and [args] should be quoted. *)
let trace line =
  Bos_log.info (fun m -> m ~header:"EXEC" "@[<2>%a@]" Fmt.text line)

let mk_line l = match Bos_cmd.to_list l with
| [] -> invalid_arg err_empty_line
| line -> String.concat ~sep:" " line

let execute line = trace line; Sys.command line

let exec_ret line = execute (mk_line line)
let handle_ret line = match execute line with
| 0 -> R.ok ()
| c -> R.error_msgf "Exited with code: %d `%s'" c line

let exec line = handle_ret (mk_line line)
let exec_read ?(trim = true) line =
  Bos_os_file.tmp "bos-%s.tmp"
  >>= fun file ->
  handle_ret (strf "%s > %s" (mk_line line) (Fpath.to_string file))
  >>= fun () -> Bos_os_file.read file
  >>= fun v -> Ok (if trim then String.trim v else v)

let exec_read_lines line =
  exec_read line >>| String.cuts ~sep:"\n"

let exec_write line file =
  Bos_os_file.tmp "bos-%s.tmp"
  >>= fun tmpf -> handle_ret (strf "%s > %s" (mk_line line)
                                (Fpath.to_string tmpf))
  >>= fun () -> Bos_os_path.move ~force:true tmpf file

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
