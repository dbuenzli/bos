(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* FIXME in these functions [cmd] and [args] should be quoted. *)

let path_str = Bos_path.to_string

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p

let exists ?err cmd =
  try
    let null = path_str Bos_file.dev_null in
    let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
    let err_msg cmd = R.error_msgf "%s: no such command" cmd in
    let exists = Sys.command (strf "%s %s 1>%s 2>%s" test cmd null null)= 0 in
    ret_exists ?err err_msg cmd exists
  with Sys_error e -> R.error_msg e

let trace cmd = Bos_log.info ~header:"EXEC" "@[<2>%a@]" Fmt.text cmd
let mk_cmd cmd args = String.concat ~sep:" " (cmd :: args)

let execute cmd = trace cmd; Sys.command cmd
let exec_ret cmd args = execute (mk_cmd cmd args)
let handle_ret cmd = match execute cmd with
| 0 -> R.ok ()
| c -> R.error_msgf "Exited with code: %d `%s'" c cmd

let exec cmd args = handle_ret (mk_cmd cmd args)
let exec_read ?(trim = true) cmd args =
  let cmd = mk_cmd cmd args in
  Bos_file.tmp "cmd-read"
  >>= fun file -> handle_ret (strf "%s > %s" cmd (path_str file))
  >>= fun () -> Bos_file.read file
  >>= fun v -> R.ok (if trim then String.trim v else v)

let exec_read_lines cmd args =
  exec_read cmd args >>| String.cuts ~sep:"\n"

let exec_write cmd args file =
  let cmd = mk_cmd cmd args in
  Bos_file.tmp "cmd-write"
  >>= fun tmpf -> handle_ret (strf "%s > %s" cmd (path_str tmpf))
  >>= fun () -> Bos_path_os.move ~force:true tmpf file

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
