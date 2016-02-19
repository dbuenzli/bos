(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let unix_buffer_size = 65536                      (* UNIX_BUFFER_SIZE 4.0.0 *)

(* Error messages *)

let err_empty_line = "empty command line"
let uerror = Unix.error_message
let pp_uerror ppf e = Fmt.string ppf (uerror e)

(* Execution status *)

type status = [ `Exited of int | `Signaled of int ]

let pp_status ppf = function
| `Exited c -> Format.fprintf ppf "exited with %d" c
| `Signaled s ->
    Format.fprintf ppf "was signaled with %s"
      begin match s with
      | s when s = Sys.sigabrt -> "SIGABRT"
      | s when s = Sys.sigalrm -> "SIGALRM"
      | s when s = Sys.sigfpe -> "SIGFPE"
      | s when s = Sys.sighup -> "SIGHUP"
      | s when s = Sys.sigill -> "SIGILL"
      | s when s = Sys.sigint -> "SIGINT"
      | s when s = Sys.sigkill -> "SIGKILL"
      | s when s = Sys.sigpipe -> "SIGPIPE"
      | s when s = Sys.sigquit -> "SIGQUIT"
      | s when s = Sys.sigsegv -> "SIGSEGV"
      | s when s = Sys.sigterm -> "SIGTERM"
      | s when s = Sys.sigusr1 -> "SIGUSR1"
      | s when s = Sys.sigusr2 -> "SIGUSR2"
      | s when s = Sys.sigchld -> "SIGCHLD"
      | s when s = Sys.sigcont -> "SIGCONT"
      | s when s = Sys.sigstop -> "SIGSTOP"
      | s when s = Sys.sigtstp -> "SIGTSTP"
      | s when s = Sys.sigttin -> "SIGTTIN"
      | s when s = Sys.sigttou -> "SIGTTOU"
      | s when s = Sys.sigvtalrm -> "SIGVTALRM"
      | s when s = Sys.sigprof -> "SIGPROF"
      | unknown -> strf "signal %d" unknown
      end

(* Primitive from Unix *)

let rec waitpid flags pid = try Unix.waitpid flags pid with
| Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let rec create_process prog args stdin stdout stderr =
  try Unix.create_process prog args stdin stdout stderr with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args stdin stdout stderr

let rec pipe () = try Unix.pipe () with
| Unix.Unix_error (Unix.EINTR, _, _) -> pipe ()

let rec set_close_on_exec fd = try Unix.set_close_on_exec fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> set_close_on_exec fd

let rec openfile fn mode perm = try Unix.openfile fn mode perm with
| Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

let rec close fd = try Unix.close fd with
| Unix.Unix_error (Unix.EINTR, _, _) -> close fd



(* Base primitive *)

let dev_null = lazy
  (Unix.openfile (Fpath.to_string Bos_os_file.dev_null) [Unix.O_RDWR] 0x644)

let create_process ?stdin ?stdout ?stderr cmd =
  let fd fd = match fd with Some fd -> fd | None -> Lazy.force dev_null in
  let line = Bos_cmd.to_list cmd in
  let prog = try List.hd line with Failure _ -> invalid_arg err_empty_line in
  let line = Array.of_list line in
  let pid = create_process prog line (fd stdin) (fd stdout) (fd stderr) in
   Bos_log.info
     (fun m ->
        let header = "EXEC:" ^ string_of_int pid in
        m ~header "@[<1>%a@]" Bos_cmd.dump cmd);
  pid

let wait_process pid = match snd (waitpid [] pid) with
| Unix.WEXITED e -> `Exited e
| Unix.WSIGNALED s -> `Signaled s
| Unix.WSTOPPED _ -> assert false

let exec_cmdline ?stdin ?stdout ?stderr line =
  wait_process (create_process ?stdin ?stdout ?stderr line)

let exec_cmdline_stdio line =
  let stdin, stdout, stderr = Unix.(stdin, stdout, stderr) in
  exec_cmdline ~stdin ~stdout ~stderr line

(* Command existence *)

let exists line =
  let line = Bos_cmd.to_list line in
  let cmd = try List.hd line with Failure _ -> invalid_arg err_empty_line in
  try
    let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
    let test = Bos_cmd.(v test % cmd) in
    match exec_cmdline test with
    | `Exited 0 -> Ok true
    | `Exited _ -> Ok false
    | `Signaled _ as s ->
        R.error_msgf "cmd %s exists: %a %a" cmd Bos_cmd.dump test pp_status s
  with
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "cmd %s exists: %s" cmd (uerror e)

let must_exist cmd =
  exists cmd >>= function
  | false -> R.error_msgf "%s: no such command" (List.hd (Bos_cmd.to_list cmd))
  | true -> Ok cmd

(* Execution *)

let err line pp e = R.error_msgf "exec %a: %a" Bos_cmd.dump line pp e

let exec_ret line = try Ok (exec_cmdline_stdio line) with
| Unix.Unix_error (e, _, _) -> err line pp_uerror e

let exec line = try
  match exec_cmdline_stdio line with
  | `Exited 0 -> Ok ()
  | status -> err line pp_status status
with
| Unix.Unix_error (e, _, _) -> err line pp_uerror e

let string_of_fd fd =
  let len = unix_buffer_size in
  let store = Buffer.create len in
  let b = Bytes.create len in
  let rec loop fd store b =
    match Unix.(try read fd b 0 len with Unix_error (EINTR,_,_) -> -1) with
    | -1 -> loop fd store b
    | 0 -> Buffer.contents store
    | n -> Buffer.add_subbytes store b 0 n; loop fd store b
  in
  loop fd store b

let exec_read ?(trim = true) line =
  try
    let read_stdout, stdout = pipe () in
    try
      Unix.set_close_on_exec read_stdout;
      let stdin, stderr = Unix.stdin, Unix.stderr in
      let pid = create_process ~stdin ~stdout ~stderr line in
      let res = (close stdout; string_of_fd read_stdout) in
      let res = if trim then String.trim res else res in
      match wait_process pid with
      | `Exited 0 -> close read_stdout; Ok res
      | status -> close read_stdout; err line pp_status status
    with
    | Unix.Unix_error (e, _, _) -> close read_stdout; err line pp_uerror e
  with
  | Unix.Unix_error (e, _, _) -> err line pp_uerror e

let exec_read_lines line =
  exec_read line >>| String.cuts ~sep:"\n"

let exec_write ?(mode = 0o644) line file =
  let exec_write file =
    try
      let flags = Unix.([O_WRONLY; O_CREAT]) in
      let stdout = openfile (Fpath.to_string file) flags mode in
      try
        let stdin, stderr = Unix.stdin, Unix.stderr in
        let pid = create_process ~stdin ~stdout ~stderr line in
        match (close stdout; wait_process pid) with
        | `Exited 0 -> Ok ()
      | status -> err line pp_status status
      with
      | Unix.Unix_error (e, _, _) -> err line pp_uerror e
    with Unix.Unix_error (e, _, _) ->
      (* FIXME bad error *)
      err line pp_uerror e
  in
  Bos_os_file.tmp "bos-%s.tmp" ~mode ~dir:(Fpath.parent file)
  >>= fun tmpf -> exec_write tmpf
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
