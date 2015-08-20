(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p


let path_str = Bos_path.to_string

let dir_exists ?err dir =
  try
    let dir = path_str dir in
    let err_msg file = R.error_msgf "%s: no such directory" dir in
    let exists = Sys.file_exists dir && Sys.is_directory dir in
    ret_exists ?err err_msg dir exists
  with Sys_error e -> R.error_msg e

let exists = dir_exists
let current () =
  try
    let p = Sys.getcwd () in
    match Bos_path.of_string p with
    | Some p -> R.ok p
    | None ->
        R.error_msgf "cannot parse getcwd to a path (%a)" String.dump p
  with
  | Sys_error e -> R.error_msgf "current working directory: %s" e

let set_current dir =
  try R.ok (Sys.chdir (path_str dir)) with
  | Sys_error e -> R.error_msg e

let contents ?(path = true) dir =
  try
    let name = if path then Bos_path.add_seg dir else Bos_path.v in
    let files = Sys.readdir (path_str dir) in
    let add_file acc f = name f :: acc in
    R.ok (Array.fold_left add_file [] files)
  with Sys_error e -> R.error_msg e

(* Directory folding *)

let fold_contents ?err ?over ?traverse f acc d =
  contents d >>= Bos_path_os.fold ?err ?over ?traverse f acc

let descendants ?err ?over ?traverse d =
  fold_contents ?err ?over ?traverse (fun acc p -> p :: acc) [] d

let mkdir err mode d =
  try R.ok (Unix.mkdir d mode) with
  | Unix.Unix_error (Unix.EEXIST, _, _) when not err -> Ok ()
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "mkdir %a: %s" Bos_path.pp d (Unix.error_message e)

let create ?(err = false) ?(path = false) ?(mode = 0o777) d =
  if not path then mkdir err mode d else
  let rec todo p acc =
    exists p >>= fun exists ->
    if exists then R.ok acc else todo (Bos_path.parent p) (p :: acc)
  in
  let rec create_them = function
  | d :: [] -> mkdir err mode d
  | d :: ds -> mkdir false mode d >>= fun () -> create_them ds
  | [] -> R.ok ()
  in
  todo d [] >>= create_them

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
