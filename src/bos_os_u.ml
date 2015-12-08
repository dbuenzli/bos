(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

type 'a result = ('a, [`Unix of Unix.error]) Rresult.result
let pp_error ppf (`Unix e ) = Fmt.string ppf (Unix.error_message e)
let open_error = function Ok _ as r -> r | Error (`Unix _) as r -> r
let error_to_msg r = R.error_to_msg ~pp_error r

let rec call f v = try Ok (f v) with
| Unix.Unix_error (Unix.EINTR, _, _) -> call f v
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let mkdir p m = try Ok (Unix.mkdir (Fpath.to_string p) m) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let link p p' =
  try Ok (Unix.link (Fpath.to_string p) (Fpath.to_string p')) with
  | Unix.Unix_error (e, _, _) -> Error (`Unix e)

let unlink p = try Ok (Unix.unlink (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let rename p p' =
  try Ok (Unix.rename (Fpath.to_string p) (Fpath.to_string p')) with
  | Unix.Unix_error (e, _, _) -> Error (`Unix e)

let stat p = try Ok (Unix.stat (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let lstat p = try Ok (Unix.lstat (Fpath.to_string p)) with
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

let rec truncate p size = try Ok (Unix.truncate (Fpath.to_string p) size) with
| Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
| Unix.Unix_error (e, _, _) -> Error (`Unix e)

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
