(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_prelude

let err_error = str "result value is (`Error _)"

type ('a, 'b) t = [`Ok of 'a | `Error of 'b]



let ret v = `Ok v
let error e = `Error e
let bind v f = match v with `Ok v -> f v | `Error _ as e -> e
let map v f = match v with `Ok v -> `Ok (f v) | `Error _ as e -> e
let get = function `Ok v -> v | `Error msg -> invalid_arg err_error
let ignore_error ~use r = match r with `Ok v -> v | `Error _ -> use
let on_error ?(level = Bos_log.Error) ~use r = match r with
| `Ok v -> v
| `Error msg -> Bos_log.kmsg (fun () -> use) level "@[%a@]" Bos_fmt.pp_text msg

let reword_error ?(replace = false) msg r = match r with
| `Ok _ as r -> r
| `Error _ when replace -> `Error msg
| `Error old -> `Error (str "%s\n%s" msg old)

let exn_msg bt _ _ = Printexc.raw_backtrace_to_string bt
let exn_error ?(msg = exn_msg) f v = try `Ok (f v) with
| e ->
    let bt = Printexc.get_raw_backtrace () in
    `Error (msg bt e v)

let ( >>= ) = bind
let ( >>| ) = map

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

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
