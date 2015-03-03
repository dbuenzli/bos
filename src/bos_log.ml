(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Log level and output *)

type level = Show | Error | Warning | Info | Debug

let level = ref (Some Warning)
let set_level l = level := l
let level () = !level

let should_log l = match level () with
| None -> false | Some l' -> l <= l'

let show_ppf = ref Format.std_formatter
let err_ppf = ref Format.err_formatter
let warn_ppf = ref Format.err_formatter
let info_ppf = ref Format.err_formatter
let debug_ppf = ref Format.err_formatter

let set_formatter spec ppf = match spec with
| `Level Show -> show_ppf := ppf
| `Level Error -> err_ppf := ppf
| `Level Warning -> warn_ppf := ppf
| `Level Info -> info_ppf := ppf
| `Level Debug -> debug_ppf := ppf
| `All ->
    show_ppf := ppf; err_ppf := ppf; warn_ppf := ppf; info_ppf := ppf;
    debug_ppf := ppf

(* Log messages *)

let dumb = Format.err_formatter (* any will do *)
let err_count = ref 0
let warn_count = ref 0

let kmsg ?header k l fmt =
  let default d = match header with None -> d | Some h -> h in
  let k _ = k () in
  if not (should_log l) then Format.ikfprintf k dumb fmt else
  let pp_msg ppf style label fmt =
    Format.kfprintf k ppf
      ("[%a] @[" ^^ fmt ^^ "@]@.") (Bos_fmt.pp_styled_str style) label
  in
  match l with
  | Show ->
      begin match header with
      | None -> Format.kfprintf k !show_ppf ("@[" ^^ fmt ^^ "@]@.")
      | Some h -> pp_msg !show_ppf `Bold h fmt
      end
  | Error ->
      incr err_count; pp_msg !err_ppf `Red (default "ERROR") fmt
  | Warning ->
      incr warn_count; pp_msg !warn_ppf `Yellow (default "WARNING") fmt
  | Info ->
      pp_msg !info_ppf `Blue (default "INFO") fmt
  | Debug ->
      pp_msg !debug_ppf `Green (default "DEBUG") fmt

let msg ?header l fmt = kmsg ?header (fun () -> ()) l fmt
let show ?header fmt = msg ?header Show fmt
let err ?header fmt = msg ?header Error fmt
let warn ?header fmt = msg ?header Warning fmt
let info ?header fmt = msg ?header Info fmt
let debug ?header fmt = msg ?header Debug fmt

(* Log error Results *)

let on_error ?(log = Error) ~pp ~use = function
| Resultv.Ok v -> v
| Resultv.Error e -> kmsg (fun () -> use) log "@[%a@]" pp e

let kon_error ?(log = Error) ~pp ~use = function
| Resultv.Ok _ as r -> r
| Resultv.Error e -> kmsg (fun () -> use) log "@[%a@]" pp e

let on_err_msg ?log ~use = on_error ?log ~pp:Resultv.R.pp_err_msg ~use
let kon_err_msg ?log ~use = kon_error ?log ~pp:Resultv.R.pp_err_msg ~use

(* Log monitoring *)

let err_count () = !err_count
let warn_count () = !warn_count

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
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
