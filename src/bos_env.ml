(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring

(* Lookup *)

let var name = try Some (Sys.getenv name) with Not_found -> None
let opt_var name ~absent = try Sys.getenv name with Not_found -> absent
let req_var name = try Ok (Sys.getenv name) with
| Not_found -> R.error_msgf "environment variable %s: undefined" name

(* Typed lookup *)

type 'a parser = string -> ('a, R.msg) result

let parser kind k_of_string =
  fun s -> match k_of_string s with
  | None -> R.error_msgf "could not parse %s value from %a" kind String.dump s
  | Some v -> Ok v

let bool =
  let of_string s = match String.Ascii.lowercase s with
  | "" | "false" | "no" | "n" | "0" -> Some false
  | "true" | "yes" | "y" | "1" -> Some true
  | _ -> None
  in
  parser "bool" of_string

let string = fun s -> Ok s

let some p =
  fun s -> match p s with
  | Ok v -> Ok (Some v)
  | Error _ as e -> e

let value ?(log = Bos_log.Error) name parse ~absent = match var name with
| None -> absent
| Some s ->
    match parse s with
    | Ok v -> v
    | Error (`Msg m) ->
        Bos_log.msg log "environment variable %s: %s" name m; absent

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
