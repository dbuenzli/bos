(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Quick setup for simple programs.

    Linking against this module setups {!Logs} and issuing:
{[
open Bos_setup
]}
    in a module is sufficient to bring {!Rresult}, {!Astring} and
    {!Bos} in scope. See also how to use this for
    {{!interpreted}interpreted programs}.

    {e v%%VERSION%% - {{:%%PKG_WWW%% }homepage}} *)

(** {1:interpreted Interpreted programs}

To use {!Bos} and this setup in an interpreted program, start the
file with:
{[
#!/usr/bin/env ocaml
#use "topfind"
#require "bos.setup"
open Bos_setup
]}
   In [emacs] a [M-x merlin-use bos.setup] will allow merlin
   to function correctly.
*)

(** {1 Results} *)

(** The type for results. *)
type ('a, 'b) result = ('a, 'b) Rresult.result = Ok of 'a | Error of 'b

open Result

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
(** [(>>=)] is {!R.( >>= )}. *)

val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
(** [(>>|)] is {!R.( >>| )}. *)

module R = Rresult.R

(** {1 Astring} *)

module Char = Astring.Char
module String = Astring.String

(** {1 Bos} *)

module Pat = Bos.Pat
module Cmd = Bos.Cmd
module OS = Bos.OS

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli.
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
