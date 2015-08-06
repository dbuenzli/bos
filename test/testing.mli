(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* {1 Value equality and pretty printing} *)

type 'a eq = 'a -> 'a -> bool
type 'a pp = Format.formatter -> 'a -> unit

(* {1 Pretty printers} *)

val pp_int : int pp
val pp_bool : bool pp
val pp_float : float pp
val pp_char : char pp
val pp_str : string pp
val pp_list : 'a pp -> 'a list pp
val pp_option : 'a pp -> 'a option pp

(* {1 Logging} *)

val log_part : ('a, Format.formatter, unit) format -> 'a
val log : ?header:string -> ('a, Format.formatter, unit) format -> 'a
val log_results : unit -> bool

(* {1 Testing scopes} *)

type test
type suite

val block : (unit -> unit) -> unit
val test : string -> (unit -> unit) -> test
val suite : string -> test list -> suite

val run : suite list -> unit

(* {1 Passing and failing tests} *)

val pass : unit -> unit
val fail : ('a, Format.formatter, unit, unit) format4 -> 'a

(* {1 Checking values} *)

val eq : eq:'a eq -> pp:'a pp -> 'a -> 'a -> unit
val eq_char : char -> char -> unit
val eq_str : string -> string -> unit
val eq_bool : bool -> bool -> unit
val eq_int : int -> int -> unit
val eq_int32 : int32 -> int32 -> unit
val eq_int64 : int64 -> int64 -> unit
val eq_float : float -> float -> unit
val eq_nan : float -> unit

val eq_option : eq:'a eq -> pp:'a pp -> 'a option -> 'a option -> unit
val eq_some : 'a option -> unit
val eq_none : pp:'a pp -> 'a option -> unit

val eq_list : eq:'a eq -> pp:'a pp -> 'a list -> 'a list -> unit

(* {1 Tracing and checking function applications} *)

type app (* holds information about the application *)

val ( $ ) : 'a -> (app -> 'a -> 'b) -> 'b
val ( @-> ) : 'a pp -> (app -> 'b -> 'c) -> app -> ('a -> 'b) -> 'a -> 'c

val ret : 'a pp -> app -> 'a -> 'a
val ret_eq : eq:'a eq -> 'a pp -> 'a -> app -> 'a -> 'a
val ret_some : 'a pp -> app -> 'a option -> 'a option
val ret_none : 'a pp -> app -> 'a option -> 'a option
val ret_get_option : 'a pp -> app -> 'a option -> 'a

val app_invalid : pp:'b pp -> ('a -> 'b) -> 'a -> unit
val app_exn : pp:'b pp -> exn -> ('a -> 'b) -> 'a -> unit
val app_raises : pp:'b pp -> ('a -> 'b) -> 'a -> unit

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
