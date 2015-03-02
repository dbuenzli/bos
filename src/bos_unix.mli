(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Bos} Unix support.

    This is {!Bos} enhanced with {!Unix} support. Open this module
    rather than {!Bos} to use it. *)

(** {1 Preliminaries, formatting and logging} *)

module Prelude : module type of Bos.Prelude
  with type String.t = Bos.Prelude.String.t
  and type String.Set.t = Bos.Prelude.String.Set.t
  and type 'a String.Map.t = 'a Bos.Prelude.String.Map.t

module Fmt : sig
  include module type of Bos.Fmt with type 'a t = 'a Bos.Fmt.t

  val pp_now : ?rfc:bool -> unit t
  (** [pp_now space] formats the current time according to
      {{:https://tools.ietf.org/html/rfc3339}RFC3339} if [rfc] is
      [false] (default) date and time are separated by a space
      character rather than a ['T']; use [true] if you need a timestamp
      that matches the [date-time] production of RFC3339. *)
end

module Log : module type of Bos.Log with type level = Bos.Log.level

(** {1 Paths} *)

type path = Bos.path
module Path : module type of Bos.Path
  with type rel = Bos.Path.rel
  and type abs = Bos.Path.abs
  and type t = Bos.Path.t
  and type Set.t = Bos.Path.Set.t
  and type 'a Map.t = 'a Bos.Path.Map.t

(** {1 OS interaction} *)

module OS : sig

  type 'a result = 'a Bos.OS.result

  module Path = Bos.OS.Path
  module File = Bos.OS.File

  module Dir : sig
    include module type of Bos.OS.Dir

    val create : ?err:bool -> ?path:bool -> ?mode:Unix.file_perm -> path ->
      unit result
    (** [create ~err ~path ~mode dir] creates the directory [dir] with
        file permission [mode] (defaults [0o777]). If [path] is [true]
        (defaults to [false]) intermediate directories are created
        aswell. If [err] is [false] (default) no error is returned if
        the directory already exists. *)
  end

  module Cmd = Bos.OS.Cmd
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
