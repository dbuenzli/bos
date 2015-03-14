(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Bos} with {!Unix} module support.

    [Bos_unix] is {!Bos} enhanced with {!Unix} support. Open this
    module rather than {!Bos} to use it.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Preliminaries, formatting and logging} *)

module String : module type of Bos.String
  with type t = Bos.String.t
   and type Set.t = Bos.String.Set.t
   and type 'a Map.t = 'a Bos.String.Map.t

module Fmt : module type of Bos.Fmt with type 'a t = 'a Bos.Fmt.t
module Pat : module type of Bos.Pat
  with type t = Bos.Pat.t
   and type env = Bos.Pat.env

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

(** OS interaction

    Like {!Bos} most functions in the following modules return
    {!result} values which use {{!Result.R.msgs}error messages}. If you need
    fine grained control over unix errors use the lower level
    functions in {!U}. *)
module OS : sig

  (** {1 File system operations and commands} *)

  type 'a result = 'a Bos.OS.result

  module Path : sig
    include module type of Bos.OS.Path

    (** {1 Path status} *)

    val stat : path -> Unix.stats result
    (** [stat p] is [p]'s file information. See also {!U.stat}. *)

    val lstat : path -> Unix.stats result
    (** [lstat p] same as {!stat} but if [p] is a link returns
        information about the link itself. See also {!U.lstat}. *)
  end

  module File : sig
    include module type of Bos.OS.File

    val truncate : path -> int -> unit result
    (** [truncate p size] truncates [p] to [s]. See also {!U.truncate}. *)

  end

  module Dir : sig
    include module type of Bos.OS.Dir

    val create : ?err:bool -> ?path:bool -> ?mode:Unix.file_perm -> path ->
      unit result
    (** [create ~err ~path ~mode dir] creates the directory [dir] with
        file permission [mode] (defaults [0o777]). If [path] is [true]
        (defaults to [false]) intermediate directories are created
        aswell. If [err] is [false] (default) no error is returned if
        the directory already exists. See also {!U.mkdir}. *)
  end

  module Cmd : module type of Bos.OS.Cmd

  (** {1 Environment variables} *)

  (** Environment variables. *)
  module Env : sig
    include module type of Bos.OS.Env

    val set_var : string -> string option -> unit result
    (** [set_var name v] sets the environment variable [name] to [v].

        {b BUG.} The {!Unix} module doesn't bind to [unsetenv(3)],
        hence for now using [None] will not unset the variable, it
        will set it to [""]. This behaviour may change in future
        versions of the library. *)

    val vars : unit -> string String.Map.t result
    (** [vars ()] is a map corresponding to the process environment. *)
  end

  (** {1 POSIX time} *)

  (** POSIX time. *)
  module Time : sig

    include module type of Bos.OS.Time
    with type posix_s = Bos.OS.Time.posix_s
     and type tz_offset_min = Bos.OS.Time.tz_offset_min

    (** {1 Now} *)

    val now_s : unit -> posix_s
    (** [now_s ()] is the operating system's
        {{!Bos.OS.Time.posix_s}POSIX timestamp} for the current time.

        {b Warning.} These timestamps are not monotonic they
        are subject to operating system time adjustements and can
        even go back in time. If you need to measure time spans
        in a single program run use a monotonic time source (e.g.
        {!Mtime}) *)

    (** {1 Time zone offset} *)

    val current_tz_offset_min : unit -> tz_offset_min
    (** [current_tz_offset_min ()] is the operating system's current local
        {{!Bos.OS.Time.tz_offset_min}time zone offset} to UTC in minutes. *)

    (** {1 Printing} *)

    val pp_stamp : ?human:bool -> ?tz_offset_min:tz_offset_min ->
      Format.formatter -> posix_s -> unit
    (** [pp_stamp tz_offset_min human ppf t] formats the POSIX
        timestamp [t] and time zone offset [tz_offset_min] (defaults to [0])
        according to {{:https://tools.ietf.org/html/rfc3339}RFC 3339}.

        If [human] is [true] (defaults to [false]) date and time are
        separated by a space rather than a ['T'], and a space is
        inserted betwen time and offset but this is {b not} RFC 3339
        compliant. *)

    val pp_stamp_now : ?human:bool -> Format.formatter -> unit  -> unit
    (** [pp_now human ppf ()] is
        [pp_stamp ~human ~tz_offset:(current_tz_offset_min ()) ppf
         (now_s ())]. *)
  end


  (** {1 Low level {!Unix} access} *)

  (** Low level {!Unix} access.

      These functions simply {{!call}call} functions from the {!Unix}
      module and replace strings with {!path} where appropriate.  They
      also provide more fine grained error handling, for example
      {!OS.Path.stat} converts the error to a message while {!stat}
      gives you the {{!Unix.error}Unix error}. *)
  module U : sig

    (** {1 Error handling} *)

    type 'a result = ('a, [`Unix of Unix.error]) Rresult.result
    (** The type for Unix results. *)

    val pp_error : Format.formatter -> [`Unix of Unix.error] -> unit
    (** [pp_error ppf e] prints [e] on [ppf]. *)

    val open_error : ('a, [`Unix of Unix.error]) Rresult.result ->
      ('a, [> `Unix of Unix.error]) Rresult.result
    (** [open_error r] allows to combine a closed unix error
        variant with other variants. *)

    val error_to_msg : ('a, [`Unix of Unix.error]) Rresult.result ->
      ('a, Rresult.R.msg) Rresult.result
    (** [error_to_msg r] converts unix errors in [r] to an error message. *)

    (** {1 Wrapping {!Unix} calls} *)

    val call : ('a -> 'b) -> 'a -> 'b result
    (** [call f v] is [Ok (f v)] but {!Unix.EINTR} errors are catched
        and handled by retrying the call. Other errors [e] are catched
        aswell and returned as [Error (`Unix e)]. *)

    (** {1 File system operations} *)

    val mkdir : path -> Unix.file_perm -> unit result
    (** [mkdir] is {!Unix.mkdir}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdir.html}
        POSIX [mkdir]}. *)

    val link : path -> path -> unit result
    (** [link] is {!Unix.link}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/link.html}
        POSIX [link]}. *)

    val unlink : path -> unit result
    (** [stat] is {!Unix.unlink},
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/unlink.html}
        POSIX [unlink]}. *)

    val rename : path -> path -> unit result
    (** [rename] is {!Unix.rename}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/rename.html}
        POSIX [rename]}. *)

    val stat : path -> Unix.stats result
    (** [stat] is {!Unix.stat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html}
        POSIX [stat]}. *)

    val lstat : path -> Unix.stats result
    (** [lstat] is {!Unix.lstat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html}
        POSIX [lstat]}. *)

    val truncate : path -> int -> unit result
    (** [truncate] is {!Unix.truncate}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/truncate.html}
        POSIX [truncate]}. *)
  end


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
