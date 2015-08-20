(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

(* Patterns and logging *)

module Pat = Bos_pat
module Log = Bos_log

(* Paths *)

module Path = Bos_path
type path = Path.t

(* OS interaction *)

module OS = struct
  type 'a result = ('a, R.msg) R.t

  module Path = Bos_path_os
  module File = Bos_file
  module Dir = Bos_dir
  module Cmd = Bos_cmd
  module Env = Bos_env
  module U = Bos_u

  (* FIXME remove this. *)

  module Time = struct
    type posix_s = float
    type tz_offset_s = float

    let now_s = Unix.gettimeofday

    let tz_offset_s t =
      let utc = Unix.gmtime t in
      let local = Unix.localtime t in
      let dd = local.Unix.tm_yday - utc.Unix.tm_yday in
      let dh = local.Unix.tm_hour - utc.Unix.tm_hour in
      let dm = dh * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
      let dm =
        if dd = 1 || dd < -1 (* year wrap *) then dm + (24 * 60) else
        if dd = -1 || dd > 1 (* year wrap *) then dm - (24 * 60) else
        dm  (* same day *)
      in
      float (60 * dm)

    let current_tz_offset_s () = tz_offset_s (now_s ())

    let pp_stamp ?(human = false) ?(tz_offset_s = 0.) ppf t =
      (* RFC 3339 is written in local time w.r.t. to the offset, so we
         add the offset to the stamp to render the calendar fields *)
      let local_t = t +. tz_offset_s in
      let c = Unix.gmtime local_t in
      let int_of_round x = truncate (floor (x +. 0.5)) in
      let tz_min = int_of_round (tz_offset_s /. 60.) in
      let tsep = if human then ' ' else 'T' in
      let osep = if human then " " else "" in
      Fmt.pf ppf "%04d-%02d-%02d%c%02d:%02d:%02d%s%c%02d%02d"
        (c.Unix.tm_year + 1900) (c.Unix.tm_mon + 1) c.Unix.tm_mday
        tsep
        c.Unix.tm_hour c.Unix.tm_min c.Unix.tm_sec
        osep
        (if tz_min < 0 then '-' else '+') (tz_min / 60) (tz_min mod 60)

    let pp_stamp_now ?human ppf () =
      let now = now_s () in
      pp_stamp ?human ~tz_offset_s:(tz_offset_s now) ppf now
  end
end

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
