(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

module Prelude = Bos.Prelude
module Fmt = struct
  include Bos.Fmt

  let pp_now ?(rfc = false) ppf () =
    let tz_offset local utc = (* computes the timezone offset w.r.t. utc. *)
      let dd = local.Unix.tm_yday - utc.Unix.tm_yday in
      let dh = local.Unix.tm_hour - utc.Unix.tm_hour in
      let dm = dh * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
      if dd = 1 || dd < -1 (* year wrap *) then dm + (24 * 60) else
      if dd = -1 || dd > 1 (* year wrap *) then dm - (24 * 60) else
      dm (* same day *)
    in
    let now = Unix.gettimeofday () in
    let local = Unix.localtime now in
    let utc = Unix.gmtime now in
    let tz = tz_offset local utc in
    let sep = if rfc then 'T' else ' ' in
    Format.fprintf ppf "%04d-%02d-%02d%c%02d:%02d:%02d%c%02d%02d"
      (local.Unix.tm_year + 1900) (local.Unix.tm_mon + 1) local.Unix.tm_mday
      sep
      local.Unix.tm_hour local.Unix.tm_min local.Unix.tm_sec
      (if tz < 0 then '-' else '+') (tz / 60) (tz mod 60)
end

module Log = Bos.Log

module Path = Bos.Path
type path = Path.t

module OS = Bos.OS


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
