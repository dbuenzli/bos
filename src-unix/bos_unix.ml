(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult_infix

module String = Bos.String
module Fmt = Bos.Fmt
module Pat = Bos.Pat
module Log = Bos.Log
module Path = Bos.Path
module OS = struct

  let pstr = Path.to_string
  let pp_path = Path.pp

  module U = struct
    type 'a result = ('a, [`Unix of Unix.error]) Rresult.result
    let pp_error ppf (`Unix e ) = Fmt.pp_str ppf (Unix.error_message e)
    let open_error = function Ok _ as r -> r | Error (`Unix _) as r -> r
    let error_to_msg r = R.error_to_msg ~pp_error r

    let rec call f v = try Ok (f v) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> call f v
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let mkdir p m = try Ok (Unix.mkdir (pstr p) m) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let link p p' = try Ok (Unix.link (pstr p) (pstr p')) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let unlink p = try Ok (Unix.unlink (pstr p)) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let rename p p' = try Ok (Unix.rename (pstr p) (pstr p')) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let stat p = try Ok (Unix.stat (pstr p)) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)

    let lstat p = try Ok (Unix.lstat (pstr p)) with
    | Unix.Unix_error (e, _, _) -> Error (`Unix e)
  end

  type 'a result = 'a Bos.OS.result
  module Path = struct

    include Bos.OS.Path

    let stat p = try Ok (Unix.stat (pstr p)) with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "stat %a: %s" pp_path p (Unix.error_message e)

    let lstat p = try Ok (Unix.lstat (pstr p)) with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "lstat %a: %s" pp_path p (Unix.error_message e)
  end

  module File = Bos.OS.File
  module Dir = struct
    include Bos.OS.Dir

    let mkdir err mode d =
      try R.ok (Unix.mkdir (Bos.Path.to_string d) mode) with
      | Unix.Unix_error (Unix.EEXIST, _, _) when not err -> Ok ()
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "mkdir %a: %s" pp_path d (Unix.error_message e)

    let create ?(err = false) ?(path = false) ?(mode = 0o777) d =
      if not path then mkdir err mode d else
      let rec todo p acc =
        exists p >>= fun exists ->
        if exists then R.ok acc else todo (Bos.Path.dirname p) (p :: acc)
      in
      let rec create_them = function
      | d :: [] -> mkdir err mode d
      | d :: ds -> mkdir false mode d >>= fun () -> create_them ds
      | [] -> R.ok ()
      in
      todo d [] >>= create_them
  end

  module Cmd = Bos.OS.Cmd

  module Env = struct
    include Bos.OS.Env

    let set_var name v =
      let v = match v with None -> "" | Some v -> v in
      try R.ok (Unix.putenv name v) with
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "environment variable %s: %s" name (Unix.error_message e)

    let vars () =
      try
        let env = Unix.environment () in
        let add acc assign = match acc with
        | Error _ as e -> e
        | Ok m ->
            match String.cut ~sep:"=" assign with
            | Some (var, value) -> R.ok (String.Map.add var value m)
            | None ->
                R.error_msgf
                  "could not parse process environment variable (%S)" assign
        in
        Array.fold_left add (R.ok String.Map.empty) env
      with
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf
            "could not get process environment: %s" (Unix.error_message e)
  end

  module Time = struct
    include Bos.OS.Time
(*      with type posix_s = Bos.OS.Time.posix_s
       and type tz_offset_min = Bos.OS.Time.tz_offset_min *)


    let now_s = Unix.gettimeofday

    let tz_offset_min t =
      let utc = Unix.gmtime t in
      let local = Unix.localtime t in
      let dd = local.Unix.tm_yday - utc.Unix.tm_yday in
      let dh = local.Unix.tm_hour - utc.Unix.tm_hour in
      let dm = dh * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
      if dd = 1 || dd < -1 (* year wrap *) then dm + (24 * 60) else
      if dd = -1 || dd > 1 (* year wrap *) then dm - (24 * 60) else
      dm (* same day *)

    let current_tz_offset_min () = tz_offset_min (now_s ())

    let pp_stamp ?(human = false) ?(tz_offset_min = 0) ppf t =
      (* RFC 3339 is written in local time w.r.t. to the offset, so we
         add the offset to the stamp to render the calendar fields *)
      let tz = tz_offset_min in
      let local_t = t +. (float (60 * tz)) in
      let c = Unix.gmtime local_t in
      let tsep = if human then ' ' else 'T' in
      let osep = if human then " " else "" in
      Fmt.pp ppf "%04d-%02d-%02d%c%02d:%02d:%02d%s%c%02d%02d"
        (c.Unix.tm_year + 1900) (c.Unix.tm_mon + 1) c.Unix.tm_mday
        tsep
        c.Unix.tm_hour c.Unix.tm_min c.Unix.tm_sec
        osep
        (if tz < 0 then '-' else '+') (tz / 60) (tz mod 60)

    let pp_stamp_now ?human ppf () =
      let now = now_s () in
      pp_stamp ?human ~tz_offset_min:(tz_offset_min now) ppf now

  end
end

type path = Path.t

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
