(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Bos

(* Watch a directory for changes. First run will create a database
   watchdb in the directory with modification times. Subsquent runs
   will check files against that database. *)

module Db = struct
  let db_file = Path.v "watchdb"
  let exists () = OS.File.exists db_file
  let scan () =             (* returns list of (path, modification time) *)
    let add acc p =
      (OS.Path.stat p >>= fun stats ->
       if stats.Unix.st_kind <> Unix.S_REG then Ok acc else
       Ok ((p, stats.Unix.st_mtime) :: acc))
      |> Log.on_error_msg ~use:acc
    in
    Log.show "Scanning files";
    OS.Dir.current ()
    >>= fun dir -> OS.Dir.contents_fold ~over:`Files add [] dir

  let dump oc db = Ok Marshal.(to_channel oc db [No_sharing; Compat_32])
  let slurp ic () = Ok (Marshal.from_channel ic : float Path.Map.t)

  let create files =
    Log.show "Writing modification time database %a" Path.pp db_file;
    let count = ref 0 in
    let add acc (f, time) = incr count; Path.Map.add f time acc in
    let db = List.fold_left add Path.Map.empty files in
    OS.File.with_oc db_file dump db >>= fun () -> Ok !count

  let check files =
    let count = ref 0 in
    let changes db (f, time) = match (incr count; Path.Map.find f db) with
    | None -> Log.show "New file: %a" Path.pp f
    | Some stamp when stamp <> time -> Log.show "File changed: %a" Path.pp f
    | _ -> ()
    in
    Log.show "Checking against %a" Path.pp db_file;
    OS.File.with_ic db_file slurp ()
    >>= fun db -> List.iter (changes db) files; Ok !count
end

let watch () =
  Db.scan ()
  >>= fun files -> Db.exists ()
  >>= fun exists -> if exists then Db.check files else Db.create files

let main () =
  let c = Mtime.counter () in
  let count = watch () |> Log.on_error_msg ~use:0 in
  Log.show "Watch completed for %d files in %a on %a"
    count Mtime.pp_span (Mtime.count c) OS.Time.(pp_stamp_now ~human:true) ()

let () = main ()

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
