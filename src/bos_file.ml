(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Famous file paths *)

let dev_null = Bos_path.v (if Sys.os_type = "Win32" then "NUL" else "/dev/null")
let dash = Bos_path.v "-"
let is_dash = Bos_path.equal dash

(* Existence and deletion *)

let exists file =
  try Ok (Unix.((stat file).st_kind = S_REG)) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp file (Unix.error_message e)

let must_exist file =
  try
    match Unix.((stat file).st_kind) with
    | Unix.S_REG -> Ok ()
    | _ -> R.error_msgf "%a: Not a file" Bos_path.pp file
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> R.error_msgf "%s: No such file" file
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp file (Unix.error_message e)

let delete ?must_exist:(must = false) file =
  let unlink file = try Ok (Unix.unlink file) with
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "%a: %s" Bos_path.pp file (Unix.error_message e)
  in
  if must then must_exist file >>= fun () -> unlink file else
  exists file >>= function
  | true -> unlink file
  | false -> Ok ()

let rec truncate p size =
  try Ok (Unix.truncate p size) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "truncate %a: %s" Bos_path.pp p (Unix.error_message e)

(* Input *)

let with_inf file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin file in
    let close ic = if is_dash file then () else close_in ic in
    Bos_base.apply (f ic) v ~finally:close ic
  with
  | End_of_file -> R.error_msgf "%a: unexpected end of file" Bos_path.pp file
  | Sys_error e -> R.error_msg e

let read file =
  let input ic () =
    let len = in_channel_length ic in
    if len <= Sys.max_string_length then begin
      let s = Bytes.create len in
      really_input ic s 0 len;
      Ok (Bytes.unsafe_to_string s)
    end else begin
      R.error_msgf "%a: file too large (%a) for Bos.File.read (max: %a)"
        Bos_path.pp file Fmt.byte_size len Fmt.byte_size Sys.max_string_length
    end
  in
  with_inf file input ()

let fold_lines f acc file =
  let input ic acc =
    let rec loop acc =
      match try Some (input_line ic) with End_of_file -> None with
      | None -> Ok acc
      | Some line -> loop (f acc line)
    in
    loop acc
  in
  with_inf file input acc

let read_lines file = fold_lines (fun acc l -> l :: acc) [] file >>| List.rev

(* Temporary files *)

let tmp ?dir suff =
  try
    let f = Filename.temp_file ?temp_dir:dir "bos" suff in
    let f = match Bos_path.of_string f with
    | None -> assert false
    | Some p -> p
    in
    at_exit (fun () -> ignore (delete f));
    Ok f
  with Sys_error e -> R.error_msg e

let with_tmp ?dir suff f v =
  try
    let p, oc = Filename.open_temp_file ?temp_dir:dir "bos" suff in
    let delete_close oc = (try Unix.unlink p with _ -> ()); close_out oc in
    at_exit (fun () -> try Unix.unlink p with _ -> ()); (* TODO *)
    Bos_base.apply (f p oc) v ~finally:delete_close oc
  with Sys_error e -> R.error_msg e

(* Output *)

let lo_with_outf file f v = (* not atomic *)
  try
    let oc = if is_dash file then stdout else open_out_bin file in
    let close oc = if is_dash file then () else close_out oc in
    Bos_base.apply (f oc) v ~finally:close oc
  with
  | Sys_error e -> R.error_msg e

let with_outf file f v =
  if is_dash file then lo_with_outf file f v else
  let do_write tmp tmp_oc v = match f tmp_oc v with
  | Error _ as e -> e
  | Ok _ as r ->
      try Unix.rename tmp file; r with
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "rename %a to %a: %s"
            Bos_path.pp tmp Bos_path.pp file (Unix.error_message e)
  in
  with_tmp ~dir:(Bos_path.parent file) "write" do_write v

let write file contents =
  let write oc contents = output_string oc contents; Ok () in
  with_outf file write contents

let writef file fmt = (* FIXME avoid the kstrf. *)
  Fmt.kstrf (fun content -> write file content) fmt

let write_lines file lines =
  let rec write oc = function
  | [] -> Ok ()
  | l :: ls ->
      output_string oc l;
      if ls <> [] then (output_char oc '\n'; write oc ls) else Ok ()
  in
  with_outf file write lines

let write_subst vars file contents =
  let write_subst oc contents =                     (* man that's ugly. *)
    let s = contents in
    let start = ref 0 in
    let last = ref 0 in
    let len = String.length s in
    while (!last < len - 4) do
      if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
      begin
        let start_subst = !last in
        let last_id = ref (!last + 2) in
        let stop = ref false in
        while (!last_id < len - 1 && not !stop) do
          if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
            if s.[!last_id] <> ' '
            then (incr last_id)
            else (stop := true; last := !last_id)
          end else begin
            let id_start = start_subst + 2 in
            let len = !last_id - id_start in
            let id = String.with_pos_len ~start:id_start ~len s in
            try
              let subst = List.assoc id vars in
              Pervasives.output_substring oc s !start (start_subst - !start);
              output_string oc subst;
              stop := true;
              start := !last_id + 2;
              last := !last_id + 2;
            with Not_found ->
              stop := true;
              last := !last_id
          end
        done
      end
    done;
    Pervasives.output_substring oc s !start (len - !start); R.ok ()
  in
  with_outf file write_subst contents

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
