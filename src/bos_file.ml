(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let writef file fmt = failwith "TODO"
let must_exist file = failwith "TODO"

let apply f x ~finally y =
  let result = try f x with
  | e -> try finally y; raise e with _ -> raise e
  in
  finally y;
  result

let dash = Bos_path.v "-"

let is_dash = Bos_path.equal dash

let ret_exists ?(err = false) err_msg p b =
  if not err then R.ok b else
  if b then R.ok b else
  err_msg p

(* Files *)

let dev_null =
  if Sys.os_type = "Win32" then Bos_path.v "NUL" else
  Bos_path.v "/dev/null"

let path_str = Bos_path.to_string

let exists ?err file =
  try
    let file = path_str file in
    let err_msg file = R.error_msgf "%s: no such file" file in
    let exists = Sys.file_exists file && not (Sys.is_directory file) in
    ret_exists ?err err_msg file exists
  with
  | Sys_error e -> R.error_msg e

let exists file = exists ~err:false file

let delete ?(must_exist = false) file =
  exists file >>= fun exists ->
  if must_exist && not exists then R.ok () else
  try R.ok (Sys.remove (path_str file)) with
  | Sys_error e -> R.error_msg e

let temp ?dir suff =
  try
    let temp_dir = match dir with
    | None -> None
    | Some d -> Some (Bos_path.to_string d)
    in
    let f = Filename.temp_file ?temp_dir "bos" suff in
    let f = match Bos_path.of_string f with
    | None -> assert false
    | Some p -> p
    in
    at_exit (fun () -> ignore (delete f));
    R.ok f
  with Sys_error e -> R.error_msg e

(* Input *)

let with_inf file f v =
  try
    let ic = if is_dash file then stdin else open_in_bin (path_str file) in
    let close ic = if is_dash file then () else close_in ic in
    apply (f ic) v ~finally:close ic
  with
  | End_of_file -> R.error_msg "unexpected end of file"
  | Sys_error e -> R.error_msg e

let read file =
  let input ic () =
    let len = in_channel_length ic in
    let s = Bytes.create len in
    really_input ic s 0 len; R.ok (Bytes.unsafe_to_string s)
  in
  with_inf file input ()

let read_lines file = read file >>| (String.cuts ~sep:"\n")

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

(* Output *)

let with_outf file f v =
  try
    let oc = if is_dash file then stdout else open_out_bin (path_str file) in
    let close oc = if is_dash file then () else close_out oc in
    apply (f oc) v ~finally:close oc
  with
  | Sys_error e -> R.error_msg e

let write file contents =
  let write oc contents = output_string oc contents; R.ok () in
  if is_dash file then with_outf file write contents else
  temp ~dir:(Bos_path.parent file) "write"
  >>= fun tmpf -> with_outf tmpf write contents
  >>= fun () -> Bos_path_os.move ~force:true tmpf file

let write_lines file lines = write file (String.concat ~sep:"\n" lines)

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
  if is_dash file then with_outf file write_subst contents else
  temp ~dir:(Bos_path.parent file) "write"
  >>= fun tmpf -> with_outf tmpf write_subst contents
  >>= fun () -> Bos_path_os.move ~force:true tmpf file

let rec truncate p size = try Ok (Unix.truncate p size) with
| Unix.Unix_error (Unix.EINTR, _, _) -> truncate p size
| Unix.Unix_error (e, _, _) ->
    R.error_msgf "truncate %a: %s" Bos_path.pp p (Unix.error_message e)

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
