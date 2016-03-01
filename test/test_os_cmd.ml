(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring
open Rresult
open Bos

let eqb = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_bool
let eqs = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_str
let equ = eq_result_msg ~eq_ok:(=) ~pp_ok:pp_unit
let eql = eq_result_msg ~eq_ok:(=) ~pp_ok:(pp_list pp_str)

let win32 = Sys.os_type = "win32"

let cat = if win32 then Cmd.(v "type") else Cmd.(v "cat")
let cat_stdin = Cmd.(cat % if win32 then "CON" else "-")
let unlikely = Cmd.v "6AC0E501-4E30-4CBC-AD03-F880F885BC18"

let exists = test "OS.Cmd.exists" @@ fun () ->
  eqb (OS.Cmd.exists cat) (Ok true);
  eqb (OS.Cmd.exists unlikely) (Ok false);
  ()

let must_exist = test "OS.Cmd.must_exist" @@ fun () ->
  begin match (OS.Cmd.must_exist cat) with
  | Error (`Msg err) -> fail "%s" err
  | Ok _ -> ()
  end;
  begin match (OS.Cmd.must_exist unlikely) with
  | Ok _ -> fail "%a exists" Cmd.dump unlikely
  | Error _ -> ()
  end;
  ()

let run_io = test "OS.Cmd.run_io" @@ fun () ->
  let in_hey = OS.Cmd.in_string "hey" in
  let tmp () = OS.File.tmp "bos_test_%s" in
  eqs OS.Cmd.(in_hey |> run_io cat_stdin |> to_string) (Ok "hey");
  eql OS.Cmd.(in_string "hey\nho\n" |> run_io cat_stdin |> to_lines)
    (Ok ["hey";"ho"]);
  equ OS.Cmd.(in_hey |> run_io cat_stdin |> to_null) (Ok ());
  eqs (tmp ()
       >>= fun tmp -> OS.Cmd.(in_hey |> run_io cat_stdin |> to_file tmp)
       >>= fun () -> OS.Cmd.(in_hey |> run_io cat |> to_file tmp ~append:true)
       >>= fun () -> OS.Cmd.(in_file tmp |> run_io Cmd.(cat_stdin % p tmp) |>
                             to_string))
    (Ok "heyheyheyhey");
  eqs (tmp ()
       >>= fun tmp1 -> tmp()
       >>= fun tmp2 -> OS.Cmd.(in_hey |> run_io cat_stdin |> to_file tmp1)
       >>= fun () -> OS.Cmd.(in_file tmp1 |> run_io cat_stdin |> out_run_in)
       >>= fun pipe ->  OS.Cmd.(pipe |> run_io cat_stdin |> to_file tmp2)
       >>= fun () -> OS.Cmd.(in_file tmp2 |> run_io cat_stdin |> to_string))
  (Ok "hey");
  ()

let suite = suite "OS command run functions"
    [ exists;
      run_io; ]

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
