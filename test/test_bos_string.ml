(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos

(* Tests some of Bos.String's additions. *)

let test_split () =
  Log.show "Testing String.split";
  assert (try ignore (String.split "" ""); false
          with Invalid_argument _ -> true);
  assert (try ignore (String.split "" "123"); false
          with Invalid_argument _ -> true);
  assert (String.split "," "" = [""]);
                       (* Str = [] *)
  assert (String.split "," "," = [""; ""]);
  assert (String.split "," ",," = [""; ""; ""]);
  assert (String.split "," ",,," = [""; ""; ""; ""]);
  assert (String.split "," "123" = ["123"]);
  assert (String.split "," ",123" = [""; "123"]);
  assert (String.split "," "123," = ["123"; ""]);
  assert (String.split "," "1,2,3" = ["1"; "2"; "3"]);
  assert (String.split "," "1, 2, 3" = ["1"; " 2"; " 3"]);
  assert (String.split "," ",1,2,,3," = [""; "1"; "2"; ""; "3"; ""]);
  assert (String.split "," ", 1, 2,, 3," = [""; " 1"; " 2"; ""; " 3"; ""]);
  assert (String.split "<>" "" = [""]);
                        (* Str = [] *)
  assert (String.split "<>" "<>" = [""; ""]);
  assert (String.split "<>" "<><>" = [""; ""; ""]);
  assert (String.split "<>" "<><><>" = [""; ""; ""; ""]);
  assert (String.split "<>" "123" = [ "123" ]);
  assert (String.split "<>" "<>123" = [""; "123"]);
  assert (String.split "<>" "123<>" = ["123"; ""]);
  assert (String.split "<>" "1<>2<>3" = ["1"; "2"; "3"]);
  assert (String.split "<>" "1<> 2<> 3" = ["1"; " 2"; " 3"]);
  assert (String.split "<>" "<>1<>2<><>3<>" = [""; "1"; "2"; ""; "3"; ""]);
  assert (String.split "<>" "<> 1<> 2<><> 3<>" = [""; " 1"; " 2"; ""; " 3";""]);
  assert (String.split "<>" ">>><>>>><>>>><>>>>" =
          [">>>"; ">>>"; ">>>"; ">>>" ]);
  assert (String.split "<->" "<->>->" = [""; ">->"]);
  assert (String.split "aa" "aa" = [""; ""]);
  assert (String.split "aa" "aaa" = [""; "a"]);
  assert (String.split "aa" "aaaa" = [""; ""; ""]);
  assert (String.split "aa" "aaaaa" = [""; ""; "a"]);
  assert (String.split "aa" "aaaaaa" = [""; ""; ""; ""]);
  ()

let test_rsplit () =
  Log.show "Testing String.rsplit";
  (* String.rsplit *)
  assert (try ignore (String.rsplit "" ""); false with
          Invalid_argument _ -> true);
  assert (try ignore (String.rsplit "" "123"); false with
          Invalid_argument _ -> true);
  assert (String.rsplit "," "" = [""]);
                       (* Str = [] *)
  assert (String.rsplit "," "," = [""; ""]);
  assert (String.rsplit "," ",," = [""; ""; ""]);
  assert (String.rsplit "," ",,," = [""; ""; ""; ""]);
  assert (String.rsplit "," "123" = ["123"]);
  assert (String.rsplit "," ",123" = [""; "123"]);
  assert (String.rsplit "," "123," = ["123"; ""]);
  assert (String.rsplit "," "1,2,3" = ["1"; "2"; "3"]);
  assert (String.rsplit "," "1, 2, 3" = ["1"; " 2"; " 3"]);
  assert (String.rsplit "," ",1,2,,3," = [""; "1"; "2"; ""; "3"; ""]);
  assert (String.rsplit "," ", 1, 2,, 3," = [""; " 1"; " 2"; ""; " 3"; ""]);
  assert (String.rsplit "<>" "" = [""]);
                        (* Str = [] *)
  assert (String.rsplit "<>" "<>" = [""; ""]);
  assert (String.rsplit "<>" "<><>" = [""; ""; ""]);
  assert (String.rsplit "<>" "<><><>" = [""; ""; ""; ""]);
  assert (String.rsplit "<>" "123" = [ "123" ]);
  assert (String.rsplit "<>" "<>123" = [""; "123"]);
  assert (String.rsplit "<>" "123<>" = ["123"; ""]);
  assert (String.rsplit "<>" "1<>2<>3" = ["1"; "2"; "3"]);
  assert (String.rsplit "<>" "1<> 2<> 3" = ["1"; " 2"; " 3"]);
  assert (String.rsplit "<>" "<>1<>2<><>3<>" = [""; "1"; "2"; ""; "3"; ""]);
  assert (String.rsplit "<>" "<> 1<> 2<><> 3<>" =
          [""; " 1"; " 2"; ""; " 3";""]);
  assert (String.rsplit "<>" ">>><>>>><>>>><>>>>" =
          [">>>"; ">>>"; ">>>"; ">>>" ]);
  assert (String.rsplit "<->" "<->>->" = [""; ">->"]);
  assert (String.rsplit "aa" "aa" = [""; ""]);
  assert (String.rsplit "aa" "aaa" = ["a"; ""]);
  assert (String.rsplit "aa" "aaaa" = [""; ""; ""]);
  assert (String.rsplit "aa" "aaaaa" = ["a"; ""; "";]);
  assert (String.rsplit "aa" "aaaaaa" = [""; ""; ""; ""]);
  ()

let test_cut () =
  Log.show "Testing String.cut";
  assert (try ignore (String.cut "" ""); false with
          Invalid_argument _ -> true);
  assert (try ignore (String.cut "" "123"); false with
          Invalid_argument _ -> true);
  assert (String.cut "," "" = None);
  assert (String.cut "," "," = Some ("", ""));
  assert (String.cut "," ",," = Some ("", ","));
  assert (String.cut "," ",,," = Some ("", ",,"));
  assert (String.cut "," "123" = None);
  assert (String.cut "," ",123" = Some ("", "123"));
  assert (String.cut "," "123," = Some ("123", ""));
  assert (String.cut "," "1,2,3" = Some ("1", "2,3"));
  assert (String.cut "," " 1,2,3" = Some (" 1", "2,3"));
  assert (String.cut "<>" "" = None);
  assert (String.cut "<>" "<>" = Some ("", ""));
  assert (String.cut "<>" "<><>" = Some ("", "<>"));
  assert (String.cut "<>" "<><><>" = Some ("", "<><>"));
  assert (String.rcut "<>" "1" = None);
  assert (String.cut "<>" "123" = None);
  assert (String.cut "<>" "<>123" = Some ("", "123"));
  assert (String.cut "<>" "123<>" = Some ("123", ""));
  assert (String.cut "<>" "1<>2<>3" = Some ("1", "2<>3"));
  assert (String.cut "<>" " 1<>2<>3" = Some (" 1", "2<>3"));
  assert (String.cut "<>" ">>><>>>><>>>><>>>>" =
          Some (">>>", ">>><>>>><>>>>"));
  assert (String.cut "<->" "<->>->" = Some ("", ">->"));
  assert (String.rcut "<->" "<-" = None);
  assert (String.cut "aa" "aa" = Some ("", ""));
  assert (String.cut "aa" "aaa" = Some ("", "a"));
  assert (String.cut "aa" "aaaa" = Some ("", "aa"));
  assert (String.cut "aa" "aaaaa" = Some ("", "aaa";));
  assert (String.cut "aa" "aaaaaa" = Some ("", "aaaa"));
  ()

let test_rcut () =
  Log.show "Testing String.rcut";
  (* String.rcut *)
  assert (try ignore (String.rcut "" ""); false with
          Invalid_argument _ -> true);
  assert (try ignore (String.rcut "" "123"); false with
          Invalid_argument _ -> true);
  assert (String.rcut "," "" = None);
  assert (String.rcut "," "," = Some ("", ""));
  assert (String.rcut "," ",," = Some (",", ""));
  assert (String.rcut "," ",,," = Some (",,", ""));
  assert (String.rcut "," "123" = None);
  assert (String.rcut "," ",123" = Some ("", "123"));
  assert (String.rcut "," "123," = Some ("123", ""));
  assert (String.rcut "," "1,2,3" = Some ("1,2", "3"));
  assert (String.rcut "," "1,2,3 " = Some ("1,2", "3 "));
  assert (String.rcut "<>" "" = None);
  assert (String.rcut "<>" "<>" = Some ("", ""));
  assert (String.rcut "<>" "<><>" = Some ("<>", ""));
  assert (String.rcut "<>" "<><><>" = Some ("<><>", ""));
  assert (String.rcut "<>" "1" = None);
  assert (String.rcut "<>" "123" = None);
  assert (String.rcut "<>" "<>123" = Some ("", "123"));
  assert (String.rcut "<>" "123<>" = Some ("123", ""));
  assert (String.rcut "<>" "1<>2<>3" = Some ("1<>2", "3"));
  assert (String.rcut "<>" "1<>2<>3 " = Some ("1<>2", "3 "));
  assert (String.rcut "<>" ">>><>>>><>>>><>>>>" =
          Some (">>><>>>><>>>>", ">>>"));
  assert (String.rcut "<->" "<->>->" = Some ("", ">->"));
  assert (String.rcut "<->" "<-" = None);
  assert (String.rcut "aa" "aa" = Some ("", ""));
  assert (String.rcut "aa" "aaa" = Some ("a", ""));
  assert (String.rcut "aa" "aaaa" = Some ("aa", ""));
  assert (String.rcut "aa" "aaaaa" = Some ("aaa", "";));
  assert (String.rcut "aa" "aaaaaa" = Some ("aaaa", ""));
  ()

let () =
  Log.show "Testing Bos.String additions";
  test_split ();
  test_rsplit ();
  test_cut ();
  test_rcut ();
  Log.show "All tests passed !";
  ()




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
