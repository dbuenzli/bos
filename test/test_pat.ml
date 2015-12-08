(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Astring
open Bos

let eqp = eq ~eq:Pat.equal ~pp:Pat.pp
let v = Fpath.v

let string_conv = test "Pat.{v,of_string,to_string}" @@ fun () ->
  let trip p = eq_str p Pat.(to_string (v p)) in
  app_invalid ~pp:Pat.pp Pat.v "$(";
  app_invalid ~pp:Pat.pp Pat.v "$(a";
  app_invalid ~pp:Pat.pp Pat.v "$$$(";
  app_invalid ~pp:Pat.pp Pat.v "$$$";
  app_invalid ~pp:Pat.pp Pat.v "$(bla,)";
  app_invalid ~pp:Pat.pp Pat.v "$(b,la)";
  trip "Hey $(ho)";
  trip "Hey $(ho) $(hu)";
  trip "Hey $(ho) $(h$u)";
  trip "Hey mo $$(hu)";
  trip "Hey mo $$30";
  trip "Hey mo $$$$";
  ()

let dom = test "Pat.dom" @@ fun () ->
  let eq s l =
    eq ~eq:String.Set.equal ~pp:String.Set.dump
      (Pat.(dom @@ v s)) (String.Set.of_list l)
  in
  eq "bla" [];
  eq "bla ha $$" [];
  eq "hey $(bla)" ["bla"];
  eq "hey $(bla) $()" ["bla"; ""];
  eq "hey $(bla) $$(ha) $()" ["bla"; ""];
  eq "hey $(bla) $(bli) $()" ["bla"; "bli"; ""];
  ()

let subst = test "Pat.subst" @@ fun () ->
  let eq p f s = eq_str Pat.(to_string @@ subst (v p) f) s in
  let subst = function "bli" -> Some "bla" | _ -> None in
  eq "hey $(bli) $(bla)" subst "hey bla $(bla)";
  eq "hey $(blo) $(bla)" subst "hey $(blo) $(bla)";
  ()

let subst_env = test "Pat.subst_env" @@ fun () ->
  let eq p e s = eq_str Pat.(to_string @@ subst_env (v p) e) s in
  let env = String.Map.of_list ["bli", "bla"] in
  eq "hey $(bli) $(bla)" env "hey bla $(bla)";
  eq "hey $(blo) $(bla)" env "hey $(blo) $(bla)";
  ()

let format = test "Pat.format" @@ fun () ->
  let eq p env s = eq_str (Pat.(format (v p) env)) s in
  let env = String.Map.of_list ["hey", "ho"; "hi", "ha"] in
  app_invalid ~pp:Fmt.(unit "()") (eq "a $(hu)" env) "bla";
  eq_str Pat.(format ~undef:(fun _ -> "undef") (v "a $(hu)") env) "a undef";
  eq "a $(hey) $(hi)" env "a ho ha";
  eq "a $$(hey) $$(hi)" env "a $(hey) $(hi)";
  ()

let matches = test "Pat.matches" @@ fun () ->
  let m p s = Pat.(matches (v p) s) in
  eq_bool (m "$(mod).mli" "string.mli") true;
  eq_bool (m "$(mod).mli" "string.mli ") false;
  eq_bool (m "$(mod).mli" ".mli") true;
  eq_bool (m "$(mod).mli" ".mli ") false;
  eq_bool (m "$(mod).$(suff)" "string.mli") true;
  eq_bool (m "$(mod).$(suff)" "string.mli ") true;
  eq_bool (m "$()aaa" "aaa") true;
  eq_bool (m "aaa$()" "aaa") true;
  eq_bool (m "$()a$()aa$()" "aaa") true;
  ()

let query = test "Pat.query" @@ fun () ->
  let u ?init p s = Pat.(query ?init (v p) s) in
  let eq = eq_option
      ~eq:(String.Map.equal String.equal) ~pp:(String.Map.dump String.dump)
  in
  let eq ?init p s = function
  | None -> eq (u ?init p s) None
  | Some l -> eq (u ?init p s) (Some (String.Map.of_list l))
  in
  let init = String.Map.of_list ["hey", "ho"] in
  eq "$(mod).mli" "string.mli" (Some ["mod", "string"]);
  eq ~init "$(mod).mli" "string.mli" (Some ["mod", "string"; "hey", "ho"]);
  eq "$(mod).mli" "string.mli " None;
  eq ~init "$(mod).mli" "string.mli " None;
  eq "$(mod).mli" "string.mli " None;
  eq "$(mod).$(suff)" "string.mli" (Some ["mod", "string"; "suff", "mli"]);
  eq "$(mod).$(suff)" "string.mli" (Some ["mod", "string"; "suff", "mli"]);
  ()

let suite = suite "Pat module"
    [ string_conv;
      dom;
      subst;
      subst_env;
      format;
      matches;
      query; ]

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
