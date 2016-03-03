(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
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
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)