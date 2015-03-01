(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let pp ppf fmt = Format.fprintf ppf fmt
let rpp fmt ppf = Format.fprintf ppf fmt
let nop fmt ppf = ()
let pp_cut = Format.pp_print_cut
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_bool = Format.pp_print_bool
let pp_int = Format.pp_print_int
let pp_larrow ppf () = pp_str ppf "<=="
let pp_rarrow ppf () = pp_str ppf "==>"
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ppf = function
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)
| [] -> ()

let pp_white_str ~spaces ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pp_text = pp_white_str ~spaces:true
let pp_lines = pp_white_str ~spaces:false
let pp_range ppf ((l0, c0), (l1, c1)) = pp ppf "%d.%d-%d.%d" l0 c0 l1 c1
let pp_doomed ppf reason =
  pp ppf "Something@ unreasonable@ is@ going@ on (%a).@ You@ are@ doomed."
    pp_text reason

(* Conditional UTF-8 formatting *)

let utf_8_enabled, set_utf_8_enabled =
  let enabled = ref false in
  (fun () -> !enabled), (fun b -> enabled := b)

let pp_if_utf_8 pp_u pp ppf v = (if utf_8_enabled () then pp_u else pp) ppf v

(* Styled formatting *)

type style_tags = [ `Ansi | `None ]
type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White | `None ]

let (style_tags : unit -> style_tags),
    (set_style_tags : style_tags -> unit) =
  let style_tags = ref `None in
  (fun () -> !style_tags), (fun s -> style_tags := s)

let ansi_style_code = function
| `Bold -> "\027[01m"
| `Underline -> "\027[04m"
| `Black -> "\027[30m"
| `Red -> "\027[31m"
| `Green -> "\027[32m"
| `Yellow -> "\027[33m"
| `Blue -> "\027[1;34m"
| `Magenta -> "\027[35m"
| `Cyan -> "\027[36m"
| `White -> "\027[37m"
| `None -> "\027[m"

let ansi_style_reset = "\027[m"

let pp_styled style pp_v ppf = match style_tags () with
| `None -> pp_v ppf
| `Ansi ->
    Format.kfprintf
      (fun ppf -> pp ppf "@<0>%s" ansi_style_reset) ppf "@<0>%s%a"
      (ansi_style_code style) pp_v

let pp_styled_str style = pp_styled style pp_str

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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
