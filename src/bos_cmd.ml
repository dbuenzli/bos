(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Command line fragments *)

type t = string list

let empty = []
let is_empty = function [] -> true | _ -> false

let v a = [a]
let ( % ) l a = a :: l
let ( %% ) l0 l1 = List.rev_append (List.rev l1) l0

let add_arg l a = l % a
let add_args l a = l %% a

let on bool l = if bool then l else []

let p = Fpath.to_string

(* Predicates and comparison *)

let equal l l' = l = l'
let compare l l' = Pervasives.compare l l'

(* Conversions and pretty printing *)

let to_list line = List.rev line
let of_list ?slip line = match slip with
| None -> List.rev line
| Some slip -> List.fold_left (fun acc v -> v :: slip :: acc) [] line

let pp ppf cmd = match List.rev cmd with
| [] -> ()
| cmd :: [] -> Fmt.(pf ppf "%s" cmd)
| cmd :: args -> Fmt.(pf ppf "@[<2>%s@ %a@]" cmd (list ~sep:sp string) args)

let dump ppf cmd = Fmt.Dump.(list String.dump) ppf (List.rev cmd)

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