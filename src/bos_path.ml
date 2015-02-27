(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* FIXME if we want to introduce .. and . we should normalize in the
   constructors to have a path that has for absolute paths, no .. or
   . at all yor for relative paths only possible .. at the beginning of
   the path. The functions should then be reviewed with that normal
   form in mind. *)

open Bos_prelude

let err_no_ext p = str "no file extension in last segment (%s)" p

(* File paths *)

type filename = string
type rel = [ `Rel of string list ]
type abs = [ `Abs of string list ]
type t = [ rel | abs ]

let segs = function `Abs segs | `Rel segs -> segs
let map f = function
| `Rel segs -> `Rel (f segs)
| `Abs segs -> `Abs (f segs)

let root = `Abs []
let empty = `Rel []
let dash = `Rel [ "-" ]

let naked_add seg segs =
  if seg = "" then segs else List.rev (seg :: List.rev segs)

let add p seg = map (naked_add seg) p

let naked_concat (`Rel segs) prefix = List.rev_append (List.rev prefix) segs
let concat prefix rel = map (naked_concat rel) prefix

let ( / ) = add
let ( // ) = concat

let file f = add empty f
let base f = add empty f

let basename p = match List.rev (segs p) with [] -> "" | seg :: _ -> seg

let naked_dirname segs = match List.rev segs with
| [] -> [] | _ :: rsegs -> List.rev rsegs

let dirname p = map naked_dirname p

let rec naked_rem_prefix segs segs' = match segs, segs' with
| s :: ss, s' :: ss' when s = s' -> naked_rem_prefix ss ss'
| s :: ss, _ -> None
| [], ss -> Some (`Rel ss)

let rem_prefix p p' = match p, p' with
| `Rel segs, `Rel segs' -> naked_rem_prefix segs segs'
| `Abs segs, `Abs segs' -> naked_rem_prefix segs segs'
| _ , _ -> None

let rec naked_find_prefix acc segs segs' = match segs, segs' with
| s :: ss, s' :: ss' when s = s' -> naked_find_prefix (s :: acc) ss ss'
| _ -> List.rev acc

let find_prefix p p' = match p, p' with
| `Rel segs, `Rel segs' -> Some (`Rel (naked_find_prefix [] segs segs'))
| `Abs segs, `Abs segs' -> Some (`Rel (naked_find_prefix [] segs segs'))
| _, _ -> None

(*
let relativize p p' =
   (* N.B. this wouldn't work if [s] could be Filename.parent_dir_name *)
  let rec relat segs segs' acc = match segs, segs' with
  | s :: ss, s' :: ss' when s = s' -> relat ss ss' acc
  | s :: ss, ss' -> relat ss ss' (Filename.parent_dir_name :: acc)
  | [], ss -> List.rev_append acc ss
  in
  match p, p' with
  | `Rel segs, `Rel segs' -> Some (`Rel (relat segs segs' []))
  | `Abs segs, `Abs segs' -> Some (`Rel (relat segs segs' []))
  | _ -> None
*)

(* Predicates and comparison *)

let is_root = function `Abs [] -> true | _ -> false
let is_empty = function `Rel [] -> true | _ -> false
let is_dash = function `Rel ["-"] -> true | _ -> false
let is_rel = function `Rel _ -> true | _ -> false
let is_abs = function `Abs _ -> true | _ -> false

let rec naked_is_prefix segs segs' = match segs, segs' with
| s :: ss, s' :: ss' when s = s' -> naked_is_prefix ss ss'
| s :: ss, _ -> false
| [], ss -> true

let is_prefix p p' = match p, p' with
| `Rel segs, `Rel segs' -> naked_is_prefix segs segs'
| `Abs segs, `Abs segs' -> naked_is_prefix segs segs'
| _ , _ -> false

let equal p p' = p = p'
let compare = Pervasives.compare

(* Conversions *)

let to_rel = function `Rel _ as v -> Some v | `Abs _ -> None
let of_rel r = (r :> t)

let to_abs = function `Abs _ as v -> Some v | `Rel _ -> None
let of_abs a = (a :> t)

let to_segs p = p

let rel_of_segs segs = (* This could be improved many revs through add *)
  `Rel (List.fold_left (fun ss s -> naked_add s ss) [] segs)

let abs_of_segs segs = (* This could be improved many revs through add *)
  `Abs (List.fold_left (fun ss s -> naked_add s ss) [] segs)

let of_segs = function
| `Rel segs -> rel_of_segs segs
| `Abs segs -> abs_of_segs segs

(* FIXME `{to,of}_string,quote` are we doing the right things ?  *)

let to_string = function
| `Rel segs -> String.concat Filename.dir_sep segs
(* FIXME windows what's the root ? *)
| `Abs segs -> (Filename.dir_sep ^ String.concat Filename.dir_sep segs)

let of_string s =                                (* N.B. collapses // to / *)
  (* FIXME unquote ? *)
  match String.split ~sep:Filename.dir_sep s with
  | "" :: segs -> of_segs (`Abs segs)   (* FIXME windows ?? *)
  | segs -> of_segs (`Rel segs)

let quote p = Filename.quote (to_string p)
let pp ppf p = Format.pp_print_string ppf (to_string p)

(* File extensions *)

type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]

let ext_to_string = function
| `A -> "a" | `Byte -> "byte" | `C -> "c" | `Cma -> "cma" | `Cmi -> "cmi"
| `Cmo -> "cmo" | `Cmt -> "cmt" | `Cmti -> "cmti" | `Cmx -> "cmx"
| `Cmxa -> "cmxa" | `Cmxs -> "cmxs" | `Css -> "css" | `Dll -> "dll"
| `Exe -> "exe" | `Gif -> "gif" | `H -> "h" | `Html -> "html"
| `Install -> "install" | `Img -> "img" | `Jpeg -> "jpeg" | `Js -> "js"
| `Json -> "json" | `Lib -> "lib" | `Md -> "md" | `Ml -> "ml"
| `Ml_dep -> "ml-dep" | `Ml_pp -> "ml-pp" | `Mli -> "mli"
| `Mli_dep -> "mli-dep" | `Mli_pp -> "mli-pp" | `Native -> "native"
| `O -> "o" | `Opt -> "opt" | `Png -> "png" | `Sh -> "sh" | `So -> "so"
| `Tar -> "tar" | `Tbz -> "tbz" | `Xml -> "xml" | `Zip -> "zip"
| `Ext ext -> ext

let ext_of_string = function
| "a" -> `A | "byte" -> `Byte | "c" -> `C | "cma" -> `Cma | "cmi" -> `Cmi
| "cmo" -> `Cmo | "cmt" -> `Cmt | "cmti" -> `Cmti | "cmx" -> `Cmx
| "cmxa" -> `Cmxa | "cmxs" -> `Cmxs | "css" -> `Css | "dll" -> `Dll
| "exe" -> `Exe | "gif" -> `Gif | "h" -> `H | "html" -> `Html
| "install" -> `Install | "img" -> `Img | "jpeg" -> `Jpeg | "js" -> `Js
| "json" -> `Json | "lib" -> `Lib | "md" -> `Md | "ml" -> `Ml
| "ml-dep" -> `Ml_dep | "ml-pp" -> `Ml_pp | "mli" -> `Mli
| "mli-dep" -> `Mli_dep | "mli-pp" -> `Mli_pp | "native" -> `Native
| "o" -> `O | "opt" -> `Opt | "png" -> `Png | "sh" -> `Sh | "so" -> `So
| "tar" -> `Tar | "tbz"  -> `Tbz | "xml" -> `Xml | "zip" -> `Zip
| ext -> `Ext ext

let pp_ext ppf e = Format.pp_print_string ppf (ext_to_string e)

let ext p = match List.rev (segs p) with
| [] -> None
| seg :: _ ->
    try
      let i = String.rindex seg '.' in
      let ext = String.sub seg (i + 1) (String.length seg - i - 1) in
      Some (ext_of_string ext)
    with Not_found -> None

let get_ext p = match ext p with
| Some ext -> ext
| None -> invalid_arg (err_no_ext (to_string p))

let naked_add_ext ext segs =
  let suff = ext_to_string ext in
  match List.rev segs with
  | [] -> [str ".%s" suff]
  | seg :: rsegs -> List.rev (str "%s.%s" seg suff :: rsegs)

let add_ext p ext = map (naked_add_ext ext) p

let naked_rem_ext segs = match List.rev segs with
| [] -> []
| seg :: segs' ->
    try
      let i = String.rindex seg '.' in
      let name = String.sub seg 0 i in
      List.rev (name :: segs')
    with Not_found -> segs

let rem_ext p = map naked_rem_ext p

let ( + ) = add_ext

let change_ext p e = add_ext (rem_ext p) e

let has_ext e p = match ext p with None -> false | Some e' -> e = e'
let ext_matches exts p = match ext p with
| None -> false
| Some e -> List.mem e exts

module Rel = struct
  type path = t
  type t = rel

  let empty = empty
  let dash = dash
  let add (`Rel segs) seg = `Rel (naked_add seg segs)
  let concat (`Rel segs) rel = `Rel (naked_concat rel segs)
  let file f = add empty f
  let base f = add empty f
  let ( / ) = add
  let ( // ) = concat
  let basename = basename
  let dirname (`Rel segs) = `Rel (naked_dirname segs)
  let rem_prefix (`Rel ss) (`Rel ss') = naked_rem_prefix ss ss'
  let find_prefix (`Rel ss) (`Rel ss') = (`Rel (naked_find_prefix [] ss ss'))
  (* Predicates and comparisons *)

  let is_empty = is_empty
  let is_dash = is_dash
  let is_prefix (`Rel segs) (`Rel segs') = naked_is_prefix segs segs'
  let equal = equal
  let compare = compare

  (* Conversions *)

  let to_segs (`Rel segs) = segs
  let of_segs segs = rel_of_segs segs
  let to_string = to_string
  let quote = quote
  let pp = pp

  (* File extensions *)

  let ext = ext
  let get_ext = get_ext
  let add_ext (`Rel segs) ext = `Rel (naked_add_ext ext segs)
  let rem_ext (`Rel segs) = `Rel (naked_rem_ext segs)
  let change_ext p ext = add_ext (rem_ext p) ext
  let ( + ) = add_ext
  let has_ext = has_ext
  let ext_matches = ext_matches

  (* Sets and maps *)

  module Path = struct
    type path = t
    type t = path
    let compare = compare
  end

  module Set = struct
    include Set.Make (Path)
    let of_list = List.fold_left (fun acc s -> add s acc) empty
  end

  module Map = struct
    include Map.Make (Path)
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  end
end

module Abs = struct
  type path = t
  type t = abs

  let root = root
  let add (`Abs segs) seg = `Abs (naked_add seg segs)
  let concat (`Abs segs) rel = `Abs (naked_concat rel segs)
  let ( / ) = add
  let ( // ) = concat
  let basename = basename
  let dirname (`Abs segs) = `Abs (naked_dirname segs)
  let rem_prefix (`Abs ss) (`Abs ss') = naked_rem_prefix ss ss'
  let find_prefix (`Abs ss) (`Abs ss') = (`Abs (naked_find_prefix [] ss ss'))

  (* Predicates and comparisons *)

  let is_root = is_root
  let is_prefix (`Abs segs) (`Abs segs') = naked_is_prefix segs segs'
  let equal = equal
  let compare = compare

  (* Conversions *)

  let to_segs (`Abs segs) = segs
  let of_segs segs = abs_of_segs segs
  let to_string = to_string
  let quote = quote
  let pp = pp

  (* File extensions *)

  let ext = ext
  let get_ext = get_ext
  let add_ext (`Abs segs) ext = `Abs (naked_add_ext ext segs)
  let rem_ext (`Abs segs) = `Abs (naked_rem_ext segs)
  let change_ext p ext = add_ext (rem_ext p) ext
  let ( + ) = add_ext
  let has_ext = has_ext
  let ext_matches = ext_matches

  (* Sets and maps *)

  module Path = struct
    type path = t
    type t = path
    let compare = compare
  end

  module Set = struct
    include Set.Make (Path)
    let of_list = List.fold_left (fun acc s -> add s acc) empty
  end

  module Map = struct
    include Map.Make (Path)
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  end
end

(** {1 Sets and maps} *)

module Path = struct
  type path = t
  type t = path
  let compare = compare
end

module Set = struct
  include Set.Make (Path)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = struct
  include Map.Make (Path)
  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
end

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
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
