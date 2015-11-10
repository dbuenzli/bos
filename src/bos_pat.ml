(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring

(* Errors *)

let err_malformed_pat s = strf "malformed named string pattern: `%s`" s
let err_undef_var s = strf "variable `%s` undefined in environment" s

(* Misc. *)

let get_cleared_buf = function
| None -> Buffer.create 255
| Some buf -> Buffer.clear buf; buf

(* Patterns *)

type lexeme = Lit of string | Var of string
type t = lexeme list

let dom p =
  let rec loop acc = function
  | Lit _ :: p -> loop acc p
  | Var v :: p -> loop (String.Set.add v acc) p
  | [] -> acc
  in
  loop String.Set.empty p

let subst p subst =
  let rec loop acc = function
  | Lit _ as l :: p -> loop (l :: acc) p
  | Var v as var :: p ->
      begin match subst v with
      | None -> loop (var :: acc) p
      | Some l -> loop (Lit l :: acc) p
      end
  | [] -> List.rev acc
  in
  loop [] p

let equal p p' = p = p'
let compare p p' = Pervasives.compare p p'

type parse_state = S_lit | S_dollar | S_var

let of_string ?buf s =
  let b = get_cleared_buf buf in
  let flush b = let s = Buffer.contents b in (Buffer.clear b; s) in
  let push_lit b acc =
    if Buffer.length b <> 0 then Lit (flush b) :: acc else acc
  in
  let max_i = String.length s - 1 in
  let rec loop acc state i =
    if i > max_i then
      if state <> S_lit then R.error_msg (err_malformed_pat s) else
      (Ok (List.rev (push_lit b acc)))
    else match state with
    | S_lit ->
        begin match s.[i] with
        | '$' -> loop acc S_dollar (i + 1)
        | c -> Buffer.add_char b c; loop acc S_lit (i + 1)
        end
    | S_dollar ->
        begin match s.[i] with
        | '$' -> Buffer.add_char b '$'; loop acc S_lit (i + 1)
        | '(' -> loop (push_lit b acc) S_var (i + 1)
        | _ -> R.error_msg (err_malformed_pat s)
        end
    | S_var ->
        begin match s.[i] with
        | ')' -> loop (Var (flush b) :: acc) S_lit (i + 1);
        | ',' -> R.error_msg (err_malformed_pat s)
        | c -> Buffer.add_char b c; loop acc S_var (i + 1)
        end
  in
  loop [] S_lit 0

let v s = R.error_msg_to_invalid_arg (of_string s)

let to_string ?buf p =
  let b = get_cleared_buf buf in
  let add = function
  | Lit l ->
      let max_i = String.length l - 1 in
      let rec loop start i =
        if i > max_i then Buffer.add_substring b l start (i - start) else
        if l.[i] <> '$' then loop start (i + 1) else
        begin
          Buffer.add_substring b l start (i - start + 1);
          Buffer.add_char b '$';
          let next = i + 1 in loop next next
        end
      in
      loop 0 0
  | Var v -> Buffer.(add_string b "$("; add_string b v; add_char b ')')
  in
  List.iter add p;
  Buffer.contents b

let escape_dollar s =
  let len = String.length s in
  let max_idx = len - 1 in
  let rec escaped_len i l =
    if i > max_idx then l else
    match String.unsafe_get s i with
    | '$' -> escaped_len (i + 1) (l + 2)
    | _ -> escaped_len (i + 1) (l + 1)
  in
  let escaped_len = escaped_len 0 0 in
  if escaped_len = len then s else
  let b = Bytes.create escaped_len in
  let rec loop i k =
    if i > max_idx then Bytes.unsafe_to_string b else
    match String.unsafe_get s i with
    | '$' ->
        Bytes.unsafe_set b k '$'; Bytes.unsafe_set b (k + 1) '$';
        loop (i + 1) (k + 2)
    | c ->
        Bytes.unsafe_set b k c;
        loop (i + 1) (k + 1)
  in
  loop 0 0

let rec pp ppf = function
| [] -> ()
| Lit l :: p -> Fmt.string ppf (escape_dollar l); pp ppf p
| Var v :: p -> Fmt.pf ppf "$(%s)" v; pp ppf p

let dump ppf p =
  let rec dump ppf = function
  | [] -> ()
  | Lit l :: p ->
      Fmt.string ppf (String.Ascii.escape_string (escape_dollar l)); pp ppf p
  | Var v :: p ->
      Fmt.pf ppf "$(%s)" v; pp ppf p
  in
  Fmt.pf ppf "\"%a\"" dump p

(* Pattern environments *)

type env = string String.map

let subst_env p env =
  let rec loop acc = function
  | Lit _ as l :: p -> loop (l :: acc) p
  | Var v as var :: p ->
      begin match String.Map.find v env with
      | None -> loop (var :: acc) p
      | Some l -> loop (Lit l :: acc) p
      end
  | [] -> List.rev acc
  in
  loop [] p

let format ?buf ?undef p env =
  let undef = match undef with
  | None -> fun v -> invalid_arg (err_undef_var v)
  | Some undef -> undef
  in
  let b = get_cleared_buf buf in
  let add = function
  | Lit l -> Buffer.add_string b l
  | Var v ->
      match String.Map.find v env with
      | None -> Buffer.add_string b (undef v)
      | Some s -> Buffer.add_string b s
  in
  List.iter add p;
  Buffer.contents b

(* Matching
   N.B. matching is not t.r. but stack is bounded by number of variables. *)

let match_literal pos s lit =              (* matches [lit] at [pos] in [s]. *)
  let l_len = String.length lit in
  let s_len = String.length s - pos in
  if l_len > s_len then None else
  try
    for i = 0 to l_len - 1 do if lit.[i] <> s.[pos + i] then raise Exit done;
    Some (pos + l_len)
  with Exit -> None

let match_pat ~env pos s pat =
  let init, no_env = match env with
  | None -> Some String.Map.empty, true
  | Some m as init -> init, false
  in
  let rec loop pos = function
  | [] -> if pos = String.length s then init else None
  | Lit lit :: p ->
      begin match (match_literal pos s lit) with
      | None -> None
      | Some pos -> loop pos p
      end
  | Var n :: p ->
      let rec try_match next_pos =
        if next_pos < pos then None else
        match loop next_pos p with
        | None -> try_match (next_pos - 1)
        | Some m as r ->
            if no_env then r else
            Some (String.Map.add n
                    (String.with_pos_range s ~start:pos ~stop:next_pos) m)
      in
      try_match (String.length s) (* Longest match first. *)
  in
  loop pos pat

let matches p s = (match_pat ~env:None 0 s p) <> None
let query ?(init = String.Map.empty) p s = match_pat ~env:(Some init) 0 s p

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
