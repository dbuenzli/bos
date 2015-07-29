(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring

let get_buf ?buf () = match buf with
| None -> Buffer.create 255
| Some buf -> Buffer.clear buf; buf

(* Patterns *)

type t = [ `Lit of string | `Var of string ] list
type env = string String.map

let of_string ?buf s =
  try
    let b = get_buf ?buf () in
    let acc = ref [] in
    let flush b = let s = Buffer.contents b in (Buffer.clear b; s) in
    let flush_lit b =
      if Buffer.length b <> 0 then acc := `Lit (flush b) :: !acc
    in
    let state = ref `Lit in
    for i = 0 to String.length s - 1 do match !state with
    | `Lit ->
        begin match s.[i] with
        | '$' -> state := `Dollar
        | c -> Buffer.add_char b c
        end
    | `Dollar ->
        begin match s.[i] with
        | '$' -> state := `Lit; Buffer.add_char b '$'
        | '(' -> state := `Var; flush_lit b;
        | _ -> raise Exit
        end
    | `Var ->
        begin match s.[i] with
        | ')' -> state := `Lit; acc := (`Var (flush b)) :: !acc;
        | c -> Buffer.add_char b c
        end
    done;
    if !state <> `Lit then raise Exit else
    (flush_lit b; Rresult.R.ok (List.rev !acc))
  with Exit -> Rresult.R.error_msgf "malformed named string pattern: `%s`" s

let v s = Rresult.R.error_msg_to_invalid_arg (of_string s)

let to_string ?buf p =
  let b = get_buf ?buf () in
  let add = function
  | `Lit l -> Buffer.add_string b l
  | `Var v -> Buffer.(add_string b "$("; add_string b v; add_char b ')')
  in
  List.iter add p;
  Buffer.contents b

let dom p =
  let rec loop acc = function
  | `Lit _ :: p -> loop acc p
  | `Var v :: p -> loop (String.Set.add v acc) p
  | [] -> acc
  in
  loop String.Set.empty p

let rec pp ppf = function
| [] -> ()
| `Lit l :: p -> Format.pp_print_string ppf l; pp ppf p
| `Var v :: p -> Format.fprintf ppf "$(%s)" v; pp ppf p

let equal p p' = p = p'
let compare p p' = Pervasives.compare p p'

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
  | `Lit lit :: p ->
      begin match (match_literal pos s lit) with
      | None -> None
      | Some pos -> loop pos p
      end
  | `Var n :: p ->
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
let unify ?(init = String.Map.empty) p s = match_pat ~env:(Some init) 0 s p

(* Formatting *)

let err_var s = Format.asprintf "variable `%s` undefined in environment" s

let format ?buf p env =
  let b = get_buf ?buf () in
  let add = function
  | `Lit l -> Buffer.add_string b l
  | `Var v ->
      match String.Map.find v env with
      | None -> invalid_arg (err_var v)
      | Some s -> Buffer.add_string b s
  in
  List.iter add p;
  Buffer.contents b

let rec pp_format p ppf env = match p with
| [] -> ()
| `Lit l :: p -> Format.pp_print_string ppf l; pp_format p ppf env
| `Var v :: p ->
    match String.Map.find v env with
    | None -> invalid_arg (err_var v)
    | Some s -> Format.pp_print_string ppf s; pp_format p ppf env

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
