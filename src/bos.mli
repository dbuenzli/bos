(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Light, basic OS interaction.

  Open the module to use it, this defines only one type and modules
  in your scope.

  {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Preliminaries, formatting and logging} *)

open Resultv

(** A few extensions to the standard library.

    Open the module to use it. It defines only modules
    and one value in your scope.

    {b FIXME} maybe this should be externalized  *)
module Prelude : sig

  (** {1 Strings} *)

  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  (** [str] is {!Format.asprintf}. *)

  (** Extended [String] module, string sets and maps. *)
  module String : sig

    (** {1 String} *)

    include module type of String

    (** These are sorely missing from the standard library. *)

    val split : sep:string -> string -> string list
    (** [split sep s] is the list of all (possibly empty) substrings of
        [s] that are delimited by matches of the non empty separator
        string [sep].

        Matching separators in [s] starts from the beginning of [s] and once
        one is found, the separator is skipped and matching starts again
        (i.e. separator matches can't overlap). If there is no separator
        match in [s], [[s]] is returned.

        The invariants [String.concat sep (String.split sep s) = s] and
        [String.split sep s <> []] always hold.

        @raise Invalid_argument if [sep] is the empty string. *)

    val rsplit : sep:string -> string -> string list
    (** [rsplit sep s] is like {!split} but the matching is
        done backwards, starting from the end of [s].

        @raise Invalid_argument if [sep] is the empty string. *)

    val cut : sep:string -> string -> (string * string) option
    (** [cut sep s] is either the pair [Some (l,r)] of the two
        (possibly empty) substrings of [s] that are delimited by the first
        match of the non empty separator string [sep] or [None] if [sep]
        can't be matched in [s]. Matching starts from the beginning of [s].

        The invariant [l ^ sep ^ r = s] holds.

        @raise Invalid_argument if [sep] is the empty string. *)

    val rcut : sep:string -> string -> (string * string) option
    (** [rcut sep s] is like {!cut} but the matching is done backwards
        starting from the end of [s].

        @raise Invalid_argument if [sep] is the empty string. *)

    val slice : ?start:int -> ?stop:int -> string -> string
    (** [slice ~start ~stop s] is the string s.[start], s.[start+1], ...
        s.[stop - 1]. [start] defaults to [0] and [stop] to [String.length s].

        If [start] or [stop] are negative they are subtracted from
        [String.length s]. This means that [-1] denotes the last
        character of the string. *)

    val tokens : string -> string list
    (** [tokens s] is the list of non empty strings made of characters
        that are not separated by [' '], ['\t'], ['\n'], ['\r'] characters in
        [s], the order of character appearance in the list is the same as
        in [s]. *)

    val uniquify : string list -> string list
    (** [uniquify ss] is [ss] without duplicates, the list order is
        preserved. *)

    (** {1 String sets and maps} *)

    module Set : sig
      include Set.S with type elt = string
      val of_list : string list -> t
      (** [of_list ss] is a set from the list [ss]. *)
    end

    val make_unique_in : ?suff:string -> Set.t -> string -> string option
    (** [make_unique_in ~suff set elt] is a string that does not belong
        [set].  If [elt] in not in [set] then this is [elt] itself
        otherwise it is a string defined by [Printf.sprintf "%s%s%d" s
        suff d] where [d] is a positive number starting from [1]. [suff]
        defaults to ["~"].  [None] in the unlikely case that all
        positive numbers were exhausted. *)

    module Map : sig
      include Map.S with type key = string
      val dom : 'a t -> Set.t
      (** [dom m] is the domain of [m]. *)
    end
  end
end

(** {!Format} helpers and shortcuts. *)
module Fmt : sig

  (** {1 Formatters} *)

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for formatters of values of type ['a]. *)

  val pp : Format.formatter -> ('a, Format.formatter, unit) Pervasives.format ->
    'a
  (** [pp] is {!Format.fprintf} *)

  val rpp : ('a, Format.formatter, unit) Pervasives.format ->
    Format.formatter -> 'a
  (** [rpp] is [pp fmt ppf] *)

  val nop : 'a t
  (** [nop] does nothing. *)

  val pp_cut : unit t
  (** [pp_cut] is {!Format.pp_print_cut}. *)

  val pp_sp : unit t
  (** [pp_sp] is {!Format.pp_print_space}. *)

  val pp_str : string t
  (** [pp_str] is {!Format.pp_print_string}. *)

  val pp_int : int t
  (** [pp_int] is {!Format.pp_print_int}. *)

  val pp_bool : bool t
  (** [pp_bool] is {!Format.pp_print_bool}. *)

  val pp_larrow : unit t
  (** [pp_larrow] formats a left arrow. *)

  val pp_rarrow : unit t
  (** [pp_rarrow] formats a right arrow. *)

  val pp_opt : ?pp_none:unit t -> 'a t -> 'a option t
  (** [pp_opt pp_none pp_v] formats value of type ['a option]. The default
      value of [pp_none] prints nothing. *)

  val pp_list : ?pp_sep:unit t -> 'a t -> 'a list t
  (** [pp_list pp_sep pp_v] formats lists of type ['a]. Each value
      is printed with [pp_v] followed by [pp_sep] (defaults to {!pp_cut}).
      Empty lists never print anything. *)

  val pp_text : string t
  (** [pp_text] formats text by replacing spaces and newlines in the string
      with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

  val pp_lines : string t
  (** [pp_lines] formats lines by replacing newlines in the string
      with calls to {!Format.pp_force_newline}. *)

  val pp_range : ((int * int) * (int * int)) t
  (** [pp_range] formats a range. *)

  val pp_doomed : string t
  (** [pp_doomed] should be used for printing a message when reasonable
      assumptions are being violated. The string should be a short
      description of what is going on. *)

  (** {1:utf8_cond Conditional UTF-8 formatting}

      {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
      may derail the pretty printing process. *)

  val pp_if_utf_8 : 'a t -> 'a t -> 'a t
  (** [pp_if_utf_8 pp_u pp] is a t that will use [pp_u] if UTF-8
      output is {{!utf_8_enabled}enabled} and [pp] otherwise. *)

  (** {2:utf8_cond Conditional UTF-8 formatting control} *)

  val utf_8_enabled : unit -> bool
  (** [utf_8_enabled ()] is [true] if UTF-8 pretty-printing is enabled. *)

  val set_utf_8_enabled : bool -> unit
  (** [set_utf_8_enabled b] sets UTF-8 pretty-printing to [b]. *)

  (** {1:styled Styled formatting} *)

  type style =
    [ `Bold
    | `Underline
    | `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `None ]
  (** The type for styles. *)

  val pp_styled : style -> 'a t -> 'a t
  (** [pp_styled style pp] formats according to [pp] but styled with [style]. *)

  val pp_styled_str : style -> string t
  (** [pp_styled_str style] is [pp_styled_str style pp_str]. *)

  (** {2 Styled formatting control} *)

  type style_tags = [ `Ansi | `None ]
  (** The type for style tags.
      {ul
      {- [`Ansi], tags the text with
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
           ANSI escape sequences}.}
      {- [`None], text remains untagged.}} *)

  val style_tags : unit -> style_tags
  (** [style_tags ()] is the current tag style used by {!Fmt.pp_styled}.
      Initial value is [`None]. *)

  val set_style_tags : style_tags -> unit
  (** [set_style_tags s] sets the current tag style used by
      {!Fmt.pp_styled}. *)
end

(** Named string patterns.

    Named string patterns are strings with variables of the form ["$(VAR)"]
    where [VAR] is any sequence of characters except [')'] or [',']. In these
    strings a ["$"] litteral must be written ["$$"].*)
module Pat : sig

  (** {1 Patterns} *)

  type t = [ `Lit of string | `Var of string ] list
  (** The type for patterns. A list of either a string literal or a
      variable. *)

  type env = string Prelude.String.Map.t
  (** Type type for pattern environments. Maps pattern variable names
      to string values. *)

  val v : string -> t
  (** [v s] parses [s] according to the pattern syntax.
      @raise Invalid_argument if [s] is not a valid pattern. Use
      {!of_string} to deal with errors. *)

  val of_string : ?buf:Buffer.t -> string -> (t, R.err_msg) result
  (** [of_string ?buf s] parses [s] according to the pattern syntax.
      [buf] can specify the temporary buffer to use. *)

  val to_string : ?buf:Buffer.t -> t -> string
  (** [to_string p] converts [p] to a string according to the pattern
      syntax. [buf] can specify the temporary buffer to use.  *)

  val dom : t -> Prelude.String.Set.t
  (** [dom p] is the set of variables in [p]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints [p] on [ppf] according ot the pattern syntax. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t -> t -> int
  (** [compare p p'] is {!Pervasives.compare}[ p p']. *)

  (** {1 Matching}

      Pattern variables greedily match from zero to more characters (i.e.
      [.*] in regexp speak). *)

  val matches : t -> string -> bool
  (** [matches p s] is [true] iff [s] matches [p]. *)

  val unify : ?init:env -> t -> string -> env option
  (** [unify ~init p s] is like {!matches} except that a matching
      string returns an environment mapping each pattern variable to
      its matched part in the string (mappings are added to [init],
      defaults to {!Prelude.String.Map.empty}). If a variable appears
      more than once in [pat] the actual mapping for the variable is
      unspecified. *)

  (** {1 Formatting} *)

  val format : ?buf:Buffer.t -> t -> env -> string
  (** [format p env] formats a string according to [p] and the bindings
      of [env]. [buf] can specify the temporary buffer to use.

      @raise Invalid_argument if [dom p] is not included in
      [String.Map.dom env]. *)

  val pp_format : t -> Format.formatter -> env -> unit
  (** [pp_format p ppf env] is like {!format} but prints the result
      on [ppf]. *)
end

(** Logging. *)
module Log : sig

  (** {1 Log level and output} *)

  (** The type for log levels. *)
  type level = Show | Error | Warning | Info | Debug

  val level : unit -> level option
  (** [level ()] is the log level (if any). If the log level is [(Some l)]
      any message whose level is [<= l] is logged. If level is [None]
      no message is ever logged. Initially the level is [(Some Warning)]. *)

  val set_level : level option -> unit
  (** [set_level l] sets the log level to [l]. See {!level}. *)

  val set_formatter : [`All | `Level of level ] -> Format.formatter -> unit
  (** [set_formatter spec ppf] sets the formatter for a given level or
      for all the levels according to [spec]. Initially the formatter
      of level [Show] is {!Format.std_formatter} and all the other level
      formatters are {!Format.err_formatter}. *)

  (** {1 Log messages} *)

  val msg : ?header:string -> level ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [msg header l fmt ...] logs a message with level [l]. [header] is
      the message header, default depends on [l]. *)

  val kmsg : ?header:string ->
    (unit -> 'a) -> level -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kmsg header k l fmt ...] is like [msg header l fmt] but calls [k ()]
      before returning. *)

  val show : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [show fmt ...] logs a message with level [Show]. [header] defaults
      to [None]. *)

  val err : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [err fmt ...] logs a message with level [Error]. [header] defaults
      to ["ERROR"]. *)

  val warn : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [warn fmt ...] logs a message with level [Warning]. [header] defaults
      to ["WARNING"]. *)

  val info : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [info fmt ...] logs a message with level [Info]. [header] defaults
      to ["INFO"]. *)

  val debug : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [debug info ...] logs a message with level [Debug]. [header] defaults
      to ["DEBUG"]. *)

  (** {1 Log error {!Resultv}s} *)

  val on_error : ?log:level -> pp:(Format.formatter -> 'b -> unit) ->
    use:'a -> ('a, 'b) result -> 'a
  (** [on_error ~log ~pp ~use r] is:
      {ul
      {- [v] if [r = `Ok v]}
      {- [use] if [r = `Error msg]. As a side effect [msg] is
         {{!Log}logged} with [pp] on  level [log]
         (defaults to {!Log.Error})}} *)

  val kon_error : ?log:level -> pp:(Format.formatter -> 'b -> unit) ->
    use:('a, 'c) result -> ('a, 'b) result -> ('a, 'c) result
  (** [kon_error ~log ~pp ~use r] is:
      {ul
      {- [v] if [r = `Ok v]}
      {- [use] if [r = `Error e]. As a side effect [e] is
         {{!Log}logged} with [pp] on level [log]
         (defaults to {!Log.Error})}} *)

  val on_err_msg : ?log:level -> use:'a -> ('a, R.err_msg) result -> 'a
  (** [on_err_msg ~log ~use] is [on_error ~log ~pp:pp_msg ~use]. *)

  val kon_err_msg : ?log:level -> use:('a, 'c) result ->
    ('a, R.err_msg) result -> ('a, 'c) result
  (** [kon_err_msg ~log ~use] is [on_errork ~log ~pp:pp_msg ~use]. *)

  (** {1 Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level
      [Warning]. *)
end

(** {1 Paths}  *)

type path
(** The type for absolute or relative paths. *)


(** File system paths, path sets and maps.

    [Path] provides three types for handling paths. Values of type
    {!Path.t} are for paths that are either relative or absolute while
    those of type {!Path.rel} and {!Path.abs} specialize to either
    case.

    Relative paths and absolute path each have corresponding modules
    {!Rel} and {!Abs} with specialized functions. {{!conversions}Conversion}
    between the three type of paths are explicit.

    {b FIXME}. We need to properly handle {!Filename.current_dir_name} and
    {!Filename.parent_dir_name} in path segments. *)
module Path : sig

  (** {1:filepaths File paths} *)

  type filename = string
  (** The type for file names (basenames). *)

  type rel
  (** The type for relative paths. *)

  type abs
  (** The type for absolute paths. *)

  type t = path
  (** The type for absolute or relative paths. *)

  val root : path
  (** [root] is the root absolute path (empty list of segments). *)

  val empty : path
  (** [empty] is the empty relative path (empty list of segments). *)

  val dash : path
  (** [dash] is the ["-"] relative path. *)

  val add : path -> string -> path
  (** [add p seg] concatenates [seg] at the end of [p]. For any [p],
      [add p "" = p]. *)

  val concat : path -> rel -> path
  (** [concat p p'] concatenates [p'] at the end of [p]. *)

  val ( / ) : path -> string -> path
  (** [p / c] is [add p c]. Left associative. *)

  val ( // ) : path -> rel -> path
  (** [p // p'] is [concat p p']. Left associative. *)

  val file : filename -> path
  (** [file name] is [add empty f]. *)

  val base : string -> path
  (** [base name] is [add empty f]. *)

  val basename : path -> string
  (** [basename p] is the basename of [p]. If [p] has no segments the
      empty string is returned. *)

  val dirname :  path -> path
  (** [dirname p] is the dirname of [p]. If [p] has no segments [p]
      is returned. *)

  val rem_prefix : path -> path -> rel option
  (** [rem_prefix pre p] is [p] with the literal prefix [pre] removed. [None]
      if [pre] is not a prefix of [p]. *)

  val find_prefix : path -> path -> t option
  (** [find_prefix p p'] is a common prefix for [p] and [p']. There is
      always a common prefix between path of the same kind (either {!root}
      or {!empty} and [None] is only returned if [p] and [p'] are of
      different kind. *)

  (** {1:predicates Predicates and comparison} *)

  val is_root : path -> bool
  (** [is_root p] is [true] iff [p] is {!root}. *)

  val is_empty : path -> bool
  (** [is_empty p] is [true] iff [p] is {!empty}. *)

  val is_dash : path -> bool
  (** [is_dash p] is [true] iff [p] is {!dash}. *)

  val is_rel : path -> bool
  (** [is_rel p] is [true] iff [p] is a relative path. *)

  val is_abs : path -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path. *)

  val is_prefix : path -> path -> bool
  (** [is_prefix p p'] is [true] if [p] is a literal prefix of [p']. *)

  val equal : path -> path -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : path  -> path -> int
  (** [compare p p'] is [Pervasives.compare p p']. *)

  (** {1:conversions Conversions} *)

  val to_rel : path -> rel option
  (** [to_rel p] is [Some r] if [p] is a relative path. *)

  val of_rel : rel -> path
  (** [of_rel r] is [r] as a path. *)

  val to_abs : path -> abs option
  (** [to_abs p] is [Some a] if [p] is an absolute path. *)

  val of_abs : abs -> path
  (** [of_abs a] is [a] as a path. *)

  val to_segs : path -> [ `Abs of string list | `Rel of string list ]
  (** [to_segs p] is [p]'s segments. *)

  val of_segs : [ `Abs of string list | `Rel of string list ] -> path
  (** [of_segs segs] is a path from [segs] segments. *)

  val to_string : path -> string
  (** [to_string p] is the path [p] as a string according to
      the driver's platform convention with {!Filename.dir_sep}. *)

  val of_string : string -> path
  (** [of_string s] is the string [s] as a path. [s] is splitted
      according to the driver's platform convention with {!Filename.dir_sep}. *)

  val quote : path -> string
  (** [quote p] is the path [p] as a string, quoted according
      to the driver's platform conventions with {!Filename.quote}. *)

  val pp : Format.formatter -> path -> unit
  (** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

  (** {1:file_exts File extensions} *)

  type ext =
    [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
    | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
    | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
    | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
    | `Tbz | `Xml | `Zip
    | `Ext of string ]
  (** The type for file extensions. *)

  val ext_to_string : ext -> string
  (** [ext_to_string ext] is [ext] as a string (without separator). *)

  val ext_of_string : string -> ext
  (** [ext_of_string ext] is [ext] as a file extension ([ext] without
      separator). *)

  val pp_ext : Format.formatter -> ext -> unit
  (** [pp_ext ppf p] prints file extension [ext] on [ppf] using
      {!ext_to_string}. *)

  val ext : path -> ext option
  (** [ext p] is [p]'s last segment file extension (if any). *)

  val get_ext : path -> ext
  (** [get_ext p] is [p]'s last segment file extension.

      @raise Invalid_argument if [p]'s last segment has no file extension. *)

  val add_ext : path -> ext -> path
  (** [add_ext p ext] is [p] with [ext] concatenated to [p]'s last segment. *)

  val rem_ext : path -> path
  (** [rem_ext p] is [p] with [ext] removed from [p]'s last segment
      (if it has an extension). *)

  val change_ext : path -> ext -> path
  (** [change_ext p e] is [add_ext (rem_ext p)]. *)

  val ( + ) : path -> ext -> path
  (** [p + ext] is [add_ext p e]. Left associative. *)

  val has_ext : ext -> path -> bool
  (** [has_ext p ext] is [true] iff [p]'s last segment has file extension
      [ext]. *)

  val ext_matches : ext list -> path -> bool
  (** [ext_matches exts p] is [true] iff [p]'s last segment has a file
      extension in [exts]. *)

  (** {1:rel Relative paths} *)

  (** Relative paths. *)
  module Rel : sig

    (** {1 Relative paths} *)

    type path = t
    (** The type for absolute or relative paths. *)

    type t = rel
    (** The type for relative paths. *)

    val empty : rel
    (** See {!Path.empty}. *)

    val dash : rel
    (** See {!Path.dash}. *)

    val add : rel -> string -> rel
    (** See {!Path.add}. *)

    val concat : rel -> rel -> rel
    (** See {!Path.concat}. *)

    val file : filename -> rel
    (** [file name] is [add empty f]. *)

    val base : string -> rel
    (** [base name] is [add empty f]. *)

    val ( / ) : rel -> string -> rel
    (** See {!Path.( / )}. *)

    val ( // ) : rel -> rel -> rel
    (** See {!Path.( // )}. *)

    val basename : rel -> string
    (** See {!Path.basename}. *)

    val dirname :  rel -> rel
    (** See {!Path.dirname}. *)

    val rem_prefix : rel -> rel -> rel option
    (** See {!Path.rem_prefix}. *)

    val find_prefix : rel -> rel -> rel
    (** See {!Path.find_prefix}. *)

    (** {1:predicates Predicates and comparison} *)

    val is_empty : rel -> bool
    (** See {!Path.is_empty}. *)

    val is_dash : rel -> bool
    (** See {!Path.is_dash}. *)

    val is_prefix : rel -> rel -> bool
    (** See {!Path.is_prefix}. *)

    val equal : rel -> rel -> bool
    (** See {!Path.equal}. *)

    val compare : rel  -> rel -> int
    (** See {!Path.compare}. *)

    (** {1 Conversions} *)

    val to_segs : rel -> string list
    (** [to_segs r] is [r]'s segments. *)

    val of_segs : string list -> rel
    (** [of_segs segs] is a path from [segs] segments. *)

    val to_string : rel -> string
    (** See {!Path.to_string}. *)

    val quote : rel -> string
    (** See {!Path.quote}. *)

    val pp : Format.formatter -> rel -> unit
    (** See {!Path.pp}. *)

    (** {1:file_exts File extensions} *)

    val ext : rel -> ext option
    (** See {!Path.ext}. *)

    val get_ext : rel -> ext
    (** See {!Path.get_ext}. *)

    val add_ext : rel -> ext -> rel
    (** See {!Path.add_ext}. *)

    val rem_ext : rel -> rel
    (** See {!Path.rem_ext}. *)

    val change_ext : rel -> ext -> rel
    (** See {!Path.change_ext}. *)

    val ( + ) : rel -> ext -> rel
    (** See {!Path.( + )}. *)

    val has_ext : ext -> rel -> bool
    (** See {!Path.has_ext}. *)

    val ext_matches : ext list -> rel -> bool
    (** See {!Path.ext_matches}. *)

    (** {1:sets_maps Path sets and maps} *)

    module Set : sig
      include Set.S with type elt = rel
      val of_list : elt list -> t
    end

    module Map : sig
      include Map.S with type key = rel
      val dom : 'a t -> Set.t
      (** [dom m] is the domain of [m]. *)
    end
  end

  (** {1:abs Absolute paths} *)

  (** Absolute paths. *)
  module Abs : sig

    (** {1 Absolute paths} *)

    type path = t
    (** The type for absolute or relative paths. *)

    type t = abs
    (** The type for absolute paths. *)

    val root : abs
    (** See {!Path.root}. *)

    val add : abs -> string -> abs
    (** See {!Path.add}. *)

    val concat : abs -> rel -> abs
    (** See {!Path.concat}. *)

    val ( / ) : abs -> string -> abs
    (** See {!Path.( / )}. *)

    val ( // ) : abs -> rel -> abs
    (** See {!Path.( // )}. *)

    val basename : abs -> string
    (** See {!Path.basename}. *)

    val dirname :  abs -> abs
    (** See {!Path.dirname}. *)

    val rem_prefix : abs -> abs -> rel option
    (** See {!Path.rem_prefix}. *)

    val find_prefix : abs -> abs -> abs
    (** See {!Path.find_prefix}. *)

    (** {1:predicates Predicates and comparison} *)

    val is_root : abs -> bool
    (** See {!Path.is_root}. *)

    val is_prefix : abs -> abs -> bool
    (** See {!Path.is_prefix}. *)

    val equal : abs -> abs -> bool
    (** See {!Path.equal}. *)

    val compare : abs  -> abs -> int
    (** See {!Path.compare}. *)

    (** {1:conversions Conversions} *)

    val to_segs : abs -> string list
    (** [to_segs a] is [a]'s segments. *)

    val of_segs : string list -> abs
    (** [of_segs segs] is a path from [segs] segments. *)

    val to_string : abs -> string
    (** See {!Path.to_string}. *)

    val quote : abs -> string
    (** See {!Path.quote}. *)

    val pp : Format.formatter -> abs -> unit
    (** See {!Path.pp}. *)

    (** {1:file_exts File extensions} *)

    val ext : abs -> ext option
    (** See {!Path.ext}. *)

    val get_ext : abs -> ext
    (** See {!Path.get_ext}. *)

    val add_ext : abs -> ext -> abs
    (** See {!Path.add_ext}. *)

    val rem_ext : abs -> abs
    (** See {!Path.rem_ext}. *)

    val change_ext : abs -> ext -> abs
    (** See {!Path.change_ext}. *)

    val ( + ) : abs -> ext -> abs
    (** See {!Path.( + )}. *)

    val has_ext : ext -> abs -> bool
    (** See {!Path.has_ext}. *)

    val ext_matches : ext list -> abs -> bool
    (** See {!Path.ext_matches}. *)

    (** {1:sets_maps Path sets and maps} *)

    module Set : sig
      include Set.S with type elt = abs
      val of_list : elt list -> t
    end

    module Map : sig
      include Map.S with type key = abs
      val dom : 'a t -> Set.t
      (** [dom m] is the domain of [m]. *)
    end
  end

  (** {1:sets_maps Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = t
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = t
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

(** {1 OS interaction.} *)

module OS : sig
  (** OS interaction

      {1 File system operations and commands} *)

  type 'a result = ('a, R.err_msg) R.t

  module Path : sig
    (** Path operations. *)

    (** {1:pathops Path operations} *)

    val exists : ?err:bool -> path -> bool result
    (** [exists path] is [true] iff [path] exists.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val move : ?force:bool -> path -> path -> unit result
    (** [move ~force src dst] moves path [src] to [dst]. If [force] is
        [false] (default) the operation fails if [dst] exists. *)

    (** {1:pathmatch Matching paths} *)

    val matches : Path.t -> path list result
    (** [matches pat] is the list of paths in the file system that
        match the pattern [pat].

        [pat] is a path whose segments are made of {{!Pat}named string
        patterns}. Each variable of the pattern greedily matches a
        segment or sub-segment. For example the pattern:
{[
        Path.(base "data" / "$(dir)" / "$(file).txt")
]}
        will match any existing file of the form data/*/*.txt. *)

    val unify : ?init:Pat.env -> Path.t -> (path * Pat.env) list result
    (** [unify ~init pat] is like {!matches} except each
        matching path is returned with an environment mapping pattern
        variables to their matched part in the path. See {!Pat.unify}. *)
  end

  module File : sig
    (** File operations. *)

    (** {1:fileops File operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.getcwd}current working directory}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists file] is [true] iff [file] exists and is not a directory.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val dev_null : path
    (** [dev_null] represents a file that discards all writes. *)

    val delete : ?maybe:bool -> path -> unit result
    (** [delete ~maybe file] deletes file [file]. If [maybe] is [false]
        (default) no error is returned if the file doesn't exit. *)

    val temp : ?dir:path -> string -> path result
    (** [temp dir suffix] creates a temporary file with suffix
        [suffix] in [dir] (defaults to {!Filename.get_temp_dir_name})
        and returns its name. The file is destroyed at the end of
        program execution. *)

    (** {1:input Input} *)

    val with_inf : (in_channel -> 'a -> 'b result) -> path -> 'a ->
      'b result
    (** [with_inf f inf v] opens [inf] as a channel [ic] and returns [f
        ic v] if no error occurs. In case of error the channel is closed
        and the error is returned. If [inf] is {!Path.dash}, [ic] is
        {!Pervasives.stdin} and not closed. *)

    val read : path -> string result
    (** [read file] is [file]'s content. If [file] is {!Path.dash} reads
        from {!Pervasives.stdin}. *)

    val read_lines : path -> string list result
    (** [read_lines file] is [file]'s content splitted at ['\n']. If
        [file] is {!Path.dash} reads from {!Pervasives.stdin}. *)

    (** {1:output Output} *)

    val with_outf : (out_channel -> 'a -> 'b result) -> path -> 'a ->
      'b result
    (** [with_inf f outf v] opens [outf] as a channel [oc] and returns
        [f oc v] if no error occurs. In case of error the channel is
        closed and the error is returned. If [outf] is {!Path.dash}, [oc] is
        {!Pervasives.stdout} and not closed. *)

    val write : path -> string -> unit result
    (** [write file content] outputs [content] to [file]. If [file]
        is {!Path.dash}, writes to {!Pervasives.stdout}. If an error is
        returned [file] is left untouched except if {!Pervasives.stdout}
        is written.*)

    val write_lines : path -> string list -> unit result
    (** [write_lines file lines] outputs [lines] separated by ['\n'] to
        [file]. If [file] is {!Path.dash}, writes to {!Pervasives.stdout}.
        If an error is returned [file] is left untouched except if
        {!Pervasives.stdout} is written.*)

    val write_subst : (string * string) list -> path -> string -> unit result
    (** [write_subst vars file content] outputs [content] to [file]. In
        [content] patterns of the form ["%%ID%%"] are replaced by the value
        of [List.assoc "ID" vars] (if any). If [file] is {!Path.dash}, writes
        to {!Pervasives.stdout}. If an error is returned [file] is left
        untouched except if {!Pervasives.stdout} is written. *)
  end

  module Dir : sig
    (** Directory operations. *)

    (** {1:dirops Directory operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.current}current working directory}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists dir] is [true] if directory [dir] exists.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val current : unit -> path result
    (** [current ()] is the current working directory. *)

    val set_current : path -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val contents : path -> path list result
    (** [contents dir] is the contents of [dir]. Resulting paths
        are prefixed by [dir]. *)

    val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) ->
      'a -> string list -> 'a result
    (** [fold_files_rec skip f acc paths] folds [f] over the files
        found in [paths]. Files and directories whose suffix matches an
        element of [skip] are skipped. {b FIXME} this should be using
        {!Path.t} and {!Path.ext}. *)
  end

  module Cmd : sig
    (** Executing commands.

        {1 Commands} *)

    val exists : ?err:bool -> string -> bool result
    (** [exists cmd] is [true] if [cmd] exists and can be invoked.
        If [err] is [true] (defaults to [false]) an error is returned
        when the command doesn't exist. *)

    val exec_ret : string -> string list -> int
    (** [exec_ret cmd args] executes [cmd] with arguments [args] and
        returns the exit code of the invocation. *)

    val exec : string -> string list -> unit result
    (** [exec cmd args] executes [cmd] with arguments [args]. On exit
        code [0] returns [`Ok ()]. Otherwise an error message with
        the failed invocation and its exit code is returned in [`Error]. *)

    val exec_read : ?trim:bool -> string -> string list -> string result
    (** [exec_read cmd args] execute [cmd] with arguments [args] and returns
        its standard output. If [cmd]'s return code is non zero returns
        an error message. If [trim] is [true] (default) the contents is
        passed to {!Prelude.String.trim} before being returned. *)

    val exec_read_lines : string -> string list -> string list result
    (** [exec_readl_lines cmd args] is like [input ~trim:false cmd args] but
        the input is splitted at ['\n']. *)

    val exec_write : string -> string list -> path -> unit result
    (** [exec_write cmd args file] execute [cmd] with arguments [args] and
        writes the invocation's [stdout] to [file]. In [cmd]'s return code
        is non zero returns an error message and [file] is left intact. *)
  end

  (** {1:env_lookup Environment variables lookup} *)


  module Env : sig
    (** Environment variables lookup.

        TODO, review with parsers/default values. *)

    val find : string -> string option
    (** [find var] is the value if the environment variable [var], if
        defined. *)

    val get : string -> string result
    (** [get_env var] is like {!find} but returns an error if [var] is
        undefined. *)
  end
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
