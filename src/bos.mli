(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Light, basic OS interaction.

    Open the module to use it, this defines only {{!path}one type},
    {{!strf}one value} and modules in your scope.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Preliminaries, formatting and logging} *)

open Rresult
open Astring

(** Named string patterns.

    Named string patterns are strings with variables of the form ["$(VAR)"]
    where [VAR] is any sequence of characters except [')'] or [',']. In these
    strings a ["$"] litteral must be written ["$$"].*)
module Pat : sig

  (** {1 Patterns} *)

  type t = [ `Lit of string | `Var of string ] list
  (** The type for patterns. A list of either a string literal or a
      variable. *)

  type env = string String.Map.t
  (** Type type for pattern environments. Maps pattern variable names
      to string values. *)

  val v : string -> t
  (** [v s] parses [s] according to the pattern syntax.
      @raise Invalid_argument if [s] is not a valid pattern. Use
      {!of_string} to deal with errors. *)

  val of_string : ?buf:Buffer.t -> string -> (t, [> R.msg]) result
  (** [of_string ?buf s] parses [s] according to the pattern syntax.
      [buf] can specify the temporary buffer to use. *)

  val to_string : ?buf:Buffer.t -> t -> string
  (** [to_string p] converts [p] to a string according to the pattern
      syntax. [buf] can specify the temporary buffer to use.  *)

  val dom : t -> String.Set.t
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
      defaults to {!String.Map.empty}). If a variable appears
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

  (** {1 Log error {!Rresult}s} *)

  val on_error : ?level:level -> pp:(Format.formatter -> 'b -> unit) ->
    use:'a -> ('a, 'b) result -> 'a
  (** [on_error ~level ~pp ~use r] is:
      {ul
      {- [v] if [r = `Ok v]}
      {- [use] if [r = `Error msg]. As a side effect [msg] is
         {{!Log}logged} with [pp] on  level [level]
         (defaults to {!Log.Error})}} *)

  val kon_error : ?level:level -> pp:(Format.formatter -> 'b -> unit) ->
    use:('a, 'c) result -> ('a, 'b) result -> ('a, 'c) result
  (** [kon_error ~log ~pp ~use r] is:
      {ul
      {- [v] if [r = `Ok v]}
      {- [use] if [r = `Error e]. As a side effect [e] is
         {{!Log}logged} with [pp] on level [level]
         (defaults to {!Log.Error})}} *)

  val on_error_msg : ?level:level -> use:'a -> ('a, R.msg) result -> 'a
  (** [on_error_msg ~level ~use] is [error ~log ~pp:pp_msg ~use]. *)

  val kon_error_msg : ?level:level -> use:('a, 'c) result ->
    ('a, R.msg) result -> ('a, 'c) result
  (** [kon_error_msg ~log ~use] is [errork ~log ~pp:pp_msg ~use]. *)

  (** {1 Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level
      [Warning]. *)
end

(** {1 Paths}  *)

type path
(** The type for file paths. *)

(** File paths, file {{!file_exts}extensions}, path {{!Set}sets}
    and {{!Map}maps}.

    A file path specifies a file or a directory in a file hierarchy. A
    file path has three parts, an optional platform-dependent
    {{!split_volume}volume}, an optional root directory separator
    {!dir_sep}, followed by a list of {!dir_sep} separated
    segments. Segments are non empty strings except for maybe the last
    one, the latter distinguishes directories (["/a/b/"]) from file
    specifications (["/a/b"]).

    A file path is {e absolute} if the optional root {!dir_sep} is
    present and {e relative} otherwise.

    Windows accepts both ['\\'] and ['/'] as directory separators.  On
    Windows ['/'] are converted to ['\\'] on the fly by the module and
    should be preferred to write portable programs. Note that in the
    following documentation backslashes are escaped as per OCaml
    conventions in strings so ["\\"] is really a single backslash.

    This module operates on paths without accessing the operating
    system. The {!OS.Path} module has the functions that do so. *)
module Path : sig

  (** {1:filepaths File paths} *)

  val dir_sep : string
  (** [dir_sep] is the platform dependent path directory separator. *)

  type t = path
  (** The type for paths. *)

  val v : string -> path
  (** [v s] is the string [s] as path, see {!of_string} for details.

      @raise Invalid_argument if {!of_string}[ p] is [None]. Use {!of_string}
      to deal with errors. *)

  val add_seg : path -> string -> path
  (** [add_seg p seg] adds [seg] at the end of [p]. An empty [seg]
      is only added if [p] hasn't one yet. Examples:
      {ul
      {- [equal (add_seg (v "/a") "b") (v "/a/b")]}
      {- [equal (add_seg (v "/a/") "b") (v "/a/b")]}
      {- [equal (add_seg (v "/a/b") "") (v "/a/b/")]}
      {- [equal (add_seg (v "/a/b/") "") (v "/a/b/")]}
      {- [equal (add_seg (v "/") "") (v "/")]}
      {- [equal (add_seg (v "/") "a") (v "/a")]}}

      @raise Invalid_argument if {!is_seg_valid}[ seg] is [false]. *)

  val append : path -> path -> path
  (** [append p p'] appends [p'] to [p] as follows:
      {ul
      {- If [p'] is absolute or has a non-empty {{!split_volume}volume} then
         [p'] is returned.}
      {- Otherwise appends [p'] to [p] using a {!dir_sep} if needed.}}
      Examples:
      {ul
      {- [equal (append (v "/a/b/") (v "e/f")) (v "/a/b/e/f")]}
      {- [equal (append (v "/a/b") (v "e/f")) (v "/a/b/e/f")]}
      {- [equal (append (v "/a/b/") (v "/e/f")) (v "/e/f")]}
      {- [equal (append (v "a/b/") (v "e/f")) (v "a/b/e/f")]}
      {- [equal (append (v "a/b") (v "C:e")) (v "C:e")] (Windows)}} *)

  val ( / ) : path -> string -> path
  (** [p / seg] is {!add_seg}[ p seg]. Left associative. *)

  val ( // ) : path -> path -> path
  (** [p // p'] is {!append}[ p p']. Left associative. *)

  (** {1:cst Constants} *)

  val root : path
  (** [root] is [v "/"], the root absolute path. *)

  val cur_dir : path
  (** [cur_dir] is [v "."], the current directory. *)

  val par_dir : path
  (** [par_dir] is [v ".."], the parent directory. *)

  (** {1:predicates Predicates and comparison} *)

  val is_seg_valid : string -> bool
  (** [is_seg_valid s] is [true] iff [s] does not contain {!dir_sep} or a
      [0x00] byte. *)

  val is_rel : path -> bool
  (** [is_rel p] is [true] iff [p] is a relative path. *)

  val is_abs : path -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path. *)

  val is_prefix : root:path -> path -> bool
  (** [is_prefix ~root p] is [true] if [root] is a prefix of [p].  This
      checks that [root] has the same optional volume as [p], the same
      optional root directory separator and that the list of segments
      of [root] is a prefix of the segments of [p]. Examples:
      {ul
      {- [is_prefix (v "/a/b") (v "/a/b") = true]}
      {- [is_prefix (v "/a/b") (v "/a/b/") = true]}
      {- [is_prefix (v "/a/b") (v "/a/bc") = false]}
      {- [is_prefix (v "/a/b") (v "/a/b/c") = true]}
      {- [is_prefix (v "/a/b/") (v "/a/b") = false]}
      {- [is_prefix (v "a/b") (v "/a/b") = false]}
      {- [is_prefix (v "a/b") (v "a/b") = true]}
      {- [is_prefix (v "//a/b") (v "/a/b") = false]}
      {- [is_prefix (v "C:a") (v "a") = false] (Windows)}} *)

  val equal : path -> path -> bool
  (** [equal p p'] is [true] if [p] and [p'] have the same volume
      are both relative or absolute and have the same segments. This
      is a byte level comparison. *)

  val compare : path  -> path -> int
  (** [compare p p'] is a total order on paths compatible with {!equal}. *)

  (** {1:vol Volume and segments} *)

  val split_volume : path -> string * path
  (** [split_volume p] is the pair [(vol, q)] where [vol] is
      the platform dependent volume of [p] or the empty string
      if there is none and [q] the path [p] without its volume, i.e. is
      its optional root {!dir_sep} and segments.

      On POSIX if [v] is non-empty then it
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267}can} only be ["/"] (e.g. in [v "//a/b"]). On Windows [v] may be
      one of the following prefixes parsed before an
      absolute root {!dir_sep}, except in the first case
      where a relative path can follow:
{[
$(drive):
\\$(server)\$(share)
\\?\$(drive):
\\?\$(server)\$(share)
\\?\UNC\$(server)\$(share)
\\.\$(device)
]}
     The following invariant holds:
     {ul
     {- [equal (v (vol ^ (to_string q))) p]}}
     {b Warning.} [equal (append (v vol) q) p] does not hold. *)

  val segs : path -> string list
  (** [segs p] is [p]'s (non-empty) list of segments. Absolute paths have an
      initial empty string added, this allows to recover the path with
      {!String.concat}[ ~sep:dir_sep]. Examples:
      {ul
      {- [segs (v "/a/b/") = [""; "a"; "b"; ""]]}
      {- [segs (v "/a/b") = [""; "a"; "b"]]}
      {- [segs (v "a/b/") = ["a"; "b"; ""]]}
      {- [segs (v "a/b") = ["a"; "b"]]}
      {- [segs (v "a") = ["a"]]}
      {- [segs (v "") = ["."]]}
      {- [segs (v "/") = [""; ""]]}
      {- [segs (v "\\\\.\\dev\\") = ["";""]] (Windows)}
      {- [segs (v "\\\\server\\share\\a") = ["";"a"]] (Windows)}
      {- [segs (v "C:a") = ["a"]] (Windows)}
      {- [segs (v "C:\\a") = ["";"a"]] (Windows)}}

      The following invariant holds:
      {ul
      {- [to_string (snd (split_volume p)) = (String.concat ~sep:dir_sep
      (segs p))]}} *)

  val filename : path -> string
  (** [filename p] is the filename of [p], that is the last segment of
      [p]. Examples:
      {ul
      {- [filename (v "/a/b/") = ""]}
      {- [filename (v "/a/b") = "b"]}
      {- [filename (v "a") = "a"]}
      {- [filename (v "/") = ""]}
      {- [filename (v "C:\\") = ""] (Windows)}
      {- [filename (v "C:a") = "a"] (Windows)}} *)

  val base : path -> path
  (** [base p] is the path made of the last {e non-empty} segment of
      [p] or [p] itself on root paths.
      Examples:
      {ul
      {- [equal (base @@ v "/a/b/") (v "b")]}
      {- [equal (base @@ v "/a/b") (v "b")]}
      {- [equal (base @@ v "a") (v "a")]}
      {- [equal (base @@ v ".") (v ".")]}
      {- [equal (base @@ v "..") (v "..")]}
      {- [equal (base @@ v "/") (v "/")]}
      {- [equal (base @@ v "\\\\server\\share\\") (v "\\\\server\\share\\")]
         (Windows)}
      {- [equal (base @@ v "C:\\") (v "C:\\")] (Windows)}} *)

  val parent : path -> path
  (** [parent p] is the parent path of [p]. This is defined as [p] without
      its last {e non-empty} segment or [p] if there is no such segment
      or {!cur_dir} for a single segment relative path. Examples:
      {ul
      {- [equal (parent @@ v "/a/b") (v "/a")]}
      {- [equal (parent @@ v "/a/b/") (v "/a")]}
      {- [equal (parent @@ v "/a") (v "/")]}
      {- [equal (parent @@ v "/a/") (v "/")]}
      {- [equal (parent @@ v "a/b/") (v "a")]}
      {- [equal (parent @@ v "a/b") (v "a")]}
      {- [equal (parent @@ v "a") (v ".")]}
      {- [equal (parent @@ v "a/") (v ".")]}
      {- [equal (parent @@ v ".") (v ".")]}
      {- [equal (parent @@ v "..") (v ".")]}
      {- [equal (parent @@ v "/") (v "/")]}
      {- [equal (parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\")]
         (Windows)}
      {- [equal (parent @@ v "C:a") (v "C:.")] (Windows)}
      {- [equal (parent @@ v "C:\\") (v "C:\\")] (Windows)}} *)

  val file_to_dir : path -> path
  (** [file_to_dir p] is {!add_seg}[ p ""]. It ensures the result has a
      trailing {!dir_sep}. Examples:
      {ul
      {- [equal (file_to_dir @@ v "/a/b") (v "/a/b/")]}
      {- [equal (file_to_dir @@ v "/a/b/") (v "/a/b/")]}
      {- [equal (file_to_dir @@ v "a") (v "a/")]}
      {- [equal (file_to_dir @@ v "/") (v "/")]}
      {- [equal (file_to_dir @@ v "\\\\server\\share\\")
         (v "\\\\server\\share\\")]
         (Windows)}
      {- [equal (file_to_dir @@ v "C:a") (v "C:a/")] (Windows)}
      {- [equal (file_to_dir @@ v "C:\\") (v "C:\\")] (Windows)}} *)

  val dir_to_file : path -> path
  (** [dir_to_file p] removes the last segment of [p] if it is empty.
      It ensures the result has no trailing {!dir_sep}. Examples:
      {ul
      {- [equal (dir_to_file @@ v "/a/b") (v "/a/b")]}
      {- [equal (dir_to_file @@ v "/a/b/") (v "/a/b")]}
      {- [equal (dir_to_file @@ v "a/") (v "a")]}
      {- [equal (dir_to_file @@ v "/") (v "/")]}
      {- [equal (dir_to_file @@ v "\\\\server\\share\\")
         (v "\\\\server\\share\\")]
         (Windows)}
      {- [equal (dir_to_file @@ v "C:a/") (v "C:a")] (Windows)}
      {- [equal (dir_to_file @@ v "C:\\") (v "C:\\")] (Windows)}} *)

  val find_prefix : path -> path -> path option
  (** [find_prefix p p'] is [Some root] if there exists [root] such that
      [root] is the longest path with
      [is_prefix root p && is_prefix root p' = true] and [None] otherwise.
      Note that if both [p] and [p'] are relative or absolute
      and have the same volume then a prefix exists.
      {ul
      {- [find_prefix (v "a/b/c") (v "a/b/d")] is [Some (v "a/b/")]}
      {- [find_prefix (v "a/b/c") (v "a/b/cd")] is [Some (v "a/b/")]}
      {- [find_prefix (v "/a/b/c") (v "/a/b/d")] is [Some (v "/a/b/")]}
      {- [find_prefix (v "a/b") (v "e/f")] is [Some (v ".")]}
      {- [find_prefix (v "/a/b") (v "/e/f")] is [Some (v "/")]}
      {- [find_prefix (v "/a/b") (v "e/f")] is [None]}
      {- [find_prefix (v "C:\\a") (v "\\a")] is [None]} (Windows)} *)

  val rem_prefix : root:path -> path -> path option
  (** [rem_prefix root p] is [Some q] if [root] is a
      {{!is_prefix}prefix} of [p]; [q] is [p] without the [root]
      prefix but interpreted as a {{!file_to_dir}directory} (hence [q]
      is always relative). Examples:
      {ul
      {- [rem_prefix (v "/a/b") (v "/a/bc")] is [None]}
      {- [rem_prefix (v "/a/b") (v "/a/b")] is [Some (v ".")]}
      {- [rem_prefix (v "/a/b/") (v "/a/b")] is [None]}
      {- [rem_prefix (v "/a/b") (v "/a/b/")] is [Some (v ".")]}
      {- [rem_prefix (v "/a/b/") (v "/a/b/")] is [Some (v ".")]}
      {- [rem_prefix (v "/a/b") (v "/a/b/c")] is [Some (v "c")]}
      {- [rem_prefix (v "/a/b/") (v "/a/b/c")] is [Some (v "c")]}
      {- [rem_prefix (v "a") (v "a/b/c")] is [Some (v "b/c")]}}

      {b Note.} If you {{!find_prefix}find} a prefix and this
      prefix is ["."], [rem_prefix] may return [None] on that
      prefix and the path where you found it. *)

  val normalize : path -> path
  (** [normalize p] normalizes [p] to a path referring to the same
      {{!dir_to_file}file} without consulting the filesystem. If [p]
      is absolute the resulting path has no {!cur_dir} and {!par_dir}
      segments. If [p] is relative it has no {!cur_dir} and may only
      have potential {!par_dir} as initial segments. Note that except
      if the path is a root, the path never has a trailing directory
      separator. Examples:
      {ul
      {- [equal (normalize @@ v "./a/..") (v ".")]}
      {- [equal (normalize @@ v "/a/b/./..") (v "/a")]}
      {- [equal (normalize @@ v "/../..") (v "/")]}
      {- [equal (normalize @@ v "/a/../..") (v "/")]}
      {- [equal (normalize @@ v "./../..") (v "../..")]}
      {- [equal (normalize @@ v "../../a/") (v "../../a")]}
      {- [equal (normalize @@ v "/a/b/c/./../../g") (v "/a/g")]}
      {- [equal (normalize @@ v "\\\\?\\UNC\\server\\share\\..")
         (v "\\\\?\\UNC\\server\\share\\")] (Windows)}} *)

  val rooted : root:path -> path -> path option
  (** [rooted ~root p] is:
      {ul
      {- [None] if
         [is_prefix (normalize root) (normalize @@ append root p) = false].}
      {- [Some (normalize @@ append root p)] otherwise.}}
      In other words it ensures that an absolute path [p] or a relative
      path [p] expressed w.r.t. [root] expresses a path that is
      within the [root] file hierarchy. Examples:
      {ul
      {- [rooted (v "/a/b") (v "c")] is [Some (v "/a/b/c")]}
      {- [rooted (v "/a/b") (v "/a/b/c")] is [Some (v "/a/b/c")]}
      {- [rooted (v "/a/b") (v "/a/b/c/")] is [Some (v "/a/b/c")]}
      {- [rooted (v "/a/b") (v "/a/b/c/.")] is [Some (v "/a/b/c")]}
      {- [rooted (v "/a/b") (v "../c")] is [None]}
      {- [rooted (v "a/b") (v "c")] is [Some (v "a/b/c")]}
      {- [rooted (v "a/b") (v "/c")] is [None]}
      {- [rooted (v "a/b") (v "../c")] is [None]}
      {- [rooted (v "a/b") (v "c/..")] is [Some (v "a/b")]}
      {- [rooted (v "a/b") (v "c/../..")] is [None]}} *)

  val relativize : root:path -> path -> path option
  (** [relativize ~root p] expresses [p] relative to [root] without
      consulting the file system. This is:
      {ul
      {- [None] if [find_prefix (normalize root) (normalize p)] is [None] or
         if the number of initial relative [..] segments is larger in
         [(normalize root)] than in [normalize p] (intuitively you can't
         come back from [root] to [p] without knowing the absolute path to
         the current working directory).}
      {- [Some q] otherwise with [q] such that
         [equal (normalize (append root q)) (normalize p) = true].}}
      Examples:
      {ul
      {- [relativize (v "/a/b") (v "c")] is [None]}
      {- [relativize (v "/a/b") (v "/c")] is [Some (v "../../c")]}
      {- [relativize (v "/a/b") (v "/c/")] is [Some (v "../../c")]}
      {- [relativize (v "/a/b") (v "/a/b/c")] is [Some (v "c")]}
      {- [relativize (v "/a/b") (v "/a/b")] is [Some (v ".")]}
      {- [relativize (v "/a/b") (v "/a/b/")] is [Some (v ".")]}
      {- [relativize (v "a/b") (v "/c")] is [None].}
      {- [relativize (v "a/b") (v "c")] is [Some (v "../../c")]}
      {- [relativize (v "a/b") (v "c/")] is [Some (v "../../c")]}
      {- [relativize (v "a/b") (v "a/b/c")] is [Some (v "c")]}
      {- [relativize (v "a/b") (v "a/b")] is [Some (v ".")]}
      {- [relativize (v "a/b") (v "a/b/")] is [Some (v ".")]}
      {- [relativize (v "../a") (v "b")] is [None]}
      {- [relativize (v "../../a") (v "../b")] is [None]}
      {- [relativize (v "../a") (v "../../b")] is [(Some "../../b")]}}
  *)

  (** {1:conversions Conversions and pretty printing} *)

  val to_string : path -> string
  (** [to_string p] is the path [p] as a string. This path can
      be safely read back by {!v}. *)

  val of_string : string -> path option
  (** [of_string s] is the string [s] as a path. [None] is returned if
      there is a ['\x00'] byte in [s] or, on Windows, if this is an
      invalid UNC path (e.g. ["\\\\"] or ["\\\\a"]). The following
      transformations are performed:
      {ul
      {- On Windows any ['/'] occurence is converted to ['\\'] before
         any processing occurs.}
      {- Non-initial empty segments are suppressed;
         ["a//b"] becomes ["a/b"], ["//a////b//"] becomes ["//a/b/"], etc.}
      {- Empty relative paths are converted to {!cur_dir}. For example
         [""] becomes ["."], ["C:"] becomes ["C:."], etc.}
      {- On Windows empty absolute UNC paths are completed to
         their root. For example ["\\\\server\\share"] becomes
         ["\\\\server\\share\\"],
         but incomplete UNC volumes like ["\\\\a"] return [None].}} *)

  val pp : Format.formatter -> path -> unit
  (** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

  (** {1:file_exts File extensions}

      The {e file extension} (resp. {e multiple file extension}) of a
      path segment is the suffix that starts at the last (resp. first)
      occurence of a ['.'] that is preceeded by at least one non ['.']
      character.  If there is no such occurence in the segment, the
      extension is empty.  With these definitions, ["."], [".."],
      ["..."] and dot files like [".ocamlinit"] or ["..ocamlinit"]
      have no extension, but [".emacs.d"] and ["..emacs.d"] do have
      one. *)

  type ext = string
  (** The type for file extensions. *)

  val ext : ?multi:bool -> path -> ext
  (** [ext p] is [p]'s last segment file extension or the empty
      string if there is no extension. If [multi] is [true] (defaults to
      [false]), returns the multiple file
      extension.
      Examples:
      {ul
      {- [ext (v "/a/b") = ""]}
      {- [ext (v "a/.") = ""]}
      {- [ext (v "a/..") = ""]}
      {- [ext (v "a/.ocamlinit") = ""]}
      {- [ext (v "/a/b.") = "."]}
      {- [ext (v "/a/b.mli") = ".mli"]}
      {- [ext (v "a.tar.gz") = ".gz"]}
      {- [ext (v "a/.emacs.d") = ".d"]}
      {- [ext ~multi:true (v "/a/b.mli") = ".mli"]}
      {- [ext ~multi:true (v "a.tar.gz") = ".tar.gz"]}
      {- [ext ~multi:true (v "a/.emacs.d") = ".d"]}} *)

  val ext_is : ext -> path -> bool
  (** [ext_is e p] is [true] iff [ext p = e || ext ~multi:true p = e].
      If [e] doesn't start with a ['.'] one is prefixed before making
      the test.
      {ul
      {- [ext_is ".mli" (v "a/b.mli")  = true]}
      {- [ext_is "mli" (v "a/b.mli")  = true]}
      {- [ext_is "mli" (v "a/bmli")  = false]}
      {- [ext_is ".tar.gz" (v "a/f.tar.gz") = true]}
      {- [ext_is "tar.gz" (v "a/f.tar.gz") = true]}
      {- [ext_is ".tar" (v "a/f.tar.gz") = false]}} *)

  val has_ext : ?multi:bool -> path -> bool
  (** [has_ext ~multi p] is [true] iff [p]'s last segment has an extension.
      If [multi] is [true] (default to [false]) returns [true] iff
      [p] has {e more than one} extension. This can be understood as:
      Examples:
      {ul
      {- [has_ext (v "a/f") = false]}
      {- [has_ext (v "a/f.") = true]}
      {- [has_ext (v "a/f.gz") = true]}
      {- [has_ext (v "a/f.tar.gz") = true]}
      {- [has_ext (v ".emacs.d") = true]}
      {- [has_ext ~multi:true (v "a/f.gz") = false]}
      {- [has_ext ~multi:true (v "a/f.tar.gz") = true]}
      {- [has_ext ~multi:true (v ".emacs.d") = false]}} *)

  val add_ext : path -> ext -> path
  (** [add_ext p ext] is [p] with the string [ext] concatenated to [p]'s
      last segment. If [ext] doesn't start with a ['.'] one is prefixed to it
      before concatenation. Examples:
      {ul
      {- [equal (add_ext (v "a/b") ".mli") (v "a/b.mli")]}
      {- [equal (add_ext (v "a/b") "mli") (v "a/b.mli")]}
      {- [equal (add_ext (v "a/f") ".tar.gz") (v "a/f.tar.gz")]}
      {- [equal (add_ext (v "a/f") "tar.gz") (v "a/f.tar.gz")]}
      {- [equal (add_ext (v "a/f.tar") ".gz") (v "a/f.tar.gz")]}
      {- [equal (add_ext (v "a/f.tar") "gz") (v "a/f.tar.gz")]}}

      @raise Invalid_argument if {!is_seg_valid}[ ext] is [false]. *)

  val rem_ext : ?multi:bool -> path -> path
  (** [rem_ext p] is [p] with the file extension of [p]'s last segment
      removed. If [multi] is [true] (default to [false]), the multiple
      file extension is removed.
      {ul
      {- [equal (rem_ext @@ v "/a/b") (v "/a/b")]}
      {- [equal (rem_ext @@ v "/a/b.mli") (v "/a/b")]}
      {- [equal (rem_ext @@ v "a/.ocamlinit") (v "a/.ocamlinit")]}
      {- [equal (rem_ext @@ v "f.tar.gz") (v "f.tar")]}
      {- [equal (rem_ext ~multi:true @@ v "f.tar.gz") (v "f")]}} *)

  val set_ext : ?multi:bool -> path -> ext -> path
  (** [set_ext ~multi p ext] is [add_ext (rem_ext ~multi p) ext].

      @raise Invalid_argument if {!is_seg_valid}[ ext] is [false]. *)

  val ( + ) : path -> ext -> path
  (** [p + ext] is [add_ext p e]. Left associative. *)

  (** {1:sets_maps Path sets and maps} *)

  type set
  (** The type for path sets *)

  (** Path sets. *)
  module Set : sig

    (** {1 Path sets} *)

    include Set.S with type elt := path
                   and type t := set

    type t = set

    val min_elt : set -> string option
    (** Exception safe {!Set.S.min_elt}. *)

    val get_min_elt : set -> string
    (** [get_min_let] is like {!min_elt} but @raise Invalid_argument
        on the empty set. *)

    val max_elt : set -> string option
    (** Exception safe {!Set.S.max_elt}. *)

    val get_max_elt : set -> string
    (** [get_max_elt] is like {!max_elt} but @raise Invalid_argument
        on the empty set. *)

    val choose : set -> string option
    (** Exception safe {!Set.S.choose}. *)

    val get_any_elt : set -> string
    (** [get_any_elt] is like {!choose} but @raise Invalid_argument on the
        empty set. *)

    val find : string -> set -> string option
    (** Exception safe {!Set.S.find}. *)

    val get : string -> set -> string
    (** [get] is like {!Set.S.find} but @raise Invalid_argument if
        [elt] is not in [s]. *)

    val of_list : string list -> set
    (** [of_list ss] is a set from the list [ss]. *)

    val pp : Format.formatter -> set -> unit
    (** [pp ppf ss] prints an unspecified representation of [ss]
        on [ppf]. *)
  end

  type +'a map
  (** The type for maps from paths to values of type ['a]. *)

  (** Path maps. *)
  module Map : sig

    (** {1 String maps} *)

    include Map.S with type key := path
                   and type 'a t := 'a map

    type 'a t = 'a map

    val min_binding : 'a map -> (path * 'a) option
    (** Exception safe {!Map.S.min_binding}. *)

    val get_min_binding : 'a map -> (path * 'a)
    (** [get_min_binding] is like {!min_binding} but @raise Invalid_argument
        on the empty map. *)

    val max_binding : 'a map -> (path * 'a) option
    (** Exception safe {!Map.S.max_binding}. *)

    val get_max_binding : 'a map -> string * 'a
    (** [get_min_binding] is like {!max_binding} but @raise Invalid_argument
        on the empty map. *)

    val choose : 'a map -> (path * 'a) option
    (** Exception safe {!Map.S.choose}. *)

    val get_any_binding : 'a map -> (path * 'a)
    (** [get_any_binding] is like {!choose} but @raise Invalid_argument
        on the empty map. *)

    val find : path -> 'a map -> 'a option
    (** Exception safe {!Map.S.find}. *)

    val get : path -> 'a map -> 'a
    (** [get k m] is like {!Map.S.find} but raises [Invalid_argument] if
        [k] is not bound in [m]. *)

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)

    val of_list : (path * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter ->
      'a map -> unit
    (** [pp ppf pp_v m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to prints the map codomain elements. *)

    val pp_string_map : Format.formatter -> path map -> unit
    (** [pp ppf m] prints an unspecified representation of [m]
        on [ppf]. *)
  end
end

(** {1 OS interaction} *)

(** OS interaction *)
module OS : sig

  (** {1 File system operations and commands} *)

  type 'a result = ('a, R.msg) R.t

  (** Path operations.

      See also {!Bos_unix.OS.Path}. *)
  module Path : sig

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
        Path.(v "data" / "$(dir)" / "$(file).txt")
]}
        will match any existing file of the form data/*/*.txt. *)

    val unify : ?init:Pat.env -> Path.t -> (path * Pat.env) list result
    (** [unify ~init pat] is like {!matches} except each
        matching path is returned with an environment mapping pattern
        variables to their matched part in the path. See {!Pat.unify}. *)

    (** {1:fold Folding over file system hierarchies} *)

    type traverse = [`All | `None | `If of path -> bool result ]
    (** The type for controlling directory traversals. The predicate of
        [`If] will only be called with directory paths (but there may
        be OS races). *)

    type elements = [ `Any | `Files | `Dirs | `Is of path -> bool result ]
    (** The type for specifying elements being folded over. *)

    type 'a fold_error = path -> 'a result -> unit result
    (** The type for managing fold errors.

        During the fold errors may be generated at different points.
        Examples are determining traversal with {!traverse},
        determining folded {!elements} or trying to [readdir(3)] a
        directory without having permissions.

        These errors are given to a function of this type. If the
        function returns [Error _] the fold stops and returns that
        error. If the function returns [`Ok ()] the path is ignored
        for the operation and the fold continues. *)

    val log_fold_error : level:Log.level -> 'a fold_error
    (** [log_fold_error level] is a {!fold_error} function that logs
        error with level [level] and always returns [`Ok ()]. *)

    val fold : ?err:'b fold_error -> ?over:elements ->
      ?traverse:traverse ->
        ('a -> path -> 'a) -> 'a -> path list -> 'a result
    (** [fold err over traverse f acc paths] folds over the list of
        paths [paths] traversing directories according to [traverse]
        (defaults to [`All]) and selecting elements to fold over
        according to [over] (defaults to [`Any]).

        [err] manages fold errors (see {!fold_error}), defaults to
        {!log_fold_error}[ ~level:Log.Error]. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:fileops File operations}

        {b Note.} When paths are {{!Path.rel}relative} they are expressed
        relative to the {{!Dir.getcwd}current working directory}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists file] is [true] iff [file] exists and is not a directory.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val dev_null : path
    (** [dev_null] is [Path.v "/dev/null"] on POSIX and [Path.v "NUL"] on
        Windows. It represents a file that discards all writes. *)

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

    val fold_lines : ('a -> string -> 'a) -> 'a -> path -> 'a result
    (** [fold_lines f acc file] is [List.fold_left f acc (read_lines p)] but
        doesn't build the list of lines in-memory. *)

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

  (** Directory operations.

      {b Note.} When paths are {{!Path.rel}relative} they are expressed
      relative to the {{!Dir.current}current working directory}. *)
  module Dir : sig

    (** {1:dirops Directory operations}

        For creating a directory see {!Bos_unix.OS.Dir}. *)

    val exists : ?err:bool -> path -> bool result
    (** [exists dir] is [true] if directory [dir] exists.
        If [err] is [true] (defaults to [false]) an error is returned
        when the file doesn't exist. *)

    val current : unit -> path result
    (** [current ()] is the current working directory. *)

    val set_current : path -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val contents : ?path:bool -> path -> path list result
    (** [contents dir] is the contents of [dir] if [path] is
        [true] (default) the basenames are prefixed by [dir].
        Elements are returned according to [kind]. *)

    (** {1:fold Folding over directory contents}

        For more details see {!Path.fold}. *)

    val fold_contents : ?err:'b Path.fold_error -> ?over:Path.elements ->
      ?traverse:Path.traverse -> ('a -> path -> 'a) -> 'a -> path -> 'a result
    (** [fold_contents err over traverse f acc d] is
        [(contents d >>= ]{!Path.fold}[ err over traverse f acc)]. *)

    val descendants : ?err:'b Path.fold_error -> ?over:Path.elements ->
      ?traverse:Path.traverse -> path -> path list result
    (** [descendants err over traverse p] is
        [(fold_contents err over traverse List.consr [])] *)
  end

  (** Executing commands.

      {1 Commands} *)
  module Cmd : sig

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
        passed to {!String.trim} before being returned. *)

    val exec_read_lines : string -> string list -> string list result
    (** [exec_readl_lines cmd args] is like [input ~trim:false cmd args] but
        the input is splitted at ['\n']. *)

    val exec_write : string -> string list -> path -> unit result
    (** [exec_write cmd args file] execute [cmd] with arguments [args] and
        writes the invocation's [stdout] to [file]. In [cmd]'s return code
        is non zero returns an error message and [file] is left intact. *)
  end

  (** {1:env Environment variables} *)

  (** Environment variables.

      To determine the process environment or set variables
      use {!Bos_unix.OS.Env}. *)
  module Env : sig

    (** {1 Lookup} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    (** {1 Typed lookup}

        See the {{!examples}examples}. *)

    val value : ?log:Log.level -> string ->
      (string -> ('a, R.msg) Rresult.result) -> absent:'a -> 'a
    (** [value ~log name parse ~absent] is:
        {ul
        {- [v] if [(Env.value name) = Some s] and [(parse s) = Ok v].}
        {- [absent] if [(Env.var name) = None] or if it is [Some s]
           and [(parse s) = Error _]. In the latter case the error
           message is logged with level [log] (defaults to
           {!Log.Error}).}} *)

    val bool : string -> (bool, R.msg) Rresult.result
    (** [bool s] is a boolean parser. The string is lowercased and
        any of [""], ["false"], ["no"], ["n"] and ["0"] is mapped
        to [false] while any if ["true"], ["yes"], ["y"] and ["1"]
        is mapped to [true]. Other string values error. *)

    val parser : string -> (string -> 'a option) ->
      (string -> ('a, R.msg) Rresult.result)
    (** [parser kind k_of_string] is an environment variable parser
        for {!value} from the [k_of_string] function.  [kind] is used
        for error reports. *)

     (** {1:examples Example}
{[
let debug = OS.Env.(value "DEBUG" bool ~absent:false)
let timeout =
  let int = OS.Env.parser "int" R.int_of_string in
  OS.Env.value "TIMEOUT" int ~absent:60
]}
*)
  end

  (** {1 POSIX time} *)

  (** POSIX time.

      See also {!Bos_unix.OS.Time}. *)
  module Time : sig

    (** {1 POSIX time} *)

    type posix_s = float
    (** The type for POSIX time in seconds.

        POSIX time counts seconds since the epoch 1970-01-01
        00:00:00 UTC. As such A POSIX timestamp is {e always} on the
        UTC time line.

        POSIX time doesn't count leap seconds, so by definition it
        cannot represent them. Whenever a leap second occurs a POSIX
        second can be two SI seconds or zero SI seconds. *)

    (** {1 Time zone offsets to UTC} *)

    type tz_offset_s = float
    (** The type for time zone offsets to UTC in seconds.

        A value of [3600.] means that we are sixty minutes ahead of UTC,
        i.e. we need to add 3600 seconds to UTC to get the local time. A
        value of [-3600.] means that we are sixty minutes behind of UTC,
        i.e. we need to subtract 3600 seconds to UTC to get the local
        time. *)
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
