(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Light, basic OS interaction.

    Open the module to use it, this defines only {{!path}one type},
    and modules in your scope.

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1 Basic types} *)

open Rresult
open Astring

(** Named string patterns.

    Named string patterns are strings with variables of the form
    ["$(VAR)"] where [VAR] is any sequence of bytes except [')'] or
    [',']. In a named string pattern a ["$"] litteral must be escaped
    by ["$$"].

    Named string patterns can be used to {{!format}format} strings or
    to {{!match}match} data. *)
module Pat : sig

  (** {1:pats Patterns} *)

  type t
  (** The type for patterns. *)

  val v : string -> t
  (** [v s] is a pattern from the string [s].

      @raise Invalid_argument if [s] is not a valid pattern. Use
      {!of_string} to deal with errors. *)

  val dom : t -> String.Set.t
  (** [dom p] is the set of variables in [p]. *)

  val subst : t -> (string -> string option) -> t
  (** [subst p subst] substitutes variables in [p] by the value
      they map to in [subst] (if any). See also {!subst_env}. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t -> t -> int
  (** [compare p p'] is {!Pervasives.compare}[ p p']. *)

  val of_string : ?buf:Buffer.t -> string -> (t, [> R.msg]) result
  (** [of_string ?buf s] parses [s] according to the pattern syntax
      (i.e.  a '$' will be represented by ["$$"]). [buf] can specify a
      temporary buffer to use. *)

  val to_string : ?buf:Buffer.t -> t -> string
  (** [to_string p] converts [p] to a string according to the pattern
      syntax. [buf] can specify a temporary buffer to use.  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints [p] on [ppf] according to the pattern syntax. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf p] prints [p] as a syntactically valid OCaml string on
      [ppf]. *)

  (** {1:envs Pattern environments} *)

  type env = string String.Map.t
  (** Type type for pattern environments. Maps pattern variable names
      to string values. *)

  val subst_env : t -> env -> t
  (** [subst_env p env] substitutes variables in [p] by the value
      they map to in [env]. The {{!dom}domain} of the resulting pattern
      is [String.Set.diff (dom p) (String.Map.dom env)]. *)

  val format : ?buf:Buffer.t -> ?undef:(string -> string) -> t -> env ->
    string
  (** [format p env] formats a string by substituting the variables of
      [p] with their value as found in [env]. The resulting string is
      is not in pattern syntax (a ['$'] will be represented by ['$'] in
      the result). [buf] can specify a temporary buffer to use.

      If [undef] is provided and a variable [v] of [p] is undefined in
      [env] the value of [undef v] is substituted.

      @raise Invalid_argument if [dom p] is not included in
      [String.Map.dom env] and [undef] is unspecified. *)

  (** {1:match Matching}

      Pattern variables greedily match from zero to more bytes from
      left to right. This is [.*] in regexp speak. *)

  val matches : t -> string -> bool
  (** [matches p s] is [true] iff the string [s] matches [p]. Here are a few
      examples:
      {ul
      {- [matches (v "$(mod).mli") "string.mli"] is [true].}
      {- [matches (v "$(mod).mli") "string.mli "] is [false].}
      {- [matches (v "$(mod).mli") ".mli"] is [true].}
      {- [matches (v "$(mod).$(suff)") "string.mli"] is [true].}
      {- [matches (v "$(mod).$(suff)") "string.mli "] is [true].}} *)

  val query : ?init:env -> t -> string -> env option
  (** [query ~init p s] is like {!matches} except that a matching
      string returns an environment mapping each pattern variable to
      its matched part in the string (mappings are added to [init],
      defaults to {!String.Map.empty}) or [None] if [s] doesn't match
      [p].  If a variable appears more than once in [pat] the actual
      mapping for the variable is unspecified. *)
end

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

  val dump : Format.formatter -> path -> unit
  (** [dump ppf p] prints path [p] on [ppf] using {!String.dump}. *)

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

  val add_ext : ext -> path -> path
  (** [add_ext ext p] is [p] with the string [ext] concatenated to [p]'s
      last segment. If [ext] doesn't start with a ['.'] one is prefixed to it
      before concatenation except if [ext] is [""]. Examples:
      {ul
      {- [equal (add_ext ".mli" (v "a/b")) (v "a/b.mli")]}
      {- [equal (add_ext "mli" (v "a/b")) (v "a/b.mli")]}
      {- [equal (add_ext "." (v "a/b")) (v "a/b.")]}
      {- [equal (add_ext "" (v "a/b")) (v "a/b")]}
      {- [equal (add_ext ".tar.gz" (v "a/f")) (v "a/f.tar.gz")]}
      {- [equal (add_ext "tar.gz" (v "a/f")) (v "a/f.tar.gz")]}
      {- [equal (add_ext ".gz" (v "a/f.tar") ) (v "a/f.tar.gz")]}
      {- [equal (add_ext "gz" (v "a/f.tar") ) (v "a/f.tar.gz")]}}

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

  val set_ext : ?multi:bool -> ext -> path -> path
  (** [set_ext ~multi p ext] is [add_ext ext (rem_ext ~multi p)].

      @raise Invalid_argument if {!is_seg_valid}[ ext] is [false]. *)

  val ( + ) : path -> ext -> path
  (** [p + ext] is [add_ext ext p]. Left associative. *)

  (** {1:sets_maps Path sets and maps} *)

  type set
  (** The type for path sets *)

  (** Path sets. *)
  module Set : sig

    (** {1 Path sets} *)

    include Set.S with type elt := path
                   and type t := set

    type t = set

    val min_elt : set -> path option
    (** Exception safe {!Set.S.min_elt}. *)

    val get_min_elt : set -> path
    (** [get_min_let] is like {!min_elt} but @raise Invalid_argument
        on the empty set. *)

    val max_elt : set -> path option
    (** Exception safe {!Set.S.max_elt}. *)

    val get_max_elt : set -> path
    (** [get_max_elt] is like {!max_elt} but @raise Invalid_argument
        on the empty set. *)

    val choose : set -> path option
    (** Exception safe {!Set.S.choose}. *)

    val get_any_elt : set -> path
    (** [get_any_elt] is like {!choose} but @raise Invalid_argument on the
        empty set. *)

    val find : path -> set -> path option
    (** Exception safe {!Set.S.find}. *)

    val get : path -> set -> path
    (** [get] is like {!Set.S.find} but @raise Invalid_argument if
        [elt] is not in [s]. *)

    val of_list : path list -> set
    (** [of_list ps] is a set from the list [ps]. *)

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> string -> unit) ->
      Format.formatter -> set -> unit
    (** [pp ~sep pp_elt ppf ps] formats the elements of [ps] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

    val dump : Format.formatter -> set -> unit
    (** [dump ppf ps] prints an unspecified representation of [ps] on
        [ppf]. *)
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

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> path * 'a -> unit) -> Format.formatter ->
      'a map -> unit
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val dump : (Format.formatter -> 'a -> unit) -> Format.formatter ->
      'a map -> unit
    (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)
  end
end

(** Command lines.

    For API usability reasons we represent both command lines and
    command line fragments using the same {{!t}type}. When a command
    line is {{!section:OS.Cmd.exec}executed} the first element of the
    line defines the program name and each other element is an
    argument that will be passed {e as is} in the program's [argv]
    array: no shell interpretation or any form of argument
    concatenation occurs.

    See {{!ex}examples}. *)
module Cmd : sig

  (** {1:lines Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      made of [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : path -> string
  (** [p] is {!Path.to_string}. This combinator is here to make
      path argument specification brief. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list [l]. If [slip]
      is specified it is added on the command line before each element
      of [l]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf l] formats an unspecified representation of [l] on
      [ppf]. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf l] dumps and unspecified representation of [l]
      on [ppf]. *)

  (** {1:ex Examples}
{[
let ls path = Cmd.(v "ls" % "-a" % p path)

let tar archive path = Cmd.(v "tar" % "-cvf" % p archive % p path)

let opam cmd = Cmd.(v "opam" % cmd)

let opam_install pkgs = Cmd.(opam "install" %% of_list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(v "ocamlc" % "-c" %% (on debug @@ v "-g") % p file)

let ocamlopt ?(profile = false) ?(debug = false) file =
  let profile = Cmd.(on profile @@ v "-p") in
  let debug = Cmd.(on debug @@ v "-g") in
  Cmd.(v "ocamlopt" % "-c" %% debug %% profile % p file)
]} *)
end

(** {1 OS interaction} *)

(** OS interaction *)
module OS : sig

  (** {1 Results}

      The functions of this module never raise {!Sys_error} or
      {!Unix.Unix_error} instead they turn these errors into
      {{!Rresult.R.msgs}error messages}. If you need fine grained
      control over unix errors use the lower level functions in
      {!Bos.OS.U}. *)

  type 'a result = ('a, R.msg) R.t
  (** The type for OS results. *)

  (** {1:env Environment variables and program arguments} *)

  (** Environment variables. *)
  module Env : sig

    (** {1:env Process environment} *)

    val vars : unit -> string String.Map.t result
    (** [vars ()] is a string map corresponding to the process environment. *)

    (** {1:vars Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    val set_var : string -> string option -> unit result
    (** [set_var name v] sets the environment variable [name] to [v].

        {b BUG.} The {!Unix} module doesn't bind to [unsetenv(3)],
        hence for now using [None] will not unset the variable, it
        will set it to [""]. This behaviour may change in future
        versions of the library. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name absent] is the value of the optionally defined
        environment variable [name] if defined and [absent] if
        undefined. *)

    val req_var : string -> string result
    (** [req_var name]  is the value of the environment variable [name] or
        an error if [name] is undefined in the environment. *)

    (** {1 Typed lookup}

        See the {{!examples}examples}. *)

    type 'a parser = string -> ('a, R.msg) Rresult.result
    (** The type for environment variable value parsers. *)

    val parser : string -> (string -> 'a option) -> 'a parser
    (** [parser kind k_of_string] is an environment variable value
        from the [k_of_string] function. [kind] is used for error
        reports (e.g. could be ["int"] for an [int] parser). *)

    val bool : bool parser
    (** [bool s] is a boolean parser. The string is lowercased and
        the result is:
        {ul
        {- [Ok false] if it is one of [""], ["false"], ["no"], ["n"] or ["0"].}
        {- [Ok true] if it is one of ["true"], ["yes"], ["y"] or ["1"].}
        {- An [Error] otherwise.}} *)

    val string : string parser
    (** [string s] is a string parser, it always succeeds. *)

    val path : path parser
    (** [path s] is a path parser using {!Path.of_string}. *)

    val some : 'a parser -> 'a option parser
    (** [some p] is wraps [p]'s parse result in [Some]. *)

    val value : ?log:Logs.level -> string -> 'a parser -> absent:'a -> 'a
    (** [value ~log name parse ~absent] is:
        {ul
        {- [absent] if [Env.var name = None]}
        {- [v] if [Env.var name = Some s] and [parse s = Ok v].}
        {- [absent] if [Env.var name = Some s] and [parse s = Error msg].
           In this case the error message is logged with level [log]
           (defaults to {!Log.Error})}} *)

     (** {1:examples Examples}
{[
let debug : bool = OS.Env.(value "DEBUG" bool ~absent:false)
let msg : string = OS.Env.(value "MSG" string ~absent:"no message")

let timeout : int option =
  let int = OS.Env.(some @@ parser "int" String.to_int) in
  OS.Env.value "TIMEOUT" int ~absent:None
]}
*)
  end

  (** Quick and dirty program arguments parsing.

      This is for quick hacks and scripts. If your program evolves to
      a tool for end users you should rather use {!Cmdliner} to parse
      your command lines: it generates man pages and its parsing is
      more flexible and user friendly. The syntax of command lines
      parsed by this module is a subset of what {!Cmdliner} is able to
      parse so migrating there should not be a problem for existing
      invocations of your program.

      This module supports short and long options with option
      arguments either glued to the option or specified as the next
      program argument.  It also supports the [--] program argument to
      notify that subsequent arguments have to be treated as
      positional arguments. Parsing functions always respond to the
      [-h], [-help] or [--help] flags by showing the program's usage
      and command line options documentation.

      See the {{!argbasics}basics}.

      {b Warning.} This module is not thread-safe. *)
  module Arg : sig

    (** {1 Executable name} *)

    val exec : string
    (** [exec] is the name of the executable. *)

    (** {1:conv Argument converters}

        Argument converters transform string arguments of the command
        line to OCaml values. Consult the predefined
        {{!predefconvs}converters}. *)

    type 'a parser = string -> ('a, R.msg) Rresult.result
    (** The type for option argument parsers. *)

    type 'a printer = Format.formatter -> 'a -> unit
    (** The type for converted argument printers. *)

    type 'a converter = 'a parser * 'a printer
    (** The type for argument converters. *)

    val converter : string -> (string -> 'a option) -> 'a printer ->
      'a converter
    (** [converter kind k_of_string pp_k] is a converter for values
        using the [k_of_string] function for parsing and [pp_k] for
        printing. [kind] is the kind of value and used for error
        reports (e.g. could be ["an integer"] for an [int] parser. *)

    val some : ?none:string -> 'a converter -> 'a option converter
    (** [some none c] is like the converter [c] but wraps its result
        in [Some]. This is used for command line arguments that
        default to [None] when absent. [none] is what should be printed
        by the printer for [None] (defaults to [""]). *)

    (** {1:queries Flag and option queries}

        {b Flag and option names.} They are specified without dashes.
        A one character name defines a short option; ["d"] is [-d].
        Longer names define long options ["debug"] is [--debug].

        {b Option argument specification.} On the command line, option
        arguments are either specified as the next program argument or
        glued to the option. For short options gluing is done
        directly: [-farchive.tar]. For long options an ["="] characters
        stands between the option and the value: [--file=archive.tar].

        {b Warning.} These functions are effectful invoking them twice
        on the same option names will result in parse errors.  All the
        following functions raise [Invalid_argument] if they are
        invoked after {{!section:parse}parsing}. *)

    val flag : ?doc:string -> ?env:string -> string list -> bool
    (** [flag ~doc ~env names] is [true] if one of the flags in
        [names] is present on the command line {e at most once} and
        [false] otherwise.

        If there is no flag on the command line and [env] is specified
        and defined in the environment, its value is parsed with
        {!Env.bool} and the resulting value is used. [doc] is a
        documentation string. *)

    val flag_all : ?doc:string -> ?env:string -> string list -> int
    (** [flag_all] is like {!flag} but counts the number of occurences
        of the flag on the command line. If there is no flag on the command
        line and [env] is specified and defined in the environment, its
        value is parsed with {!Env.bool} and converted to an integer. *)

    val opt : ?docv:string -> ?doc:string -> ?env:string -> string list ->
      'a converter -> absent:'a -> 'a
    (** [opt ~docv ~doc ~env names c ~absent] is a value defined by
        the value of an optional argument that may appear {e at most
        once} on the command line under one of the names specified by
        [names].

        The argument of the option is converted with [c] and [absent]
        is used if the option is absent from the command line. If
        there is no option on the command line and [env] is specified
        and defined in the environment, its value is parsed with
        [parse] and that value is used instead of [absent].

        [doc] is is a documentation string. [docv] a documentation
        meta-variable used in the documentation to stand for the option
        argument. In [doc] occurences of the substring [$(docv)] in are
        replaced by the value of [docv] *)

    val opt_all : ?docv:string -> ?doc:string -> ?env:string -> string list ->
      'a converter -> absent:'a list -> 'a list
    (** [opt_all] is like {!opt} but the optional argument can be repeated. *)

    (** {1:parse Parsing}

        {b Note.} Before parsing make sure you have invoked all the
        {{!queries}queries}.

        {b Warning.} All the following functions raise
        [Invalid_argument] if they are reinvoked after
        {{!section:parse}parsing}. *)

    val parse_opts : ?doc:string -> ?usage:string -> unit -> unit
    (** [parse_opts ()] can:
        {ul
        {- Return [()] if no command line error occured and [-help] or [--help]
           was not specified.}
        {- Never return and exit the program with [0] after having
           printed the help on {!stdout}.}
        {- Never return and exit the program with [1] after having
           printed an error on {!stderr} if a parsing
           error occured.}}

        A parsing error occurs either if an option parser failed, if a
        non repeatable option was specified more than once, if there
        is an unknown option on the line, if there is a positional
        argument on the command line (use {!parse} to parse them).
        [usage] is the command argument synopsis (default is
        automatically inferred).  [doc] is a documentation string for
        the program. *)

    val parse : ?doc:string -> ?usage:string -> pos:'a converter -> unit ->
      'a list
    (** [parse ~pos] is like {!parse_opts} but returns and converts
        the positional arguments with [pos] rather than error on them.
        Note that any thing that comes after a [--] argument on the
        command line is deemed to be a positional argument. *)

    (** {1:predefconvs Predefined argument converters} *)

    val string : string converter
    (** [string] converts a string argument. This never errors. *)

    val path : path converter
    (** [path] converts a path argument using {!Path.of_string}. *)

    val bin : Cmd.t converter
    (** [bin] is {!string} mapped by {!Cmd.v}. *)

   (* FIXME add:
    val cmd : Cmd.t converter
    (** [cmd] converts a command line with {!Cmd.of_string} *) *)

    val char : char converter
    (** [char] converts a single character. *)

    val bool : bool converter
    (** [bool] converts a boolean with {!String.to_bool}. *)

    val int : int converter
    (** [int] converts an integer with {!String.to_int}. *)

    val nativeint : nativeint converter
    (** [int] converts a [nativeint] with {!String.to_nativeint}. *)

    val int32 : int32 converter
    (** [int32] converts an [int32] with {!String.to_int32}. *)

    val int64 : int64 converter
    (** [int64] converts an [int64] with {!String.to_int64}. *)

    val float : float converter
    (** [float] converts an float with {!String.to_float}. *)

    val enum : (string * 'a) list -> 'a converter
    (** [enum l p] converts values such that string names in [l]
        map to the corresponding value of type ['a].

        {b Warning.} The type ['a] must be comparable with
        {!Pervasives.compare}.

        @raise Invalid_argument if [l] is empty. *)

    val list : ?sep:string -> 'a converter -> 'a list converter
    (** [list ~sep c] converts a list of [c]. For parsing the
        argument is first {!String.cuts}[ ~sep] and the resulting
        string list is converted using [c]. *)

    val array : ?sep:string -> 'a converter -> 'a array converter
    (** [array ~sep c] is like {!list} but returns an array instead. *)

    val pair : ?sep:string -> 'a converter -> 'b converter -> ('a * 'b)
        converter
    (** [pair ~sep fst snd] converts a pair of [fst] and [snd]. For parsing
        the argument is {!String.cut}[ ~sep] and the resulting strings
        are converted using [fst] and [snd]. *)

    (** {1:argbasics Basics}

        To parse a command line, {b first} perform all the option
        {{!queries}queries} and then invoke one of the
        {{!parse}parsing} functions. Do not invoke any query after
        parsing has been done, this will raise [Invalid_argument].
        This leads to the following program structure:
{[
(* It is possible to define things at the toplevel as follows. But do not
   abuse this. The following flag, if unspecified on the command line, can
   also be specified with the DEBUG environment variable. *)
let debug = OS.Arg.(flag ["g"; "debug"] ~env:"DEBUG" ~doc:"debug mode.")
...

let main () =
  let depth =
    OS.Arg.(opt ["d"; "depth"] int ~absent:2
              ~doc:"recurses $(docv) times." ~docv:"INT")
  in
  let pos_args = OS.Arg.(parse ~pos:string ()) in
  (* No command line error or help request occured, run the program. *)
  ...

let main () = main ()
]}
 *)

  end

  (** {1 File system operations and commands}

      {b Note.} When paths are relative they are expressed relative to
      the {{!Dir.current}current working directory}. *)

  (** Path operations. *)
  module Path : sig

    (** {1:ops Existence and move} *)

    val exists : path -> bool result
    (** [exists p] is [true] if [p] exists for the file system
        and [false] otherwise. *)

    val must_exist : path -> unit result
    (** [must_exist p] is [()] if [p] exists for the file system
        and an error otherwise. *)

    val move : ?force:bool -> path -> path -> unit result
    (** [move ~force src dst] moves path [src] to [dst]. If [force] is
        [true] (defaults to [false]) the operation doesn't error if
        [dst] exists and can be replaced by [src]. *)

    val stat : path -> Unix.stats result
    (** [stat p] is [p]'s file information. *)

    (** {1:link Path links} *)

    val link : ?force:bool -> target:path -> path -> unit result
    (** [link ~force target p] hard links [target] to [p].  If
        [force] is [true] (defaults to [false]) and [p] exists, it is
        is [rmdir]ed or [unlink]ed before making the link. *)

    val symlink : ?force:bool -> target:path -> path -> unit result
    (** [symlink ~force target p] symbolically links [target] to
        [dst]. If [force] is [true] (defaults to [false]) and [p]
        exists, it is [rmdir]ed or [unlink]ed before making the
        link.*)

    val symlink_target : path -> path result
    (** [slink_target p] is [p]'s target if [p] is a symbolic link. *)

    val symlink_stat : path -> Unix.stats result
    (** [symlink_stat p] is the same as {!stat} but if [p] is a link
        returns information about the link itself. *)

    (** {1:pathmatch Matching path patterns against the file system}

        A path pattern [pat] is a path whose segments are made of
        {{!Pat}named string patterns}. Each variable of the pattern
        greedily matches a segment or sub-segment. For example the path
        pattern:
{[
        Path.(v "data" / "$(dir)" / "$(file).txt")
]}
        matches any existing path of the file system that matches the
        regexp  [data/.*/.*\.txt].

        {b Warning.} When segments with pattern variables are matched
        against the file system they never match ["."]  and
        [".."]. For example the pattern ["$(file).$(ext)"] does not
        match ["."]. *)

    val matches : ?dotfiles:bool -> Path.t -> path list result
    (** [matches ~dotfiles pat] is the list of paths in the file
        system that match the path pattern [pat]. If [dotfiles] is
        [false] (default) paths which have at least one segment that
        starts with a ['.'] character are not part of the list. *)

    val query : ?dotfiles:bool -> ?init:Pat.env -> Path.t ->
      (path * Pat.env) list result
    (** [query ~init pat] is like {!matches} except each matching path
        is returned with an environment mapping pattern variables to
        their matched part in the path. For each path the mappings are
        added to [init] (defaults to {!String.Map.empty}). *)

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

    val log_fold_error : level:Logs.level -> 'a fold_error
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

    (** {1:paths Famous file paths} *)

    val dev_null : path
    (** [dev_null] is [Path.v "/dev/null"] on POSIX and [Path.v "NUL"] on
        Windows. It represents a file on the OS that discards all
        writes. *)

    val dash : path
    (** [dash] is [Path.v "-"]. This value is used by {{!input}input}
        and {{!output}output} functions to respectively denote [stdin]
        and [stdout].

        {b Note.} Representing [stdin] and [stdout] by this path is a
        widespread command line tool convention. However it is
        perfectly possible to have files that bear this name in the
        file system. If you need to operate on such path from the
        current directory you can simply specify them as
        [Path.(cur_dir / "-")] and so can your users on the command
        line by using ["./-"]. *)

    (** {1:ops Existence and deletion} *)

    val exists : path -> bool result
    (** [exists file] is [true] if [file] is a regular file in the
        file system and [false] otherwise.  Symbolic links are
        followed. *)

    val must_exist : path -> unit result
    (** [must_exist file] is [()] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are
        followed. *)

    val delete : ?must_exist:bool -> path -> unit result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    val truncate : path -> int -> unit result
    (** [truncate p size] truncates [p] to [s]. *)

    (** {1:input Input}

        {b Stdin.} In the following functions if the path is {!dash},
        bytes are read from [stdin]. *)

    type input = unit -> (bytes * int * int) option
    (** The type for file inputs. The function is called by the client
        to input more bytes. It returns [Some (b, pos, len)] if the
        bytes [b] can be read in the range \[[pos];[pos+len]\]; this
        byte range is immutable until the next function call.  [None]
        is returned at the end of input. *)

    val with_input : path -> (input -> 'a -> 'b result) -> 'a -> 'b result
    (** [with_input file f v] provides contents of [file] with an input
        [i] and returns [f i v]. After the function returns
        (exceptions included) a call to [i] by the client
        raises [Invalid_argument]. *)

    val with_ic : path -> (in_channel -> 'a -> 'b result) -> 'a ->
      'b result
    (** [with_ic file f v] opens [file] as a channel [ic] and returns
        [f ic v]. After the function returns (exceptions included),
        [ic] is ensured to be closed.  If [file] is {!dash}, [ic] is
        {!Pervasives.stdin} and not closed when the function
        returns. If [End_of_file] is raised in [f] turns it into
        an error message. *)

    val read : path -> string result
    (** [read file] is [file]'s content as a string. *)

    val read_lines : path -> string list result
    (** [read_lines file] is [file]'s content, split at each
        "\n" character. *)

    val fold_lines : ('a -> string -> 'a) -> 'a -> path -> 'a result
    (** [fold_lines f acc file] is like
        [List.fold_left f acc (read_lines p)]. *)

    (** {1:output Output}

        The following applies to every function in this section.

        {b Stdout.} If the path is {!dash}, bytes are written to
        [stdout].

        {b Default permission mode.} The optional [mode] argument
        specifies the permissions of the created file. It defaults to
        [0o622] (readable by everyone writeable by the user).

        {b Atomic writes.} Files are written atomically by the
        functions. They create a temporary file [t] in the directory
        of the file [f] to write, write the contents to [t] and
        renames it to [f] on success. In case of error the [t] is
        deleted and [f] left intact. *)

    type output = (bytes * int * int) option -> unit
    (** The type for file outputs. The function is called by the
        client with [Some (b, pos, len)] to output the bytes of [b] in
        the range \[[pos];[pos+len]\]. [None] is called to denote
        end of output. *)

    val with_output : ?mode:int -> path -> (output -> 'a -> 'b result) ->
      'a -> 'b result
    (** [with_output file f v] writes the contents of [file] using an
        output [o] given to [f] and returns [f o v]. After the
        function returns a call to [o] by the client raises
        [Invalid_argument]. *)

    val with_oc : ?mode:int -> path -> (out_channel -> 'a -> 'b result) ->
      'a -> 'b result
    (** [with_oc file f v] opens [file] as a channel [oc] and returns
        [f oc v]. After the function returns [oc] is closed. If [file]
        is {!dash}, [oc] is {!Pervasives.stdout} and not closed when
        the function returns. *)

    val write : ?mode:int -> path -> string -> unit result
    (** [write file content] outputs [content] to [file]. If [file]
        is {!dash}, writes to {!Pervasives.stdout}. If an error is
        returned [file] is left untouched except if {!Pervasives.stdout}
        is written. *)

    val writef : ?mode:int -> path ->
      ('a, Format.formatter, unit, unit result) format4 -> 'a
    (** [write file fmt ...] is like [write file (Format.asprintf fmt ...)]. *)

    val write_lines : ?mode:int -> path -> string list -> unit result
    (** [write_lines file lines] is like [write file (String.concat
        ~sep:"\n" lines)]. *)

    val write_subst : ?mode:int -> (string * string) list -> path -> string ->
      unit result
    (** [write_subst vars file content] outputs [content] to [file]. In
        [content] patterns of the form ["%%ID%%"] are replaced by the value
        of [List.assoc "ID" vars] (if any). If [file] is {!Path.dash}, writes
        to {!Pervasives.stdout}. If an error is returned [file] is left
        untouched except if {!Pervasives.stdout} is written.

        FIXME review that with {!Path} and {!String.Map} in mind. *)

    (** {1:tmpfiles Temporary files} *)

    type tmp_name_pat = (string -> string, Format.formatter, unit, string)
        format4
    (** The type for temporary file name patterns. The string format is
        replaced by random characters. *)

    val tmp : ?mode:int -> ?dir:path -> tmp_name_pat -> path result
    (** [tmp mode dir pat] is a new empty temporary file in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        created with permissions [mode] (defaults to [0o600] only
        readable and writable by the user). The file is deleted at the
        end of program execution using a {!Pervasives.at_exit}
        handler.

        {b Warning.} If you want to write to the file, using
        {!with_tmp_output} or {!with_tmp_oc} is more secure as it
        ensures that none replaces the file, e.g. by a symbolic link,
        between the time you create the file and open it. *)

    val with_tmp_output : ?mode:int -> ?dir:path -> tmp_name_pat ->
      (path -> output -> 'a -> 'b result) -> 'a -> 'b result
    (** [with_tmp_output dir pat f v] is a new temporary file in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        atomically created and opened with permissions [mode]
        (defaults to [0o600] only readable and writable by the
        user). Returns the value of [f file o v] with [file] the file
        path and [o] an output to write the file. After the function
        returns, calls to [o] raise [Invalid_argument] and [file] is
        deleted. *)

    val with_tmp_oc : ?mode:int -> ?dir:path -> tmp_name_pat ->
      (path -> out_channel -> 'a -> 'b result) -> 'a -> 'b result
    (** [with_tmp_oc mode dir pat f v] is a new temporary file in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        atomically created and opened with permission [mode]
        (defaults to [0x600] only readable and writable by the
        user). Returns the value of [f file oc v] with [file] the file
        path and [oc] an output channel to write the file. After the
        function returns, [oc] is closed and [file] is deleted. *) end

  (** Directory operations. *)
  module Dir : sig

    (** {1:dirops Existence, creation, deletion and contents} *)

    val exists : path -> bool result
    (** [exists dir] is [true] if [dir] is a directory in the file system
        and [false] otherwise. Symbolic links are followed. *)

    val must_exist : path -> unit result
    (** [must_exist dir] is [()] if [dir] is a directory in the file system
        and an error otherwise. Symbolic links are followed. *)

    val create : ?path:bool -> ?mode:int -> path -> unit result
    (** [create ~path ~mode dir] creates the directory [dir] with file
        permission [mode] (defaults [0o755] readable and traversable
        by everyone, writeable by the user). If [path] is [true]
        (default) intermediate directories are created with the same
        [mode], otherwise missing intermediate directories lead to an
        error. If [dir] exists, no error is returned but its
        permissions are changed to the given [mode]. *)

    val delete : ?must_exist:bool -> ?recurse:bool -> path -> unit result
    (** [delete ~must_exist ~recurse dir] deletes the directory [dir]. If
        [must_exist] is [true] (defaults to [false]) an error is returned
        if [dir] doesn't exist. If [recurse] is [true] (default to [false])
        no error occurs if the directory is non-empty: its contents is
        recursively deleted first. *)

    val contents : ?rel:bool -> path -> path list result
    (** [contents ~rel dir] is the contents of [dir].  If [rel] is
        [true] (defaults to [false]) the resulting paths are relative
        to [dir], otherwise they have [dir] prepended. See also
        {!contents_fold}. *)

    (** {1:current Current working directory} *)

    val current : unit -> path result
    (** [current ()] is the current working directory. *)

    val set_current : path -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val with_current : path -> ('a -> 'b result) -> 'a -> 'b result
    (** [with_current dir f v] is [f v] with the current working directory
        bound to [dir]. After the function returns the current working
        directory is back to its initial value. *)

    (** {1:fold Folding over directory contents}

        For more details see {!Path.fold}. *)

    val contents_fold : ?err:'b Path.fold_error -> ?over:Path.elements ->
      ?traverse:Path.traverse -> ('a -> path -> 'a) -> 'a -> path -> 'a result
    (** [contents_fold err over traverse f acc d] is
        [(contents d >>= ]{!Path.fold}[ err over traverse f acc)]. *)

    val descendants : ?err:'b Path.fold_error -> ?over:Path.elements ->
      ?traverse:Path.traverse -> path -> path list result
    (** [descendants err over traverse p] is
        [(contents_fold err over traverse (fun l p -> p :: l) [])] *)

    (** {1:tmpdirs Temporary directories} *)

    type tmp_name_pat = (string -> string, Format.formatter, unit, string)
        format4
    (** The type for temporary directory name patterns. The string format is
        replaced by random characters. *)

    val tmp : ?mode:int -> ?dir:path -> tmp_name_pat -> path result
    (** [tmp mode dir pat] is a new empty directory in [dir] (defaults
        to {!Dir.default_tmp}) named according to [pat] and created
        with permissions [mode] (defaults to [0o700] only readable and
        writable by the user). The directory path and its content is
        deleted at the end of program execution using a
        {!Pervasives.at_exit} handler. *)

    val with_tmp : ?mode:int -> ?dir:path -> tmp_name_pat ->
      (path -> 'a -> 'b result) -> 'a -> 'b result
    (** [with_tmp mode dir pat f v] is a new empty directory in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        created with permissions [mode] (defaults to [0o700] only
        readable and writable by the user). Returns the value of [f
        tmpdir v] with [tmpdir] the directory path. After the function
        returns the directory path [tmpdir] and its content is
        deleted. *)

    (** {1:defaulttmpdir Default temporary directory} *)

    val default_tmp : unit -> path
    (** [default_tmp ()] is the directory used as a default value for
        creating {{!File.tmpfiles}temporary files} and
        {{!tmpdirs}directories}. If {!set_default_tmp} hasn't been
        called this is:
        {ul
        {- On POSIX, the value of the [TMPDIR] environment variable or
           [Path.v "/tmp"] if the variable is not set or empty.}
        {- On Windows, the value [TEMP] environment variable or
           {!Path.cur_dir} if it is not set or empty}} *)

    val set_default_tmp : path -> unit
    (** [set_default_tmp p] sets the value returned by {!default_tmp} to
        [p]. *)
  end

  (** Executing command lines.

      {b Warning.} All the functions of this module raise [Invalid_argument]
      if the given command line {!is_empty}. *)
  module Cmd : sig

    (** {1:exist Command existence} *)

    val exists : Cmd.t -> bool result
    (** [exists cmd] is [true] if the executable of [cmd] can be found in
        the path and [false] otherwise. *)

    val must_exist : Cmd.t -> unit result
    (** [must_exist cmd] is [()] if the executable of [cmd] can be found
        in the path and an error otherwise. *)

    (** {1:exec Command line execution} *)

    val exec_ret : Cmd.t -> int
    (** [exec_ret l] executes command line [l] and returns the exit
        code of the invocation. *)

    val exec : Cmd.t -> unit result
    (** [exec l] executes the command line [l]. On exit code [0] returns
        [`Ok ()]. Otherwise an error message with the failed
        invocation and its exit code is returned in [`Error]. *)

    val exec_read : ?trim:bool -> Cmd.t -> string result
    (** [exec_read ~trim l] executes the command line [l] and returns
        its standard output. If the excution return code is non zero
        returns an error message. If [trim] is [true] (default) the
        contents is passed to {!String.trim} before being returned. *)

    val exec_read_lines : Cmd.t -> string list result
    (** [exec_read_lines l] is like [exec_read ~trim:false cmd args] but
        the input is splitted at ['\n']. *)

    val exec_write : Cmd.t -> path -> unit result
    (** [exec_write cmd args file] execute [cmd] with arguments [args] and
        writes the invocation's [stdout] to [file]. In [cmd]'s return code
        is non zero returns an error message and [file] is left intact. *)
  end

  (** {1 Low level {!Unix} access} *)

  (** Low level {!Unix} access.

      These functions simply {{!call}call} functions from the {!Unix}
      module and replace strings with {!path} where appropriate.  They
      also provide more fine grained error handling, for example
      {!OS.Path.stat} converts the error to a message while {!stat}
      gives you the {{!Unix.error}Unix error}. *)
  module U : sig

    (** {1 Error handling} *)

    type 'a result = ('a, [`Unix of Unix.error]) Rresult.result
    (** The type for Unix results. *)

    val pp_error : Format.formatter -> [`Unix of Unix.error] -> unit
    (** [pp_error ppf e] prints [e] on [ppf]. *)

    val open_error : 'a result -> ('a, [> `Unix of Unix.error]) Rresult.result
    (** [open_error r] allows to combine a closed unix error
        variant with other variants. *)

    val error_to_msg : 'a result -> ('a, Rresult.R.msg) Rresult.result
    (** [error_to_msg r] converts unix errors in [r] to an error message. *)

    (** {1 Wrapping {!Unix} calls} *)

    val call : ('a -> 'b) -> 'a -> 'b result
    (** [call f v] is [Ok (f v)] but {!Unix.EINTR} errors are catched
        and handled by retrying the call. Other errors [e] are catched
        aswell and returned as [Error (`Unix e)]. *)

    (** {1 File system operations} *)

    val mkdir : path -> Unix.file_perm -> unit result
    (** [mkdir] is {!Unix.mkdir}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdir.html}
        POSIX [mkdir]}. *)

    val link : path -> path -> unit result
    (** [link] is {!Unix.link}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/link.html}
        POSIX [link]}. *)

    val unlink : path -> unit result
    (** [stat] is {!Unix.unlink},
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/unlink.html}
        POSIX [unlink]}. *)

    val rename : path -> path -> unit result
    (** [rename] is {!Unix.rename}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/rename.html}
        POSIX [rename]}. *)

    val stat : path -> Unix.stats result
    (** [stat] is {!Unix.stat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html}
        POSIX [stat]}. *)

    val lstat : path -> Unix.stats result
    (** [lstat] is {!Unix.lstat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html}
        POSIX [lstat]}. *)

    val truncate : path -> int -> unit result
    (** [truncate] is {!Unix.truncate}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/truncate.html}
        POSIX [truncate]}. *)
  end

  (** {1 POSIX time} *)

  (** POSIX time TODO move this to ptime. *)
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

    (** {1 Now} *)

    val now_s : unit -> posix_s
    (** [now_s ()] is the operating system's
        {{!Bos.OS.Time.posix_s}POSIX timestamp} for the current time.

        {b Warning.} These timestamps are not monotonic they
        are subject to operating system time adjustements and can
        even go back in time. If you need to measure time spans
        in a single program run use a monotonic time source (e.g.
        {!Mtime}) *)

    (** {1 Time zone offset} *)

    val current_tz_offset_s : unit -> tz_offset_s
    (** [current_tz_offset_s ()] is the operating system's current local
        {{!Bos.OS.Time.tz_offset_min}time zone offset} to UTC in seconds. *)

    (** {1 Printing} *)

    val pp_stamp : ?human:bool -> ?tz_offset_s:tz_offset_s ->
      Format.formatter -> posix_s -> unit
    (** [pp_stamp tz_offset_min human ppf t] formats the POSIX
        timestamp [t] and time zone offset [tz_offset_min] (defaults to [0])
        according to {{:https://tools.ietf.org/html/rfc3339}RFC 3339}.

        If [human] is [true] (defaults to [false]) date and time are
        separated by a space rather than a ['T'], and a space is
        inserted betwen time and offset but this is {b not} RFC 3339
        compliant. *)

    val pp_stamp_now : ?human:bool -> Format.formatter -> unit  -> unit
    (** [pp_now human ppf ()] is
        [pp_stamp ~human ~tz_offset_s:(current_tz_offset_s ()) ppf
         (now_s ())]. *)
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
