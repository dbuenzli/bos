#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let unix = Env.bool "unix"

let () =
  Pkg.describe "bos" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/bos";
    Pkg.lib ~exts:Exts.library "src/bos_top";
    Pkg.lib "src/bos_top_init.ml";
    Pkg.lib ~cond:unix ~exts:Exts.module_library "src-unix/bos_unix";
    Pkg.lib ~exts:Exts.library "src-unix/bos_unix_top";
    Pkg.lib "src-unix/bos_unix_top_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
