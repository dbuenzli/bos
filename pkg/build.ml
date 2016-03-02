#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "bos" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/bos";
    Pkg.lib ~exts:Exts.module_library "src/bos_setup";
    Pkg.lib ~exts:Exts.library "src/bos_top";
    Pkg.lib "src/bos_top_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
