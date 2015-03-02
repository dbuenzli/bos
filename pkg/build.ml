#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let unix = Env.bool "unix"

let () =
  Pkg.describe "bos" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/bos";
    Pkg.lib ~exts:Exts.library "src/bos_top";
    Pkg.lib ~cond:unix ~exts:Exts.module_library "src/bos_unix";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
