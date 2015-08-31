(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Bos

let windows = Sys.os_type = "Win32"

let eqp = eq ~eq:Path.equal ~pp:Path.pp
let v = Path.v

let of_string = test "Path.{v,of_string}" @@ fun () ->
  let eq = eq_option ~eq:Path.equal ~pp:Path.pp in
  let some s = (Some (v s)) in
  eq (Path.of_string "/\x00") None;
  eq (Path.of_string "/") (Some Path.root);
  eq_bool (Path.equal (v "/") (v "/ ")) false;
  eq (Path.of_string "//") (if windows then None else some "//");
  eq (Path.of_string "/a/b/c") (some "/a/b/c");
  eq_bool (Path.equal (v "/a/b/c/") (v "/a/b/c")) false;
  eq (Path.of_string "") (some "."); (* no empty path *)
  eq (Path.of_string "a///b///////c///") (some "a/b/c/"); (* seg collapse *)
  eq (Path.of_string "a///b///////c") (some "a/b/c"); (* seg collapse *)
  if windows then begin
    eq (Path.of_string "C:\x00") None;
    eq (Path.of_string "C:") (some "C:."); (* no empty path *)
    eq (Path.of_string "C:\\") (some "C:\\");
    eq (Path.of_string "C:rel") (some "C:rel");
    eq (Path.of_string "\\\\") None;
    eq (Path.of_string "\\\\server") None;
    eq (Path.of_string "\\\\server\\") None;
    eq (Path.of_string "\\\\server\\share")
      (some "\\\\server\\share\\") (* root add *);
    eq (Path.of_string "\\\\?") None;
    eq (Path.of_string "\\\\?\\") None;
    eq (Path.of_string "\\\\?\\a") None;
    eq (Path.of_string "\\\\?\\a:") (some "\\\\?\\a:\\"); (* root add *)
    eq (Path.of_string "\\\\?\\a:\\") (some "\\\\?\\a:\\");
    eq (Path.of_string "\\\\?\\a:\\c") (some "\\\\?\\a:\\c");
    eq (Path.of_string "\\\\?\\server\\") None;
    eq (Path.of_string "\\\\?\\server\\\\") None;
    eq (Path.of_string "\\\\?\\server\\share")
      (some "\\\\?\\server\\share\\"); (* root add *)
    eq (Path.of_string "\\\\?\\server\\\\share")
      (some "\\\\?\\server\\share\\"); (* seg collapse and root add *)
    eq (Path.of_string "\\\\?\\server\\share\\")
      (some "\\\\?\\server\\share\\");
    eq (Path.of_string "\\\\?\\server\\share\\a")
      (some "\\\\?\\server\\share\\a");
    eq (Path.of_string "\\\\?\\UNC") None;
    eq (Path.of_string "\\\\?\\UNC\\") None;
    eq (Path.of_string "\\\\?\\UNC\\server") None;
    eq (Path.of_string "\\\\?\\UNC\\server\\") None;
    eq (Path.of_string "\\\\?\\UNC\\server\\\\") None;
    eq (Path.of_string "\\\\?\\UNC\\server\\share")
      (some "\\\\?\\UNC\\server\\share\\"); (* root add *)
    eq (Path.of_string "\\\\?\\UNC\\server\\share\\")
      (some "\\\\?\\UNC\\server\\share\\");
    eq (Path.of_string "\\\\?\\UNC\\server\\share\\a")
      (some "\\\\?\\UNC\\server\\share\\a");
    eq (Path.of_string "\\\\.") None;
    eq (Path.of_string "\\\\.\\") None;
    eq (Path.of_string "\\\\.\\device") (some "\\\\.\\device\\") (* root add *);
    eq (Path.of_string "\\\\.\\device\\") (some "\\\\.\\device\\");
    eq (Path.of_string "\\\\.\\device\\a") (some "\\\\.\\device\\a");
  end;
  ()

let add_seg = test "Path.add_seg" @@ fun () ->
  app_raises ~pp:Path.pp (Path.add_seg (v "a/b/c")) "a\x00o";
  app_raises ~pp:Path.pp (Path.add_seg (v "a/b/c")) "a/o";
  if windows then app_raises ~pp:Path.pp (Path.add_seg (v "a/b/c")) "a\\o";
  eqp (Path.add_seg (v "/a") "b") (v "/a/b");
  eqp (Path.add_seg (v "/a/") "b") (v "/a/b");
  eqp (Path.add_seg (v "a/b") "") (v "a/b/");
  eqp (Path.add_seg (v "a/b/") "") (v "a/b/");
  eqp (Path.add_seg (v "/a/b") "") (v "/a/b/");
  eqp (Path.add_seg (v "/a/b/") "") (v "/a/b/");
  eqp (Path.add_seg (v "/a/b/") "e") (v "/a/b/e");
  eqp (Path.add_seg (v "/a/b") "e") (v "/a/b/e");
  eqp (Path.add_seg (v "/") "") (v "/");
  eqp (Path.add_seg (v "/") "a") (v "/a");
  ()

let append = test "Path.append" @@ fun () ->
  eqp (Path.append (v "/a/b/") (v "e/f")) (v "/a/b/e/f");
  eqp (Path.append (v "/a/b") (v "e/f")) (v "/a/b/e/f");
  eqp (Path.append (v "/a/b/") (v "/e/f")) (v "/e/f");
  eqp (Path.append (v "a/b/") (v "e/f")) (v "a/b/e/f");
  eqp (Path.append (v "bla") (v "/bli")) (v "/bli");
  if not windows then eqp (Path.append (v "bla") (v "//bli")) (v "//bli");
  if windows then begin
    eqp (Path.append (v "a/b") (v "C:e")) (v "C:e");
    eqp (Path.append (v "C:") (v "blu")) (v "C:.\\blu");
    eqp (Path.append (v "C:bla") (v "blu")) (v "C:bla/blu");
    eqp (Path.append (v "C:\\bla") (v "blu")) (v "C:\\bla\\blu");
    eqp (Path.append (v "C:\\bla") (v "\\blu")) (v "\\blu");
    eqp (Path.append (v "\\\\srv\\share\\a") (v "b"))
      (v "\\\\srv\\share\\a\\b");
    eqp (Path.append (v "\\\\srv\\share\\a\\") (v "b"))
      (v "\\\\srv\\share\\a\\b");
  end;
  ()

let constants = test "Constants" @@ fun () ->
  eq_str Path.dir_sep (if windows then "\\" else "/");
  eqp Path.cur_dir (v ".");
  eqp Path.par_dir (v "..");
  ()

let is_seg_valid = test "Path.is_seg_valid" @@ fun () ->
  eq_bool (Path.is_seg_valid "abc") true;
  eq_bool (Path.is_seg_valid "ab/c") false;
  eq_bool (Path.is_seg_valid "ab\x00c") false;
  if windows then eq_bool (Path.is_seg_valid "ab\\c") false;
  ()

let is_abs_rel = test "Path.is_abs_rel" @@ fun () ->
  let is_abs bool p =
    let p = v p in
    eq_bool (Path.is_abs p) bool;
    eq_bool (Path.is_rel p) (not bool);
  in
  is_abs true "/a/b/c";
  if not windows then is_abs true "//a/b/c";
  is_abs false ".";
  is_abs false "..";
  is_abs false "../";
  is_abs false "a";
  is_abs false "a/b";
  is_abs true "/";
  if windows then begin
    is_abs false "C:.";
    is_abs true "C:\\";
    is_abs true "C:/";
    is_abs false "C:bli/bla";
    is_abs false "C:bli/bla";
    is_abs false  "C:rel";
    is_abs true "\\\\server\\share\\";
    is_abs true "\\\\?\\a:\\";
    is_abs true "\\\\?\\a:\\c";
    is_abs true "\\\\?\\server\\share\\";
    is_abs true "\\\\?\\server\\share\\a";
    is_abs true "\\\\?\\UNC\\server\\share\\";
    is_abs true "\\\\?\\UNC\\server\\share\\a";
    is_abs true "\\\\.\\device\\";
    is_abs true "\\\\.\\device\\a";
  end;
  ()

let is_prefix = test "Path.is_prefix" @@ fun () ->
  eq_bool (Path.is_prefix (v "/a/b") (v "/a/b")) true;
  eq_bool (Path.is_prefix (v "/a/b") (v "/a/b/")) true;
  eq_bool (Path.is_prefix (v "/a/b") (v "/a/bc")) false;
  eq_bool (Path.is_prefix (v "/a/b") (v "/a/b/c")) true;
  eq_bool (Path.is_prefix (v "/a/b/") (v "/a/b/c")) true;
  eq_bool (Path.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Path.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Path.is_prefix (v "a/b") (v "/a/b")) false;
  eq_bool (Path.is_prefix (v "abcd/") (v "abcd")) false;
  eq_bool (Path.is_prefix (v "abcd") (v "abcd/bla")) true;
  if not windows then begin
    eq_bool (Path.is_prefix (v "//a/b") (v "/a/b")) false
  end;
  if windows then begin
    eq_bool (Path.is_prefix (v "C:a") (v "a")) false;
  end;
  ()

let split_volume = test "Path.split_volume" @@ fun () ->
  let eq_split p vol q =
    let p = v p in
    let vol', q' = Path.split_volume p in
    eq_str vol vol';
    eqp (v q) q';
    eqp (v (vol' ^ (Path.to_string q'))) p
  in
  eq_split "/bla" "" "/bla";
  eq_split "bla" "" "bla";
  eq_split "bla/a" "" "bla/a";
  eq_split "bla/a/" "" "bla/a/";
  if not windows then begin
    eq_split "//" "/" "/";
    eq_split "//a/b/c" "/" "/a/b/c";
    eq_split "//a/b/c/" "/" "/a/b/c/";
  end;
  if windows then begin
    eq_split "C:." "C:" ".";
    eq_split "C:\\" "C:" "\\";
    eq_split "C:\\a" "C:" "\\a";
    eq_split "C:rel" "C:" "rel";
    eq_split "\\\\server\\share\\" "\\\\server\\share" "\\";
    eq_split "\\\\server\\share\\a" "\\\\server\\share" "\\a";
    eq_split "\\\\?\\a:\\" "\\\\?\\a:" "\\";
    eq_split "\\\\?\\a:\\c" "\\\\?\\a:" "\\c";
    eq_split "\\\\?\\server\\share\\" "\\\\?\\server\\share" "\\";
    eq_split "\\\\?\\server\\share\\a" "\\\\?\\server\\share" "\\a";
    eq_split "\\\\?\\UNC\\server\\share\\" "\\\\?\\UNC\\server\\share" "\\";
    eq_split "\\\\?\\UNC\\server\\share\\a" "\\\\?\\UNC\\server\\share" "\\a";
    eq_split "\\\\.\\device\\" "\\\\.\\device" "\\";
    eq_split "\\\\.\\device\\a" "\\\\.\\device" "\\a";
  end;
  ()

let segs = test "Path.segs" @@ fun () ->
  let eq = eq_list ~eq:(=) ~pp:pp_str in
  eq (Path.segs @@ v "/a/b/") [""; "a"; "b"; ""];
  eq (Path.segs @@ v "/a/b") [""; "a"; "b"];
  eq (Path.segs @@ v "a/b/") ["a"; "b"; ""];
  eq (Path.segs @@ v "a/b") ["a"; "b"];
  eq (Path.segs @@ v "a") ["a"];
  eq (Path.segs @@ v "/") [""; ""];
  eq (Path.segs @@ v "/a/b/c") [""; "a"; "b"; "c"];
  eq (Path.segs @@ v "/a/b/c/") [""; "a"; "b"; "c"; ""];
  eq (Path.segs @@ v "a/b/c") ["a"; "b"; "c";];
  eq (Path.segs @@ v "a/b/c/") ["a"; "b"; "c"; ""];
  if not windows then begin
    eq (Path.segs @@ v "//") [""; ""];
    eq (Path.segs @@ v "//a/b") [""; "a"; "b"];
  end;
  if windows then begin
    eq (Path.segs @@ v "C:\\bla") [""; "bla"];
    eq (Path.segs @@ v "C:bla") ["bla"];
    eq (Path.segs @@ v "\\\\Server\\share\\bla") [""; "bla"];
    eq (Path.segs @@ v "\\\\?\\C:\\bla") ["";"bla"];
    eq (Path.segs @@ v "\\\\?\\Server\\share\\bla") [""; "bla"];
    eq (Path.segs @@ v "\\\\?\\UNC\\Server\\share\\bla") [""; "bla"];
    eq (Path.segs @@ v "\\\\.\\dev\\bla") [""; "bla"];
    eq (Path.segs @@ v "\\a") [""; "a"];
    eq (Path.segs @@ v "\\a\\b") [""; "a"; "b"];
    eq (Path.segs @@ v "\\a\\b\\") [""; "a"; "b"; ""];
    eq (Path.segs @@ v "C:.") ["."];
    eq (Path.segs @@ v "C:\\") ["";""];
    eq (Path.segs @@ v "C:\\a") ["";"a"];
    eq (Path.segs @@ v "C:rel") ["rel";];
    eq (Path.segs @@ v "\\\\server\\share\\") [""; ""];
    eq (Path.segs @@ v "\\\\server\\share\\a") [""; "a"];
    eq (Path.segs @@ v "\\\\?\\a:\\") [""; ""];
    eq (Path.segs @@ v "\\\\?\\a:\\c") [""; "c"];
    eq (Path.segs @@ v "\\\\?\\server\\share\\") [""; ""];
    eq (Path.segs @@ v "\\\\?\\server\\share\\a") [""; "a"];
    eq (Path.segs @@ v "\\\\?\\UNC\\server\\share\\") [""; ""];
    eq (Path.segs @@ v "\\\\?\\UNC\\server\\share\\a") [""; "a"];
    eq (Path.segs @@ v "\\\\.\\device\\") ["";""];
    eq (Path.segs @@ v "\\\\.\\device\\a") ["";"a"];
    eq (Path.segs @@ v "\\\\server\\share\\a") ["";"a"];
    eq (Path.segs @@ v "C:a") ["a"];
    eq (Path.segs @@ v "C:\\a") ["";"a"];
  end;
  ()

let filename = test "Path.filename" @@ fun () ->
  eq_str (Path.filename @@ v "/a/b/") "";
  eq_str (Path.filename @@ v "/a/b") "b";
  eq_str (Path.filename @@ v "a") "a";
  eq_str (Path.filename @@ v "/") "";
  if not windows then begin
    eq_str (Path.filename @@ v "//") "";
    eq_str (Path.filename @@ v "//a/b") "b";
    eq_str (Path.filename @@ v "//a/b/") "";
  end;
  if windows then begin
    eq_str (Path.filename @@ v "\\\\server\\share\\a") "a";
    eq_str (Path.filename @@ v "\\\\.\\device\\") "";
    eq_str (Path.filename @@ v "\\\\.\\device\\a") "a";
    eq_str (Path.filename @@ v "C:\\") "";
    eq_str (Path.filename @@ v "C:a") "a";
  end;
  ()

let base = test "Path.base" @@ fun () ->
  eqp (Path.base @@ v "/a/b/") (v "b");
  eqp (Path.base @@ v "/a/b") (v "b");
  eqp (Path.base @@ v "a") (v "a");
  eqp (Path.base @@ v "a/") (v "a");
  eqp (Path.base @@ v "ab") (v "ab");
  eqp (Path.base @@ v "ab/") (v "ab");
  eqp (Path.base @@ v ".") (v ".");
  eqp (Path.base @@ v "..") (v "..");
  eqp (Path.base @@ v "/") (v "/");
  if not windows then begin
    eqp (Path.base @@ v "//") (v "//");
    eqp (Path.base @@ v "//a/b") (v "b");
    eqp (Path.base @@ v "//a/b/") (v "b");
  end;
  if windows then begin
    eqp (Path.base @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Path.base @@ v "\\\\server\\share\\a") (v "a");
    eqp (Path.base @@ v "\\\\server\\share\\a\\") (v "a");
    eqp (Path.base @@ v "C:\\") (v "C:\\");
    eqp (Path.base @@ v "C:\\") (v "C:\\");
    eqp (Path.base @@ v "C:\\a\\") (v "a");
    eqp (Path.base @@ v "C:\\a") (v "a");
    eqp (Path.base @@ v "C:a\\") (v "a");
  end;
  ()

let parent = test "Path.parent" @@ fun () ->
  eqp (Path.parent @@ v "/a/b") (v "/a");
  eqp (Path.parent @@ v "/a/b/") (v "/a");
  eqp (Path.parent @@ v "/a") (v "/");
  eqp (Path.parent @@ v "/a/") (v "/");
  eqp (Path.parent @@ v "a/b/") (v "a");
  eqp (Path.parent @@ v "a/b") (v "a");
  eqp (Path.parent @@ v "a") (v ".");
  eqp (Path.parent @@ v "a/") (v ".");
  eqp (Path.parent @@ v ".") (v ".");
  eqp (Path.parent @@ v "..") (v ".");
  eqp (Path.parent @@ v "/") (v "/");
  eqp (Path.parent @@ v "/aab") (v "/");
  if not windows then begin
    eqp (Path.parent @@ v "//") (v "//");
    eqp (Path.parent @@ v "//a/b") (v "//a");
    eqp (Path.parent @@ v "//a/b/") (v "//a");
    eqp (Path.parent @@ v "//a") (v "//");
    eqp (Path.parent @@ v "//abcd") (v "//");
  end;
  if windows then begin
    eqp (Path.parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Path.parent @@ v "C:a") (v "C:.");
    eqp (Path.parent @@ v "C:\\") (v "C:\\");
  end;
  ()

let file_to_dir = test "Path.file_to_dir" @@ fun () ->
  eqp (Path.file_to_dir @@ v "/a/b") (v "/a/b/");
  eqp (Path.file_to_dir @@ v "/a/b/") (v "/a/b/");
  eqp (Path.file_to_dir @@ v "a") (v "a/");
  eqp (Path.file_to_dir @@ v "a/") (v "a/");
  eqp (Path.file_to_dir @@ v "/") (v "/");
  if not windows then begin
    eqp (Path.file_to_dir @@ v "//") (v "//");
    eqp (Path.file_to_dir @@ v "//a") (v "//a/");
  end;
  if windows then begin
    eqp (Path.file_to_dir @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Path.file_to_dir @@ v "C:a") (v "C:a/");
    eqp (Path.file_to_dir @@ v "C:\\") (v "C:\\");
  end;
  ()

let dir_to_file = test "Path.dir_to_file" @@ fun () ->
  eqp (Path.dir_to_file @@ v "/a/b") (v "/a/b");
  eqp (Path.dir_to_file @@ v "/a/b/") (v "/a/b");
  eqp (Path.dir_to_file @@ v "a") (v "a");
  eqp (Path.dir_to_file @@ v "a/") (v "a");
  eqp (Path.dir_to_file @@ v "/") (v "/");
  if not windows then begin
    eqp (Path.dir_to_file @@ v "//") (v "//");
    eqp (Path.dir_to_file @@ v "//a") (v "//a");
    eqp (Path.dir_to_file @@ v "//a/") (v "//a");
  end;
  if windows then begin
    eqp (Path.dir_to_file @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Path.dir_to_file @@ v "\\\\server\\share\\a\\")
      (v "\\\\server\\share\\a");
    eqp (Path.dir_to_file @@ v "C:a") (v "C:a");
    eqp (Path.dir_to_file @@ v "C:a/") (v "C:a");
    eqp (Path.dir_to_file @@ v "C:\\") (v "C:\\");
  end;
  ()

let find_prefix = test "Path.find_prefix" @@ fun () ->
  let eq = eq_option ~eq:Path.equal ~pp:Path.pp in
  eq (Path.find_prefix (v "a/b/c") (v "a/b/d")) (Some (v "a/b/"));
  eq (Path.find_prefix (v "a/b/c") (v "a/b/cd")) (Some (v "a/b/"));
  eq (Path.find_prefix (v "/a/b/c") (v "/a/b/d")) (Some (v "/a/b/"));
  eq (Path.find_prefix (v "a/b") (v "e/f")) (Some (v "."));
  eq (Path.find_prefix (v "/a/b") (v "/e/f")) (Some (v "/"));
  eq (Path.find_prefix (v "/a/b") (v "e/f")) None;
  eq (Path.find_prefix (v "a/b") (v "/e/f")) None;
  eq (Path.find_prefix (v "ab") (v "abc")) (Some (v "."));
  eq (Path.find_prefix (v "ab") (v "ab")) (Some (v "ab"));
  eq (Path.find_prefix (v "/") (v "/")) (Some (v "/"));
  eq (Path.find_prefix (v "a/") (v "a")) (Some (v "a"));
  eq (Path.find_prefix (v "abc/") (v "abc")) (Some (v "abc"));
  eq (Path.find_prefix (v "abcd/") (v "abc")) (Some (v "."));
  eq (Path.find_prefix (v "a/") (v "a/a")) (Some (v "a/"));
  if not windows then begin
    eq (Path.find_prefix (v "//") (v "/a/b")) None;
    eq (Path.find_prefix (v "//a/b/c") (v "/")) None;
    eq (Path.find_prefix (v "//a/b/c") (v "//")) (Some (v "//"));
    eq (Path.find_prefix (v "//a/b") (v "/a/b")) None;
    eq (Path.find_prefix (v "//") (v "/")) None;
    eq (Path.find_prefix (v "//a/c") (v "/a/b")) None;
    eq (Path.find_prefix (v "//a/c") (v "a/b")) None;
  end;
  if windows then begin
    eq (Path.find_prefix (v "C:\\a") (v "\\a")) None;
    eq (Path.find_prefix (v "C:\\a") (v "C:\\a")) (Some (v "C:\\a"));
    eq (Path.find_prefix (v "C:a") (v "C:a")) (Some (v "C:a"));
    eq (Path.find_prefix (v "C:a") (v "C:b")) (Some (v "C:."));
    eq (Path.find_prefix (v "C:a") (v "C:b/c")) (Some (v "C:."));
    eq (Path.find_prefix (v "C:a/f") (v "C:b/c")) (Some (v "C:."));
    eq (Path.find_prefix (v "C:a/f") (v "C:/b/c")) None;
    eq (Path.find_prefix (v "C:\\") (v "C:\\")) (Some (v "C:\\"));
    eq (Path.find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\"))
      (Some (v "\\\\server\\share\\"));
    eq (Path.find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\a"))
      (Some (v "\\\\server\\share\\"));
    eq (Path.find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\a"))
      (Some (v "\\\\server\\share\\a"));
    eq (Path.find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\b"))
      (Some (v "\\\\server\\share\\"));
  end;
  ()

let rem_prefix = test "Path.rem_prefix" @@ fun () ->
  let eq = eq_option ~eq:Path.equal ~pp:Path.pp in
  eq (Path.rem_prefix (v "/a/b") (v "/a/bc")) None;
  eq (Path.rem_prefix (v "/a/b") (v "/a/b")) (Some (v "."));
  eq (Path.rem_prefix (v "/a/b/") (v "/a/b")) None;
  eq (Path.rem_prefix (v "/a/b") (v "/a/b/")) (Some (v "."));
  eq (Path.rem_prefix (v "/a/b/") (v "/a/b/")) (Some (v "."));
  eq (Path.rem_prefix (v "/a/b") (v "/a/b/c")) (Some (v "c"));
  eq (Path.rem_prefix (v "/a/b/") (v "/a/b/c")) (Some (v "c"));
  eq (Path.rem_prefix (v "a") (v "a/b/c")) (Some (v "b/c"));
  ()

let normalize = test "Path.normalize" @@ fun () ->
  eqp (Path.normalize (v ".")) (v ".");
  eqp (Path.normalize (v "././.")) (v ".");
  eqp (Path.normalize (v "./././")) (v ".");
  eqp (Path.normalize (v "./a/..")) (v ".");
  eqp (Path.normalize (v "./a/../")) (v ".");
  eqp (Path.normalize (v "..")) (v "..");
  eqp (Path.normalize (v "../../../a")) (v "../../../a");
  eqp (Path.normalize (v "../../../a/")) (v "../../../a");
  eqp (Path.normalize (v "/")) (v "/");
  eqp (Path.normalize (v "/.")) (v "/");
  eqp (Path.normalize (v "/..")) (v "/");
  eqp (Path.normalize (v "/./../../.")) (v "/");
  eqp (Path.normalize (v "/./../../.")) (v "/");
  eqp (Path.normalize (v "../../a/..")) (v "../..");
  eqp (Path.normalize (v "../../a/../.")) (v "../..");
  eqp (Path.normalize (v "../../a/.././..")) (v "../../..");
  eqp (Path.normalize (v "../../a/../..")) (v "../../..");
  eqp (Path.normalize (v "/a/b/c/./../../g")) (v "/a/g");
  eqp (Path.normalize (v "./a/b/c/./../../g")) (v "a/g");
  eqp (Path.normalize (v "./a/b/c/./../../g/")) (v "a/g");
  eqp (Path.normalize (v "a/b/c/./../../g")) (v "a/g");
  eqp (Path.normalize (v "a/b/c/./../../g/")) (v "a/g");
  if not windows then begin
    eqp (Path.normalize (v "//a/b/c/./../../g")) (v "//a/g");
  end;
  if windows then begin
    eqp (Path.normalize (v "C:/a/b/c/./../../g")) (v "C:/a/g");
    eqp (Path.normalize (v "C:/a/b/c/./../../g")) (v "C:/a/g");
    eqp (Path.normalize (v "\\\\?\\UNC\\server\\share\\.."))
           (v "\\\\?\\UNC\\server\\share\\");
  end;
  ()

let rooted = test "Path.rooted" @@ fun () ->
  let eq = eq_option ~eq:Path.equal ~pp:Path.pp in
  eq (Path.rooted (v "/a/b") (v "c")) (Some (v "/a/b/c"));
  eq (Path.rooted (v "/a/b") (v "/a/b/c")) (Some (v "/a/b/c"));
  eq (Path.rooted (v "/a/b") (v "/a/b/c/")) (Some (v "/a/b/c"));
  eq (Path.rooted (v "/a/b") (v "/a/b/c/.")) (Some (v "/a/b/c"));
  eq (Path.rooted (v "/a/b") (v "../c")) None;
  eq (Path.rooted (v "a/b") (v "c")) (Some (v "a/b/c"));
  eq (Path.rooted (v "a/b") (v "/c")) None;
  eq (Path.rooted (v "a/b") (v "../c")) None;
  eq (Path.rooted (v "a/b") (v "c/..")) (Some (v "a/b"));
  eq (Path.rooted (v "a/b") (v "c/../..")) None;
  eq (Path.rooted (v "a/b") (v "c/d/../..")) (Some (v "a/b"));
  eq (Path.rooted (v "../../a") (v "a")) (Some (v "../../a/a"));
  eq (Path.rooted (v "../../a") (v "a/..")) (Some (v "../../a"));
  eq (Path.rooted (v "../../a") (v "../../b")) None;
  eq (Path.rooted (v "../../a") (v "../../a")) (None);
  ()

let relativize = test "Path.relativize" @@ fun () ->
  let eq_opt = eq_option ~eq:Path.equal ~pp:Path.pp in
  let relativize root p result = match Path.relativize root p with
  | None -> eq_opt None result
  | Some rp as r ->
      eq_opt r result;
      eqp (Path.normalize (Path.append root rp)) (Path.normalize p);
  in
  relativize (v "/a/b") (v "c") None;
  relativize (v "/a/b") (v "/c") (Some (v "../../c"));
  relativize (v "/a/b") (v "/c/") (Some (v "../../c"));
  relativize (v "/a/b") (v "/a/b/c") (Some (v "c"));
  relativize (v "/a/b") (v "/a/b") (Some (v "."));
  relativize (v "/a/b") (v "/a/b/") (Some (v "."));
  relativize (v "/a/b/c") (v "/d/e/f") (Some (v "../../../d/e/f"));
  relativize (v "/a/b/c") (v "/a/b/d") (Some (v "../d"));
  relativize (v "a/b") (v "/c") None;
  relativize (v "a/b") (v "c") (Some (v "../../c"));
  relativize (v "a/b") (v "c/") (Some (v "../../c"));
  relativize (v "a/b") (v "a/b/c") (Some (v "c"));
  relativize (v "a/b") (v "a/b") (Some (v "."));
  relativize (v "a/b") (v "a/b/") (Some (v "."));
  relativize (v "../a") (v "b") None;
  relativize (v "../../a") (v "../b") None;
  relativize (v "../a") (v "../../b") (Some (v "../../b"));
  relativize (v "a") (v "../../b") (Some (v "../../../b"));
  relativize (v "a/c") (v "../../b") (Some (v "../../../../b"));
  ()

let ext = test "Path.ext" @@ fun () ->
  eq_str (Path.ext @@ v ".") "";
  eq_str (Path.ext @@ v "..") "";
  eq_str (Path.ext @@ v "...") "";
  eq_str (Path.ext @@ v "....") "";
  eq_str (Path.ext @@ v ".....") "";
  eq_str (Path.ext @@ v ".a") "";
  eq_str (Path.ext @@ v ".a.") ".";
  eq_str (Path.ext @@ v ".a..") ".";
  eq_str (Path.ext @@ v ".a...") ".";
  eq_str (Path.ext @@ v ".a....") ".";
  eq_str (Path.ext @@ v "a/...") "";
  eq_str (Path.ext @@ v "a/.") "";
  eq_str (Path.ext @@ v "a/..") "";
  eq_str (Path.ext @@ v "a/.a") "";
  eq_str (Path.ext @@ v "a/..b") "";
  eq_str (Path.ext @@ v "a/..b.a") ".a";
  eq_str (Path.ext @@ v "a/..b..ac") ".ac";
  eq_str (Path.ext @@ v "/a/b") "";
  eq_str (Path.ext @@ v "/a/b.") ".";
  eq_str (Path.ext @@ v "a/.ocamlinit") "";
  eq_str (Path.ext @@ v "a/.emacs.d") ".d";
  eq_str (Path.ext @@ v "/a/b.mli") ".mli";
  eq_str (Path.ext @@ v "a.tar.gz") ".gz";
  eq_str (Path.ext @@ v "./a.") ".";
  eq_str (Path.ext @@ v "./a..") ".";
  eq_str (Path.ext @@ v "./.a.") ".";
  eq_str (Path.ext @@ v "./.a..") ".";
  eq_str (Path.ext ~multi:true @@ v ".") "";
  eq_str (Path.ext ~multi:true @@ v "..") "";
  eq_str (Path.ext ~multi:true @@ v "...") "";
  eq_str (Path.ext ~multi:true @@ v "....") "";
  eq_str (Path.ext ~multi:true @@ v ".....") "";
  eq_str (Path.ext ~multi:true @@ v ".a") "";
  eq_str (Path.ext ~multi:true @@ v ".a.") ".";
  eq_str (Path.ext ~multi:true @@ v ".a..") "..";
  eq_str (Path.ext ~multi:true @@ v ".a...") "...";
  eq_str (Path.ext ~multi:true @@ v ".a....") "....";
  eq_str (Path.ext ~multi:true @@ v "a/...") "";
  eq_str (Path.ext ~multi:true @@ v "a/.a") "";
  eq_str (Path.ext ~multi:true @@ v "a/..") "";
  eq_str (Path.ext ~multi:true @@ v "a/..b") "";
  eq_str (Path.ext ~multi:true @@ v "a/..b.a") ".a";
  eq_str (Path.ext ~multi:true @@ v "a/..b..ac") "..ac";
  eq_str (Path.ext ~multi:true @@ v "a/.emacs.d") ".d";
  eq_str (Path.ext ~multi:true @@ v "/a/b.mli") ".mli";
  eq_str (Path.ext ~multi:true @@ v "a.tar.gz") ".tar.gz";
  eq_str (Path.ext ~multi:true @@ v "./a.") ".";
  eq_str (Path.ext ~multi:true @@ v "./a..") "..";
  eq_str (Path.ext ~multi:true @@ v "./.a.") ".";
  eq_str (Path.ext ~multi:true @@ v "./.a..") "..";
  ()

let ext_is = test "Path.ext_is" @@ fun () ->
  eq_bool (Path.ext_is "." @@ v ".") false;
  eq_bool (Path.ext_is "." @@ v "..") false;
  eq_bool (Path.ext_is "." @@ v "...") false;
  eq_bool (Path.ext_is "." @@ v "...a") false;
  eq_bool (Path.ext_is "." @@ v "...a.") true;
  eq_bool (Path.ext_is "." @@ v "...a..") true;
  eq_bool (Path.ext_is "" @@ v ".") false;
  eq_bool (Path.ext_is "" @@ v "..") false;
  eq_bool (Path.ext_is "" @@ v "...") false;
  eq_bool (Path.ext_is "" @@ v "...a") false;
  eq_bool (Path.ext_is "" @@ v "...a.") true;
  eq_bool (Path.ext_is "" @@ v "...a..") true;
  eq_bool (Path.ext_is ".." @@ v ".") false;
  eq_bool (Path.ext_is ".." @@ v "..") false;
  eq_bool (Path.ext_is ".." @@ v "..a.") false;
  eq_bool (Path.ext_is ".." @@ v "..a..") true;
  eq_bool (Path.ext_is ".." @@ v "...") false;
  eq_bool (Path.ext_is ".." @@ v "...a.") false;
  eq_bool (Path.ext_is ".." @@ v "...a..") true;
  eq_bool (Path.ext_is "..." @@ v "..") false;
  eq_bool (Path.ext_is "..." @@ v "...") false;
  eq_bool (Path.ext_is "..." @@ v "....") false;
  eq_bool (Path.ext_is "..." @@ v ".a...") true;
  eq_bool (Path.ext_is ".mli" @@ v "a/b.mli") true;
  eq_bool (Path.ext_is "mli" @@ v "a/b.mli") true;
  eq_bool (Path.ext_is "mli" @@ v "a/bmli") false;
  eq_bool (Path.ext_is "mli" @@ v "a/.mli") false;
  eq_bool (Path.ext_is ".tar.gz" @@ v "a/f.tar.gz") true;
  eq_bool (Path.ext_is "tar.gz" @@ v "a/f.tar.gz") true;
  eq_bool (Path.ext_is "tar.gz" @@ v "a/ftar.gz") false;
  eq_bool (Path.ext_is "tar.gz" @@ v "a/tar.gz") false;
  eq_bool (Path.ext_is "tar.gz" @@ v "a/.tar.gz") false;
  eq_bool (Path.ext_is ".tar" @@ v "a/f.tar.gz") false;
  eq_bool (Path.ext_is ".ocamlinit" @@ v ".ocamlinit") false;
  eq_bool (Path.ext_is ".ocamlinit" @@ v "..ocamlinit") false;
  eq_bool (Path.ext_is "..ocamlinit" @@ v "...ocamlinit") false;
  eq_bool (Path.ext_is "..ocamlinit" @@ v ".a..ocamlinit") true;
  eq_bool (Path.ext_is "..a" @@ v "..") false;
  ()

let has_ext = test "Path.has_ext" @@ fun () ->
  eq_bool (Path.has_ext @@ v "a/f") false;
  eq_bool (Path.has_ext @@ v "a/f.") true;
  eq_bool (Path.has_ext @@ v "a/f.gz") true;
  eq_bool (Path.has_ext @@ v "a/f.tar.gz") true;
  eq_bool (Path.has_ext ~multi:true @@ v "a/f") false;
  eq_bool (Path.has_ext ~multi:true @@ v "a/f.") false;
  eq_bool (Path.has_ext ~multi:true @@ v "a/f.gz") false;
  eq_bool (Path.has_ext ~multi:true @@ v "a/f.tar.gz") true;
  eq_bool (Path.has_ext ~multi:true @@ v "a/.a..") true;
  eq_bool (Path.has_ext ~multi:true @@ v "a/.a.") false;
  ()

let add_ext = test "Path.add_ext" @@ fun () ->
  eqp (Path.add_ext ".mli" (v "a/b")) (v "a/b.mli");
  eqp (Path.add_ext "mli" (v "a/b")) (v "a/b.mli");
  eqp (Path.add_ext "" (v "a/b")) (v "a/b");
  eqp (Path.add_ext "." (v "a/b")) (v "a/b.");
  eqp (Path.add_ext ".tar.gz" (v "a/f")) (v "a/f.tar.gz");
  eqp (Path.add_ext "tar.gz" (v "a/f")) (v "a/f.tar.gz");
  eqp (Path.add_ext ".gz" (v "a/f.tar")) (v "a/f.tar.gz");
  eqp (Path.add_ext "gz" (v "a/f.tar")) (v "a/f.tar.gz");
  ()

let rem_ext = test "Path.rem_ext" @@ fun () ->
  eqp (Path.rem_ext @@ v "/a/b") (v "/a/b");
  eqp (Path.rem_ext @@ v "/a/b.mli") (v "/a/b");
  eqp (Path.rem_ext @@ v "a/.ocamlinit") (v "a/.ocamlinit");
  eqp (Path.rem_ext @@ v "f.tar.gz") (v "f.tar");
  eqp (Path.rem_ext ~multi:true @@ v "f.tar.gz") (v "f");
  ()

let set_ext = test "Path.set_ext" @@ fun () ->
  eqp (Path.set_ext ".bla" (v "/a/b")) (v "/a/b.bla");
  eqp (Path.set_ext "bla" (v "/a/b")) (v "/a/b.bla");
  eqp (Path.set_ext ".bla" (v "/a/b.mli")) (v "/a/b.bla");
  eqp (Path.set_ext "bla" (v "/a/b.mli")) (v "/a/b.bla");
  eqp (Path.set_ext "bla" (v "a/.ocamlinit")) (v "a/.ocamlinit.bla");
  eqp (Path.set_ext "bla" (v "f.tar.gz")) (v "f.tar.bla");
  eqp (Path.set_ext ~multi:true "bla" (v "f.tar.gz")) (v "f.bla");
  eqp (Path.set_ext ~multi:true "" (v "f.tar.gz")) (v "f.");
  ()

let suite = suite "Path module"
    [ of_string;
      add_seg;
      append;
      constants;
      is_seg_valid;
      is_abs_rel;
      is_prefix;
      split_volume;
      segs;
      filename;
      base;
      parent;
      file_to_dir;
      dir_to_file;
      find_prefix;
      rem_prefix;
      normalize;
      rooted;
      relativize;
      ext;
      ext_is;
      has_ext;
      add_ext;
      rem_ext;
      set_ext; ]

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
