v0.1.5 2016-03-18 La Forclaz (VS)
---------------------------------

- Fix `OS.Cmd.{err_file,out_file,to_file}`. Files were not truncated
  on `append = false`.
- `OS.File.with_input`, allow to specify the input buffer as an
  optional argument.

v0.1.4 2016-08-30 Zagreb
------------------------

- Fix `OS.Path.fold` on root and relative paths (#61).
  Thanks to Hezekiah M. Carty for the report and the help.
- Fix `OS.File.write` on Windows (#59). Thanks
  to Hezekiah M. Carty for the report and the fix.

v0.1.3 2016-07-12 Cambridge (UK)
--------------------------------

- `Cmd.dump`, make representation cut and paste friendly. This
  affects logging made by the library.
- Add `Cmd.of_values`, converts arbitrary list of values to
  a corresponding argument list.
- Fix `OS.Path.exists`. Existing file path traversals returned
  and error rather than `false`.
  
v0.1.2 2016-06-17 Cambridge (UK)
--------------------------------

- Fix `OS.File` creation mode from `0o622` to `0o644` (#55).
- Fix semantics of dotfile handling in `OS.Path.{matches,query}`.
  `~dotfile:false` (default) used to not return any path that had a
  dot segment, even if this was a constant segment without pattern
  variables. This is no longer the case, `~dotfile:false` now only
  prevents segments starting with a pattern variable to match against
  dot files, i.e. it controls the exploration of the file system made
  by the function. Thanks to David Kaloper for the discussion.
  
v0.1.1 2016-06-08 Cambridge (UK)
--------------------------------

- Fix `OS.Cmd` combinators on Linux. Thanks to Andreas Hauptmann for
  the help (#51)
- Fix `OS.Dir.delete` on Linux and Windows. Thanks to Andreas Hauptmann
  for the help (#50).
- Fix `OS.Cmd.exists` on Linux. Thanks to Andreas Hauptmann and
  Petter Urkedal for the help (#52).

v0.1.0 2016-05-23 La Forclaz (VS)
---------------------------------

First release.
