

- Fix semantics of dotfile handling in `OS.Path.{matches,query}`.
  `~dotfile:false` (default) used to not return any path that had a
  dot segment, even if this was a constant segment without pattern
  variables. This is no longer the case, `~dotfile:false` now only
  prevents segments starting with a pattern variable to match against
  dot files, i.e. it controls the exploration of the file system made
  by the function. Thanks to David Kaloper for the discussion.
  

v0.1.1 2016-05-08 Cambridge (UK)
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
