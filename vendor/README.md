# vendor/emacs-lisp

Unmodified copies of Emacs's own `.el` files, so this tree loads Emacs's
real library code instead of a hand-written subset of it.

## What is here

```
vendor/emacs-lisp/
  emacs-lisp/subr-x.el
  emacs-lisp/rx.el
  emacs-lisp/cl-seq.el
  calendar/time-date.el
```

Sub-paths mirror their location in Emacs's own `lisp/` tree, so `rx.el`
sits under `emacs-lisp/` and `time-date.el` under `calendar/`, matching
`(locate-library "rx")` on a real Emacs.

## Known gap: `time-date.el` is vendored without the `decoded-time-*` accessors

`time-date.el`'s `decode-time` returns a `decoded-time` struct, but the
`cl-defstruct` that defines that struct -- and therefore every
`decoded-time-*` accessor -- lives in `simple.el`, not `time-date.el`.
Measured on real Emacs 31.1 with `(symbol-file 'decoded-time-year 'defun)`
(not guessed): it answers `simple.el`. `simple.el` is one of the five
files this segment tried and did NOT vendor (see below: it fails to load
immediately, `defface`, then `overlay-arrow-variable-list`, needing the
buffer/display/editing-command substrate), so these names are still
missing on this runtime:

```
decoded-time-second   decoded-time-minute   decoded-time-hour
decoded-time-day      decoded-time-month    decoded-time-year
decoded-time-weekday  decoded-time-dst      decoded-time-zone
```

`(decoded-time-year (decode-time ...))` is `void-function` here. Code
that needs a field out of a `decoded-time' struct on this runtime has to
index the list directly (`decode-time` itself works and returns the same
shape Emacs does; only the named accessors are missing) until `simple.el`
-- or just its `decoded-time` struct definition, split out -- is
vendored. `make-decoded-time` and `decoded-time-p` are ALSO not vendored
by this segment for the same reason, and are not fboundp on real Emacs
31.1 either (measured: both `nil`), so their absence is not a new gap
this segment introduces.

## Provenance

- **Upstream**: GNU Emacs (GPLv3-or-later). File headers read
  `Copyright (C) 2001-2025 Free Software Foundation, Inc.`, consistent
  with the Emacs 31.1 this tree's parity work is measured against.
- **Immediate source**: the sibling repository `nelisp-emacs-lib`, whose
  `vendor/emacs-lisp/` tree bundles 1,654 upstream `.el` files. These four
  were copied byte-for-byte (verified with `md5sum`) from that tree at
  its commit `941c65e3398b46402bc61b258619bc384b4ecd56` (2026-09-04,
  "feat: markers that follow edits, a buffer-local mark, and 25 cl-seq
  functions"), which is that repository's own pin of the upstream files.
- Chosen from that tree's 1,654 files because these four are the ones
  this repository's own `docs/design` "segment I" work (vendor real
  Emacs Lisp instead of hand-writing subsets of it) measured as loading
  and behaving correctly on the NeLisp standalone reader, out of a
  9-file candidate list. `emacs-lisp/seq.el`, `json.el`,
  `url/url-parse.el`, `emacs-lisp/ert.el` and `simple.el` were also
  tried and are NOT vendored yet -- see that segment's report for why
  (mainly: they need `emacs-lisp/cl-generic.el`, which needs
  `emacs-lisp/oclosure.el`, which does not load on this runtime today --
  a closure-representation mismatch, not a missing name).

## Rules

- **Do not edit these files.** They are not maintained here. A local
  fix does not survive the next refresh and will not be visible to
  anyone diffing against upstream Emacs. If one of these files needs a
  behavior change, that is a NeLisp standalone-runtime plumbing gap
  (see `scripts/nelisp-stdlib-prelude.el`'s guarded `unless (fboundp ...)`
  compatibility shims) or an upstream Emacs bug, not something to patch
  in place.
- **License**: this repository is already GPLv3 (see `LICENSE`), the
  same license as GNU Emacs itself, so vendoring GPLv3-or-later Emacs
  Lisp sources here carries no license conflict.
- **Load-path**: `scripts/nelisp-standalone-build.el`'s
  `nelisp-standalone--reader-tree-load-path` puts `vendor/emacs-lisp` and
  its `emacs-lisp/`/`calendar/` subdirectories on the standalone
  reader's default `load-path`, ordered after this tree's own
  `lisp/`/`src/`/`scripts/`/`packages/*/src` (so a name this tree
  defines itself always wins) but before `standalone-compat/` (so a
  vendored file wins over the hand-written subset it replaces, where
  one still exists for a file that does not load here).

## Refreshing

1. In `nelisp-emacs-lib`, update its own `vendor/emacs-lisp/` pin (that
   repository owns tracking upstream Emacs).
2. Re-copy the four files listed above from its `vendor/emacs-lisp/`
   tree into the matching paths here, verifying byte-for-byte with
   `md5sum` against the source.
3. Re-run this tree's Phase 1 measurement (try `(require 'FEATURE)` on
   the rebuilt standalone reader for each vendored file, plus the other
   candidates in the 9-file list) before assuming the refreshed copies
   still load and behave the same way; a newer upstream Emacs can
   introduce a new missing primitive this runtime does not have.
4. Record the new nelisp-emacs-lib commit hash and date in this file.
