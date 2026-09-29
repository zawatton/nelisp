# vendor/emacs-lisp

Unmodified copies of Emacs's own `.el` files, so this tree loads Emacs's
real library code instead of a hand-written subset of it.

## What is here

```
vendor/emacs-lisp/
  comint.el
  menu-bar.el
  international/mule-conf.el (staged source provider; not runtime-loaded)
  emacs-lisp/subr-x.el
  emacs-lisp/rx.el
  emacs-lisp/cl-seq.el
  emacs-lisp/bytecomp.el
  emacs-lisp/byte-opt.el
  emacs-lisp/macroexp.el
  emacs-lisp/cconv.el
  emacs-lisp/inline.el
  emacs-lisp/byte-run.el
  emacs-lisp/ring.el
  emacs-lisp/easy-mmode.el
  emacs-lisp/backquote.el
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
(not guessed): it answers `simple.el`. The full `simple.el` library is one of
the five files this segment tried and did NOT load (see below: it fails
immediately on `defface`, then `overlay-arrow-variable-list`, needing the
buffer/display/editing-command substrate). Its source is staged outside the
runtime library path only to provide selected exact forms; it does not provide
the `decoded-time-*` definitions, so these names remain missing:

```
decoded-time-second   decoded-time-minute   decoded-time-hour
decoded-time-day      decoded-time-month    decoded-time-year
decoded-time-weekday  decoded-time-dst      decoded-time-zone
```

`(decoded-time-year (decode-time ...))` is `void-function` here. Code
that needs a field out of a `decoded-time' struct on this runtime has to
index the list directly (`decode-time` itself works and returns the same
shape Emacs does; only the named accessors are missing) until the relevant
`decoded-time` struct definition is staged or the full library becomes
loadable. `make-decoded-time` and `decoded-time-p` are ALSO not vendored
by this segment for the same reason, and are not fboundp on real Emacs
31.1 either (measured: both `nil`), so their absence is not a new gap
this segment introduces.

## Provenance

- `emacs-lisp/menu-bar.el` is byte-identical to the installed GNU Emacs 31.1
  `lisp/menu-bar.el.gz` source after decompression. The compressed source SHA-256
  is `b08a8ff027befa6d86d9c2620c8bc4c8e5d2591b2a2dc8971fc80eee645c8bb3`; the
  uncompressed vendor SHA-256 is
  `cfcbfb991eaea631b66b79f0e4419318703eb9f52006db5d4962fd29562d49d4`.
  `nelisp-vendor-source` pins the full file and selects its unique top-level
  `(setq menu-bar-final-items '(help-menu))` form before comint appends menu
  items; it does not load the interactive menu tree.

- `emacs-lisp/international/mule-conf.el` is the byte-for-byte uncompressed
  GNU Emacs 31.1 source from the installed `lisp/international/mule-conf.el.gz`
  provider (`symbol-file` for `password-word-equivalents` on Host Emacs 31.1).
  The installed compressed source SHA-256 is
  `da9dd1fba0cfc40295e7a895c6e7a7973dc840e8776a079d125b5414380d40f4`; the
  uncompressed vendored source SHA-256 is
  `c706015365dc9763d26c10ba487ba014517b58eb08e2d4101107a3f094119f94`.
  The standalone stages only the exact `password-word-equivalents` and
  `password-colon-equivalents` Custom forms before `comint.el` uses them; it
  does not load the full multilingual configuration library.

- `emacs-lisp/ansi-color.el` is an unmodified GNU Emacs 31.1 source file from
  the official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`; byte-for-byte checked against
  the installed 31.1 source and the commit's raw source. Its SHA-256 is
  `674d0f42e9b9d142ef2644b234ba75692acfd755d3e1a12203cca6d4b7372177`.
- `emacs-lisp/ansi-osc.el` is the unmodified GNU Emacs 31.1 source from the
  same release commit, checked against the installed 31.1 source. Its SHA-256
  is `75df0e972ace2f9ca255c8d88ee917398f51b702dccba1afca5eda809deb9f61`.
  It is resolved through the existing standalone vendor load path.
- `emacs-lisp/button.el` is an unmodified GNU Emacs 31.1 source file from the
  same release commit, checked against the installed 31.1 source. Its SHA-256
  is `8316efddb342a6ed9473475e5db69f61a52cc63587985d7cad03345f6c27676f`.
  The standalone bootstrap stages only `button-category-symbol` and
  `define-button-type` for GNU `ansi-osc` initialization.
- `emacs-lisp/emacs-lisp/regexp-opt.el` is an unmodified GNU Emacs 31.1 source
  file from the same release commit, checked against the installed 31.1 source.
  Its SHA-256 is
  `90e9e70c583c3b18036353b593e22bcd5510b32ce017dcb27acd6ce22ec449ae`; the
  existing standalone vendor load path provides it directly.
- `emacs-lisp/ring.el` is an unmodified GNU Emacs 31.1 source file from the
  official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its SHA-256 is
  `2f66173b2e84e4b033d0ebccfd612d64470a58d61e8709996eeade0a77ee6a32`.
  Its `cl-deftype` dependency is staged from the exact GNU `cl-macs.el` form
  below.
- `staged-emacs-lisp/cl-macs.el` is an unmodified GNU Emacs 31.1 source file
  from the official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled
  commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`; byte-for-byte checked
  against that commit's raw source. Its SHA-256 is
  `f58f3f9b88755549a7713b59045059affda99c2e22251e22e5c40fbe017f2394`.
  The standalone build stages only its exact `cl-deftype` macro form, needed
  by `ring.el` during compile-time loading; it does not load all of `cl-macs`.
- `staged-emacs-lisp/cl-preloaded.el` is an unmodified GNU Emacs 31.1 source
  file from the same official release commit, byte-for-byte checked against
  the raw upstream file. Its SHA-256 is
  `73c114b7a8899ae278fdd1eae29c902703f20bc0662f99c3edb391362a35514c`.
  The standalone build stages the exact `cl--class` and
  `cl-derived-type-class` structure forms plus `cl--define-derived-type`,
  preserving the GNU class layout and constructor contract.
- `staged-emacs-lisp/custom.el` is an unmodified GNU Emacs 31.1 source file
  from the same official release commit, byte-for-byte checked against the
  raw upstream and installed 31.1 sources. Its SHA-256 is
  `363b7f408c88c1788aa53f35a15ef46c07ad86fb20a9d110f381b0cd0e85a5c0`.
  The standalone bootstrap stages its exact `defface` macro and the
  customization keyword-dispatch and version-metadata functions required by
  GNU declarations; it does not load all of `custom.el`.
- `staged-emacs-lisp/cus-face.el` is an unmodified GNU Emacs 31.1 source file
  from the same official release commit, byte-for-byte checked against the
  raw upstream and installed 31.1 sources. Its SHA-256 is
  `fe8718def781adfdbd3b8b87d9d30f8715827e1d264b854802ed8182625cd892`.
  The standalone bootstrap stages only the exact `custom-declare-face`
  function needed by GNU `defface`; it does not load all of `cus-face.el`.
- `staged-emacs-lisp/faces.el` is an unmodified GNU Emacs 31.1 source file
  from the same official release commit, byte-for-byte checked against the
  raw upstream and installed 31.1 sources. Its SHA-256 is
  `d6464ee14c8c6e2b6146cecce9bdfb6513ced36fd6ebac172fc84e00be9d22f3`.
  The standalone bootstrap stages only the exact `face-spec-set`,
  `make-empty-face`, `facep`, and `set-face-documentation` functions used by
  GNU `custom-declare-face`; it does not load all of `faces.el`.
- The standalone's frame-less face registry in
  `scripts/nelisp-stdlib-prelude.el` models the global face-ID/attribute-vector
  contract of GNU 31.1 `src/xfaces.c` at release commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9` (SHA-256
  `c1dc220be50f2b30d40d4752aed2a485b4a6b7b579bd996dc410a2d354d25e3e`),
  specifically `internal-make-lisp-face` and `internal-lisp-face-p`. It does
  not implement frame-local face semantics or display attributes; those are
  outside this bounded headless registry.
- `comint.el` is an unmodified GNU Emacs 31.1 source file from the official
  `emacs-mirror/emacs` release tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its SHA-256 is
  `29ae8d26f671178b6c086c8492ce9c3890351d706ba63c298c5186ad4808b13b`.
  It is discoverable through the top-level vendor load path, but its `ring`
  prerequisite is not yet available in standalone.
- `emacs-lisp/bytecomp.el` is an unmodified GNU Emacs 31.1 source file from
  the official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its SHA-256 is
  `094fa608bed9d9feffd4364b8df3288c1eb2bf1efd5dfa57fcd6d9bd13cdd099`.
  It supplies the missing library required by `require 'bytecomp`; the
  existing `vendor/emacs-lisp/emacs-lisp` load path makes it discoverable.
- `emacs-lisp/byte-opt.el` is an unmodified GNU Emacs 31.1 source file
  (decompressed byte-for-byte from `lisp/emacs-lisp/byte-opt.el.gz` of the
  installed 31.1 tree, i.e. `emacs-mirror/emacs` tag `emacs-31.1`, peeled
  commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`). Its SHA-256 is
  `84e5d15e9fc3d413a9a69d03fff6742c52cd746ec7988d0441e402b0dcb04f99`.
  `bytecomp.el` reaches it through the `byte-optimize-*' autoloads; the
  standalone runtime loads it on first call via GNU autoload semantics and
  bakes it like `bytecomp.el` (`nelisp-standalone--vendor-bytecode-files`).
- `emacs-lisp/progmodes/compile.el` is an unmodified GNU Emacs 31.1 source
  file from the official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled
  commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its SHA-256 is
  `e613d933e2813349b36c1cfe910e6c36ed54316f215551e04d5b0036f8cd2997`.
  The existing `vendor/emacs-lisp` load path makes it discoverable for
  `require 'compile`.
- `emacs-lisp/emacs-lisp/text-property-search.el` is an unmodified GNU Emacs
  31.1 source file from the official `emacs-mirror/emacs` release tag
  `emacs-31.1`, peeled commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its
  SHA-256 is `5f15fbbee8d66de226da059fe7338c52a1a5642d0973b6cac229d7c947848470`.
  It supplies `compile.el`'s `(require 'text-property-search)`, reached from
  `bytecomp.el`'s own `(eval-when-compile (require 'compile))`; the existing
  `vendor/emacs-lisp/emacs-lisp` load path makes it discoverable.
- `emacs-lisp/tool-bar.el` is an unmodified GNU Emacs 31.1 source file from
  the official `emacs-mirror/emacs` release tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`. Its SHA-256 is
  `fe32fb250ba7e6a5d0f0ef602769b498a9c56d3051c7f19876e88f6299ed2187`.
  The existing `vendor/emacs-lisp` load path makes it discoverable for
  `require 'tool-bar`.
- `emacs-lisp/macroexp.el` is an unmodified GNU Emacs 31.1 source file from
  the same official release commit. Its SHA-256 is
  `10df0fe326e6f436a3ec3a74fdaa79b06272e5a35556383d8e2741ce3c16a0c2`. It is
  separate from the older, single-form `vendor/staged-emacs-lisp/macroexp.el`
  provider and is discoverable through the existing Emacs Lisp load path.
- `emacs-lisp/cconv.el` is an unmodified GNU Emacs 31.1 source file from
  the same official release commit. Its SHA-256 is
  `1639c5812c18837332ad90f3d7f835e7abeab1af22f49f2c391c813e74ab03cc`. The
  existing Emacs Lisp load path makes it discoverable for `require 'cconv`.
- `emacs-lisp/inline.el` is an unmodified GNU Emacs 31.1 source file from
  the same official release commit. Its SHA-256 is
  `21742b8714c617550ba6dc72645f725e84614fb4f1bc00c2066a5bb116504040`. GNU
  Emacs normally exposes `define-inline` through generated autoloads; the
  standalone bootstrap loads this provider before source libraries that use it.
- `emacs-lisp/byte-run.el` is an unmodified GNU Emacs 31.1 source file from
  the same official release commit. Its SHA-256 is
  `cbac268cd6cc2f9ca2fefd28e7888a3dfa72896514d2652c7ef5addb4c5b73f7`.
  The standalone source provider extracts only its exact `function-put`
  top-level form before loading `inline.el`; the rest of byte-run.el is not loaded.

- **Upstream**: GNU Emacs (GPLv3-or-later). File headers read
  `Copyright (C) 2001-2025 Free Software Foundation, Inc.`, consistent
  with the Emacs 31.1 this tree's parity work is measured against.
- **Immediate source**: the sibling repository `nelisp-emacs-lib`, whose
  `vendor/emacs-lisp/` tree bundles 1,654 upstream `.el` files. The four
  segment-I libraries were copied byte-for-byte (verified with `md5sum`)
  from that tree at
  its commit `941c65e3398b46402bc61b258619bc384b4ecd56` (2026-09-04,
  "feat: markers that follow edits, a buffer-local mark, and 25 cl-seq
  functions"), which is that repository's own pin of the upstream files.
- The staged source `vendor/staged-emacs-lisp/subr.el` was copied byte-for-byte from sibling commit
  `cbf4f49d2620c264d39c887d024cd23df9cc9c62`; its SHA-256 is
  `410c34e030bdd667ff21842a8513e41100394662dacf5e54b40938106f0d6327`.
  The standalone bootstrap stages only its exact top-level
  `def-edebug-elem-spec`, `define-symbol-prop`, `string-lines`, and
  `autoloadp` forms;
  loading the full file is not required. `string-lines` is staged before
  `easy-mmode.el`, which calls it while defining minor-mode support; `autoloadp`
  is staged from its original `defsubst` form because `function-get` uses it
  when following autoloaded function definitions. This
  staged-source directory is outside `vendor/emacs-lisp/` so it is not
  added to the runtime library search path or vendor-shadow audit. The
  standalone bootstrap and source evaluator share the SHA-checked selector
  in `src/nelisp-vendor-source.el`; each evaluates selected forms in its own
  runtime. The selector also stages exact `assoc-default`, `assoc-delete-all`,
  and `assq-delete-all` forms. The source evaluator treats leading GNU
  `declare` forms as function metadata. Both runtimes also install the exact
  `string-trim-left`, `string-trim-right`, `string-trim`, `string-prefix-p`,
  and `string-suffix-p` forms. Their only extra source-evaluator dependencies
  are the existing `match-end` and `compare-strings` primitives; local
  duplicate definitions were removed. The same pinned source now supplies
  the exact `string-equal-ignore-case` (`defsubst`) and `string-greaterp`
  (`defun`) forms, plus GNU's exact `defalias` forms for `string=`, `string<`,
  and `string>`. The source evaluator uses its borrowed `string-equal` and
  `string-lessp` C primitives for the first two aliases. The source selector
  indexes only static `defalias` names (literal or quoted symbols), so it can
  select these forms by alias name without evaluating or guessing at computed
  aliases. `string-empty-p` is defined in GNU `simple.el`, not the pinned
  `subr.el` or vendored `subr-x.el`; both evaluators stage its exact form from
  the SHA-pinned source after installing `string=`.
- The staged source `vendor/staged-emacs-lisp/bindings.el` is byte-identical
  to GNU Emacs 31.1 `lisp/bindings.el` at official tag `emacs-31.1`, peeled
  commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`; its SHA-256 is
  `479dd97f6b78f644f036b8279a2594da7913f404c71f34451a26ed3642342b3b`.
  The standalone stages only the exact `bound-and-true-p` macro form before
  loading libraries that use it; it does not load full `bindings.el`.
- The staged source `vendor/staged-emacs-lisp/keymap.el` is byte-identical to
  GNU Emacs 31.1 `lisp/keymap.el` at official tag `emacs-31.1`, peeled commit
  `a360712c9d272d950d8d8255ef74570f7e90b7d9`; its SHA-256 is
  `77337ec8988f4279b9d08a12cbb7af84bd1113e97c2807e63c76c8175475ac11`.
  The standalone stages only the exact `defvar-keymap` macro form from it.
- `vendor/staged-emacs-lisp/simple.el` is byte-identical to the GNU Emacs
  31.1 source in the sibling `nelisp-emacs-lib` vendor tree (SHA-256
  `c19d208e61100fc9ff692cef0c95d895e54e17fc1f9031500ac8474c9c2e48f2`).
  The selector evaluates only the exact `string-empty-p` defsubst; it does
  not load the full `simple.el` library.
- The staged `vendor/staged-emacs-lisp/subr.el` provider also supplies exact
  GNU forms for `internal--build-binding`, `internal--build-bindings`,
  `if-let`, `if-let*`, `when-let`, `when-let*`, `and-let*`, and `while-let`.
  Their only extra dependency is `macroexp-progn`, selected from the pinned
  `vendor/staged-emacs-lisp/macroexp.el` (SHA-256
  `716a3ac7bd9756ad901f4307075cc4bd0e371248ab94c7a200962c7a1e2614e3`).
  Both source and standalone evaluators install these definitions after the
  macro system is available; neither loads the full `macroexp.el` library.
  The same provider stages the exact GNU list forms `last`, `butlast`,
  `nbutlast`, and `copy-tree`; source evaluation uses its existing `safe-length`,
  `take`, and `recordp` primitives required by those forms.
  Both evaluators also install the exact GNU forms `number-sequence`,
  `ensure-list`, and `flatten-tree` from this pinned source. They rely on the
  existing arithmetic/list operations and `push`/`pop` macros; the local
  prelude duplicates were removed.
- Both evaluators also install exact `booleanp`, `fixnump`, `zerop`, `ignore`,
  and `always` forms from the pinned `subr.el`; their hand-written duplicate
  forms were removed. `fixnump` depends on GNU Emacs's
  `most-negative-fixnum` / `most-positive-fixnum` values. The standalone
  prelude already defines the measured 64-bit values; the source evaluator
  imports the host's C-provided constants into its separate value table after
  every reset. `ignore` contains GNU's leading `interactive` declaration, so
  the source evaluator and standalone prelude both provide the same no-op
  declaration macro. Parity checks cover both fixnum boundaries and the first
  value beyond them, as well as direct calls to all five functions.
- Both evaluators install the exact GNU `ignore-errors` macro and `user-error`
  function from the pinned `subr.el`; their local simplified duplicates were
  removed. Focused parity checks cover successful and signalled `ignore-errors`
  bodies and formatted `user-error` conditions.
- `vendor/staged-emacs-lisp/files.el` is byte-identical to GNU Emacs 31.1
  `files.el` in the sibling vendor tree (SHA-256
  `00b8b0ee9e718a34bc8b9ca04526d8ce7449fb3ebf613bef28e4ca01b0a7ad9e`).
  Both evaluators select only `file-attribute-size`,
  `file-attribute-modification-time`, and `file-attribute-file-identifier`;
  the standalone prelude's accessor substitutes were removed without loading
  the full `files.el` library.
- The same pinned `subr.el` also supplies `gensym-counter`, `gensym`,
  `frame-configuration-p`, `apply-partially`, and `bignump`. The selector
  accepts exact top-level `defvar` and `defconst` forms for this purpose.
  NeLisp no longer registers a native `bignump` arm, so its public function
  cell reaches the GNU Lisp definition. Source and standalone probes cover
  function results, including positive and negative bignum literals.
- The standalone bootstrap stages exact forms from the byte-identical
  `vendor/emacs-lisp/emacs-lisp/subr-x.el` (commit
  `cbf4f49d2620c264d39c887d024cd23df9cc9c62`, SHA-256
  `b1e7797646f6af8c30fe506b4f098e79a9568610ae6d3e8e78dfaa690042456b`):
  `internal--thread-argument`, `thread-first`, `thread-last`,
  `hash-table-keys`, `hash-table-values`, `string-join`, `string-blank-p`,
  `string-remove-prefix`, `string-remove-suffix`, `string-pad`, and
  `string-chop-newline`. It does not load all of `subr-x.el`; `named-let`
  remains an explicit unresolved NeLisp recursion incompatibility.
- `vendor/emacs-lisp/emacs-lisp/easy-mmode.el` is byte-identical to the
  sibling vendor source at commit
  `cbf4f49d2620c264d39c887d024cd23df9cc9c62`; its SHA-256 is
  `f2c7fd77680a771134c9cc3f1af8df8782d71cb918957ad4d9f8431cad168741`.
  The common standalone bootstrap requires this GNU provider before
  `cl-lib`, which uses `define-minor-mode` while loading.
- `vendor/emacs-lisp/emacs-lisp/backquote.el` is byte-identical to the
  sibling vendor source at commit
  `0787c778dfce5c73360363d093e6997e708ec63c`; its SHA-256 is
  `66ab254956c5e715c6d986132bb7e26278a01e6c5b0608cdaebf969a5e2494b1`.
  The standalone prelude loads this provider after the `put` primitive is
  available; the generated bootstrap supplies its source path without
  exposing other vendor libraries early.
- Chosen from that tree's 1,654 files because these four are the ones
  this repository's own `docs/design` "segment I" work (vendor real
  Emacs Lisp instead of hand-writing subsets of it) measured as loading
  and behaving correctly on the NeLisp standalone reader, out of a
  9-file candidate list. `emacs-lisp/seq.el`, `json.el`,
  `url/url-parse.el`, `emacs-lisp/ert.el` and the full `simple.el` library
  were also tried and are NOT loaded from the runtime vendor path yet -- see
  that segment's report for why
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

The list-accessor forms `internal--compiler-macro-cXXr`, `caar`, `cadr`,
`cdar`, `cddr`, `caaar`, `caadr`, `cadar`, `caddr`, `cdaar`, `cdadr`,
`cddar`, `cdddr`, and `cadddr` are selected byte-for-byte from staged
GNU Emacs 31.1 `subr.el` (source commit
`cbf4f49d2620c264d39c887d024cd23df9cc9c62`, SHA-256
`410c34e030bdd667ff21842a8513e41100394662dacf5e54b40938106f0d6327`).
They are installed in the source evaluator and staged in each standalone
prelude before local definitions are loaded.

1. In `nelisp-emacs-lib`, update its own `vendor/emacs-lisp/` pin (that
   repository owns tracking upstream Emacs).
2. Re-copy the four segment-I runtime libraries, `backquote.el`, and separately tracked
   bootstrap providers from its `vendor/emacs-lisp/` tree into the matching
   paths here. Keep staged-only source files under `vendor/staged-emacs-lisp/`.
   Verify byte-for-byte identity with `md5sum` or `sha256sum` against the source.
3. Re-run this tree's Phase 1 measurement (try `(require 'FEATURE)` on
   the rebuilt standalone reader for each vendored file, plus the other
   candidates in the 9-file list) before assuming the refreshed copies
   still load and behave the same way; a newer upstream Emacs can
   introduce a new missing primitive this runtime does not have.
4. Record the new nelisp-emacs-lib commit hash and date in this file.
