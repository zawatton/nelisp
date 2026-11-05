# Design: switching NeLisp's cl layer to GNU 31.1

Status: investigation only (no source edits, no builds). Measurements ran on the K=140 binary
(`build-on140/nelisp-dynamic`, `ulimit -v 8388608`) plus host reads of /usr/local/share/emacs/31.1/lisp.

## 1. How GNU bootstraps these files

lisp/loadup.el (31.1) order, by line:
- 123-125 byte-run, backquote, subr; 148 macroexp; 154 `pcase` (loaded with eager-load skipped:
  "macroexp's uses of pcase will simply fail until pcase is explicitly loaded"), 157 macroexp reloaded.
- 175 `loaddefs` (autoloads: `cl-defstruct`, `cl-defmethod`, `cl-loop`... point at cl-macs / cl-generic / cl-lib).
- 187-195 `(require 'gv)` guarded by `interpreted-function-p add-hook` (comment: cl-preloaded's `provide` must not hit a void `setf`).
- 197 `cl-preloaded`, 198 `oclosure` ("used by cl-generic"), 250 `cl-generic`, 251 simple, 252 seq, 253 `nadvice` (last).
- cl-lib, cl-macs, cl-extra are NOT preloaded; they come via autoload.

Circle breaking in cl-preloaded.el (line numbers of the .el):
- 25-35 commentary: cl-structure-class is defined with itself; cl-structure-object is its own parent's class.
- 39-40 `(eval-when-compile (require 'cl-lib/cl-macs))`: when the file is loaded as source, these execute, so
  cl-macs's `cl-defstruct` is available while the file loads.
- 54, 82-104: `cl--find-class`/`cl--struct-get-class` tolerate "Early bootstrap" (`built-in-class-p` unbound).
- 69-70: placeholder `(setf (cl--find-class 'cl-structure-object) 'dummy)` so the first defstruct can name its parent.
- 194-198: the `cl-defstruct` of cl-structure-class itself declares `(:constructor cl--struct-new-class ...)`;
  the expansion defines the constructor before calling it. 219, 227, 247: the other three structs.
- 260-284: assertions plus `setf` of `parents` repair the dummy and tie the knot. 475-480: record parent fix-up.
Key point: GNU's own source bootstrap works because cl-macs is loaded completely BEFORE the first defstruct,
`nadvice` is not yet defined, and no other `cl-defstruct` implementation exists. oclosure.el (214-262, 332, 441-462)
then builds on `cl-defstruct` (oclosure--class) and on interpreted closures whose slot 4 (the docstring slot) holds the type symbol.

## 2. Where NeLisp defines the native versions, and who depends on them

- scripts/nelisp-stdlib-prelude.el: `cl-defstruct` (l.8535), `cl-loop` (8294), `cl-defgeneric` (10108),
  `cl-defmethod` (10133), `setf` (12118), `pcase` family (12444-12660). Mirrors in lisp/nelisp-cl-macros.el (2520 lines:
  defstruct 681, generic 1870/1895, setf 2379) and lisp/nelisp-pcase.el (348 lines).
- Foundation layer (nelisp-emacs): cl-lib.el shim (1253 lines), emacs-cl-macros.el (3022), emacs-pcase.el (594), emacs-parity-eieio.el.
- Prelude couples to the native defstruct tables: `nelisp-cl-macros--struct-info` is read at l.1451 (setf accessor info)
  and l.3319-3323 (`cl-typep`/type checks, `nelisp--record-type`). `type-of` on records goes through the same tables.
- Usage counts (call-site text matches, not unique libraries):
  `(pcase `: prelude 6, standalone-build.el 60 (host side, GNU pcase), lisp/ 206, src/ 72, packages/ 103, vendor/emacs-lisp 344.
  `(cl-defstruct`: prelude 15, lisp/ 7, src/ 38, packages/ 48, vendor 32.
  `(cl-defgeneric` vendor 10, `(cl-defmethod` vendor 23, lisp 2, packages 2. `(oclosure` packages 7, vendor 66.
- Build/boot: scripts/nelisp-standalone-build.el runs on host Emacs, so its own pcase/defstruct are GNU's and do not care.
  What matters is the generated image: prelude text is evaluated at build time into the heap image, and
  scripts/nelisp-boot-image.el bakes `macroexp` as bytecode compiled by the host (already GNU-pcase-expanded, so independent of the
  native pcase). Features `cl-lib` and `cl-generic` are provided natively at boot (`featurep` t on K=140), `pcase`, `gv`, `nadvice`, `cconv` are not.
  `advice-add`/`add-function` ARE fboundp natively (see gap 5).
- Vendored tree today lacks GNU pcase.el, cl-lib.el, cl-generic.el, cl-extra.el, cl-print.el (they exist as
  .el.gz in /usr/local/share/emacs/31.1/lisp/emacs-lisp; sizes 16K/7.5K/19K/12K/7.5K). vendor has gv, oclosure, cconv, nadvice, macroexp, cl-seq;
  staged-emacs-lisp has cl-macs (166 KB) and cl-preloaded (25 KB).
- Tests that pin native behaviour: test/nelisp-cl-generic*-test.el, nelisp-pcase-app-test.el, nelisp-cl-defstruct-docstring-test.el,
  standalone-cl-defstruct-lazy-default-check.el, plus the progress ledgers (alignhot/dynfast use r3-pcase rows for performance).

## 3. Measured gaps (source-load of each GNU file form by form on K=140)

1. `static-if` void. Minimal: `(macroexpand '(static-if t 1 2))`. Cause: GNU subr.el 31 macro absent. Fix: stage the exact form from
   vendor/staged-emacs-lisp/subr.el (existing mechanism). Staging only.
2. Closure representation: NOT a gap any more. `(make-interpreted-closure '(x) '((+ x a)) '((a . 1)) 'my-type)` gives 5 slots,
   `(aref f 4)` = type, `oclosure-type` works, and vendor oclosure.el loads (`ok`). The vendor/README note ("closure-representation
   mismatch") is stale. Residual discrepancies to check: closures created by `function`/`lambda` evaluation report `length` 3,
   GNU reports 5 (oclosure--copy tests `(> (length oclosure) 5)`); `cconv-make-interpreted-closure` is unbound until cconv.el loads;
   `documentation` is void on this binary. Substrate (evaluator), small, verify before relying.
3. pcase `or` with branch-local bindings. Minimal: `(pcase '(integer) (`(,(and name (or 'integer 'float)) . ,(or `(,min ,max) pcase--dontcare)) name))`
   raises "pcase or pattern has branch-local bindings" from lisp/nelisp-pcase.el:249 (prelude 12520); simple `or` forms work. This is
   the cl-macs `cl-typep` define-inline. Resolution: retire native pcase and load GNU pcase.el; GNU pcase.el loads cleanly now
   (52 forms, 0 failures, 0.02 s). Staging a GNU file.
4. cl-defstruct circle. Minimal: `(load "vendor/staged-emacs-lisp/cl-preloaded.el")` fails at `(eval-when-compile (require 'cl-macs))`
   (cl-macs incomplete) and then `(cl-defstruct (cl-structure-class ...))` => `void-function cl--struct-new-class`, followed by
   `wrong-type-argument cl-structure-class dummy` (cascade). Cause: the native `cl-defstruct` expands the form because GNU's was never
   completely installed. cl-macs itself: 3 of 178 forms fail (#3 below, static-if, pcase or). Fix order matters, no new substrate.
5. Boot-order trap in cl-macs.el l.3508: `(when (fboundp 'advice-add) (advice-add 'pcase--mutually-exclusive-p ...))`, commented
   "Not available during bootstrap". Native `advice-add` is already fboundp, so the form runs, calls into a native pcase that has no
   `pcase--mutually-exclusive-p`, and (as measured) dies with void `cl--struct-get-class`. GNU loads nadvice last (loadup 253).
   Fix: keep `advice-add` unbound until cl-macs and cl-generic have loaded, or load nadvice.el at the GNU position. Ordering.
6. cl-lib.el: `(unless (load "cl-loaddefs" 'noerror 'quiet))` form #92 fails (void `cl--struct-get-class`, so same root as 4). The
   foundation shim claims the reader rejects GNU cl-lib.el; GNU cl-lib.el actually evaluated 96 of 97 forms. Verify the claim with the
   real file, then delete the shim.
7. cl-generic.el: 34 of 107 forms fail, all downstream of 4 (`cl-generic-make-generalizer` void, `oclosure-define` => void
   `cl--struct-get-class`, `(eval-when-compile (require 'pcase))` file-missing because pcase.el is not vendored).
8. cl-extra.el: 3 failures: cl-macs (4), `define-button-type 'cl-type-definition` ("Unknown button type help-function-def": needs help-mode/button
   staging), and the generalizer (7). Warnings: unknown `gv-setter`/`gv-expander` defun properties (gv.el must be loaded first).
9. eieio-core: `cl-declaim` void until cl-macs is complete (S1.4's blocker).
Summary: 3 are staging (1, 3 via pcase.el, 5/6 files), 1 is ordering (5), 1 is a small evaluator check (2). No large core change found.

## 4. Staged migration plan (each step has a rollback = revert one commit; build index K from 141)

P0 Baseline and acceptance. Add ledger criteria (machine-checkable, in a new tools/ai/cl-gnu-progress.org): per file `locate-library` resolves to
vendor/staged, `(symbol-file 'cl-defstruct)` etc.; a differential corpus (pcase/cl-defstruct/cl-defgeneric/setf/gv cases evaluated by GNU 31.1 and
NeLisp, compared); timing rows. Capture baselines on build-on140: startup time, `r3-pcase` row, load time of org/magit-sized libs, verify22 outputs.
P1 Vendor missing GNU sources verbatim: pcase.el, cl-lib.el, cl-generic.el, cl-extra.el, cl-print.el (ORIGIN/SHA256SUMS). No behaviour change; gates unchanged.
P2 Stage `static-if` (+ any other missing subr.el forms found by a form-level probe). Gate: probe + verify22 identical.
P3 pcase: add pcase.el to the image (loaded at the GNU position, before macroexp is re-baked), delete lisp/nelisp-pcase.el, the prelude pcase block and the
   foundation emacs-pcase.el. Acceptance: differential pcase corpus (incl. the `or`/`app`/`guard`/backquote cases), nelisp-pcase-app-test, probe-load,
   verify22, perf: r3-pcase and a macro-expansion-heavy load (GNU pcase is interpreted source here; consider the existing vendor bytecode
   transform `nelisp-prelude-bytecode-vendor-transform`, which already compiles macroexp). Rollback: restore native files.
P4 gv + setf: GNU gv.el replaces prelude/lisp setf and the struct-info accessor hook (prelude l.1451). Acceptance: setf/gv corpus, the 215 prelude
   setf sites, vendor libs that use `gv-define-setter`.
P5 cl-macs + cl-preloaded + oclosure + cl-defstruct, in GNU order: complete cl-macs load with advice-add still unbound (gap 5), then cl-preloaded, then oclosure;
   retire prelude `cl-defstruct`/`nelisp-cl-macros--struct-info` and rewrite prelude type checks (l.3319-3323) to `cl-typep`/`cl--find-class` (GNU
   `type-of`/`cl-type-of` on records). This is the riskiest step. Acceptance: GNU cl-preloaded assertions (l.260-284) run, defstruct corpus
   (:include, :type, :copier, printing, `cl-print`), nelisp-cl-defstruct tests, load of the vendored libs that define structs (32 sites).
P6 cl-generic + cl-lib + cl-extra (+ nadvice at its GNU position): retire prelude cl-defgeneric/cl-defmethod, native cl-lib feature and the foundation cl-lib.el
   shim. Acceptance: nelisp-cl-generic tests, `(require 'eieio-core)` and `(require 'auth-source)`, then S1.4 (`url-parse`) goes green
   and standalone-compat/url-parse.el is deleted. Rollback per step.
Per-step regression gates (reuse the S1 set): verify22 + cmp of reader-probe/kd/status against the parent, selfhost `probe-load`, boot-library checks,
`make emacs-compat` shared-shadowing count (must not rise), the S1 probes, host ERT on test files for each retired native file.
Performance checks: cold startup time and resident size of the image (target: no regression beyond noise), `r3-pcase`, time to `(require 'org)` / bytecomp
on the binary, and the load time of cl-macs (0.18 s) and cl-generic (0.42 s) as measured sources.

Risks: (a) pcase-heavy hot code may slow down: GNU pcase expands larger code than the native one and GNU macroexp-driven expansion is interpreted;
mitigate by baking pcase/cl-macs through the vendor bytecode transform and measuring before deleting natives. (b) Boot order: image text is
evaluated in one pass; native `advice-add`, `cl-lib`/`cl-generic` features, and the prelude's own defstructs (15 uses) must be sequenced so no prelude code
needs cl-defstruct before cl-preloaded is up. (c) Heap image size and `ulimit -v` headroom. (d) Third-party/local code relying on native-only behaviour
(lazy defaults, docstring handling tests above). (e) The nelisp-emacs host bundle (build-nelisp-bootstrap) still lists foundation cl-lib/emacs-pcase.

## 5. Open decisions for the user

1. Keep native pcase/cl-defstruct until the GNU ones pass the differential corpus (parallel period), or switch step by step with no overlap?
2. Performance budget: accepted slowdown for pcase-heavy code and image size, and whether bytecode baking of GNU cl files into the image is allowed.
3. Should GNU `cl-lib`, `cl-generic` features stay provided at boot (as now) or become demand-loaded like GNU autoloads?
4. Fate of the foundation packages (emacs-cl-macros.el, emacs-pcase.el, cl-lib.el shim) used by the host-side nelisp-emacs bundle: delete, or keep for the host bundle only.
5. Is closure-length parity (3 vs 5 slots for evaluator-made closures) in scope as a core evaluator fix?
6. Stop point: is S1.4 (url-parse) the acceptance target, or all of P3-P6 including a performance gate before deleting natives?

## 6. Decisions (user, 2026-11-05)

1. Parallel period: keep the native pcase/cl-defstruct/cl-defgeneric until the
   GNU replacement passes the differential corpus and the performance checks;
   only then delete the native version.  Each step stays one revertible commit.
2. Performance: baking the GNU cl files into the image as byte-code through the
   existing vendor byte-code transform is allowed; cold startup time and
   pcase-heavy code must not regress beyond noise.
3. The nelisp-emacs-lib bundle moves to the GNU files too; its foundation
   copies (emacs-pcase.el, emacs-cl-macros.el, cl-lib.el shim) are deleted.
4. Stop point: all of P3-P6 including the performance gate and deletion of the
   native versions (S1.4 url-parse green on the way).
Defaults taken without asking: follow GNU for what is preloaded (cl-preloaded,
oclosure, cl-generic at boot; cl-lib on demand); closure-length parity (5 slots)
is an in-scope core fix, scheduled after P6.
