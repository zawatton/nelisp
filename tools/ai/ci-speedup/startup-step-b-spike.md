# Step B feasibility spike: heap image of the artifact/source substrate

Date 2026-11-11. Binary: `make standalone-reader` output of this tree, run WITHOUT its
`.cold` companion (a no-cold boot, as the artifact commands see today). All runs:
`ulimit -v 8388608`, `env -i PATH=/usr/bin:/bin LANG=C.UTF-8 HOME=/nonexistent`,
neutral cwd, stdin /dev/null. Nothing here is committed to the build.

## Result: dump succeeds when the substrate is evaluated form by form

- `--eval '(progn (load SUBSTRATE.el) <cold-build scrub> (garbage-collect)
  (nelisp--arena-dump-image-stream OUT))'` FAILS even with the scrub
  (`nelisp--fx-register-prelude`, `nlre--nat-scratch` nil, garbage-collect):
  "heap image has ~22700 out-of-image references" (varies per run, 22675-22716).
  Holders and targets both lie in 0x7f.. mappings; offender kinds 3080/2568, slots 0/1,5,8.
  The scrub alone therefore does not fix it.
- Reading SUBSTRATE.el (6.3 MB, `nelisp-standalone--artifact-source-command-substrate-src`)
  into a buffer and evaluating each form with `(eval form t)`, then the same scrub, and
  dumping succeeds at every checkpoint (every 250-400 forms), including after all 5503 forms:
  - empty-ish baseline 17,645,108 bytes; after prelude part ~30 MB; final 38,437,681 bytes.
- So the out-of-image references come from `load` of the large file (its read/loader state
  holds large out-of-chunk blocks), not from the substrate's content (jit/ffi/process-buffer
  all dump fine). Not root-caused further.

## Restore and dispatch

- `nelisp --cold-load-from sub.cold --eval '(princ (fboundp (quote nelisp-read-from-string)))'`:
  0.012 s per launch (3 runs), prints `t` (substrate present).
- Dispatch tail of `artifact-direct-command-src` (1302 bytes) run in the restored world
  with `nelisp-standalone-argv` = (exec-elisp-artifact art.el.neln "(princ (ps-shim 1))"):
  stdout `2`, exit 0, 0.26 s wall total (vs 7.0 s today, which prints the same `2`).
  The 0.26 s is mostly loading the artifact + the tail via `--load`; the restore itself is 0.012 s.

## Caveats / not verified

- Form-by-form eval is not identical to `load`: no `load-file-name`/`load-in-progress`,
  `load-history`/`features` order and per-form side effects must be compared (design 5c-style probes).
  The earlier run with plain eval (no lexical-binding) mis-evaluated some forms; `(eval form t)` is required.
- Only exec-elisp-artifact tail was run, not eval-elisp-source's cache tail nor `--cold-load-from`
  inside the real driver (driver still takes the current source path; image selection, digest of
  SUBSTRATE.el, argv re-init are unimplemented).
- Image is 38 MB (2x the REPL cold image).

## Recommendation: GO (conditional)
Implement Step B with a build-time dumper that evaluates the substrate form by form (not `load`).
First gate: compare `features`/`load-history`/a few `fboundp` probes between image and source modes.
