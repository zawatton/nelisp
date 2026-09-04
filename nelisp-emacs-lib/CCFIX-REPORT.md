# CCFIX Report

Date of this session: 2026-07-20.

The handoff file is dated 2026-07-21. I treat the numbers in that handoff as the supplied baseline reference, not as a new local measurement from this session.

## Fix

Changed [src/emacs-parity-cc.el](/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/src/emacs-parity-cc.el:83) in two ways:

1. Replaced the standalone-only identity `c--macroexpand-all` override with a safe hand-written walker that rewrites only runtime `c-lang-const` calls into `c-get-lang-constant` calls.
2. Made the walker quasiquote-aware for NeLisp's `(backquote ...)`, `(comma ...)`, and `(comma-at ...)` reader forms so evaluation positions inside backquote templates are rewritten while literal template data is left alone.
3. Added standalone-only memoization for `c-get-lang-constant`, keyed by `(name . mode)`, plus full cache invalidation on every `c-define-lang-constant` registration so later definitions cannot return stale values.

Relevant code:

- Rewrite walker and quasiquote handling: [src/emacs-parity-cc.el](/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/src/emacs-parity-cc.el:115)
- Memoization and invalidation: [src/emacs-parity-cc.el](/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/src/emacs-parity-cc.el:201)
- Standalone-only install hook: [src/emacs-parity-cc.el](/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/src/emacs-parity-cc.el:238)

Added focused host ERTs:

- [test/emacs-parity-cc-test.el](/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/test/emacs-parity-cc-test.el:14)

## Verification

Host checks passed on 2026-07-20:

- `emacs -Q --batch -L src -L test -l test/emacs-parity-cc-test.el -f ert-run-tests-batch-and-exit`
  - 4/4 tests passed.
- `emacs -Q --batch -L vendor/emacs-lisp/progmodes -L src ...`
  - `HOST_SAFE_OK`
  - Loading `src/emacs-parity-cc.el` did not replace host Emacs's `c--macroexpand-all`.

Focused host rewrite sanity check passed:

- `UNQ=(comma (c-get-lang-constant 'c-symbol-start))`
- `BQ=(backquote ((comma (c-get-lang-constant 'c-symbol-start)) (comma-at (list (c-get-lang-constant 'c-simple-ws))) (c-lang-const c-symbol-chars)))`

## Throughput Numbers

Baseline from the 2026-07-21 handoff reference:

- `font-lock dt=317.411 cdlc=0 cglc=0`
- `cc-langs dt=764.182 cdlc=336 cglc=0`
- `cc-fonts dt=778.892 cdlc=336 cglc=0`
- `cc-fonts dt=1597.958 cdlc=344 cglc=160`
- `geiser dt=1691.168 cdlc=345 cglc=160`

Observed in this 2026-07-20 session before the memoization follow-up edit:

- `font-lock dt=311.027 cdlc=0 cglc=0`
- `cc-langs dt=750.288 cdlc=336 cglc=0`
- `cc-fonts dt=764.782 cdlc=336 cglc=0`

Observed after the memoization follow-up edit:

- `font-lock dt=307.653 cdlc=0 cglc=0`

I do not yet have a completed post-fix `cc-fonts dt=... cglc=160` or `geiser dt=...` measurement from the final `A+B` version. I interrupted repeated long runs to refine the walker and then add memoization, and the last run was stopped before it reached the `cc-*` block again.

## Value Equality

What is established:

- The partial expander now rewrites to the same runtime operator CC Mode uses for `c-lang-const`: `c-get-lang-constant`.
- The rewrite is constrained to evaluation positions, including quasiquote evaluation positions, and leaves quoted/literal template data alone.

What I did not finish proving in this session:

- A fresh standalone sample recheck of `(c-get-lang-constant 'c-symbol-start nil 'c-mode)` against the expected `"[a-zA-Z_$]"`.

## Residual

- The main throughput success condition is not yet closed. The final `A+B` build still needs one uninterrupted retry-safe standalone run that reaches:
  - `cc-fonts` with `cglc≈160`
  - the next provide after that
  - ideally `geiser`
- The long-running standalone err stream still shows the same family of residual failures seen in the handoff area, including:
  - `nelisp-rx-syntax-error`
  - `java-font-lock-keywords-*` bare aborts
  - `c-sentence-end-with-esc-eol`
- Those residuals do not prove the new rewrite is wrong, but they currently prevent me from claiming the 819s block is collapsed.

## Suggested Next Step

Run one uninterrupted final `A+B` measurement with the existing retry harness and let it continue until either:

1. `cc-fonts` reaches `cglc≈160` and the next provide arrives far earlier than `1597.958`, or
2. the run clearly remains stuck in the same long resolution block, in which case the next investigation target should be whether the remaining cost is inside repeated failing `c-matchers-*` / `java-font-lock-keywords-*` paths rather than plain `c-lang-const` expansion.
