;;; subr-x.el --- minimal subr-x for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-compat: the standalone `require' searches
;; `load-path' for a FILE named after the feature and never finds one
;; named "subr-x" (there is no `lisp/subr-x.el' or similar) -- every
;; `(require 'subr-x)' in the nelisp-agent host-only test corpus dies
;; `file-missing: subr-x' before any of its actual names are ever looked
;; up.  This directory is on the standalone's default `load-path' (see
;; `nelisp-standalone--reader-default-load-path' in
;; scripts/nelisp-standalone-build.el) but NOT on any host `-L' path used
;; by `make test' (verified: `make -n test | grep -c standalone-compat'
;; is 0), so it never shadows anything for the host Emacs suite.
;;
;; What actually needs to exist, checked two ways:
;;
;;   1. `grep' every subr-x-shaped name
;;      (string-join/string-trim(-left/-right)?/string-empty-p/
;;      string-blank-p/string-remove-prefix/-suffix/string-chop-newline/
;;      string-pad/when-let\*?/if-let\*?/and-let\*?/thread-first/
;;      thread-last/named-let/hash-table-keys/-values/-empty-p/
;;      ensure-list/xor/always) across ../nelisp-agent/test/*.el AND
;;      ../nelisp-agent/lisp/*.el (the test files' own `require'd
;;      dependencies, also loaded during `--load').  Only six are used
;;      anywhere in that corpus: `string-join', `string-trim',
;;      `string-trim-right', `string-empty-p', `always' (`string-trim-
;;      left' never appears; it was only a candidate name checked below).
;;
;;   2. Probe each on target/nelisp with `fboundp' (measured against the
;;      2026-09-19 15:26 build, before this file existed):
;;
;;        string-join: fboundp=t   string-trim: fboundp=t
;;        string-trim-left: fboundp=t   string-trim-right: fboundp=t
;;        string-empty-p: fboundp=t   always: fboundp=t
;;
;;      All six are already native standalone primitives -- unsurprising
;;      once cross-checked against what real Emacs 31.1 (this tree's own
;;      compat oracle) actually ships in `subr-x.el' today via
;;      `load-history': thread-first/thread-last/hash-table-empty-p/
;;      hash-table-keys/hash-table-values/string-join/string-reverse/
;;      string-truncate-left/string-blank-p/string-remove-prefix/-suffix/
;;      string-clean-whitespace/string-fill/string-limit/string-pad/
;;      string-chop-newline/named-let/(GUI work-buffer/pixel-width names,
;;      irrelevant here).  `string-trim'/`string-trim-left'/
;;      `string-trim-right'/`string-empty-p'/`always'/`when-let'/`if-let'/
;;      `and-let*'/`ensure-list'/`xor' moved into `subr.el'/`simple.el'
;;      (core, no `require' needed) well before 31.1 -- `subr-x.el' in
;;      this Emacs version does not even define most of the names a
;;      pre-30 memory of "what's in subr-x" would expect, and the
;;      standalone's own prelude already tracks the modern (31.1) split.
;;
;; Net: nothing here needs a body.  `subr-x' was missing only as a
;; loadable FEATURE, never as missing functionality -- `(provide 'subr-x)'
;; is therefore the whole file.  If a future test corpus calls a real
;; subr-x name this probe found absent, add it here the same way
;; `standalone-compat/json.el' adds JSON names: `(unless (fboundp 'NAME)
;; (defun NAME (...) ...))'.

;;; Code:

(provide 'subr-x)

;;; subr-x.el ends here
