;;; simple.el --- minimal simple for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-loadpath: same shape as `standalone-compat/subr-
;; x.el' -- the standalone `require' searches `load-path' for a FILE
;; named after the feature and never finds one named "simple" (no
;; `lisp/simple.el' or similar), so `../nelisp-agent/lisp/nl-agent-ui.el's
;; `(require 'simple)' died `file-missing: simple' before any of its
;; actual names were ever looked up.  This directory is on the
;; standalone's default `load-path' (see `nelisp-standalone--reader-
;; default-load-path' in scripts/nelisp-standalone-build.el) but NOT on
;; any host `-L' path used by `make test' (`make -n test | grep -c
;; standalone-compat' is 0), so it never shadows anything for the host
;; Emacs suite.
;;
;; What actually needs to exist, checked two ways:
;;
;;   1. `nl-agent-ui.el' is the ONLY `(require 'simple)' in the 80
;;      host-only test files' load closure (../nelisp-agent/test/*.el,
;;      ../nelisp-agent/lisp/*.el).  Grepping that whole file (not just
;;      its top level -- every name it calls anywhere, since a name
;;      called only from inside a `defun' body cannot block `--load'
;;      reaching `Ran N tests', only a later test invocation of that
;;      function could, which is out of this segment's scope) for the
;;      candidate simple.el names below found ZERO call sites for all but
;;      `yes-or-no-p' (called twice, both inside `defun' bodies --
;;      `nl-agent-ui--approve-tool-call'/`nl-agent-ui--disconnect', never
;;      at `nl-agent-ui.el's top level, so loading the file never touches
;;      it): count-lines, delete-trailing-whitespace,
;;      beginning-of-buffer, end-of-buffer, newline, open-line,
;;      delete-blank-lines, just-one-space, back-to-indentation (already
;;      native), kill-word, zap-to-char, transpose-chars/-words/-lines/
;;      -sexps, upcase-word, downcase-word, capitalize-word, kill-line,
;;      kill-whole-line, delete-indentation, forward-line/current-column
;;      (already native), move-to-column, read-from-minibuffer,
;;      completing-read, yes-or-no-p, y-or-n-p.
;;
;;   2. Probe on target/nelisp with `fboundp' (2026-09-19 22:52 build,
;;      before this file existed) confirms most of the above ARE missing
;;      (`line-number-at-pos', `back-to-indentation', `forward-line',
;;      `current-column' are already native) -- but since none of them
;;      are reached before the file's own top-level forms finish
;;      evaluating, none of them block `--load' from reaching `Ran N
;;      tests' for any of the 80 files.  `test/ui-test.el' additionally
;;      stubs `yes-or-no-p' out entirely with `cl-letf' before any test
;;      that would call it runs, so even a REAL `yes-or-no-p' body is
;;      never reached from that file either.
;;
;; Net: nothing here needs a body for THIS segment's criterion (reaching
;; `Ran N tests').  `simple' was missing only as a loadable FEATURE --
;; `(provide 'simple)' is therefore the whole file.  Whether
;; `nl-agent-ui.el's actual UI functions work when a test later CALLS them
;; (`yes-or-no-p' foremost) is a pass/fail-inside-ERT question, explicitly
;; out of scope here; add real bodies the same way
;; `standalone-compat/json.el' adds JSON names when that becomes the
;; task, each `(unless (fboundp 'NAME) (defun NAME (...) ...))' with a
;; differential against host Emacs 31.1.

;;; Code:

(provide 'simple)

;;; simple.el ends here
