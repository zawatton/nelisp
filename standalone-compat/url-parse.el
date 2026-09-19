;;; url-parse.el --- minimal url-parse for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-loadpath: same shape as `standalone-compat/subr-
;; x.el' -- discovered only once `-L' let more of the ~80 host-only test
;; files' load closure run far enough to reach it (absent from the
;; bare-`--load' "Measured facts" this segment started from; present
;; under `-L lisp -L ../nelisp-llm/lisp -L ../nelisp-photon/lisp').  The
;; standalone `require' searches `load-path' for a FILE named after the
;; feature and never finds one named "url-parse" (no `lisp/url-parse.el'
;; or similar), so `(require 'url-parse)' died `file-missing: url-parse'
;; before any of its actual names were ever looked up.  This directory is
;; on the standalone's default `load-path' (see `nelisp-standalone--
;; reader-default-load-path' in scripts/nelisp-standalone-build.el) but
;; NOT on any host `-L' path used by `make test' (`make -n test | grep -c
;; standalone-compat' is 0), so it never shadows anything for the host
;; Emacs suite.
;;
;; What actually needs to exist, checked two ways:
;;
;;   1. `(require 'url-parse)' (unconditional, no NOERROR) appears at the
;;      top level of THREE files under ../nelisp-agent/examples/ that the
;;      80 host-only test files `load' directly (not `require' --
;;      `semantic-render-test.el'/`bulk-eval-test.el'/
;;      `bulk-policy-eval-test.el' each `(load (expand-file-name
;;      "examples/....el" ...) nil t)'): `semantic-render-example.el',
;;      `evaluate-bulk-reader.el', `evaluate-semantic-render.el'.  Every
;;      `url-*' name they call (`url-generic-parse-url', `url-type',
;;      `url-host', `url-user', `url-password') is called ONLY from
;;      inside a `defun' body (`nl-agent-*-loopback-url-p', each guarding
;;      a live network request against loopback -- irrelevant to this
;;      segment, whose tests never make one), never at the file's own
;;      top level, so none of them can block `--load' from reaching `Ran
;;      N tests' -- only a later test that actually CALLS one of those
;;      functions could (out of scope: "whether the tests PASS or FAIL
;;      inside ERT is NOT this segment").  Neither nelisp-llm nor
;;      nelisp-photon requires `url-parse' anywhere in their own `lisp/'.
;;
;;   2. Probe on target/nelisp with `fboundp' (2026-09-19 22:52 build,
;;      before this file existed): `url-generic-parse-url'/`url-type'/
;;      `url-host'/`url-user'/`url-password'/`url-recreate-url'/
;;      `url-hexify-string'/`url-retrieve-synchronously' are all
;;      fboundp=nil -- genuinely absent, not merely un-`provide'd like
;;      `seq'/`simple' turned out to be.
;;
;; Net: nothing here needs a body for THIS segment's criterion (reaching
;; `Ran N tests').  `url-parse' was missing as a loadable FEATURE, and
;; also genuinely missing its real functionality -- but nothing in this
;; corpus's LOAD path (as opposed to individual tests' bodies) calls any
;; of it, so `(provide 'url-parse)' is deliberately the whole file.  A
;; real `url-generic-parse-url' (Emacs's is a `cl-defstruct url' with
;; type/user/password/host/port/filename/target/attributes/fullness
;; slots plus an RFC-3986-ish parser) is a substantially bigger task than
;; `seq'/`subr-x''s missing-`provide'-only fixes and belongs with
;; whichever later segment makes these tests' bodies (not just their
;; load) pass -- add it here the same way `standalone-compat/json.el'
;; adds JSON names when that becomes the task, each with a differential
;; against host Emacs 31.1's `url-parse.el'.

;;; Code:

(provide 'url-parse)

;;; url-parse.el ends here
