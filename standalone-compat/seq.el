;;; seq.el --- minimal seq for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-loadpath: same shape as `standalone-compat/subr-
;; x.el' -- the standalone `require' searches `load-path' for a FILE named
;; after the feature and never finds one named "seq" (no `lisp/seq.el' or
;; similar), so `(require 'seq)' died `file-missing: seq' before any of
;; its actual names were ever looked up.  This directory is on the
;; standalone's default `load-path' (see `nelisp-standalone--reader-
;; default-load-path' in scripts/nelisp-standalone-build.el) but NOT on
;; any host `-L' path used by `make test' (`make -n test | grep -c
;; standalone-compat' is 0), so it never shadows anything for the host
;; Emacs suite.
;;
;; What actually needs to exist, checked two ways:
;;
;;   1. `grep -rhoE '\(seq-[a-z-]+' ../nelisp-agent/test/*.el
;;      ../nelisp-agent/lisp/*.el ../nelisp-llm/lisp/*.el
;;      ../nelisp-photon/lisp/*.el' -- three names anywhere in that
;;      corpus: `seq-filter', `seq-some', `seq-take' (llm/photon use
;;      none).
;;
;;   2. Probe on target/nelisp with `fboundp' (2026-09-19 22:52 build,
;;      before this file existed):
;;
;;        seq-filter: fboundp=t   seq-some: fboundp=t   seq-take: fboundp=t
;;
;;      (also checked and already present, though unused by this corpus:
;;      seq-map, seq-reduce, seq-find, seq-contains-p, seq-empty-p,
;;      seq-length, seq-elt, seq-remove.)
;;
;;      All three are already native standalone primitives -- the
;;      stdlib prelude (scripts/nelisp-stdlib-prelude.el) defines the
;;      common `seq-*' names directly, it just never called `(provide
;;      'seq)'.
;;
;; Net: nothing here needs a body.  `seq' was missing only as a loadable
;; FEATURE, never as missing functionality -- `(provide 'seq)' is
;; therefore the whole file.  If a future test corpus calls a real `seq'
;; name this probe found absent, add it here the same way
;; `standalone-compat/json.el' adds JSON names: `(unless (fboundp 'NAME)
;; (defun NAME (...) ...))', with a differential against host Emacs 31.1.

;;; Code:

(provide 'seq)

;;; seq.el ends here
