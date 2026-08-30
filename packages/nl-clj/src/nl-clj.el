;;; nl-clj.el --- A Clojure-compat persistent-data library for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Umbrella file: `(require 'nl-clj)' pulls in the build-first Tier 1
;; slice this package ships (docs/design/195-clojure-compat-library.org's
;; build-first ranking, §6, items 1-3 plus the seq slice of items in
;; between it) PLUS the lazy-seq phase (§4.7's deferred half):
;;
;;   nl-clj-core.el    tags, errors (no public API of its own)
;;   nl-clj-atom.el    atom / deref / swap! / reset! / compare-and-set!
;;   nl-clj-vector.el  persistent vector (32-way bit-partitioned trie)
;;   nl-clj-hash.el    persistent hash-map / hash-set (HAMT)
;;   nl-clj-seq.el     generic seq/conj/assoc/get/map/filter/reduce/...
;;                     across all three, plus ordinary Elisp lists AND
;;                     (this phase) lazy seqs, transparently
;;   nl-clj-lazy.el    lazy-seq / lazy-cons / take / drop / take-while /
;;                     drop-while / range (bounded and unbounded) /
;;                     iterate / repeat / cycle / doall / dorun
;;
;; Deliberately out of scope still (Doc 195 §5/§6's own phasing; this
;; package's own build-first brief): transients (§4.4), channels and
;; `go' blocks over `nelisp-actor' (§4.6), refs/STM (§5.1), and true
;; multicore (§5.2).  See this package's README.org "Next phases" section.
;;
;; Naming (Doc 195 §3.1): every public symbol is `nl-clj-' prefixed.
;; `nl-ns-in' sugar (packages/nl-ns) is optional, demonstrated in
;; README.org, and never required to call any function here directly.

;;; Code:

(require 'nl-clj-core)
(require 'nl-clj-atom)
(require 'nl-clj-vector)
(require 'nl-clj-hash)
(require 'nl-clj-seq)
(require 'nl-clj-lazy)

(provide 'nl-clj)

;;; nl-clj.el ends here
