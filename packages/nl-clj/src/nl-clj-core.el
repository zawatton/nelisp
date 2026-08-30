;;; nl-clj-core.el --- Shared tags and errors for nl-clj -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §3.2: every
;; nl-clj collection is a plain Elisp vector whose slot 0 is a
;; distinguishing tag symbol -- the same idiom `nl-cell' already ships
;; (`packages/nl-safe/src/nl-safe.el': `(vector 'nl--cell value 0)') --
;; NOT a `cl-defstruct'/`record'.  Doc 195 §2.1 measured, against a
;; built `target/nelisp', that records print with real Emacs
;; `#s(...)' syntax but do not round-trip through `read-from-string'
;; on this substrate (the standalone reader's `#s(...)' handler
;; reconstructs a broken instance -- a genuine print/read asymmetry,
;; not merely "records are unreliable"), while a plain tagged vector
;; round-trips correctly.  This file holds the tag symbols and the
;; one shared tag-check helper every other nl-clj module uses, so the
;; representation convention lives in exactly one place.
;;
;; Representation (Doc 195 §3.2):
;;
;;   persistent vector: [nl-clj--pvec COUNT SHIFT ROOT TAIL]
;;   persistent map:    [nl-clj--pmap COUNT ROOT]
;;   persistent set:    [nl-clj--pset COUNT ROOT]
;;   persistent list:   ordinary cons cells, nil-terminated -- already
;;                      Clojure's own representation, no wrapper needed
;;   atom:              [nl-clj--atom VALUE WATCHES]
;;   lazy-seq cell:     [nl-clj--lazy CELL]  CELL an `nl-cell' (`nl-safe',
;;                      packages/nl-clj/src/nl-clj-lazy.el) wrapping
;;                      (THUNK . REALIZED-VALUE-OR-NIL), exactly Doc 195
;;                      §3.2's own sketch -- see nl-clj-lazy.el's
;;                      Commentary for the realize-once/reentrancy engine
;;                      built on it.
;;
;; Doc 195 §3.2 itself describes the set representation as literally
;; sharing the map's `nl-clj--pmap' tag ("a map whose values are all
;; the sentinel `nl-clj--set-member'").  This package uses a distinct
;; `nl-clj--pset' tag instead -- a deliberate, small divergence from
;; that literal sketch: sharing one tag would make `nl-clj-map-p' and
;; `nl-clj-set-p' unable to tell a real map from a set without walking
;; every value and checking it against the sentinel (and, in the
;; limit, a genuine map whose values all happen to equal the sentinel
;; would misreport as a set).  A second tag costs nothing -- both
;; still route through the identical HAMT engine in `nl-clj-hash.el',
;; sharing every node-assoc/node-get/node-dissoc function; only the
;; envelope tag differs.  Doc 195 is itself a DRAFT survey ("Nothing
;; in this doc is implemented" -- its own Status line); its measured
;; facts and decisions (tagged vectors, HAMT design, naming) are
;; followed exactly, this one representation micro-detail is refined.
;;
;; Trie *internal* nodes (branch arrays inside a vector or a HAMT) are
;; deliberately untagged plain Elisp vectors/conses -- never handed to
;; a caller directly, so there is nothing to discriminate by type at
;; that layer (Doc 195 §3.2).
;;
;; Naming (Doc 195 §3.1): every public name is `nl-clj-' prefixed.
;; Elisp already has `assoc'/`get'/`pop'/`atom'/`concat' with
;; different meanings (measured collisions, Doc 195 §2.6); nl-clj
;; never shadows them.  `nl-ns-in' sugar (packages/nl-ns) is optional
;; opt-in ergonomics, not required by anything in this package.
;;
;; This file depends only on `nl-prelude', for the `nl-error' error
;; condition every nl-clj error derives from (the same convention
;; `nl-safe' and `nl-condition' already use).
;;
;; Forward-reference slots for the lazy-seq phase (packages/nl-clj/src/
;; nl-clj-lazy.el): `nl-clj-seq.el' is the single place every generic,
;; cross-type nl-clj function lives (`nl-clj-seq'/`first'/`rest'/`next'/
;; `map'/`filter'/`reduce'/`count'/`into'/`conj', Doc 195 §4.7), and a
;; lazy value must dispatch through every one of those exactly like a
;; vector/map/set/list does.  But `nl-clj-lazy.el' itself needs the
;; OTHER direction -- a lazy producer (`map'/`filter'/`take'/`range'/...)
;; pulls from an arbitrary source, lazy or eager, via `nl-clj-seq.el''s
;; own generic `nl-clj-seq'/`nl-clj-first'/`nl-clj-rest' -- so
;; `nl-clj-seq.el' cannot `(require 'nl-clj-lazy)' without a require
;; cycle.  These three vars are the break: `nl-clj-lazy.el' sets them,
;; by name, once, right after defining the functions they point at;
;; `nl-clj-seq.el' only ever reads them, and only for the one tag it
;; cannot otherwise recognize (`nl-clj--lazy-tag', above).  All three
;; stay nil until `nl-clj-lazy.el' loads, at which point every value
;; tagged `nl-clj--lazy-tag' was necessarily built by a function in
;; that same file, so a real lazy value existing at all already implies
;; these are populated -- see nl-clj-seq.el's own callers for the loud
;; (not silent) guard on that invariant.

;;; Code:

(require 'nl-prelude)

(defvar nl-clj--lazy-force-fn nil
  "(FUNCALL this COLL) forces lazy-tagged COLL down to a non-lazy seq
view (nil or a cons); installed by `nl-clj-lazy.el' as `nl-clj-lazy--force'.
See this file's Commentary for why this forward-reference lives here.")

(defvar nl-clj--lazy-map-fn nil
  "(FUNCALL this F COLL) returns a new lazy seq of (F x) for each x in
lazy-tagged COLL; installed by `nl-clj-lazy.el' as `nl-clj-lazy-map'.")

(defvar nl-clj--lazy-filter-fn nil
  "(FUNCALL this PRED COLL) returns a new lazy seq of lazy-tagged COLL's
elements matching PRED; installed by `nl-clj-lazy.el' as `nl-clj-lazy-filter'.")

(define-error 'nl-clj-error "nl-clj error" 'nl-error)
(define-error 'nl-clj-index-error "nl-clj index out of bounds" 'nl-clj-error)
(define-error 'nl-clj-type-error "nl-clj wrong collection type" 'nl-clj-error)

(defconst nl-clj--pvec-tag 'nl-clj--pvec
  "Tag symbol (slot 0) of a persistent vector envelope.")
(defconst nl-clj--pmap-tag 'nl-clj--pmap
  "Tag symbol (slot 0) of a persistent map envelope.")
(defconst nl-clj--pset-tag 'nl-clj--pset
  "Tag symbol (slot 0) of a persistent set envelope.")
(defconst nl-clj--atom-tag 'nl-clj--atom
  "Tag symbol (slot 0) of an atom envelope.")
(defconst nl-clj--lazy-tag 'nl-clj--lazy
  "Tag symbol (slot 0) of a lazy-seq envelope.")

(defconst nl-clj--set-member 'nl-clj--set-member
  "Sentinel value stored for every key of a persistent set's backing map.")

(defconst nl-clj--not-found (make-symbol "nl-clj-not-found")
  "Private, `eq'-unique sentinel distinguishing \"absent\" from a stored nil.
Never leaked to a caller -- only used internally to tell `(get m k)'
returning nil because the value IS nil apart from `k' being absent.")

(defun nl-clj--tagged-p (object tag)
  "Return non-nil when OBJECT is a plain vector tagged with TAG in slot 0."
  (and (vectorp object)
       (> (length object) 0)
       (eq (aref object 0) tag)))

(provide 'nl-clj-core)

;;; nl-clj-core.el ends here
