;;; nelisp-aot-doc147-groupA-test.el --- Doc 147 Phase 1.5 Group A guard  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 147 Phase 1.5 Group A reachability regression guard.
;;
;; The container-shrink (Doc 147 Phase 2) will make Vector/Record data
;; buffers 8B-per-slot.  Any site that writes a 32B Sexp through a raw
;; interior pointer `(vector-ref-ptr SCRATCH INDEX)' into a buffer that
;; will shrink would corrupt neighbours.
;; `nelisp-aot-compiler--scratch-slot' emits exactly such interior write
;; dests for the literal materializer
;; (`--top-level-literal-write-forms', `--top-level-make-hash-table-write-forms').
;;
;; Group A's STEP-1 conclusion (2026-06-07): that native WRITE path is
;; NOT reachable at runtime in the standalone reader.  These tests pin
;; the three pillars of that conclusion so a future change that makes it
;; reachable trips red here instead of corrupting heap neighbours under
;; the Phase-2 layout:
;;
;;   1. The reader-unit compile path NEVER lowers top-level var forms, so
;;      `--scratch-slot' / the literal materializer are not even invoked
;;      while building the standalone reader.
;;   2. The host AOT init context's SCRATCH is a host Emacs vector, not a
;;      native NlVector (so it is immune to the container shrink anyway).
;;   3. The materializer's interior-write surface still routes through the
;;      single `--scratch-slot' chokepoint (so the chokepoint analysis,
;;      and any future Group-A migration, stays valid).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-runtime)

(defun nelisp-aot-doc147-groupA-test--load-standalone-build ()
  "Load `nelisp-standalone-build' from this repo's scripts/ if needed."
  (unless (featurep 'nelisp-standalone-build)
    (let* ((this (or load-file-name buffer-file-name
                     (locate-library "nelisp-aot-doc147-groupA-test")))
           (root (and this (file-name-directory
                            (directory-file-name
                             (file-name-directory this)))))
           (scripts (and root (expand-file-name "scripts" root))))
      (when (and scripts (file-directory-p scripts))
        (add-to-list 'load-path scripts))
      (require 'nelisp-standalone-build nil t))))

(ert-deftest nelisp-aot-doc147-groupA/reader-build-never-emits-scratch-slot ()
  "Group A pillar 1: building the reader units invokes neither
`--scratch-slot' nor the literal materializer nor top-level-var
lowering.  If this fails, the native scratch-slot interior write
surface has become reachable in the standalone-reader build and Doc
147 Phase 2 (container shrink) would corrupt heap neighbours through
it -- migrate the native scratch to a raw alloc-bytes(N*32) buffer
before flipping the Vector/Record layout."
  (skip-unless (nelisp-aot-doc147-groupA-test--load-standalone-build))
  (let ((scratch-calls 0)
        (materializer-calls 0)
        (lower-calls 0))
    (cl-letf* ((orig-scratch
                (symbol-function 'nelisp-aot-compiler--scratch-slot))
               (orig-mat
                (symbol-function
                 'nelisp-aot-compiler--top-level-literal-write-forms))
               (orig-lower
                (symbol-function
                 'nelisp-aot-compiler--lower-top-level-var-form)))
      (cl-letf (((symbol-function 'nelisp-aot-compiler--scratch-slot)
                 (lambda (&rest args)
                   (cl-incf scratch-calls)
                   (apply orig-scratch args)))
                ((symbol-function
                  'nelisp-aot-compiler--top-level-literal-write-forms)
                 (lambda (&rest args)
                   (cl-incf materializer-calls)
                   (apply orig-mat args)))
                ((symbol-function
                  'nelisp-aot-compiler--lower-top-level-var-form)
                 (lambda (&rest args)
                   (cl-incf lower-calls)
                   (apply orig-lower args))))
        (funcall (intern "nelisp-standalone--reader-units"))))
    (should (= scratch-calls 0))
    (should (= materializer-calls 0))
    (should (= lower-calls 0))))

(ert-deftest nelisp-aot-doc147-groupA/host-init-scratch-is-host-vector ()
  "Group A pillar 2: the default AOT init context SCRATCH is a host
Emacs vector, not a native NlVector, so it is not subject to the Doc
147 Phase 2 container-slot 32B->8B shrink."
  (let ((ctx (nelisp-cc-runtime-make-aot-init-context)))
    (should (vectorp (plist-get ctx :scratch)))))

(ert-deftest nelisp-aot-doc147-groupA/materializer-routes-through-scratch-slot ()
  "Group A pillar 3: the literal materializer still funnels its interior
write dests through the single `--scratch-slot' chokepoint, so the
reachability analysis (and any future Group-A migration) keeps a single
edit point.  A literal-heavy form (nested list + vector + hash table)
must produce at least one `vector-ref-ptr' scratch write dest."
  (let* ((seen nil)
         (orig (symbol-function 'nelisp-aot-compiler--scratch-slot)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--scratch-slot)
               (lambda (scratch index)
                 (let ((form (funcall orig scratch index)))
                   (push form seen)
                   form))))
      ;; Nested aggregate literal -> exercises the cons + vector arms.
      (nelisp-aot-compiler--top-level-literal-write-forms
       'out '((a . 1) [x y] "s") 'scratch 0)
      ;; make-hash-table arm -> exercises the tag-slot write dest.
      (nelisp-aot-compiler--top-level-make-hash-table-write-forms
       'out '(make-hash-table :test 'equal) 'scratch))
    (should seen)
    (should (cl-every (lambda (f)
                        (and (consp f) (eq (car f) 'vector-ref-ptr)))
                      seen))))

(provide 'nelisp-aot-doc147-groupA-test)
;;; nelisp-aot-doc147-groupA-test.el ends here
