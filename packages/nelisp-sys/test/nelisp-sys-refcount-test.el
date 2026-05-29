;;; nelisp-sys-refcount-test.el --- Doc 133 P2 refcount kernel lowering -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 Phase 2: the box refcount kernel (clone = inc, drop-step = dec)
;; expressed in nelisp-sys, composing Phase 0 atomics (sys:atomic-add!/
;; sub!/cas) with Phase 1 struct layout (sys:offsetof on the shadow
;; boxes) + raw pointer arithmetic (sys:ptr-add).  These lower to the
;; Phase 47 atomic ops the existing Doc 122.E / 124 grammar already
;; emits to native code.  This test pins the *lowering* (parse -> check
;; -> Phase 47 form); the native codegen for atomic-fetch-add /
;; atomic-compare-exchange is already shipped + tested at the Phase 47
;; layer.  Full standalone-binary e2e lands once nelisp-sys freestanding
;; entry ships (Doc 133 Phase 7).

;;; Code:

(require 'ert)
(require 'nelisp-sys-driver)
(require 'nelisp-sys-backend)

(defconst nelisp-sys-refcount-test--lx "x86_64-unknown-linux-gnu")

(defun nelisp-sys-refcount-test--lower (forms)
  (nelisp-sys-backend-lower-module
   (nelisp-sys-frontend-parse-module forms)
   nelisp-sys-refcount-test--lx))

;; The shadow boxes (must match Doc 133 P1 nelisp-sys-sexp-shadow):
;; NlConsBox refcount @64 (car@0, cdr@32, both 32-byte Sexp slots).
(defconst nelisp-sys-refcount-test--boxes
  '((sys:defstruct sexp (:repr c)
      (tag u8) (payload u64) (pad (array u8 16)))
    (sys:defstruct nlconsbox (:repr c)
      (car (struct sexp)) (cdr (struct sexp)) (refcount u64))))

(ert-deftest nelisp-sys-refcount-clone-inc ()
  "Box clone = atomic increment of the refcount field.
rc-inc(p) -> atomic-fetch-add(*(p + offsetof(nlconsbox,refcount)), 1)
           -> (atomic-fetch-add (+ p 64) 1)."
  (should (equal '(defun rc_inc (p) (atomic-fetch-add (+ p 64) 1))
                 (nelisp-sys-refcount-test--lower
                  (append nelisp-sys-refcount-test--boxes
                          '((sys:defun rc_inc ((p usize)) i64 ()
                              (sys:atomic-add!
                               (+ p (sys:offsetof nlconsbox refcount))
                               1))))))))

(ert-deftest nelisp-sys-refcount-drop-dec ()
  "Box drop-step = atomic decrement; returns the PREVIOUS count so the
caller frees when it observes 1.  rc-dec(p) -> (atomic-fetch-add (+ p 64) (- 0 1))."
  (should (equal '(defun rc_dec (p) (atomic-fetch-add (+ p 64) (- 0 1)))
                 (nelisp-sys-refcount-test--lower
                  (append nelisp-sys-refcount-test--boxes
                          '((sys:defun rc_dec ((p usize)) i64 ()
                              (sys:atomic-sub!
                               (+ p (sys:offsetof nlconsbox refcount))
                               1))))))))

(ert-deftest nelisp-sys-refcount-cas-promote ()
  "CAS on the refcount slot (Bacon-Rajan style promote gate)."
  (should (equal '(defun rc_cas (p e n) (atomic-compare-exchange (+ p 64) e n))
                 (nelisp-sys-refcount-test--lower
                  (append nelisp-sys-refcount-test--boxes
                          '((sys:defun rc_cas ((p usize) (e i64) (n i64)) i64 ()
                              (sys:cas
                               (+ p (sys:offsetof nlconsbox refcount))
                               e n))))))))

(ert-deftest nelisp-sys-refcount-peek-poke-lower ()
  "Doc 133 P2: raw u64 box-field read/write (sys:peek-u64 / sys:poke-u64)
on a usize address lower to the Phase 47 ptr-read-u64 / ptr-write-u64
ops, composing with the usize field-offset arithmetic."
  (should (equal '(defun rd (p) (ptr-read-u64 (+ p 64) 0))
                 (nelisp-sys-refcount-test--lower
                  (append nelisp-sys-refcount-test--boxes
                          '((sys:defun rd ((p usize)) i64 ()
                              (sys:peek-u64
                               (+ p (sys:offsetof nlconsbox refcount)))))))))
  (should (equal '(defun wr (p) (ptr-write-u64 (+ p 64) 0 1))
                 (nelisp-sys-refcount-test--lower
                  (append nelisp-sys-refcount-test--boxes
                          '((sys:defun wr ((p usize)) i64 ()
                              (sys:poke-u64
                               (+ p (sys:offsetof nlconsbox refcount)) 1))))))))

;; NOTE (Doc 133 P2 finding): the full alloc->clone->drop->read e2e in a
;; *freestanding* standalone binary is blocked because `alloc-bytes'
;; calls the Rust runtime's `nl_alloc_bytes', which a freestanding binary
;; does not link (no heap allocator outside the Rust runtime).  A native
;; refcount e2e therefore needs either (a) an mmap-syscall freestanding
;; allocator, or (b) static-data addressing — a Phase 2/3 sub-task.  The
;; refcount/peek/poke/alloc *lowering* is pinned above + in the backend
;; tests; the native codegen for each underlying Phase 47 op is already
;; shipped + tested at the grammar layer.

;;; nelisp-sys-refcount-test.el ends here
