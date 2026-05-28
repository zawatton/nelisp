;;; nelisp-sys-sexp-abi-test.el --- Doc 133 P1 Sexp ABI shadow gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 Phase 1 verification: the nelisp-sys `:repr c' shadow structs
;; (`nelisp-sys-sexp-shadow') reproduce the frozen NeLisp Sexp ABI
;; (`lisp/nelisp-sexp-layout.el') byte-for-byte, computed entirely by the
;; nelisp-sys layout engine (no cargo, no Rust build).  Since `make
;; sexp-abi-check' already pins the Rust ABI to those same elisp
;; constants, "nelisp-sys layout == elisp constants" closes the third
;; side of the triangle (Rust == elisp == nelisp-sys).

;;; Code:

(require 'ert)
(require 'nelisp-sys-abi-layout)
(require 'nelisp-sys-sexp-shadow)
(require 'nelisp-sexp-layout)

(defconst nelisp-sys-sexp-abi-test--lx "x86_64-unknown-linux-gnu")

(defmacro nelisp-sys-sexp-abi-test--off (env struct field)
  `(nelisp-sys-layout-offsetof ',struct ',field
                               nelisp-sys-sexp-abi-test--lx ,env))

(defmacro nelisp-sys-sexp-abi-test--size (env struct)
  `(plist-get (nelisp-sys-layout-struct
               ',struct nelisp-sys-sexp-abi-test--lx ,env)
              :size))

(ert-deftest nelisp-sys-sexp-abi-sexp-slot ()
  "Sexp slot: tag@0, payload@8, size 32 — matches the frozen ABI."
  (let ((env (nelisp-sys-sexp-shadow-env)))
    (should (= nelisp-sexp--offset-tag
               (nelisp-sys-sexp-abi-test--off env sexp tag)))
    (should (= nelisp-sexp--offset-payload
               (nelisp-sys-sexp-abi-test--off env sexp payload)))
    (should (= nelisp-sexp--slot-size
               (nelisp-sys-sexp-abi-test--size env sexp)))))

(ert-deftest nelisp-sys-sexp-abi-nlconsbox ()
  (let ((env (nelisp-sys-sexp-shadow-env)))
    (should (= nelisp-nlconsbox--offset-car
               (nelisp-sys-sexp-abi-test--off env nlconsbox car)))
    (should (= nelisp-nlconsbox--offset-cdr
               (nelisp-sys-sexp-abi-test--off env nlconsbox cdr)))
    (should (= nelisp-nlconsbox--offset-refcount
               (nelisp-sys-sexp-abi-test--off env nlconsbox refcount)))
    (should (= nelisp-nlconsbox--size
               (nelisp-sys-sexp-abi-test--size env nlconsbox)))))

(ert-deftest nelisp-sys-sexp-abi-nlvector ()
  (let ((env (nelisp-sys-sexp-shadow-env)))
    (should (= nelisp-nlvector--offset-value-ptr
               (nelisp-sys-sexp-abi-test--off env nlvector ptr)))
    (should (= nelisp-nlvector--offset-value-capacity
               (nelisp-sys-sexp-abi-test--off env nlvector cap)))
    (should (= nelisp-nlvector--offset-value-length
               (nelisp-sys-sexp-abi-test--off env nlvector len)))
    (should (= nelisp-nlvector--offset-refcount
               (nelisp-sys-sexp-abi-test--off env nlvector refcount)))
    (should (= nelisp-nlvector--size
               (nelisp-sys-sexp-abi-test--size env nlvector)))))

(ert-deftest nelisp-sys-sexp-abi-nlcell ()
  (let ((env (nelisp-sys-sexp-shadow-env)))
    (should (= nelisp-nlcell--offset-value
               (nelisp-sys-sexp-abi-test--off env nlcell value)))
    (should (= nelisp-nlcell--offset-refcount
               (nelisp-sys-sexp-abi-test--off env nlcell refcount)))
    (should (= nelisp-nlcell--size
               (nelisp-sys-sexp-abi-test--size env nlcell)))))

(ert-deftest nelisp-sys-sexp-abi-nlrecord ()
  (let ((env (nelisp-sys-sexp-shadow-env)))
    (should (= nelisp-nlrecord--offset-type-tag
               (nelisp-sys-sexp-abi-test--off env nlrecord type-tag)))
    (should (= nelisp-nlrecord--offset-slots-ptr
               (nelisp-sys-sexp-abi-test--off env nlrecord slots-ptr)))
    (should (= nelisp-nlrecord--offset-slots-capacity
               (nelisp-sys-sexp-abi-test--off env nlrecord slots-cap)))
    (should (= nelisp-nlrecord--offset-slots-length
               (nelisp-sys-sexp-abi-test--off env nlrecord slots-len)))
    (should (= nelisp-nlrecord--offset-refcount
               (nelisp-sys-sexp-abi-test--off env nlrecord refcount)))
    (should (= nelisp-nlrecord--size
               (nelisp-sys-sexp-abi-test--size env nlrecord)))))

(ert-deftest nelisp-sys-sexp-abi-string-header ()
  "String header sits at the Sexp payload offset (+8): the shadow
header offsets + payload-offset reproduce the slot-relative frozen
constants (capacity@8, ptr@16, length@24, header 24 bytes)."
  (let ((env (nelisp-sys-sexp-shadow-env))
        (pay nelisp-sexp--offset-payload))
    (should (= nelisp-string--offset-capacity
               (+ pay (nelisp-sys-sexp-abi-test--off env nlstringheader capacity))))
    (should (= nelisp-string--offset-ptr
               (+ pay (nelisp-sys-sexp-abi-test--off env nlstringheader ptr))))
    (should (= nelisp-string--offset-length
               (+ pay (nelisp-sys-sexp-abi-test--off env nlstringheader length))))
    (should (= nelisp-string--header-size
               (nelisp-sys-sexp-abi-test--size env nlstringheader)))))

;;; nelisp-sys-sexp-abi-test.el ends here
