;;; nelisp-sexp-layout-test.el --- ERT layout tests replacing sexp_abi_assert.rs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests replace the compile-time `const_assert!' checks that
;; formerly lived in `build-tool/src/eval/sexp_abi_assert.rs' (deleted
;; as part of the zero-Rust mandate).  Each test verifies that a
;; constant in `lisp/nelisp-sexp-layout.el' matches the expected value
;; derived from the Rust `Sexp' ABI spec documented in
;; `docs/arch/sexp-abi.md'.
;;
;; NOTE: Rust `const_assert!' ran at COMPILE TIME.  These ERT tests run
;; at TEST TIME (`make test' / CI).  They catch drift only when tests
;; run, which is acceptable per the zero-Rust mandate.  The `make
;; sexp-abi-check' target provides the dynamic cross-check between the
;; compiled Rust binary output and these same constants.

;;; Code:

(require 'ert)
(require 'nelisp-sexp-layout)

(ert-deftest nelisp-sexp-layout--make-test-loads-lisp-dir ()
  "`make test' must include lisp/ so ABI tests can require this module."
  (let* ((root (locate-dominating-file
                (or load-file-name buffer-file-name default-directory)
                "Makefile"))
         (makefile (and root (expand-file-name "Makefile" root))))
    (should makefile)
    (with-temp-buffer
      (insert-file-contents makefile)
      (should (string-match-p "^test: clean\n\t\\$(EMACS) --batch -Q -L lisp "
                              (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Variant tag bytes
;; Each test mirrors one `assert!(SEXP_TAG_XXX == N)' from the deleted file.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--tag-nil ()
  "Sexp::Nil tag byte must be 0."
  (should (= nelisp-sexp--tag-nil 0)))

(ert-deftest nelisp-sexp-layout--tag-t ()
  "Sexp::T tag byte must be 1."
  (should (= nelisp-sexp--tag-t 1)))

(ert-deftest nelisp-sexp-layout--tag-int ()
  "Sexp::Int tag byte must be 2."
  (should (= nelisp-sexp--tag-int 2)))

(ert-deftest nelisp-sexp-layout--tag-float ()
  "Sexp::Float tag byte must be 3."
  (should (= nelisp-sexp--tag-float 3)))

(ert-deftest nelisp-sexp-layout--tag-symbol ()
  "Sexp::Symbol tag byte must be 4."
  (should (= nelisp-sexp--tag-symbol 4)))

(ert-deftest nelisp-sexp-layout--tag-str ()
  "Sexp::Str tag byte must be 5."
  (should (= nelisp-sexp--tag-str 5)))

(ert-deftest nelisp-sexp-layout--tag-mut-str ()
  "Sexp::MutStr tag byte must be 6."
  (should (= nelisp-sexp--tag-mut-str 6)))

(ert-deftest nelisp-sexp-layout--tag-cons ()
  "Sexp::Cons tag byte must be 7."
  (should (= nelisp-sexp--tag-cons 7)))

(ert-deftest nelisp-sexp-layout--tag-vector ()
  "Sexp::Vector tag byte must be 8."
  (should (= nelisp-sexp--tag-vector 8)))

(ert-deftest nelisp-sexp-layout--tag-char-table ()
  "Sexp::CharTable tag byte must be 9."
  (should (= nelisp-sexp--tag-char-table 9)))

(ert-deftest nelisp-sexp-layout--tag-bool-vector ()
  "Sexp::BoolVector tag byte must be 10."
  (should (= nelisp-sexp--tag-bool-vector 10)))

(ert-deftest nelisp-sexp-layout--tag-cell ()
  "Sexp::Cell tag byte must be 11."
  (should (= nelisp-sexp--tag-cell 11)))

(ert-deftest nelisp-sexp-layout--tag-record ()
  "Sexp::Record tag byte must be 12."
  (should (= nelisp-sexp--tag-record 12)))

;; ---------------------------------------------------------------------------
;; Sexp slot layout offsets and size
;; Mirrors `assert!(SEXP_PAYLOAD_OFFSET == 8)' and `size_of::<Sexp>() == 32'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--offset-tag ()
  "Sexp tag discriminant is at byte offset 0."
  (should (= nelisp-sexp--offset-tag 0)))

(ert-deftest nelisp-sexp-layout--offset-payload ()
  "Sexp variant payload starts at byte offset 8 (= SEXP_PAYLOAD_OFFSET)."
  (should (= nelisp-sexp--offset-payload 8)))

(ert-deftest nelisp-sexp-layout--offset-int-payload ()
  "Sexp::Int(i64) payload is at byte offset 8."
  (should (= nelisp-sexp--offset-int-payload 8)))

(ert-deftest nelisp-sexp-layout--slot-size ()
  "Each Sexp slot occupies exactly 32 bytes (= size_of::<Sexp>())."
  (should (= nelisp-sexp--slot-size 32)))

;; ---------------------------------------------------------------------------
;; NlConsBox field offsets
;; Mirrors `offset_of!(NlConsBox, car) == 0', `cdr == 32', `refcount == 64',
;; `size_of::<NlConsBox>() == 72'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--nlconsbox-offset-car ()
  "NlConsBox::car is at byte offset 0."
  (should (= nelisp-nlconsbox--offset-car 0)))

(ert-deftest nelisp-sexp-layout--nlconsbox-offset-cdr ()
  "NlConsBox::cdr is at byte offset 32 (= one Sexp slot past car)."
  (should (= nelisp-nlconsbox--offset-cdr 32)))

(ert-deftest nelisp-sexp-layout--nlconsbox-offset-refcount ()
  "NlConsBox::refcount is at byte offset 64 (= two Sexp slots past start)."
  (should (= nelisp-nlconsbox--offset-refcount 64)))

(ert-deftest nelisp-sexp-layout--nlconsbox-size ()
  "NlConsBox total size is 72 bytes."
  (should (= nelisp-nlconsbox--size 72)))

;; ---------------------------------------------------------------------------
;; Rust String header offsets (within a Sexp::Symbol / Sexp::Str slot)
;; Mirrors `size_of::<String>() == 24' and `align_of::<String>() == 8'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--string-header-size ()
  "Rust String header is 24 bytes (ptr + capacity + length)."
  (should (= nelisp-string--header-size 24)))

(ert-deftest nelisp-sexp-layout--string-offset-capacity ()
  "String::capacity field within Sexp::Symbol/Str slot is at byte offset 8."
  (should (= nelisp-string--offset-capacity 8)))

(ert-deftest nelisp-sexp-layout--string-offset-ptr ()
  "String::ptr field within Sexp::Symbol/Str slot is at byte offset 16."
  (should (= nelisp-string--offset-ptr 16)))

(ert-deftest nelisp-sexp-layout--string-offset-length ()
  "String::length field within Sexp::Symbol/Str slot is at byte offset 24."
  (should (= nelisp-string--offset-length 24)))

;; ---------------------------------------------------------------------------
;; NlRecord field offsets and size
;; Mirrors `offset_of!(NlRecord, type_tag) == 0', `slots == 32',
;; `refcount == 56', `size_of::<NlRecord>() == 64'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--nlrecord-offset-type-tag ()
  "NlRecord::type_tag is at byte offset 0."
  (should (= nelisp-nlrecord--offset-type-tag 0)))

(ert-deftest nelisp-sexp-layout--nlrecord-offset-slots ()
  "NlRecord::slots Vec header starts at byte offset 32."
  (should (= nelisp-nlrecord--offset-slots-vec 32)))

(ert-deftest nelisp-sexp-layout--nlrecord-offset-slots-ptr ()
  "NlRecord::slots.ptr alias is 32 (same as slots-vec)."
  (should (= nelisp-nlrecord--offset-slots-ptr 32)))

(ert-deftest nelisp-sexp-layout--nlrecord-offset-slots-capacity ()
  "NlRecord::slots.capacity is at byte offset 40."
  (should (= nelisp-nlrecord--offset-slots-capacity 40)))

(ert-deftest nelisp-sexp-layout--nlrecord-offset-slots-length ()
  "NlRecord::slots.length is at byte offset 48."
  (should (= nelisp-nlrecord--offset-slots-length 48)))

(ert-deftest nelisp-sexp-layout--nlrecord-offset-refcount ()
  "NlRecord::refcount is at byte offset 56."
  (should (= nelisp-nlrecord--offset-refcount 56)))

(ert-deftest nelisp-sexp-layout--nlrecord-size ()
  "NlRecord total size is 64 bytes."
  (should (= nelisp-nlrecord--size 64)))

;; ---------------------------------------------------------------------------
;; NlVector field offsets and size
;; Mirrors `offset_of!(NlVector, value) == 0', `refcount == 24',
;; `size_of::<NlVector>() == 32'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--nlvector-offset-value ()
  "NlVector::value Vec header starts at byte offset 0."
  (should (= nelisp-nlvector--offset-value-vec 0)))

(ert-deftest nelisp-sexp-layout--nlvector-offset-value-ptr ()
  "NlVector::value.ptr alias is 0."
  (should (= nelisp-nlvector--offset-value-ptr 0)))

(ert-deftest nelisp-sexp-layout--nlvector-offset-value-capacity ()
  "NlVector::value.capacity is at byte offset 8."
  (should (= nelisp-nlvector--offset-value-capacity 8)))

(ert-deftest nelisp-sexp-layout--nlvector-offset-value-length ()
  "NlVector::value.length is at byte offset 16."
  (should (= nelisp-nlvector--offset-value-length 16)))

(ert-deftest nelisp-sexp-layout--nlvector-offset-refcount ()
  "NlVector::refcount is at byte offset 24."
  (should (= nelisp-nlvector--offset-refcount 24)))

(ert-deftest nelisp-sexp-layout--nlvector-size ()
  "NlVector total size is 32 bytes."
  (should (= nelisp-nlvector--size 32)))

;; ---------------------------------------------------------------------------
;; NlCell field offsets and size
;; Mirrors `offset_of!(NlCell, value) == 0', `refcount == 32',
;; `size_of::<NlCell>() == 40'.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--nlcell-offset-value ()
  "NlCell::value Sexp is at byte offset 0."
  (should (= nelisp-nlcell--offset-value 0)))

(ert-deftest nelisp-sexp-layout--nlcell-offset-refcount ()
  "NlCell::refcount is at byte offset 32 (= one Sexp slot past value)."
  (should (= nelisp-nlcell--offset-refcount 32)))

(ert-deftest nelisp-sexp-layout--nlcell-size ()
  "NlCell total size is 40 bytes."
  (should (= nelisp-nlcell--size 40)))

;; ---------------------------------------------------------------------------
;; ABI export table completeness check
;; Verifies that `nelisp-sexp--abi-export' contains all expected keys.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-sexp-layout--abi-export-keys ()
  "nelisp-sexp--abi-export must contain all expected layout keys."
  (let ((keys (mapcar #'car nelisp-sexp--abi-export))
        (expected '(tag-nil tag-t tag-int tag-float tag-symbol tag-str
                    tag-mut-str tag-cons tag-vector tag-char-table
                    tag-bool-vector tag-cell tag-record
                    offset-tag offset-payload slot-size
                    nlconsbox-offset-car nlconsbox-offset-cdr
                    nlconsbox-offset-refcount nlconsbox-size
                    string-offset-capacity string-offset-ptr
                    string-offset-length string-header-size
                    nlrecord-offset-type-tag nlrecord-offset-slots-vec
                    nlrecord-offset-slots-ptr nlrecord-offset-slots-capacity
                    nlrecord-offset-slots-length nlrecord-offset-refcount
                    nlrecord-size nlvector-offset-value-vec
                    nlvector-offset-value-ptr nlvector-offset-value-capacity
                    nlvector-offset-value-length nlvector-offset-refcount
                    nlvector-size nlcell-offset-value
                    nlcell-offset-refcount nlcell-size)))
    (dolist (k expected)
      (should (memq k keys)))))

(ert-deftest nelisp-sexp-layout--abi-export-values ()
  "nelisp-sexp--abi-export values must match the individual defconsts."
  ;; nelisp-sexp--abi-export is an alist of (SYMBOL . INTEGER).
  (let ((export nelisp-sexp--abi-export))
    ;; Spot-check a representative sample.
    (should (= (cdr (assq 'tag-nil export)) nelisp-sexp--tag-nil))
    (should (= (cdr (assq 'tag-record export)) nelisp-sexp--tag-record))
    (should (= (cdr (assq 'offset-payload export)) nelisp-sexp--offset-payload))
    (should (= (cdr (assq 'slot-size export)) nelisp-sexp--slot-size))
    (should (= (cdr (assq 'nlconsbox-size export)) nelisp-nlconsbox--size))
    (should (= (cdr (assq 'nlrecord-size export)) nelisp-nlrecord--size))
    (should (= (cdr (assq 'nlvector-size export)) nelisp-nlvector--size))
    (should (= (cdr (assq 'nlcell-size export)) nelisp-nlcell--size))))

(provide 'nelisp-sexp-layout-test)

;;; nelisp-sexp-layout-test.el ends here
