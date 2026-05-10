;;; nelisp-static-linker-test.el --- ERT tests for Doc 93 §93.a  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 93 §93.a — ert tests for the relocation core in
;; `lisp/nelisp-static-linker.el'.  Coverage:
;;   1. symbol-table struct + add / lookup + global/global collide,
;;   2. relocation-entry struct + bad-type rejection,
;;   3. PC32 patch math (= S + A - P) + bit-exact byte check,
;;   4. ABS64 patch math (= S + A) + 8-byte LE check,
;;   5. PLT32 = PC32 semantic equivalence under static link,
;;   6. multi-relocation in one byte stream (= 3 entries patched),
;;   7. unresolved-symbol signal,
;;   8. rel32 overflow signal at 2GB boundary,
;;   9. addend handling (negative + positive) for PC32 and ABS64.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-static-linker)

;; ---- helpers ----

(defun nelisp-link-test--ub (&rest bs)
  "Construct a unibyte-string from integer args BS."
  (apply #'unibyte-string bs))

(defun nelisp-link-test--bytes (n)
  "Return a unibyte-string of N zero bytes (= reloc placeholder)."
  (make-string n 0))

;; ---- §93.a (1) symbol table ----

(ert-deftest nelisp-link-symtab-empty ()
  (let ((tab (nelisp-link-symtab-make)))
    (should (hash-table-p tab))
    (should (= 0 (hash-table-count tab)))
    (should-not (nelisp-link-symtab-lookup tab "missing"))))

(ert-deftest nelisp-link-symtab-add-and-lookup ()
  (let* ((tab (nelisp-link-symtab-make))
         (sym (nelisp-link-symbol "foo" #x401000)))
    (nelisp-link-symtab-add tab sym)
    (let ((found (nelisp-link-symtab-lookup tab "foo")))
      (should found)
      (should (equal (plist-get found :name) "foo"))
      (should (= (plist-get found :value) #x401000))
      (should (eq (plist-get found :bind) 'global))
      (should (eq (plist-get found :section) 'text))
      (should (eq (plist-get found :type) 'func)))))

(ert-deftest nelisp-link-symtab-defaults-and-overrides ()
  (let ((s (nelisp-link-symbol "bar" #x402000
                               :size 16 :section 'rodata
                               :bind 'local :type 'object)))
    (should (= (plist-get s :size) 16))
    (should (eq (plist-get s :section) 'rodata))
    (should (eq (plist-get s :bind) 'local))
    (should (eq (plist-get s :type) 'object))))

(ert-deftest nelisp-link-symtab-global-global-collision ()
  (let ((tab (nelisp-link-symtab-make)))
    (nelisp-link-symtab-add tab
                            (nelisp-link-symbol "dup" #x401000))
    (should-error
     (nelisp-link-symtab-add tab
                             (nelisp-link-symbol "dup" #x402000))
     :type 'nelisp-link-error)))

;; ---- §93.a (2) reloc entry ----

(ert-deftest nelisp-link-reloc-rejects-unknown-type ()
  (should-error (nelisp-link-reloc 0 'wat "foo")
                :type 'nelisp-link-error))

(ert-deftest nelisp-link-reloc-defaults-addend-zero ()
  (let ((r (nelisp-link-reloc 4 'pc32 "foo")))
    (should (= (plist-get r :addend) 0))
    (should (eq (plist-get r :type) 'pc32))
    (should (equal (plist-get r :symbol) "foo"))
    (should (= (plist-get r :offset) 4))))

;; ---- §93.a (3) PC32 patch math (spec example) ----

(ert-deftest nelisp-link-pc32-spec-example ()
  ;; Spec: symbol foo at VA 0x401000, reloc at offset 4, section VA
  ;; 0x400000, addend 0 -> rel32 = 0x401000 - (0x400000 + 4 + 4)
  ;; = 0xFF8 -> LE bytes F8 0F 00 00.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "foo" #x401000)))
         (bytes (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (reloc (nelisp-link-reloc 4 'pc32 "foo"))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    (should (equal out
                   (nelisp-link-test--ub 0 0 0 0
                                         #xF8 #x0F 0 0)))))

(ert-deftest nelisp-link-pc32-call-rel32-shape ()
  ;; CALL E8 + 4 placeholder bytes, callee 5 bytes later (= classic
  ;; tail-call layout).  offset = 1, section VA = 0, callee = 5.
  ;; P = 0 + 1 + 4 = 5, d = 5 - 5 = 0.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "callee" 5)))
         (bytes (nelisp-link-test--ub #xE8 0 0 0 0))
         (reloc (nelisp-link-reloc 1 'pc32 "callee"))
         (out (nelisp-link-apply-reloc bytes reloc tab 0)))
    (should (equal out
                   (nelisp-link-test--ub #xE8 0 0 0 0)))))

(ert-deftest nelisp-link-pc32-negative-displacement ()
  ;; Backward branch: target before patch site.  offset = 4,
  ;; section VA = 0x400000, target = 0x400000 (= same).
  ;; P = 0x400000 + 4 + 4 = 0x400008, d = -8 -> F8 FF FF FF.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "back" #x400000)))
         (bytes (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (reloc (nelisp-link-reloc 4 'pc32 "back"))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    (should (equal out
                   (nelisp-link-test--ub 0 0 0 0
                                         #xF8 #xFF #xFF #xFF)))))

(ert-deftest nelisp-link-pc32-with-addend ()
  ;; Addend -4 is the common case for `CALL rel32' on x86_64
  ;; (= the assembler bakes -4 into the addend so the dest = sym
  ;; rather than sym + 4).
  ;; foo @ 0x401000, offset 4, section VA 0x400000, addend = -4.
  ;; P = 0x400008, d = 0x401000 - 4 - 0x400008 = 0xFF4.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "foo" #x401000)))
         (bytes (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (reloc (nelisp-link-reloc 4 'pc32 "foo" -4))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    (should (equal out
                   (nelisp-link-test--ub 0 0 0 0
                                         #xF4 #x0F 0 0)))))

;; ---- §93.a (3) ABS64 ----

(ert-deftest nelisp-link-abs64-spec-example ()
  ;; Spec: symbol at 0x400000, addend 0x100 -> 0x400100, LE bytes
  ;; 00 01 40 00 00 00 00 00.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "ptr" #x400000)))
         (bytes (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (reloc (nelisp-link-reloc 0 'abs64 "ptr" #x100))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    (should (equal out
                   (nelisp-link-test--ub #x00 #x01 #x40 #x00
                                         #x00 #x00 #x00 #x00)))))

(ert-deftest nelisp-link-abs64-zero-addend ()
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "p" #xDEADBEEF)))
         (bytes (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (reloc (nelisp-link-reloc 0 'abs64 "p"))
         (out (nelisp-link-apply-reloc bytes reloc tab 0)))
    (should (equal out
                   (nelisp-link-test--ub #xEF #xBE #xAD #xDE
                                         0 0 0 0)))))

;; ---- §93.a (3) PLT32 = PC32 under static link ----

(ert-deftest nelisp-link-plt32-equals-pc32 ()
  ;; Same inputs, two different types -> same patched bytes.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "foo" #x401000)))
         (bytes-a (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (bytes-b (nelisp-link-test--ub 0 0 0 0 0 0 0 0))
         (a (nelisp-link-apply-reloc
             bytes-a (nelisp-link-reloc 4 'pc32 "foo")
             tab #x400000))
         (b (nelisp-link-apply-reloc
             bytes-b (nelisp-link-reloc 4 'plt32 "foo")
             tab #x400000)))
    (should (equal a b))))

;; ---- §93.a (4) multi-reloc ----

(ert-deftest nelisp-link-apply-relocs-three-entries ()
  ;; Three pc32 placeholders at offsets 0, 4, 8 in a 12-byte stream,
  ;; each pointing at a different symbol.  All three patched.
  (let* ((tab (nelisp-link-symtab-make))
         (_a (nelisp-link-symtab-add
              tab (nelisp-link-symbol "s0" 100)))
         (_b (nelisp-link-symtab-add
              tab (nelisp-link-symbol "s1" 200)))
         (_c (nelisp-link-symtab-add
              tab (nelisp-link-symbol "s2" 300)))
         (bytes (nelisp-link-test--bytes 12))
         (relocs (list (nelisp-link-reloc 0 'pc32 "s0")
                       (nelisp-link-reloc 4 'pc32 "s1")
                       (nelisp-link-reloc 8 'pc32 "s2")))
         (out (nelisp-link-apply-relocs bytes relocs tab 0)))
    ;; d0 = 100 - (0 + 0 + 4)  = 96  = 0x60
    ;; d1 = 200 - (0 + 4 + 4)  = 192 = 0xC0
    ;; d2 = 300 - (0 + 8 + 4)  = 288 = 0x120
    (should (equal out
                   (nelisp-link-test--ub #x60 0 0 0
                                         #xC0 0 0 0
                                         #x20 #x01 0 0)))))

(ert-deftest nelisp-link-apply-relocs-mixed-types ()
  ;; pc32 at 0, abs64 at 4 -> 12-byte stream.
  (let* ((tab (nelisp-link-symtab-make))
         (_a (nelisp-link-symtab-add
              tab (nelisp-link-symbol "fn" #x401000)))
         (_b (nelisp-link-symtab-add
              tab (nelisp-link-symbol "data" #x402000)))
         (bytes (nelisp-link-test--bytes 12))
         (relocs (list (nelisp-link-reloc 0 'pc32 "fn")
                       (nelisp-link-reloc 4 'abs64 "data")))
         (out (nelisp-link-apply-relocs bytes relocs tab #x400000)))
    ;; pc32 d = 0x401000 - (0x400000 + 0 + 4) = 0xFFC -> FC 0F 00 00
    ;; abs64 v = 0x402000 -> 00 20 40 00 00 00 00 00
    (should (equal out
                   (nelisp-link-test--ub #xFC #x0F 0 0
                                         #x00 #x20 #x40 #x00
                                         0 0 0 0)))))

;; ---- §93.a (4) error paths ----

(ert-deftest nelisp-link-unresolved-symbol-signals ()
  (let* ((tab (nelisp-link-symtab-make))
         (bytes (nelisp-link-test--bytes 4))
         (r (nelisp-link-reloc 0 'pc32 "nope")))
    (should-error (nelisp-link-apply-reloc bytes r tab 0)
                  :type 'nelisp-link--unresolved-symbol)))

(ert-deftest nelisp-link-rel32-overflow-signals ()
  ;; symbol at 1<<32, section VA 0 -> displacement > 2^31 -> error.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "far" (ash 1 32))))
         (bytes (nelisp-link-test--bytes 4))
         (r (nelisp-link-reloc 0 'pc32 "far")))
    (should-error (nelisp-link-apply-reloc bytes r tab 0)
                  :type 'nelisp-link--rel32-overflow)))

(ert-deftest nelisp-link-rel32-overflow-negative ()
  ;; section VA 1<<32 referencing symbol at 0 -> -ve overflow.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "zero" 0)))
         (bytes (nelisp-link-test--bytes 4))
         (r (nelisp-link-reloc 0 'pc32 "zero")))
    (should-error (nelisp-link-apply-reloc bytes r tab (ash 1 32))
                  :type 'nelisp-link--rel32-overflow)))

;; ---- §93.a (5) reloc-entry alias (= asm-side `:sym' key) ----

(ert-deftest nelisp-link-reloc-accepts-asm-sym-alias ()
  ;; Doc 92 `nelisp-asm-x86_64-emit-reloc' writes `(:type ... :sym
  ;; ... :offset ... :addend ...)' -- the linker accepts `:sym' as
  ;; an alias of `:symbol' so live assembler output drops in.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "foo" #x401000)))
         (bytes (nelisp-link-test--ub 0 0 0 0))
         ;; Note: this raw plist mirrors the assembler shape.
         (reloc (list :offset 0 :type 'pc32 :sym "foo" :addend -4))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    ;; P = 0x400000 + 0 + 4 = 0x400004
    ;; d = (0x401000 + (-4)) - 0x400004 = 0xFF8 -> F8 0F 00 00
    (should (equal out
                   (nelisp-link-test--ub #xF8 #x0F 0 0)))))

;; ---- §93.a (5) test fixture builder ----

(ert-deftest nelisp-link-make-test-asm-shape ()
  (let* ((sym (nelisp-link-symbol "foo" 0))
         (rel (nelisp-link-reloc 0 'pc32 "foo"))
         (asm (nelisp-link--make-test-asm
               (nelisp-link-test--bytes 4)
               (list rel) (list sym))))
    (should (equal (plist-get asm :bytes)
                   (nelisp-link-test--bytes 4)))
    (should (equal (plist-get asm :relocs) (list rel)))
    (should (equal (plist-get asm :symbols) (list sym)))))

;; ---- §93.a end-to-end smoke (not exec'd: awaits §93.c) ----

(ert-deftest nelisp-link-end-to-end-smoke ()
  ;; Minimal pre-linked .text fragment: `E8 00 00 00 00 C3' (=
  ;; `CALL rel32; RET').  callee_fn lives 0x10 bytes after the
  ;; section start.  Section VA = 0x400000, CALL offset = 1.
  ;; Addend = -4 (= standard for x86_64 CALL imm32, per Doc 92).
  ;; P = 0x400000 + 1 + 4 = 0x400005; d = 0x400010 - 4 - 0x400005
  ;; = 7 -> bytes 07 00 00 00.
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "callee_fn" #x400010)))
         (bytes (nelisp-link-test--ub #xE8 0 0 0 0 #xC3))
         (reloc (nelisp-link-reloc 1 'pc32 "callee_fn" -4))
         (out (nelisp-link-apply-reloc bytes reloc tab #x400000)))
    (should (equal out
                   (nelisp-link-test--ub #xE8 #x07 0 0 0 #xC3)))))

(provide 'nelisp-static-linker-test)

;;; nelisp-static-linker-test.el ends here
