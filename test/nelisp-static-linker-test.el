;;; nelisp-static-linker-test.el --- ERT tests for Doc 93 §93.a + §93.b  -*- lexical-binding: t; -*-

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
(require 'nelisp-elf-write)
(require 'nelisp-mach-o-write)

;; ---- helpers ----

(defun nelisp-link-test--ub (&rest bs)
  "Construct a unibyte-string from integer args BS."
  (apply #'unibyte-string bs))

(defun nelisp-link-test--bytes (n)
  "Return a unibyte-string of N zero bytes (= reloc placeholder)."
  (make-string n 0))

(defun nelisp-link-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-link-test--read-le32 (bytes offset)
  "Read unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-link-test--read-le16 (bytes offset)
  "Read unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-link-test--read-le64 (bytes offset)
  "Read unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 0))
    (while (< i 8)
      (setq acc (logior acc (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    acc))

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

(ert-deftest nelisp-link-b26-pc-patches-arm64-bl ()
  "AArch64 CALL26-style relocs patch the BL imm26 field."
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "callee" 8)))
         (bytes (nelisp-link-test--ub #x00 #x00 #x00 #x94
                                      #xc0 #x03 #x5f #xd6))
         (reloc (nelisp-link-reloc 0 'b26-pc "callee"))
         (out (nelisp-link-apply-reloc bytes reloc tab 0)))
    (should (equal out
                   (nelisp-link-test--ub #x02 #x00 #x00 #x94
                                         #xc0 #x03 #x5f #xd6)))))

(ert-deftest nelisp-link-b26-pc-rejects-out-of-range ()
  "AArch64 branch relocs retain the architectural ±128 MiB range check."
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "far" (ash 1 28))))
         (bytes (nelisp-link-test--ub #x00 #x00 #x00 #x94))
         (reloc (nelisp-link-reloc 0 'b26-pc "far")))
    (should-error
     (nelisp-link-apply-reloc bytes reloc tab 0)
     :type 'nelisp-link--branch26-overflow)))

(ert-deftest nelisp-link-adrp-add-reloc-pair-patches-arm64-data-addr ()
  "ADRP+ADD relocs materialize an external data symbol address in x0."
  (let* ((tab (nelisp-link-symtab-make))
         (_ (nelisp-link-symtab-add
             tab (nelisp-link-symbol "nl_ctrl_block" #x401ABC
                                     :section 'data :type 'object)))
         (bytes (nelisp-link-test--ub
                 #x00 #x00 #x00 #x90
                 #x00 #x00 #x00 #x91))
         (relocs (list (nelisp-link-reloc 0 'adr-prel-pg-hi21 "nl_ctrl_block")
                       (nelisp-link-reloc 4 'add-abs-lo12-nc "nl_ctrl_block")))
         (out (nelisp-link-apply-relocs bytes relocs tab #x400000)))
    ;; Symbol 0x401ABC from a section at 0x400000:
    ;;   ADRP page delta = +1 page -> 0xB0000000
    ;;   ADD  lo12      = 0xABC    -> 0x912AF000
    (should (equal out
                   (nelisp-link-test--ub
                    #x00 #x00 #x00 #xB0
                    #x00 #xF0 #x2A #x91)))))

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

;; ============================================================
;; §93.b — section combine + 2-pass symtab merge + WEAK
;; ============================================================

(defun nelisp-link-test--unit (name &rest kvs)
  "Build a §93.b compile-unit from NAME + plist KVS.
Recognised keys: `:text', `:rodata', `:data', `:bss', `:symbols',
`:relocs'.  Missing byte sections default to the empty string and
missing bss defaults to 0 so the unit-builder is concise in
fixture code."
  (nelisp-link-unit-make
   name
   (list (cons 'text   (or (plist-get kvs :text) ""))
         (cons 'rodata (or (plist-get kvs :rodata) ""))
         (cons 'data   (or (plist-get kvs :data) ""))
         (cons 'bss    (or (plist-get kvs :bss) 0)))
   (or (plist-get kvs :symbols) nil)
   (or (plist-get kvs :relocs) nil)))

;; ---- §93.b (1) compile-unit struct ----

(ert-deftest nelisp-link-unit-make-shape ()
  (let* ((u (nelisp-link-unit-make
             "main.o"
             (list (cons 'text (nelisp-link-test--ub 1 2 3))
                   (cons 'rodata "")
                   (cons 'data "")
                   (cons 'bss 16))
             nil nil)))
    (should (equal (plist-get u :name) "main.o"))
    (should (equal (cdr (assq 'text (plist-get u :sections)))
                   (nelisp-link-test--ub 1 2 3)))
    (should (= (cdr (assq 'bss (plist-get u :sections))) 16))
    (should (null (plist-get u :symbols)))
    (should (null (plist-get u :relocs)))))

;; ---- §93.b (2) section combine ----

(ert-deftest nelisp-link-combine-two-text-units ()
  (let* ((ua (nelisp-link-test--unit
              "a.o" :text (nelisp-link-test--ub #x90 #x90)))
         (ub (nelisp-link-test--unit
              "b.o" :text (nelisp-link-test--ub #xCC #xCC #xCC)))
         (c (nelisp-link-combine-sections (list ua ub)))
         (text (cdr (assq 'text c))))
    (should (equal (plist-get text :bytes)
                   (nelisp-link-test--ub #x90 #x90 #xCC #xCC #xCC)))
    (should (equal (plist-get text :offsets)
                   '(("a.o" . 0) ("b.o" . 2))))))

(ert-deftest nelisp-link-combine-text-and-rodata-offsets ()
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--ub 1 2 3 4)
              :rodata (nelisp-link-test--ub #xAA #xBB)))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--ub 5 6)
              :rodata (nelisp-link-test--ub #xCC)))
         (c (nelisp-link-combine-sections (list ua ub))))
    (should (equal (plist-get (cdr (assq 'rodata c)) :bytes)
                   (nelisp-link-test--ub #xAA #xBB #xCC)))
    (should (equal (plist-get (cdr (assq 'rodata c)) :offsets)
                   '(("a.o" . 0) ("b.o" . 2))))
    (should (equal (plist-get (cdr (assq 'text c)) :offsets)
                   '(("a.o" . 0) ("b.o" . 4))))))

(ert-deftest nelisp-link-combine-bss-sums-sizes ()
  (let* ((ua (nelisp-link-test--unit "a.o" :bss 16))
         (ub (nelisp-link-test--unit "b.o" :bss 32))
         (uc (nelisp-link-test--unit "c.o" :bss 8))
         (c (nelisp-link-combine-sections (list ua ub uc)))
         (b (cdr (assq 'bss c))))
    (should (= (plist-get b :bytes) 56))
    (should (equal (plist-get b :offsets)
                   '(("a.o" . 0) ("b.o" . 16) ("c.o" . 48))))))

(ert-deftest nelisp-link-combine-empty-section-still-offsets-units ()
  ;; Neither unit contributes rodata bytes; both should still appear
  ;; in the offsets list at position 0 so cross-section relocs into
  ;; an empty section can be diagnosed.
  (let* ((ua (nelisp-link-test--unit
              "a.o" :text (nelisp-link-test--ub 1)))
         (ub (nelisp-link-test--unit
              "b.o" :text (nelisp-link-test--ub 2)))
         (c (nelisp-link-combine-sections (list ua ub))))
    (should (equal (plist-get (cdr (assq 'rodata c)) :bytes) ""))
    (should (equal (plist-get (cdr (assq 'rodata c)) :offsets)
                   '(("a.o" . 0) ("b.o" . 0))))))

(ert-deftest nelisp-link-combine-empty-units ()
  (let ((c (nelisp-link-combine-sections nil)))
    (should (equal (plist-get (cdr (assq 'text c)) :bytes) ""))
    (should (equal (plist-get (cdr (assq 'text c)) :offsets) nil))
    (should (= (plist-get (cdr (assq 'bss c)) :bytes) 0))))

;; ---- §93.b (3) 2-pass symtab merge ----

(ert-deftest nelisp-link-collect-single-defined ()
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 16)
             :symbols (list (nelisp-link-symbol "foo" 8))))
         (combined (nelisp-link-combine-sections (list u)))
         (tab (nelisp-link--collect-defined-symbols
               (list u) combined '((text . #x400000)
                                   (rodata . #x500000)
                                   (data . #x600000))))
         (entry (nelisp-link-symtab-lookup tab "foo")))
    (should entry)
    (should (= (plist-get entry :value) (+ #x400000 0 8)))
    (should (eq (plist-get entry :bind) 'global))))

(ert-deftest nelisp-link-collect-two-units-distinct-globals ()
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol "alpha" 0))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--bytes 8)
              :symbols (list (nelisp-link-symbol "beta" 4))))
         (combined (nelisp-link-combine-sections (list ua ub)))
         (tab (nelisp-link--collect-defined-symbols
               (list ua ub) combined '((text . #x400000)
                                       (rodata . #x500000)
                                       (data . #x600000)))))
    ;; alpha @ text + 0 + 0 = 0x400000.
    (should (= (plist-get (nelisp-link-symtab-lookup tab "alpha")
                          :value)
               #x400000))
    ;; beta @ text + 4 (b.o offset) + 4 (sym offset) = 0x400008.
    (should (= (plist-get (nelisp-link-symtab-lookup tab "beta")
                          :value)
               #x400008))))

(ert-deftest nelisp-link-collect-cross-section-va ()
  ;; Symbol declared as rodata gets the rodata section VA, not text.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :rodata (nelisp-link-test--bytes 32)
             :symbols (list (nelisp-link-symbol
                             "konst" 16 :section 'rodata
                             :type 'object))))
         (combined (nelisp-link-combine-sections (list u)))
         (tab (nelisp-link--collect-defined-symbols
               (list u) combined '((text . #x400000)
                                   (rodata . #x500000)
                                   (data . #x600000))))
         (e (nelisp-link-symtab-lookup tab "konst")))
    (should (= (plist-get e :value) (+ #x500000 0 16)))
    (should (eq (plist-get e :section) 'rodata))))

(ert-deftest nelisp-link-collect-duplicate-strong-signals ()
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol "dup" 0))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol "dup" 0))))
         (combined (nelisp-link-combine-sections (list ua ub))))
    (should-error
     (nelisp-link--collect-defined-symbols
      (list ua ub) combined '((text . #x400000)
                              (rodata . #x500000)
                              (data . #x600000)))
     :type 'nelisp-link--duplicate-symbol)))

(ert-deftest nelisp-link-collect-strong-beats-weak ()
  ;; Either order: STRONG must win.  Inserting STRONG first (a.o
  ;; sorted first) then WEAK (b.o) should keep STRONG.  Inserting
  ;; WEAK first (a-weak.o) then STRONG (b.o) should still keep
  ;; STRONG.
  (let* ((u-strong (nelisp-link-test--unit
                    "a.o"
                    :text (nelisp-link-test--bytes 4)
                    :symbols (list (nelisp-link-symbol "x" 0))))
         (u-weak (nelisp-link-test--unit
                  "b.o"
                  :text (nelisp-link-test--bytes 4)
                  :symbols (list (nelisp-link-symbol
                                  "x" 0 :bind 'weak))))
         (combined (nelisp-link-combine-sections
                    (list u-strong u-weak)))
         (tab (nelisp-link--collect-defined-symbols
               (list u-strong u-weak) combined
               '((text . #x400000)
                 (rodata . #x500000)
                 (data . #x600000)))))
    (should (eq (plist-get (nelisp-link-symtab-lookup tab "x")
                           :bind)
                'global))
    (should (= (plist-get (nelisp-link-symtab-lookup tab "x")
                          :value)
               #x400000))))

(ert-deftest nelisp-link-collect-weak-then-strong ()
  ;; WEAK comes first by unit-sort (a-weak.o), STRONG second
  ;; (z-strong.o).  STRONG must overwrite.
  (let* ((u-weak (nelisp-link-test--unit
                  "a-weak.o"
                  :text (nelisp-link-test--bytes 4)
                  :symbols (list (nelisp-link-symbol
                                  "y" 0 :bind 'weak))))
         (u-strong (nelisp-link-test--unit
                    "z-strong.o"
                    :text (nelisp-link-test--bytes 4)
                    :symbols (list (nelisp-link-symbol "y" 0))))
         (combined (nelisp-link-combine-sections
                    (list u-weak u-strong)))
         (tab (nelisp-link--collect-defined-symbols
               (list u-weak u-strong) combined
               '((text . #x400000)
                 (rodata . #x500000)
                 (data . #x600000)))))
    ;; STRONG (z-strong.o) is unit-offset 4 → VA 0x400004.
    (should (eq (plist-get (nelisp-link-symtab-lookup tab "y")
                           :bind)
                'global))
    (should (= (plist-get (nelisp-link-symtab-lookup tab "y")
                          :value)
               #x400004))))

(ert-deftest nelisp-link-collect-two-weak-deterministic ()
  ;; Two WEAK definitions of the same symbol.  Tie-break = unit
  ;; sort by :name.  Inputs in either order must yield same winner
  ;; (= "a.o" before "b.o").
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol
                              "w" 0 :bind 'weak))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol
                              "w" 0 :bind 'weak))))
         (layout '((text . #x400000)
                   (rodata . #x500000)
                   (data . #x600000)))
         (c1 (nelisp-link-combine-sections (list ua ub)))
         (t1 (nelisp-link--collect-defined-symbols
              (list ua ub) c1 layout))
         (c2 (nelisp-link-combine-sections (list ub ua)))
         (t2 (nelisp-link--collect-defined-symbols
              (list ub ua) c2 layout)))
    ;; In c1, a.o is at text offset 0 -> winner VA = 0x400000.
    (should (equal (plist-get (nelisp-link-symtab-lookup t1 "w")
                              :unit)
                   "a.o"))
    ;; In c2, b.o is at text offset 0 first -- but unit-sort puts
    ;; a.o first, so a.o wins; a.o's offset in c2 is 4.
    (should (equal (plist-get (nelisp-link-symtab-lookup t2 "w")
                              :unit)
                   "a.o"))
    (should (= (plist-get (nelisp-link-symtab-lookup t2 "w")
                          :value)
               #x400004))))

;; ---- §93.b (3) resolve-relocs pass + cross-unit smoke ----

(ert-deftest nelisp-link-resolve-unresolved-signals ()
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 8)
             :relocs (list (nelisp-link-reloc 0 'pc32 "missing"))))
         (combined (nelisp-link-combine-sections (list u)))
         (tab (nelisp-link--collect-defined-symbols
               (list u) combined '((text . #x400000)
                                   (rodata . #x500000)
                                   (data . #x600000)))))
    (should-error
     (nelisp-link--resolve-relocs
      (list u) combined '((text . #x400000)
                          (rodata . #x500000)
                          (data . #x600000))
      tab)
     :type 'nelisp-link--unresolved-symbol)))

(ert-deftest nelisp-link-resolve-cross-unit-pc32 ()
  ;; Unit A's .text has a pc32 reloc to symbol `target' defined in
  ;; unit B's .text.  After link the call site must point at B's
  ;; symbol's final VA.
  ;;
  ;; Layout: A.text = 8 bytes (offset 0), B.text = 8 bytes (offset 8).
  ;; Combined text VA = 0x400000.
  ;; B defines `target' at intra-unit offset 4 -> VA = 0x40000C.
  ;; A's reloc at offset 1 (= CALL E8 + 4 imm placeholder), addend -4.
  ;; Combined patch position = A.offset (0) + reloc offset (1) = 1.
  ;; P = 0x400000 + 1 + 4 = 0x400005.
  ;; d = (0x40000C + -4) - 0x400005 = 0x40000C - 4 - 0x400005 = 3.
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--ub #xE8 0 0 0 0 #xC3 #x90 #x90)
              :relocs (list (nelisp-link-reloc
                             1 'pc32 "target" -4))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--ub #x90 #x90 #x90 #x90
                                          #xC3 0 0 0)
              :symbols (list (nelisp-link-symbol "target" 4))))
         (layout '((text . #x400000)
                   (rodata . #x500000)
                   (data . #x600000)))
         (result (nelisp-link-units-2pass (list ua ub) layout))
         (text (cdr (assq 'text (plist-get result :bytes)))))
    (should (equal (substring text 0 6)
                   (nelisp-link-test--ub #xE8 #x03 0 0 0 #xC3)))
    (should (= (plist-get (nelisp-link-symtab-lookup
                           (plist-get result :symtab) "target")
                          :value)
               #x40000C))))

(ert-deftest nelisp-link-resolve-multi-section-reloc ()
  ;; Unit A's .text has an abs64 reloc referencing a symbol in its
  ;; own .rodata.  The rodata symbol's VA = rodata_va + 0 (only unit)
  ;; + 8 (intra-unit offset).
  ;;
  ;; rodata_va = 0x500000 -> S = 0x500008.  abs64 (S + A) = 0x500010
  ;; with addend 8.  Patched 8 LE bytes at text offset 0.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 8)
             :rodata (nelisp-link-test--bytes 16)
             :symbols (list (nelisp-link-symbol
                             "kdata" 8 :section 'rodata
                             :type 'object))
             :relocs (list (nelisp-link-reloc
                            0 'abs64 "kdata" 8))))
         (layout '((text . #x400000)
                   (rodata . #x500000)
                   (data . #x600000)))
         (result (nelisp-link-units-2pass (list u) layout))
         (text (cdr (assq 'text (plist-get result :bytes)))))
    (should (equal (substring text 0 8)
                   (nelisp-link-test--ub #x10 #x00 #x50 #x00
                                         #x00 #x00 #x00 #x00)))))

(ert-deftest nelisp-link-resolve-reloc-into-rodata-section ()
  ;; Reloc whose `:section' = rodata (= patch site lives in rodata,
  ;; not text).  abs64 placeholder in rodata at offset 0 references
  ;; a `func' symbol in text.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 16)
             :rodata (nelisp-link-test--bytes 8)
             :symbols (list (nelisp-link-symbol "fn" 8))
             :relocs (list (append (nelisp-link-reloc
                                    0 'abs64 "fn" 0)
                                   (list :section 'rodata)))))
         (layout '((text . #x400000)
                   (rodata . #x500000)
                   (data . #x600000)))
         (result (nelisp-link-units-2pass (list u) layout))
         (rodata (cdr (assq 'rodata (plist-get result :bytes)))))
    ;; S = text_va + 0 + 8 = 0x400008.
    (should (equal (substring rodata 0 8)
                   (nelisp-link-test--ub #x08 #x00 #x40 #x00
                                         #x00 #x00 #x00 #x00)))))

;; ---- §93.b (4) end-to-end driver ----

(ert-deftest nelisp-link-units-2pass-result-shape ()
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 4)
             :symbols (list (nelisp-link-symbol "f" 0))))
         (result (nelisp-link-units-2pass
                  (list u) '((text . #x400000)
                             (rodata . #x500000)
                             (data . #x600000)))))
    (should (assq 'text (plist-get result :bytes)))
    (should (assq 'rodata (plist-get result :bytes)))
    (should (assq 'data (plist-get result :bytes)))
    (should (assq 'bss (plist-get result :bytes)))
    (should (hash-table-p (plist-get result :symtab)))
    (should (assq 'text (plist-get result :section-offsets)))))

(ert-deftest nelisp-link-units-2pass-two-unit-cross-reference ()
  ;; A defines `entry', B defines `helper'; entry calls helper.
  ;; All section VAs given; verify resolved bytes + final VAs in
  ;; the merged symtab.
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--ub
                     #xE8 0 0 0 0 #xC3 #x90 #x90)
              :symbols (list (nelisp-link-symbol "entry" 0))
              :relocs (list (nelisp-link-reloc
                             1 'pc32 "helper" -4))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--ub #xC3 #x90 #x90 #x90)
              :symbols (list (nelisp-link-symbol "helper" 0))))
         (result (nelisp-link-units-2pass
                  (list ua ub) '((text . #x400000)
                                 (rodata . #x500000)
                                 (data . #x600000))))
         (tab (plist-get result :symtab)))
    (should (= (plist-get (nelisp-link-symtab-lookup tab "entry")
                          :value)
               #x400000))
    ;; helper @ b.o offset 8 -> VA 0x400008.
    (should (= (plist-get (nelisp-link-symtab-lookup tab "helper")
                          :value)
               #x400008))
    ;; rel32 = (0x400008 + -4) - (0x400000 + 1 + 4) = 0x400004 -
    ;; 0x400005 = -1.  LE bytes: FF FF FF FF.
    (let ((text (cdr (assq 'text (plist-get result :bytes)))))
      (should (equal (substring text 0 6)
                     (nelisp-link-test--ub #xE8 #xFF #xFF #xFF #xFF
                                           #xC3))))))

(ert-deftest nelisp-link-units-2pass-duplicate-strong-bubbles ()
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol "main" 0))))
         (ub (nelisp-link-test--unit
              "b.o"
              :text (nelisp-link-test--bytes 4)
              :symbols (list (nelisp-link-symbol "main" 0)))))
    (should-error
     (nelisp-link-units-2pass
      (list ua ub) '((text . #x400000)
                     (rodata . #x500000)
                     (data . #x600000)))
     :type 'nelisp-link--duplicate-symbol)))

(ert-deftest nelisp-link-units-2pass-unresolved-bubbles ()
  (let* ((ua (nelisp-link-test--unit
              "a.o"
              :text (nelisp-link-test--bytes 8)
              :relocs (list (nelisp-link-reloc
                             0 'pc32 "ghost")))))
    (should-error
     (nelisp-link-units-2pass
      (list ua) '((text . #x400000)
                  (rodata . #x500000)
                  (data . #x600000)))
     :type 'nelisp-link--unresolved-symbol)))

(ert-deftest nelisp-link-units-2pass-bss-passthrough ()
  ;; bss should be visible in the result without bytes, as an
  ;; integer size summed across units.
  (let* ((ua (nelisp-link-test--unit
              "a.o" :text (nelisp-link-test--bytes 4) :bss 16))
         (ub (nelisp-link-test--unit
              "b.o" :text (nelisp-link-test--bytes 4) :bss 32))
         (result (nelisp-link-units-2pass
                  (list ua ub) '((text . #x400000)
                                 (rodata . #x500000)
                                 (data . #x600000)))))
    (should (= (cdr (assq 'bss (plist-get result :bytes)))
               48))))

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

;; ============================================================
;; §93.c — layout + top-level driver + e2e exec
;; ============================================================

;; ---- §93.c (1) layout compute ----

(ert-deftest nelisp-link-compute-layout-single-unit-text-only ()
  ;; 1 text-only unit: phnum = 1, text-off = 64 + 56 = 120 = 0x78.
  ;; text VA = 0x400000 + 0x78 = 0x400078.
  (let* ((u (nelisp-link-test--unit
             "a.o" :text (nelisp-link-test--bytes 7)))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c)))
    (should (= (cdr (assq 'text layout)) #x400078))
    (should (= (cdr (assq 'rodata layout)) (+ #x400078 7)))))

(ert-deftest nelisp-link-compute-layout-two-unit-stack-back-to-back ()
  ;; 2 text-only units: text-size = 7 + 4 = 11; rodata VA sits right
  ;; after, no rw segment so data/bss collapse to rx-end.
  (let* ((ua (nelisp-link-test--unit
              "a.o" :text (nelisp-link-test--bytes 7)))
         (ub (nelisp-link-test--unit
              "b.o" :text (nelisp-link-test--bytes 4)))
         (c (nelisp-link-combine-sections (list ua ub)))
         (layout (nelisp-link--compute-layout c)))
    (should (= (cdr (assq 'text layout)) #x400078))
    (should (= (cdr (assq 'rodata layout)) (+ #x400078 11)))
    (should (= (cdr (assq 'data layout)) (+ #x400078 11)))
    (should (= (cdr (assq 'bss layout)) (+ #x400078 11)))))

(ert-deftest nelisp-link-compute-layout-with-rw-segment-page-aligned ()
  ;; .data present -> phnum = 2; text-off = 64 + 112 = 176 = 0xB0.
  ;; text-size = 8, rodata empty, rx-end = text-off + 8 = 184.
  ;; data-off aligned up to 0x1000 = 4096; data VA = 0x400000 + 4096.
  ;; bss VA = data VA + data-size.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 8)
             :data (nelisp-link-test--bytes 4)
             :bss 16))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c)))
    (should (= (cdr (assq 'text layout)) #x4000B0))
    (should (= (cdr (assq 'rodata layout)) (+ #x4000B0 8)))
    (should (= (cdr (assq 'data layout)) #x401000))
    (should (= (cdr (assq 'bss layout)) (+ #x401000 4)))))

(ert-deftest nelisp-link-compute-layout-three-unit-no-overlap ()
  ;; 3 units with text + rodata: verify VAs don't overlap.
  (let* ((ua (nelisp-link-test--unit
              "a.o" :text (nelisp-link-test--bytes 16)
              :rodata (nelisp-link-test--bytes 8)))
         (ub (nelisp-link-test--unit
              "b.o" :text (nelisp-link-test--bytes 8)))
         (uc (nelisp-link-test--unit
              "c.o" :text (nelisp-link-test--bytes 4)
              :rodata (nelisp-link-test--bytes 12)))
         (c (nelisp-link-combine-sections (list ua ub uc)))
         (layout (nelisp-link--compute-layout c)))
    ;; text-size = 28; rodata follows immediately, size = 20.
    (should (= (cdr (assq 'text layout)) #x400078))
    (should (= (cdr (assq 'rodata layout)) (+ #x400078 28)))
    ;; rodata range = [+28, +48); data starts at rx-end = +48 (no rw).
    (should (= (cdr (assq 'data layout)) (+ #x400078 48)))))

(ert-deftest nelisp-link-compute-layout-respects-base-va ()
  ;; Caller can override the default base VA.
  (let* ((u (nelisp-link-test--unit
             "a.o" :text (nelisp-link-test--bytes 4)))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c #x800000 #x1000)))
    (should (= (cdr (assq 'text layout)) (+ #x800000 #x78)))))

;; ---- §93.c (2) symtab export ----

(ert-deftest nelisp-link-export-symtab-shape ()
  ;; Build a 1-unit linker run and verify exported symtab is a list
  ;; of plists with the expected keys and section-relative `:value'.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 16)
             :symbols (list (nelisp-link-symbol "_start" 0)
                            (nelisp-link-symbol "_inner" 4))))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c))
         (r (nelisp-link-units-2pass (list u) layout))
         (syms (nelisp-link--export-symtab
                (plist-get r :symtab) layout)))
    (should (= (length syms) 2))
    (let ((start (cl-find "_start" syms :key (lambda (s)
                                               (plist-get s :name))
                          :test #'equal))
          (inner (cl-find "_inner" syms :key (lambda (s)
                                               (plist-get s :name))
                          :test #'equal)))
      (should start)
      (should (= (plist-get start :value) 0))
      (should (eq (plist-get start :section) 'text))
      (should (eq (plist-get start :bind) 'global))
      (should (= (plist-get inner :value) 4)))))

(ert-deftest nelisp-link-export-symtab-preserves-bind-weak ()
  ;; A WEAK symbol should round-trip through export with :bind 'weak.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 8)
             :symbols (list (nelisp-link-symbol
                             "soft" 0 :bind 'weak))))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c))
         (r (nelisp-link-units-2pass (list u) layout))
         (syms (nelisp-link--export-symtab
                (plist-get r :symtab) layout))
         (soft (car syms)))
    (should (equal (plist-get soft :name) "soft"))
    (should (eq (plist-get soft :bind) 'weak))))

(ert-deftest nelisp-link-export-symtab-section-rodata ()
  ;; A symbol defined in rodata should export with :value =
  ;; intra-section offset and :section 'rodata.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 4)
             :rodata (nelisp-link-test--bytes 16)
             :symbols (list (nelisp-link-symbol
                             "msg" 8 :section 'rodata
                             :type 'object))))
         (c (nelisp-link-combine-sections (list u)))
         (layout (nelisp-link--compute-layout c))
         (r (nelisp-link-units-2pass (list u) layout))
         (syms (nelisp-link--export-symtab
                (plist-get r :symtab) layout))
         (msg (car syms)))
    (should (equal (plist-get msg :name) "msg"))
    (should (eq (plist-get msg :section) 'rodata))
    (should (eq (plist-get msg :type) 'object))
    (should (= (plist-get msg :value) 8))))

;; ---- §93.c (3) top-level driver: unit binary properties ----

(ert-deftest nelisp-link-units-writes-file ()
  ;; nelisp-link-units returns FILE-PATH and writes an executable
  ;; ELF.  Smoke check: file exists, +x bit set, starts with `\177ELF'.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             ;; mov eax, 60; syscall  = sys_exit(0) (= rdi defaults
             ;; non-deterministic, but used here only for byte-level
             ;; shape checks, not exec).
             :text (unibyte-string #xb8 #x3c #x00 #x00 #x00
                                   #x0f #x05)
             :symbols (list (nelisp-link-symbol "_start" 0))))
         (path (make-temp-file "nelisp-link-units-shape-")))
    (unwind-protect
        (progn
          (nelisp-link-units path (list u))
          (should (file-exists-p path))
          (unless (memq system-type '(windows-nt ms-dos))
            (let ((modes (file-modes path)))
              (should (/= 0 (logand modes #o100)))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path nil 0 4)
            (should (equal (buffer-string)
                           (unibyte-string #x7f ?E ?L ?F)))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-link-units-defaults-entry-sym-to-_start ()
  ;; When ENTRY-SYM is omitted, "_start" must exist or the linker
  ;; raises an unresolved-symbol signal.
  (let* ((u (nelisp-link-test--unit
             "a.o"
             :text (nelisp-link-test--bytes 4)
             :symbols (list (nelisp-link-symbol "main" 0))))
         (path (make-temp-file "nelisp-link-units-noent-")))
    (unwind-protect
        (should-error
         (nelisp-link-units path (list u))
         :type 'nelisp-link--unresolved-symbol)
      (when (file-exists-p path) (delete-file path)))))

;; ---- §93.c (4) THE e2e exec test (= the Phase 47 critical gate) ----

(defun nelisp-link-test--e2e-units ()
  "Construct the §93.c e2e 2-unit fixture.
Returns the list of compile units.  Unit \"main.o\" emits a `call
helper' + `mov eax, 60' + `syscall' sequence (12 bytes); unit
\"helper.o\" emits `mov rdi, 0' + `ret' (8 bytes).  helper is
called BEFORE the exit syscall so it can pre-set rdi = 0 (= exit
code), then control returns to main and the syscall fires.  The
pc32 reloc at main offset 1 points at helper with addend 0 (= per
§93.a P = section-va + offset + 4 convention, A = 0 emits a CALL
disp that targets symbol value directly)."
  (let ((main (nelisp-link-test--unit
               "main.o"
               :text (unibyte-string
                      ;; call helper (rel32 placeholder)
                      #xE8 #x00 #x00 #x00 #x00
                      ;; mov eax, 60  (= __NR_exit)
                      #xB8 #x3C #x00 #x00 #x00
                      ;; syscall
                      #x0F #x05)
               :symbols (list (nelisp-link-symbol "_start" 0))
               :relocs (list (nelisp-link-reloc
                              1 'pc32 "helper" 0))))
        (helper (nelisp-link-test--unit
                 "helper.o"
                 :text (unibyte-string
                        ;; mov rdi, 0  (= REX.W mov, imm32)
                        #x48 #xC7 #xC7 #x00 #x00 #x00 #x00
                        ;; ret
                        #xC3)
                 :symbols (list (nelisp-link-symbol "helper" 0)))))
    (list main helper)))

(ert-deftest nelisp-link-e2e-multi-unit-exec-exit-0 ()
  "THE Phase 47 critical gate: link 2 units, exec, expect exit 0.
This is the chain validation for the entire Phase 47 spike (= Doc
91 ELF writer + Doc 92 assembler shape + Doc 93 static linker)."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (let ((m (or (and (boundp 'system-configuration)
                                      system-configuration) "")))
                      (string-match-p "x86_64" m))))
  (let* ((units (nelisp-link-test--e2e-units))
         (path "/tmp/nelisp-link-e2e-test"))
    (when (file-exists-p path) (delete-file path))
    (nelisp-link-units path units)
    (should (file-exists-p path))
    (set-file-modes path #o755)
    (let ((rc (call-process path nil nil nil)))
      (should (= rc 0)))))

(ert-deftest nelisp-link-e2e-binary-has-ehdr-with-x86_64 ()
  "The e2e binary's ELF header announces ET_EXEC + EM_X86_64."
  (let* ((units (nelisp-link-test--e2e-units))
         (path (make-temp-file "nelisp-link-e2e-hdr-")))
    (unwind-protect
        (progn
          (nelisp-link-units path units)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (let ((bytes (buffer-string)))
              ;; e_type @ 0x10 = ET_EXEC (= 2).
              (should (= (aref bytes #x10) 2))
              (should (= (aref bytes #x11) 0))
              ;; e_machine @ 0x12 = EM_X86_64 (= 0x3E).
              (should (= (aref bytes #x12) #x3E))
              (should (= (aref bytes #x13) 0)))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-link-units-e2e-symtab-has-start-and-helper ()
  "Both `_start' and `helper' should be present as global symbols."
  (let* ((units (nelisp-link-test--e2e-units))
         (combined (nelisp-link-combine-sections units))
         (layout (nelisp-link--compute-layout combined))
         (result (nelisp-link-units-2pass units layout))
         (tab (plist-get result :symtab)))
    (should (nelisp-link-symtab-lookup tab "_start"))
    (should (nelisp-link-symtab-lookup tab "helper"))
    ;; _start at offset 0 of main.o which is first in combined text.
    (should (= (plist-get (nelisp-link-symtab-lookup tab "_start")
                          :value)
               (cdr (assq 'text layout))))
    ;; helper is at offset 12 of combined text (after main's 12 bytes).
    (should (= (plist-get (nelisp-link-symtab-lookup tab "helper")
                          :value)
               (+ (cdr (assq 'text layout)) 12)))))

(ert-deftest nelisp-link-units-e2e-reloc-applied-correctly ()
  "The CALL displacement in main.o should be patched to reach helper.
S = helper VA = text-VA + 12; P_link = text-VA + 1 + 4 = text-VA
+ 5.  d = S + addend - P_link = 12 + 0 - 5 = 7.  bytes at offset
1..4 should be `07 00 00 00'.  CPU then computes target =
patch_addr + 4 + disp = (text-VA + 5) + 7 = text-VA + 12 = helper."
  (let* ((units (nelisp-link-test--e2e-units))
         (combined (nelisp-link-combine-sections units))
         (layout (nelisp-link--compute-layout combined))
         (result (nelisp-link-units-2pass units layout))
         (text (cdr (assq 'text (plist-get result :bytes)))))
    (should (= (aref text 0) #xE8))
    (should (= (aref text 1) #x07))
    (should (= (aref text 2) #x00))
    (should (= (aref text 3) #x00))
    (should (= (aref text 4) #x00))))

;; ---- §93.c (5) end-to-end with .rodata ----

(ert-deftest nelisp-link-units-with-rodata-binary-shape ()
  "Linker can emit a binary that references a .rodata constant.
The text contains a `lea rsi, [rip + disp]' (= 7 bytes) plus
`mov eax, 60; syscall' (= 7 bytes); rodata holds a 6-byte string.
Reloc is pc32 with addend -4 at text offset 3 (= disp32 within
the lea)."
  (let* ((rodata-bytes (unibyte-string ?h ?e ?l ?l ?o ?\n))
         (u (nelisp-link-test--unit
             "a.o"
             :text (unibyte-string
                    ;; lea rsi, [rip + disp32]   (7 bytes; disp @ +3)
                    #x48 #x8D #x35 #x00 #x00 #x00 #x00
                    ;; mov eax, 60
                    #xB8 #x3C #x00 #x00 #x00
                    ;; syscall
                    #x0F #x05)
             :rodata rodata-bytes
             :symbols (list (nelisp-link-symbol "_start" 0)
                            (nelisp-link-symbol "msg" 0
                                                :section 'rodata
                                                :type 'object
                                                :size 6))
             :relocs (list (nelisp-link-reloc
                            3 'pc32 "msg" -4))))
         (path (make-temp-file "nelisp-link-rodata-")))
    (unwind-protect
        (progn
          (nelisp-link-units path (list u))
          (should (file-exists-p path))
          ;; Check the rodata bytes survive into the file (= they
          ;; live immediately after .text in the loaded image).
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (let* ((bytes (buffer-string))
                   ;; text-off = 0x78, text-size = 14, rodata-off =
                   ;; 0x78 + 14 = 0x86.
                   (ro-pos (+ #x78 14)))
              (should (equal (substring bytes ro-pos
                                        (+ ro-pos 6))
                             (unibyte-string ?h ?e ?l ?l ?o ?\n))))))
      (when (file-exists-p path) (delete-file path)))))

;; ---- Mach-O executable link path ----

(ert-deftest nelisp-link-units-macho-exec-arm64-branch-unit ()
  "Mach-O executable linking resolves arm64 branch relocs across units."
  (let* ((main (nelisp-link-unit-make
                "main.o"
                (list (cons 'text
                            (nelisp-link-test--ub
                             #x00 #x00 #x00 #x94       ; bl helper
                             #xc0 #x03 #x5f #xd6)))    ; ret
                (list (nelisp-link-symbol "_main" 0
                                          :section 'text
                                          :bind 'global
                                          :type 'func))
                (list (nelisp-link-reloc 0 'b26-pc "helper"))))
         (helper (nelisp-link-unit-make
                  "helper.o"
                  (list (cons 'text
                              (nelisp-link-test--ub
                               #xc0 #x03 #x5f #xd6))) ; ret
                  (list (nelisp-link-symbol "helper" 0
                                            :section 'text
                                            :bind 'global
                                            :type 'func))
                  nil))
         (path (make-temp-file "nelisp-link-macho-" nil "")))
    (unwind-protect
        (progn
          (nelisp-link-units-macho-exec path (list main helper) "_main")
          (let* ((bytes (nelisp-link-test--read-file-bytes path))
                 (code-off nelisp-mach-o--exe-code-off))
            (should (equal (substring bytes 0 4)
                           (unibyte-string #xcf #xfa #xed #xfe)))
            (should (= (nelisp-link-test--read-le32 bytes 12) 2))
            (should (= (nelisp-link-test--read-le32 bytes 16) 15))
            ;; helper starts 8 bytes after _main, so BL imm26 is 2.
            (should (equal (substring bytes code-off (+ code-off 4))
                           (nelisp-link-test--ub #x02 #x00 #x00 #x94)))))
      (when (file-exists-p path) (delete-file path)))))

;; ---- PE32+ link path ----

(ert-deftest nelisp-link-units-pe32-import-thunk ()
  "PE linking resolves a direct extern call through an import thunk."
  (let* ((text (unibyte-string
                #x48 #x83 #xec #x28       ; sub rsp, 40
                #xb9 #x2a #x00 #x00 #x00 ; mov ecx, 42
                #xe8 #x00 #x00 #x00 #x00 ; call ExitProcess thunk
                #xcc))
         (unit (nelisp-link-unit-make
                "start.o"
                (list (cons 'text text))
                (list (nelisp-link-symbol "_start" 0
                                          :section 'text
                                          :bind 'global
                                          :type 'func))
                (list (list :offset 10
                            :type 'plt32
                            :symbol "ExitProcess"
                            :addend 0
                            :section 'text))))
         (path (make-temp-file "nelisp-link-pe32-" nil ".exe")))
    (unwind-protect
        (progn
          (nelisp-link-units-pe32 path (list unit) "_start"
                                  '("ExitProcess"))
          (let* ((bytes (nelisp-link-test--read-file-bytes path))
                 (pe-off (nelisp-link-test--read-le32 bytes #x3c))
                 (file-off (+ pe-off 4))
                 (opt-off (+ file-off 20))
                 (text-raw #x200)
                 (idata-raw #x400)
                 (idata-rva #x2000)
                 (iat-dir-off (+ opt-off 112 (* 12 8)))
                 (iat-rva (nelisp-link-test--read-le32 bytes iat-dir-off))
                 (hint-rva (nelisp-link-test--read-le64
                            bytes (+ idata-raw (- iat-rva idata-rva))))
                 (hint-off (+ idata-raw (- hint-rva idata-rva))))
            (should (equal (substring bytes 0 2) (unibyte-string #x4d #x5a)))
            (should (= (nelisp-link-test--read-le16 bytes file-off) #x8664))
            (should (= (nelisp-link-test--read-le32 bytes (+ opt-off 16)) #x1000))
            ;; The original call targets the synthesized thunk appended at
            ;; text offset 15, so rel32 from next-instruction offset 14 is 1.
            (should (= (nelisp-link-test--read-le32 bytes (+ text-raw 10)) 1))
            (should (equal (substring bytes (+ text-raw 15) (+ text-raw 17))
                           (unibyte-string #xff #x25)))
            ;; Thunk: jmp qword ptr [rip + IAT], IAT is at RVA 0x2038.
            (should (= (nelisp-link-test--read-le32 bytes (+ text-raw 17))
                       #x1023))
            (should (string-prefix-p "ExitProcess"
                                     (substring bytes (+ hint-off 2)
                                                (+ hint-off 14))))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-link-units-pe32-stack-reserve-option ()
  "PE linking forwards stack reserve options to the EXE writer."
  (let* ((text (unibyte-string #xcc))
         (unit (nelisp-link-unit-make
                "start.o"
                (list (cons 'text text))
                (list (nelisp-link-symbol "_start" 0
                                          :section 'text
                                          :bind 'global
                                          :type 'func))
                nil))
         (path (make-temp-file "nelisp-link-pe32-stack-" nil ".exe")))
    (unwind-protect
        (progn
          (nelisp-link-units-pe32 path (list unit) "_start" nil
                                  '(:stack-reserve #x40000000
                                    :stack-commit #x1000))
          (let* ((bytes (nelisp-link-test--read-file-bytes path))
                 (pe-off (nelisp-link-test--read-le32 bytes #x3c))
                 (file-off (+ pe-off 4))
                 (opt-off (+ file-off 20)))
            (should (= (nelisp-link-test--read-le64 bytes (+ opt-off 72))
                       #x40000000))
            (should (= (nelisp-link-test--read-le64 bytes (+ opt-off 80))
                       #x1000))))
      (when (file-exists-p path) (delete-file path)))))

(provide 'nelisp-static-linker-test)

;;; nelisp-static-linker-test.el ends here
