;;; nelisp-macho-acceptance.el --- macOS CI acceptance artifacts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emits the Mach-O artifact set consumed by
;; scripts/macho-acceptance-check.sh (the CI macOS acceptance step):
;;
;;   dist/macho-acceptance/exit42        MH_EXECUTE — exit(42) via raw
;;                                       Darwin syscall (SVC #0x80)
;;   dist/macho-acceptance/add5          MH_EXECUTE — exit(add(2,3)) = 5,
;;                                       exercises the defun call path
;;   dist/macho-acceptance/add_macho.o   MH_OBJECT — global `_add',
;;                                       linked against a C harness by
;;                                       clang/ld64 on the runner
;;   dist/macho-acceptance/caller_macho.o MH_OBJECT — `_inc2' calling an
;;                                       UNDEFINED `_inc' through
;;                                       ARM64_RELOC_BRANCH26 records
;;                                       (writer v2 reloc lane); the C
;;                                       harness provides `inc' and
;;                                       ld64 must resolve the branch
;;   dist/macho-acceptance/libc_macho.o  MH_OBJECT — `_myabs' calling
;;                                       libSystem's `_abs' via
;;                                       extern-call; dyld binds the
;;                                       import in the linked binary,
;;                                       proving the libSystem lane
;;                                       needs no bespoke dyld writer
;;   dist/macho-acceptance/rodata_macho.o MH_OBJECT — hand-assembled
;;                                       `_getmagic' materializing the
;;                                       address of a __const `_magic'
;;                                       via ARM64_RELOC_PAGE21 +
;;                                       PAGEOFF12 (writer v3
;;                                       multi-section lane); ld64 must
;;                                       patch both instructions and
;;                                       the loaded value must be 42.
;;                                       Hand-assembled because the
;;                                       arm64 compiler lane cannot yet
;;                                       emit rodata-bearing units (its
;;                                       runtime-helper calls still use
;;                                       in-buffer labels, not relocs).
;;
;; Emission is host-agnostic (the writers are pure elisp), so this
;; runs on any CI OS; only the checks require macOS.  The executables
;; are UNSIGNED — the check script finalizes them with
;; `codesign -f -s -' (Apple Silicon mandates a signature; see the
;; ground-truth notes in scripts/nelisp-macos-build.el).

;;; Code:

(require 'nelisp-macos-build)
(require 'nelisp-aot-compiler)
(require 'nelisp-mach-o-write)
(require 'nelisp-asm-arm64)

(defvar nelisp-macho-acceptance-outdir "dist/macho-acceptance"
  "Output directory for the acceptance artifact set.")

(defun nelisp-macho-acceptance--emit-rodata-object (path)
  "Hand-assemble `_getmagic' loading a __const value via PAGE relocs.
Instruction sequence: ADRP x0, magic@PAGE / ADD x0, x0, magic@PAGEOFF
/ LDR x0, [x0] / RET.  The adrp/add emitters record the
adr-prel-pg-hi21 / add-abs-lo12-nc relocations that writer v3 lowers
to ARM64_RELOC_PAGE21 / PAGEOFF12."
  (let ((buf (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-adrp buf 'x0 "magic")
    (nelisp-asm-arm64-add-abs-lo12-nc buf 'x0 'x0 "magic")
    (nelisp-asm-arm64-mov-imm-z buf 'x1 0)
    (nelisp-asm-arm64-ldr-reg-reg buf 'x0 'x0 'x1)
    (nelisp-asm-arm64-ret buf)
    (let ((text (nelisp-asm-arm64-buffer-bytes buf))
          (relocs (mapcar (lambda (r) (append r (list :section 'text)))
                          (nelisp-asm-arm64-buffer-relocs buf))))
      (nelisp-mach-o-write-binary
       path
       (list :text text
             :rodata (unibyte-string 42 0 0 0 0 0 0 0)
             :symbols (list (list :name "getmagic" :value 0
                                  :size (length text)
                                  :section 'text :bind 'global :type 'func)
                            (list :name "magic" :value 0 :size 8
                                  :section 'rodata :bind 'global
                                  :type 'object))
             :relocs relocs
             :machine 'aarch64)))))

(defun nelisp-macho-acceptance-emit ()
  "Batch entry: write the macOS acceptance artifact set."
  (let ((dir (file-name-as-directory nelisp-macho-acceptance-outdir)))
    (make-directory dir t)
    (nelisp-macos-build-program
     '(syscall-direct 1 42 0 0 0 0 0)
     (concat dir "exit42"))
    (nelisp-macos-build-program
     '(seq (defun add (a b) (+ a b))
           (exit (add 2 3)))
     (concat dir "add5"))
    (nelisp-aot-compile-to-object
     '(defun add (a b) (+ a b))
     (concat dir "add_macho.o")
     :arch 'aarch64 :format 'mach-o)
    (nelisp-aot-compile-to-object
     '(defun inc2 (x) (inc (inc x)))
     (concat dir "caller_macho.o")
     :arch 'aarch64 :format 'mach-o)
    (nelisp-aot-compile-to-object
     '(defun myabs (x) (extern-call abs x))
     (concat dir "libc_macho.o")
     :arch 'aarch64 :format 'mach-o)
    (nelisp-macho-acceptance--emit-rodata-object
     (concat dir "rodata_macho.o"))
    ;; Compiler-emitted user module: defvar lowers to an init helper
    ;; calling nl_alloc_symbol etc. — BLs the arm64 lane externalizes
    ;; into CALL26 imports — plus the module-init metadata in __const.
    ;; The check script verifies ld64 accepts the full shape via a
    ;; relocatable merge (running it would need the NeLisp runtime).
    (nelisp-aot-compile-to-object
     '(seq (defvar counter 7))
     (concat dir "defvar_macho.o")
     :arch 'aarch64 :format 'mach-o)
    (message "macho-acceptance: artifact set written under %s" dir)))

(provide 'nelisp-macho-acceptance)

;;; nelisp-macho-acceptance.el ends here
