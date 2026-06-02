;;; nelisp-macos-build.el --- native macOS arm64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Builds native macOS arm64 (Apple Silicon) executables from the
;; Phase-47 aarch64 backend, wrapped in the pure-elisp Mach-O
;; MH_EXECUTE writer (`nelisp-mach-o-write-executable').
;;
;; Ground truth (verified on an M1, macOS 26):
;;   * a hand-built no-dyld LC_UNIXTHREAD image is SIGKILLed by the
;;     kernel — the executable MUST load /usr/lib/dyld + libSystem,
;;     even though all work is done via raw `svc' syscalls;
;;   * raw Darwin syscalls (x16 = NR, `SVC #0x80') run fine;
;;   * Apple Silicon mandates a code signature, so the image emitted
;;     here is UNSIGNED — `codesign -s -' (run on macOS) finalises it.
;;
;; The compiled program's entry instruction is placed first in __text
;; (file offset 728 = the `_main' address baked into the writer), so a
;; top-level expression that never returns (e.g. an `exit' syscall)
;; runs directly as the process entry point.

;;; Code:

(require 'nelisp-phase47-compiler)
(require 'nelisp-mach-o-write)
(require 'nelisp-asm-arm64)

(defun nelisp-macos-build--emit-text (sexp)
  "Compile a full program SEXP to aarch64 / Darwin __text bytes.
SEXP may contain `defun' forms.  The main body (entry point) is
emitted first so it runs at the Mach-O `LC_MAIN' offset, then each
defun is appended after; their labels are forward-resolved by
`resolve-fixups' (also required for B / B.cond — without it every
branch keeps its placeholder offset 0 = self-loop).  Returns a
unibyte string containing code followed by string/table rodata."
  (let* ((nelisp-phase47-compiler--arch 'aarch64)
         (nelisp-phase47-compiler--os 'darwin)
         (nelisp-phase47-compiler--label-counter 0)
         ;; Statement grammar (`exit'/`write'/`seq'/`defun'/...) accepts
         ;; programs with function definitions; a bare value expression
         ;; (e.g. ending in `syscall-direct') is `:unknown-form' there, so
         ;; fall back to the value grammar for those.
         (stmt-ir (condition-case nil
                      (nelisp-phase47-compiler--parse sexp nil)
                    (nelisp-phase47-compiler-error nil))))
    (let ((bytes
           (if stmt-ir
               (let* ((str-collected
                       (nelisp-phase47-compiler--collect-strings stmt-ir))
                      (str-offsets (car str-collected))
                      (str-rodata-bytes (cdr str-collected))
                      (table-collected
                       (nelisp-phase47-compiler--collect-tables stmt-ir))
                      (table-offsets (car table-collected))
                      (table-bytes (cdr table-collected))
                      (str-rodata-len (length str-rodata-bytes))
                      (rodata-bytes (concat str-rodata-bytes table-bytes))
                      (defuns (nelisp-phase47-compiler--collect-defuns stmt-ir))
                      (emit-pass
                       (lambda (rodata-vaddr table-vaddrs)
                         (let ((nelisp-phase47-compiler--table-vaddrs
                                table-vaddrs)
                               (buf (nelisp-asm-arm64-make-buffer)))
                           (nelisp-phase47-compiler--emit-stmt
                            stmt-ir buf str-offsets rodata-vaddr)
                           (dolist (d defuns)
                             (nelisp-phase47-compiler--emit-defun d buf))
                           buf)))
                      (pass1-table-vaddrs
                       (mapcar (lambda (entry) (cons (car entry) 0))
                               table-offsets))
                      (pass1 (funcall emit-pass 0 pass1-table-vaddrs))
                      (text-size (nelisp-asm-arm64-buffer-pos pass1))
                      (rodata-vaddr
                       (+ nelisp-mach-o--exe-text-vmaddr
                          nelisp-mach-o--exe-code-off
                          text-size))
                      (table-vaddrs
                       (mapcar (lambda (entry)
                                 (let* ((name (car entry))
                                        (info (cdr entry))
                                        (offset (plist-get info :offset)))
                                   (cons name
                                         (+ rodata-vaddr
                                            str-rodata-len
                                            offset))))
                               table-offsets))
                      (pass2 (funcall emit-pass rodata-vaddr table-vaddrs))
                      (text-bytes (nelisp-asm-arm64-resolve-fixups pass2)))
                 (concat text-bytes rodata-bytes))
             (let ((buf (nelisp-asm-arm64-make-buffer)))
               (nelisp-phase47-compiler--emit-value
                (nelisp-phase47-compiler--parse-value sexp nil nil nil) buf)
               (nelisp-asm-arm64-resolve-fixups buf)))))
      (if (multibyte-string-p bytes)
          (apply #'unibyte-string (append bytes nil))
        bytes))))

(defun nelisp-macos-build-program (sexp out-path)
  "Compile SEXP and write an UNSIGNED native macOS arm64 executable to OUT-PATH.
Sign it on macOS with `codesign -f -s - OUT-PATH' before running."
  (let ((text (nelisp-macos-build--emit-text sexp)))
    (nelisp-mach-o-write-executable
     out-path (list :text text :machine 'aarch64 :entry-sym "_main"))
    (message "nelisp-macos-build: wrote %s (%d-byte __text)" out-path (length text))
    out-path))

(defun nelisp-macos-build-exit42 ()
  "Batch entry: build target/nelisp-macos-exit42 (compiler-emitted exit 42).
Program = (syscall-direct 1 42 0 0 0 0 0) = Darwin `exit(42)' via `SVC #0x80'."
  (nelisp-macos-build-program '(syscall-direct 1 42 0 0 0 0 0)
                              "target/nelisp-macos-exit42"))

(provide 'nelisp-macos-build)

;;; nelisp-macos-build.el ends here
