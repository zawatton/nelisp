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
  "Compile top-level SEXP to aarch64 / Darwin __text bytes.
Returns a unibyte string whose first instruction is the program entry."
  (let ((nelisp-phase47-compiler--arch 'aarch64)
        (nelisp-phase47-compiler--os 'darwin)
        (buf (nelisp-asm-arm64-make-buffer)))
    (let ((ir (nelisp-phase47-compiler--parse-value sexp nil nil nil)))
      (nelisp-phase47-compiler--emit-value ir buf))
    ;; Resolve B / B.cond label fixups before materialising — without
    ;; this every branch keeps its placeholder offset 0 (= self-loop).
    (let ((bytes (nelisp-asm-arm64-resolve-fixups buf)))
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
