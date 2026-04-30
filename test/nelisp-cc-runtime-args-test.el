;;; nelisp-cc-runtime-args-test.el --- Doc 48 exec-bytes argv ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 48 Phase 7.5.4: `nelisp-exec-bytes <file> [ARG1..ARG6]'
;; accepts up to six i64 register arguments.  These smokes use tiny
;; x86_64 byte streams so the test locks the bridge ABI directly,
;; independent of frontend/backend compiler evolution.

;;; Code:

(require 'ert)
(require 'nelisp-cc-runtime)

(defun nelisp-cc-runtime-args-test--host-x86_64-p ()
  "Return non-nil on x86_64 hosts."
  (let ((cfg (downcase (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))
    (or (string-match-p "x86_64" cfg)
        (string-match-p "amd64" cfg))))

(defun nelisp-cc-runtime-args-test--skip-unless-real-exec ()
  "Skip unless the subprocess `exec-bytes' bridge is available."
  (unless (ignore-errors (nelisp-cc-runtime--locate-exec-bytes-bin))
    (ert-skip "nelisp-exec-bytes binary missing — run `make runtime-cli'"))
  (unless (nelisp-cc-runtime-args-test--host-x86_64-p)
    (ert-skip "Doc 48 argv smoke byte streams are x86_64-only")))

(defun nelisp-cc-runtime-args-test--result (bytes &optional args)
  "Run BYTES with ARGS through `nelisp-cc-runtime--exec-real'."
  (let ((result (nelisp-cc-runtime--exec-real bytes args)))
    (unless (eq (car result) :result)
      (signal 'nelisp-cc-runtime-error
              (list :real-exec-failed result)))
    (nth 2 result)))

(ert-deftest nelisp-cc-runtime-args-zero-arg ()
  "Zero-argument payloads remain compatible and return 42."
  (nelisp-cc-runtime-args-test--skip-unless-real-exec)
  (should
   (= 42
      (nelisp-cc-runtime-args-test--result
       ;; mov eax, 42 ; ret
       (vector #xB8 #x2A #x00 #x00 #x00 #xC3)))))

(ert-deftest nelisp-cc-runtime-args-one-arg-inc ()
  "ARG1 arrives in RDI: return x + 1."
  (nelisp-cc-runtime-args-test--skip-unless-real-exec)
  (should
   (= 6
      (nelisp-cc-runtime-args-test--result
       ;; mov rax, rdi ; add rax, 1 ; ret
       (vector #x48 #x89 #xF8
               #x48 #x83 #xC0 #x01
               #xC3)
       '(5)))))

(ert-deftest nelisp-cc-runtime-args-two-arg-add ()
  "ARG1/ARG2 arrive in RDI/RSI: return x + y."
  (nelisp-cc-runtime-args-test--skip-unless-real-exec)
  (should
   (= 7
      (nelisp-cc-runtime-args-test--result
       ;; mov rax, rdi ; add rax, rsi ; ret
       (vector #x48 #x89 #xF8
               #x48 #x01 #xF0
               #xC3)
       '(3 4)))))

(ert-deftest nelisp-cc-runtime-args-three-arg-fma ()
  "ARG1..ARG3 arrive in RDI/RSI/RDX: return x * y + z."
  (nelisp-cc-runtime-args-test--skip-unless-real-exec)
  (should
   (= 10
      (nelisp-cc-runtime-args-test--result
       ;; mov rax, rdi ; imul rax, rsi ; add rax, rdx ; ret
       (vector #x48 #x89 #xF8
               #x48 #x0F #xAF #xC6
               #x48 #x01 #xD0
               #xC3)
       '(2 3 4)))))

(ert-deftest nelisp-cc-runtime-args-six-arg-sum ()
  "ARG1..ARG6 arrive in RDI/RSI/RDX/RCX/R8/R9."
  (nelisp-cc-runtime-args-test--skip-unless-real-exec)
  (should
   (= 21
      (nelisp-cc-runtime-args-test--result
       ;; mov rax, rdi
       ;; add rax, rsi ; add rax, rdx ; add rax, rcx
       ;; add rax, r8  ; add rax, r9  ; ret
       (vector #x48 #x89 #xF8
               #x48 #x01 #xF0
               #x48 #x01 #xD0
               #x48 #x01 #xC8
               #x4C #x01 #xC0
               #x4C #x01 #xC8
               #xC3)
       '(1 2 3 4 5 6)))))

(ert-deftest nelisp-cc-runtime-args-rejects-too-many ()
  "The Elisp wrapper rejects arity > 6 before spawning subprocess."
  (should-error
   (nelisp-cc-runtime--exec-real (vector #xC3) '(1 2 3 4 5 6 7))
   :type 'nelisp-cc-runtime-error))

(provide 'nelisp-cc-runtime-args-test)
;;; nelisp-cc-runtime-args-test.el ends here
