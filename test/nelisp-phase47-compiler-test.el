;;; nelisp-phase47-compiler-test.el --- ert + e2e for Doc 97 compiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert + end-to-end smoke for `nelisp-phase47-compile-sexp'.
;;
;; Three concerns covered:
;;
;;  1. Parser produces the expected IR shape for each v1 form.
;;  2. Emit phase produces the expected byte patterns (= invariant
;;     length per form; lengths summed across `seq` children).
;;  3. End-to-end smoke (= the Doc 97 production engagement gate):
;;     compile a Sexp program, exec the resulting binary on the host
;;     Linux kernel, observe stdout + exit code.
;;
;; E2E tests skip on non-Linux hosts via `ert-skip'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-phase47-compiler)

;; ---- §T.0 helpers ----

(defun nelisp-phase47-compiler-test--linux-p ()
  "Return non-nil when the host kernel can exec x86_64 ELF64 binaries."
  (and (eq system-type 'gnu/linux)
       (let ((arch (and (boundp 'system-configuration)
                        system-configuration)))
         (and (stringp arch)
              (string-match-p "x86_64\\|amd64" arch)))))

(defun nelisp-phase47-compiler-test--run-binary (path)
  "Exec PATH, return a plist `(:exit N :stdout S :stderr E)'."
  (let ((stdout-buf (generate-new-buffer " *nl97-stdout*"))
        (stderr-file (make-temp-file "nl97-stderr"))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (call-process path nil
                              (list stdout-buf stderr-file)
                              nil))
          (let ((stdout-text (with-current-buffer stdout-buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                (stderr-text (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (list :exit exit-code
                  :stdout stdout-text
                  :stderr stderr-text)))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun nelisp-phase47-compiler-test--tmp-binary (suffix)
  "Return a fresh /tmp/nelisp-doc97-SUFFIX-NNNN path.
The file is not created — `nelisp-phase47-compile-sexp' creates it.
Caller is responsible for `delete-file' on cleanup."
  (make-temp-file (format "nelisp-doc97-%s-" suffix)))

;; ---- §T.1 parser unit tests ----

(ert-deftest nelisp-phase47-compiler/parse-exit-literal ()
  "Parse `(exit 0)' to `(:kind exit :status 0)'."
  (let ((ir (nelisp-phase47-compiler--parse '(exit 0))))
    (should (eq (plist-get ir :kind) 'exit))
    (should (= (plist-get ir :status) 0))))

(ert-deftest nelisp-phase47-compiler/parse-write-literal ()
  "Parse `(write \"hi\")' to a write IR node."
  (let ((ir (nelisp-phase47-compiler--parse '(write "hi"))))
    (should (eq (plist-get ir :kind) 'write))
    (should (equal (plist-get ir :str) "hi"))))

(ert-deftest nelisp-phase47-compiler/parse-seq ()
  "Parse a `seq' of two children into nested IR."
  (let ((ir (nelisp-phase47-compiler--parse
             '(seq (write "x") (exit 0)))))
    (should (eq (plist-get ir :kind) 'seq))
    (should (= (length (plist-get ir :forms)) 2))
    (should (eq (plist-get (car (plist-get ir :forms)) :kind) 'write))
    (should (eq (plist-get (cadr (plist-get ir :forms)) :kind) 'exit))))

(ert-deftest nelisp-phase47-compiler/parse-let-arith-fold ()
  "Parse `(let ((x (+ 3 4))) (exit x))' folds arith + lookup."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((x (+ 3 4))) (exit x)))))
    (should (eq (plist-get ir :kind) 'let))
    (should (eq (plist-get ir :var) 'x))
    (should (= (plist-get ir :value) 7))
    (let ((body (plist-get ir :body)))
      (should (eq (plist-get body :kind) 'exit))
      (should (= (plist-get body :status) 7)))))

(ert-deftest nelisp-phase47-compiler/parse-nested-let-arith ()
  "Nested let + chained arithmetic resolves to a single integer."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((a 2)) (let ((b (* a 5))) (exit (+ b 1)))))))
    (should (eq (plist-get ir :kind) 'let))
    (let* ((body (plist-get ir :body))
           (inner (plist-get body :body)))
      (should (= (plist-get inner :status) 11)))))

(ert-deftest nelisp-phase47-compiler/parse-free-symbol-errors ()
  "A free symbol reference signals `nelisp-phase47-compiler-error'."
  (should-error
   (nelisp-phase47-compiler--parse '(exit y))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-unknown-form-errors ()
  "An unrecognised form head signals an error."
  (should-error
   (nelisp-phase47-compiler--parse '(if t 1 2))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-status-out-of-range ()
  "An exit status outside 0..255 signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit 300))
   :type 'nelisp-phase47-compiler-error))

;; ---- §T.2 emit / byte-length tests ----

(ert-deftest nelisp-phase47-compiler/emit-exit-is-16-bytes ()
  "`(exit 0)' emits exactly 16 bytes of .text (= Doc 92 fixed length)."
  (let* ((ir (nelisp-phase47-compiler--parse '(exit 0)))
         (buf (nelisp-phase47-compiler--pass ir nil 0)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 16))))

(ert-deftest nelisp-phase47-compiler/emit-write-is-33-bytes ()
  "`(write \"hi\")' emits exactly 33 bytes of .text."
  (let* ((ir (nelisp-phase47-compiler--parse '(write "hi")))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (buf (nelisp-phase47-compiler--pass ir offsets #x401000)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 33))))

(ert-deftest nelisp-phase47-compiler/strings-dedup ()
  "Two `(write \"x\")' forms share a single .rodata slot."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "x") (write "x") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 1))
    (should (= (length rodata) 1))
    (should (equal (substring rodata 0 1) "x"))))

(ert-deftest nelisp-phase47-compiler/strings-distinct ()
  "Two distinct strings get distinct offsets summing to total length."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "ab") (write "cd") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 2))
    (should (= (length rodata) 4))
    (should (equal rodata "abcd"))
    (should (= (plist-get (cdr (assoc "ab" offsets)) :offset) 0))
    (should (= (plist-get (cdr (assoc "cd" offsets)) :offset) 2))))

(ert-deftest nelisp-phase47-compiler/pass1-pass2-byte-length-parity ()
  "Pass-1 and pass-2 emit must agree on .text byte length."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "hi") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (pass1 (nelisp-phase47-compiler--pass ir offsets 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1))
         (pass2 (nelisp-phase47-compiler--pass ir offsets
                                               (+ #x400000 size1 #x78)))
         (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
    (should (= size1 size2))))

;; ---- §T.3 e2e smoke (= the production engagement gate) ----

(ert-deftest nelisp-phase47-compiler/e2e-hello-world ()
  "Compile `(seq (write \"hello\\n\") (exit 0))', exec, observe."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "hello")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "hello\n") (exit 0)) path)
          (should (file-executable-p path))
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hello\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-exit-42 ()
  "Compile `(exit 42)', exec, observe exit code 42."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "exit42")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp '(exit 42) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 42))
            (should (equal (plist-get result :stdout) ""))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-let-exit ()
  "Compile `(seq (let ((x 7)) (exit x)))', exec, observe exit code 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "let-exit")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (let ((x 7)) (exit x))) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 7))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-arith-exit ()
  "Compile `(exit (+ 1 2))', exec, observe exit code 3."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "arith-exit")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp '(exit (+ 1 2)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 3))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-multi-write ()
  "Compile two-write seq, observe concatenated stdout + exit 0."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "multi-write")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "hi") (write " there\n") (exit 0)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hi there\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-string-dedup-execs ()
  "Duplicated string survives dedup + runs."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "dedup")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "ab") (write "ab") (exit 0)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "abab"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-arch-rejects-non-x86_64 ()
  "Asking for `:arch 'aarch64' signals (= v1 OOS)."
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "arch-rej")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-sexp '(exit 0) path :arch 'aarch64)
         :type 'nelisp-phase47-compiler-error)
      (when (file-exists-p path) (delete-file path)))))

(provide 'nelisp-phase47-compiler-test)

;;; nelisp-phase47-compiler-test.el ends here
