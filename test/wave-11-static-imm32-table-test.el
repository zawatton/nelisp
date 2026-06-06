;;; wave-11-static-imm32-table-test.el --- Doc 49 Wave 11.1 primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert + e2e for Doc 49 Wave 11.1 `static-imm32-table-define' /
;; `static-imm32-table-lookup' AOT primitives.
;;
;; Coverage:
;;
;;  1. Parser produces the expected IR shapes for define + lookup.
;;  2. Collector pass deduplicates by name + concatenates u32 bytes.
;;  3. Emit pass produces the fixed-width 20-byte (+ index expr) emit
;;     and pass-1 / pass-2 byte invariance holds.
;;  4. End-to-end: compile a program with a 5-element table, call
;;     `(static-imm32-table-lookup ...)' from a defun, exit with the
;;     loaded u32 — observe the matching exit code.
;;
;; E2E tests skip on non-Linux hosts via `ert-skip'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add ../lisp to load-path (= mirrors nelisp-asm-x86_64-test.el).
(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-asm-x86_64)
(require 'nelisp-aot-compiler)

;; ---- helpers (= mirror nelisp-aot-compiler-test helpers) ----

(defun wave-11-static-imm32-table-test--linux-p ()
  "Return non-nil when the host kernel can exec x86_64 ELF64 binaries."
  (and (eq system-type 'gnu/linux)
       (let ((arch (and (boundp 'system-configuration)
                        system-configuration)))
         (and (stringp arch)
              (string-match-p "x86_64\\|amd64" arch)))))

(defun wave-11-static-imm32-table-test--run-binary (path)
  "Exec PATH, return a plist `(:exit N :stdout S :stderr E)'."
  (let ((stdout-buf (generate-new-buffer " *w11-stdout*"))
        (stderr-file (make-temp-file "w11-stderr"))
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

(defun wave-11-static-imm32-table-test--tmp-binary (suffix)
  (make-temp-file (format "nelisp-w11-%s-" suffix)))

;; ---- §T.1 parser tests ----

(ert-deftest wave-11-static-imm32-table/parse-define ()
  "`static-imm32-table-define' parses into a table-define IR node."
  (let ((ir (nelisp-aot-compiler--parse
             '(static-imm32-table-define "t" (10 20 30)))))
    (should (eq (plist-get ir :kind) 'table-define))
    (should (equal (plist-get ir :name) "t"))
    (should (equal (plist-get ir :elements) '(10 20 30)))))

(ert-deftest wave-11-static-imm32-table/parse-define-rejects-non-int ()
  "Non-integer element signals."
  (should-error
   (nelisp-aot-compiler--parse
    '(static-imm32-table-define "t" (1 "x" 3)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest wave-11-static-imm32-table/parse-define-rejects-non-string-name ()
  "Non-string NAME signals."
  (should-error
   (nelisp-aot-compiler--parse
    '(static-imm32-table-define t (1 2 3)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest wave-11-static-imm32-table/parse-lookup ()
  "`static-imm32-table-lookup' inside a defun parses into a table-lookup IR."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun look (i) (static-imm32-table-lookup "t" i))))
         (body (plist-get ir :body)))
    (should (eq (plist-get ir :kind) 'defun))
    (should (eq (plist-get body :kind) 'table-lookup))
    (should (equal (plist-get body :name) "t"))
    (let ((idx (plist-get body :index)))
      (should (eq (plist-get idx :kind) 'ref))
      (should (eq (plist-get idx :var) 'i)))))

;; ---- §T.2 collector tests ----

(ert-deftest wave-11-static-imm32-table/collect-single-table ()
  "Single table-define lands in the collector output as 4N little-endian bytes."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (static-imm32-table-define "t" (#x0A #x14 #x1E))
                    (exit 0))))
         (collected (nelisp-aot-compiler--collect-tables ir))
         (offsets (car collected))
         (bytes   (cdr collected)))
    (should (= (length offsets) 1))
    (should (equal (plist-get (cdr (assoc "t" offsets)) :offset) 0))
    (should (= (plist-get (cdr (assoc "t" offsets)) :len) 12))
    (should (= (length bytes) 12))
    ;; Little-endian: 0x0A 00 00 00 | 0x14 00 00 00 | 0x1E 00 00 00.
    (should (= (aref bytes 0) #x0A))
    (should (= (aref bytes 4) #x14))
    (should (= (aref bytes 8) #x1E))))

(ert-deftest wave-11-static-imm32-table/collect-rejects-duplicate-name ()
  "Two table-defines with the same NAME signal."
  (should-error
   (nelisp-aot-compiler--collect-tables
    (nelisp-aot-compiler--parse
     '(seq (static-imm32-table-define "t" (1 2))
           (static-imm32-table-define "t" (3 4))
           (exit 0))))
   :type 'nelisp-aot-compiler-error))

(ert-deftest wave-11-static-imm32-table/collect-no-tables-returns-empty ()
  "Programs with no tables produce empty collector output."
  (let* ((ir (nelisp-aot-compiler--parse '(exit 0)))
         (collected (nelisp-aot-compiler--collect-tables ir)))
    (should (null (car collected)))
    (should (= (length (cdr collected)) 0))))

;; ---- §T.3 emit byte-length invariance ----

(ert-deftest wave-11-static-imm32-table/emit-lookup-byte-invariance ()
  "Pass-1 and pass-2 emit identical text byte counts for a table-lookup."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (static-imm32-table-define "t" (1 2 3))
                    (defun look (i) (static-imm32-table-lookup "t" i))
                    (exit (look 0)))))
         (str-offsets nil)
         (table-collected (nelisp-aot-compiler--collect-tables ir))
         (table-offsets (car table-collected))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (placeholder-vaddrs
          (mapcar (lambda (e) (cons (car e) 0)) table-offsets))
         (real-vaddrs
          (mapcar (lambda (e) (cons (car e) #xABCDEF0123)) table-offsets))
         (pass1 (nelisp-aot-compiler--pass
                 ir defuns str-offsets 0 placeholder-vaddrs))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1))
         (pass2 (nelisp-aot-compiler--pass
                 ir defuns str-offsets #x401000 real-vaddrs))
         (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
    (should (= size1 size2))))

(ert-deftest wave-11-static-imm32-table/asm-shl-reg-imm8-shape ()
  "`shl-reg-imm8' encodes `shl rsi, 2' as 48 C1 E6 02."
  (let ((buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (nelisp-asm-x86_64-shl-reg-imm8 buf 'rsi 2)
    (let ((bs (nelisp-asm-x86_64-buffer-bytes buf)))
      (should (= (length bs) 4))
      (should (= (aref bs 0) #x48))
      (should (= (aref bs 1) #xC1))
      (should (= (aref bs 2) #xE6))
      (should (= (aref bs 3) #x02)))))

;; ---- §T.4 end-to-end smoke ----

(ert-deftest wave-11-static-imm32-table/e2e-lookup-first-element ()
  "Compile + exec a program that returns table[0] via the lookup primitive."
  (unless (wave-11-static-imm32-table-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (wave-11-static-imm32-table-test--tmp-binary "lookup0")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq
             (static-imm32-table-define "t" (42 100 200 1 7))
             (defun look (i) (static-imm32-table-lookup "t" i))
             (exit (look 0)))
           path)
          (should (file-executable-p path))
          (let ((result (wave-11-static-imm32-table-test--run-binary path)))
            (should (= (plist-get result :exit) 42))
            (should (equal (plist-get result :stdout) ""))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest wave-11-static-imm32-table/e2e-lookup-mid-element ()
  "Compile + exec returning table[2] = 200 → exit code 200."
  (unless (wave-11-static-imm32-table-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (wave-11-static-imm32-table-test--tmp-binary "lookup2")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq
             (static-imm32-table-define "t" (42 100 200 1 7))
             (defun look (i) (static-imm32-table-lookup "t" i))
             (exit (look 2)))
           path)
          (let ((result (wave-11-static-imm32-table-test--run-binary path)))
            (should (= (plist-get result :exit) 200))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest wave-11-static-imm32-table/e2e-lookup-last-element ()
  "Compile + exec returning table[4] = 7 → exit code 7."
  (unless (wave-11-static-imm32-table-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (wave-11-static-imm32-table-test--tmp-binary "lookup4")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq
             (static-imm32-table-define "t" (42 100 200 1 7))
             (defun look (i) (static-imm32-table-lookup "t" i))
             (exit (look 4)))
           path)
          (let ((result (wave-11-static-imm32-table-test--run-binary path)))
            (should (= (plist-get result :exit) 7))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest wave-11-static-imm32-table/e2e-existing-write-no-regression ()
  "Adding a static-imm32 table does not shift string vaddrs (= hello\\n still works)."
  (unless (wave-11-static-imm32-table-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (wave-11-static-imm32-table-test--tmp-binary "regress")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq
             (static-imm32-table-define "t" (1 2 3))
             (defun look (i) (static-imm32-table-lookup "t" i))
             (write "hi\n")
             (exit (look 1)))
           path)
          (let ((result (wave-11-static-imm32-table-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hi\n"))
            (should (= (plist-get result :exit) 2))))
      (when (file-exists-p path) (delete-file path)))))

(provide 'wave-11-static-imm32-table-test)

;;; wave-11-static-imm32-table-test.el ends here
