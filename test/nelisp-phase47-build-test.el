;;; nelisp-phase47-build-test.el --- ERT tests for Doc 96 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 96 — chain-level integration ert tests for the Phase 47 build
;; orchestrator.  Covers each demo function with byte-level emission
;; checks + exec-time stdout / exit-code assertions on x86_64 Linux.
;; Cross-arch coverage asserts the `:machine' tag flows through to
;; the Doc 91 ELF writer (= EM_AARCH64 in the e_machine field).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-phase47-build)

;; ---------------------------------------------------------------- helpers

(defun nelisp-phase47-build-test--read-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-phase47-build-test--read-output (path)
  "Return PATH's contents as unibyte bytes (= for stdout capture)."
  (nelisp-phase47-build-test--read-bytes path))

(defun nelisp-phase47-build-test--x86_64-host-p ()
  "Return non-nil when running on x86_64 Linux (= can exec a binary)."
  (and (memq system-type '(gnu/linux gnu))
       (string-match-p "x86_64\\|amd64"
                       (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))

(defun nelisp-phase47-build-test--read-le16 (bytes offset)
  "Read 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

;; ---------------------------------------------------------------- §1 hello-world

(ert-deftest nelisp-phase47-build-hello-world-emits-elf ()
  "`build-hello-world' writes an ELF64 with magic 7f 45 4c 46."
  (let ((path (make-temp-file "nelisp-phase47-hello-emit-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-hello-world path)
          (let ((bytes (nelisp-phase47-build-test--read-bytes path)))
            (should (>= (length bytes) 64))
            (should (= (aref bytes 0) #x7f))
            (should (= (aref bytes 1) ?E))
            (should (= (aref bytes 2) ?L))
            (should (= (aref bytes 3) ?F))
            ;; e_machine at offset 18 = EM_X86_64 (62) for default arch.
            (should (= (nelisp-phase47-build-test--read-le16 bytes 18)
                       62))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-build-hello-world-exec ()
  "End-to-end: exec hello-world, stdout = `hello\\n', exit code 0."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-phase47-hello-exec-"))
        (output (make-temp-file "nelisp-phase47-hello-out-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-hello-world path)
          (let ((rc (call-process path nil (list :file output) nil)))
            (should (eq rc 0))
            (should
             (equal
              (nelisp-phase47-build-test--read-output output)
              (unibyte-string ?h ?e ?l ?l ?o ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(ert-deftest nelisp-phase47-build-hello-world-readelf ()
  "`readelf -h' reports ELF64 + EXEC + x86-64 on the emitted file."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-phase47-hello-readelf-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-hello-world path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-h" path)))))
            (should (string-match-p "ELF64" out))
            (should (string-match-p "EXEC" out))
            (should (string-match-p "X86-64\\|x86-64\\|x86_64" out))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- §2 exit-with-status

(ert-deftest nelisp-phase47-build-exit-with-status-zero ()
  "exit-with-status 0 → process exits with code 0."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path (make-temp-file "nelisp-phase47-exit0-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-exit-with-status path 0)
          (should (eq (call-process path nil nil nil) 0)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-build-exit-with-status-42 ()
  "exit-with-status 42 → process exits with code 42."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path (make-temp-file "nelisp-phase47-exit42-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-exit-with-status path 42)
          (should (eq (call-process path nil nil nil) 42)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-build-exit-with-status-255 ()
  "exit-with-status 255 → process exits with code 255 (= max byte)."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path (make-temp-file "nelisp-phase47-exit255-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-exit-with-status path 255)
          (should (eq (call-process path nil nil nil) 255)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-build-exit-with-status-range-check ()
  "exit-with-status signals on out-of-range STATUS (= < 0 or > 255)."
  (let ((path (make-temp-file "nelisp-phase47-exit-bad-")))
    (unwind-protect
        (progn
          (should-error (nelisp-phase47-build-exit-with-status path -1)
                        :type 'nelisp-phase47-build-error)
          (should-error (nelisp-phase47-build-exit-with-status path 256)
                        :type 'nelisp-phase47-build-error))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- §3 argc-printer

(ert-deftest nelisp-phase47-build-argc-printer-no-args ()
  "argc-printer with no args → stdout = `argc=1\\n'."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-phase47-argc1-"))
        (output (make-temp-file "nelisp-phase47-argc1-out-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-argc-printer path)
          (let ((rc (call-process path nil (list :file output) nil)))
            (should (eq rc 0))
            (should
             (equal
              (nelisp-phase47-build-test--read-output output)
              (unibyte-string ?a ?r ?g ?c ?= ?1 ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(ert-deftest nelisp-phase47-build-argc-printer-three-args ()
  "argc-printer with 3 trailing args → stdout = `argc=4\\n'."
  (skip-unless (nelisp-phase47-build-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-phase47-argc4-"))
        (output (make-temp-file "nelisp-phase47-argc4-out-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-argc-printer path)
          (let ((rc (call-process path nil (list :file output) nil
                                  "a" "b" "c")))
            (should (eq rc 0))
            (should
             (equal
              (nelisp-phase47-build-test--read-output output)
              (unibyte-string ?a ?r ?g ?c ?= ?4 ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

;; ---------------------------------------------------------------- §4 cross-arch

(ert-deftest nelisp-phase47-build-aarch64-magic ()
  "hello-world with ARCH='aarch64 emits e_machine = EM_AARCH64 (183)."
  (let ((path (make-temp-file "nelisp-phase47-arm64-")))
    (unwind-protect
        (progn
          (nelisp-phase47-build-hello-world path :arch 'aarch64)
          (let ((bytes (nelisp-phase47-build-test--read-bytes path)))
            (should (>= (length bytes) 64))
            ;; ELF magic still 7f 45 4c 46.
            (should (= (aref bytes 0) #x7f))
            (should (= (aref bytes 1) ?E))
            ;; e_machine at offset 18 = EM_AARCH64 (183).
            (should (= (nelisp-phase47-build-test--read-le16 bytes 18)
                       183))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- §5 error gates

(ert-deftest nelisp-phase47-build-unknown-arch-signals ()
  "Unknown ARCH (= 'sparc) raises `nelisp-phase47-build-error'."
  (let ((path (make-temp-file "nelisp-phase47-bad-arch-")))
    (unwind-protect
        (should-error
         (nelisp-phase47-build-hello-world path :arch 'sparc)
         :type 'nelisp-phase47-build-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-build-program-asm-fn-prebuilt ()
  "`build-program' accepts an ASM-FN returning a `:prebuilt' plist."
  (let* ((path (make-temp-file "nelisp-phase47-prebuilt-"))
         ;; Minimal 7-byte exit(0) inline (= same as exit-with-status).
         (text (concat
                (unibyte-string #xbf #x00 #x00 #x00 #x00)  ; mov edi, 0
                (unibyte-string #xb8 #x3c #x00 #x00 #x00)  ; mov eax, 60
                (unibyte-string #x0f #x05)))               ; syscall
         (asm-fn (lambda ()
                   (list :units :prebuilt
                         :text text
                         :rodata (unibyte-string)
                         :symbols (list (list :name "_start" :value 0
                                              :size (length text)
                                              :section 'text
                                              :bind 'global
                                              :type 'func))))))
    (unwind-protect
        (progn
          (nelisp-phase47-build-program asm-fn path)
          (let ((bytes (nelisp-phase47-build-test--read-bytes path)))
            (should (= (aref bytes 0) #x7f))
            (should (= (aref bytes 1) ?E)))
          (when (nelisp-phase47-build-test--x86_64-host-p)
            (should (eq (call-process path nil nil nil) 0))))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-phase47-build-test)

;;; nelisp-phase47-build-test.el ends here
