;;; nelisp-asm-wasm-test.el --- tests for wasm encoder/writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'ert)
(require 'nelisp-asm-wasm)
(require 'nelisp-aot-compiler)
(require 'nelisp-wasm-write)

(defun nelisp-asm-wasm-test--decode-uleb128 (bytes)
  "Decode unsigned LEB128 BYTES."
  (let ((result 0)
        (shift 0)
        (i 0)
        done)
    (while (not done)
      (let ((byte (aref bytes i)))
        (setq result (logior result (ash (logand byte #x7f) shift))
              shift (+ shift 7)
              i (1+ i)
              done (zerop (logand byte #x80)))))
    result))

(defun nelisp-asm-wasm-test--decode-sleb128 (bytes)
  "Decode signed LEB128 BYTES."
  (let ((result 0)
        (shift 0)
        (size 64)
        (i 0)
        byte)
    (while (progn
             (setq byte (aref bytes i))
             (setq result (logior result (ash (logand byte #x7f) shift))
                   shift (+ shift 7)
                   i (1+ i))
             (not (zerop (logand byte #x80))))
      nil)
    (when (and (< shift size) (not (zerop (logand byte #x40))))
      (setq result (logior result (ash -1 shift))))
    result))

(ert-deftest nelisp-asm-wasm/uleb128-round-trip ()
  (dolist (value '(0 63 64 127 128 16383 16384 2147483647))
    (should
     (= value
        (nelisp-asm-wasm-test--decode-uleb128
         (nelisp-asm-wasm-uleb128-bytes value))))))

(ert-deftest nelisp-asm-wasm/sleb128-round-trip ()
  (dolist (value '(0 -1 1 63 64 127 128 -64 -65 -127 -128 -129))
    (should
     (= value
        (nelisp-asm-wasm-test--decode-sleb128
         (nelisp-asm-wasm-sleb128-bytes value))))))

(ert-deftest nelisp-asm-wasm/op-call-indirect-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-call-indirect buf 3 0)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string #x11 #x03 #x00)))))

(ert-deftest nelisp-asm-wasm/op-structured-control-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-block buf)
    (nelisp-asm-wasm-op-loop buf)
    (nelisp-asm-wasm-op-br-if buf 1)
    (nelisp-asm-wasm-op-br buf 0)
    (nelisp-asm-wasm-op-else buf)
    (nelisp-asm-wasm-op-end buf)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string
                    #x02 #x40 #x03 #x40 #x0d #x01 #x0c #x00 #x05 #x0b)))))

(ert-deftest nelisp-asm-wasm/op-memory-wrap-and-load-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-i32-wrap-i64 buf)
    (nelisp-asm-wasm-op-i64-load32-u buf)
    (nelisp-asm-wasm-op-i64-extend-i32-u buf)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string #xa7 #x35 #x02 #x00 #xad)))))

(ert-deftest nelisp-asm-wasm/op-legacy-eh-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-try buf nelisp-asm-wasm--i64)
    (nelisp-asm-wasm-op-catch buf 0)
    (nelisp-asm-wasm-op-catch-all buf)
    (nelisp-asm-wasm-op-rethrow buf 0)
    (nelisp-asm-wasm-op-throw buf 0)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string #x06 #x7e #x07 #x00 #x19 #x09 #x00 #x08 #x00)))))

(ert-deftest nelisp-asm-wasm/op-i32-global-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-i32-const buf 1024)
    (nelisp-asm-wasm-op-i32-load buf)
    (nelisp-asm-wasm-op-i32-store buf)
    (nelisp-asm-wasm-op-global-get buf 1)
    (nelisp-asm-wasm-op-global-set buf 2)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string
                    #x41 #x80 #x08 #x28 #x02 #x00 #x36 #x02 #x00 #x23 #x01 #x24 #x02)))))

(ert-deftest nelisp-asm-wasm/op-p3-memory-and-i32-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-memory-size buf)
    (nelisp-asm-wasm-op-memory-grow buf)
    (nelisp-asm-wasm-op-i32-eq buf)
    (nelisp-asm-wasm-op-i32-gt-u buf)
    (nelisp-asm-wasm-op-i32-add buf)
    (nelisp-asm-wasm-op-i32-sub buf)
    (nelisp-asm-wasm-op-i32-and buf)
    (nelisp-asm-wasm-op-i32-shl buf)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string
                    #x3f #x00 #x40 #x00 #x46 #x4b #x6a #x6b #x71 #x74)))))

(ert-deftest nelisp-asm-wasm/op-f64-sqrt-bits-round-trip-encodes ()
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-op-f64-reinterpret-i64 buf)
    (nelisp-asm-wasm-op-f64-sqrt buf)
    (nelisp-asm-wasm-op-i64-reinterpret-f64 buf)
    (should (equal (nelisp-asm-wasm-buffer-bytes buf)
                   (unibyte-string #xbf #x9f #xbd)))))

(ert-deftest nelisp-asm-wasm/writer-emits-table-and-element-sections ()
  (let ((path (make-temp-file "nelisp-wasm-" nil ".wasm")))
    (unwind-protect
        (progn
          (nelisp-wasm-write-binary
           path
           (list :wasm-types (list (list :params nil :results (list #x7e)))
                 :wasm-table-size 1
                 :wasm-element-indices '(0)
                 :wasm-functions
                 (list (list :name "f"
                             :type-index 0
                             :body (nelisp-asm-wasm-make-function-body
                                    nil
                                    (unibyte-string #x42 #x00 #x0f #x0b))))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (goto-char 1)
            (should (search-forward (string #x04) nil t))
            (goto-char 1)
            (should (search-forward (string #x09) nil t))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-asm-wasm/writer-emits-import-tag-and-data-sections ()
  (let ((path (make-temp-file "nelisp-wasm-p2-" nil ".wasm")))
    (unwind-protect
        (progn
          (nelisp-wasm-write-binary
           path
           (list :wasm-types
                 (list (list :params nil :results (list #x7e))
                       (list :params (list #x7e #x7e) :results nil))
                 :wasm-imports
                 (list (list :module "env" :field "ext_add" :type-index 0))
                 :wasm-tag-type-index 1
                 :wasm-exports
                 (list (list :name "memory" :kind nelisp-asm-wasm--mem-kind :index 0)
                       (list :name "f" :kind nelisp-asm-wasm--extern-func-kind :index 1))
                 :wasm-data
                 (list (list :addr 1024 :bytes (unibyte-string ?h ?i)))
                 :wasm-functions
                 (list (list :name "f"
                             :type-index 0
                             :body (nelisp-asm-wasm-make-function-body
                                    nil
                                    (unibyte-string #x42 #x00 #x0f #x0b))))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (goto-char 1)
            (should (search-forward (string #x02) nil t))
            (goto-char 1)
            (should (search-forward (string #x0d) nil t))
            (goto-char 1)
            (should (search-forward (string #x0b) nil t))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-asm-wasm/writer-emits-global-section-and-memory-max ()
  (let ((path (make-temp-file "nelisp-wasm-p3-" nil ".wasm")))
    (unwind-protect
        (progn
          (nelisp-wasm-write-binary
           path
           (list :wasm-types (list (list :params nil :results (list #x7e)))
                 :wasm-mem-min 2
                 :wasm-mem-max 4
                 :wasm-globals
                 (list (list :type nelisp-aot-compiler--wasm-i32-type
                             :mut t
                             :init-i32 66000))
                 :wasm-exports
                 (list (list :name "memory" :kind nelisp-asm-wasm--mem-kind :index 0)
                       (list :name "heap_ptr" :kind nelisp-asm-wasm--global-kind :index 0)
                       (list :name "f" :kind nelisp-asm-wasm--extern-func-kind :index 0))
                 :wasm-functions
                 (list (list :name "f"
                             :type-index 0
                             :body (nelisp-asm-wasm-make-function-body
                                    nil
                                    (unibyte-string #x42 #x00 #x0f #x0b))))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (goto-char 1)
            (should (search-forward (string #x06) nil t))
            (goto-char 1)
            ;; Memory section layout: id=5, size=4, count=1, flags=1, min=2, max=4.
            (should (search-forward
                     (unibyte-string #x05 #x04 #x01 #x01 #x02 #x04)
                     nil t))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-asm-wasm/compiler-skips-env-imports-for-module-local-recursion ()
  (let* ((unit
          (nelisp-aot-compile-to-link-unit
           '(seq
             (defun fact (n)
               (if (< n 2)
                   1
                 (* n (fact (- n 1)))))
             (defun addx (a b)
               (+ (extern-call ext_add a b)
                  (fact 3))))
           :arch 'wasm32 :format 'wasm))
         (imports (plist-get unit :wasm-imports))
         (functions (plist-get unit :wasm-functions))
         (exports (plist-get unit :wasm-exports)))
    (should (equal (mapcar (lambda (import)
                             (list (plist-get import :module)
                                   (plist-get import :field)))
                           imports)
                   '(("env" "ext_add"))))
    (should (equal (mapcar (lambda (fn) (plist-get fn :name)) functions)
                   '("fact" "addx")))
    (should (equal (plist-get unit :wasm-element-indices) '(1 2)))
    (should (equal (mapcar (lambda (export) (plist-get export :index)) exports)
                   '(0 1 2)))))

(provide 'nelisp-asm-wasm-test)

;;; nelisp-asm-wasm-test.el ends here
