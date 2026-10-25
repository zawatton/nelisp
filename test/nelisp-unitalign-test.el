;;; nelisp-unitalign-test.el --- hot entries are absolutely 64-aligned  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The generator aligns hot entries (:entry 64) relative to their unit and pads
;; unit ends to 64.  These tests assert that the linkers (dynamic ELF, static
;; ELF, Win64 PE) put every unit start, and so every hot entry, on an absolute
;; 64-byte boundary, and that the link-time check fails when one does not.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-static-linker)
(require 'nelisp-elf-write)
(require 'nelisp-pe-write)

(defun nelisp-unitalign-test--units ()
  "Three units whose sizes are not multiples of 64, hot symbol in the last."
  (list (nelisp-link-unit-make "data.o" (list (cons 'data "xyz")) nil nil)
        (nelisp-link-unit-make
         "a.o" (list (cons 'text (make-string 14 #x90)))
         (list (nelisp-link-symbol "_start" 0)) nil)
        (nelisp-link-unit-make
         "b.o" (list (cons 'text (make-string 37 #x90)))
         (list (nelisp-link-symbol "wf_bytecode_execute_frame" 0)) nil)
        (nelisp-link-unit-make
         "c.o" (list (cons 'text (make-string 21 #x90)))
         (list (nelisp-link-symbol "wf_bytecode_w_fast_run" 0)) nil)
        (nelisp-link-unit-make
         "d.o" (list (cons 'text (make-string 5 #x90)))
         (list (nelisp-link-symbol "wf_bytecode_w_function" 0)) nil)))

(ert-deftest nelisp-unitalign-dynamic-text-start ()
  "The dynamic ELF text start is 0 mod 64 for any import count and text size."
  (dolist (imports '(nil (("libc.so.6" . "a"))
                     (("libc.so.6" . "a") ("libc.so.6" . "bb"))
                     (("libc.so.6" . "a") ("libc.so.6" . "bb") ("libm.so.6" . "ccc"))))
    (dolist (size '(1 14 4097))
      (let ((lay (nelisp-elf--dynamic-layout size imports)))
        (should (zerop (mod (plist-get lay :text-vaddr) 64)))
        (should (zerop (mod (plist-get lay :text-off) 64)))))))

(ert-deftest nelisp-unitalign-static-and-pe-layout ()
  "Static and PE layouts put every unit start on a 64-byte boundary."
  (let* ((units (nelisp-unitalign-test--units))
         (combined (nelisp-link-combine-sections units 'x86_64))
         (offs (plist-get (cdr (assq 'text combined)) :offsets)))
    (dolist (cell offs) (should (zerop (mod (cdr cell) 64))))
    (let ((lay (nelisp-link--compute-layout
                combined nil nil nelisp-link-x86-text-alignment)))
      (should (zerop (mod (cdr (assq 'text lay)) 64)))
      (nelisp-link--check-hot-alignment units combined lay nil 'x86_64))
    (let ((lay (nelisp-link--pe-layout combined nil)))
      (should (zerop (mod (cdr (assq 'text lay)) 64)))
      (nelisp-link--check-hot-alignment units combined lay nil 'x86_64))))

(ert-deftest nelisp-unitalign-check-negative-control ()
  "With 16-byte unit alignment the check signals (a perturbed unit start)."
  (cl-letf (((symbol-function 'nelisp-link--unit-section-alignment)
             (lambda (_s _m) 16)))
    (let* ((units (nelisp-unitalign-test--units))
           (combined (nelisp-link-combine-sections units 'x86_64))
           (lay (nelisp-link--compute-layout combined nil nil 16)))
      (should (memq 'nelisp-link-error
                    (get (car (should-error
                               (nelisp-link--check-hot-alignment
                                units combined lay nil 'x86_64)))
                         'error-conditions))))))

(ert-deftest nelisp-unitalign-check-hot-symbol ()
  "A named hot entry at a non-64 address is rejected."
  (let* ((units (nelisp-unitalign-test--units))
         (combined (nelisp-link-combine-sections units 'x86_64))
         (lay (nelisp-link--compute-layout
               combined nil nil nelisp-link-x86-text-alignment))
         (symtab (nelisp-link-symtab-make)))
    (nelisp-link-symtab-add
     symtab (nelisp-link-symbol "wf_bytecode_w_fast_run"
                                (+ (cdr (assq 'text lay)) 48)))
    (should-error (nelisp-link--check-hot-alignment units combined lay symtab 'x86_64))
    (let ((ok (nelisp-link-symtab-make)))
      (nelisp-link-symtab-add
       ok (nelisp-link-symbol "wf_bytecode_w_fast_run"
                              (+ (cdr (assq 'text lay)) 128)))
      (nelisp-link--check-hot-alignment units combined lay ok 'x86_64))))

(ert-deftest nelisp-unitalign-dynamic-end-to-end ()
  "A linked dynamic ELF has its hot symbols at addresses 0 mod 64."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "unitalign-")))
    (unwind-protect
        (progn
          (nelisp-link-units-dynamic path (nelisp-unitalign-test--units)
                                     '(("libc.so.6" . "toupper")
                                       ("libc.so.6" . "tolower")
                                       ("libc.so.6" . "abs")))
          (let ((out (shell-command-to-string
                      (format "readelf -sW %s" (shell-quote-argument path))))
                (seen 0))
            (dolist (name nelisp-link-hot-entry-symbols)
              (when (string-match
                     (format "\\([0-9a-f]+\\) +[0-9]+ +FUNC.* %s$" name) out)
                (setq seen (1+ seen))
                (should (zerop (mod (string-to-number (match-string 1 out) 16)
                                    64)))))
            (should (>= seen 2))))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-unitalign-test)
;;; nelisp-unitalign-test.el ends here
