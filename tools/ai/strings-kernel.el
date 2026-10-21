;;; strings-kernel.el --- Compile just the GNU string kernel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; A focused native probe: no prelude or whole-reader rebuild is required.
(require 'nelisp-standalone-build)
(require 'nelisp-artifact)
(require 'nelisp-cc-string-blocks)
(require 'nelisp-cc-sexp-clone-into)

(let* ((root (expand-file-name "../.." (file-name-directory load-file-name)))
       (output (expand-file-name "target/strings/kernel.o" root))
       (nelisp-standalone--inline-import-sources nil)
       (start (float-time))
       (unit (nelisp-standalone--compile-to-unit
              "strings-kernel.o"
              (append nelisp-cc-string-blocks--source
                      (list (cl-find 'nl_val_tag (cdr nelisp-standalone--arena-source)
                                     :key #'cadr))
                      (mapcar (lambda (name)
                                (cl-find name (cdr nelisp-standalone--arena-source) :key #'cadr))
                              '(nl_word_data nl_word_data_legacy))
                      (list (cl-find 'nl_g4arm_copy_w nelisp-standalone--applyfn-core-helpers :key #'cadr))
                      (list (cl-find 'nl_sexp_clone_into (cdr nelisp-cc-sexp-clone-into--source) :key #'cadr))
                      '((defun strings_equal_probe (a b) (str-eq a b))
                        (defun strings_tag_probe (a) (sexp-tag a))
                        (defun strings_name_probe (a) (sexp-name-eq a "abcdefgh"))
                        (defun strings_byte_probe (a i) (str-byte-at a i))
                        (defun strings_length_probe (a) (str-len a)))))))
  (nelisp-elf-write-binary
   output
   (list :e-type 'rel :machine 'x86_64
         :text (cdr (assq 'text (plist-get unit :sections)))
         :symbols (append
                   (plist-get unit :symbols)
                   (let ((known (mapcar (lambda (s) (plist-get s :name))
                                        (plist-get unit :symbols))) missing)
                     (dolist (r (plist-get unit :relocs))
                       (let ((name (plist-get r :symbol)))
                         (unless (member name known)
                           (push name known)
                           (push (nelisp-link-symbol name 0 :section 'undef
                                                     :bind 'global :type 'notype) missing))))
                     missing))
         :relocs (mapcar
                  (lambda (r)
                    (let ((copy (copy-sequence r)))
                      (plist-put copy :addend
                                 (- (or (plist-get r :addend) 0) 4))))
                  (plist-get unit :relocs))))
  (princ (format "string kernel compile %.3fs\n" (- (float-time) start))))
