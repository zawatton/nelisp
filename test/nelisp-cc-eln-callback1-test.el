;;; nelisp-cc-eln-callback1-test.el --- unary callback entry checks -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'seq)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-eln-callback7)

(defun nelisp-cc-eln-callback1-test--wrapper-form ()
  (seq-find (lambda (form)
              (eq (cadr form) 'nelisp_eln_callback1_entry_word))
            nelisp-cc-eln-callback7--source))

(defun nelisp-cc-eln-callback1-test--compiled-unit ()
  (nelisp-aot-compile-to-link-unit
   (cons 'seq
         (list '(defun nelisp_eln_callback7_entry_word
                    (arg0 arg1 arg2 arg3 arg4 arg5 arg6)
                  arg0)
               (nelisp-cc-eln-callback1-test--wrapper-form)))
   :arch 'x86_64 :format 'elf))

(ert-deftest nelisp-cc-eln-callback1/one-argument-is-forwarded-and-rest-zeroed ()
  (let* ((form (nelisp-cc-eln-callback1-test--wrapper-form))
         (args (cdr (nth 3 form))))
    (should form)
    (should (equal (nth 2 form) '(raw0)))
    (should (eq (car (nth 3 form)) 'nelisp_eln_callback7_entry_word))
    (should (equal (cdr (nth 3 form)) '(raw0 0 0 0 0 0 0)))
    (should (= (length args) 7))))

(ert-deftest nelisp-cc-eln-callback1/aot-emits-unary-wrapper-call ()
  (let* ((unit (nelisp-cc-eln-callback1-test--compiled-unit))
         (symbols (mapcar (lambda (item) (plist-get item :name))
                          (plist-get unit :symbols)))
         (defuns (plist-get unit :defuns))
         (wrapper (seq-find (lambda (item)
                              (equal (plist-get item :name)
                                     "nelisp_eln_callback1_entry_word"))
                            defuns)))
    (should (> (string-bytes (plist-get unit :text)) 0))
    (should (member "nelisp_eln_callback1_entry_word" symbols))
    (should wrapper)
    (should (> (plist-get wrapper :size) 0))))

(provide 'nelisp-cc-eln-callback1-test)
;;; nelisp-cc-eln-callback1-test.el ends here
