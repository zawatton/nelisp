;;; nelisp-cc-eln-callback7-test.el --- seven-word callback AOT checks -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-rootstack)
(require 'nelisp-cc-eln-callback)
(require 'nelisp-cc-eln-callback7)
(require 'nelisp-native-load)
(let ((scripts (expand-file-name "../scripts"
                                 (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path scripts))
(require 'nelisp-standalone-build)

(defun nelisp-cc-eln-callback7-test--compiled-unit ()
  (nelisp-aot-compile-to-link-unit
   (cons 'seq
         (append (cdr nelisp-cc-rootstack--source)
                 '((defun wf_bytecode_call_gateway (env function slots first argc out)
                     0)
                   (defun wf_write_int (out value) 0))
                 nelisp-cc-eln-callback--source
                 nelisp-cc-eln-callback7--source))
   :arch 'x86_64 :format 'elf))

(ert-deftest nelisp-cc-eln-callback7/seven-raw-args-and-nested-frame-shape ()
  (let* ((forms nelisp-cc-eln-callback7--source)
         (names (mapcar #'cadr forms))
         (entry (seq-find (lambda (form)
                           (eq (cadr form) 'nelisp_eln_callback7_entry))
                         forms)))
    (should (= nelisp-cc-eln-callback7-capacity 8))
    (should (= nelisp-cc-eln-callback7-record-bytes 80))
    (should (= nelisp-cc-eln-callback7-bss-bytes 672))
    (should (= (length (nth 2 entry)) 7))
    (should (memq 'wf_bytecode_call_gateway
                  (nelisp-cc-eln-callback7-test--symbols-used entry)))
    (should (memq 'nl_eln_callback_context
                  (nelisp-cc-eln-callback7-test--symbols-used entry)))
    (should (member "nelisp_eln_callback7_entry"
                    nelisp-native-load-bridgeable-symbols))
    (should (member "nelisp_eln_callback7_entry_word"
                    nelisp-native-load-bridgeable-symbols))
    (should (member "nl_eln_callback7_context"
                    nelisp-native-load-bridgeable-symbols))
    (should names)))

(ert-deftest nelisp-cc-eln-callback7/doc210-resume-block-and-divert-shape ()
  ;; Doc 210 S9: the resume block follows the records; the Lisp side
  ;; (`nelisp-eln-handler-port--resume-offset') hardcodes 672.
  (should (= nelisp-cc-eln-callback7-resume-offset 672))
  (should (= nelisp-cc-eln-callback7-resume-bytes 64))
  (should (= nelisp-cc-eln-callback7-total-bss-bytes 736))
  (let* ((forms nelisp-cc-eln-callback7--source)
         (divert (seq-find (lambda (form)
                             (eq (cadr form) 'nl_eln_callback7_divert_ok))
                           forms))
         (entry-word (seq-find (lambda (form)
                                 (eq (cadr form) 'nelisp_eln_callback7_entry_word))
                               forms))
         (used (nelisp-cc-eln-callback7-test--symbols-used entry-word)))
    (should divert)
    ;; the divert is an indirect call after the finish, validated by the
    ;; adapter's own check, and the entry publishes its stack pointer
    (should (memq 'call-ptr used))
    (should (memq 'aot-current-sp used))
    (should (memq 'nl_eln_callback7_divert_ok used))
    (should (memq 'logand (nelisp-cc-eln-callback7-test--symbols-used divert)))))

(defun nelisp-cc-eln-callback7-test--symbols-used (form)
  (let ((symbols nil))
    (cl-labels ((walk (node)
                  (cond ((symbolp node) (push node symbols))
                        ((consp node) (walk (car node)) (walk (cdr node))))))
      (walk form))
    (delete-dups symbols)))

(ert-deftest nelisp-cc-eln-callback7/source-compiles-as-seven-arg-x86-64-aot ()
  (let* ((unit (nelisp-cc-eln-callback7-test--compiled-unit))
         (symbols (mapcar (lambda (item) (plist-get item :name))
                          (plist-get unit :symbols))))
    (should (> (string-bytes (plist-get unit :text)) 0))
    (should (member "nelisp_eln_callback7_entry" symbols))
    (should (member "nelisp_eln_callback7_entry_word" symbols))
    (should (member "nelisp_eln_callback7_status" symbols))
    (should (member "nl_eln_callback7_context" (plist-get unit :extern-symbols)))
    (should (member "nl_eln_callback_context" (plist-get unit :extern-symbols)))
    (should (member "wf_bytecode_call_gateway" symbols))))

(ert-deftest nelisp-cc-eln-callback7/bridge-lists-are-paired-and-append-only ()
  (should (equal (cl-subseq nelisp-native-load-bridgeable-symbols 24 29)
                 '("nelisp_eln_callback7_entry"
                   "nelisp_eln_callback7_status"
                   "nelisp_eln_callback7_root_mark"
                   "nl_eln_callback7_context"
                   "nelisp_eln_callback7_entry_word")))
  (should (equal nelisp-native-load-bridgeable-symbols
                 nelisp-standalone--reader-neln-bridgeable-symbols)))

(provide 'nelisp-cc-eln-callback7-test)

;;; nelisp-cc-eln-callback7-test.el ends here
