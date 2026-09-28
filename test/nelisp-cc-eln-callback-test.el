;;; nelisp-cc-eln-callback-test.el --- callback AOT source checks -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-rootstack)
(require 'nelisp-cc-eln-callback)
(require 'nelisp-native-load)
(let ((scripts (expand-file-name
                "../scripts"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path scripts))
(require 'nelisp-standalone-build)

(defconst nelisp-cc-eln-callback-test--entries
  '(nl_eln_callback_record_at
    nl_eln_callback_lock
    nl_eln_callback_unlock
    nl_eln_callback_slot_ok
    nelisp_eln_callback_context_push
    nelisp_eln_callback_context_status
    nelisp_eln_callback_context_pop
    nelisp_eln_fixnum1_callback))

(defun nelisp-cc-eln-callback-test--compiled-unit ()
  "Compile the callback source with the root pin checker and gateway shape."
  (nelisp-aot-compile-to-link-unit
   (cons 'seq
         (append
          (cdr nelisp-cc-rootstack--source)
          ;; The real gateway is in the same standalone unit.  A signature
          ;; stub lets this test verify the raw function's actual AOT lowering.
          '((defun wf_bytecode_call_gateway (env function slots first argc out)
              0))
          nelisp-cc-eln-callback--source))
   :arch 'x86_64 :format 'elf))

(ert-deftest nelisp-cc-eln-callback/source-has-bounded-rooted-context-abi ()
  (should (= nelisp-cc-eln-callback-context-capacity 8))
  (should (= nelisp-cc-eln-callback-context-header-bytes 32))
  (should (= nelisp-cc-eln-callback-context-record-bytes 56))
  (should (= (+ nelisp-cc-eln-callback-context-header-bytes
                (* nelisp-cc-eln-callback-context-capacity
                   nelisp-cc-eln-callback-context-record-bytes))
             480))
  (should (= nelisp-cc-eln-callback-context-bss-bytes 480))
  (should (equal (car nelisp-cc-eln-callback--source)
                 `(defun nl_eln_callback_record_at
                      (control index)
                    (+ control ,nelisp-cc-eln-callback-context-header-bytes
                       (* index ,nelisp-cc-eln-callback-context-record-bytes)))))
  (let ((names (mapcar (lambda (form) (cadr form))
                       nelisp-cc-eln-callback--source)))
    (dolist (name nelisp-cc-eln-callback-test--entries)
      (should (memq name names)))
    (should (memq 'nl_root_pin_slot_active
                  (nelisp-cc-eln-callback-test--symbols-used
                   'nl_eln_callback_slot_ok)))
    (should (memq 'wf_bytecode_call_gateway
                  (nelisp-cc-eln-callback-test--symbols-used
                   'nelisp_eln_fixnum1_callback)))
    (dolist (name '(nelisp_eln_callback_context_push
                    nelisp_eln_callback_context_status
                    nelisp_eln_callback_context_pop
                    nelisp_eln_fixnum1_callback))
      (should (memq 'nl_eln_callback_record_at
                    (nelisp-cc-eln-callback-test--symbols-used name))))))

(defun nelisp-cc-eln-callback-test--symbols-used (name)
  (let ((form (seq-find (lambda (candidate) (eq (cadr candidate) name))
                        nelisp-cc-eln-callback--source))
        (symbols nil))
    (cl-labels ((walk (node)
                  (cond ((symbolp node) (push node symbols))
                        ((consp node) (walk (car node)) (walk (cdr node))))))
      (walk form))
    (delete-dups symbols)))

(ert-deftest nelisp-cc-eln-callback/source-compiles-as-real-x86-64-aot ()
  (let* ((unit (nelisp-cc-eln-callback-test--compiled-unit))
         (symbols (mapcar (lambda (item) (plist-get item :name))
                          (plist-get unit :symbols))))
    (should (> (string-bytes (plist-get unit :text)) 0))
    (dolist (name nelisp-cc-eln-callback-test--entries)
      (should (member (symbol-name name) symbols)))
    (should (member "nl_eln_callback_context"
                    (plist-get unit :extern-symbols)))
    (should (member "nl_root_pin_control"
                    (plist-get unit :extern-symbols)))
    (should (member "wf_bytecode_call_gateway" symbols))
    (should-not (member "nelisp_aot_builtin_call1"
                         (plist-get unit :extern-symbols)))))

(ert-deftest nelisp-cc-eln-callback/bridge-names-append-with-stable-indices ()
  (should (equal (cl-subseq nelisp-native-load-bridgeable-symbols 19 24)
                 '("nelisp_eln_callback_context_push"
                   "nelisp_eln_callback_context_status"
                   "nelisp_eln_callback_context_pop"
                   "nelisp_eln_fixnum1_callback"
                   "nl_eln_callback_context")))
  (should (equal nelisp-native-load-bridgeable-symbols
                 nelisp-standalone--reader-neln-bridgeable-symbols)))

(provide 'nelisp-cc-eln-callback-test)

;;; nelisp-cc-eln-callback-test.el ends here
