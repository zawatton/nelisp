;;; nelisp-abi-generator-guard-test.el --- Reject legacy result generation -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-standalone-build)

(ert-deftest nelisp-abi-generator-rejects-unsplit-box-result ()
  (should-error
   (nelisp-standalone--abi-box-split-defun
    '(defun nl_sf_dolist (args env out) (legacy-result args env out)) nil)))

(ert-deftest nelisp-abi-generator-rejects-legacy-builtin-arm ()
  (should-error (nelisp-standalone--applyfn-word-arm '(wf_write_int out 1))))

(ert-deftest nelisp-abi-generator-rejects-legacy-special-tail ()
  (should-error (nelisp-standalone--special-tail-words '(legacy-result out) 'out)))

(ert-deftest nelisp-abi-generator-retains-a-direct-scalar-word ()
  (should (equal
           (nelisp-standalone--special-tail-words
            '(nl_abi_word_to_rc (nl_imm_int (sar word 2)) out) 'out)
           '(nl_imm_int (sar word 2)))))

(ert-deftest nelisp-abi-generator-base-car-cdr-use-word-producers ()
  (dolist (entry '(("car" bf_car_w) ("cdr" bf_cdr_w)))
    (let ((arm (assoc (list :u8 (car entry)) nelisp-standalone--applyfn-dispatch-table)))
      (should (equal (cdr arm) (cons :word (list (cadr entry) 'args 0)))))))

(ert-deftest nelisp-abi-generator-platform-arms-have-word-results ()
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (target '("linux-x86_64" "windows-x86_64"))
      (dolist (dynamic '(nil "1"))
        (setenv "NELISP_READER_DYNAMIC" dynamic)
        (let ((nelisp-standalone--target (intern target)))
          (dolist (entry (nelisp-standalone--applyfn-reader-table))
            (should (eq (car-safe (cdr entry)) :word))))))))

(ert-deftest nelisp-abi-generator-windows-producers-do-not-retain-out ()
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (names '(nl_bi_process_windows_make_object_w
                  nl_bi_process_windows_start_w nl_bi_process_windows_read_output_w
                  nl_bi_process_windows_write_w nl_bi_process_windows_close_stdin_w
                  nl_bi_process_windows_poll_w nl_tls_plain_deliver_w))
         (forms (append (nelisp-standalone--fileio-process-async-forms)
                        (nelisp-standalone--tls-forms))))
    (dolist (name nelisp-standalone--tls-word-producers)
      (push (intern (format "%s_w" name)) names))
    (cl-labels ((forbidden-p (form)
                 (if (consp form) (cl-some #'forbidden-p form)
                   (memq form '(out nl_val_store_word nl_abi_rc_to_word
                                   nl_abi_rc2_to_word)))))
      (dolist (name names)
        (let ((form (cl-find name forms :key #'cadr)))
          (should form)
          (should-not (forbidden-p (nthcdr 3 form))))))))

(ert-deftest nelisp-abi-generator-schannnel-rejects-unconverted-results ()
  (should-error
   (nelisp-standalone--tls-word-defun
    '(defun nl_tls_send_impl (args out) (legacy-result args out))))
  (should-error
   (nelisp-standalone--tls-word-defun
    '(defun nl_tls_send_impl (args out)
       (if (legacy-condition out) (seq (wf_write_int out 1) 0) 1)))))

(provide 'nelisp-abi-generator-guard-test)
;;; nelisp-abi-generator-guard-test.el ends here
