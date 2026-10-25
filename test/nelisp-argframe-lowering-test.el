;;; nelisp-argframe-lowering-test.el --- Structural argv admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-standalone-argframe)

(ert-deftest nelisp-argframe-lowers-only-literal-index-reads ()
  (should
   (equal (nelisp-argframe-lower
           '(seq (wf_arg_word args 0) (wf_argval args 2)
                 (m5_list_len args 0)) 'argv 'argc)
          '(seq (if (< 0 argc) (heap-read-u64 argv 0) 0)
                (nl_word_payload (if (< 2 argc) (heap-read-u64 argv 16) 0))
                argc))))

(ert-deftest nelisp-argframe-rejects-escape-mutation-and-unknown-uses ()
  (dolist (form '((identity args) (nl_cons_cdr_word args)
                  (ptr-write-u64 args 0 3) (wf_arg_ptr args 0)
                  (wf_arg_word args index) (wf_arg_word args -1)
                  (let ((args 3)) (wf_arg_word args 0))
                  (quote args) (m5_list_len args 1)))
    (should-error (nelisp-argframe-lower form 'argv 'argc)
                  :type 'nelisp-argframe-ineligible)))

(ert-deftest nelisp-argframe-does-not-confuse-omitted-and-explicit-nil ()
  (should (equal (nelisp-argframe-lower '(wf_arg_word args 8) 'argv 'argc)
                 '(if (< 8 argc) (heap-read-u64 argv 64) 0))))

(ert-deftest nelisp-argframe-preserves-arbitrary-code-without-args ()
  (let ((body '(let* ((v (callback env))) (seq (alloc-bytes 32 8) v))))
    (should (equal (nelisp-argframe-lower body 'argv 'argc) body))))

(ert-deftest nelisp-argframe-mode-keeps-original-accessor-and-one-body ()
  (let* ((body '(seq (callback env) (wf_argval args 1)))
         (lowered (nelisp-argframe-lower body 'args '(- _pad 1) '_pad)))
    (should (equal lowered
                   '(seq (callback env)
                         (if (= _pad 0) (wf_argval args 1)
                           (nl_word_payload
                            (if (< 1 (- _pad 1)) (heap-read-u64 args 8) 0))))))
    (should-error (nelisp-argframe-lower
                   '(let ((_pad 9)) (wf_arg_word args 0)) 'args '(- _pad 1) '_pad)
                  :type 'nelisp-argframe-ineligible)))

(ert-deftest nelisp-argframe-mode-rejects-before-ineligible-effects ()
  (let ((table '(((:lit "escape") :word seq (callback env) (escape args))
                 ((:lit "fixed") :word wf_arg_word args 0))))
    (should (equal (nelisp-argframe-admitted-names table) '("fixed")))
    (should (equal (car (nelisp-argframe-mode-table table))
                   '((:lit "escape") :word if (= _pad 0)
                     (seq (callback env) (escape args)) 2)))))

(provide 'nelisp-argframe-lowering-test)
;;; nelisp-argframe-lowering-test.el ends here
