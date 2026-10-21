;;; nelisp-cc-sf-lambda.el --- WORD-valued lambda construction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Build (closure CAPTURED FORMALS BODY...) with WORD-valued producers.
;; ROOTS is a precise vector root: arguments, capture/tail, and symbol.
;; The compatible nl_sf_lambda entry publishes the result to its caller's OUT.

;;; Code:

(defconst nelisp-cc-sf-lambda--source
  '(seq
    (defun nl_sf_lambda_fill (buf s1)
      (and (ptr-write-u8 buf 0 99)
           (ptr-write-u8 buf 1 108)
           (ptr-write-u8 buf 2 111)
           (ptr-write-u8 buf 3 115)
           (ptr-write-u8 buf 4 117)
           (ptr-write-u8 buf 5 114)
           (ptr-write-u8 buf 6 101)
           (sexp-write-symbol s1 buf 7)
           (dealloc-bytes buf 7 1)))

    (defun nl_sf_lambda_alloc_sym (s1 _pad)
      (nl_sf_lambda_fill (alloc-bytes 7 1) s1))

    (defun nl_sf_lambda_with_sym_w (sym-ok tail sym roots)
      (if sym-ok (nelisp_cons_construct_w sym tail 0) (nl_abi_fail 1 0)))

    (defun nl_sf_lambda_with_tail_w (tail roots)
      (let* ((sym (alloc-bytes 32 8)))
        (ptr-write-u64 roots 8 tail)
        (ptr-write-u64 roots 16 sym)
        (nl_sf_lambda_with_sym_w (nl_sf_lambda_alloc_sym sym 0) tail sym roots)))

    (defun nl_sf_lambda_captured_w (captured args roots)
      (if (= captured 0) 0
        (seq
         (ptr-write-u64 roots 8 captured)
         ;; A nonempty lexical marker distinguishes (closure (t) ...).
         (let* ((capture (if (= captured 3) (nelisp_cons_construct_w 7 3 0) captured)))
           (ptr-write-u64 roots 8 capture)
           (nl_sf_lambda_with_tail_w (nelisp_cons_construct_w capture args 0) roots)))))

    (defun nl_sf_lambda_w (args env _pad s1)
      (if (= (nl_val_tag args) 7)
          ;; Doc 216 S6.2: three precise WORD roots (args, capture/tail,
          ;; the closure symbol) in one root-stack frame.
          (let* ((mark (nl_root_mark env)) (roots (nl_root_frame env 3)))
            (if (= roots 0) (nl_abi_fail 1 0)
              (seq
               (ptr-write-u64 roots 0 args)
               (let* ((word (nl_sf_lambda_captured_w
                             (nl_env_capture_lexical_filtered_w env args 0 0) args roots)))
                 (nl_root_release env mark)
                 word))))
        (nl_abi_fail 1 0)))

    (defun nl_sf_lambda (args env out s1)
      (nl_abi_word_to_rc (nl_sf_lambda_w args env 0 s1) out)))
  "WORD-valued lambda construction, with a single legacy publication boundary.")

(provide 'nelisp-cc-sf-lambda)
;;; nelisp-cc-sf-lambda.el ends here
