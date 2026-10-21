;;; nelisp-cc-evalport-aot-apply.el --- native funcall/apply providers -*- lexical-binding: t; -*-

;;; Commentary:

;; The AOT compiler lowers `funcall' and `apply' in a native artifact to the
;; Doc 129.7 entries
;;
;;   nelisp_aot_funcall1(mirror, frames, fn, arg, out, scratch)
;;   nelisp_aot_funcall2(mirror, frames, fn, arg0, arg1, out)
;;   nelisp_aot_funcall3(mirror, frames, fn, arg0, arg1, arg2, out)
;;   nelisp_aot_funcalln(mirror, frames, fn, argc, out, scratch, arg...)
;;   nelisp_aot_apply(mirror, frames, fn, args_list, out, scratch)
;;   nelisp_aot_applyn(mirror, frames, fn, argc, out, scratch, arg...)
;;
;; which only the host runtime (src/nelisp-cc-runtime.el) defined.  The
;; reader had none of them, so the native loader refused every artifact
;; holding a `funcall' or an `apply' ("unbridgeable") -- Doc 229, item 1.
;;
;; These providers are GNU's Ffuncall and Fapply (eval.c): they hand the
;; argument list (FN ARG...) to the evaluator's own `funcall' and `apply'
;; (`nl_apply_do_funcall_w', `nl_apply_do_apply_w'), so a symbol, a
;; builtin, a lambda and a closure are applied exactly as the interpreter
;; applies them, `apply' spreading its last argument.  The argument list
;; is built as the calln provider builds its own (one cons slot per
;; position under an `argc' if-chain), and a signal travels through the
;; result word as it does there.

;;; Code:

(require 'nelisp-cc-evalport-aot-builtin-calln)

(defun nelisp-cc-evalport-aot-apply--provider (name evaluator)
  "Return the word-ABI provider NAME that hands (FN ARG...) to EVALUATOR."
  `(defun ,name
       (mirror frames fn argc _pad scratch
               ,@(nelisp-cc-evalport-aot-builtin-calln--arg-syms))
     (let* ((nil_slot (alloc-bytes 32 8))
            (args_list (alloc-bytes 32 8))
            ,@(let ((slots nil))
                (dotimes (i (1- nelisp-cc-evalport-aot-builtin-calln--max-args))
                  (push `(,(intern (format "c%d" (1+ i))) (alloc-bytes 32 8))
                        slots))
                (nreverse slots))
            (whole (alloc-bytes 32 8)))
       (seq
        ,@(nelisp-cc-evalport-aot-builtin-calln--nil-writes 'nil_slot)
        ,(nelisp-cc-evalport-aot-builtin-calln--dispatch)
        (nelisp_cons_construct fn args_list whole)
        ;; The evaluator's env is the mirror base (MIRROR); FRAMES is
        ;; MIRROR + 32 and the unbound sentinel MIRROR + 64.
        (,evaluator whole mirror 0)))))

(defconst nelisp-cc-evalport-aot-apply--source
  (let ((args (nelisp-cc-evalport-aot-builtin-calln--arg-syms))
        (pad (lambda (n)
               (make-list (- nelisp-cc-evalport-aot-builtin-calln--max-args n) 0))))
    `(seq
      ,(nelisp-cc-evalport-aot-apply--provider
        'nelisp_aot_funcalln_w 'nl_apply_do_funcall_w)
      ,(nelisp-cc-evalport-aot-apply--provider
        'nelisp_aot_applyn_w 'nl_apply_do_apply_w)
      ;; The AOT-emitted callers' ABIs: fill OUT and return OUT.
      (defun nelisp_aot_funcalln (mirror frames fn argc out scratch ,@args)
        (seq (nl_abi_word_to_rc
              (nelisp_aot_funcalln_w mirror frames fn argc 0 scratch ,@args)
              out)
             out))
      (defun nelisp_aot_applyn (mirror frames fn argc out scratch ,@args)
        (seq (nl_abi_word_to_rc
              (nelisp_aot_applyn_w mirror frames fn argc 0 scratch ,@args)
              out)
             out))
      (defun nelisp_aot_funcall1 (mirror frames fn arg out scratch)
        (nelisp_aot_funcalln mirror frames fn 1 out scratch arg ,@(funcall pad 1)))
      (defun nelisp_aot_funcall2 (mirror frames fn arg0 arg1 out)
        (nelisp_aot_funcalln mirror frames fn 2 out 0 arg0 arg1 ,@(funcall pad 2)))
      (defun nelisp_aot_funcall3 (mirror frames fn arg0 arg1 arg2 out)
        (nelisp_aot_funcalln mirror frames fn 3 out 0 arg0 arg1 arg2
                             ,@(funcall pad 3)))
      (defun nelisp_aot_apply (mirror frames fn args_list out scratch)
        (nelisp_aot_applyn mirror frames fn 1 out scratch args_list
                           ,@(funcall pad 1)))))
  "AOT source for the native funcall and apply providers (Doc 229).")

(provide 'nelisp-cc-evalport-aot-apply)

;;; nelisp-cc-evalport-aot-apply.el ends here
