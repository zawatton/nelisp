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
;; result word as it does there. Short native Subr calls can use the
;; shared word dispatcher before the provider builds any conses.

;;; Code:

(require 'nelisp-cc-evalport-aot-builtin-calln)

(defconst nelisp-cc-evalport-aot-apply--direct-subr
  '((defun nl_aot_leaf_arg (value)
      ;; Native entry points can supply a legacy immediate view. Read it
      ;; without allocating or taking ownership of the caller's value.
      (if (if (= (logand value 7) 0)
              (if (> value 4096) (= (ptr-read-u8 value 0) 2) 0) 0)
          (let* ((fix (m5_fx_fix (ptr-read-u64 value 8))))
            (if (= fix -1) value fix))
        (nl_word_canon_view value)))
    (defun nl_aot_funcall_subr_w (env fn argc a0 a1 a2)
     ;; The generic provider still owns all closures, autoloads and spread
     ;; arguments. Read the live Subr cell before copying any operands.
     (if (if (< argc 0) 1 (> argc 3)) 2
       (let* ((name (nl_word_canon fn)) (cls (logand name 7))
              (subr (if (= cls 6)
                        (if (< name 4096) name
                          (let* ((kind (logand (ptr-read-u8 (- name 6) 0) 63)))
                            (if (or (= kind 4) (= kind 16))
                                (ptr-read-u64 (- name 6) 24)
                              name)))
                      (if (if (= cls 0) (> name 4096) 0)
                          (let* ((tag (ptr-read-u8 name 0)))
                            (if (or (= tag 4) (= tag 16))
                                (bf_function_cell_w name env)
                              name))
                        name)))
              (id (nl_directcall_id subr name argc)))
         (if (= id 0) 2
           (let* ((leaf (if (if (= id 101) 1
                             (if (<= argc 2)
                                 (or (= id 1) (= id 10) (= id 88) (= id 109)
                                     (and (>= id 94) (<= id 100))) 0))
                           (if (= (m5_fx_leaf_allowed env) 1)
                               (let* ((a (nl_aot_leaf_arg a0))
                                      (b (if (>= argc 2) (nl_aot_leaf_arg a1) 3)))
                                 (if (= id 101)
                                     (nl_directcall_hash_get_leaf_w a b
                                       (if (= argc 3) (nl_aot_leaf_arg a2) 3))
                                   (nl_directcall_leaf_value id a b env))) 0) 0)))
             ;; The artifact frame owns the arguments through the leaf call.
             (if (/= leaf 0) leaf
             (let* ((mark (nl_root_mark env))
                    (argv (nl_root_frame env (+ argc 3))))
               (if (= argv 0) 2
                 (let* ((out (+ argv (* argc 8))))
                   (seq
                    (ptr-write-u64 out 16 subr)
                    (if (> argc 0) (nl_val_clone_into a0 argv) 0)
                    (if (> argc 1) (nl_val_clone_into a1 (+ argv 8)) 0)
                    (if (> argc 2) (nl_val_clone_into a2 (+ argv 16)) 0)
                    (let* ((operator (nl_val_clone_into name (+ out 8)))
                           (status (nl_directcall_invoke subr operator argv argc env out)))
                      (nl_cons_root_finish env mark
                        (if (= status 0) (ptr-read-u64 out 0)
                          (if (= status 2) 2 0)))))))))))))))
  "Try a native Subr with up to three arguments. Result 2 means decline.")

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
      ,@nelisp-cc-evalport-aot-apply--direct-subr
      ,(nelisp-cc-evalport-aot-apply--provider
        'nl_aot_funcall_list_w 'nl_apply_do_funcall_w)
      ,(nelisp-cc-evalport-aot-apply--provider
        'nelisp_aot_applyn_w 'nl_apply_do_apply_w)
      (defun nelisp_aot_funcalln_w (mirror frames fn argc _pad scratch ,@args)
        (let* ((word (nl_aot_funcall_subr_w mirror fn argc a0 a1 a2)))
          (if (= word 2)
              (nl_aot_funcall_list_w mirror frames fn argc 0 scratch ,@args)
            word)))
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
