;;; callpath2-gencheck.el --- callpath2 S2: per-call costs from compiled code, offline  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/callpath2-gencheck.el
;; Prints one CHECK line per property and GENCHECK bad=N; exit 0 iff bad = 0.
;; - admit: intern and intern-soft are argv-admitted (their obarray argument is
;;   tested by the argument count; only the Lisp reference takes the list).
;; - lower: intern's argv body declines (2) only where it calls the Lisp reference.
;; - prim2: a Subr object's interpreter primitive (`*', ids 15 and up) runs on the
;;   operand words from the VM's CALL and `nc_call' (`m5_fx_subr_prim2').
;; - funcall: `funcall' of a byte-code function or a Subr object runs on the words
;;   (`m5_fx_funcall_value': `wf_bytecode_w_function' / `m5_fx_subr_argv').
;; - arity: `nelisp--native-subr-arity' reads a builtin's Subr object
;;   (`bf_builtin_subr_arity_w'); the prelude defines `subr-arity'.
;; - apply: the call-out runs `apply' (kind 14) on the words (`m5_fx_apply_run' ->
;;   `nl_eic_apply_words'), not the argument list.
;; - spread: an apply `nl_eic_apply_words' leaves is spread onto words and funcalled;
;;   the call-out runs a byte-code function / Subr object on the words before the list.
;; - sfn: `symbol-function' (kind -21) reads the symbol object's cell on the word.
;; - stubs: link sets other than the reader keep the -3 / 0 stubs.
;; - parse: the changed defuns pass the AOT parser.
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(defvar cp2-bad 0)
(defun cp2-check (name ok &optional detail)
  (unless ok (setq cp2-bad (1+ cp2-bad)))
  (princ (format "CHECK %s %s%s\n" name (if ok "ok" "BAD") (if detail (format " %s" detail) ""))))
(defun cp2-has (x form)
  (cond ((equal x form) t)
        ((consp form) (or (cp2-has x (car form)) (cp2-has x (cdr form))))))
(defun cp2-find (name forms)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name))) forms))
(defun cp2-parse (defun-form)
  (let ((nelisp-aot-compiler--allow-external-user-calls t))
    (condition-case err
        (progn (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse defun-form)) t)
      (error (princ (format "PARSE-ERROR %S\n" err)) nil))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (table (nelisp-standalone--applyfn-guarded-reader-table))
       (adm (nelisp-argframe-admitted-names table)))
  ;; v28 admits 195 (nelisp--bv-binop and nelisp--bv-count without a static id,
  ;; red on v28 already in test/nelisp-argframe-generation-test.el).
  (cp2-check "admit" (and (member "intern" adm) (member "intern-soft" adm) (member "memq" adm)
                          (>= (length adm) 197)
                          (nelisp-standalone--sid "intern") (nelisp-standalone--sid "intern-soft"))
             (format "admitted=%d intern=%s" (length adm) (and (member "intern" adm) t)))
  (let* ((in (assoc '(:lit "intern") table))
         (low (and in (condition-case nil (nelisp-argframe-lower (cddr in) 'argv 'argc)
                        (nelisp-argframe-ineligible nil)))))
    (cp2-check "lower" (and low (not (cp2-has 'args low)) (cp2-has 2 low)
                            (cp2-has 'nl_intern_lookup_w low)
                            (not (cp2-has 'wf_slow_named_w low))))))

(let* ((helpers (nelisp-standalone--argframe-m5-helpers))
       (p2 (cp2-find 'm5_fx_subr_prim2 helpers))
       (fv (cp2-find 'm5_fx_funcall_value helpers))
       (ar (cp2-find 'bf_builtin_subr_arity_w helpers))
       (any (cp2-find 'm5_fx_call_subr_any helpers))
       (site (cp2-find 'm5_fx_funcall_site helpers))
       (stub-p2 (cp2-find 'm5_fx_subr_prim2 nelisp-standalone--applyfn-m5-helpers))
       (stub-fv (cp2-find 'm5_fx_funcall_value nelisp-standalone--applyfn-m5-helpers))
       (stub-ar (cp2-find 'bf_builtin_subr_arity_w nelisp-standalone--applyfn-m5-helpers))
       (ap (cp2-find 'm5_fx_apply_run helpers))
       ;; applyfn2: the word apply moved into `m5_fx_apply_words_run', which
       ;; `m5_fx_apply_run' runs after GNU Fapply's builtin spread declines.
       (apw (cp2-find 'm5_fx_apply_words_run helpers))
       (stub-ap (cp2-find 'm5_fx_apply_run nelisp-standalone--applyfn-m5-helpers))
       (kind (cp2-find 'm5_fx_callout_kind helpers))
       (prun (cp2-find 'm5_fx_primitive_run helpers))
       (pid (cp2-find 'm5_fx_primitive_id helpers)))
  (cp2-check "apply" (and ap apw kind prun (cp2-has 'nl_eic_apply_words apw) (cp2-has 'nl_root_release apw)
                          (cp2-has 'm5_fx_apply_words_run ap)
                          (not (cp2-has 'm5_fx_apply_run prun))
                          (cp2-has '(m5_fx_apply_run stk sp argc resw env)
                                   (cp2-find 'm5_fx_callout helpers))
                          (string-match-p "(m5_fx_apply_run stk argc argc resw env)"
                                          (prin1-to-string (nelisp-cc-fxir-native-source)))
                          (cp2-has "apply" kind) (equal (nthcdr 3 stub-ap) '(-3))
                          (cp2-parse ap) (cp2-parse kind) (cp2-parse prun)))
  (let ((sp (cp2-find 'm5_fx_apply_spread helpers))
        (co (cp2-find 'm5_fx_callout helpers)))
    (cp2-check "spread" (and sp co (cp2-has 'm5_fx_funcall_site sp) (cp2-has 'nl_eic_apply_list_ok sp)
                             (cp2-has '(m5_fx_apply_spread base argc resw env) apw)
                             (equal (nthcdr 3 (cp2-find 'm5_fx_apply_spread nelisp-standalone--applyfn-m5-helpers)) '(-3))
                             (cp2-has '(m5_fx_funcall_value name 0 fn stk sp argc resw env) co)
                             (cp2-parse sp) (cp2-parse co))))
  (let ((nw (cp2-find 'm5_fx_apply_no_word_subr helpers))
        (co (cp2-find 'm5_fx_callout helpers)))
    (cp2-check "fixes" (and nw (cp2-has '(m5_fx_apply_no_word_subr fw) ap)
                            (cp2-has '(fw (heap-read-u64 (+ stk (* (- sp argc) 8)) 0)) ap)
                            (equal (nthcdr 3 (cp2-find 'm5_fx_apply_no_word_subr nelisp-standalone--applyfn-m5-helpers)) '(0))
                            (cp2-has '(/= kind 0) co)
                            (cp2-has '(= (nl_val_tag fn) 7) site) (cp2-parse nw) (cp2-parse co) (cp2-parse site))))
  (cp2-check "sfn" (and pid prun (cp2-has "symbol-function" pid) (cp2-has '(= id -21) prun)
                        (cp2-has 'nelisp_env_lookup_function_w prun) (cp2-parse pid)))
  (cp2-check "prim2" (and p2 any (cp2-has 'nl_eic_prim2_run p2) (cp2-has 'nl_subr_arity_bad p2)
                          (cp2-has 'wf_bytecode_poll p2) (cp2-has 'm5_fx_subr_prim2 any)))
  (cp2-check "funcall" (and fv site (cp2-has 'wf_bytecode_w_function fv) (cp2-has 'm5_fx_subr_argv fv)
                            (cp2-has 'nl_n4_push_words fv) (cp2-has 'm5_fx_funcall_value site)))
  (cp2-check "arity" (and ar (cp2-has 'nelisp_env_lookup_function_raw_w ar)
                          (let ((arm (assoc '(:lit "nelisp--native-subr-arity")
                                            (nelisp-standalone--applyfn-guarded-reader-table))))
                            (and arm (cp2-has 'bf_builtin_subr_arity_w arm)))
                          (with-temp-buffer
                            (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
                            (re-search-forward "^  (defun subr-arity (subr)" nil t))))
  (cp2-check "stubs" (and stub-p2 stub-fv stub-ar (equal (nthcdr 3 stub-p2) '(-3))
                          (equal (nthcdr 3 stub-fv) '(-3)) (equal (nthcdr 3 stub-ar) '(0))))
  (cp2-check "parse" (and p2 fv ar any site (cp2-parse p2) (cp2-parse fv) (cp2-parse ar)
                          (cp2-parse any) (cp2-parse site))))

;; The `*' Subr object carries the interpreter descriptor (id 17) the prim2 arm reads.
(let* ((d (nelisp-standalone--subr-desc "*")) (p (ash d -25))
       (desc (logior (logand p 63) (ash (ash p -6) 8))))
  (cp2-check "desc" (= (logand desc 255) 17) (format "desc=%d" desc)))

(princ (format "GENCHECK bad=%d\n" cp2-bad))
(kill-emacs (if (= cp2-bad 0) 0 1))
