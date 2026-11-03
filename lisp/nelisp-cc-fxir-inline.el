;;; nelisp-cc-fxir-inline.el --- Guarded parameter projection splicing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Experimental M1a only.  Native qualification is a separate requirement.
;; Reuse the compiler's complete wrapper/activation templates, rather than
;; reproducing their frame, root, depth or common-exit protocols here.
;;; Code:
(require 'cl-lib)
(require 'nelisp-cc-fxir-analysis)
(declare-function nelisp-cc-fxir-native--wrapper "nelisp-cc-fxir-native" (row))
(declare-function nelisp-cc-fxir-native--defun "nelisp-cc-fxir-native" (row &optional body-factory))
(defvar nelisp-cc-fxir-native-inline-projections)
(defvar nelisp-cc-fxir-native-inline-additions)

(defun nelisp-cc-fxir-inline-projection (row)
  "Return a structural projection certificate for ROW, or nil.
This is not a runtime admission certificate.  Live record/code guards and
the canonical wrapper protocol remain mandatory at every selected site."
  (let* ((arity (nth 5 row)) (n (car arity)) (body (nth 1 (nth 4 row))))
    (when (and (plist-get (nelisp-cc-fxir-analysis-row row) :valid)
               (integerp n) (<= 1 n) (<= n 4)
               (equal arity (list n n -1)) (= (nth 2 row) n)
               (equal (car-safe body) :slot) (= (length body) 2)
               (< (nth 1 body) n))
      (list :row-id (nth 1 row) :arity n :slot (nth 1 body)
            :code-words (list 69 3 9 (1+ (* 4 (nth 1 body))) 1 13)))))

(defun nelisp-cc-fxir-inline-addition (row)
  "Return a certificate for the exact nested two-addition ROW."
  (when (and (plist-get (nelisp-cc-fxir-analysis-row row) :valid)
             (= (nth 2 row) 2) (null (nth 3 row)) (equal (nth 5 row) '(2 2 -1))
             (equal (nth 4 row)
                    '(:fn (:inline 4 11 (:inline 4 7 (:slot 0) (:slot 1)) (:const 9)))))
    (list :row-id (nth 1 row) :arity 2
          :code-words '((0 . 69) (1 . 3) (2 . 9) (3 . 1) (4 . 9) (5 . 5)
                        (6 . 17) (8 . 5) (9 . 5) (10 . 17) (12 . 1) (13 . 17)))))

(defun nelisp-cc-fxir-inline--contains (form target)
  "Return non-nil when FORM contains TARGET structurally."
  (or (equal form target)
      (and (consp form) (or (nelisp-cc-fxir-inline--contains (car form) target)
                            (nelisp-cc-fxir-inline--contains (cdr form) target)))))

(defun nelisp-cc-fxir-inline--count-form (form target)
  "Count TARGET in FORM without traversing a matched subtree."
  (cond ((equal form target) 1)
        ((consp form) (+ (nelisp-cc-fxir-inline--count-form (car form) target)
                         (nelisp-cc-fxir-inline--count-form (cdr form) target)))
        (t 0)))

(defun nelisp-cc-fxir-inline--replace-once (form old new)
  "Replace the unique OLD subtree in FORM, refusing template drift."
  (unless (= (nelisp-cc-fxir-inline--count-form form old) 1)
    (error "Expected one guarded activation subtree: %S" old))
  (cl-subst new old form :test #'equal))

(defun nelisp-cc-fxir-inline--checked-activation (activation)
  "Remove dead direct-argument work from the exact checked activation."
  ;; The direct caller has already staged both arguments in its live window.
  ;; This transformation does not change the seven roots or their contents.
  (unless (and (= (nelisp-cc-fxir-inline--count-form activation 'l0) 2)
               (= (nelisp-cc-fxir-inline--count-form activation 'l1) 2)
               (= (nelisp-cc-fxir-inline--count-form activation 'nb) 3))
    (error "Checked activation local use changed"))
  (let* ((arg-list
          '(if (= argw 0)
               (seq (if (= mem 0) (setq mem (nl_root_frame env 7)) 0)
                    (if (= mem 0) (setq st -4)
                      (setq argw (nc_list_words args (+ mem (* 5 8)) (if (< n 2) n 2)))))
             0))
         (result '(+ (shl next 2) 1))
         (entry '(heap-read-u64 (+ (+ (nl_word_payload code) 8) 8) 0))
         (form (nelisp-cc-fxir-inline--replace-once activation arg-list 0))
         (form (nelisp-cc-fxir-inline--replace-once form
                 '(setq l0 (heap-read-u64 (+ argw (* 0 8)) 0)) 0))
         (form (nelisp-cc-fxir-inline--replace-once form
                 '(setq l1 (heap-read-u64 (+ argw (* 1 8)) 0)) 0))
         (form (nelisp-cc-fxir-inline--replace-once form
                 `(if (= wout 0) (nl_abi_word_to_rc ,result out)
                    (ptr-write-u64 wout 0 ,result))
                 `(ptr-write-u64 wout 0 ,result)))
         (form (nelisp-cc-fxir-inline--replace-once form
                 '(if (> nb 0) (wf_bytecode_unbind (+ env 32) nb) 0) 0)))
    ;; Read CODE again after the complete push, preserving the original
    ;; fallback and all non-nil ENTRY behavior for noncompact values.
    (nelisp-cc-fxir-inline--replace-once form entry
     `(if (= (logand code 7) 6) (ptr-read-u64 (+ code 10) 0) ,entry))))

(defun nelisp-cc-fxir-inline--replace-call (form target replacement)
  "Replace the unique call to TARGET in FORM with REPLACEMENT."
  (let ((count 0))
    (cl-labels ((walk (x)
                  (if (atom x) x
                    (if (eq (car x) target)
                        (progn (setq count (1+ count)) (copy-tree replacement))
                      (cons (walk (car x)) (walk (cdr x)))))))
      (let ((result (walk form)))
        (unless (= count 1) (error "Expected one native body invocation, got %d" count))
        result))))

(defun nelisp-cc-fxir-inline--hygienic (form parameters values token)
  "Clone FORM with fresh deterministic lexical bindings for site TOKEN.
PARAMETERS are bound to caller-side VALUES exactly once, in order."
  (let ((serial 0) (prefix (format "ni_%s_" (mapconcat #'number-to-string token "_"))))
    (cl-labels
        ((fresh (name) (intern (format "%s%d_%s" prefix (cl-incf serial) name)))
         (walk (x env)
           (cond
            ((symbolp x) (or (cdr (assq x env)) x))
            ((atom x) x)
            ((memq (car x) '(let let*))
             (let ((next env) (bindings nil))
               (dolist (b (nth 1 x))
                 (let* ((name (if (consp b) (car b) b)) (new (fresh name))
                        (value (and (consp b) (walk (nth 1 b) (if (eq (car x) 'let*) next env)))))
                   (push (list new value) bindings) (push (cons name new) next)))
               (cons (car x) (cons (nreverse bindings)
                                   (mapcar (lambda (b) (walk b next)) (cddr x))))))
            ((eq (car x) 'setq)
             (cons 'setq (cl-loop for (name value) on (cdr x) by #'cddr
                                  append (list (or (cdr (assq name env)) name) (walk value env)))))
            ;; The dialect has no closures/function-valued local operators.
            (t (cons (car x) (mapcar (lambda (a) (walk a env)) (cdr x)))))))
      (let* ((env (mapcar (lambda (p) (cons p (fresh p))) parameters))
             (body (walk form env)))
        `(let* ,(cl-mapcar (lambda (p value) (list (cdr (assq p env)) value)) parameters values)
           ,body)))))

(defun nelisp-cc-fxir-inline--enter-seven ()
  "Derive the seven-root entry from the exact canonical runtime template."
  (let* ((template (cl-find-if (lambda (x) (and (eq (car-safe x) 'defun)
                                                (eq (cadr x) 'nc_enter)))
                               (symbol-value 'nelisp-cc-fxir-native--shared-source)))
         (print-length nil) (print-level nil) (print-circle nil) (print-quoted t)
         (loop-form '(while (< slot end)
                       (seq (ptr-write-u64 slot 0 3) (setq slot (+ slot 8)))))
         (slow '(nc_enter_slow env n fb)))
    (unless (and (equal (secure-hash 'sha256 (prin1-to-string template))
                        "636c5fe90a5c4402317504ac381032b94440b9ef37ecc8048a467e0d079a3f31")
                 (nelisp-cc-fxir-inline--contains template loop-form)
                 (nelisp-cc-fxir-inline--contains template slow))
      (error "Canonical nc_enter template changed"))
    ;; SLOT is not used after this loop in the authenticated template.
    (let* ((body (nth 3 template))
           (writes (cons 'seq (cl-loop for i below 7 collect `(ptr-write-u64 slot ,(* i 8) 3))))
           (body (cl-subst writes loop-form body :test #'equal))
           (body (cl-subst '(nc_enter env 7) slow body :test #'equal)))
      (cl-subst 7 'n body :test #'eq))))

(defun nelisp-cc-fxir-inline-form (row site argc token &optional kind rewrite)
  "Splice ROW's guarded projection at SITE with staged ARGC and stable TOKEN.
On an authenticated same-ID code mismatch, execute current bytecode with
the original positive local count, never the stale native sentinel.
Other dispatch mismatches retain the ordinary call.  All code selection
happens before the logical push; the admitted prefix cannot re-enter Lisp:
main thread only, no dynamic mode, and depth+2 strictly below 100."
  (let* ((addition (eq kind :addition))
         (certificate (if addition (nelisp-cc-fxir-inline-addition row)
                        (nelisp-cc-fxir-inline-projection row)))
         (n (plist-get certificate :arity))
         (id (nth 1 row)) (native (intern (format "nc_%d" id)))
         (nelisp-cc-fxir-native-inline-projections nil)
         (nelisp-cc-fxir-native-inline-additions nil)
         ;; The outer guard authenticates REC before the callback-free code
         ;; and argument preflight.  Reuse it only up to the logical push.
         (wrapper0 (nth 3 (nelisp-cc-fxir-native--wrapper row)))
         (push-form (symbol-value 'nelisp-cc-fxir-native--push-form))
         (wrapper-if (nth 2 wrapper0))
         ;; Keep the ordinary wrapper unchanged.  These guarded replacements
         ;; preserve the getter arm when the address has String tag bits.
         (site-form (cl-subst '(if (/= (logand env 7) 4)
                                   (ptr-read-u64 (+ env 112) 0)
                                 (heap-read-u64 (+ env 112) 0))
                              '(heap-read-u64 (+ env 112) 0)
                              (symbol-value 'nelisp-cc-fxir-native--site-rec-form) :test #'equal))
         (wrapper (cl-subst '(if (/= (logand env 7) 4)
                                  (ptr-read-u64 (+ env 96) 0)
                                (heap-read-u64 (+ env 96) 0))
                            '(heap-read-u64 (+ env 96) 0)
                            (nth 2 wrapper-if) :test #'equal))
         (checked-wrapper
          (if (and addition rewrite)
              ;; `site-form' has proved class 110.  Read each slot at the
              ;; original position; no value is carried across the push.
              (nelisp-cc-fxir-inline--replace-once
               (nelisp-cc-fxir-inline--replace-once
                wrapper '(nl_vector_slot_word site 3) '(ptr-read-u64 (+ site 2) 24))
               '(nl_vector_slot_word site 0) '(ptr-read-u64 (+ site 2) 0))
            wrapper))
         (activation0 (nth 3 (nelisp-cc-fxir-native--defun
                             row (when rewrite (lambda (_body) (cons nil '(+ (shl next 2) 1)))))))
         (activation1 (if (and addition rewrite)
                          (nelisp-cc-fxir-inline--checked-activation activation0)
                        activation0))
         (activation (if addition
                         (cl-subst (nelisp-cc-fxir-inline--enter-seven)
                                   '(nc_enter env 7) activation1 :test #'equal)
                       activation1))
         (expanded `(let* ((code (ptr-read-u64 (+ rec 2) 0)) (out 0) (args 0)
                           (nparams argc) (argw stk) (wout resw))
                      ,activation))
         (vm `(m5_fx_run_body (ptr-read-u64 (+ rec 2) 0) ,n env 0 0 argc stk resw))
         (inline-body (nelisp-cc-fxir-inline--replace-call checked-wrapper native expanded))
         (vm-body (nelisp-cc-fxir-inline--replace-call wrapper native vm))
         (code-tests (if addition
                         (mapcar (lambda (pair) `(= (nl_vector_slot_word livecode ,(car pair)) ,(cdr pair)))
                                 (plist-get certificate :code-words))
                       (cl-loop for word in (plist-get certificate :code-words)
                                for i from 0 collect `(= (nl_vector_slot_word livecode ,i) ,word))))
         (direct-code-tests
          (when addition
            (mapcar (lambda (pair) `(logxor (ptr-read-u64 codebase ,(* 8 (car pair))) ,(cdr pair)))
                    (plist-get certificate :code-words))))
         (accepted (if addition
                       ;; The two producer opcode operands are read only on
                       ;; slow fallback. Both fast operations are proved here;
                       ;; a miss executes current bytecode once, before effects.
                       `(let* ((rtop (heap-read-u64 (data-addr nl_rootstack_top) 0))
                               ;; STK is the aligned rooted window staged by
                               ;; `nelisp-cc-fxir-native--direct-stmts'.
                               (x (ptr-read-u64 stk 0)) (y (ptr-read-u64 (+ stk 8) 0)))
                          (if (and (/= rtop 0)
                                   (<= (+ rtop 128) (+ (data-addr nl_rootstack_region) 4194304))
                                   (= (logand x 3) 1) (= (logand y 3) 1))
                              (let* ((sum (+ (sar x 2) (sar y 2))) (next (+ sum 1)))
                                (if (and (= (sar (shl sum 2) 2) sum)
                                         (= (sar (shl next 2) 2) next))
                                    ,inline-body ,vm-body))
                            ,vm-body))
                     inline-body)))
    (unless (and certificate (= argc n)) (error "Not a fixed-arity projection"))
    (when (and rewrite (not addition)) (error "Rewrite requires the addition leaf"))
    (unless (and (eq (car-safe wrapper0) 'let*)
                 (eq (car-safe wrapper-if) 'if)
                 (equal (nth 3 wrapper-if) '(nc_call env site stk argc resw oslot))
                 (equal (nth 1 wrapper0)
                        `((rec ,(symbol-value 'nelisp-cc-fxir-native--site-rec-form))))
                 (equal (nth 1 wrapper-if)
                        `(if (= rec 0) 0 (= (nl_vector_slot_word rec 1) ,(1+ (* 4 (- -16 id))))))
                 (nelisp-cc-fxir-inline--contains wrapper push-form)
                 (equal (member '(ptr-write-u64 bfr 56 (nl_n4_cut_capture env))
                                (nth 2 (nth 3 (nth 2 push-form))))
                        '((ptr-write-u64 bfr 56 (nl_n4_cut_capture env))
                          (ptr-write-u64 bstate 0 (+ bdepth 1)) bfr)))
      (error "Canonical wrapper template changed"))
    ;; Seven root WORDs, rounded to 64 bytes, plus 64 bytes for frame and
    ;; view headers. Refuse if the current activation template differs.
    (when (and addition
               (or (/= (nelisp-cc-fxir-inline--count-form activation0 '(nc_enter env 7)) 1)
                   (not (nelisp-cc-fxir-inline--contains
                         activation0 '(seq (setq mem (nc_enter env 7))
                                          (if (= mem 0) (setq st -4) (setq oslot (+ mem 64))))))))
      (error "Two-addition root reservation template changed"))
    (nelisp-cc-fxir-inline--hygienic
     `(let* ((rec ,site-form))
        (if (and (/= rec 0)
                 (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
                 ,@(unless addition
                     '((= (heap-read-u64 env 168) 0)
                       (= (logand (if (/= (logand env 7) 4)
                                      (ptr-read-u64 (+ env 112) 0)
                                    (heap-read-u64 (+ env 112) 0)) 2097152) 0)))
                 (< (+ (if (/= (logand env 7) 4)
                           (ptr-read-u64 (+ env 96) 0)
                         (heap-read-u64 (+ env 96) 0)) 2) 100)
                 (= (ptr-read-u64 (+ rec 2) 8) ,(1+ (* 4 (- -16 id))))
                 (= (ptr-read-u64 (+ rec 2) 16) ,(1+ (* 4 n)))
                 (= (ptr-read-u64 (+ rec 2) 40) ,(1+ (* 4 n)))
                 (= (ptr-read-u64 (+ rec 2) 48) -3)
                 ,@(unless addition '((= (ptr-read-u64 (+ rec 2) 56) 3))))
            (let* ((livecode (ptr-read-u64 (+ rec 2) 0)))
              ;; The authenticated native ID certifies the original local
              ;; count via install's live recompile, code, IR, callees and
              ;; arity checks.  As in the VM, current producer bytecode must
              ;; remain coherent with that preserved frame metadata.
              (if ,(if (and addition rewrite)
                       `(if (= (logand livecode 7) 6)
                            ;; Keep the byte kind read ahead of the wider
                            ;; length read for compact non-vector objects.
                            (if (= (logand (ptr-read-u8 (- livecode 6) 0) 63) 8)
                                (if (= (sar (ptr-read-u64 (- livecode 6) 0) 8) 14)
                                    (let* ((codebase (+ livecode 2)))
                                      (= ,(cl-reduce (lambda (a b) `(logior ,a ,b)) direct-code-tests) 0))
                                  0)
                              0)
                          (if (= (nl_val_tag livecode) 8)
                              (and (= (vector-len livecode) 14) ,@code-tests) 0))
                     `(if (= (nl_val_tag livecode) 8)
                          ,(if addition
                               `(and (= (vector-len livecode) 14)
                                     (if (= (logand livecode 7) 6)
                                         (let* ((codebase (+ livecode 2)))
                                           (= ,(cl-reduce (lambda (a b) `(logior ,a ,b)) direct-code-tests) 0))
                                       (and ,@code-tests)))
                             `(and (= (vector-len livecode) 6) ,@code-tests)) 0))
                  ,accepted ,vm-body))
          (nc_call env site stk argc resw oslot)))
     '(env site stk argc resw oslot) (list 'env site '(+ mem 24) argc 'mem 'oslot) token)))

(provide 'nelisp-cc-fxir-inline)
;;; nelisp-cc-fxir-inline.el ends here
