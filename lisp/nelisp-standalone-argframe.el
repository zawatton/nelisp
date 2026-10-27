;;; nelisp-standalone-argframe.el --- Proven indexed argv transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep admission a structural proof over the current handler body. An
;; unrecognized use of the argument list must retain the ordinary list path.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-argframe-ineligible "Argument list requires ordinary transport")

(defconst nelisp-argframe-static-symbol-suffix
  '("atom" "stringp" "integerp" "numberp" "floatp" "vectorp" "listp" "nlistp"
    "nelisp--declare-local-special" "nelisp--intern-table-symbols"
    "nelisp--format-simple" "nelisp--native-subr-live-count" "fetch-bytecode"
    "nelisp--char-table-vector-bridge-p" "nelisp--string-mark-intervals"
    "nl-current-unix-time" "nl-unix-time-usec" "syscall-direct" "atomic-fetch-add"
    "ptr-read-u8" "ptr-write-u8" "ptr-read-u32" "ptr-write-u32"
    "ptr-read-u64" "ptr-write-u64" "alloc-bytes" "nelisp-process-async-ready-p"
    "nelisp--target-os-code" "nelisp--target-arch-code" "sin" "cos"
    "nelisp--native-symbol-addr" "nelisp--native-env"
    "nelisp--native-runtime-contract-word" "nl-ffi-call" "nelisp--native-pin-eq-slots"
    ;; subr1: the handlers `wf_arg_ptr' lowering admits (append-only).
    "subrp" "symbolp" "symbol-with-pos-p" "nelisp--symbol-position-op" "bare-symbol"
    "natnump" "sxhash-eq" "nl--read-int" "nl--int-token-p" "nl--read-float-token" "zerop"
    "symbol-name" "make-symbol" "nelisp--intern-lookup" "nelisp--native-subr-arity"
    "sxhash-equal" "sxhash-eql" "ash" "lognot" "nelisp--float-word-half"
    "nelisp-process-object-p" "nelisp-process-pid" "nelisp-process-status"
    "nelisp-process-exit-status" "nelisp-process-wait" "nelisp-process-delete"
    "tan" "asin" "acos" "exp" "sqrt" "ffloor" "fceiling" "ftruncate" "fround" "log" "atan"
    "nelisp--pow" "float" "logb" "nelisp--symbol-global-cell-p" "ptr-call-typed"
    "special-form-p" "nelisp--native-pin-copy" "nelisp--native-pin-copy-v2"
    ;; callpath: the handlers admitted once helper calls are opened (append-only).
    "makunbound" "featurep" "make-record" "recordp" "byte-code-function-p"
    "nelisp--raw-aref" "nelisp--raw-aset" "char-table-p" "nelisp--char-table-slots"
    "nelisp--char-table-set-slots" "char-table-subtype" "char-table-parent"
    "set-char-table-parent" "char-table-extra-slot" "set-char-table-extra-slot"
    "char-table-range" "set-char-table-range" "map-char-table"
    "nelisp--clear-string-bytes" "nelisp--equal-leaf" "nelisp--eval-source-string"
    "ptr-call" "thread-spawn" "nelisp-spin-until" "fork-spawn"
    ;; callpath2: intern's handlers, admitted once the obarray argument is
    ;; tested by the argument count (append-only).
    "intern" "intern-soft")
  "Frozen append-only IDs needed before the first dispatch is generated.
The full reader table later ratchets this suffix against structural admission;
no old symbol ID is moved and no late mutation changes a generated dispatch.")

(defvar nelisp-argframe-inline-helpers nil
  "callpath: helper definitions the lowering may open, or nil for none.
A hash table (or a function answering one) from a helper's name to its
\=(PARAMS . BODY), for helpers whose first parameter is `args'.  A handler arm
that hands its whole argument list to such a helper, `(HELPER args E...)',
is lowered as the helper's body with `args' read through the argument array,
the other parameters renamed and bound to E..., so a handler admitted by
structure no longer depends on how its body is split into helpers (GNU: a
SUBR's C function receives its arguments, however it delegates).  A helper
whose body is not provably indexed leaves the arm on the list route.")

(defconst nelisp-argframe-inline-depth 3
  "callpath: how many helper levels the lowering opens.")

(defconst nelisp-argframe-inline-max-size 600
  "callpath: the largest helper body (in conses) the lowering opens.")

(defvar nelisp-argframe--inline-stack nil
  "callpath: the helpers being opened, innermost first.")

(defun nelisp-argframe--size (form)
  (if (consp form) (+ 1 (nelisp-argframe--size (car form)) (nelisp-argframe--size (cdr form))) 0))

(defun nelisp-argframe--helper (name)
  "The (PARAMS . BODY) of helper NAME the lowering may open, else nil."
  (let ((table (if (functionp nelisp-argframe-inline-helpers)
                   (funcall nelisp-argframe-inline-helpers)
                 nelisp-argframe-inline-helpers)))
    (and (hash-table-p table) (symbolp name)
         (< (length nelisp-argframe--inline-stack) nelisp-argframe-inline-depth)
         (not (memq name nelisp-argframe--inline-stack))
         (let ((def (gethash name table)))
           (and def (eq (car-safe (car def)) 'args)
                (<= (nelisp-argframe--size (cdr def)) nelisp-argframe-inline-max-size)
                def)))))

(defun nelisp-argframe--open (name def actuals base argc mode)
  "Lower the call (NAME args . ACTUALS) as the body of DEF opened in place."
  (let* ((params (cdar def)) (body (cdr def)) (n 0) (bindings nil))
    (when (/= (length params) (length actuals))
      (signal 'nelisp-argframe-ineligible (list name)))
    (dolist (p params)
      (let ((fresh (intern (format "nl_afi_%s_%d" name n))))
        (setq n (1+ n))
        (setq body (cl-subst fresh p body))
        (push (list fresh (nelisp-argframe-lower (car actuals) base argc mode)) bindings)
        (setq actuals (cdr actuals))))
    (let* ((nelisp-argframe--inline-stack (cons name nelisp-argframe--inline-stack))
           (lowered (nelisp-argframe-lower (if (cdr body) (cons 'seq body) (car body))
                                           base argc mode)))
      (if bindings `(let* ,(nreverse bindings) ,lowered) lowered))))

(defun nelisp-argframe-lower (form base argc &optional mode)
  "Lower proven indexed reads in FORM to BASE and ARGC.
With MODE, retain the original read when MODE is zero. Missing arguments
return zero, matching `wf_arg_word'; an explicit nil argument remains three.
Reject every other occurrence of `args', including bindings and escapes.
A call handing `args' whole to a helper of `nelisp-argframe-inline-helpers'
is opened in place (with MODE, only the argv side; the list side keeps the
call).  `(nl_argframe_list_only X)' keeps X on the list route: the argv side
declines with 2 there, which its callers answer by running the list route on
the same evaluated words; it may only wrap work that no effect precedes."
  (cond
   ((or (eq form 'args) (and mode (eq form mode)))
    (signal 'nelisp-argframe-ineligible (list form)))
   ((atom form) form)
   ((and (eq (car form) 'nl_argframe_list_only) (consp (cdr form)) (null (cddr form)))
    (if mode `(if (= ,mode 0) ,form 2) 2))
   ((and (memq (car form) '(wf_arg_word wf_argval wf_arg_ptr))
         (eq (cadr form) 'args) (integerp (caddr form))
         (>= (caddr form) 0) (null (cdddr form)))
    ;; subr1: BASE is a raw argv pointer (never a String), so its words are
    ;; raw loads; `heap-read-u64' at offsets 0-24 of a variable is the
    ;; String-aware `nl_string_read64' call.  `wf_arg_ptr' is
    ;; `nl_cons_car_ptr' of the list cell: the same view-or-materialise test
    ;; on the element WORD (`m5_fx_word_ptr'), and 0 past the end, as the
    ;; list walk reaches nil.
    (let* ((index (caddr form))
           (word `(if (< ,index ,argc) (ptr-read-u64 ,base ,(* index 8)) 0))
           (read (pcase (car form)
                   ('wf_argval `(nl_word_payload ,word))
                   ('wf_arg_ptr `(m5_fx_word_ptr ,word))
                   (_ word))))
      (if mode `(if (= ,mode 0) ,form ,read) read)))
   ((equal form '(m5_list_len args 0))
    (if mode `(if (= ,mode 0) ,form ,argc) argc))
   ((and (consp (cdr form)) (eq (cadr form) 'args) (nelisp-argframe--helper (car form)))
    (let ((opened (nelisp-argframe--open (car form) (nelisp-argframe--helper (car form))
                                         (cddr form) base argc
                                         ;; The list side keeps the original call whole.
                                         nil)))
      (if mode `(if (= ,mode 0) ,form ,opened) opened)))
   (t (cons (nelisp-argframe-lower (car form) base argc mode)
            (nelisp-argframe-lower (cdr form) base argc mode)))))

(defun nelisp-argframe-mode-table (table &optional admitted-name-p)
  "Add private argv mode to TABLE without duplicating handler bodies.
The caller supplies `_pad' = argc + 1 in frame mode and zero in list mode.
An ineligible frame arm declines before any handler effect. ADMITTED-NAME-P
may impose an additional metadata proof, such as an existing frozen SID."
  (mapcar
   (lambda (entry)
     (unless (eq (cadr entry) :word)
       (error "Argument-frame transport requires a WORD arm: %S" (car entry)))
     (cons (car entry)
           (cons :word
                 (if (and admitted-name-p (not (funcall admitted-name-p (cadar entry))))
                     `(if (= _pad 0) ,(cddr entry) 2)
                   (condition-case nil
                     (nelisp-argframe-lower
                      (cddr entry) 'args '(- _pad 1) '_pad)
                   (nelisp-argframe-ineligible
                    `(if (= _pad 0) ,(cddr entry) 2)))))))
   table))

(defun nelisp-argframe-admitted-names (table)
  "Return the names whose complete guarded TABLE bodies prove eligible."
  (let (names)
    (dolist (entry table (nreverse names))
      (when (eq (cadr entry) :word)
        (condition-case nil
            (progn (nelisp-argframe-lower
                    (cddr entry) 'args '(- _pad 1) '_pad)
                   (push (cadar entry) names))
          (nelisp-argframe-ineligible nil))))))

(provide 'nelisp-standalone-argframe)
;;; nelisp-standalone-argframe.el ends here
