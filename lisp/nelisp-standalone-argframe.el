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
    "nelisp--native-runtime-contract-word" "nl-ffi-call" "nelisp--native-pin-eq-slots")
  "Frozen append-only IDs needed before the first dispatch is generated.
The full reader table later ratchets this suffix against structural admission;
no old symbol ID is moved and no late mutation changes a generated dispatch.")

(defun nelisp-argframe-lower (form base argc &optional mode)
  "Lower proven indexed reads in FORM to BASE and ARGC.
With MODE, retain the original read when MODE is zero. Missing arguments
return zero, matching `wf_arg_word'; an explicit nil argument remains three.
Reject every other occurrence of `args', including bindings and escapes."
  (cond
   ((or (eq form 'args) (and mode (eq form mode)))
    (signal 'nelisp-argframe-ineligible (list form)))
   ((atom form) form)
   ((and (memq (car form) '(wf_arg_word wf_argval))
         (eq (cadr form) 'args) (integerp (caddr form))
         (>= (caddr form) 0) (null (cdddr form)))
    (let* ((index (caddr form))
           (word `(if (< ,index ,argc) (heap-read-u64 ,base ,(* index 8)) 0))
           (read (if (eq (car form) 'wf_argval) `(nl_word_payload ,word) word)))
      (if mode `(if (= ,mode 0) ,form ,read) read)))
   ((equal form '(m5_list_len args 0))
    (if mode `(if (= ,mode 0) ,form ,argc) argc))
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
