;;; nl-typecheck-repl.el --- Doc 236 REPL layers 1-2 -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 1: check each REPL input against an accumulated signature
;; environment before it is evaluated.  Layer 2: when a definition's
;; signature changes, re-check the recorded callers and report only the
;; call sites the new signature breaks.  Plain Elisp (no cl-lib) so it
;; loads on the standalone binary.  The standalone REPL calls
;; `nelisp-repl-input-check-function' (see scripts/nelisp-stdlib-prelude.el)
;; with each input form; nil means "unhooked".

;;; Code:

(require 'nl-typecheck)

(defvar nelisp-repl-input-check-function nil
  "When non-nil, called with each REPL input form; non-nil result evaluates it.")

(defvar nelisp-typecheck-repl-mode nil
  "Non-nil while `nelisp-typecheck-repl-mode' is enabled.")

(define-error 'nl-typecheck-rejected "Input rejected by the type checker")

(defvar nl-typecheck-repl--sigs nil "Accumulated signatures (symbol -> type).")
(defvar nl-typecheck-repl--defs nil "Accumulated defined-function symbols.")
(defvar nl-typecheck-repl--callers nil
  "Hash: callee -> list of (OWNER LABEL LINE FORM) caller records.")
(defvar nl-typecheck-repl--count 0 "Number of REPL inputs seen.")
(defvar nl-typecheck-repl-last-redef nil
  "Diagnostics reported for broken callers by the last redefinition.")

(defun nl-typecheck-repl-reset ()
  "Forget the accumulated environment, caller map and input counter."
  (setq nl-typecheck-repl--sigs (make-hash-table :test 'eq)
        nl-typecheck-repl--defs (make-hash-table :test 'eq)
        nl-typecheck-repl--callers (make-hash-table :test 'eq)
        nl-typecheck-repl--count 0
        nl-typecheck-repl-last-redef nil))

(defun nl-typecheck-repl--ensure ()
  "Create the accumulated environment on first use."
  (unless nl-typecheck-repl--sigs (nl-typecheck-repl-reset)))

(defun nl-typecheck-repl--def-names (form)
  "Return the function names FORM defines or declares a type for."
  (let ((head (and (consp form) (car form))))
    (cond
     ((and (memq head '(defun defmacro defsubst nl-defun))
           (consp (cdr form)) (symbolp (car (cdr form))))
      (list (car (cdr form))))
     ((and (eq head 'nl-typed) (symbolp (nth 2 form))) (list (nth 2 form)))
     ((and (eq head 'nelisp-declare-type) (consp (cdr form)))
      (let ((sym (car (cdr form))))
        (when (and (consp sym) (eq (car sym) 'quote)) (setq sym (car (cdr sym))))
        (and (symbolp sym) sym (list sym))))
     ((eq head 'progn)
      (let ((acc nil))
        (dolist (f (cdr form))
          (setq acc (append acc (nl-typecheck-repl--def-names f))))
        acc)))))

(defun nl-typecheck-repl--definition-p (form)
  "Non-nil when FORM defines a function whose body can be re-checked."
  (and (consp form) (memq (car form) '(defun nl-typed))
       (nl-typecheck-repl--def-names form)))

(defun nl-typecheck-repl--called (form)
  "Return the distinct symbols in function position anywhere in FORM."
  (let ((acc nil) (stack (list form)))
    (while stack
      (let ((x (car stack)))
        (setq stack (cdr stack))
        (when (consp x)
          (when (and (symbolp (car x)) (car x) (not (eq (car x) 'quote))
                     (not (memq (car x) acc)))
            (setq acc (cons (car x) acc)))
          (unless (eq (car x) 'quote)
            (while (consp x)
              (setq stack (cons (car x) stack))
              (setq x (cdr x)))))))
    acc))

(defun nl-typecheck-repl--run (form label line)
  "Check FORM in the accumulated environment; return its diagnostics.
Diagnostics are reported against LABEL and LINE."
  (let* ((nl-typecheck--sigs nl-typecheck-repl--sigs)
         (nl-typecheck--defs nl-typecheck-repl--defs)
         (nl-typecheck--assigned (make-hash-table :test 'eq))
         (nl-typecheck--occ (make-hash-table :test 'eq))
         (nl-typecheck--diags nil)
         (nl-typecheck--summary nil)
         (nl-typecheck--extra nil)
         (nl-typecheck--file label)
         (nl-typecheck--src (generate-new-buffer " *nl-typecheck-repl*")))
    (unwind-protect
        (progn
          (with-current-buffer nl-typecheck--src
            (let ((print-length nil) (print-level nil))
              (prin1 form nl-typecheck--src)))
          (nl-typecheck--note-defs form)
          (nl-typecheck--process-forms
           (list (list form 1 (with-current-buffer nl-typecheck--src
                                (point-max)))))
          (mapcar (lambda (d) (plist-put d :line line))
                  (nreverse nl-typecheck--diags)))
      (kill-buffer nl-typecheck--src))))

(defun nl-typecheck-repl--errors (diags)
  "Return the error-level members of DIAGS."
  (let ((acc nil))
    (dolist (d diags) (when (eq (plist-get d :level) 'error) (setq acc (cons d acc))))
    (nreverse acc)))

(defun nl-typecheck-repl--print (diags)
  "Print DIAGS in gate format."
  (dolist (d diags)
    (princ (concat (nl-typecheck-format-diagnostic d) "\n"))))

(defun nl-typecheck-repl--forget-owner (owner)
  "Drop every caller record owned by OWNER."
  (maphash (lambda (callee recs)
             (let ((keep nil))
               (dolist (r recs) (unless (eq (car r) owner) (setq keep (cons r keep))))
               (puthash callee (nreverse keep) nl-typecheck-repl--callers)))
           nl-typecheck-repl--callers))

(defun nl-typecheck-repl--record-callers (form label line)
  "Record FORM, when a definition, as a caller of the functions it mentions."
  (let ((names (nl-typecheck-repl--definition-p form)))
    (when names
      (let ((owner (car names)))
        (nl-typecheck-repl--forget-owner owner)
        (dolist (c (nl-typecheck-repl--called form))
          (unless (eq c owner)
            (puthash c (cons (list owner label line form)
                             (gethash c nl-typecheck-repl--callers))
                     nl-typecheck-repl--callers)))))))

(defun nl-typecheck-repl--recheck (callee names)
  "Re-check recorded callers of CALLEE; return diagnostics about CALLEE.
Callers defined by the current input (NAMES) are skipped."
  (let ((out nil))
    (dolist (r (reverse (gethash callee nl-typecheck-repl--callers)))
      (unless (memq (car r) names)
        (dolist (d (nl-typecheck-repl--run (nth 3 r) (nth 1 r) (nth 2 r)))
          (when (and (eq (plist-get d :callee) callee)
                     (memq (plist-get d :level) '(error warning)))
            (setq out (cons (plist-put (copy-sequence d) :message
                                       (format "%s [caller %S, after redefinition of %S]"
                                               (plist-get d :message)
                                               (car r) callee))
                            out))))))
    (nreverse out)))

(defun nl-typecheck-repl--restore (saved)
  "Restore the SAVED (NAME OLD-SIG . OLD-DEF) environment entries."
  (dolist (s saved)
    (if (car (cdr s))
        (puthash (car s) (car (cdr s)) nl-typecheck-repl--sigs)
      (remhash (car s) nl-typecheck-repl--sigs))
    (if (cdr (cdr s))
        (puthash (car s) t nl-typecheck-repl--defs)
      (remhash (car s) nl-typecheck-repl--defs))))

(defun nl-typecheck-repl--process (form label line reject)
  "Check FORM, record it, and re-check callers of changed signatures.
When REJECT is non-nil and FORM has errors the environment is left
unchanged and (:rejected . ERRORS) is returned; otherwise the result
is (:ok . DIAGS)."
  (nl-typecheck-repl--ensure)
  (let* ((names (nl-typecheck-repl--def-names form))
         (saved (mapcar (lambda (n)
                          (cons n (cons (gethash n nl-typecheck-repl--sigs)
                                        (gethash n nl-typecheck-repl--defs))))
                        names)))
    (dolist (n names) (remhash n nl-typecheck-repl--sigs))
    (let* ((diags (nl-typecheck-repl--run form label line))
           (errs (nl-typecheck-repl--errors diags)))
      (cond
       ((and reject errs)
        (nl-typecheck-repl--restore saved)
        (nl-typecheck-repl--print diags)
        (cons :rejected errs))
       (t
        (nl-typecheck-repl--print diags)
        (nl-typecheck-repl--record-callers form label line)
        (setq nl-typecheck-repl-last-redef nil)
        (dolist (s saved)
          (unless (equal (car (cdr s)) (gethash (car s) nl-typecheck-repl--sigs))
            (let ((broken (nl-typecheck-repl--recheck (car s) names)))
              (nl-typecheck-repl--print broken)
              (setq nl-typecheck-repl-last-redef
                    (append nl-typecheck-repl-last-redef broken)))))
        (cons :ok diags))))))

(defun nl-typecheck-repl--blame (err)
  "Return the blame plist for the error diagnostic ERR."
  (list :callee (plist-get err :callee) :param (plist-get err :param)
        :expected (plist-get err :expected)
        :actual-type (plist-get err :actual-type)))

(defun nl-typecheck-repl-check-input (form)
  "Check one REPL input FORM; return non-nil when it should be evaluated.
Under `nelisp-typecheck-strict' an input with an error diagnostic is
refused by signalling `nl-typecheck-rejected'."
  (nl-typecheck-repl--ensure)
  (setq nl-typecheck-repl--count (1+ nl-typecheck-repl--count))
  (let ((res (nl-typecheck-repl--process
              form "<repl>" nl-typecheck-repl--count nelisp-typecheck-strict)))
    (when (eq (car res) :rejected)
      (princ (format "nl-typecheck: input %d refused (%d type error%s)\n"
                     nl-typecheck-repl--count (length (cdr res))
                     (if (cdr (cdr res)) "s" "")))
      (signal 'nl-typecheck-rejected
              (list (format "input %d refused by type check" nl-typecheck-repl--count)
                    (nl-typecheck-repl--blame (car (cdr res))))))
    t))

(defun nl-typecheck-repl-load-file (path)
  "Check the forms of PATH into the accumulated environment; return error count."
  (nl-typecheck-repl--ensure)
  (let* ((read (nl-typecheck--read-forms path)) (errors 0))
    (unwind-protect
        (dolist (f (car read))
          (let ((line (with-current-buffer (car (cdr read))
                        (line-number-at-pos (nth 1 f)))))
            (let ((res (nl-typecheck-repl--process (car f) path line nil)))
              (setq errors (+ errors (length (nl-typecheck-repl--errors (cdr res))))))))
      (kill-buffer (car (cdr read))))
    errors))

(defun nelisp-typecheck-repl-mode (&optional arg)
  "Toggle the REPL input check; ARG > 0 enables, ARG <= 0 disables."
  (setq nelisp-typecheck-repl-mode
        (cond ((null arg) (not nelisp-typecheck-repl-mode))
              ((and (numberp arg) (<= arg 0)) nil)
              (t t)))
  (setq nelisp-repl-input-check-function
        (and nelisp-typecheck-repl-mode #'nl-typecheck-repl-check-input))
  nelisp-typecheck-repl-mode)

(provide 'nl-typecheck-repl)
;;; nl-typecheck-repl.el ends here
