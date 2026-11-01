;;; nl-typecheck-entry.el --- Doc 236 typed-boundary entry checks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 3 (Stage 3a).  A file whose first line carries the
;; `nelisp-typed: t' cookie is loaded through `nl-typecheck-entry-load',
;; which rewrites every `defun' with a declared ftype so that ordinary
;; code at the head of its body checks the typed arguments and signals
;; (wrong-type-argument PREDICATE VALUE).  The checks run only while
;; `nelisp-typecheck-enforce' is non-nil.  The check is part of the
;; function body, so `fset' replaces it and advice wraps it.

;;; Code:

(require 'nl-typecheck)

(defvar nelisp-typecheck-enforce (and (fboundp 'nelisp--repr) t)
  "Non-nil enables entry checks; on by default under NeLisp only.")

(defvar nl-typecheck-entry-last-blame nil
  "Plist (:callee :param :expected :actual-type) of the last entry failure.")

(defun nl-typecheck-entry--test (type v)
  "Return a form testing that variable V has TYPE, or :none if unchecked."
  (cond
   ((memq type '(t any)) :none)
   ((symbolp type)
    (let ((p (rassq type nl-typecheck--predicates)))
      (if p (list (car p) v) :none)))
   ((and (consp type) (eq (car type) 'or))
    (let ((tests nil) (ok t))
      (dolist (a (cdr type))
        (let ((x (nl-typecheck-entry--test a v)))
          (if (eq x :none) (setq ok nil) (setq tests (cons x tests)))))
      (if (and ok tests) (cons 'or (nreverse tests)) :none)))
   ((and (consp type) (eq (car type) 'member))
    (list 'memql v (list 'quote (cdr type))))
   ((and (consp type) (eq (car type) 'integer))
    (let ((lo (nth 1 type)) (hi (nth 2 type)) (tests (list (list 'integerp v))))
      (when (integerp lo) (setq tests (cons (list '>= v lo) tests)))
      (when (integerp hi) (setq tests (cons (list '<= v hi) tests)))
      (cons 'and (nreverse tests))))
   ((and (consp type) (eq (car type) 'satisfies) (symbolp (nth 1 type)))
    (list (nth 1 type) v))
   (t :none)))

(defun nl-typecheck-entry--predicate (type test v)
  "Return the predicate datum for TYPE: a natural symbol or a lambda."
  (let ((p (and (symbolp type) (rassq type nl-typecheck--predicates))))
    (if p (car p)
      (list 'lambda (list 'x)
            (nl-typecheck-entry--subst test v 'x)))))

(defun nl-typecheck-entry--subst (form from to)
  "Replace symbol FROM by TO in FORM."
  (cond ((eq form from) to)
        ((consp form) (cons (nl-typecheck-entry--subst (car form) from to)
                            (nl-typecheck-entry--subst (cdr form) from to)))
        (t form)))

(defun nl-typecheck-entry-fail (callee param type pred value)
  "Record the blame for CALLEE's PARAM and signal wrong-type-argument."
  (setq nl-typecheck-entry-last-blame
        (list :callee callee :param param :expected type
              :actual-type (condition-case nil (type-of value) (error 'unknown))))
  (signal 'wrong-type-argument (list pred value)))

(defun nl-typecheck-entry--checks (name arglist sig)
  "Return the entry-check forms for NAME with ARGLIST and ftype SIG."
  (let ((mode 'req) (i 0) (checks nil))
    (dolist (p arglist)
      (cond
       ((eq p '&optional) (setq mode 'opt))
       ((eq p '&rest) (setq mode 'rest))
       ((eq mode 'rest))
       (t
        (let ((type (nl-typecheck--param-type (car (cdr sig)) i)))
          (setq i (1+ i))
          (unless (eq type :none)
            (let ((test (nl-typecheck-entry--test type p)))
              (unless (eq test :none)
                (when (eq mode 'opt) (setq test (list 'or (list 'null p) test)))
                (setq checks
                      (cons (list 'or test
                                  (list 'nl-typecheck-entry-fail
                                        (list 'quote name) (list 'quote p)
                                        (list 'quote type)
                                        (list 'quote (nl-typecheck-entry--predicate
                                                      type test p))
                                        p))
                            checks)))))))))
    (nreverse checks)))

(defun nl-typecheck-entry-transform (form)
  "Return FORM with entry checks inserted when it is a typed `defun'."
  (cond
   ((and (consp form) (eq (car form) 'progn))
    (cons 'progn (mapcar #'nl-typecheck-entry-transform (cdr form))))
   ((and (consp form) (eq (car form) 'defun) (consp (cdr form))
         (consp (cdr (cdr form))))
    (let* ((name (nth 1 form)) (arglist (nth 2 form)) (body (nthcdr 3 form))
           (head nil) (sig nil))
      (when (and (stringp (car body)) (cdr body))
        (setq head (list (car body)) body (cdr body)))
      (while (and (consp (car body)) (memq (car (car body)) '(declare interactive)))
        (let ((f (car body)))
          (if (eq (car f) 'declare)
              (let ((keep nil))
                (dolist (spec (cdr f))
                  (if (and (consp spec) (eq (car spec) 'ftype))
                      (setq sig (car (nl-typecheck--ftype (list (list 'declare spec)))))
                    (setq keep (cons spec keep))))
                (when keep (setq head (append head (list (cons 'declare (nreverse keep)))))))
            (setq head (append head (list f)))))
        (setq body (cdr body)))
      (let ((checks (and sig (nl-typecheck-entry--checks name arglist sig))))
        (if (null checks)
            (cons 'defun (cons name (cons arglist (append head body))))
          (cons 'defun
                (cons name
                      (cons arglist
                            (append head
                                    (list (cons 'when (cons 'nelisp-typecheck-enforce checks)))
                                    body))))))))
   (t form)))

(defun nl-typecheck-entry-typed-file-p (file)
  "Non-nil when FILE's first line carries the nelisp-typed cookie."
  (with-temp-buffer
    (insert-file-contents file nil 0 512)
    (goto-char (point-min))
    (let ((line (buffer-substring (point) (line-end-position))))
      (and (string-match "-\\*-.*nelisp-typed:[ \t]*t[ \t;].*-\\*-" line) t))))

(defun nl-typecheck-entry-load (file)
  "Load FILE; in a nelisp-typed file add entry checks to typed defuns."
  (if (not (nl-typecheck-entry-typed-file-p file))
      (load file nil t)
    (let ((read (nl-typecheck--read-forms file)))
      (unwind-protect
          (dolist (f (car read))
            (eval (nl-typecheck-entry-transform (car f)) t))
        (kill-buffer (car (cdr read)))))
    t))

(provide 'nl-typecheck-entry)
;;; nl-typecheck-entry.el ends here
