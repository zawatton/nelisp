;;; emacs-cc-chartab-1.el --- Unicode property char-table primitives -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-chartab-1--tables nil)
(defvar emacs-cc-chartab-1--properties nil)

(defun emacs-cc-chartab-1--property-name (char-table)
  (car (rassq char-table emacs-cc-chartab-1--properties)))

(defun emacs-cc-chartab-1--valid-char-table-p (table)
  (or (and (fboundp 'char-table-p) (char-table-p table))
      (and (fboundp 'emacs-char-table-p) (emacs-char-table-p table))))

(defun emacs-cc-chartab-1--property-table (table)
  (cdr (assq table emacs-cc-chartab-1--tables)))

(unless (fboundp 'get-unicode-property-internal)
  (defun get-unicode-property-internal (char-table ch)
    "Return an element of CHAR-TABLE for character CH; CHAR-TABLE must be what returned by `unicode-property-table-internal'."
    (unless (emacs-cc-chartab-1--valid-char-table-p char-table)
      (signal 'wrong-type-argument (list 'char-table-p char-table)))
    (unless (integerp ch)
      (signal 'wrong-type-argument (list 'integerp ch)))
    (let ((table (emacs-cc-chartab-1--property-table char-table)))
      (let ((prop (emacs-cc-chartab-1--property-name char-table)))
        (or (and table (gethash ch table))
            (when (and prop (fboundp 'get-char-code-property))
              (get-char-code-property ch prop))
            (when (eq prop 'general-category)
              (cond ((and (>= ch 48) (<= ch 57)) 'Nd)
                    ((and (>= ch 65) (<= ch 90)) 'Lu)
                    ((and (>= ch 97) (<= ch 122)) 'Ll))))))))

(unless (fboundp 'put-unicode-property-internal)
  (defun put-unicode-property-internal (char-table ch value)
    "Set an element of CHAR-TABLE for character CH to VALUE; CHAR-TABLE must be what returned by `unicode-property-table-internal'."
    (unless (emacs-cc-chartab-1--valid-char-table-p char-table)
      (signal 'wrong-type-argument (list 'char-table-p char-table)))
    (unless (integerp ch)
      (signal 'wrong-type-argument (list 'integerp ch)))
    (when (and (eq (emacs-cc-chartab-1--property-name char-table) 'general-category)
               (not (memq value '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn))))
      (signal 'wrong-type-argument (list "Unicode property value" value)))
    (let ((table (emacs-cc-chartab-1--property-table char-table)))
      (if table (puthash ch value table)
        (if (fboundp 'emacs-char-table-set)
            (emacs-char-table-set char-table ch value)
          (set-char-table-range char-table ch value))))))

(unless (fboundp 'optimize-char-table)
  (defun optimize-char-table (char-table &optional test)
    "Optimize CHAR-TABLE. TEST is the comparison function used to decide whether two entries are equivalent and can be merged. It defaults to `equal'."
    (unless (emacs-cc-chartab-1--valid-char-table-p char-table)
      (signal 'wrong-type-argument (list 'char-table-p char-table)))
    (unless (or (null test) (functionp test))
      (signal 'wrong-type-argument (list 'functionp test)))
    nil))

;; The prelude optimizer operates on NeLisp's native char-table storage.
;; The emacs-char-table package also exposes tagged Lisp-owned tables, for
;; which the default optimizer is a logical no-op: it may compact native
;; storage, but must not change any character's value.  Keep the previous
;; optimizer for native tables and foreign calls.  Owned tables with an
;; explicit comparator use their Lisp topology so partial mutations and
;; cache publication remain visible if the comparator signals.
(defvar emacs-cc-chartab-1--previous-optimize-char-table
  (and (fboundp 'optimize-char-table)
       (symbol-function 'optimize-char-table)))

(when (and (fboundp 'nelisp--chartab-standalone-p)
           (nelisp--chartab-standalone-p)
           emacs-cc-chartab-1--previous-optimize-char-table)
  (defalias 'optimize-char-table
    (lambda (char-table &optional test)
      "Optimize CHAR-TABLE, preserving owned-table mutation semantics.
TEST defaults to `equal'; custom owned-table calls use its topology helper."
      (if (and (fboundp 'emacs-char-table--owned-p)
               (emacs-char-table--owned-p char-table))
          (if (null test)
              nil
            (if (fboundp 'emacs-char-table--owner-optimize)
                (emacs-char-table--owner-optimize char-table test)
              (funcall emacs-cc-chartab-1--previous-optimize-char-table
                       char-table test)))
        (funcall emacs-cc-chartab-1--previous-optimize-char-table
                 char-table test)))))

(unless (fboundp 'unicode-property-table-internal)
  (defun unicode-property-table-internal (prop)
    "Return a char-table for Unicode character property PROP; use `get-unicode-property-internal' and `put-unicode-property-internal' instead of `aref' and `aset' to get and put an element value."
    (let ((entry (assq prop emacs-cc-chartab-1--properties)))
      (or (cdr entry)
          (when (and (symbolp prop)
                     (or (eq prop 'general-category)
                         (and (boundp 'char-code-property-alist)
                              (assq prop char-code-property-alist))))
            (let ((table (if (fboundp 'make-char-table)
                             (make-char-table 'char-code-property-table)
                           (emacs-char-table-make 'char-code-property-table))))
              (push (cons prop table) emacs-cc-chartab-1--properties)
              (push (cons table (make-hash-table :test 'eql)) emacs-cc-chartab-1--tables)
              table))))))

(provide 'emacs-cc-chartab-1)
