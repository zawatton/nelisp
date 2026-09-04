;;; subr-x.el --- lightweight standard subr-x facade for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Keep common vendor `(require 'subr-x)' paths on the small Layer-2
;; surface.  Most primitives here are normally preloaded from subr.el or
;; provided by subr-x.el in GNU Emacs; standalone NeLisp reaches them while
;; loading vendor files before a full dump/loaddefs image exists.

;;; Code:

(defconst subr-x--load-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that contains the subr-x facade and its sibling features.")

(defun subr-x--load-feature (feature)
  "Load FEATURE from the subr-x facade directory."
  (load (expand-file-name (concat (symbol-name feature) ".el")
                          subr-x--load-directory)
        nil t))

(subr-x--load-feature 'emacs-eval)
(subr-x--load-feature 'emacs-subr-extras)
(subr-x--load-feature 'emacs-string)
(subr-x--load-feature 'emacs-hash)
(subr-x--load-feature 'cl-lib)

(defun subr-x--define-p (symbol)
  "Return non-nil when SYMBOL should be supplied by this facade."
  (or (not (fboundp symbol))
      (and (fboundp 'autoloadp)
           (autoloadp (symbol-function symbol)))))

(when (subr-x--define-p 'internal--thread-argument)
  (defmacro internal--thread-argument (first &rest forms)
    "Internal implementation for `thread-first' and `thread-last'."
    (let ((value (car forms))
          (tail (cdr forms)))
      (while tail
        (let ((form (car tail)))
          (setq value
                (cond
                 ((consp form)
                  (if first
                      (cons (car form) (cons value (cdr form)))
                    (append form (list value))))
                 (first (list form value))
                 (t (list form value)))))
        (setq tail (cdr tail)))
      value)))

(when (subr-x--define-p 'thread-first)
  (defmacro thread-first (&rest forms)
    "Thread FORMS as the first argument through each successive form."
    (declare (indent 0))
    (cons 'internal--thread-argument (cons t forms))))

(when (subr-x--define-p 'thread-last)
  (defmacro thread-last (&rest forms)
    "Thread FORMS as the last argument through each successive form."
    (declare (indent 0))
    (cons 'internal--thread-argument (cons nil forms))))

(when (subr-x--define-p 'named-let)
  (defmacro named-let (name bindings &rest body)
    "Looping let form named NAME with BINDINGS and BODY."
    (declare (indent 2))
    (let ((vars nil)
          (vals nil))
      (dolist (binding bindings)
        (push (car binding) vars)
        (push (cadr binding) vals))
      (setq vars (nreverse vars)
            vals (nreverse vals))
      `(cl-labels ((,name ,vars ,@body))
         (,name ,@vals)))))

(defun hash-table-empty-p (hash-table)
  "Return non-nil when HASH-TABLE has no entries."
  (= (hash-table-count hash-table) 0))

(defun hash-table-keys (hash-table)
  "Return a list of HASH-TABLE keys."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) hash-table)
    keys))

(defun hash-table-values (hash-table)
  "Return a list of HASH-TABLE values."
  (let (values)
    (maphash (lambda (_key value) (push value values)) hash-table)
    values))

(when (subr-x--define-p 'string-remove-prefix)
  (defun string-remove-prefix (prefix string)
    "Remove PREFIX from STRING when present."
    (if (string-prefix-p prefix string)
        (substring string (length prefix))
      string)))

(when (subr-x--define-p 'string-remove-suffix)
  (defun string-remove-suffix (suffix string)
    "Remove SUFFIX from STRING when present."
    (if (string-suffix-p suffix string)
        (substring string 0 (- (length string) (length suffix)))
      string)))

(when (subr-x--define-p 'string-replace)
  (defun string-replace (from-string to-string in-string)
    "Replace all non-overlapping FROM-STRING matches with TO-STRING."
    (if (= (length from-string) 0)
        in-string
      (let ((start 0)
            (pieces nil)
            pos)
        (while (setq pos (string-search from-string in-string start))
          (push (substring in-string start pos) pieces)
          (push to-string pieces)
          (setq start (+ pos (length from-string))))
        (push (substring in-string start) pieces)
        (apply #'concat (nreverse pieces))))))

(when (subr-x--define-p 'string-truncate-left)
  (defun string-truncate-left (string length)
    "If STRING is longer than LENGTH, truncate it from the left."
    (if (<= (length string) length)
        string
      (let ((keep (max 0 (- length 3))))
        (concat "..." (substring string (- (length string) keep)))))))

(when (subr-x--define-p 'string-limit)
  (defun string-limit (string length &optional end _coding-system)
    "Return up to LENGTH characters from STRING.
When END is non-nil, keep the last LENGTH characters."
    (unless (and (integerp length) (>= length 0))
      (signal 'wrong-type-argument (list 'natnump length)))
    (cond
     ((<= (length string) length) string)
     (end (substring string (- (length string) length)))
     (t (substring string 0 length)))))

(when (subr-x--define-p 'string-pad)
  (defun string-pad (string length &optional padding start)
    "Pad STRING to LENGTH using PADDING.
When START is non-nil, pad on the left."
    (unless (and (integerp length) (>= length 0))
      (signal 'wrong-type-argument (list 'natnump length)))
    (let ((pad-length (- length (length string))))
      (if (<= pad-length 0)
          string
        (let ((pad (make-string pad-length (or padding ?\s))))
          (if start (concat pad string) (concat string pad)))))))

(when (subr-x--define-p 'string-chop-newline)
  (defun string-chop-newline (string)
    "Remove STRING's final newline, when present."
    (string-remove-suffix "\n" string)))

(when (subr-x--define-p 'proper-list-p)
  (defun proper-list-p (object)
    "Return OBJECT's list length when it is a proper list, else nil."
    (let ((slow object)
          (fast object)
          (len 0)
          (done nil)
          result)
      (while (not done)
        (cond
         ((null fast)
          (setq result len
                done t))
         ((not (consp fast))
          (setq done t))
         ((null (cdr fast))
          (setq result (1+ len)
                done t))
         ((not (consp (cdr fast)))
          (setq done t))
         (t
          (setq slow (cdr slow)
                fast (cdr (cdr fast))
                len (+ len 2))
          (when (eq slow fast)
            (setq done t)))))
      result)))

(when (subr-x--define-p 'mapcan)
  (defun mapcan (function sequence &rest more-sequences)
    "Apply FUNCTION across SEQUENCE and concatenate the list results."
    (let ((results (apply #'mapcar function sequence more-sequences)))
      (apply #'nconc results))))

(provide 'subr-x)

;;; subr-x.el ends here
