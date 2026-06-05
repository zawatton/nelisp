;;; standalone-source-normalize.el --- host-side standalone source rewrites  -*- lexical-binding: t; -*-

;;; Commentary:

;; Small source-to-source rewrites for feeding Emacs Lisp into the standalone
;; NeLisp reader from nelisp-emacs tooling.  These are intentionally kept here,
;; not in nelisp: they adapt Emacs compatibility forms to the current standalone
;; evaluator without changing the pure Elisp runtime boundary.

;;; Code:

(defvar standalone-source-normalize-cache-directory nil
  "Directory for cached normalized top-level source forms.
When nil, source normalization always reads the source file directly.")

(defconst standalone-source-normalize-cache-version 2
  "Cache format version for normalized standalone source forms.")

(defun standalone-source-normalize--setq-local (args)
  "Return an expanded form for `(setq-local . ARGS)'.
When ARGS is malformed, leave the original form intact so the normal runtime
error surface is preserved."
  (if (or (not (zerop (% (length args) 2)))
          (let ((rest args)
                bad)
            (while rest
              (unless (symbolp (car rest))
                (setq bad t))
              (setq rest (cddr rest)))
            bad))
      (cons 'setq-local args)
    (let (forms)
      (while args
        (let ((sym (car args))
              (val (cadr args)))
          (push (list 'set
                      (list 'make-local-variable (list 'quote sym))
                      (standalone-source-normalize-form val))
                forms))
        (setq args (cddr args)))
      (setq forms (nreverse forms))
      (if (cdr forms)
          (cons 'progn forms)
        (car forms)))))

(defun standalone-source-normalize--self-evaluating-p (form)
  "Return non-nil when FORM evaluates to itself."
  (or (null form)
      (eq form t)
      (keywordp form)
      (numberp form)
      (stringp form)))

(defun standalone-source-normalize--backquote-datum-expr (datum)
  "Return an expression that reconstructs backquoted DATUM."
  (cond
   ((vectorp datum)
    (cons 'vector
          (mapcar #'standalone-source-normalize--backquote-datum-expr
                  (append datum nil))))
   ((standalone-source-normalize--self-evaluating-p datum) datum)
   (t (list 'quote datum))))

(defun standalone-source-normalize--backquote-datum (datum)
  "Return DATUM rewritten for standalone evaluation inside backquote."
  (cond
   ((vectorp datum)
    (list '\,
          (standalone-source-normalize--backquote-datum-expr datum)))
   ((consp datum)
    (let ((head (car datum)))
      (cond
       ((or (eq head 'comma) (eq head '\,))
        (list head (standalone-source-normalize-form (cadr datum))))
       ((or (eq head 'comma-at) (eq head '\,@))
        (list head (standalone-source-normalize-form (cadr datum))))
       ((or (eq head 'backquote) (eq head '\`))
        (list head
              (standalone-source-normalize--backquote-datum (cadr datum))))
       (t
        (cons (standalone-source-normalize--backquote-datum (car datum))
              (standalone-source-normalize--backquote-datum (cdr datum)))))))
   (t datum)))

(defun standalone-source-normalize-form (form)
  "Return FORM rewritten for standalone NeLisp evaluation.
Quoted data is preserved.  Code positions are walked recursively."
  (cond
   ((consp form)
    (cond
     ((eq (car form) 'quote) form)
     ((or (eq (car form) 'backquote) (eq (car form) '\`))
      (list (car form)
            (standalone-source-normalize--backquote-datum (cadr form))))
     ((eq (car form) 'setq-local)
      (standalone-source-normalize--setq-local (cdr form)))
     (t
      (cons (standalone-source-normalize-form (car form))
            (standalone-source-normalize-form (cdr form))))))
   ((vectorp form)
    (apply #'vector
           (mapcar #'standalone-source-normalize-form (append form nil))))
   (t form)))

(defun standalone-source-normalize--expr-for-value (value)
  "Return an expression that evaluates to VALUE."
  (if (standalone-source-normalize--self-evaluating-p value)
      value
    (list 'quote value)))

(defun standalone-source-normalize--quoted-hash-table-defconst-p (form)
  "Return non-nil when FORM is `(defconst NAME '#s(hash-table ...))'."
  (and (consp form)
       (eq (car form) 'defconst)
       (symbolp (cadr form))
       (let ((value-form (caddr form)))
         (and (consp value-form)
              (eq (car value-form) 'quote)
              (hash-table-p (cadr value-form))))))

(defun standalone-source-normalize--hash-table-defconst-forms (form)
  "Return standalone forms for a quoted hash-table DEFCONST FORM.
Generated vendor files can contain very large `#s(hash-table ...)' literals.
The standalone reader/evaluator path is more stable when those are materialized
as many small `puthash' forms."
  (let* ((name (cadr form))
         (table (cadr (caddr form)))
         (test (hash-table-test table))
         (doc (nthcdr 3 form))
         (forms (list (append (list 'defconst
                                    name
                                    (list 'make-hash-table
                                          :test
                                          (list 'quote test)))
                              doc))))
    (maphash
     (lambda (key value)
       (push (list 'puthash
                   (standalone-source-normalize--expr-for-value key)
                   (standalone-source-normalize--expr-for-value value)
                   name)
             forms))
     table)
    (nreverse forms)))

(defun standalone-source-normalize-top-level-forms (form)
  "Return normalized standalone top-level forms for FORM."
  (if (standalone-source-normalize--quoted-hash-table-defconst-p form)
      (standalone-source-normalize--hash-table-defconst-forms form)
    (list (standalone-source-normalize-form form))))

(defun standalone-source-normalize-read-forms-from-file (file)
  "Return top-level forms from FILE, normalized for standalone NeLisp."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (goto-char (point-min))
      (condition-case err
          (while t
            (setq forms
                  (nconc forms
                         (standalone-source-normalize-top-level-forms
                          (read (current-buffer))))))
        (end-of-file nil)
        (error
         (error "cannot read %s: %S" file err)))
      forms)))

(defun standalone-source-normalize--file-state (file)
  "Return the cache-relevant source state for FILE."
  (let ((attrs (file-attributes file)))
    (list :truename (file-truename file)
          :mtime (nth 5 attrs)
          :size (nth 7 attrs))))

(defun standalone-source-normalize--cache-file (file)
  "Return the normalized-source cache path for FILE, or nil."
  (when standalone-source-normalize-cache-directory
    (expand-file-name
     (concat (secure-hash 'sha1 (file-truename file)) ".elcache")
     standalone-source-normalize-cache-directory)))

(defun standalone-source-normalize--cache-read (file)
  "Return cached normalized source strings for FILE, or nil on miss."
  (let ((cache-file (standalone-source-normalize--cache-file file))
        (state (standalone-source-normalize--file-state file)))
    (when (and cache-file (file-readable-p cache-file))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents cache-file)
            (let ((entry (read (current-buffer))))
              (when (and (consp entry)
                         (= (plist-get entry :version)
                            standalone-source-normalize-cache-version)
                         (equal (plist-get entry :state) state)
                         (listp (plist-get entry :forms)))
                (plist-get entry :forms))))
        (error nil)))))

(defun standalone-source-normalize--cache-write (file forms)
  "Write normalized source FORMS for FILE to the cache when enabled."
  (let ((cache-file (standalone-source-normalize--cache-file file)))
    (when cache-file
      (make-directory (file-name-directory cache-file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file cache-file
          (let ((print-escape-newlines t))
            (prin1 (list :version standalone-source-normalize-cache-version
                         :state (standalone-source-normalize--file-state file)
                         :forms forms)
                   (current-buffer))))))))

(defun standalone-source-normalize-form-to-string (form)
  "Return normalized FORM as standalone-readable source text."
  (with-temp-buffer
    (let ((print-escape-newlines t)
          ;; `nelisp--eval-source-string' does not yet read the `#'foo'
          ;; abbreviation consistently.  Print `(function foo)' instead.
          (print-quoted nil))
      (prin1 (standalone-source-normalize-form form) (current-buffer)))
    (buffer-string)))

(defun standalone-source-normalize-file-to-form-strings (file)
  "Return FILE as a list of normalized top-level source strings."
  (or (standalone-source-normalize--cache-read file)
      (let ((forms (mapcar #'standalone-source-normalize-form-to-string
                           (standalone-source-normalize-read-forms-from-file
                            file))))
        (standalone-source-normalize--cache-write file forms)
        forms)))

(defun standalone-source-normalize-file-to-string (file)
  "Return FILE as normalized standalone-readable source text."
  (with-temp-buffer
    (dolist (source (standalone-source-normalize-file-to-form-strings file))
      (insert source)
      (insert "\n"))
    (buffer-string)))

(defun standalone-source-normalize-source-to-progn-string (source)
  "Return SOURCE wrapped as one standalone-readable `progn' form.
The current standalone `nelisp--eval-source-string' development surface
evaluates one top-level form.  Diagnostic tools therefore pass one
explicit `progn' so every source form is evaluated without requiring a
runtime change in nelisp itself."
  (concat "(progn\n" source "\n)"))

(defun standalone-source-normalize-file-to-progn-string (file)
  "Return FILE as normalized source wrapped in one `progn' form."
  (standalone-source-normalize-source-to-progn-string
   (standalone-source-normalize-file-to-string file)))

(provide 'standalone-source-normalize)

;;; standalone-source-normalize.el ends here
