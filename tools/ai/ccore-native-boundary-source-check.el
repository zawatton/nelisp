;;; ccore-native-boundary-source-check.el --- Check native boundary DSL -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l tools/ai/ccore-native-boundary-source-check.el
;; Optional override: NELISP_BOUNDARY_SOURCE=/path/to/generator.el. Relative
;; paths are resolved from this script's directory.

(require 'cl-lib)
(require 'seq)

(defconst ccore-native-boundary-source-check--script-dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst ccore-native-boundary-source-check--source
  (expand-file-name
   (or (getenv "NELISP_BOUNDARY_SOURCE")
       "../../scripts/nelisp-standalone-build.el")
   ccore-native-boundary-source-check--script-dir))

(defun ccore-native-boundary-source-check--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (point-max)) (< (point) (point-max)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))

(defun ccore-native-boundary-source-check--const (forms name)
  (let ((matches
         (seq-filter (lambda (form)
                       (and (eq (car-safe form) 'defconst)
                            (eq (cadr form) name)))
                     forms)))
    (unless (= (length matches) 1)
      (error "Expected one defconst %s; found %d" name (length matches)))
    (caddr (car matches))))

(defun ccore-native-boundary-source-check--quoted-value (form)
  (if (and (consp form) (eq (car form) 'quote))
      (cadr form)
    (error "Expected quoted DSL constant")))

(defun ccore-native-boundary-source-check--find-arms (tree name)
  (let (result)
    (cl-labels ((walk (node)
                  (when (consp node)
                    (if (and (consp (car node))
                             (eq (caar node) :lit)
                             (equal (cadar node) name))
                        (push node result)
                      (walk (car node))
                      (walk (cdr node))))))
      (walk tree))
    (nreverse result)))

(defun ccore-native-boundary-source-check--collect-defuns (tree)
  (let (result)
    (cl-labels ((walk (node)
                  (when (consp node)
                    (when (and (eq (car node) 'defun)
                               (symbolp (cadr node))
                               (listp (caddr node)))
                      (push (cons (cadr node) (length (caddr node))) result))
                    (walk (car node))
                    (walk (cdr node)))))
      (walk tree))
    result))

(defun ccore-native-boundary-source-check--walk-body (tree signatures)
  (cond
   ((atom tree) nil)
   ((eq (car tree) 'quote) nil)
   ((eq (car tree) 'let*)
    (unless (and (listp (cadr tree)) (>= (length tree) 3))
      (error "Malformed let* form: %S" tree))
    (dolist (binding (cadr tree))
      (unless (and (listp binding) (<= 1 (length binding) 2))
        (error "Malformed let* binding: %S" binding))
      (when (cadr binding)
        (ccore-native-boundary-source-check--walk-body
         (cadr binding) signatures)))
    (dolist (body (cddr tree))
      (ccore-native-boundary-source-check--walk-body body signatures)))
   ((eq (car tree) 'if)
    (unless (= (length tree) 4) (error "Malformed if form: %S" tree))
    (dolist (form (cdr tree))
      (ccore-native-boundary-source-check--walk-body form signatures)))
   ((eq (car tree) 'seq)
    (dolist (form (cdr tree))
      (ccore-native-boundary-source-check--walk-body form signatures)))
   ;; These local arity rules mirror the compiler source: cmp-ops lists = and
   ;; /= at lisp/nelisp-aot-compiler.el:800 and the two-operand branch checks
   ;; length 3 at :9794; ptr-read-u64 checks length 3 at :11196. This bounded
   ;; checker records the audited contracts; it does not execute the compiler.
   ((memq (car tree) '(= /=))
    (unless (= (length tree) 3)
      (error "Compiler intrinsic %s expects 2 args; found %d"
             (car tree) (1- (length tree))))
    (dolist (form (cdr tree))
      (ccore-native-boundary-source-check--walk-body form signatures)))
   ((eq (car tree) 'ptr-read-u64)
    (unless (= (length tree) 3)
      (error "Compiler intrinsic ptr-read-u64 expects 2 args; found %d"
             (1- (length tree))))
    (dolist (form (cdr tree))
      (ccore-native-boundary-source-check--walk-body form signatures)))
   ((symbolp (car tree))
    (let* ((signature (assq (car tree) signatures))
           (arity (cdr signature)))
      (unless signature (error "Unresolved arm helper %s" (car tree)))
      (unless (= (length (cdr tree)) arity)
        (error "Helper %s expects %d args; found %d"
               (car tree) arity (length (cdr tree))))
      (dolist (arg (cdr tree))
        (ccore-native-boundary-source-check--walk-body arg signatures))))
   (t
    (dolist (form tree)
      (ccore-native-boundary-source-check--walk-body form signatures)))))

(defun ccore-native-boundary-source-check--run ()
  (unless (file-readable-p ccore-native-boundary-source-check--source)
    (error "Generator is not readable: %s"
           ccore-native-boundary-source-check--source))
  (let* ((forms (ccore-native-boundary-source-check--read-file
                 ccore-native-boundary-source-check--source))
         (table (ccore-native-boundary-source-check--quoted-value
                 (ccore-native-boundary-source-check--const
                  forms 'nelisp-standalone--applyfn-dispatch-table)))
         (helper-constants
          '(nelisp-standalone--applyfn-core-helpers
            nelisp-standalone--applyfn-ht-helpers
            nelisp-standalone--applyfn-m5-helpers
            nelisp-standalone--applyfn-bf-helpers))
         (definitions
          (apply #'append
                 (mapcar
                  (lambda (constant)
                    (ccore-native-boundary-source-check--collect-defuns
                     (ccore-native-boundary-source-check--quoted-value
                      (ccore-native-boundary-source-check--const forms constant))))
                  helper-constants)))
         (names '("puthash" "remhash" "string-bytes")))
    (dolist (definition definitions)
      (let ((name (car definition)) (arity (cdr definition)))
        (when (and (assq name definitions)
                   (/= arity (cdr (assq name definitions))))
          (error "Conflicting helper signature for %s" name))))
    (dolist (name names)
      (let ((arms (ccore-native-boundary-source-check--find-arms table name)))
        (unless (= (length arms) 1)
          (error "Expected one %s arm; found %d" name (length arms)))
        (let* ((arm (car arms))
               (key (car arm))
               (body (cdr arm)))
          (unless (and (listp key) (= (length key) 2)
                       (eq (car key) :lit)
                       (equal (cadr key) name)
                       (consp body) (listp body)
                       (eq (car body) 'let*))
            (error "Expected literal (:lit %s) key and nonempty let* body"
                   name))
        (ccore-native-boundary-source-check--walk-body
         body definitions))))
    (princ (format "ccore-native-boundary-source-check: PASS (%s)\n"
                   ccore-native-boundary-source-check--source))))

(ccore-native-boundary-source-check--run)

;;; ccore-native-boundary-source-check.el ends here
