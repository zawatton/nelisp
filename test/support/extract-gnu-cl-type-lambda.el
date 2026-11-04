;;; extract-type-closure.el --- Authenticated GNU type/lambda expansion -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'cl-macs)
(require 'inline)
(require 'macroexp)
(unless (equal emacs-version "31.1")
  (error "GNU31 expansion owner required"))
(let* ((source (expand-file-name "cl-macs.el.gz" (getenv "GNU_CL_SOURCE_ROOT")))
       (names '(cl-function cl--transform-lambda cl--slet cl--slet*
                cl--const-expr-p cl--const-expr-val
                cl--make-usage-var cl--make-usage-args cl--do-&aux
                cl--do-arglist cl--arglist-args cl--pop2
                cl--lambda-list-keywords cl--optimize-safety
                cl--bind-block cl--bind-defs cl--bind-enquote
                cl--bind-lets cl--bind-forms cl-typep))
       forms counts help-forms help-counts (adapted 0))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally source)
    (unless (equal (secure-hash 'sha256 (current-buffer))
                   "5fe90ef193a26d47d226507c5f624b85e4116b2f724af9aa8e98bbfed2861043")
      (error "GNU cl-macs source pin mismatch")))
  ;; Load authenticated expansion owners rather than inheriting their cells
  ;; from whatever byte-compiled libraries the host happened to preload.
  (dolist (spec '(("pcase" . "f4c2355bb662e79827f83f609ce50be2780d20ced59bb0dbd916a73c3a82a9d1")
                  ("macroexp" . "6f13853ee439eda2371eb16657d0a1cee642f951c1698409da9e95ba20ca02db")
                  ("inline" . "a7267d40d188adc88bf050099912b48fe95b60df286e66b92b7e525a53d23fb5")))
    (let ((owner (expand-file-name (concat (car spec) ".el.gz")
                                   (getenv "GNU_CL_SOURCE_ROOT"))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally owner)
        (unless (equal (secure-hash 'sha256 (current-buffer)) (cdr spec))
          (error "GNU %s expander source pin mismatch" (car spec))))
      (load owner nil t)))
  (load source nil t)
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (memq (car-safe form)
                             '(defun defmacro defvar defconst define-inline))
                       (memq (cadr form) names))
              (let ((entry (assq (cadr form) counts)))
                (if entry (setcdr entry (1+ (cdr entry)))
                  (push (cons (cadr form) 1) counts)))
              (push form forms))))
      (end-of-file nil)))
  (dolist (name names)
    (unless (= (or (cdr (assq name counts)) 0) 1)
      (error "Expected one exact GNU form: %S" name)))
  (let ((help-source (expand-file-name "../help.el.gz" (getenv "GNU_CL_SOURCE_ROOT")))
        (helpers '(help--docstring-quote help-add-fundoc-usage
                   help--make-usage help--make-usage-docstring)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally help-source)
      (unless (equal (secure-hash 'sha256 (current-buffer))
                     "947215d93680fe141892ccc152dad668559f2f2e63228d0d5e70ec68581e3cea")
        (error "GNU help helper source pin mismatch")))
    (with-temp-buffer
      (insert-file-contents help-source)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'defun) (memq (cadr form) helpers))
                (let ((entry (assq (cadr form) help-counts)))
                  (if entry (setcdr entry (1+ (cdr entry)))
                    (push (cons (cadr form) 1) help-counts)))
                (push form help-forms))))
        (end-of-file nil)))
    (dolist (name helpers)
      (unless (= (or (cdr (assq name help-counts)) 0) 1)
        (error "Expected one exact GNU help helper: %S" name))))
  ;; The only source adaptation avoids the unrelated full help startup when
  ;; our genuine captured doc-helper owners remain unchanged.  It never
  ;; publishes `help' and falls back to GNU's original require if unavailable.
  (cl-labels ((adapt (form)
                (cond
                 ((equal form '(require 'help))
                  (setq adapted (1+ adapted))
                  '(unless (nelisp-stdlib-cl-type-lambda--help-owners-live-p)
                     (require 'help)))
                 ((consp form) (cons (adapt (car form)) (adapt (cdr form))))
                 (t form))))
    (setq forms (mapcar (lambda (form)
                         (if (eq (cadr form) 'cl--transform-lambda)
                             (adapt form) form)) forms)))
  (unless (= adapted 1) (error "Expected one GNU CL help dependency gate"))
  (with-temp-file (getenv "GNU_TYPE_PROVIDER_OUTPUT")
    (insert ";;; Authenticated GNU31 type/lambda closure -*- lexical-binding: t; -*-\n")
    (dolist (form (append (nreverse help-forms) (nreverse forms)))
      (prin1 (macroexpand-all form) (current-buffer))
      (insert "\n")
      (when (eq (cadr form) 'cl-typep)
        ;; GNU define-inline's declaration records this during definition;
        ;; macroexpand-all alone does not serialize that host-side effect.
        ;; Recover it from actual evaluation of the authenticated defining
        ;; form, then emit the same explicit load-time property owner.
        (eval form t)
        (let ((owner (get 'cl-typep 'compiler-macro)))
          (unless (eq owner 'cl-typep--inliner)
            (error "Unexpected genuine cl-typep compiler-macro owner: %S" owner))
          (let ((definition (symbol-function owner)))
            (unless (and (closurep definition)
                         (not (byte-code-function-p definition))
                         (equal (aref definition 2) '(t)))
              (error "GNU inliner is not a source closure with empty lexical environment"))
            (prin1 (macroexpand-all
                    (list 'defalias (list 'quote owner)
                          (list 'function
                                (cons 'lambda (cons (aref definition 0)
                                                    (aref definition 1))))))
                   (current-buffer))
            (insert "\n"))
          (prin1 (list 'function-put ''cl-typep ''compiler-macro
                       (list 'quote owner)) (current-buffer))
          (insert "\n"))))))
