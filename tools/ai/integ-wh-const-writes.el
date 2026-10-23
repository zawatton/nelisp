;;; integ-wh-const-writes.el --- find writes to GNU's C-level constants  -*- lexical-binding: t -*-

;; Usage: emacs -Q --batch -l tools/ai/integ-wh-const-writes.el -- FILE-OR-DIR...
;;
;; GNU marks six variables with make_symbol_constant (callpath 467c73b19):
;; most-positive-fixnum (data.c:4377), most-negative-fixnum (data.c:4383),
;; enable-multibyte-characters (buffer.c:5279), font-weight-table (font.c:5989),
;; font-slant-table (font.c:5996), font-width-table (font.c:6003).  The merged runtime refuses
;; every write to them (setting-constant), so a write that survives in a prelude, a staged
;; GNU file or the runtime Lisp fails at boot or while the cold image is made.
;;
;; The scan reads every top-level form with the real reader and walks the tree.  A site is a
;; list whose head is a setting form with one of the six names in a place position:
;;   setq / setq-local / setq-default      the name at an odd argument position
;;   set / set-default / set-default-toplevel-value / make-local-variable /
;;   makunbound / defvaralias / defconst / add-variable-watcher /
;;   kill-local-variable / make-variable-buffer-local     (quote NAME) in any argument
;;   let / let* / dlet / letrec / named-let               NAME or (NAME ...) in the bindings
;;   setf                                                 the name at an odd argument position
;;   push / cl-pushnew (last argument), pop / cl-incf / cl-decf / cl-callf (first)   NAME as the bare place
;;   add-to-list                                          (quote NAME) as its first argument
;; Prints one line per site, "SITE file: top-level form: (head ... name ...)", then
;; "CONST-WRITES n sites in m files".  Exit 0 iff every site is covered by the allowlist
;; (a regexp on the "file: form" text, in tools/ai/integ-wh-const-writes.allow, one per line).

(require 'cl-lib)

(defconst cw--names
  '(most-positive-fixnum most-negative-fixnum enable-multibyte-characters
    font-weight-table font-slant-table font-width-table))
(defconst cw--setq-heads '(setq setq-local setq-default setf))
(defconst cw--quote-heads
  '(set set-default set-default-toplevel-value make-local-variable makunbound defvaralias
    defconst add-variable-watcher kill-local-variable make-variable-buffer-local
    setq-default-toplevel defvar-local add-to-list))
(defconst cw--let-heads '(let let* dlet letrec named-let cl-letf cl-letf* lexical-let))
(defconst cw--place-heads '(push pop cl-incf cl-decf cl-pushnew cl-callf incf decf))

(defun cw--quoted-name-p (x)
  (and (consp x) (memq (car x) '(quote function)) (consp (cdr x)) (memq (cadr x) cw--names)))

(defun cw--site-p (form)
  "Return the name when FORM is a setting form for one of the constants, else nil."
  (when (and (consp form) (symbolp (car form)) (proper-list-p form))
    (let ((head (car form)) (args (cdr form)) hit)
      (cond
       ((memq head cw--setq-heads)
        (let ((i 0))
          (dolist (a args)
            (when (and (cl-evenp i) (memq a cw--names)) (setq hit a))
            (setq i (1+ i)))))
       ((memq head cw--quote-heads)
        (dolist (a args)
          (cond ((cw--quoted-name-p a) (setq hit (cadr a)))
                ((and (eq head 'defconst) (memq a cw--names)) (setq hit a)))))
       ((memq head cw--let-heads)
        (when (and args (proper-list-p (car args)))
          (dolist (b (car args))
            (cond ((memq b cw--names) (setq hit b))
                  ((and (consp b) (memq (car b) cw--names)) (setq hit (car b)))))))
       ((memq head cw--place-heads)   ; the place is the last argument of push, the first of the others
        (let ((place (if (memq head '(push cl-pushnew)) (car (last args)) (car args))))
          (when (memq place cw--names) (setq hit place)))))
      hit)))

(defun cw--walk (form fn)
  (when (consp form)
    (let ((name (cw--site-p form)))
      (when name (funcall fn form name)))
    (let ((x form))
      (while (consp x)
        (cw--walk (car x) fn)
        (setq x (cdr x)))
      (when (and x (not (atom x))) (cw--walk x fn)))))

(defvar cw--read-errors 0)
(defconst cw--def-heads '(defun defmacro defvar defconst define-inline cl-defun cl-defmacro defsubst))
(defun cw--def-label (form)
  "The first definition inside FORM, \"defun NAME\", or nil."
  (when (consp form)
    (if (and (memq (car form) cw--def-heads) (consp (cdr form)) (symbolp (cadr form)))
        (format "%s %s" (car form) (cadr form))
      (let ((x form) r)
        (while (and (consp x) (not r))
          (setq r (cw--def-label (car x)) x (cdr x)))
        r))))
(defun cw--head-line (form)
  (let ((s (format "%S" form)))
    (replace-regexp-in-string "[\n ]+" " " (if (> (length s) 110) (concat (substring s 0 110) "...") s))))

(defun cw--scan-file (file allow)
  (let ((sites 0) (bad 0))
    (with-temp-buffer
      (insert-file-contents file)
      (set-syntax-table emacs-lisp-mode-syntax-table)   ; forward-comment must skip ";" comments
      (goto-char (point-min))
      (condition-case err
          (while (progn (forward-comment (buffer-size)) (not (eobp)))
            (let* ((start-line (line-number-at-pos))
                   (form (read (current-buffer)))
                   (top (or (cw--def-label form) (format "line %d" start-line))))
              (cw--walk form
                        (lambda (f name)
                          (let ((line (format "%s: %s: %s" file top (cw--head-line f))))
                            (setq sites (1+ sites))
                            (if (cl-some (lambda (re) (string-match-p re line)) allow)
                                (princ (format "SITE allowed %s [%s]\n" line name))
                              (setq bad (1+ bad))
                              (princ (format "SITE %s [%s]\n" line name))))))))
        (error (setq cw--read-errors (1+ cw--read-errors))
               (princ (format "READ-ERROR %s line %d: %S\n" file (line-number-at-pos) err)))))
    (cons sites bad)))

(defun cw--files (arg)
  (cond ((file-directory-p arg) (directory-files-recursively arg "\\.el\\'"))
        ((file-exists-p arg) (list arg))
        (t (error "no such file or directory: %s" arg))))

(let* ((args (cdr (member "--" command-line-args)))
       (allow-file (expand-file-name "tools/ai/integ-wh-const-writes.allow"
                                     (or (locate-dominating-file default-directory "tools/ai") default-directory)))
       (allow (when (file-exists-p allow-file)
                (with-temp-buffer
                  (insert-file-contents allow-file)
                  (cl-remove-if (lambda (l) (or (string= l "") (string-prefix-p "#" l)))
                                (split-string (buffer-string) "\n")))))
       (total 0) (bad 0) (nfiles 0) (hit-files 0))
  (dolist (a args)
    (dolist (f (cw--files a))
      (setq nfiles (1+ nfiles))
      (let ((r (cw--scan-file f allow)))
        (when (> (car r) 0) (setq hit-files (1+ hit-files)))
        (setq total (+ total (car r)) bad (+ bad (cdr r))))))
  (princ (format "CONST-WRITES %d sites (%d not allowed) in %d of %d files, %d unreadable\n"
                 total bad hit-files nfiles cw--read-errors))
  (kill-emacs (if (> bad 0) 1 0)))

;;; integ-wh-const-writes.el ends here
