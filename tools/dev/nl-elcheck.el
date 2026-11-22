;;; nl-elcheck.el --- robust edit checks for Emacs Lisp files, run on host Emacs  -*- lexical-binding: t; -*-
;; Findings print as FILE:LINE:COL: error|warning: [kind] message.  Kinds:
;;   read-error      the real reader fails (stray close paren, invalid syntax)
;;   unclosed-paren  a form never closes before end of file
;;   unterminated-string  a string opened here never closes before end of file
;;   swallowed-form  a column-0 (def.../(provide/(require sits inside another form, or inside a
;;                   string: the previous form lost a close paren or a quote
;;   defun-count     column-0 (def... lines outnumber top-level def forms the reader produced
;;   docstring-quote a string closes/opens directly against a symbol character: an unescaped
;;                   double quote inside a string, or a missing space
;;   string-col0-paren  a ( at column 0 inside a string (escape it as \( ; breaks defun motion; warning, common in vendored GNU docstrings)
;;   bytecomp        byte-compiler warning/error (skipped when a read-level error was found)
(require 'bytecomp)
(require 'subr-x)
(defvar nl-elcheck--errors 0)
(defvar nl-elcheck--warnings 0)
(defvar nl-elcheck--file nil)

(defun nl-elcheck--report (sev pos kind fmt &rest args)
  (if (eq sev 'error) (setq nl-elcheck--errors (1+ nl-elcheck--errors))
    (setq nl-elcheck--warnings (1+ nl-elcheck--warnings)))
  (save-excursion
    (goto-char (max (point-min) (min pos (point-max))))
    (princ (format "%s:%d:%d: %s: [%s] %s\n" nl-elcheck--file (line-number-at-pos)
                   (1+ (current-column)) sev kind (apply #'format fmt args)))))

(defun nl-elcheck--where (pos)
  (save-excursion (goto-char pos) (format "%d:%d" (line-number-at-pos) (1+ (current-column)))))

(defconst nl-elcheck--defwords
  "\\(?:cl-\\)?\\(?:defun\\|defmacro\\|defvar\\|defconst\\|defcustom\\|defalias\\|defsubst\\|defgroup\\|defface\\|defstruct\\|defclass\\|defgeneric\\|defmethod\\|define-[a-z0-9-]+\\)")
(defun nl-elcheck--def-line-p ()
  (looking-at (concat "(\\(?:" nl-elcheck--defwords "\\|provide\\|require\\)\\_>")))

(defun nl-elcheck--read-pass ()
  "Read every top-level form with the real reader.  Return (OK-P . DEF-FORM-COUNT)."
  (goto-char (point-min))
  (let ((ok t) (defs 0) (go t))
    (while go
      (forward-comment (buffer-size))
      (if (eobp) (setq go nil)
        (let ((start (point)))
          (condition-case err
              (let ((form (read (current-buffer))))
                (when (and (consp form) (symbolp (car form))
                           (string-match-p (concat "\\`" nl-elcheck--defwords "\\'") (symbol-name (car form))))
                  (setq defs (1+ defs))))
            (end-of-file
             (setq ok nil go nil)
             (let ((st (save-excursion (syntax-ppss (point-max)))))
               (cond ((nth 3 st)
                      (nl-elcheck--report 'error (nth 8 st) 'unterminated-string
                                          "string opened here never closes (file ends inside it); form started at %s"
                                          (nl-elcheck--where start)))
                     (t (nl-elcheck--report
                         'error start 'unclosed-paren
                         "top-level form starting here never closes (depth %d at end of file; innermost still-open paren at %s)"
                         (car st) (if (nth 1 st) (nl-elcheck--where (nth 1 st)) "?"))))))
            (invalid-read-syntax
             (setq ok nil go nil)
             (nl-elcheck--report 'error (point) 'read-error "%s"
                                 (let ((d (cdr err)))
                                   (format "invalid read syntax %s%s" (car d)
                                           (if (equal (car d) ")") " (stray close paren; the form before it already closed)" "")))))
            (error
             (setq ok nil go nil)
             (nl-elcheck--report 'error start 'read-error "%S" err))))))
    (cons ok defs)))

(defun nl-elcheck--col0-pass (defs ok)
  (let ((col0 0) (swallowed nil))
    (goto-char (point-min))
    (while (re-search-forward "^(" nil t)
      (let ((bol (match-beginning 0)))
        (goto-char bol)
        (let* ((st (syntax-ppss bol)) (defl (nl-elcheck--def-line-p)))
          (when (and defl (= (car st) 0) (not (nth 3 st)) (not (nth 4 st))
                     (not (looking-at "(\\(?:provide\\|require\\)")))
            (setq col0 (1+ col0)))
          (cond
           ((nth 3 st)
            (if (and defl (not ok))
                (progn (setq swallowed t)
                       (nl-elcheck--report 'error bol 'swallowed-form
                                           "column-0 definition inside the string opened at %s (an earlier unescaped or missing quote)"
                                           (nl-elcheck--where (nth 8 st))))
              (nl-elcheck--report 'warning bol 'string-col0-paren
                                  "( at column 0 inside the string opened at %s; write \\( instead"
                                  (nl-elcheck--where (nth 8 st)))))
           ((and (> (car st) 0) (not (nth 4 st)) defl
                 ;; a top-level (let/(progn/(when wrapper around defuns is legitimate when the
                 ;; file reads cleanly; only a def-headed enclosing form, or a read failure, is a defect
                 (or (not ok)
                     (save-excursion (goto-char (car (nth 9 st)))
                                     (looking-at "(\\(?:cl-\\)?def\\(?:un\\|macro\\|subst\\|generic\\|method\\)\\_>"))))
            (setq swallowed t)
            (nl-elcheck--report 'error bol 'swallowed-form
                                "column-0 definition nested at depth %d inside the form opened at %s: that form is missing %d close paren(s)"
                                (car st) (nl-elcheck--where (car (nth 9 st))) (car st)))))
        (forward-line 1)))
    (when (and ok (> col0 defs))
      (nl-elcheck--report 'error (point-min) 'defun-count
                          "%d column-0 (def... lines but the reader found %d top-level def forms%s"
                          col0 defs (if swallowed " (see swallowed-form above)" "")))))

(defun nl-elcheck--string-pass ()
  (let ((pos (point-min)) (st nil) (open nil) (symch "[[:alnum:]_*+<>=/!$%&-]"))
    (while (< pos (point-max))
      (setq st (parse-partial-sexp pos (point-max) nil nil st 'syntax-table)
            pos (point))
      (cond
       ((and (nth 3 st) (characterp (nth 3 st)) (eq (nth 3 st) ?\"))
        (setq open (nth 8 st))
        (when (and (> open (point-min))
                   (string-match-p (concat "\\`" symch "\\'") (string (char-before open)))
                   (not (eq (char-before open) ??)))
          (nl-elcheck--report 'error open 'docstring-quote
                              "string opens directly after `%c' (missing space, or the closing quote of a previous string was lost)"
                              (char-before open))))
       ((and open (not (nth 3 st)) (not (nth 4 st)) (> pos (point-min)) (eq (char-before pos) ?\"))
        (when (and (< pos (point-max))
                   (string-match-p (concat "\\`" symch "\\'") (string (char-after pos))))
          (nl-elcheck--report 'error (1- pos) 'docstring-quote
                              "string (opened at %s) closes here and is directly followed by `%c': an unescaped double quote inside the string?"
                              (nl-elcheck--where open) (char-after pos)))
        (setq open nil))))))

(defun nl-elcheck--bytecomp (file loadpath)
  (let* ((dir (make-temp-file "nlel-" t))
         (copy (expand-file-name (file-name-nondirectory file) dir))
         (load-path (append loadpath (list (file-name-directory (expand-file-name file))) load-path))
         (byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-nondirectory f) "c") dir)))
         (byte-compile-warnings t))
    (copy-file file copy t)
    (let ((inhibit-message t))
      (ignore-errors (byte-compile-file copy)))
    (let ((buf (get-buffer "*Compile-Log*")))
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\(?:.*/\\)?\\(?:[^/:\n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(Warning\\|Error\\): \\(.*\\(?:\n  .*\\)*\\)" nil t)
            (let ((l (string-to-number (match-string 1))) (c (match-string 2))
                  (sev (if (string= (match-string 3) "Error") 'error 'warning))
                  (msg (replace-regexp-in-string "\n *" " " (match-string 4))))
              (with-current-buffer (get-buffer-create " *nl-elcheck-src*")
                nil)
              (setq nl-elcheck--bc (cons (list sev l c msg) nl-elcheck--bc)))))
        (kill-buffer buf)))
    (delete-directory dir t)))
(defvar nl-elcheck--bc nil)

(defun nl-elcheck-file (file loadpath bytecomp)
  (let ((nl-elcheck--file file) (before nl-elcheck--errors))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (syntax-propertize (point-max))
      (let ((r (nl-elcheck--read-pass)))
        (nl-elcheck--col0-pass (cdr r) (car r))
        (goto-char (point-min))
        (nl-elcheck--string-pass)))
    (when (and bytecomp (= before nl-elcheck--errors))
      (setq nl-elcheck--bc nil)
      (nl-elcheck--bytecomp file loadpath)
      (dolist (w (nreverse nl-elcheck--bc))
        (if (eq (car w) 'error) (setq nl-elcheck--errors (1+ nl-elcheck--errors))
          (setq nl-elcheck--warnings (1+ nl-elcheck--warnings)))
        (princ (format "%s:%d:%s: %s: [bytecomp] %s\n" file (nth 1 w) (nth 2 w) (car w) (nth 3 w)))))))

(defun nl-elcheck-batch ()
  "Entry: emacs --batch -l nl-elcheck.el -f nl-elcheck-batch [--no-bytecomp] [--strict] [-L DIR] FILE..."
  (let ((files nil) (loadpath nil) (bytecomp t) (strict nil) (args command-line-args-left))
    (setq command-line-args-left nil)
    (while args
      (let ((a (pop args)))
        (cond ((equal a "--no-bytecomp") (setq bytecomp nil))
              ((equal a "--strict") (setq strict t))
              ((equal a "-L") (push (expand-file-name (pop args)) loadpath))
              (t (push a files)))))
    (setq files (nreverse files))
    (dolist (f files)
      (if (file-readable-p f) (nl-elcheck-file f (reverse loadpath) bytecomp)
        (princ (format "%s:1:1: error: [read-error] file not readable\n" f))
        (setq nl-elcheck--errors (1+ nl-elcheck--errors))))
    (princ (format "nl-elcheck: %d file(s), %d error(s), %d warning(s)\n"
                   (length files) nl-elcheck--errors nl-elcheck--warnings))
    (kill-emacs (if (or (> nl-elcheck--errors 0) (and strict (> nl-elcheck--warnings 0))) 1 0))))
(provide 'nl-elcheck)
