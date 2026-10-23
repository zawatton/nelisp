;;; symbols-name-word-defuns.el --- the defuns that differ from a revision -*- lexical-binding: t; -*-

;; emacs -Q --batch -l tools/ai/symbols-name-word-defuns.el -- REV BOUNDARY-FILE
;;
;; Reads every Elisp file of lisp/ and scripts/ that differs between REV and the working tree with the Elisp reader,
;; collects each (defun NAME ...) form anywhere in it (the runtime's DSL defuns sit inside quoted lists), and compares
;; the printed forms by name (a builtin dispatch entry ((:lit "make-string") ...) is named lit:make-string).  BOUNDARY-FILE lists names to keep unchanged (one per line, `#' starts a comment, a
;; trailing `*' makes a prefix); the check fails when a listed name is changed, added or removed.  The last line
;; says how many defuns changed.  Used by symbols-name-word-check.sh boundary (the ledger's S0.2).

(require 'cl-lib)
(let* ((args (cdr (member "--" command-line-args)))
       (rev (nth 0 args))
       (boundary-file (nth 1 args))
       (files (split-string
               (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process "git" nil t nil "diff" "--name-only" rev "--" "lisp" "scripts")))
               "\n" t))
       (changed nil) (added nil) (removed nil) (forbidden nil) (patterns nil))
  (with-temp-buffer
    (insert-file-contents boundary-file)
    (dolist (line (split-string (buffer-string) "\n" t))
      (let ((l (string-trim (replace-regexp-in-string "#.*" "" line))))
        (unless (string-empty-p l) (push l patterns)))))
  (defun snw-boundary-p (name)
    (cl-some (lambda (p) (if (string-suffix-p "*" p)
                             (string-prefix-p (substring p 0 -1) name)
                           (string= p name)))
             patterns))
  (defun snw-collect (form table)
    ;; A (defun NAME ...) is keyed NAME, a builtin dispatch entry ((:lit "NAME") ...) is keyed lit:NAME.
    (when (consp form)
      (let ((key (cond ((and (eq (car form) 'defun) (consp (cdr form)) (symbolp (cadr form))) (symbol-name (cadr form)))
                       ((and (consp (car form)) (eq (car (car form)) :lit) (consp (cdr (car form)))
                             (stringp (cadr (car form))))
                        (concat "lit:" (cadr (car form)))))))
        (when key
          (puthash key (append (gethash key table) (list (prin1-to-string form))) table))
        (let ((x form))
          (while (consp x)
            (snw-collect (car x) table)
            (setq x (cdr x)))))))
  (defun snw-read-all (buffer)
    (let ((table (make-hash-table :test 'equal)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (condition-case nil
            (while t (snw-collect (read (current-buffer)) table))
          (end-of-file nil)))
      table))
  (dolist (f files)
    (when (string-suffix-p ".el" f)
      (let ((old (generate-new-buffer " old")) (new (generate-new-buffer " new")))
        (with-current-buffer old
          (let ((coding-system-for-read 'utf-8))
            (call-process "git" nil t nil "show" (concat rev ":" f))))
        (when (file-exists-p f)
          (with-current-buffer new
            (let ((coding-system-for-read 'utf-8)) (insert-file-contents f))))
        (let ((to (snw-read-all old)) (tn (snw-read-all new)))
          (maphash (lambda (k v)
                     (let ((o (gethash k to)))
                       (cond ((null o) (push (cons f k) added))
                             ((not (equal o v)) (push (cons f k) changed)))))
                   tn)
          (maphash (lambda (k _v) (unless (gethash k tn) (push (cons f k) removed))) to))
        (kill-buffer old) (kill-buffer new))))
  (dolist (set (list (cons "changed" changed) (cons "added" added) (cons "removed" removed)))
    (dolist (e (cdr set))
      (when (snw-boundary-p (cdr e))
        (push (format "%s %s in %s" (car set) (cdr e) (car e)) forbidden))))
  (dolist (e (reverse changed)) (princ (format "changed %s (%s)\n" (cdr e) (car e))))
  (dolist (e (reverse added)) (princ (format "added %s (%s)\n" (cdr e) (car e))))
  (dolist (e (reverse removed)) (princ (format "removed %s (%s)\n" (cdr e) (car e))))
  (dolist (b (reverse forbidden)) (princ (format "BOUNDARY VIOLATION %s\n" b)))
  (princ (format "%d defuns changed, %d added, %d removed against %s; %d in the boundary (%d patterns)\n"
                 (length changed) (length added) (length removed) rev (length forbidden) (length patterns)))
  (kill-emacs (if forbidden 1 0)))
