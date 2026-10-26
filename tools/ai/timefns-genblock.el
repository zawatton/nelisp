;;; timefns-genblock.el --- emit the prelude block for lisp/nelisp-timefns.el  -*- lexical-binding: t; -*-
;; GNU Emacs: emacs -Q --batch -l timefns-genblock.el SRC.el OUT.el
;;   env TF_GUARD=1 (default) keeps the source's (unless (fboundp ...)) guards around public names
;;   (what the prelude needs); TF_GUARD=0 emits plain fsets (overlay testing on a runtime that
;;   already defines the names).
;; defmacro forms are evaluated for the compiler but not emitted; every defun becomes
;; (fset 'NAME #[...]) bytecode compiled by GNU Emacs; defvar/defconst forms are printed verbatim.
(setq byte-compile-warnings nil)
(defun tf-gen--compile (f)
  "Return the printed (fset 'NAME BYTECODE) form text for defun form F."
  (let* ((sym (nth 1 f))
         (fn (byte-compile `(lambda ,(nth 2 f) ,@(nthcdr 3 f)))))
    (unless (byte-code-function-p fn) (error "%s not compiled" sym))
    (with-temp-buffer
      (let ((print-escape-newlines t) (print-escape-control-characters t)
            (print-length nil) (print-level nil) (print-circle nil))
        (insert (format "(fset '%S " sym))
        (let ((p (point)))
          (prin1 fn (current-buffer))
          (let ((back (car (read-from-string (buffer-substring p (point))))))
            (unless (equal (aref back 1) (aref fn 1)) (error "%s: roundtrip mismatch" sym))))
        (insert ")")
        (buffer-string)))))
(let* ((src (nth 0 command-line-args-left)) (out (nth 1 command-line-args-left))
       (guard (not (equal (getenv "TF_GUARD") "0")))
       (forms nil))
  (setq command-line-args-left nil)
  (with-temp-buffer
    (insert-file-contents src)
    (goto-char (point-min))
    (condition-case nil
        (while t (push (read (current-buffer)) forms))
      (end-of-file nil)))
  (setq forms (nreverse forms))
  (dolist (f forms) (when (and (consp f) (eq (car f) 'defmacro)) (eval f t)))
  (with-temp-buffer
    (let ((print-escape-newlines t) (print-escape-control-characters t)
          (print-length nil) (print-level nil) (print-circle nil))
      (dolist (f forms)
        (cond
         ((and (consp f) (memq (car f) '(provide defmacro))) nil)
         ((and (consp f) (eq (car f) 'defun))
          (insert (tf-gen--compile f) "\n"))
         ;; (unless TEST (defun NAME ...)): keep the guard in guard mode.
         ((and (consp f) (eq (car f) 'unless) (eq (car-safe (nth 2 f)) 'defun) (null (nthcdr 3 f)))
          (let* ((test (nth 1 f)) (d (nth 2 f)) (txt (tf-gen--compile d)))
            (cond
             ((not guard) (insert txt "\n"))
             ((eq (nth 1 d) 'float-time)
              (insert (format "(unless %S\n  (defvar nelisp--float-time-wrapped t)\n  %s)\n" test txt)))
             (t (insert (format "(unless %S\n  %s)\n" test txt))))))
         ((and (consp f) (eq (car f) 'defvar) (eq (nth 1 f) 'nelisp--float-time-native))
          (insert "(defvar nelisp--float-time-native (symbol-function 'float-time))\n"))
         (t (prin1 f (current-buffer)) (insert "\n")))))
    (write-region (point-min) (point-max) out nil 'silent)))
