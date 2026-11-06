;;; e2e-repl.el --- host sessions for e2e-repl.sh -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'nl-typecheck-recover)

(defconst e2e-repl--corpus
  '((+ 1 2) (setq e2e-x 5) (list e2e-x "a" 'b) (defun e2e-f (n) (* n 3))
    (e2e-f 4) (car '(1 2)) (concat "a" "b")))

(defconst e2e-repl--def
  '(defun clash-fn (n) (declare (ftype (function (integer) integer))) (* n 2)))

(defun e2e-repl--feed (form)
  "Feed FORM to the driver, printing its result or error."
  (condition-case err
      (prin1 (nl-typecheck-recover-eval-input form))
    (error (princ (format "ERROR %S" err))))
  (princ "\n"))

(defun e2e-repl--fail (msg) (princ (concat "FAIL: " msg "\n")) (kill-emacs 1))

(defun e2e-repl-run (mode)
  (cond
   ((equal mode "hook-off")
    (let ((out (nth 1 command-line-args-left)))
      (setq nelisp-repl-input-check-function nil)
      (with-temp-file (concat out ".plain")
        (dolist (f e2e-repl--corpus)
          (insert (prin1-to-string (eval f t)) "\n")))
      (with-temp-file (concat out ".driver")
        (dolist (f e2e-repl--corpus)
          (insert (prin1-to-string (nl-typecheck-recover-eval-input f)) "\n")))))
   ((equal mode "input-check")
    (nelisp-typecheck-repl-mode 1)
    (setq nelisp-typecheck-strict t)
    (let ((out (with-output-to-string
                 (e2e-repl--feed e2e-repl--def)
                 (e2e-repl--feed '(progn (setq probe-flag t) (clash-fn "s")))
                 (e2e-repl--feed '(clash-fn 4)))))
      (princ out)
      (unless (string-match-p "input 2 refused" out) (e2e-repl--fail "no refusal line"))
      (unless (string-match-p "\n8\n" out) (e2e-repl--fail "valid input not evaluated"))
      (when (boundp 'probe-flag) (e2e-repl--fail "refused input had a side effect"))))
   ((equal mode "redefine")
    (nelisp-typecheck-repl-mode 1)
    (let ((out (with-output-to-string
                 (e2e-repl--feed '(defun rd-callee (n) (declare (ftype (function (integer) integer))) n))
                 (e2e-repl--feed '(defun rd-ok (x) (rd-callee x)))
                 (e2e-repl--feed '(defun rd-bad () (rd-callee 5)))
                 (e2e-repl--feed '(defun rd-callee (s) (declare (ftype (function (string) integer))) (length s))))))
      (princ out)
      (unless (string-match-p "rd-bad.*after redefinition of rd-callee" out)
        (e2e-repl--fail "broken caller not reported"))
      (when (string-match-p "caller rd-ok" out) (e2e-repl--fail "consistent caller reported"))))
   ((equal mode "recover")
    (setq nelisp-typecheck-enforce t)
    (nl-typecheck-entry-load (expand-file-name "fixtures/entry-typed.el"
                                               (file-name-directory load-file-name)))
    (nelisp-typecheck-repl-mode 0)
    (e2e-repl--feed '(defun rc-caller () (funcall 'entry-add (concat "4" "1") 1)))
    (e2e-repl--feed '(rc-caller))
    (let* ((id (plist-get (car (last (nelisp-repl-session-failures))) :id))
           (blame (nl-typecheck-recover-blame id)))
      (princ (format "failure %S blame %S\n" id blame))
      (unless (eq (plist-get blame :callee) 'entry-add) (e2e-repl--fail "blame"))
      (e2e-repl--feed '(defun rc-caller () (funcall 'entry-add 41 1)))
      (let ((v (nl-typecheck-recover-retry id)))
        (princ (format "retry => %S\n" v))
        (unless (eql v 42) (e2e-repl--fail "retry did not succeed")))))
   (t (e2e-repl--fail "unknown mode"))))

(e2e-repl-run (car command-line-args-left))
(setq command-line-args-left (cdr command-line-args-left))
;;; e2e-repl.el ends here
