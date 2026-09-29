;;; c-core-parity-driver.el --- run C-core probes, print comparable lines  -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs unchanged on host GNU Emacs (`emacs -Q --batch -l') and on the NeLisp
;; standalone after build/nemacs-bootstrap.el has loaded.  Each probe file
;; under test/c-core-probes/ holds entries of the form
;;
;;   (NAME FORM...)
;;
;; where NAME is the C primitive the forms exercise.  Every FORM is evaluated
;; (lexically) and printed as one line
;;
;;   P| NAME | VALUE-or-(ERR SYMBOL DATA)
;;
;; so the two runtimes' outputs can be diffed.  Forms must return printable,
;; runtime-independent data (numbers, strings, symbols, lists; not buffers,
;; windows or other #<...> objects).
;;
;; Environment: C_CORE_UNIT=NAME restricts the run to test/c-core-probes/NAME.el.

;;; Code:

(defun c-core-parity--read-entries (file)
  "Return the entries read from probe FILE, in order."
  (let ((entries nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (setq entries (cons (read (current-buffer)) entries)))
        (end-of-file nil)))
    (nreverse entries)))

(defun c-core-parity--run ()
  "Evaluate every probe entry and print one comparable line per form."
  (let* ((dir (expand-file-name "test/c-core-probes"))
         (unit (getenv "C_CORE_UNIT"))
         (files (if (and unit (not (equal unit "")))
                    (list (expand-file-name (concat unit ".el") dir))
                  (directory-files dir t "\\.el\\'"))))
    (dolist (file files)
      (dolist (entry (c-core-parity--read-entries file))
        (let ((name (car entry)))
          (dolist (form (cdr entry))
            (princ (format "P| %s | %s\n" name
                           (condition-case err
                               (prin1-to-string (eval form t))
                             (error (prin1-to-string
                                     (list 'ERR (car err) (cdr err))))))))))))
  (princ "P-DONE\n"))

(c-core-parity--run)

;;; c-core-parity-driver.el ends here
