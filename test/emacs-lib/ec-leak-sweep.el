;;; ec-leak-sweep.el --- find emacs-lib functions that need the ec buffer  -*- lexical-binding: t; -*-

;; The buffer of record is the runtime `nelisp-buffer' (unification plan
;; 2026-11-03, decision 5.1).  A function emacs-lib installs that still
;; reads the `nelisp-ec-buffer' layer fails inside a native buffer with
;; `nelisp-ec-no-current-buffer' or a `nelisp-ec-*' wrong-type-argument.
;;
;; This calls every zero-argument function among SWEEP_NAMES inside a
;; native `with-temp-buffer' holding some text, and prints one EC-LEAK row
;; per function that fails that way, then
;;   EC-LEAK-SUMMARY called=N leaks=M
;; Functions that prompt, touch files or processes, or change the selected
;; buffer/window are skipped by name.  The run also checks itself: a control
;; function that reads the ec current buffer must be reported, otherwise
;; EC-LEAK-CONTROL FAIL is printed (the classifier is broken).
;;
;; Also prints one row per buffer constructor:
;;   BUFFER-TYPE <form> <type-of result>
;; and BUFFER-OWNER PASS iff all of them are `nelisp-buffer'.
;;
;; Env: SWEEP_NAMES  file with one symbol name per line.

(defvar ec-leak-sweep--skip
  (concat "\\`\\(?:read-\\|completing\\|minibuffer\\|.*prompt\\|y-or-n\\|"
          "yes-or-no\\|sit-for\\|sleep\\|kill-emacs\\|save-\\|.*exit\\|"
          "suspend\\|recursive-edit\\|top-level\\|abort\\|keyboard-quit\\|"
          ".*shell\\|.*process\\|delete-file\\|delete-directory\\|rename\\|"
          "copy-file\\|write-\\|find-file\\|server\\|kill-buffer\\|kill-all\\|"
          "kill-this\\|bury\\|switch-to\\|display-\\|pop-to\\|other-\\|"
          "delete-window\\|delete-frame\\|make-frame\\|redisplay\\|redraw\\|"
          "execute-\\|call-interactively\\|command-execute\\|self-insert\\|"
          "undo\\|revert\\|insert-file\\|view-\\|describe-\\|help\\|info\\|"
          "eval-\\|load\\|require\\|byte-\\|garbage\\|set-buffer\\|"
          "erase-buffer\\|widen\\|narrow\\)"))

(defun ec-leak-sweep--ec-error-p (err)
  "Non-nil when ERR comes from the ec buffer layer."
  (let ((sym (car-safe err)))
    (and (symbolp sym)
         (or (string-prefix-p "nelisp-ec-" (symbol-name sym))
             (and (eq sym 'wrong-type-argument)
                  (symbolp (cadr err))
                  (string-prefix-p "nelisp-ec-" (symbol-name (cadr err))))))))

(defun ec-leak-sweep--call (sym)
  "Call SYM with no arguments in a native temp buffer; return the error."
  (condition-case err
      (with-temp-buffer
        (insert "aaa bbb ccc.  Ddd eee.\n\nfff ggg\n")
        (goto-char 5)
        (funcall sym)
        nil)
    (error err)))

(defun ec-leak-sweep--zero-arg-p (sym)
  (condition-case nil
      (let ((arity (func-arity sym)))
        (and (consp arity) (eql (car arity) 0)))
    (error nil)))

(let ((names (with-temp-buffer
               (insert-file-contents (getenv "SWEEP_NAMES"))
               (split-string (buffer-string) "\n" t)))
      (called 0) (leaks 0))
  (dolist (name names)
    (let ((sym (intern name)))
      (when (and (fboundp sym)
                 (not (string-match-p ec-leak-sweep--skip name))
                 (ec-leak-sweep--zero-arg-p sym))
        (setq called (1+ called))
        (let ((err (ec-leak-sweep--call sym)))
          (when (and err (ec-leak-sweep--ec-error-p err))
            (setq leaks (1+ leaks))
            (princ (format "EC-LEAK\t%s\t%S\n" name err)))))))
  (princ (format "EC-LEAK-SUMMARY called=%d leaks=%d\n" called leaks)))

;; Negative control: a function that needs the ec current buffer.
(defun ec-leak-sweep--control ()
  (nelisp-ec-buffer-name (nelisp-ec-current-buffer)))
(princ (if (and (fboundp 'nelisp-ec-current-buffer)
                (ec-leak-sweep--ec-error-p
                 (ec-leak-sweep--call 'ec-leak-sweep--control)))
           "EC-LEAK-CONTROL PASS\n"
         "EC-LEAK-CONTROL FAIL\n"))

;; One buffer type after bootstrap.
(let ((rows
       (list
        (cons "current-buffer" (type-of (current-buffer)))
        (cons "get-buffer-create" (type-of (get-buffer-create " *ec-sweep*")))
        (cons "generate-new-buffer" (type-of (generate-new-buffer "ec-sweep")))
        (cons "with-temp-buffer" (with-temp-buffer (type-of (current-buffer))))
        (cons "save-excursion+set-buffer"
              (save-excursion (set-buffer (get-buffer-create " *ec-sweep*"))
                              (type-of (current-buffer))))
        (cons "buffer-list" (let ((types (delete-dups
                                          (mapcar #'type-of (buffer-list)))))
                              (if (cdr types) types (car types)))))))
  (dolist (row rows)
    (princ (format "BUFFER-TYPE\t%s\t%S\n" (car row) (cdr row))))
  (princ (if (cl-every (lambda (row) (eq (cdr row) 'nelisp-buffer)) rows)
             "BUFFER-OWNER PASS\n" "BUFFER-OWNER FAIL\n")))

;;; ec-leak-sweep.el ends here
