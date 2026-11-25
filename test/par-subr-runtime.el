;;; par-subr-runtime.el --- GNU subr.el runtime definitions -*- lexical-binding: t; -*-

(defmacro par-subr-runtime--row (name &rest body)
  `(progn
     (prin1 (cons ',name (condition-case err (progn ,@body)
                           (error (list 'error (car err))))))
     (terpri)))

(par-subr-runtime--row owners
  (mapcar (lambda (name)
            (let ((file (symbol-file name 'defun)))
              (and file (equal (file-name-base file) "subr")
                   (or (not (fboundp 'nemacs-runtime-dump-reinit))
                       (string-match-p "/vendor/" file))
                   t)))
          '(event-start event-end posn-point posnp add-to-history
            read-key read-number y-or-n-p y-or-n-p-insert-y
            y-or-n-p-insert-n y-or-n-p-insert-other define-mail-user-agent
            buffer-match-p match-buffers make-progress-reporter
            progress-reporter-text progress-reporter-context
            progress-reporter-force-update progress-reporter-echo-area
            progress-reporter-do-update progress-reporter-done)))

(defvar par-subr-runtime-history nil)

(par-subr-runtime--row history
  (let ((par-subr-runtime-history '("a" "b"))
        (history-delete-duplicates t))
    (add-to-history 'par-subr-runtime-history "b" 2)
    (add-to-history 'par-subr-runtime-history "c" 2)
    par-subr-runtime-history))

(par-subr-runtime--row progress-defaults
  (list (default-value 'progress-reporter-update-functions)
        (symbol-function 'progress-reporter-make)))

(par-subr-runtime--row progress-updates
  (let* ((updates nil)
         (progress-reporter-update-functions
          (list (lambda (reporter state text)
                  (push (list (progress-reporter-text reporter)
                              (progress-reporter-context reporter)
                              state text)
                        updates))))
         (reporter (progress-reporter-make "Work" 0 10 0 1 0 'async)))
    (progress-reporter-force-update reporter 5 "Half")
    (progress-reporter-done reporter)
    (nreverse updates)))
