;;; nemacs-org-workflow-probe.el --- vendor-first Org workflow smoke -*- lexical-binding: t; -*-

(defun nemacs-org-workflow-probe--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-org-workflow-probe--backtrace-lines ()
  "Return current backtrace as a list of lines."
  (let ((text (with-output-to-string (backtrace))))
    (split-string text "\n" t)))

(defvar nemacs-org-workflow-probe--repo-root nil
  "Explicit repository root for the Org workflow probe.")

(defvar nemacs-org-workflow-probe--trace-agenda nil
  "Non-nil means trace agenda-side workflow functions.")

(defvar nemacs-org-workflow-probe--debug nil
  "Non-nil means print extra workflow diagnostics on successful paths.")

(defun nemacs-org-workflow-probe--trace-function (symbol)
  "Wrap SYMBOL to print entry/return markers during the workflow probe."
  (when (fboundp symbol)
    (let ((original (symbol-function symbol)))
      (fset symbol
            (lambda (&rest args)
              (nemacs-org-workflow-probe--print
               "ORG-WORKFLOW TRACE enter %S args=%S" symbol args)
              (condition-case err
                  (let ((value (apply original args)))
                    (nemacs-org-workflow-probe--print
                     "ORG-WORKFLOW TRACE return %S => %S" symbol value)
                    value)
                (error
                 (nemacs-org-workflow-probe--print
                  "ORG-WORKFLOW TRACE error %S %S" symbol err)
                 (signal (car err) (cdr err)))))))))

(defun nemacs-org-workflow-probe--function-state (symbol)
  "Return a compact description of SYMBOL's function cell."
  (cond
   ((not (fboundp symbol)) 'unbound)
   ((eq (symbol-function symbol) 'nelisp--unbound-marker) 'nelisp-unbound-marker)
   (t (car-safe (symbol-function symbol)))))

(defun nemacs-org-workflow-probe--trace-regex-compile ()
  "Wrap `nelisp-rx-compile' to print regex syntax failures."
  (when (fboundp 'nelisp-rx-compile)
    (let ((original (symbol-function 'nelisp-rx-compile)))
      (fset 'nelisp-rx-compile
            (lambda (regexp)
              (condition-case err
                  (funcall original regexp)
                (nelisp-rx-syntax-error
                 (nemacs-org-workflow-probe--print
                  "ORG-WORKFLOW REGEX-ERROR regexp=%S err=%S" regexp err)
                 (signal (car err) (cdr err)))))))))

(let ((ok t)
      (repo (or nemacs-org-workflow-probe--repo-root
                (and (boundp 'nemacs-org-mode-smoke--repo-root)
                     nemacs-org-mode-smoke--repo-root)
                (and (boundp 'default-directory) default-directory)
                "."))
      (inbox nil)
      (agenda-buffer nil))
  (condition-case err
      (progn
        (load (expand-file-name "src/nelisp-emacs-org-bridge.el" repo)
              nil 'no-message t t)
        (setq nelisp-emacs-org-bridge-repo-root repo)
        (nelisp-emacs-org-bridge-load)
        (setq inbox (make-temp-file "nemacs-org-workflow-" nil ".org"))
        (with-temp-file inbox
          (insert "* INBOX\n"))
        (setq org-directory (file-name-directory inbox)
              org-default-notes-file inbox
              org-agenda-files (list inbox)
              org-agenda-span 1
              org-capture-templates
              `(("t" "Todo" entry (file ,inbox)
                 "* TODO captured\nSCHEDULED: <2026-07-10 Fri>\n"
                 :immediate-finish t)))
        (setq org-capture-link-is-already-stored t
              org-store-link-plist nil)
        (nemacs-org-workflow-probe--print
         "ORG-WORKFLOW bridge=%S capture=%S agenda=%S file=%S"
         (featurep 'org) (featurep 'org-capture) (featurep 'org-agenda) inbox)
        (when (and (boundp 'nemacs-org-workflow-probe--trace)
                   nemacs-org-workflow-probe--trace)
          (dolist (symbol '(org-capture-select-template
                            org-capture-set-plist
                            org-capture-get-template
                            org-capture-set-target-location
                            org-capture-fill-template
                            org-capture-place-template
                            org-capture-get-indirect-buffer
                            pop-to-buffer
                            org-fold-show-all
                            org-capture-place-entry
                            substitute-command-keys
                            run-hooks
                            org-capture--run-template-functions
                            org-capture-put
                            org-capture-get
                            org-get-x-clipboard
                            current-kill
                            marker-buffer
                            org-no-properties
                            switch-to-buffer-other-window
                            get-buffer-create
                            erase-buffer
                            org-mode
                            org-clone-local-variables
                            org-check-agenda-file
                            org-get-agenda-file-buffer
                            org-find-base-buffer-visiting
                            find-file-noselect
                            buffer-file-name
                            buffer-base-buffer
                            derived-mode-p
                            save-buffer
                            kill-buffer
                            org-capture-finalize))
            (nemacs-org-workflow-probe--trace-function symbol)))
        (when nemacs-org-workflow-probe--debug
          (nemacs-org-workflow-probe--print
           "ORG-WORKFLOW funcs capture-mode=%S setq-local=%S make-local=%S subst=%S run-hooks=%S"
           (nemacs-org-workflow-probe--function-state 'org-capture-mode)
           (nemacs-org-workflow-probe--function-state 'setq-local)
           (nemacs-org-workflow-probe--function-state 'make-local-variable)
           (nemacs-org-workflow-probe--function-state 'substitute-command-keys)
           (nemacs-org-workflow-probe--function-state 'run-hooks))
          (nemacs-org-workflow-probe--print
           "ORG-WORKFLOW indirect-fns make=%S base=%S clone=%S"
           (symbol-function 'make-indirect-buffer)
           (symbol-function 'buffer-base-buffer)
           (symbol-function 'emacs-buffer-clone-indirect-buffer))
          (nemacs-org-workflow-probe--print
           "ORG-WORKFLOW easy-mmode standalone=%S runtime=%S needs=%S define-minor=%S emacs-version=%S"
           (and (boundp 'emacs-easy-mmode--standalone-p)
                emacs-easy-mmode--standalone-p)
           (and (fboundp 'emacs-easy-mmode--standalone-runtime-p)
                (emacs-easy-mmode--standalone-runtime-p))
           (and (fboundp 'emacs-easy-mmode--define-minor-mode-needs-fallback-p)
                (emacs-easy-mmode--define-minor-mode-needs-fallback-p))
           (nemacs-org-workflow-probe--function-state 'define-minor-mode)
           (and (boundp 'emacs-version) emacs-version)))
        (nemacs-org-workflow-probe--print
         "ORG-WORKFLOW S1 before-capture fboundp-mode=%S"
         (fboundp 'org-capture-mode))
        (org-capture nil "t")
        (nemacs-org-workflow-probe--print "ORG-WORKFLOW S2 after-capture")
        (with-temp-buffer
          (nemacs-org-workflow-probe--print "ORG-WORKFLOW S3 before-read-capture")
          (insert-file-contents inbox)
          (let ((contents (buffer-string)))
            (nemacs-org-workflow-probe--print
             "ORG-WORKFLOW capture-file=%S" contents)
            (unless (and (string-match-p "TODO captured" contents)
                         (string-match-p "SCHEDULED: <2026-07-10 Fri>" contents))
              (setq ok nil))))
        (let ((stale (find-file-noselect inbox)))
          (when stale
            (kill-buffer stale)))
        (nemacs-org-workflow-probe--print "ORG-WORKFLOW S4 before-agenda")
        (with-current-buffer (find-file-noselect inbox)
          (make-local-variable 'major-mode)
          (make-local-variable 'mode-name)
          (setq major-mode 'org-mode
                mode-name "Org"))
        (setq major-mode 'org-mode
              mode-name "Org")
        (nemacs-org-workflow-probe--print
         "ORG-WORKFLOW agenda-file-state exists=%S readable=%S"
         (and (fboundp 'file-exists-p) (file-exists-p inbox))
         (and (fboundp 'file-readable-p) (file-readable-p inbox)))
        (when nemacs-org-workflow-probe--debug
          (nemacs-org-workflow-probe--print
           "ORG-WORKFLOW agenda-files-effective=%S"
           (condition-case files-err
               (org-agenda-files nil 'ifmode)
             (error (list 'ERROR files-err))))
          (nemacs-org-workflow-probe--print
           "ORG-WORKFLOW direct-day-entries=%S"
           (condition-case entries-err
               (org-agenda-get-day-entries inbox '(7 10 2026) :scheduled)
             (error (list 'ERROR entries-err))))
          (with-current-buffer (find-file-noselect inbox)
            (nemacs-org-workflow-probe--print
             "ORG-WORKFLOW element-at-todo=%S"
             (condition-case element-err
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "TODO captured" nil t)
                   (beginning-of-line)
                   (let* ((el (org-element-at-point))
                          (scheduled (and el (org-element-property :scheduled el))))
                     (list :type (and el (org-element-type el))
                           :raw (and el (org-element-property :raw-value el))
                           :scheduled scheduled
                           :scheduled-type (and scheduled
                                                (org-element-property :type scheduled))
                           :scheduled-raw (and scheduled
                                               (org-element-property :raw-value scheduled)))))
               (error (list 'ERROR element-err))))))
        (nemacs-org-workflow-probe--trace-regex-compile)
        (when (and (boundp 'nemacs-org-workflow-probe--trace-agenda)
                   nemacs-org-workflow-probe--trace-agenda)
          (dolist (symbol '(org-check-agenda-file
                            org-get-agenda-file-buffer
                            org-find-base-buffer-visiting
                            find-file-noselect
                            org-agenda-get-day-entries
                            org-agenda-get-scheduled
                            org-agenda-finalize-entries
                            derived-mode-p))
            (nemacs-org-workflow-probe--trace-function symbol)))
        (org-agenda-list nil "2026-07-10" 1)
        (nemacs-org-workflow-probe--print "ORG-WORKFLOW S5 after-agenda")
        (setq agenda-buffer (get-buffer org-agenda-buffer-name))
        (unless agenda-buffer
          (error "agenda buffer missing: %S" org-agenda-buffer-name))
        (with-current-buffer agenda-buffer
          (let ((contents (buffer-string)))
            (nemacs-org-workflow-probe--print
             "ORG-WORKFLOW agenda-mode=%S contains-captured=%S size=%S org-agenda-mode-fn=%S"
             major-mode
             (string-match-p "captured" contents)
             (length contents)
             (nemacs-org-workflow-probe--function-state 'org-agenda-mode))
            (unless (and (eq major-mode 'org-agenda-mode)
                         (string-match-p "captured" contents))
              (nemacs-org-workflow-probe--print
               "ORG-WORKFLOW agenda-content=%S" contents))
            (unless (and (eq major-mode 'org-agenda-mode)
                         (string-match-p "captured" contents))
              (setq ok nil)))))
    (quit
     (setq ok nil)
     (nemacs-org-workflow-probe--print "ORG-WORKFLOW QUIT")
     (dolist (line (nemacs-org-workflow-probe--backtrace-lines))
       (nemacs-org-workflow-probe--print "ORG-WORKFLOW BT %s" line)))
    (error
     (setq ok nil)
     (nemacs-org-workflow-probe--print "ORG-WORKFLOW ERROR %S" err)
     (when (and (fboundp 'current-buffer)
                (fboundp 'buffer-base-buffer))
       (let ((current (ignore-errors (current-buffer))))
         (nemacs-org-workflow-probe--print
          "ORG-WORKFLOW ERROR-CURRENT current=%S base=%S mode-var=%S"
          current
          (and current (ignore-errors (buffer-base-buffer current)))
          (and (boundp 'org-capture-mode) org-capture-mode))))
     (dolist (line (nemacs-org-workflow-probe--backtrace-lines))
       (nemacs-org-workflow-probe--print "ORG-WORKFLOW BT %s" line))))
  (when agenda-buffer
    (ignore-errors (kill-buffer agenda-buffer)))
  (when inbox
    (ignore-errors (delete-file inbox)))
  (nemacs-org-workflow-probe--print
   "ORG-WORKFLOW %s" (if ok "PASS" "FAIL")))

;;; nemacs-org-workflow-probe.el ends here
