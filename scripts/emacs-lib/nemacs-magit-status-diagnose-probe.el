;;; nemacs-magit-status-diagnose-probe.el --- staged Magit M2 diagnosis -*- lexical-binding: t; -*-

;; Evaluated by `make magit-status-diagnose' against
;; `build/nemacs-magit-runtime.nlri'.  Unlike the final smoke, this probe
;; emits one stable marker per stage so the first broken layer is visible.

(defvar nemacs-magit-status-diagnose--ok t)
(defvar nemacs-magit-status-diagnose--done nil)

(defun nemacs-magit-status-diagnose--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-magit-status-diagnose--backtrace-lines ()
  (let ((text (with-output-to-string (backtrace))))
    (split-string text "\n" t)))

(defun nemacs-magit-status-diagnose--run (name thunk)
  (nemacs-magit-status-diagnose--print "MAGIT-DIAG %s BEGIN" name)
  (condition-case err
      (let ((value (funcall thunk)))
        (nemacs-magit-status-diagnose--print
         "MAGIT-DIAG %s PASS %S" name value)
        value)
    (error
     (setq nemacs-magit-status-diagnose--ok nil)
     (nemacs-magit-status-diagnose--print
      "MAGIT-DIAG %s ERROR %S" name err)
     (dolist (line (nemacs-magit-status-diagnose--backtrace-lines))
       (nemacs-magit-status-diagnose--print
        "MAGIT-DIAG %s BT %s" name line))
     nil)))

(let* ((fixture (file-name-as-directory (getenv "NEMACS_MAGIT_FIXTURE_DIR")))
       (status-buffer nil)
       (dargs nil)
       (dfiles nil)
       (largs nil)
       (lfiles nil))
  (nemacs-magit-status-diagnose--print "MAGIT-DIAG fixture PASS %S" fixture)

  (nemacs-magit-status-diagnose--run
   "git-toplevel"
   (lambda ()
     (let ((default-directory fixture))
       (magit-toplevel))))

  (nemacs-magit-status-diagnose--run
   "git-head"
   (lambda ()
     (let ((default-directory fixture))
       (magit-git-string "rev-parse" "HEAD"))))

  (nemacs-magit-status-diagnose--run
   "status-values"
   (lambda ()
     (let ((default-directory fixture))
       (pcase-let ((`(,dargs* ,dfiles*)
                    (magit-diff--get-value 'magit-status-mode 'status))
                   (`(,largs* ,lfiles*)
                    (magit-log--get-value 'magit-status-mode 'status)))
         (setq dargs dargs*)
         (setq dfiles dfiles*)
         (setq largs largs*)
         (setq lfiles lfiles*)
         (list :dargs dargs :dfiles dfiles :largs largs :lfiles lfiles)))))

  (nemacs-magit-status-diagnose--run
   "status-values-never-buffer-arguments"
   (lambda ()
     (let ((default-directory fixture)
           (magit-status-use-buffer-arguments 'never))
       (pcase-let ((`(,dargs* ,dfiles*)
                    (magit-diff--get-value 'magit-status-mode 'status))
                   (`(,largs* ,lfiles*)
                    (magit-log--get-value 'magit-status-mode 'status)))
         (list :dargs dargs* :dfiles dfiles* :largs largs* :lfiles lfiles*)))))

  (nemacs-magit-status-diagnose--run
   "diff-raw-git-insert"
   (lambda ()
     (let ((default-directory fixture))
       (with-temp-buffer
         (let ((rc (magit-git-insert "diff" dargs "--no-prefix" "--" dfiles)))
           (list :rc rc
                 :size (buffer-size)
                 :head (buffer-substring-no-properties
                        (point-min)
                        (min (point-max) (+ (point-min) 80)))))))))

  (nemacs-magit-status-diagnose--run
   "diff-git-wash-noop"
   (lambda ()
     (let ((default-directory fixture))
       (with-temp-buffer
         (let ((rc (magit--git-wash
                    (lambda (_args) (goto-char (point-max)))
                    nil
                    "diff" dargs "--no-prefix" "--" dfiles)))
           (list :rc rc :size (buffer-size)))))))

  (nemacs-magit-status-diagnose--run
   "diff-first-headline"
   (lambda ()
     (let ((default-directory fixture))
       (with-temp-buffer
         (magit-git-insert "diff" dargs "--no-prefix" "--" dfiles)
         (goto-char (point-min))
         (and (re-search-forward magit-diff-headline-re nil t)
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))))))

  (nemacs-magit-status-diagnose--run
   "diff-first-wash-diff"
   (lambda ()
     (let ((default-directory fixture))
       (with-temp-buffer
         (funcall 'magit-status-mode)
         (let ((magit-buffer-diff-args dargs)
               (magit-buffer-diff-files dfiles)
               (inhibit-read-only t))
           (magit-git-insert "diff" dargs "--no-prefix" "--" dfiles)
           (goto-char (point-min))
           (when (re-search-forward magit-diff-headline-re nil t)
             (goto-char (line-beginning-position)))
           (let ((value
                  (magit-diff-wash-diff
                   (flatten-tree
                    (list "diff" dargs "--no-prefix" "--" dfiles)))))
             (list :value value :size (buffer-size) :point (point))))))))

  (setq status-buffer
        (nemacs-magit-status-diagnose--run
         "status-setup"
         (lambda ()
           (let ((default-directory fixture))
             (magit-status-setup-buffer fixture))
           (magit-get-mode-buffer 'magit-status-mode))))

  (nemacs-magit-status-diagnose--run
   "status-setup-never-buffer-arguments"
   (lambda ()
     (let ((default-directory fixture)
           (magit-status-use-buffer-arguments 'never))
       (magit-status-setup-buffer fixture))
     (magit-get-mode-buffer 'magit-status-mode)))

  (nemacs-magit-status-diagnose--run
   "status-buffer"
   (lambda ()
     (if (not (bufferp status-buffer))
         (error "missing magit-status buffer")
       (with-current-buffer status-buffer
         (list :mode major-mode
               :size (buffer-size)
               :has-point-section
               (and (> (buffer-size) 0)
                    (get-text-property (point-min) 'magit-section))
               :children
               (and (boundp 'magit-root-section)
                    (oref magit-root-section children)))))))

  (nemacs-magit-status-diagnose--run
   "section-forward"
   (lambda ()
     (if (not (bufferp status-buffer))
         (error "missing magit-status buffer")
       (with-current-buffer status-buffer
         (let ((sec0 (magit-current-section)))
           (magit-section-forward)
           (not (eq sec0 (magit-current-section))))))))

  (nemacs-magit-status-diagnose--run
   "git-after-status"
   (lambda ()
     (let ((default-directory fixture))
       (list :top (magit-toplevel)
             :head (magit-git-string "rev-parse" "HEAD")))))

  (setq nemacs-magit-status-diagnose--done t)
  (nemacs-magit-status-diagnose--print
   "MAGIT-DIAG-SUMMARY %s"
   (if nemacs-magit-status-diagnose--ok "PASS" "FAIL")))

;;; nemacs-magit-status-diagnose-probe.el ends here
