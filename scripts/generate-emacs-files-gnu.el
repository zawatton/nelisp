;;; generate-emacs-files-gnu.el --- extract GNU dir-locals forms  -*- lexical-binding: t; -*-

;; Run: emacs --batch -l scripts/generate-emacs-files-gnu.el
;; Copies the directory-local-variables definitions of GNU Emacs 31.1
;; files.el verbatim from the pinned staged copy into
;; packages/emacs-lib-core/src/emacs-files-gnu.el.

(defun gen-dl--mode-table-registration-p (form)
  "Non-nil when FORM contains an add-to-list into a GNU mode table."
  (and (consp form)
       (or (and (eq (car form) 'add-to-list)
                (member (nth 1 form)
                        '((quote auto-mode-alist) (quote interpreter-mode-alist)
                          (quote magic-mode-alist)
                          (quote magic-fallback-mode-alist))))
           (let ((tail form) found)
             (while (and (consp tail) (not found))
               (setq found (gen-dl--mode-table-registration-p (car tail))
                     tail (cdr tail)))
             found))))

(defconst gen-dl--names
  '(find-file-visit-truename minibuffer-with-setup-hook revert-without-query revert-buffer-function revert-buffer-insert-file-contents-function revert-buffer-in-progress revert-buffer-preserve-modes revert-buffer-restore-functions revert-buffer-restore-read-only revert-buffer revert-buffer--default revert-buffer-insert-file-contents--default-function file-size-human-readable file-size-human-readable-iec files--name-absolute-system-p backup-file-name-p auto-save-file-name-p find-file-run-dired get-free-disk-space byte-count-to-string-function create-file-buffer insert-directory-program files--use-insert-directory-program-p directory-listing-before-filename-regexp insert-directory-ls-version insert-directory-wildcard-in-dir-p insert-directory-clean insert-directory insert-directory-safely enable-local-variables enable-local-eval ignored-local-variables
    safe-local-variable-directories hack-local-variables-hook
    safe-local-variable-values ignored-local-variable-values
    safe-local-eval-forms file-local-variables-alist dir-local-variables-alist
    hack-local-variables-confirm hack-local-variables-filter
    safe-local-variable-p risky-local-variable-p
    hack-local-variables--inhibit-eval
    dir-locals-class-alist dir-locals-directory-cache
    dir-locals-get-class-variables dir-locals-collect-mode-variables
    dir-locals--load-mode-if-needed dir-locals-collect-variables
    dir-locals-set-directory-class dir-locals-set-class-variables
    ;; Directory switching has no standalone implementation yet.  Keep the
    ;; GNU 31.1 definitions together so their CDPATH and validation helpers
    ;; remain byte-for-byte extracted from the pinned source.
    cd-path parse-colon-path cd-absolute cd
    dir-locals-file dir-locals--all-files dir-locals--base-file
    dir-locals-find-file dir-locals--get-sort-score dir-locals--sort-variables
    dir-locals-read-from-dir enable-remote-dir-locals
    hack-dir-local-variables--warned-coding hack-dir-local--get-variables
    hack-dir-local-get-variables-functions hack-dir-local-variables
    hack-dir-local-variables-non-file-buffer
    ;; Applying the collected variables (called by the two functions above).
    hack-local-variables-apply hack-one-local-variable
    hack-one-local-variable--obsolete hack-one-local-variable-quotep
    hack-one-local-variable-eval-safep
    ;; File visiting: `find-file-noselect' and its call path.
    buffer-file-numbers-unique find-buffer-visiting find-file-wildcards
    find-file-suppress-same-file-warnings find-file-existing-other-name
    find-file-not-found-functions query-about-changed-file
    file-local-copy
    view-read-only auto-save-default buffer-file-read-only find-file-literally backup-enable-predicate normal-backup-enable-predicate
    out-of-memory-warning-percentage abort-if-file-too-large warn-maybe-out-of-memory files--message
    find-file-noselect find-file-noselect-1 after-find-file
    find-directory-functions read-directory-name
    ;; The -*- line finder GNU mule.el's `find-auto-coding' calls, and the
    ;; local-variables inhibition it checks.
    inhibit-local-variables-regexps inhibit-local-variables-suffixes
    inhibit-local-variables-ignore-case inhibit-local-variables-p
    file-auto-mode-skip set-auto-mode-1
    ;; GNU's file-name -> major-mode tables and `set-auto-mode' itself.
    auto-mode-alist magic-fallback-mode-alist set-auto-mode
    ;; GNU info.el builds its default directory list with it.
    prune-directory-list
    ;; GNU insert-directory quotes wildcard patterns and switches with it.
    shell-quote-wildcard-pattern
    ;; GNU files.el's file visiting/saving/backup commands and their
    ;; helpers (plan A of the files slice): everything the visiting and
    ;; saving paths reach that the standalone did not define, plus the
    ;; commands NeLisp re-implemented (find-file, save-buffer, write-file,
    ;; save-some-buffers, set-visited-file-name).
    make-backup-files backup-inhibited backup-by-copying
    backup-by-copying-when-linked backup-by-copying-when-mismatch
    backup-by-copying-when-privileged-mismatch buffer-offer-save
    small-temporary-file-directory file-name-invalid-regexp
    file-precious-flag break-hardlink-on-save version-control-safe-local-p
    dired-kept-versions delete-old-versions kept-old-versions
    kept-new-versions auto-save-file-name-transforms auto-save--timer
    auto-save-visited-interval auto-save-visited-predicate
    remote-file-name-inhibit-auto-save-visited save-abbrevs
    lock-file-name-transforms remote-file-name-inhibit-locks
    file-name-at-point-functions write-file-functions local-write-file-hooks
    write-contents-functions untrusted-content trusted-content
    trusted-content-p local-enable-local-variables save-silently
    ange-ftp-completion-hook-function pwd directory-files-recursively
    directory-empty-p module-file-suffix comp-eln-to-el-h
    locate-file-completion-table user-emacs-directory-warning
    require-with-check remote-file-name-inhibit-cache
    remote-file-name-access-timeout files--splice-dirname-file
    file-chase-links files--make-magic-temp-file make-nearby-temp-file
    recode-file-name confirm-nonexistent-file-or-buffer find-file-read-args
    find-file find-file-other-window find-file-other-frame
    find-file-existing find-file--read-only find-file-read-only
    find-file-read-only-other-window find-file-read-only-other-frame
    find-alternate-file-other-window find-alternate-file-dont-kill-client
    find-alternate-file files--ask-user-about-large-file-help-text
    files--ask-user-about-large-file insert-file-1 insert-file-literally
    auto-mode-case-fold conf-mode-maybe interpreter-mode-alist
    auto-mode-interpreter-regexp magic-mode-alist
    magic-mode-regexp-match-limit set-auto-mode--find-matching-alist-entry
    set-auto-mode--apply-alist set-auto-mode--dir-local-valid-p
    set-auto-mode--last major-mode-remap-alist major-mode-remap-defaults
    major-mode-remap set-auto-mode-0 before-hack-local-variables-hook
    permanently-enabled-local-variables hack-local-variables-prop-line
    hack-local-variables--warned-lexical
    internal--get-default-lexical-binding
    hack-local-variables--find-variables change-major-mode-with-file-name
    set-visited-file-name write-file rename-visited-file
    file-extended-attributes set-file-extended-attributes backup-buffer
    backup-buffer-copy file-ownership-preserved-p file-name-with-extension
    file-name-split file-name-parent-directory
    make-backup-file-name-function backup-directory-alist
    make-backup-file-name make-backup-file-name--default-function
    make-backup-file-name-1 backup-extract-version-start
    backup-extract-version find-backup-file-name file-nlinks save-buffer
    delete-auto-save-file-if-necessary auto-save-hook before-save-hook
    after-save-hook copy-directory-create-symlink
    file-preserve-symlinks-on-save save-buffer-coding-system
    basic-save-buffer basic-save-buffer-1 basic-save-buffer-2
    save-some-buffers--switch-window-callback save-some-buffers-action-alist
    save-some-buffers-default-predicate save-some-buffers-root
    files--buffers-needing-to-be-saved save-some-buffers-functions
    save-some-buffers clear-visited-file-modtime not-modified insert-file
    append-to-file file-newest-backup file-backup-file-names rename-uniquely
    files--ensure-directory make-empty-file files--force
    remote-file-name-inhibit-delete-by-moving-to-trash
    file-has-changed-p--hash-table file-has-changed-p copy-directory
    buffer-stale--default-function buffer-stale-function
    buffer-auto-revert-by-notification before-revert-hook
    revert-buffer-internal-hook revert-buffer-with-fine-grain-max-seconds
    revert-buffer-insert-file-contents-delicately
    revert-buffer-with-fine-grain revert-buffer-quick-short-answers
    revert-buffer-quick recover-this-file recover-file recover-session
    recover-session-finish kill-buffer-ask kill-some-buffers
    kill-matching-buffers kill-matching-buffers-no-ask rename-auto-save-file
    make-auto-save-file-name files--transform-file-name make-lock-file-name
    list-directory-brief-switches list-directory-verbose-switches
    find-sibling-rules find-sibling-file find-sibling-file-search
    list-directory directory-free-space-program directory-free-space-args
    kill-emacs-query-functions confirm-kill-emacs confirm-kill-processes
    save-buffers-kill-terminal restart-emacs file-name-non-special
    file-modes-char-to-who file-modes-char-to-right
    file-modes-rights-to-number file-modes-number-to-symbolic
    file-modes-symbolic-to-number read-file-modes trash-directory
    trash--hexify-table move-file-to-trash file-attribute-collect)
  "GNU files.el names this file copies: the directory-local-variables
subsystem, which the standalone lacks (project.el calls
`hack-dir-local-variables').")

(defconst gen-dl--provider-names
  '(parse-colon-path cd-absolute cd file-local-copy)
  "GNU files.el definitions that replace standalone partial providers.")

;; Preloaded GNU loaddefs.el entries for PCL-CVS: GNU's default
;; `find-directory-functions' starts with `cvs-dired-noselect', which
;; loaddefs.el defines in full; the commands it calls autoload from the
;; vendored pcvs.el.  Appended after the files.el forms.
(defconst gen-dl--loaddefs-names
  '(cvs-examine cvs-status cvs-quickdir
    cvs-dired-action cvs-dired-use-hook cvs-dired-noselect))

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
       (src (expand-file-name "vendor/staged-emacs-lisp/files.el" root))
       (loaddefs-src (expand-file-name "vendor/emacs-lisp/loaddefs.el" root))
       (out (expand-file-name "packages/emacs-lib-core/src/emacs-files-gnu.el" root))
       chunks)
  (dolist (spec (list (cons src (cons 0 gen-dl--names))
                      (cons loaddefs-src (cons 20000000 gen-dl--loaddefs-names))))
  (with-temp-buffer
    (insert-file-contents (car spec))
    (emacs-lisp-mode)
    (dolist (name (cddr spec))
      (goto-char (point-min))
      (let ((re (format "^(\\(?:defmacro\\|defvar-local\\|defvar\\|defcustom\\|defconst\\|defsubst\\|defun\\) %s\\(?:[ \n]\\|$\\)\\|^(autoload '%s "
                        (regexp-quote (symbol-name name))
                        (regexp-quote (symbol-name name)))))
        ;; Copy every top-level definition of NAME: files.el defines
        ;; some names twice, e.g. `confirm-nonexistent-file-or-buffer' is
        ;; both a defcustom and a defun.
        (let ((found nil))
         (while (re-search-forward re nil t)
          (setq found t)
          (goto-char (match-beginning 0))
          (let ((beg (point)))
            (forward-sexp 1)
            (let ((form (buffer-substring beg (point))))
              (when (memq name gen-dl--provider-names)
                (setq form
                      (format "(when (emacs-files-gnu--install-provider-p '%s)\n%s\n)"
                              name form)))
              (push (cons (+ beg (cadr spec)) form) chunks))
            ;; Keep the `put' forms that follow the definition directly.
            (while (looking-at "\n+(put '[^ ]+ '[^ ]+ [^\n]*)")
              (let ((pb (progn (skip-chars-forward "\n") (point))))
                (forward-sexp 1)
                (push (cons (+ pb (cadr spec)) (buffer-substring pb (point))) chunks)))))
         (unless found (message "skip %s" name)))))))
  ;; GNU files.el's own key bindings for its commands (top-level
  ;; define-key forms on the C-x maps and esc-map) and its
  ;; (provide 'files '(remote-wildcards)), copied verbatim.
  (with-temp-buffer
    (insert-file-contents src)
    (goto-char (point-min))
    (while (re-search-forward
            "^(\\(?:define-key \\(?:ctl-x-\\(?:[45]-\\)?\\|esc-\\)map \\|provide 'files \\)" nil t)
      (goto-char (match-beginning 0))
      (let ((beg (point)))
        (forward-sexp 1)
        (push (cons beg (buffer-substring beg (point))) chunks))))
  (setq chunks (sort (delete-dups chunks) (lambda (a b) (< (car a) (car b)))))
  ;; GNU loaddefs.el's top-level registrations into the mode tables above
  ;; (e.g. org-mode for .org), copied verbatim after the files.el forms,
  ;; in loaddefs order, as GNU loadup loads loaddefs after files.el.
  (let ((ld (expand-file-name "../emacs-lisp/loaddefs.el"
                              (file-name-directory src)))
        (base (1+ (or (caar (last chunks)) 0))))
    (with-temp-buffer
      (insert-file-contents ld)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (let* ((beg (point)) (form (read (current-buffer))))
              (when (or (and (memq (car-safe form) '(add-to-list when dolist))
                             (gen-dl--mode-table-registration-p form))
                        ;; A regexp constant a registration uses, such as
                        ;; `python--auto-mode-alist-regexp'.
                        (and (memq (car-safe form) '(defconst defvar))
                             (symbolp (nth 1 form))
                             (string-match-p "auto-mode-alist"
                                             (symbol-name (nth 1 form)))))
                (setq chunks (append chunks
                                     (list (cons (+ base beg)
                                                 (buffer-substring beg (point)))))))))
        (end-of-file nil))))
  (with-temp-file out
    (insert ";;; emacs-files-gnu.el --- GNU files.el definitions  -*- lexical-binding: t; -*-\n\n"
            ";; GENERATED by scripts/generate-emacs-files-gnu.el from the pinned\n"
            ";; vendor/staged-emacs-lisp/files.el (GNU Emacs 31.1).  The forms below\n"
            ";; are copied verbatim; do not edit by hand.\n\n"
            "(defun emacs-files-gnu--install-provider-p (name)\n"
            "  \"Install a GNU files.el provider only when it fills a gap.\"\n"
            "  (or (not (fboundp name)) (fboundp 'nelisp--write-stdout-bytes)))\n\n")
    (dolist (c chunks) (insert (cdr c) "\n\n"))
    (insert "(provide 'emacs-files-gnu)\n\n;;; emacs-files-gnu.el ends here\n")))
