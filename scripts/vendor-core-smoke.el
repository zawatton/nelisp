;;; vendor-core-smoke.el --- smoke daily-driver vendor modules  -*- lexical-binding: t; -*-

;; This file is loaded by `make verify-vendor-core' under the NeLisp
;; CLI.  Lightweight lanes load their own shim features directly; lanes
;; that still need the full Layer 2 substrate explicitly require
;; `emacs-init' inside their checker.

;;; Code:

(defvar vendor-core-smoke-candidates
  '((files . vendor-core-smoke--check-files)
    (simple . vendor-core-smoke--check-simple)
    (dired . vendor-core-smoke--check-dired)
    (help-mode . vendor-core-smoke--check-help-mode)
    (help-fns . vendor-core-smoke--check-help-fns)
    (subr-x . vendor-core-smoke--check-subr-x)
    (seq . vendor-core-smoke--check-seq)
    (map . vendor-core-smoke--check-map)
    (lisp . vendor-core-smoke--check-lisp)
    (case-table . vendor-core-smoke--check-case-table)
    (cdl . vendor-core-smoke--check-cdl)
    (range . vendor-core-smoke--check-range)
    (regi . vendor-core-smoke--check-regi)
    (lisp-mode . vendor-core-smoke--check-lisp-mode)
    (ielm . vendor-core-smoke--check-ielm)
    (isearch . vendor-core-smoke--check-isearch)
    (minibuffer . vendor-core-smoke--check-minibuffer)
    (project . vendor-core-smoke--check-project)
    (hex-util . vendor-core-smoke--check-hex-util)
    (map-ynp . vendor-core-smoke--check-map-ynp)
    (charprop . vendor-core-smoke--check-charprop)
    (charscript . vendor-core-smoke--check-charscript)
    (emoji-labels . vendor-core-smoke--check-emoji-labels)
    (iso-transl . vendor-core-smoke--check-iso-transl)
    (cp51932 . vendor-core-smoke--check-cp51932)
    (eucjp-ms . vendor-core-smoke--check-eucjp-ms)
    (fontset . vendor-core-smoke--check-fontset)
    (idna-mapping . vendor-core-smoke--check-idna-mapping)
    (ja-dic-utl . vendor-core-smoke--check-ja-dic-utl))
  "Daily-driver vendor module candidates from Doc 03 Phase 1.")

(defvar vendor-core-smoke-modules nil
  "Explicit daily-driver vendor modules to smoke.
When nil, `vendor-core-smoke--selected-modules' chooses the first
`vendor-core-smoke-default-limit' entries from
`vendor-core-smoke-candidates'.")

(defvar vendor-core-smoke-module-spec nil
  "Comma/whitespace-separated module names overriding the limit.
This mirrors VENDOR_CORE_MODULES but can be set directly by callers
whose NeLisp environment does not expose process env vars reliably.")

(defvar vendor-core-smoke-default-limit 0
  "Default number of candidate modules to smoke.
Use 0 to smoke all candidates.  The default is 0 because the
standalone-reader gate now verifies the full current daily-driver
vendor-core candidate set.")

(defvar vendor-core-smoke-strict t
  "Non-nil means signal an error when any core smoke fails.")

(defun vendor-core-smoke--env-flag-p (name)
  "Return non-nil when env var NAME is set to 1, t, true, or yes."
  (let ((value (and (fboundp 'getenv) (getenv name))))
    (and value (member value '("1" "t" "true" "yes")))))

(defun vendor-core-smoke--env-number (name default)
  "Return numeric env var NAME, or DEFAULT."
  (let ((value (and (fboundp 'getenv) (getenv name))))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun vendor-core-smoke--separator-char-p (char)
  "Return non-nil when CHAR separates module names."
  (or (= char 44)  ; comma
      (= char 32)  ; space
      (= char 9)   ; tab
      (= char 10))) ; newline

(defun vendor-core-smoke--parse-module-symbols (value)
  "Parse VALUE as comma/whitespace-separated module symbols."
  (let ((len (length value))
        (start 0)
        (i 0)
        modules)
    (while (<= i len)
      (when (or (= i len)
                (vendor-core-smoke--separator-char-p (aref value i)))
        (let ((token (substring value start i)))
          (unless (= (length token) 0)
            (push (intern token) modules)))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (nreverse modules)))

(defun vendor-core-smoke--candidate-entry (feature)
  "Return candidate entry for FEATURE, or signal an error."
  (let ((entry (assoc feature vendor-core-smoke-candidates)))
    (unless entry
      (error "unknown vendor core module: %S" feature))
    entry))

(defun vendor-core-smoke--modules-from-env ()
  "Return explicit module entries from VENDOR_CORE_MODULES, if set."
  (let ((value (or vendor-core-smoke-module-spec
                   (and (fboundp 'getenv)
                        (getenv "VENDOR_CORE_MODULES")))))
    (when (and value (not (string= value "")))
      (let (entries)
        (dolist (feature (vendor-core-smoke--parse-module-symbols value))
          (push (vendor-core-smoke--candidate-entry feature) entries))
        (nreverse entries)))))

(defun vendor-core-smoke--strict-p ()
  "Return non-nil when the smoke should fail on module errors."
  (or vendor-core-smoke-strict
      (vendor-core-smoke--env-flag-p "VENDOR_CORE_STRICT")))

(defun vendor-core-smoke--assert (condition message)
  "Signal MESSAGE unless CONDITION is non-nil."
  (unless condition
    (error "%s" message)))

(defun vendor-core-smoke--check-fbound (symbols)
  "Assert that every symbol in SYMBOLS has a function binding."
  (dolist (sym symbols)
    (vendor-core-smoke--assert
     (fboundp sym)
     (format "%S is not fbound after vendor load" sym))))

(defun vendor-core-smoke--check-bound (symbols)
  "Assert that every symbol in SYMBOLS has a variable binding."
  (dolist (sym symbols)
    (vendor-core-smoke--assert
     (boundp sym)
     (format "%S is not bound after vendor load" sym))))

(defun vendor-core-smoke--check-feature-provided (feature)
  "Assert that FEATURE has been provided."
  (vendor-core-smoke--assert
   (featurep feature)
   (format "%S feature was not provided" feature)))

(defun vendor-core-smoke--check-translation-tables (symbols)
  "Assert that every symbol in SYMBOLS names a registered translation table."
  (dolist (sym symbols)
    (vendor-core-smoke--assert
     (get sym 'translation-table)
     (format "%S has no translation-table property" sym))))

(defun vendor-core-smoke--check-key (map-symbol key command)
  "Assert that MAP-SYMBOL binds KEY to COMMAND."
  (vendor-core-smoke--assert
   (boundp map-symbol)
   (format "%S is not bound" map-symbol))
  (let ((map (symbol-value map-symbol)))
    (vendor-core-smoke--assert
     (keymapp map)
     (format "%S is not a keymap" map-symbol))
    (let ((actual (lookup-key map key)))
      (vendor-core-smoke--assert
       (eq actual command)
       (format "%S key %S expected %S, got %S"
	       map-symbol key command actual)))))

(defun vendor-core-smoke--selected-modules ()
  "Return the vendor module entries to smoke."
  (or vendor-core-smoke-modules
      (vendor-core-smoke--modules-from-env)
      (let ((limit (vendor-core-smoke--env-number
                    "VENDOR_CORE_LIMIT"
                    vendor-core-smoke-default-limit))
            (modules vendor-core-smoke-candidates)
            selected)
        (if (<= limit 0)
            modules
          (while (and modules (> limit 0))
            (push (car modules) selected)
            (setq modules (cdr modules)
                  limit (1- limit)))
          (nreverse selected)))))

(defun vendor-core-smoke--check-files ()
  "Load the daily-driver files feature and verify M1 file entry points."
  (require 'files)
  (vendor-core-smoke--assert
   (featurep 'files)
   "files feature was not provided")
  (vendor-core-smoke--check-fbound
   '(find-file find-file-read-only find-alternate-file save-buffer
               save-some-buffers write-file insert-file list-directory))
  (vendor-core-smoke--check-key 'ctl-x-map "\C-f" 'find-file)
  (vendor-core-smoke--check-key 'ctl-x-map "\C-r" 'find-file-read-only)
  (vendor-core-smoke--check-key 'ctl-x-map "\C-v" 'find-alternate-file)
  (vendor-core-smoke--check-key 'ctl-x-map "\C-s" 'save-buffer)
  (vendor-core-smoke--check-key 'ctl-x-map "\C-w" 'write-file)
  (vendor-core-smoke--check-key 'ctl-x-map "i" 'insert-file)
  (vendor-core-smoke--check-key 'ctl-x-4-map "f" 'find-file-other-window)
  (vendor-core-smoke--check-key 'ctl-x-5-map "f" 'find-file-other-frame)
  'ok)

(defun vendor-core-smoke--check-simple ()
  "Load the daily-driver simple feature and verify core editing commands."
  (require 'simple)
  (vendor-core-smoke--assert (featurep 'simple)
                             "simple feature was not provided")
  (vendor-core-smoke--check-fbound
   '(open-line quoted-insert indent-for-tab-command))
  (when (featurep 'emacs-line-builtins)
    (vendor-core-smoke--check-fbound '(beginning-of-line end-of-line)))
  (when (featurep 'emacs-edit-builtins)
    (vendor-core-smoke--check-fbound '(kill-line newline)))
  'ok)

(defun vendor-core-smoke--check-dired ()
  "Load the daily-driver dired feature and verify file manager entry points."
  (require 'dired)
  (vendor-core-smoke--assert (featurep 'dired)
                             "dired feature was not provided")
  (vendor-core-smoke--check-fbound
   '(dired dired-mode dired-find-file dired-next-line
           dired-previous-line dired-up-directory))
  'ok)

(defun vendor-core-smoke--check-help-mode ()
  "Load the daily-driver help-mode feature and verify help buffer surfaces."
  (require 'help-mode)
  (vendor-core-smoke--assert (featurep 'help-mode)
                             "help-mode feature was not provided")
  (vendor-core-smoke--check-fbound '(help-mode help-go-back help-go-forward))
  'ok)

(defun vendor-core-smoke--check-help-fns ()
  "Load the daily-driver help-fns feature and verify describe-* entry points."
  (require 'help-fns)
  (vendor-core-smoke--assert (featurep 'help-fns)
                             "help-fns feature was not provided")
  (vendor-core-smoke--check-fbound
   '(describe-function describe-variable describe-symbol))
  'ok)

(defun vendor-core-smoke--check-subr-x ()
  "Load the common subr-x support feature and verify vendor helpers."
  (require 'subr-x)
  (vendor-core-smoke--assert (featurep 'subr-x)
                             "subr-x feature was not provided")
  (vendor-core-smoke--check-fbound
   '(thread-first thread-last hash-table-empty-p hash-table-keys
                  hash-table-values string-remove-prefix
                  string-remove-suffix string-replace string-limit
                  string-pad proper-list-p mapcan))
  'ok)

(defun vendor-core-smoke--check-seq ()
  "Load the common seq support feature and verify vendor helpers."
  (require 'seq)
  (vendor-core-smoke--assert (featurep 'seq)
                             "seq feature was not provided")
  (vendor-core-smoke--check-fbound
   '(seqp seq-length seq-elt seq-map seq-filter seq-remove seq-find
          seq-some seq-every-p seq-reduce seq-uniq seq-concatenate))
  'ok)

(defun vendor-core-smoke--check-map ()
  "Load the common map support feature and verify vendor helpers."
  (require 'map)
  (vendor-core-smoke--assert (featurep 'map)
                             "map feature was not provided")
  (vendor-core-smoke--check-fbound
   '(mapp map-elt map-keys map-values map-pairs map-apply map-do
          map-empty-p map-contains-key map-merge map-merge-with
          map-into map-put! map-insert))
  'ok)

(defun vendor-core-smoke--check-lisp ()
  "Load the common Lisp editing feature and verify sexp helpers."
  (require 'lisp)
  (vendor-core-smoke--assert (featurep 'lisp)
                             "lisp feature was not provided")
  (vendor-core-smoke--check-fbound
   '(forward-sexp backward-sexp mark-sexp forward-list backward-list
          down-list up-list backward-up-list kill-sexp backward-kill-sexp
          beginning-of-defun end-of-defun mark-defun insert-pair
          insert-parentheses delete-pair check-parens))
  'ok)

(defun vendor-core-smoke--check-case-table ()
  "Load the common case-table feature and verify helpers."
  (require 'case-table)
  (vendor-core-smoke--assert (featurep 'case-table)
                             "case-table feature was not provided")
  (vendor-core-smoke--check-fbound
   '(describe-buffer-case-table case-table-get-table get-upcase-table
          copy-case-table set-case-syntax-delims set-case-syntax-pair
          set-upcase-syntax set-downcase-syntax set-case-syntax
          make-char-table char-table-p char-table-range
          set-char-table-range current-case-table standard-case-table))
  'ok)

(defun vendor-core-smoke--check-cdl ()
  "Load the common cdl support feature and verify helpers."
  (require 'cdl)
  (vendor-core-smoke--assert (featurep 'cdl)
                             "cdl feature was not provided")
  (vendor-core-smoke--check-fbound
   '(cdl-get-file cdl-put-region call-process call-process-region))
  'ok)

(defun vendor-core-smoke--check-range ()
  "Load the common range support feature and verify helpers."
  (require 'range)
  (vendor-core-smoke--assert (featurep 'range)
                             "range feature was not provided")
  (vendor-core-smoke--check-fbound
   '(range-normalize range-denormalize range-difference
          range-intersection range-compress-list range-uncompress
          range-add-list range-remove range-member-p
          range-list-intersection range-list-difference range-length
          range-concat range-map))
  'ok)

(defun vendor-core-smoke--check-regi ()
  "Load the common regi support feature and verify helpers."
  (require 'regi)
  (vendor-core-smoke--assert (featurep 'regi)
                             "regi feature was not provided")
  (vendor-core-smoke--check-fbound
   '(regi-pos regi-mapcar regi-interpret))
  'ok)

(defun vendor-core-smoke--check-lisp-mode ()
  "Load the daily-driver lisp-mode feature and verify Elisp editing modes."
  (require 'lisp-mode)
  (vendor-core-smoke--assert (featurep 'lisp-mode)
                             "lisp-mode feature was not provided")
  (vendor-core-smoke--check-fbound
   '(emacs-lisp-mode lisp-mode eval-defun indent-sexp))
  'ok)

(defun vendor-core-smoke--check-ielm ()
  "Load the daily-driver ielm feature and verify the REPL command surface."
  (require 'ielm)
  (vendor-core-smoke--assert (featurep 'ielm)
                             "ielm feature was not provided")
  (vendor-core-smoke--check-fbound '(ielm ielm-send-input))
  'ok)

(defun vendor-core-smoke--check-isearch ()
  "Load the daily-driver isearch feature and verify incremental search."
  (require 'isearch)
  (vendor-core-smoke--assert (featurep 'isearch)
                             "isearch feature was not provided")
  (vendor-core-smoke--check-fbound
   '(isearch-forward isearch-backward isearch-forward-regexp))
  'ok)

(defun vendor-core-smoke--check-minibuffer ()
  "Load the daily-driver minibuffer feature and verify completion commands."
  (require 'minibuffer)
  (vendor-core-smoke--assert (featurep 'minibuffer)
                             "minibuffer feature was not provided")
  (vendor-core-smoke--check-fbound
   '(completing-read minibuffer-complete minibuffer-complete-and-exit))
  'ok)

(defun vendor-core-smoke--check-project ()
  "Load the daily-driver project feature and verify project commands."
  (require 'project)
  (vendor-core-smoke--assert (featurep 'project)
                             "project feature was not provided")
  (vendor-core-smoke--check-fbound
   '(project-current project-find-file project-switch-project))
  'ok)

(defun vendor-core-smoke--check-hex-util ()
  "Load the lightweight hex-util feature and verify octet helpers."
  (require 'hex-util)
  (vendor-core-smoke--check-feature-provided 'hex-util)
  (vendor-core-smoke--check-fbound
   '(decode-hex-string encode-hex-string))
  'ok)

(defun vendor-core-smoke--check-map-ynp ()
  "Load the lightweight map-ynp feature and verify prompt helpers."
  (require 'map-ynp)
  (vendor-core-smoke--check-feature-provided 'map-ynp)
  (vendor-core-smoke--check-fbound
   '(map-y-or-n-p read-answer))
  (vendor-core-smoke--check-bound
   '(read-answer-short read-answer-map--memoize))
  'ok)

(defun vendor-core-smoke--check-charprop ()
  "Load the lightweight charprop feature and verify Unicode property surface."
  (require 'charprop)
  (vendor-core-smoke--check-feature-provided 'charprop)
  (vendor-core-smoke--check-fbound
   '(define-char-code-property get-char-code-property
                               put-char-code-property
                               unicode-property-table-internal
                               char-code-property-description))
  'ok)

(defun vendor-core-smoke--check-charscript ()
  "Load the lightweight charscript feature and verify script table data."
  (require 'charscript)
  (vendor-core-smoke--check-feature-provided 'charscript)
  (vendor-core-smoke--check-bound '(char-script-table))
  (vendor-core-smoke--assert
   (and (boundp 'char-script-table)
        (char-table-p char-script-table))
   "char-script-table is not a char table")
  'ok)

(defun vendor-core-smoke--check-emoji-labels ()
  "Load the lightweight emoji-labels feature and verify data tables."
  (require 'emoji-labels)
  (vendor-core-smoke--check-feature-provided 'emoji-labels)
  (vendor-core-smoke--check-bound
   '(emoji--labels emoji--names emoji--derived))
  (vendor-core-smoke--assert
   (hash-table-p emoji--names)
   "emoji--names is not a hash table")
  (vendor-core-smoke--assert
   (hash-table-p emoji--derived)
   "emoji--derived is not a hash table")
  'ok)

(defun vendor-core-smoke--check-iso-transl ()
  "Load the lightweight iso-transl feature and verify C-x 8 translation data."
  (require 'iso-transl)
  (vendor-core-smoke--check-feature-provided 'iso-transl)
  (vendor-core-smoke--check-fbound
   '(iso-transl-define-keys iso-transl-set-language))
  (vendor-core-smoke--check-bound
   '(iso-transl-char-map iso-transl-language-alist
                         iso-transl-ctl-x-8-map key-translation-map))
  (vendor-core-smoke--assert
   (keymapp iso-transl-ctl-x-8-map)
   "iso-transl-ctl-x-8-map is not a keymap")
  'ok)

(defun vendor-core-smoke--check-cp51932 ()
  "Load the lightweight cp51932 feature and verify translation tables."
  (require 'cp51932)
  (vendor-core-smoke--check-feature-provided 'cp51932)
  (vendor-core-smoke--check-translation-tables
   '(cp51932-decode cp51932-encode))
  'ok)

(defun vendor-core-smoke--check-eucjp-ms ()
  "Load the lightweight eucjp-ms feature and verify translation tables."
  (require 'eucjp-ms)
  (vendor-core-smoke--check-feature-provided 'eucjp-ms)
  (vendor-core-smoke--check-translation-tables
   '(eucjp-ms-decode eucjp-ms-encode))
  'ok)

(defun vendor-core-smoke--check-fontset ()
  "Load the lightweight fontset feature and verify GUI-neutral helpers."
  (require 'fontset)
  (vendor-core-smoke--check-feature-provided 'fontset)
  (vendor-core-smoke--check-fbound
   '(x-decompose-font-name x-compose-font-name set-font-encoding
                           fontset-name-p fontset-plain-name
                           generate-fontset-menu setup-default-fontset
                           create-default-fontset))
  (vendor-core-smoke--check-bound
   '(font-encoding-alist script-representative-chars
                         fontset-alias-alist standard-fontset-spec))
  'ok)

(defun vendor-core-smoke--check-idna-mapping ()
  "Load the lightweight idna-mapping feature and verify direct elt table."
  (require 'idna-mapping)
  (vendor-core-smoke--check-feature-provided 'idna-mapping)
  (vendor-core-smoke--check-bound '(idna-mapping-table))
  (vendor-core-smoke--assert
   (and (vectorp idna-mapping-table)
        (> (length idna-mapping-table) #x10ffff))
   "idna-mapping-table is not a full Unicode vector")
  'ok)

(defun vendor-core-smoke--check-ja-dic-utl ()
  "Load the lightweight ja-dic-utl feature and verify SKK helper surface."
  (require 'ja-dic-utl)
  (vendor-core-smoke--check-feature-provided 'ja-dic-utl)
  (vendor-core-smoke--check-fbound
   '(skkdic-lookup-key skkdic-merge-head-and-tail))
  (vendor-core-smoke--check-bound
   '(skkdic-okurigana-table skkdic-okuri-ari skkdic-okuri-nasi
                            skkdic-prefix skkdic-postfix))
  'ok)

(defun vendor-core-smoke--run-one (entry)
  "Run one core smoke ENTRY and return (FEATURE STATUS DETAIL)."
  (let ((feature (car entry))
        (checker (cdr entry)))
    (condition-case err
        (progn
          (funcall checker)
          (list feature 'pass ""))
      (error
       (list feature 'fail (format "%S" err))))))

(defun vendor-core-smoke-batch ()
  "Run the daily-driver vendor smoke suite."
  (let ((failures 0)
        (modules (vendor-core-smoke--selected-modules))
        results)
    (dolist (entry modules)
      (princ (format "vendor-core module=%S status=start detail=\n"
                     (car entry)))
      (let ((result (vendor-core-smoke--run-one entry)))
        (push result results)
        (when (eq (cadr result) 'fail)
          (setq failures (1+ failures)))
        (princ (format "vendor-core module=%S status=%S detail=%s\n"
                       (car result) (cadr result) (caddr result)))))
    (princ (format "vendor-core-summary total=%d candidates=%d failures=%d strict=%S\n"
                   (length modules)
                   (length vendor-core-smoke-candidates)
                   failures
                   (vendor-core-smoke--strict-p)))
    (when (and (> failures 0)
               (vendor-core-smoke--strict-p))
      (error "vendor core smoke failed: %d/%d"
             failures (length modules)))
    (nreverse results)))

(provide 'vendor-core-smoke)

;;; vendor-core-smoke.el ends here
