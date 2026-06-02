;;; nemacs-runtime-image-preload-test.el --- tests for image preload helpers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name
       "../scripts/nemacs-runtime-image-preload.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(load (expand-file-name
       "../scripts/vendor-core-smoke.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst nemacs-runtime-image-preload-test--repo-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst nemacs-runtime-image-preload-test--direct-install-functions
  '(buffer-file-name
    set-visited-file-name
    find-file
    find-file-noselect
    find-file-read-only
    find-alternate-file
    find-file-other-window
    find-file-other-frame
    save-buffer
    save-some-buffers
    write-file
    insert-file
    list-directory
    dired
    dired-mode
    dired-find-file
    dired-next-line
    dired-previous-line
    dired-up-directory
    help-mode
    help-go-back
    help-go-forward
    describe-function
    describe-variable
    describe-symbol
    describe-key
    emacs-lisp-mode
    lisp-mode
    eval-defun
    indent-sexp
    ielm
    ielm-send-input
    isearch-forward
    isearch-backward
    isearch-forward-regexp
    completing-read
    minibuffer-complete
    minibuffer-complete-and-exit
    project-current
    project-find-file
    project-switch-project
    open-line
    quoted-insert
    indent-for-tab-command
    thread-first
    thread-last
    hash-table-empty-p
    hash-table-keys
    hash-table-values
    string-remove-prefix
    string-remove-suffix
    string-replace
    string-limit
    string-pad
    proper-list-p
    mapcan
    seqp
    seq-length
    seq-elt
    seq-map
    seq-filter
    seq-remove
    seq-find
    seq-some
    seq-every-p
    seq-reduce
    seq-uniq
    seq-concatenate
    mapp
    map-elt
    map-keys
    map-values
    map-pairs
    map-apply
    map-do
    map-empty-p
    map-contains-key
    map-merge
    map-merge-with
    map-into
    map-put!
    map-insert
    forward-sexp
    backward-sexp
    mark-sexp
    forward-list
    backward-list
    down-list
    up-list
    backward-up-list
    kill-sexp
    backward-kill-sexp
    buffer-end
    forward-sexp-default-function
    beginning-of-defun
    beginning-of-defun-raw
    beginning-of-defun-comments
    end-of-defun
    mark
    set-mark
    push-mark
    mark-defun
    narrow-to-defun
    insert-pair
    insert-parentheses
    delete-pair
    kill-backward-up-list
    raise-sexp
    move-past-close-and-reindent
    check-parens
    field-complete
    lisp-complete-symbol
    describe-buffer-case-table
    case-table-get-table
    get-upcase-table
    copy-case-table
    set-case-syntax-delims
    set-case-syntax-pair
    set-upcase-syntax
    set-downcase-syntax
    set-case-syntax
    make-char-table
    char-table-p
    char-table-range
    set-char-table-range
    char-table-extra-slot
    set-char-table-extra-slot
    map-char-table
    set-char-table-parent
    current-case-table
    standard-case-table
    set-case-table
    set-standard-case-table
    standard-syntax-table
    modify-syntax-entry
    cdl-get-file
    cdl-put-region
    range-normalize
    range-denormalize
    range-difference
    range-intersection
    range-compress-list
    range-uncompress
    range-add-list
    range-remove
    range-member-p
    range-list-intersection
    range-list-difference
    range-length
    range-concat
    range-map
    regi-pos
    regi-mapcar
    regi-interpret
    decode-hex-string
    encode-hex-string
    map-y-or-n-p
    read-answer
    iso-transl-define-keys
    iso-transl-set-language
    x-decompose-font-name
    x-compose-font-name
    set-font-encoding
    fontset-name-p
    fontset-plain-name
    generate-fontset-menu
    setup-default-fontset
    create-default-fontset
    skkdic-lookup-key
    skkdic-merge-head-and-tail)
  "Public command surface installed directly into vendor-core images.")

(defconst nemacs-runtime-image-preload-test--direct-install-variables
  '(ctl-x-map
    ctl-x-4-map
    ctl-x-5-map
    files--current-file-name
    files--buffer-file-names
    files--buffer-string
    files--buffer-strings
    files--point
    files--buffer-points
    files--buffer-modified-p
    files--buffer-modified-flags
    max-mini-window-lines
    indent-line-function
    defun-prompt-regexp
    parens-require-spaces
    forward-sexp-function
    beginning-of-defun-function
    end-of-defun-function
    end-of-defun-moves-to-eol
    narrow-to-defun-include-comments
    insert-pair-alist
    delete-pair-blink-delay
    lisp--mark
    mark-active
    case-table--standard
    case-table--current
    case-table--standard-syntax-table
    read-answer-short
    read-answer-map--memoize
    charprop--registry
    char-script-table
    emoji--labels
    emoji--names
    emoji--derived
    key-translation-map
    iso-transl-ctl-x-8-map
    iso-transl-char-map
    iso-transl-language-alist
    font-encoding-alist
    script-representative-chars
    fontset-alias-alist
    standard-fontset-spec
    idna-mapping-table
    skkdic-okurigana-table
    skkdic-okuri-ari
    skkdic-okuri-nasi
    skkdic-prefix
    skkdic-postfix
    curline
    curframe
    curentry)
  "Global variables installed directly into vendor-core images.")

(defconst nemacs-runtime-image-preload-test--direct-install-features
  '(files simple dired help-mode help-fns lisp-mode ielm isearch
          minibuffer project subr-x seq map lisp case-table cdl range regi
          hex-util map-ynp charprop charscript emoji-labels iso-transl
          cp51932 eucjp-ms fontset idna-mapping ja-dic-utl)
  "Features installed directly into vendor-core images.")

(defconst nemacs-runtime-image-preload-test--translation-symbols
  '(cp51932-decode cp51932-encode eucjp-ms-decode eucjp-ms-encode)
  "Translation-table symbols installed directly into vendor-core images.")

(defvar nemacs-runtime-image-preload-test--seen nil
  "Dynamic scratch list used by direct runtime-image facade tests.")

(defmacro nemacs-runtime-image-preload-test--with-clean-direct-install (&rest body)
  "Run BODY with direct-install symbols temporarily unbound."
  (declare (indent 0) (debug (body)))
  `(let ((function-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol)
                               (symbol-function symbol))))
                  nemacs-runtime-image-preload-test--direct-install-functions))
         (value-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (boundp symbol)
                               (list (symbol-value symbol)))))
                  nemacs-runtime-image-preload-test--direct-install-variables))
         (translation-cells
          (mapcar (lambda (symbol)
                    (cons symbol (get symbol 'translation-table)))
                  nemacs-runtime-image-preload-test--translation-symbols))
         (original-features features))
     (unwind-protect
         (progn
           (dolist (feature nemacs-runtime-image-preload-test--direct-install-features)
             (setq features (remove feature features)))
           (dolist (symbol nemacs-runtime-image-preload-test--direct-install-functions)
             (fmakunbound symbol))
           (dolist (symbol nemacs-runtime-image-preload-test--direct-install-variables)
             (makunbound symbol))
           ,@body)
       (setq features original-features)
       (dolist (cell function-cells)
         (if (cdr cell)
             (let ((native-comp-enable-subr-trampolines nil))
               (fset (car cell) (cdr cell)))
           (fmakunbound (car cell))))
       (dolist (cell value-cells)
         (if (cdr cell)
             (set (car cell) (cadr cell))
           (makunbound (car cell))))
       (dolist (cell translation-cells)
         (put (car cell) 'translation-table (cdr cell))))))

(ert-deftest nemacs-runtime-image-preload-test/feature-loaded ()
  (should (featurep 'nemacs-runtime-image-preload))
  (dolist (sym '(nemacs-runtime-image-setup-paths
                 nemacs-runtime-image-load-bootstrap
                 nemacs-runtime-image-preload-batch
                 nemacs-runtime-image-preload-interactive
                 nemacs-runtime-image-preload-vendor-core
                 nemacs-runtime-image-preload-vendor-core-extension
                 nemacs-runtime-image-preload--install-file-command
                 nemacs-runtime-image-preload--install-files-core
                 nemacs-runtime-image-preload--install-dired-command
                 nemacs-runtime-image-preload--install-dired-core
                 nemacs-runtime-image-preload--install-help-command
                 nemacs-runtime-image-preload--install-help-core
                 nemacs-runtime-image-preload--install-module-command
                 nemacs-runtime-image-preload--install-elisp-core
                 nemacs-runtime-image-preload--install-ielm-core
                 nemacs-runtime-image-preload--install-isearch-core
                 nemacs-runtime-image-preload--install-minibuffer-core
                 nemacs-runtime-image-preload--install-project-core
                 nemacs-runtime-image-preload--install-simple-core
                 nemacs-runtime-image-preload--install-subr-x-core
                 nemacs-runtime-image-preload--install-seq-core
                 nemacs-runtime-image-preload--install-map-core
                 nemacs-runtime-image-preload--install-lisp-core
                 nemacs-runtime-image-preload--install-case-table-core
                 nemacs-runtime-image-preload--install-cdl-core
                 nemacs-runtime-image-preload--install-range-core
                 nemacs-runtime-image-preload--install-regi-core
                 nemacs-runtime-image-preload--install-support-core
                 nemacs-runtime-image-preload--install-hex-util-core
                 nemacs-runtime-image-preload--install-map-ynp-core
                 nemacs-runtime-image-preload--install-charprop-core
                 nemacs-runtime-image-preload--install-charscript-core
                 nemacs-runtime-image-preload--install-emoji-labels-core
                 nemacs-runtime-image-preload--install-iso-transl-core
                 nemacs-runtime-image-preload--install-translation-table-core
                 nemacs-runtime-image-preload--install-fontset-core
                 nemacs-runtime-image-preload--install-idna-mapping-core
                 nemacs-runtime-image-preload--install-ja-dic-utl-core
                 nemacs-runtime-image-preload--install-utility-i18n-core))
    (should (fboundp sym))))

(ert-deftest nemacs-runtime-image-preload-test/setup-paths-adds-src-and-vendor ()
  (let ((load-path nil))
    (should (nemacs-runtime-image-setup-paths
             nemacs-runtime-image-preload-test--repo-root))
    (should (equal (symbol-value 'nelisp-emacs-vendor-root)
                   (concat nemacs-runtime-image-preload-test--repo-root
                           "/vendor")))
    (dolist (path (list (concat nemacs-runtime-image-preload-test--repo-root
                                "/src")
                        (concat nemacs-runtime-image-preload-test--repo-root
                                "/vendor/emacs-lisp")
                        (concat nemacs-runtime-image-preload-test--repo-root
                                "/vendor/emacs-lisp/emacs-lisp")
                        (concat nemacs-runtime-image-preload-test--repo-root
                                "/vendor/emacs-lisp/vc")))
      (should (member path load-path)))))

(ert-deftest nemacs-runtime-image-preload-test/load-bootstrap-skips-empty ()
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load)
               (lambda (&rest _)
                 (setq loaded t))))
      (should (nemacs-runtime-image-load-bootstrap ""))
      (should-not loaded))))

(ert-deftest nemacs-runtime-image-preload-test/preload-batch-requires-main ()
  (let ((setup-args nil)
        (bootstrap-args nil)
        (required nil))
    (cl-letf (((symbol-function 'nemacs-runtime-image-setup-paths)
               (lambda (repo-root)
                 (setq setup-args (list repo-root))
                 t))
              ((symbol-function 'nemacs-runtime-image-load-bootstrap)
               (lambda (bootstrap-file)
                 (setq bootstrap-args (list bootstrap-file))
                 t))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature)))
      (should (nemacs-runtime-image-preload-batch "/repo" "/bootstrap.el"))
      (should (equal setup-args '("/repo")))
      (should (equal bootstrap-args '("/bootstrap.el")))
      (should (equal required '(nemacs-main))))))

(ert-deftest nemacs-runtime-image-preload-test/preload-vendor-core-requires-files ()
  (let ((base-args nil)
        (extension-called nil))
    (cl-letf (((symbol-function 'nemacs-runtime-image-preload-batch)
               (lambda (repo-root bootstrap-file)
                 (setq base-args (list repo-root bootstrap-file))
                 t))
              ((symbol-function 'nemacs-runtime-image-preload-vendor-core-extension)
               (lambda ()
                 (setq extension-called t)
                 t)))
      (should (nemacs-runtime-image-preload-vendor-core "/repo" "/bootstrap.el"))
      (should (equal base-args '("/repo" "/bootstrap.el")))
      (should extension-called))))

(ert-deftest nemacs-runtime-image-preload-test/vendor-core-extension-skips-source-require ()
  (let ((required nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature)))
      (should (nemacs-runtime-image-preload-vendor-core-extension))
      (should (equal required nil)))))

(ert-deftest nemacs-runtime-image-preload-test/file-command-wrapper-is-data-lambda ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push (list 'require feature) calls)
                 feature))
              ((symbol-function 'nemacs-runtime-image-preload-test--target)
               (lambda (&rest args)
                 (push (cons 'target args) calls)
                 'ok)))
      (when (fboundp 'nemacs-runtime-image-preload-test--public)
        (fmakunbound 'nemacs-runtime-image-preload-test--public))
      (nemacs-runtime-image-preload--install-file-command
       'nemacs-runtime-image-preload-test--public
       'nemacs-runtime-image-preload-test--target)
      (should (equal (symbol-function
                      'nemacs-runtime-image-preload-test--public)
                     '(lambda (&rest args)
                        (require 'files-standalone-buffer)
                        (apply 'nemacs-runtime-image-preload-test--target
                               args))))
      (should (eq (nemacs-runtime-image-preload-test--public 1 2) 'ok))
      (should (equal (nreverse calls)
                     '((require files-standalone-buffer)
                       (target 1 2)))))))

(ert-deftest nemacs-runtime-image-preload-test/direct-install-creates-daily-core-surface ()
  (nemacs-runtime-image-preload-test--with-clean-direct-install
    (should-not (featurep 'files))
    (should-not (featurep 'simple))
    (should-not (featurep 'dired))
    (should-not (featurep 'help-mode))
    (should-not (featurep 'help-fns))
    (should-not (featurep 'lisp-mode))
    (should-not (featurep 'ielm))
    (should-not (featurep 'isearch))
    (should-not (featurep 'minibuffer))
    (should-not (featurep 'project))
    (should (nemacs-runtime-image-preload--install-files-core))
    (should (nemacs-runtime-image-preload--install-simple-core))
    (should (nemacs-runtime-image-preload--install-dired-core))
    (should (nemacs-runtime-image-preload--install-help-core))
    (should (nemacs-runtime-image-preload--install-elisp-core))
    (should (nemacs-runtime-image-preload--install-ielm-core))
    (should (nemacs-runtime-image-preload--install-isearch-core))
    (should (nemacs-runtime-image-preload--install-minibuffer-core))
    (should (nemacs-runtime-image-preload--install-project-core))
    (should (nemacs-runtime-image-preload--install-support-core))
    (should (nemacs-runtime-image-preload--install-utility-i18n-core))
    (should (featurep 'files))
    (should (featurep 'simple))
    (should (featurep 'dired))
    (should (featurep 'help-mode))
    (should (featurep 'help-fns))
    (should (featurep 'lisp-mode))
    (should (featurep 'ielm))
    (should (featurep 'isearch))
    (should (featurep 'minibuffer))
    (should (featurep 'project))
    (dolist (feature '(subr-x seq map lisp case-table cdl range regi))
      (should (featurep feature)))
    (dolist (feature '(hex-util map-ynp charprop charscript emoji-labels
                                iso-transl cp51932 eucjp-ms fontset
                                idna-mapping ja-dic-utl))
      (should (featurep feature)))
    (dolist (symbol nemacs-runtime-image-preload-test--direct-install-functions)
      (should (fboundp symbol)))
    (dolist (symbol '(dired dired-mode dired-find-file dired-next-line
                            dired-previous-line dired-up-directory))
      (should (fboundp symbol)))
    (dolist (symbol nemacs-runtime-image-preload-test--direct-install-variables)
      (should (boundp symbol)))
    (should (eq (lookup-key ctl-x-map "\C-f") 'find-file))
    (should (eq (lookup-key ctl-x-map "\C-r") 'find-file-read-only))
    (should (eq (lookup-key ctl-x-map "\C-v") 'find-alternate-file))
    (should (eq (lookup-key ctl-x-map "\C-s") 'save-buffer))
    (should (eq (lookup-key ctl-x-map "\C-w") 'write-file))
    (should (eq (lookup-key ctl-x-map "i") 'insert-file))
    (should (eq (lookup-key ctl-x-4-map "f") 'find-file-other-window))
    (should (eq (lookup-key ctl-x-5-map "f") 'find-file-other-frame))
    (should (equal (encode-hex-string "AZ") "415a"))
    (should (equal (decode-hex-string "415a") "AZ"))
    (should (hash-table-p emoji--names))
    (should (hash-table-p emoji--derived))
    (should (keymapp iso-transl-ctl-x-8-map))
    (should (get 'cp51932-decode 'translation-table))
    (should (get 'cp51932-encode 'translation-table))
    (should (get 'eucjp-ms-decode 'translation-table))
    (should (get 'eucjp-ms-encode 'translation-table))
    (should (and (vectorp idna-mapping-table)
                 (> (length idna-mapping-table) #x10ffff)))
    (let ((table (make-char-table 'case-table)))
      (should (char-table-p table))
      (should (= (char-table-range table ?A) ?A))
      (set-char-table-range table '(?A . ?C) ?a)
      (should (= (char-table-range table ?A) ?a))
      (should (= (char-table-range table ?B) ?a))
      (set-case-syntax-pair ?A ?a table)
      (should (= (aref table ?A) ?a))
      (should (= (aref table ?a) ?a))
      (let ((up (case-table-get-table table 'up)))
        (should (= (aref up ?A) ?A))
        (should (= (aref up ?a) ?A)))
      (set-char-table-extra-slot table 1 'canon)
      (let ((copy (copy-case-table table)))
        (should (= (aref copy ?A) ?a))
        (should (char-table-extra-slot copy 0))
        (should-not (char-table-extra-slot copy 1))
        (should-not (char-table-extra-slot copy 2)))
      (set-case-table table)
      (should (eq (current-case-table) table)))
    (with-temp-buffer
      (insert "(foo [bar] \"baz\") tail")
      (goto-char (point-min))
      (forward-sexp)
      (should (= (point) 18))
      (backward-sexp)
      (should (= (point) (point-min))))
    (with-temp-buffer
      (insert "alpha")
      (goto-char (point-min))
      (insert-pair)
      (should (equal (buffer-string) "()alpha"))
      (should (= (point) 2))
      (goto-char (point-min))
      (delete-pair)
      (should (equal (buffer-string) "alpha")))
    (with-temp-buffer
      (insert "(ok) )")
      (should-error (check-parens) :type 'scan-error))
    (with-temp-buffer
      (insert "'foo bar")
      (goto-char (point-min))
      (mark-sexp)
      (should (= (mark t) 5))
      (should mark-active))
    (should (equal (range-compress-list '(1 2 2 3 7 9 10))
                   '((1 . 3) 7 (9 . 10))))
    (should (equal (range-uncompress '((1 . 3) 7 (9 . 10)))
                   '(1 2 3 7 9 10)))
    (should (equal (range-difference '((1 . 6) 10) '((3 . 4) 10))
                   '((1 . 2) (5 . 6))))
    (should (equal (range-intersection '((1 . 5) 8) '((3 . 7)))
                   '(3 . 5)))
    (should (= (range-length '((1 . 3) 5 (7 . 9))) 7))
    (should (equal (regi-mapcar '("^a" "^b")
                                '(push curline
                                       nemacs-runtime-image-preload-test--seen)
                                t nil)
                   '(("^a" (push curline
                                  nemacs-runtime-image-preload-test--seen)
                      t)
                     ("^b" (push curline
                                  nemacs-runtime-image-preload-test--seen)
                      t))))
    (let ((nemacs-runtime-image-preload-test--seen nil))
      (with-temp-buffer
        (insert "foo\nbar\nfood\n")
        (regi-interpret
         '(("^foo" (push curline
                         nemacs-runtime-image-preload-test--seen)))
         (point-min) (point-max))
        (should (equal (nreverse nemacs-runtime-image-preload-test--seen)
                       '("foo" "food")))))
    (should (equal (symbol-function 'find-file)
                   '(lambda (&rest args)
                      (require 'files-standalone-buffer)
                      (apply 'files-standalone-find-file args))))
    (should (equal (symbol-function 'buffer-file-name)
                   '(lambda (&rest args)
                      (require 'files-standalone-buffer)
                      (apply 'files--buffer-file-name args))))
    (should (equal (symbol-function 'set-visited-file-name)
                   '(lambda (&rest args)
                      (require 'files-standalone-buffer)
                      (apply 'files--set-visited-file-name args))))
    (should (equal (symbol-function 'dired)
                   '(lambda (&rest args)
                      (require 'emacs-dired-min)
                      (apply 'dired args))))
    (should (equal (symbol-function 'describe-function)
                   '(lambda (&rest args)
                      (require 'emacs-help)
                      (apply 'describe-function args))))
    (should (equal (symbol-function 'emacs-lisp-mode)
                   '(lambda (&rest args)
                      (require 'lisp-mode)
                      (apply 'emacs-mode-emacs-lisp-mode args))))
    (should (equal (symbol-function 'ielm-send-input)
                   '(lambda (&rest args)
                      (require 'emacs-ielm)
                      (apply 'ielm-input-handler args))))
    (should (equal (symbol-function 'isearch-forward-regexp)
                   '(lambda (&optional no-recursive-edit)
                      (require 'emacs-isearch)
                      (isearch-forward t no-recursive-edit))))
    (should (equal (symbol-function 'project-current)
                   '(lambda (&rest args)
                      (require 'emacs-project)
                      (apply 'project-current args))))))

(ert-deftest nemacs-runtime-image-preload-test/vendor-core-extension-satisfies-all-smoke-candidates ()
  "The runtime-image vendor-core extension must cover every smoke lane directly."
  (nemacs-runtime-image-preload-test--with-clean-direct-install
    (let ((vendor-core-smoke-modules nil)
          (vendor-core-smoke-module-spec nil)
          (vendor-core-smoke-default-limit 0)
          (vendor-core-smoke-strict t))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional _filename _noerror)
                   (unless (featurep feature)
                     (error "runtime preload did not provide %S" feature))
                   feature)))
        (should (nemacs-runtime-image-preload-vendor-core-extension))
        (should (equal (mapcar #'car (vendor-core-smoke-batch))
                       (mapcar #'car vendor-core-smoke-candidates)))))))

(ert-deftest nemacs-runtime-image-preload-test/vendor-core-extension-supports-real-file-edit-flow ()
  "Exercise direct preload against an actual read/edit/write path."
  (let* ((source-functions '(files--buffer-file-name
                             files--buffer-key
                             files--buffer-file-cell
                             files--buffer-state-cell
                             files--buffer-live-or-unknown-p
                             files--live-buffer-cells
                             files--prune-dead-buffer-state
                             files--set-buffer-state-cell
                             files--buffer-string-value
                             files--set-buffer-string-value
                             files--buffer-point-value
                             files--set-buffer-point-value
                             files--buffer-modified-value
                             files--set-buffer-modified-value
                             files--set-buffer-file-name
                             files--set-visited-file-name
                             files--file-name-equal-p
                             files--visited-buffer-for-file
                             files--file-buffer-name
                             files--create-buffer-for-file
                             files--expand-file-name
                             files--buffer-for-file
                             files--read-file-text
                             files--region-text
                             files--write-file-text
                             files--concat-strings
                             files--current-buffer-if-available
                             files--set-buffer-if-available
                             files--file-readable-or-unknown-p
                             files--insert-file-if-readable
                             files--load-file-into-buffer
                             files-standalone-find-file
                             files-standalone-find-file-noselect
                             files-standalone-find-file-read-only
                             files-standalone-find-alternate-file
                             files-standalone-save-buffer
                             files--save-current-buffer-if-needed
                             files--buffer-modified-for-save-p
                             files--save-buffer-entry-if-needed
                             files--save-buffer-entries-if-needed
                             files-standalone-write-file
                             files-standalone-save-some-buffers
                             files-standalone-insert-file
                             files-standalone-list-directory))
         (source-variables '(files--buffer-string
                             files--buffer-strings
                             files--point
                             files--buffer-points
                             files--buffer-modified-p
                             files--buffer-modified-flags
                             files--current-file-name
                             files--buffer-file-names
                             files--standalone-runtime-p
                             files--native-write-region
                             files--native-insert-file-contents
                             files--native-buffer-string))
         (function-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (fboundp symbol)
                               (symbol-function symbol))))
                  source-functions))
         (value-cells
          (mapcar (lambda (symbol)
                    (cons symbol
                          (and (boundp symbol)
                               (list (symbol-value symbol)))))
                  source-variables))
         (original-features features)
         (input-file (make-temp-file "nemacs-runtime-edit-in-" nil ".el"))
         (second-file (make-temp-file "nemacs-runtime-edit-second-" nil ".el"))
         (output-file (make-temp-file "nemacs-runtime-edit-out-" nil ".el"))
         visited-buffer
         second-buffer)
    (unwind-protect
        (progn
          (with-temp-file input-file
            (insert "(alpha beta)"))
          (with-temp-file second-file
            (insert "(gamma)"))
          (delete-file output-file)
          (nemacs-runtime-image-preload-test--with-clean-direct-install
            (setq features (remove 'files-standalone-buffer features))
            (should (nemacs-runtime-image-preload-vendor-core-extension))
            (setq visited-buffer (find-file input-file))
            (should (bufferp visited-buffer))
            (should (eq (current-buffer) visited-buffer))
            (should (equal (buffer-string) "(alpha beta)"))
            (goto-char (point-min))
            (forward-sexp)
            (should (= (point) (point-max)))
            (goto-char (point-min))
            (open-line 1)
            (insert-pair)
            (should (equal (buffer-string) "()\n(alpha beta)"))
            (should (equal (save-buffer) input-file))
            (with-temp-buffer
              (insert-file-contents input-file)
              (should (equal (buffer-string) "()\n(alpha beta)")))
            (should (equal (write-file output-file) output-file))
            (with-temp-buffer
              (insert-file-contents output-file)
              (should (equal (buffer-string) "()\n(alpha beta)")))
            (setq second-buffer (find-file second-file))
            (should (eq (current-buffer) second-buffer))
            (goto-char (point-max))
            (insert " delta")
            (should (save-some-buffers))
            (should (eq (current-buffer) second-buffer))
            (with-temp-buffer
              (insert-file-contents second-file)
              (should (equal (buffer-string) "(gamma) delta")))))
      (when (and visited-buffer (buffer-live-p visited-buffer))
        (kill-buffer visited-buffer))
      (when (and second-buffer (buffer-live-p second-buffer))
        (kill-buffer second-buffer))
      (dolist (file (list input-file second-file output-file))
        (when (file-exists-p file)
          (delete-file file)))
      (setq features original-features)
      (dolist (cell function-cells)
        (if (cdr cell)
            (let ((native-comp-enable-subr-trampolines nil))
              (fset (car cell) (cdr cell)))
          (fmakunbound (car cell))))
      (dolist (cell value-cells)
        (if (cdr cell)
            (set (car cell) (cadr cell))
          (makunbound (car cell)))))))

(ert-deftest nemacs-runtime-image-preload-test/vendor-core-extension-supports-daily-edit-flow ()
  "Exercise a small edit/save/help/project flow after direct preload."
  (let ((cleanup-targets '(files-standalone-find-file
                           files-standalone-save-buffer)))
    (unwind-protect
        (nemacs-runtime-image-preload-test--with-clean-direct-install
          (let (calls saved-text)
            (cl-letf (((symbol-function 'require)
                       (lambda (feature &optional _filename _noerror)
                         (push (list 'require feature) calls)
                         (pcase feature
                           ('files-standalone-buffer
                            (fset 'files-standalone-find-file
                                  (lambda (filename)
                                    (push (list 'find-file filename) calls)
                                    'visited-buffer))
                            (fset 'files-standalone-save-buffer
                                  (lambda ()
                                    (setq saved-text (buffer-string))
                                    (push '(save-buffer) calls)
                                    'saved-buffer))
                            (provide feature))
                           ('emacs-help
                            (fset 'describe-function
                                  (lambda (symbol &optional _buffer-name)
                                    (push (list 'describe-function symbol)
                                          calls)
                                    (list 'help symbol)))
                            (provide feature))
                           ('emacs-project
                            (fset 'project-current
                                  (lambda (&rest _args)
                                    (push '(project-current) calls)
                                    '(project-vc "/tmp/runtime-project/")))
                            (provide feature))
                           (_
                            (provide feature)))
                         feature)))
              (should (nemacs-runtime-image-preload-vendor-core-extension))
              (with-temp-buffer
                (insert "(alpha beta)")
                (goto-char (point-min))
                (should (eq (find-file "note.el") 'visited-buffer))
                (forward-sexp)
                (should (= (point) (point-max)))
                (goto-char (point-min))
                (open-line 1)
                (insert-pair)
                (should (equal (buffer-string) "()\n(alpha beta)"))
                (should (eq (save-buffer) 'saved-buffer))
                (should (equal saved-text "()\n(alpha beta)")))
              (should (equal (describe-function 'find-file)
                             '(help find-file)))
              (should (equal (project-current)
                             '(project-vc "/tmp/runtime-project/")))
              (dolist (feature '(files-standalone-buffer
                                  emacs-help
                                  emacs-project))
                (should (member (list 'require feature) calls)))
              (should (member '(find-file "note.el") calls))
              (should (member '(save-buffer) calls))
              (should (member '(describe-function find-file) calls))
              (should (member '(project-current) calls)))))
      (dolist (symbol cleanup-targets)
        (when (fboundp symbol)
          (fmakunbound symbol)))
      (dolist (feature '(files-standalone-buffer emacs-help emacs-project))
        (setq features (remove feature features))))))

(ert-deftest nemacs-runtime-image-preload-test/preload-vendor-core-base-before-extension ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'nemacs-runtime-image-preload-batch)
               (lambda (&rest _)
                 (push 'base calls)
                 t))
              ((symbol-function 'nemacs-runtime-image-preload-vendor-core-extension)
               (lambda ()
                 (push 'extension calls)
                 t)))
      (should (nemacs-runtime-image-preload-vendor-core "/repo" "/bootstrap.el"))
      (should (equal (nreverse calls) '(base extension))))))

(ert-deftest nemacs-runtime-image-preload-test/legacy-vendor-core-installs-daily-core-directly ()
  "Keep the vendor-core lane off slow source `require' calls."
  (let ((base-args nil)
        (required nil))
    (cl-letf (((symbol-function 'nemacs-runtime-image-preload-batch)
               (lambda (repo-root bootstrap-file)
                 (setq base-args (list repo-root bootstrap-file))
                 t))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 feature)))
      (should (nemacs-runtime-image-preload-vendor-core "/repo" "/bootstrap.el"))
      (should (equal base-args '("/repo" "/bootstrap.el")))
      (should (equal required nil)))))

(ert-deftest nemacs-runtime-image-preload-test/makefile-vendor-core-extends-existing-base-image ()
  "The vendor-core image target should not force a fresh base image bake."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "Makefile" nemacs-runtime-image-preload-test--repo-root))
    (should (search-forward "bake-vendor-core-runtime-image:" nil t))
    (goto-char (point-min))
    (should (search-forward
             "test -r \"$(NEMACS_RUNTIME_IMAGE)\" || $(MAKE) \"$(NEMACS_RUNTIME_IMAGE)\""
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "\"$(NELISP_BIN)\" extend-runtime-image \"$(abspath $(NEMACS_RUNTIME_IMAGE))\""
             nil t))
    (goto-char (point-min))
    (should-not (search-forward
                 "bake-vendor-core-runtime-image: bake-runtime-image"
                 nil t))))

(ert-deftest nemacs-runtime-image-preload-test/makefile-standalone-gate-stays-on-reader ()
  "The pure standalone gate should include runtime-image through the reader."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "Makefile" nemacs-runtime-image-preload-test--repo-root))
    (should (search-forward
             "verify-nelisp-standalone: doctor test-nelisp test-nelisp-runtime-image verify-vendor-class-a verify-vendor-core"
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "NEMACS_RUNTIME_IMAGE="
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "tmp=$$(mktemp \"$${TMPDIR:-/tmp}/nemacs-vendor-core.XXXXXX.el\")"
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "cat \"$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))\""
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "cat \"$(abspath scripts/vendor-core-smoke.el)\""
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "VENDOR-CORE-STANDALONE=ok exit=42"
             nil t))
    (goto-char (point-min))
    (should (search-forward
             "\"$(NELISP_BIN)\" \"$$tmp\""
             nil t))
    (goto-char (point-min))
    (should-not (search-forward
                 "standalone-reader does not provide dump-runtime-image yet"
                 nil t))
    (goto-char (point-min))
    (should-not (search-forward
                 "verify-nelisp-standalone: verify-vendor-core"
                 nil t))))

(ert-deftest nemacs-runtime-image-preload-test/makefile-diagnostics-prefer-standalone-reader ()
  "Bootstrap profiling and vendor form walk should prefer standalone-reader."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "Makefile" nemacs-runtime-image-preload-test--repo-root))
    (should (search-forward "profile-nelisp-bootstrap: build-nelisp-bootstrap" nil t))
    (goto-char (point-min))
    (should (search-forward "-l standalone-bootstrap-profile" nil t))
    (goto-char (point-min))
    (should (search-forward "-f standalone-bootstrap-profile-batch" nil t))
    (goto-char (point-min))
    (should (search-forward "diagnose-vendor-form-walk: build-nelisp-bootstrap" nil t))
    (goto-char (point-min))
    (should (search-forward "-l vendor-form-standalone-walk" nil t))
    (goto-char (point-min))
    (should (search-forward "-f vendor-form-standalone-batch" nil t))
    (goto-char (point-min))
    (should (search-forward "diagnose-vendor-load-replay: build-nelisp-bootstrap" nil t))
    (goto-char (point-min))
    (should (search-forward "vendor-load-standalone-prelude" nil t))
    (goto-char (point-min))
    (should (search-forward "-l vendor-load-standalone-replay" nil t))
    (goto-char (point-min))
    (should (search-forward "-f vendor-load-standalone-batch" nil t))
    (goto-char (point-min))
    (should-not (search-forward
                 "standalone-reader does not provide eval FORM yet"
                 nil t))))

(provide 'nemacs-runtime-image-preload-test)

;;; nemacs-runtime-image-preload-test.el ends here
