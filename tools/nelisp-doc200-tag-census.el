;;; nelisp-doc200-tag-census.el --- enumerate Doc 200 tag sites -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 200 option A adds two Sexp tags.  Before that can be safe, every place
;; which tests or writes the existing Str (5) and MutStr (6) tags needs a
;; stable, countable audit key.  This tool reads tracked Emacs Lisp files as
;; forms and walks their structure, including quoted standalone DSL bodies.
;; It deliberately does not key sites by line number.
;;
;; The repository used to census these sites with grep.  That confused Sexp
;; tags with unrelated numeric comparisons and with the AOT grammar-op tag
;; namespace.  Unresolved `(= SYMBOL 5)' and `(= SYMBOL 6)' forms therefore
;; remain visible under `unresolved-numeric' instead of being guessed at.
;;
;; Generated source assembled from string literals is not structurally
;; reachable from the outer file's `read'.  The Elisp generators maintained by
;; `nelisp-generated-source-parse.el', plus the native artifact C harness which
;; contains its own Sexp tag dispatch, are represented by one explicit
;; `unparsed-region' site each.
;;
;; Run: make doc200-census

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-doc200-tag-census--ledger
  "tools/nelisp-doc200-tag-sites.txt"
  "Repository-relative path to the checked-in Doc 200 ledger.")

(defconst nelisp-doc200-tag-census--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                            (or load-file-name buffer-file-name))))
  "Absolute root of the checkout containing this tool.")

(defconst nelisp-doc200-tag-census--generated-source-enclosings
  '(nelisp-artifact--native-driver-c
    nelisp-standalone--artifact-command-runtime-src
    nelisp-standalone--artifact-command-src
    nelisp-standalone--artifact-command-cache-src
    nelisp-standalone--artifact-source-command-cache-src
    nelisp-standalone--reader-repl-prelude-source)
  "Generated-source regions invisible to the outer file's reader.
The five `nelisp-standalone--' names must stay in step with
`nelisp-generated-source-parse--generators'.  The native-driver entry is C
source rather than Elisp, so that parser intentionally does not cover it.")

(defconst nelisp-doc200-tag-census--tag-read-kinds
  '((sexp-tag . tag-test-sexp-tag)
    (ptr-read-u8 . tag-test-read-u8)
    (ptr-read-u64 . tag-test-read-u64)
    (nelisp_ptr_read_u8 . tag-test-runtime-read-u8))
  "Tag-read functions and the KIND used for a direct numeric test.
`nelisp_ptr_read_u8' is the additional standalone-DSL shape found by this
census beyond the minimum shapes listed in Doc 200's audit brief.")

(cl-defstruct (nelisp-doc200-tag-census--site
               (:constructor nelisp-doc200-tag-census--make-site))
  file enclosing kind nth)

(cl-defstruct (nelisp-doc200-tag-census--scan
               (:constructor nelisp-doc200-tag-census--make-scan))
  files sites errors)

(defvar nelisp-doc200-tag-census--current-file nil)
(defvar nelisp-doc200-tag-census--sites nil)
(defvar nelisp-doc200-tag-census--site-counts nil)

(defun nelisp-doc200-tag-census--repo-root ()
  "Return this checkout's root directory."
  nelisp-doc200-tag-census--root)

(defun nelisp-doc200-tag-census--u32 (bytes offset)
  "Read a big-endian unsigned 32-bit integer from BYTES at OFFSET."
  (+ (ash (aref bytes offset) 24)
     (ash (aref bytes (+ offset 1)) 16)
     (ash (aref bytes (+ offset 2)) 8)
     (aref bytes (+ offset 3))))

(defun nelisp-doc200-tag-census--u16 (bytes offset)
  "Read a big-endian unsigned 16-bit integer from BYTES at OFFSET."
  (+ (ash (aref bytes offset) 8)
     (aref bytes (+ offset 1))))

(defun nelisp-doc200-tag-census--index-path ()
  "Return the Git index path without invoking git."
  (let* ((root (nelisp-doc200-tag-census--repo-root))
         (dotgit (expand-file-name ".git" root)))
    (cond
     ((file-directory-p dotgit) (expand-file-name "index" dotgit))
     ((file-regular-p dotgit)
      (with-temp-buffer
        (insert-file-contents dotgit)
        (goto-char (point-min))
        (unless (looking-at "gitdir: \\(.*\\)$")
          (error "Malformed .git indirection file: %s" dotgit))
        (expand-file-name "index"
                          (expand-file-name (match-string 1)
                                            (file-name-directory dotgit)))))
     (t (error "No .git directory or indirection file at %s" dotgit)))))

(defun nelisp-doc200-tag-census--tracked-files ()
  "Return repository-relative paths from the Git index.
This is a small reader for index versions 2 and 3.  It exists because the
census contract is over tracked files while the audit environment does not
provide, and must not invoke, the git executable."
  (let ((index (nelisp-doc200-tag-census--index-path)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally index)
      (let* ((bytes (buffer-string))
             (size (length bytes)))
        (unless (and (>= size 12) (string= (substring bytes 0 4) "DIRC"))
          (error "Not a Git index: %s" index))
        (let ((version (nelisp-doc200-tag-census--u32 bytes 4))
              (entries (nelisp-doc200-tag-census--u32 bytes 8))
              (offset 12)
              (paths nil))
          (unless (memq version '(2 3))
            (error "Unsupported Git index version %d in %s" version index))
          (dotimes (_ entries)
            (let ((start offset))
              (when (> (+ start 62) size)
                (error "Truncated Git index entry in %s" index))
              (let* ((flags (nelisp-doc200-tag-census--u16 bytes (+ start 60)))
                     (extended (not (zerop (logand flags #x4000))))
                     (path-start (+ start 62 (if extended 2 0)))
                     (path-end (cl-position 0 bytes :start path-start)))
                (unless path-end
                  (error "Unterminated Git index path in %s" index))
                (push (decode-coding-string
                       (substring bytes path-start path-end) 'utf-8)
                      paths)
                (let ((entry-size (1+ (- path-end start))))
                  (setq offset (+ start (* 8 (/ (+ entry-size 7) 8))))))))
          (sort (delete-dups paths) #'string<))))))

(defun nelisp-doc200-tag-census--included-file-p (path)
  "Return non-nil when tracked PATH is in the Doc 200 ledger scan set."
  (and (string-suffix-p ".el" path)
       (or (string-match-p "\\`\\(?:scripts\\|lisp\\|src\\|tools\\)/" path)
           (string-match-p "\\`packages/[^/]+/src/" path))))

(defun nelisp-doc200-tag-census--test-file-p (path)
  "Return non-nil when tracked PATH is in the explicitly excluded test set."
  (and (string-suffix-p ".el" path)
       (or (string-prefix-p "test/" path)
           (string-match-p "\\`packages/[^/]+/test/" path))))

(defun nelisp-doc200-tag-census--target-number-p (object)
  "Return non-nil when OBJECT is the numeric Str or MutStr tag."
  (and (integerp object) (or (= object 5) (= object 6))))

(defun nelisp-doc200-tag-census--zero-p (object)
  "Return non-nil when OBJECT is the literal zero offset."
  (and (integerp object) (= object 0)))

(defun nelisp-doc200-tag-census--proper-length-p (form length)
  "Return non-nil when FORM is a proper list of LENGTH elements."
  (let ((actual (proper-list-p form)))
    (and actual (= actual length))))

(defun nelisp-doc200-tag-census--tag-read (form)
  "Return FORM's direct tag-read KIND, or nil.
The address expression is deliberately opaque.  Only the reader function and
literal zero offset determine whether this is a tag read."
  (when (consp form)
    (let ((entry (assq (car form) nelisp-doc200-tag-census--tag-read-kinds)))
      (when entry
        (pcase (car form)
          ('sexp-tag
           (and (nelisp-doc200-tag-census--proper-length-p form 2)
                (cdr entry)))
          (_
           (and (nelisp-doc200-tag-census--proper-length-p form 3)
                (nelisp-doc200-tag-census--zero-p (nth 2 form))
                (cdr entry))))))))

(defun nelisp-doc200-tag-census--target-const-p (symbol)
  "Return non-nil when SYMBOL names one of the two existing string tags."
  ;; Construct the names so this census does not become its own const-ref site
  ;; once the coordinator adds this new file to the Git index.
  (and (symbolp symbol)
       (member (symbol-name symbol)
               (list (concat "nelisp-sexp--tag-" "str")
                     (concat "nelisp-sexp--tag-" "mut-str")))))

(defun nelisp-doc200-tag-census--site-key (site)
  "Return the stable ledger key for SITE."
  (format "%s\t%s\t%s\t%d"
          (nelisp-doc200-tag-census--site-file site)
          (nelisp-doc200-tag-census--site-enclosing site)
          (nelisp-doc200-tag-census--site-kind site)
          (nelisp-doc200-tag-census--site-nth site)))

(defun nelisp-doc200-tag-census--add-site (enclosing kind)
  "Record a KIND site in the current file under ENCLOSING."
  (let* ((name (if (and enclosing (symbolp enclosing))
                   (symbol-name enclosing)
                 "-"))
         (prefix (format "%s\t%s\t%s"
                         nelisp-doc200-tag-census--current-file name kind))
         (nth (gethash prefix nelisp-doc200-tag-census--site-counts 0)))
    (puthash prefix (1+ nth) nelisp-doc200-tag-census--site-counts)
    (push (nelisp-doc200-tag-census--make-site
           :file nelisp-doc200-tag-census--current-file
           :enclosing name :kind kind :nth nth)
          nelisp-doc200-tag-census--sites)))

(defun nelisp-doc200-tag-census--env-read-kind (symbol env)
  "Return SYMBOL's tag-read kind in ENV, respecting shadowing."
  (let ((binding (and (symbolp symbol) (assq symbol env))))
    (and binding (cdr binding))))

(defun nelisp-doc200-tag-census--walk-equality (form enclosing env)
  "Classify numeric tag equality FORM under ENCLOSING and ENV."
  (when (and (nelisp-doc200-tag-census--proper-length-p form 3)
             (eq (car form) '=))
    (let ((left (nth 1 form))
          (right (nth 2 form)))
      (when (or (nelisp-doc200-tag-census--target-number-p left)
                (nelisp-doc200-tag-census--target-number-p right))
        (let* ((probe (if (nelisp-doc200-tag-census--target-number-p right)
                          left right))
               (direct-kind (nelisp-doc200-tag-census--tag-read probe))
               (bound-kind (and (symbolp probe)
                                (nelisp-doc200-tag-census--env-read-kind
                                 probe env))))
          (cond
           (direct-kind
            (nelisp-doc200-tag-census--add-site enclosing direct-kind))
           (bound-kind
            (nelisp-doc200-tag-census--add-site enclosing
                                                  'tag-test-bound-sym))
           (t
            (nelisp-doc200-tag-census--add-site enclosing
                                                  'unresolved-numeric))))))))

(defun nelisp-doc200-tag-census--walk-write (form enclosing)
  "Classify a literal tag write FORM under ENCLOSING."
  (when (and (nelisp-doc200-tag-census--proper-length-p form 4)
             (memq (car form) '(ptr-write-u8 ptr-write-u64))
             (nelisp-doc200-tag-census--zero-p (nth 2 form))
             (nelisp-doc200-tag-census--target-number-p (nth 3 form)))
    (nelisp-doc200-tag-census--add-site
     enclosing (if (eq (car form) 'ptr-write-u8)
                   'tag-write-u8
                 'tag-write-u64))))

(defun nelisp-doc200-tag-census--binding (binding)
  "Return (SYMBOL . INIT) for a let BINDING, or nil."
  (cond
   ((symbolp binding) (cons binding nil))
   ((and (consp binding) (symbolp (car binding)))
    (cons (car binding) (cadr binding)))
   (t nil)))

(defun nelisp-doc200-tag-census--walk-let (form enclosing env sequential)
  "Walk let FORM under ENCLOSING and ENV.
When SEQUENTIAL is non-nil, apply `let*' binding scope."
  (let ((bindings (nth 1 form))
        (body (cddr form))
        (body-env env))
    (if (not (proper-list-p bindings))
        (nelisp-doc200-tag-census--walk-list (cdr form) enclosing env)
      (if sequential
          (dolist (raw bindings)
            (let ((binding (nelisp-doc200-tag-census--binding raw)))
              (if (not binding)
                  (nelisp-doc200-tag-census--walk raw enclosing body-env)
                (nelisp-doc200-tag-census--walk (cdr binding)
                                                 enclosing body-env)
                (push (cons (car binding)
                            (nelisp-doc200-tag-census--tag-read (cdr binding)))
                      body-env))))
        (let ((new-bindings nil))
          (dolist (raw bindings)
            (let ((binding (nelisp-doc200-tag-census--binding raw)))
              (if (not binding)
                  (nelisp-doc200-tag-census--walk raw enclosing env)
                (nelisp-doc200-tag-census--walk (cdr binding) enclosing env)
                (push (cons (car binding)
                            (nelisp-doc200-tag-census--tag-read (cdr binding)))
                      new-bindings))))
          (setq body-env (append new-bindings env))))
      (nelisp-doc200-tag-census--walk-list body enclosing body-env))))

(defun nelisp-doc200-tag-census--walk-list (forms enclosing env)
  "Walk each element of proper or dotted FORMS under ENCLOSING and ENV."
  (while (consp forms)
    (nelisp-doc200-tag-census--walk (car forms) enclosing env)
    (setq forms (cdr forms)))
  (unless (null forms)
    (nelisp-doc200-tag-census--walk forms enclosing env)))

(defun nelisp-doc200-tag-census--walk (form enclosing env)
  "Walk FORM structurally under ENCLOSING and lexical tag ENV."
  (cond
   ((symbolp form)
    (when (nelisp-doc200-tag-census--target-const-p form)
      (nelisp-doc200-tag-census--add-site enclosing 'tag-const-ref)))
   ((vectorp form)
    (mapc (lambda (item)
            (nelisp-doc200-tag-census--walk item enclosing env))
          form))
   ((consp form)
    (cond
      ((and (memq (car form) '(defun defconst defvar))
            (let ((size (proper-list-p form))) (and size (>= size 3)))
            (symbolp (nth 1 form)))
       (let ((name (and (symbolp (nth 1 form)) (nth 1 form))))
         (when (and (eq (car form) 'defun)
                    (memq name
                          nelisp-doc200-tag-census--generated-source-enclosings))
           (nelisp-doc200-tag-census--add-site name 'unparsed-region))
         ;; A defun introduces a fresh lexical scope.  Skipping the binding
         ;; name also prevents a tag constant declaration from counting as a
         ;; reference to itself.
         (nelisp-doc200-tag-census--walk-list
          (if (eq (car form) 'defun) (cdddr form) (cddr form))
          (or name enclosing) nil)))
      ((and (eq (car form) 'let)
            (let ((size (proper-list-p form))) (and size (>= size 2))))
       (nelisp-doc200-tag-census--walk-let form enclosing env nil))
      ((and (eq (car form) 'let*)
            (let ((size (proper-list-p form))) (and size (>= size 2))))
       (nelisp-doc200-tag-census--walk-let form enclosing env t))
      (t
       (nelisp-doc200-tag-census--walk-equality form enclosing env)
       (nelisp-doc200-tag-census--walk-write form enclosing)
       (nelisp-doc200-tag-census--walk-list form enclosing env))))))

(defun nelisp-doc200-tag-census--scan-file (root path)
  "Read and structurally walk tracked PATH relative to ROOT.
Return nil on success or an error description."
  (let ((nelisp-doc200-tag-census--current-file path))
    (condition-case-unless-debug err
        (with-temp-buffer
          (insert-file-contents (expand-file-name path root))
          (emacs-lisp-mode)
          (goto-char (point-min))
          (let ((done nil))
            (while (not done)
              (forward-comment (point-max))
              (if (eobp)
                  (setq done t)
                (let ((start (point))
                      form)
                  (condition-case-unless-debug read-err
                      (setq form (read (current-buffer)))
                    (error
                     (error "%s:%d: read failed at byte %d: %s"
                            path (line-number-at-pos start) start
                            (error-message-string read-err))))
                  (nelisp-doc200-tag-census--walk form nil nil)))))
          nil)
      (error (format "%s: %s" path (error-message-string err))))))

(defun nelisp-doc200-tag-census--site-less-p (a b)
  "Return non-nil when site A sorts before site B by stable key."
  (let ((a-prefix (format "%s\t%s\t%s"
                          (nelisp-doc200-tag-census--site-file a)
                          (nelisp-doc200-tag-census--site-enclosing a)
                          (nelisp-doc200-tag-census--site-kind a)))
        (b-prefix (format "%s\t%s\t%s"
                          (nelisp-doc200-tag-census--site-file b)
                          (nelisp-doc200-tag-census--site-enclosing b)
                          (nelisp-doc200-tag-census--site-kind b))))
    (if (string= a-prefix b-prefix)
        (< (nelisp-doc200-tag-census--site-nth a)
           (nelisp-doc200-tag-census--site-nth b))
      (string< a-prefix b-prefix))))

(defun nelisp-doc200-tag-census--scan-paths (files)
  "Return a scan result for repository-relative FILES."
  (let ((root (nelisp-doc200-tag-census--repo-root))
        (nelisp-doc200-tag-census--sites nil)
        (nelisp-doc200-tag-census--site-counts (make-hash-table :test #'equal))
        (errors nil))
    (dolist (file files)
      (let ((err (nelisp-doc200-tag-census--scan-file root file)))
        (when err (push err errors))))
    (nelisp-doc200-tag-census--make-scan
     :files files
     :sites (sort (nreverse nelisp-doc200-tag-census--sites)
                  #'nelisp-doc200-tag-census--site-less-p)
     :errors (nreverse errors))))

(defun nelisp-doc200-tag-census--ledger-rows ()
  "Read and validate the checked-in ledger.
Return a list of (KEY STATUS NOTE)."
  (let ((path (expand-file-name nelisp-doc200-tag-census--ledger
                                (nelisp-doc200-tag-census--repo-root)))
        (rows nil)
        (seen (make-hash-table :test #'equal)))
    (unless (file-readable-p path)
      (error "Doc 200 ledger is missing: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((line-number 0))
        (while (not (eobp))
          (setq line-number (1+ line-number))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (let ((fields (split-string line "\t" nil)))
                (unless (= (length fields) 6)
                  (error "%s:%d: expected six tab-separated fields"
                         nelisp-doc200-tag-census--ledger line-number))
                (let ((key (mapconcat #'identity (cl-subseq fields 0 4) "\t"))
                      (status (nth 4 fields))
                      (note (nth 5 fields)))
                  (unless (member status '("pending" "walked" "n-a"))
                    (error "%s:%d: invalid status %S"
                           nelisp-doc200-tag-census--ledger line-number status))
                  (when (gethash key seen)
                    (error "%s:%d: duplicate key %s"
                           nelisp-doc200-tag-census--ledger line-number key))
                  (puthash key t seen)
                  (push (list key status note) rows)))))
          (forward-line 1))))
    (nreverse rows)))

(defun nelisp-doc200-tag-census--ledger-rows-or-exit (checked live-sites)
  "Read ledger rows or report a counted failure and exit.
CHECKED is the real number of files scanned and LIVE-SITES is the number of
structurally enumerated rows.  The explicit final count keeps malformed-ledger
failures inside the gate-report contract instead of merely dying during load."
  (condition-case err
      (nelisp-doc200-tag-census--ledger-rows)
    (error
     (princ (format "doc200-census: LEDGER-ERROR %s\n"
                    (error-message-string err)))
     (princ (format "GATE-COUNT checked=%d findings=%d\n"
                    checked live-sites))
     (kill-emacs 1))))

(defun nelisp-doc200-tag-census--hash-keys (keys)
  "Return an equality hash table containing KEYS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (key keys table) (puthash key t table))))

(defun nelisp-doc200-tag-census--difference (left right-table)
  "Return elements of LEFT absent from RIGHT-TABLE."
  (cl-remove-if (lambda (item) (gethash item right-table)) left))

(defun nelisp-doc200-tag-census--count-kind (sites kind)
  "Count SITES whose kind is KIND."
  (cl-count kind sites :key #'nelisp-doc200-tag-census--site-kind))

(defun nelisp-doc200-tag-census--print-summary (included excluded)
  "Print stable diagnostic counts for INCLUDED and EXCLUDED scans."
  (let ((sites (nelisp-doc200-tag-census--scan-sites included))
        (test-sites (nelisp-doc200-tag-census--scan-sites excluded))
        (kind-counts (make-hash-table :test #'eq)))
    (dolist (site sites)
      (let ((kind (nelisp-doc200-tag-census--site-kind site)))
        (puthash kind (1+ (gethash kind kind-counts 0)) kind-counts)))
    (let (kinds)
      (maphash (lambda (kind _count) (push kind kinds)) kind-counts)
      (dolist (kind (sort kinds
                          (lambda (a b)
                            (string< (symbol-name a) (symbol-name b)))))
        (princ (format "DOC200-KIND %s=%d\n" kind
                       (gethash kind kind-counts)))))
    (princ (format "DOC200-TEST-EXCLUDED checked=%d candidates=%d unresolved-numeric=%d\n"
                   (length (nelisp-doc200-tag-census--scan-files excluded))
                   (length test-sites)
                   (nelisp-doc200-tag-census--count-kind
                    test-sites 'unresolved-numeric)))
    (princ (format "DOC200-CENSUS included-files=%d sites=%d unresolved-numeric=%d unparsed-region=%d\n"
                   (length (nelisp-doc200-tag-census--scan-files included))
                   (length sites)
                   (nelisp-doc200-tag-census--count-kind
                    sites 'unresolved-numeric)
                   (nelisp-doc200-tag-census--count-kind
                    sites 'unparsed-region)))))

(defun nelisp-doc200-tag-census--write-ledger (sites)
  "Create the initial pending ledger from SITES, refusing overwrite."
  (let ((path (expand-file-name nelisp-doc200-tag-census--ledger
                                (nelisp-doc200-tag-census--repo-root))))
    (when (file-exists-p path)
      (error "Refusing to overwrite existing human statuses: %s" path))
    (with-temp-file path
      (insert "# FILE\tENCLOSING\tKIND\tNTH\tSTATUS\tNOTE\n")
      (dolist (site sites)
        (insert (nelisp-doc200-tag-census--site-key site) "\tpending\t-\n")))))

;;;###autoload
(defun nelisp-doc200-tag-census-write-ledger ()
  "Create the initial checked-in Doc 200 ledger with pending rows."
  (interactive)
  (let* ((tracked (nelisp-doc200-tag-census--tracked-files))
         (files (cl-remove-if-not
                 #'nelisp-doc200-tag-census--included-file-p tracked))
         (scan (nelisp-doc200-tag-census--scan-paths files)))
    (when (nelisp-doc200-tag-census--scan-errors scan)
      (error "Cannot write ledger; read errors: %s"
             (string-join (nelisp-doc200-tag-census--scan-errors scan) "; ")))
    (nelisp-doc200-tag-census--write-ledger
     (nelisp-doc200-tag-census--scan-sites scan))
    (princ (format "Wrote %s with %d pending rows\n"
                   nelisp-doc200-tag-census--ledger
                   (length (nelisp-doc200-tag-census--scan-sites scan))))))

;;;###autoload
(defun nelisp-doc200-tag-census-run ()
  "Regenerate Doc 200 census keys and compare them with the ledger."
  (interactive)
  (let* ((tracked (nelisp-doc200-tag-census--tracked-files))
         (included-files (cl-remove-if-not
                          #'nelisp-doc200-tag-census--included-file-p tracked))
         (test-files (cl-remove-if-not
                      #'nelisp-doc200-tag-census--test-file-p tracked))
         (included (nelisp-doc200-tag-census--scan-paths included-files))
         (excluded (nelisp-doc200-tag-census--scan-paths test-files))
         (errors (append (nelisp-doc200-tag-census--scan-errors included)
                         (nelisp-doc200-tag-census--scan-errors excluded)))
         (current-keys
          (mapcar #'nelisp-doc200-tag-census--site-key
                  (nelisp-doc200-tag-census--scan-sites included)))
         (checked (+ (length included-files) (length test-files)))
         (ledger-rows
          (nelisp-doc200-tag-census--ledger-rows-or-exit
           checked (length current-keys)))
         (ledger-keys (mapcar #'car ledger-rows))
         (current-table (nelisp-doc200-tag-census--hash-keys current-keys))
         (ledger-table (nelisp-doc200-tag-census--hash-keys ledger-keys))
         (missing (nelisp-doc200-tag-census--difference
                   current-keys ledger-table))
         (stale (nelisp-doc200-tag-census--difference
                 ledger-keys current-table))
         (failed nil))
    (nelisp-doc200-tag-census--print-summary included excluded)
    (dolist (err errors)
      (setq failed t)
      (princ (format "doc200-census: READ-ERROR %s\n" err)))
    (dolist (key missing)
      (setq failed t)
      (princ (format "doc200-census: MISSING-FROM-LEDGER %s\n" key)))
    (dolist (key stale)
      (setq failed t)
      (princ (format "doc200-census: STALE-LEDGER-ROW %s\n" key)))
    (unless failed
      (princ "doc200-census: PASS (live keys match ledger; STATUS/NOTE preserved)\n"))
    ;; This must remain the final line: `nelisp-ai.sh gate' consumes it.
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   checked
                   (length ledger-rows)))
    (when failed (kill-emacs 1))))

(provide 'nelisp-doc200-tag-census)

;;; nelisp-doc200-tag-census.el ends here
