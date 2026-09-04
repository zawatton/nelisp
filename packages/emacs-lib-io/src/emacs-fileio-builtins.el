;;; emacs-fileio-builtins.el --- File I/O bridges + find-file / save-buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track D Phase D (2026-05-03) — Layer 2.
;;
;; Bridges the unprefixed file-I/O primitives (= `expand-file-name',
;; `file-name-directory', `file-exists-p', `insert-file-contents',
;; `write-region', ...) to the existing `nelisp-emacs-compat-fileio'
;; (= `nelisp-ec-*') substrate, AND derives the high-level commands
;; the user-facing edit cycle needs (= `find-file-noselect',
;; `find-file', `save-buffer', `write-file', `revert-buffer',
;; `buffer-file-name', `set-visited-file-name').
;;
;; Architecture:
;;
;;   - 1:1 defalias bridges for the substrate-direct primitives.
;;   - A global alist `emacs-fileio--buffer-files' keyed by buffer
;;     record (= eq) maps each visited buffer to its filename.  This
;;     stands in for Emacs' buffer-local `buffer-file-name' until
;;     `nelisp-ec-buffer' grows a real per-buffer slot.
;;   - High-level commands compose the bridges: e.g. `find-file-noselect'
;;     = `(get-buffer-create NAME)' + `(insert-file-contents)' +
;;     `(set-visited-file-name)'.
;;
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub-bulk.el' no-op shims.
;;
;; Phase D unblocks the β-stage edit cycle: `nemacs hello.txt' ->
;; `(insert "world")' -> `(save-buffer)' -> `(kill-buffer)'.
;; Reading and writing UTF-8 files is the whole substrate target;
;; locking, auto-save, backup files, and coding-system selection
;; remain L1.5 / future-phase work.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)
(require 'emacs-buffer-builtins)
(require 'emacs-string)
(require 'files-runtime)
(require 'emacs-file-name-handler)

(defconst emacs-fileio-builtins--standalone-overrides
  '(insert-file-contents
    load-file
    normal-top-level-add-subdirs-to-load-path
    add-to-load-path
    write-region
    file-name-quoted-p
    file-name-quote
    file-name-unquote
    expand-file-name
    buffer-file-name
    set-visited-file-name
    locate-library
    executable-find
    substitute-in-file-name
    find-file-noselect
    find-file
    save-buffer
    write-file
    revert-buffer
    file-exists-p
    file-readable-p
    file-directory-p
    file-attributes)
  "Functions this bridge may overwrite under standalone NeLisp.
Path parsing, predicates, and directory/syscall primitives are left to
the runtime when they already exist because `load' / `require' depend on
those semantics during bootstrap. `expand-file-name' is explicitly on this
override list so standalone readers always get the pure helper (including
`~' expansion) even when bootstrap stubs are present.")

(defvar emacs-fileio-builtins--host-write-region
  (and (not (files-standalone-runtime-p))
       (fboundp 'write-region)
       (subrp (symbol-function 'write-region))
       (symbol-function 'write-region))
  "Host `write-region' subr captured before any standalone override logic.")

(defun emacs-fileio-builtins--host-emacs-p ()
  "Return non-nil only for real host Emacs, never for standalone images."
  (and (boundp 'emacs-version)
       (not (eq emacs-version 'nelisp--unbound-marker))
       (not (files-standalone-runtime-p))))

(defvar emacs-fileio-builtins--host-make-temp-file
  (and (emacs-fileio-builtins--host-emacs-p)
       (fboundp 'make-temp-file)
       (symbol-function 'make-temp-file))
  "Host `make-temp-file' captured before sentinel-safe wrapper install.")

(defun emacs-fileio-builtins--standalone-p ()
  "Return non-nil when running on the standalone NeLisp reader.
A bound `emacs-version' is unreliable here: the reader binds it to the
`nelisp--unbound-marker' sentinel, so `boundp' returns t even with no
host Emacs.  Use `files-standalone-runtime-p' so
`--standalone-overrides' force-install fires on the reader instead of
leaving stub function cells in place."
  (files-standalone-runtime-p))

(defun emacs-fileio-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (or (not (emacs-fileio-builtins--function-cell-live-p symbol))
      (and (emacs-fileio-builtins--standalone-p)
           (memq symbol emacs-fileio-builtins--standalone-overrides))))

(defun emacs-fileio-builtins--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (let ((function (condition-case nil
                           (symbol-function symbol)
                         (error nil))))
         (and function
              (not (eq function 'nelisp--unbound-marker))))))

;;;; --- batched trivial defaliases (Doc 51 Phase 5 boot perf) -----------
;;
;; Pattern source: commit d3c17fa (emacs-stub-bulk Phase 11.D batch).  The
;; nelisp standalone interpreter charges ~47ms per top-level form for the
;; original `(unless (fboundp X) (defalias X #'nelisp-ec-Y))' idiom — 14
;; clauses below + 23 in `emacs-buffer-builtins.el' add ~1.8s on every
;; bootstrap.  Collapsing through one dolist body keeps the gate semantics
;; identical (= each entry still does exactly one fboundp test) while
;; paying the per-form interpreter overhead only once.  Under host Emacs
;; the C subr wins fboundp so this is a no-op either way.
;;
;; All 14 substrate-direct file-I/O bridges in this file map uniformly to
;; `nelisp-ec-<same-name>', so we use a bare symbol list (no pairs) and
;; synthesize the target name with `(intern (concat "nelisp-ec-" ...))'.
;;
;; Doc 37 (Tramp ssh-only lane, task #16) splits the original single
;; dolist in two.  Most of these primitives are Emacs "magic file name"
;; operations: real Emacs dispatches them through `file-name-handler-alist'
;; (via `find-file-name-handler') before ever reaching the local
;; filesystem, which is exactly how Tramp intercepts a `/ssh:host:' path.
;; `emacs-fileio-builtins--handler-dispatch-ops' gets a thin
;; `emacs-fnh-wrap' wrapper instead of a bare alias so a magic file name
;; reaches its handler first; `emacs-fileio-builtins--handler-passthrough-ops'
;; keeps the previous bare alias because Emacs does not dispatch those two
;; through `file-name-handler-alist' (`file-name-absolute-p' is pure
;; string syntax; `executable-find' walks `exec-path', not a file name).
;;
;; This is purely additive: which primitives get *installed* here is
;; still governed by `emacs-fileio-builtins--install-function-p' exactly
;; as before (host Emacs's own C primitives win there and this whole
;; block stays inert); only what a standalone install becomes has
;; changed, from a bare `nelisp-ec-*' alias to a handler-aware wrapper
;; around the same `nelisp-ec-*' function.

(defconst emacs-fileio-builtins--handler-passthrough-ops
  '(file-name-absolute-p
    executable-find)
  "File-I/O primitives Emacs does NOT dispatch through
`file-name-handler-alist' (pure string syntax or a `exec-path' search).
These keep the historical bare `nelisp-ec-*' alias.")

(defconst emacs-fileio-builtins--handler-dispatch-ops
  '(;; --- file-name parsing
    expand-file-name
    file-name-directory
    file-name-nondirectory
    file-name-as-directory
    substitute-in-file-name
    ;; --- predicates / attributes
    file-exists-p
    file-readable-p
    file-executable-p
    file-directory-p
    file-attributes
    directory-files
    ;; --- mutation
    delete-file
    rename-file
    ;; --- read
    insert-file-contents)
  "File-I/O primitives whose Emacs operation symbol matches their own
function name in `file-name-handler-alist' dispatch (the `files.el'/
`fileio.c' convention).  Each entry here gets a handler-aware wrapper
(see `emacs-fnh-wrap') instead of a bare `nelisp-ec-*' alias so a magic
file name (e.g. a Tramp `/ssh:' path) reaches its handler before falling
through to the local `nelisp-ec-*' substrate.
`emacs-fnh-wrap' only inspects the FIRST argument.  `rename-file' takes
FILE and NEWNAME; real Emacs also checks NEWNAME's handler for
cross-filesystem renames, but the ssh-only lane's approved scope
excludes cross-host operations, so only FILE is dispatched here.")

(dolist (--name-- emacs-fileio-builtins--handler-passthrough-ops)
  (when (emacs-fileio-builtins--install-function-p --name--)
    (defalias --name--
      (intern (concat "nelisp-ec-" (symbol-name --name--))))))

(dolist (--name-- emacs-fileio-builtins--handler-dispatch-ops)
  (when (emacs-fileio-builtins--install-function-p --name--)
    (fset --name--
          (emacs-fnh-wrap --name--
                          (intern (concat "nelisp-ec-" (symbol-name --name--)))))))

;; --- access(2)-backed predicates for the standalone reader ----------
;; The `emacs-fileio-builtins--install-function-p' standalone clause keys
;; off `(not (boundp 'emacs-version))', but nemacs binds `emacs-version'
;; (for vendor compatibility), so that clause never fires here.  Its
;; native `file-exists-p' meanwhile resolves to a stat path that
;; misreports on the reader (always nil), and `file-executable-p' is
;; absent.  When the verified access(2) primitive `nelisp--syscall-path-int'
;; is present we therefore force-install the `nelisp-ec-*' access-backed
;; predicates over whatever the runtime bound.  This block is inert under
;; host Emacs (no `nelisp--syscall-path-int'), so the C builtins stand.
(when (fboundp 'nelisp--syscall-path-int)
  (defalias 'file-exists-p #'nelisp-ec-file-exists-p)
  (defalias 'file-readable-p #'nelisp-ec-file-readable-p)
  (defalias 'file-executable-p #'nelisp-ec-file-executable-p)
  ;; `executable-find' is in `--standalone-overrides' but, like the
  ;; predicates above, that clause does not fire on nemacs; an earlier
  ;; `emacs-stub' stub otherwise shadows the working `nelisp-ec' PATH walk.
  (defalias 'executable-find #'nelisp-ec-executable-find))

(when (emacs-fileio-builtins--install-function-p 'file-accessible-directory-p)
  (defun file-accessible-directory-p (filename)
    "Return non-nil if FILENAME is a searchable directory."
    (and (file-directory-p filename)
         (file-executable-p filename))))

(unless (boundp 'auto-save-visited-mode)
  (defvar auto-save-visited-mode nil
    "Headless no-op compatibility toggle for `auto-save-visited-mode'."))

(unless (boundp 'global-auto-revert-mode)
  (defvar global-auto-revert-mode nil
    "Headless no-op compatibility toggle for `global-auto-revert-mode'."))

(unless (boundp 'current-language-environment)
  (defvar current-language-environment "English"
    "Standalone language environment marker for the UTF-8-only substrate."))

(unless (boundp 'language-environment-history)
  (defvar language-environment-history nil
    "History list for `set-language-environment' prompts."))

(unless (boundp 'buffer-file-coding-system)
  (defvar buffer-file-coding-system 'utf-8-unix
    "Standalone default buffer-file coding system."))

(unless (boundp 'default-buffer-file-coding-system)
  (defvar default-buffer-file-coding-system 'utf-8-unix
    "Standalone default coding system for visited files."))

(unless (boundp 'default-process-coding-system)
  (defvar default-process-coding-system '(utf-8-unix . utf-8-unix)
    "Standalone default coding systems for subprocess I/O."))

(unless (boundp 'terminal-coding-system)
  (defvar terminal-coding-system 'utf-8-unix
    "Standalone default terminal coding system."))

(unless (boundp 'image-file-name-extensions)
  (defvar image-file-name-extensions
    '("png" "jpg" "jpeg" "gif" "svg" "webp")
    "Standalone image filename extensions seen by init compatibility code."))

(unless (boundp 'large-file-warning-threshold)
  (defvar large-file-warning-threshold nil
    "Standalone large-file warning threshold compatibility variable."))

(defun emacs-fileio-builtins--noop-toggle (symbol arg)
  "Update SYMBOL from ARG using minor-mode toggle rules."
  (set symbol
       (if (null arg)
           (not (and (boundp symbol) (symbol-value symbol)))
         (not (or (eq arg 0)
                  (eq arg -1)
                  (and (numberp arg) (<= arg 0))))))
  nil)

(when (emacs-fileio-builtins--install-function-p 'auto-save-visited-mode)
  (defalias 'auto-save-visited-mode #'ignore))

(when (emacs-fileio-builtins--install-function-p 'global-auto-revert-mode)
  (defalias 'global-auto-revert-mode #'ignore))

(when (emacs-fileio-builtins--install-function-p 'set-language-environment)
  (defun set-language-environment (environment)
    "Record ENVIRONMENT in the UTF-8-only standalone substrate."
    (setq current-language-environment
          (if (and (stringp environment) (> (length environment) 0))
              environment
            "English"))
    (setq language-environment-history
          (cons current-language-environment
                (delete current-language-environment
                        language-environment-history)))
    current-language-environment))

(when (emacs-fileio-builtins--install-function-p 'prefer-coding-system)
  (defun prefer-coding-system (coding-system)
    "Record CODING-SYSTEM as the preferred UTF-8-only coding choice."
    (setq buffer-file-coding-system coding-system
          default-buffer-file-coding-system coding-system
          default-process-coding-system (cons coding-system coding-system))
    coding-system))

(when (emacs-fileio-builtins--install-function-p 'set-terminal-coding-system)
  (defun set-terminal-coding-system (coding-system)
    "Record CODING-SYSTEM as the standalone terminal coding choice."
    (setq terminal-coding-system coding-system)
    coding-system))

(defun emacs-fileio-rdf-file-exists-p (filename)
  "Return non-nil when standalone `rdf' can open FILENAME.
This is a standalone fallback for runtimes where the stat/access
compatibility paths can report ENOSYS while `rdf' is the verified
read-open route."
  (let ((text (and (stringp filename)
                   (fboundp 'rdf)
                   (condition-case nil
                       (rdf filename)
                     (error nil)))))
    (and (stringp text)
         (> (length text) 0))))

(when (and (fboundp 'rdf)
           (not (fboundp 'nelisp--syscall-path-int))
           (not (fboundp 'nl-syscall-opendir)))
  (defalias 'file-exists-p #'emacs-fileio-rdf-file-exists-p)
  (defalias 'file-readable-p #'emacs-fileio-rdf-file-exists-p))

;;;; --- file-name parsing ----------------------------------------------

;; expand-file-name / file-name-absolute-p / file-name-directory /
;; file-name-nondirectory / file-name-as-directory batched into the dolist
;; near the top.

(defun emacs-fileio--directory-file-names (dir)
  "Return the non-full file names in DIR, or nil if DIR cannot be read."
  (condition-case nil
      (directory-files dir nil nil t)
    (error nil)))

(defun emacs-fileio--safe-directory-probe-p ()
  "Return non-nil when `locate-library' may inspect directories.
The legacy standalone `nelisp--syscall-readdir' / stat predicates can
stop the evaluator on some failure paths.  Until the newer opendir API
is present, standalone `locate-library' must prefer a safe nil result
over probing the filesystem."
  (if (files-standalone-runtime-p)
      (fboundp 'nl-syscall-opendir)
    t))

(defun emacs-fileio--locate-library-candidate (base name)
  "Return absolute path for NAME under BASE when the directory lists it."
  (let* ((relative-dir (file-name-directory name))
         (leaf (file-name-nondirectory name))
         (search-dir (if relative-dir
                         (expand-file-name relative-dir base)
                       base))
         (entries (emacs-fileio--directory-file-names search-dir)))
    (and (member leaf entries)
         (expand-file-name name base))))

(defun emacs-fileio--regular-file-p (candidate)
  "Return non-nil when CANDIDATE names an existing non-directory file."
  (and (nelisp-ec-file-exists-p candidate)
       (not (nelisp-ec-file-directory-p candidate))))

(defun emacs-fileio-locate-library (library &optional nosuffix path interactive-call)
  "Return the absolute path of LIBRARY found on PATH or `load-path'.
This implementation probes exact candidates without requiring opendir.
INTERACTIVE-CALL is accepted for Emacs API parity and ignored."
  (ignore interactive-call)
  (let* ((absolute (and (> (length library) 0)
                        (eq (aref library 0) ?/)))
         (has-el (string-suffix-p ".el" library))
         (names (cond
                 (nosuffix (list library))
                 ;; An explicit `.el' name is the caller's decision and must
                 ;; win over a sibling `.elc': probing "c" first resolved
                 ;; FILE.el to a stale host byte-compiled FILE.elc, whose
                 ;; `#@NNN' dynamic-docstring markers read as symbols on the
                 ;; text load path (measured: `void-variable: (#@187)' while
                 ;; loading the magit bridge).  Bare names keep the
                 ;; `.elc'-first fast path below.
                 (has-el (list library (concat library "c")))
                 (t (list (concat library ".elc")
                          (concat library ".el")
                          library))))
         (dirs (if absolute
                   (list "")
                 (cons (and (boundp 'default-directory) default-directory)
                       (or path load-path))))
         found)
    (catch 'done
      (dolist (dir dirs)
        (when (or absolute (and (stringp dir) (> (length dir) 0)))
          (dolist (name names)
            (let ((candidate (if absolute
                                 name
                               (nelisp-ec-expand-file-name name dir))))
              (when (emacs-fileio--regular-file-p candidate)
                (setq found candidate)
                (throw 'done nil)))))))
    found))

(when (emacs-fileio-builtins--install-function-p 'locate-library)
  (defalias 'locate-library #'emacs-fileio-locate-library))

(when (emacs-fileio-builtins--install-function-p 'file-name-quoted-p)
  (defun file-name-quoted-p (name &optional top)
    "Return non-nil when NAME starts with the Emacs quote prefix `/:'."
    (ignore top)
    (and (stringp name)
         (string-prefix-p "/:" name))))

(when (emacs-fileio-builtins--install-function-p 'file-name-quote)
  (defun file-name-quote (name &optional top)
    "Add the Emacs file-name quote prefix `/:` to NAME if needed."
    (ignore top)
    (if (file-name-quoted-p name)
        name
      (concat "/:" name))))

(when (emacs-fileio-builtins--install-function-p 'file-name-unquote)
  (defun file-name-unquote (name &optional top)
    "Remove the Emacs file-name quote prefix `/:` from NAME when present."
    (ignore top)
    (if (file-name-quoted-p name)
        (substring name 2)
      name)))

;;;; --- predicates / attributes ----------------------------------------

;; file-exists-p / file-readable-p / file-directory-p / file-attributes /
;; directory-files / executable-find batched into the dolist near the top.

;;;; --- mutation ------------------------------------------------------

;; delete-file / rename-file batched into the dolist near the top.

;;;; --- read / write ---------------------------------------------------

;; insert-file-contents batched into the dolist near the top.

(defun emacs-fileio-builtins-load-file (file)
  "Load exactly FILE, propagating read and evaluation errors."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (let ((absolute (nelisp-ec-expand-file-name file)))
    (unless (nelisp-ec-file-exists-p absolute)
      (signal 'file-error (list "Cannot open load file" absolute)))
    (if (emacs-fileio-builtins--standalone-p)
        (let ((source (rdf absolute)))
          (unless (stringp source)
            (signal 'file-error (list "Cannot open load file" absolute)))
          (let ((prior-name (and (boundp 'load-file-name) load-file-name))
                (prior-directory
                 (and (boundp 'default-directory) default-directory)))
            (setq load-file-name absolute)
            (setq default-directory
                  (or (nelisp-ec-file-name-directory absolute) "/"))
            (unwind-protect
                (nelisp--eval-source-string source)
              (setq load-file-name prior-name)
              (setq default-directory prior-directory)))
          t)
      (load absolute nil nil t nil))))

(when (emacs-fileio-builtins--install-function-p 'load-file)
  (defalias 'load-file #'emacs-fileio-builtins-load-file))

(defconst emacs-fileio-builtins--load-path-excluded-directories
  '("RCS" "CVS" "SCCS")
  "Directory basenames excluded from recursive load-path discovery.")

(defun emacs-fileio-builtins--load-path-directory-name-p (name)
  "Return non-nil when NAME starts with an ASCII alphanumeric character."
  (and (stringp name)
       (> (length name) 0)
       (let ((char (aref name 0)))
         (or (and (>= char ?A) (<= char ?Z))
             (and (>= char ?a) (<= char ?z))
             (and (>= char ?0) (<= char ?9))))
       (not (member name emacs-fileio-builtins--load-path-excluded-directories))))

(defvar emacs-fileio-builtins--canonical-directory-cache nil
  "Dynamically bound cache for directory canonical identities.")

(defvar files--truename-prefix-cache nil
  "Dynamically bound cache for standalone truename prefix resolution.")

(defun emacs-fileio-builtins--canonical-directory (directory)
  "Return a stable canonical identity for DIRECTORY."
  (let* ((absolute (nelisp-ec-expand-file-name directory))
         (cached (and emacs-fileio-builtins--canonical-directory-cache
                      (gethash absolute
                               emacs-fileio-builtins--canonical-directory-cache))))
    (if cached
        cached
      (let* ((symlink-target (and (fboundp 'file-symlink-p)
                                  (file-symlink-p absolute)))
             (canonical
              (cond
               ((not symlink-target)
                (directory-file-name absolute))
               ((and (emacs-fileio-builtins--standalone-p)
                     (fboundp 'files--truename-walk))
                (directory-file-name (files--truename-walk absolute 0)))
               ((emacs-fileio-builtins--host-emacs-p)
                (condition-case nil
                    (directory-file-name (file-truename absolute))
                  (error (directory-file-name absolute))))
               (t (directory-file-name absolute)))))
        (when emacs-fileio-builtins--canonical-directory-cache
          (puthash absolute canonical
                   emacs-fileio-builtins--canonical-directory-cache))
        canonical))))

(defun emacs-fileio-builtins--canonical-member-p (directory directories)
  "Return non-nil when DIRECTORY's canonical identity occurs in DIRECTORIES."
  (let ((identity (emacs-fileio-builtins--canonical-directory directory))
        (tail directories)
        found)
    (while (and tail (not found))
      (when (and (stringp (car tail))
                 (string-equal
                  identity
                  (emacs-fileio-builtins--canonical-directory (car tail))))
        (setq found t))
      (setq tail (cdr tail)))
    found))

(defun emacs-fileio-builtins--discover-load-path-subdirs (root)
  "Return eligible subdirectories below ROOT in stable breadth-first order."
  (let* ((root-id (emacs-fileio-builtins--canonical-directory root))
         (level (list root-id))
         (visited (make-hash-table :test 'equal))
         (result nil))
    (puthash root-id t visited)
    (while level
      (let ((next nil))
        (dolist (directory level)
          (let ((directory-name (file-name-as-directory directory)))
            ;; `directory-files' already returns `string-lessp'-sorted names.
            ;; Request basenames so that its internal sort avoids comparing
            ;; repeated absolute-path prefixes for every sibling pair.
            (dolist (name (directory-files directory nil nil nil))
              (unless (or (string-equal name ".")
                          (string-equal name ".."))
                (let ((child (concat directory-name name)))
                  (when (and (emacs-fileio-builtins--load-path-directory-name-p name)
                         (file-directory-p child)
                         (not (file-exists-p
                               (nelisp-ec-expand-file-name ".nosearch" child))))
                    (let ((child-id
                           (if (and (fboundp 'file-symlink-p)
                                    (file-symlink-p child))
                               (emacs-fileio-builtins--canonical-directory child)
                             (directory-file-name child))))
                      (unless (gethash child-id visited)
                        (puthash child-id t visited)
                        (setq result (cons child-id result))
                        (setq next (cons child-id next))))))))))
        (setq level (nreverse next))))
    (nreverse result)))

(defun emacs-fileio-builtins-add-load-path-tree (root)
  "Add ROOT's eligible directory tree to `load-path' without duplicates."
  (let ((emacs-fileio-builtins--canonical-directory-cache
         (make-hash-table :test 'equal))
        (files--truename-prefix-cache
         (make-hash-table :test 'equal)))
    (let* ((root (nelisp-ec-expand-file-name root))
           (root-id (emacs-fileio-builtins--canonical-directory root))
           (subdirs (emacs-fileio-builtins--discover-load-path-subdirs root))
           (subdir-ids (make-hash-table :test 'equal))
           (old load-path)
           (out nil)
           inserted)
      (dolist (subdir subdirs)
        (puthash subdir t subdir-ids))
      (while old
        (let ((entry (car old)))
          (if (stringp entry)
              (let ((entry-id (emacs-fileio-builtins--canonical-directory entry)))
                (if (string-equal root-id entry-id)
                    (unless inserted
                      (push entry out)
                      (dolist (subdir subdirs)
                        (push subdir out))
                      (setq inserted t))
                  (unless (gethash entry-id subdir-ids)
                    (push entry out))))
            (push entry out)))
        (setq old (cdr old)))
      (unless inserted
        (push root out)
        (dolist (subdir subdirs)
          (push subdir out)))
      (setq load-path (nreverse out)))))

(defun emacs-fileio-builtins-normal-top-level-add-subdirs-to-load-path ()
  "Add eligible directories below `default-directory' to `load-path'."
  (emacs-fileio-builtins-add-load-path-tree default-directory))

(defun emacs-fileio-builtins-add-to-load-path (&rest directories)
  "Add each of DIRECTORIES and its eligible descendants to `load-path'."
  (dolist (directory directories)
    (emacs-fileio-builtins-add-load-path-tree directory))
  load-path)

(when (emacs-fileio-builtins--install-function-p
       'normal-top-level-add-subdirs-to-load-path)
  (defalias 'normal-top-level-add-subdirs-to-load-path
    #'emacs-fileio-builtins-normal-top-level-add-subdirs-to-load-path))

(when (emacs-fileio-builtins--install-function-p 'add-to-load-path)
  (defalias 'add-to-load-path #'emacs-fileio-builtins-add-to-load-path))

(defun emacs-fileio-builtins--local-write-region
    (start end filename &optional append visit _lockname _mustbenew)
  "Local `write-region' body: forward to `nelisp-ec-write-region'.
LOCKNAME / MUSTBENEW are accepted for API parity but ignored -- the
substrate has no file-locking subsystem yet."
  (cond
   ((and (stringp start) (null end))
    (cond
     ((and (not (emacs-fileio-builtins--standalone-p))
           emacs-fileio-builtins--host-write-region)
      (funcall emacs-fileio-builtins--host-write-region
               start nil filename append visit))
     ((and append (fboundp 'nl-append-file))
      (nl-append-file filename start)
      (length start))
     ((and (fboundp 'nl-write-file) (not append))
      (nl-write-file filename start)
      (length start))
     (t
      ;; One behaviour, one owner: `nelisp-ec-write-region' now handles
      ;; string START itself (and reaches the captured host writer through
      ;; `nelisp-ec--write-raw-bytes'), so the former dead-end signal is
      ;; replaced by the same path every other case takes.
      (nelisp-ec-write-region start nil filename append visit))))
   ;; Buffer-sourced writes (nil or integer START) must read from the
   ;; buffer substrate that owns the current buffer.  When no nelisp-ec
   ;; buffer is selected, the current buffer is a real host buffer
   ;; (e.g. host `with-temp-file' expanding to `write-region nil nil');
   ;; the captured pre-wrap host writer implements the nil/integer
   ;; START contract natively, VISIT and coding included.  On the
   ;; standalone reader the capture is nil, so this arm never fires
   ;; there.
   ((and (not (bound-and-true-p nelisp-ec--current-buffer))
         emacs-fileio-builtins--host-write-region)
    (funcall emacs-fileio-builtins--host-write-region
             start end filename append visit))
   (t
    (nelisp-ec-write-region start end filename append visit))))

(defun emacs-fileio-builtins-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Phase D polyfill: forward to `nelisp-ec-write-region', with dispatch.
Doc 37: FILENAME (the third argument, not the first -- unlike the
generic `emacs-fnh-dispatch' convention) is checked against
`file-name-handler-alist' first, so a magic file name (e.g. a Tramp
`/ssh:' path) reaches its handler; a local FILENAME falls through
unchanged to `emacs-fileio-builtins--local-write-region'."
  (let ((handler (emacs-fnh-find-file-name-handler filename 'write-region)))
    (if handler
        (funcall handler 'write-region
                 start end filename append visit lockname mustbenew)
      (emacs-fileio-builtins--local-write-region
       start end filename append visit lockname mustbenew))))

;;;; --- temp files -----------------------------------------------------
;; `make-temp-file' is absent on the standalone reader (nemacs), and a
;; call to an unbound function there is a non-catchable abort (it does not
;; signal `void-function'), so callers like test harnesses cannot even
;; degrade gracefully.  Provide a working implementation on top of the
;; primitives that do exist (`write-region' + `file-exists-p').  Gated on
;; `fboundp' so host Emacs keeps its C builtin.

(declare-function emacs-pid "ext:editfns")

(defvar emacs-fileio--temp-counter 0
  "Monotonic counter feeding `emacs-fileio-make-temp-name' uniqueness.")

(unless (boundp 'temp-directory)
  (defvar temp-directory nil
    "Compatibility fallback for runtimes that do not bind temp-directory."))

(defun emacs-fileio-builtins--usable-temp-dir-value (symbol)
  "Return SYMBOL's usable temp-directory value, or nil."
  (when (boundp symbol)
    (let ((value (symbol-value symbol)))
      (and (stringp value)
           (> (length value) 0)
           (not (eq value 'nelisp--unbound-marker))
           value))))

(defun emacs-fileio-builtins--host-temp-vars-usable-p ()
  "Return non-nil when host `make-temp-file' can safely read temp vars."
  (or (emacs-fileio-builtins--usable-temp-dir-value
       'temporary-file-directory)
      (emacs-fileio-builtins--usable-temp-dir-value
       'temp-directory)))

(defun emacs-fileio-make-temp-name (prefix)
  "Return a unique, currently-nonexistent file name built from PREFIX.
PREFIX already includes any directory.  Standalone counterpart to
Emacs' `make-temp-name'; it does NOT create the file."
  (let* ((pid-value (and (fboundp 'emacs-pid) (emacs-pid)))
         (pid (if (integerp pid-value) pid-value 0))
         (name nil)
         (n 0))
    (while (and (or (null name)
                    (nelisp-ec-file-exists-p name))
                (< n 100000))
      (setq emacs-fileio--temp-counter (1+ emacs-fileio--temp-counter)
            n (1+ n)
            name (concat prefix (format "%d-%d" pid emacs-fileio--temp-counter))))
    name))

(defun emacs-fileio-builtins--usable-temp-dir ()
  "Return a usable temp directory string from runtime defaults."
  (let ((candidates (list (emacs-fileio-builtins--usable-temp-dir-value
                           'temporary-file-directory)
                          (emacs-fileio-builtins--usable-temp-dir-value
                           'temp-directory)
                          "/tmp/"))
        (found nil))
    (while (and candidates (not found))
      (let ((candidate (car candidates)))
        (when (and (stringp candidate)
                   (> (length candidate) 0))
          (setq found candidate)))
      (setq candidates (cdr candidates)))
    (or found "/tmp/")))

(defun emacs-fileio-make-temp-file (prefix &optional dir-flag suffix text)
  "Standalone `make-temp-file': create a unique temp file, return its name.
PREFIX is taken relative to `temporary-file-directory'.  SUFFIX, when a
string, is appended.  TEXT, when a string, is written as the initial
contents.  DIR-FLAG creates a directory instead (needs `make-directory')."
  (let* ((dir (file-name-as-directory
               (emacs-fileio-builtins--usable-temp-dir)))
         (name (emacs-fileio-make-temp-name (concat dir prefix))))
    ;; A suffix can re-introduce a collision; bump until the full name is free.
    (when (stringp suffix)
      (setq name (concat name suffix))
      (while (nelisp-ec-file-exists-p name)
        (setq name (concat (emacs-fileio-make-temp-name (concat dir prefix))
                           suffix))))
    (cond
     (dir-flag
      (if (fboundp 'make-directory)
          (progn (make-directory name t) name)
        (signal 'file-error
                (list "make-temp-file: directory creation unsupported" name))))
     (t
      (write-region (if (stringp text) text "") nil name)
      name))))

(defun emacs-fileio-builtins-make-temp-file (prefix &optional dir-flag suffix text)
  "Sentinel-safe wrapper for `make-temp-file'."
  (if (and emacs-fileio-builtins--host-make-temp-file
           (emacs-fileio-builtins--host-temp-vars-usable-p))
      (funcall emacs-fileio-builtins--host-make-temp-file
               prefix dir-flag suffix text)
    (emacs-fileio-make-temp-file prefix dir-flag suffix text)))

(unless (fboundp 'make-temp-name)
  (defalias 'make-temp-name #'emacs-fileio-make-temp-name))
(defalias 'make-temp-file #'emacs-fileio-builtins-make-temp-file)

(defun emacs-fileio-builtins-install-standalone-temp-shims ()
  "Reinstall standalone temp-name/temp-file shims when vendor code overrides them.
On host Emacs keep the native implementation untouched.  On the standalone
reader some vendor loads replace `make-temp-file' with upstream Lisp that
expects C primitives the reader does not provide."
  (when (emacs-fileio-builtins--standalone-p)
    (defalias 'make-temp-name #'emacs-fileio-make-temp-name)
    (defalias 'make-temp-file #'emacs-fileio-make-temp-file))
  t)

(defun emacs-fileio-builtins-reinstall-standalone-overrides ()
  "Reinstall standalone file I/O entry points after vendor override loads."
  (when (emacs-fileio-builtins--standalone-p)
    (defalias 'write-region #'emacs-fileio-builtins-write-region)
    (emacs-fileio-builtins-install-standalone-temp-shims))
  t)

;;;; --- standalone-reader band-aid fills -------------------------------
;; Functions the anvil-pkg test suite calls that are absent on the
;; standalone reader (nemacs).  On nemacs a call to an unbound function is
;; a non-catchable abort (it does not signal `void-function'), so the
;; suite runner cannot degrade past them; defining them removes that abort.
;; All are gated on `fboundp', so host Emacs keeps its C/lisp builtins.
;; They are grouped here for expedience (cross-cutting, not all file I/O).

(declare-function nelisp-ec-access "nelisp-emacs-compat-fileio" (file mode))

(unless (fboundp 'booleanp)
  (defun booleanp (object)
    "Return t if OBJECT is one of the two canonical booleans (nil or t)."
    (and (memq object '(nil t)) t)))

(unless (fboundp 'file-name-base)
  (defun file-name-base (&optional filename)
    "Return the base name of FILENAME: no directory, no extension."
    (file-name-sans-extension (file-name-nondirectory (or filename "")))))

(unless (fboundp 'file-writable-p)
  (defun file-writable-p (filename)
    "Return non-nil if FILENAME can be written or created.
access(2) W_OK on the file, or on its directory when the file does not
exist yet (approximating Emacs' creatable-path semantics)."
    (let ((rc (and (fboundp 'nelisp-ec-access) (nelisp-ec-access filename 2))))
      (cond
       ((and (integerp rc) (zerop rc)) t)
       ((and (fboundp 'file-exists-p) (file-exists-p filename)) nil)
       (t (let* ((dir (or (file-name-directory (directory-file-name filename))
                          "./"))
                 (drc (and (fboundp 'nelisp-ec-access)
                           (nelisp-ec-access dir 2))))
            (and (integerp drc) (zerop drc))))))))

(unless (fboundp 'insert-file-contents-literally)
  (defun insert-file-contents-literally (filename &optional visit beg end replace)
    "Like `insert-file-contents' but without coding/format conversion."
    (insert-file-contents filename visit beg end replace)))

(unless (fboundp 'lwarn)
  (defun lwarn (type level message &rest args)
    "Minimal `lwarn': route to `message'; no *Warnings* buffer."
    (ignore type level)
    (when (fboundp 'message) (apply #'message message args))
    nil))

(unless (fboundp 'format-time-string)
  (defun format-time-string (format-string &optional time zone)
    "Minimal stand-in: ignores FORMAT-STRING directives and returns the
time as an integer-seconds string.  Adequate for non-display uses (e.g.
cache timestamps); NOT a faithful strftime."
    (ignore format-string zone)
    (format "%d" (cond ((numberp time) (truncate time))
                       ((fboundp 'float-time)
                        (condition-case nil (truncate (float-time time))
                          (error 0)))
                       (t 0)))))

(unless (fboundp 'delete-directory)
  (defun delete-directory (directory &optional recursive trash)
    "Minimal stand-in: no-op when the runtime lacks a directory-remove
primitive.  Returns nil (temp-dir cleanup leaks rather than erroring)."
    (ignore directory recursive trash)
    nil))

;;;; --- visited-file state ---------------------------------------------

(defvar emacs-fileio--buffer-files nil
  "Phase D state: alist mapping `nelisp-ec-buffer' record → filename.
Stands in for Emacs' buffer-local `buffer-file-name' until the
substrate adds a real per-buffer slot.  Entries for killed buffers
are cleaned up by `find-file-noselect' on next visit.")

(defun emacs-fileio--clean-killed ()
  "Drop entries from `emacs-fileio--buffer-files' whose buffer is killed."
  (let ((live nil))
    (dolist (cell emacs-fileio--buffer-files)
      (when (and (nelisp-ec-buffer-p (car cell))
                 (not (nelisp-ec-buffer-killed-p (car cell))))
        (setq live (cons cell live))))
    (setq emacs-fileio--buffer-files (nreverse live))))

(when (emacs-fileio-builtins--install-function-p 'buffer-file-name)
  (defun buffer-file-name (&optional buffer)
    "Phase D polyfill: read the visited filename of BUFFER (default = current).
Returns nil when the buffer is not visiting a file."
    (let ((b (or buffer (nelisp-ec-current-buffer))))
      (when (and b (nelisp-ec-buffer-p b)
                 (not (nelisp-ec-buffer-killed-p b)))
        (or (cdr (assq b emacs-fileio--buffer-files))
            (let ((base (and (fboundp 'buffer-base-buffer)
                             (buffer-base-buffer b))))
              (and base (buffer-file-name base))))))))

(when (emacs-fileio-builtins--install-function-p 'set-visited-file-name)
  (defun set-visited-file-name (filename &optional no-query along-with-file)
    "Phase D polyfill: associate the current buffer with FILENAME.
NO-QUERY / ALONG-WITH-FILE are accepted for API parity but ignored —
the substrate has no rename-on-visit / lockfile interaction yet."
    (ignore no-query along-with-file)
    (let ((b (nelisp-ec-current-buffer)))
      (when (and b (nelisp-ec-buffer-p b))
        (setq emacs-fileio--buffer-files
              (cons (cons b filename)
                    (assq-delete-all b emacs-fileio--buffer-files)))
        filename))))

(when (emacs-fileio-builtins--install-function-p 'get-file-buffer)
  (defun get-file-buffer (filename)
    "Return a live buffer already visiting FILENAME, or nil."
    (let ((abs (and (stringp filename)
                    (if (fboundp 'expand-file-name)
                        (expand-file-name filename)
                      filename)))
          (found nil))
      (cond
       ((and abs (fboundp 'find-buffer-visiting))
        (setq found (find-buffer-visiting abs)))
       ((and abs (fboundp 'buffer-list))
        (catch 'done
          (dolist (buffer (buffer-list))
            (let ((visited (emacs-fileio--direct-buffer-file-name buffer)))
              (when (and (stringp visited)
                         (equal (if (fboundp 'expand-file-name)
                                    (expand-file-name visited)
                                  visited)
                                abs))
                (setq found buffer)
                (throw 'done found)))))))
      found)))

;;;; --- find-file / save-buffer / write-file / revert-buffer ----------

(defun emacs-fileio--direct-buffer-file-name (buffer)
  "Return BUFFER's visited file name, or nil."
  (and buffer
       (condition-case nil
           (let ((registered (and (boundp 'emacs-fileio--buffer-files)
                                  (cdr (assq buffer emacs-fileio--buffer-files)))))
             (or registered
                 (and (not (and (fboundp 'nelisp-ec-buffer-p)
                                (nelisp-ec-buffer-p buffer)))
                      (fboundp 'buffer-file-name)
                      (buffer-file-name buffer))))
         (error
          (and (boundp 'emacs-fileio--buffer-files)
               (cdr (assq buffer emacs-fileio--buffer-files)))))))

(defun emacs-fileio--direct-buffer-string (buffer)
  "Return BUFFER contents as a string."
  (cond
   ((and buffer
         (fboundp 'nelisp-ec-with-current-buffer)
         (fboundp 'nelisp-ec-buffer-string))
    (nelisp-ec-with-current-buffer buffer
      (nelisp-ec-buffer-string)))
   ((and buffer (fboundp 'with-current-buffer) (fboundp 'buffer-string))
    (with-current-buffer buffer
      (buffer-string)))
   ((fboundp 'nelisp-ec-buffer-string)
    (nelisp-ec-buffer-string))
   ((fboundp 'buffer-string)
    (buffer-string))
   (t
    (signal 'error '("save-buffer: no buffer string reader available")))))

(defun emacs-fileio--write-file-text-direct (path text)
  "Write TEXT to PATH using the best available runtime primitive."
  (cond
   ((and (fboundp 'nelisp-ec--write-raw-bytes)
         (fboundp 'nelisp-coding-utf8-encode-string))
    (nelisp-ec--write-raw-bytes
     path (nelisp-coding-utf8-encode-string text) nil))
   ((fboundp 'write-region)
    (write-region text nil path nil 'silent))
   (t
    (signal 'error '("save-buffer: no file writer available")))))

(defun emacs-fileio-file-exists-direct-p (path)
  "Return non-nil when PATH exists using the safest available primitive."
  (cond
   ((and (fboundp 'nelisp-ec-file-exists-p)
         (nelisp-ec-file-exists-p path))
    t)
   ((and (fboundp 'file-exists-p)
         (file-exists-p path))
    t)
   (t nil)))

(defun emacs-fileio-read-file-text-direct (path)
  "Return PATH contents as a string for direct frontend file visits."
  (cond
   ((and (fboundp 'nl-syscall-read-file)
         (emacs-fileio-file-exists-direct-p path))
    (nl-syscall-read-file path 0 nil))
   ((and (fboundp 'insert-file-contents)
         (fboundp 'buffer-string)
         (emacs-fileio-file-exists-direct-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))
   ;; `nelisp--syscall-read-file' is intentionally not used here: the
   ;; current standalone implementation can stop evaluation after the call.
   (t "")))

(defun emacs-fileio-buffer-name-for-file (path)
  "Return the buffer name to use for PATH."
  (let ((name (if (fboundp 'file-name-nondirectory)
                  (file-name-nondirectory path)
                path)))
    (if (and (stringp name) (> (length name) 0))
        name
      " *find-file*")))

(defun emacs-fileio-record-buffer-file (buffer path)
  "Record BUFFER as visiting PATH when the core file table is available."
  (when (boundp 'emacs-fileio--buffer-files)
    (setq emacs-fileio--buffer-files
          (cons (cons buffer path)
                (assq-delete-all buffer emacs-fileio--buffer-files))))
  path)

(defun emacs-fileio-buffer-file-direct (&optional buffer)
  "Return BUFFER's visited file from available file tables."
  (let ((buf (or buffer
                 (and (fboundp 'nelisp-ec-current-buffer)
                      (nelisp-ec-current-buffer))
                 (and (fboundp 'current-buffer)
                      (current-buffer)))))
    (cond
     ((and (boundp 'emacs-fileio--buffer-files)
           (cdr (assq buf emacs-fileio--buffer-files))))
     ((and (fboundp 'nelisp-ec-buffer-p)
           (nelisp-ec-buffer-p buf))
      nil)
     (t
      (or (and buf
               (boundp 'buffer-file-name)
               (fboundp 'buffer-local-value)
               (condition-case nil
                   (buffer-local-value 'buffer-file-name buf)
                 (error nil)))
          (and (fboundp 'buffer-file-name)
               (condition-case nil
                   (if buf
                       (with-current-buffer buf
                         (buffer-file-name))
                     (buffer-file-name))
                 (error nil)))
          nil)))))

(defun emacs-fileio--expand-direct-path (path &optional default-directory)
  "Expand PATH for direct file visits using the owned NeLisp substrate.
The unprefixed runtime builtin is only a fallback because some runtime-image
shapes leave leading `~/' paths literal instead of expanding `$HOME'."
  (cond
   ((fboundp 'nelisp-ec-expand-file-name)
    (nelisp-ec-expand-file-name path default-directory))
   ((fboundp 'expand-file-name)
    (expand-file-name path default-directory))
   (t path)))

(defun emacs-fileio--replace-direct-buffer-text (buffer text)
  "Replace BUFFER contents with TEXT through the owned buffer substrate."
  (cond
   ((and (fboundp 'nelisp-text-buffer--standalone-p)
         (nelisp-text-buffer--standalone-p)
         (fboundp 'nelisp-ec--text)
         (fboundp 'nelisp-text-buffer--standalone-replace-logical))
    (let ((chars (length text)))
      (nelisp-text-buffer--standalone-replace-logical
       (nelisp-ec--text buffer) text chars)
      (when (fboundp 'nelisp-ec--set-buffer-point)
        (nelisp-ec--set-buffer-point buffer (1+ chars)))
      (when (fboundp 'nelisp-ec--set-buffer-narrow-start)
        (nelisp-ec--set-buffer-narrow-start buffer nil))
      (when (fboundp 'nelisp-ec--set-buffer-narrow-end)
        (nelisp-ec--set-buffer-narrow-end buffer nil))
      (when (fboundp 'nelisp-ec--bump-buffer-text-tick)
        (nelisp-ec--bump-buffer-text-tick buffer))))
   (t
    (when (fboundp 'nelisp-ec-erase-buffer)
      (nelisp-ec-erase-buffer))
    (when (and (> (length text) 0)
               (fboundp 'nelisp-ec-insert))
      (nelisp-ec-insert text)))))

(defun emacs-fileio-visit-file-direct (path)
  "Visit PATH using direct NeLisp buffers and return the buffer.
This path is intended for frontends that need a small file visit surface
before the full interactive file I/O runtime is available."
  (let* ((abs (emacs-fileio--expand-direct-path path))
         (existing nil))
    (when (boundp 'emacs-fileio--buffer-files)
      (catch 'found
        (dolist (cell emacs-fileio--buffer-files)
          (when (equal abs (cdr cell))
            (setq existing (car cell))
            (throw 'found existing)))))
    (let ((buffer (or existing
                      (and (fboundp 'nelisp-ec-generate-new-buffer)
                           (nelisp-ec-generate-new-buffer
                            (emacs-fileio-buffer-name-for-file abs))))))
      (unless buffer
        (signal 'error (list "cannot create buffer for file" abs)))
      (when (and (not existing)
                 (fboundp 'nelisp-ec-with-current-buffer))
        (nelisp-ec-with-current-buffer buffer
          (let ((text (emacs-fileio-read-file-text-direct abs)))
            (when (stringp text)
              (emacs-fileio--replace-direct-buffer-text buffer text)))
          (cond
           ((fboundp 'nelisp-ec--set-buffer-modified-p)
            (nelisp-ec--set-buffer-modified-p buffer nil))
           ((fboundp 'emacs-buffer-set-buffer-modified-p)
            (emacs-buffer-set-buffer-modified-p nil buffer))
           ((fboundp 'set-buffer-modified-p)
            (set-buffer-modified-p nil)))))
      (emacs-fileio-record-buffer-file buffer abs)
      (when (fboundp 'nelisp-ec-set-buffer)
        (nelisp-ec-set-buffer buffer))
      buffer)))

(defun emacs-fileio-save-buffer-direct (&rest plist)
  "Save a buffer to its visited file and return the path.
PLIST accepts:

- `:buffer': buffer to save, defaulting to the current NeLisp buffer.
- `:file-function': function called with the buffer to return its path.
- `:string-function': function called with the buffer to return contents.
- `:write-function': function called with path and contents."
  (let* ((buffer (or (plist-get plist :buffer)
                     (and (fboundp 'nelisp-ec-current-buffer)
                          (nelisp-ec-current-buffer))
                     (and (fboundp 'current-buffer)
                          (current-buffer))))
         (file-function (or (plist-get plist :file-function)
                            #'emacs-fileio--direct-buffer-file-name))
         (string-function (or (plist-get plist :string-function)
                              #'emacs-fileio--direct-buffer-string))
         (write-function (or (plist-get plist :write-function)
                             #'emacs-fileio--write-file-text-direct))
         (path (and buffer (funcall file-function buffer))))
    (unless path
      (signal 'error '("save-buffer: buffer is not visiting a file")))
    (funcall write-function path (funcall string-function buffer))
    (when (fboundp 'emacs-buffer-set-buffer-modified-p)
      (emacs-buffer-set-buffer-modified-p nil buffer))
    (when (and (not (fboundp 'emacs-buffer-set-buffer-modified-p))
               (fboundp 'set-buffer-modified-p))
      (if (and buffer (fboundp 'with-current-buffer))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
        (set-buffer-modified-p nil)))
    path))

(defun emacs-fileio-run-find-file-command (&rest plist)
  "Run a frontend-provided find-file command.
PLIST accepts `:read-string', `:visit-function', `:direct-visit-p',
`:sync-window', `:after-success', `:cancel-function',
`:missing-function', and `:message-function'."
  (let* ((read-string (plist-get plist :read-string))
         (visit-function (plist-get plist :visit-function))
         (direct-visit-p (or (plist-get plist :direct-visit-p)
                             (lambda ()
                               (and (fboundp 'nl-write-file)
                                    (fboundp 'nelisp-ec-generate-new-buffer)))))
         (sync-window (plist-get plist :sync-window))
         (after-success (plist-get plist :after-success))
         (cancel-function (plist-get plist :cancel-function))
         (missing-function (plist-get plist :missing-function))
         (message-function (plist-get plist :message-function))
         (path (and read-string (funcall read-string "Find file: "))))
    (cond
     ((or (null path) (= (length path) 0))
      (when cancel-function
        (funcall cancel-function))
      nil)
     (t
      (condition-case err
          (let ((buffer
                 (cond
                  (visit-function
                   (funcall visit-function path))
                  ((funcall direct-visit-p)
                   (emacs-fileio-visit-file-direct path))
                  (t
                   (find-file path)))))
            (cond
             (buffer
              (when sync-window
                (funcall sync-window buffer))
              (when after-success
                (funcall after-success buffer path))
              buffer)
             (t
              (when missing-function
                (funcall missing-function path))
              nil)))
        (error
         (when message-function
           (funcall message-function "find-file failed: %S" err))
         nil))))))

(defun emacs-fileio-run-save-buffer-command (&rest plist)
  "Run a frontend-provided save-buffer command.
PLIST accepts `:read-string', `:current-buffer', `:file-function',
`:string-function', `:write-function', `:direct-save-p', and
`:message-function'.  Buffers with no visited file prompt for a path and
delegate to `write-file'."
  (let* ((read-string (plist-get plist :read-string))
         (current-buffer (or (plist-get plist :current-buffer)
                             (lambda ()
                               (or (and (fboundp 'nelisp-ec-current-buffer)
                                        (nelisp-ec-current-buffer))
                                   (and (fboundp 'current-buffer)
                                        (current-buffer))))))
         (file-function (or (plist-get plist :file-function)
                            #'emacs-fileio-buffer-file-direct))
         (string-function (plist-get plist :string-function))
         (write-function (plist-get plist :write-function))
         (direct-save-p (or (plist-get plist :direct-save-p)
                            (lambda ()
                              (and (fboundp 'nl-write-file)
                                   (fboundp 'nelisp-ec-buffer-string)))))
         (message-function (plist-get plist :message-function))
         (buffer (and current-buffer (funcall current-buffer)))
         (file (and buffer (funcall file-function buffer))))
    (cond
     (file
      (condition-case err
          (if (funcall direct-save-p)
              (emacs-fileio-save-buffer-direct
               :buffer buffer
               :file-function file-function
               :string-function
               (or string-function #'emacs-fileio--direct-buffer-string)
               :write-function
               (or write-function #'emacs-fileio--write-file-text-direct))
            (when (fboundp 'save-buffer)
              (save-buffer)))
        (error
         (when message-function
           (funcall message-function "save-buffer failed: %S" err))
         nil)))
     (t
      (let ((path (and read-string (funcall read-string "Write file: "))))
        (when (and path (> (length path) 0)
                   (fboundp 'write-file))
          (condition-case err
              (write-file path)
            (error
             (when message-function
               (funcall message-function "write-file failed: %S" err))
             nil))))))))

(defun emacs-fileio-run-write-file-command (&rest plist)
  "Run a frontend-provided write-file command.
PLIST accepts `:read-string', `:prompt', `:write-file-function',
`:after-success', `:status-function', and `:message-function'.  The
frontend supplies concrete prompt and buffer-context callbacks while this
helper owns empty input handling, write dispatch, success status, and
error reporting."
  (let* ((read-string (plist-get plist :read-string))
         (prompt (or (plist-get plist :prompt) "Write file: "))
         (write-file-function (or (plist-get plist :write-file-function)
                                  #'write-file))
         (after-success (plist-get plist :after-success))
         (status-function (plist-get plist :status-function))
         (message-function (plist-get plist :message-function))
         (path (and read-string (funcall read-string prompt))))
    (cond
     ((or (null path) (= 0 (length path)))
      (when status-function
        (funcall status-function "write-file: empty path"))
      nil)
     (t
      (condition-case err
          (let ((written (funcall write-file-function path)))
            (when after-success
              (funcall after-success written path))
            (when status-function
              (funcall status-function
                       (format "Wrote: %s" (or written path))))
            written)
        (error
         (when message-function
           (funcall message-function
                    "write-file: %s"
                    (cond
                     ((stringp (cadr err)) (cadr err))
                    (t (prin1-to-string err)))))
         nil))))))

(defun emacs-fileio-run-save-buffers-quit-command (&rest plist)
  "Run a frontend save-buffers-then-quit command.
PLIST accepts `:dirty-buffers', `:begin-prompt', `:save-buffer-function',
`:quit-function', and `:status-function'.  DIRTY-BUFFERS may be a list or
a zero-argument function.  BEGIN-PROMPT is called with prompt and confirm
callback when modified file buffers exist."
  (let* ((dirty-source (plist-get plist :dirty-buffers))
         (dirty (cond
                 ((functionp dirty-source) (funcall dirty-source))
                 (t dirty-source)))
         (begin-prompt (plist-get plist :begin-prompt))
         (save-buffer-function (or (plist-get plist :save-buffer-function)
                                   (lambda (buffer)
                                     (with-current-buffer buffer
                                       (save-buffer)))))
         (quit-function (plist-get plist :quit-function))
         (status-function (plist-get plist :status-function)))
    (let* ((set-status
            (lambda (message)
              (when status-function
                (funcall status-function message))
              message))
           (quit
            (lambda ()
              (when quit-function
                (funcall quit-function))))
           (save-all-and-quit
            (lambda ()
              (let ((saved 0)
                    (failed 0))
                (dolist (buffer dirty)
                  (condition-case _err
                      (progn
                        (funcall save-buffer-function buffer)
                        (setq saved (1+ saved)))
                    (error
                     (setq failed (1+ failed)))))
                (funcall quit)
                (funcall
                 set-status
                 (cond
                  ((zerop failed)
                   (format "Saved %d buffer(s) — quit" saved))
                  (t
                   (format "Saved %d, %d failed — quit anyway"
                           saved failed))))))))
      (cond
       ((null dirty)
        (funcall quit)
        (funcall set-status "C-x C-c → quit"))
       (begin-prompt
        (funcall
         begin-prompt
         (format "%d modified buffer(s).  Save? (y/n/c): " (length dirty))
         (lambda (input)
	           (let ((choice (and (stringp input) (> (length input) 0)
	                              (downcase (substring input 0 1)))))
	             (cond
	              ((equal choice "y") (funcall save-all-and-quit))
	              ((equal choice "n")
	               (funcall quit)
	               (funcall set-status "Quit (unsaved)"))
	              (t
	               (funcall set-status "Quit cancelled")))))))
       (t
        (funcall set-status "Quit cancelled"))))))

(when (emacs-fileio-builtins--install-function-p 'find-file-noselect)
  (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
    "Phase D polyfill: return a buffer visiting FILENAME, loading it if needed.
NOWARN / RAWFILE / WILDCARDS are accepted for API parity but ignored
(= no warning system, no raw-byte mode, no glob expansion in MVP)."
    (ignore nowarn rawfile wildcards)
    (emacs-fileio--clean-killed)
    (let* ((abs (nelisp-ec-expand-file-name filename))
           (existing
            (catch 'found
              (dolist (cell emacs-fileio--buffer-files)
                (when (equal abs (cdr cell))
                  (throw 'found (car cell))))
              nil)))
      (cond
       (existing existing)
       (t
        (let* ((bname (nelisp-ec-file-name-nondirectory abs))
               (buf (nelisp-ec-generate-new-buffer
                     (if (and bname (> (length bname) 0)) bname " *find-file*"))))
          (nelisp-ec-with-current-buffer buf
            (when (nelisp-ec-file-exists-p abs)
              (nelisp-ec-insert-file-contents abs))
            (setq emacs-fileio--buffer-files
                  (cons (cons buf abs)
                        (assq-delete-all buf emacs-fileio--buffer-files))))
          buf))))))

(when (emacs-fileio-builtins--install-function-p 'find-file)
  (defun find-file (filename &optional wildcards)
    "Phase D polyfill: visit FILENAME and make it the current buffer."
    (interactive
     (list (if (fboundp 'read-file-name)
               (read-file-name
                "Find file: "
                (and (boundp 'default-directory) default-directory)
                nil nil)
             (if (fboundp 'read-string)
                 (read-string "Find file: ")
               ""))
           nil))
    (ignore wildcards)
    (let ((buf (find-file-noselect filename)))
      (nelisp-ec-set-buffer buf)
      buf)))

(when (emacs-fileio-builtins--install-function-p 'save-buffer)
  (defun save-buffer (&optional arg)
    "Phase D polyfill: write the current buffer to its visited file.
ARG is accepted for API parity but ignored — the host's prefix-arg
disambiguation (= multiple backup levels) has no MVP equivalent.

Clears `(buffer-modified-p)' on success so the GUI mode-line `**'
indicator drops back to `--' after a save."
    (interactive "P")
    (ignore arg)
    (emacs-fileio-save-buffer-direct
     :string-function
     (lambda (_buffer)
       (nelisp-ec-buffer-substring
        (nelisp-ec-point-min)
        (nelisp-ec-point-max))))))

(when (emacs-fileio-builtins--install-function-p 'write-file)
  (defun write-file (filename &optional confirm)
    "Phase D polyfill: write the current buffer to FILENAME and visit it.
CONFIRM is accepted for API parity but ignored (= no interactive
confirmation prompt in MVP).

Clears `(buffer-modified-p)' on success — `set-visited-file-name'
takes the buffer's contents to be in-sync with the new path."
    (interactive
     (list (if (fboundp 'read-file-name)
               (read-file-name
                "Write file: "
                (and (boundp 'default-directory) default-directory)
                nil nil)
             (if (fboundp 'read-string)
                 (read-string "Write file: ")
               ""))
           nil))
    (ignore confirm)
    (let ((abs (nelisp-ec-expand-file-name filename)))
      (write-region (nelisp-ec-point-min) (nelisp-ec-point-max) abs)
      (set-visited-file-name abs)
      (when (fboundp 'set-buffer-modified-p)
        (set-buffer-modified-p nil))
      abs)))

(when (emacs-fileio-builtins--install-function-p 'revert-buffer)
  (defun revert-buffer (&optional ignore-auto noconfirm preserve-modes)
    "Phase D polyfill: reload the visited file into the current buffer.
IGNORE-AUTO / NOCONFIRM / PRESERVE-MODES are accepted for API parity
but ignored (= no auto-save subsystem, no confirm prompt, no major
mode rerun yet)."
    (ignore ignore-auto noconfirm preserve-modes)
    (let* ((b (nelisp-ec-current-buffer))
           (f (and b (buffer-file-name b))))
      (when f
        (nelisp-ec-erase-buffer)
        (nelisp-ec-insert-file-contents f)
        f))))

(when (emacs-fileio-builtins--install-function-p 'copy-file)
  (defun copy-file (file newname &optional ok-if-already-exists
                         _keep-time _preserve-uid-gid _preserve-permissions)
    "Copy FILE to NEWNAME (standalone MVP: byte-content copy).
Signal an error when NEWNAME already exists unless OK-IF-ALREADY-EXISTS is
non-nil.  Modification time, uid/gid, and permission preservation are not
modeled."
    (when (and (not ok-if-already-exists)
               (fboundp 'file-exists-p)
               (file-exists-p newname))
      (error "File already exists: %s" newname))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (write-region (point-min) (point-max) newname))
    nil))

(provide 'emacs-fileio-builtins)

;;; emacs-fileio-builtins.el ends here
