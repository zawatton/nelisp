;;; nelisp-emacs-compat-fileio.el --- File I/O extension for nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 9d.A4 (T78) — extends `nelisp-emacs-compat' (Phase 9a SHIPPED,
;; T39) with the minimal Emacs file I/O surface required by anvil.el +
;; downstream extension packages.  Sister task = T76 (Rust syscall
;; extension) that lands `nelisp_syscall_opendir / readdir / mkdir /
;; unlink / rename / access' as the eventual hard backend.
;;
;; Layer policy: this file follows the same dual-runtime contract as
;; `nelisp-coding' (see src/nelisp-coding.el §file-IO comment, Phase
;; 7.5 plan):
;;
;;   * Today (T76 in flight)     — host Emacs primitives are used as a
;;                                  *simulator* for the Rust syscalls.
;;                                  The wire shape (= argument layout
;;                                  and return value contract) is
;;                                  identical to what the FFI will
;;                                  expose, so Phase 7.5 integration
;;                                  is a one-line swap of each helper.
;;   * Phase 7.5 (T76 SHIPPED)   — `nl-syscall-*' takes over and the
;;                                  host fallback is reduced to a
;;                                  smoke test for the simulator path.
;;
;; Naming: `nelisp-ec-' (NeLisp Emacs Compat) prefix is preserved so
;; that loading this module inside a host Emacs does NOT shadow
;; built-in `insert-file-contents', `write-region', etc.
;;
;; API surface (13 public APIs, +4 file-name string-surgery APIs):
;;
;;   File I/O (nelisp-coding integrated, UTF-8 default)
;;     1.  insert-file-contents      FILE [VISIT BEG END REPLACE]
;;     2.  write-region              START END FILE [APPEND VISIT]
;;
;;   Predicates (stat-backed)
;;     3.  file-exists-p             FILE
;;     4.  file-readable-p           FILE
;;     5.  file-directory-p          FILE
;;     6.  file-attributes           FILE [ID-FORMAT]
;;
;;   Directory operations (opendir / mkdir / rename / unlink)
;;     7.  directory-files           DIR [FULL MATCH NOSORT COUNT]
;;     8.  make-directory            DIR [PARENTS]
;;     9.  delete-file               FILE
;;     10. rename-file               OLD NEW [OK-IF-ALREADY-EXISTS]
;;
;;   Pure string surgery (no host syscall)
;;     11. expand-file-name          NAME [DEFAULT-DIRECTORY]
;;     12. file-name-directory       NAME
;;     13. file-name-nondirectory    NAME
;;     14. file-name-sans-extension  NAME
;;     15. file-name-as-directory    NAME
;;     16. file-name-absolute-p      NAME
;;
;;   PATH walk
;;     17. executable-find           COMMAND [REMOTE]
;;
;; nelisp-coding integration:
;;
;;   `nelisp-ec-insert-file-contents' = read raw bytes via simulator →
;;     `nelisp-coding-utf8-decode' (UTF-8 default, replace strategy) →
;;     insert into current `nelisp-ec' buffer at point.
;;   `nelisp-ec-write-region'         = grab `buffer-substring' from
;;     current `nelisp-ec' buffer → `nelisp-coding-utf8-encode-string'
;;     → write raw bytes via simulator (no-conversion).
;;
;; Non-goals (deferred to later phases, per task spec):
;;   * Windows / macOS specific path normalization (POSIX-only MVP).
;;   * file-notify (= Phase 9d.A4 separate task = T82).
;;   * symlink resolution corner cases (`file-truename', etc).
;;   * VISIT side-effects on buffer-modified-p / buffer-name (Emacs's
;;     visit semantics are tied to the file-visiting buffer machinery,
;;     which is *not* part of `nelisp-ec' buffers in MVP).  We accept
;;     a VISIT argument for shape-compat and silently ignore it.

;;; Code:

(require 'cl-lib)
(require 'nelisp-coding)
(require 'nelisp-emacs-compat)

;;; ──────────────────────────────────────────────────────────────────────
;;; Errors
;;; ──────────────────────────────────────────────────────────────────────

(define-error 'nelisp-ec-file-error
  "NeLisp emacs-compat file I/O error" 'nelisp-ec-error)
(define-error 'nelisp-ec-file-missing
  "File does not exist" 'nelisp-ec-file-error)
(define-error 'nelisp-ec-file-already-exists
  "File already exists" 'nelisp-ec-file-error)
(define-error 'nelisp-ec-syscall-unimplemented
  "Underlying syscall not yet wired (T76 pending)" 'nelisp-ec-file-error)

;;; ──────────────────────────────────────────────────────────────────────
;;; Phase 7.5 FFI declarations (T76 wires these for real)
;;; ──────────────────────────────────────────────────────────────────────
;;
;; Until T76 SHIPPED these names resolve via `fboundp'-guarded lookup
;; and the simulator path runs.  When T76 lands, every helper below
;; flips to call the FFI symbol with no argument-shape change.

(declare-function nl-syscall-opendir   "nelisp-runtime")
(declare-function nl-syscall-readdir   "nelisp-runtime")
(declare-function nl-syscall-closedir  "nelisp-runtime")
(declare-function nl-syscall-mkdir     "nelisp-runtime")
(declare-function nl-syscall-unlink    "nelisp-runtime")
(declare-function nl-syscall-rename    "nelisp-runtime")
(declare-function nl-syscall-access    "nelisp-runtime")
(declare-function nl-syscall-stat-ex   "nelisp-runtime")
(declare-function nl-syscall-read-file "nelisp-runtime")
(declare-function nl-syscall-write-file "nelisp-runtime")

(defun nelisp-ec--syscall-available-p (sym)
  "Return non-nil if the Rust syscall SYM is wired (T76 SHIPPED)."
  (fboundp sym))

;;; ──────────────────────────────────────────────────────────────────────
;;; §1. Pure string-surgery APIs (no host syscall, deterministic)
;;; ──────────────────────────────────────────────────────────────────────

;;;###autoload
(defun nelisp-ec-file-name-absolute-p (name)
  "Return non-nil if NAME starts with `/' (POSIX absolute path).
Tilde-expansion (~user/) is treated as absolute as in Emacs.  This
helper does NOT touch the filesystem."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (and (> (length name) 0)
       (or (eq (aref name 0) ?/)
           (eq (aref name 0) ?~))))

;;;###autoload
(defun nelisp-ec-file-name-directory (name)
  "Return the directory part of NAME, or nil if NAME has no slash.
The trailing slash is preserved (= directory part is itself a
directory name)."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let ((idx (cl-position ?/ name :from-end t)))
    (and idx (substring name 0 (1+ idx)))))

;;;###autoload
(defun nelisp-ec-file-name-nondirectory (name)
  "Return the non-directory part of NAME (= last `/'-delimited component).
Returns NAME itself if there is no slash."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let ((idx (cl-position ?/ name :from-end t)))
    (if idx (substring name (1+ idx)) name)))

;;;###autoload
(defun nelisp-ec-file-name-sans-extension (name)
  "Return NAME with its final extension (last `.' onwards) stripped.
The directory part of NAME is preserved.  A leading `.' on the basename
is treated as a hidden-file marker and NOT stripped (= `.bashrc'
returns `.bashrc').  No extension → NAME returned unchanged."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((dir (nelisp-ec-file-name-directory name))
         (base (nelisp-ec-file-name-nondirectory name))
         (idx (cl-position ?. base :from-end t)))
    (cond
     ;; No `.' in basename, or `.' is the very first character (= hidden file).
     ((or (null idx) (zerop idx)) name)
     (t (concat (or dir "") (substring base 0 idx))))))

;;;###autoload
(defun nelisp-ec-file-name-as-directory (name)
  "Return NAME with a trailing `/' appended (idempotent)."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (if (and (> (length name) 0)
           (eq (aref name (1- (length name))) ?/))
      name
    (concat name "/")))

(defun nelisp-ec--collapse-segments (segments)
  "Collapse `.' / `..' / empty SEGMENTS in a POSIX-style path list.
Returns the simplified list (does NOT touch leading `/')."
  (let ((acc nil))
    (dolist (seg segments)
      (cond
       ((or (string-empty-p seg) (string-equal seg ".")) nil)
       ((string-equal seg "..")
        (when acc (pop acc)))
       (t (push seg acc))))
    (nreverse acc)))

;;;###autoload
(defun nelisp-ec-expand-file-name (name &optional default-dir)
  "Convert NAME to an absolute POSIX path.
If NAME is already absolute, only `.' / `..' / `//' collapsing is
performed.  Otherwise NAME is appended to DEFAULT-DIR (which is
itself made absolute against the host CWD when relative).  When
DEFAULT-DIR is omitted the value of the host `default-directory' is
used.

This helper is *pure NeLisp string surgery* — no host syscall is
invoked beyond reading `default-directory' for the seed CWD."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((dd (or default-dir
                 (and (boundp 'default-directory) default-directory)
                 "/"))
         (dd (if (eq (aref dd 0) ?/) dd (concat "/" dd)))
         (seed (cond
                ;; NAME absolute → seed = NAME, ignore dd
                ((nelisp-ec-file-name-absolute-p name) name)
                (t (concat (nelisp-ec-file-name-as-directory dd) name))))
         ;; Tilde at very start of seed → expand to $HOME (host getenv).
         (seed (cond
                ((and (> (length seed) 0) (eq (aref seed 0) ?~))
                 (let ((home (or (getenv "HOME") "/")))
                   (cond
                    ((or (= (length seed) 1) (eq (aref seed 1) ?/))
                     (concat home (substring seed 1)))
                    (t seed)))) ;; ~user/ unsupported — leave verbatim
                (t seed)))
         (segments (split-string seed "/" t))
         (collapsed (nelisp-ec--collapse-segments segments))
         (joined (mapconcat #'identity collapsed "/")))
    (concat "/" joined)))

;;; ──────────────────────────────────────────────────────────────────────
;;; §2. Stat-backed predicates
;;; ──────────────────────────────────────────────────────────────────────
;;;
;;; T76 will provide `nl-syscall-stat-ex' / `nl-syscall-access' as the
;;; hard backend.  For now we delegate to host Emacs primitives, which
;;; are themselves thin libc wrappers — preserving the wire-shape and
;;; return contract a future swap will require.

;;;###autoload
(defun nelisp-ec-file-exists-p (file)
  "Return non-nil if FILE exists.  Wraps stat(2)."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-stat-ex)
    (let ((rc (nl-syscall-stat-ex file)))
      (and rc (>= (or (plist-get rc :rc) 0) 0))))
   (t (file-exists-p file))))

;;;###autoload
(defun nelisp-ec-file-readable-p (file)
  "Return non-nil if FILE exists and is readable.  Wraps access(F_OK | R_OK)."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-access)
    (zerop (nl-syscall-access file 4))) ;; R_OK = 4
   (t (file-readable-p file))))

;;;###autoload
(defun nelisp-ec-file-directory-p (file)
  "Return non-nil if FILE is a directory.  Wraps stat(2) + S_ISDIR."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-stat-ex)
    (let ((rc (nl-syscall-stat-ex file)))
      (and rc (eq (plist-get rc :type) 'directory))))
   (t (file-directory-p file))))

;;;###autoload
(defun nelisp-ec-file-attributes (file &optional id-format)
  "Return attributes of FILE as a `file-attributes'-shaped list.
ID-FORMAT (`'integer'' / `'string'') controls UID/GID rendering and is
forwarded to the underlying call.  Returns nil if FILE does not exist
(matches Emacs `file-attributes' contract)."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-stat-ex)
    (let ((s (nl-syscall-stat-ex file)))
      (and s (>= (or (plist-get s :rc) 0) 0)
           ;; Format: (TYPE LINKS UID GID ATIME MTIME CTIME SIZE
           ;;          MODES UNUSED INODE DEVICE).  Match Emacs shape.
           (list (cond ((eq (plist-get s :type) 'directory) t)
                       ((eq (plist-get s :type) 'symlink)
                        (plist-get s :link-target))
                       (t nil))
                 (plist-get s :nlink)
                 (plist-get s :uid)
                 (plist-get s :gid)
                 (plist-get s :atime)
                 (plist-get s :mtime)
                 (plist-get s :ctime)
                 (plist-get s :size)
                 (plist-get s :mode-string)
                 nil
                 (plist-get s :inode)
                 (plist-get s :dev)))))
   (t (file-attributes file id-format))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §3. Directory operations
;;; ──────────────────────────────────────────────────────────────────────

;;;###autoload
(defun nelisp-ec-directory-files (dir &optional full match nosort count)
  "Return a list of files in DIR.
FULL non-nil → return absolute paths.
MATCH non-nil → keep only filenames matching this regexp.
NOSORT non-nil → preserve readdir order; otherwise the result is
  sorted lexicographically.
COUNT non-nil → return at most COUNT entries (post-filter, post-sort)."
  (unless (stringp dir)
    (signal 'wrong-type-argument (list 'stringp dir)))
  (let ((entries
         (cond
          ((nelisp-ec--syscall-available-p 'nl-syscall-opendir)
           (let ((dh (nl-syscall-opendir dir))
                 (acc nil))
             (unwind-protect
                 (let (next)
                   (while (setq next (nl-syscall-readdir dh))
                     (push next acc)))
               (nl-syscall-closedir dh))
             (nreverse acc)))
          (t
           ;; Simulator: host directory-files but without sort here so
           ;; the NOSORT semantics flow through one code path.
           (directory-files dir nil nil t)))))
    (when match
      (setq entries (cl-remove-if-not (lambda (n) (string-match-p match n))
                                      entries)))
    (unless nosort
      (setq entries (sort entries #'string-lessp)))
    (when count
      (setq entries (cl-subseq entries 0 (min (length entries) count))))
    (when full
      (setq entries
            (mapcar (lambda (n)
                      (concat (nelisp-ec-file-name-as-directory dir) n))
                    entries)))
    entries))

;;;###autoload
(defun nelisp-ec-make-directory (dir &optional parents)
  "Create directory DIR.  When PARENTS non-nil create intermediate dirs.
Returns DIR.  Signals `nelisp-ec-file-error' on failure."
  (unless (stringp dir)
    (signal 'wrong-type-argument (list 'stringp dir)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-mkdir)
    (let ((rc (nl-syscall-mkdir dir #o755 (if parents 1 0))))
      (when (< rc 0)
        (signal 'nelisp-ec-file-error
                (list "mkdir" dir rc)))
      dir))
   (t
    (condition-case err
        (progn (make-directory dir parents) dir)
      (error (signal 'nelisp-ec-file-error
                     (list "mkdir" dir (error-message-string err))))))))

;;;###autoload
(defun nelisp-ec-delete-file (file)
  "Delete FILE via unlink(2).  Returns t on success."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-unlink)
    (let ((rc (nl-syscall-unlink file)))
      (when (< rc 0)
        (signal 'nelisp-ec-file-error (list "unlink" file rc)))
      t))
   (t
    (condition-case err
        (progn (delete-file file) t)
      (error (signal 'nelisp-ec-file-error
                     (list "unlink" file (error-message-string err))))))))

;;;###autoload
(defun nelisp-ec-rename-file (oldname newname &optional ok-if-already-exists)
  "Rename OLDNAME to NEWNAME.  Returns t on success.
When OK-IF-ALREADY-EXISTS is nil and NEWNAME exists, signals
`nelisp-ec-file-already-exists'."
  (unless (and (stringp oldname) (stringp newname))
    (signal 'wrong-type-argument (list 'stringp oldname newname)))
  (when (and (not ok-if-already-exists)
             (nelisp-ec-file-exists-p newname))
    (signal 'nelisp-ec-file-already-exists (list newname)))
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-rename)
    (let ((rc (nl-syscall-rename oldname newname)))
      (when (< rc 0)
        (signal 'nelisp-ec-file-error
                (list "rename" oldname newname rc)))
      t))
   (t
    (condition-case err
        (progn (rename-file oldname newname (if ok-if-already-exists t nil)) t)
      (error (signal 'nelisp-ec-file-error
                     (list "rename" oldname newname
                           (error-message-string err))))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §4. PATH walk
;;; ──────────────────────────────────────────────────────────────────────

;;;###autoload
(defun nelisp-ec-executable-find (command &optional remote)
  "Return the absolute path of executable COMMAND, or nil if not found.
Walks $PATH, testing each candidate with access(X_OK).  REMOTE is
accepted for shape-compat with Emacs `executable-find' but is
currently a no-op (Phase 9d MVP is local-only)."
  (unless (stringp command)
    (signal 'wrong-type-argument (list 'stringp command)))
  (when remote
    ;; Shape-compat only — TRAMP-style remote PATH probing is deferred.
    (ignore remote))
  ;; Absolute / explicit-relative names skip the PATH walk entirely.
  (cond
   ((or (eq (aref command 0) ?/)
        (and (> (length command) 1)
             (eq (aref command 0) ?.)
             (or (eq (aref command 1) ?/)
                 (eq (aref command 1) ?.))))
    (and (nelisp-ec-file-exists-p command) command))
   (t
    (let* ((path (or (getenv "PATH") ""))
           (dirs (split-string path ":" t))
           (found nil))
      (catch 'done
        (dolist (d dirs)
          (let ((cand (concat (nelisp-ec-file-name-as-directory d) command)))
            (when (cond
                   ((nelisp-ec--syscall-available-p 'nl-syscall-access)
                    (zerop (nl-syscall-access cand 1))) ;; X_OK = 1
                   (t (and (file-exists-p cand)
                           (file-executable-p cand))))
              (setq found cand)
              (throw 'done nil)))))
      found))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §5. File I/O — read / write through nelisp-coding (UTF-8 default)
;;; ──────────────────────────────────────────────────────────────────────
;;;
;;; Both helpers operate on the *current* `nelisp-ec' buffer (= the
;;; one returned by `nelisp-ec-current-buffer').  This matches Emacs
;;; `insert-file-contents' / `write-region' semantics where the
;;; current-buffer is the implicit subject.

(defun nelisp-ec--read-raw-bytes (file &optional beg end)
  "Read FILE between byte offsets BEG (inclusive) and END (exclusive).
Returns a unibyte string of raw bytes.  Phase 7.5 will swap this to
`nl-syscall-read-file' once T76 lands."
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-read-file)
    (nl-syscall-read-file file (or beg 0) end))
   (t
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil beg end)
      (buffer-substring-no-properties (point-min) (point-max))))))

(defun nelisp-ec--write-raw-bytes (file unibyte append)
  "Write UNIBYTE bytes to FILE.  When APPEND non-nil, append.
Phase 7.5 will swap this to `nl-syscall-write-file' once T76 lands."
  (cond
   ((nelisp-ec--syscall-available-p 'nl-syscall-write-file)
    (nl-syscall-write-file file unibyte (if append 1 0)))
   (t
    (let ((coding-system-for-write 'no-conversion)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region unibyte nil file append 'silent)
      (length unibyte)))))

;;;###autoload
(defun nelisp-ec-insert-file-contents (file &optional visit beg end replace)
  "Insert contents of FILE into the current `nelisp-ec' buffer at point.
Decoded under `nelisp-coding-utf8-decode' (= UTF-8 with `replace'
strategy).

VISIT  — accepted for shape-compat with Emacs but ignored in MVP
         (= no buffer-file-name machinery in `nelisp-ec' buffers).
BEG/END — byte offsets into FILE (raw, pre-decode).
REPLACE — when non-nil, erase the visible region before insertion.

Returns the cons (FILE . CHARS-INSERTED), matching Emacs's
`insert-file-contents' return contract (FILE-NAME, BYTES-INSERTED).
We report CHARS rather than BYTES because the codec layer handles
the byte→char conversion; downstream call sites that only care
about the bytes inserted should use file-attributes for the source
file size."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (ignore visit)
  (nelisp-ec--ensure-current)
  (unless (nelisp-ec-file-exists-p file)
    (signal 'nelisp-ec-file-missing (list file)))
  (let* ((raw (nelisp-ec--read-raw-bytes file beg end))
         (plist (nelisp-coding-utf8-decode raw))
         (decoded (plist-get plist :string)))
    (when replace
      (nelisp-ec-erase-buffer))
    (nelisp-ec-insert decoded)
    (cons file (length decoded))))

;;;###autoload
(defun nelisp-ec-write-region (start end file &optional append visit)
  "Write text between START and END of the current buffer to FILE.
The text is encoded under `nelisp-coding-utf8-encode-string' (UTF-8,
`replace' strategy).

START / END — 1-based positions (matches `nelisp-ec' convention).
APPEND      — non-nil → open FILE in append mode.
VISIT       — accepted for shape-compat; ignored in MVP.

Returns the number of *bytes* written to disk."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (ignore visit)
  (let* ((text (nelisp-ec-buffer-substring (min start end) (max start end)))
         (unibyte (nelisp-coding-utf8-encode-string text)))
    (nelisp-ec--write-raw-bytes file unibyte append)
    (length unibyte)))

(provide 'nelisp-emacs-compat-fileio)
;;; nelisp-emacs-compat-fileio.el ends here
