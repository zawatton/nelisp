;;; nelisp-eln-system-loader.el --- system-loader .eln metadata handles -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A narrow metadata backend for GNU .eln files already loaded by the host
;; system dynamic linker.  This does not invoke native functions in an .eln.

;;; Code:

(require 'nelisp-eln-metadata)
(require 'nl-ffi)
(require 'nl-ffi-memory)

(define-error 'nelisp-eln-system-loader-error
  "Invalid system-loaded GNU .eln handle" 'nelisp-eln-metadata-error)

(declare-function ptr-call "ext:nelisp-runtime" (address a b c d e f))

(defconst nelisp-eln-system-loader--magic 'nelisp-eln-system-handle)
(defconst nelisp-eln-system-loader--shn-undef 0)
(defconst nelisp-eln-system-loader--shn-reserved #xff00)
(defconst nelisp-eln-system-loader--sht-strtab 3)
(defconst nelisp-eln-system-loader--sht-dynsym 11)
(defconst nelisp-eln-system-loader--stt-object 1)
(defconst nelisp-eln-system-loader--stt-func 2)
(defconst nelisp-eln-system-loader--pf-r 4)
(defconst nelisp-eln-system-loader--pf-x 1)
(defconst nelisp-eln-system-loader--function-capability-magic
  'nelisp-eln-system-function-capability)

(defvar nelisp-eln-system-loader--handles (make-hash-table :test 'eq))
(defvar nelisp-eln-system-loader--pending-cleanups nil)
(defvar nelisp-eln-system-loader--next-module-id 1)

(defvar nelisp-eln-system-loader-preopen-validator nil
  "Optional function called with a root .eln's raw file bytes, once,
inside `nelisp-eln-system-loader-open' -- after that file's own ELF
symbol/program-header parsing, but strictly before `nl-ffi--dlopen' is
ever called on any copy of it. Must signal to reject; any non-error
return admits.

This exists because `nl-ffi--dlopen' runs `.init'/DT_INIT and
INIT_ARRAY (and, later, `nelisp-eln-system-loader-close' runs `.fini'/
FINI_ARRAY) as an ordinary, automatic part of opening/closing a shared
object -- before or fully independent of any Lisp-level admission
decision this module or its callers make. A validator installed here is
the only place that can still matter for that surface; anything checked
only after this function returns runs too late for it, because the
dynamic loader has already executed it by then.

nelisp-eln-registration.el (`nelisp-eln-registration--validate-preopen')
installs itself here, at `require' time, precisely to avoid this module
requiring that one back (a circular `require': that module already
requires this one for the ordinary metadata/symbol services it uses
everywhere else). This module never calls `require' on it.")

(defvar nelisp-eln-system-loader--private-copy-root nil
  "This process's own 0700 directory for dlopen'd private copies (see
`nelisp-eln-system-loader--write-private-copy'), created lazily once.")

(defun nelisp-eln-system-loader--retain-cleanup (kind resource)
  (push (cons kind resource) nelisp-eln-system-loader--pending-cleanups)
  nil)

(defun nelisp-eln-system-loader--close-resource (handle)
  (condition-case _error
      (if (= (nl-ffi--call-checked
              'nelisp-eln-system-loader--close-resource "dlclose" handle)
             0)
          t
        (nelisp-eln-system-loader--retain-cleanup 'dlclose handle))
    (error (nelisp-eln-system-loader--retain-cleanup 'dlclose handle))))

(defun nelisp-eln-system-loader--release-owner (owner)
  (condition-case _error
      (progn (nl-ffi-memory-release owner) t)
    (error (nelisp-eln-system-loader--retain-cleanup 'memory owner))))

(defun nelisp-eln-system-loader-retry-pending-cleanups ()
  "Retry deferred dlclose and mmap cleanup; return the remaining count."
  (let ((pending (nreverse nelisp-eln-system-loader--pending-cleanups))
        (remaining nil))
    (setq nelisp-eln-system-loader--pending-cleanups nil)
    (dolist (entry pending)
      (let ((ok
             (condition-case _error
                 (pcase (car entry)
                   ('dlclose
                    (= (nl-ffi--call-checked
                        'nelisp-eln-system-loader-retry-pending-cleanups
                        "dlclose" (cdr entry)) 0))
                   ('memory
                    (nl-ffi-memory-release (cdr entry))
                    t)
                   (_ nil))
               (error nil))))
        (unless ok (push entry remaining))))
    (setq nelisp-eln-system-loader--pending-cleanups (nreverse remaining))
    (length remaining)))

(defun nelisp-eln-system-loader--fail (reason &optional detail)
  (signal 'nelisp-eln-system-loader-error (list reason detail)))

(defun nelisp-eln-system-loader--file-range-p (size offset length)
  (and (integerp offset) (>= offset 0)
       (integerp length) (>= length 0)
       (<= offset size) (<= length (- size offset))))

(defun nelisp-eln-system-loader--file-u (bytes offset width)
  (unless (nelisp-eln-system-loader--file-range-p
           (length bytes) offset width)
    (nelisp-eln-system-loader--fail 'elf-read-out-of-bounds
                                     (list offset width (length bytes))))
  (let ((i 0) (value 0))
    (while (< i width)
      (setq value (+ value (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    value))

(defun nelisp-eln-system-loader--file-string (bytes offset limit)
  (unless (nelisp-eln-system-loader--file-range-p
           (length bytes) offset limit)
    (nelisp-eln-system-loader--fail 'invalid-string-range
                                     (list offset limit (length bytes))))
  (let ((i 0) (chars nil) (done nil))
    (while (and (< i limit) (not done))
      (let ((byte (aref bytes (+ offset i))))
        (if (= byte 0) (setq done t) (push byte chars)))
      (setq i (1+ i)))
    (unless done
      (nelisp-eln-system-loader--fail 'unterminated-elf-string
                                       (list offset limit)))
    (apply #'unibyte-string (nreverse chars))))

(defun nelisp-eln-system-loader--read-file (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun nelisp-eln-system-loader--file-symbols (path bytes)
  "Read PATH's root dynsym and PT_LOAD metadata from immutable BYTES."
  (let* ((size (length bytes)) (loads nil) (symtab nil) (strtab nil)
         (sym-count nil) (symbols (make-hash-table :test 'equal))
         (phoff (nelisp-eln-system-loader--file-u bytes 32 8))
         (phentsize (nelisp-eln-system-loader--file-u bytes 54 2))
         (phnum (nelisp-eln-system-loader--file-u bytes 56 2))
         (shoff (nelisp-eln-system-loader--file-u bytes 40 8))
         (shentsize (nelisp-eln-system-loader--file-u bytes 58 2))
         (shnum (nelisp-eln-system-loader--file-u bytes 60 2)))
    (unless (and (>= size 64)
                 (= (aref bytes 0) #x7f) (= (aref bytes 1) #x45)
                 (= (aref bytes 2) #x4c) (= (aref bytes 3) #x46)
                 (= (aref bytes 4) 2) (= (aref bytes 5) 1)
                 (= (nelisp-eln-system-loader--file-u bytes 16 2) 3)
                 (= (nelisp-eln-system-loader--file-u bytes 18 2) 62)
                 (= phentsize 56)
                 (nelisp-eln-system-loader--file-range-p
                  size phoff (* phentsize phnum))
                 (= shentsize 64) (> shnum 0)
                 (nelisp-eln-system-loader--file-range-p
                  size shoff (* shentsize shnum)))
      (nelisp-eln-system-loader--fail 'invalid-elf-profile path))
    (let ((i 0))
      (while (< i phnum)
        (let* ((ph (+ phoff (* i phentsize)))
               (type (nelisp-eln-system-loader--file-u bytes ph 4))
               (flags (nelisp-eln-system-loader--file-u bytes (+ ph 4) 4))
               (offset (nelisp-eln-system-loader--file-u bytes (+ ph 8) 8))
               (vaddr (nelisp-eln-system-loader--file-u bytes (+ ph 16) 8))
               (file-size (nelisp-eln-system-loader--file-u bytes (+ ph 32) 8))
               (mem-size (nelisp-eln-system-loader--file-u bytes (+ ph 40) 8)))
          (when (= type 1)
            (unless (nelisp-eln-system-loader--file-range-p
                     size offset file-size)
              (nelisp-eln-system-loader--fail 'invalid-load-segment i))
            (push (list vaddr offset file-size mem-size flags) loads)))
        (setq i (1+ i))))
    (setq loads (nreverse loads))
    (let ((i 0))
      (while (< i shnum)
        (let* ((section (+ shoff (* i shentsize)))
               (type (nelisp-eln-system-loader--file-u bytes (+ section 4) 4))
               (offset (nelisp-eln-system-loader--file-u bytes (+ section 24) 8))
               (section-size (nelisp-eln-system-loader--file-u bytes (+ section 32) 8))
               (link (nelisp-eln-system-loader--file-u bytes (+ section 40) 4))
               (entry-size (nelisp-eln-system-loader--file-u bytes (+ section 56) 8)))
          (when (= type nelisp-eln-system-loader--sht-dynsym)
            (when symtab
              (nelisp-eln-system-loader--fail 'duplicate-dynsym))
            (unless (and (= entry-size 24) (= (% section-size 24) 0)
                         (nelisp-eln-system-loader--file-range-p
                          size offset section-size)
                         (< link shnum))
              (nelisp-eln-system-loader--fail 'invalid-dynsym
                                               (list i offset section-size link)))
            (let* ((str-section (+ shoff (* link shentsize)))
                   (str-type (nelisp-eln-system-loader--file-u
                              bytes (+ str-section 4) 4))
                   (str-offset (nelisp-eln-system-loader--file-u
                                bytes (+ str-section 24) 8))
                   (str-size (nelisp-eln-system-loader--file-u
                              bytes (+ str-section 32) 8)))
              (unless (and (= str-type nelisp-eln-system-loader--sht-strtab)
                           (nelisp-eln-system-loader--file-range-p
                            size str-offset str-size))
                (nelisp-eln-system-loader--fail 'invalid-dynstr
                                                 (list link str-offset str-size)))
              (setq symtab offset strtab (cons str-offset str-size)
                    sym-count (/ section-size 24))))
        (setq i (1+ i))))
    (unless (and symtab strtab sym-count loads)
      (nelisp-eln-system-loader--fail 'missing-root-elf-tables path))
    (let ((i 0))
      (while (< i sym-count)
        (let* ((sym (+ symtab (* i 24)))
               (name-off (nelisp-eln-system-loader--file-u bytes sym 4))
               (info (nelisp-eln-system-loader--file-u bytes (+ sym 4) 1))
               (shndx (nelisp-eln-system-loader--file-u bytes (+ sym 6) 2))
               (value (nelisp-eln-system-loader--file-u bytes (+ sym 8) 8))
               (object-size (nelisp-eln-system-loader--file-u bytes (+ sym 16) 8)))
          (when (and (/= shndx nelisp-eln-system-loader--shn-undef)
                     (< shndx nelisp-eln-system-loader--shn-reserved)
                     (< name-off (cdr strtab)))
            (let* ((name (nelisp-eln-system-loader--file-string
                          bytes (+ (car strtab) name-off) (- (cdr strtab) name-off)))
                   (entry (list :value value :size object-size
                                :type (logand info #xf) :binding (ash info -4)
                                :section-index shndx :name name)))
              (when (> (length name) 0)
                (if (gethash name symbols)
                    (nelisp-eln-system-loader--fail 'duplicate-root-symbol name)
                  (puthash name entry symbols)))))
        (setq i (1+ i)))))
    (list :path path :loads loads :symbols symbols))))

(defun nelisp-eln-system-loader--load-bias (dl-handle path elf)
  "Verify a root-owned anchor through dladdr and return its load bias."
  (let ((anchor (gethash "top_level_run" (plist-get elf :symbols))))
    (unless (and anchor
                 (= (plist-get anchor :type)
                    nelisp-eln-system-loader--stt-func))
      (nelisp-eln-system-loader--fail 'invalid-root-anchor anchor))
    (let ((anchor-address (nl-ffi--dlsym dl-handle "top_level_run")))
      (unless (and (integerp anchor-address) (> anchor-address 0))
        (nelisp-eln-system-loader--fail 'missing-root-anchor anchor))
      (let ((libc-handle (nl-ffi--dlopen "libc.so.6")))
        (unwind-protect
            (let* ((dladdr-address (nl-ffi--dlsym libc-handle "dladdr"))
                   (info-owner (nl-ffi-memory-allocate 32)))
              (unwind-protect
                  (let ((info (nl-ffi-memory-address info-owner)))
                    (unless (and (integerp dladdr-address) (> dladdr-address 0))
                      (nelisp-eln-system-loader--fail 'missing-dladdr))
                    (dotimes (i 4) (ptr-write-u64 info (* i 8) 0))
                    (unless (/= 0 (ptr-call dladdr-address anchor-address info 0 0 0 0))
                      (nelisp-eln-system-loader--fail 'dladdr-failed anchor-address))
                    (let* ((owner-pointer (ptr-read-u64 info 0))
                           (base (ptr-read-u64 info 8))
                           (owner (and (> owner-pointer 0)
                                       (nelisp-eln-system-loader--read-c-string
                                        owner-pointer)))
                           (expected (+ base (plist-get anchor :value))))
                      (unless (and (stringp owner)
                                   (equal (file-truename owner) path)
                                   (> base 0) (= anchor-address expected))
                        (nelisp-eln-system-loader--fail
                         'anchor-owner-or-bias-mismatch
                         (list owner path base anchor-address expected)))
                      base))
                (nelisp-eln-system-loader--release-owner info-owner)))
          (nelisp-eln-system-loader--close-resource libc-handle))))))

(defun nelisp-eln-system-loader--private-copy-dir ()
  "Return this process's private 0700 directory for dlopen'd copies,
creating it (mode 0700, refusing to reuse an existing path with any
other mode or owner) the first time it is needed. Never under /tmp:
this repository's own convention -- ~/.cache/tmp -- applies to a
long-lived runtime directory exactly as it does to any other generated
artifact."
  (or nelisp-eln-system-loader--private-copy-root
      (let* ((root (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name ".cache" (or (getenv "HOME") "~"))))
             (dir (expand-file-name
                   (format "tmp/nelisp-eln-private-%d" (emacs-pid)) root)))
        (if (file-exists-p dir)
            (let ((attrs (file-attributes dir)))
              (unless (and attrs (eq (file-attribute-type attrs) t)
                           (= (file-attribute-user-id attrs) (user-uid))
                           (= (logand (file-modes dir) #o777) #o700))
                (nelisp-eln-system-loader--fail
                 'private-copy-dir-unsafe dir)))
          (make-directory dir t)
          (set-file-modes dir #o700))
        (setq nelisp-eln-system-loader--private-copy-root dir)
        dir)))

(defun nelisp-eln-system-loader--write-private-copy (bytes)
  "Write the exact BYTES already read and validated to a fresh,
process-private file and return its path.  `nl-ffi--dlopen' is called
on this copy, never on the caller's own PATH: once BYTES are captured
here, nothing further reads PATH again before mapping, so there is no
window left for it to be swapped out from under this open -- unlike
comparing PATH's identity or hash before and after dlopen, which can
only detect such a swap after `dlopen' has already mapped (and, for
`.init'/INIT_ARRAY, already executed) whatever was at PATH by then."
  (let* ((dir (nelisp-eln-system-loader--private-copy-dir))
         (path (make-temp-file (expand-file-name "copy-" dir) nil ".eln")))
    (set-file-modes path #o600)
    (let ((coding-system-for-write 'no-conversion)
          (write-region-inhibit-fsync nil))
      (write-region bytes nil path nil 'silent))
    path))

(defun nelisp-eln-system-loader-open (path)
  "Open PATH with the system linker and return a metadata-only handle."
  (let* ((canonical (file-truename path))
         (bytes (nelisp-eln-system-loader--read-file canonical))
         (before (nelisp-eln-system-loader--file-sha256 bytes))
         (elf (nelisp-eln-system-loader--file-symbols canonical bytes))
         (anchor (gethash "top_level_run" (plist-get elf :symbols))))
    (unless (and anchor
                 (= (plist-get anchor :type) nelisp-eln-system-loader--stt-func))
      (nelisp-eln-system-loader--fail 'invalid-root-anchor anchor))
    ;; Authenticate every executable byte `nl-ffi--dlopen'/a later dlclose
    ;; can run automatically (`.init', INIT_ARRAY, `.fini', FINI_ARRAY,
    ;; CRT stubs, DT_RPATH/RUNPATH/NEEDED/TEXTREL) from BYTES itself,
    ;; before dlopen ever runs -- see
    ;; `nelisp-eln-system-loader-preopen-validator''s docstring for why
    ;; this must happen here and cannot happen any later.
    (when (functionp nelisp-eln-system-loader-preopen-validator)
      (funcall nelisp-eln-system-loader-preopen-validator bytes))
    ;; Never dlopen PATH itself: dlopen a fresh private copy of the exact
    ;; BYTES just validated instead, so there is no window left in which
    ;; PATH could be swapped for something else between validation and
    ;; execution (see `nelisp-eln-system-loader--write-private-copy').
    (let ((private-path (file-truename
                         (nelisp-eln-system-loader--write-private-copy bytes))))
      (unwind-protect
          (let ((dl-handle (nl-ffi--dlopen private-path)) (registered nil))
            (unwind-protect
                (let* (
                       (bias (nelisp-eln-system-loader--load-bias
                              dl-handle private-path elf))
                       (after-bytes (nelisp-eln-system-loader--read-file canonical))
                       (after (nelisp-eln-system-loader--file-sha256 after-bytes))
                       ;; Captured once, alongside the before/after re-hash race
                       ;; check above, so the per-call fast path in
                       ;; `nelisp-eln-system-loader--ensure-file-unchanged' has a
                       ;; baseline identity to compare against from the very first
                       ;; post-open validation.
                       (identity (nelisp-eln-system-loader--file-identity canonical))
                       (module-id nelisp-eln-system-loader--next-module-id)
                       (handle (list :backend nelisp-eln-system-loader--magic
                                     :path canonical))
                       (state (list :path canonical :dl-handle dl-handle :bias bias
                                    :elf elf :file-bytes bytes :file-sha before
                                    :identity identity
                                    :module-id module-id
                                    :function-tokens (make-hash-table :test 'equal)
                                    :state 'live)))
                  (unless (and (integerp module-id) (> module-id 0)
                               (<= module-id most-positive-fixnum))
                    (nelisp-eln-system-loader--fail 'module-id-exhausted))
                  ;; PATH is a secondary integrity signal now (execution
                  ;; safety no longer depends on it: the private copy
                  ;; above is what was actually dlopen'd) -- still worth
                  ;; keeping, to detect a concurrently misbehaving source.
                  (unless (and (equal before after) (equal bytes after-bytes))
                    (nelisp-eln-system-loader--fail 'file-changed-during-open
                                                    canonical))
                  (setq nelisp-eln-system-loader--next-module-id (1+ module-id))
                  (puthash handle state nelisp-eln-system-loader--handles)
                  (setq registered t)
                  handle)
              (unless registered
                (nelisp-eln-system-loader--close-resource dl-handle))))
        ;; The mapping (and, per POSIX, any already-completed access to
        ;; it) survives an unlink of its own path; nothing further ever
        ;; needs this file to exist on disk once dlopen has returned.
        (ignore-errors (delete-file private-path))))))

(defun nelisp-eln-system-loader-module-id (handle)
  "Return HANDLE's non-reused exact integer identity for native leases."
  (plist-get (nelisp-eln-system-loader--state handle) :module-id))

(defun nelisp-eln-system-loader--state (handle &optional allow-closing)
  (let ((state (gethash handle nelisp-eln-system-loader--handles)))
    (unless (and state
                 (or (eq (plist-get state :state) 'live)
                     (and allow-closing
                          (eq (plist-get state :state) 'closing))))
      (nelisp-eln-system-loader--fail 'stale-handle))
    state))

(defun nelisp-eln-system-loader-close (handle)
  "Close HANDLE; failed closes remain quarantined and retryable."
  (let ((state (nelisp-eln-system-loader--state handle t)))
    (when (and (fboundp 'nelisp--native-subr-live-count)
               (> (nelisp--native-subr-live-count
                   (plist-get state :module-id)) 0))
      (nelisp-eln-system-loader--fail
       'module-has-live-native-subrs (plist-get state :module-id)))
    (setq state (plist-put state :state 'closing))
    (puthash handle state nelisp-eln-system-loader--handles)
    (let ((result (nl-ffi--call-checked
                   'nelisp-eln-system-loader-close "dlclose"
                   (plist-get state :dl-handle))))
      (unless (and (integerp result) (= result 0))
        (nelisp-eln-system-loader--fail 'dlclose-failed result))
      (remhash handle nelisp-eln-system-loader--handles)
      t)))

(defun nelisp-eln-system-loader-symbol-info (handle name)
  "Return root-owned symbol metadata for NAME in a live HANDLE."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (elf (plist-get state :elf))
         (entry (gethash name (plist-get elf :symbols))))
    (when entry
      (let* ((value (plist-get entry :value))
             (size (plist-get entry :size))
             (loads (plist-get elf :loads))
             (load (catch 'found
                     (while loads
                       (let ((row (car loads)))
                         (when (and (/= 0 (logand (nth 4 row)
                                                  nelisp-eln-system-loader--pf-r))
                                    (>= value (nth 0 row))
                                    (<= (- value (nth 0 row)) (nth 3 row))
                                    (<= size (- (nth 3 row)
                                                (- value (nth 0 row)))))
                           (throw 'found row)))
                       (setq loads (cdr loads)))
                     nil)))
        (unless (= (plist-get entry :type) nelisp-eln-system-loader--stt-object)
          (nelisp-eln-system-loader--fail 'not-root-object name))
        (unless load
          (nelisp-eln-system-loader--fail 'object-outside-root-load name))
        (list :address (+ (plist-get state :bias) value)
              :binding (plist-get entry :binding)
              :type nelisp-eln-system-loader--stt-object
              :source-path (plist-get state :path) :size size
              :section-index (plist-get entry :section-index)
              :file-offset (+ (nth 1 load) (- value (nth 0 load)))
              :file-backed-size (- (nth 2 load) (- value (nth 0 load))))))))

(defun nelisp-eln-system-loader-function-capability (handle name)
  "Return a checked capability for root function NAME in live HANDLE.
The returned object binds the root symbol address to HANDLE identity and
the module's validated file digest.  It does not invoke the function."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (elf (plist-get state :elf))
         (entry (gethash name (plist-get elf :symbols))))
    (unless entry
      (nelisp-eln-system-loader--fail 'root-function-not-found name))
    (unless (= (plist-get entry :type) nelisp-eln-system-loader--stt-func)
      (nelisp-eln-system-loader--fail 'not-root-function name))
    (let* ((value (plist-get entry :value))
           (size (plist-get entry :size))
           (loads (plist-get elf :loads))
           (load (catch 'found
                   (while loads
                     (let ((row (car loads)))
                       (when (and (/= 0 (logand (nth 4 row)
                                                nelisp-eln-system-loader--pf-x))
                                  (>= value (nth 0 row))
                                  (< (- value (nth 0 row)) (nth 2 row))
                                  (> size 0)
                                  (<= size (- (nth 2 row)
                                              (- value (nth 0 row))))
                                  (<= size (- (nth 3 row)
                                              (- value (nth 0 row)))))
                         (throw 'found row)))
                     (setq loads (cdr loads)))
                   nil)))
      (unless load
        (nelisp-eln-system-loader--fail 'function-outside-root-executable-load
                                        name))
      (nelisp-eln-system-loader--ensure-file-unchanged handle name)
      (let* ((address (+ (plist-get state :bias) value))
             (resolved (nl-ffi--dlsym (plist-get state :dl-handle) name)))
        (unless (and (integerp resolved) (> resolved 0) (= resolved address))
          (nelisp-eln-system-loader--fail
           'root-function-address-mismatch (list name address resolved)))
        (let ((token (or (gethash name (plist-get state :function-tokens))
                         (let ((new-token (make-symbol
                                           "eln-root-function-capability")))
                           (puthash name new-token
                                    (plist-get state :function-tokens))
                           new-token))))
          (list nelisp-eln-system-loader--function-capability-magic
                handle name address (plist-get state :file-sha)
                (plist-get entry :binding) size token))))))

(defun nelisp-eln-system-loader--function-capability-shape-p (value)
  (let ((cursor value) (count 0) (proper t))
    (while (and proper (< count 8))
      (if (consp cursor)
          (setq cursor (cdr cursor) count (+ count 1))
        (setq proper nil)))
    (and proper (= count 8) (null cursor))))

(defun nelisp-eln-system-loader-validate-function-capability (capability)
  "Revalidate CAPABILITY against its exact live root module and symbol."
  (unless (and (nelisp-eln-system-loader--function-capability-shape-p
                capability)
               (eq (car capability)
                   nelisp-eln-system-loader--function-capability-magic))
    (nelisp-eln-system-loader--fail 'invalid-function-capability))
  (let* ((handle (nth 1 capability))
         (name (nth 2 capability))
         (address (nth 3 capability))
         (digest (nth 4 capability))
         (token (nth 7 capability))
         (state (nelisp-eln-system-loader--state handle))
         (fresh (nelisp-eln-system-loader-function-capability handle name)))
    (unless (and (integerp address)
                 (= address (nth 3 fresh))
                 (equal digest (nth 4 fresh))
                 (equal digest (plist-get state :file-sha))
                 (eq token (nth 7 fresh)))
      (nelisp-eln-system-loader--fail 'function-capability-mismatch name))
    fresh))

(defun nelisp-eln-system-loader-read-root-function-bytes
    (handle name offset length)
  "Read bounded executable bytes for root function NAME in HANDLE."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (cap (nelisp-eln-system-loader-function-capability handle name))
         (entry (gethash name (plist-get (plist-get state :elf) :symbols)))
         (size (plist-get entry :size))
         (value (plist-get entry :value))
         (load (catch 'found
                 (dolist (row (plist-get (plist-get state :elf) :loads))
                   (when (and (/= 0 (logand (nth 4 row)
                                            nelisp-eln-system-loader--pf-x))
                              (>= value (nth 0 row))
                              (< (- value (nth 0 row)) (nth 2 row))
                              (<= size (- (nth 2 row) (- value (nth 0 row)))))
                     (throw 'found row)))
                 nil)))
    (unless (and cap load
                 (integerp offset) (>= offset 0)
                 (integerp length) (>= length 0)
                 (<= offset size) (<= length (- size offset))
                 (<= offset (- (nth 2 load) (- value (nth 0 load))))
                 (<= length (- (- (nth 2 load) (- value (nth 0 load))) offset)))
      (nelisp-eln-system-loader--fail
       'function-read-out-of-bounds (list name offset length size)))
    (nelisp-eln-system-loader--ensure-file-unchanged handle name)
    (let* ((file-offset (+ (nth 1 load) (- value (nth 0 load)) offset))
           (address (+ (nth 3 cap) offset))
           (memory (ptr-read-bytes address length))
           (disk (substring (plist-get state :file-bytes)
                            file-offset (+ file-offset length))))
      (unless (equal memory disk)
        (nelisp-eln-system-loader--fail 'loaded-code-does-not-match-file name))
      memory)))

(defun nelisp-eln-system-loader-read-root-object-bytes
    (handle name offset length)
  "Read bounded bytes from a root-defined, file-backed STT_OBJECT."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (info (nelisp-eln-system-loader-symbol-info handle name))
         (size (and info (plist-get info :size)))
         (file-backed-size (and info (plist-get info :file-backed-size))))
    (unless info
      (nelisp-eln-system-loader--fail 'symbol-not-found name))
    (unless (and (integerp offset) (>= offset 0)
                 (integerp length) (>= length 0)
                 (<= offset size) (<= length (- size offset))
                 (<= offset file-backed-size)
                 (<= length (- file-backed-size offset)))
      (nelisp-eln-system-loader--fail
       'object-read-out-of-bounds (list name offset length size file-backed-size)))
    (nelisp-eln-system-loader--ensure-file-unchanged handle name)
    (let* ((address (+ (plist-get info :address) offset))
           (memory (ptr-read-bytes address length))
           (disk (substring (plist-get state :file-bytes)
                            (+ (plist-get info :file-offset) offset)
                            (+ (plist-get info :file-offset) offset length))))
      (unless (equal memory disk)
        (nelisp-eln-system-loader--fail 'loaded-bytes-do-not-match-file name))
      memory)))

(defun nelisp-eln-system-loader-validate-root-indirection
    (handle address expected-symbol)
  "Validate an eight-byte readable root slot at ADDRESS.
EXPECTED-SYMBOL must name its exported root-owned object target.  Return that
object's authenticated loaded address after the slot matches it.  ADDRESS is
never dereferenced until its full range and the root file digest are verified."
  (unless (and (integerp address) (> address 0)
               (<= (+ address 8) (ash 1 64))
               (stringp expected-symbol) (> (length expected-symbol) 0))
    (nelisp-eln-system-loader--fail
     'invalid-root-indirection (list address expected-symbol)))
  (let* ((state (nelisp-eln-system-loader--state handle))
         (info (nelisp-eln-system-loader-symbol-info handle expected-symbol)))
    (unless info
      (nelisp-eln-system-loader--fail 'symbol-not-found expected-symbol))
    (unless (and (= (plist-get info :type)
                    nelisp-eln-system-loader--stt-object)
                 (memq (plist-get info :binding) '(1 2))
                 (> (plist-get info :size) 0))
      (nelisp-eln-system-loader--fail
       'not-root-exported-readable-object expected-symbol))
    (let* ((relative (- address (plist-get state :bias)))
           (loads (plist-get (plist-get state :elf) :loads))
           (slot (catch 'found
                   (while loads
                     (let* ((row (car loads))
                            (delta (- relative (nth 0 row))))
                       (when (and (>= delta 0)
                                  (/= 0 (logand (nth 4 row)
                                                nelisp-eln-system-loader--pf-r))
                                  (<= delta (nth 2 row))
                                  (<= 8 (- (nth 2 row) delta)))
                         (throw 'found (cons row delta))))
                     (setq loads (cdr loads)))
                   nil)))
      (unless slot
        (nelisp-eln-system-loader--fail
         'indirection-outside-root-readable-file-load
         (list expected-symbol address)))
      (let* ((row (car slot))
             (delta (cdr slot))
             (file-offset (+ (nth 1 row) delta))
             (file-bytes (plist-get state :file-bytes))
             (expected-address (plist-get info :address)))
        (unless (nelisp-eln-system-loader--file-range-p
                 (length file-bytes) file-offset 8)
          (nelisp-eln-system-loader--fail
           'indirection-outside-root-file (list expected-symbol address)))
        (nelisp-eln-system-loader--ensure-file-unchanged handle expected-symbol)
        (let ((actual-address (ptr-read-u64 address 0)))
          (unless (and (integerp actual-address)
                       (= actual-address expected-address))
            (nelisp-eln-system-loader--fail
             'root-indirection-target-mismatch
             (list expected-symbol actual-address expected-address)))
          expected-address)))))

(defun nelisp-eln-system-loader--file-sha256 (bytes)
  (secure-hash 'sha256 bytes))

;; Per-call capability revalidation used to re-read and re-hash the whole
;; root .eln file on every call (see the four call sites below that compare
;; `:file-sha' against a freshly computed `--file-sha256'/`--read-file'
;; pair).  Instrumented against a real GNU .eln, that cost ~380ms of a
;; ~440ms call, dominated by `secure-hash' (~275ms) plus the file read
;; (~105ms) -- the whole artifact was re-read and re-hashed on every call.
;; Doc 206 requires per-use liveness revalidation of the address/token/
;; handle triple (cheap; kept exactly as-is below) but does not mandate
;; re-hashing the file's bytes on every use; Doc 206 also notes these
;; descriptors "do not pin executable lifetime", i.e. the documented
;; concern is a handle outliving its file, not the file mutating silently
;; under a still-live handle.
;;
;; THREAT MODEL for the cheap path below: a fresh `stat(2)'-derived
;; identity record (device, inode, size, mtime, ctime, at the finest
;; resolution the running stat primitive exposes) is compared against the
;; one captured at open time (or refreshed by the last full re-hash).  A
;; match skips the full read+hash.  This is safe because the only way to
;; rewrite a file's bytes in place while preserving its device, inode,
;; size, mtime AND ctime all at once is to hold privileges (raw block-
;; device access, a debugger poking the exact page-cache pages, direct
;; filesystem-image surgery, or clock manipulation to fake mtime/ctime)
;; that already defeat this module's model: an attacker able to do that
;; can equally patch the already-loaded code in memory or the running
;; process directly, which no amount of re-hashing this Lisp-level
;; metadata backend would detect either.  An ordinary atomic replace (the
;; "write a new file, then rename it over the old name" pattern every
;; sane build/deploy tool uses, including this repo's own AOT pipeline)
;; allocates a new inode, so it is caught by the inode comparison alone,
;; with no privilege required to catch it.  Any identity mismatch, or any
;; missing identity field, falls back unconditionally to the pre-existing
;; full read+hash below, so this caching layer can only make detection
;; MORE eager (it also reacts to metadata-only changes that leave the
;; digest untouched, e.g. a bare `touch'), never less eager than before.
(defun nelisp-eln-system-loader--file-identity (path)
  "Return a comparable file-identity record for PATH, or nil.
The record captures device, inode, size, mtime and ctime at the finest
resolution the running NeLisp stat primitive exposes.  On the
standalone runtime this reads the raw `struct stat' fields directly
through `nelisp--syscall-stat-field' rather than going through this
runtime's own `file-attributes' fallback (see
scripts/nelisp-stdlib-prelude.el): that fallback hard-codes ctime to a
constant and truncates mtime to whole seconds, while reading the raw
fields recovers real nanosecond-resolution mtime/ctime, which this
cache's safety depends on.  On host Emacs (used by this file's own ERT
suite, where `nelisp--syscall-stat-field' is not `fboundp') the native
`file-attributes' already reports full-resolution Lisp time values, so
it is used directly.  Returns nil -- meaning \"identity unavailable,
always take the full path\" -- when PATH does not exist or any field
could not be read as a plausible non-negative value."
  (if (fboundp 'nelisp--syscall-stat-field)
      (let ((device (nelisp--syscall-stat-field path 0))
            (inode (nelisp--syscall-stat-field path 8))
            (size (nelisp--syscall-stat-field path 48))
            (mtime-sec (nelisp--syscall-stat-field path 88))
            (mtime-nsec (nelisp--syscall-stat-field path 96))
            (ctime-sec (nelisp--syscall-stat-field path 104))
            (ctime-nsec (nelisp--syscall-stat-field path 112)))
        (when (and (integerp device) (>= device 0)
                   (integerp inode) (>= inode 0)
                   (integerp size) (>= size 0)
                   (integerp mtime-sec) (>= mtime-sec 0)
                   (integerp mtime-nsec) (>= mtime-nsec 0)
                   (integerp ctime-sec) (>= ctime-sec 0)
                   (integerp ctime-nsec) (>= ctime-nsec 0))
          (list device inode size mtime-sec mtime-nsec ctime-sec ctime-nsec)))
    (let ((attrs (ignore-errors (file-attributes path))))
      (when attrs
        (let ((device (file-attribute-device-number attrs))
              (inode (file-attribute-inode-number attrs))
              (size (file-attribute-size attrs))
              (mtime (file-attribute-modification-time attrs))
              (ctime (file-attribute-status-change-time attrs)))
          (when (and (integerp device) (integerp inode) (integerp size)
                     mtime ctime)
            (list device inode size mtime ctime)))))))

(defun nelisp-eln-system-loader--ensure-file-unchanged (handle detail)
  "Revalidate that HANDLE's root .eln file has not changed on disk.
Signal `nelisp-eln-system-loader-error' with `root-file-changed' and
DETAIL when it has.  See `nelisp-eln-system-loader--file-identity' for
the cheap stat-only fast path and the comment above it for the threat
model.  This falls back to the pre-existing full read+SHA-256 re-hash
whenever the fast path cannot certify the file as unchanged, and
refreshes HANDLE's stored identity record after any full re-hash that
still matches, so the fast path can resume on the next call."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (path (plist-get state :path))
         (fresh (nelisp-eln-system-loader--file-identity path)))
    (unless (and fresh (equal fresh (plist-get state :identity)))
      (unless (equal (plist-get state :file-sha)
                      (nelisp-eln-system-loader--file-sha256
                       (nelisp-eln-system-loader--read-file path)))
        (nelisp-eln-system-loader--fail 'root-file-changed detail))
      (when fresh
        (puthash handle (plist-put state :identity fresh)
                 nelisp-eln-system-loader--handles)))
    t))

(defun nelisp-eln-system-loader--read-c-string (address)
  "Read a bounded NUL-terminated byte string from ADDRESS."
  (let ((i 0) (bytes nil) (done nil))
    (while (and (< i 4096) (not done))
      (let ((byte (ptr-read-u8 address i)))
        (if (= byte 0) (setq done t) (push byte bytes)))
      (setq i (1+ i)))
    (unless done
      (nelisp-eln-system-loader--fail 'unterminated-owner-path address))
    (decode-coding-string (apply #'unibyte-string (nreverse bytes))
                          'utf-8-unix)))

(defun nelisp-eln-system-loader-read (handle)
  "Read validated metadata through the system-loader backend HANDLE."
  (nelisp-eln-metadata-read-with-backend
   handle #'nelisp-eln-system-loader-symbol-info
   #'nelisp-eln-system-loader-read-root-object-bytes))

(provide 'nelisp-eln-system-loader)

;;; nelisp-eln-system-loader.el ends here
