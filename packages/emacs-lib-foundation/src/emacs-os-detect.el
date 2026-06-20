;;; emacs-os-detect.el --- Runtime OS detection via uname(2)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2 — Layer 2 "OS introspection".
;;
;; `emacs-vars.el' hard-codes `system-type' = `gnu/linux' (its docstring
;; literally says "Override per-host once `system-type' detection
;; lands"), `emacs-stub.el' hard-codes `system-configuration', and the
;; standalone `system-name' fallback returns "standalone".  Those were
;; always placeholders waiting for "OS introspection" — this module is
;; that introspection.
;;
;; How real Emacs decides these:
;;   - `system-type'          : the C `SYSTEM_TYPE' macro, fixed by
;;                              configure at *build* time.
;;   - `system-configuration' : the configure triple `EMACS_CONFIGURATION'.
;;   - `system-name'          : filled at *runtime* by `init_system_name'
;;                              (gethostname / uname) in sysdep.c.
;;
;; The standalone NeLisp reader has no build-time configure step, so we
;; do the equivalent entirely at *runtime* with one POSIX `uname(2)'
;; call, issued straight through the reader's `syscall-direct'
;; substrate (the very primitive `emacs-network-syscall-shim.el'
;; already uses).  A single syscall yields every field we need:
;;
;;   struct utsname {            ; Linux, 6 * (NEW_UTS_LEN+1=65) bytes
;;     char sysname[65];         ; offset   0  -> `system-type'
;;     char nodename[65];        ; offset  65  -> `system-name'
;;     char release[65];         ; offset 130  -> kernel rev (plist only)
;;     char version[65];         ; offset 195  -> plist only
;;     char machine[65];         ; offset 260  -> `system-configuration'
;;     char domainname[65]; };   ; offset 325  -> plist only
;;
;; This is pure Elisp on top of the existing substrate: NO new reader
;; primitive and NO Rust.  When the substrate is absent (running under
;; the host-Emacs driver, or on any target the Linux/x86_64 reader does
;; not cover) detection returns nil and every hard-coded default in
;; `emacs-vars.el' / `emacs-stub.el' is left untouched.

;;; Code:

(defconst emacs-os-uname-syscall-number 63
  "Linux x86_64 syscall number for `uname(2)'.
The standalone reader's `syscall-direct' substrate targets Linux
x86_64 only, so this is the single ABI we issue the call against.")

(defconst emacs-os--utsname-field-len 65
  "Byte length of each NUL-padded field in Linux `struct utsname'.
Equals `__NEW_UTS_LEN' + 1.")

(defconst emacs-os--utsname-size 390
  "Total byte size of Linux `struct utsname' (6 * 65).")

(defvar emacs-os--uname-cache nil
  "Cached result of `emacs-uname'.
nil before the first probe; a plist on success; the symbol
`unavailable' when a probe ran but the substrate could not satisfy
it (so we do not re-issue the syscall on every call).")

(defvar emacs-os--system-name nil
  "Host node name detected via `uname(2)', or nil when undetected.
Backs the standalone `system-name' function defined below.")

(defun emacs-os--substrate-available-p ()
  "Return non-nil when the reader exposes the raw syscall substrate."
  (and (fboundp 'syscall-direct)
       (fboundp 'alloc-bytes)
       (fboundp 'ptr-read-u64)
       (fboundp 'ptr-write-u64)))

(defun emacs-os--zero (ptr bytes)
  "Zero BYTES bytes at PTR (`alloc-bytes' is not guaranteed zero-init)."
  (let ((i 0))
    (while (< i bytes)
      (ptr-write-u64 ptr i 0)
      (setq i (+ i 8)))))

(defun emacs-os--read-field (ptr off)
  "Read the NUL-terminated `utsname' field at PTR+OFF into a string.
Reads at most `emacs-os--utsname-field-len' bytes.  Byte access is
unaligned `ptr-read-u64' masking, matching the network syscall shim."
  (let ((s "") (j 0) (done nil))
    (while (and (not done) (< j emacs-os--utsname-field-len))
      (let ((b (logand (ptr-read-u64 ptr (+ off j)) 255)))
        (if (= b 0)
            (setq done t)
          (setq s (concat s (char-to-string b)))))
      (setq j (1+ j)))
    s))

(defun emacs-uname (&optional refresh)
  "Return host info from POSIX `uname(2)' as a plist, or nil.
The plist carries :sysname :nodename :release :version :machine
:domainname.  The result is cached; pass non-nil REFRESH to
re-probe.  Returns nil when the syscall substrate is unavailable or
the call fails — callers must treat nil as \"unknown, keep
defaults\"."
  (when (or refresh (null emacs-os--uname-cache))
    (setq emacs-os--uname-cache
          (if (not (emacs-os--substrate-available-p))
              'unavailable
            (condition-case nil
                (let ((buf (alloc-bytes (+ emacs-os--utsname-size 16) 8)))
                  (emacs-os--zero buf (+ emacs-os--utsname-size 16))
                  (let ((rc (syscall-direct emacs-os-uname-syscall-number
                                            buf 0 0 0 0 0)))
                    (if (and (integerp rc) (>= rc 0))
                        (list :sysname    (emacs-os--read-field buf 0)
                              :nodename   (emacs-os--read-field buf 65)
                              :release    (emacs-os--read-field buf 130)
                              :version    (emacs-os--read-field buf 195)
                              :machine    (emacs-os--read-field buf 260)
                              :domainname (emacs-os--read-field buf 325))
                      'unavailable)))
              (error 'unavailable)))))
  (and (listp emacs-os--uname-cache) emacs-os--uname-cache))

(defun emacs-os-sysname->system-type (sysname)
  "Map a `uname' SYSNAME string to an Emacs `system-type' symbol.
Returns nil for an unrecognised SYSNAME so callers keep their
existing default instead of guessing."
  (let ((s (downcase (or sysname ""))))
    (cond
     ((string-prefix-p "linux" s) 'gnu/linux)
     ((string-prefix-p "gnu/kfreebsd" s) 'gnu/kfreebsd)
     ((string-prefix-p "darwin" s) 'darwin)
     ((member s '("freebsd" "netbsd" "openbsd" "dragonfly")) 'berkeley-unix)
     ((string= s "gnu") 'gnu)               ; GNU/Hurd
     ((string= s "sunos") 'usg-unix-v)      ; Solaris reports "SunOS"
     ((string= s "aix") 'aix)
     ((string= s "haiku") 'haiku)
     ((string-prefix-p "cygwin" s) 'cygwin)
     ((or (string-prefix-p "mingw" s)
          (string-prefix-p "msys" s)
          (string-prefix-p "windows" s))
      'windows-nt)
     (t nil))))

(defun emacs-os--config-vendor (machine stype)
  "Pick the GNU-triple vendor field for MACHINE / STYPE."
  (cond
   ((eq stype 'darwin) "apple")
   ((string-match-p "\\`\\(i[3-6]86\\|x86_64\\|amd64\\)\\'" (or machine "")) "pc")
   (t "unknown")))

(defun emacs-os--config-os (stype release)
  "Pick the GNU-triple OS field for STYPE / kernel RELEASE."
  (cond
   ((eq stype 'gnu/linux) "linux-gnu")
   ((eq stype 'darwin)
    (concat "darwin" (car (split-string (or release "") "\\."))))
   ((eq stype 'berkeley-unix) "bsd")
   ((eq stype 'gnu/kfreebsd) "kfreebsd-gnu")
   ((eq stype 'gnu) "gnu")
   ((eq stype 'usg-unix-v) "solaris")
   ((eq stype 'aix) "aix")
   ((eq stype 'haiku) "haiku")
   ((eq stype 'cygwin) "cygwin")
   ((eq stype 'windows-nt) "mingw32")
   (t "unknown")))

(defun emacs-os--config-triple (machine stype release)
  "Synthesise a GNU config triple from `uname' fields.
Real Emacs gets `system-configuration' from configure; we
approximate it as MACHINE-VENDOR-OS so that, on the current Linux
x86_64 target, it reproduces the historical \"x86_64-pc-linux-gnu\"."
  (format "%s-%s-%s"
          (or machine "unknown")
          (emacs-os--config-vendor machine stype)
          (emacs-os--config-os stype release)))

(defun emacs-detect-system-type ()
  "Return the detected `system-type' symbol, or nil when unknown.
Pure: probes `uname' (cached) and maps its sysname but mutates no
global.  Use `emacs-os-detect-and-set!' to apply the detection."
  (let ((u (emacs-uname)))
    (and u (emacs-os-sysname->system-type (plist-get u :sysname)))))

;;;###autoload
(defun emacs-os-detect-and-set! ()
  "Detect the running OS via `uname(2)' and apply it to globals.
On success sets `system-type', `system-configuration', and the
standalone `system-name' (through `emacs-os--system-name'), and
returns the `uname' plist.  Returns nil when the substrate is
absent or the sysname is unrecognised — in which case every
existing default from `emacs-vars.el' / `emacs-stub.el' is left
intact."
  (let ((u (emacs-uname t)))
    (when u
      (let* ((sysname  (plist-get u :sysname))
             (stype    (emacs-os-sysname->system-type sysname))
             (nodename (plist-get u :nodename))
             (machine  (plist-get u :machine))
             (release  (plist-get u :release)))
        (when stype
          (setq system-type stype)
          (when (and (boundp 'system-configuration) machine)
            (setq system-configuration
                  (emacs-os--config-triple machine stype release)))
          (when (and nodename (> (length nodename) 0))
            (setq emacs-os--system-name nodename))
          u)))))

;;;; --- environment-derived emacs-vars defaults (Doc 51 Phase 2b) -------
;;
;; emacs-vars.el / emacs-stub.el hard-code temporary-file-directory,
;; user-emacs-directory, exec-path, path-separator, and exec-suffixes
;; ("Phase 2 will resolve dynamically once getenv is wired").  Now that
;; emacs-callproc seeds `process-environment' from /proc/self/environ and
;; getenv reflects the real OS env, derive the dir/path vars from
;; TMPDIR / HOME / PATH, and the OS-shaped vars from the detected
;; `system-type'.  All gated on the substrate, so host Emacs keeps its
;; own values.

(defun emacs-os--env (name)
  "Return env var NAME as a non-empty string, or nil."
  (and (fboundp 'getenv)
       (let ((v (ignore-errors (getenv name))))
         (and (stringp v) (> (length v) 0) v))))

(defun emacs-os-apply-os-polyfills! ()
  "Set OS-shaped emacs-vars defaults from the detected `system-type'.
`path-separator' and `exec-suffixes' follow the OS so the standalone
reader matches host Emacs per platform.  Returns a summary plist."
  (let ((win (eq system-type 'windows-nt)))
    (when (boundp 'path-separator)
      (setq path-separator (if win ";" ":")))
    (when (boundp 'exec-suffixes)
      (setq exec-suffixes (if win '(".exe" ".com" ".bat" ".cmd" "") nil)))
    (list :path-separator (and (boundp 'path-separator) path-separator)
          :exec-suffixes (and (boundp 'exec-suffixes) exec-suffixes))))

(defun emacs-os-detect-and-set-dirs! ()
  "Derive dir/path emacs-vars from the live OS environment.
Updates `temporary-file-directory' (TMPDIR), `user-emacs-directory'
(HOME), and `exec-path' (PATH split on `path-separator').  Each var is
touched only when its env source is present, so a missing var keeps the
existing default.  Returns a summary plist."
  (let ((tmp  (emacs-os--env "TMPDIR"))
        (home (emacs-os--env "HOME"))
        (path (emacs-os--env "PATH")))
    (when (and tmp (boundp 'temporary-file-directory))
      (setq temporary-file-directory (file-name-as-directory tmp)))
    (when (and home (boundp 'user-emacs-directory))
      (setq user-emacs-directory
            (file-name-as-directory
             (concat (file-name-as-directory home) ".emacs.d"))))
    (when (and path (boundp 'exec-path))
      (let ((sep (if (boundp 'path-separator) path-separator ":")))
        (setq exec-path (split-string path (regexp-quote sep) t))))
    (list :tmp tmp :home home :path (and path t))))

;; Standalone `system-name': the host-Emacs driver already provides a
;; builtin, so guard with `unless fboundp' and never clobber it; the
;; NeLisp reader does not, so we serve the detected node name.
(unless (fboundp 'system-name)
  (defun system-name ()
    "Return this host's node name (detected via `uname' when available)."
    (or emacs-os--system-name "standalone")))

;; Apply at load time, but only when the syscall substrate is present.
;; Under the host-Emacs driver this is a no-op (real Emacs values win);
;; under the NeLisp reader it replaces the hard-coded `gnu/linux'
;; default with the live OS.
(when (emacs-os--substrate-available-p)
  ;; Order matters: set system-type first, then the OS-shaped vars
  ;; (path-separator) it drives, then the env-derived dirs (PATH split
  ;; uses path-separator).
  (ignore-errors (emacs-os-detect-and-set!))
  (ignore-errors (emacs-os-apply-os-polyfills!))
  (ignore-errors (emacs-os-detect-and-set-dirs!)))

(provide 'emacs-os-detect)

;;; emacs-os-detect.el ends here
