;;; nelisp-sys.el --- Typed C-replacement systems subset for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; nelisp-sys is a typed, statically checked systems-programming subset
;; hosted on the NeLisp AOT toolchain.  It is NOT ordinary dynamic
;; Elisp: source written in the `sys:' family of forms is type checked,
;; ownership/borrow checked, and lowered to native object code through a
;; thin adapter over the existing NeLisp compiler/assembler/linker.
;;
;; Design references:
;;   docs/design/130-nelisp-sys-extractable-c-replacement-subset.org
;;   docs/design/131-nelisp-sys-semantics-and-safety-contract.org
;;   docs/design/132-nelisp-sys-abi-and-platform-model.org
;;
;; Boundary rule (Doc 130): only `nelisp-sys-adapter-nelisp.el' is allowed
;; to call private NeLisp compiler/asm/linker internals.  Every other
;; module talks to the backend through the adapter contract so the package
;; stays extractable into its own repository later.
;;
;; This file is the BASE layer (dependency root): it defines only the
;; whole front end (types, target model, layout, AST, frontend, checkers)
;; but NOT the backend adapter, which is required explicitly by the driver
;; so that pure-analysis use (type/borrow checking) never needs a toolchain.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function nelisp-os-open "nelisp-stdlib-os" (path flags mode))
(declare-function nelisp-os-read "nelisp-stdlib-os" (fd nbytes))
(declare-function nelisp-os-write "nelisp-stdlib-os" (fd str))
(declare-function nelisp-os-close "nelisp-stdlib-os" (fd))
(declare-function nelisp-os-exit "nelisp-stdlib-os" (code))
(declare-function nelisp-os-dup2 "nelisp-stdlib-os" (oldfd newfd))
(declare-function nelisp-os-fork "nelisp-stdlib-os" ())
(declare-function nelisp-os-execve "nelisp-stdlib-os" (path argv envp))
(declare-function nelisp-os-wait "nelisp-stdlib-os" (pid options))
(declare-function nelisp-os-pipe "nelisp-stdlib-os" ())
(declare-function nelisp-os--alloc-cstring "nelisp-stdlib-os" (s))
(declare-function nelisp-os--build-cstr-array "nelisp-stdlib-os" (strs))
(declare-function nelisp-os--free "nelisp-stdlib-os" (ptr))
(declare-function nelisp-os--free-cstr-array "nelisp-stdlib-os" (pair))
(declare-function nelisp--syscall "ext:nelisp-runtime"
                  (nr a0 a1 a2 a3 a4 a5))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))

(defvar nelisp--startup-argc nil)
(defvar nelisp--startup-argv nil)
(defvar nelisp--startup-envp nil)

(defconst nelisp-sys-version "0.1.0-dev"
  "Semantic version of the nelisp-sys package.

The version is independent from NeLisp core (Doc 130 extraction
criterion 15: the release cadence differs from NeLisp core).")

(define-error 'nelisp-sys-error "nelisp-sys error")

(defun nelisp-sys-version ()
  "Return the nelisp-sys package version string."
  nelisp-sys-version)

;;; Host-backed OS substrate helpers ---------------------------------

(defconst nelisp-sys-access-x-ok 1
  "Executable access bit used by `nelisp-sys-access'.")

(defconst nelisp-sys-o-rdonly 0)
(defconst nelisp-sys-o-wronly 1)
(defconst nelisp-sys-o-rdwr 2)
(defconst nelisp-sys-o-creat 64)
(defconst nelisp-sys-o-trunc 512)
(defconst nelisp-sys-o-append 1024)
(defconst nelisp-sys-stdin 0)
(defconst nelisp-sys-stdout 1)
(defconst nelisp-sys-stderr 2)
(defconst nelisp-sys-wnohang 1)

(defconst nelisp-sys--portable-syscall-table
  '((gnu/linux
     (read . 0) (write . 1) (open . 2) (close . 3)
     (poll . 7) (mmap . 9) (munmap . 11) (mprotect . 10)
     (dup2 . 33) (fork . 57) (execve . 59) (exit . 60)
     (wait4 . 61) (kill . 62) (fcntl . 72) (pipe . 22)
     (stat . 4) (fstat . 5) (getpid . 39)
     (getcwd . 79) (chdir . 80) (rmdir . 84) (symlink . 88))
    (darwin
     (read . 3) (write . 4) (open . 5) (close . 6)
     (fork . 2) (execve . 59) (exit . 1) (wait4 . 7)
     (kill . 37) (dup2 . 90) (fcntl . 92) (poll . 230)
     (mmap . 197) (munmap . 73) (mprotect . 74)
     (stat . 188) (fstat . 189) (getpid . 20)
     ;; chdir(2) = 12, symlink(2) = 57, rmdir(2) = 137 on macOS.
     ;; getcwd is NOT a simple syscall on macOS (it is implemented in
     ;; libc via __getcwd / getdirentries64); omit from this table and
     ;; use the host fallback path on Darwin.
     (chdir . 12) (symlink . 57) (rmdir . 137)))
  "Portable syscall-name to target-number table.

This is deliberately small: it covers the file/process/event-loop surface used
by `nelisp-sys' and `nelisp-process'.  Callers should resolve by symbolic name
through `nelisp-sys-syscall-number' / `nelisp-sys-syscall' rather than embedding
Linux or Darwin numbers in package code.")

(defvar nelisp-sys--cstring-array-owners (make-hash-table :test 'eql)
  "Pointer array ownership table for `nelisp-sys-cstring-array'.")

(defun nelisp-sys--ensure-os ()
  "Load `nelisp-stdlib-os' when available.
Return non-nil when the OS layer is present.  The standalone runtime
provides `nelisp--syscall-supported-p'; host tests may not, so install a
conservative fallback before loading the package."
  (unless (fboundp 'nelisp--syscall-supported-p)
    (defalias 'nelisp--syscall-supported-p (lambda () nil)))
  (or (featurep 'nelisp-stdlib-os)
      (require 'nelisp-stdlib-os nil t)))

(defun nelisp-sys--require-os (operation)
  "Ensure the OS layer is available for OPERATION."
  (unless (nelisp-sys--ensure-os)
    (signal 'nelisp-sys-error
            (list (format "%s requires nelisp-stdlib-os" operation)))))

(defun nelisp-sys-current-platform ()
  "Return the portable platform key used by `nelisp-sys' syscall tables."
  (cond
   ((eq system-type 'darwin) 'darwin)
   ((memq system-type '(gnu/linux berkeley-unix)) 'gnu/linux)
   ((eq system-type 'windows-nt) 'windows)
   (t system-type)))

(defun nelisp-sys-syscall-number (name &optional platform)
  "Return syscall number for symbolic NAME on PLATFORM.

NAME may be a symbol or string.  PLATFORM defaults to
`nelisp-sys-current-platform'.  Signals `nelisp-sys-error' when the name is not
available for the requested platform."
  (let* ((key (if (symbolp name) name (intern name)))
         (plat (or platform (nelisp-sys-current-platform)))
         (table (cdr (assq plat nelisp-sys--portable-syscall-table)))
         (entry (assq key table)))
    (unless entry
      (signal 'nelisp-sys-error
              (list (format "unsupported syscall %S for %S" key plat))))
    (cdr entry)))

(defun nelisp-sys-syscall (name &optional a0 a1 a2 a3 a4 a5)
  "Invoke portable syscall NAME with up to six integer/pointer arguments.

The symbolic NAME is resolved through `nelisp-sys-syscall-number'.  This keeps
platform syscall numbers behind the `nelisp-sys' boundary.  Hosted Emacs runs
without the standalone syscall trampoline signal `nelisp-sys-error' rather than
pretending the syscall path is available."
  (unless (fboundp 'nelisp--syscall)
    (signal 'nelisp-sys-error
            (list "nelisp-sys-syscall requires nelisp--syscall runtime binding")))
  (nelisp--syscall (nelisp-sys-syscall-number name)
                   (or a0 0) (or a1 0) (or a2 0)
                   (or a3 0) (or a4 0) (or a5 0)))

(defun nelisp-sys--read-ptr-bytes (ptr len)
  "Return LEN bytes read from PTR as a unibyte string."
  (let ((out (make-string len 0))
        (idx 0))
    (while (< idx len)
      (aset out idx (ptr-read-u8 ptr idx))
      (setq idx (1+ idx)))
    out))

(defun nelisp-sys--write-ptr-bytes (ptr bytes)
  "Copy BYTES into PTR and return byte count."
  (let* ((raw (encode-coding-string bytes 'no-conversion t))
         (len (string-bytes raw))
         (idx 0))
    (while (< idx len)
      (ptr-write-u8 ptr idx (aref raw idx))
      (setq idx (1+ idx)))
    len))

(unless (fboundp 'nelisp-startup-argc)
  (defun nelisp-startup-argc ()
    "Return the current host startup argument count.
In standalone this name is reserved for the VM-provided initial argc."
    (or nelisp--startup-argc
        (length command-line-args))))

(unless (fboundp 'nelisp-startup-argv)
  (defun nelisp-startup-argv ()
    "Return the current host startup argv representation.
In standalone this name is reserved for the VM-provided argv pointer."
    (copy-sequence
     (or nelisp--startup-argv command-line-args))))

(unless (fboundp 'nelisp-startup-envp)
  (defun nelisp-startup-envp ()
    "Return the current host startup envp representation.
In standalone this name is reserved for the VM-provided envp pointer."
    (copy-sequence
     (or nelisp--startup-envp process-environment))))

(defun nelisp-sys-startup-argc ()
  "Return the process startup argc through the configured lower name."
  (funcall (symbol-function 'nelisp-startup-argc)))

(defun nelisp-sys-startup-argv ()
  "Return the process startup argv through the configured lower name."
  (funcall (symbol-function 'nelisp-startup-argv)))

(defun nelisp-sys-startup-envp ()
  "Return the process startup envp through the configured lower name."
  (funcall (symbol-function 'nelisp-startup-envp)))

(defun nelisp-sys-cstring (string)
  "Allocate a NUL-terminated copy of STRING and return its pointer."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (nelisp-sys--require-os "nelisp-sys-cstring")
  (nelisp-os--alloc-cstring string))

(defun nelisp-sys-free (ptr)
  "Free PTR allocated by the lower OS/runtime allocator."
  (nelisp-sys--require-os "nelisp-sys-free")
  (nelisp-os--free ptr))

(defun nelisp-sys-cstring-array (strings)
  "Allocate a NULL-terminated char ** for STRINGS and return its pointer.
Ownership is tracked internally; call `nelisp-sys-free-cstring-array' on
the returned pointer on non-exec failure paths."
  (unless (and (listp strings) (cl-every #'stringp strings))
    (signal 'wrong-type-argument (list '(list string) strings)))
  (nelisp-sys--require-os "nelisp-sys-cstring-array")
  (let ((pair (nelisp-os--build-cstr-array strings)))
    (puthash (car pair) pair nelisp-sys--cstring-array-owners)
    (car pair)))

(defun nelisp-sys-free-cstring-array (ptr)
  "Free PTR previously returned by `nelisp-sys-cstring-array'."
  (nelisp-sys--require-os "nelisp-sys-free-cstring-array")
  (let ((pair (gethash ptr nelisp-sys--cstring-array-owners)))
    (unless pair
      (signal 'nelisp-sys-error
              (list "unknown cstring-array pointer" ptr)))
    (remhash ptr nelisp-sys--cstring-array-owners)
    (nelisp-os--free-cstr-array pair)))

(defun nelisp-sys-getenv (name)
  "Return environment variable NAME, or nil when it is absent.

Doc 44 defines this as a NeLisp library operation over startup envp.
This package-level implementation uses the host `process-environment'
when running inside Emacs, preserving the public NeLisp API while the
standalone envp primitive is wired underneath it."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (getenv name))

(defun nelisp-sys--path-split (path)
  "Split PATH like a POSIX PATH variable.
Empty elements denote the current directory."
  (mapcar (lambda (part) (if (string-empty-p part) "." part))
          (split-string (or path "") path-separator)))

(defun nelisp-sys-access (path mode)
  "Return 0 when PATH satisfies MODE, -1 otherwise.

Only executable checks (`nelisp-sys-access-x-ok') are currently exposed
because that is the Doc 44 requirement for PATH resolution.  Other modes
fall back to existence checks until the raw syscall layer is available."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (if (pcase mode
        ((or 1 'x 'exec 'executable) (file-executable-p path))
        (_ (file-exists-p path)))
      0
    -1))

(defun nelisp-sys-open (path flags mode)
  "Open PATH with FLAGS and MODE; return fd."
  (nelisp-sys--require-os "nelisp-sys-open")
  (nelisp-os-open path flags mode))

(defun nelisp-sys-read (fd ptr len)
  "Read up to LEN bytes from FD into PTR; return byte count.
When PTR is nil, return the read byte string as a hosted convenience."
  (nelisp-sys--require-os "nelisp-sys-read")
  (let ((bytes (nelisp-os-read fd len)))
    (if ptr
        (nelisp-sys--write-ptr-bytes ptr bytes)
      bytes)))

(defun nelisp-sys-write (fd ptr len)
  "Write LEN bytes from PTR to FD; return byte count.
PTR may be a runtime pointer or, in hosted tests, a string."
  (nelisp-sys--require-os "nelisp-sys-write")
  (nelisp-os-write fd
                   (if (stringp ptr)
                       (substring ptr 0 (min len (string-bytes ptr)))
                     (nelisp-sys--read-ptr-bytes ptr len))))

(defun nelisp-sys-close (fd)
  "Close FD and return 0 on success."
  (nelisp-sys--require-os "nelisp-sys-close")
  (nelisp-os-close fd)
  0)

(defun nelisp-sys-exit (code)
  "Exit the current process with CODE.  Does not return."
  (nelisp-sys--require-os "nelisp-sys-exit")
  (nelisp-os-exit code))

(defun nelisp-sys-dup2 (old new)
  "Duplicate fd OLD onto NEW and return NEW."
  (nelisp-sys--require-os "nelisp-sys-dup2")
  (nelisp-os-dup2 old new))

(defun nelisp-sys-fork ()
  "Fork the current process; return child pid in parent and 0 in child."
  (nelisp-sys--require-os "nelisp-sys-fork")
  (nelisp-os-fork))

(defun nelisp-sys-execve (path argv envp)
  "Replace current process image with PATH using ARGV and ENVP."
  (nelisp-sys--require-os "nelisp-sys-execve")
  (nelisp-os-execve path argv envp))

(defun nelisp-sys-waitpid (pid options)
  "Wait for PID with OPTIONS and return the raw wait status."
  (nelisp-sys--require-os "nelisp-sys-waitpid")
  (cdr (nelisp-os-wait pid options)))

(defun nelisp-sys-pipe ()
  "Create a pipe and return (READ-FD . WRITE-FD)."
  (nelisp-sys--require-os "nelisp-sys-pipe")
  (nelisp-os-pipe))

(defun nelisp-sys-executable-find (command)
  "Return absolute executable path for COMMAND, or nil.

This is the Doc 44 Layer 2 PATH resolver: commands containing a slash are
checked directly; bare commands are searched through PATH using
`nelisp-sys-getenv'."
  (unless (stringp command)
    (signal 'wrong-type-argument (list 'stringp command)))
  (unless (string-empty-p command)
    (if (string-match-p "/" command)
        (let ((path (expand-file-name command)))
          (and (zerop (nelisp-sys-access path nelisp-sys-access-x-ok))
               path))
      (cl-loop for dir in (nelisp-sys--path-split
                           (or (nelisp-sys-getenv "PATH") ""))
               for path = (expand-file-name command dir)
               when (zerop (nelisp-sys-access
                            path nelisp-sys-access-x-ok))
               return path))))

;;; Filesystem primitives (M0 increment 1 — §10.2) -------------------

;; Host fallbacks: when `nelisp--syscall' is not bound (i.e. running on
;; a regular Emacs, not the standalone NeLisp binary) each wrapper
;; delegates to the equivalent host Emacs primitive.  This lets ERT
;; validate the wrappers on the development host without a standalone
;; binary present.

(defun nelisp-sys-chdir (path)
  "Change the process working directory to PATH.
Returns 0 on success or signals `nelisp-sys-error' on failure.

On the standalone NeLisp runtime this resolves NR via
`nelisp-sys-syscall-number' and invokes chdir(2) directly using
`nelisp--syscall-path-int' (which marshals PATH to a cstring
internally).  On a host Emacs the working directory is emulated via
`default-directory'; the host variant signals if PATH does not
exist or is not a directory."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (if (fboundp 'nelisp--syscall-path-int)
      ;; Standalone NeLisp binary: use nelisp--syscall-path-int which
      ;; marshals PATH to a cstring internally — no alloc/free needed.
      (let* ((nr (nelisp-sys-syscall-number 'chdir))
             (ret (nelisp--syscall-path-int nr path 0)))
        (when (< ret 0)
          (signal 'nelisp-sys-error
                  (list (format "chdir %S failed: %d" path ret))))
        0)
    ;; Host fallback: validate and set default-directory.
    (let ((expanded (expand-file-name path)))
      (unless (file-directory-p expanded)
        (signal 'nelisp-sys-error
                (list (format "chdir %S: not a directory" path))))
      (setq default-directory (file-name-as-directory expanded))
      0)))

(defun nelisp-sys-symlink (target linkpath)
  "Create a symbolic link at LINKPATH pointing to TARGET.
Returns 0 on success or signals `nelisp-sys-error' on failure.

On a host Emacs this delegates to `make-symbolic-link'.
On the standalone NeLisp runtime this is DEFERRED: symlink(2) requires
a two-path syscall builtin (`nelisp--syscall-path-path' or similar) that
is not yet wired in the standalone substrate.
See design doc §10 (M0 increment 2) for the planned wiring.
The syscall table entries for symlink are intentionally kept
as documentation of the intended interface."
  (unless (stringp target)
    (signal 'wrong-type-argument (list 'stringp target)))
  (unless (stringp linkpath)
    (signal 'wrong-type-argument (list 'stringp linkpath)))
  (if (fboundp 'nelisp--syscall-path-int)
      ;; Standalone: deferred — no two-path syscall builtin available yet.
      ;; M0 increment 2 will wire this once nelisp--syscall-path-path
      ;; (or equivalent) is added to the runtime.
      (signal 'nelisp-sys-error
              (list (concat "nelisp-sys-symlink not yet wired on standalone "
                            "(M0 increment 2): needs a 2-path syscall builtin")))
    ;; Host fallback.
    (make-symbolic-link target linkpath)
    0))

(defun nelisp-sys-rmdir (path)
  "Remove the empty directory at PATH.
Returns 0 on success or signals `nelisp-sys-error' on failure.

On the standalone NeLisp runtime this resolves NR via
`nelisp-sys-syscall-number' and invokes rmdir(2) directly using
`nelisp--syscall-path-int' (which marshals PATH to a cstring
internally).  On a host Emacs this delegates to `delete-directory'
(non-recursive)."
  (unless (stringp path)
    (signal 'wrong-type-argument (list 'stringp path)))
  (if (fboundp 'nelisp--syscall-path-int)
      ;; Standalone NeLisp binary: use nelisp--syscall-path-int which
      ;; marshals PATH to a cstring internally — no alloc/free needed.
      (let* ((nr (nelisp-sys-syscall-number 'rmdir))
             (ret (nelisp--syscall-path-int nr path 0)))
        (when (< ret 0)
          (signal 'nelisp-sys-error
                  (list (format "rmdir %S failed: %d" path ret))))
        0)
    ;; Host fallback.
    (delete-directory path)
    0))

(defconst nelisp-sys--getcwd-buf-size 4096
  "Buffer size used by `nelisp-sys-getcwd' for the standalone syscall path.")

(defun nelisp-sys-getcwd ()
  "Return the current working directory as a string.

On a host Emacs it returns `default-directory' via `expand-file-name',
with any trailing slash stripped to match POSIX getcwd(2) semantics.
On the standalone NeLisp runtime this is DEFERRED: getcwd(2) requires
passing a writeable buffer pointer to the kernel, which needs a buffer
syscall builtin (`nelisp--syscall-buf' or similar) that is not yet wired
in the standalone substrate.  On Linux getcwd(2) = NR 79 is in the
syscall table but cannot be called without a buffer pointer.  On macOS
getcwd is not a raw syscall at all (libc-mediated), so the Darwin table
intentionally has no getcwd entry.
See design doc §10 (M0 increment 2) for the planned wiring."
  (if (fboundp 'nelisp--syscall-path-int)
      ;; Standalone: deferred — no buffer-returning syscall builtin yet.
      ;; M0 increment 2 will wire this once nelisp--syscall-buf
      ;; (or equivalent) is added to the runtime.
      (signal 'nelisp-sys-error
              (list (concat "nelisp-sys-getcwd not yet wired on standalone "
                            "(M0 increment 2): needs a buffer syscall builtin")))
    ;; Host fallback: expand and strip trailing slash.
    (let ((d (expand-file-name default-directory)))
      (if (and (> (length d) 1) (string-suffix-p "/" d))
          (substring d 0 (1- (length d)))
        d))))

(defvar nelisp-sys--mkdtemp-counter 0
  "Monotonic counter used by `nelisp-sys-mkdtemp' for suffix uniqueness.")

(defun nelisp-sys-mkdtemp (template)
  "Create a unique temporary directory based on TEMPLATE prefix.
Returns the path of the created directory as a string.

This is a pure-Elisp composition; it does NOT call the mkdtemp(3)
libc function (which would require a Rust wrapper violating the zero-
Rust-delta rule).  The suffix is derived solely from the monotonic
counter `nelisp-sys--mkdtemp-counter' (incremented on each attempt),
which avoids any dependency on `emacs-pid', `current-time', or
`random' — none of which are available on the standalone NeLisp binary.
The real collision guard is `make-directory''s atomic failure on a
pre-existing path; up to 100 attempts are made before signalling.
This function works identically on both host Emacs and standalone.

TEMPLATE is a directory prefix string such as \"/tmp/nelisp-build-\"."
  (unless (stringp template)
    (signal 'wrong-type-argument (list 'stringp template)))
  (let ((max-tries 100)
        result)
    (while (and (null result) (> max-tries 0))
      (setq nelisp-sys--mkdtemp-counter
            (1+ nelisp-sys--mkdtemp-counter))
      (let ((candidate (format "%s%x" template nelisp-sys--mkdtemp-counter)))
        (condition-case nil
            (progn
              (make-directory candidate)
              (setq result candidate))
          (error nil)))
      (setq max-tries (1- max-tries)))
    (unless result
      (signal 'nelisp-sys-error
              (list (format "mkdtemp: could not create dir with template %S"
                            template))))
    result))

;; Front-end modules are required lazily as they are implemented.  During
;; the scaffold stage (130.1) only this aggregator and the adapter stub
;; exist; later phases add `(require 'nelisp-sys-types)' etc. here.

;; No sibling `require's here on purpose: this file is the dependency root.

(provide 'nelisp-sys)

;;; nelisp-sys.el ends here
