;;; emacs-ffi.el --- Reusable FFI facade for NeLisp standalone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Library-level FFI adapter.  Consumers should call this facade rather
;; than probing `nl-ffi-call' directly.  Host Emacs remains safe to load:
;; no global FFI functions are installed, and calls signal a clear error
;; when the runtime primitive is unavailable.

;;; Code:

(defun emacs-ffi--nonempty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (> (length value) 0)))

(defun emacs-ffi-available-p ()
  "Return non-nil when a callable FFI backend is present."
  (or (fboundp 'nl-ffi-call)
      (fboundp 'nelisp-ffi-call)))

(defun emacs-ffi-buffer-available-p ()
  "Return non-nil when pointer buffer helpers are present."
  (and (fboundp 'nl-ffi-malloc)
       (fboundp 'nl-ffi-read-bytes)
       (fboundp 'nl-ffi-free)))

(defun emacs-ffi-call (library function signature &rest args)
  "Call FUNCTION in shared LIBRARY using SIGNATURE and ARGS.
SIGNATURE follows the NeLisp FFI vector convention, for example
`[:sint64 :string]'."
  (cond
   ((fboundp 'nl-ffi-call)
    (apply #'nl-ffi-call library function signature args))
   ((fboundp 'nelisp-ffi-call)
    (apply #'nelisp-ffi-call library function signature args))
   (t
    (error "emacs-ffi-call: no FFI backend is available"))))

(defun emacs-ffi-malloc (size)
  "Allocate SIZE bytes using the active NeLisp FFI backend."
  (if (fboundp 'nl-ffi-malloc)
      (nl-ffi-malloc size)
    (error "emacs-ffi-malloc: no FFI buffer backend is available")))

(defun emacs-ffi-read-bytes (pointer size)
  "Read SIZE bytes from POINTER using the active NeLisp FFI backend."
  (if (fboundp 'nl-ffi-read-bytes)
      (nl-ffi-read-bytes pointer size)
    (error "emacs-ffi-read-bytes: no FFI buffer backend is available")))

(defun emacs-ffi-free (pointer)
  "Free POINTER using the active NeLisp FFI backend."
  (if (fboundp 'nl-ffi-free)
      (nl-ffi-free pointer)
    (error "emacs-ffi-free: no FFI buffer backend is available")))

(defun emacs-ffi--windows-p ()
  "Return non-nil when the active runtime target is Windows."
  (or (eq system-type 'windows-nt)
      (and (boundp 'system-configuration)
           (stringp system-configuration)
           (string-match-p "mingw\\|msys\\|w64\\|windows"
                           system-configuration))))

(defun emacs-ffi--darwin-p ()
  "Return non-nil when the active runtime target is macOS."
  (or (eq system-type 'darwin)
      (and (boundp 'system-configuration)
           (stringp system-configuration)
           (string-match-p "darwin\\|apple" system-configuration))))

(defun emacs-ffi-shared-library-suffix ()
  "Return the shared library suffix for the active runtime target."
  (cond
   ((emacs-ffi--windows-p) ".dll")
   ((emacs-ffi--darwin-p) ".dylib")
   (t ".so")))

(defun emacs-ffi--runtime-library-names ()
  "Return plausible NeLisp runtime shared library file names."
  (cond
   ((emacs-ffi--windows-p)
    '("nelisp_runtime.dll" "libnelisp_runtime.dll"))
   ((emacs-ffi--darwin-p)
    '("libnelisp_runtime.dylib" "nelisp_runtime.dylib"))
   (t
    '("libnelisp_runtime.so" "nelisp_runtime.so"))))

(defun emacs-ffi--path-join (dir file)
  "Return FILE under DIR, preserving ordinary Emacs path semantics."
  (expand-file-name file (file-name-as-directory dir)))

(defun emacs-ffi--first-readable (paths)
  "Return the first readable path in PATHS, or nil."
  (let ((rest paths)
        (found nil))
    (while (and rest (not found))
      (when (and (stringp (car rest)) (file-readable-p (car rest)))
        (setq found (car rest)))
      (setq rest (cdr rest)))
    found))

(defun emacs-ffi-default-nelisp-runtime-library ()
  "Return the preferred NeLisp runtime shared library path.
Resolution order:

  1. `NELISP_RUNTIME_SO' (kept for compatibility, even on Windows)
  2. `NELISP_RUNTIME_LIBRARY'
  3. `NELISP_HOME'/target/release/<platform library name>
  4. `ANVIL_HOME'/target/release/<platform library name>
  5. common Notes checkout paths under HOME."
  (let* ((override-a (and (fboundp 'getenv) (getenv "NELISP_RUNTIME_SO")))
         (override-b (and (fboundp 'getenv) (getenv "NELISP_RUNTIME_LIBRARY")))
         (home (and (fboundp 'getenv) (getenv "NELISP_HOME")))
         (anvil (and (fboundp 'getenv) (getenv "ANVIL_HOME")))
         (user-home (or (and (fboundp 'getenv) (getenv "HOME")) "~"))
         (names (emacs-ffi--runtime-library-names))
         (candidates nil))
    (cond
     ((emacs-ffi--nonempty-string-p override-a) override-a)
     ((emacs-ffi--nonempty-string-p override-b) override-b)
     (t
      (when (emacs-ffi--nonempty-string-p home)
        (dolist (name names)
          (push (emacs-ffi--path-join
                 (emacs-ffi--path-join home "target/release")
                 name)
                candidates)))
      (when (emacs-ffi--nonempty-string-p anvil)
        (dolist (name names)
          (push (emacs-ffi--path-join
                 (emacs-ffi--path-join anvil "target/release")
                 name)
                candidates)))
      (dolist (root '("Cowork/Notes/dev/nelisp" "Notes/dev/nelisp"))
        (dolist (name names)
          (push (emacs-ffi--path-join
                 (emacs-ffi--path-join
                  (expand-file-name root user-home)
                  "target/release")
                 name)
                candidates)))
      (let ((ordered (nreverse candidates)))
        (or (emacs-ffi--first-readable ordered)
            (car ordered)))))))

(provide 'emacs-ffi)

;;; emacs-ffi.el ends here
