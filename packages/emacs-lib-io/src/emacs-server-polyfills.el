;;; emacs-server-polyfills.el --- C-level polyfills for vendor server.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 7c (= K3, 2026-05-11) — small polyfills that bridge the
;; gap between standalone NeLisp's stubbed C-core and what
;; `vendor/emacs-lisp/server.el' (2101 LOC) needs at load / `server-start'
;; time.  Loaded AFTER the K1 network stack
;; (`emacs-network-ffi.el' / `emacs-process-events.el' /
;; `emacs-eventloop.el') so we can use libc FFI for the bits that need
;; real I/O.
;;
;; Surface added:
;;
;;   - `file-attributes' (+ `file-attribute-{type,user-id,group-id,
;;      link-number,modes}') via a synthesised attrs list.  Server-start
;;      calls this from `server-ensure-safe-dir' to check the socket-dir
;;      ownership / mode bits; we return `(t 1 UID UID ... "drwx------")'
;;      for any path our `file-exists-p' confirms.
;;
;;   - `file-exists-p' wrapping libc `access(F_OK)' so server.el's
;;      socket-file checks see the real state.
;;
;;   - `make-directory' wrapping libc `mkdir' so server-start's safe-dir
;;      creation actually happens (= emacs-stub-bulk leaves this as a
;;      no-op).  Honours the `parents' flag by walking the path
;;      components.  EEXIST (errno 17) is tolerated for both branches.
;;
;;   - `with-file-modes' as a passthrough macro — the umask side effect
;;      is moot under standalone where our stubs do not honour it.
;;
;;   - `process-put' / `process-get' on slot 8 of the process-events
;;      vector (= the plist slot).  Server.el stashes its `:server-file'
;;      / `:auth-key' / `:server-stop-timer' etc here.
;;
;;   - `featurep' override that recognises
;;      `(featurep 'make-network-process '(:family local))' (= the
;;      `defcustom server-use-tcp' init guard) and returns t since K1
;;      gave us UNIX-family sockets.
;;
;;   - C-level scalar defvars that vendor server.el reads at load time
;;     (`internal--daemon-sockname' / `before-init-time' /
;;      `global-minor-modes' / `terminal-frame' / `emacs-pid' /
;;      `load-in-progress' / `ctl-x-map').
;;
;;   - User-identity stubs `user-uid' / `user-real-uid' / `system-name'
;;     / `emacs-pid' / `daemonp' / `frame-list' / `selected-frame'.
;;
;;   - `defvar-keymap' as a `(defvar NAME nil)` macro.  Emacs 29+
;;      keymap syntax — server.el touches it in its keybinding setup
;;      section, which is orthogonal to the IPC core.
;;
;;   - `substitute-command-keys' as an identity stub.
;;
;; Gate: only override the stubbed names when running under standalone
;; NeLisp (= `nl-ffi-call' is fboundp).  Under host Emacs this file is
;; a no-op via the gate, so it can be loaded unconditionally.

;;; Code:

(require 'emacs-network-ffi)
(require 'emacs-process-events)


(defconst emacs-server-polyfills--standalone-p
  (fboundp 'nl-ffi-call)
  "Non-nil when running under standalone NeLisp (= the in-process
libffi primitive is available).")


;;;; --- C-level variables that vendor server.el touches ----------------

;; Daemon-mode globals — nil since standalone is not an Emacs daemon.
(defvar internal--daemon-sockname nil)
(defvar internal--daemon-mode nil)

;; Init-time bookkeeping — server.el sometimes references these in
;; debug / startup-time messages.
(defvar before-init-time '(0 0 0 0))
(defvar after-init-time '(0 0 0 0))
(defvar emacs-pid 0)
(defvar load-in-progress t)

;; Minor mode book-keeping — `push'ed by `server-start' when it flips
;; `server-mode' on.
(defvar global-minor-modes nil)
;; server-start builds the listener :coding from this C-level scalar;
;; on the standalone reader an unbound variable reference hard-aborts
;; the whole form (it does not signal through `condition-case'), so
;; the defvar is load-bearing for M14 (2026-06-11).
(defvar locale-coding-system nil)

;; Frame / TTY globals — only consulted inside `daemonp' branches we
;; never take, but stubbed for safety.
(defvar terminal-frame t)

;; Top-level keymap server.el augments in its keybinding section.  A
;; 256-element no-op vector is enough to keep `define-key' stubs happy.
(defvar ctl-x-map (make-vector 256 nil))


;;;; --- user / system identity stubs -----------------------------------

(when emacs-server-polyfills--standalone-p
  (unless (fboundp 'user-uid)         (defun user-uid () 1000))
  (unless (fboundp 'user-real-uid)    (defun user-real-uid () (user-uid)))
  (unless (fboundp 'system-name)      (defun system-name () "standalone"))
  (unless (fboundp 'emacs-pid)        (defun emacs-pid () 0))
  (unless (fboundp 'daemonp)          (defun daemonp () nil))
  (unless (fboundp 'frame-list)       (defun frame-list () '(t)))
  (unless (fboundp 'selected-frame)   (defun selected-frame () t)))


;;;; --- featurep override for `:family local' -------------------------

(when emacs-server-polyfills--standalone-p
  (let ((old (and (fboundp 'featurep) (symbol-function 'featurep))))
    (defun featurep (feat &optional sub)
      "Polyfill: 2-arg `featurep' that recognises the `:family local'
sub-feature query vendor server.el / make-network-process callers use.
Delegates other queries to the underlying stub (which returns nil)."
      (cond
       ((and (eq feat 'make-network-process)
             (consp sub) (memq 'local sub))
        t)
       ((and (functionp old)
             (condition-case _ (progn (funcall old feat) t) (error nil)))
        (funcall old feat))
       (t nil)))))


;;;; --- file primitives via libc -----------------------------------------

(when emacs-server-polyfills--standalone-p

  (defun file-exists-p (path)
    "Polyfill: wrap libc `access(path, F_OK)'.  Returns t when the
path exists (= readable or writable or just present)."
    (and (stringp path)
         (let ((rc (nl-ffi-call emacs-network-ffi-libc-path
                                "access" [:sint32 :string :sint32]
                                path 0)))   ; F_OK = 0
           (and (integerp rc) (zerop rc)))))

  (defun file-attributes (path &optional _id-format)
    "Polyfill: synthesised attrs list for any existing PATH.

server-start uses `(file-attributes DIR \\='integer)' to confirm the
socket-dir is owned by us and has 0700 mode bits.  We return:
  (TYPE LINK-COUNT UID GID ATIME MTIME CTIME SIZE MODES UNUSED
   INODE DEVICE)
with TYPE = t (= directory) when `file-directory-p' agrees, else nil.
ACCESS times are zero since standalone has no real stat."
    (when (and (stringp path) (file-exists-p path))
      (let ((uid (user-uid)))
        (list (if (file-directory-p path) t nil)
              1 uid uid '(0 0) '(0 0) '(0 0) 0 "drwx------" nil 0 0))))

  (unless (fboundp 'file-attribute-type)
    (defun file-attribute-type (attrs) (nth 0 attrs)))
  (unless (fboundp 'file-attribute-link-number)
    (defun file-attribute-link-number (attrs) (nth 1 attrs)))
  (unless (fboundp 'file-attribute-user-id)
    (defun file-attribute-user-id (attrs) (nth 2 attrs)))
  (unless (fboundp 'file-attribute-group-id)
    (defun file-attribute-group-id (attrs) (nth 3 attrs)))
  (unless (fboundp 'file-attribute-modes)
    (defun file-attribute-modes (attrs) (nth 8 attrs))))

;; with-file-modes — the umask side effect is moot under standalone.
(unless (fboundp 'with-file-modes)
  (defmacro with-file-modes (_modes &rest body) `(progn ,@body)))

(when emacs-server-polyfills--standalone-p

  (defun emacs-server-polyfills--mkdir-1 (path)
    "Single-shot `mkdir' via libc.  Returns t on success or EEXIST."
    (let ((rc (nl-ffi-call emacs-network-ffi-libc-path
                           "mkdir" [:sint32 :string :sint32]
                           path #o700)))
      (cond
       ((and (integerp rc) (zerop rc)) t)
       (t (= 17 (emacs-network-ffi--errno))))))   ; EEXIST

  (defun make-directory (dir &optional parents)
    "Polyfill: `mkdir' via libc, with optional recursive `parents' flag."
    (let ((path (directory-file-name (expand-file-name dir))))
      (if parents
          (let ((parts (split-string path "/" t))
                (acc ""))
            (dolist (p parts)
              (setq acc (concat acc "/" p))
              (emacs-server-polyfills--mkdir-1 acc)))
        (emacs-server-polyfills--mkdir-1 path))
      nil)))


;;;; --- process plist accessors --------------------------------------

(when emacs-server-polyfills--standalone-p

  (defun process-put (process key value)
    "Polyfill: stash KEY=VALUE on PROCESS's plist (slot 8)."
    (let ((pl (emacs-process-events--get process 8)))
      (emacs-process-events--set process 8 (plist-put pl key value))
      value))

  (defun process-get (process key)
    "Polyfill: retrieve KEY from PROCESS's plist (slot 8)."
    (plist-get (emacs-process-events--get process 8) key)))


;;;; --- defvar-keymap stub ------------------------------------------

(unless (fboundp 'defvar-keymap)
  (defmacro defvar-keymap (name &rest _ignored)
    "Polyfill: minimal Emacs 29+ `defvar-keymap' stub that just declares
NAME as a nil variable — server.el's keybinding entries are not used
under standalone IPC."
    (list 'defvar name nil)))


;;;; --- misc small stubs ---------------------------------------------

(unless (fboundp 'substitute-command-keys)
  (defun substitute-command-keys (s) s))


(provide 'emacs-server-polyfills)

;;; emacs-server-polyfills.el ends here
