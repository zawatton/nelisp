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

(defun emacs-server-polyfills--uid (uid)
  "Convert UID to an unsigned 32-bit user ID, or signal GNU's error."
  (let ((value uid))
    (when (consp value)
      (let ((high (car value))
            (low (if (consp (cdr value)) (cadr value) (cdr value)))
            (tail (and (consp (cdr value)) (cddr value))))
        ;; The obsolete (HIGH MIDDLE . LOW) representation appends a
        ;; 16-bit LOW component.  A valid LOW selects that format; otherwise GNU
        ;; treats the first two components as a pair of 16-bit integers.
        (setq value
              (and (integerp high) (integerp low)
                   (<= 0 high 65535) (<= 0 low 65535)
                   (if (and (integerp tail) (<= 0 tail 65535))
                       ;; A nonzero HIGH cannot fit an unsigned 32-bit UID.
                       (and (= high 0) (+ (* low 65536) tail))
                     (+ (* high 65536) low))))))
    (unless (and (numberp value) (<= 0 value #xffffffff)
                 (or (integerp value) (= value (truncate value))))
      (signal 'error
              '("Not an in-range integer, integral float, or cons of integers")))
    (truncate value)))

(defun emacs-server-polyfills--passwd (key)
  "Look up KEY in the operating system's user database.
Return the colon-separated account fields, or nil for an unknown account."
  (with-temp-buffer
    (when (eq (call-process "getent" nil t nil "passwd"
                            (if (stringp key) key (number-to-string key)))
              0)
      (let ((fields (split-string (buffer-string) ":")))
        ;; getent also accepts numeric keys; a string key names a login.
        (when (and (>= (length fields) 7)
                   (or (not (stringp key)) (equal key (car fields))))
          fields)))))

(when emacs-server-polyfills--standalone-p
  (unless (fboundp 'user-uid)         (defun user-uid () 1000))
  (unless (fboundp 'user-real-uid)    (defun user-real-uid () (user-uid)))
  (unless (fboundp 'system-name)      (defun system-name () "standalone"))
  (unless (fboundp 'emacs-pid)        (defun emacs-pid () 0))
  (unless (fboundp 'daemonp)          (defun daemonp () nil))
  (unless (fboundp 'frame-list)       (defun frame-list () '(t)))
  (unless (fboundp 'selected-frame)   (defun selected-frame () t))
  (unless (and (fboundp 'user-login-name)
               (not (get 'user-login-name 'emacs-stub-bulk)))
    (defun user-login-name (&optional uid)
      "Return the current login name, or the login name belonging to UID.
An unknown numeric user ID returns nil."
      (if (null uid)
          (if (boundp 'user-login-name)
              (symbol-value 'user-login-name)
            (or (getenv "LOGNAME") (getenv "USER")
                (car (emacs-server-polyfills--passwd (user-uid)))))
        (car (emacs-server-polyfills--passwd
              (emacs-server-polyfills--uid uid)))))
    (put 'user-login-name 'emacs-stub-bulk nil)))


;;;; --- featurep override for `:family local' -------------------------

(defun emacs-server-polyfills--subfeature-p (sub subfeatures)
  "Return t if SUB is `equal' to an element of SUBFEATURES.
Signal list errors with the original list as their data."
  (let ((tail subfeatures)
        (slow subfeatures)
        (advance nil)
        (found nil))
    (while (and (consp tail) (not found))
      (if (equal sub (car tail))
          (setq found t)
        (setq tail (cdr tail))
        (when advance
          (setq slow (cdr slow)))
        (setq advance (not advance))
        (when (and (consp tail) (eq tail slow))
          (signal 'circular-list (list subfeatures)))))
    (unless (or found (null tail))
      (signal 'wrong-type-argument (list 'listp subfeatures)))
    found))

(when emacs-server-polyfills--standalone-p
  ;; GNU starts with the `emacs' feature.  K1 supplies local sockets;
  ;; record that capability using the ordinary subfeature registry.
  (provide 'emacs)
  (provide 'make-network-process)
  (unless (member '(:family local) (get 'make-network-process 'subfeatures))
    (put 'make-network-process 'subfeatures
         (cons '(:family local) (get 'make-network-process 'subfeatures))))

  (defun featurep (feat &optional sub)
    "Return t if FEAT is present in `features'.
FEAT must be a symbol.  If SUB is non-nil, it must also occur, using
`equal', in FEAT's `subfeatures' property."
    (unless (symbolp feat)
      (signal 'wrong-type-argument (list 'symbolp feat)))
    (and (memq feat features)
         (or (null sub)
             (emacs-server-polyfills--subfeature-p
              sub (get feat 'subfeatures)))
         t)))


;;;; --- file primitives via libc -----------------------------------------

(when emacs-server-polyfills--standalone-p

  (unless (fboundp 'file-exists-p)
    (defun file-exists-p (path)
      "Polyfill: wrap libc `access(path, F_OK)'.  Returns t when the
path exists (= readable or writable or just present)."
      (and (stringp path)
           (let ((rc (nl-ffi-call emacs-network-ffi-libc-path
                                  "access" [:sint32 :string :sint32]
                                  path 0)))   ; F_OK = 0
             (and (integerp rc) (zerop rc))))))

  (unless (fboundp 'file-attributes)
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
		1 uid uid '(0 0) '(0 0) '(0 0) 0 "drwx------" nil 0 0)))))

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

  (unless (fboundp 'make-directory)
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
	nil))))


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


(provide 'emacs-server-polyfills)

;;; emacs-server-polyfills.el ends here
