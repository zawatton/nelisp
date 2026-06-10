;;; emacs-server-client-polyfills.el --- emacsclient round-trip polyfills -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; M14 server/emacsclient lane — the surface vendor server.el's
;; `server-process-filter' / `server-execute' / `server-eval-and-print'
;; path touches beyond what `emacs-server-polyfills.el' (K3,
;; server-start only) provides.  Together they let a REAL
;; `emacsclient -s SOCK -e EXPR' round-trip against the standalone
;; reader.
;;
;; Load order: emacs-stub(+bulk) → emacs-network-syscall-shim →
;; K1 network stack → emacs-server-polyfills → THIS FILE →
;; vendor/emacs-lisp/server.el → server-start → event loop.
;;
;; Design constraint discovered while building this lane (2026-06-11):
;; on the standalone reader a call to a MISSING function hard-aborts
;; the whole top-level form — it does NOT signal through
;; `condition-case' / `ignore-errors'.  Every stub below is therefore
;; load-bearing: it exists so the filter path never touches an
;; unbound function, even on branches that immediately no-op.
;;
;; The one behavioral override (installed AFTER vendor server.el
;; loads, see `emacs-server-client-polyfills-install') is
;; `server-eval-and-print': the vendor version renders the value
;; through a temp buffer + `pp' + `standard-output', which is far
;; more buffer machinery than the standalone substrate carries.  The
;; override keeps the same protocol (reply `-print' with quoted text)
;; via `prin1-to-string'.
;;
;; Out of scope (documented once): TCP servers (auth-key file,
;; `with-temp-file', `format-network-address' real formatting), file
;; visiting (`-file' clients), tty / window-system frames.  Those
;; commands answer through the normal server.el error path instead of
;; crashing.

;;; Code:

(defconst emacs-server-client-polyfills--standalone-p
  (fboundp 'syscall-direct)
  "Non-nil when running on the standalone reader (never host Emacs —
`syscall-direct' is a NeLisp-only primitive).")

(when emacs-server-client-polyfills--standalone-p

  ;; --- macros the filter path expands at run time -----------------------
  (defmacro with-local-quit (&rest body)
    `(progn ,@body))
  (defmacro with-temp-message (_message &rest body)
    `(progn ,@body))
  (defmacro when-let (spec &rest body)
    "Minimal single-binding `when-let' (enough for vendor server.el)."
    (let ((binding (car spec)))
      `(let ((,(car binding) ,(cadr binding)))
         (when ,(car binding)
           ,@body))))
  (defmacro setopt (sym val)
    `(setq ,sym ,val))
  (defmacro cl-assert (form &rest _args)
    `(progn ,form nil))

  ;; --- tiny pure functions ---------------------------------------------
  (defun length> (sequence n)
    (> (length sequence) n))
  (defun called-interactively-p (&optional _kind) nil)
  (defun minibuffer-depth () 0)
  (defun pp (object &optional _stream)
    (prin1-to-string object))
  (defun pp-to-string (object)
    (prin1-to-string object))
  (defun command-line-normalize-file-name (file) file)
  (defun substitute-key-definition (&rest _ignored) nil)
  (defun format-network-address (_address &optional _omit-port) "")
  (defun set-buffer-multibyte (_flag) nil)
  (defun getenv-internal (variable &optional _env)
    (if (fboundp 'getenv) (getenv variable) nil))

  ;; --- file ops over the raw syscall surface ----------------------------
  (defun delete-file (filename &optional _trash)
    (if (fboundp 'nelisp--syscall-path)
        (nelisp--syscall-path 87 filename)
      nil))
  (defun delete-directory (directory &optional _recursive _trash)
    (if (fboundp 'nelisp--syscall-path)
        (nelisp--syscall-path 84 directory)
      nil))
  (defun file-directory-p (filename)
    ;; st_mode is the u64 at offset 24 of struct stat; S_IFDIR = #o40000.
    (if (fboundp 'nelisp--syscall-stat-field)
        (let ((mode (nelisp--syscall-stat-field filename 24)))
          (and (integerp mode) (>= mode 0)
               (= (logand mode #o170000) #o40000)))
      nil))

  ;; --- terminal / frame / buffer surface the -eval path may brush -------
  (defvar last-nonmenu-event nil)
  (defvar process-environment nil)
  (defvar delete-by-moving-to-trash nil)
  (defvar coding-system-for-read nil)
  (defvar coding-system-for-write nil)
  (defvar version-control nil)
  (defvar use-dialog-box-override nil)
  (defun terminal-live-p (_terminal) nil)
  (defun frame-terminal (&optional _frame) nil)
  (defun delete-terminal (&rest _ignored) nil)
  (defun suspend-tty (&rest _ignored) nil)
  (defun resume-tty (&rest _ignored) nil)
  (defun window-minibuffer-p (&optional _window) nil)
  (defun one-window-p (&rest _ignored) t)
  (defun get-window-with-predicate (&rest _ignored) nil)
  (defun frame-first-window (&optional _frame) nil)
  (defun get-buffer-window (&rest _ignored) nil)
  (defun window-system-for-display (_display) nil)
  (defun make-frame-on-display (&rest _ignored) nil)
  (defun select-frame-set-input-focus (&rest _ignored) nil)
  (defun bury-buffer (&rest _ignored) nil)
  (defun next-buffer (&rest _ignored) nil)
  (defun pop-to-buffer (buffer &rest _ignored) buffer)
  (defun get-file-buffer (_filename) nil)
  (defun find-file-noselect (filename &rest _ignored)
    (error "emacs-server-client-polyfills: file visiting not wired (%s)"
           filename))
  (defun generate-new-buffer (name &optional _inhibit-hooks) name)
  (defun get-scratch-buffer-create () "*scratch*")
  (defun revert-buffer (&rest _ignored) nil)
  (defun save-buffer (&rest _ignored) nil)
  (defun write-file (&rest _ignored) nil)
  (defun save-some-buffers (&rest _ignored) nil)
  (defun save-buffers-kill-emacs (&rest _ignored) nil)
  (defun verify-visited-file-modtime (&optional _buf) t)
  (defun switch-to-buffer-preserve-window-point (&rest _ignored) nil)
  (defun file-name-history--add (_file) nil)
  (defun process-contact (process &optional key _no-block)
    "Minimal: only the plist keys server.el asks about."
    (if (and (fboundp 'process-get) key)
        (process-get process key)
      nil))
  (defun insert-file-contents (&rest _ignored)
    (error "emacs-server-client-polyfills: insert-file-contents not wired"))
  (defun insert-file-contents-literally (&rest _ignored)
    (error "emacs-server-client-polyfills: tcp auth file path not wired"))
  (defun isearch-cancel () nil)

  (defmacro with-no-warnings (&rest body)
    `(progn ,@body))

  ;; --- minimal emacsclient wire protocol helpers -------------------------
  ;;
  ;; The vendor `server-unquote-arg' / `server-quote-arg' run
  ;; `replace-regexp-in-string' with a lambda + `pcase' replacement and
  ;; the vendor `server-process-filter' parses the full command surface
  ;; (frames, tty, files, env).  On the standalone reader any missing
  ;; function inside that surface hard-aborts the whole event-loop form
  ;; (no condition-case can catch it), so M14 ships a deliberately
  ;; minimal, dependency-free filter for the local `-eval' subset and
  ;; leaves the full client surface as a documented omission.

  (defun emacs-server-client-polyfills--unquote (arg)
    "Remove &-quotation from ARG (wire format of emacsclient)."
    (let ((out "")
          (i 0)
          (n (length arg)))
      (while (< i n)
        (let ((c (aref arg i)))
          (if (and (= c ?&) (< (1+ i) n))
              (let ((next (aref arg (1+ i))))
                (setq out (concat out
                                  (cond ((= next ?&) "&")
                                        ((= next ?-) "-")
                                        ((= next ?n) "\n")
                                        (t " "))))
                (setq i (+ i 2)))
            (setq out (concat out (char-to-string c)))
            (setq i (1+ i)))))
      out))

  (defun emacs-server-client-polyfills--quote (arg)
    "Add &-quotation to ARG for the emacsclient wire."
    (let ((out "")
          (i 0)
          (n (length arg)))
      (while (< i n)
        (let ((c (aref arg i)))
          (setq out
                (concat out
                        (cond ((= c ?&) "&&")
                              ((= c ?\s) "&_")
                              ((= c ?\n) "&n")
                              ((and (= c ?-) (= i 0)) "&-")
                              (t (char-to-string c)))))
          (setq i (1+ i))))
      out))

  (defun emacs-server-client-polyfills--split (line)
    "Split LINE on single spaces, dropping empty tokens."
    (let ((out nil)
          (start 0)
          (i 0)
          (n (length line)))
      (while (<= i n)
        (if (or (= i n) (= (aref line i) ?\s))
            (progn
              (when (> i start)
                (setq out (cons (substring line start i) out)))
              (setq start (1+ i))
              (setq i (1+ i)))
          (setq i (1+ i))))
      (nreverse out)))

  ;; --- post-vendor-load overrides ----------------------------------------
  (defun emacs-server-client-polyfills-install ()
    "Install the M14 minimal `-eval' protocol overrides.
Call AFTER vendor server.el has loaded.  Replaces
`server-eval-and-print' (buffer-free printing) and
`server-process-filter' (local `-eval' subset; file/frame/tty client
commands are out of scope and simply ignored)."
    (defun server-eval-and-print (expr proc)
      "Evaluate EXPR as a string and reply the printed value to PROC."
      (let ((v (eval (car (read-from-string expr)) t)))
        (when proc
          (with-no-warnings
            (server-send-string
             proc
             (concat "-print "
                     (emacs-server-client-polyfills--quote
                      (prin1-to-string v))
                     "\n"))))))
    (defun server-process-filter (proc string)
      "M14 minimal filter: authenticate, handle `-eval', close."
      (let ((partial (or (process-get proc :m14-partial) "")))
        (setq string (concat partial string))
        (if (or (= 0 (length string))
                (not (= (aref string (1- (length string))) ?\n)))
            (process-put proc :m14-partial string)
          (process-put proc :m14-partial nil)
          (if (not (process-get proc :authenticated))
              (progn
                (server-send-string proc "-error Authentication failed\n")
                (delete-process proc))
            (let ((tokens (emacs-server-client-polyfills--split
                           (substring string 0 (1- (length string)))))
                  (exprs nil)
                  (files nil))
              (while tokens
                (cond
                 ((equal (car tokens) "-eval")
                  (when (cdr tokens)
                    (setq exprs
                          (cons (emacs-server-client-polyfills--unquote
                                 (car (cdr tokens)))
                                exprs))
                    (setq tokens (cdr tokens)))
                  (setq tokens (cdr tokens)))
                 ((equal (car tokens) "-file")
                  ;; M17: queue the file into the editor transport — the
                  ;; GUI session poll loop picks it up as a find-file.
                  ;; nemacs-cmd is the documented migration-fallback
                  ;; channel; full client-buffer lifecycle (wait, C-x #)
                  ;; stays out of scope.
                  (when (cdr tokens)
                    (setq files
                          (cons (emacs-server-client-polyfills--unquote
                                 (car (cdr tokens)))
                                files))
                    (setq tokens (cdr tokens)))
                  (setq tokens (cdr tokens)))
                 (t
                  ;; -dir / -current-frame / -env / -nowait / -tty /
                  ;; -position ... — out of the M14 subset.
                  (setq tokens (cdr tokens)))))
              (setq exprs (nreverse exprs))
              (setq files (nreverse files))
              (while files
                (nl-write-file "/tmp/nemacs-arg" (car files))
                (nl-write-file "/tmp/nemacs-keys" "")
                (nl-write-file "/tmp/nemacs-cmd" "find-file")
                (server-send-string
                 proc
                 (concat "-print "
                         (emacs-server-client-polyfills--quote
                          (concat "queued " (car files)))
                         "\n"))
                (setq files (cdr files)))
              (while exprs
                (server-eval-and-print (car exprs) proc)
                (setq exprs (cdr exprs)))
              (delete-process proc))))))
    (defun nemacs-server-start ()
      "M14 standalone server bring-up.
Vendor `server-start' still trips reader gaps inside its prologue;
this mirrors its socket setup exactly (safe dir, listener with the
authenticated plist, `server-process' bookkeeping) and relies on the
M14 filter override for the protocol."
      (server-ensure-safe-dir (file-name-directory (server--file-name)))
      (setq server-process
            (apply #'make-network-process
                   :name server-name
                   :server t
                   :noquery t
                   :sentinel #'server-sentinel
                   :filter #'server-process-filter
                   :use-external-socket t
                   :coding (cons 'raw-text-unix locale-coding-system)
                   (list :family 'local
                         :service (server--file-name)
                         :plist '(:authenticated t))))
      (process-put server-process :server-file (server--file-name))
      (setq server-mode t)
      server-process)
    t))

(provide 'emacs-server-client-polyfills)

;;; emacs-server-client-polyfills.el ends here
