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
;; `with-temp-file'), file
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
  (unless (fboundp 'when-let)
    (defmacro when-let (spec &rest body)
      "Minimal single-binding `when-let' (enough for vendor server.el)."
      (let ((binding (car spec)))
	`(let ((,(car binding) ,(cadr binding)))
           (when ,(car binding)
             ,@body)))))
  (defmacro setopt (sym val)
    `(setq ,sym ,val))
  (unless (fboundp 'cl-assert)
    (defmacro cl-assert (form &rest _args)
      `(progn ,form nil)))

  ;; --- tiny pure functions ---------------------------------------------
  (unless (fboundp 'length>)
    (defun length> (sequence n)
      (> (length sequence) n)))
  ;; These GNU variables must remain dynamic in the standalone closures.
  (defvar noninteractive)
  (defvar executing-kbd-macro)
  (defun called-interactively-p (&optional kind)
    "Return non-nil when the calling command was invoked interactively.
When KIND is `interactive', exclude batch mode and keyboard macros.
Other KIND values include all interactive calls.  The command loop's
dynamic flag approximates call-stack inspection."
    (and (boundp 'emacs-command-loop--called-interactively)
         emacs-command-loop--called-interactively
         (or (not (eq kind 'interactive))
             (not (or noninteractive
                      (and (boundp 'executing-kbd-macro)
                           executing-kbd-macro))))))
  (defun minibuffer-depth () 0)
  (defun pp (object &optional _stream)
    (prin1-to-string object))
  (defun pp-to-string (object)
    (prin1-to-string object))
  (provide 'pp)
  (defun command-line-normalize-file-name (file) file)
  (unless (fboundp 'substitute-key-definition)
    (defun substitute-key-definition (&rest _ignored) nil))
  (defun format-network-address (address &optional omit-port)
    "Convert an IPv4, IPv6, local, or unknown-family ADDRESS to text.
Return nil for an invalid address.  OMIT-PORT suppresses port formatting."
    (cond
     ((stringp address) address)
     ((consp address) (format "<Family %d>" (car address)))
     ((vectorp address)
      (let* ((n (length address))
             (ipv4 (or (= n 4) (= n 5)))
             (count (if ipv4 4 8))
             (valid (or ipv4 (= n 8) (= n 9)))
             (i 0)
             (out ""))
        (while (and valid (< i count))
          (let ((part (aref address i)))
            (if (and (integerp part) (>= part 0)
                     (<= part (if ipv4 255 65535)))
                (setq out (concat out (if (= i 0) "" (if ipv4 "." ":"))
                                  (format (if ipv4 "%d" "%x") part)))
              (setq valid nil)))
          (setq i (1+ i)))
        (when (and valid (not omit-port) (> n count))
          (let ((port (aref address count)))
            (if (and (integerp port) (>= port 0) (<= port 65535))
                (setq out (concat (if ipv4 out (concat "[" out "]"))
                                  ":" (number-to-string port)))
              (setq valid nil))))
        (and valid out)))))
  (unless (fboundp 'set-buffer-multibyte)
    (defun set-buffer-multibyte (_flag) nil))
  (defun getenv-internal (variable &optional env)
    "Find VARIABLE in ENV, or in `process-environment'.
An explicit environment list returns t for a negative entry."
    (unless (stringp variable)
      (signal 'wrong-type-argument (list 'stringp variable)))
    (let ((entries (if (listp env) env process-environment))
          (negative (listp env))
          (size (length variable))
          (found nil)
          (value nil))
      ;; A nil ENV, like any non-list ENV, selects the process environment.
      (unless env
        (setq entries process-environment negative nil))
      (while (and (consp entries) (not found))
        (let ((entry (car entries)))
          (when (and (stringp entry) (>= (length entry) size)
                     (equal variable (substring entry 0 size)))
            (cond
             ((= (length entry) size)
              (setq found t value (and negative t)))
             ((= (aref entry size) ?=)
              (setq found t value (substring entry (1+ size)))))))
        (setq entries (cdr entries)))
      value))

  ;; --- file ops over the raw syscall surface ----------------------------
  (unless (fboundp 'delete-file)
    (defun delete-file (filename &optional _trash)
      (if (fboundp 'nelisp--syscall-path)
          (nelisp--syscall-path 87 filename)
	nil)))
  (unless (fboundp 'delete-directory)
    (defun delete-directory (directory &optional _recursive _trash)
      (if (fboundp 'nelisp--syscall-path)
          (nelisp--syscall-path 84 directory)
	nil)))
  (unless (fboundp 'file-directory-p)
    (defun file-directory-p (filename)
      ;; st_mode is the u64 at offset 24 of struct stat; S_IFDIR = #o40000.
      (if (fboundp 'nelisp--syscall-stat-field)
          (let ((mode (nelisp--syscall-stat-field filename 24)))
            (and (integerp mode) (>= mode 0)
		 (= (logand mode #o170000) #o40000)))
	nil)))

  ;; --- terminal / frame / buffer surface the -eval path may brush -------
  (defvar last-nonmenu-event nil)
  (defvar process-environment nil)
  (defvar delete-by-moving-to-trash nil)
  (defvar coding-system-for-read nil)
  (defvar coding-system-for-write nil)
  (defvar version-control nil)
  (defvar use-dialog-box-override nil)
  (defvar emacs-server-client-polyfills--terminal nil
    "Opaque handle for the standalone's initial display terminal.")
  (defun terminal-live-p (terminal)
    "Return the output kind of a live TERMINAL or frame, or nil."
    (cond
     ((null terminal) (frame-live-p (selected-frame)))
     ((and emacs-server-client-polyfills--terminal
           (eq terminal emacs-server-client-polyfills--terminal))
      (frame-live-p (selected-frame)))
     (t (frame-live-p terminal))))
  (defun frame-terminal (&optional frame)
    "Return the display terminal of live FRAME, defaulting to the selected frame."
    (let ((frame (or frame (selected-frame))))
      (unless (frame-live-p frame)
        (signal 'wrong-type-argument (list 'frame-live-p frame)))
      ;; The headless frame model shares one initial display terminal.
      (or emacs-server-client-polyfills--terminal
          (setq emacs-server-client-polyfills--terminal
                (vector 'emacs-server-client-polyfills-terminal)))))
  (defun delete-terminal (&optional terminal force)
    "Delete TERMINAL; FORCE permits deleting the sole active terminal."
    (when (terminal-live-p terminal)
      (if force
          ;; GNU exits with status 70 when its last batch display disappears.
          (kill-emacs 70)
        (error "Attempt to delete the sole active display terminal"))))
  (defun suspend-tty (&optional tty)
    "Suspend the text terminal TTY, defaulting to the selected terminal."
    (unless (terminal-live-p tty)
      (signal 'wrong-type-argument (list 'terminal-live-p tty)))
    ;; The initial batch display is not a text terminal device.
    (error "Attempt to suspend a non-text terminal device"))
  (defun resume-tty (&optional tty)
    "Resume the text terminal TTY, defaulting to the selected terminal."
    (unless (terminal-live-p tty)
      (signal 'wrong-type-argument (list 'terminal-live-p tty)))
    (error "Attempt to resume a non-text terminal device"))
  (defun window-minibuffer-p (&optional window)
    (emacs-cc-census-display-b34window01--window-minibuffer-p window))
  (defun one-window-p (&rest _ignored) t)
  (defun get-window-with-predicate (&rest _ignored) nil)
  (defun frame-first-window (&optional frame-or-window)
    "Return the topmost leftmost live window of FRAME-OR-WINDOW's frame."
    (let* ((frame (cond
                   ((null frame-or-window) (selected-frame))
                   ((window-valid-p frame-or-window)
                    (window-frame frame-or-window))
                   ((frame-live-p frame-or-window) frame-or-window)
                   (t (signal 'wrong-type-argument
                              (list 'frame-live-p frame-or-window)))))
           (root (emacs-frame-root-window frame)))
      (unless root
        ;; The initial frame adopts the window module's implicit tree.
        (selected-window)
        (setq root emacs-window--root))
      (while (and root (not (window-live-p root)))
        (setq root (car (emacs-window-children root))))
      root))
  (defun get-buffer-window (&rest _ignored) nil)
  (defun window-system-for-display (_display) nil)
  (defun make-frame-on-display (&rest _ignored) nil)
  (defun select-frame-set-input-focus (&rest _ignored) nil)
  (defun bury-buffer (&rest _ignored) nil)
  (defun next-buffer (&rest _ignored) nil)
  (unless (fboundp 'pop-to-buffer)
    (defun pop-to-buffer (buffer &rest _ignored) buffer))
  (unless (fboundp 'get-file-buffer)
    (defun get-file-buffer (_filename) nil))
  (defun find-file-noselect (filename &rest _ignored)
    (error "emacs-server-client-polyfills: file visiting not wired (%s)"
           filename))
  ;; This defun used to be unconditional (a bare `name' return), unlike
  ;; every other stub in this block.  That is a real difference: unlike
  ;; `find-file-noselect' / `save-buffer' / etc. above, whose `fboundp' is
  ;; nil here, the standalone reader's OWN prelude already provides a
  ;; complete, self-consistent native `generate-new-buffer' /
  ;; `with-current-buffer' / `insert' / `buffer-string' / `current-buffer'
  ;; buffer family (measured 2026-09-28: works end to end with zero Elisp
  ;; loaded).  Installing this stub unconditionally silently discarded that
  ;; working native buffer and returned the bare NAME string instead, which
  ;; broke every later caller outside the emacsclient filter lane -- e.g.
  ;; `emacs-fileio.el's `find-file-noselect', which does
  ;; `(generate-new-buffer ...)' then `(with-current-buffer buf ...)' --
  ;; `with-current-buffer' then errored ("No such buffer") because BUF was
  ;; just a string with no buffer behind it (caught via the S6.4 usable-
  ;; progress smoke: `find-file-noselect' erroring before any edit/save).
  ;; Guard it like the rest of the codebase's bridges do (see CLAUDE.md /
  ;; AGENTS.md's API policy: "Host Emacs compatibility must not silently
  ;; override host C primitives unless the module is explicitly a
  ;; compatibility shim and is gated") so the native primitive wins when it
  ;; exists, and this remains the load-bearing never-void fallback only
  ;; when it does not.
  (unless (fboundp 'generate-new-buffer)
    (defun generate-new-buffer (name &optional _inhibit-hooks) name))
  (defun get-scratch-buffer-create () "*scratch*")
  (defun revert-buffer (&rest _ignored) nil)
  (defun save-buffer (&rest _ignored) nil)
  (defun write-file (&rest _ignored) nil)
  (defun save-some-buffers (&rest _ignored) nil)
  (defun save-buffers-kill-emacs (&rest _ignored) nil)
  (defun verify-visited-file-modtime (&optional _buf) t)
  (defun switch-to-buffer-preserve-window-point (&rest _ignored) nil)
  (defun file-name-history--add (_file) nil)
  (unless (fboundp 'process-contact)
    (defun process-contact (process &optional key _no-block)
      "Minimal: only the plist keys server.el asks about."
      (if (and (fboundp 'process-get) key)
          (process-get process key)
	nil)))
  ;; Never shadow a real implementation: newer NeLisp runtimes provide
  ;; `insert-file-contents' natively, and this erroring stub replaced it
  ;; unconditionally, breaking every later file read (S1.4 bootstrap load).
  (unless (fboundp 'insert-file-contents)
    (defun insert-file-contents (&rest _ignored)
      (error "emacs-server-client-polyfills: insert-file-contents not wired")))
  (unless (fboundp 'insert-file-contents-literally)
    (defun insert-file-contents-literally (&rest _ignored)
      (error "emacs-server-client-polyfills: tcp auth file path not wired")))
  (defun isearch-cancel () nil)

  (unless (fboundp 'with-no-warnings)
    (defmacro with-no-warnings (&rest body)
      `(progn ,@body)))

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

  (defvar emacs-server-client-polyfills--network-functions
    (let ((names '(processp process-list process-status process-id
                   process-buffer process-name process-filter
                   process-sentinel process-plist process-send-string
                   delete-process process-query-on-exit-flag
                   set-process-query-on-exit-flag
                   accept-process-output)))
      (let ((out nil))
        (dolist (name names)
          (when (fboundp name)
            (push (cons name (symbol-function name)) out)))
        out))
    "Network process functions captured before subprocess bridges load.")

  (defun emacs-server-client-polyfills--restore-network-functions ()
    "Restore network process/eventloop functions for server.el.
The full nemacs runtime loads subprocess bridges later; those bridges
are useful for shell/process-file, but their unprefixed process
aliases do not understand the network process vectors used by
server.el."
    (dolist (entry emacs-server-client-polyfills--network-functions)
      (fset (car entry) (cdr entry))))

  (defun nemacs-server-start ()
    "M14 standalone server bring-up.
Vendor `server-start' still trips reader gaps inside its prologue;
this mirrors its socket setup exactly (safe dir, listener with the
authenticated plist, `server-process' bookkeeping) and relies on the
M14 filter override for the protocol."
    (let ((server-file (server--file-name)))
      ;; The launcher pre-creates the socket directory with mode 0700.
      ;; Calling vendor `server-ensure-safe-dir' from this standalone
      ;; function currently triggers a reader control-flow bug where
      ;; the rest of the function body is skipped without a catchable
      ;; error, leaving a false "listening" message and no socket.
      (emacs-server-client-polyfills--restore-network-functions)
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
                         :service server-file
                         :plist '(:authenticated t))))
      (unless (and (processp server-process)
                   (eq (process-status server-process) 'listen))
        (error "nemacs-server-start: listener creation failed: %S"
               server-process))
      (unless (file-exists-p server-file)
        (error "nemacs-server-start: listener did not create socket: %s"
               server-file))
      (process-put server-process :server-file server-file)
      (setq server-mode t)
      server-process))

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
    t))

(provide 'emacs-server-client-polyfills)

;;; emacs-server-client-polyfills.el ends here
