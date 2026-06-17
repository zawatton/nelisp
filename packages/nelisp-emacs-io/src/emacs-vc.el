;;; emacs-vc.el --- minimal read-only VC (git) command semantics  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) read-only VC command semantics: status / diff /
;; log, backed by the `call-process' process substrate (git subprocess).
;;
;; Boundary (see ../nelisp-gui/docs/design/00-three-layer-architecture.org and
;; 04-completion-boundary-plan.org):
;;   - nelisp-emacs OWNS: VC backend command invocation, status/diff/log
;;     buffer *construction*, project/work-tree root detection.
;;   - nelisp-gui OWNS: rendering of the produced buffers + key transport.
;; This module therefore only computes state and fills buffers; it never makes
;; X11/display decisions.  Mutating VC commands are intentionally out of scope
;; (04 minimal order: read-only status/diff/log first).
;;
;; Only the git backend is implemented.  The subprocess is invoked through
;; `call-process' (host built-in under ERT; `emacs-process-call-process' under
;; the standalone reader), so the logic is validated host-side independently of
;; the Layer 1 reader.

;;; Code:

(require 'emacs-process-builtins)

(defvar emacs-vc-git-program "git"
  "Program used for the git VC backend.")

(defvar emacs-vc-log-max-entries 50
  "Maximum number of entries shown by `emacs-vc-print-log'.")

(defvar emacs-vc-diff-buffer-name "*vc-diff*"
  "Buffer name used by `emacs-vc-diff'.")

(defvar emacs-vc-log-buffer-name "*vc-change-log*"
  "Buffer name used by `emacs-vc-print-log'.")

(defvar emacs-vc-dir-buffer-name "*vc-dir*"
  "Buffer name used by `emacs-vc-dir'.")

(defvar emacs-vc-annotate-buffer-name "*vc-annotate*"
  "Buffer name used by `emacs-vc-annotate'.")

;;;; --- backend / work-tree root -------------------------------------

(defun emacs-vc--git-root (&optional dir)
  "Return the git work-tree root (a directory name) containing DIR, or nil.
Walks ancestors of DIR (default `default-directory') looking for `.git'."
  (let ((d (expand-file-name (or dir default-directory)))
        (result nil)
        (continue t))
    (while continue
      (cond
       ((file-exists-p (expand-file-name ".git" d))
        (setq result (file-name-as-directory d)
              continue nil))
       (t
        (let ((parent (file-name-directory (directory-file-name d))))
          (if (or (null parent) (string= parent d))
              (setq continue nil)
            (setq d parent))))))
    result))

(defun emacs-vc--call-process-available-p ()
  "Non-nil when a `call-process' substrate is reachable."
  (fboundp 'call-process))

(defvar emacs-vc--git-program-cache nil
  "Cached resolved git program path (see `emacs-vc--git-program').")

(defun emacs-vc--git-program ()
  "Resolve `emacs-vc-git-program' to an invokable path.
On the standalone reader `call-process' does no PATH lookup for a bare
command name and `executable-find' returns nil (the environment PATH is
unavailable), so fall back to probing common absolute locations.  On host
Emacs `executable-find' resolves the program directly."
  (or emacs-vc--git-program-cache
      (setq emacs-vc--git-program-cache
            (or (and (fboundp 'executable-find)
                     (executable-find emacs-vc-git-program))
                (and (not (file-name-absolute-p emacs-vc-git-program))
                     (let ((dirs '("/usr/bin" "/bin" "/usr/local/bin"))
                           (found nil))
                       (while (and dirs (not found))
                         (let ((p (expand-file-name emacs-vc-git-program
                                                    (car dirs))))
                           (when (file-executable-p p) (setq found p)))
                         (setq dirs (cdr dirs)))
                       found))
                emacs-vc-git-program))))

(defun emacs-vc--run-git (root &rest args)
  "Run git with ARGS against work-tree ROOT.
Return a cons (EXIT-CODE . OUTPUT-STRING).  EXIT-CODE is nil when no
`call-process' substrate is available."
  (if (not (emacs-vc--call-process-available-p))
      (cons nil "")
    (with-temp-buffer
      (let* ((dir (expand-file-name (or root default-directory)))
             (full-args (append (list "-C" dir) args))
             (code (apply #'call-process (emacs-vc--git-program) nil t nil
                          full-args)))
        (cons code (buffer-string))))))

;;;; --- status parsing -----------------------------------------------

(defun emacs-vc--parse-status (porcelain)
  "Parse git status --porcelain text PORCELAIN.
Return a list of (STATE . FILE), where STATE is the 2-char XY code with
surrounding whitespace preserved as git emits it (e.g. \" M\", \"??\")."
  (let ((lines (split-string (or porcelain "") "\n" t))
        (entries nil))
    (dolist (line lines)
      ;; porcelain v1: XY<space>PATH  (XY = 2 chars, then a single space)
      (when (> (length line) 3)
        (let ((state (substring line 0 2))
              (file (substring line 3)))
          (push (cons state file) entries))))
    (nreverse entries)))

(defun emacs-vc-status (&optional dir)
  "Return the parsed git status entries for the work-tree containing DIR.
Each entry is (STATE . FILE).  Returns nil when not in a git work-tree or
when no process substrate is available."
  (let ((root (emacs-vc--git-root dir)))
    (when root
      (emacs-vc--parse-status
       (cdr (emacs-vc--run-git root "status" "--porcelain"))))))

;;;; --- buffer construction (display belongs to Layer 3) -------------

(defun emacs-vc--show (bufname content)
  "Fill BUFNAME with CONTENT and request its display.  Return the buffer."
  (let ((buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content "")))
      (goto-char (point-min)))
    (when (fboundp 'display-buffer)
      (display-buffer buf))
    buf))

;;;; --- read-only commands -------------------------------------------

(defun emacs-vc-diff (&optional file)
  "Show `git diff' for FILE (or the whole work-tree) in `*vc-diff*'.
Returns the diff buffer."
  (interactive)
  (let ((root (emacs-vc--git-root)))
    (unless root
      (error "emacs-vc-diff: not inside a git work-tree"))
    (let ((res (if file
                   (emacs-vc--run-git root "diff" "--" file)
                 (emacs-vc--run-git root "diff"))))
      (emacs-vc--show emacs-vc-diff-buffer-name (cdr res)))))

(defun emacs-vc-print-log (&optional file)
  "Show `git log' (most recent first) in `*vc-change-log*'.
With FILE, restrict the log to that path.  Returns the log buffer."
  (interactive)
  (let ((root (emacs-vc--git-root)))
    (unless root
      (error "emacs-vc-print-log: not inside a git work-tree"))
    (let* ((args (append
                  (list "log"
                        (format "--max-count=%d" emacs-vc-log-max-entries)
                        "--pretty=format:%h %an %s")
                  (when file (list "--" file))))
           (res (apply #'emacs-vc--run-git root args)))
      (emacs-vc--show emacs-vc-log-buffer-name (cdr res)))))

(defun emacs-vc--format-status (entries)
  "Format parsed status ENTRIES into a `*vc-dir*' display string."
  (if (null entries)
      "(working tree clean)\n"
    (mapconcat (lambda (e) (format "%s  %s" (car e) (cdr e)))
               entries "\n")))

(defun emacs-vc-dir (&optional _dir)
  "Show `git status' for the current work-tree in `*vc-dir*'.
Returns the status buffer."
  (interactive)
  (let ((root (emacs-vc--git-root)))
    (unless root
      (error "emacs-vc-dir: not inside a git work-tree"))
    (let ((entries (emacs-vc--parse-status
                    (cdr (emacs-vc--run-git root "status" "--porcelain")))))
      (emacs-vc--show emacs-vc-dir-buffer-name
                      (emacs-vc--format-status entries)))))

(defun emacs-vc-annotate (&optional file)
  "Show `git blame' (line annotations) for FILE in `*vc-annotate*'.
FILE defaults to the visited file of the current buffer.  Returns the
annotation buffer."
  (interactive)
  (let ((root (emacs-vc--git-root))
        (target (or file (and (boundp 'buffer-file-name) buffer-file-name))))
    (unless root
      (error "emacs-vc-annotate: not inside a git work-tree"))
    (unless target
      (error "emacs-vc-annotate: no file to annotate"))
    (let ((res (emacs-vc--run-git root "blame" "--" target)))
      (emacs-vc--show emacs-vc-annotate-buffer-name (cdr res)))))

(defun emacs-vc-revision-diff (rev-a &optional rev-b file)
  "Show `git diff REV-A..REV-B' (or just REV-A when REV-B is nil).
With FILE, restrict the diff to that path.  Returns the diff buffer."
  (interactive "sOlder revision: \nsNewer revision (empty = working tree): ")
  (let ((root (emacs-vc--git-root)))
    (unless root
      (error "emacs-vc-revision-diff: not inside a git work-tree"))
    (let* ((range (if (and rev-b (> (length rev-b) 0))
                      (format "%s..%s" rev-a rev-b)
                    rev-a))
           (args (append (list "diff" range)
                         (when file (list "--" file))))
           (res (apply #'emacs-vc--run-git root args)))
      (emacs-vc--show emacs-vc-diff-buffer-name (cdr res)))))

;;;; --- key bindings -------------------------------------------------

(defun emacs-vc--install-bindings ()
  "Install the read-only VC key bindings on the global map (`C-x v' prefix)."
  (let ((map (and (fboundp 'current-global-map) (current-global-map))))
    (when (and map (fboundp 'define-key) (fboundp 'kbd))
      (define-key map (kbd "C-x v =") #'emacs-vc-diff)
      (define-key map (kbd "C-x v l") #'emacs-vc-print-log)
      (define-key map (kbd "C-x v d") #'emacs-vc-dir)
      (define-key map (kbd "C-x v g") #'emacs-vc-annotate))))

(emacs-vc--install-bindings)

;;;; --- standard-name facade install ---------------------------------

(defun emacs-vc-install ()
  "Bind the standard Emacs VC command names to the `emacs-vc-*' read-only
implementations, overriding any Tier 3 `unsupported' stub.  Called from the
`vc' loader (`vc.el'); NOT run on `require' so that a bare `(require
\\='emacs-vc)' does not touch the shared `vc-*' symbols (keeps ERT / Tier 3
facade tests isolated)."
  (defalias 'vc-diff #'emacs-vc-diff)
  (defalias 'vc-print-log #'emacs-vc-print-log)
  (defalias 'vc-dir #'emacs-vc-dir)
  (defalias 'vc-annotate #'emacs-vc-annotate))

(provide 'emacs-vc)

;;; emacs-vc.el ends here
