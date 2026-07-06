;;; emacs-toolbar.el --- GUI toolbar runtime helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Runtime-owned toolbar/dropdown semantics for the GUI bridge.  The
;; bridge still owns transport files; this module owns the default
;; toolbar spec, menu contents, hit testing, and click resolution.

;;; Code:

(defconst emacs-toolbar-gui-default-spec
  (concat "New\tC-x C-f\n"
          "Open\tC-x C-f\n"
          "Save\tC-x C-s\n"
          "Undo\tC-/\n"
          "Cut\tC-w\n"
          "Copy\tM-w\n"
          "Paste\tC-y\n"
          "Search\tC-s\n")
  "Default GUI toolbar spec as LABEL<TAB>KEYS lines.")

(defconst emacs-toolbar-gui-default-menus
  '(("New" . "Find File\tC-x C-f\nSwitch Buffer\tC-x b\n")
    ("Open" . "Open File\tC-x C-f\nOpen Read Only\tC-x C-r\nInsert File\tC-x i\n")
    ("Save" . "Save\tC-x C-s\nWrite File\tC-x C-w\nSave Some\tC-x s\n")
    ("Undo" . "Undo\tC-/\nRedo\tC-?\n")
    ("Cut" . "Cut\tC-w\nKill Line\tC-k\nDelete Region\tC-w\n")
    ("Copy" . "Copy\tM-w\nSelect All\tC-x h\n")
    ("Paste" . "Paste\tC-y\nYank Pop\tM-y\n")
    ("Search" . "Search Forward\tC-s\nSearch Backward\tC-r\nQuery Replace\tM-%\n"))
  "Default toolbar dropdown menus.")

(defvar emacs-toolbar-gui-backend nil
  "PLIST of GUI bridge toolbar backend callbacks.")

(defvar emacs-toolbar-gui-spec emacs-toolbar-gui-default-spec
  "Current GUI toolbar spec as LABEL<TAB>KEYS lines.")

(defvar emacs-toolbar-gui-cell-width-default 9
  "Fallback toolbar cell width in pixels.")

(defvar emacs-toolbar-gui-button-left-padding 6
  "Left pixel offset of the first toolbar button.")

(defvar emacs-toolbar-gui-button-extra-width 14
  "Non-text pixel width included in every toolbar button.")

(defvar emacs-toolbar-gui-menu-top 18
  "Y pixel coordinate where toolbar dropdown rows begin.")

(defvar emacs-toolbar-gui-menu-row-height 16
  "Dropdown row height in pixels.")

;;;###autoload
(defun emacs-toolbar-gui-register-backend (&rest backend)
  "Register BACKEND plist for GUI toolbar transport callbacks."
  (setq emacs-toolbar-gui-backend backend))

(defun emacs-toolbar-gui--backend-call (key &rest args)
  "Call toolbar backend function KEY with ARGS when registered."
  (let ((fn (and emacs-toolbar-gui-backend
                 (plist-get emacs-toolbar-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-toolbar-gui--digits-number (text)
  "Parse decimal digits from TEXT, ignoring non-digits."
  (let ((i 0)
        (n 0)
        (text (or text "")))
    (while (< i (length text))
      (let ((ch (aref text i)))
        (when (and (>= ch ?0) (<= ch ?9))
          (setq n (+ (* n 10) (- ch ?0)))))
      (setq i (1+ i)))
    n))

;;;###autoload
(defun emacs-toolbar-gui-cell-width ()
  "Return the GUI toolbar cell width in pixels."
  (let ((value (emacs-toolbar-gui--backend-call :cell-width)))
    (cond
     ((and (integerp value) (> value 0) (< value 256)) value)
     ((stringp value)
      (let ((n (emacs-toolbar-gui--digits-number value)))
        (if (and (> n 0) (< n 256))
            n
          emacs-toolbar-gui-cell-width-default)))
     (t emacs-toolbar-gui-cell-width-default))))

;;;###autoload
(defun emacs-toolbar-gui-write-state ()
  "Write the current toolbar state through the registered backend."
  (emacs-toolbar-gui--backend-call :write-state emacs-toolbar-gui-spec))

;;;###autoload
(defun emacs-toolbar-gui-write-menu (menu)
  "Write MENU through the registered toolbar backend."
  (emacs-toolbar-gui--backend-call :write-menu (or menu "")))

;;;###autoload
(defun emacs-toolbar-gui-current-menu ()
  "Read the currently open toolbar menu through the backend."
  (or (emacs-toolbar-gui--backend-call :read-menu) ""))

;;;###autoload
(defun emacs-toolbar-gui-menu-for-label (label)
  "Return dropdown menu text for toolbar LABEL."
  (or (cdr (assoc (or label "") emacs-toolbar-gui-default-menus))
      ""))

(defun emacs-toolbar-gui--entry-at-x (clickx)
  "Return toolbar entry plist at CLICKX, or nil."
  (let ((spec (or emacs-toolbar-gui-spec ""))
        (i 0)
        (tx emacs-toolbar-gui-button-left-padding)
        (found nil)
        (cw (emacs-toolbar-gui-cell-width)))
    (while (and (< i (length spec)) (not found))
      (let ((lstart i)
            (keys ""))
        (while (and (< i (length spec))
                    (/= (aref spec i) ?\t)
                    (/= (aref spec i) ?\n))
          (setq i (1+ i)))
        (let ((label (substring spec lstart i)))
          (when (and (< i (length spec)) (= (aref spec i) ?\t))
            (setq i (1+ i))
            (let ((ks i))
              (while (and (< i (length spec)) (/= (aref spec i) ?\n))
                (setq i (1+ i)))
              (setq keys (substring spec ks i))))
          (when (and (< i (length spec)) (= (aref spec i) ?\n))
            (setq i (1+ i)))
          (let ((bw (+ emacs-toolbar-gui-button-extra-width
                       (* (length label) cw))))
            (when (and (>= clickx tx) (< clickx (+ tx bw)))
              (setq found (list :label label :keys keys)))
            (setq tx (+ tx bw))))))
    found))

;;;###autoload
(defun emacs-toolbar-gui-keys-at-x (clickx)
  "Return the key sequence for the toolbar button at CLICKX."
  (or (plist-get (emacs-toolbar-gui--entry-at-x clickx) :keys) ""))

;;;###autoload
(defun emacs-toolbar-gui-label-at-x (clickx)
  "Return the label for the toolbar button at CLICKX."
  (or (plist-get (emacs-toolbar-gui--entry-at-x clickx) :label) ""))

;;;###autoload
(defun emacs-toolbar-gui-menu-keys-at-row (menu row)
  "Return key sequence from MENU at zero-based ROW."
  (let ((lines (split-string (or menu "") "\n" t))
        (line nil))
    (setq line (nth row lines))
    (if (and line (string-match "\t\\([^\t\n]+\\)\\'" line))
        (match-string 1 line)
      "")))

;;;###autoload
(defun emacs-toolbar-gui-parse-click (raw)
  "Parse RAW toolbar click text into (X . Y).
Old GUI builds sent only X; that form is treated as Y=0."
  (let ((i 0)
        (x 0)
        (y 0)
        (seen-comma nil)
        (raw (or raw "")))
    (while (< i (length raw))
      (let ((ch (aref raw i)))
        (cond
         ((= ch ?,)
          (setq seen-comma t))
         ((and (>= ch ?0) (<= ch ?9))
          (if seen-comma
              (setq y (+ (* y 10) (- ch ?0)))
            (setq x (+ (* x 10) (- ch ?0)))))))
      (setq i (1+ i)))
    (unless seen-comma
      (setq y 0))
    (cons x y)))

;;;###autoload
(defun emacs-toolbar-gui-handle-click (raw)
  "Resolve RAW toolbar click text and update dropdown backend state.
Return a plist with `:keys', `:command', `:effective-command', and
`:menu'.  Opening or cancelling a dropdown returns command `ignore'."
  (let* ((xy (emacs-toolbar-gui-parse-click raw))
         (cx (car xy))
         (cy (cdr xy)))
    (if (< cy emacs-toolbar-gui-menu-top)
        (let* ((label (emacs-toolbar-gui-label-at-x cx))
               (menu (emacs-toolbar-gui-menu-for-label label)))
          (if (equal menu "")
              (progn
                (emacs-toolbar-gui-write-menu "")
                (list :keys (emacs-toolbar-gui-keys-at-x cx)
                      :command nil
                      :effective-command ""
                      :menu ""))
            (emacs-toolbar-gui-write-menu menu)
            (list :keys ""
                  :command 'ignore
                  :effective-command "ignore"
                  :menu menu)))
      (let* ((menu (emacs-toolbar-gui-current-menu))
             (row (/ (- cy emacs-toolbar-gui-menu-top)
                     emacs-toolbar-gui-menu-row-height))
             (keys (emacs-toolbar-gui-menu-keys-at-row menu row)))
        (emacs-toolbar-gui-write-menu "")
        (if (equal keys "")
            (list :keys ""
                  :command 'ignore
                  :effective-command "ignore"
                  :menu "")
          (list :keys keys
                :command nil
                :effective-command ""
                :menu ""))))))

;;; Icon registry: TUI glyph resolution + GUI image asset resolution -----
;;
;; `nemacs-next-session-toolbar-render-line' (an app/session adapter)
;; resolves the `:icon' name on each toolbar item through
;; `emacs-toolbar-icon-glyph' instead of hard-coding TUI decoration
;; here.  GUI frontends can resolve the same names to a vendored image
;; through `emacs-toolbar-icon-file'.  Names match the vendor
;; `tool-bar-setup' basenames already used by
;; `nemacs-next-session-default-toolbar-spec' :icon slots
;; (new/open/diropen/close/save/undo/cut/copy/paste/search) and the
;; vendored image basenames under `vendor/emacs-etc/images/'.

(defconst emacs-toolbar-icon-registry
  '(("new"     . (:glyph "✚" :ascii "[N]" :file "new"))
    ("open"    . (:glyph "▶" :ascii "[O]" :file "open"))
    ("diropen" . (:glyph "▤" :ascii "[D]" :file "diropen"))
    ("close"   . (:glyph "✕" :ascii "[X]" :file "close"))
    ("save"    . (:glyph "▣" :ascii "[S]" :file "save"))
    ("undo"    . (:glyph "↺" :ascii "[U]" :file "undo"))
    ("cut"     . (:glyph "✂" :ascii "[K]" :file "cut"))
    ("copy"    . (:glyph "❐" :ascii "[W]" :file "copy"))
    ("paste"   . (:glyph "❏" :ascii "[Y]" :file "paste"))
    ("search"  . (:glyph "⌕" :ascii "[/]" :file "search")))
  "Toolbar icon name -> (:glyph UNICODE :ascii FALLBACK :file BASENAME).
Every `:glyph' is a single BMP codepoint outside the East Asian Wide /
emoji ranges in `emacs-string--build-char-width-table', so it always
measures as one display column under this runtime's `string-width'.
`:ascii' is a short bracketed mnemonic for non-UTF-8 terminals, loosely
tied to the underlying Emacs command (K = kill-region, W = kill-ring-save,
Y = yank).  `:file' is the vendored image basename resolved by
`emacs-toolbar-icon-file'.")

(defvar emacs-toolbar-icon-force-mode nil
  "Override automatic locale detection for icon glyph resolution.
Nil auto-detects Unicode capability from `LC_ALL'/`LC_CTYPE'/`LANG' (see
`emacs-toolbar-icon-unicode-capable-p').  Set to `unicode' or `ascii' to
force a mode regardless of the process environment.  Tests and
frontends that already know the terminal's capability out-of-band
should let-bind this instead of mutating environment variables.")

(defun emacs-toolbar-icon--locale-value ()
  "Return the most specific locale environment value, or nil.
Consults `LC_ALL', then `LC_CTYPE', then `LANG', matching POSIX locale
precedence."
  (let (found)
    (dolist (name '("LC_ALL" "LC_CTYPE" "LANG"))
      (unless found
        (let ((value (and (fboundp 'getenv) (getenv name))))
          (when (and (stringp value) (> (length value) 0))
            (setq found value)))))
    found))

(defun emacs-toolbar-icon-environment-unicode-p ()
  "Return non-nil when the locale environment declares a UTF-8 charset."
  (let ((value (emacs-toolbar-icon--locale-value)))
    (and value (string-match-p "utf-?8" (downcase value)) t)))

;;;###autoload
(defun emacs-toolbar-icon-unicode-capable-p ()
  "Return non-nil when toolbar icons should render as Unicode glyphs.
`emacs-toolbar-icon-force-mode' overrides detection when set; otherwise
this consults `emacs-toolbar-icon-environment-unicode-p'.  An
environment that declares nothing falls back to nil (ASCII) rather
than risk unreadable glyphs, matching this module's
silent-fallback-over-broken-output policy."
  (cond
   ((eq emacs-toolbar-icon-force-mode 'unicode) t)
   ((eq emacs-toolbar-icon-force-mode 'ascii) nil)
   (t (emacs-toolbar-icon-environment-unicode-p))))

;;;###autoload
(defun emacs-toolbar-icon-glyph (name)
  "Return the resolved icon prefix text for icon NAME.
Returns the registry's `:glyph' when the environment is Unicode-capable
\(see `emacs-toolbar-icon-unicode-capable-p'\), the `:ascii' fallback
otherwise, and \"\" for an unknown NAME so callers can silently omit
the icon instead of misrendering."
  (let ((entry (and (stringp name) (assoc name emacs-toolbar-icon-registry))))
    (if (not entry)
        ""
      (or (plist-get (cdr entry)
                      (if (emacs-toolbar-icon-unicode-capable-p) :glyph :ascii))
          ""))))

(defconst emacs-toolbar-icon-file-extensions '("xpm" "pbm")
  "Preferred vendored image extensions for toolbar icon resolution, in order.")

(defconst emacs-toolbar-icon--source-directory
  (and (boundp 'load-file-name)
       (stringp load-file-name)
       (fboundp 'file-name-directory)
       (file-name-directory load-file-name))
  "Directory this module was loaded from (src/), or nil.")

;;;###autoload
(defun emacs-toolbar-icon-file (name &optional directory)
  "Return the vendored GUI image path for icon NAME, or nil.
DIRECTORY overrides the default `vendor/emacs-etc/images/' lookup root,
which is otherwise resolved relative to this module's own source
directory when known (mirrors `emacs-startup-screen-image-path').
Prefers `.xpm' then `.pbm' and only returns a path that exists on disk;
returns nil for an unknown NAME or when no vendored asset is present so
GUI callers fall back to the text glyph instead of referencing a
missing file."
  (let ((entry (and (stringp name) (assoc name emacs-toolbar-icon-registry)))
        (dir (or directory
                 (and emacs-toolbar-icon--source-directory
                      (concat emacs-toolbar-icon--source-directory
                              "../vendor/emacs-etc/images/"))
                 "vendor/emacs-etc/images/"))
        found)
    (when (and entry (fboundp 'file-exists-p))
      (let ((base (plist-get (cdr entry) :file)))
        (when base
          (dolist (ext emacs-toolbar-icon-file-extensions)
            (let ((path (concat (file-name-as-directory dir) base "." ext)))
              (when (and (not found) (file-exists-p path))
                (setq found path)))))))
    found))

(provide 'emacs-toolbar)

;;; emacs-toolbar.el ends here
