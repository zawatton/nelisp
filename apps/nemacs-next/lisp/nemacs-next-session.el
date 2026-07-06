;;; nemacs-next-session.el --- Session snapshot adapter for nemacs-next  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Thin app/session adapter for the nemacs-next protocol.  This module
;; reports editor state owned by reusable nelisp-emacs libraries; it does not
;; implement editing command semantics.

;;; Code:

(require 'nemacs-next)
(require 'emacs-toolbar)

(defconst nemacs-next-session-snapshot-version 0
  "Current nemacs-next snapshot payload version.")

(defconst nemacs-next-session-default-buffer-name "*scratch*"
  "Default buffer name used when the session has no current buffer yet.")

(defconst nemacs-next-session-gui-shell-capabilities
  '((:surface native-window :owner frontend :path shell)
    (:surface renderer :owner frontend :path shell)
    (:surface keyboard-input :owner frontend :path input)
    (:surface ime :owner frontend :path input)
    (:surface clipboard :owner frontend :path request)
    (:surface buffer-viewport :owner session :path snapshot)
    (:surface modeline :owner session :path snapshot)
    (:surface menu :owner frontend :path command)
    (:surface toolbar :owner frontend :path command))
  "M4 modern GUI shell capabilities exposed through the session protocol.")

(defconst nemacs-next-session-default-toolbar-spec
  '((:id "toolbar.new-file" :icon "new" :label "New File"
     :command "find-file" :vendor-command find-file
     :enable always :fidelity "find-file prompt; creates a new visited buffer when the chosen path does not exist")
    (:id "toolbar.open-file" :icon "open" :label "Open"
     :command "find-file" :vendor-command menu-find-file-existing
     :enable always :fidelity "find-file prompt using the existing file command path")
    (:id "toolbar.dired" :icon "diropen" :label "Dired"
     :command "dired" :vendor-command dired
     :enable always :fidelity "directory-listing buffer v1; real Dired marks and file operations are not claimed")
    (:id "toolbar.kill-buffer" :icon "close" :label "Close"
     :command "kill-buffer" :vendor-command kill-this-buffer
     :enable has-buffer :fidelity "kills the current session buffer through kill-buffer")
    (:id "toolbar.save-buffer" :icon "save" :label "Save"
     :command "save-buffer" :vendor-command save-buffer
     :enable has-buffer :fidelity "real save-buffer through reusable file I/O")
    (:id "separator-1" :separator t)
    (:id "toolbar.undo" :icon "undo" :label "Undo"
     :command "undo" :vendor-command undo
     :enable has-buffer :fidelity "single edit-group undo through emacs-undo")
    (:id "separator-2" :separator t)
    (:id "toolbar.cut" :icon "cut" :label "Cut"
     :command "kill-region" :vendor-command kill-region
     :enable region :fidelity "kill-region when :start and :end are supplied; no implicit GUI selection state in protocol v0")
    (:id "toolbar.copy" :icon "copy" :label "Copy"
     :command "copy-region-as-kill" :vendor-command copy-region-as-kill
     :enable region :fidelity "copy-region-as-kill when :start and :end are supplied; no implicit GUI selection state in protocol v0")
    (:id "toolbar.paste" :icon "paste" :label "Paste"
     :command "yank" :vendor-command yank
     :enable kill-ring :fidelity "real yank from the session kill ring")
    (:id "separator-3" :separator t)
    (:id "toolbar.search" :icon "search" :label "Search"
     :command "isearch-forward" :vendor-command isearch-forward
     :enable has-buffer :fidelity "search-prompt v1: prompts for a string and moves to the next match; no live incremental overlay yet"))
  "Vendor-derived default toolbar spec from `vendor/emacs-lisp/tool-bar.el'.")

(defvar nemacs-next-session-frame-width 80
  "Current protocol frame width in character cells for M4 snapshots.")

(defvar nemacs-next-session-frame-height 24
  "Current protocol frame height in character rows for M4 snapshots.")

(defvar nemacs-next-session-echo-message ""
  "Last echo-area message exported to the frontend shell.")

(defvar nemacs-next-session-frame-config-notes
  '((:field width :note "TUI treats width as terminal-constrained")
    (:field height :note "TUI treats height as terminal-constrained")
    (:field font :note "TUI ignores font; GUI frontends may consume it"))
  "Frame-config notes for frontends whose surface is terminal-constrained.")

(defvar nemacs-next-session--minibuffer-active nil
  "Non-nil while a generic session minibuffer prompt (M-x and its
chained follow-up prompts) is reading input.  Session `input' messages
route to the active prompt instead of buffer-editing commands while
this is set (see `nemacs-next-session--handle-input').  Lifecycle
state comes from `emacs-minibuffer-gui-enter-state'/
`emacs-minibuffer-gui-exit-state' -- the same GUI-neutral helpers
`nemacs-gtk--begin-prompt'/`nemacs-gtk--end-prompt' already apply to
GTK's own prompt vars -- so this never switches the session's current
buffer to a minibuffer buffer.")

(defvar nemacs-next-session--minibuffer-purpose nil
  "Symbol naming the active session minibuffer prompt's purpose, e.g.
`exec' for M-x, or `file'/`directory'/`buffer' for a chained follow-up
argument prompt.")

(defvar nemacs-next-session--minibuffer-prompt ""
  "Prompt string for the active session minibuffer.")

(defvar nemacs-next-session--minibuffer-input ""
  "Accumulated input text for the active session minibuffer.")

(defvar nemacs-next-session--minibuffer-on-confirm nil
  "Function called with the committed input string when the active
session minibuffer prompt is confirmed with Return.")

(defvar nemacs-next-session--minibuffer-completion-fn nil
  "Function of one argument (the current input) returning the raw
candidate collection for the active session minibuffer prompt.
Prefix filtering happens centrally in
`nemacs-next-session--minibuffer-candidates-for' via
`emacs-minibuffer-all-completions'.")

(defvar nemacs-next-session--minibuffer-candidates nil
  "Cached, already prefix-filtered completion candidates for the
active session minibuffer prompt's current input.")

(defvar nemacs-next-session--minibuffer-depth 0
  "Recursive session-minibuffer nesting depth (0 = inactive).  D-1
tracks this only for display -- a `[N]' recursion indicator on the
prompt line -- it does not implement a real recursive-minibuffer
stack: entering a new prompt while one is already active overwrites
the active prompt's state instead of suspending it underneath a
resumable outer read.  Full recursive-minibuffer command-loop
semantics land with the command-loop integration (D-2).")

(defconst nemacs-next-session-minibuffer-reserved-lines 2
  "Frame rows the session reserves for an active minibuffer prompt: one
candidate-summary row and one prompt/input row, mirrored to frontends
through `nemacs-next-session-frame-snapshot's `:minibuffer-lines'.")

(defun nemacs-next-session--string-equal (a b)
  "Return non-nil when A and B name the same protocol atom."
  (let ((as (if (symbolp a) (symbol-name a) a))
        (bs (if (symbolp b) (symbol-name b) b)))
    (and (stringp as)
         (stringp bs)
         (string= as bs))))

(defun nemacs-next-session--alist-get (key alist)
  "Return KEY's value from ALIST using `eq' comparison."
  (let (found)
    (while (and alist (not found))
      (let ((cell (car alist)))
        (when (and (consp cell) (eq (car cell) key))
          (setq found cell)))
      (setq alist (cdr alist)))
    (and found (cdr found))))

(defun nemacs-next-session--frame-param (key)
  "Return frame parameter KEY from initial/default frame alists."
  (let ((initial (and (boundp 'initial-frame-alist)
                      (nemacs-next-session--alist-get
                       key initial-frame-alist)))
        (default (and (boundp 'default-frame-alist)
                      (nemacs-next-session--alist-get
                       key default-frame-alist))))
    (if initial initial default)))

(defun nemacs-next-session-frame-config ()
  "Return the protocol V0 frame-config message.
The payload is computed after `nemacs-init' has had a chance to load
early-init.el and init.el, so user frame alist changes are visible before the
first frame snapshot."
  (let* ((tool-bar-lines
          (or (nemacs-next-session--frame-param 'tool-bar-lines)
              (and (boundp 'tool-bar-mode)
                   (not tool-bar-mode)
                   0)
              1))
         (width (nemacs-next-session--frame-param 'width))
         (height (nemacs-next-session--frame-param 'height))
         (font (nemacs-next-session--frame-param 'font)))
    (list :type 'frame-config
          :tool-bar-lines tool-bar-lines
          :width width
          :height height
          :font font
          :notes nemacs-next-session-frame-config-notes)))

(defun nemacs-next-session--buffer-name (buffer)
  "Return BUFFER's name using the reusable buffer API."
  (cond
   ((and buffer
         (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer)
         (fboundp 'nelisp-ec-buffer-name))
    (nelisp-ec-buffer-name buffer))
   ((and buffer (fboundp 'buffer-name))
    (buffer-name buffer))
   (t nil)))

(defun nemacs-next-session--buffer-file-name (buffer)
  "Return BUFFER's visited file name through reusable file I/O APIs."
  (let (path)
    (when (and (null path)
               (fboundp 'emacs-fileio-buffer-file-direct)
               buffer)
      (setq path (emacs-fileio-buffer-file-direct buffer)))
    (when (and (or (not (stringp path))
                   (equal path "nelisp--unbound-marker"))
               (boundp 'emacs-fileio--buffer-files))
      (setq path (cdr (assq buffer emacs-fileio--buffer-files))))
    (when (and (or (not (stringp path))
                   (equal path "nelisp--unbound-marker"))
               (fboundp 'emacs-fileio-buffer-file-name)
               buffer)
      (setq path (emacs-fileio-buffer-file-name buffer)))
    (when (and (or (not (stringp path))
                   (equal path "nelisp--unbound-marker"))
               (fboundp 'buffer-file-name)
               buffer)
      (setq path
            (condition-case nil
                (buffer-file-name buffer)
              (error nil))))
    (and (stringp path)
         (not (equal path "nelisp--unbound-marker"))
         path)))

(defun nemacs-next-session--buffer-modified-p (buffer)
  "Return non-nil when BUFFER has unsaved changes."
  (cond
   ((and (fboundp 'emacs-buffer-buffer-modified-p) buffer)
    (emacs-buffer-buffer-modified-p buffer))
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer)
         (fboundp 'nelisp-ec-buffer-modified-p)
         buffer)
    (nelisp-ec-buffer-modified-p buffer))
   ((and (fboundp 'buffer-modified-p) buffer)
    (condition-case nil
        (buffer-modified-p buffer)
      (error nil)))
   (t nil)))

(defun nemacs-next-session--with-buffer (buffer thunk)
  "Call THUNK with BUFFER current when possible."
  (if (and buffer (fboundp 'nelisp-ec-set-buffer))
      (let ((previous (and (fboundp 'nelisp-ec-current-buffer)
                           (nelisp-ec-current-buffer))))
        (unwind-protect
            (progn
              (nelisp-ec-set-buffer buffer)
              (funcall thunk))
          (when previous
            (nelisp-ec-set-buffer previous))))
    (funcall thunk)))

(defun nemacs-next-session-current-buffer-or-create (&optional name)
  "Return the current buffer, creating NAME when there is none.
This is session assembly only; buffer allocation is delegated to
`nelisp-ec-generate-new-buffer'."
  (or (and (fboundp 'nelisp-ec-current-buffer)
           (nelisp-ec-current-buffer))
      (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                         (nelisp-ec-generate-new-buffer
                          (or name nemacs-next-session-default-buffer-name)))))
        (when (and buffer (fboundp 'nelisp-ec-set-buffer))
          (nelisp-ec-set-buffer buffer))
        buffer)))

(defun nemacs-next-session-apply-startup-screen ()
  "Select the startup splash buffer as the session's initial buffer.
When the `emacs-startup-screen' owner is loaded and its gate
\(`emacs-startup-screen-use-p') allows the splash — no
`inhibit-startup-screen', no loaded user init file, no CLI file
arguments — create the `*GNU Emacs*' buffer and make it current, so the
first `frame-snapshot' renders the welcome screen without any frontend
change.  Returns the splash buffer when selected, nil otherwise.  This
is session assembly only; the splash semantics are owned by
`emacs-startup-screen'."
  (when (and (fboundp 'emacs-startup-screen-use-p)
             (fboundp 'emacs-startup-screen-select)
             (emacs-startup-screen-use-p))
    (emacs-startup-screen-select)))

(defun nemacs-next-session-buffer-snapshot (&optional buffer)
  "Return a protocol snapshot plist for BUFFER or the current buffer.
The text, point, and size are read through reusable buffer APIs."
  (let ((target (or buffer
                    (and (fboundp 'nelisp-ec-current-buffer)
                         (nelisp-ec-current-buffer)))))
    (nemacs-next-session--with-buffer
     target
     (lambda ()
       (list :type 'snapshot
             :version nemacs-next-session-snapshot-version
             :protocol-version nemacs-next-protocol-version
             :buffer-name (nemacs-next-session--buffer-name target)
             :file-name (nemacs-next-session--buffer-file-name target)
             :modified (and target
                            (nemacs-next-session--buffer-modified-p target))
             :point (and (fboundp 'nelisp-ec-point)
                         (nelisp-ec-point))
             :point-min (and (fboundp 'nelisp-ec-point-min)
                             (nelisp-ec-point-min))
             :point-max (and (fboundp 'nelisp-ec-point-max)
                             (nelisp-ec-point-max))
             :size (and (fboundp 'nelisp-ec-buffer-size)
                        (nelisp-ec-buffer-size))
             :text (and (fboundp 'nelisp-ec-buffer-string)
                        (nelisp-ec-buffer-string)))))))

(defun nemacs-next-session--split-lines (text)
  "Return TEXT split into display lines."
  (let ((start 0)
        lines)
    (while (string-match "\n" text start)
      (setq lines (cons (substring text start (match-beginning 0)) lines))
      (setq start (match-end 0)))
    (nreverse (cons (substring text start) lines))))

(defun nemacs-next-session--truncate-line (line width)
  "Return LINE truncated to WIDTH display cells."
  (if (and (integerp width)
           (> width 0)
           (> (length line) width))
      (substring line 0 width)
    line))

(defun nemacs-next-session--face-symbol (face)
  "Return the first concrete face symbol in FACE."
  (cond
   ((symbolp face) face)
   ((and (consp face) (symbolp (car face))) (car face))
   (t nil)))

(defun nemacs-next-session--face-color (face attribute)
  "Return FACE's ATTRIBUTE color when the face substrate can answer it."
  (let ((symbol (nemacs-next-session--face-symbol face)))
    (and symbol
         (fboundp 'emacs-faces-attribute)
         (let ((value (emacs-faces-attribute symbol attribute nil t)))
           (and (not (eq value 'unspecified)) value)))))

(defun nemacs-next-session--face-weight (face)
  "Return FACE's weight attribute when available."
  (nemacs-next-session--face-color face :weight))

(defun nemacs-next-session--org-heading-face (line)
  "Return an Org heading face for LINE, or nil."
  (let ((i 0)
        (n (length line)))
    (while (and (< i n) (= (aref line i) ?*))
      (setq i (+ i 1)))
    (and (> i 0)
         (< i n)
         (= (aref line i) ?\s)
         (cond
          ((= i 1) 'org-level-1)
          ((= i 2) 'org-level-2)
          ((= i 3) 'org-level-3)
          (t 'org-level-4)))))

(defun nemacs-next-session--face-run (start end face)
  "Return a protocol face run for START..END carrying FACE."
  (let ((run (list :start start
                   :end end
                   :face (nemacs-next-session--face-symbol face)))
        (fg (nemacs-next-session--face-color face :foreground))
        (bg (nemacs-next-session--face-color face :background))
        (weight (nemacs-next-session--face-weight face)))
    (when fg
      (setq run (append run (list :foreground fg))))
    (when bg
      (setq run (append run (list :background bg))))
    (when weight
      (setq run (append run (list :weight weight))))
    run))

(defun nemacs-next-session--line-face-runs (buffer line line-start line-width)
  "Return face runs for BUFFER LINE at LINE-START clipped to LINE-WIDTH."
  (let ((line-end (+ line-start (length line)))
        runs)
    (when (and buffer
               (> line-width 0)
               (fboundp 'emacs-buffer-text-property-view))
      (dolist (span (emacs-buffer-text-property-view
                     line-start line-end '(face font-lock-face) buffer))
        (let* ((start (nth 0 span))
               (end (nth 1 span))
               (props (nth 2 span))
               (face (or (plist-get props 'face)
                         (plist-get props 'font-lock-face))))
          (when face
            (setq runs
                  (cons (nemacs-next-session--face-run
                         (- start line-start)
                         (min line-width (- end line-start))
                         face)
                        runs))))))
    (when (and (null runs) (> line-width 0))
      (let ((org-face (nemacs-next-session--org-heading-face line)))
        (when org-face
          (setq runs (list (nemacs-next-session--face-run
                            0 line-width org-face))))))
    (nreverse runs)))

(defun nemacs-next-session--maybe-fontify-buffer (buffer)
  "Refresh font-lock for BUFFER when the substrate has active font-lock."
  (when buffer
    (nemacs-next-session--with-buffer
     buffer
     (lambda ()
       (cond
        ((and (fboundp 'emacs-font-lock-flush-pending)
              (fboundp 'emacs-font-lock-mode-enabled-p)
              (emacs-font-lock-mode-enabled-p buffer))
         (emacs-font-lock-flush-pending buffer))
        ((and (fboundp 'font-lock-fontify-buffer)
              (boundp 'font-lock-mode)
              font-lock-mode)
         (font-lock-fontify-buffer)))))))

(defun nemacs-next-session--viewport-lines (text width height &optional buffer)
  "Return a viewport line list for TEXT, WIDTH, HEIGHT, and BUFFER."
  (let ((lines (nemacs-next-session--split-lines (or text "")))
        (row 0)
        (line-start 1)
        out)
    (while (and lines (< row height))
      (let* ((raw (car lines))
             (line (nemacs-next-session--truncate-line raw width))
             (face-runs (nemacs-next-session--line-face-runs
                         buffer line line-start (length line))))
        (setq out
              (cons (append (list :row row :text line)
                            (when face-runs
                              (list :face-runs face-runs)))
                    out))
        (setq line-start (+ line-start (length raw) 1)))
      (setq row (+ row 1))
      (setq lines (cdr lines)))
    (while (< row height)
      (setq out (cons (list :row row :text "") out))
      (setq row (+ row 1)))
    (nreverse out)))

(defun nemacs-next-session--cursor-position (text point)
  "Return cursor row/column for 1-based POINT in TEXT."
  (let ((limit (max 0 (1- (or point 1))))
        (i 0)
        (row 0)
        (column 0))
    (while (and (< i limit) (< i (length text)))
      (if (= (aref text i) ?\n)
          (progn
            (setq row (+ row 1))
            (setq column 0))
        (setq column (+ column 1)))
      (setq i (+ i 1)))
    (list :row row :column column)))

(defun nemacs-next-session--mode-line (snapshot width)
  "Return a modeline string for SNAPSHOT clipped to WIDTH."
  (let* ((name (or (plist-get snapshot :buffer-name) "<no-buffer>"))
         (file (or (plist-get snapshot :file-name) ""))
         (modified (if (plist-get snapshot :modified) "**" "--"))
         (raw (format " %s  %s  %s  point:%s "
                      modified name file (or (plist-get snapshot :point) "?"))))
    (nemacs-next-session--truncate-line raw width)))

(defun nemacs-next-session--toolbar-item-enabled-p (item)
  "Return non-nil when toolbar ITEM should be enabled in protocol v0."
  (let ((predicate (plist-get item :enable)))
    (cond
     ((plist-get item :separator) nil)
     ((eq predicate 'always) t)
     ((eq predicate 'has-buffer)
      (and (nemacs-next-session-current-buffer-or-create) t))
     ((eq predicate 'kill-ring)
      (and (boundp 'kill-ring) kill-ring t))
     ((eq predicate 'region)
      nil)
     (t t))))

(defun nemacs-next-session-toolbar-items ()
  "Return structured default toolbar items for the current session state."
  (let (items)
    (dolist (item nemacs-next-session-default-toolbar-spec (nreverse items))
      (setq items
            (cons
             (if (plist-get item :separator)
                 (list :id (plist-get item :id) :separator t)
               (list :id (plist-get item :id)
                     :icon (plist-get item :icon)
                     :label (plist-get item :label)
                     :command (plist-get item :command)
                     :vendor-command (plist-get item :vendor-command)
                     :enabled (nemacs-next-session--toolbar-item-enabled-p item)
                     :fidelity (plist-get item :fidelity)))
             items)))))

(defun nemacs-next-session--toolbar-icon-prefix (icon)
  "Return the resolved glyph prefix for toolbar ICON name, or \"\".
Delegates to `emacs-toolbar-icon-glyph' (the reusable icon registry
owner); an unknown ICON name or a missing `emacs-toolbar' resolves to
\"\" so the caller silently falls back to a label-only button instead
of misrendering."
  (if (and icon (fboundp 'emacs-toolbar-icon-glyph))
      (or (emacs-toolbar-icon-glyph icon) "")
    ""))

(defun nemacs-next-session-toolbar-render-line (&optional selected focused width)
  "Return a plain text toolbar line for SELECTED item index and WIDTH."
  (let ((index 0)
        (line ""))
    (dolist (item (nemacs-next-session-toolbar-items))
      (if (plist-get item :separator)
          (setq line (concat line " |"))
        (let* ((icon (nemacs-next-session--toolbar-icon-prefix
                      (plist-get item :icon)))
               (label (plist-get item :label))
               (label (if (> (length icon) 0) (concat icon " " label) label))
               (enabled (plist-get item :enabled))
               (text (cond
                      ((and focused (= index (or selected 0)))
                       (format ">{%s}<" label))
                      ((= index (or selected -1))
                       (format "[%s]" label))
                      (enabled
                       (format " %s " label))
                      (t
                       (format " (%s) " label)))))
          (setq line (concat line text))
          (setq index (1+ index)))))
    (nemacs-next-session--truncate-line line (or width nemacs-next-session-frame-width))))

(defun nemacs-next-session-tool-bar-lines ()
  "Return the configured number of toolbar rows for protocol snapshots."
  (let ((value (plist-get (nemacs-next-session-frame-config) :tool-bar-lines)))
    (if (and (integerp value) (> value 0)) value 0)))

(defun nemacs-next-session-frame-snapshot (&optional width height)
  "Return a complete M4 frame snapshot for the current buffer.
WIDTH and HEIGHT, when supplied, update the session frame geometry."
  (when (and (integerp width) (> width 0))
    (setq nemacs-next-session-frame-width width))
  (when (and (integerp height) (> height 1))
    (setq nemacs-next-session-frame-height height))
  (let* ((buffer (nemacs-next-session-current-buffer-or-create))
         (_fontified (nemacs-next-session--maybe-fontify-buffer buffer))
         (snapshot
          (nemacs-next-session-buffer-snapshot buffer))
         (frame-width nemacs-next-session-frame-width)
         (frame-height nemacs-next-session-frame-height)
         (tool-bar-lines (nemacs-next-session-tool-bar-lines))
         (minibuffer-lines (nemacs-next-session-minibuffer-lines))
         (text (or (plist-get snapshot :text) ""))
         (body-height (max 1 (- frame-height 1 tool-bar-lines minibuffer-lines)))
         (toolbar (nemacs-next-session-toolbar-items))
         (frame
          (append
           (list :id "main"
                 :width frame-width
                 :height frame-height
                 :tool-bar-lines tool-bar-lines
                 :minibuffer-lines minibuffer-lines)
           (when (> tool-bar-lines 0)
             (list :toolbar toolbar))
           (when (> minibuffer-lines 0)
             (list :minibuffer
                   (nemacs-next-session--minibuffer-payload frame-width)))
           (list :cursor (nemacs-next-session--cursor-position
                          text (plist-get snapshot :point))
                 :viewport (nemacs-next-session--viewport-lines
                            text frame-width body-height buffer)
                 :mode-line (nemacs-next-session--mode-line
                             snapshot frame-width)
                 :echo nemacs-next-session-echo-message))))
    (list :type 'snapshot
          :version nemacs-next-session-snapshot-version
          :protocol-version nemacs-next-protocol-version
          :frame frame
          :buffers (list snapshot))))

(defun nemacs-next-session-frame-delta (reason snapshot)
  "Return a frame delta for REASON using SNAPSHOT as the new frame state."
  (list :type 'delta
        :frame (plist-get snapshot :frame)
        :changes (list (list :reason reason))))

(defun nemacs-next-session-render-frame-text (&optional snapshot)
  "Render SNAPSHOT, or the current frame snapshot, as plain text.
This is a smoke/demo renderer for the M4 protocol frame payload; it is not
a replacement for the native GUI renderer."
  (let* ((state (or snapshot (nemacs-next-session-frame-snapshot)))
         (frame (plist-get state :frame))
         (lines (plist-get frame :viewport))
         (minibuffer (plist-get frame :minibuffer))
         (out ""))
    (dolist (line lines)
      (setq out
            (concat out
                    (plist-get line :text)
                    "\n")))
    (setq out (concat out (plist-get frame :mode-line) "\n"))
    (when minibuffer
      (setq out (concat out
                        (or (plist-get minibuffer :candidates-line) "") "\n"
                        (or (plist-get minibuffer :prompt) "")
                        (or (plist-get minibuffer :contents) "") "\n")))
    (when (> (length (or (plist-get frame :echo) "")) 0)
      (setq out (concat out "echo: " (plist-get frame :echo) "\n")))
    out))

(defun nemacs-next-session-menu-model ()
  "Return the M4 menu/toolbar model.
Items name protocol commands.  They do not duplicate command semantics in
frontend code."
  (list :type 'menu
        :items
        (list
         (list :id "file.open" :label "Open" :command "find-file")
         (list :id "file.save" :label "Save" :command "save-buffer")
         (list :id "buffer.switch" :label "Switch Buffer"
               :command "switch-to-buffer")
         (list :id "buffer.kill" :label "Kill Buffer"
               :command "kill-buffer"))
        :toolbar
        (list
         (list :id "toolbar.open" :label "Open" :command "find-file")
         (list :id "toolbar.save" :label "Save" :command "save-buffer")
         (list :id "toolbar.undo" :label "Undo" :command "undo"))
        :default-toolbar (nemacs-next-session-toolbar-items)))

(defun nemacs-next-session--clipboard-request (op &optional payload)
  "Return a frontend clipboard request for OP and optional PAYLOAD."
  (append (list :type 'request
                :request-id (format "clipboard-%s" op)
                :op op)
          (when payload
            (list :payload payload))))

(defun nemacs-next-session--handle-input (message)
  "Handle an M4 frontend input MESSAGE and return a frame delta.
Routes to the active session minibuffer prompt first (M-x, or a
chained follow-up) when one is active; otherwise dispatches to the
current buffer's editing commands as before."
  (if nemacs-next-session--minibuffer-active
      (nemacs-next-session--minibuffer-handle-input message)
    (let* ((event (plist-get message :event))
           (text (or (and (listp event) (plist-get event :text))
                     (and (listp event) (plist-get event :commit))
                     (plist-get message :text)))
           (key (or (and (listp event) (plist-get event :key))
                    (plist-get message :key)))
           response)
      (cond
       ((stringp text)
        (setq response
              (nemacs-next-session-handle-command
               (list :type 'command :name 'insert-text :text text))))
       ((or (nemacs-next-session--string-equal key 'return)
            (nemacs-next-session--string-equal key "RET")
            (nemacs-next-session--string-equal key "Enter"))
        (setq response
              (nemacs-next-session-handle-command
               '(:type command :name newline))))
       ((or (nemacs-next-session--string-equal key 'backspace)
            (nemacs-next-session--string-equal key "Backspace"))
        (setq response
              (nemacs-next-session-handle-command
               '(:type command :name delete-char :count -1))))
       (t
        (setq response
              (nemacs-next-session-error
               'bad-input "input requires :event text/commit or a supported key"
               message))))
      (if (eq (plist-get response :type) 'error)
          response
        (nemacs-next-session-frame-delta
         'input (nemacs-next-session-frame-snapshot))))))

(defun nemacs-next-session-hello ()
  "Return a minimal protocol hello payload for frontend negotiation."
  (list :type 'hello
        :protocol-version nemacs-next-protocol-version
        :snapshot-version nemacs-next-session-snapshot-version
        :gui-shell-capabilities
        (copy-sequence nemacs-next-session-gui-shell-capabilities)
        :client-message-types (copy-sequence nemacs-next-client-message-types)
        :session-message-types (copy-sequence nemacs-next-session-message-types)
        :session-plan (nemacs-next-session-plan)))

(defun nemacs-next-session-error (code message &optional request)
  "Return a protocol error payload for CODE and MESSAGE.
REQUEST is included for diagnostics when supplied."
  (let ((payload (list :type 'error
                       :code code
                       :message message)))
    (if request
        (append payload (list :request request))
      payload)))

(defun nemacs-next-session--command-name (message)
  "Return the command name from MESSAGE."
  (or (plist-get message :name)
      (plist-get message :command)))

(defun nemacs-next-session--command-text (message)
  "Return command text payload from MESSAGE."
  (or (plist-get message :text)
      (plist-get message :char)
      (let ((args (plist-get message :args)))
        (and (listp args)
             (or (plist-get args :text)
                 (plist-get args :char))))))

(defun nemacs-next-session--command-arg (message key)
  "Return KEY from MESSAGE, falling back to MESSAGE's nested :args plist."
  (or (plist-get message key)
      (let ((args (plist-get message :args)))
        (and (listp args) (plist-get args key)))))

(defun nemacs-next-session--command-count (message &optional default)
  "Return an integer :count argument from MESSAGE, or DEFAULT (default 1)."
  (let ((count (nemacs-next-session--command-arg message :count)))
    (if (integerp count) count (or default 1))))

(defun nemacs-next-session--buffer-list ()
  "Return live session buffers through reusable buffer APIs."
  (let (buffers)
    (when (fboundp 'emacs-buffer-buffer-list)
      (dolist (buffer (emacs-buffer-buffer-list))
        (when (and buffer
                   (or (not (fboundp 'nelisp-ec-buffer-p))
                       (nelisp-ec-buffer-p buffer)))
          (setq buffers (cons buffer buffers)))))
    (when (and (boundp 'nelisp-ec--buffers)
               (listp nelisp-ec--buffers))
      (dolist (cell nelisp-ec--buffers)
        (when (cdr cell)
          (setq buffers (cons (cdr cell) buffers)))))
    (when (fboundp 'buffer-list)
      (dolist (buffer (buffer-list))
        (when (and buffer
                   (or (not (fboundp 'nelisp-ec-buffer-p))
                       (nelisp-ec-buffer-p buffer)))
          (setq buffers (cons buffer buffers)))))
    (let (seen out)
      (dolist (buffer buffers (nreverse out))
        (unless (memq buffer seen)
          (setq seen (cons buffer seen))
          (setq out (cons buffer out)))))))

(defun nemacs-next-session--find-buffer (buffer-or-name)
  "Return a live buffer named by BUFFER-OR-NAME, or nil."
  (cond
   ((and buffer-or-name
         (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer-or-name)
         (not (nelisp-ec-buffer-killed-p buffer-or-name)))
    buffer-or-name)
   ((and (stringp buffer-or-name)
         (fboundp 'get-buffer)
         (get-buffer buffer-or-name))
    (get-buffer buffer-or-name))
   ((stringp buffer-or-name)
    (catch 'found
      (dolist (buffer (nemacs-next-session--buffer-list))
        (when (equal (nemacs-next-session--buffer-name buffer) buffer-or-name)
          (throw 'found buffer)))
      nil))
   (t nil)))

(defun nemacs-next-session--buffer-name-candidates ()
  "Return live buffer names for minibuffer completion."
  (let (names)
    (dolist (buffer (nemacs-next-session--buffer-list) (nreverse names))
      (let ((name (nemacs-next-session--buffer-name buffer)))
        (when (stringp name)
          (setq names (cons name names)))))))

(defun nemacs-next-session--file-candidates (input)
  "Return file-name completion candidates for INPUT."
  (let* ((text (or input ""))
         (dir (or (and (fboundp 'file-name-directory)
                       (file-name-directory text))
                  "."))
         (prefix (or (and (fboundp 'file-name-nondirectory)
                          (file-name-nondirectory text))
                     text))
         (entries (and (fboundp 'directory-files)
                       (condition-case nil
                           (directory-files dir nil nil t)
                         (error nil))))
         candidates)
    (dolist (entry entries (nreverse candidates))
      (when (and (stringp entry)
                 (not (equal entry "."))
                 (not (equal entry ".."))
                 (or (equal prefix "")
                     (string-prefix-p prefix entry)))
        (setq candidates
              (cons (if (or (null dir) (equal dir "./") (equal dir "."))
                        entry
                      (concat dir entry))
                    candidates))))))

(defun nemacs-next-session--minibuffer-completion (message)
  "Return a minibuffer candidate payload for MESSAGE."
  (let* ((purpose (or (nemacs-next-session--command-arg message :purpose)
                      (nemacs-next-session--command-arg message :kind)
                      'generic))
         (input (or (nemacs-next-session--command-arg message :input) ""))
         (collection (nemacs-next-session--command-arg message :collection))
         (table (cond
                 ((and collection (listp collection)) collection)
                 ((or (nemacs-next-session--string-equal purpose 'buffer)
                      (nemacs-next-session--string-equal purpose "buffer"))
                  (nemacs-next-session--buffer-name-candidates))
                 ((or (nemacs-next-session--string-equal purpose 'file)
                      (nemacs-next-session--string-equal purpose "file"))
                  (nemacs-next-session--file-candidates input))
                 (t nil)))
         (candidates
          (cond
           ((and (fboundp 'emacs-minibuffer-all-completions) table)
            (emacs-minibuffer-all-completions input table))
           (table
            (let (matches)
              (dolist (candidate table (nreverse matches))
                (when (and (stringp candidate)
                           (string-prefix-p input candidate))
                  (setq matches (cons candidate matches))))))
           (t nil)))
         (completion
          (and (fboundp 'emacs-minibuffer-try-completion)
               table
               (emacs-minibuffer-try-completion input table))))
    (list :type 'minibuffer
          :active t
          :purpose purpose
          :prompt (or (nemacs-next-session--command-arg message :prompt)
                      "Completion: ")
          :contents input
          :completion completion
          :candidates candidates)))

;;; Generic session minibuffer prompt (M-x and chained follow-ups).
;;
;; D-1 wiring: a session-owned minibuffer read/write state plus a TUI
;; input loop generalization, both non-recursive (see
;; `nemacs-next-session--minibuffer-depth').  Recursive-minibuffer
;; command-loop integration is D-2, deferred until the magit
;; `emacs-command-loop.el' changes land (that file is not touched
;; here).

(defconst nemacs-next-session-mx-command-names
  '("save-buffer" "find-file" "dired" "switch-to-buffer" "kill-buffer"
    "undo" "yank" "kill-line" "newline" "next-line" "previous-line"
    "forward-char" "backward-char" "delete-char" "snapshot"
    "execute-extended-command")
  "Curated M-x candidate command names -- the data-table projection of
`nemacs-next-session-handle-command's dispatch cond used for M-x
completion.  `nemacs-next-session--mx-confirm' re-invokes the typed
command name through that same cond on commit, matching the pattern
already used for the GTK frontend's `nemacs-gtk--m-x-commands'.
`find-file'/`dired'/`switch-to-buffer' need a follow-up argument and
chain into a fresh prompt instead of dispatching directly; the rest
run with their existing zero-argument defaults.")

(defun nemacs-next-session--mx-completion-fn (_input)
  "Return the raw M-x command-name table for any INPUT.
Prefix filtering happens centrally via `emacs-minibuffer-all-completions'."
  nemacs-next-session-mx-command-names)

(defun nemacs-next-session--minibuffer-candidates-for (completion-fn input)
  "Return COMPLETION-FN's candidates for INPUT filtered by prefix
through `emacs-minibuffer-all-completions', or nil when COMPLETION-FN
is nil or returns no collection."
  (and completion-fn (fboundp 'emacs-minibuffer-all-completions)
       (let ((collection (funcall completion-fn (or input ""))))
         (and collection
              (emacs-minibuffer-all-completions (or input "") collection)))))

(defun nemacs-next-session--minibuffer-begin
    (purpose prompt on-confirm &optional completion-fn)
  "Enter a generic session minibuffer prompt for PURPOSE.
Uses `emacs-minibuffer-gui-enter-state' so the frontend's current
buffer is never switched to a minibuffer buffer -- the same
buffer-switch-free pattern `nemacs-gtk--begin-prompt' already applies
for the GTK frontend."
  (let ((state (if (fboundp 'emacs-minibuffer-gui-enter-state)
                   (emacs-minibuffer-gui-enter-state prompt on-confirm completion-fn)
                 (list :active t :prompt prompt :input "" :on-confirm on-confirm
                       :completion-fn completion-fn :candidates nil))))
    (setq nemacs-next-session--minibuffer-depth
          (1+ nemacs-next-session--minibuffer-depth))
    (setq nemacs-next-session--minibuffer-purpose purpose)
    (setq nemacs-next-session--minibuffer-active (plist-get state :active))
    (setq nemacs-next-session--minibuffer-prompt (plist-get state :prompt))
    (setq nemacs-next-session--minibuffer-input (plist-get state :input))
    (setq nemacs-next-session--minibuffer-on-confirm (plist-get state :on-confirm))
    (setq nemacs-next-session--minibuffer-completion-fn
          (plist-get state :completion-fn))
    (setq nemacs-next-session--minibuffer-candidates
          (plist-get state :candidates))))

(defun nemacs-next-session--minibuffer-end ()
  "Exit the active session minibuffer prompt.
D-1 has no real recursive-minibuffer stack, so ending always fully
closes the session minibuffer (depth resets to 0) regardless of how
deep Esc-x nesting reached -- there is no inner-prompt state to pop
back to an outer prompt that nesting may already have overwritten; see
`nemacs-next-session--minibuffer-depth'."
  (let ((state (if (fboundp 'emacs-minibuffer-gui-exit-state)
                   (emacs-minibuffer-gui-exit-state)
                 (list :active nil :prompt "" :input "" :on-confirm nil
                       :completion-fn nil :candidates nil))))
    (setq nemacs-next-session--minibuffer-depth 0)
    (setq nemacs-next-session--minibuffer-purpose nil)
    (setq nemacs-next-session--minibuffer-active (plist-get state :active))
    (setq nemacs-next-session--minibuffer-prompt (plist-get state :prompt))
    (setq nemacs-next-session--minibuffer-input (plist-get state :input))
    (setq nemacs-next-session--minibuffer-on-confirm (plist-get state :on-confirm))
    (setq nemacs-next-session--minibuffer-completion-fn
          (plist-get state :completion-fn))
    (setq nemacs-next-session--minibuffer-candidates
          (plist-get state :candidates))))

(defun nemacs-next-session-minibuffer-lines ()
  "Return the number of frame rows the active session minibuffer
prompt reserves (see `nemacs-next-session-minibuffer-reserved-lines'),
or 0 when no session minibuffer prompt is active."
  (if nemacs-next-session--minibuffer-active
      nemacs-next-session-minibuffer-reserved-lines
    0))

(defun nemacs-next-session--minibuffer-payload (&optional width)
  "Return the protocol minibuffer payload for the active session
prompt.  `:candidates-line' is a single reusable-library-formatted
summary row (`emacs-minibuffer-gui-candidate-suffix'); a full
multi-column `*Completions*'-style grid is out of scope for this v1
fidelity, matching the fidelity notes used elsewhere in this file
(e.g. `nemacs-next-session--directory-listing')."
  (list :type 'minibuffer
        :active (and nemacs-next-session--minibuffer-active t)
        :purpose nemacs-next-session--minibuffer-purpose
        :prompt nemacs-next-session--minibuffer-prompt
        :contents nemacs-next-session--minibuffer-input
        :candidates nemacs-next-session--minibuffer-candidates
        :candidates-line
        (nemacs-next-session--truncate-line
         (if (fboundp 'emacs-minibuffer-gui-candidate-suffix)
             (emacs-minibuffer-gui-candidate-suffix
              (and nemacs-next-session--minibuffer-completion-fn t)
              nemacs-next-session--minibuffer-candidates)
           "")
         (or width nemacs-next-session-frame-width))
        :depth nemacs-next-session--minibuffer-depth))

(defun nemacs-next-session--minibuffer-refresh-candidates ()
  "Recompute `nemacs-next-session--minibuffer-candidates' for the
current input."
  (setq nemacs-next-session--minibuffer-candidates
        (nemacs-next-session--minibuffer-candidates-for
         nemacs-next-session--minibuffer-completion-fn
         nemacs-next-session--minibuffer-input)))

(defun nemacs-next-session--minibuffer-insert (text)
  "Insert TEXT at the end of the active session minibuffer input."
  (when (fboundp 'emacs-minibuffer-gui-text-insert-state)
    (let ((state (emacs-minibuffer-gui-text-insert-state
                  nemacs-next-session--minibuffer-input
                  (length nemacs-next-session--minibuffer-input)
                  text)))
      (setq nemacs-next-session--minibuffer-input (plist-get state :text))))
  (nemacs-next-session--minibuffer-refresh-candidates)
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--minibuffer-delete-backward ()
  "Delete one character backward from the active session minibuffer input."
  (when (fboundp 'emacs-minibuffer-gui-text-delete-backward-state)
    (let ((state (emacs-minibuffer-gui-text-delete-backward-state
                  nemacs-next-session--minibuffer-input
                  (length nemacs-next-session--minibuffer-input))))
      (setq nemacs-next-session--minibuffer-input (plist-get state :text))))
  (nemacs-next-session--minibuffer-refresh-candidates)
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--minibuffer-complete ()
  "Handle Tab in the active session minibuffer: extend input to the
longest unambiguous completion via `emacs-minibuffer-try-completion'
and refresh the candidate list via `emacs-minibuffer-all-completions'
-- the same reusable completion primitives
`nemacs-next-session--minibuffer-completion' already uses for the
buffer/file completion payload."
  (let ((table (nemacs-next-session--minibuffer-candidates-for
                nemacs-next-session--minibuffer-completion-fn
                nemacs-next-session--minibuffer-input)))
    (when (and table (fboundp 'emacs-minibuffer-try-completion))
      (let ((completion (emacs-minibuffer-try-completion
                         nemacs-next-session--minibuffer-input table)))
        (when (stringp completion)
          (setq nemacs-next-session--minibuffer-input completion)))))
  (nemacs-next-session--minibuffer-refresh-candidates)
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--minibuffer-confirm ()
  "Commit the active session minibuffer prompt on Return.
The callback's raw result (e.g. a `save-buffer'/`find-file' buffer
snapshot carrying `:saved-file'/`:file-name') is returned unwrapped,
exactly like a plain `:type command' dispatch already returns it.  Do
not pre-build a `:frame' snapshot here: the frontend only recomputes
the frame snapshot itself when the response has no `:frame' key, and
recomputing it after this function returns is what lets it observe
the echo-area status this same command dispatch may have just set (see
`nemacs-next-session-echo-message'); embedding a `:frame' captured
before that would show a stale status line."
  (let ((input nemacs-next-session--minibuffer-input)
        (callback nemacs-next-session--minibuffer-on-confirm))
    (nemacs-next-session--minibuffer-end)
    (if (not callback)
        (nemacs-next-session-frame-delta
         'minibuffer (nemacs-next-session-frame-snapshot))
      (funcall callback input))))

(defun nemacs-next-session--minibuffer-abort ()
  "Abort the active session minibuffer prompt on C-g/Escape."
  (nemacs-next-session--minibuffer-end)
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--minibuffer-handle-input (message)
  "Route an M4 input MESSAGE to the active session minibuffer prompt.
`nemacs-next-session--handle-input' delegates here instead of running
buffer-editing commands while a session minibuffer prompt is active."
  (let* ((event (plist-get message :event))
         (text (and (listp event) (plist-get event :text)))
         (key (or (and (listp event) (plist-get event :key))
                  (plist-get message :key))))
    (cond
     ((or (nemacs-next-session--string-equal key 'return)
          (nemacs-next-session--string-equal key "RET")
          (nemacs-next-session--string-equal key "Enter"))
      (nemacs-next-session--minibuffer-confirm))
     ((or (nemacs-next-session--string-equal key 'tab)
          (nemacs-next-session--string-equal key "TAB")
          (nemacs-next-session--string-equal key "Tab"))
      (nemacs-next-session--minibuffer-complete))
     ((or (nemacs-next-session--string-equal key 'backspace)
          (nemacs-next-session--string-equal key "Backspace"))
      (nemacs-next-session--minibuffer-delete-backward))
     ((or (nemacs-next-session--string-equal key 'C-g)
          (nemacs-next-session--string-equal key "C-g")
          (nemacs-next-session--string-equal key 'escape)
          (nemacs-next-session--string-equal key "Escape"))
      (nemacs-next-session--minibuffer-abort))
     ((stringp text)
      (nemacs-next-session--minibuffer-insert text))
     (t
      (nemacs-next-session-frame-delta
       'minibuffer (nemacs-next-session-frame-snapshot))))))

(defun nemacs-next-session--mx-chain-argument-prompt (name)
  "Chain M-x NAME into its own follow-up argument prompt.
`find-file'/`dired'/`switch-to-buffer' need a path/directory/buffer
name the M-x command line does not carry; entering a fresh prompt for
it mirrors real Emacs's M-x argument prompting."
  (cond
   ((equal name "find-file")
    (nemacs-next-session--minibuffer-begin
     'file "Find file: "
     #'nemacs-next-session--find-file
     #'nemacs-next-session--file-candidates))
   ((equal name "dired")
    (nemacs-next-session--minibuffer-begin
     'directory "Dired directory: "
     #'nemacs-next-session--directory-listing
     nil))
   ((equal name "switch-to-buffer")
    (nemacs-next-session--minibuffer-begin
     'buffer "Switch to buffer: "
     #'nemacs-next-session--switch-to-buffer
     (lambda (_input) (nemacs-next-session--buffer-name-candidates)))))
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--mx-confirm (name)
  "Commit an M-x command NAME (typed into the M-x prompt) by
re-invoking it through `nemacs-next-session-handle-command' -- the
same dispatch cond every other frontend entry point already uses."
  (cond
   ((or (null name) (equal name ""))
    (nemacs-next-session-error 'bad-command "M-x: empty command name"))
   ((not (member name nemacs-next-session-mx-command-names))
    (nemacs-next-session-error
     'unknown-command (format "M-x: %s -- unbound" name)))
   ((member name '("find-file" "dired" "switch-to-buffer"))
    (nemacs-next-session--mx-chain-argument-prompt name))
   (t
    (nemacs-next-session-handle-command
     (list :type 'command :name (intern name))))))

(defun nemacs-next-session--execute-extended-command ()
  "Begin the M-x (`execute-extended-command') session minibuffer
prompt.  Recursive invocation (Esc x again while already active) is
allowed and only bumps `nemacs-next-session--minibuffer-depth' for
display -- see that variable's docstring for the exact limitation."
  (nemacs-next-session--minibuffer-begin
   'exec "M-x " #'nemacs-next-session--mx-confirm
   #'nemacs-next-session--mx-completion-fn)
  (nemacs-next-session-frame-delta
   'minibuffer (nemacs-next-session-frame-snapshot)))

(defun nemacs-next-session--move-point (delta)
  "Move point by DELTA characters through the reusable movement API.
Return a buffer snapshot, or a structured `out-of-range' protocol error
when DELTA would move point outside the buffer/narrowing bounds."
  (nemacs-next-session-current-buffer-or-create)
  (condition-case _err
      (progn
        (nelisp-ec-forward-char delta)
        (nemacs-next-session-buffer-snapshot))
    (nelisp-ec-args-out-of-range
     (nemacs-next-session-error
      'out-of-range "move would leave buffer bounds"))))

(defun nemacs-next-session--goto-char (position)
  "Move point to POSITION through the reusable movement API.
Return a buffer snapshot, or a structured protocol error when POSITION is
missing or out of range."
  (nemacs-next-session-current-buffer-or-create)
  (if (integerp position)
      (condition-case _err
          (progn
            (nelisp-ec-goto-char position)
            (nemacs-next-session-buffer-snapshot))
        (nelisp-ec-args-out-of-range
         (nemacs-next-session-error
          'out-of-range "goto-char position out of range")))
    (nemacs-next-session-error
     'bad-command "goto-char requires an integer :position")))

(defun nemacs-next-session--record-undo-boundary ()
  "Push an undo boundary after a completed edit command, when available.
Each protocol command that mutates the buffer closes its own undo group
this way, so a single `undo' command reverts exactly one prior command."
  (when (fboundp 'emacs-undo-undo-boundary)
    (emacs-undo-undo-boundary)))

(defun nemacs-next-session--delete-char (count)
  "Delete COUNT characters (negative = backward) through the reusable
editing API.  Return a buffer snapshot, or a structured `out-of-range'
protocol error when COUNT would delete past the buffer bounds.
The deleted text is captured before deletion and recorded on the
current buffer's undo list (Track E.2 `emacs-undo') so a later `undo'
command can restore it."
  (nemacs-next-session-current-buffer-or-create)
  (condition-case _err
      (let* ((point (nelisp-ec-point))
             (start (if (>= count 0) point (+ point count)))
             (end (if (>= count 0) (+ point count) point))
             ;; Reading the pre-delete text through the same bounds-checked
             ;; API used by `nelisp-ec-delete-char' means an out-of-range
             ;; COUNT is rejected here, before any mutation happens.
             (text (nelisp-ec-buffer-substring start end)))
        (nelisp-ec-delete-char count)
        (when (and (fboundp 'emacs-undo-record-delete)
                   (> (length text) 0))
          (emacs-undo-record-delete text start))
        (nemacs-next-session--record-undo-boundary)
        (nemacs-next-session-buffer-snapshot))
    (nelisp-ec-args-out-of-range
     (nemacs-next-session-error
      'out-of-range "delete-char count out of range"))))

(defun nemacs-next-session--newline (count)
  "Insert COUNT newline characters (default 1; non-positive is a no-op)
at point and return a buffer snapshot.

This inserts through `nelisp-ec-insert', the same reusable primitive
`insert-text' uses, rather than through the unprefixed Emacs-compatible
`newline' command.  Host Emacs already owns the unprefixed `newline'
name for its own native buffers (`emacs-edit-builtins' only installs
its polyfill when a name is not already `fboundp'), so calling it here
would silently operate on the host's current buffer instead of the
`nelisp-ec' session buffer under the host-Emacs smoke path.  A literal
\\n in `insert-text' :text takes this same `nelisp-ec-insert' path;
`newline' exists only as a named convenience for frontend Enter-key
handling."
  (nemacs-next-session-current-buffer-or-create)
  (let ((n (max 0 count))
        (beg (nelisp-ec-point)))
    (while (> n 0)
      (nelisp-ec-insert "\n")
      (setq n (1- n)))
    (when (and (fboundp 'emacs-undo-record-insert)
               (> (nelisp-ec-point) beg))
      (emacs-undo-record-insert beg (nelisp-ec-point))))
  (nemacs-next-session--record-undo-boundary)
  (nemacs-next-session-buffer-snapshot))

(defun nemacs-next-session--undo ()
  "Undo one edit group on the current buffer through `emacs-undo' and
return a buffer snapshot.  Returns a structured protocol error --
`no-further-undo-information' or `buffer-undo-list-disabled' -- instead
of an uncaught signal when there is nothing left to undo."
  (nemacs-next-session-current-buffer-or-create)
  (if (fboundp 'emacs-undo-undo-direct)
      (let ((result (emacs-undo-undo-direct)))
        (if (eq (plist-get result :status) 'ok)
            (nemacs-next-session-buffer-snapshot)
          (let ((reason (car (plist-get result :data))))
            (nemacs-next-session-error
             (or reason 'undo-error)
             (or (plist-get result :message) "undo failed")))))
    (nemacs-next-session-error 'unavailable "undo command is not available")))

(defun nemacs-next-session--kill-region (start end)
  "Kill START..END through the reusable kill-ring API and return a
buffer snapshot.  Returns a structured protocol error when START/END
are missing or not integers (`bad-command'), or out of buffer bounds
(`out-of-range')."
  (nemacs-next-session-current-buffer-or-create)
  (if (and (integerp start) (integerp end))
      (condition-case _err
          (progn
            (emacs-edit-kill-region-direct start end)
            (nemacs-next-session--record-undo-boundary)
            (nemacs-next-session-buffer-snapshot))
        (nelisp-ec-args-out-of-range
         (nemacs-next-session-error
          'out-of-range "kill-region start/end out of range")))
    (nemacs-next-session-error
     'bad-command "kill-region requires integer :start and :end")))

(defun nemacs-next-session--copy-region (start end)
  "Copy START..END through the reusable kill-ring API and return a snapshot."
  (nemacs-next-session-current-buffer-or-create)
  (if (and (integerp start) (integerp end))
      (condition-case _err
          (progn
            (emacs-edit-copy-region-direct start end)
            (nemacs-next-session-buffer-snapshot))
        (nelisp-ec-args-out-of-range
         (nemacs-next-session-error
          'out-of-range "copy-region-as-kill start/end out of range")))
    (nemacs-next-session-error
     'bad-command "copy-region-as-kill requires integer :start and :end")))

(defun nemacs-next-session--kill-line ()
  "Kill from point to end of line (including the trailing newline when
point is already at end of line) through the reusable kill-ring API and
return a buffer snapshot.  A no-op at end of buffer leaves the buffer
unchanged, matching `kill-line' in Emacs -- it is not a protocol error."
  (nemacs-next-session-current-buffer-or-create)
  (emacs-edit-kill-line-direct)
  (nemacs-next-session--record-undo-boundary)
  (nemacs-next-session-buffer-snapshot))

(defun nemacs-next-session--yank ()
  "Insert the current kill-ring head at point through the reusable
editing API and return a buffer snapshot.  Returns a structured
`empty-kill-ring' protocol error, instead of silently inserting nothing,
when the kill ring has no entries."
  (nemacs-next-session-current-buffer-or-create)
  (if (fboundp 'emacs-edit-yank-direct)
      (let ((edit (emacs-edit-yank-direct)))
        (if (plist-get edit :text)
            (progn
              (nemacs-next-session--record-undo-boundary)
              (nemacs-next-session-buffer-snapshot))
          (nemacs-next-session-error
           'empty-kill-ring "yank: kill ring is empty")))
    (nemacs-next-session-error 'unavailable "yank command is not available")))

(defun nemacs-next-session--find-file (path)
  "Visit PATH through reusable file I/O APIs and return a snapshot."
  (if (and (stringp path) (> (length path) 0))
      (condition-case err
          (let ((buffer (cond
                         ((fboundp 'emacs-fileio-visit-file-direct)
                          (emacs-fileio-visit-file-direct path))
                         ((fboundp 'find-file)
                          (find-file path))
                         (t nil))))
            (if buffer
                (nemacs-next-session-buffer-snapshot buffer)
              (nemacs-next-session-error
               'unavailable "find-file command is not available")))
        (error
         (nemacs-next-session-error
          'file-error (format "find-file failed: %S" err))))
    (nemacs-next-session-error
     'bad-command "find-file requires a non-empty string :path")))

(defun nemacs-next-session--directory-listing (directory)
  "Open DIRECTORY as a simple directory-listing buffer and return a snapshot.
This is a toolbar Dired fallback, not a full Dired implementation."
  (let ((dir (or directory "."))
        entries)
    (if (and (stringp dir) (> (length dir) 0))
        (condition-case err
            (progn
              (setq entries (directory-files dir nil nil t))
              (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                                 (nelisp-ec-generate-new-buffer
                                  (format "*Directory %s*" dir)))))
                (when (and buffer (fboundp 'nelisp-ec-set-buffer))
                  (nelisp-ec-set-buffer buffer))
                (nelisp-ec-insert (format "Directory: %s\n\n" dir))
                (dolist (entry entries)
                  (unless (member entry '("." ".."))
                    (nelisp-ec-insert entry)
                    (nelisp-ec-insert "\n")))
                (append (nemacs-next-session-buffer-snapshot buffer)
                        (list :directory dir
                              :fidelity "directory-listing buffer v1"))))
          (error
           (nemacs-next-session-error
            'file-error (format "dired directory listing failed: %S" err))))
      (nemacs-next-session-error
       'bad-command "dired requires a non-empty string :directory"))))

(defun nemacs-next-session--search-forward (query)
  "Move point to the next literal QUERY match and return a snapshot."
  (nemacs-next-session-current-buffer-or-create)
  (if (and (stringp query) (> (length query) 0))
      (let* ((text (or (and (fboundp 'nelisp-ec-buffer-string)
                            (nelisp-ec-buffer-string))
                       ""))
             (start (max 0 (1- (or (and (fboundp 'nelisp-ec-point)
                                         (nelisp-ec-point))
                                    1))))
             (pos (or (string-match (regexp-quote query) text start)
                      (string-match (regexp-quote query) text 0))))
        (if pos
            (progn
              (nelisp-ec-goto-char (1+ pos))
              (append (nemacs-next-session-buffer-snapshot)
                      (list :search-query query
                            :fidelity "search-prompt v1")))
          (nemacs-next-session-error
           'search-failed (format "search string not found: %s" query))))
    (list :type 'minibuffer
          :active t
          :purpose 'search
          :prompt "Search: "
          :contents ""
          :candidates nil)))

(defun nemacs-next-session--save-buffer ()
  "Save the current buffer through reusable file I/O APIs and return a snapshot."
  (nemacs-next-session-current-buffer-or-create)
  (condition-case err
      (let ((path (cond
                   ((fboundp 'emacs-fileio-save-buffer-direct)
                    (emacs-fileio-save-buffer-direct))
                   ((fboundp 'save-buffer)
                    (save-buffer))
                   (t nil))))
        (if path
            (append (nemacs-next-session-buffer-snapshot)
                    (list :saved-file path))
          (nemacs-next-session-error
           'unavailable "save-buffer command is not available")))
    (error
     (nemacs-next-session-error
      'file-error (format "save-buffer failed: %S" err)))))

(defun nemacs-next-session--switch-to-buffer (target)
  "Switch to TARGET buffer through reusable buffer APIs and return a snapshot."
  (if (and (stringp target) (> (length target) 0))
      (let ((buffer (or (nemacs-next-session--find-buffer target)
                        (and (fboundp 'nelisp-ec-generate-new-buffer)
                             (nelisp-ec-generate-new-buffer target)))))
        (if buffer
            (progn
              (when (fboundp 'nelisp-ec-set-buffer)
                (nelisp-ec-set-buffer buffer))
              (nemacs-next-session-buffer-snapshot buffer))
          (nemacs-next-session-error
           'unavailable "switch-to-buffer command is not available")))
    (nemacs-next-session-error
     'bad-command "switch-to-buffer requires a non-empty string :buffer-name")))

(defun nemacs-next-session--kill-buffer (target)
  "Kill TARGET or the current buffer and return the next buffer snapshot."
  (let ((buffer (cond
                 ((stringp target)
                  (nemacs-next-session--find-buffer target))
                 (target target)
                 (t (nemacs-next-session-current-buffer-or-create)))))
    (cond
     ((null buffer)
      (nemacs-next-session-error
       'no-such-buffer "kill-buffer target does not name a live buffer"))
     ((and (fboundp 'nelisp-ec-kill-buffer)
           (fboundp 'nelisp-ec-buffer-p)
           (nelisp-ec-buffer-p buffer))
      (nelisp-ec-kill-buffer buffer)
      (let ((next (car (nemacs-next-session--buffer-list))))
        (if next
            (progn
              (when (fboundp 'nelisp-ec-set-buffer)
                (nelisp-ec-set-buffer next))
              (nemacs-next-session-buffer-snapshot next))
          (nemacs-next-session-buffer-snapshot
           (nemacs-next-session-current-buffer-or-create)))))
     ((fboundp 'kill-buffer)
      (condition-case err
          (progn
            (kill-buffer buffer)
            (nemacs-next-session-buffer-snapshot
             (nemacs-next-session-current-buffer-or-create)))
        (error
         (nemacs-next-session-error
          'buffer-error (format "kill-buffer failed: %S" err)))))
     (t
      (nemacs-next-session-error
       'unavailable "kill-buffer command is not available")))))

(defun nemacs-next-session--toolbar-prompt (purpose prompt)
  "Return a toolbar minibuffer prompt for PURPOSE with PROMPT."
  (list :type 'minibuffer
        :active t
        :purpose purpose
        :prompt prompt
        :contents ""
        :candidates nil))

(defun nemacs-next-session--toolbar-invoke (id message)
  "Invoke toolbar item ID using protocol command semantics from MESSAGE."
  (let ((path (or (nemacs-next-session--command-arg message :path)
                  (nemacs-next-session--command-arg message :file)))
        (directory (nemacs-next-session--command-arg message :directory))
        (query (nemacs-next-session--command-arg message :query))
        (start (nemacs-next-session--command-arg message :start))
        (end (nemacs-next-session--command-arg message :end)))
    (cond
     ((or (nemacs-next-session--string-equal id "toolbar.new-file")
          (nemacs-next-session--string-equal id 'toolbar.new-file)
          (nemacs-next-session--string-equal id "toolbar.open-file")
          (nemacs-next-session--string-equal id 'toolbar.open-file))
      (if path
          (nemacs-next-session--find-file path)
        (nemacs-next-session--toolbar-prompt 'file "Find file: ")))
     ((or (nemacs-next-session--string-equal id "toolbar.dired")
          (nemacs-next-session--string-equal id 'toolbar.dired))
      (if directory
          (nemacs-next-session--directory-listing directory)
        (nemacs-next-session--toolbar-prompt 'directory "Dired directory: ")))
     ((or (nemacs-next-session--string-equal id "toolbar.kill-buffer")
          (nemacs-next-session--string-equal id 'toolbar.kill-buffer))
      (nemacs-next-session--kill-buffer nil))
     ((or (nemacs-next-session--string-equal id "toolbar.save-buffer")
          (nemacs-next-session--string-equal id 'toolbar.save-buffer))
      (nemacs-next-session--save-buffer))
     ((or (nemacs-next-session--string-equal id "toolbar.undo")
          (nemacs-next-session--string-equal id 'toolbar.undo))
      (nemacs-next-session--undo))
     ((or (nemacs-next-session--string-equal id "toolbar.cut")
          (nemacs-next-session--string-equal id 'toolbar.cut))
      (nemacs-next-session--kill-region start end))
     ((or (nemacs-next-session--string-equal id "toolbar.copy")
          (nemacs-next-session--string-equal id 'toolbar.copy))
      (nemacs-next-session--copy-region start end))
     ((or (nemacs-next-session--string-equal id "toolbar.paste")
          (nemacs-next-session--string-equal id 'toolbar.paste))
      (nemacs-next-session--yank))
     ((or (nemacs-next-session--string-equal id "toolbar.search")
          (nemacs-next-session--string-equal id 'toolbar.search))
      (nemacs-next-session--search-forward query))
     (t
      (nemacs-next-session-error
       'unknown-toolbar-item
       (format "unknown toolbar item: %S" id)
       message)))))

(defun nemacs-next-session-handle-command (message)
  "Handle a command protocol MESSAGE and return a response payload.
The supported commands are deliberately small and delegate editor
mutation, file I/O, and completion to reusable library APIs."
  (let ((name (nemacs-next-session--command-name message)))
    (cond
     ((or (nemacs-next-session--string-equal name 'snapshot)
          (nemacs-next-session--string-equal name "snapshot"))
      (nemacs-next-session-buffer-snapshot
       (nemacs-next-session-current-buffer-or-create)))
     ((or (nemacs-next-session--string-equal name 'create-buffer)
          (nemacs-next-session--string-equal name "create-buffer"))
      (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                         (nelisp-ec-generate-new-buffer
                          (or (plist-get message :buffer-name)
                              (plist-get message :name-arg)
                              nemacs-next-session-default-buffer-name)))))
        (when (and buffer (fboundp 'nelisp-ec-set-buffer))
          (nelisp-ec-set-buffer buffer))
        (nemacs-next-session-buffer-snapshot buffer)))
     ((or (nemacs-next-session--string-equal name 'insert-text)
          (nemacs-next-session--string-equal name "insert-text"))
      (let ((text (nemacs-next-session--command-text message)))
        (if (stringp text)
            (progn
              (nemacs-next-session-current-buffer-or-create)
              (let ((beg (nelisp-ec-point)))
                (nelisp-ec-insert text)
                (when (fboundp 'emacs-undo-record-insert)
                  (emacs-undo-record-insert beg (nelisp-ec-point))))
              (nemacs-next-session--record-undo-boundary)
              (nemacs-next-session-buffer-snapshot))
          (nemacs-next-session-error
           'bad-command "insert-text requires a string :text" message))))
     ((or (nemacs-next-session--string-equal name 'forward-char)
          (nemacs-next-session--string-equal name "forward-char"))
      (nemacs-next-session--move-point
       (nemacs-next-session--command-count message 1)))
     ((or (nemacs-next-session--string-equal name 'backward-char)
          (nemacs-next-session--string-equal name "backward-char"))
      (nemacs-next-session--move-point
       (- (nemacs-next-session--command-count message 1))))
     ((or (nemacs-next-session--string-equal name 'next-line)
          (nemacs-next-session--string-equal name "next-line"))
      (nemacs-next-session-current-buffer-or-create)
      (if (fboundp 'emacs-line-next-line-direct)
          (progn
            (emacs-line-next-line-direct
             (nemacs-next-session--command-count message 1))
            (nemacs-next-session-buffer-snapshot))
        (nemacs-next-session-error
         'unavailable "next-line command is not available")))
     ((or (nemacs-next-session--string-equal name 'previous-line)
          (nemacs-next-session--string-equal name "previous-line"))
      (nemacs-next-session-current-buffer-or-create)
      (if (fboundp 'emacs-line-previous-line-direct)
          (progn
            (emacs-line-previous-line-direct
             (nemacs-next-session--command-count message 1))
            (nemacs-next-session-buffer-snapshot))
        (nemacs-next-session-error
         'unavailable "previous-line command is not available")))
     ((or (nemacs-next-session--string-equal name 'goto-char)
          (nemacs-next-session--string-equal name "goto-char"))
      (nemacs-next-session--goto-char
       (nemacs-next-session--command-arg message :position)))
     ((or (nemacs-next-session--string-equal name 'delete-char)
          (nemacs-next-session--string-equal name "delete-char"))
      (nemacs-next-session--delete-char
       (nemacs-next-session--command-count message 1)))
     ((or (nemacs-next-session--string-equal name 'newline)
          (nemacs-next-session--string-equal name "newline"))
      (nemacs-next-session--newline
       (nemacs-next-session--command-count message 1)))
     ((or (nemacs-next-session--string-equal name 'undo)
          (nemacs-next-session--string-equal name "undo"))
      (nemacs-next-session--undo))
     ((or (nemacs-next-session--string-equal name 'kill-region)
          (nemacs-next-session--string-equal name "kill-region"))
      (nemacs-next-session--kill-region
       (nemacs-next-session--command-arg message :start)
       (nemacs-next-session--command-arg message :end)))
     ((or (nemacs-next-session--string-equal name 'copy-region-as-kill)
          (nemacs-next-session--string-equal name "copy-region-as-kill"))
      (nemacs-next-session--copy-region
       (nemacs-next-session--command-arg message :start)
       (nemacs-next-session--command-arg message :end)))
     ((or (nemacs-next-session--string-equal name 'kill-line)
          (nemacs-next-session--string-equal name "kill-line"))
      (nemacs-next-session--kill-line))
     ((or (nemacs-next-session--string-equal name 'yank)
          (nemacs-next-session--string-equal name "yank"))
      (nemacs-next-session--yank))
     ((or (nemacs-next-session--string-equal name 'find-file)
          (nemacs-next-session--string-equal name "find-file"))
      (nemacs-next-session--find-file
       (or (nemacs-next-session--command-arg message :path)
           (nemacs-next-session--command-arg message :file))))
     ((or (nemacs-next-session--string-equal name 'dired)
          (nemacs-next-session--string-equal name "dired"))
      (nemacs-next-session--directory-listing
       (or (nemacs-next-session--command-arg message :directory)
           (nemacs-next-session--command-arg message :path))))
     ((or (nemacs-next-session--string-equal name 'isearch-forward)
          (nemacs-next-session--string-equal name "isearch-forward"))
      (nemacs-next-session--search-forward
       (nemacs-next-session--command-arg message :query)))
     ((or (nemacs-next-session--string-equal name 'save-buffer)
          (nemacs-next-session--string-equal name "save-buffer"))
      (nemacs-next-session--save-buffer))
     ((or (nemacs-next-session--string-equal name 'switch-to-buffer)
          (nemacs-next-session--string-equal name "switch-to-buffer"))
      (nemacs-next-session--switch-to-buffer
       (or (nemacs-next-session--command-arg message :buffer-name)
           (nemacs-next-session--command-arg message :buffer))))
     ((or (nemacs-next-session--string-equal name 'kill-buffer)
          (nemacs-next-session--string-equal name "kill-buffer"))
      (nemacs-next-session--kill-buffer
       (or (nemacs-next-session--command-arg message :buffer-name)
           (nemacs-next-session--command-arg message :buffer))))
     ((or (nemacs-next-session--string-equal name 'complete)
          (nemacs-next-session--string-equal name "complete")
          (nemacs-next-session--string-equal name 'completion)
          (nemacs-next-session--string-equal name "completion"))
      (nemacs-next-session--minibuffer-completion message))
     ((or (nemacs-next-session--string-equal name 'execute-extended-command)
          (nemacs-next-session--string-equal name "execute-extended-command"))
      (nemacs-next-session--execute-extended-command))
     ((or (nemacs-next-session--string-equal name 'minibuffer-state)
          (nemacs-next-session--string-equal name "minibuffer-state"))
      (nemacs-next-session--minibuffer-payload))
     ((or (nemacs-next-session--string-equal name 'toolbar-invoke)
          (nemacs-next-session--string-equal name "toolbar-invoke"))
      (nemacs-next-session--toolbar-invoke
       (or (nemacs-next-session--command-arg message :id)
           (nemacs-next-session--command-arg message :item))
       message))
     ((or (nemacs-next-session--string-equal name 'frame-snapshot)
          (nemacs-next-session--string-equal name "frame-snapshot"))
      (nemacs-next-session-frame-snapshot
       (nemacs-next-session--command-arg message :width)
       (nemacs-next-session--command-arg message :height)))
     ((or (nemacs-next-session--string-equal name 'frame-config)
          (nemacs-next-session--string-equal name "frame-config"))
      (nemacs-next-session-frame-config))
     ((or (nemacs-next-session--string-equal name 'render-frame-text)
          (nemacs-next-session--string-equal name "render-frame-text"))
      (let ((frame (nemacs-next-session-frame-snapshot
                    (nemacs-next-session--command-arg message :width)
                    (nemacs-next-session--command-arg message :height))))
        (append frame
                (list :rendered-text
                      (nemacs-next-session-render-frame-text frame)))))
     ((or (nemacs-next-session--string-equal name 'menu)
          (nemacs-next-session--string-equal name "menu")
          (nemacs-next-session--string-equal name 'toolbar)
          (nemacs-next-session--string-equal name "toolbar"))
      (nemacs-next-session-menu-model))
     ((or (nemacs-next-session--string-equal name 'clipboard-read)
          (nemacs-next-session--string-equal name "clipboard-read"))
      (nemacs-next-session--clipboard-request 'clipboard-read))
     ((or (nemacs-next-session--string-equal name 'clipboard-write)
          (nemacs-next-session--string-equal name "clipboard-write"))
      (let ((text (nemacs-next-session--command-text message)))
        (if (stringp text)
            (nemacs-next-session--clipboard-request 'clipboard-write text)
          (nemacs-next-session-error
           'bad-command "clipboard-write requires a string :text" message))))
     (t
      (nemacs-next-session-error
       'unknown-command
       (format "unknown command: %S" name)
       message)))))

(defun nemacs-next-session-handle-message (message)
  "Handle one protocol MESSAGE plist and return a response payload."
  (let ((type (plist-get message :type)))
    (cond
     ((or (nemacs-next-session--string-equal type 'hello)
          (nemacs-next-session--string-equal type "hello"))
      (nemacs-next-session-hello))
     ((or (nemacs-next-session--string-equal type 'snapshot)
          (nemacs-next-session--string-equal type "snapshot"))
      (nemacs-next-session-buffer-snapshot
       (nemacs-next-session-current-buffer-or-create)))
     ((or (nemacs-next-session--string-equal type 'frame-config)
          (nemacs-next-session--string-equal type "frame-config"))
      (nemacs-next-session-frame-config))
     ((or (nemacs-next-session--string-equal type 'command)
          (nemacs-next-session--string-equal type "command"))
      (nemacs-next-session-handle-command message))
     ((or (nemacs-next-session--string-equal type 'resize)
          (nemacs-next-session--string-equal type "resize"))
      (nemacs-next-session-frame-delta
       'resize
       (nemacs-next-session-frame-snapshot
        (plist-get message :width)
        (plist-get message :height))))
     ((or (nemacs-next-session--string-equal type 'input)
          (nemacs-next-session--string-equal type "input"))
      (nemacs-next-session--handle-input message))
     ((or (nemacs-next-session--string-equal type 'clipboard)
          (nemacs-next-session--string-equal type "clipboard"))
      (let ((payload (plist-get message :payload)))
        (if (stringp payload)
            (nemacs-next-session--handle-input
             (list :type 'input :event (list :commit payload)))
          (nemacs-next-session--clipboard-request 'clipboard-read))))
     ((or (nemacs-next-session--string-equal type 'open)
          (nemacs-next-session--string-equal type "open"))
      (nemacs-next-session--find-file (plist-get message :path)))
     (t
      (nemacs-next-session-error
       'unknown-message
       (format "unknown message type: %S" type)
       message)))))

(provide 'nemacs-next-session)

;;; nemacs-next-session.el ends here
