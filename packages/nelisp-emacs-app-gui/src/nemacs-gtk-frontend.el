;;; nemacs-gtk-frontend.el --- elisp driver for nelisp-emacs-gtk -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Commentary:

;; Layer 3 elisp frontend for the GTK4 GUI display backend
;; (= `nelisp-emacs-gtk' Rust binary).  All display policy lives here:
;; mode-line composition, buffer-area refresh, cursor positioning,
;; key dispatch through `emacs-command-loop'.  The Rust side
;; (`nelisp-gtk-*' builtins) only provides GTK plumbing primitives.
;;
;; Boot sequence (driven by the Rust `main()'):
;;
;;   1. Rust loads Layer 2 substrate via `(require 'emacs-init)'.
;;   2. Rust flips `emacs-display-system' to `'gtk'.
;;   3. Rust registers `nelisp-gtk-*' builtins on the Env.
;;   4. Rust evals `(require 'nemacs-gtk-frontend)' (= this file).
;;   5. Rust evals `(nemacs-gtk-main)' which:
;;      - calls `(nelisp-gtk-init ROWS COLS)' to bring up the window
;;      - installs the global keymap (= `nemacs-gtk--init-keymap')
;;      - prepares the `*welcome*' buffer
;;      - paints the initial frame
;;      - drives the main loop: iterate / poll / dispatch / repaint
;;
;; Architecture mirrors `emacs-tui-backend.el' (the curses-style TUI
;; sibling) — same Layer 2 → Layer 3 split, different concrete grid.

;;; Code:

(require 'emacs-buffer-builtins)

;; Grid dimensions are now mutable defvars (Phase 2.I) — the GTK
;; window is resizable and `nelisp-gtk-poll-resize' surfaces the
;; new (rows, cols) tuple per drag.  Boot defaults match the old
;; defconst values; `nemacs-gtk--apply-grid-size' refreshes them
;; coherently on each resize event.
(defvar nemacs-gtk--rows 24)
(defvar nemacs-gtk--cols 80)
(defvar nemacs-gtk--mode-line-row (- nemacs-gtk--rows 2))
(defvar nemacs-gtk--echo-area-row (- nemacs-gtk--rows 1))
(defvar nemacs-gtk--buffer-area-end nemacs-gtk--mode-line-row) ; exclusive

(defvar nemacs-gtk--last-key-text ""
  "Most recent key event description (= what the echo area shows).")

(defvar nemacs-gtk--clipboard-cache nil
  "Last text we pushed onto the GTK system clipboard via our cut
function.  Used by the paste function to suppress duplicate pulls
when nothing newer has shown up on the clipboard.")

(defvar nemacs-gtk--active-buffer-name "*welcome*"
  "Name of the buffer currently rendered into the GTK grid.  Updated
by `nemacs-gtk--menu-open-file' (= File > Open...) when the user
visits a new file.  All paint / mode-line / cursor / dispatch
helpers query this rather than hardcoding `*welcome*' so subsequent
phases can swap buffers freely.")

(defvar nemacs-gtk--quit-requested nil
  "Non-nil when an elisp-side handler (= File > Quit menu, C-x C-c)
wants the main loop to exit.  Checked alongside
`(nelisp-gtk-should-quit)' which covers the GTK window-close path.")

(defvar nemacs-gtk--pending-prefix nil
  "Vector of accumulated events when the previous keypresses formed
a keymap prefix (= e.g. `[24]' after C-x).  Reset to nil once a
non-keymap binding is reached.

Accumulating in elisp lets us hand the FULL key sequence to
`emacs-command-loop-feed-events' in a single call so
`emacs-command-loop-step's `read-keys-vec' can consume it without
running out of events mid-prefix (= `read-event' on an empty
queue would otherwise raise `emacs-command-loop-no-input').")

(defvar nemacs-gtk--mark-pos nil
  "Active mark position (= absolute buffer point) for region ops.
nil when no mark.  Set by `nemacs-gtk-set-mark-command' (= C-SPC),
read by `nemacs-gtk--region-bounds' / `nemacs-gtk-copy-region' /
`nemacs-gtk-kill-region', cleared by `keyboard-quit' / a fresh
C-SPC / a successful copy/kill.

Lives in the frontend instead of the substrate (= Layer 2 still
ships only a `set-mark' stub returning nil — implementing real
mark tracking would require buffer-struct surgery; this is the
MVP shortcut).")

(defvar nemacs-gtk--mark-buffer nil
  "Name of the buffer the mark was set in.  When the user switches
to another buffer (= `C-x b' / open file) the mark becomes
inactive — `nemacs-gtk--region-bounds' refuses to return bounds
unless the active buffer matches this slot.")

(defvar nemacs-gtk--shift-region nil
  "Non-nil when the active mark was activated via Shift+motion (=
shift-select).  A subsequent plain (= non-shifted) motion will
deactivate the mark, mirroring real Emacs's `shift-select-mode'.
Cleared by `nemacs-gtk--deactivate-mark'.

Distinct from `--mark-pos' so that an explicit `C-SPC' followed
by motion keeps the mark alive (= manual marks are sticky), only
auto-set marks are auto-deactivated.")

;; GDK ModifierType bit positions (= `gdk_modifier_type' in libgdk-4).
;; Hoisted here so shift-select can reference `--gdk-shift-mask' before
;; the key-event translator block defines them all together.
(defconst nemacs-gtk--gdk-shift-mask    1)
(defconst nemacs-gtk--gdk-control-mask  4)
(defconst nemacs-gtk--gdk-alt-mask      8)

(defvar nemacs-gtk--last-mouse-event nil
  "Most-recent mouse-press tuple for the bound mouse commands
(= `nemacs-gtk-mouse-set-point' / `nemacs-gtk-mouse-yank-primary')
to consume.  Format mirrors what `(nelisp-gtk-poll-mouse)' returns:
(KIND BUTTON ROW COL MODS).  Set by `nemacs-gtk--handle-mouse-event'
just before feeding the synthetic `mouse-N' event into
`emacs-command-loop'.")

(defvar nemacs-gtk--press-point nil
  "Buffer point at the most-recent mouse-1 press.  The drag handler
reads this to set the mark on the first drag motion since the
press, anchoring the region at the click position even though the
substrate doesn't have a hidden anchor concept.

Cleared by every fresh mouse-1 press (= reset before the next
drag) and on `--deactivate-mark'.")

(defvar nemacs-gtk--scroll-offset 0
  "First buffer line (= 0-based row index in the active buffer's
text) that maps to grid row 0.  `nemacs-gtk--paint-buffer-area'
uses this to slide a window of `nemacs-gtk--buffer-area-end' lines
across the buffer.  Adjusted by mouse-wheel scroll +
auto-scroll-when-cursor-leaves-viewport in `dispatch-key'.")

(defconst nemacs-gtk--scroll-step 3
  "Number of buffer lines moved per mouse-wheel notch.")

(defun nemacs-gtk--active-buffer ()
  "Return the buffer object currently displayed in the GTK grid,
falling back to the welcome buffer when the named one has been
killed (= defensive — should not normally happen)."
  (or (get-buffer nemacs-gtk--active-buffer-name)
      (get-buffer "*welcome*")))

(defconst nemacs-gtk--menu-spec
  '(("File"
     ("Open..." . "open")
     ("Save"    . "save")
     ("Quit"    . "quit"))
    ("Edit"
     ("Cut"   . "cut")
     ("Copy"  . "copy")
     ("Paste" . "paste"))
    ("Help"
     ("About" . "about")))
  "Menu structure handed to `(nelisp-gtk-set-menu-bar ...)' at boot.
Each entry is `(LABEL . SUBENTRY-LIST)' for a submenu, or
`(LABEL . ACTION-NAME-STRING)' for a leaf.  When a leaf is clicked
the ACTION-NAME-STRING surfaces via `(nelisp-gtk-poll-menu-event)'.")

(defconst nemacs-gtk--context-menu-spec
  '(("Cut"        . "cut")
    ("Copy"       . "copy")
    ("Paste"      . "paste")
    ("Select All" . "select-all"))
  "Flat list of `(LABEL . ACTION-NAME-STRING)' leaves the right-click
context menu offers (Phase 2.S).  Reuses the same action-name pool
as `--menu-spec' so `--handle-menu-action' dispatches both with the
same cond chain.")


;;;; --- bootstrap helpers ----------------------------------------------------

(defun nemacs-gtk--init-keymap ()
  "Install the GUI's global keymap.  Mirrors the subset
`nemacs-main--init-keymap' (`nemacs-main.el') wires for the TUI,
plus a few common Ctrl-prefix chords for keyboard parity with
real Emacs.

  ASCII 32..126           → `self-insert-command'
  byte 13 / `'return'     → `newline'
  byte 127 / `'backspace' → `delete-backward-char'
  `'left' / `'right'      → `backward-char' / `forward-char'
  `'up'   / `'down'       → `previous-line' / `next-line'
  C-a / C-e               → beginning-of-line / end-of-line
  C-f / C-b               → forward-char / backward-char
  C-n / C-p               → next-line / previous-line
  C-d                     → delete-char
  C-k                     → kill-line
  C-y                     → yank
  C-x C-s / C-x C-f       → save / find-file (= our menu handlers)

Idempotent — re-calling replaces the global map with a fresh one."
  (let ((m (make-sparse-keymap))
        (ctl-x-map (make-sparse-keymap)))
    (let ((c 32))
      (while (<= c 126)
        (define-key m (vector c) 'self-insert-command)
        (setq c (1+ c))))
    (define-key m (vector 13) 'newline)
    (define-key m (vector 'return) 'newline)
    (define-key m (vector 'backspace) 'delete-backward-char)
    (define-key m (vector 127) 'delete-backward-char)
    (define-key m (vector 'left) 'backward-char)
    (define-key m (vector 'right) 'forward-char)
    (define-key m (vector 'up) 'previous-line)
    (define-key m (vector 'down) 'next-line)
    (define-key m (vector 'home)  'beginning-of-line)
    (define-key m (vector 'end)   'end-of-line)
    (define-key m (vector 'prior) 'nemacs-gtk-page-up)
    (define-key m (vector 'next)  'nemacs-gtk-page-down)
    ;; Single-chord control bindings (= byte 1..26 = C-a..C-z).
    (define-key m (vector ?\C-a) 'beginning-of-line)
    (define-key m (vector ?\C-e) 'end-of-line)
    (define-key m (vector ?\C-f) 'forward-char)
    (define-key m (vector ?\C-b) 'backward-char)
    (define-key m (vector ?\C-n) 'next-line)
    (define-key m (vector ?\C-p) 'previous-line)
    (define-key m (vector ?\C-d) 'delete-char)
    (define-key m (vector ?\C-k) 'kill-line)
    (define-key m (vector ?\C-y) 'yank)
    (define-key m (vector ?\C-s) 'nemacs-gtk-isearch-forward)
    (define-key m (vector ?\C-r) 'nemacs-gtk-isearch-backward)
    (define-key m (vector ?\C-w) 'nemacs-gtk-kill-region)
    (define-key m (vector ?\C-g) 'nemacs-gtk-keyboard-quit)
    ;; C-SPC = ?\C-@ = byte 0
    (define-key m (vector 0) 'nemacs-gtk-set-mark-command)
    ;; C-x prefix map — common substrate-level commands behind the
    ;; same handlers the menu uses.
    (define-key ctl-x-map (vector ?\C-s) 'nemacs-gtk-keyboard-save)
    (define-key ctl-x-map (vector ?\C-f) 'nemacs-gtk-keyboard-find-file)
    (define-key ctl-x-map (vector ?b)   'nemacs-gtk-switch-to-buffer)
    (define-key ctl-x-map (vector ?k)   'nemacs-gtk-kill-buffer)
    (define-key ctl-x-map (vector ?\C-c) 'nemacs-gtk-save-buffers-kill-emacs)
    (define-key m (vector ?\C-x) ctl-x-map)
    ;; Mouse-2 (= middle click) → set point + yank, mirroring real
    ;; Emacs's `mouse-yank-primary' / Linux X-clipboard convention.
    (define-key m (vector 'mouse-2) 'nemacs-gtk-mouse-yank-primary)
    ;; Esc-prefix → meta commands.  Reached either by pressing Esc
    ;; explicitly (= old terminal style) or by Alt+KEY which the
    ;; dispatch-key Alt-folding rewrites to the same 2-event vec.
    (let ((esc-map (make-sparse-keymap)))
      (define-key esc-map (vector ?x) 'execute-extended-command)
      (define-key esc-map (vector ?f) 'forward-word)
      (define-key esc-map (vector ?b) 'backward-word)
      (define-key esc-map (vector ?d) 'nemacs-gtk-meta-kill-word)
      (define-key esc-map (vector ?w) 'nemacs-gtk-copy-region)
      (define-key esc-map (vector ?<) 'nemacs-gtk-meta-beginning-of-buffer)
      (define-key esc-map (vector ?>) 'nemacs-gtk-meta-end-of-buffer)
      (define-key m (vector 27) esc-map))
    ;; Mouse: left click inside buffer area routes through
    ;; `emacs-command-loop' as a `mouse-1' event bound to
    ;; `nemacs-gtk-mouse-set-point' (= grid → goto-char).
    (define-key m (vector 'mouse-1) 'nemacs-gtk-mouse-set-point)
    ;; Phase 2.U: drag (= motion while button-1 held) extends the
    ;; region between the click position and the current cell.
    (define-key m (vector 'mouse-drag-1) 'nemacs-gtk-mouse-drag-region)
    ;; Phase 2.V: double / triple click select word / line at point.
    (define-key m (vector 'mouse-double-1) 'nemacs-gtk-mouse-select-word)
    (define-key m (vector 'mouse-triple-1) 'nemacs-gtk-mouse-select-line)
    (use-global-map m)))

(defun nemacs-gtk-keyboard-save ()
  "Bound to `C-x C-s' — wraps the same menu handler as File > Save."
  (interactive)
  (nemacs-gtk--menu-save-file))

(defun nemacs-gtk-keyboard-find-file ()
  "Bound to `C-x C-f' — wraps the same menu handler as File > Open."
  (interactive)
  (nemacs-gtk--menu-open-file))

(defun nemacs-gtk-switch-to-buffer ()
  "Bound to `C-x b' — prompt for a buffer name and switch the
active GUI buffer to it (= flips `nemacs-gtk--active-buffer-name'
+ resets scroll-offset)."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Switch to buffer: "
   (lambda (input)
     (cond
      ((string-empty-p input)
       (setq nemacs-gtk--last-key-text "switch-to-buffer: empty"))
      ((not (get-buffer input))
       (setq nemacs-gtk--last-key-text
             (format "No buffer: %s" input)))
      (t
       (setq nemacs-gtk--active-buffer-name input)
       (setq nemacs-gtk--scroll-offset 0)
       (nemacs-gtk--sync-window-title)
       (setq nemacs-gtk--last-key-text
             (format "Switched: %s" input)))))))

(defun nemacs-gtk-kill-buffer ()
  "Bound to `C-x k' — kill the active buffer + revert to *welcome*.
Refuses to kill *welcome* itself (= it's the boot fallback the
`nemacs-gtk--active-buffer' helper falls back to)."
  (interactive)
  (let ((bn nemacs-gtk--active-buffer-name))
    (cond
     ((string= bn "*welcome*")
      (setq nemacs-gtk--last-key-text "kill-buffer: refusing *welcome*"))
     (t
      (let ((buf (get-buffer bn)))
        (when buf (kill-buffer buf)))
      (setq nemacs-gtk--active-buffer-name "*welcome*")
      (setq nemacs-gtk--scroll-offset 0)
      (nemacs-gtk--sync-window-title)
      (setq nemacs-gtk--last-key-text
            (format "Killed: %s" bn))))))

(defun nemacs-gtk--unsaved-file-buffers ()
  "Return the list of live file-visiting buffers that are currently
modified.  Used by `nemacs-gtk-save-buffers-kill-emacs' to decide
whether to prompt before quitting."
  (let ((acc '()))
    (dolist (b (and (fboundp 'buffer-list) (buffer-list)))
      (let ((f (and (fboundp 'buffer-file-name) (buffer-file-name b))))
        (when (and f (nemacs-gtk--buffer-modified-p b))
          (push b acc))))
    (nreverse acc)))

(defun nemacs-gtk--save-all-dirty-and-quit (bufs)
  "Save each buffer in BUFS via `save-buffer' and arm the quit flag.
Per-buffer errors are caught + echoed but don't abort the loop —
the user already chose `y' (= save all), losing one save shouldn't
strand them in a half-quit state."
  (let ((saved 0)
        (failed 0))
    (dolist (b bufs)
      (condition-case _err
          (with-current-buffer b
            (save-buffer)
            (setq saved (1+ saved)))
        (error (setq failed (1+ failed)))))
    (setq nemacs-gtk--quit-requested t)
    (setq nemacs-gtk--last-key-text
          (cond
           ((zerop failed) (format "Saved %d buffer(s) — quit" saved))
           (t (format "Saved %d, %d failed — quit anyway" saved failed))))))

(defun nemacs-gtk-save-buffers-kill-emacs ()
  "Bound to `C-x C-c' — quit the GUI.  When at least one
file-visiting buffer is modified, prompt via the minibuffer:
  - `y' / `Y' → save all dirty file-visiting buffers + quit.
  - `n' / `N' → quit without saving.
  - anything else (= empty / `c') → cancel the quit.
With no dirty buffers, sets the quit flag immediately."
  (interactive)
  (let ((dirty (nemacs-gtk--unsaved-file-buffers)))
    (cond
     ((null dirty)
      (setq nemacs-gtk--quit-requested t)
      (setq nemacs-gtk--last-key-text "C-x C-c → quit"))
     (t
      (nemacs-gtk--enter-minibuffer
       (format "%d modified buffer(s).  Save? (y/n/c): " (length dirty))
       (lambda (input)
         (let ((c (and (stringp input) (> (length input) 0)
                       (downcase (substring input 0 1)))))
           (cond
            ((equal c "y") (nemacs-gtk--save-all-dirty-and-quit dirty))
            ((equal c "n")
             (setq nemacs-gtk--quit-requested t)
             (setq nemacs-gtk--last-key-text "Quit (unsaved)"))
            (t
             (setq nemacs-gtk--last-key-text "Quit cancelled"))))))))))

(defun nemacs-gtk-page-up ()
  "Bound to PageUp — scroll the viewport up by `(buffer-area-end - 2)'
lines and move point along so it stays in the visible region."
  (interactive)
  (let ((delta (max 1 (- nemacs-gtk--buffer-area-end 2))))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (forward-line (- delta)))
    (nemacs-gtk--scroll-by (- delta))
    (nemacs-gtk--ensure-cursor-visible)))

(defun nemacs-gtk-page-down ()
  "Bound to PageDown — scroll the viewport down by `(buffer-area-end - 2)'
lines and move point along so it stays in the visible region."
  (interactive)
  (let ((delta (max 1 (- nemacs-gtk--buffer-area-end 2))))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (forward-line delta))
    (nemacs-gtk--scroll-by delta)
    (nemacs-gtk--ensure-cursor-visible)))

;;;; --- region/mark (Phase 2.P — frontend-side mark tracking) -------------

(defun nemacs-gtk-set-mark-command ()
  "Bound to `C-SPC' (= byte 0).  Set the mark to the current point
in the active buffer + remember which buffer we're in.  Sets
`nemacs-gtk--mark-pos' / `--mark-buffer'.  A second `C-SPC'
overwrites the mark; `keyboard-quit' (= C-g) deactivates."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (setq nemacs-gtk--mark-pos    (nelisp-ec-point))
    (setq nemacs-gtk--mark-buffer nemacs-gtk--active-buffer-name)
    (setq nemacs-gtk--last-key-text
          (format "Mark set @ %d" nemacs-gtk--mark-pos))))

(defun nemacs-gtk--region-bounds ()
  "Return (BEG . END) when an active region exists in the active
buffer, else nil.  Active means: `--mark-pos' is non-nil AND
`--mark-buffer' equals the active-buffer-name AND mark != point."
  (when (and nemacs-gtk--mark-pos
             (string= nemacs-gtk--mark-buffer
                      nemacs-gtk--active-buffer-name))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((p (nelisp-ec-point))
             (m nemacs-gtk--mark-pos))
        (cond
         ((= p m) nil)
         ((< m p) (cons m p))
         (t       (cons p m)))))))

(defun nemacs-gtk--deactivate-mark ()
  (setq nemacs-gtk--mark-pos     nil)
  (setq nemacs-gtk--mark-buffer  nil)
  (setq nemacs-gtk--shift-region nil))

(defconst nemacs-gtk--shift-motion-events
  '(left right up down home end prior next)
  "Event symbols that participate in shift-select.  When any of these
fire with the Shift modifier held + no active region in the current
buffer, `nemacs-gtk--shift-arrow-pre-dispatch' auto-sets the mark.")

(defun nemacs-gtk--shift-arrow-pre-dispatch (event mods)
  "Maintain the shift-select region in front of EVENT/MODS dispatch.

When EVENT is a motion (= a member of `nemacs-gtk--shift-motion-events')
and the Shift bit is set in MODS:
  - if no active region exists in the current buffer, set the mark at
    point + flag the region as shift-selected so a later non-shifted
    motion can auto-deactivate it.

When EVENT is a motion and Shift is NOT held + a shift-selected region
is currently active:
  - deactivate the mark (= mirrors real Emacs `shift-select-mode' where
    a non-shifted motion drops the region).

A region set by an explicit `C-SPC' (= `--shift-region' is nil) is
sticky — plain motions don't deactivate it.  Returns nil; side-effects
only."
  (when (memq event nemacs-gtk--shift-motion-events)
    (let* ((shift-p (= (logand mods nemacs-gtk--gdk-shift-mask)
                       nemacs-gtk--gdk-shift-mask))
           (bn nemacs-gtk--active-buffer-name)
           (active-here (and nemacs-gtk--mark-pos
                             (equal nemacs-gtk--mark-buffer bn))))
      (cond
       ((and shift-p (not active-here))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (setq nemacs-gtk--mark-pos     (nelisp-ec-point))
          (setq nemacs-gtk--mark-buffer  bn)
          (setq nemacs-gtk--shift-region t))
        (setq nemacs-gtk--last-key-text "Mark activated"))
       ((and (not shift-p) nemacs-gtk--shift-region)
        (nemacs-gtk--deactivate-mark))))))

(defun nemacs-gtk-keyboard-quit ()
  "Bound to `C-g' — generic abort.  Deactivates the mark + clears
any pending key prefix + drops the user back to a clean state.
Echoes `Quit'."
  (interactive)
  (nemacs-gtk--deactivate-mark)
  (setq nemacs-gtk--pending-prefix nil)
  (setq nemacs-gtk--last-key-text "Quit"))

(defun nemacs-gtk-copy-region ()
  "Bound to `M-w' / Edit > Copy.  When a region is active, copy
the (mark .. point) range to kill-ring + mirror onto the system
clipboard via `interprogram-cut-function'.  Falls back to current-
line copy otherwise."
  (interactive)
  (let ((rg (nemacs-gtk--region-bounds)))
    (cond
     (rg (with-current-buffer (nemacs-gtk--active-buffer)
           (copy-region-as-kill (car rg) (cdr rg)))
         (nemacs-gtk--deactivate-mark)
         (setq nemacs-gtk--last-key-text
               (format "Region copied (%d chars)"
                       (- (cdr rg) (car rg)))))
     (t (nemacs-gtk--menu-copy-current-line)))))

(defun nemacs-gtk-kill-region ()
  "Bound to `C-w' / Edit > Cut.  When a region is active, cut the
(mark .. point) range to kill-ring + delete it.  Falls back to
current-line cut otherwise."
  (interactive)
  (let ((rg (nemacs-gtk--region-bounds)))
    (cond
     (rg (with-current-buffer (nemacs-gtk--active-buffer)
           (kill-region (car rg) (cdr rg)))
         (nemacs-gtk--deactivate-mark)
         (setq nemacs-gtk--last-key-text
               (format "Region killed (%d chars)"
                       (- (cdr rg) (car rg)))))
     (t (nemacs-gtk--menu-cut-current-line)))))


(defun nemacs-gtk-mark-whole-buffer ()
  "Bound to `Select All' in the right-click context menu (Phase 2.S).
Sets the mark at point-min, moves point to point-max — region-aware
copy/cut handlers then operate on the whole buffer."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((b (nelisp-ec-point-min))
          (e (nelisp-ec-point-max)))
      (nelisp-ec-goto-char e)
      (setq nemacs-gtk--mark-pos     b)
      (setq nemacs-gtk--mark-buffer  nemacs-gtk--active-buffer-name)
      (setq nemacs-gtk--shift-region nil))
    (setq nemacs-gtk--last-key-text
          (format "Selected whole buffer (%d chars)"
                  (- (nelisp-ec-point-max) (nelisp-ec-point-min))))))

(defun nemacs-gtk-meta-beginning-of-buffer ()
  "Bound to `M-<' / `Esc <' — point ← (point-min) of active buffer."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (nelisp-ec-goto-char (nelisp-ec-point-min))))

(defun nemacs-gtk-meta-end-of-buffer ()
  "Bound to `M->' / `Esc >' — point ← (point-max) of active buffer."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (nelisp-ec-goto-char (nelisp-ec-point-max))))

(defun nemacs-gtk-meta-kill-word ()
  "Bound to `M-d' / `Esc d' — kill chars from point to end of next
word.  Wraps `forward-word' + `kill-region' so the deletion sits
on `kill-ring' (= clipboard via the installed cut hook)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((start (nelisp-ec-point)))
      (forward-word 1)
      (let ((end (nelisp-ec-point)))
        (when (> end start)
          (kill-region start end))))))

(defun nemacs-gtk-mouse-yank-primary ()
  "Bound to `mouse-2' — move point to the click location, then
`yank' (= clipboard via the installed `interprogram-paste-function').
Mirrors `mouse-yank-primary' from real Emacs / the Linux middle-
click paste convention."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when ev
      (let ((p (nemacs-gtk--cell-to-point row col)))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char p)
          (yank))
        (setq nemacs-gtk--last-key-text
              (format "mouse-2 yank @ point %d" p))))))


;;;; --- minibuffer mode (Phase 2.J — M-x execute-extended-command) ----------

(defvar nemacs-gtk--minibuffer-active nil
  "Non-nil when the GUI is in inline-minibuffer mode (= M-x prompt
on the echo-area row).  All key dispatch routes through
`nemacs-gtk--minibuffer-handle-key' instead of the normal command
loop while this is true.")

(defvar nemacs-gtk--isearch-active nil
  "Non-nil during incremental search (= C-s).  Routes key dispatch
through `nemacs-gtk--isearch-handle-key' instead of the normal
keymap so query characters extend the search rather than
self-insert.")
(defvar nemacs-gtk--isearch-query "" "Live isearch query string.")
(defvar nemacs-gtk--isearch-start-pos 0
  "Point position at the moment isearch was entered, restored on
C-g cancel.")
(defvar nemacs-gtk--isearch-failing nil
  "Non-nil when the current `--isearch-query' has no match — flips
the echo prompt from `I-search:' to `I-search (failing):'.")

(defvar nemacs-gtk--isearch-direction 'forward
  "Direction of the active isearch: `forward' (= C-s) or
`backward' (= C-r).  Toggled mid-search by hitting the opposite
direction's key.  Drives the echo prompt + which substrate
search primitive `--isearch-search-from-start' calls.")

(defvar nemacs-gtk--minibuffer-prompt "")
(defvar nemacs-gtk--minibuffer-input "")
(defvar nemacs-gtk--minibuffer-on-confirm nil
  "Function called with the minibuffer's accumulated INPUT when
the user presses `Return'.  Set by `nemacs-gtk--enter-minibuffer'.")

(defvar nemacs-gtk--minibuffer-completion-fn nil
  "Optional function (INPUT) → list of completion candidates for the
active minibuffer (Phase 2.T).  When non-nil:
  - candidates are recomputed after each keystroke and surfaced
    after the input in the echo area
  - Tab completes to the longest common prefix; if a single match
    is left, the input is replaced with the full match
nil disables completion (= prompts that take free-form text).")

(defvar nemacs-gtk--minibuffer-candidates nil
  "Cached completion candidates for the current `--minibuffer-input'.
Recomputed by `--minibuffer-recompute-candidates' on every input
change so the echo-area painter doesn't re-run the completion fn.")

(defun nemacs-gtk--enter-minibuffer (prompt on-confirm &optional completion-fn)
  "Activate minibuffer mode.  PROMPT shows on the echo-area row
ahead of the live input; ON-CONFIRM is called with the accumulated
input string when the user presses Return.  C-g / Escape cancels
without calling ON-CONFIRM.

Optional COMPLETION-FN (Phase 2.T) is a function (INPUT) → list of
candidate strings.  When supplied, candidates are surfaced after
the input in the echo area and Tab completes to the longest common
prefix; nil disables completion."
  (setq nemacs-gtk--minibuffer-active        t)
  (setq nemacs-gtk--minibuffer-prompt        prompt)
  (setq nemacs-gtk--minibuffer-input         "")
  (setq nemacs-gtk--minibuffer-on-confirm    on-confirm)
  (setq nemacs-gtk--minibuffer-completion-fn completion-fn)
  (nemacs-gtk--minibuffer-recompute-candidates))

(defun nemacs-gtk--exit-minibuffer ()
  (setq nemacs-gtk--minibuffer-active        nil)
  (setq nemacs-gtk--minibuffer-prompt        "")
  (setq nemacs-gtk--minibuffer-input         "")
  (setq nemacs-gtk--minibuffer-on-confirm    nil)
  (setq nemacs-gtk--minibuffer-completion-fn nil)
  (setq nemacs-gtk--minibuffer-candidates    nil))

(defun nemacs-gtk--minibuffer-recompute-candidates ()
  "Refresh `--minibuffer-candidates' against the current input.
No-op when no completion fn is installed."
  (setq nemacs-gtk--minibuffer-candidates
        (when nemacs-gtk--minibuffer-completion-fn
          (condition-case _err
              (funcall nemacs-gtk--minibuffer-completion-fn
                       nemacs-gtk--minibuffer-input)
            (error nil)))))

(defun nemacs-gtk--longest-common-prefix (strs)
  "Return the longest string that is a prefix of every entry in STRS.
Empty list → empty string; single entry → that string."
  (cond
   ((null strs) "")
   ((null (cdr strs)) (car strs))
   (t
    (let ((p (car strs))
          (rest (cdr strs)))
      (while (and rest (> (length p) 0))
        (let* ((s     (car rest))
               (limit (min (length p) (length s)))
               (i     0))
          (while (and (< i limit) (eq (aref p i) (aref s i)))
            (setq i (1+ i)))
          (setq p (substring p 0 i)))
        (setq rest (cdr rest)))
      p))))

(defun nemacs-gtk--minibuffer-tab-complete ()
  "Tab handler for the minibuffer.  Replaces the current input with
the longest common prefix of `--minibuffer-candidates'; if there's
only one candidate, replaces with the full match; if no progress
can be made, echoes the candidate count."
  (let ((cands nemacs-gtk--minibuffer-candidates))
    (cond
     ((null cands)
      (setq nemacs-gtk--last-key-text "No match"))
     ((null (cdr cands))
      (setq nemacs-gtk--minibuffer-input (car cands))
      (nemacs-gtk--minibuffer-recompute-candidates))
     (t
      (let ((lcp (nemacs-gtk--longest-common-prefix cands)))
        (cond
         ((and (stringp lcp)
               (> (length lcp) (length nemacs-gtk--minibuffer-input)))
          (setq nemacs-gtk--minibuffer-input lcp)
          (nemacs-gtk--minibuffer-recompute-candidates))
         (t
          (setq nemacs-gtk--last-key-text
                (format "%d candidates" (length cands))))))))))

(defun nemacs-gtk--minibuffer-handle-key (event)
  "Consume one event while in minibuffer mode.  Returns t when
the event was handled (= caller should not run normal dispatch)."
  (cond
   ((eq event 'return)
    (let ((input nemacs-gtk--minibuffer-input)
          (cb    nemacs-gtk--minibuffer-on-confirm))
      (nemacs-gtk--exit-minibuffer)
      (when cb
        (condition-case err
            (funcall cb input)
          (error (setq nemacs-gtk--last-key-text
                       (format "minibuffer error: %S" err))))))
    t)
   ;; Cancel: C-g (= byte 7) or Escape.
   ((or (eq event 7) (eq event 27))
    (nemacs-gtk--exit-minibuffer)
    (setq nemacs-gtk--last-key-text "Quit")
    t)
   ;; Tab — completion (Phase 2.T).  No-op when no completion fn is
   ;; installed, otherwise advance to longest common prefix.
   ((eq event 'tab)
    (when nemacs-gtk--minibuffer-completion-fn
      (nemacs-gtk--minibuffer-tab-complete))
    t)
   ((eq event 'backspace)
    (when (> (length nemacs-gtk--minibuffer-input) 0)
      (setq nemacs-gtk--minibuffer-input
            (substring nemacs-gtk--minibuffer-input 0
                       (1- (length nemacs-gtk--minibuffer-input))))
      (nemacs-gtk--minibuffer-recompute-candidates))
    t)
   ((and (integerp event) (>= event 32) (< event 127))
    (setq nemacs-gtk--minibuffer-input
          (concat nemacs-gtk--minibuffer-input
                  (char-to-string event)))
    (nemacs-gtk--minibuffer-recompute-candidates)
    t)
   ;; Anything else (= arrow keys, mouse-1, function keys) is
   ;; ignored while minibuffer-active so the user doesn't
   ;; accidentally walk the cursor.
   (t t)))

;;;; --- isearch (Phase 2.N — C-s incremental forward search) ----------------

(defun nemacs-gtk-isearch-forward ()
  "Bound to `C-s' — start an incremental forward search.  Saves
the current point on `--isearch-start-pos' so C-g can restore it.
While active, every printable key extends the query and re-searches
from the saved start; another C-s jumps to the next match starting
after the current point; C-r toggles to backward search; backspace
shrinks the query; Return / Escape exit at the current match; C-g
cancels and restores point."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (setq nemacs-gtk--isearch-start-pos (nelisp-ec-point)))
  (setq nemacs-gtk--isearch-active    t)
  (setq nemacs-gtk--isearch-query     "")
  (setq nemacs-gtk--isearch-failing   nil)
  (setq nemacs-gtk--isearch-direction 'forward))

(defun nemacs-gtk-isearch-backward ()
  "Bound to `C-r' — start an incremental backward search.  Symmetric
counterpart to `nemacs-gtk-isearch-forward' (Phase 2.W).  Inside an
already-active forward isearch, C-r toggles direction instead of
starting fresh."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (setq nemacs-gtk--isearch-start-pos (nelisp-ec-point)))
  (setq nemacs-gtk--isearch-active    t)
  (setq nemacs-gtk--isearch-query     "")
  (setq nemacs-gtk--isearch-failing   nil)
  (setq nemacs-gtk--isearch-direction 'backward))

(defun nemacs-gtk--isearch-search-from-start ()
  "Reset point to start-pos + search for the current query in the
direction `--isearch-direction'.  Updates `--isearch-failing' on
success / failure."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((q nemacs-gtk--isearch-query))
      (cond
       ((string-empty-p q)
        (nelisp-ec-goto-char nemacs-gtk--isearch-start-pos)
        (setq nemacs-gtk--isearch-failing nil))
       (t
        (nelisp-ec-goto-char nemacs-gtk--isearch-start-pos)
        (let ((found
               (condition-case nil
                   (cond
                    ((eq nemacs-gtk--isearch-direction 'backward)
                     (search-backward q nil t))
                    (t (search-forward q nil t)))
                 (error nil))))
          (setq nemacs-gtk--isearch-failing (not found))
          (unless found
            (nelisp-ec-goto-char nemacs-gtk--isearch-start-pos))))))))

(defun nemacs-gtk--isearch-handle-key (event)
  "Consume one event during isearch.  Returns t when handled."
  (cond
   ((eq event 'return)
    (setq nemacs-gtk--isearch-active nil)
    (setq nemacs-gtk--last-key-text
          (format "isearch: %s" nemacs-gtk--isearch-query))
    t)
   ;; C-g (= byte 7) — cancel + restore point.
   ((eq event 7)
    (with-current-buffer (nemacs-gtk--active-buffer)
      (nelisp-ec-goto-char nemacs-gtk--isearch-start-pos))
    (setq nemacs-gtk--isearch-active nil)
    (setq nemacs-gtk--last-key-text "isearch cancelled")
    t)
   ((eq event 27) ; Escape — accept at current match.
    (setq nemacs-gtk--isearch-active nil)
    (setq nemacs-gtk--last-key-text "isearch ended")
    t)
   ;; C-s (= byte 19) during isearch:
   ;;   - if currently forward → jump to NEXT forward match
   ;;   - if currently backward → flip to forward + search-from-start
   ;;     (= "switch direction" gesture)
   ((eq event 19)
    (cond
     ((eq nemacs-gtk--isearch-direction 'backward)
      (setq nemacs-gtk--isearch-direction 'forward)
      (nemacs-gtk--isearch-search-from-start))
     (t
      (when (> (length nemacs-gtk--isearch-query) 0)
        (with-current-buffer (nemacs-gtk--active-buffer)
          (let ((found (condition-case nil
                           (search-forward nemacs-gtk--isearch-query nil t)
                         (error nil))))
            (setq nemacs-gtk--isearch-failing (not found)))))))
    t)
   ;; C-r (= byte 18) during isearch — symmetric to C-s.
   ((eq event 18)
    (cond
     ((eq nemacs-gtk--isearch-direction 'forward)
      (setq nemacs-gtk--isearch-direction 'backward)
      (nemacs-gtk--isearch-search-from-start))
     (t
      (when (> (length nemacs-gtk--isearch-query) 0)
        (with-current-buffer (nemacs-gtk--active-buffer)
          (let ((found (condition-case nil
                           (search-backward nemacs-gtk--isearch-query nil t)
                         (error nil))))
            (setq nemacs-gtk--isearch-failing (not found)))))))
    t)
   ((eq event 'backspace)
    (when (> (length nemacs-gtk--isearch-query) 0)
      (setq nemacs-gtk--isearch-query
            (substring nemacs-gtk--isearch-query 0
                       (1- (length nemacs-gtk--isearch-query))))
      (nemacs-gtk--isearch-search-from-start))
    t)
   ((and (integerp event) (>= event 32) (< event 127))
    (setq nemacs-gtk--isearch-query
          (concat nemacs-gtk--isearch-query (char-to-string event)))
    (nemacs-gtk--isearch-search-from-start)
    t)
   ;; Anything else (= arrows, mouse, function keys) ignored.
   (t t)))


(defconst nemacs-gtk--m-x-commands
  '("backward-char"
    "backward-word"
    "beginning-of-line"
    "copy-region"
    "delete-backward-char"
    "delete-char"
    "end-of-line"
    "execute-extended-command"
    "find-file"
    "forward-char"
    "forward-word"
    "isearch-backward"
    "isearch-forward"
    "kill-buffer"
    "kill-line"
    "kill-region"
    "keyboard-quit"
    "mark-whole-buffer"
    "newline"
    "nemacs-gtk-copy-region"
    "nemacs-gtk-isearch-backward"
    "nemacs-gtk-isearch-forward"
    "nemacs-gtk-keyboard-find-file"
    "nemacs-gtk-keyboard-quit"
    "nemacs-gtk-keyboard-save"
    "nemacs-gtk-kill-buffer"
    "nemacs-gtk-kill-region"
    "nemacs-gtk-mark-whole-buffer"
    "nemacs-gtk-meta-beginning-of-buffer"
    "nemacs-gtk-meta-end-of-buffer"
    "nemacs-gtk-meta-kill-word"
    "nemacs-gtk-mouse-set-point"
    "nemacs-gtk-mouse-yank-primary"
    "nemacs-gtk-page-down"
    "nemacs-gtk-page-up"
    "nemacs-gtk-save-buffers-kill-emacs"
    "nemacs-gtk-set-mark-command"
    "nemacs-gtk-switch-to-buffer"
    "next-line"
    "previous-line"
    "save-buffer"
    "save-buffers-kill-emacs"
    "self-insert-command"
    "set-mark-command"
    "switch-to-buffer"
    "yank")
  "Curated list of M-x candidate command names (Phase 2.T).  nelisp's
`mapatoms' / `commandp' return nil stubs (= we can't enumerate the
obarray to find interactive commands), so this is the trusted seed
for completion.  Extend when new GUI-facing commands ship.")

(defun nemacs-gtk--m-x-completion-fn (input)
  "Completion fn for execute-extended-command — return the
sub-list of `--m-x-commands' whose name has INPUT as a prefix."
  (let ((acc '()))
    (dolist (name nemacs-gtk--m-x-commands)
      (when (string-prefix-p input name)
        (push name acc)))
    (sort acc #'string<)))

(defun execute-extended-command (&optional _prefix-arg
                                            _command-name
                                            _typed)
  "M-x — read a command name from the minibuffer + run it.

PREFIXARG / COMMAND-NAME / TYPED accepted for API parity with
real Emacs (= our MVP ignores them; minibuffer-typed input is
the only source).  Bound to `Esc x' (= [27 120]) in the GUI's
global keymap.  Type a name, press Return — we intern it, verify
it's fboundp, and `call-interactively' it.

Tab in the prompt completes against `--m-x-commands' (Phase 2.T)."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "M-x "
   (lambda (input)
     (cond
      ((string-empty-p input)
       (setq nemacs-gtk--last-key-text "M-x: empty"))
      (t
       (let ((sym (intern input)))
         (cond
          ((not (fboundp sym))
           (setq nemacs-gtk--last-key-text
                 (format "M-x: %s — unbound" input)))
          (t
           (with-current-buffer (nemacs-gtk--active-buffer)
             (call-interactively sym))
           (setq nemacs-gtk--last-key-text
                 (format "M-x %s ✓" input))))))))
   #'nemacs-gtk--m-x-completion-fn))

(defun nemacs-gtk--prepare-welcome-buffer ()
  "Create / reset the `*welcome*' buffer + drop the cursor at end."
  (let ((buf (or (get-buffer "*welcome*")
                 (generate-new-buffer "*welcome*"))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Welcome to nemacs-gtk!\n")
      (insert "\n")
      (insert "Phase 2 architecture: this UI is now elisp-driven.\n")
      (insert "Rust ships GTK plumbing primitives (`nelisp-gtk-*');\n")
      (insert "every layout / dispatch decision lives in this file.\n")
      (insert "\n")
      (insert (format "(window-system) => %S\n"
                      (if (fboundp 'window-system)
                          (window-system) 'unbound)))
      (insert (format "(display-graphic-p) => %S\n"
                      (if (fboundp 'display-graphic-p)
                          (display-graphic-p) 'unbound)))
      (insert "\n")
      (insert "Type printable keys; Backspace / Enter / arrows for\n")
      (insert "motion + edits.  Mode line + cursor follow point.\n")
      (insert "\n")
      (insert "> "))
    (buffer-name buf)))


;;;; --- redraw composition ---------------------------------------------------

(defun nemacs-gtk--truncate (s n)
  "Truncate S to at most N chars, padding with spaces if shorter.
NeLisp standalone lacks `string-to-list', so we walk the string by
index using `length' + `aref' instead — same shape, fewer deps."
  (let ((len (length s)))
    (cond
     ((= len n) s)
     ((< len n)
      (concat s (make-string (- n len) ?\s)))
     (t (substring s 0 n)))))

(defun nemacs-gtk--scroll-position-label ()
  "Emacs-style position label for the mode-line: Top / All / Bot /
NN%.  Computed against `--scroll-offset' + `--buffer-area-end'
+ buffer line count."
  (let* ((line-count    (nemacs-gtk--buffer-line-count))
         (visible-rows  nemacs-gtk--buffer-area-end)
         (offset        nemacs-gtk--scroll-offset)
         (last-visible-buf-row (+ offset (- visible-rows 1))))
    (cond
     ((<= line-count visible-rows) "All")
     ((= offset 0)                 "Top")
     ((>= last-visible-buf-row (- line-count 1)) "Bot")
     (t
      (let ((denom (max 1 (- line-count visible-rows))))
        (format "%d%%" (/ (* 100 offset) denom)))))))

(defun nemacs-gtk--buffer-modified-p (buf)
  "Return non-nil when BUF has unsaved changes.  Tries the public
`buffer-modified-p' first, falls back to the substrate-level
`nelisp-ec-buffer-modified-p' accessor.  Returns nil when neither
is available (= safe default — assume clean)."
  (cond
   ((fboundp 'buffer-modified-p)
    (condition-case _ (buffer-modified-p buf) (error nil)))
   ((fboundp 'nelisp-ec-buffer-modified-p)
    (condition-case _ (nelisp-ec-buffer-modified-p buf) (error nil)))
   (t nil)))

(defun nemacs-gtk--mode-line-text ()
  "Compose the mode-line for the active buffer.  Includes:
modified-flag (= `**' / `--'), buffer-name, current line number,
viewport scroll-position label (= Top/All/Bot/NN%), major-mode name."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((name (buffer-name))
           (line (line-number-at-pos))
           (mode (symbol-name (if (boundp 'major-mode) major-mode
                                'fundamental-mode)))
           (pos  (nemacs-gtk--scroll-position-label))
           (mod-flag (if (nemacs-gtk--buffer-modified-p (current-buffer))
                         "**" "--"))
           (body (format "-U:%s-  %s    L%d   %s   (%s) "
                         mod-flag name line pos mode))
           (pad (- nemacs-gtk--cols (length body))))
      (if (> pad 0)
          (concat body (make-string pad ?-))
        (substring body 0 nemacs-gtk--cols)))))

(defun nemacs-gtk--paint-buffer-area ()
  "Stamp the active buffer's content into rows 0..MODE_LINE_ROW of
the grid, starting from buffer line `nemacs-gtk--scroll-offset'.
Lines past EOB stamp blanks so a vertically-too-short buffer
doesn't leak the previous repaint's tail."
  (let* ((content
          (with-current-buffer (nemacs-gtk--active-buffer) (buffer-string)))
         (lines (split-string content "\n"))
         (max-rows nemacs-gtk--buffer-area-end)
         (i 0))
    (while (< i max-rows)
      (let ((line (or (nth (+ i nemacs-gtk--scroll-offset) lines) "")))
        (nelisp-gtk-grid-put-row i (nemacs-gtk--truncate line nemacs-gtk--cols)))
      (setq i (1+ i)))))

(defun nemacs-gtk--buffer-line-count ()
  "Return the number of lines in the active buffer (= 1 + number
of newlines, counting the trailing-no-newline line)."
  (let* ((content
          (with-current-buffer (nemacs-gtk--active-buffer) (buffer-string))))
    (length (split-string content "\n"))))

(defun nemacs-gtk--clamp-scroll-offset ()
  "Clamp `nemacs-gtk--scroll-offset' to [0, line-count - 1] so the
viewport never slides past the buffer's end."
  (let* ((line-count (nemacs-gtk--buffer-line-count))
         (max-off (max 0 (- line-count 1))))
    (when (< nemacs-gtk--scroll-offset 0)
      (setq nemacs-gtk--scroll-offset 0))
    (when (> nemacs-gtk--scroll-offset max-off)
      (setq nemacs-gtk--scroll-offset max-off))))

(defun nemacs-gtk--sync-window-title ()
  "Push the current active-buffer-name to the OS window titlebar
via `(nelisp-gtk-set-window-title ...)'.  Called whenever the
active buffer changes (= File > Open / C-x b / kill-buffer)."
  (when (fboundp 'nelisp-gtk-set-window-title)
    (nelisp-gtk-set-window-title
     (format "nemacs-gtk — %s" nemacs-gtk--active-buffer-name))))

(defun nemacs-gtk--apply-grid-size (new-rows new-cols)
  "Refresh the grid-dimension defvars and the dependent layout
constants when the GTK area gets resized.  Re-pushes the
`mode-line-row' to the Rust side so the painted bar follows the
new bottom, and re-clamps `scroll-offset' so a viewport that was
hugging EOB doesn't fall past it."
  (when (and (integerp new-rows) (integerp new-cols)
             (>= new-rows 5) (>= new-cols 20)
             (or (/= new-rows nemacs-gtk--rows)
                 (/= new-cols nemacs-gtk--cols)))
    (setq nemacs-gtk--rows           new-rows)
    (setq nemacs-gtk--cols           new-cols)
    (setq nemacs-gtk--mode-line-row  (- new-rows 2))
    (setq nemacs-gtk--echo-area-row  (- new-rows 1))
    (setq nemacs-gtk--buffer-area-end nemacs-gtk--mode-line-row)
    (nelisp-gtk-set-mode-line-row nemacs-gtk--mode-line-row)
    (nemacs-gtk--clamp-scroll-offset)
    (nemacs-gtk--ensure-cursor-visible)))

(defun nemacs-gtk--scroll-by (delta)
  "Slide the buffer view by DELTA buffer-lines (= negative scrolls
up / shows earlier lines, positive scrolls down)."
  (setq nemacs-gtk--scroll-offset
        (+ nemacs-gtk--scroll-offset delta))
  (nemacs-gtk--clamp-scroll-offset))

(defun nemacs-gtk--point-to-buf-row ()
  "Return the buffer-row (= 0-based) at which point sits in the
active buffer."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (point))
           (target (1- p))
           (text (buffer-string))
           (buf-row 0) (i 0))
      (while (< i target)
        (when (eq (aref text i) ?\n)
          (setq buf-row (1+ buf-row)))
        (setq i (1+ i)))
      buf-row)))

(defun nemacs-gtk--ensure-cursor-visible ()
  "Adjust `nemacs-gtk--scroll-offset' so point's buffer-row is
within the viewport (= rows scroll-offset .. scroll-offset +
buffer-area-end - 1)."
  (let ((buf-row (nemacs-gtk--point-to-buf-row)))
    (cond
     ((< buf-row nemacs-gtk--scroll-offset)
      (setq nemacs-gtk--scroll-offset buf-row))
     ((>= buf-row (+ nemacs-gtk--scroll-offset
                     nemacs-gtk--buffer-area-end))
      (setq nemacs-gtk--scroll-offset
            (1+ (- buf-row nemacs-gtk--buffer-area-end)))))
    (nemacs-gtk--clamp-scroll-offset)))

(defun nemacs-gtk--paint-mode-line ()
  (nelisp-gtk-grid-put-row nemacs-gtk--mode-line-row
                           (nemacs-gtk--mode-line-text)))

(defun nemacs-gtk--minibuffer-candidate-suffix ()
  "Compose a `{cand1 cand2 ...}' suffix listing the current
completion candidates for the echo area, or empty when none."
  (let ((cands nemacs-gtk--minibuffer-candidates))
    (cond
     ((null nemacs-gtk--minibuffer-completion-fn) "")
     ((null cands) "  {no match}")
     ((null (cdr cands)) (format "  {%s}" (car cands)))
     (t
      (format "  {%s}" (mapconcat #'identity cands " "))))))

(defun nemacs-gtk--paint-echo-area ()
  (let ((text (cond
               (nemacs-gtk--minibuffer-active
                (concat nemacs-gtk--minibuffer-prompt
                        nemacs-gtk--minibuffer-input
                        ;; trailing block-cursor-ish marker so the
                        ;; user knows the prompt is awaiting input.
                        "_"
                        (nemacs-gtk--minibuffer-candidate-suffix)))
               (nemacs-gtk--isearch-active
                (format "I-search%s%s: %s_"
                        (if (eq nemacs-gtk--isearch-direction 'backward)
                            " backward" "")
                        (if nemacs-gtk--isearch-failing
                            " (failing)" "")
                        nemacs-gtk--isearch-query))
               ((string-empty-p nemacs-gtk--last-key-text)
                "(press any key)")
               (t (format "Last key: %s" nemacs-gtk--last-key-text)))))
    (nelisp-gtk-grid-put-row nemacs-gtk--echo-area-row
                             (nemacs-gtk--truncate text nemacs-gtk--cols))))

(defun nemacs-gtk--cell-to-point (row col)
  "Inverse of `nemacs-gtk--cursor-row-col': map a grid cell (ROW, COL)
back to a buffer position in the active buffer.  ROW is the
on-screen row — `nemacs-gtk--scroll-offset' is added so the
buffer-row walked is the absolute one.

When the target cell is past EOB / past the line's length, the
result clamps to the last reachable position in the buffer text
(= mouse-1 on the empty area below content puts point at EOB,
which is what users expect).  Always returns a 1-based point."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((target-row (+ row nemacs-gtk--scroll-offset))
           (text (buffer-string))
           (len  (length text))
           (i 0)
           (cur-row 0))
      ;; Walk forward to the start of TARGET-ROW.  Stop early at EOB.
      (while (and (< i len) (< cur-row target-row))
        (when (eq (aref text i) ?\n)
          (setq cur-row (1+ cur-row)))
        (setq i (1+ i)))
      ;; Walk forward COL chars within the row, stopping at newline / EOB.
      (let ((col-i 0))
        (while (and (< i len) (< col-i col)
                    (not (eq (aref text i) ?\n)))
          (setq i (1+ i))
          (setq col-i (1+ col-i))))
      (1+ i))))

(defun nemacs-gtk--cursor-row-col ()
  "Compute the on-screen (row . col) of point in the active buffer,
subtracting `nemacs-gtk--scroll-offset' so a scrolled-out cursor
returns nil instead of a row outside the viewport."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (point))
           (target (1- p))
           (buf-row 0) (col 0)
           (i 0)
           (text (buffer-string)))
      (while (< i target)
        (let ((c (aref text i)))
          (if (eq c ?\n)
              (setq buf-row (1+ buf-row) col 0)
            (setq col (1+ col))))
        (setq i (1+ i)))
      (let ((screen-row (- buf-row nemacs-gtk--scroll-offset)))
        (if (and (>= screen-row 0)
                 (< screen-row nemacs-gtk--buffer-area-end)
                 (<= col nemacs-gtk--cols))
            (cons screen-row col)
          nil)))))

(defun nemacs-gtk--repaint ()
  "One full redraw cycle: buffer, mode line, echo, cursor, queue draw."
  (nelisp-gtk-grid-clear)
  (nemacs-gtk--paint-buffer-area)
  (nemacs-gtk--paint-mode-line)
  (nemacs-gtk--paint-echo-area)
  (let ((rc (nemacs-gtk--cursor-row-col)))
    (if rc
        (nelisp-gtk-set-cursor (car rc) (cdr rc))
      (nelisp-gtk-set-cursor nil nil)))
  (nelisp-gtk-redraw))


;;;; --- key event translation ------------------------------------------------

;; gdk keysym constants we route as named symbols (= what
;; `nemacs-gtk--init-keymap' binds).  Values lifted from
;; gtk4-rs `gdk::Key::name()' inverse — the ones we care about.
(defconst nemacs-gtk--keysym-backspace #xff08)
(defconst nemacs-gtk--keysym-return    #xff0d)
(defconst nemacs-gtk--keysym-escape    #xff1b)
(defconst nemacs-gtk--keysym-home      #xff50)
(defconst nemacs-gtk--keysym-left      #xff51)
(defconst nemacs-gtk--keysym-up        #xff52)
(defconst nemacs-gtk--keysym-right     #xff53)
(defconst nemacs-gtk--keysym-down      #xff54)
(defconst nemacs-gtk--keysym-prior     #xff55) ; PageUp
(defconst nemacs-gtk--keysym-next      #xff56) ; PageDown
(defconst nemacs-gtk--keysym-end       #xff57)
(defconst nemacs-gtk--keysym-kp-enter  #xff8d)
(defconst nemacs-gtk--keysym-tab       #xff09)

;; GDK modifier defconsts hoisted above (= near `--shift-region' defvar)
;; so shift-select pre-dispatch can reference `--gdk-shift-mask'.

(defun nemacs-gtk--key-event->command-loop-event (keysym mods unicode)
  "Map a GDK key event to the event symbol / integer
`emacs-command-loop' expects in its unread queue.  Returns nil if the
key has no handled mapping (= modifier-only, function key without a
binding) so the caller can drop it.

Control-modifier handling: when ControlMask is set + the unicode is
an ASCII letter, fold to the canonical control byte (= C-a → 1,
C-x → 24, etc., matching `?\\C-x' literals in the keymap).  This
makes `(define-key m [?\\C-x] ...)' style bindings just work without
a separate event-prefix system."
  (let ((ctrl (= (logand mods nemacs-gtk--gdk-control-mask)
                 nemacs-gtk--gdk-control-mask)))
    (cond
     ((= keysym nemacs-gtk--keysym-backspace) 'backspace)
     ((or (= keysym nemacs-gtk--keysym-return)
          (= keysym nemacs-gtk--keysym-kp-enter))
      'return)
     ((= keysym nemacs-gtk--keysym-escape) 27)
     ((= keysym nemacs-gtk--keysym-tab)     'tab)
     ((= keysym nemacs-gtk--keysym-left)  'left)
     ((= keysym nemacs-gtk--keysym-right) 'right)
     ((= keysym nemacs-gtk--keysym-up)    'up)
     ((= keysym nemacs-gtk--keysym-down)  'down)
     ((= keysym nemacs-gtk--keysym-home)  'home)
     ((= keysym nemacs-gtk--keysym-end)   'end)
     ((= keysym nemacs-gtk--keysym-prior) 'prior)
     ((= keysym nemacs-gtk--keysym-next)  'next)
     ;; Ctrl + ASCII letter → control byte.  Try unicode first
     ;; (= what GDK delivers when the key produces a printable),
     ;; fall back to keysym for the Ctrl-only case where unicode
     ;; comes through as 0.
     (ctrl
      (let ((ch (cond
                 ;; Ctrl+Space → ?\C-@ = 0 (= set-mark-command).
                 ((or (= unicode ?\s) (= keysym ?\s)) 0)
                 ((and (>= unicode ?a) (<= unicode ?z)) (- unicode (1- ?a)))
                 ((and (>= unicode ?A) (<= unicode ?Z)) (- unicode (1- ?A)))
                 ((and (>= keysym  ?a) (<= keysym  ?z)) (- keysym  (1- ?a)))
                 ((and (>= keysym  ?A) (<= keysym  ?Z)) (- keysym  (1- ?A)))
                 (t nil))))
        ch))
     ((and (> unicode 0)
           (>= unicode 32)
           (< unicode 127))
      unicode)
     (t nil))))

(defun nemacs-gtk--describe-key (keysym mods unicode)
  "Human-readable summary for the echo area."
  (let* ((named
          (cond ((= keysym nemacs-gtk--keysym-backspace) "BackSpace")
                ((= keysym nemacs-gtk--keysym-return)    "Return")
                ((= keysym nemacs-gtk--keysym-left)      "Left")
                ((= keysym nemacs-gtk--keysym-right)     "Right")
                ((= keysym nemacs-gtk--keysym-up)        "Up")
                ((= keysym nemacs-gtk--keysym-down)      "Down")
                (t (format "key#%d" keysym))))
         (uni (if (and (> unicode 31) (< unicode 127))
                  (format " '%c'" unicode) "")))
    (format "%s mods=%d%s" named mods uni)))

(defun nemacs-gtk--lookup-key-vec (vec)
  "Look up VEC against the active keymap chain, preferring
`emacs-keymap-key-binding' (= our substrate's keymap walker)."
  (cond
   ((fboundp 'emacs-keymap-key-binding) (emacs-keymap-key-binding vec))
   ((fboundp 'key-binding) (key-binding vec))
   (t nil)))

(defun nemacs-gtk--keymap-binding-p (binding)
  "Return non-nil when BINDING (= the result of a keymap lookup) is
itself a keymap (= a prefix mid-sequence)."
  (or (and (fboundp 'emacs-keymap-keymapp) (emacs-keymap-keymapp binding))
      (and (fboundp 'keymapp) (keymapp binding))))

(defun nemacs-gtk--describe-key-vec (vec)
  "Return a human-readable echo string for the prefix VEC."
  (let ((parts '())
        (i 0)
        (n (length vec)))
    (while (< i n)
      (let ((ev (aref vec i)))
        (push
         (cond
          ((symbolp ev) (symbol-name ev))
          ((and (integerp ev) (> ev 0) (< ev 27))
           (format "C-%c" (+ ev (1- ?a))))
          ((integerp ev) (format "%c" ev))
          (t (format "%S" ev)))
         parts))
      (setq i (1+ i)))
    (mapconcat 'identity (nreverse parts) " ")))

(defun nemacs-gtk--dispatch-key (keysym mods unicode)
  "Translate a GDK key event + run one dispatch step against the
active buffer.  When the minibuffer is active, route through
`nemacs-gtk--minibuffer-handle-key' instead of the keymap.

Alt modifier folding: when GDK reports Alt+KEY (= ALT_MASK bit
set + a translated event), prepend 27 (= Esc) to the event so
the same Esc-prefix sub-keymap that `Esc x' targets is reached.
This is the canonical terminal-style Meta fallback — a single
`Alt+x' produces the [27 ?x] sequence which already binds to
`execute-extended-command'.

Prefix-key accumulation: when a partial sequence resolves to a
keymap (= `C-x' / `Esc' partial), stage it on
`nemacs-gtk--pending-prefix' and wait for the next event.

After the command runs, ensure the cursor stays inside the
viewport."
  (let* ((alt-p (= (logand mods nemacs-gtk--gdk-alt-mask)
                   nemacs-gtk--gdk-alt-mask))
         (event (nemacs-gtk--key-event->command-loop-event
                 keysym mods unicode))
         ;; Alt-prefix folds to a 2-event vec; bare keys to a 1-event vec.
         (event-vec (cond
                     ((null event) nil)
                     (alt-p        (vector 27 event))
                     (t            (vector event)))))
    (when event-vec
      (cond
       (nemacs-gtk--minibuffer-active
        ;; Minibuffer eats events one at a time.  Alt+KEY in
        ;; minibuffer-mode degenerates to KEY (= drop the Esc
        ;; prefix); the user pressing Alt while typing into a
        ;; prompt almost certainly means the bare letter.
        (nemacs-gtk--minibuffer-handle-key event))
       (nemacs-gtk--isearch-active
        ;; Same: isearch eats one event at a time, Alt-prefix
        ;; dropped (= a literal letter is what the user wants
        ;; mid-search).  Auto-scroll to whatever match-row point
        ;; landed on so the cursor stays visible.
        (nemacs-gtk--isearch-handle-key event)
        (nemacs-gtk--ensure-cursor-visible))
       (t
        ;; Phase 2.Q shift-select: only at top-level (= no pending
        ;; prefix), let Shift+motion auto-set the mark and a plain
        ;; motion auto-deactivate it.  Inside a `C-x'-style prefix
        ;; this is skipped because the user is mid-command and we
        ;; don't want to mutate the region from incomplete input.
        (when (null nemacs-gtk--pending-prefix)
          (nemacs-gtk--shift-arrow-pre-dispatch event mods))
        (let* ((accumulated (vconcat (or nemacs-gtk--pending-prefix [])
                                     event-vec))
               (binding (nemacs-gtk--lookup-key-vec accumulated)))
          (cond
           ((nemacs-gtk--keymap-binding-p binding)
            (setq nemacs-gtk--pending-prefix accumulated)
            (setq nemacs-gtk--last-key-text
                  (format "%s-" (nemacs-gtk--describe-key-vec accumulated))))
           (t
            (setq nemacs-gtk--pending-prefix nil)
            (with-current-buffer (nemacs-gtk--active-buffer)
              (apply #'emacs-command-loop-feed-events
                     (append accumulated nil))
              (emacs-command-loop-step))
            (nemacs-gtk--ensure-cursor-visible)))))))))


;;;; --- clipboard glue ------------------------------------------------------

(defun nemacs-gtk--clipboard-cut-fn (text)
  "Push TEXT onto the GTK system clipboard.  Wired into
`interprogram-cut-function' so any `kill-new' (= `copy-region-as-kill',
`kill-region', `kill-line') automatically mirrors onto the clipboard."
  (when (and (stringp text) (> (length text) 0))
    (nelisp-gtk-clipboard-set text)
    (setq nemacs-gtk--clipboard-cache text)))

(defun nemacs-gtk--clipboard-paste-fn ()
  "Return the current GTK clipboard text, or nil when it matches our
last cut (= nothing newer to surface for `yank').  Wired into
`interprogram-paste-function'."
  (let ((text (nelisp-gtk-clipboard-get)))
    (cond
     ((null text) nil)
     ((equal text nemacs-gtk--clipboard-cache) nil)
     (t text))))

(defun nemacs-gtk--install-clipboard-glue ()
  "Install the GTK clipboard ↔ kill-ring bridge by setting
`interprogram-cut-function' and `interprogram-paste-function'.
Idempotent — re-installing replaces the same two function slots."
  (setq interprogram-cut-function   #'nemacs-gtk--clipboard-cut-fn)
  (setq interprogram-paste-function #'nemacs-gtk--clipboard-paste-fn))


;;;; --- menu dispatch -------------------------------------------------------

(defun nemacs-gtk--current-line-bounds ()
  "Return (BEG . END) for the current line in `*welcome*' (= nelisp-ec
point coords, both inclusive of `line-beginning-position', exclusive
of `line-end-position'+1)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (cons (line-beginning-position) (line-end-position))))

(defun nemacs-gtk--menu-copy-current-line ()
  "Copy the current line of `*welcome*' onto kill-ring (and via the
installed cut hook, the system clipboard)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((b (line-beginning-position))
           (e (line-end-position)))
      (if (= b e)
          (setq nemacs-gtk--last-key-text "Copy: line is empty")
        (copy-region-as-kill b e)
        (setq nemacs-gtk--last-key-text
              (format "Copied %d chars" (- e b)))))))

(defun nemacs-gtk--menu-cut-current-line ()
  "Cut the current line of `*welcome*' (= push to kill-ring + delete)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((b (line-beginning-position))
           (e (line-end-position)))
      (if (= b e)
          (setq nemacs-gtk--last-key-text "Cut: line is empty")
        (kill-region b e)
        (setq nemacs-gtk--last-key-text
              (format "Cut %d chars" (- e b)))))))

(defun nemacs-gtk--menu-paste ()
  "Paste from kill-ring (= clipboard via `interprogram-paste-function')
into `*welcome*' at point."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (yank)
    (setq nemacs-gtk--last-key-text "Pasted from clipboard")))

(defun nemacs-gtk--menu-open-file ()
  "Pop the GTK4 native open-file dialog (= `(nelisp-gtk-show-open-dialog)'),
load the chosen file via `find-file-noselect', and switch the active
buffer to the loaded one so the next repaint shows it.  Cancelled
dialogs leave the current buffer in place."
  (let ((path (nelisp-gtk-show-open-dialog "Open File")))
    (cond
     ((null path)
      (setq nemacs-gtk--last-key-text "Open: cancelled"))
     (t
      (let ((buf (find-file-noselect path)))
        (cond
         ((null buf)
          (setq nemacs-gtk--last-key-text
                (format "Open failed: %s" path)))
         (t
          (setq nemacs-gtk--active-buffer-name (buffer-name buf))
          (setq nemacs-gtk--scroll-offset 0)
          (nemacs-gtk--sync-window-title)
          (setq nemacs-gtk--last-key-text
                (format "Opened: %s" path)))))))))

(defun nemacs-gtk--menu-save-file ()
  "Save the active buffer.  When it visits a file, call `save-buffer'
directly.  Otherwise pop the GTK save-file dialog seeded with the
buffer name, then `write-file' the buffer to the chosen path (=
sets visited-file-name + writes contents)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((bn (buffer-name))
           (visited (and (fboundp 'buffer-file-name) (buffer-file-name))))
      (cond
       (visited
        (save-buffer)
        (setq nemacs-gtk--last-key-text (format "Saved: %s" visited)))
       (t
        (let ((path (nelisp-gtk-show-save-dialog "Save As..." bn)))
          (cond
           ((null path)
            (setq nemacs-gtk--last-key-text "Save: cancelled"))
           (t
            (write-file path)
            ;; `write-file' updated the buffer's visited file; refresh
            ;; the active-buffer-name in case the buffer was renamed.
            (setq nemacs-gtk--active-buffer-name (buffer-name))
            (nemacs-gtk--sync-window-title)
            (setq nemacs-gtk--last-key-text
                  (format "Saved as: %s" path))))))))))

(defun nemacs-gtk--handle-menu-action (action)
  "Dispatch a menu click — ACTION is the leaf's name-string from
`nemacs-gtk--menu-spec'.  Cut/Copy/Paste operate on the current line
of `*welcome*' (= region/mark API not yet wired in this MVP); the
clipboard bridge handles cross-app sync via the installed
`interprogram-cut-function' / `-paste-function'."
  (cond
   ((string= action "quit")
    ;; Synthesize the same close path the WM-X button takes.  Layer 3
    ;; sets `nemacs-gtk--quit-requested' which the main loop checks.
    (setq nemacs-gtk--last-key-text "menu: Quit")
    (setq nemacs-gtk--quit-requested t))
   ((string= action "open")        (nemacs-gtk--menu-open-file))
   ((string= action "save")        (nemacs-gtk--menu-save-file))
   ((string= action "cut")         (nemacs-gtk-kill-region))
   ((string= action "copy")        (nemacs-gtk-copy-region))
   ((string= action "paste")       (nemacs-gtk--menu-paste))
   ((string= action "select-all")  (nemacs-gtk-mark-whole-buffer))
   ((string= action "about")
    (setq nemacs-gtk--last-key-text "nemacs-gtk Phase 2 — elisp-driven"))
   (t
    (setq nemacs-gtk--last-key-text (format "menu: %s (unhandled)" action)))))


;;;; --- mouse dispatch ------------------------------------------------------

(defun nemacs-gtk-mouse-set-point ()
  "Bound to `mouse-1' in the GUI's global keymap.  Move point in the
active buffer to the cell stored on `nemacs-gtk--last-mouse-event'.

Mirrors Emacs' `mouse-set-point' contract — the GUI emits a synthetic
`mouse-1' event into the command loop; the handler stages the event
on a defvar and the command consumes it.  This keeps the keymap
binding ordinary (= no `(interactive \"e\")' event-arg plumbing) and
matches how the keyboard dispatch already works.

Phase 2.U: also clears any active region (= mouse-1 click without a
drag should drop a stale shift-select region) and stashes the click
position on `--press-point' so the first drag motion can anchor the
mark there."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when ev
      (let ((p (nemacs-gtk--cell-to-point row col)))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char p))
        (nemacs-gtk--deactivate-mark)
        (setq nemacs-gtk--press-point p)
        (setq nemacs-gtk--last-key-text
              (format "mouse-1 → point %d (cell %d,%d)" p row col))))))

(defun nemacs-gtk-mouse-drag-region ()
  "Bound to `mouse-drag-1' (= mouse-1 motion while held).  Extend the
region between `--press-point' and the current drag cell.

First drag motion since the press: stamps the mark at `--press-point'
so the region is anchored at the click.  Subsequent motions only
update point — the mark stays put, region grows/shrinks naturally.

No-op when there's no remembered press point (= drag arrived
without a preceding press, defensive)."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when (and ev nemacs-gtk--press-point)
      (let ((p (nemacs-gtk--cell-to-point row col))
            (bn nemacs-gtk--active-buffer-name))
        ;; Mark gets anchored at the press position the first time we
        ;; drag — stamping `--shift-region' nil keeps it sticky (=
        ;; user-driven, not auto-deactivated by a plain motion key).
        (unless (and nemacs-gtk--mark-pos
                     (equal nemacs-gtk--mark-buffer bn))
          (setq nemacs-gtk--mark-pos     nemacs-gtk--press-point)
          (setq nemacs-gtk--mark-buffer  bn)
          (setq nemacs-gtk--shift-region nil))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char p))
        (setq nemacs-gtk--last-key-text
              (format "drag → %d..%d" nemacs-gtk--mark-pos p))))))

(defun nemacs-gtk--word-char-p (c)
  "Return non-nil when integer C (= a single-byte character) is a
word constituent for the click-to-select-word feature.  Conservative
ASCII-only set: alphanumeric + underscore — matches the substrate's
default word syntax for the MVP."
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))
      (eq c ?_)))

(defun nemacs-gtk--word-bounds-at (p)
  "Return (BEG . END) of the word containing point P, or nil when P
is not on a word constituent.  BEG / END are 1-based buffer positions
in the active buffer.  Phase 2.V helper for `select-word'."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((s    (buffer-string))
           (pmin (nelisp-ec-point-min))
           (idx  (- p pmin))
           (len  (length s)))
      (cond
       ((or (< idx 0) (>= idx len)) nil)
       ((not (nemacs-gtk--word-char-p (aref s idx))) nil)
       (t
        (let ((b idx))
          (while (and (> b 0)
                      (nemacs-gtk--word-char-p (aref s (1- b))))
            (setq b (1- b)))
          (let ((e idx))
            (while (and (< e len)
                        (nemacs-gtk--word-char-p (aref s e)))
              (setq e (1+ e)))
            (cons (+ pmin b) (+ pmin e)))))))))

(defun nemacs-gtk-mouse-select-word ()
  "Bound to `mouse-double-1' (Phase 2.V).  Select the word at the
click position.  Sets the mark at the word's start, point at its
end.  Echoes when the click lands on whitespace."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when ev
      (let* ((p      (nemacs-gtk--cell-to-point row col))
             (bounds (nemacs-gtk--word-bounds-at p))
             (bn     nemacs-gtk--active-buffer-name))
        (cond
         (bounds
          (with-current-buffer (nemacs-gtk--active-buffer)
            (nelisp-ec-goto-char (cdr bounds)))
          (setq nemacs-gtk--mark-pos     (car bounds))
          (setq nemacs-gtk--mark-buffer  bn)
          (setq nemacs-gtk--shift-region nil)
          (setq nemacs-gtk--last-key-text
                (format "Selected word (%d chars)"
                        (- (cdr bounds) (car bounds)))))
         (t
          (setq nemacs-gtk--last-key-text "double-click: no word at point")))))))

(defun nemacs-gtk-mouse-select-line ()
  "Bound to `mouse-triple-1' (Phase 2.V).  Select the line at the
click position.  Mark at line-beginning-position, point at
line-end-position."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when ev
      (let* ((p  (nemacs-gtk--cell-to-point row col))
             (bn nemacs-gtk--active-buffer-name))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char p)
          (let* ((b (line-beginning-position))
                 (e (line-end-position)))
            (nelisp-ec-goto-char e)
            (setq nemacs-gtk--mark-pos     b)
            (setq nemacs-gtk--mark-buffer  bn)
            (setq nemacs-gtk--shift-region nil)
            (setq nemacs-gtk--last-key-text
                  (format "Selected line (%d chars)" (- e b)))))))))

(defun nemacs-gtk--handle-mouse-event (ev)
  "Dispatch a mouse event surfaced by `(nelisp-gtk-poll-mouse)'.  EV is
the (KIND BUTTON ROW COL MODS N-PRESS) tuple — KIND is `'press' /
`'release' / `'motion' / `'scroll-up' / `'scroll-down'.  N-PRESS is
the GestureClick click-count (1 / 2 / 3+) — used to dispatch
single / double / triple click events.

Left-button (= button 1) press inside the buffer area routes through
`emacs-command-loop' as a synthetic `mouse-1' event so the keymap
binding (= `nemacs-gtk-mouse-set-point') decides what to do.  This
makes mouse-1 a real Emacs command — `M-x global-set-key
mouse-1 ...' would just work.

Other cases (button 2/3 press, scroll wheel, clicks on mode-line /
echo area) still echo placeholder strings — the command-loop
routing is incremental and lands here as `mouse-2' / `mouse-3' /
`wheel-up' / `wheel-down' bindings get added in later phases.
Release is intentionally silent so click events don't double-fire."
  (let ((kind   (nth 0 ev))
        (button (nth 1 ev))
        (row    (nth 2 ev))
        (col    (nth 3 ev)))
    (cond
     ((and (eq kind 'press)
           (= button 1)
           (< row nemacs-gtk--buffer-area-end))
      ;; Phase 2.V: dispatch click count → single / double / triple
      ;; mouse-1 events.  GestureClick reports n_press as the running
      ;; count (= 1, 2, 3, 4, ...), so an n_press >= 3 maps to triple-1
      ;; (= line select, no further escalation).
      (let ((n-press (or (nth 5 ev) 1))
            (event   nil))
        (setq nemacs-gtk--last-mouse-event ev)
        (setq event
              (cond
               ((<= n-press 1) 'mouse-1)
               ((= n-press 2)  'mouse-double-1)
               (t              'mouse-triple-1)))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (emacs-command-loop-feed-events event)
          (emacs-command-loop-step)))
      (nemacs-gtk--ensure-cursor-visible))
     ;; Phase 2.U: motion with button-1 held = drag region.  Routes
     ;; through `mouse-drag-1' so the keymap binding decides the
     ;; behavior.  Confined to the buffer area so dragging onto the
     ;; mode-line / echo area stops extending.
     ((and (eq kind 'motion)
           (= button 1)
           (< row nemacs-gtk--buffer-area-end))
      (setq nemacs-gtk--last-mouse-event ev)
      (with-current-buffer (nemacs-gtk--active-buffer)
        (emacs-command-loop-feed-events 'mouse-drag-1)
        (emacs-command-loop-step))
      (nemacs-gtk--ensure-cursor-visible))
     ((and (eq kind 'press)
           (= button 2)
           (< row nemacs-gtk--buffer-area-end))
      (setq nemacs-gtk--last-mouse-event ev)
      (with-current-buffer (nemacs-gtk--active-buffer)
        (emacs-command-loop-feed-events 'mouse-2)
        (emacs-command-loop-step))
      (nemacs-gtk--ensure-cursor-visible))
     ;; Right-click (= button 3): pop the context menu at the click
     ;; cell.  Phase 2.S — reuses the menu_event_queue so existing
     ;; `--handle-menu-action' dispatches Cut/Copy/Paste/Select All.
     ((and (eq kind 'press)
           (= button 3)
           (< row nemacs-gtk--buffer-area-end))
      (when (fboundp 'nelisp-gtk-show-context-menu)
        (nelisp-gtk-show-context-menu nemacs-gtk--context-menu-spec row col))
      (setq nemacs-gtk--last-key-text
            (format "Context menu @ (%d,%d)" row col)))
     ((eq kind 'press)
      (setq nemacs-gtk--last-key-text
            (format "mouse-%d press @ (%d,%d)" button row col)))
     ((eq kind 'scroll-up)
      (nemacs-gtk--scroll-by (- nemacs-gtk--scroll-step))
      (setq nemacs-gtk--last-key-text
            (format "scroll up → offset %d" nemacs-gtk--scroll-offset)))
     ((eq kind 'scroll-down)
      (nemacs-gtk--scroll-by nemacs-gtk--scroll-step)
      (setq nemacs-gtk--last-key-text
            (format "scroll down → offset %d" nemacs-gtk--scroll-offset)))
     ;; release: silent (covered by the press echo).
     (t nil))))


;;;; --- main entry -----------------------------------------------------------

;;;###autoload
(defun nemacs-gtk-main ()
  "Entry point invoked by the Rust boot stub.  Brings up the GUI,
paints the initial frame, and drives the main loop until the window
is closed."
  ;; 1. GTK init.
  (nelisp-gtk-init nemacs-gtk--rows nemacs-gtk--cols)
  (nelisp-gtk-set-mode-line-row nemacs-gtk--mode-line-row)
  ;; 2. Native menu bar (= Phase 2.A re-add, now elisp-driven).
  (nelisp-gtk-set-menu-bar nemacs-gtk--menu-spec)
  ;; 3. System clipboard bridge — kill-ring ↔ GTK clipboard
  ;; (Phase 2.C, lives behind the substrate's `interprogram-*'
  ;; hook points so `kill-new' / `yank' transparently sync).
  (nemacs-gtk--install-clipboard-glue)
  ;; 4. Layer 2 keymap + welcome buffer.
  (nemacs-gtk--init-keymap)
  (nemacs-gtk--prepare-welcome-buffer)
  (nemacs-gtk--sync-window-title)
  ;; 4. First paint.
  (nemacs-gtk--repaint)
  ;; 5. Main loop — drains both the key queue and the menu queue
  ;; per iteration so a single `iterate(t)' wake handles whichever
  ;; channel fired.
  (setq nemacs-gtk--quit-requested nil)
  (while (and (not (nelisp-gtk-should-quit))
              (not nemacs-gtk--quit-requested))
    (nelisp-gtk-iterate t)
    (let ((kv (nelisp-gtk-poll-key)))
      (when kv
        (let ((keysym (car kv))
              (mods   (cadr kv))
              (uni    (car (cddr kv))))
          (setq nemacs-gtk--last-key-text
                (nemacs-gtk--describe-key keysym mods uni))
          (nemacs-gtk--dispatch-key keysym mods uni)
          (nemacs-gtk--repaint))))
    (let ((m (nelisp-gtk-poll-menu-event)))
      (when m
        (nemacs-gtk--handle-menu-action m)
        (nemacs-gtk--repaint)))
    (let ((mev (nelisp-gtk-poll-mouse)))
      (when mev
        (nemacs-gtk--handle-mouse-event mev)
        (nemacs-gtk--repaint)))
    (let ((rs (nelisp-gtk-poll-resize)))
      (when rs
        (nemacs-gtk--apply-grid-size (nth 0 rs) (nth 1 rs))
        (nemacs-gtk--repaint))))
  'done)

(provide 'nemacs-gtk-frontend)

;;; nemacs-gtk-frontend.el ends here
