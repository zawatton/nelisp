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
(require 'emacs-mode-builtins)
(require 'cl-lib)

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

(defvar nemacs-gtk--windows nil
  "Phase 2.AU+AW — list of windows, or nil for single-window mode.
Each entry is a plist `(:buffer NAME :scroll N :top-row R :rows N
:left-col C :cols N)'.  When non-nil, the entry at
`--current-window-idx' mirrors the global `--active-buffer-name' /
`--scroll-offset' state.

`:left-col' / `:cols' identify the rectangle's horizontal band — both
horizontal (Phase 2.AU C-x 2) and vertical (Phase 2.AW C-x 3) splits
mutate windows into rectangles inside the buffer area.  Mode-line
of each window goes on row `(:top-row + :rows - 1)' starting at
column `:left-col' for `:cols' chars.")

(defvar nemacs-gtk--current-window-idx 0
  "Phase 2.AU — index of the current window into `--windows'.  Only
meaningful when `--windows' is non-nil.")

(defvar nemacs-gtk--registers nil
  "Phase 2.AX — alist of `(CHAR . VALUE)' register entries.  VALUE is
either a string (= `copy-to-register' / `insert-register') or a
list `(:point BUFFER-NAME POS)' (= `point-to-register' /
`jump-to-register').  CHAR is the register name = the key the user
typed after the `C-x r' prefix.")

(defvar nemacs-gtk--register-pending-op nil
  "Phase 2.AX — operation symbol waiting for its register-name char.
nil = no pending register op; otherwise one of `copy' / `insert' /
`point' / `jump'.  The dispatcher consumes the next event as the
register name when this is non-nil.")

(defvar nemacs-gtk--bookmarks nil
  "Phase 2.AZ — alist of `(NAME . (:buffer BUFFER-NAME :pos POS))'
bookmark entries.  Unlike registers, bookmark names are strings
(= read via the minibuffer) so users can hand them meaningful
labels.  In-memory only — not persisted to disk in MVP.")

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
    ;; Phase 2.AB — C-v = PageDown (= scroll-up-command).
    (define-key m (vector ?\C-v) 'nemacs-gtk-page-down)
    (define-key m (vector ?\C-t) 'nemacs-gtk-transpose-chars)
    (define-key m (vector ?\C-s) 'nemacs-gtk-isearch-forward)
    (define-key m (vector ?\C-r) 'nemacs-gtk-isearch-backward)
    (define-key m (vector ?\C-w) 'nemacs-gtk-kill-region)
    (define-key m (vector ?\C-g) 'nemacs-gtk-keyboard-quit)
    ;; Phase 2.AF — C-q = quoted-insert (= insert next char literal).
    (define-key m (vector ?\C-q) 'nemacs-gtk-quoted-insert)
    ;; Phase 2.AG — C-/ + C-_ = undo.  Both keysyms surface as the
    ;; same (control + slash/underscore) chord depending on locale.
    (define-key m (vector ?\C-/) 'nemacs-gtk-undo)
    (define-key m (vector ?\C-_) 'nemacs-gtk-undo)
    ;; Phase 2.AH — C-l = recenter (point's row → middle of viewport).
    (define-key m (vector ?\C-l) 'nemacs-gtk-recenter)
    ;; Phase 2.AI — Insert key toggles overwrite-mode.
    (define-key m (vector 'insert) 'nemacs-gtk-overwrite-mode)
    ;; Phase 2.AJ — C-h prefix.  C-h k = describe-key (= consume next key
    ;; raw + report binding).  Other C-h chords reserved for future help.
    (let ((help-map (make-sparse-keymap)))
      (define-key help-map (vector ?k) 'nemacs-gtk-describe-key)
      (define-key help-map (vector ?b) 'nemacs-gtk-describe-bindings)
      ;; Phase 2.BD — function / variable / apropos help.
      (define-key help-map (vector ?f) 'nemacs-gtk-describe-function)
      (define-key help-map (vector ?v) 'nemacs-gtk-describe-variable)
      (define-key help-map (vector ?a) 'nemacs-gtk-apropos)
      (define-key m (vector ?\C-h) help-map))
    ;; C-SPC = ?\C-@ = byte 0
    (define-key m (vector 0) 'nemacs-gtk-set-mark-command)
    ;; Phase 2.BI — C-z = iconify-frame (= minimize the GTK window).
    (define-key m (vector ?\C-z) 'nemacs-gtk-iconify-frame)
    ;; C-x prefix map — common substrate-level commands behind the
    ;; same handlers the menu uses.
    (define-key ctl-x-map (vector ?\C-s) 'nemacs-gtk-keyboard-save)
    (define-key ctl-x-map (vector ?\C-f) 'nemacs-gtk-keyboard-find-file)
    (define-key ctl-x-map (vector ?b)   'nemacs-gtk-switch-to-buffer)
    ;; Phase 2.AC — `C-x C-b' = popup buffer-menu (= context-menu).
    (define-key ctl-x-map (vector ?\C-b) 'nemacs-gtk-buffer-menu)
    (define-key ctl-x-map (vector ?k)   'nemacs-gtk-kill-buffer)
    (define-key ctl-x-map (vector ?\C-c) 'nemacs-gtk-save-buffers-kill-emacs)
    ;; Phase 2.AG — `C-x u' = undo (alternative chord).
    (define-key ctl-x-map (vector ?u)   'nemacs-gtk-undo)
    ;; Phase 2.AN — `C-x C-x' = exchange-point-and-mark.
    (define-key ctl-x-map (vector ?\C-x) 'nemacs-gtk-exchange-point-and-mark)
    ;; Phase 2.AO — `C-x C-w' = write-file (save-as).
    (define-key ctl-x-map (vector ?\C-w) 'nemacs-gtk-write-file)
    ;; Phase 2.AO — `C-x s' = save-some-buffers (= save all dirty).
    (define-key ctl-x-map (vector ?s)    'nemacs-gtk-save-some-buffers)
    ;; Phase 2.AP — kbd-macro recording.
    (define-key ctl-x-map (vector ?\() 'nemacs-gtk-start-kbd-macro)
    (define-key ctl-x-map (vector ?\)) 'nemacs-gtk-end-kbd-macro)
    (define-key ctl-x-map (vector ?e)  'nemacs-gtk-call-last-kbd-macro)
    ;; Phase 2.AQ — `C-x C-q' = toggle-read-only.
    (define-key ctl-x-map (vector ?\C-q) 'nemacs-gtk-toggle-read-only)
    ;; Phase 2.AT — `C-x =' = what-cursor-position.
    (define-key ctl-x-map (vector ?=)    'nemacs-gtk-what-cursor-position)
    ;; Phase 2.AU+AW — window splitting.
    (define-key ctl-x-map (vector ?2)    'nemacs-gtk-split-window-below)
    (define-key ctl-x-map (vector ?3)    'nemacs-gtk-split-window-right)
    (define-key ctl-x-map (vector ?0)    'nemacs-gtk-delete-window)
    (define-key ctl-x-map (vector ?1)    'nemacs-gtk-delete-other-windows)
    (define-key ctl-x-map (vector ?o)    'nemacs-gtk-other-window)
    ;; Phase 2.AV — `C-x ^' = enlarge-window (= +1 row from next).
    (define-key ctl-x-map (vector ?^)    'nemacs-gtk-enlarge-window)
    ;; Phase 2.BB — `C-x C-o' = delete-blank-lines, `C-x f' = set-fill-column.
    (define-key ctl-x-map (vector ?\C-o) 'nemacs-gtk-delete-blank-lines)
    (define-key ctl-x-map (vector ?f)    'nemacs-gtk-set-fill-column)
    ;; Phase 2.BC — `C-x n' prefix → narrowing.
    (let ((c-x-n-map (make-sparse-keymap)))
      (define-key c-x-n-map (vector ?n) 'nemacs-gtk-narrow-to-region)
      (define-key c-x-n-map (vector ?w) 'nemacs-gtk-widen)
      (define-key c-x-n-map (vector ?d) 'nemacs-gtk-narrow-to-defun)
      (define-key ctl-x-map (vector ?n) c-x-n-map))
    ;; Phase 2.BE — `C-x DEL' = backward-kill-sentence, `C-x z' = repeat.
    (define-key ctl-x-map (vector 127) 'nemacs-gtk-backward-kill-sentence)
    (define-key ctl-x-map (vector ?z)  'nemacs-gtk-repeat)
    ;; Phase 2.BF — `C-x +' / `C-x -' = font zoom in / out.
    (define-key ctl-x-map (vector ?+) 'nemacs-gtk-text-scale-increase)
    (define-key ctl-x-map (vector ?-) 'nemacs-gtk-text-scale-decrease)
    ;; Phase 2.BG — `C-x C-d' / `C-x C-v' = list-dir / find-alternate.
    (define-key ctl-x-map (vector ?\C-d) 'nemacs-gtk-list-directory)
    (define-key ctl-x-map (vector ?\C-v) 'nemacs-gtk-find-alternate-file)
    ;; Phase 2.BJ — `C-x C-u' / `C-x C-l' = upcase / downcase region.
    (define-key ctl-x-map (vector ?\C-u) 'nemacs-gtk-upcase-region)
    (define-key ctl-x-map (vector ?\C-l) 'nemacs-gtk-downcase-region)
    ;; Phase 2.AX/AZ — `C-x r' prefix → registers + bookmarks.
    (let ((c-x-r-map (make-sparse-keymap)))
      (define-key c-x-r-map (vector ?s)  'nemacs-gtk-copy-to-register)
      (define-key c-x-r-map (vector ?i)  'nemacs-gtk-insert-register)
      (define-key c-x-r-map (vector ?\s) 'nemacs-gtk-point-to-register)
      (define-key c-x-r-map (vector ?j)  'nemacs-gtk-jump-to-register)
      ;; Phase 2.AZ — bookmarks.
      (define-key c-x-r-map (vector ?m)  'nemacs-gtk-bookmark-set)
      (define-key c-x-r-map (vector ?b)  'nemacs-gtk-bookmark-jump)
      (define-key c-x-r-map (vector ?l)  'nemacs-gtk-bookmark-list)
      (define-key ctl-x-map (vector ?r) c-x-r-map))
    (define-key m (vector ?\C-x) ctl-x-map)
    ;; Mouse-2 (= middle click) → set point + yank, mirroring real
    ;; Emacs's `mouse-yank-primary' / Linux X-clipboard convention.
    (define-key m (vector 'mouse-2) 'nemacs-gtk-mouse-yank-primary)
    ;; Esc-prefix → meta commands.  Reached either by pressing Esc
    ;; explicitly (= old terminal style) or by Alt+KEY which the
    ;; dispatch-key Alt-folding rewrites to the same 2-event vec.
    (let ((esc-map  (make-sparse-keymap))
          (meta-g-map (make-sparse-keymap)))
      (define-key esc-map (vector ?x) 'execute-extended-command)
      (define-key esc-map (vector ?f) 'forward-word)
      (define-key esc-map (vector ?b) 'backward-word)
      (define-key esc-map (vector ?d) 'nemacs-gtk-meta-kill-word)
      (define-key esc-map (vector ?w) 'nemacs-gtk-copy-region)
      (define-key esc-map (vector ?<) 'nemacs-gtk-meta-beginning-of-buffer)
      (define-key esc-map (vector ?>) 'nemacs-gtk-meta-end-of-buffer)
      ;; Phase 2.Y — case-change words + paragraph navigation.
      (define-key esc-map (vector ?u) 'nemacs-gtk-upcase-word)
      (define-key esc-map (vector ?l) 'nemacs-gtk-downcase-word)
      (define-key esc-map (vector ?c) 'nemacs-gtk-capitalize-word)
      (define-key esc-map (vector ?{) 'nemacs-gtk-backward-paragraph)
      (define-key esc-map (vector ?}) 'nemacs-gtk-forward-paragraph)
      ;; Phase 2.Z — whitespace ops (= M-SPC / M-\\).
      (define-key esc-map (vector ?\s) 'nemacs-gtk-just-one-space)
      (define-key esc-map (vector ?\\) 'nemacs-gtk-delete-horizontal-space)
      ;; Phase 2.AA — M-y = yank-pop after C-y (cycle kill-ring).
      (define-key esc-map (vector ?y) 'nemacs-gtk-yank-pop)
      ;; Phase 2.AB — M-v = PageUp (= scroll-down-command).
      (define-key esc-map (vector ?v) 'nemacs-gtk-page-up)
      ;; Phase 2.AD — M-= = count-words-region.
      (define-key esc-map (vector ?=) 'nemacs-gtk-count-words-region)
      ;; Phase 2.AE — M-z = zap-to-char.
      (define-key esc-map (vector ?z) 'nemacs-gtk-zap-to-char)
      ;; Phase 2.AK — M-% = query-replace.
      (define-key esc-map (vector ?%) 'nemacs-gtk-query-replace)
      ;; Phase 2.AL — M-h = mark-paragraph.
      (define-key esc-map (vector ?h) 'nemacs-gtk-mark-paragraph)
      ;; Phase 2.AM — M-; = comment-dwim.
      (define-key esc-map (vector ?\;) 'nemacs-gtk-comment-dwim)
      ;; Phase 2.AN — bundle.
      (define-key esc-map (vector ?i) 'nemacs-gtk-tab-to-tab-stop)
      (define-key esc-map (vector ?q) 'nemacs-gtk-fill-paragraph)
      (define-key esc-map (vector ?^) 'nemacs-gtk-delete-indentation)
      ;; Phase 2.AO — `M-/' = dabbrev-expand.
      (define-key esc-map (vector ?/) 'nemacs-gtk-dabbrev-expand)
      ;; Phase 2.AR — M-: = eval-expression.
      (define-key esc-map (vector ?:) 'nemacs-gtk-eval-expression)
      ;; Phase 2.AS — M-! = shell-command, M-| = shell-command-on-region.
      (define-key esc-map (vector ?!) 'nemacs-gtk-shell-command)
      (define-key esc-map (vector ?|) 'nemacs-gtk-shell-command-on-region)
      ;; Phase 2.AY — sexp navigation (C-M-f/b/k = [27 C-f/C-b/C-k]).
      (define-key esc-map (vector ?\C-f) 'nemacs-gtk-forward-sexp)
      (define-key esc-map (vector ?\C-b) 'nemacs-gtk-backward-sexp)
      (define-key esc-map (vector ?\C-k) 'nemacs-gtk-kill-sexp)
      ;; Phase 2.BA — sentence motion + mark-defun.
      (define-key esc-map (vector ?a)    'nemacs-gtk-backward-sentence)
      (define-key esc-map (vector ?e)    'nemacs-gtk-forward-sentence)
      (define-key esc-map (vector ?\C-h) 'nemacs-gtk-mark-defun)
      ;; Phase 2.BB — M-k = kill-sentence.
      (define-key esc-map (vector ?k)    'nemacs-gtk-kill-sentence)
      ;; Phase 2.BE — C-M-SPC = mark-sexp, M-r = move-to-window-line.
      (define-key esc-map (vector 0)     'nemacs-gtk-mark-sexp)
      (define-key esc-map (vector ?r)    'nemacs-gtk-move-to-window-line-top-bottom)
      ;; Phase 2.X — `M-g g' = goto-line, `M-g M-g' aliased to same.
      (define-key meta-g-map (vector ?g)    'nemacs-gtk-goto-line)
      (define-key meta-g-map (vector ?\C-g) 'nemacs-gtk-goto-line)
      (define-key esc-map (vector ?g) meta-g-map)
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
       ;; Phase 3.A — re-mirror this buffer's recorded mode to the
       ;; substrate's `major-mode' so the mode-line + introspection
       ;; reflect the switch.  We don't re-run the mode fn (= side
       ;; effects); just promote the symbol so display reads it.
       (let ((m (nemacs-gtk--buffer-mode input)))
         (when (boundp 'major-mode) (setq major-mode m)))
       (nemacs-gtk--sync-window-title)
       (setq nemacs-gtk--last-key-text
             (format "Switched: %s" input)))))))

(defun nemacs-gtk--buffer-menu-spec ()
  "Build the popup spec list for `nemacs-gtk-buffer-menu' — one entry
per live buffer.  Each entry is `(LABEL . \"switch-to-buffer:NAME\")'.
Hidden buffers (= names starting with space) are filtered out."
  (let ((acc '()))
    (dolist (b (and (fboundp 'buffer-list) (buffer-list)))
      (let* ((name (and (fboundp 'buffer-name) (buffer-name b)))
             (file (and name (fboundp 'buffer-file-name)
                        (buffer-file-name b)))
             (modp (and name (nemacs-gtk--buffer-modified-p b))))
        (when (and (stringp name)
                   (> (length name) 0)
                   (not (eq (aref name 0) ?\s)))
          (let ((label (cond
                        (file (format "%s%s  (%s)"
                                      (if modp "* " "  ") name file))
                        (t (format "%s%s" (if modp "* " "  ") name)))))
            (push (cons label (concat "switch-to-buffer:" name)) acc)))))
    (nreverse acc)))

(defun nemacs-gtk-buffer-menu ()
  "Bound to `C-x C-b' — show a popup of all live buffers; clicking
one switches to it.  Implemented via `nelisp-gtk-show-context-menu'
so dispatch reuses `--handle-menu-action'.  Falls back to inline
echo when GTK isn't initialised (= TUI smoke / batch tests)."
  (interactive)
  (let ((spec (nemacs-gtk--buffer-menu-spec)))
    (cond
     ((null spec)
      (setq nemacs-gtk--last-key-text "buffer-menu: empty"))
     ((not (fboundp 'nelisp-gtk-show-context-menu))
      (setq nemacs-gtk--last-key-text
            (format "buffer-menu: %d buffers (no popup backend)"
                    (length spec))))
     (t
      (nelisp-gtk-show-context-menu spec 1 0)
      (setq nemacs-gtk--last-key-text
            (format "buffer-menu: %d buffers" (length spec)))))))

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

;;;; --- window splitting (Phase 2.AU) -----------------------------------

(defun nemacs-gtk--ensure-multi-windows ()
  "Materialise `--windows' from the current globals when not already
in multi-window mode.  Idempotent — when `--windows' is already
non-nil, this is a no-op.  Default rectangle: top=0, rows=buffer-area-end,
left-col=0, cols=cols (= one full-width window)."
  (unless nemacs-gtk--windows
    (setq nemacs-gtk--windows
          (list (list :buffer nemacs-gtk--active-buffer-name
                      :scroll nemacs-gtk--scroll-offset
                      :top-row 0
                      :rows nemacs-gtk--buffer-area-end
                      :left-col 0
                      :cols nemacs-gtk--cols)))
    (setq nemacs-gtk--current-window-idx 0)))

(defun nemacs-gtk--sync-current-to-window ()
  "Copy global active-buffer-name + scroll-offset into the current
window slot.  Called before mutating `--current-window-idx'."
  (when nemacs-gtk--windows
    (let* ((idx nemacs-gtk--current-window-idx)
           (cur (nth idx nemacs-gtk--windows))
           (new (plist-put cur :buffer nemacs-gtk--active-buffer-name)))
      (setq new (plist-put new :scroll nemacs-gtk--scroll-offset))
      (setcar (nthcdr idx nemacs-gtk--windows) new))))

(defun nemacs-gtk--load-window-to-globals ()
  "Copy the current window slot's buffer + scroll into globals + sync
the GTK title.  Called after mutating `--current-window-idx'."
  (when nemacs-gtk--windows
    (let* ((cur (nth nemacs-gtk--current-window-idx
                     nemacs-gtk--windows)))
      (setq nemacs-gtk--active-buffer-name (plist-get cur :buffer))
      (setq nemacs-gtk--scroll-offset (or (plist-get cur :scroll) 0))
      (nemacs-gtk--sync-window-title))))

(defun nemacs-gtk--current-window-rows ()
  "Return the row count of the current window (= `--buffer-area-end'
when single-window, else the slot's :rows)."
  (cond
   ((null nemacs-gtk--windows) nemacs-gtk--buffer-area-end)
   (t (plist-get
       (nth nemacs-gtk--current-window-idx nemacs-gtk--windows)
       :rows))))

(defun nemacs-gtk--current-window-top ()
  "Return the top-row of the current window (= 0 single-window)."
  (cond
   ((null nemacs-gtk--windows) 0)
   (t (plist-get
       (nth nemacs-gtk--current-window-idx nemacs-gtk--windows)
       :top-row))))

(defun nemacs-gtk-split-window-below ()
  "Bound to `C-x 2' — split the current window horizontally into two
halves.  The current window keeps the upper half showing the same
buffer; a new window with the same buffer + scroll occupies the
lower half.  Each window must have at least 4 rows for the split
to proceed (= 3 content + 1 inline mode-line)."
  (interactive)
  (nemacs-gtk--ensure-multi-windows)
  (nemacs-gtk--sync-current-to-window)
  (let* ((idx nemacs-gtk--current-window-idx)
         (cur (nth idx nemacs-gtk--windows))
         (top (plist-get cur :top-row))
         (rows (plist-get cur :rows))
         (half-up   (/ rows 2))
         (half-down (- rows half-up)))
    (cond
     ((< rows 8)
      (setq nemacs-gtk--last-key-text
            "split-window-below: window too small"))
     (t
      (let* ((left-col (plist-get cur :left-col))
             (cols (plist-get cur :cols))
             (cur-up (plist-put (plist-put cur :rows half-up)
                                :top-row top))
             (new-down (list :buffer (plist-get cur :buffer)
                             :scroll (plist-get cur :scroll)
                             :top-row (+ top half-up)
                             :rows half-down
                             :left-col left-col
                             :cols cols)))
        ;; replace cur-slot, then insert new after it.
        (setcar (nthcdr idx nemacs-gtk--windows) cur-up)
        (let ((after (nthcdr (1+ idx) nemacs-gtk--windows)))
          (setcdr (nthcdr idx nemacs-gtk--windows)
                  (cons new-down after))))
      (setq nemacs-gtk--last-key-text
            (format "split-window-below: %d windows"
                    (length nemacs-gtk--windows)))))))

(defun nemacs-gtk-split-window-right ()
  "Bound to `C-x 3' — split the current window vertically into two
side-by-side halves.  The current window keeps the left half, a new
window with the same buffer + scroll occupies the right.  Each
window must have at least 16 columns for the split to proceed."
  (interactive)
  (nemacs-gtk--ensure-multi-windows)
  (nemacs-gtk--sync-current-to-window)
  (let* ((idx nemacs-gtk--current-window-idx)
         (cur (nth idx nemacs-gtk--windows))
         (left (plist-get cur :left-col))
         (cols (plist-get cur :cols))
         (half-l   (/ cols 2))
         (half-r   (- cols half-l)))
    (cond
     ((< cols 32)
      (setq nemacs-gtk--last-key-text
            "split-window-right: window too narrow"))
     (t
      (let* ((top (plist-get cur :top-row))
             (rows (plist-get cur :rows))
             (cur-left (plist-put cur :cols half-l))
             (new-right (list :buffer (plist-get cur :buffer)
                              :scroll (plist-get cur :scroll)
                              :top-row top
                              :rows rows
                              :left-col (+ left half-l)
                              :cols half-r)))
        (setcar (nthcdr idx nemacs-gtk--windows) cur-left)
        (let ((after (nthcdr (1+ idx) nemacs-gtk--windows)))
          (setcdr (nthcdr idx nemacs-gtk--windows)
                  (cons new-right after))))
      (setq nemacs-gtk--last-key-text
            (format "split-window-right: %d windows"
                    (length nemacs-gtk--windows)))))))

(defun nemacs-gtk-other-window ()
  "Bound to `C-x o' — cycle the current window forward (= last window
wraps to first).  No-op + echo when only one window."
  (interactive)
  (cond
   ((or (null nemacs-gtk--windows)
        (<= (length nemacs-gtk--windows) 1))
    (setq nemacs-gtk--last-key-text "other-window: only one window"))
   (t
    (nemacs-gtk--sync-current-to-window)
    (setq nemacs-gtk--current-window-idx
          (mod (1+ nemacs-gtk--current-window-idx)
               (length nemacs-gtk--windows)))
    (nemacs-gtk--load-window-to-globals)
    (setq nemacs-gtk--last-key-text
          (format "other-window: %d/%d"
                  (1+ nemacs-gtk--current-window-idx)
                  (length nemacs-gtk--windows))))))

(defun nemacs-gtk--find-merge-sibling (idx)
  "Find a window whose rectangle can absorb the rectangle of window
IDX exactly.  Returns (DONOR-IDX EXPAND-PROP DELTA NEW-LEFT-OR-TOP)
where:
  EXPAND-PROP = `:rows' (vertical merge) or `:cols' (horizontal merge)
  DELTA       = how much to add to that prop
  NEW-LEFT-OR-TOP = donor's new :top-row (rows-merge) or :left-col
                    (cols-merge), or nil if unchanged.
Returns nil when no perfect sibling exists."
  (let* ((cur (nth idx nemacs-gtk--windows))
         (cur-top (plist-get cur :top-row))
         (cur-rows (plist-get cur :rows))
         (cur-left (plist-get cur :left-col))
         (cur-cols (plist-get cur :cols))
         (n (length nemacs-gtk--windows))
         (j 0)
         (hit nil))
    (while (and (< j n) (null hit))
      (unless (= j idx)
        (let* ((w (nth j nemacs-gtk--windows))
               (w-top (plist-get w :top-row))
               (w-rows (plist-get w :rows))
               (w-left (plist-get w :left-col))
               (w-cols (plist-get w :cols)))
          (cond
           ;; vertical merge: same column band + adjacent rows.
           ((and (= w-left cur-left)
                 (= w-cols cur-cols)
                 (or (= (+ w-top w-rows) cur-top)              ; donor above
                     (= (+ cur-top cur-rows) w-top)))         ; donor below
            (setq hit
                  (list j :rows cur-rows
                        (cond
                         ((= (+ cur-top cur-rows) w-top) cur-top)
                         (t nil)))))
           ;; horizontal merge: same row band + adjacent cols.
           ((and (= w-top cur-top)
                 (= w-rows cur-rows)
                 (or (= (+ w-left w-cols) cur-left)            ; donor left
                     (= (+ cur-left cur-cols) w-left)))       ; donor right
            (setq hit
                  (list j :cols cur-cols
                        (cond
                         ((= (+ cur-left cur-cols) w-left) cur-left)
                         (t nil))))))))
      (setq j (1+ j)))
    hit))

(defun nemacs-gtk-delete-window ()
  "Bound to `C-x 0' — close the current window and give its rectangle
to a perfect sibling (= adjacent same-column or same-row band).
Drops back to single-window mode when only one remains.  Reports
\"cannot merge\" when no sibling can fully absorb the rectangle."
  (interactive)
  (cond
   ((or (null nemacs-gtk--windows)
        (<= (length nemacs-gtk--windows) 1))
    (setq nemacs-gtk--last-key-text "delete-window: only one window"))
   (t
    (nemacs-gtk--sync-current-to-window)
    (let* ((idx nemacs-gtk--current-window-idx)
           (sibling (nemacs-gtk--find-merge-sibling idx)))
      (cond
       ((null sibling)
        (setq nemacs-gtk--last-key-text
              "delete-window: no mergeable sibling"))
       (t
        (let* ((donor-idx (nth 0 sibling))
               (prop (nth 1 sibling))
               (delta (nth 2 sibling))
               (new-anchor (nth 3 sibling))
               (donor (nth donor-idx nemacs-gtk--windows))
               (donor-rows (plist-get donor :rows))
               (donor-cols (plist-get donor :cols))
               (donor-new
                (cond
                 ((eq prop :rows)
                  (let ((d (plist-put donor :rows (+ donor-rows delta))))
                    (cond
                     (new-anchor (plist-put d :top-row new-anchor))
                     (t d))))
                 (t
                  (let ((d (plist-put donor :cols (+ donor-cols delta))))
                    (cond
                     (new-anchor (plist-put d :left-col new-anchor))
                     (t d)))))))
          (setcar (nthcdr donor-idx nemacs-gtk--windows) donor-new)
          ;; remove the deleted slot.
          (setq nemacs-gtk--windows
                (append (cl-subseq nemacs-gtk--windows 0 idx)
                        (cl-subseq nemacs-gtk--windows
                                   (1+ idx)
                                   (length nemacs-gtk--windows))))
          ;; donor index shifts if cur was before it.
          (when (< idx donor-idx)
            (setq donor-idx (1- donor-idx)))
          ;; collapse to single-window if exactly one remains.
          (cond
           ((= (length nemacs-gtk--windows) 1)
            (let ((only (car nemacs-gtk--windows)))
              (setq nemacs-gtk--active-buffer-name
                    (plist-get only :buffer))
              (setq nemacs-gtk--scroll-offset
                    (or (plist-get only :scroll) 0))
              (setq nemacs-gtk--windows nil)
              (setq nemacs-gtk--current-window-idx 0)
              (nemacs-gtk--sync-window-title))
            (setq nemacs-gtk--last-key-text "delete-window: → single"))
           (t
            (setq nemacs-gtk--current-window-idx
                  (min donor-idx (1- (length nemacs-gtk--windows))))
            (nemacs-gtk--load-window-to-globals)
            (setq nemacs-gtk--last-key-text
                  (format "delete-window: %d remaining"
                          (length nemacs-gtk--windows))))))))))))

(defun nemacs-gtk--resize-window (delta)
  "Phase 2.AV: shrink/grow the current window by DELTA rows.
Positive = grow current, negative = shrink.  The donor / recipient is
the next window in `--windows' (= the one immediately below);
falls back to previous when current is the last.  No-op + echo when
either side would drop below 4 rows."
  (cond
   ((or (null nemacs-gtk--windows)
        (<= (length nemacs-gtk--windows) 1))
    (setq nemacs-gtk--last-key-text "resize-window: only one window"))
   (t
    (nemacs-gtk--sync-current-to-window)
    (let* ((idx nemacs-gtk--current-window-idx)
           (n (length nemacs-gtk--windows))
           (donor-idx (cond
                       ((< idx (1- n)) (1+ idx))
                       (t (1- idx))))
           (cur (nth idx nemacs-gtk--windows))
           (donor (nth donor-idx nemacs-gtk--windows))
           (cur-rows (plist-get cur :rows))
           (donor-rows (plist-get donor :rows))
           (new-cur-rows (+ cur-rows delta))
           (new-donor-rows (- donor-rows delta)))
      (cond
       ((or (< new-cur-rows 4) (< new-donor-rows 4))
        (setq nemacs-gtk--last-key-text
              (format "resize-window: refusing (delta=%d)" delta)))
       (t
        (setcar (nthcdr idx nemacs-gtk--windows)
                (plist-put cur :rows new-cur-rows))
        (setcar (nthcdr donor-idx nemacs-gtk--windows)
                (plist-put donor :rows new-donor-rows))
        ;; Re-anchor :top-row top-down.
        (let ((cursor 0)
              (newlist '()))
          (dolist (w nemacs-gtk--windows)
            (let ((w2 (plist-put w :top-row cursor)))
              (push w2 newlist)
              (setq cursor (+ cursor (plist-get w2 :rows)))))
          (setq nemacs-gtk--windows (nreverse newlist)))
        (setq nemacs-gtk--last-key-text
              (format "resize-window: cur=%d donor=%d"
                      new-cur-rows new-donor-rows))))))))

(defun nemacs-gtk-enlarge-window ()
  "Bound to `C-x ^' — grow the current window by one row, taking it
from the next window."
  (interactive)
  (nemacs-gtk--resize-window 1))

(defun nemacs-gtk-shrink-window ()
  "M-x command — shrink the current window by one row, giving it
to the next window."
  (interactive)
  (nemacs-gtk--resize-window -1))

(defun nemacs-gtk-delete-other-windows ()
  "Bound to `C-x 1' — keep the current window and discard all others.
Drops back to single-window mode."
  (interactive)
  (cond
   ((or (null nemacs-gtk--windows)
        (<= (length nemacs-gtk--windows) 1))
    (setq nemacs-gtk--last-key-text
          "delete-other-windows: only one window"))
   (t
    (nemacs-gtk--sync-current-to-window)
    (let* ((cur (nth nemacs-gtk--current-window-idx
                     nemacs-gtk--windows)))
      (setq nemacs-gtk--active-buffer-name (plist-get cur :buffer))
      (setq nemacs-gtk--scroll-offset (or (plist-get cur :scroll) 0))
      (setq nemacs-gtk--windows nil)
      (setq nemacs-gtk--current-window-idx 0)
      (nemacs-gtk--sync-window-title)
      (setq nemacs-gtk--last-key-text "delete-other-windows: → single")))))

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

(defun nemacs-gtk-goto-line ()
  "Bound to `M-g g' / `M-g M-g' / `M-x goto-line' (Phase 2.X).
Prompt for a 1-based line number in the minibuffer + move point
to that line's beginning.  Out-of-range numbers clamp to first /
last line.  Empty input is a no-op."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Goto line: "
   (lambda (input)
     (cond
      ((or (null input) (string-empty-p input))
       (setq nemacs-gtk--last-key-text "goto-line: empty"))
      (t
       (let ((n (condition-case _err
                    (string-to-number input)
                  (error 0))))
         (cond
          ((<= n 0)
           (setq nemacs-gtk--last-key-text
                 (format "goto-line: bad number %s" input)))
          (t
           (with-current-buffer (nemacs-gtk--active-buffer)
             (let* ((total  (nemacs-gtk--buffer-line-count))
                    (target (min n total)))
               (nelisp-ec-goto-char (nelisp-ec-point-min))
               (forward-line (- target 1))
               (setq nemacs-gtk--last-key-text
                     (format "Line %d/%d" target total))))
           (nemacs-gtk--ensure-cursor-visible))))))))) ; cursor-on-screen

(defun what-line ()
  "Echo the current line number / total line count of the active
buffer (Phase 2.X).  Bound nowhere directly — accessible via
`M-x what-line'.  Mirrors real Emacs's `what-line' contract
(= numbers only, no \"Line N\" prefix on the user side; we add one
in the echo for readability)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((line  (line-number-at-pos))
           (total (nemacs-gtk--buffer-line-count)))
      (setq nemacs-gtk--last-key-text
            (format "Line %d/%d" line total)))))

(defun nemacs-gtk--case-change-word (case-fn)
  "Helper for upcase-word / downcase-word / capitalize-word.  Apply
the string transformer CASE-FN to the next word from point + replace
it in-place.  No-op when point is at EOB or there's only whitespace
ahead."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((start (nelisp-ec-point)))
      (forward-word 1)
      (let ((end (nelisp-ec-point)))
        (when (> end start)
          (let ((text (buffer-substring start end)))
            (nelisp-ec-delete-region start end)
            (nelisp-ec-insert (funcall case-fn text))))))))

(defun nemacs-gtk-upcase-word ()
  "Bound to `M-u' / `Esc u' (Phase 2.Y).  UPPERCASE the next word
from point.  Point ends at the word's end."
  (interactive)
  (nemacs-gtk--case-change-word #'upcase))

(defun nemacs-gtk-downcase-word ()
  "Bound to `M-l' / `Esc l' (Phase 2.Y).  Lowercase the next word
from point."
  (interactive)
  (nemacs-gtk--case-change-word #'downcase))

(defun nemacs-gtk-capitalize-word ()
  "Bound to `M-c' / `Esc c' (Phase 2.Y).  Capitalize the next word
(= first letter upper, rest lower)."
  (interactive)
  (nemacs-gtk--case-change-word
   (lambda (s)
     (if (string-empty-p s) s
       (concat (upcase   (substring s 0 1))
               (downcase (substring s 1)))))))

(defun nemacs-gtk-transpose-chars ()
  "Bound to `C-t' (Phase 2.Y).  Swap the chars before and after
point + advance point by one.  At BOB does nothing.  At EOB swaps
the two preceding chars (= mirrors real Emacs's `transpose-chars')."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((p    (nelisp-ec-point))
          (pmin (nelisp-ec-point-min))
          (pmax (nelisp-ec-point-max)))
      (when (>= p (+ pmin 2))
        ;; At EOB: drop point one back so we swap the last two chars.
        (when (= p pmax) (setq p (1- p)))
        (let ((c1 (buffer-substring (- p 1) p))
              (c2 (buffer-substring p (+ p 1))))
          (nelisp-ec-delete-region (- p 1) (+ p 1))
          (nelisp-ec-insert (concat c2 c1))
          (nelisp-ec-goto-char (+ p 1)))))))

(defun nemacs-gtk--horizontal-whitespace-bounds-around (p)
  "Return (BEG . END) of the run of horizontal whitespace
(= space + tab) that touches point P, or nil when P is not adjacent
to any whitespace.  BEG / END are 1-based buffer positions."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((s    (buffer-string))
           (pmin (nelisp-ec-point-min))
           (idx  (- p pmin))
           (len  (length s))
           (ws-p (lambda (c) (or (eq c ?\s) (eq c ?\t)))))
      (let ((b idx)
            (e idx))
        (while (and (> b 0) (funcall ws-p (aref s (1- b))))
          (setq b (1- b)))
        (while (and (< e len) (funcall ws-p (aref s e)))
          (setq e (1+ e)))
        (cond
         ((= b e) nil)
         (t (cons (+ pmin b) (+ pmin e))))))))

(defun nemacs-gtk-just-one-space ()
  "Bound to `M-SPC' (= byte 32 under Esc-prefix) (Phase 2.Z).  Collapse
the run of horizontal whitespace touching point to a single space.
No-op when point is not adjacent to any whitespace."
  (interactive)
  (let ((bounds (nemacs-gtk--horizontal-whitespace-bounds-around
                 (with-current-buffer (nemacs-gtk--active-buffer)
                   (nelisp-ec-point)))))
    (when bounds
      (with-current-buffer (nemacs-gtk--active-buffer)
        (nelisp-ec-delete-region (car bounds) (cdr bounds))
        (nelisp-ec-insert " ")))))

(defun nemacs-gtk-delete-horizontal-space ()
  "Bound to `M-\\' (= Esc \\) (Phase 2.Z).  Delete all horizontal
whitespace touching point.  No-op when point is not adjacent to
any whitespace."
  (interactive)
  (let ((bounds (nemacs-gtk--horizontal-whitespace-bounds-around
                 (with-current-buffer (nemacs-gtk--active-buffer)
                   (nelisp-ec-point)))))
    (when bounds
      (with-current-buffer (nemacs-gtk--active-buffer)
        (nelisp-ec-delete-region (car bounds) (cdr bounds))))))

(defun nemacs-gtk-kill-whole-line ()
  "Bound to `M-x' / context (Phase 2.Z).  Kill the entire current
line including its trailing newline + push to kill-ring (= clipboard
via the cut hook).  Same shape as real Emacs's `kill-whole-line':
cursor stays at the line's column on the next line."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((b    (line-beginning-position))
           (e    (line-end-position))
           (pmax (nelisp-ec-point-max)))
      (cond
       ((= b e pmax)
        (setq nemacs-gtk--last-key-text "kill-whole-line: empty buffer"))
       ((>= e pmax)
        (kill-region b e)
        (setq nemacs-gtk--last-key-text "Killed last line"))
       (t
        (kill-region b (1+ e))
        (setq nemacs-gtk--last-key-text "Killed whole line"))))))

(defun nemacs-gtk--blank-line-p ()
  "Return non-nil when point is on an empty (= zero-width) line."
  (= (line-beginning-position) (line-end-position)))

(defun nemacs-gtk-forward-paragraph ()
  "Bound to `M-}' / `Esc }' (Phase 2.Y).  Move point past the
current paragraph (= skip blank lines we may be in, then advance
through non-blank lines until the next blank line or EOB)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((max (nelisp-ec-point-max)))
      (while (and (< (nelisp-ec-point) max)
                  (nemacs-gtk--blank-line-p))
        (forward-line 1))
      (while (and (< (nelisp-ec-point) max)
                  (not (nemacs-gtk--blank-line-p)))
        (forward-line 1)))))

(defvar nemacs-gtk--kbd-macro-recording nil
  "Phase 2.AP: t while between `C-x (' and `C-x )'.  When set, the
dispatch loop appends each event-vec it processes to
`--kbd-macro-current'.")

(defvar nemacs-gtk--kbd-macro-current nil
  "Phase 2.AP: list of event-vecs being accumulated for the active
recording.  Reversed at `end-kbd-macro' time and stored on
`--kbd-macro-last' for replay.")

(defvar nemacs-gtk--kbd-macro-last nil
  "Phase 2.AP: vector of events from the most recently completed
recording.  Replayed by `C-x e'.")

(defun nemacs-gtk-start-kbd-macro ()
  "Bound to `C-x (' — begin recording a keyboard macro.  The
dispatcher captures every subsequent event into
`--kbd-macro-current' until `C-x )'."
  (interactive)
  (cond
   (nemacs-gtk--kbd-macro-recording
    (setq nemacs-gtk--last-key-text "kbd-macro: already recording"))
   (t
    (setq nemacs-gtk--kbd-macro-recording t)
    (setq nemacs-gtk--kbd-macro-current nil)
    (setq nemacs-gtk--last-key-text "Defining kbd macro..."))))

(defun nemacs-gtk-end-kbd-macro ()
  "Bound to `C-x )' — finish recording the keyboard macro and
publish it on `--kbd-macro-last'.  No-op + echo when not currently
recording."
  (interactive)
  (cond
   ((not nemacs-gtk--kbd-macro-recording)
    (setq nemacs-gtk--last-key-text "kbd-macro: not recording"))
   (t
    (setq nemacs-gtk--kbd-macro-recording nil)
    (setq nemacs-gtk--kbd-macro-last
          (apply #'vconcat (nreverse nemacs-gtk--kbd-macro-current)))
    (let ((n (length nemacs-gtk--kbd-macro-last)))
      (setq nemacs-gtk--kbd-macro-current nil)
      (setq nemacs-gtk--last-key-text
            (format "Macro defined (%d events)" n))))))

(defun nemacs-gtk-call-last-kbd-macro ()
  "Bound to `C-x e' — replay the most recently recorded keyboard
macro by feeding its events back through the command loop."
  (interactive)
  (cond
   ((or (null nemacs-gtk--kbd-macro-last)
        (= 0 (length nemacs-gtk--kbd-macro-last)))
    (setq nemacs-gtk--last-key-text "kbd-macro: no macro to replay"))
   (t
    (let ((vec nemacs-gtk--kbd-macro-last))
      (with-current-buffer (nemacs-gtk--active-buffer)
        (apply #'emacs-command-loop-feed-events (append vec nil))
        ;; Run as many command-loop steps as we have events; each
        ;; emacs-command-loop-step consumes one keymap-resolved unit.
        (let ((i 0) (n (length vec)))
          (while (< i n)
            (emacs-command-loop-step)
            (setq i (1+ i))))))
    (setq nemacs-gtk--last-key-text
          (format "Replayed macro (%d events)"
                  (length nemacs-gtk--kbd-macro-last))))))

(defun nemacs-gtk-eval-expression ()
  "Bound to `M-:' / `Esc :' — read an elisp expression via the
inline minibuffer, evaluate it, and surface the result on the
echo-area row.  Errors during read or eval report on echo area
without crashing."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Eval: "
   (lambda (input)
     (cond
      ((or (null input) (string-empty-p input))
       (setq nemacs-gtk--last-key-text "eval: empty"))
      (t
       (condition-case err
           (let* ((form (read input))
                  (result (eval form lexical-binding)))
             (setq nemacs-gtk--last-key-text
                   (format "%S" result)))
         (error
          (setq nemacs-gtk--last-key-text
                (format "eval: %s"
                        (cond
                         ((stringp (cadr err)) (cadr err))
                         (t (prin1-to-string err))))))))))))

(defun nemacs-gtk--shell-command-output-buffer-fill (text)
  "Stuff TEXT into a fresh `*Shell Command Output*' buffer + switch
to it.  Empty TEXT shows a placeholder so the user knows the
command ran (rather than wondering if the prompt fizzled)."
  (let ((buf (get-buffer-create "*Shell Command Output*")))
    (with-current-buffer buf
      (when (fboundp 'erase-buffer)
        (erase-buffer))
      (cond
       ((or (null text) (= 0 (length text)))
        (nelisp-ec-insert "[shell-command: no output]\n"))
       (t (nelisp-ec-insert text))))
    (setq nemacs-gtk--active-buffer-name "*Shell Command Output*")
    (setq nemacs-gtk--scroll-offset 0)
    (nemacs-gtk--sync-window-title)))

(defun nemacs-gtk--shell-command-runner (command &optional input-text)
  "Run COMMAND through the shell, optionally passing INPUT-TEXT on stdin.
Returns the stdout string (or empty on no output)."
  (cond
   ((null input-text)
    (or (and (fboundp 'shell-command-to-string)
             (shell-command-to-string command))
        ""))
   (t
    ;; pipe INPUT-TEXT through stdin via a temp file so we don't have
    ;; to thread a real pipe.  Tiny enough for MVP.
    (let* ((tmp (and (fboundp 'make-temp-file)
                     (make-temp-file "nemacs-gtk-stdin-")))
           (rendered nil))
      (cond
       ((null tmp)
        (or (shell-command-to-string command) ""))
       (t
        (unwind-protect
            (progn
              (with-temp-buffer
                (insert input-text)
                (write-region (point-min) (point-max) tmp))
              (setq rendered
                    (or (shell-command-to-string
                         (format "%s < %s"
                                 command
                                 (shell-quote-argument tmp)))
                        "")))
          (when (and tmp (file-exists-p tmp))
            (delete-file tmp)))
        rendered))))))

(defun nemacs-gtk-shell-command ()
  "Bound to `M-!' / `Esc !' — prompt for a shell command, run it,
display the stdout in `*Shell Command Output*' and switch to it."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Shell command: "
   (lambda (cmd)
     (cond
      ((or (null cmd) (string-empty-p cmd))
       (setq nemacs-gtk--last-key-text "shell-command: empty"))
      (t
       (condition-case err
           (let ((out (nemacs-gtk--shell-command-runner cmd)))
             (nemacs-gtk--shell-command-output-buffer-fill out)
             (setq nemacs-gtk--last-key-text
                   (format "shell-command: %s (%d bytes)"
                           cmd (length (or out "")))))
         (error
          (setq nemacs-gtk--last-key-text
                (format "shell-command: %s" (cadr err))))))))))

(defun nemacs-gtk-shell-command-on-region ()
  "Bound to `M-|' / `Esc |' — pipe the active region through a shell
command and display stdout in `*Shell Command Output*'.  Reports
\"no region\" when the active buffer has no current selection."
  (interactive)
  (let ((bounds (nemacs-gtk--region-bounds)))
    (cond
     ((null bounds)
      (setq nemacs-gtk--last-key-text "shell-command-on-region: no region"))
     (t
      (nemacs-gtk--enter-minibuffer
       "Shell command on region: "
       (lambda (cmd)
         (cond
          ((or (null cmd) (string-empty-p cmd))
           (setq nemacs-gtk--last-key-text
                 "shell-command-on-region: empty"))
          (t
           (let* ((beg (car bounds))
                  (end (cdr bounds))
                  (text (with-current-buffer (nemacs-gtk--active-buffer)
                          (nelisp-ec-buffer-substring beg end))))
             (condition-case err
                 (let ((out (nemacs-gtk--shell-command-runner cmd text)))
                   (nemacs-gtk--shell-command-output-buffer-fill out)
                   (setq nemacs-gtk--last-key-text
                         (format "shell-command-on-region: %d→%d bytes"
                                 (length text) (length (or out "")))))
               (error
                (setq nemacs-gtk--last-key-text
                      (format "shell-command-on-region: %s"
                              (cadr err))))))))))))))

(defun nemacs-gtk-what-cursor-position ()
  "Bound to `C-x =' — display char at point + decimal/hex/octal value
+ buffer-position percentage on the echo-area row.  Mirrors Emacs'
`what-cursor-position' MVP."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (nelisp-ec-point))
           (max (nelisp-ec-point-max))
           (min (nelisp-ec-point-min))
           (size (- max min))
           (ch (and (< p max) (emacs-edit--char-at p)))
           (pct (cond
                 ((or (= size 0) (= p min)) "Top")
                 ((= p max) "Bot")
                 (t (format "%d%%"
                            (/ (* 100 (- p min)) (max 1 size)))))))
      (cond
       ((null ch)
        (setq nemacs-gtk--last-key-text
              (format "point=%d of %d (%s) — at EOB" p max pct)))
       (t
        (setq nemacs-gtk--last-key-text
              (format "Char: %c (%d, #o%o, #x%x)  point=%d of %d (%s)"
                      ch ch ch ch p max pct)))))))

(defun nemacs-gtk-toggle-read-only ()
  "Bound to `C-x C-q' — toggle the active buffer's `buffer-read-only'
flag.  The mode-line `--' / `**' marker is replaced by `%%' when
read-only is on (= matches Emacs convention).  Edit-class commands
honor the flag via the dispatcher's read-only guard."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (setq buffer-read-only (not buffer-read-only))
    (setq nemacs-gtk--last-key-text
          (format "buffer-read-only: %s"
                  (if buffer-read-only "on" "off")))))

(defun nemacs-gtk-sort-lines ()
  "M-x command — sort the lines of the active region alphabetically.
Without an active region, reports an error on echo area."
  (interactive)
  (let ((bounds (nemacs-gtk--region-bounds)))
    (cond
     ((null bounds)
      (setq nemacs-gtk--last-key-text "sort-lines: no active region"))
     (t
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (text (with-current-buffer (nemacs-gtk--active-buffer)
                     (nelisp-ec-buffer-substring beg end)))
             (trailing-nl (and (> (length text) 0)
                               (eq (aref text (1- (length text))) ?\n)))
             (chunk (cond
                     (trailing-nl (substring text 0 (1- (length text))))
                     (t text)))
             (lines (split-string chunk "\n"))
             (sorted (sort lines #'string<))
             (rejoined (concat (mapconcat #'identity sorted "\n")
                               (if trailing-nl "\n" ""))))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (kill-region beg end)
          (nelisp-ec-goto-char beg)
          (nelisp-ec-insert rejoined))
        (setq nemacs-gtk--last-key-text
              (format "sort-lines: %d lines" (length sorted))))))))

(defun nemacs-gtk-write-file ()
  "Bound to `C-x C-w' — prompt for a new path and write the active
buffer there (= `write-file').  Updates the buffer's visited filename
on success."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Write file: "
   (lambda (path)
     (cond
      ((or (null path) (string-empty-p path))
       (setq nemacs-gtk--last-key-text "write-file: empty path"))
      (t
       (with-current-buffer (nemacs-gtk--active-buffer)
         (condition-case err
             (let ((abs (write-file path)))
               (nemacs-gtk--sync-window-title)
               (setq nemacs-gtk--last-key-text
                     (format "Wrote: %s" abs)))
           (error
            (setq nemacs-gtk--last-key-text
                  (format "write-file: %s" (cadr err)))))))))))

(defun nemacs-gtk-save-some-buffers ()
  "Bound to `C-x s' — save every modified file-visiting buffer
without prompting per-buffer.  Mirrors `(save-some-buffers t)' from
real Emacs (= the no-confirm variant most users alias to C-x s)."
  (interactive)
  (let ((dirty (nemacs-gtk--unsaved-file-buffers))
        (saved 0)
        (failed 0))
    (cond
     ((null dirty)
      (setq nemacs-gtk--last-key-text "save-some-buffers: nothing to save"))
     (t
      (dolist (b dirty)
        (condition-case _err
            (with-current-buffer b
              (save-buffer)
              (setq saved (1+ saved)))
          (error (setq failed (1+ failed)))))
      (setq nemacs-gtk--last-key-text
            (cond
             ((zerop failed) (format "Saved %d buffer(s)" saved))
             (t (format "Saved %d, %d failed" saved failed))))))))

(defvar nemacs-gtk--dabbrev-state nil
  "Phase 2.AO: tracks an in-progress `M-/' chain.
List `(PREFIX REPLACED-BEG REPLACED-END SCAN-FROM CYCLED)' where
PREFIX is the original word fragment, REPLACED-BEG..END is the
buffer span we're currently substituting at, SCAN-FROM is where the
*next* backward search starts, and CYCLED is the list of completions
already shown so we never repeat one in a single session.")

(defun nemacs-gtk--dabbrev-word-at-point-prefix ()
  "Return (BEG . PREFIX) where BEG is BOW of the word ending at point
and PREFIX is the substring (= chars up to point).  Returns nil
when point is not adjacent to a word."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((p (nelisp-ec-point))
          (min (nelisp-ec-point-min)))
      (let ((q p))
        (while (and (> q min)
                    (emacs-edit--word-char-p
                     (emacs-edit--char-at (- q 1))))
          (setq q (1- q)))
        (cond
         ((= q p) nil)
         (t (cons q (nelisp-ec-buffer-substring q p))))))))

(defun nemacs-gtk--dabbrev-find-completion (prefix scan-from cycled)
  "Walk the active buffer backward from SCAN-FROM looking for a word
that starts with PREFIX (case-sensitive) AND isn't yet in CYCLED.
Return (BEG END WORD NEW-SCAN-FROM) on hit, nil on miss."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((p scan-from)
          (min (nelisp-ec-point-min))
          (plen (length prefix))
          (hit nil))
      (while (and (> p min) (not hit))
        ;; step back one char
        (setq p (1- p))
        (when (emacs-edit--word-char-p (emacs-edit--char-at p))
          ;; walk back to BOW
          (let ((bow p))
            (while (and (> bow min)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at (- bow 1))))
              (setq bow (1- bow)))
            ;; walk forward to EOW
            (let ((eow p))
              (while (and (< eow (nelisp-ec-point-max))
                          (emacs-edit--word-char-p
                           (emacs-edit--char-at eow)))
                (setq eow (1+ eow)))
              (let ((word (nelisp-ec-buffer-substring bow eow)))
                (when (and (> (length word) plen)
                           (string= prefix (substring word 0 plen))
                           (not (member word cycled)))
                  (setq hit (list bow eow word bow)))))
            (setq p bow))))
      hit)))

(defun nemacs-gtk-dabbrev-expand ()
  "Bound to `M-/' / `Esc /' — expand the word fragment before point
to a longer word that occurs earlier in the buffer.  Repeated `M-/'
cycles through alternative completions."
  (interactive)
  (let* ((reuse (and nemacs-gtk--dabbrev-state
                     (let* ((st nemacs-gtk--dabbrev-state)
                            (rb (nth 1 st))
                            (re (nth 2 st)))
                       (and (= (with-current-buffer
                                   (nemacs-gtk--active-buffer)
                                 (nelisp-ec-point))
                               re)
                            ;; the spelling at REPLACED-BEG..END must still
                            ;; equal whatever we previously substituted
                            t))))
         (st (cond
              (reuse nemacs-gtk--dabbrev-state)
              (t nil))))
    (cond
     (st
      (let* ((prefix (nth 0 st))
             (rb (nth 1 st))
             (re (nth 2 st))
             (scan (nth 3 st))
             (cycled (nth 4 st))
             (hit (nemacs-gtk--dabbrev-find-completion
                   prefix scan cycled)))
        (cond
         ((null hit)
          (setq nemacs-gtk--dabbrev-state nil)
          (setq nemacs-gtk--last-key-text
                (format "dabbrev-expand: no more matches for %s" prefix)))
         (t
          (with-current-buffer (nemacs-gtk--active-buffer)
            (kill-region rb re)
            (nelisp-ec-goto-char rb)
            (nelisp-ec-insert (nth 2 hit)))
          (setq nemacs-gtk--dabbrev-state
                (list prefix rb (+ rb (length (nth 2 hit)))
                      (nth 3 hit) (cons (nth 2 hit) cycled)))
          (setq nemacs-gtk--last-key-text
                (format "dabbrev-expand: %s" (nth 2 hit)))))))
     (t
      ;; fresh M-/ — read prefix at point.
      (let ((bp (nemacs-gtk--dabbrev-word-at-point-prefix)))
        (cond
         ((null bp)
          (setq nemacs-gtk--last-key-text
                "dabbrev-expand: no word fragment before point"))
         (t
          (let* ((prefix (cdr bp))
                 (beg (car bp))
                 (end (with-current-buffer (nemacs-gtk--active-buffer)
                        (nelisp-ec-point)))
                 (hit (nemacs-gtk--dabbrev-find-completion
                       prefix beg (list prefix))))
            (cond
             ((null hit)
              (setq nemacs-gtk--last-key-text
                    (format "dabbrev-expand: no match for %s" prefix)))
             (t
              (with-current-buffer (nemacs-gtk--active-buffer)
                (kill-region beg end)
                (nelisp-ec-goto-char beg)
                (nelisp-ec-insert (nth 2 hit)))
              (setq nemacs-gtk--dabbrev-state
                    (list prefix beg (+ beg (length (nth 2 hit)))
                          (nth 3 hit) (list prefix (nth 2 hit))))
              (setq nemacs-gtk--last-key-text
                    (format "dabbrev-expand: %s" (nth 2 hit))))))))))))
  ;; Clear state when next M-/ won't be in the same place.
  ;; (= state lifetime is a bit pessimistic but accurate enough.)
  )

(defun nemacs-gtk-exchange-point-and-mark ()
  "Bound to `C-x C-x' — swap point and mark in the active buffer.
Reactivates the region as side-effect (= `--shift-region' cleared
since this is an explicit command, not a shift-select drift)."
  (interactive)
  (cond
   ((or (null nemacs-gtk--mark-pos)
        (not (string= nemacs-gtk--mark-buffer
                      nemacs-gtk--active-buffer-name)))
    (setq nemacs-gtk--last-key-text "exchange-point-and-mark: no mark"))
   (t
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((p (nelisp-ec-point))
             (m nemacs-gtk--mark-pos))
        (setq nemacs-gtk--mark-pos p)
        (nelisp-ec-goto-char m)))
    (setq nemacs-gtk--shift-region nil)
    (setq nemacs-gtk--last-key-text "Exchange point and mark"))))

(defconst nemacs-gtk--tab-stop-width 4
  "Phase 2.AN: column width of a tab-stop column for `M-i'
(= insert spaces up to the next multiple of this width).")

(defun nemacs-gtk--current-column-in-line ()
  "Return point's column index (= chars since BOL of the current line)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (nelisp-ec-point))
           (q p) (min (nelisp-ec-point-min)))
      (while (and (> q min)
                  (let ((s (nelisp-ec-buffer-substring (- q 1) q)))
                    (not (and (> (length s) 0) (eq (aref s 0) ?\n)))))
        (setq q (1- q)))
      (- p q))))

(defun nemacs-gtk-tab-to-tab-stop ()
  "Bound to `M-i' / `Esc i' — insert spaces from point up to the
next column that's a multiple of `--tab-stop-width' (= 4).
Inserts at least one space."
  (interactive)
  (let* ((col (nemacs-gtk--current-column-in-line))
         (stop nemacs-gtk--tab-stop-width)
         (delta (- stop (mod col stop)))
         (n (if (= delta 0) stop delta)))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let ((i 0))
        (while (< i n)
          (nelisp-ec-insert " ")
          (setq i (1+ i)))))
    (setq nemacs-gtk--last-key-text
          (format "tab-to-tab-stop: +%d cols → %d" n (+ col n)))))

(defvar nemacs-gtk--fill-column 70
  "Phase 2.AN: target column for `M-q' wrap.  Made user-mutable in
Phase 2.BB so `C-x f' / `set-fill-column' can rewire it.")

(defun nemacs-gtk-fill-paragraph ()
  "Bound to `M-q' / `Esc q' — re-wrap the current paragraph so no
line exceeds `--fill-column' (= 70).  MVP: detects paragraph by
walking to the surrounding blank lines, joins all internal lines
with single spaces, then re-breaks at word boundaries."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    ;; Step 1: locate the paragraph bounds.
    (let* ((min (nelisp-ec-point-min))
           (max (nelisp-ec-point-max))
           ;; back to paragraph start
           (start
            (save-excursion
              (when (nemacs-gtk--blank-line-p)
                (forward-line 1))
              (while (and (> (nelisp-ec-point) min)
                          (not (nemacs-gtk--blank-line-p)))
                (forward-line -1))
              (when (nemacs-gtk--blank-line-p)
                (forward-line 1))
              (nelisp-ec-point)))
           ;; forward to paragraph end
           (end
            (save-excursion
              (nelisp-ec-goto-char start)
              (while (and (< (nelisp-ec-point) max)
                          (not (nemacs-gtk--blank-line-p)))
                (forward-line 1))
              (nelisp-ec-point))))
      (cond
       ((>= start end)
        (setq nemacs-gtk--last-key-text "fill-paragraph: empty"))
       (t
        ;; Step 2: extract + canonicalise (collapse all whitespace runs to one space).
        (let* ((text (nelisp-ec-buffer-substring start end))
               (i 0) (n (length text)) (canon "") (last-ws nil))
          (while (< i n)
            (let ((ch (aref text i)))
              (cond
               ((or (eq ch ?\s) (eq ch ?\t) (eq ch ?\n))
                (unless last-ws
                  (setq canon (concat canon " "))
                  (setq last-ws t)))
               (t
                (setq canon (concat canon (string ch)))
                (setq last-ws nil))))
            (setq i (1+ i)))
          ;; trim trailing space
          (when (and (> (length canon) 0)
                     (eq (aref canon (1- (length canon))) ?\s))
            (setq canon (substring canon 0 (1- (length canon)))))
          ;; Step 3: greedy break at fill-column.
          (let ((parts '())
                (col 0)
                (j 0)
                (m (length canon)))
            (while (< j m)
              ;; find next word
              (while (and (< j m) (eq (aref canon j) ?\s))
                (setq j (1+ j)))
              (let ((wstart j))
                (while (and (< j m) (not (eq (aref canon j) ?\s)))
                  (setq j (1+ j)))
                (let* ((word (substring canon wstart j))
                       (wlen (length word))
                       (sep (if (= col 0) "" " ")))
                  (cond
                   ((= col 0)
                    (push word parts)
                    (setq col wlen))
                   ((<= (+ col 1 wlen) nemacs-gtk--fill-column)
                    (push sep parts)
                    (push word parts)
                    (setq col (+ col 1 wlen)))
                   (t
                    (push "\n" parts)
                    (push word parts)
                    (setq col wlen))))))
            (let ((rebuilt (apply #'concat (nreverse parts))))
              (kill-region start end)
              (nelisp-ec-goto-char start)
              (nelisp-ec-insert rebuilt)
              (setq nemacs-gtk--last-key-text
                    (format "fill-paragraph: %d→%d chars"
                            (length text) (length rebuilt)))))))))))

(defun nemacs-gtk-delete-indentation ()
  "Bound to `M-^' / `Esc ^' — join the current line with the previous
one (= `delete-indentation').  Removes the preceding newline plus
any leading whitespace on the current line, leaving exactly one
space between the joined text (or none when the first line ends
in whitespace already)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (nelisp-ec-point))
           (min (nelisp-ec-point-min)))
      ;; move to BOL of current line
      (let ((q p))
        (while (and (> q min)
                    (let ((s (nelisp-ec-buffer-substring (- q 1) q)))
                      (not (and (> (length s) 0) (eq (aref s 0) ?\n)))))
          (setq q (1- q)))
        (cond
         ((<= q min)
          (setq nemacs-gtk--last-key-text "delete-indentation: at BOB"))
         (t
          (let* ((bol q)
                 ;; the \n separator is at (bol-1)
                 (sep-pos (- bol 1))
                 ;; locate end of leading whitespace on this line
                 (skip bol)
                 (max (nelisp-ec-point-max)))
            (while (and (< skip max)
                        (let ((s (nelisp-ec-buffer-substring skip (1+ skip))))
                          (or (and (> (length s) 0) (eq (aref s 0) ?\s))
                              (and (> (length s) 0) (eq (aref s 0) ?\t)))))
              (setq skip (1+ skip)))
            (kill-region sep-pos skip)
            (nelisp-ec-goto-char sep-pos)
            ;; insert one space if previous char isn't whitespace AND we're
            ;; not at BOB now
            (when (> sep-pos min)
              (let ((prev (nelisp-ec-buffer-substring (- sep-pos 1) sep-pos)))
                (unless (or (and (> (length prev) 0)
                                 (eq (aref prev 0) ?\s))
                            (and (> (length prev) 0)
                                 (eq (aref prev 0) ?\t)))
                  (nelisp-ec-insert " "))))
            (setq nemacs-gtk--last-key-text "delete-indentation"))))))))

(defun nemacs-gtk-mark-paragraph ()
  "Bound to `M-h' / `Esc h' — set mark at the beginning of the
current paragraph and move point to its end (= activates the
region around the paragraph)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    ;; backward-paragraph from current point
    (let ((min (nelisp-ec-point-min)))
      (while (and (> (nelisp-ec-point) min)
                  (nemacs-gtk--blank-line-p))
        (forward-line -1))
      (while (and (> (nelisp-ec-point) min)
                  (not (nemacs-gtk--blank-line-p)))
        (forward-line -1))
      (when (nemacs-gtk--blank-line-p)
        (forward-line 1))))
  ;; mark @ current point
  (setq nemacs-gtk--mark-pos    (with-current-buffer
                                    (nemacs-gtk--active-buffer)
                                  (nelisp-ec-point)))
  (setq nemacs-gtk--mark-buffer nemacs-gtk--active-buffer-name)
  (setq nemacs-gtk--shift-region nil)
  ;; forward-paragraph from there
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((max (nelisp-ec-point-max)))
      (while (and (< (nelisp-ec-point) max)
                  (not (nemacs-gtk--blank-line-p)))
        (forward-line 1))))
  (setq nemacs-gtk--last-key-text "Mark paragraph"))

(defun nemacs-gtk--count-words-in-range (beg end)
  "Return word count in BEG..END of the current substrate buffer.
A word is a maximal run of `emacs-edit--word-char-p'-true chars."
  (let ((p beg) (in-word nil) (n 0))
    (while (< p end)
      (let ((ch (emacs-edit--char-at p)))
        (cond
         ((emacs-edit--word-char-p ch)
          (unless in-word (setq n (1+ n) in-word t)))
         (t (setq in-word nil))))
      (setq p (1+ p)))
    n))

(defun nemacs-gtk--count-lines-in-range (beg end)
  "Return line count in BEG..END (= newlines + 1 if non-empty range
not ending in newline)."
  (let ((p beg) (n 0))
    (while (< p end)
      (let ((ch (emacs-edit--char-at p)))
        (when (eq ch ?\n) (setq n (1+ n))))
      (setq p (1+ p)))
    (cond
     ((= beg end) 0)
     ((eq (emacs-edit--char-at (1- end)) ?\n) n)
     (t (1+ n)))))

(defun nemacs-gtk-count-words-region ()
  "Bound to `M-=' / `Esc =' — report lines / words / chars in the
active region (= mark .. point) or, when no region is active, in
the whole buffer.  Result lands on the echo-area row."
  (interactive)
  (let* ((bn nemacs-gtk--active-buffer-name)
         (bounds (nemacs-gtk--region-bounds))
         (beg-end
          (cond
           (bounds bounds)
           (t (with-current-buffer (nemacs-gtk--active-buffer)
                (cons (nelisp-ec-point-min) (nelisp-ec-point-max))))))
         (beg (car beg-end))
         (end (cdr beg-end)))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let ((words (nemacs-gtk--count-words-in-range beg end))
            (lines (nemacs-gtk--count-lines-in-range beg end))
            (chars (- end beg)))
        (setq nemacs-gtk--last-key-text
              (format "%s %s: %d lines, %d words, %d chars"
                      (if bounds "Region" "Buffer")
                      bn lines words chars))))))

(defun nemacs-gtk--scan-forward-to-char (ch limit)
  "Scan from point in the current substrate buffer forward for
CH, stopping at LIMIT.  Return position one past CH, or nil if
CH not found in [point, LIMIT)."
  (let ((p (nelisp-ec-point))
        (found nil))
    (while (and (< p limit) (not found))
      (when (eq (emacs-edit--char-at p) ch)
        (setq found (1+ p)))
      (setq p (1+ p)))
    found))

(defun nemacs-gtk-zap-to-char ()
  "Bound to `M-z' / `Esc z' — kill from point to (and including)
the next occurrence of a CHAR read from a mini-prompt.  No-op +
echo when CHAR isn't found before EOB."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Zap to char: "
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "zap-to-char: empty"))
      (t
       (let ((ch (aref input 0)))
         (with-current-buffer (nemacs-gtk--active-buffer)
           (let* ((start (nelisp-ec-point))
                  (max (nelisp-ec-point-max))
                  (found (nemacs-gtk--scan-forward-to-char ch max)))
             (cond
              ((null found)
               (setq nemacs-gtk--last-key-text
                     (format "zap-to-char: %c not found" ch)))
              (t
               (kill-region start found)
               (setq nemacs-gtk--last-key-text
                     (format "zap-to-char: %c" ch))))))))))))

(defvar nemacs-gtk--query-replace-state nil
  "Phase 2.AK: list `(FROM TO POS COUNT)' tracking an in-progress
query-replace.  Set when M-% reads both arguments; cleared when the
loop hits a `q' answer or runs out of matches.  POS = where to
resume the next forward-search; COUNT = number of replacements
done so far.")

(defvar nemacs-gtk--query-replace-pending-key nil
  "Phase 2.AK: t while waiting for the user's y/n/!/q answer.  The
dispatch loop checks this and routes the next event into
`--query-replace-handle-key' instead of the normal keymap.")

(defun nemacs-gtk--query-replace-find-next ()
  "Advance to the next occurrence of FROM starting at POS in the
active buffer.  Returns the cons (BEG . END) of the match, or nil
when no more matches exist before point-max.  Mutates POS to the
match end on hit."
  (let* ((st nemacs-gtk--query-replace-state)
         (from (nth 0 st))
         (pos  (nth 2 st)))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((max (nelisp-ec-point-max))
             (haystack (nelisp-ec-buffer-substring pos max))
             (idx (and (> (length from) 0)
                       (string-match (regexp-quote from) haystack))))
        (cond
         ((null idx) nil)
         (t
          (let* ((beg (+ pos idx))
                 (end (+ beg (length from))))
            (setcar (nthcdr 2 nemacs-gtk--query-replace-state) end)
            (cons beg end))))))))

(defun nemacs-gtk--query-replace-prompt ()
  "Set the echo-area prompt for the current pending match."
  (let* ((st nemacs-gtk--query-replace-state)
         (from (nth 0 st))
         (to   (nth 1 st)))
    (setq nemacs-gtk--last-key-text
          (format "Replace %s with %s? (y/n/!/q)" from to))))

(defun nemacs-gtk--query-replace-step ()
  "Advance to the next match in the current state and either prompt
or finalize the loop."
  (let ((m (nemacs-gtk--query-replace-find-next)))
    (cond
     ((null m)
      (let ((count (nth 3 nemacs-gtk--query-replace-state)))
        (setq nemacs-gtk--query-replace-state nil)
        (setq nemacs-gtk--query-replace-pending-key nil)
        (setq nemacs-gtk--last-key-text
              (format "Replaced %d occurrence%s"
                      count (if (= count 1) "" "s")))))
     (t
      (with-current-buffer (nemacs-gtk--active-buffer)
        (nelisp-ec-goto-char (car m)))
      (setq nemacs-gtk--query-replace-pending-key t)
      (nemacs-gtk--query-replace-prompt)))))

(defun nemacs-gtk--query-replace-do-replace (beg end)
  "Replace BEG..END with the TO from `--query-replace-state'."
  (let* ((to (nth 1 nemacs-gtk--query-replace-state)))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (kill-region beg end)
      (nelisp-ec-insert to)
      (setcar (nthcdr 2 nemacs-gtk--query-replace-state)
              (nelisp-ec-point))
      (setcar (nthcdr 3 nemacs-gtk--query-replace-state)
              (1+ (nth 3 nemacs-gtk--query-replace-state))))))

(defun nemacs-gtk--query-replace-handle-key (event)
  "Dispatch one y/n/!/q answer EVENT for the in-progress
query-replace.  Returns t when consumed."
  (let* ((st nemacs-gtk--query-replace-state)
         (from (nth 0 st))
         (pos-end (nth 2 st))
         (beg (- pos-end (length from))))
    (cond
     ((or (eq event ?y) (eq event ?\s))
      (nemacs-gtk--query-replace-do-replace beg pos-end)
      (nemacs-gtk--query-replace-step))
     ((or (eq event ?n) (eq event 127) (eq event 'backspace))
      ;; skip — leave POS at end of match (set by find-next).
      (nemacs-gtk--query-replace-step))
     ((eq event ?!)
      ;; replace all remaining without further prompts.
      (nemacs-gtk--query-replace-do-replace beg pos-end)
      (let ((more t))
        (while more
          (let ((m (nemacs-gtk--query-replace-find-next)))
            (cond
             ((null m) (setq more nil))
             (t (nemacs-gtk--query-replace-do-replace (car m) (cdr m)))))))
      (let ((count (nth 3 nemacs-gtk--query-replace-state)))
        (setq nemacs-gtk--query-replace-state nil)
        (setq nemacs-gtk--query-replace-pending-key nil)
        (setq nemacs-gtk--last-key-text
              (format "Replaced %d (! all)" count))))
     ((or (eq event ?q) (eq event 7) (eq event 'escape))
      (let ((count (nth 3 nemacs-gtk--query-replace-state)))
        (setq nemacs-gtk--query-replace-state nil)
        (setq nemacs-gtk--query-replace-pending-key nil)
        (setq nemacs-gtk--last-key-text
              (format "query-replace: quit (%d done)" count))))
     (t
      ;; unknown answer — re-prompt.
      (nemacs-gtk--query-replace-prompt))))
  t)

(defun nemacs-gtk-query-replace ()
  "Bound to `M-%' / `Esc %' — interactive search-and-replace.
Reads FROM, then TO, via two minibuffer prompts.  After both are
captured, the dispatcher routes y/n/!/q answers through
`--query-replace-handle-key'."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Query replace: "
   (lambda (from)
     (cond
      ((or (null from) (string-empty-p from))
       (setq nemacs-gtk--last-key-text "query-replace: empty FROM"))
      (t
       (nemacs-gtk--enter-minibuffer
        (format "Query replace %s with: " from)
        (lambda (to)
          (let ((start (with-current-buffer (nemacs-gtk--active-buffer)
                         (nelisp-ec-point))))
            (setq nemacs-gtk--query-replace-state
                  (list from (or to "") start 0))
            (nemacs-gtk--query-replace-step)))))))))

(defun nemacs-gtk--line-bounds-around-point ()
  "Return (BOL . EOL) for the line point is on in the active buffer."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((p (nelisp-ec-point)))
      (cons
       ;; BOL
       (let ((q p) (min (nelisp-ec-point-min)))
         (while (and (> q min)
                     (let ((s (nelisp-ec-buffer-substring (- q 1) q)))
                       (not (and (> (length s) 0) (eq (aref s 0) ?\n)))))
           (setq q (1- q)))
         q)
       ;; EOL
       (let ((q p) (max (nelisp-ec-point-max)))
         (while (and (< q max)
                     (let ((s (nelisp-ec-buffer-substring q (1+ q))))
                       (not (and (> (length s) 0) (eq (aref s 0) ?\n)))))
           (setq q (1+ q)))
         q)))))

(defun nemacs-gtk--line-already-commented-p (bol eol)
  "Return non-nil when line BOL..EOL starts with `;; ' (after any
leading whitespace)."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((s (nelisp-ec-buffer-substring bol eol))
          (i 0))
      (while (and (< i (length s))
                  (or (eq (aref s i) ?\s) (eq (aref s i) ?\t)))
        (setq i (1+ i)))
      (and (<= (+ i 2) (length s))
           (eq (aref s i) ?\;)
           (eq (aref s (1+ i)) ?\;)))))

(defun nemacs-gtk--toggle-line-comment (bol eol)
  "Flip the line at BOL..EOL between commented (`;; ' prefix) and
uncommented (= remove leading `;; ' if present, after whitespace)."
  (cond
   ((nemacs-gtk--line-already-commented-p bol eol)
    ;; uncomment: find the ;; ; remove it (and optional trailing space).
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((s (nelisp-ec-buffer-substring bol eol))
             (i 0))
        (while (and (< i (length s))
                    (or (eq (aref s i) ?\s) (eq (aref s i) ?\t)))
          (setq i (1+ i)))
        ;; i now at ;
        (let ((cut-end (cond
                        ((and (< (+ i 2) (length s))
                              (eq (aref s (+ i 2)) ?\s))
                         (+ i 3))
                        (t (+ i 2)))))
          (kill-region (+ bol i) (+ bol cut-end))))))
   (t
    ;; comment: insert `;; ' at BOL.
    (with-current-buffer (nemacs-gtk--active-buffer)
      (nelisp-ec-goto-char bol)
      (nelisp-ec-insert ";; ")))))

(defun nemacs-gtk-comment-dwim ()
  "Bound to `M-;' / `Esc ;' — toggle line comment using `;;' prefix.
With an active region, toggle every line in the region.  Without a
region, toggle the line containing point."
  (interactive)
  (let ((bounds (nemacs-gtk--region-bounds)))
    (cond
     (bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (orig-end-len (- end beg)))
        (ignore orig-end-len)
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char beg)
          ;; collect line BOLs in range up front so insert/delete don't
          ;; shift the iterator.
          (let ((bols '())
                (p beg)
                (max end))
            (while (< p max)
              (let ((b (car (let ((nelisp-ec--current-buffer
                                   nelisp-ec--current-buffer))
                              (nelisp-ec-goto-char p)
                              (nemacs-gtk--line-bounds-around-point)))))
                (push b bols)
                ;; advance to next line
                (let ((eol (cdr (progn
                                  (nelisp-ec-goto-char b)
                                  (nemacs-gtk--line-bounds-around-point)))))
                  (setq p (min max (1+ eol))))))
            (setq bols (nreverse (delete-dups bols)))
            ;; toggle each line — since the lines are listed in order
            ;; and we mutate in order, BOLs after the first shift; do
            ;; backwards instead.
            (dolist (b (nreverse bols))
              (nelisp-ec-goto-char b)
              (let ((line-bounds (nemacs-gtk--line-bounds-around-point)))
                (nemacs-gtk--toggle-line-comment
                 (car line-bounds) (cdr line-bounds))))))
        (setq nemacs-gtk--last-key-text "comment-dwim region")))
     (t
      (let ((b (nemacs-gtk--line-bounds-around-point)))
        (nemacs-gtk--toggle-line-comment (car b) (cdr b)))
      (setq nemacs-gtk--last-key-text "comment-dwim line")))))

(defvar nemacs-gtk--describe-key-pending nil
  "Phase 2.AJ: t while waiting for the next key event after C-h k.
The dispatch loop checks this and reports the binding instead of
running it.")

(defun nemacs-gtk-describe-key ()
  "Bound to `C-h k' — read the next key event and report its
binding (= command symbol or `unbound') on the echo-area row."
  (interactive)
  (setq nemacs-gtk--describe-key-pending t)
  (setq nemacs-gtk--last-key-text "Describe key (press a key)..."))

(defun nemacs-gtk-describe-bindings ()
  "Bound to `C-h b' — render the global keymap into a `*Bindings*'
buffer and switch to it.  MVP: lists only the curated `--m-x-commands'
plus their key chord (the GTK keymap walker doesn't expose a
flat enumeration — substrate-level work)."
  (interactive)
  (let ((buf (get-buffer-create "*Bindings*")))
    (with-current-buffer buf
      (when (fboundp 'erase-buffer)
        (erase-buffer))
      (nelisp-ec-insert "Curated command list (M-x candidates):\n\n")
      (dolist (name nemacs-gtk--m-x-commands)
        (nelisp-ec-insert (format "  M-x %s\n" name))))
    (setq nemacs-gtk--active-buffer-name "*Bindings*")
    (setq nemacs-gtk--scroll-offset 0)
    (nemacs-gtk--sync-window-title)
    (setq nemacs-gtk--last-key-text "describe-bindings")))

(defvar nemacs-gtk--quoted-insert-pending nil
  "Phase 2.AF: t while waiting for the next key event after C-q.
The dispatch loop checks this before keymap lookup and inserts the
event verbatim instead of running its bound command.")

(defun nemacs-gtk-recenter ()
  "Bound to `C-l' — re-position `--scroll-offset' so point's row
sits in the middle of the viewport.  MVP: single-shot center;
real Emacs cycles through middle / top / bottom on repeat presses."
  (interactive)
  (let* ((row (nemacs-gtk--point-to-buf-row))
         (height nemacs-gtk--buffer-area-end)
         (target (- row (/ height 2))))
    (setq nemacs-gtk--scroll-offset (max 0 target))
    (nemacs-gtk--clamp-scroll-offset)
    (setq nemacs-gtk--last-key-text "recenter")))

(defun nemacs-gtk-overwrite-mode ()
  "Bound to `Insert' — toggle `overwrite-mode' (= substrate defvar
honoured by `self-insert-command')."
  (interactive)
  (cond
   ((not (boundp 'overwrite-mode))
    (setq nemacs-gtk--last-key-text "overwrite-mode: substrate missing"))
   (t
    (setq overwrite-mode (not overwrite-mode))
    (setq nemacs-gtk--last-key-text
          (format "overwrite-mode: %s" (if overwrite-mode "on" "off"))))))

(defun nemacs-gtk-undo ()
  "Bound to `C-/' / `C-_' / `C-x u' — undo one group from the
active buffer's `buffer-undo-list'.  Wraps the substrate's
`undo' polyfill with a `condition-case' so the
`no-further-undo-information' / `buffer-undo-list-disabled'
signals report on the echo-area row instead of bubbling."
  (interactive)
  (cond
   ((not (fboundp 'undo))
    (setq nemacs-gtk--last-key-text "undo: substrate not loaded"))
   (t
    (with-current-buffer (nemacs-gtk--active-buffer)
      (condition-case err
          (progn
            (undo)
            (setq nemacs-gtk--last-key-text "undo"))
        (emacs-undo-error
         (setq nemacs-gtk--last-key-text
               (format "undo: %s" (cadr err))))
        (error
         (setq nemacs-gtk--last-key-text
               (format "undo: %s" (cadr err)))))))))

(defun nemacs-gtk-quoted-insert ()
  "Bound to `C-q' — read the next key event raw and insert it as a
literal char.  Sets `--quoted-insert-pending' so the next key event
the dispatch loop sees gets stuffed into the buffer rather than
keymap-resolved."
  (interactive)
  (setq nemacs-gtk--quoted-insert-pending t)
  (setq nemacs-gtk--last-key-text "C-q (quoted-insert) — next key inserted literal"))

(defun nemacs-gtk-backward-paragraph ()
  "Bound to `M-{' / `Esc {' (Phase 2.Y).  Move point back to the
beginning of the current paragraph (= step up through non-blank
lines, then through blank lines until BOB or content)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((min (nelisp-ec-point-min)))
      (forward-line -1)
      (while (and (> (nelisp-ec-point) min)
                  (nemacs-gtk--blank-line-p))
        (forward-line -1))
      (while (and (> (nelisp-ec-point) min)
                  (not (nemacs-gtk--blank-line-p)))
        (forward-line -1)))))

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

(defun nemacs-gtk-yank-pop ()
  "Bound to `M-y' — replace the most recently yanked text with the
next entry from `kill-ring' (= `yank-pop').  Only meaningful right
after `C-y' or another `M-y'; otherwise reports the failure on the
echo-area row instead of letting the substrate signal."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (condition-case err
        (progn
          (yank-pop 1)
          (setq nemacs-gtk--last-key-text "yank-pop"))
      (error
       (setq nemacs-gtk--last-key-text
             (format "yank-pop: %s" (cadr err)))))))

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

;;;; --- registers (Phase 2.AX — C-x r s/i/SPC/j) ---------------------------

(defun nemacs-gtk-copy-to-register ()
  "Bound to `C-x r s' — set `--register-pending-op' to `copy' so the
next key consumed becomes the register name; the active region is
saved into the register as a string.  Reports `no mark' on the
echo-area row when the active buffer has no mark set."
  (interactive)
  (setq nemacs-gtk--register-pending-op 'copy)
  (setq nemacs-gtk--last-key-text "copy-to-register: register name?"))

(defun nemacs-gtk-insert-register ()
  "Bound to `C-x r i' — set `--register-pending-op' to `insert' so the
next key consumed becomes the register name; the stored string is
inserted at point.  No-op + echo when the named register is empty
or holds a position record."
  (interactive)
  (setq nemacs-gtk--register-pending-op 'insert)
  (setq nemacs-gtk--last-key-text "insert-register: register name?"))

(defun nemacs-gtk-point-to-register ()
  "Bound to `C-x r SPC' — set `--register-pending-op' to `point' so the
next key consumed becomes the register name; the current buffer
name + point position are saved as a `(:point BUF POS)' record."
  (interactive)
  (setq nemacs-gtk--register-pending-op 'point)
  (setq nemacs-gtk--last-key-text "point-to-register: register name?"))

(defun nemacs-gtk-jump-to-register ()
  "Bound to `C-x r j' — set `--register-pending-op' to `jump' so the
next key consumed becomes the register name; if the register holds
a `(:point BUF POS)' record, switch to BUF and `goto-char' POS.
No-op + echo for empty / string-only registers + dead buffers."
  (interactive)
  (setq nemacs-gtk--register-pending-op 'jump)
  (setq nemacs-gtk--last-key-text "jump-to-register: register name?"))

(defun nemacs-gtk--register-perform (op ch)
  "Phase 2.AX dispatcher tail — apply OP (= `copy' / `insert' / `point'
/ `jump') with the user-typed register name CH.  All paths set
`--last-key-text' so the user gets a status string back; mutating
ops update `--registers' before returning."
  (cond
   ((eq op 'copy)
    (with-current-buffer (nemacs-gtk--active-buffer)
      (cond
       ((null nemacs-gtk--mark-pos)
        (setq nemacs-gtk--last-key-text "copy-to-register: no mark set"))
       (t
        (let* ((s (min (nelisp-ec-point) nemacs-gtk--mark-pos))
               (e (max (nelisp-ec-point) nemacs-gtk--mark-pos))
               (text (nelisp-ec-buffer-substring s e)))
          (setq nemacs-gtk--registers
                (cons (cons ch text)
                      (assq-delete-all ch nemacs-gtk--registers)))
          (setq nemacs-gtk--last-key-text
                (format "copy-to-register: %d chars -> %c"
                        (length text) ch)))))))
   ((eq op 'insert)
    (let ((cell (assq ch nemacs-gtk--registers)))
      (cond
       ((null cell)
        (setq nemacs-gtk--last-key-text
              (format "insert-register: %c is empty" ch)))
       ((stringp (cdr cell))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-insert (cdr cell)))
        (nemacs-gtk--ensure-cursor-visible)
        (setq nemacs-gtk--last-key-text
              (format "insert-register: %d chars from %c"
                      (length (cdr cell)) ch)))
       (t
        (setq nemacs-gtk--last-key-text
              (format "insert-register: %c is a position (use C-x r j)"
                      ch))))))
   ((eq op 'point)
    (let ((bn nemacs-gtk--active-buffer-name)
          (pos (with-current-buffer (nemacs-gtk--active-buffer)
                 (nelisp-ec-point))))
      (setq nemacs-gtk--registers
            (cons (cons ch (list :point bn pos))
                  (assq-delete-all ch nemacs-gtk--registers)))
      (setq nemacs-gtk--last-key-text
            (format "point-to-register: %s:%d -> %c" bn pos ch))))
   ((eq op 'jump)
    (let ((cell (assq ch nemacs-gtk--registers)))
      (cond
       ((null cell)
        (setq nemacs-gtk--last-key-text
              (format "jump-to-register: %c is empty" ch)))
       ((and (consp (cdr cell)) (eq (car (cdr cell)) :point))
        (let* ((bn (nth 1 (cdr cell)))
               (pos (nth 2 (cdr cell)))
               (buf (get-buffer bn)))
          (cond
           ((null buf)
            (setq nemacs-gtk--last-key-text
                  (format "jump-to-register: buffer %s gone" bn)))
           (t
            (setq nemacs-gtk--active-buffer-name bn)
            (with-current-buffer buf
              (let ((clamped (max (nelisp-ec-point-min)
                                  (min pos (nelisp-ec-point-max)))))
                (nelisp-ec-goto-char clamped)))
            (nemacs-gtk--ensure-cursor-visible)
            (setq nemacs-gtk--last-key-text
                  (format "jump-to-register: %c -> %s:%d" ch bn pos))))))
       (t
        (setq nemacs-gtk--last-key-text
              (format "jump-to-register: %c is a string (use C-x r i)"
                      ch))))))
   (t
    (setq nemacs-gtk--last-key-text
          (format "register: unknown op %S" op)))))


;;;; --- bookmarks (Phase 2.AZ — C-x r m / b / l) --------------------------

(defun nemacs-gtk--bookmark-completion (input)
  "Return the bookmark names whose string starts with INPUT (= sorted)."
  (let ((acc '()))
    (dolist (cell nemacs-gtk--bookmarks)
      (when (string-prefix-p input (car cell))
        (push (car cell) acc)))
    (sort acc #'string<)))

(defun nemacs-gtk-bookmark-set ()
  "Bound to `C-x r m' — prompt for a bookmark NAME + save the active
buffer's current `(buffer-name, point)' under NAME.  Replaces any
existing bookmark with the same NAME."
  (interactive)
  (let ((bn nemacs-gtk--active-buffer-name)
        (pos (with-current-buffer (nemacs-gtk--active-buffer)
               (nelisp-ec-point))))
    (nemacs-gtk--enter-minibuffer
     (format "Set bookmark (%s:%d): " bn pos)
     (lambda (input)
       (cond
        ((or (null input) (= (length input) 0))
         (setq nemacs-gtk--last-key-text "bookmark-set: empty name, cancelled"))
        (t
         (setq nemacs-gtk--bookmarks
               (cons (cons input (list :buffer bn :pos pos))
                     (assoc-delete-all input nemacs-gtk--bookmarks)))
         (setq nemacs-gtk--last-key-text
               (format "bookmark-set: %s -> %s:%d" input bn pos))))))))

(defun nemacs-gtk-bookmark-jump ()
  "Bound to `C-x r b' — prompt for a bookmark NAME (with completion
across `--bookmarks') and jump to its saved buffer + position.
Tab completes to the longest common prefix.  Reports `not found' on
unknown names + `buffer gone' when the saved buffer was killed."
  (interactive)
  (cond
   ((null nemacs-gtk--bookmarks)
    (setq nemacs-gtk--last-key-text "bookmark-jump: no bookmarks"))
   (t
    (nemacs-gtk--enter-minibuffer
     "Jump to bookmark: "
     (lambda (input)
       (let ((cell (assoc input nemacs-gtk--bookmarks)))
         (cond
          ((null cell)
           (setq nemacs-gtk--last-key-text
                 (format "bookmark-jump: %s not found" input)))
          (t
           (let* ((rec (cdr cell))
                  (bn (plist-get rec :buffer))
                  (pos (plist-get rec :pos))
                  (buf (get-buffer bn)))
             (cond
              ((null buf)
               (setq nemacs-gtk--last-key-text
                     (format "bookmark-jump: buffer %s gone" bn)))
              (t
               (setq nemacs-gtk--active-buffer-name bn)
               (with-current-buffer buf
                 (let ((clamped (max (nelisp-ec-point-min)
                                     (min pos (nelisp-ec-point-max)))))
                   (nelisp-ec-goto-char clamped)))
               (nemacs-gtk--ensure-cursor-visible)
               (setq nemacs-gtk--last-key-text
                     (format "bookmark-jump: %s -> %s:%d" input bn pos)))))))))
     #'nemacs-gtk--bookmark-completion))))

(defun nemacs-gtk-bookmark-list ()
  "Bound to `C-x r l' — render `--bookmarks' into a `*Bookmarks*'
buffer + switch to it.  Output is one line per bookmark in the
form `NAME -> BUFFER:POS', sorted alphabetically by NAME."
  (interactive)
  (let* ((buf (get-buffer-create "*Bookmarks*"))
         (sorted (sort (copy-sequence nemacs-gtk--bookmarks)
                       (lambda (a b) (string< (car a) (car b))))))
    (with-current-buffer buf
      (when (fboundp 'erase-buffer)
        (erase-buffer))
      (cond
       ((null sorted)
        (nelisp-ec-insert "No bookmarks set.\n"))
       (t
        (nelisp-ec-insert "Bookmarks:\n\n")
        (dolist (cell sorted)
          (let ((name (car cell))
                (bn (plist-get (cdr cell) :buffer))
                (pos (plist-get (cdr cell) :pos)))
            (nelisp-ec-insert
             (format "  %-30s -> %s:%d\n" name bn pos)))))))
    (setq nemacs-gtk--active-buffer-name "*Bookmarks*")
    (setq nemacs-gtk--scroll-offset 0)
    (nemacs-gtk--sync-window-title)
    (setq nemacs-gtk--last-key-text
          (format "bookmark-list: %d entries" (length sorted)))))


;;;; --- sexp navigation (Phase 2.AY — C-M-f / C-M-b / C-M-k) --------------

(defun nemacs-gtk--sexp-symbol-char-p (ch)
  "Phase 2.AY — t when CH is part of a Lisp-style symbol token (=
alnum + the standard symbol-constituent punctuation set)."
  (or (and (>= ch ?a) (<= ch ?z))
      (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?0) (<= ch ?9))
      (memq ch '(?- ?_ ?: ?+ ?* ?/ ?< ?> ?= ?? ?! ?& ?~ ?@ ?. ?$ ?%))))

(defun nemacs-gtk--sexp-skip-forward-ws (pmax)
  "Advance point past whitespace + `;'-line-comments up to PMAX.
Returns the new point (= the position of the next non-trivia char,
or PMAX if EOB reached)."
  (let ((p (nelisp-ec-point)))
    (catch 'done
      (while (< p pmax)
        (let ((ch (emacs-edit--char-at p)))
          (cond
           ((memq ch '(?\s ?\t ?\n)) (setq p (1+ p)))
           ((eq ch ?\;)
            (while (and (< p pmax)
                        (not (eq (emacs-edit--char-at p) ?\n)))
              (setq p (1+ p))))
           (t (throw 'done nil))))))
    (nelisp-ec-goto-char (min p pmax))
    (nelisp-ec-point)))

(defun nemacs-gtk--sexp-skip-backward-ws (pmin)
  "Step point backward over plain whitespace down to PMIN.  Comments
are not skipped — backward comment recognition needs a line-walk
the MVP scan doesn't justify."
  (let ((p (nelisp-ec-point)))
    (while (and (> p pmin)
                (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
      (setq p (1- p)))
    (nelisp-ec-goto-char (max p pmin))
    (nelisp-ec-point)))

(defun nemacs-gtk--scan-sexp-forward (pmax)
  "Phase 2.AY — parse one balanced sexp forward from point, leaving
point at its end + returning that position.  Returns nil + leaves
point unchanged when the scan fails (= unmatched delimiter / EOB
mid-string).  Recognises (), [], {}, \"...\" with backslash escapes,
and bare symbol tokens.  Inside brackets, nested ()/[]/{} bump the
depth count and \"...\" / `;'-comments are skipped over."
  (let* ((start (nelisp-ec-point))
         (ch (and (< start pmax) (emacs-edit--char-at start))))
    (cond
     ((null ch) nil)
     ((memq ch '(?\( ?\[ ?\{))
      (let* ((close (cdr (assq ch '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))))
             (depth 1)
             (p (1+ start))
             (found nil))
        (catch 'done
          (while (< p pmax)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ch) (setq depth (1+ depth)))
               ((eq c close)
                (setq depth (1- depth))
                (when (zerop depth)
                  (setq found (1+ p))
                  (throw 'done nil)))
               ((eq c ?\")
                (setq p (1+ p))
                (while (and (< p pmax)
                            (not (eq (emacs-edit--char-at p) ?\")))
                  (when (eq (emacs-edit--char-at p) ?\\)
                    (setq p (1+ p)))
                  (setq p (1+ p))))
               ((eq c ?\;)
                (while (and (< p pmax)
                            (not (eq (emacs-edit--char-at p) ?\n)))
                  (setq p (1+ p))))))
            (setq p (1+ p))))
        (cond
         (found (nelisp-ec-goto-char found) found)
         (t nil))))
     ((eq ch ?\")
      (let ((p (1+ start)) (found nil))
        (catch 'done
          (while (< p pmax)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ?\\) (setq p (+ p 2)))
               ((eq c ?\") (setq found (1+ p)) (throw 'done nil))
               (t (setq p (1+ p)))))))
        (cond
         (found (nelisp-ec-goto-char found) found)
         (t nil))))
     ((nemacs-gtk--sexp-symbol-char-p ch)
      (let ((p start))
        (while (and (< p pmax)
                    (nemacs-gtk--sexp-symbol-char-p
                     (emacs-edit--char-at p)))
          (setq p (1+ p)))
        (nelisp-ec-goto-char p) p))
     (t
      ;; quote / unquote / sharp / etc. — step past 1 char so the next
      ;; sexp starts on the form being prefixed.
      (nelisp-ec-goto-char (1+ start)) (1+ start)))))

(defun nemacs-gtk--scan-sexp-backward (pmin)
  "Phase 2.AY — parse one balanced sexp backward from point, leaving
point at its start + returning that position.  Returns nil on
unmatched-delimiter scan failure.  Recognises ()/[]/{}, \"...\" and
symbol tokens.  Comments / nested strings are not detected on the
reverse pass — the MVP cost/value trade favours simplicity."
  (let* ((end (nelisp-ec-point))
         (ch (and (> end pmin) (emacs-edit--char-at (1- end)))))
    (cond
     ((null ch) nil)
     ((memq ch '(?\) ?\] ?\}))
      (let* ((open (cdr (assq ch '((?\) . ?\() (?\] . ?\[) (?\} . ?\{)))))
             (depth 1)
             (p (1- end))
             (found nil))
        (catch 'done
          (while (> p pmin)
            (setq p (1- p))
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ch) (setq depth (1+ depth)))
               ((eq c open)
                (setq depth (1- depth))
                (when (zerop depth)
                  (setq found p)
                  (throw 'done nil)))))))
        (cond
         (found (nelisp-ec-goto-char found) found)
         (t nil))))
     ((eq ch ?\")
      (let ((p (- end 2)) (found nil))
        (catch 'done
          (while (>= p pmin)
            (let ((c (emacs-edit--char-at p)))
              (cond
               ((eq c ?\") (setq found p) (throw 'done nil))
               (t (setq p (1- p)))))))
        (cond
         (found (nelisp-ec-goto-char found) found)
         (t nil))))
     ((nemacs-gtk--sexp-symbol-char-p ch)
      (let ((p (1- end)))
        (while (and (> p pmin)
                    (nemacs-gtk--sexp-symbol-char-p
                     (emacs-edit--char-at (1- p))))
          (setq p (1- p)))
        (nelisp-ec-goto-char p) p))
     (t
      (nelisp-ec-goto-char (1- end))
      (1- end)))))

(defun nemacs-gtk-forward-sexp ()
  "Bound to `C-M-f' (= [27 ?\\C-f]) — move point forward across one
balanced sexp, skipping leading whitespace + line comments."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((pmax (nelisp-ec-point-max)))
      (nemacs-gtk--sexp-skip-forward-ws pmax)
      (let ((res (nemacs-gtk--scan-sexp-forward pmax)))
        (cond
         ((null res)
          (setq nemacs-gtk--last-key-text "forward-sexp: scan-error"))
         (t
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "forward-sexp -> %d" res))))))))

(defun nemacs-gtk-backward-sexp ()
  "Bound to `C-M-b' (= [27 ?\\C-b]) — move point backward across one
balanced sexp, skipping trailing whitespace.  Comments are not
recognised on the reverse pass (MVP)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((pmin (nelisp-ec-point-min)))
      (nemacs-gtk--sexp-skip-backward-ws pmin)
      (let ((res (nemacs-gtk--scan-sexp-backward pmin)))
        (cond
         ((null res)
          (setq nemacs-gtk--last-key-text "backward-sexp: scan-error"))
         (t
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "backward-sexp -> %d" res))))))))

(defun nemacs-gtk-kill-sexp ()
  "Bound to `C-M-k' (= [27 ?\\C-k]) — kill the sexp following point.
Composes `forward-sexp' + `kill-region' so the deletion lands on
the kill-ring (= clipboard via the installed cut hook)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((pmax (nelisp-ec-point-max))
           (start (nelisp-ec-point)))
      (nemacs-gtk--sexp-skip-forward-ws pmax)
      (let* ((scan-start (nelisp-ec-point))
             (res (nemacs-gtk--scan-sexp-forward pmax)))
        (cond
         ((null res)
          (nelisp-ec-goto-char start)
          (setq nemacs-gtk--last-key-text "kill-sexp: scan-error"))
         (t
          (kill-region scan-start res)
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "kill-sexp: %d chars" (- res scan-start)))))))))


;;;; --- sentence + defun motion (Phase 2.BA — M-a / M-e / C-M-h) ----------

(defun nemacs-gtk--sentence-end-char-p (ch)
  "Phase 2.BA — t when CH closes a sentence (= `.', `!', `?').  Quote
+ paren chars that frequently follow are skipped over by the
sentence scanners after the closing punct rather than detected
here."
  (memq ch '(?. ?! ??)))

(defun nemacs-gtk-forward-sentence ()
  "Bound to `M-a' (= [27 ?a]) is `backward'; `M-e' is `forward'.
Forward sentence: scan forward from point past one sentence-ending
punctuation char + the trailing whitespace/quotes/parens.  Stops
at EOB.  Echoes `forward-sentence -> POS' on success."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((pmax (nelisp-ec-point-max))
           (p (nelisp-ec-point))
           (found nil))
      (catch 'done
        (while (< p pmax)
          (let ((ch (emacs-edit--char-at p)))
            (when (nemacs-gtk--sentence-end-char-p ch)
              ;; advance past the punct + any closing quote/paren
              ;; chars + the following whitespace
              (setq p (1+ p))
              (while (and (< p pmax)
                          (memq (emacs-edit--char-at p)
                                '(?\" ?\) ?\] ?\} ?\')))
                (setq p (1+ p)))
              (while (and (< p pmax)
                          (memq (emacs-edit--char-at p)
                                '(?\s ?\t ?\n)))
                (setq p (1+ p)))
              (setq found p)
              (throw 'done nil)))
          (setq p (1+ p))))
      (cond
       (found
        (nelisp-ec-goto-char found)
        (nemacs-gtk--ensure-cursor-visible)
        (setq nemacs-gtk--last-key-text
              (format "forward-sentence -> %d" found)))
       (t
        (nelisp-ec-goto-char pmax)
        (nemacs-gtk--ensure-cursor-visible)
        (setq nemacs-gtk--last-key-text
              "forward-sentence -> EOB"))))))

(defun nemacs-gtk-backward-sentence ()
  "Bound to `M-a' — scan backward to the start of the current sentence.
Definition: skip back through whitespace + closing-punct chars,
then find the nearest preceding sentence-ending punct (= `.', `!',
`?') and land just past the whitespace that follows it.  Stops at
BOB."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((pmin (nelisp-ec-point-min))
           (p (nelisp-ec-point))
           (found nil))
      ;; step back at least 1 if we're on a sentence-end char so the
      ;; same call can repeat-progress backward.
      (when (and (> p pmin)
                 (memq (emacs-edit--char-at (1- p))
                       '(?\s ?\t ?\n)))
        (setq p (1- p))
        (while (and (> p pmin)
                    (memq (emacs-edit--char-at (1- p))
                          '(?\s ?\t ?\n)))
          (setq p (1- p))))
      (catch 'done
        (while (> p pmin)
          (setq p (1- p))
          (when (nemacs-gtk--sentence-end-char-p (emacs-edit--char-at p))
            ;; skip back through the punct + closing chars to find a
            ;; sentence boundary preceded by whitespace.
            (setq found (1+ p))
            ;; advance past whitespace following the punct so found
            ;; lands on the start of the next sentence.
            (while (and (< found (nelisp-ec-point-max))
                        (memq (emacs-edit--char-at found)
                              '(?\s ?\t ?\n ?\" ?\) ?\] ?\} ?\')))
              (setq found (1+ found)))
            (throw 'done nil))))
      (cond
       (found
        (nelisp-ec-goto-char found)
        (nemacs-gtk--ensure-cursor-visible)
        (setq nemacs-gtk--last-key-text
              (format "backward-sentence -> %d" found)))
       (t
        (nelisp-ec-goto-char pmin)
        (nemacs-gtk--ensure-cursor-visible)
        (setq nemacs-gtk--last-key-text
              "backward-sentence -> BOB"))))))

(defun nemacs-gtk--beginning-of-defun ()
  "Move point to the nearest preceding line-starting `('.  This is
the substrate-MVP version of `beginning-of-defun' — true Emacs
honours `defun-prompt-regexp' / nested forms; we treat any `(' at
column 0 as a top-level form opener (which matches the convention
in elisp / scheme / etc.)."
  (let ((pmin (nelisp-ec-point-min))
        (p (nelisp-ec-point)))
    (catch 'done
      (while (> p pmin)
        ;; jump to the start of the current line
        (let ((bol p))
          (while (and (> bol pmin)
                      (not (eq (emacs-edit--char-at (1- bol)) ?\n)))
            (setq bol (1- bol)))
          (when (and (< bol (nelisp-ec-point-max))
                     (eq (emacs-edit--char-at bol) ?\())
            (nelisp-ec-goto-char bol)
            (throw 'done bol))
          (setq p (max pmin (1- bol))))))
    (nelisp-ec-point)))

(defun nemacs-gtk-mark-defun ()
  "Bound to `C-M-h' (= [27 ?\\C-h]) — set point at the start of the
enclosing top-level `(' form and mark at its matching `)'.
No-op + echo when no enclosing top-level form is found before BOB."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((start (nemacs-gtk--beginning-of-defun)))
      (cond
       ((not (eq (emacs-edit--char-at start) ?\())
        (setq nemacs-gtk--last-key-text "mark-defun: no top-level form"))
       (t
        (let* ((pmax (nelisp-ec-point-max))
               (end (nemacs-gtk--scan-sexp-forward pmax)))
          (cond
           ((null end)
            (nelisp-ec-goto-char start)
            (setq nemacs-gtk--last-key-text "mark-defun: scan-error"))
           (t
            (setq nemacs-gtk--mark-pos end)
            (setq nemacs-gtk--mark-buffer (nemacs-gtk--active-buffer))
            (nelisp-ec-goto-char start)
            (nemacs-gtk--ensure-cursor-visible)
            (setq nemacs-gtk--last-key-text
                  (format "mark-defun: %d..%d" start end))))))))))


;;;; --- bundle Phase 2.BB (C-x C-o, M-k, C-x f) ----------------------------

(defun nemacs-gtk--blank-line-at-p (pos)
  "Phase 2.BB — t when POS is on a logical blank line (= line content
between its bol/eol contains only whitespace)."
  (save-current-buffer
    (set-buffer (nemacs-gtk--active-buffer))
    (let ((pmin (nelisp-ec-point-min))
          (pmax (nelisp-ec-point-max))
          (bol pos)
          (eol pos)
          (blank t))
      (while (and (> bol pmin)
                  (not (eq (emacs-edit--char-at (1- bol)) ?\n)))
        (setq bol (1- bol)))
      (while (and (< eol pmax)
                  (not (eq (emacs-edit--char-at eol) ?\n)))
        (setq eol (1+ eol)))
      (let ((p bol))
        (while (and blank (< p eol))
          (unless (memq (emacs-edit--char-at p) '(?\s ?\t))
            (setq blank nil))
          (setq p (1+ p))))
      blank)))

(defun nemacs-gtk-delete-blank-lines ()
  "Bound to `C-x C-o' — delete the run of blank lines around point.
On a non-blank line, delete the blank lines that follow.  Leaves a
single blank line when the run was multi-line; deletes the lone
blank when only one was present.  No-op + echo when the buffer has
no blanks adjacent to point."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((pmin (nelisp-ec-point-min))
           (pmax (nelisp-ec-point-max))
           (p (nelisp-ec-point))
           (bol p))
      (while (and (> bol pmin)
                  (not (eq (emacs-edit--char-at (1- bol)) ?\n)))
        (setq bol (1- bol)))
      (cond
       ((nemacs-gtk--blank-line-at-p p)
        ;; on blank: scan up + down for run, keep one blank line.
        (let ((run-start bol)
              (run-end-bol bol))
          ;; walk up over blank lines
          (while (and (> run-start pmin)
                      (let ((prev-bol (1- run-start)))
                        (while (and (> prev-bol pmin)
                                    (not (eq (emacs-edit--char-at
                                              (1- prev-bol)) ?\n)))
                          (setq prev-bol (1- prev-bol)))
                        (and (nemacs-gtk--blank-line-at-p prev-bol)
                             (setq run-start prev-bol)))))
          ;; walk down over blank lines (advance bol past each \n)
          (let ((next-bol bol))
            (catch 'done
              (while (< next-bol pmax)
                (let ((eol next-bol))
                  (while (and (< eol pmax)
                              (not (eq (emacs-edit--char-at eol) ?\n)))
                    (setq eol (1+ eol)))
                  (when (and (< eol pmax)
                             (eq (emacs-edit--char-at eol) ?\n))
                    (setq next-bol (1+ eol))
                    (cond
                     ((and (< next-bol pmax)
                           (nemacs-gtk--blank-line-at-p next-bol))
                      (setq run-end-bol next-bol))
                     (t (throw 'done nil))))
                  (when (= eol pmax) (throw 'done nil))))))
          ;; find eol after run-end-bol — that's where the run's last
          ;; line's `\n' lives.  We keep one blank line: keep
          ;; [run-start, run-start+1) (= the leading `\n' that closes
          ;; the previous non-blank line) and delete from there to
          ;; the end of the run.
          (let ((run-end run-end-bol))
            (while (and (< run-end pmax)
                        (not (eq (emacs-edit--char-at run-end) ?\n)))
              (setq run-end (1+ run-end)))
            (when (and (< run-end pmax)
                       (eq (emacs-edit--char-at run-end) ?\n))
              (setq run-end (1+ run-end)))
            (let ((keep-end (min run-end (1+ run-start))))
              (cond
               ((>= keep-end run-end)
                (setq nemacs-gtk--last-key-text
                      "delete-blank-lines: nothing to remove"))
               (t
                (nelisp-ec-delete-region keep-end run-end)
                (nelisp-ec-goto-char run-start)
                (setq nemacs-gtk--last-key-text "delete-blank-lines")))))))
       (t
        ;; not on blank: delete blank lines that follow current line.
        (let ((next-bol bol))
          (while (and (< next-bol pmax)
                      (not (eq (emacs-edit--char-at next-bol) ?\n)))
            (setq next-bol (1+ next-bol)))
          (when (and (< next-bol pmax)
                     (eq (emacs-edit--char-at next-bol) ?\n))
            (setq next-bol (1+ next-bol)))
          (let ((scan next-bol))
            (while (and (< scan pmax)
                        (nemacs-gtk--blank-line-at-p scan))
              (let ((eol scan))
                (while (and (< eol pmax)
                            (not (eq (emacs-edit--char-at eol) ?\n)))
                  (setq eol (1+ eol)))
                (when (and (< eol pmax)
                           (eq (emacs-edit--char-at eol) ?\n))
                  (setq scan (1+ eol)))
                (when (= eol pmax) (setq scan pmax))))
            (cond
             ((> scan next-bol)
              (nelisp-ec-delete-region next-bol scan)
              (setq nemacs-gtk--last-key-text "delete-blank-lines"))
             (t
              (setq nemacs-gtk--last-key-text "delete-blank-lines: none to delete"))))))))))

(defun nemacs-gtk-kill-sentence ()
  "Bound to `M-k' — kill from point to the end of the current sentence
(= composes `forward-sentence' + `kill-region' so the deletion
lands on `kill-ring')."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((start (nelisp-ec-point))
           (pmax (nelisp-ec-point-max))
           (p start)
           (found nil))
      (catch 'done
        (while (< p pmax)
          (let ((ch (emacs-edit--char-at p)))
            (when (nemacs-gtk--sentence-end-char-p ch)
              (setq p (1+ p))
              (while (and (< p pmax)
                          (memq (emacs-edit--char-at p)
                                '(?\" ?\) ?\] ?\} ?\')))
                (setq p (1+ p)))
              (setq found p)
              (throw 'done nil)))
          (setq p (1+ p))))
      (let ((end (or found pmax)))
        (cond
         ((= end start)
          (setq nemacs-gtk--last-key-text "kill-sentence: empty"))
         (t
          (kill-region start end)
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "kill-sentence: %d chars" (- end start)))))))))

(defun nemacs-gtk-set-fill-column ()
  "Bound to `C-x f' — minibuffer-prompt for an integer + set the
substrate's `--fill-column' (= the column `M-q' / `fill-paragraph'
wraps at).  Rejects non-numeric input + values < 1."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   (format "Set fill-column (current %d): " nemacs-gtk--fill-column)
   (lambda (input)
     (let ((n (and input (> (length input) 0)
                   (string-to-number input))))
       (cond
        ((or (null n) (< n 1))
         (setq nemacs-gtk--last-key-text
               "set-fill-column: invalid input"))
        (t
         (setq nemacs-gtk--fill-column n)
         (setq nemacs-gtk--last-key-text
               (format "set-fill-column: %d" n))))))))


;;;; --- narrowing (Phase 2.BC — C-x n n / C-x n w / C-x n d) ---------------

(defun nemacs-gtk-narrow-to-region ()
  "Bound to `C-x n n' — restrict the active buffer's point-min / -max
to the current region (= [mark .. point]).  All buffer ops + paint
honour the narrowed window since the substrate's point-min /
point-max accessors are what they all read."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (cond
     ((null nemacs-gtk--mark-pos)
      (setq nemacs-gtk--last-key-text "narrow-to-region: no mark"))
     (t
      (let* ((s (min (nelisp-ec-point) nemacs-gtk--mark-pos))
             (e (max (nelisp-ec-point) nemacs-gtk--mark-pos)))
        (narrow-to-region s e)
        (setq nemacs-gtk--last-key-text
              (format "narrow-to-region: %d..%d" s e)))))))

(defun nemacs-gtk-widen ()
  "Bound to `C-x n w' — drop any narrowing restriction on the active
buffer (= the buffer's `point-min'/`point-max' accessors revert to
its absolute bounds)."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (widen)
    (setq nemacs-gtk--last-key-text "widen")))

(defun nemacs-gtk-narrow-to-defun ()
  "Bound to `C-x n d' — narrow the active buffer to the enclosing
top-level `(' form (= what `mark-defun' would mark).  No-op + echo
when no enclosing form is found."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let ((start (nemacs-gtk--beginning-of-defun)))
      (cond
       ((not (eq (emacs-edit--char-at start) ?\())
        (setq nemacs-gtk--last-key-text "narrow-to-defun: no top-level form"))
       (t
        (let ((end (nemacs-gtk--scan-sexp-forward (nelisp-ec-point-max))))
          (cond
           ((null end)
            (setq nemacs-gtk--last-key-text "narrow-to-defun: scan-error"))
           (t
            (narrow-to-region start end)
            (setq nemacs-gtk--last-key-text
                  (format "narrow-to-defun: %d..%d" start end))))))))))


;;;; --- help (Phase 2.BD — C-h f / v / a) ----------------------------------

(defun nemacs-gtk--symbol-completion (input)
  "Phase 2.BD — return curated names whose prefix matches INPUT, sorted.
Reuses `--m-x-commands' so the help bundle benefits from the same
hand-curated set the M-x prompt uses."
  (let ((acc '()))
    (dolist (name nemacs-gtk--m-x-commands)
      (when (string-prefix-p input name)
        (push name acc)))
    (sort acc #'string<)))

(defun nemacs-gtk--describe-symbol-function (sym)
  "Return a string describing SYM as a function, or nil if SYM has
no function binding."
  (when (fboundp sym)
    (let ((fn (symbol-function sym)))
      (concat
       (format "%s is " (symbol-name sym))
       (cond
        ((subrp fn) (format "a built-in function.\n\n%S" fn))
        ((byte-code-function-p fn)
         (format "a compiled function.\n\n%S" fn))
        ((and (consp fn) (eq (car fn) 'closure))
         (format "an interpreted closure.\n\n%S" fn))
        ((and (consp fn) (eq (car fn) 'lambda))
         (format "an interpreted function.\n\n%S" fn))
        ((and (consp fn) (eq (car fn) 'macro))
         (format "a macro.\n\n%S" fn))
        ((symbolp fn)
         (format "an alias for `%s'.\n\n%S" fn fn))
        (t (format "a function.\n\n%S" fn)))))))

(defun nemacs-gtk--describe-symbol-variable (sym)
  "Return a string describing SYM as a variable, or nil if SYM has
no value binding."
  (when (boundp sym)
    (format "%s is a variable.\n\nValue: %S"
            (symbol-name sym) (symbol-value sym))))

(defun nemacs-gtk-describe-function ()
  "Bound to `C-h f' — minibuffer-prompt for a function name with
completion across `--m-x-commands'; render its definition into a
`*Help*' buffer + switch to it.  Reports `not a function' on names
that aren't `fboundp' (= the user is offered everything, but only
real functions resolve)."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Describe function: "
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "describe-function: empty"))
      (t
       (let* ((sym (intern input))
              (text (nemacs-gtk--describe-symbol-function sym))
              (buf (get-buffer-create "*Help*")))
         (cond
          ((null text)
           (setq nemacs-gtk--last-key-text
                 (format "describe-function: %s is not a function" input)))
          (t
           (with-current-buffer buf
             (when (fboundp 'erase-buffer) (erase-buffer))
             (nelisp-ec-insert text)
             (nelisp-ec-insert "\n"))
           (setq nemacs-gtk--active-buffer-name "*Help*")
           (setq nemacs-gtk--scroll-offset 0)
           (nemacs-gtk--sync-window-title)
           (setq nemacs-gtk--last-key-text
                 (format "describe-function: %s" input))))))))
   #'nemacs-gtk--symbol-completion))

(defun nemacs-gtk-describe-variable ()
  "Bound to `C-h v' — minibuffer-prompt for a variable name; render
its current value into a `*Help*' buffer + switch to it.  Reports
`not bound' on names that aren't `boundp'."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Describe variable: "
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "describe-variable: empty"))
      (t
       (let* ((sym (intern input))
              (text (nemacs-gtk--describe-symbol-variable sym))
              (buf (get-buffer-create "*Help*")))
         (cond
          ((null text)
           (setq nemacs-gtk--last-key-text
                 (format "describe-variable: %s not bound" input)))
          (t
           (with-current-buffer buf
             (when (fboundp 'erase-buffer) (erase-buffer))
             (nelisp-ec-insert text)
             (nelisp-ec-insert "\n"))
           (setq nemacs-gtk--active-buffer-name "*Help*")
           (setq nemacs-gtk--scroll-offset 0)
           (nemacs-gtk--sync-window-title)
           (setq nemacs-gtk--last-key-text
                 (format "describe-variable: %s" input))))))))))

(defun nemacs-gtk-apropos ()
  "Bound to `C-h a' — minibuffer-prompt for a substring; render names
from `--m-x-commands' containing that substring (case-insensitive)
into an `*Apropos*' buffer + switch to it."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   "Apropos: "
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "apropos: empty"))
      (t
       (let* ((needle (downcase input))
              (matches (let ((acc '()))
                         (dolist (name nemacs-gtk--m-x-commands)
                           (when (string-match-p
                                  (regexp-quote needle)
                                  (downcase name))
                             (push name acc)))
                         (sort acc #'string<)))
              (buf (get-buffer-create "*Apropos*")))
         (with-current-buffer buf
           (when (fboundp 'erase-buffer) (erase-buffer))
           (nelisp-ec-insert
            (format "Apropos: %s\n\n%d matches:\n\n"
                    input (length matches)))
           (dolist (name matches)
             (nelisp-ec-insert (format "  %s\n" name))))
         (setq nemacs-gtk--active-buffer-name "*Apropos*")
         (setq nemacs-gtk--scroll-offset 0)
         (nemacs-gtk--sync-window-title)
         (setq nemacs-gtk--last-key-text
               (format "apropos: %d matches for %s"
                       (length matches) input))))))))


;;;; --- bundle Phase 2.BE (mark-sexp, M-r cycle, C-x DEL, C-x z repeat) -----

(defun nemacs-gtk-mark-sexp ()
  "Bound to `C-M-SPC' (= [27 0]) — extend the active region by one
sexp.  When no mark is set, set mark at point first; then advance
point across the next sexp using the Phase 2.AY scanner.  Repeated
calls extend the region one sexp at a time."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (when (null nemacs-gtk--mark-pos)
      (setq nemacs-gtk--mark-pos (nelisp-ec-point))
      (setq nemacs-gtk--mark-buffer (nemacs-gtk--active-buffer)))
    (let ((pmax (nelisp-ec-point-max)))
      (nemacs-gtk--sexp-skip-forward-ws pmax)
      (let ((res (nemacs-gtk--scan-sexp-forward pmax)))
        (cond
         ((null res)
          (setq nemacs-gtk--last-key-text "mark-sexp: scan-error"))
         (t
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "mark-sexp -> %d" res))))))))

(defvar nemacs-gtk--m-r-state 0
  "Phase 2.BE — M-r cycle counter (0=top, 1=middle, 2=bottom).
Bumped each time `move-to-window-line-top-bottom' runs; reset to
0 by any other command via the dispatcher post-step hook.")

(defun nemacs-gtk-move-to-window-line-top-bottom ()
  "Bound to `M-r' — move point to the top, middle, or bottom of the
viewport on a 3-cycle.  First press = top, second = middle,
third = bottom; subsequent press wraps back to top.  Cycle resets
when any non-M-r command runs in between."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((scroll nemacs-gtk--scroll-offset)
           (rows nemacs-gtk--buffer-area-end)
           (target-row (cond
                        ((= nemacs-gtk--m-r-state 0) scroll)
                        ((= nemacs-gtk--m-r-state 1)
                         (+ scroll (/ rows 2)))
                        (t (+ scroll (max 0 (- rows 1))))))
           (label (cond
                   ((= nemacs-gtk--m-r-state 0) "top")
                   ((= nemacs-gtk--m-r-state 1) "middle")
                   (t "bottom")))
           (pmin (nelisp-ec-point-min))
           (pmax (nelisp-ec-point-max))
           (p pmin)
           (cur 0))
      (while (and (< p pmax) (< cur target-row))
        (when (eq (emacs-edit--char-at p) ?\n)
          (setq cur (1+ cur)))
        (setq p (1+ p)))
      (nelisp-ec-goto-char p)
      (setq nemacs-gtk--m-r-state (mod (1+ nemacs-gtk--m-r-state) 3))
      (setq nemacs-gtk--last-key-text
            (format "move-to-window-line: %s" label)))))

(defun nemacs-gtk-backward-kill-sentence ()
  "Bound to `C-x DEL' — kill from the start of the current sentence
to point (= the inverse of M-k).  Composes the backward-sentence
scanner with `kill-region' so the deletion lands on `kill-ring'."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((end (nelisp-ec-point))
           (pmin (nelisp-ec-point-min))
           (p end)
           (found nil))
      (when (and (> p pmin)
                 (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
        (while (and (> p pmin)
                    (memq (emacs-edit--char-at (1- p)) '(?\s ?\t ?\n)))
          (setq p (1- p))))
      (catch 'done
        (while (> p pmin)
          (setq p (1- p))
          (when (nemacs-gtk--sentence-end-char-p (emacs-edit--char-at p))
            (setq found (1+ p))
            (while (and (< found end)
                        (memq (emacs-edit--char-at found)
                              '(?\s ?\t ?\n ?\" ?\) ?\] ?\} ?\')))
              (setq found (1+ found)))
            (throw 'done nil))))
      (let ((start (or found pmin)))
        (cond
         ((= start end)
          (setq nemacs-gtk--last-key-text "backward-kill-sentence: empty"))
         (t
          (kill-region start end)
          (nemacs-gtk--ensure-cursor-visible)
          (setq nemacs-gtk--last-key-text
                (format "backward-kill-sentence: %d chars" (- end start)))))))))

(defun nemacs-gtk-repeat ()
  "Bound to `C-x z' — re-run the most recent command tracked by
`emacs-command-loop--last-command' (= the substrate's `this-command'
promotion slot, set after each successful dispatch).  Echoes a
diagnostic + does nothing when the slot is unset / the command
isn't fboundp."
  (interactive)
  (cond
   ((not (boundp 'emacs-command-loop--last-command))
    (setq nemacs-gtk--last-key-text "repeat: substrate slot missing"))
   ((or (null emacs-command-loop--last-command)
        (eq emacs-command-loop--last-command 'nemacs-gtk-repeat))
    (setq nemacs-gtk--last-key-text "repeat: nothing to repeat"))
   ((not (fboundp emacs-command-loop--last-command))
    (setq nemacs-gtk--last-key-text
          (format "repeat: %s not fboundp"
                  emacs-command-loop--last-command)))
   (t
    (let ((cmd emacs-command-loop--last-command))
      (call-interactively cmd)
      (setq nemacs-gtk--last-key-text
            (format "repeat: %s" cmd))))))


;;;; --- font zoom (Phase 2.BF — C-x + / C-x - / text-scale-reset) ---------

(defvar nemacs-gtk--font-size 12
  "Phase 2.BF — current Pango point size used by the GTK backend.
Mirrors the Rust side's `font' field via
`nelisp-gtk-set-font-size'.  Default = 12 (= matches the original
const FONT in gtk_backend.rs).")

(defconst nemacs-gtk--font-size-min 4
  "Lower bound for `--font-size' — below this Pango refuses to render.")

(defconst nemacs-gtk--font-size-max 60
  "Upper bound for `--font-size' — capped well below the Rust extern's
limit (200) so a typo doesn't blank the screen.")

(defun nemacs-gtk--apply-font-size ()
  "Push `--font-size' into the Rust backend.  Errors from the Rust
extern (= out-of-range / extern not bound) are caught + reported on
the echo-area row instead of bubbling."
  (cond
   ((not (fboundp 'nelisp-gtk-set-font-size))
    (setq nemacs-gtk--last-key-text
          "font-zoom: Rust extern not loaded"))
   (t
    (condition-case err
        (progn
          (nelisp-gtk-set-font-size nemacs-gtk--font-size)
          (when (fboundp 'nelisp-gtk-redraw)
            (nelisp-gtk-redraw))
          (setq nemacs-gtk--last-key-text
                (format "font-size: %d" nemacs-gtk--font-size)))
      (error
       (setq nemacs-gtk--last-key-text
             (format "font-zoom: %s" (error-message-string err))))))))

(defun nemacs-gtk-text-scale-increase ()
  "Bound to `C-x +' — bump the GTK font size by 1 point (capped at
`--font-size-max').  Triggers a re-measure + redraw on the Rust
side; the next paint surfaces the new cell metrics."
  (interactive)
  (let ((new (1+ nemacs-gtk--font-size)))
    (cond
     ((> new nemacs-gtk--font-size-max)
      (setq nemacs-gtk--last-key-text
            (format "text-scale-increase: max %d reached"
                    nemacs-gtk--font-size-max)))
     (t
      (setq nemacs-gtk--font-size new)
      (nemacs-gtk--apply-font-size)))))

(defun nemacs-gtk-text-scale-decrease ()
  "Bound to `C-x -' — drop the GTK font size by 1 point (floor at
`--font-size-min')."
  (interactive)
  (let ((new (1- nemacs-gtk--font-size)))
    (cond
     ((< new nemacs-gtk--font-size-min)
      (setq nemacs-gtk--last-key-text
            (format "text-scale-decrease: min %d reached"
                    nemacs-gtk--font-size-min)))
     (t
      (setq nemacs-gtk--font-size new)
      (nemacs-gtk--apply-font-size)))))

(defun nemacs-gtk-text-scale-reset ()
  "Reset the GTK font size to the default (= 12 points).  No keymap
binding by default; reachable via `M-x'.  Useful when iterative
+/- has drifted off the original size."
  (interactive)
  (setq nemacs-gtk--font-size 12)
  (nemacs-gtk--apply-font-size))


;;;; --- bundle Phase 2.BG (C-x C-d list-dir, C-x C-v alternate, version) ---

(defun nemacs-gtk-list-directory ()
  "Bound to `C-x C-d' — minibuffer-prompt for a directory path; list
its contents into a `*Directory*' buffer.  Each entry shows a file
type indicator (`d' / `-') + the filename.  Errors from
`directory-files' (= path doesn't exist / not readable) are caught
and reported on the echo-area row."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   (format "List directory (%s): "
           (or (and (boundp 'default-directory) default-directory) "."))
   (lambda (input)
     (let ((dir (if (or (null input) (= (length input) 0))
                    (or (and (boundp 'default-directory) default-directory)
                        ".")
                  input)))
       (condition-case err
           (let* ((abs (expand-file-name dir))
                  (files (sort (directory-files abs) #'string<))
                  (buf (get-buffer-create "*Directory*")))
             (with-current-buffer buf
               (when (fboundp 'erase-buffer) (erase-buffer))
               (nelisp-ec-insert (format "Directory: %s\n\n" abs))
               (dolist (f files)
                 (let* ((full (expand-file-name f abs))
                        (kind (cond
                               ((file-directory-p full) "d")
                               ((file-exists-p full) "-")
                               (t "?"))))
                   (nelisp-ec-insert (format "  %s  %s\n" kind f)))))
             (setq nemacs-gtk--active-buffer-name "*Directory*")
             (setq nemacs-gtk--scroll-offset 0)
             (nemacs-gtk--sync-window-title)
             (setq nemacs-gtk--last-key-text
                   (format "list-directory: %d entries in %s"
                           (length files) abs)))
         (error
          (setq nemacs-gtk--last-key-text
                (format "list-directory: %s"
                        (error-message-string err)))))))))

(defun nemacs-gtk-find-alternate-file ()
  "Bound to `C-x C-v' — replace the active buffer's contents with the
contents of a different file (= the user-supplied path).  In real
Emacs this also disposes the current buffer; our MVP keeps it
around but rewires its visited filename + content via
`set-visited-file-name' + `revert-buffer'-style reload."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   (format "Find alternate file (current %s): "
           (or (and (fboundp 'buffer-file-name) (buffer-file-name)) "<none>"))
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "find-alternate-file: empty"))
      (t
       (condition-case err
           (let ((abs (expand-file-name input)))
             (with-current-buffer (nemacs-gtk--active-buffer)
               (when (fboundp 'erase-buffer) (erase-buffer))
               (when (file-exists-p abs)
                 (insert-file-contents abs))
               (when (fboundp 'set-visited-file-name)
                 (set-visited-file-name abs))
               (when (fboundp 'set-buffer-modified-p)
                 (set-buffer-modified-p nil)))
             (nemacs-gtk--ensure-cursor-visible)
             (setq nemacs-gtk--last-key-text
                   (format "find-alternate-file: %s" abs)))
         (error
          (setq nemacs-gtk--last-key-text
                (format "find-alternate-file: %s"
                        (error-message-string err))))))))))

(defun nemacs-gtk-version ()
  "M-x version — echo a one-line nelisp-emacs build identifier on the
echo-area row.  Read-only diagnostic; safe to call any time."
  (interactive)
  (setq nemacs-gtk--last-key-text
        (format "nelisp-emacs (Doc 51 Track D + GTK Layer 3, Phase 2 / font %dpt)"
                nemacs-gtk--font-size)))


;;;; --- bundle Phase 2.BI (electric-pair, transient-mark, iconify) ---------

(defvar nemacs-gtk--electric-pair-mode nil
  "Phase 2.BI — t when typing an opener (= `(' / `[' / `{' / `\"')
auto-inserts the matching close after point, and typing a closer
that already sits at point steps over it instead of inserting a
duplicate.  Off by default — toggle via M-x electric-pair-mode.")

(defvar nemacs-gtk--transient-mark-mode t
  "Phase 2.BI — t when the [mark .. point] region renders as a
translucent overlay (= what Phase 2.BH paints).  Off when the user
prefers Emacs's traditional always-active mark semantics + no
visible region.  Default = on, matching modern Emacs.")

(defun nemacs-gtk-electric-pair-mode ()
  "Toggle `--electric-pair-mode'.  When enabled, opener / closer
chars dispatch through `--electric-pair-handle' before the normal
self-insert path; when disabled, they self-insert as usual."
  (interactive)
  (setq nemacs-gtk--electric-pair-mode
        (not nemacs-gtk--electric-pair-mode))
  (setq nemacs-gtk--last-key-text
        (format "electric-pair-mode: %s"
                (if nemacs-gtk--electric-pair-mode "on" "off"))))

(defun nemacs-gtk-transient-mark-mode ()
  "Toggle `--transient-mark-mode'.  When off, `--paint-region-overlay'
clears any active region highlight on the next paint cycle (= the
mark is still set, but the GTK overlay isn't drawn).  Useful for
users who want the canonical Emacs `(setq mark-active nil)' feel."
  (interactive)
  (setq nemacs-gtk--transient-mark-mode
        (not nemacs-gtk--transient-mark-mode))
  (setq nemacs-gtk--last-key-text
        (format "transient-mark-mode: %s"
                (if nemacs-gtk--transient-mark-mode "on" "off"))))

(defun nemacs-gtk-iconify-frame ()
  "Bound to `C-z' — ask the GTK side to minimize the application
window via `nelisp-gtk-iconify-frame'.  No-op + echo when the
extern isn't loaded."
  (interactive)
  (cond
   ((not (fboundp 'nelisp-gtk-iconify-frame))
    (setq nemacs-gtk--last-key-text "iconify-frame: extern not loaded"))
   (t
    (nelisp-gtk-iconify-frame)
    (setq nemacs-gtk--last-key-text "iconify-frame"))))

(defconst nemacs-gtk--electric-open-pairs
  '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?\" . ?\"))
  "Phase 2.BI — alist mapping an electric opener char to its closer.
Note: the apostrophe `\\'' is intentionally omitted — its overload
as a quote prefix in lisp + as a contraction marker in prose makes
auto-pairing it net-negative.")

(defconst nemacs-gtk--electric-close-set
  '(?\) ?\] ?\} ?\")
  "Phase 2.BI — set of closer chars electric-pair-mode treats as
`step-past' candidates when the next char at point already
matches.")

(defun nemacs-gtk--electric-pair-handle (ch)
  "Phase 2.BI dispatcher tail — apply electric-pair logic for CH:

  - opener: insert OPEN+CLOSE, leave point between
  - closer at next-char: just step past (= `eat')
  - unmatched closer: self-insert as usual

CH is the bare integer event the dispatcher would otherwise feed
through `self-insert-command'."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (nelisp-ec-point))
           (pmax (nelisp-ec-point-max))
           (next (and (< p pmax) (emacs-edit--char-at p)))
           (open-pair (assq ch nemacs-gtk--electric-open-pairs)))
      (cond
       ;; Closer + next char is the same → step past
       ((and (memq ch nemacs-gtk--electric-close-set)
             (eq next ch))
        (nelisp-ec-goto-char (1+ p))
        (setq nemacs-gtk--last-key-text
              (format "electric-pair: skip %c" ch)))
       ;; Opener → insert OPEN+CLOSE, point between
       (open-pair
        (let ((close (cdr open-pair)))
          (nelisp-ec-insert (string ch close))
          (nelisp-ec-goto-char (1+ p))
          (setq nemacs-gtk--last-key-text
                (format "electric-pair: %c%c" ch close))))
       ;; Plain closer with no match → just self-insert
       (t
        (nelisp-ec-insert (string ch))
        (setq nemacs-gtk--last-key-text
              (format "electric-pair: %c (no match)" ch)))))))


;;;; --- bundle Phase 2.BJ (case regions + kill-this-buffer + quit-window) --

(defun nemacs-gtk--region-bounds-or-error ()
  "Return `(START . END)' for the active region in the active buffer,
or nil + set echo when no mark is set / point == mark."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (cond
     ((null nemacs-gtk--mark-pos)
      (setq nemacs-gtk--last-key-text "no mark set")
      nil)
     (t
      (let* ((p (nelisp-ec-point))
             (s (min p nemacs-gtk--mark-pos))
             (e (max p nemacs-gtk--mark-pos)))
        (cond
         ((= s e)
          (setq nemacs-gtk--last-key-text "empty region")
          nil)
         (t (cons s e))))))))

(defun nemacs-gtk-upcase-region ()
  "Bound to `C-x C-u' — replace the active region's text with its
upper-cased form via `upcase'.  No-op + echo when no region is set."
  (interactive)
  (let ((b (nemacs-gtk--region-bounds-or-error)))
    (when b
      (with-current-buffer (nemacs-gtk--active-buffer)
        (let* ((s (car b)) (e (cdr b))
               (text (nelisp-ec-buffer-substring s e))
               (up   (upcase text)))
          (nelisp-ec-delete-region s e)
          (nelisp-ec-goto-char s)
          (nelisp-ec-insert up)
          (setq nemacs-gtk--last-key-text
                (format "upcase-region: %d chars" (length text))))))))

(defun nemacs-gtk-downcase-region ()
  "Bound to `C-x C-l' — replace the active region's text with its
lower-cased form via `downcase'.  No-op + echo when no region is set."
  (interactive)
  (let ((b (nemacs-gtk--region-bounds-or-error)))
    (when b
      (with-current-buffer (nemacs-gtk--active-buffer)
        (let* ((s (car b)) (e (cdr b))
               (text (nelisp-ec-buffer-substring s e))
               (dn   (downcase text)))
          (nelisp-ec-delete-region s e)
          (nelisp-ec-goto-char s)
          (nelisp-ec-insert dn)
          (setq nemacs-gtk--last-key-text
                (format "downcase-region: %d chars" (length text))))))))

(defun nemacs-gtk-capitalize-region ()
  "M-x capitalize-region — title-case each word in the active region.
Whitespace + punctuation between words are preserved verbatim."
  (interactive)
  (let ((b (nemacs-gtk--region-bounds-or-error)))
    (when b
      (with-current-buffer (nemacs-gtk--active-buffer)
        (let* ((s (car b)) (e (cdr b))
               (text (nelisp-ec-buffer-substring s e))
               (cap  (capitalize text)))
          (nelisp-ec-delete-region s e)
          (nelisp-ec-goto-char s)
          (nelisp-ec-insert cap)
          (setq nemacs-gtk--last-key-text
                (format "capitalize-region: %d chars" (length text))))))))

(defun nemacs-gtk-kill-this-buffer ()
  "M-x kill-this-buffer — kill the active buffer without prompting.
Switches the GUI to `*welcome*' afterward (= what `kill-buffer'
does when the killed buffer was the only display target).  Refuses
to kill `*welcome*' itself + `*Messages*' (= protective)."
  (interactive)
  (let ((bn nemacs-gtk--active-buffer-name))
    (cond
     ((member bn '("*welcome*" "*Messages*"))
      (setq nemacs-gtk--last-key-text
            (format "kill-this-buffer: refusing to kill %s" bn)))
     (t
      (when (fboundp 'kill-buffer)
        (kill-buffer bn))
      (setq nemacs-gtk--active-buffer-name "*welcome*")
      (setq nemacs-gtk--scroll-offset 0)
      (nemacs-gtk--sync-window-title)
      (setq nemacs-gtk--last-key-text
            (format "kill-this-buffer: %s" bn))))))

(defun nemacs-gtk-quit-window ()
  "M-x quit-window — switch the GUI back to `*welcome*' without
killing the current buffer.  Useful for dismissing read-only
help / list buffers (= `*Help*' / `*Bookmarks*' / `*Apropos*')
where the user wants to keep the content but stop displaying it."
  (interactive)
  (let ((bn nemacs-gtk--active-buffer-name))
    (cond
     ((string= bn "*welcome*")
      (setq nemacs-gtk--last-key-text "quit-window: already at welcome"))
     (t
      (setq nemacs-gtk--active-buffer-name "*welcome*")
      (setq nemacs-gtk--scroll-offset 0)
      (nemacs-gtk--sync-window-title)
      (setq nemacs-gtk--last-key-text
            (format "quit-window: hid %s" bn))))))


;;;; --- bundle Phase 2.BK (cheat-sheet + scratch + messages) --------------

(defun nemacs-gtk-cheat-sheet ()
  "M-x cheat-sheet — refresh + display the welcome buffer.  Same
content the GUI shows on boot, useful when the user has switched
away + wants the keybinding reference back."
  (interactive)
  (nemacs-gtk--prepare-welcome-buffer)
  (setq nemacs-gtk--active-buffer-name "*welcome*")
  (setq nemacs-gtk--scroll-offset 0)
  (nemacs-gtk--sync-window-title)
  (setq nemacs-gtk--last-key-text "cheat-sheet"))

(defun nemacs-gtk-scratch-buffer ()
  "M-x scratch-buffer — switch to `*scratch*', create + seed with a
boilerplate header if it doesn't exist yet.  The default content
mirrors real Emacs's `initial-scratch-message'."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (when (= (with-current-buffer buf (nelisp-ec-point-max))
             (with-current-buffer buf (nelisp-ec-point-min)))
      (with-current-buffer buf
        (nelisp-ec-insert
         (concat
          ";; This buffer is for text that is not saved, and for\n"
          ";; Lisp evaluation.\n"
          ";;\n"
          ";; To create a file, visit it with C-x C-f and enter\n"
          ";; text in its buffer.\n\n"))))
    (setq nemacs-gtk--active-buffer-name "*scratch*")
    (setq nemacs-gtk--scroll-offset 0)
    (nemacs-gtk--sync-window-title)
    (setq nemacs-gtk--last-key-text "scratch-buffer")))

(defun nemacs-gtk-messages-buffer ()
  "M-x messages-buffer — switch to `*Messages*', create if missing.
Useful for reviewing past echo-area output (= the `--last-key-text'
log we mirror onto `*Messages*' via the existing `message' wiring)."
  (interactive)
  (get-buffer-create "*Messages*")
  (setq nemacs-gtk--active-buffer-name "*Messages*")
  (setq nemacs-gtk--scroll-offset 0)
  (nemacs-gtk--sync-window-title)
  (setq nemacs-gtk--last-key-text "messages-buffer"))


;;;; --- bundle Phase 2.BM (whitespace tools) -------------------------------

(defun nemacs-gtk-show-trailing-whitespace ()
  "M-x show-trailing-whitespace — toggle red overlay for trailing
whitespace at line ends in the active buffer.  Works with the
Phase 2.BL highlight infrastructure: each affected span gets a
red translucent rectangle on the next paint cycle."
  (interactive)
  (setq nemacs-gtk--show-trailing-whitespace
        (not nemacs-gtk--show-trailing-whitespace))
  (setq nemacs-gtk--last-key-text
        (format "show-trailing-whitespace: %s"
                (if nemacs-gtk--show-trailing-whitespace "on" "off"))))

(defun nemacs-gtk-font-lock-mode ()
  "M-x font-lock-mode — toggle elisp syntax tinting (= comments gray,
strings green).  MVP scope (Phase 2.BN); deeper highlighting
needs a glyph-color path the Rust paint pipeline doesn't yet have."
  (interactive)
  (setq nemacs-gtk--font-lock-mode
        (not nemacs-gtk--font-lock-mode))
  (setq nemacs-gtk--last-key-text
        (format "font-lock-mode: %s"
                (if nemacs-gtk--font-lock-mode "on" "off"))))

(defun nemacs-gtk-column-marker-mode ()
  "M-x column-marker-mode — toggle a pink vertical ruler at column
`--fill-column' (default 70).  Useful for visualising the wrap
column while writing; pairs naturally with `M-q' / fill-paragraph."
  (interactive)
  (setq nemacs-gtk--column-marker-mode
        (not nemacs-gtk--column-marker-mode))
  (setq nemacs-gtk--last-key-text
        (format "column-marker-mode: %s @ col %d"
                (if nemacs-gtk--column-marker-mode "on" "off")
                nemacs-gtk--fill-column)))

(defun nemacs-gtk-delete-trailing-whitespace ()
  "M-x delete-trailing-whitespace — strip all `\\s' / `\\t' chars
that immediately precede a `\\n' (or EOB) in the active buffer.
Iterates from the end so positions stay valid mid-walk."
  (interactive)
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((text (buffer-string))
           (tlen (length text))
           (i 0)
           (line-start 0)
           (deletes '()))
      (while (< i tlen)
        (let ((c (aref text i)))
          (when (or (eq c ?\n) (= i (1- tlen)))
            (let ((eol (cond ((eq c ?\n) i)
                             (t (1+ i)))))
              (let ((j eol))
                (while (and (> j line-start)
                            (memq (aref text (1- j)) '(?\s ?\t)))
                  (setq j (1- j)))
                (when (< j eol)
                  (push (cons (1+ j) (1+ eol)) deletes)))
              (setq line-start (1+ i)))))
        (setq i (1+ i)))
      (cond
       ((null deletes)
        (setq nemacs-gtk--last-key-text
              "delete-trailing-whitespace: nothing to delete"))
       (t
        (let ((count 0))
          (dolist (d deletes)
            (let ((s (car d)) (e (cdr d)))
              (nelisp-ec-delete-region s e)
              (setq count (+ count (- e s)))))
          (setq nemacs-gtk--last-key-text
                (format "delete-trailing-whitespace: %d chars / %d lines"
                        count (length deletes)))))))))


;;;; --- init file loading (Phase 3.C — ~/.emacs.d/init.el / ~/.emacs) ----

(defvar nemacs-gtk--init-file-loaded nil
  "Phase 3.C — non-nil after `--load-user-init-file' has either
loaded a user init file successfully or determined that no such
file exists.  Prevents accidental double-load.")

(defvar nemacs-gtk--init-file-error nil
  "Phase 3.C — when an error occurred loading the user init file,
this is the (FILE . ERROR-STRING) pair.  Otherwise nil.  Surfaced
on the echo-area row + appended to `*Messages*' so the user can
diagnose without leaving the GUI.")

(defun nemacs-gtk--candidate-init-files ()
  "Return the list of paths the GUI checks for a user init file,
in priority order.  Mirrors real Emacs's `user-init-file' search
(= `~/.emacs.d/init.el', then `~/.emacs.el', then `~/.emacs')."
  (let ((home (or (and (fboundp 'getenv) (getenv "HOME")) "~")))
    (list (concat home "/.emacs.d/init.el")
          (concat home "/.emacs.el")
          (concat home "/.emacs"))))

(defun nemacs-gtk--load-user-init-file ()
  "Phase 3.C — find + load the user's init file.  Errors are caught
+ recorded on `--init-file-error' so a broken init doesn't kill
the boot; the GUI still comes up at the welcome buffer with an
echo-line diagnostic.  No-op when `--init-file-loaded' is already t."
  (cond
   (nemacs-gtk--init-file-loaded
    (setq nemacs-gtk--last-key-text "init: already loaded"))
   (t
    (let ((found nil))
      (catch 'done
        (dolist (path (nemacs-gtk--candidate-init-files))
          (when (and (fboundp 'file-exists-p) (file-exists-p path))
            (setq found path)
            (throw 'done t))))
      (cond
       ((null found)
        (setq nemacs-gtk--init-file-loaded t)
        (setq nemacs-gtk--last-key-text "init: no user init file found"))
       (t
        (condition-case err
            (progn
              (load found)
              (setq nemacs-gtk--init-file-loaded t)
              (setq nemacs-gtk--last-key-text
                    (format "Loaded %s" found)))
          (error
           (setq nemacs-gtk--init-file-loaded t)
           (setq nemacs-gtk--init-file-error
                 (cons found (error-message-string err)))
           (setq nemacs-gtk--last-key-text
                 (format "init error: %s — %s"
                         found (error-message-string err)))
           ;; Mirror to *Messages* so the user can pull it back later.
           (when (fboundp 'get-buffer-create)
             (with-current-buffer (get-buffer-create "*Messages*")
               (when (fboundp 'goto-char)
                 (goto-char (or (and (fboundp 'point-max) (point-max)) 0)))
               (when (fboundp 'insert)
                 (insert (format "init error: %s — %s\n"
                                 found
                                 (error-message-string err)))))))))))
    nemacs-gtk--init-file-loaded)))

(defun nemacs-gtk-load-user-init-file ()
  "M-x load-user-init-file — re-run init file loading.  Useful for
testing init.el changes without restarting; the variable
`--init-file-loaded' is reset first so the search re-runs."
  (interactive)
  (setq nemacs-gtk--init-file-loaded nil)
  (setq nemacs-gtk--init-file-error nil)
  (nemacs-gtk--load-user-init-file))


;;;; --- major-mode plumbing (Phase 3.A — per-buffer mode tracking) -------

(defvar nemacs-gtk--buffer-modes nil
  "Phase 3.A — alist `(BUFFER-NAME . MODE-SYMBOL)' tracking each
buffer's major mode in the GTK GUI.  Substrate's `major-mode' defvar
is global (= one slot for all buffers); this side-table gives us
per-buffer dispatch without rewriting the substrate.")

(defun nemacs-gtk--buffer-mode (&optional bn)
  "Return the major-mode symbol for buffer BN (default = active),
falling back to `fundamental-mode' when no entry is recorded."
  (or (cdr (assoc (or bn nemacs-gtk--active-buffer-name)
                  nemacs-gtk--buffer-modes))
      'fundamental-mode))

(defun nemacs-gtk--set-buffer-mode (bn mode)
  "Record buffer BN's major-mode as MODE in `--buffer-modes'.
Mirrors to the substrate's `major-mode' defvar when BN is the
active buffer.  The substrate's mode functions (= `fundamental-mode',
`text-mode', `emacs-lisp-mode') already set `mode-name' themselves
so we don't duplicate that here."
  (setq nemacs-gtk--buffer-modes
        (cons (cons bn mode)
              (assoc-delete-all bn nemacs-gtk--buffer-modes)))
  (when (string= bn nemacs-gtk--active-buffer-name)
    (when (boundp 'major-mode) (setq major-mode mode))))

(defun nemacs-gtk--mode-for-path (path)
  "Walk `auto-mode-alist' for PATH and return the matching mode
symbol or nil.  Honors first-match-wins ordering."
  (when (and path (boundp 'auto-mode-alist))
    (catch 'found
      (dolist (cell auto-mode-alist)
        (when (and (consp cell) (stringp (car cell))
                   (string-match (car cell) path))
          (throw 'found (cdr cell))))
      nil)))

(defun nemacs-gtk--apply-mode-for-buffer (bn)
  "Invoke `set-auto-mode' on BN — pick a mode from `auto-mode-alist'
based on its visited filename + activate it.  No-op when BN has no
visited file or no rule matches."
  (let* ((buf (get-buffer bn))
         (path (and buf (fboundp 'buffer-file-name)
                    (with-current-buffer buf (buffer-file-name))))
         (mode (and path (nemacs-gtk--mode-for-path path))))
    (when (and mode (fboundp mode))
      (funcall mode)
      (nemacs-gtk--set-buffer-mode bn mode))))

(defun nemacs-gtk--seed-auto-mode-alist ()
  "Phase 3.A — seed `auto-mode-alist' with a sensible default set
of (REGEX . MODE-SYMBOL) entries.  Idempotent — already-present
entries don't get duplicated."
  (when (boundp 'auto-mode-alist)
    (dolist (entry '(("\\.el\\'"        . emacs-lisp-mode)
                     ("\\.txt\\'"       . text-mode)
                     ("\\.org\\'"       . text-mode)
                     ("\\.md\\'"        . text-mode)
                     ("\\.markdown\\'"  . text-mode)
                     ("\\.lisp\\'"      . emacs-lisp-mode)
                     ("\\.scm\\'"       . emacs-lisp-mode)))
      (unless (assoc (car entry) auto-mode-alist)
        (push entry auto-mode-alist)))))

(defun nemacs-gtk-set-major-mode-prompt ()
  "M-x set-major-mode — prompt for a mode symbol + activate it on
the current buffer.  Useful for buffers without a visited file
(= `*scratch*' starts in fundamental-mode but the user wants
emacs-lisp-mode for elisp evaluation)."
  (interactive)
  (nemacs-gtk--enter-minibuffer
   (format "Set major-mode (current %s): "
           (symbol-name (nemacs-gtk--buffer-mode)))
   (lambda (input)
     (cond
      ((or (null input) (= (length input) 0))
       (setq nemacs-gtk--last-key-text "set-major-mode: empty"))
      (t
       (let ((sym (intern input)))
         (cond
          ((not (fboundp sym))
           (setq nemacs-gtk--last-key-text
                 (format "set-major-mode: %s not fboundp" input)))
          (t
           (funcall sym)
           (nemacs-gtk--set-buffer-mode
            nemacs-gtk--active-buffer-name sym)
           (setq nemacs-gtk--last-key-text
                 (format "Mode: %s" input))))))))
   (lambda (input)
     (let ((acc '()))
       (dolist (cand '("fundamental-mode" "text-mode" "emacs-lisp-mode"))
         (when (string-prefix-p input cand)
           (push cand acc)))
       (sort acc #'string<)))))


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
    "backward-paragraph"
    "backward-word"
    "beginning-of-line"
    "capitalize-word"
    "copy-region"
    "delete-backward-char"
    "delete-char"
    "delete-horizontal-space"
    "downcase-word"
    "end-of-line"
    "execute-extended-command"
    "find-file"
    "forward-char"
    "forward-paragraph"
    "forward-word"
    "goto-line"
    "isearch-backward"
    "isearch-forward"
    "just-one-space"
    "count-words-region"
    "kill-buffer"
    "kill-line"
    "kill-region"
    "kill-whole-line"
    "keyboard-quit"
    "mark-whole-buffer"
    "newline"
    "nemacs-gtk-backward-paragraph"
    "nemacs-gtk-buffer-menu"
    "nemacs-gtk-call-last-kbd-macro"
    "nemacs-gtk-capitalize-word"
    "nemacs-gtk-copy-region"
    "nemacs-gtk-count-words-region"
    "nemacs-gtk-delete-horizontal-space"
    "nemacs-gtk-downcase-word"
    "nemacs-gtk-forward-paragraph"
    "nemacs-gtk-goto-line"
    "nemacs-gtk-isearch-backward"
    "nemacs-gtk-isearch-forward"
    "nemacs-gtk-just-one-space"
    "nemacs-gtk-keyboard-find-file"
    "nemacs-gtk-keyboard-quit"
    "nemacs-gtk-keyboard-save"
    "nemacs-gtk-kill-buffer"
    "nemacs-gtk-kill-region"
    "nemacs-gtk-kill-whole-line"
    "nemacs-gtk-mark-whole-buffer"
    "nemacs-gtk-meta-beginning-of-buffer"
    "nemacs-gtk-meta-end-of-buffer"
    "nemacs-gtk-meta-kill-word"
    "nemacs-gtk-mouse-set-point"
    "nemacs-gtk-mouse-yank-primary"
    "nemacs-gtk-other-window"
    "nemacs-gtk-page-down"
    "nemacs-gtk-page-up"
    "nemacs-gtk-comment-dwim"
    "nemacs-gtk-dabbrev-expand"
    "nemacs-gtk-delete-indentation"
    "nemacs-gtk-delete-other-windows"
    "nemacs-gtk-delete-window"
    "nemacs-gtk-describe-bindings"
    "nemacs-gtk-enlarge-window"
    "nemacs-gtk-describe-key"
    "nemacs-gtk-end-kbd-macro"
    "nemacs-gtk-eval-expression"
    "nemacs-gtk-exchange-point-and-mark"
    "nemacs-gtk-fill-paragraph"
    "nemacs-gtk-mark-paragraph"
    "nemacs-gtk-overwrite-mode"
    "nemacs-gtk-query-replace"
    "nemacs-gtk-quoted-insert"
    "nemacs-gtk-recenter"
    "nemacs-gtk-save-some-buffers"
    "nemacs-gtk-shell-command"
    "nemacs-gtk-shell-command-on-region"
    "nemacs-gtk-shrink-window"
    "nemacs-gtk-sort-lines"
    "nemacs-gtk-split-window-below"
    "nemacs-gtk-split-window-right"
    "nemacs-gtk-start-kbd-macro"
    "nemacs-gtk-tab-to-tab-stop"
    "nemacs-gtk-toggle-read-only"
    "nemacs-gtk-what-cursor-position"
    "nemacs-gtk-undo"
    "nemacs-gtk-write-file"
    "nemacs-gtk-save-buffers-kill-emacs"
    "nemacs-gtk-set-mark-command"
    "nemacs-gtk-switch-to-buffer"
    "nemacs-gtk-transpose-chars"
    "nemacs-gtk-upcase-word"
    "nemacs-gtk-yank-pop"
    "nemacs-gtk-zap-to-char"
    "next-line"
    "previous-line"
    "save-buffer"
    "save-buffers-kill-emacs"
    "self-insert-command"
    "set-mark-command"
    "switch-to-buffer"
    "transpose-chars"
    "upcase-word"
    "what-line"
    "yank"
    "yank-pop"
    "zap-to-char"
    "quoted-insert"
    "undo"
    "recenter"
    "overwrite-mode"
    "describe-key"
    "describe-bindings"
    "mark-paragraph"
    "query-replace"
    "comment-dwim"
    "exchange-point-and-mark"
    "tab-to-tab-stop"
    "fill-paragraph"
    "delete-indentation"
    "write-file"
    "save-some-buffers"
    "dabbrev-expand"
    "start-kbd-macro"
    "end-kbd-macro"
    "call-last-kbd-macro"
    "toggle-read-only"
    "sort-lines"
    "eval-expression"
    "shell-command"
    "shell-command-on-region"
    "what-cursor-position"
    "split-window-below"
    "split-window-right"
    "delete-window"
    "delete-other-windows"
    "other-window"
    "enlarge-window"
    "shrink-window"
    "nemacs-gtk-copy-to-register"
    "nemacs-gtk-insert-register"
    "nemacs-gtk-point-to-register"
    "nemacs-gtk-jump-to-register"
    "copy-to-register"
    "insert-register"
    "point-to-register"
    "jump-to-register"
    "nemacs-gtk-forward-sexp"
    "nemacs-gtk-backward-sexp"
    "nemacs-gtk-kill-sexp"
    "forward-sexp"
    "backward-sexp"
    "kill-sexp"
    "nemacs-gtk-bookmark-set"
    "nemacs-gtk-bookmark-jump"
    "nemacs-gtk-bookmark-list"
    "bookmark-set"
    "bookmark-jump"
    "bookmark-list"
    "nemacs-gtk-forward-sentence"
    "nemacs-gtk-backward-sentence"
    "nemacs-gtk-mark-defun"
    "forward-sentence"
    "backward-sentence"
    "mark-defun"
    "nemacs-gtk-delete-blank-lines"
    "nemacs-gtk-kill-sentence"
    "nemacs-gtk-set-fill-column"
    "delete-blank-lines"
    "kill-sentence"
    "set-fill-column"
    "nemacs-gtk-narrow-to-region"
    "nemacs-gtk-narrow-to-defun"
    "nemacs-gtk-widen"
    "narrow-to-region"
    "narrow-to-defun"
    "widen"
    "nemacs-gtk-describe-function"
    "nemacs-gtk-describe-variable"
    "nemacs-gtk-apropos"
    "describe-function"
    "describe-variable"
    "apropos"
    "nemacs-gtk-mark-sexp"
    "nemacs-gtk-move-to-window-line-top-bottom"
    "nemacs-gtk-backward-kill-sentence"
    "nemacs-gtk-repeat"
    "mark-sexp"
    "move-to-window-line-top-bottom"
    "backward-kill-sentence"
    "repeat"
    "nemacs-gtk-text-scale-increase"
    "nemacs-gtk-text-scale-decrease"
    "nemacs-gtk-text-scale-reset"
    "text-scale-increase"
    "text-scale-decrease"
    "text-scale-reset"
    "nemacs-gtk-list-directory"
    "nemacs-gtk-find-alternate-file"
    "nemacs-gtk-version"
    "list-directory"
    "find-alternate-file"
    "version"
    "nemacs-gtk-electric-pair-mode"
    "nemacs-gtk-transient-mark-mode"
    "nemacs-gtk-iconify-frame"
    "electric-pair-mode"
    "transient-mark-mode"
    "iconify-frame"
    "nemacs-gtk-upcase-region"
    "nemacs-gtk-downcase-region"
    "nemacs-gtk-capitalize-region"
    "nemacs-gtk-kill-this-buffer"
    "nemacs-gtk-quit-window"
    "upcase-region"
    "downcase-region"
    "capitalize-region"
    "kill-this-buffer"
    "quit-window"
    "nemacs-gtk-cheat-sheet"
    "nemacs-gtk-scratch-buffer"
    "nemacs-gtk-messages-buffer"
    "cheat-sheet"
    "scratch-buffer"
    "messages-buffer"
    "nemacs-gtk-show-trailing-whitespace"
    "nemacs-gtk-delete-trailing-whitespace"
    "nemacs-gtk-font-lock-mode"
    "nemacs-gtk-column-marker-mode"
    "show-trailing-whitespace"
    "delete-trailing-whitespace"
    "font-lock-mode"
    "column-marker-mode"
    "nemacs-gtk-set-major-mode-prompt"
    "set-major-mode"
    "fundamental-mode"
    "text-mode"
    "emacs-lisp-mode"
    "nemacs-gtk-load-user-init-file"
    "load-user-init-file"
    "nemacs-gtk-paint-extras-mode"
    "paint-extras-mode")
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
  "Create / reset the `*welcome*' buffer + drop the cursor at end.
Phase 2.BK: doubles as the cheat-sheet renderer — `M-x cheat-sheet'
re-runs this fn to refresh the content with the latest key bindings."
  (let ((buf (or (get-buffer "*welcome*")
                 (generate-new-buffer "*welcome*"))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Welcome to nemacs-gtk (= nelisp-emacs, Doc 51 Track D + GTK Layer 3)\n")
      (insert "===============================================================\n\n")
      (insert "Quick reference — Phase 2 key bindings:\n\n")
      (insert "Files\n")
      (insert "  C-x C-f   find-file              C-x C-s   save-buffer\n")
      (insert "  C-x C-w   write-file             C-x C-v   find-alternate-file\n")
      (insert "  C-x s     save-some-buffers      C-x C-d   list-directory\n")
      (insert "Buffers\n")
      (insert "  C-x b     switch-to-buffer       C-x C-b   buffer-menu\n")
      (insert "  C-x k     kill-buffer\n")
      (insert "Motion\n")
      (insert "  C-f/b     forward/back-char      C-n/p     next/prev-line\n")
      (insert "  M-f/b     forward/back-word      C-a/e     beg/end-of-line\n")
      (insert "  M-</>     beg/end-of-buffer      C-v/M-v   page-down/up\n")
      (insert "  M-{/}     back/forward-paragraph M-a/e     back/forward-sentence\n")
      (insert "  C-M-f/b   forward/backward-sexp  M-g g     goto-line\n")
      (insert "  M-r       cycle top/middle/bot   C-l       recenter\n")
      (insert "Edit\n")
      (insert "  C-d       delete-char            DEL       delete-backward-char\n")
      (insert "  C-k       kill-line              M-k       kill-sentence\n")
      (insert "  C-w/M-w   kill/copy-region       C-y       yank   M-y yank-pop\n")
      (insert "  C-x u/C-/ undo                   C-q       quoted-insert\n")
      (insert "  M-d       kill-word              C-x DEL   back-kill-sentence\n")
      (insert "  C-M-k     kill-sexp              C-x C-o   delete-blank-lines\n")
      (insert "  M-u/l/c   upcase/down/cap-word   C-x C-u/l upcase/down-region\n")
      (insert "  C-x +/-   text-scale-incr/decr   C-x z     repeat\n")
      (insert "Search / Replace\n")
      (insert "  C-s/r     isearch fwd/back       M-%       query-replace\n")
      (insert "  M-/       dabbrev-expand         M-z       zap-to-char\n")
      (insert "Mark / Region\n")
      (insert "  C-SPC     set-mark               C-x C-x   exchange-pt-and-mark\n")
      (insert "  M-h       mark-paragraph         C-M-h     mark-defun\n")
      (insert "  C-M-SPC   mark-sexp              C-x h     mark-whole-buffer\n")
      (insert "Windows / Narrowing\n")
      (insert "  C-x 2/3   split-below/right      C-x 0/1   delete/keep-window\n")
      (insert "  C-x o     other-window           C-x ^     enlarge-window\n")
      (insert "  C-x n n/w narrow-region/widen    C-x n d   narrow-to-defun\n")
      (insert "Registers / Bookmarks\n")
      (insert "  C-x r s/i copy/insert-register   C-x r SPC/j point/jump\n")
      (insert "  C-x r m/b bookmark-set/jump      C-x r l   bookmark-list\n")
      (insert "Help\n")
      (insert "  C-h k     describe-key           C-h b     describe-bindings\n")
      (insert "  C-h f/v   describe-fn/variable   C-h a     apropos\n")
      (insert "Modes / Quit\n")
      (insert "  M-x electric-pair-mode  M-x transient-mark-mode\n")
      (insert "  M-x overwrite-mode      M-x kill-this-buffer  C-z iconify\n")
      (insert "  C-x C-c   save-buffers-kill-emacs           M-x version\n")
      (insert "\nMouse: click sets point; drag selects; right-click context menu.\n"))
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
           ;; Phase 3.A — read the GUI's per-buffer mode table first;
           ;; substrate's `major-mode' is global and only reflects the
           ;; LATEST mode change, not the active-buffer's mode.
           (mode (symbol-name (nemacs-gtk--buffer-mode)))
           (pos  (nemacs-gtk--scroll-position-label))
           (mod-flag (cond
                      ((and (boundp 'buffer-read-only) buffer-read-only)
                       "%%")
                      ((nemacs-gtk--buffer-modified-p (current-buffer))
                       "**")
                      (t "--")))
           (body (format "-U:%s-  %s    L%d   %s   (%s) "
                         mod-flag name line pos mode))
           (pad (- nemacs-gtk--cols (length body))))
      (if (> pad 0)
          (concat body (make-string pad ?-))
        (substring body 0 nemacs-gtk--cols)))))

(defun nemacs-gtk--pad-or-trunc-to (s width)
  "Return S truncated to WIDTH chars, or padded with spaces."
  (cond
   ((>= (length s) width) (substring s 0 width))
   (t (concat s (make-string (- width (length s)) ?\s)))))

(defun nemacs-gtk--paint-single-window-area (top rows scroll bn
                                                  &optional left-col cols)
  "Paint a window of buffer named BN into the rectangle at (TOP,
LEFT-COL) of size ROWS x COLS, with SCROLL as the buffer's first
visible line.  Last row of the band is left for an inline mode-line.
When LEFT-COL / COLS are nil, fall back to full-width paint via
`nelisp-gtk-grid-put-row' (= preserves Phase 2.AU horizontal-only
behaviour)."
  (let* ((buf (or (get-buffer bn) (get-buffer "*welcome*")))
         (content (with-current-buffer buf (buffer-string)))
         (lines (split-string content "\n"))
         (content-rows (max 0 (- rows 1)))
         (full-width-p (or (null left-col) (null cols)))
         (use-cols (or cols nemacs-gtk--cols))
         (i 0))
    (while (< i content-rows)
      (let* ((raw-line (or (nth (+ i scroll) lines) ""))
             (cell (nemacs-gtk--pad-or-trunc-to raw-line use-cols)))
        (cond
         (full-width-p
          (nelisp-gtk-grid-put-row (+ top i) cell))
         (t
          (nelisp-gtk-grid-put-substr (+ top i) left-col cell))))
      (setq i (1+ i)))))

(defun nemacs-gtk--inline-mode-line-text (bn &optional cols)
  "Mode-line text for the inline divider of a window showing buffer
named BN.  COLS = total width to pad to (defaults to grid width).
Body = `-:FLAG-  NAME    ' padded with `-' to COLS."
  (let* ((buf (or (get-buffer bn) (get-buffer "*welcome*")))
         (modp (with-current-buffer buf
                 (or (and (boundp 'buffer-read-only) buffer-read-only
                          "%%")
                     (and (nemacs-gtk--buffer-modified-p buf) "**")
                     "--")))
         (body (format "-:%s-  %s    " modp bn))
         (use-cols (or cols nemacs-gtk--cols))
         (pad (- use-cols (length body))))
    (cond
     ((> pad 0) (concat body (make-string pad ?-)))
     (t (substring body 0 use-cols)))))

(defun nemacs-gtk--paint-buffer-area ()
  "Phase 2.AU — multi-window aware.  When `--windows' is nil, fall
back to legacy single-window paint.  Otherwise iterate windows,
painting each in its row band + inline mode-line at the bottom of
all but the last window (= last window's mode-line is drawn by the
frame's `--paint-mode-line')."
  (cond
   ((null nemacs-gtk--windows)
    (nemacs-gtk--paint-single-window-area
     0 nemacs-gtk--buffer-area-end
     nemacs-gtk--scroll-offset
     nemacs-gtk--active-buffer-name)
    ;; legacy: when single-window we paint the FULL buffer-area-end rows
    ;; without reserving a mode-line row; --paint-mode-line writes to
    ;; mode-line-row anyway.  Re-do the last row from the active buffer.
    (let* ((buf (nemacs-gtk--active-buffer))
           (content (with-current-buffer buf (buffer-string)))
           (lines (split-string content "\n"))
           (last-i (1- nemacs-gtk--buffer-area-end))
           (line (or (nth (+ last-i nemacs-gtk--scroll-offset) lines) "")))
      (nelisp-gtk-grid-put-row last-i
                               (nemacs-gtk--truncate line nemacs-gtk--cols))))
   (t
    ;; sync current globals into the slot first so paint sees latest state.
    (nemacs-gtk--sync-current-to-window)
    (let ((wins nemacs-gtk--windows)
          (n (length nemacs-gtk--windows))
          (i 0))
      (while (< i n)
        (let* ((w (nth i wins))
               (top (plist-get w :top-row))
               (rows (plist-get w :rows))
               (scroll (or (plist-get w :scroll) 0))
               (bn (plist-get w :buffer))
               (left (plist-get w :left-col))
               (cols (plist-get w :cols)))
          ;; All windows reserve their bottom row for an inline mode-line
          ;; (= matches Emacs convention).  The frame's `--paint-mode-line'
          ;; at row `--mode-line-row' (below the buffer area) shows the
          ;; CURRENT window's full mode-line as a global indicator.
          (nemacs-gtk--paint-single-window-area top rows scroll bn left cols)
          (let ((mode-row (+ top rows -1)))
            (cond
             ((or (null left) (null cols)
                  (= cols nemacs-gtk--cols))
              (nelisp-gtk-grid-put-row
               mode-row (nemacs-gtk--inline-mode-line-text bn cols)))
             (t
              (nelisp-gtk-grid-put-substr
               mode-row left
               (nemacs-gtk--inline-mode-line-text bn cols))))))
        (setq i (1+ i)))))))

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
within the current window's content rows.  In multi-window mode the
window's `:rows' (minus the inline mode-line row) is used; in
single-window mode the full `--buffer-area-end' is used."
  (let* ((buf-row (nemacs-gtk--point-to-buf-row))
         (visible-rows
          (cond
           ((null nemacs-gtk--windows) nemacs-gtk--buffer-area-end)
           (t (max 1 (- (nemacs-gtk--current-window-rows) 1))))))
    (cond
     ((< buf-row nemacs-gtk--scroll-offset)
      (setq nemacs-gtk--scroll-offset buf-row))
     ((>= buf-row (+ nemacs-gtk--scroll-offset visible-rows))
      (setq nemacs-gtk--scroll-offset
            (1+ (- buf-row visible-rows)))))
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

(defun nemacs-gtk--window-at-cell (row col)
  "Return the index in `--windows' that contains grid (ROW, COL), or
nil when single-window mode or the cell is on a mode-line row.
Phase 2.AW: also checks `:left-col' / `:cols' so vertical splits
route correctly."
  (cond
   ((null nemacs-gtk--windows) nil)
   (t
    (let ((i 0)
          (n (length nemacs-gtk--windows))
          (hit nil))
      (while (and (< i n) (null hit))
        (let* ((w (nth i nemacs-gtk--windows))
               (top (plist-get w :top-row))
               (rows (plist-get w :rows))
               (left (or (plist-get w :left-col) 0))
               (wcols (or (plist-get w :cols) nemacs-gtk--cols))
               (content-rows (max 0 (- rows 1))))
          (when (and (>= row top) (< row (+ top content-rows))
                     (>= col left) (< col (+ left wcols)))
            (setq hit i)))
        (setq i (1+ i)))
      hit))))

(defun nemacs-gtk--cell-to-point (row col)
  "Inverse of `nemacs-gtk--cursor-row-col': map a grid cell (ROW, COL)
back to a buffer position in the active buffer.  ROW is the
on-screen row.  Phase 2.AU: in multi-window mode, the row is first
mapped to the current window's `:top-row' offset before computing
the buffer-row.

When the target cell is past EOB / past the line's length, the
result clamps to the last reachable position in the buffer text
(= mouse-1 on the empty area below content puts point at EOB,
which is what users expect).  Always returns a 1-based point."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((top (nemacs-gtk--current-window-top))
           (left (cond
                  ((null nemacs-gtk--windows) 0)
                  (t (or (plist-get
                          (nth nemacs-gtk--current-window-idx
                               nemacs-gtk--windows)
                          :left-col)
                         0))))
           (row-in-window (- row top))
           (col-in-window (max 0 (- col left)))
           (target-row (+ row-in-window nemacs-gtk--scroll-offset))
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
        (while (and (< i len) (< col-i col-in-window)
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
      (let* ((screen-row-in-window (- buf-row nemacs-gtk--scroll-offset))
             (top (nemacs-gtk--current-window-top))
             (rows (nemacs-gtk--current-window-rows))
             (left (cond
                    ((null nemacs-gtk--windows) 0)
                    (t (or (plist-get
                            (nth nemacs-gtk--current-window-idx
                                 nemacs-gtk--windows)
                            :left-col)
                           0))))
             (window-cols (cond
                           ((null nemacs-gtk--windows) nemacs-gtk--cols)
                           (t (or (plist-get
                                   (nth nemacs-gtk--current-window-idx
                                        nemacs-gtk--windows)
                                   :cols)
                                  nemacs-gtk--cols))))
             (max-content-rows
              (cond
               ((null nemacs-gtk--windows) rows)
               (t (max 0 (- rows 1)))))
             (screen-row (+ top screen-row-in-window))
             (screen-col (+ left col)))
        (if (and (>= screen-row-in-window 0)
                 (< screen-row-in-window max-content-rows)
                 (< col window-cols))
            (cons screen-row screen-col)
          nil)))))

(defun nemacs-gtk--pos-to-row-col (pos)
  "Phase 2.BH — return the on-screen (ROW . COL) of buffer POS in the
active window, or nil if POS is outside the viewport.  Mirrors
`--cursor-row-col' but takes an explicit position argument so the
region overlay can resolve mark + point on the same frame."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((target (max 0 (1- pos)))
           (buf-row 0) (col 0)
           (i 0)
           (text (buffer-string))
           (len (length text))
           (clamped (min target len)))
      (while (< i clamped)
        (let ((c (aref text i)))
          (if (eq c ?\n)
              (setq buf-row (1+ buf-row) col 0)
            (setq col (1+ col))))
        (setq i (1+ i)))
      (let* ((screen-row-in-window (- buf-row nemacs-gtk--scroll-offset))
             (top (nemacs-gtk--current-window-top))
             (rows (nemacs-gtk--current-window-rows))
             (left (cond
                    ((null nemacs-gtk--windows) 0)
                    (t (or (plist-get
                            (nth nemacs-gtk--current-window-idx
                                 nemacs-gtk--windows)
                            :left-col)
                           0))))
             (window-cols (cond
                           ((null nemacs-gtk--windows) nemacs-gtk--cols)
                           (t (or (plist-get
                                   (nth nemacs-gtk--current-window-idx
                                        nemacs-gtk--windows)
                                   :cols)
                                  nemacs-gtk--cols))))
             (max-content-rows
              (cond
               ((null nemacs-gtk--windows) rows)
               (t (max 0 (- rows 1)))))
             (screen-row (+ top screen-row-in-window))
             (screen-col (+ left (min col window-cols))))
        (if (and (>= screen-row-in-window 0)
                 (< screen-row-in-window max-content-rows))
            (cons screen-row screen-col)
          nil)))))

(defun nemacs-gtk--paint-region-overlay ()
  "Phase 2.BH — push the [mark .. point] cell range to the Rust side
via `nelisp-gtk-set-region', or 0/0/0/0 to clear when no mark is
active.  Phase 2.BI: also honors `--transient-mark-mode' — when
off, always clears the overlay regardless of mark state.  No-op
when the extern isn't loaded (= substrate-only boot)."
  (when (fboundp 'nelisp-gtk-set-region)
    (cond
     ((not nemacs-gtk--transient-mark-mode)
      (nelisp-gtk-set-region 0 0 0 0))
     ((or (null nemacs-gtk--mark-pos)
          (null nemacs-gtk--mark-buffer)
          (not (eq nemacs-gtk--mark-buffer (nemacs-gtk--active-buffer))))
      (nelisp-gtk-set-region 0 0 0 0))
     (t
      (let* ((p (with-current-buffer (nemacs-gtk--active-buffer)
                  (nelisp-ec-point)))
             (m nemacs-gtk--mark-pos)
             (start (min p m))
             (end (max p m)))
        (cond
         ((= start end)
          (nelisp-gtk-set-region 0 0 0 0))
         (t
          (let ((rc-s (nemacs-gtk--pos-to-row-col start))
                (rc-e (nemacs-gtk--pos-to-row-col end)))
            (cond
             ((and rc-s rc-e)
              (nelisp-gtk-set-region (car rc-s) (cdr rc-s)
                                     (car rc-e) (cdr rc-e)))
             (t (nelisp-gtk-set-region 0 0 0 0)))))))))))

(defun nemacs-gtk--collect-isearch-highlights ()
  "Phase 2.BL — return a list of `(SR SC ER EC R G B A)' entries for
each isearch match in the visible viewport, or nil when isearch
isn't active / the query is empty.  Yellow tint (rgb 255 230 80)
at 40% alpha — bright enough to spot, faint enough not to obscure."
  (when (and nemacs-gtk--isearch-active
             (stringp nemacs-gtk--isearch-query)
             (> (length nemacs-gtk--isearch-query) 0))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((needle nemacs-gtk--isearch-query)
             (nlen (length needle))
             (text (buffer-string))
             (tlen (length text))
             (i 0)
             (acc '()))
        (while (< i (- tlen nlen -1))
          (cond
           ((and (<= (+ i nlen) tlen)
                 (string= needle (substring text i (+ i nlen))))
            (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ i)))
                   (rc-e (nemacs-gtk--pos-to-row-col (1+ (+ i nlen)))))
              (when (and rc-s rc-e)
                (push (list (car rc-s) (cdr rc-s)
                            (car rc-e) (cdr rc-e)
                            255 230 80 102)
                      acc)))
            (setq i (+ i nlen)))
           (t (setq i (1+ i)))))
        (nreverse acc)))))

(defun nemacs-gtk--paren-match-pos ()
  "Phase 2.BL — when point is on (or just after) a paren, return the
buffer position of the matching paren in the same buffer, or nil.
Uses the existing Phase 2.AY scanners."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((p (nelisp-ec-point))
           (pmax (nelisp-ec-point-max))
           (pmin (nelisp-ec-point-min))
           (ch-after (and (< p pmax) (emacs-edit--char-at p)))
           (ch-before (and (> p pmin) (emacs-edit--char-at (1- p)))))
      (cond
       ;; Point on an opener — look for matching closer
       ((memq ch-after '(?\( ?\[ ?\{))
        (let ((saved p))
          (let ((res (nemacs-gtk--scan-sexp-forward pmax)))
            (nelisp-ec-goto-char saved)
            (and res (1- res)))))
       ;; Point just after a closer — look back for matching opener
       ((memq ch-before '(?\) ?\] ?\}))
        (let ((saved p))
          (let ((res (nemacs-gtk--scan-sexp-backward pmin)))
            (nelisp-ec-goto-char saved)
            res)))
       (t nil)))))

(defun nemacs-gtk--collect-paren-highlight ()
  "Phase 2.BL — return a 1-element highlight list for the matching
paren when point sits on or just past one, or nil otherwise.
Green tint (rgb 80 220 120) at 50% alpha."
  (let ((mp (nemacs-gtk--paren-match-pos)))
    (when mp
      (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ mp)))
             (rc-e (nemacs-gtk--pos-to-row-col (+ 2 mp))))
        (when (and rc-s rc-e)
          (list (list (car rc-s) (cdr rc-s)
                      (car rc-e) (cdr rc-e)
                      80 220 120 128)))))))

(defvar nemacs-gtk--show-trailing-whitespace nil
  "Phase 2.BM — when t, `--paint-highlights' adds red overlays for
trailing whitespace at the end of each line in the active buffer.
Toggle via M-x show-trailing-whitespace.")

(defvar nemacs-gtk--font-lock-mode nil
  "Phase 2.BN — when t, `--paint-highlights' adds tint overlays for
elisp syntax: `;'-comments get a gray tint, `\"..\"'-strings get a
green tint.  MVP scope (comments + strings); function names /
keywords / numbers are deferred — Cairo's per-cell rectangles can
only tint the background, so a deeper font-lock would need a
glyph-color extension to the Rust paint pipeline.")

(defvar nemacs-gtk--column-marker-mode nil
  "Phase 2.BO — when t, `--paint-highlights' draws a vertical
ruler at column `--fill-column' across the buffer area.  Toggle
via M-x column-marker-mode.")

(defun nemacs-gtk--collect-trailing-whitespace-highlights ()
  "Phase 2.BM — return a list of `(SR SC ER EC R G B A)' entries for
each line's trailing whitespace span (= the maximal run of `\\s'/`\\t'
ending at `\\n' or EOB).  Red tint (rgb 240 80 80) at 40% alpha."
  (when nemacs-gtk--show-trailing-whitespace
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((text (buffer-string))
             (tlen (length text))
             (i 0)
             (line-start 0)
             (acc '()))
        (while (< i tlen)
          (let ((c (aref text i)))
            (when (or (eq c ?\n) (= i (1- tlen)))
              ;; line ends at i (= newline) or i+1 (= EOB)
              (let ((eol (cond ((eq c ?\n) i)
                               (t (1+ i)))))
                ;; Walk back from eol over whitespace
                (let ((j eol))
                  (while (and (> j line-start)
                              (memq (aref text (1- j)) '(?\s ?\t)))
                    (setq j (1- j)))
                  (when (< j eol)
                    (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ j)))
                           (rc-e (nemacs-gtk--pos-to-row-col (1+ eol))))
                      (when (and rc-s rc-e)
                        (push (list (car rc-s) (cdr rc-s)
                                    (car rc-e) (cdr rc-e)
                                    240 80 80 102)
                              acc)))))
                (setq line-start (1+ i)))))
          (setq i (1+ i)))
        (nreverse acc)))))

(defconst nemacs-gtk--font-locked-modes
  '(emacs-lisp-mode lisp-interaction-mode lisp-mode)
  "Phase 3.A — modes for which font-lock auto-activates.  Other
modes need an explicit `M-x font-lock-mode' toggle to enable
the comment/string tinting.")

(defun nemacs-gtk--font-lock-active-p ()
  "Phase 3.B — t when font-lock should fire on the active buffer:
either `--font-lock-mode' is forced on, or the buffer's major
mode is in `--font-locked-modes'."
  (or nemacs-gtk--font-lock-mode
      (memq (nemacs-gtk--buffer-mode)
            nemacs-gtk--font-locked-modes)))

(defconst nemacs-gtk--lisp-keywords
  '("defun" "defmacro" "defvar" "defconst" "defcustom" "defalias"
    "defsubst" "let" "let*" "letrec" "lambda" "function"
    "if" "cond" "when" "unless" "while" "and" "or" "not"
    "progn" "prog1" "prog2" "setq" "setq-default" "setq-local" "set"
    "save-excursion" "save-restriction" "save-match-data"
    "with-current-buffer" "with-temp-buffer" "with-output-to-string"
    "catch" "throw" "condition-case" "unwind-protect" "ignore-errors"
    "dolist" "dotimes" "mapcar" "mapc" "require" "provide"
    "interactive" "declare" "use-package")
  "Phase 3.B — set of elisp form names that get a blue color span
when font-lock-mode is active and they appear after `(' / `(['.")

(defun nemacs-gtk--collect-font-lock-color-spans ()
  "Phase 3.B — return color spans for elisp syntax:
comments  → gray  (rgb 130 130 130) for the entire `;…\\n' region;
strings   → green (rgb 50 130 50) for `\"…\"' content;
keywords  → blue  (rgb 60 80 200) for known special forms when they
            appear immediately after `('.  Backslash escapes inside
strings are honoured."
  (when (nemacs-gtk--font-lock-active-p)
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((text (buffer-string))
             (tlen (length text))
             (i 0)
             (acc '())
             (kw-set nemacs-gtk--lisp-keywords))
        (while (< i tlen)
          (let ((c (aref text i)))
            (cond
             ;; Comment: `;' through `\n' or EOB → gray.
             ((eq c ?\;)
              (let ((s i))
                (while (and (< i tlen)
                            (not (eq (aref text i) ?\n)))
                  (setq i (1+ i)))
                (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ s)))
                       (rc-e (nemacs-gtk--pos-to-row-col (1+ i))))
                  (when (and rc-s rc-e)
                    (push (list (car rc-s) (cdr rc-s)
                                (car rc-e) (cdr rc-e)
                                130 130 130)
                          acc)))))
             ;; String: `"' through matching `"' → dark green.
             ((eq c ?\")
              (let ((s i))
                (setq i (1+ i))
                (while (and (< i tlen) (not (eq (aref text i) ?\")))
                  (when (eq (aref text i) ?\\)
                    (setq i (1+ i)))
                  (setq i (1+ i)))
                (when (< i tlen) (setq i (1+ i)))
                (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ s)))
                       (rc-e (nemacs-gtk--pos-to-row-col (1+ i))))
                  (when (and rc-s rc-e)
                    (push (list (car rc-s) (cdr rc-s)
                                (car rc-e) (cdr rc-e)
                                50 130 50)
                          acc)))))
             ;; Keyword: `(WORD' where WORD ∈ kw-set → blue.
             ((eq c ?\()
              (let ((s (1+ i))
                    (k (1+ i)))
                (while (and (< k tlen)
                            (let ((ch (aref text k)))
                              (or (and (>= ch ?a) (<= ch ?z))
                                  (and (>= ch ?A) (<= ch ?Z))
                                  (and (>= ch ?0) (<= ch ?9))
                                  (memq ch '(?- ?_ ?* ?:)))))
                  (setq k (1+ k)))
                (when (and (> k s)
                           (member (substring text s k) kw-set))
                  (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ s)))
                         (rc-e (nemacs-gtk--pos-to-row-col (1+ k))))
                    (when (and rc-s rc-e)
                      (push (list (car rc-s) (cdr rc-s)
                                  (car rc-e) (cdr rc-e)
                                  60 80 200)
                            acc))))
                (setq i k))
              (setq i (1+ i)))
             (t (setq i (1+ i))))))
        (nreverse acc)))))

(defun nemacs-gtk--paint-color-spans ()
  "Phase 3.B — push the active buffer's font-lock color spans to the
Rust side via `nelisp-gtk-set-color-spans', or an empty list to
clear when font-lock isn't active.  No-op when the extern isn't
loaded (= substrate-only boot)."
  (when (fboundp 'nelisp-gtk-set-color-spans)
    (nelisp-gtk-set-color-spans
     (or (nemacs-gtk--collect-font-lock-color-spans) nil))))

;; The legacy background-tint version — kept for users running on
;; older Rust backends that lack `set-color-spans'.  It's a no-op
;; when the new color-span path is active (= same trigger condition
;; turns it off below).
(defun nemacs-gtk--collect-font-lock-highlights ()
  "Phase 2.BN — legacy background-tint font-lock.  Suppressed when
the Rust side has the new `set-color-spans' extern (= Phase 3.B);
otherwise still emits gray + green tints for comments + strings."
  (when (and (not (fboundp 'nelisp-gtk-set-color-spans))
             (nemacs-gtk--font-lock-active-p))
    (with-current-buffer (nemacs-gtk--active-buffer)
      (let* ((text (buffer-string))
             (tlen (length text))
             (i 0)
             (acc '()))
        (while (< i tlen)
          (let ((c (aref text i)))
            (cond
             ;; Comment: `;' through `\n' or EOB.
             ((eq c ?\;)
              (let ((s i))
                (while (and (< i tlen)
                            (not (eq (aref text i) ?\n)))
                  (setq i (1+ i)))
                (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ s)))
                       (rc-e (nemacs-gtk--pos-to-row-col (1+ i))))
                  (when (and rc-s rc-e)
                    (push (list (car rc-s) (cdr rc-s)
                                (car rc-e) (cdr rc-e)
                                180 180 180 64)
                          acc)))))
             ;; String: `"' through matching `"'.  Honor `\\' escapes.
             ((eq c ?\")
              (let ((s i))
                (setq i (1+ i))
                (while (and (< i tlen) (not (eq (aref text i) ?\")))
                  (when (eq (aref text i) ?\\)
                    (setq i (1+ i)))
                  (setq i (1+ i)))
                (when (< i tlen) (setq i (1+ i)))
                (let* ((rc-s (nemacs-gtk--pos-to-row-col (1+ s)))
                       (rc-e (nemacs-gtk--pos-to-row-col (1+ i))))
                  (when (and rc-s rc-e)
                    (push (list (car rc-s) (cdr rc-s)
                                (car rc-e) (cdr rc-e)
                                140 220 140 64)
                          acc)))))
             (t (setq i (1+ i))))))
        (nreverse acc)))))

(defun nemacs-gtk--collect-column-marker-highlights ()
  "Phase 2.BO — return a 1-element highlight for the column ruler
when `--column-marker-mode' is on.  Renders as a thin pink line
(rgb 220 120 200) at 60% alpha spanning the buffer area's height
at column `--fill-column'."
  (when nemacs-gtk--column-marker-mode
    (let* ((col nemacs-gtk--fill-column)
           (top 0)
           (bot nemacs-gtk--buffer-area-end))
      (list (list top col bot (1+ col) 220 120 200 153)))))

(defun nemacs-gtk--paint-highlights ()
  "Phase 2.BL+BM+BN+BO — combine isearch matches, paren-match,
trailing whitespace, font-lock spans, and column ruler into a
single highlight list and push to the Rust side.  No-op when the
extern isn't loaded."
  (when (fboundp 'nelisp-gtk-set-highlights)
    (let ((isearch-hl   (nemacs-gtk--collect-isearch-highlights))
          (paren-hl     (nemacs-gtk--collect-paren-highlight))
          (ws-hl        (nemacs-gtk--collect-trailing-whitespace-highlights))
          (font-lock-hl (nemacs-gtk--collect-font-lock-highlights))
          (col-hl       (nemacs-gtk--collect-column-marker-highlights)))
      (nelisp-gtk-set-highlights
       ;; font-lock first (= least-priority background tint), then
       ;; column ruler, then whitespace, then paren-match, then
       ;; isearch on top.  Later entries paint over earlier ones.
       (append font-lock-hl col-hl ws-hl paren-hl isearch-hl)))))

(defvar nemacs-gtk--guard-seen nil
  "Phase 3.D — alist `(LABEL . MSG)' of guard errors already logged
this session.  Used to deduplicate `--guard' diagnostics so a
permanently-broken sub-paint doesn't spam `*Messages*' on every
frame; the echo-area still updates each time so the user sees
something is wrong.")

(defmacro nemacs-gtk--guard (label &rest body)
  "Phase 3.D — run BODY catching any error + reporting to the
echo-area row + `*Messages*' buffer (= once per LABEL+MSG pair,
to keep the log + frame rate bounded).  Returns nil on error so
the rest of the redraw can proceed."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (let* ((emsg (condition-case _ (error-message-string err)
                     (error (format "%S" err))))
             (msg  (format "[%s] %s" ,label emsg))
             (key  (cons ,label emsg))
             (new  (not (member key nemacs-gtk--guard-seen))))
        (setq nemacs-gtk--last-key-text msg)
        (when new
          (push key nemacs-gtk--guard-seen)
          (when (and (fboundp 'get-buffer-create)
                     (fboundp 'with-current-buffer))
            (condition-case _
                (with-current-buffer (get-buffer-create "*Messages*")
                  (when (fboundp 'goto-char)
                    (goto-char
                     (or (and (fboundp 'point-max) (point-max)) 1)))
                  (when (fboundp 'insert)
                    (insert msg "\n")))
              (error nil))))
        nil))))

(defvar nemacs-gtk--paint-extras-enabled nil
  "Phase 3.E — when t, `--repaint' runs the heavier extras (= region
overlay, all-match isearch highlights, paren-match, font-lock
color spans, trailing-whitespace, column ruler).  Default = nil
(= core paint only) because on resource-constrained VMs / VMware
Wayland sessions the extras can OOM the process.

Toggle via `M-x paint-extras-mode' to opt-in once the user is
sure the GUI is responsive enough for the extra work.")

(defun nemacs-gtk-paint-extras-mode ()
  "M-x paint-extras-mode — toggle `--paint-extras-enabled'.  When
on, isearch all-match / paren-match / font-lock color / trailing-
whitespace / column-ruler overlays render on each repaint.  When
off (= default), only the core grid + mode-line + echo-area + cursor
paint.  The latter is what the GUI runs in low-resource environments
to keep frame rate sane."
  (interactive)
  (setq nemacs-gtk--paint-extras-enabled
        (not nemacs-gtk--paint-extras-enabled))
  (setq nemacs-gtk--last-key-text
        (format "paint-extras-mode: %s"
                (if nemacs-gtk--paint-extras-enabled "on" "off"))))

(defun nemacs-gtk--repaint-fast-eligible-p ()
  "Phase 3.F — t when the fast-path Rust extern can serve this paint:
single-window mode, extras off, and the extern is loaded.  Multi-
window splits or extras-on fall back to the per-step elisp path."
  (and (fboundp 'nelisp-gtk-paint-frame-simple)
       (null nemacs-gtk--windows)
       (not nemacs-gtk--paint-extras-enabled)))

(defun nemacs-gtk--repaint-fast ()
  "Phase 3.F — one-shot Rust paint that bundles grid-clear + buffer
area + mode-line + echo + cursor + redraw into a single extern
call.  Skips ~50 elisp→Rust round-trips per frame, the
responsiveness fix for resource-constrained VMs."
  (let* ((buf (nemacs-gtk--active-buffer))
         (content (with-current-buffer buf (buffer-string)))
         (point (with-current-buffer buf (nelisp-ec-point)))
         (mode-line (nemacs-gtk--mode-line-text))
         (echo (cond
                (nemacs-gtk--minibuffer-active
                 (concat nemacs-gtk--minibuffer-prompt
                         nemacs-gtk--minibuffer-input
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
    (nelisp-gtk-paint-frame-simple
     nemacs-gtk--rows
     nemacs-gtk--cols
     nemacs-gtk--buffer-area-end
     nemacs-gtk--scroll-offset
     content
     point
     mode-line
     echo)))

(defun nemacs-gtk--repaint ()
  "One full redraw cycle: buffer, mode line, echo, cursor, queue draw.
Phase 3.D: each sub-step is guarded by `--guard' so a single
broken pass logs to *Messages* + the echo-area without aborting
the whole redraw.  Phase 3.E: heavy extras (= region overlay /
isearch all-match / paren-match / font-lock color spans /
trailing-whitespace / column ruler) are gated on
`--paint-extras-enabled' so the default boot stays fast on
resource-constrained VMs.  Phase 3.F: when extras are off + we're
single-window, take the all-in-one Rust fast path; the slow
elisp-loop path stays around for multi-window + extras-on."
  (cond
   ((nemacs-gtk--repaint-fast-eligible-p)
    (nemacs-gtk--guard "fast-paint" (nemacs-gtk--repaint-fast)))
   (t
    (nemacs-gtk--guard "grid-clear"  (nelisp-gtk-grid-clear))
    (nemacs-gtk--guard "buffer-area" (nemacs-gtk--paint-buffer-area))
    (nemacs-gtk--guard "mode-line"   (nemacs-gtk--paint-mode-line))
    (nemacs-gtk--guard "echo-area"   (nemacs-gtk--paint-echo-area))
    (when nemacs-gtk--paint-extras-enabled
      (nemacs-gtk--guard "region"      (nemacs-gtk--paint-region-overlay))
      (nemacs-gtk--guard "highlights"  (nemacs-gtk--paint-highlights))
      (nemacs-gtk--guard "color-spans" (nemacs-gtk--paint-color-spans)))
    (nemacs-gtk--guard "cursor"
      (let ((rc (nemacs-gtk--cursor-row-col)))
        (if rc
            (nelisp-gtk-set-cursor (car rc) (cdr rc))
          (nelisp-gtk-set-cursor nil nil))))
    (nemacs-gtk--guard "redraw"      (nelisp-gtk-redraw)))))


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

(defconst nemacs-gtk--read-only-blocked-commands
  '(self-insert-command
    newline
    yank
    nemacs-gtk-yank-pop
    kill-line
    nemacs-gtk-kill-region
    nemacs-gtk-meta-kill-word
    nemacs-gtk-just-one-space
    nemacs-gtk-delete-horizontal-space
    nemacs-gtk-kill-whole-line
    nemacs-gtk-transpose-chars
    nemacs-gtk-zap-to-char
    nemacs-gtk-comment-dwim
    nemacs-gtk-fill-paragraph
    nemacs-gtk-delete-indentation
    nemacs-gtk-tab-to-tab-stop
    nemacs-gtk-quoted-insert
    nemacs-gtk-undo
    nemacs-gtk-overwrite-mode
    nemacs-gtk-upcase-word
    nemacs-gtk-downcase-word
    nemacs-gtk-capitalize-word
    nemacs-gtk-dabbrev-expand
    nemacs-gtk-query-replace
    nemacs-gtk-call-last-kbd-macro
    nemacs-gtk-sort-lines
    nemacs-gtk-kill-sexp
    nemacs-gtk-insert-register
    nemacs-gtk-delete-blank-lines
    nemacs-gtk-kill-sentence
    nemacs-gtk-backward-kill-sentence
    nemacs-gtk-upcase-region
    nemacs-gtk-downcase-region
    nemacs-gtk-capitalize-region
    nemacs-gtk-delete-trailing-whitespace
    delete-char
    delete-backward-char)
  "Phase 2.AQ: command symbols the dispatcher refuses to run when
the active buffer's `buffer-read-only' is set.  Cursor motion,
search, mode-flags, frame ops and the like flow through normally.")

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
       (nemacs-gtk--query-replace-pending-key
        ;; Phase 2.AK: y/n/!/q answer for the active query-replace.
        (setq nemacs-gtk--query-replace-pending-key nil)
        (nemacs-gtk--query-replace-handle-key event)
        (nemacs-gtk--ensure-cursor-visible))
       (nemacs-gtk--describe-key-pending
        ;; Phase 2.AJ: `C-h k' just fired — the next event is
        ;; consumed and resolved against the keymap, the binding
        ;; is reported instead of being run.
        (setq nemacs-gtk--describe-key-pending nil)
        (let* ((b (nemacs-gtk--lookup-key-vec event-vec))
               (label (nemacs-gtk--describe-key-vec event-vec)))
          (setq nemacs-gtk--last-key-text
                (cond
                 ((null b) (format "%s is unbound" label))
                 ((symbolp b) (format "%s runs %s" label (symbol-name b)))
                 ((nemacs-gtk--keymap-binding-p b)
                  (format "%s (prefix)" label))
                 (t (format "%s runs %S" label b))))))
       (nemacs-gtk--register-pending-op
        ;; Phase 2.AX: a `C-x r s/i/SPC/j' just fired — the next
        ;; event is consumed as the register name (= a single char).
        (let ((op nemacs-gtk--register-pending-op))
          (setq nemacs-gtk--register-pending-op nil)
          (let ((ch (cond
                     ((and (integerp event) (>= event 0) (< event #x110000))
                      event)
                     ((eq event 'return) ?\n)
                     ((eq event 'tab)    ?\t)
                     (t nil))))
            (cond
             ((null ch)
              (setq nemacs-gtk--last-key-text
                    "register: non-char event, cancelled"))
             (t
              (nemacs-gtk--register-perform op ch))))))
       (nemacs-gtk--quoted-insert-pending
        ;; Phase 2.AF: a `C-q' just fired — the next event is
        ;; consumed verbatim regardless of its keymap binding.
        ;; Tab / RET / printable chars all become literal inserts.
        (setq nemacs-gtk--quoted-insert-pending nil)
        (let ((ch (cond
                   ((and (integerp event) (>= event 0) (< event #x110000))
                    event)
                   ((eq event 'return) ?\n)
                   ((eq event 'tab)    ?\t)
                   (t nil))))
          (cond
           ((null ch)
            (setq nemacs-gtk--last-key-text
                  "quoted-insert: non-char event, ignored"))
           (t
            (with-current-buffer (nemacs-gtk--active-buffer)
              (nelisp-ec-insert (string ch)))
            (setq nemacs-gtk--last-key-text
                  (format "quoted-insert: %c (#%d)" ch ch))
            (nemacs-gtk--ensure-cursor-visible)))))
       ((and nemacs-gtk--electric-pair-mode
             (null nemacs-gtk--pending-prefix)
             (integerp event)
             (or (assq event nemacs-gtk--electric-open-pairs)
                 (memq event nemacs-gtk--electric-close-set))
             (with-current-buffer (nemacs-gtk--active-buffer)
               (not (and (boundp 'buffer-read-only) buffer-read-only))))
        ;; Phase 2.BI — electric-pair: opener inserts pair, closer
        ;; at-point steps past, unmatched closer self-inserts.
        ;; Skipped under any prefix (= the user is mid-`C-x'-style
        ;; chord and the bare `(' is part of a sequence, not text).
        (nemacs-gtk--electric-pair-handle event)
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
            ;; Phase 2.AQ — read-only guard.  When the active buffer
            ;; has `buffer-read-only' set, edit-class commands report
            ;; "Buffer is read-only" instead of running.  Motion /
            ;; search / mode toggles flow through normally.
            (cond
             ((and (memq binding nemacs-gtk--read-only-blocked-commands)
                   (with-current-buffer (nemacs-gtk--active-buffer)
                     (and (boundp 'buffer-read-only)
                          buffer-read-only)))
              (setq nemacs-gtk--last-key-text "Buffer is read-only"))
             (t
            ;; Phase 2.AP — record the resolved key sequence on the
            ;; active macro recording (= the whole `accumulated' vec
            ;; including any prefix events, so replay reproduces the
            ;; exact dispatch path).  Don't record macro start/end
            ;; meta-keys themselves — that would trap the user in an
            ;; infinite recursion when replaying.
            (when (and nemacs-gtk--kbd-macro-recording
                       (not (memq binding
                                  '(nemacs-gtk-start-kbd-macro
                                    nemacs-gtk-end-kbd-macro
                                    nemacs-gtk-call-last-kbd-macro))))
              (push accumulated nemacs-gtk--kbd-macro-current))
            (with-current-buffer (nemacs-gtk--active-buffer)
              (apply #'emacs-command-loop-feed-events
                     (append accumulated nil))
              (emacs-command-loop-step)
              ;; Phase 2.AG — close one undo group per command, except
              ;; consecutive `self-insert-command' which collapse into
              ;; one group (= matches Emacs' typing-cluster semantics).
              ;; `emacs-command-loop--last-command' was just promoted
              ;; from `this-command' inside the step.
              (when (and (boundp 'emacs-command-loop--last-command)
                         emacs-command-loop--last-command
                         (not (eq emacs-command-loop--last-command
                                  'self-insert-command))
                         (fboundp 'undo-boundary))
                (undo-boundary))
              ;; Phase 2.BE — reset M-r cycle when a non-M-r command runs.
              (when (and (boundp 'emacs-command-loop--last-command)
                         emacs-command-loop--last-command
                         (not (eq emacs-command-loop--last-command
                                  'nemacs-gtk-move-to-window-line-top-bottom)))
                (setq nemacs-gtk--m-r-state 0)))
            (nemacs-gtk--ensure-cursor-visible)))))))))))


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
dialogs leave the current buffer in place.

Phase 3.A: after the load succeeds, `set-auto-mode' picks a
major-mode based on `auto-mode-alist' so e.g. opening a `.el' file
auto-activates `emacs-lisp-mode'."
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
          (let ((bn (buffer-name buf)))
            (setq nemacs-gtk--active-buffer-name bn)
            (setq nemacs-gtk--scroll-offset 0)
            (nemacs-gtk--apply-mode-for-buffer bn)
            (nemacs-gtk--sync-window-title)
            (setq nemacs-gtk--last-key-text
                  (format "Opened: %s [%s]" path
                          (symbol-name
                           (nemacs-gtk--buffer-mode bn))))))))))))

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
   ;; Phase 2.AC — buffer-menu leaves emit "switch-to-buffer:NAME".
   ((and (>= (length action) 17)
         (string= (substring action 0 17) "switch-to-buffer:"))
    (let ((name (substring action 17)))
      (cond
       ((not (get-buffer name))
        (setq nemacs-gtk--last-key-text
              (format "buffer-menu: %s gone" name)))
       (t
        (setq nemacs-gtk--active-buffer-name name)
        (setq nemacs-gtk--scroll-offset 0)
        (nemacs-gtk--sync-window-title)
        (setq nemacs-gtk--last-key-text
              (format "Switched: %s" name))))))
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
      ;; Phase 2.AU+AW — in multi-window mode, switch current window
      ;; to whichever rectangle contains the click cell.
      (let ((widx (nemacs-gtk--window-at-cell row col)))
        (when (and widx
                   (not (= widx nemacs-gtk--current-window-idx)))
          (nemacs-gtk--sync-current-to-window)
          (setq nemacs-gtk--current-window-idx widx)
          (nemacs-gtk--load-window-to-globals)))
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
  ;; Phase 3.A — seed auto-mode-alist before any file open.
  (nemacs-gtk--seed-auto-mode-alist)
  ;; Phase 3.C — load user init file (~/.emacs.d/init.el or ~/.emacs)
  ;; before painting so any `setq` of GUI defvars / `electric-pair-mode'
  ;; calls / etc. take effect on the first frame.
  (nemacs-gtk--load-user-init-file)
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
