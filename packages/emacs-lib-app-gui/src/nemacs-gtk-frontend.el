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

(defconst nemacs-gtk--rows 24)
(defconst nemacs-gtk--cols 80)
(defconst nemacs-gtk--mode-line-row (- nemacs-gtk--rows 2))
(defconst nemacs-gtk--echo-area-row (- nemacs-gtk--rows 1))
(defconst nemacs-gtk--buffer-area-end nemacs-gtk--mode-line-row) ; exclusive

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
    ;; C-x prefix map — Save / Open round-trip via the same handlers
    ;; the menu uses.
    (define-key ctl-x-map (vector ?\C-s) 'nemacs-gtk-keyboard-save)
    (define-key ctl-x-map (vector ?\C-f) 'nemacs-gtk-keyboard-find-file)
    (define-key m (vector ?\C-x) ctl-x-map)
    ;; Mouse: left click inside buffer area routes through
    ;; `emacs-command-loop' as a `mouse-1' event bound to
    ;; `nemacs-gtk-mouse-set-point' (= grid → goto-char).
    (define-key m (vector 'mouse-1) 'nemacs-gtk-mouse-set-point)
    (use-global-map m)))

(defun nemacs-gtk-keyboard-save ()
  "Bound to `C-x C-s' — wraps the same menu handler as File > Save."
  (interactive)
  (nemacs-gtk--menu-save-file))

(defun nemacs-gtk-keyboard-find-file ()
  "Bound to `C-x C-f' — wraps the same menu handler as File > Open."
  (interactive)
  (nemacs-gtk--menu-open-file))

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

(defun nemacs-gtk--mode-line-text ()
  "Compose the mode-line for the current `*welcome*' buffer state."
  (with-current-buffer (nemacs-gtk--active-buffer)
    (let* ((name (buffer-name))
           (line (line-number-at-pos))
           (mode (symbol-name (if (boundp 'major-mode) major-mode
                                'fundamental-mode)))
           (body (format "-U:---  %s    L%d   All   (%s) "
                         name line mode))
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

(defun nemacs-gtk--paint-echo-area ()
  (let ((text (if (string-empty-p nemacs-gtk--last-key-text)
                  "(press any key)"
                (format "Last key: %s" nemacs-gtk--last-key-text))))
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
(defconst nemacs-gtk--keysym-left      #xff51)
(defconst nemacs-gtk--keysym-up        #xff52)
(defconst nemacs-gtk--keysym-right     #xff53)
(defconst nemacs-gtk--keysym-down      #xff54)
(defconst nemacs-gtk--keysym-kp-enter  #xff8d)

;; GDK ModifierType bit positions (= `gdk_modifier_type' in libgdk-4):
(defconst nemacs-gtk--gdk-shift-mask    1)
(defconst nemacs-gtk--gdk-control-mask  4)

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
     ((= keysym nemacs-gtk--keysym-left)  'left)
     ((= keysym nemacs-gtk--keysym-right) 'right)
     ((= keysym nemacs-gtk--keysym-up)    'up)
     ((= keysym nemacs-gtk--keysym-down)  'down)
     ;; Ctrl + ASCII letter → control byte.  Try unicode first
     ;; (= what GDK delivers when the key produces a printable),
     ;; fall back to keysym for the Ctrl-only case where unicode
     ;; comes through as 0.
     (ctrl
      (let ((ch (cond
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

(defvar nemacs-gtk--pending-prefix nil
  "Vector of accumulated events when the previous keypresses formed
a keymap prefix (= e.g. `[24]' after C-x).  Reset to nil once a
non-keymap binding is reached.

Accumulating in elisp lets us hand the FULL key sequence to
`emacs-command-loop-feed-events' in a single call so
`emacs-command-loop-step's `read-keys-vec' can consume it without
running out of events mid-prefix (= `read-event' on an empty
queue would otherwise raise `emacs-command-loop-no-input').")

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
active buffer.  Handles prefix keys (= C-x prefix → wait for next
event before stepping) by accumulating onto
`nemacs-gtk--pending-prefix'.  After the command runs, ensure the
cursor stays inside the viewport."
  (let ((event (nemacs-gtk--key-event->command-loop-event
                keysym mods unicode)))
    (when event
      (let* ((accumulated (vconcat (or nemacs-gtk--pending-prefix [])
                                   (vector event)))
             (binding (nemacs-gtk--lookup-key-vec accumulated)))
        (cond
         ((nemacs-gtk--keymap-binding-p binding)
          ;; More events expected — stage prefix + echo "C-x -" style.
          (setq nemacs-gtk--pending-prefix accumulated)
          (setq nemacs-gtk--last-key-text
                (format "%s-" (nemacs-gtk--describe-key-vec accumulated))))
         (t
          ;; Either a real binding or unbound key — flush + step.
          (setq nemacs-gtk--pending-prefix nil)
          (with-current-buffer (nemacs-gtk--active-buffer)
            (apply #'emacs-command-loop-feed-events
                   (append accumulated nil))
            (emacs-command-loop-step))
          (nemacs-gtk--ensure-cursor-visible)))))))


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

(defvar nemacs-gtk--quit-requested nil
  "Non-nil when an elisp-side handler (= File > Quit menu) wants the
main loop to exit.  Checked alongside `(nelisp-gtk-should-quit)'
which covers the GTK window-close path.")

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
   ((string= action "open")   (nemacs-gtk--menu-open-file))
   ((string= action "save")   (nemacs-gtk--menu-save-file))
   ((string= action "cut")    (nemacs-gtk--menu-cut-current-line))
   ((string= action "copy")   (nemacs-gtk--menu-copy-current-line))
   ((string= action "paste")  (nemacs-gtk--menu-paste))
   ((string= action "about")
    (setq nemacs-gtk--last-key-text "nemacs-gtk Phase 2 — elisp-driven"))
   (t
    (setq nemacs-gtk--last-key-text (format "menu: %s (unhandled)" action)))))


;;;; --- mouse dispatch ------------------------------------------------------

(defvar nemacs-gtk--last-mouse-event nil
  "Most-recent mouse-press tuple for the bound mouse command to
consume.  Format mirrors what `(nelisp-gtk-poll-mouse)' returns:
(KIND BUTTON ROW COL MODS).  Set by `nemacs-gtk--handle-mouse-event'
just before feeding the synthetic `mouse-1' event into
`emacs-command-loop', read by `nemacs-gtk-mouse-set-point'.")

(defun nemacs-gtk-mouse-set-point ()
  "Bound to `mouse-1' in the GUI's global keymap.  Move point in the
active buffer to the cell stored on `nemacs-gtk--last-mouse-event'.

Mirrors Emacs' `mouse-set-point' contract — the GUI emits a synthetic
`mouse-1' event into the command loop; the handler stages the event
on a defvar and the command consumes it.  This keeps the keymap
binding ordinary (= no `(interactive \"e\")' event-arg plumbing) and
matches how the keyboard dispatch already works."
  (interactive)
  (let* ((ev nemacs-gtk--last-mouse-event)
         (row (nth 2 ev))
         (col (nth 3 ev)))
    (when ev
      (let ((p (nemacs-gtk--cell-to-point row col)))
        (with-current-buffer (nemacs-gtk--active-buffer)
          (nelisp-ec-goto-char p))
        (setq nemacs-gtk--last-key-text
              (format "mouse-1 → point %d (cell %d,%d)" p row col))))))

(defun nemacs-gtk--handle-mouse-event (ev)
  "Dispatch a mouse event surfaced by `(nelisp-gtk-poll-mouse)'.  EV is
the (KIND BUTTON ROW COL MODS) tuple — KIND is `'press' / `'release' /
`'scroll-up' / `'scroll-down'.

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
      (setq nemacs-gtk--last-mouse-event ev)
      (with-current-buffer (nemacs-gtk--active-buffer)
        (emacs-command-loop-feed-events 'mouse-1)
        (emacs-command-loop-step)))
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
        (nemacs-gtk--repaint))))
  'done)

(provide 'nemacs-gtk-frontend)

;;; nemacs-gtk-frontend.el ends here
