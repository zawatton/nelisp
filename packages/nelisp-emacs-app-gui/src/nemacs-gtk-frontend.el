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

(defconst nemacs-gtk--menu-spec
  '(("File"
     ("Save" . "save")
     ("Quit" . "quit"))
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
`nemacs-main--init-keymap' (`nemacs-main.el') wires for the TUI:

  ASCII 32..126 → `self-insert-command'
  byte 13 / `'return'  → `newline'
  byte 127 / `'backspace' → `delete-backward-char'
  `'left' / `'right'   → `backward-char' / `forward-char'
  `'up'   / `'down'    → `previous-line' / `next-line'

Idempotent — re-calling replaces the global map with a fresh one."
  (let ((m (make-sparse-keymap)))
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
    (use-global-map m)))

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
  (with-current-buffer (get-buffer "*welcome*")
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
  "Stamp `*welcome*' content into rows 0..MODE_LINE_ROW of the grid."
  (let* ((content
          (with-current-buffer (get-buffer "*welcome*") (buffer-string)))
         (lines (split-string content "\n"))
         (max-rows nemacs-gtk--buffer-area-end)
         (i 0))
    (while (< i max-rows)
      (let ((line (or (nth i lines) "")))
        (nelisp-gtk-grid-put-row i (nemacs-gtk--truncate line nemacs-gtk--cols)))
      (setq i (1+ i)))))

(defun nemacs-gtk--paint-mode-line ()
  (nelisp-gtk-grid-put-row nemacs-gtk--mode-line-row
                           (nemacs-gtk--mode-line-text)))

(defun nemacs-gtk--paint-echo-area ()
  (let ((text (if (string-empty-p nemacs-gtk--last-key-text)
                  "(press any key)"
                (format "Last key: %s" nemacs-gtk--last-key-text))))
    (nelisp-gtk-grid-put-row nemacs-gtk--echo-area-row
                             (nemacs-gtk--truncate text nemacs-gtk--cols))))

(defun nemacs-gtk--cursor-row-col ()
  "Compute the screen (row . col) of point in `*welcome*', or nil
when point is outside the visible buffer area."
  (with-current-buffer (get-buffer "*welcome*")
    (let* ((p (point))
           (target (1- p))
           (row 0) (col 0)
           (i 0)
           (text (buffer-string)))
      (while (< i target)
        (let ((c (aref text i)))
          (if (eq c ?\n)
              (setq row (1+ row) col 0)
            (setq col (1+ col))))
        (setq i (1+ i)))
      (if (and (< row nemacs-gtk--buffer-area-end)
               (<= col nemacs-gtk--cols))
          (cons row col)
        nil))))

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

(defun nemacs-gtk--key-event->command-loop-event (keysym _mods unicode)
  "Map a GDK key event to the event symbol / integer
`emacs-command-loop' expects in its unread queue.  Returns nil if the
key has no handled mapping (= modifier-only, function key without a
binding) so the caller can drop it."
  (cond
   ((= keysym nemacs-gtk--keysym-backspace) 'backspace)
   ((or (= keysym nemacs-gtk--keysym-return)
        (= keysym nemacs-gtk--keysym-kp-enter))
    'return)
   ((= keysym nemacs-gtk--keysym-left)  'left)
   ((= keysym nemacs-gtk--keysym-right) 'right)
   ((= keysym nemacs-gtk--keysym-up)    'up)
   ((= keysym nemacs-gtk--keysym-down)  'down)
   ((and (> unicode 0)
         (>= unicode 32)
         (< unicode 127))
    unicode)
   (t nil)))

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

(defun nemacs-gtk--dispatch-key (keysym mods unicode)
  "Translate a GDK key event into a command-loop event + run one
dispatch step against the `*welcome*' buffer."
  (let ((event (nemacs-gtk--key-event->command-loop-event
                keysym mods unicode)))
    (when event
      (with-current-buffer (get-buffer "*welcome*")
        (emacs-command-loop-feed-events event)
        (emacs-command-loop-step)))))


;;;; --- menu dispatch -------------------------------------------------------

(defvar nemacs-gtk--quit-requested nil
  "Non-nil when an elisp-side handler (= File > Quit menu) wants the
main loop to exit.  Checked alongside `(nelisp-gtk-should-quit)'
which covers the GTK window-close path.")

(defun nemacs-gtk--handle-menu-action (action)
  "Dispatch a menu click — ACTION is the leaf's name-string from
`nemacs-gtk--menu-spec'.  Most actions are still placeholders pending
later phases; the echo area surfaces what was clicked."
  (cond
   ((string= action "quit")
    ;; Synthesize the same close path the WM-X button takes.  Layer 3
    ;; sets `nemacs-gtk--quit-requested' which the main loop checks.
    (setq nemacs-gtk--last-key-text "menu: Quit")
    (setq nemacs-gtk--quit-requested t))
   ((string= action "save")
    (setq nemacs-gtk--last-key-text "menu: Save (Phase 2.B planned)"))
   ((string= action "cut")
    (setq nemacs-gtk--last-key-text "menu: Cut (Phase 2.C clipboard planned)"))
   ((string= action "copy")
    (setq nemacs-gtk--last-key-text "menu: Copy (Phase 2.C clipboard planned)"))
   ((string= action "paste")
    (setq nemacs-gtk--last-key-text "menu: Paste (Phase 2.C clipboard planned)"))
   ((string= action "about")
    (setq nemacs-gtk--last-key-text "nemacs-gtk Phase 2 — elisp-driven"))
   (t
    (setq nemacs-gtk--last-key-text (format "menu: %s (unhandled)" action)))))


;;;; --- mouse dispatch ------------------------------------------------------

(defun nemacs-gtk--handle-mouse-event (ev)
  "Dispatch a mouse event surfaced by `(nelisp-gtk-poll-mouse)'.  EV is
the (KIND BUTTON ROW COL MODS) tuple — KIND is `'press' / `'release' /
`'scroll-up' / `'scroll-down'.  MVP: press echoes button + cell, scroll
echoes direction; release is intentionally ignored so click events
don't double-fire.  Future phases will route presses through
`emacs-command-loop' as `mouse-1' / `mouse-2' / `mouse-3' events."
  (let ((kind   (nth 0 ev))
        (button (nth 1 ev))
        (row    (nth 2 ev))
        (col    (nth 3 ev)))
    (cond
     ((eq kind 'press)
      (setq nemacs-gtk--last-key-text
            (format "mouse-%d press @ (%d,%d)" button row col)))
     ((eq kind 'scroll-up)
      (setq nemacs-gtk--last-key-text "scroll up"))
     ((eq kind 'scroll-down)
      (setq nemacs-gtk--last-key-text "scroll down"))
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
  ;; 3. Layer 2 keymap + welcome buffer.
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
