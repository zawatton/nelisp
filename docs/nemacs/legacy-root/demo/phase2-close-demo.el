;;; phase2-close-demo.el --- Phase 2 close gate mini demo  -*- lexical-binding: t; -*-

;; nelisp-emacs Phase 2 close demo per Doc 43 §3.1 Phase 11.A ship gate.
;; Exercises the full TUI substrate end-to-end in a single scenario:
;;
;;   - emacs-frame (use-tui-backend) : swap dispatch to the TUI backend
;;                                     so subsequent frame ops route
;;                                     through `emacs-tui-backend'.
;;   - emacs-tui-terminfo            : capability detection from a
;;                                     deterministic env (xterm-256color).
;;   - emacs-tui-event (feed-bytes)  : simulate user keystrokes by
;;                                     feeding raw bytes to the stdin
;;                                     parser.
;;   - emacs-frame--tui-read-event   : T161 keyboard wire-up consumes
;;                                     parsed events through
;;                                     `emacs-keymap--read-event-fn'.
;;   - emacs-buffer / nelisp-ec      : insert-file-contents (= "find-file"),
;;                                     self-insert via the keymap reader,
;;                                     write-region (= "save-buffer").
;;   - SIGWINCH (T160)               : a dispatched resize event applies
;;                                     to every live frame on the way.
;;   - emacs-frame (use-stub-backend): clean revert, prior keymap
;;                                     reader restored.
;;
;; The `phase2-close-demo-run' entry point performs the whole sequence
;; and returns a plist of observable state so
;; `test/phase2-close-demo-test.el' can assert per-checkpoint.

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)
(require 'emacs-buffer)
(require 'emacs-frame)
(require 'emacs-keymap)
(require 'emacs-tui-backend)
(require 'emacs-tui-event)
(require 'emacs-tui-terminfo)

(defun phase2-close-demo--reset-world ()
  "Wipe per-module global state so the demo runs from a clean slate."
  (setq nelisp-ec--buffers nil
        nelisp-ec--current-buffer nil
        emacs-buffer--state (make-hash-table :test 'eq)
        emacs-buffer--variable-buffer-local nil
        emacs-buffer--default-values (make-hash-table :test 'eq)
        emacs-buffer--overlay-counter 0
        emacs-frame--id-counter 0
        emacs-frame--registry nil
        emacs-frame--selected nil
        emacs-frame--focus nil
        emacs-frame--backend-dispatch nil
        emacs-frame--tui-handle nil
        emacs-frame--tui-event-handle nil
        emacs-frame--tui-terminfo nil
        emacs-tui-backend--handle-counter 0
        emacs-tui-event--handle-counter 0
        emacs-tui-event--installed-handles nil
        emacs-tui-terminfo--cache nil))

(defun phase2-close-demo--self-insert-loop (buffer n)
  "Pull N events through `emacs-keymap--read-event-fn' and insert chars.
Non-character events (= symbols such as `up') are forwarded into a
return list so the caller can inspect them; characters are inserted
into BUFFER at point.  Returns the list of non-character events seen
in pull order."
  (let ((seen-symbols nil))
    (nelisp-ec-with-current-buffer buffer
      (dotimes (_ n)
        (let ((ev (funcall emacs-keymap--read-event-fn)))
          (cond
           ((integerp ev) (nelisp-ec-insert (string ev)))
           ((symbolp ev)  (push ev seen-symbols))))))
    (nreverse seen-symbols)))

;;;###autoload
(defun phase2-close-demo-run (&optional opts)
  "Run the Phase 2 close mini demo and return its observable state.

OPTS is an optional plist; recognised keys:
  :env          alist forwarded to `emacs-frame-use-tui-backend' (default
                = xterm-256color).
  :tmpfile      explicit temp file path (default = a fresh `make-temp-file').
  :seed-text    initial contents written to TMPFILE before find-file
                (default = \"hello \").
  :typed-bytes  string fed to `emacs-tui-event-feed-bytes' to simulate
                user keystrokes (default = \"world\").

Returns a plist:
  :env-term            string TERM value used for detection.
  :backend-before      symbol — `emacs-frame-current-backend' at start.
  :backend-after-tui   symbol — backend after `use-tui-backend'.
  :backend-after-stub  symbol — backend after `use-stub-backend'.
  :term-colors         integer — :colors slot of the detected terminfo.
  :insert-file-result  cons returned by `nelisp-ec-insert-file-contents'
                       (= FILE . CHARS-INSERTED).
  :typed-chars         integer — number of characters consumed via the
                       keymap reader during the self-insert loop.
  :seen-symbols        list of symbol events surfaced through the reader
                       (= drained by the loop but not insertable).
  :buffer-final        final buffer contents after self-insert.
  :write-bytes         integer — number of bytes written by
                       `nelisp-ec-write-region'.
  :disk-contents       contents of TMPFILE after the save.
  :resize-applied      cons (W . H) of the frame size after a SIGWINCH
                       (= verifies T160 SIGWINCH wire-up + T161 keyboard
                       wire-up coexist).
  :keymap-reader-restored  t iff the prior reader fn was restored on
                           `use-stub-backend' (= nil before, nil after).
  :tmpfile             the temp file path used (so callers can clean up)."
  (phase2-close-demo--reset-world)
  (let* ((env         (or (plist-get opts :env) '(("TERM" . "xterm-256color"))))
         (tmpfile     (or (plist-get opts :tmpfile)
                          (make-temp-file "phase2-close-demo-" nil ".txt")))
         (seed-text   (or (plist-get opts :seed-text) "hello "))
         (typed-bytes (or (plist-get opts :typed-bytes) "world"))
         (backend-before (emacs-frame-current-backend))
         (prior-reader emacs-keymap--read-event-fn))
    (unwind-protect
        (progn
          ;; Seed the temp file so find-file has something to read.
          (with-temp-file tmpfile (insert seed-text))

          ;; ── (1) install TUI substrate ────────────────────────────────
          (let* ((install-result
                  (emacs-frame-use-tui-backend (list :env env)))
                 (backend-after-tui (emacs-frame-current-backend))
                 (info (plist-get install-result :info))
                 (term-colors (and info (plist-get info :colors)))
                 (event-handle (plist-get install-result :event)))

            ;; ── (2) mini find-file: read TMPFILE into a new buffer ─────
            (let* ((buf (nelisp-ec-generate-new-buffer "scratch"))
                   (insert-result
                    (nelisp-ec-with-current-buffer buf
                      (nelisp-ec-goto-char (nelisp-ec-point-max))
                      (nelisp-ec-insert-file-contents tmpfile))))

              ;; ── (3) simulate keystrokes: bytes → parser → keymap reader
              (emacs-tui-event-feed-bytes event-handle typed-bytes)
              (let* ((seen-symbols
                      (phase2-close-demo--self-insert-loop
                       buf (length typed-bytes)))
                     (typed-chars
                      (- (length typed-bytes) (length seen-symbols))))

                ;; ── (4) SIGWINCH coexistence check ────────────────────
                (let* ((frame (emacs-frame-make-frame
                               '((width . 80) (height . 24)))))
                  (emacs-tui-event-dispatch-resize event-handle 132 50)

                  ;; ── (5) mini save-buffer: write back to TMPFILE ─────
                  (let* ((write-bytes
                          (nelisp-ec-with-current-buffer buf
                            (nelisp-ec-write-region
                             (nelisp-ec-point-min)
                             (nelisp-ec-point-max)
                             tmpfile)))
                         (final-text
                          (nelisp-ec-with-current-buffer buf
                            (nelisp-ec-buffer-substring
                             (nelisp-ec-point-min)
                             (nelisp-ec-point-max))))
                         (disk-text (with-temp-buffer
                                      (insert-file-contents tmpfile)
                                      (buffer-string))))

                    ;; ── (6) revert to stub ────────────────────────────
                    (emacs-frame-use-stub-backend)

                    (list :env-term         (cdr (assoc "TERM" env))
                          :backend-before   backend-before
                          :backend-after-tui backend-after-tui
                          :backend-after-stub (emacs-frame-current-backend)
                          :term-colors      term-colors
                          :insert-file-result insert-result
                          :typed-chars      typed-chars
                          :seen-symbols     seen-symbols
                          :buffer-final     final-text
                          :write-bytes      write-bytes
                          :disk-contents    disk-text
                          :resize-applied   (cons (emacs-frame-frame-width frame)
                                                  (emacs-frame-frame-height frame))
                          :keymap-reader-restored
                          (eq prior-reader emacs-keymap--read-event-fn)
                          :tmpfile          tmpfile)))))))
      ;; Best-effort cleanup so repeated runs don't litter /tmp.
      (when (and tmpfile (file-exists-p tmpfile))
        (ignore-errors (delete-file tmpfile))))))

(provide 'phase2-close-demo)
;;; phase2-close-demo.el ends here
