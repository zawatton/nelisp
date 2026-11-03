;;; phase1-close-demo.el --- Phase 1 close gate mini demo  -*- lexical-binding: t; -*-

;; nelisp-emacs Phase 1 close demo per Doc 34 §9.5 / gate-M9.
;; Exercises every Phase 1 §4 sub-task in a single end-to-end scenario:
;;
;;   - emacs-buffer       : 2 buffers with inserted text + text-property
;;                          range merge + category inheritance.
;;   - emacs-window       : split-window-below, set-window-buffer per
;;                          pane, other-window cycle, window-live-p.
;;   - emacs-buffer (F.)  : overlay with face property, get-char-property
;;                          overlay-first / text-prop fallback.
;;   - emacs-keymap (G.)  : kbd-style keymap-set, keymap-lookup.
;;   - emacs-minibuffer   : read-from-minibuffer round-trip via the
;;                          plug-in reader (= no terminal needed).
;;   - emacs-tui-stub     : frame-create, canvas-draw-text, canvas-flush
;;                          for each window's text region.
;;
;; The `phase1-close-demo-run' entry point returns a plist of observable
;; state (= buffer names, window layout, canvas snapshot, keymap binding)
;; so `test/phase1-close-demo-test.el' can assert per-checkpoint.
;;
;; Doc 34 §9.5 scope: 2 buffer / 2 window / split / select (the
;; open-save-quit pieces stay deferred to the future nelisp-elisp-compat
;; repo per Doc 33 §3.2 L11.D delegate boundary).

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-keymap)
(require 'emacs-minibuffer)
(require 'emacs-tui-stub)

(defun phase1-close-demo--reset-world ()
  "Wipe the per-module global state so the demo runs from a clean slate."
  (setq nelisp-ec--buffers nil
        nelisp-ec--current-buffer nil
        emacs-buffer--state (make-hash-table :test 'eq)
        emacs-buffer--variable-buffer-local nil
        emacs-buffer--default-values (make-hash-table :test 'eq)
        emacs-buffer--overlay-counter 0
        emacs-window--id-counter 0
        emacs-window--root nil
        emacs-window--selected nil)
  (emacs-minibuffer-reset))

(defun phase1-close-demo--draw-window (handle frame win row)
  "Render WIN's buffer into HANDLE's FRAME canvas at ROW.
Returns the row immediately below the drawn text."
  (let* ((buf  (emacs-window-window-buffer win))
         (text (and buf (nelisp-ec-with-current-buffer buf
                          (nelisp-ec-buffer-substring
                           (nelisp-ec-point-min)
                           (nelisp-ec-point-max))))))
    (emacs-tui-stub-canvas-draw-text handle frame row 0 (or text ""))
    (1+ row)))

;;;###autoload
(defun phase1-close-demo-run ()
  "Run the Phase 1 close mini demo and return its observable state.
The returned plist contains:
  :buffers        — list of (NAME . SIZE) for every buffer created.
  :windows        — count of live leaf windows after the split.
  :window-buffers — list of buffer names shown in each window in order.
  :overlay-face   — face value of the overlay applied to buffer 2.
  :textprop-face  — face at position 3 of buffer 1 (= category inheritance).
  :keymap-binding — command symbol bound to \"C-x C-s\" in the demo map.
  :minibuffer-out — the string returned by the minibuffer reader.
  :canvas-rows    — number of canvas rows drawn (= one per window).
  :tui-handle-live — t iff the TUI stub handle is still initialized."
  (phase1-close-demo--reset-world)

  ;; ── (1) two buffers with content ───────────────────────────────────
  (let* ((b1 (nelisp-ec-generate-new-buffer "draft.txt"))
         (b2 (nelisp-ec-generate-new-buffer "notes.txt")))
    (nelisp-ec-with-current-buffer b1
      (nelisp-ec-insert "Phase 1 close demo — draft buffer"))
    (nelisp-ec-with-current-buffer b2
      (nelisp-ec-insert "Phase 1 close demo — notes buffer"))

    ;; ── (2) text-property with `category' inheritance on b1 ──────────
    (let ((cat (make-symbol "phase1-demo-cat")))
      (put cat 'face 'phase1-demo-face)
      (emacs-buffer-add-text-properties 1 (1+ (length "Phase"))
                                        (list 'category cat)
                                        b1))

    ;; ── (3) overlay with face on b2 ──────────────────────────────────
    (let ((ov (emacs-buffer-make-overlay 1 (1+ (length "Phase")) b2)))
      (emacs-buffer-overlay-put ov 'face 'phase1-demo-overlay-face))

    ;; ── (4) split-window-below + assign buffers ──────────────────────
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window-below)))
      (emacs-window-set-window-buffer w1 b1)
      (emacs-window-set-window-buffer w2 b2)

      ;; ── (5) other-window cycle returns to w1 ───────────────────────
      (emacs-window-select-window w1)
      (emacs-window-other-window 1)            ;; → w2
      (emacs-window-other-window 1)            ;; → w1 (wraps in 2-leaf tree)

      ;; Snapshot window state BEFORE the minibuffer reader — the latter
      ;; allocates its own leaf via `emacs-window-split-window' on first
      ;; use (Doc 34 §2.7 minibuffer-as-window invariant) and we do not
      ;; want that bookkeeping leaf to inflate the demo's window count.
      (let ((window-count (length (emacs-window--all-leaves)))
            (window-buf-names
             (mapcar (lambda (w)
                       (nelisp-ec-buffer-name
                        (emacs-window-window-buffer w)))
                     (emacs-window--all-leaves))))

        ;; ── (6) keymap-set / keymap-lookup round-trip ─────────────────
        (let* ((map (emacs-keymap-make-sparse-keymap))
               (save-cmd 'phase1-close-demo-save))
          (emacs-keymap-keymap-set map "C-x C-s" save-cmd)

          ;; ── (7) minibuffer round-trip via plug-in reader ────────────
          (emacs-minibuffer-feed-input "saved")
          (let ((reply (emacs-minibuffer-read-from-minibuffer
                        "Command: " nil nil nil nil "fallback")))

            ;; ── (8) TUI stub canvas — render each window's text ──────
            (let* ((handle (emacs-tui-stub-init))
                   (frame  (emacs-tui-stub-frame-create handle "phase1-close"))
                   (rows   0))
              (setq rows (phase1-close-demo--draw-window handle frame w1 0))
              (setq rows (phase1-close-demo--draw-window handle frame w2 rows))
              (emacs-tui-stub-canvas-flush handle frame)

              (let ((result
                     (list :buffers
                           (list (cons (nelisp-ec-buffer-name b1)
                                       (nelisp-ec-buffer-size b1))
                                 (cons (nelisp-ec-buffer-name b2)
                                       (nelisp-ec-buffer-size b2)))
                           :windows window-count
                           :window-buffers window-buf-names
                           :overlay-face
                           (emacs-buffer-get-char-property
                            1 'face b2)
                           :textprop-face
                           (emacs-buffer-get-text-property 3 'face b1)
                           :keymap-binding
                           (emacs-keymap-keymap-lookup map "C-x C-s")
                           :minibuffer-out reply
                           :canvas-rows rows
                           :tui-handle-live (emacs-tui-stub-handlep handle))))
                (emacs-tui-stub-frame-destroy handle frame)
                (emacs-tui-stub-shutdown handle)
                result))))))))

(provide 'phase1-close-demo)
;;; phase1-close-demo.el ends here
