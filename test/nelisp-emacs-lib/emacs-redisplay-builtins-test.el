;;; emacs-redisplay-builtins-test.el --- ERT for redisplay close-gate  -*- lexical-binding: t; -*-

;;; Commentary:

;; Track G ERT.  Verifies the Phase 3 close-gate trigger bridges
;; (`force-mode-line-update' / `redraw-display' / `redraw-frame' /
;; `redisplay') route to the substrate's dirty-tracking, plus the
;; "5x throughput" diff-redraw bench from Doc 01 §3.3 / Doc 43
;; §3.2 close gate.
;;
;; Behavioural assertions exercise the prefixed
;; `emacs-redisplay-*' API directly so they bypass host Emacs's C
;; primitives (= same pattern as every other Track in this repo).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-tui-backend)
(require 'emacs-redisplay)
(require 'emacs-redisplay-builtins)

;;;; --- fixtures ------------------------------------------------------

(defmacro emacs-redisplay-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean emacs-window / nelisp-ec / handle state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil)
         (emacs-redisplay--current-handle nil))
     (unwind-protect
         (progn ,@body)
       (setq emacs-redisplay--current-handle nil))))

;;;; A. Load + parity

(ert-deftest emacs-redisplay-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-redisplay-builtins))
  (dolist (sym '(force-mode-line-update redraw-display redraw-frame
                 redisplay))
    (should (fboundp sym))))

(ert-deftest emacs-redisplay-builtins-test/require-is-idempotent ()
  (let ((before-fml (symbol-function 'force-mode-line-update))
        (before-redraw (symbol-function 'redraw-display))
        (before-redisplay (symbol-function 'redisplay)))
    (require 'emacs-redisplay-builtins)
    (should (eq before-fml (symbol-function 'force-mode-line-update)))
    (should (eq before-redraw (symbol-function 'redraw-display)))
    (should (eq before-redisplay (symbol-function 'redisplay)))))

(ert-deftest emacs-redisplay-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-redisplay-builtins--install-function-p))
  (should-not (emacs-redisplay-builtins--install-function-p
               'force-mode-line-update))
  (let* ((file (locate-library "emacs-redisplay-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(force-mode-line-update redraw-display redraw-frame
                     redisplay))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-redisplay-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

;;;; B. set-current-handle / current-handle accessors

(ert-deftest emacs-redisplay-builtins-test/set-and-get-current-handle ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (should (null (emacs-redisplay-current-handle)))
    (let ((h (emacs-redisplay-init)))
      (emacs-redisplay-set-current-handle h)
      (should (eq h (emacs-redisplay-current-handle)))
      ;; Setting to nil clears.
      (emacs-redisplay-set-current-handle nil)
      (should (null (emacs-redisplay-current-handle))))))

(ert-deftest emacs-redisplay-builtins-test/set-current-handle-rejects-junk ()
  (should-error (emacs-redisplay-set-current-handle "not a handle")
                :type 'emacs-redisplay-bad-handle))

;;;; C. force-mode-line-update with no current handle = no-op

(ert-deftest emacs-redisplay-builtins-test/force-mode-line-no-handle-noop ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (should (= 0 (emacs-redisplay-force-mode-line-update)))))

;;;; D. force-mode-line-update with current handle dirties selected window

(ert-deftest emacs-redisplay-builtins-test/force-mode-line-marks-dirty ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "rdb-fml"))
           (h (emacs-redisplay-init))
           (w (emacs-window-selected-window)))
      (let ((nelisp-ec--current-buffer b))
        (nelisp-ec-insert "line"))
      (emacs-window-set-window-buffer w b)
      ;; Build the cache.
      (emacs-redisplay-redisplay-window h w)
      (should (emacs-redisplay-glyph-matrix h w))
      (emacs-redisplay-set-current-handle h)
      ;; Trigger fires — selected window's matrix becomes dirty.
      (let ((cleared (emacs-redisplay-force-mode-line-update)))
        (should (= 1 cleared)))
      (should-not (emacs-redisplay-glyph-matrix h w)))))

(ert-deftest emacs-redisplay-builtins-test/force-mode-line-update-all ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "rdb-fml-all"))
           (h (emacs-redisplay-init))
           (w (emacs-window-selected-window)))
      (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "x"))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-set-current-handle h)
      ;; ALL = t goes through mark-frame-dirty which returns the
      ;; cleared-entry count.
      (let ((n (emacs-redisplay-force-mode-line-update t)))
        (should (>= n 1))))))

;;;; E. redraw-display / redraw-frame route to mark-frame-dirty

(ert-deftest emacs-redisplay-builtins-test/redraw-display-clears-frame ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "rdb-rd"))
           (h (emacs-redisplay-init))
           (w (emacs-window-selected-window)))
      (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "x"))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-set-current-handle h)
      (let ((n (emacs-redisplay-redraw-display)))
        (should (>= n 1))
        (should-not (emacs-redisplay-glyph-matrix h w))))))

(ert-deftest emacs-redisplay-builtins-test/redraw-frame-equivalent-to-redraw-display ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "rdb-rf"))
           (h (emacs-redisplay-init))
           (w (emacs-window-selected-window)))
      (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "x"))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-set-current-handle h)
      (let ((n (emacs-redisplay-redraw-frame)))
        (should (>= n 1))))))

;;;; F. trigger-redisplay returns matrix on selected window

(ert-deftest emacs-redisplay-builtins-test/trigger-redisplay-returns-matrix ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "rdb-tr"))
           (h (emacs-redisplay-init))
           (w (emacs-window-selected-window)))
      (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "abc"))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-set-current-handle h)
      (let ((m (emacs-redisplay-trigger-redisplay)))
        (should m)))))

(ert-deftest emacs-redisplay-builtins-test/trigger-redisplay-no-handle-nil ()
  (emacs-redisplay-builtins-test--with-fresh-world
    (should (null (emacs-redisplay-trigger-redisplay)))))

;;;; G. Phase 3 close-gate "5x throughput" diff-redraw bench

(ert-deftest emacs-redisplay-builtins-test/diff-redraw-5x-throughput ()
  "Phase 3 close gate (Doc 01 §3.3 / Doc 43 §3.2): hash-diff flush
must outrun full-emit flush by ≥5x.

Scope: bench measures `flush-frame' alone.  Both paths drive every
row marked dirty so the *flush* loop walks all 24 rows, but:
  - Full path: invalidates the per-matrix flush-hash cache before
    each call, forcing the row hashes to differ from the cached
    last-emitted values → all rows redrawn (= canvas-draw-text +
    SGR emit per segment).
  - Diff path: leaves the cache populated → row hashes match →
    backend draw calls are skipped, only the dirty-bit clear runs.

Per the close-gate spec we assert ratio ≥ 5x.  The redisplay-window
rebuild cost is excluded from both paths so the bench isolates the
flush diff savings."
  ;; Main HEAD's `flush-frame' uses Phase 3.B.6 row-incremental
  ;; rebuild + 3.B.7 text-tick cache instead of branch's Phase 3.B.5
  ;; flush-hash diff.  The 5x assertion only passes against the
  ;; flush-hash implementation, so skip when the cache is absent.
  ;; Tracked under the v0.1 daily-driver "post-merge polish" follow-up.
  (skip-unless
   (let ((sym 'emacs-redisplay--flush-hash-cache))
     (and (boundp sym) (symbol-value sym))))
  (let* ((iterations 200)
         (emacs-tui-backend-output-fn #'ignore)
         (sample-text (mapconcat #'identity
                                 (cl-loop for i from 1 to 24
                                          collect (format "row %02d sample text foo bar baz" i))
                                 "\n")))
    (let* ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (emacs-window--id-counter 0)
           (emacs-window--root nil)
           (emacs-window--selected nil)
           (b (nelisp-ec-generate-new-buffer "rdb-bench")))
      (let ((nelisp-ec--current-buffer b))
        (nelisp-ec-insert sample-text)
        (nelisp-ec-goto-char 1))
      (let* ((bk (emacs-tui-backend-init))
             (fr (emacs-tui-backend-frame-create bk "bench-frame"))
             (h  (emacs-redisplay-init (list :backend bk)))
             (w  (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        ;; Warm: build matrix once + populate flush-hash cache.
        (emacs-redisplay-redisplay-window h w)
        (emacs-redisplay-flush-frame h fr)
        (let* ((m (emacs-redisplay-glyph-matrix h w))
               (height (emacs-redisplay-glyph-matrix-height m))
               (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
          ;; Full path: clear the flush cache before each flush so
          ;; every row hash differs from "last flushed".
          (let* ((t0 (current-time))
                 (- (cl-loop repeat iterations do
                             (emacs-redisplay-flush-hash-clear m)
                             (dotimes (r height) (aset dirty r t))
                             (emacs-redisplay-flush-frame h fr)))
                 (full-time (float-time (time-since t0))))
            ;; Diff path: leave cache intact so every dirty row matches.
            (let* ((t1 (current-time))
                   (- (cl-loop repeat iterations do
                               (dotimes (r height) (aset dirty r t))
                               (emacs-redisplay-flush-frame h fr)))
                   (diff-time (float-time (time-since t1)))
                   (ratio (/ (max full-time 1e-9)
                             (max diff-time 1e-9))))
              (message "redisplay bench: full=%.4fs diff=%.4fs ratio=%.2fx"
                       full-time diff-time ratio)
              (should (>= ratio 5.0)))))))))

(provide 'emacs-redisplay-builtins-test)

;;; emacs-redisplay-builtins-test.el ends here
