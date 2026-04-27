;;; emacs-redisplay-test.el --- ERT for emacs-redisplay.el  -*- lexical-binding: t; -*-

;; Phase 3 module ERT per nelisp-emacs Doc 01 (LOCKED v2 §3.3),
;; mirroring NeLisp Doc 43 v2 §3.2 Phase 11.B redisplay engine MVP.
;; Phase 3.B.1 (face-realize MVP per Doc 43 §2.4) adds section G below.
;;
;; Coverage:
;;   A. driver lifecycle  (init / shutdown / handlep + version consts)
;;   B. text → glyph      (text-to-glyphs, char/face/buf-pos preservation)
;;   C. matrix building   (empty / multi-line / window-narrow / TAB)
;;   D. dirty tracking    (mark-window-dirty / mark-frame-dirty)
;;   E. backend wiring    (flush-frame writes to TUI canvas, set-cursor)
;;   F. cross-cutting     (handle errors, narrowed visible, overlays)
;;   G. face-realize MVP  (Phase 3.B.1, Doc 43 §2.4) — registry, plist
;;                         + symbol + cascade resolution, color string
;;                         normalization, weight→bold, glyph realized-
;;                         face slot, overlay merge realize, SGR emit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-tui-backend)
(require 'emacs-redisplay)

;; Optional NeLisp upstream APIs — guarded require so the test file
;; loads in either environment.  When upstream isn't available we
;; simply skip the dependent assertions inside individual tests.
(ignore-errors (require 'nelisp-emacs-compat-face))
(ignore-errors (require 'nelisp-textprop-display))
(ignore-errors (require 'nelisp-overlay))

;;; Fresh-world fixture

(defmacro emacs-redisplay-test--with-fresh-world (&rest body)
  "Run BODY with a clean emacs-window + nelisp-ec state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil))
     ,@body))

(defmacro emacs-redisplay-test--with-buffer (var content &rest body)
  "Bind VAR to a fresh nelisp-ec-buffer pre-filled with CONTENT, run BODY."
  (declare (indent 2) (debug (symbolp form body)))
  `(let* ((,var (nelisp-ec-generate-new-buffer "rd-t")))
     (let ((nelisp-ec--current-buffer ,var))
       (nelisp-ec-insert ,content)
       (nelisp-ec-goto-char 1))
     ,@body))

;;; ANSI capture sink (reused from tui-backend test pattern)

(defvar emacs-redisplay-test--captured ""
  "Accumulator for ANSI escape output captured during tests.")

(defun emacs-redisplay-test--capture-fn (string)
  "Append STRING to `emacs-redisplay-test--captured'."
  (setq emacs-redisplay-test--captured
        (concat emacs-redisplay-test--captured string)))

(defmacro emacs-redisplay-test--with-capture (&rest body)
  "Run BODY with the TUI backend output redirected to a capture sink."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-tui-backend-output-fn
          #'emacs-redisplay-test--capture-fn)
         (emacs-redisplay-test--captured ""))
     ,@body))

;;; A. driver lifecycle (4 tests)

(ert-deftest emacs-redisplay-test-init-returns-handle ()
  "init returns an alive handle with monotonic id."
  (let ((h (emacs-redisplay-init)))
    (should (emacs-redisplay-handlep h))
    (should (emacs-redisplay-handle-alive-p h))
    (should (symbolp (emacs-redisplay-handle-id h)))))

(ert-deftest emacs-redisplay-test-init-with-backend ()
  "init :backend wires the TUI backend handle into the redisplay handle."
  (let* ((b (emacs-tui-backend-init))
         (h (emacs-redisplay-init (list :backend b))))
    (should (eq b (emacs-redisplay-handle-backend h)))))

(ert-deftest emacs-redisplay-test-shutdown-marks-dead ()
  "shutdown clears alive flag and rejects subsequent ops."
  (let ((h (emacs-redisplay-init)))
    (should (eq t (emacs-redisplay-shutdown h)))
    (should-not (emacs-redisplay-handle-alive-p h))
    (should-error (emacs-redisplay-redisplay h)
                  :type 'emacs-redisplay-bad-handle)))

(ert-deftest emacs-redisplay-test-version-constants ()
  "Driver + glyph-matrix contract version constants are >= 1."
  (should (>= emacs-redisplay-driver-contract-version 1))
  (should (>= emacs-redisplay-glyph-matrix-contract-version 1)))

;;; B. text → glyph (3 tests)

(ert-deftest emacs-redisplay-test-text-to-glyphs-from-string ()
  "text-to-glyphs accepts a literal string buffer (test convenience)."
  (let* ((h (emacs-redisplay-init))
         (g (emacs-redisplay-text-to-glyphs h "hi")))
    (should (= 2 (length g)))
    (should (eq ?h (emacs-redisplay-glyph-char (aref g 0))))
    (should (eq ?i (emacs-redisplay-glyph-char (aref g 1))))))

(ert-deftest emacs-redisplay-test-text-to-glyphs-from-buffer ()
  "text-to-glyphs reads buffer text + records buf-pos."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (let* ((h (emacs-redisplay-init))
             (g (emacs-redisplay-text-to-glyphs h b)))
        (should (= 4 (length g)))
        (should (eq ?a (emacs-redisplay-glyph-char (aref g 0))))
        (should (eq 1  (emacs-redisplay-glyph-buf-pos (aref g 0))))
        (should (eq ?d (emacs-redisplay-glyph-char (aref g 3))))
        (should (eq 4  (emacs-redisplay-glyph-buf-pos (aref g 3))))))))

(ert-deftest emacs-redisplay-test-text-to-glyphs-range ()
  "text-to-glyphs with START / END returns the slice only."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcdef"
      (let* ((h (emacs-redisplay-init))
             (g (emacs-redisplay-text-to-glyphs h b 2 5)))
        (should (= 3 (length g)))
        (should (eq ?b (emacs-redisplay-glyph-char (aref g 0))))
        (should (eq ?d (emacs-redisplay-glyph-char (aref g 2))))))))

;;; C. matrix building (8 tests)

(ert-deftest emacs-redisplay-test-redisplay-empty-buffer-clears-canvas ()
  "An empty buffer yields a single empty row + height-1 padding rows."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b ""
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (= (emacs-window-window-width w)
                     (emacs-redisplay-glyph-matrix-width m)))
          (should (= (emacs-window-window-height w)
                     (emacs-redisplay-glyph-matrix-height m)))
          (should (string= ""
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0)))))))))

(ert-deftest emacs-redisplay-test-redisplay-text-draw-line ()
  "Single-line buffer paints characters into row 0."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "hello"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row (emacs-redisplay-glyph-row m 0)))
          (should (string= "hello"
                           (emacs-redisplay-glyph-row-text row)))
          (should (= 5 (emacs-redisplay-glyph-row-used row))))))))

(ert-deftest emacs-redisplay-test-redisplay-multi-line-buffer ()
  "Multi-line buffer fills consecutive rows."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "one\ntwo\nthree"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (string= "one"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0))))
          (should (string= "two"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 1))))
          (should (string= "three"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 2)))))))))

(ert-deftest emacs-redisplay-test-redisplay-truncates-long-line ()
  "Lines wider than the window width are clipped (truncate-lines = t)."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b (make-string 200 ?x)
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window))
             (width (emacs-window-window-width w)))
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row (emacs-redisplay-glyph-row m 0)))
          (should (<= (emacs-redisplay-glyph-row-used row) width))
          (should (string-match-p "^x+\\'"
                                  (emacs-redisplay-glyph-row-text row))))))))

(ert-deftest emacs-redisplay-test-redisplay-tab-expands ()
  "TAB is expanded to spaces up to the next tab stop."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "a\tb"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row (emacs-redisplay-glyph-row m 0))
               (text (emacs-redisplay-glyph-row-text row)))
          ;; "a", spaces filling to col 8, "b" → 9 chars used, last is "b".
          (should (eq ?a (aref text 0)))
          (should (eq ?b (aref text (1- (length text)))))
          (should (string-match-p "^a +b$" text)))))))

(ert-deftest emacs-redisplay-test-redisplay-window-narrow-region ()
  "Smaller window width forces narrower row (truncation)."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcdef"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (setf (emacs-window-total-cols  w) 4)
        (setf (emacs-window-total-lines w) 2)
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row (emacs-redisplay-glyph-row m 0)))
          (should (= 4 (emacs-redisplay-glyph-matrix-width m)))
          (should (<= (emacs-redisplay-glyph-row-used row) 4))
          (should (string= "abcd"
                           (emacs-redisplay-glyph-row-text row))))))))

(ert-deftest emacs-redisplay-test-redisplay-window-start-offsets-display ()
  "Setting window-start past line 1 starts display from later position."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta\ngamma"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (emacs-window-set-window-start w 7) ;; "beta"
        (let* ((m (emacs-redisplay-redisplay-window h w)))
          (should (string= "beta"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0))))
          (should (string= "gamma"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 1)))))))))

(ert-deftest emacs-redisplay-test-redisplay-cursor-position ()
  "Cursor (ROW . COL) is computed from window-point."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc\ndef"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (emacs-window-set-window-point w 6) ;; "e" of "def"
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (cur (emacs-redisplay-glyph-matrix-cursor m)))
          (should (consp cur))
          (should (= 1 (car cur)))
          (should (= 1 (cdr cur))))))))

;;; D. dirty tracking (3 tests)

(ert-deftest emacs-redisplay-test-mark-window-dirty-drops-cache ()
  "mark-window-dirty drops only the targeted matrix; nil if uncached."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (should-not (emacs-redisplay-mark-window-dirty h w))
        (emacs-redisplay-redisplay-window h w)
        (should (emacs-redisplay-glyph-matrix h w))
        (should (eq t (emacs-redisplay-mark-window-dirty h w)))
        (should-not (emacs-redisplay-glyph-matrix h w))))))

(ert-deftest emacs-redisplay-test-mark-frame-dirty-clears-all ()
  "mark-frame-dirty drops every cached entry."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (emacs-redisplay-redisplay-window h w)
        (should (= 1 (emacs-redisplay-mark-frame-dirty h)))
        (should-not (emacs-redisplay-glyph-matrix h w))))))

(ert-deftest emacs-redisplay-test-redisplay-rebuilds-after-dirty ()
  "After mark-window-dirty, redisplay rebuilds a fresh matrix."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let ((m1 (emacs-redisplay-redisplay-window h w)))
          (emacs-redisplay-mark-window-dirty h w)
          (let ((m2 (emacs-redisplay-redisplay-window h w)))
            (should-not (eq m1 m2))
            (should (string= "alpha"
                             (emacs-redisplay-glyph-row-text
                              (emacs-redisplay-glyph-row m2 0))))))))))

;;; E. backend wiring + cursor (4 tests)

(ert-deftest emacs-redisplay-test-flush-frame-writes-to-canvas ()
  "flush-frame draws into the TUI backend's canvas."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "hi"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (let ((emitted (emacs-redisplay-flush-frame h fr)))
            (should (> emitted 0))
            ;; Backend canvas should now contain "h" + "i" as the first
            ;; two cells of row 0.
            (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                   (row (aref canvas 0)))
              (should (eq ?h (car (aref row 0))))
              (should (eq ?i (car (aref row 1)))))
            (should (string-match-p (regexp-quote "h")
                                    emacs-redisplay-test--captured))))))))

(ert-deftest emacs-redisplay-test-flush-frame-without-backend-noop ()
  "flush-frame returns 0 when no backend is wired."
  (let ((h (emacs-redisplay-init)))
    (should (= 0 (emacs-redisplay-flush-frame h nil)))))

(ert-deftest emacs-redisplay-test-set-cursor-emits-cup ()
  "set-cursor delegates to backend cursor-show, emitting CUP."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc\ndef"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-window-set-window-point w 6) ;; row=1 col=1
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (emacs-redisplay-set-cursor h fr w)
          (should (string-match-p "\e\\[" emacs-redisplay-test--captured)))))))

(ert-deftest emacs-redisplay-test-redisplay-pass-counts-windows ()
  "redisplay returns the count of leaf windows visited."
  (emacs-redisplay-test--with-fresh-world
    (let* ((h (emacs-redisplay-init))
           (w1 (emacs-window-selected-window)))
      (emacs-window-set-window-buffer
       w1 (nelisp-ec-generate-new-buffer "t1"))
      (emacs-window-set-window-buffer
       (emacs-window-split-window)
       (nelisp-ec-generate-new-buffer "t2"))
      (let ((count (emacs-redisplay-redisplay h)))
        (should (= 2 count))))))

;;; F. cross-cutting (4 tests)

(ert-deftest emacs-redisplay-test-redisplay-rejects-non-window ()
  "redisplay-window signals on non-emacs-window argument."
  (let ((h (emacs-redisplay-init)))
    (should-error (emacs-redisplay-redisplay-window h 'not-a-window)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-redisplay-test-glyph-matrix-nil-before-redisplay ()
  "glyph-matrix returns nil for a window with no cached matrix."
  (emacs-redisplay-test--with-fresh-world
    (let ((h (emacs-redisplay-init))
          (w (emacs-window-selected-window)))
      (should-not (emacs-redisplay-glyph-matrix h w)))))

(ert-deftest emacs-redisplay-test-overlay-face-applies ()
  "Overlay face is merged into the glyph face slot.
We mock the overlay accessor trio so the test exercises the redisplay
merge logic without depending on the upstream `nelisp-ovly' storage
layer (which only accepts raw Emacs buffers, not `nelisp-ec-buffer')."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (let ((mock-ov 'mock-overlay))
        (cl-letf (((symbol-function 'emacs-redisplay--overlays-in)
                   (lambda (_beg _end &optional _buffer) (list mock-ov)))
                  ((symbol-function 'emacs-redisplay--ovly-bounds)
                   (lambda (ov)
                     (when (eq ov mock-ov) (cons 2 4))))
                  ((symbol-function 'emacs-redisplay--ovly-prop)
                   (lambda (ov prop)
                     (when (and (eq ov mock-ov) (eq prop 'face))
                       'highlight))))
          (let* ((h  (emacs-redisplay-init))
                 (w  (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (let* ((m (emacs-redisplay-redisplay-window h w))
                   (row (emacs-redisplay-glyph-row m 0))
                   (vec (emacs-redisplay-glyph-row-glyphs row)))
              ;; "b" (pos 2) = 2nd glyph, "c" (pos 3) = 3rd glyph
              ;; should carry the overlay face.  "a" + "d" should not.
              (should-not (emacs-redisplay-glyph-face (aref vec 0)))
              (should (let ((f (emacs-redisplay-glyph-face (aref vec 1))))
                        (or (eq f 'highlight)
                            (and (listp f) (memq 'highlight f)))))
              (should (let ((f (emacs-redisplay-glyph-face (aref vec 2))))
                        (or (eq f 'highlight)
                            (and (listp f) (memq 'highlight f))))))))))))

(ert-deftest emacs-redisplay-test-handle-bad-after-shutdown ()
  "Multiple post-shutdown ops all raise emacs-redisplay-bad-handle."
  (let ((h (emacs-redisplay-init)))
    (emacs-redisplay-shutdown h)
    (should-error (emacs-redisplay-redisplay-window
                   h (emacs-window-selected-window))
                  :type 'emacs-redisplay-bad-handle)
    (should-error (emacs-redisplay-mark-frame-dirty h)
                  :type 'emacs-redisplay-bad-handle)
    (should-error (emacs-redisplay-text-to-glyphs h "x")
                  :type 'emacs-redisplay-bad-handle)))

;;; G. face-realize MVP (Phase 3.B.1, Doc 43 §2.4) — 10 tests

(defmacro emacs-redisplay-test--with-fresh-face-registry (&rest body)
  "Reset the local face registry + cache before BODY."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-redisplay--face-registry (make-hash-table :test 'eq))
         (emacs-redisplay--face-cache (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest emacs-redisplay-test-realize-face-nil-returns-nil ()
  "Realizing nil yields nil (= default face, no SGR emitted)."
  (emacs-redisplay-test--with-fresh-face-registry
    (should-not (emacs-redisplay-realize-face nil))))

(ert-deftest emacs-redisplay-test-realize-face-plist-foreground-string ()
  "Color string `\"red\"' normalizes to backend symbol `red'."
  (emacs-redisplay-test--with-fresh-face-registry
    (let ((alist (emacs-redisplay-realize-face '(:foreground "red"))))
      (should (consp alist))
      (should (eq 'red (cdr (assq :foreground alist)))))))

(ert-deftest emacs-redisplay-test-realize-face-plist-bg-and-attrs ()
  "Background + bold + underline survive normalization."
  (emacs-redisplay-test--with-fresh-face-registry
    (let ((a (emacs-redisplay-realize-face
              '(:background "blue" :weight bold :underline t))))
      (should (eq 'blue (cdr (assq :background a))))
      (should (eq t (cdr (assq :bold a))))
      (should (eq t (cdr (assq :underline a)))))))

(ert-deftest emacs-redisplay-test-realize-face-defface-symbol ()
  "Symbol face spec resolves through the local registry."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-defface 'rdt-warn '(:foreground "yellow" :weight bold))
    (let ((a (emacs-redisplay-realize-face 'rdt-warn)))
      (should (eq 'yellow (cdr (assq :foreground a))))
      (should (eq t (cdr (assq :bold a)))))))

(ert-deftest emacs-redisplay-test-realize-face-cascade-left-wins ()
  "Cascade list (FACE PLIST) merges left-wins on conflicting keys."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-defface 'rdt-base '(:foreground "blue"
                                          :background "white"))
    (let ((a (emacs-redisplay-realize-face
              '((:foreground "red") rdt-base))))
      ;; left-wins: foreground = red, background = white (from base).
      (should (eq 'red   (cdr (assq :foreground a))))
      (should (eq 'white (cdr (assq :background a)))))))

(ert-deftest emacs-redisplay-test-realize-face-inherit-chain ()
  "`:inherit' from a registered face flattens into the result."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-defface 'rdt-parent '(:foreground "green"))
    (emacs-redisplay-defface 'rdt-child  '(:weight bold :inherit rdt-parent))
    (let ((a (emacs-redisplay-realize-face 'rdt-child)))
      (should (eq t (cdr (assq :bold a))))
      (should (eq 'green (cdr (assq :foreground a)))))))

(ert-deftest emacs-redisplay-test-realize-face-cache-hit ()
  "Repeated realize calls hit the memo cache (= same eq alist returned)."
  (emacs-redisplay-test--with-fresh-face-registry
    (let* ((spec '(:foreground "cyan"))
           (a1 (emacs-redisplay-realize-face spec))
           (a2 (emacs-redisplay-realize-face spec)))
      (should (eq a1 a2)))))

(ert-deftest emacs-redisplay-test-defface-clears-cache ()
  "Registering a face invalidates the realization cache."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-defface 'rdt-flip '(:foreground "red"))
    (let ((a1 (emacs-redisplay-realize-face 'rdt-flip)))
      (should (eq 'red (cdr (assq :foreground a1)))))
    ;; Re-defface with a different color → cache flushed → new value.
    (emacs-redisplay-defface 'rdt-flip '(:foreground "green"))
    (let ((a2 (emacs-redisplay-realize-face 'rdt-flip)))
      (should (eq 'green (cdr (assq :foreground a2)))))))

(ert-deftest emacs-redisplay-test-glyph-carries-realized-face ()
  "`emacs-redisplay-redisplay-window' populates glyph realized-face slot.
We attach a `face' text-property to one cell and verify the realized
form is a backend-ready alist (not the raw plist)."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "hello"
        ;; Mark cell 2 (= "e") with a `face' property.
        (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0))
                 (vec (emacs-redisplay-glyph-row-glyphs row))
                 (g1  (aref vec 1)))
            (let ((realized (emacs-redisplay-glyph-realized-face g1)))
              (should (consp realized))
              (should (eq 'red (cdr (assq :foreground realized)))))))))))

(ert-deftest emacs-redisplay-test-flush-emits-sgr-for-realized-face ()
  "flush-frame routes the realized face into the backend → SGR escape."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "ab"
        (emacs-buffer-put-text-property 1 3 'face '(:foreground "red") b)
        (emacs-redisplay-test--with-capture
          (let* ((bk (emacs-tui-backend-init))
                 (fr (emacs-tui-backend-frame-create bk "frm"))
                 (h  (emacs-redisplay-init (list :backend bk)))
                 (w  (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (emacs-redisplay-redisplay-window h w)
            (emacs-redisplay-flush-frame h fr)
            ;; SGR 31 = ANSI red foreground (= 30 + 1 from color table).
            (should (string-match-p "\e\\[[^m]*31[^m]*m"
                                    emacs-redisplay-test--captured))))))))

(provide 'emacs-redisplay-test)

;;; emacs-redisplay-test.el ends here
