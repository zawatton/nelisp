;;; emacs-redisplay-test.el --- ERT for emacs-redisplay.el  -*- lexical-binding: t; -*-

;; Phase 3 module ERT per nelisp-emacs Doc 01 (LOCKED v2 §3.3),
;; mirroring NeLisp Doc 43 v2 §3.2 Phase 11.B redisplay engine MVP.
;; Phase 3.B.1 (face-realize MVP per Doc 43 §2.4) adds section G below.
;; Phase 3.B.2 (overlay before/after-string emission) adds section H.
;;
;; Coverage:
;;   A. driver lifecycle  (init / shutdown / handlep + version consts)
;;   B. text → glyph      (text-to-glyphs, char/face/buf-pos preservation)
;;   C. matrix building   (empty / multi-line / window-narrow / TAB)
;;   D. dirty tracking    (mark-window-dirty / mark-frame-dirty,
;;                         force-mode-line-update / redraw-display)
;;   E. backend wiring    (flush-frame writes to TUI canvas, set-cursor)
;;   F. cross-cutting     (handle errors, narrowed visible, overlays)
;;                         + invisible text/overlay suppression
;;   G. face-realize MVP  (Phase 3.B.1, Doc 43 §2.4) — registry, plist
;;                         + symbol + cascade resolution, color string
;;                         normalization, weight→bold, glyph realized-
;;                         face slot, overlay merge realize, SGR emit
;;   H. overlay strings   (Phase 3.B.2) — before-string / after-string
;;                         emission, face propagation, nil/empty graceful
;;                         handling, multi-overlay priority ordering,
;;                         mid-line anchor, end-of-line after-string

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

;; --- Phase 3.C.1 gate #2: window scroll re-fill after redisplay ---

(ert-deftest emacs-redisplay-test-scroll-after-redisplay-shifts-row-content ()
  "Changing window-start after redisplay #1 makes redisplay #2 re-fill rows."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta\ngamma\ndelta"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        ;; Initial state: window-start = 1 → row 0 is "alpha".
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (string= "alpha"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0)))))
        ;; Now scroll: window-start = 7 (= start of "beta").
        (emacs-window-set-window-start w 7)
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (string= "beta"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0))))
          (should (string= "gamma"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 1)))))))))

(ert-deftest emacs-redisplay-test-scroll-back-restores-row-content ()
  "Scrolling away then back rebuilds the original visible region."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta\ngamma\ndelta"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (emacs-redisplay-redisplay-window h w)
        ;; Scroll to "beta", then back to "alpha".
        (emacs-window-set-window-start w 7)
        (emacs-redisplay-redisplay-window h w)
        (emacs-window-set-window-start w 1)
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (string= "alpha"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 0))))
          (should (string= "beta"
                           (emacs-redisplay-glyph-row-text
                            (emacs-redisplay-glyph-row m 1)))))))))

(ert-deftest emacs-redisplay-test-scroll-after-redisplay-marks-rows-dirty ()
  "Scroll mutates window-start so the fingerprint changes and rows dirty."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta\ngamma\ndelta"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          ;; Scroll → fingerprint must invalidate.
          (emacs-window-set-window-start w 7)
          (emacs-redisplay-redisplay-window h w)
          (should (> (emacs-redisplay-flush-frame h fr) 0)))))))

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

;;; D. dirty tracking / diff redraw / trigger handlers (7 tests)

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

(ert-deftest emacs-redisplay-test-redisplay-unchanged-row-skips-flush ()
  "A second identical redisplay leaves row dirty bits clear."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "stable"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (should (> (emacs-redisplay-flush-frame h fr) 0))
          (emacs-redisplay-redisplay-window h w)
          (should (= 0 (emacs-redisplay-flush-frame h fr))))))))

(ert-deftest emacs-redisplay-test-redisplay-fingerprint-stamps-matrix ()
  "After redisplay, the matrix carries a non-nil fingerprint slot."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "fp"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let ((m (emacs-redisplay-redisplay-window h w)))
          (should (emacs-redisplay-glyph-matrix-fingerprint m)))))))

(ert-deftest emacs-redisplay-test-redisplay-short-circuits-on-equal-fingerprint ()
  "When inputs are unchanged, redisplay skips rebuild — every row stays nil."
  ;; Setup: redisplay once, flush (clears dirty bits), record cursor identity,
  ;; redisplay again on the unchanged buffer.  The second call must NOT raise
  ;; any dirty bit because it short-circuits.
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "short-circuit"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
            (dotimes (r (length dirty))
              (should-not (aref dirty r)))))))))

(ert-deftest emacs-redisplay-test-redisplay-rebuilds-when-buffer-grows ()
  "Buffer-size change invalidates the fingerprint and forces rebuild."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "ab"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-goto-char 3)
            (nelisp-ec-insert "c"))
          (emacs-redisplay-redisplay-window h w)
          ;; Some row must be dirty (= row containing the change).
          (should (> (emacs-redisplay-flush-frame h fr) 0)))))))

(ert-deftest emacs-redisplay-test-redisplay-changed-row-flushes-again ()
  "A changed row hash is marked dirty on the next redisplay pass."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "before"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-delete-region 1 (1+ (length "before")))
            (nelisp-ec-insert "after"))
          (emacs-redisplay-redisplay-window h w)
          (should (> (emacs-redisplay-flush-frame h fr) 0))
          (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                 (row (aref canvas 0)))
            (should (eq ?a (car (aref row 0))))))))))

;; --- Phase 3.C.1 gate #1: text-property face change → dirty propagation ---

(ert-deftest emacs-redisplay-test-face-change-after-redisplay-marks-row-dirty ()
  "put-text-property-after-redisplay invalidates the cached fingerprint."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "abcd"
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          ;; Clear dirty bits via a simulated flush (mark all nil).
          (let* ((m (emacs-redisplay-glyph-matrix h w))
                 (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
            (dotimes (r (length dirty)) (aset dirty r nil)))
          ;; Now mutate a text-property face — same buffer size + point.
          (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
          (emacs-redisplay-redisplay-window h w)
          (let* ((m (emacs-redisplay-glyph-matrix h w))
                 (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
            ;; Row 0 holds "abcd" — its hash must have changed.
            (should (aref dirty 0))))))))

(ert-deftest emacs-redisplay-test-face-change-after-redisplay-flushes-new-sgr ()
  "Changing a `face' property after redisplay re-emits the new SGR escape."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "abcd"
        (emacs-redisplay-test--with-capture
          (let* ((bk (emacs-tui-backend-init))
                 (fr (emacs-tui-backend-frame-create bk "frm"))
                 (h  (emacs-redisplay-init (list :backend bk)))
                 (w  (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (emacs-redisplay-redisplay-window h w)
            (emacs-redisplay-flush-frame h fr)
            (setq emacs-redisplay-test--captured "")
            (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
            (emacs-redisplay-redisplay-window h w)
            (should (> (emacs-redisplay-flush-frame h fr) 0))
            ;; SGR 31 = ANSI red foreground.
            (should (string-match-p "\e\\[[^m]*31[^m]*m"
                                    emacs-redisplay-test--captured))))))))

(ert-deftest emacs-redisplay-test-remove-text-property-also-marks-row-dirty ()
  "remove-text-properties bumps modified-tick so dirty propagates."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "abcd"
        (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (let* ((m (emacs-redisplay-glyph-matrix h w))
                 (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
            (dotimes (r (length dirty)) (aset dirty r nil)))
          (emacs-buffer-remove-text-properties 2 3 '(face) b)
          (emacs-redisplay-redisplay-window h w)
          (let* ((m (emacs-redisplay-glyph-matrix h w))
                 (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
            (should (aref dirty 0))))))))

;; --- Phase 3.B.6 row-incremental rebuild: skip path correctness ---

(ert-deftest emacs-redisplay-test-row-incremental-skips-unchanged-rows ()
  "Single-line edit: only the changed row is rebuilt, others reuse glyphs."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta\ngamma\ndelta"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window))
             (rebuild-count 0))
        (emacs-window-set-window-buffer w b)
        (emacs-redisplay-redisplay-window h w)
        ;; Spy on --clear-row to count actual row rebuilds.
        (advice-add 'emacs-redisplay--clear-row :before
                    (lambda (&rest _) (cl-incf rebuild-count)))
        (unwind-protect
            (progn
              ;; Edit only line 1 ("beta" → "betaX").
              (let ((nelisp-ec--current-buffer b))
                (nelisp-ec-goto-char 11) ;; end of "beta"
                (nelisp-ec-insert "X"))
              (setq rebuild-count 0)
              (emacs-redisplay-redisplay-window h w)
              ;; Only row 1 should have been cleared+rebuilt; row 0/2/3
              ;; reuse cached glyphs.  Mode-line row may also be cleared
              ;; (cache miss on first call after dim change), but normal
              ;; static buffer keeps it stable.
              (should (<= rebuild-count 2)))
          (advice-remove 'emacs-redisplay--clear-row
                         (lambda (&rest _) (cl-incf rebuild-count))))))))

(ert-deftest emacs-redisplay-test-row-incremental-shifts-buf-pos-via-pos-delta ()
  "After insert before a row, glyph effective-buf-pos reflects the shift."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row1 (emacs-redisplay-glyph-row m 1))
               (row1-glyph0 (aref (emacs-redisplay-glyph-row-glyphs row1) 0))
               (orig-bp (emacs-redisplay-glyph-buf-pos row1-glyph0)))
          ;; Insert at start (= shifts buffer positions for row 1 by +1).
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-goto-char 1)
            (nelisp-ec-insert "X"))
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row1 (emacs-redisplay-glyph-row m 1))
                 (row1-glyph0 (aref (emacs-redisplay-glyph-row-glyphs row1) 0))
                 (eff-bp (emacs-redisplay--effective-buf-pos row1 row1-glyph0)))
            ;; Effective position must reflect the +1 shift.
            (should (= (1+ orig-bp) eff-bp))))))))

(ert-deftest emacs-redisplay-test-text-tick-bumps-on-insert ()
  "Phase 3.B.7: nelisp-ec-insert advice bumps emacs-buffer text-tick."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (let ((tick0 (emacs-buffer-buffer-text-tick b)))
        (let ((nelisp-ec--current-buffer b))
          (nelisp-ec-goto-char 4)
          (nelisp-ec-insert "d"))
        (should (> (emacs-buffer-buffer-text-tick b) tick0))))))

(ert-deftest emacs-redisplay-test-text-tick-unchanged-by-text-property ()
  "Phase 3.B.7: text-property mutation does NOT bump text-tick."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (let ((tick0 (emacs-buffer-buffer-text-tick b)))
        (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
        (should (= (emacs-buffer-buffer-text-tick b) tick0))))))

(ert-deftest emacs-redisplay-test-buffer-string-cache-hits-on-stable-tick ()
  "Phase 3.B.7: cached buffer-string is reused when text-tick is stable."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha"
      (let* ((h (emacs-redisplay-init))
             (n-bs 0))
        (advice-add 'emacs-redisplay--buffer-string :before
                    (lambda (&rest _) (cl-incf n-bs)))
        (unwind-protect
            (progn
              (emacs-redisplay--cached-buffer-string h b)
              (should (= 1 n-bs))
              ;; Same buffer + same tick → reuse cached.
              (emacs-redisplay--cached-buffer-string h b)
              (should (= 1 n-bs))
              (emacs-redisplay--cached-buffer-string h b)
              (should (= 1 n-bs)))
          (advice-remove 'emacs-redisplay--buffer-string
                         (lambda (&rest _) (cl-incf n-bs))))))))

(ert-deftest emacs-redisplay-test-buffer-string-cache-misses-after-edit ()
  "Phase 3.B.7: cache key includes text-tick; edit invalidates it."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha"
      (let* ((h (emacs-redisplay-init))
             (s1 (emacs-redisplay--cached-buffer-string h b)))
        (let ((nelisp-ec--current-buffer b))
          (nelisp-ec-goto-char 6)
          (nelisp-ec-insert "X"))
        (let ((s2 (emacs-redisplay--cached-buffer-string h b)))
          (should-not (string= s1 s2))
          (should (string= "alphaX" s2)))))))

(ert-deftest emacs-redisplay-test-row-incremental-cursor-after-shift ()
  "cursor-for-point lands correctly even after a skip-path row shift."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (emacs-redisplay-redisplay-window h w)
        (let ((nelisp-ec--current-buffer b))
          (nelisp-ec-goto-char 1)
          (nelisp-ec-insert "X"))
        ;; Place cursor on "e" of "beta" (now at pos 9 after insert).
        (emacs-window-set-window-point w 9)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (cur (emacs-redisplay-glyph-matrix-cursor m)))
          ;; Row 1 column 1 = "e" of "beta".
          (should (consp cur))
          (should (= 1 (car cur)))
          (should (= 1 (cdr cur))))))))

(ert-deftest emacs-redisplay-test-force-mode-line-update-redraws-mode-line ()
  "force-mode-line-update makes an unchanged mode-line row flush again."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "body"
      (emacs-buffer-set-buffer-local-value 'mode-line-format b " ML:%b ")
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (setf (emacs-window-total-lines w) 3)
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (should (> (emacs-redisplay-flush-frame h fr) 0))
          (emacs-redisplay-redisplay-window h w)
          (should (= 0 (emacs-redisplay-flush-frame h fr)))
          (should (eq t (emacs-redisplay-force-mode-line-update h nil w)))
          (emacs-redisplay-redisplay-window h w)
          (should (> (emacs-redisplay-flush-frame h fr) 0))
          (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                 (row (aref canvas 2))
                 (text (apply #'string
                              (cl-loop for i below 7
                                       collect (car (aref row i))))))
            (should (string= " ML:rd-" text))))))))

(ert-deftest emacs-redisplay-test-redraw-display-invalidates-all-windows ()
  "redraw-display drops cached matrices and runs a full redisplay pass."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let ((m1 (emacs-redisplay-redisplay-window h w)))
          (should (= 1 (emacs-redisplay-redraw-display h)))
          (let ((m2 (emacs-redisplay-glyph-matrix h w)))
            (should (emacs-redisplay-glyph-matrix-p m2))
            (should-not (eq m1 m2))))))))

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
;;; H'. mouse-face (C1 v2 increment — Doc 15)

(ert-deftest emacs-redisplay-test-mouse-face-text-property ()
  "A `mouse-face' text property is copied into the glyph mouse-face slot."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (emacs-buffer-put-text-property 2 3 'mouse-face 'highlight b)
      (let* ((h (emacs-redisplay-init))
             (g (emacs-redisplay-text-to-glyphs h b)))
        ;; pos 2 = "b" = glyph 1 carries mouse-face; neighbours do not
        (should-not (emacs-redisplay-glyph-mouse-face (aref g 0)))
        (should (eq 'highlight (emacs-redisplay-glyph-mouse-face (aref g 1))))
        (should-not (emacs-redisplay-glyph-mouse-face (aref g 2)))))))

(ert-deftest emacs-redisplay-test-mouse-face-overlay ()
  "Overlay `mouse-face' is merged into the glyph mouse-face slot.
Mocks the overlay accessor trio (as the overlay-face test does) so the
merge is exercised without the upstream `nelisp-ovly' storage layer."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (let ((mock-ov 'mock-overlay))
        (cl-letf (((symbol-function 'emacs-redisplay--overlays-in)
                   (lambda (_beg _end &optional _buffer) (list mock-ov)))
                  ((symbol-function 'emacs-redisplay--ovly-bounds)
                   (lambda (ov) (when (eq ov mock-ov) (cons 2 4))))
                  ((symbol-function 'emacs-redisplay--ovly-prop)
                   (lambda (ov prop)
                     (when (and (eq ov mock-ov) (eq prop 'mouse-face))
                       'highlight))))
          (let* ((h (emacs-redisplay-init))
                 (w (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (let* ((m   (emacs-redisplay-redisplay-window h w))
                   (row (emacs-redisplay-glyph-row m 0))
                   (vec (emacs-redisplay-glyph-row-glyphs row)))
              ;; overlay spans pos 2..4 ("b","c") = glyphs 1,2
              (should-not (emacs-redisplay-glyph-mouse-face (aref vec 0)))
              (should (eq 'highlight
                          (emacs-redisplay-glyph-mouse-face (aref vec 1))))
              (should (eq 'highlight
                          (emacs-redisplay-glyph-mouse-face (aref vec 2)))))))))))

(ert-deftest emacs-redisplay-test-mouse-face-overlay-overrides-text-property ()
  "An overlay `mouse-face' overrides the text-property mouse-face by priority."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (emacs-buffer-put-text-property 2 3 'mouse-face 'from-text b)
      (let ((mock-ov 'mock-overlay))
        (cl-letf (((symbol-function 'emacs-redisplay--overlays-in)
                   (lambda (_beg _end &optional _buffer) (list mock-ov)))
                  ((symbol-function 'emacs-redisplay--ovly-bounds)
                   (lambda (ov) (when (eq ov mock-ov) (cons 2 3))))
                  ((symbol-function 'emacs-redisplay--ovly-prop)
                   (lambda (ov prop)
                     (when (and (eq ov mock-ov) (eq prop 'mouse-face))
                       'from-overlay))))
          (let* ((h (emacs-redisplay-init))
                 (w (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (let* ((m   (emacs-redisplay-redisplay-window h w))
                   (row (emacs-redisplay-glyph-row m 0))
                   (vec (emacs-redisplay-glyph-row-glyphs row)))
              (should (eq 'from-overlay
                          (emacs-redisplay-glyph-mouse-face (aref vec 1)))))))))))


(ert-deftest emacs-redisplay-test-text-to-glyphs-skips-invisible-property ()
  "The Phase 3 MVP `invisible' text property suppresses glyph output."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (emacs-buffer-put-text-property 2 4 'invisible t b)
      (let* ((h (emacs-redisplay-init))
             (g (emacs-redisplay-text-to-glyphs h b)))
        (should (= 2 (length g)))
        (should (eq ?a (emacs-redisplay-glyph-char (aref g 0))))
        (should (eq 1  (emacs-redisplay-glyph-buf-pos (aref g 0))))
        (should (eq ?d (emacs-redisplay-glyph-char (aref g 1))))
        (should (eq 4  (emacs-redisplay-glyph-buf-pos (aref g 1))))))))

(ert-deftest emacs-redisplay-test-redisplay-skips-invisible-property ()
  "redisplay-window omits characters with non-nil `invisible'."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcde"
      (emacs-buffer-put-text-property 2 5 'invisible t b)
      (let* ((h (emacs-redisplay-init))
             (w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b)
        (let* ((m (emacs-redisplay-redisplay-window h w))
               (row (emacs-redisplay-glyph-row m 0)))
          (should (string= "ae" (emacs-redisplay-glyph-row-text row)))
          (should (= 2 (emacs-redisplay-glyph-row-used row))))))))

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

;;; H. overlay before-string / after-string (Phase 3.B.2) — 7 tests

(defmacro emacs-redisplay-test--with-mock-overlays (overlays-spec &rest body)
  "Run BODY with mocked overlay accessors driven by OVERLAYS-SPEC.
OVERLAYS-SPEC is a list of plists, one per mock overlay, with keys:
  :id     SYMBOL  (= the overlay identity returned by the accessors)
  :start  INT     (= overlay start, inclusive)
  :end    INT     (= overlay end, exclusive)
  :face   FACE    (optional)
  :before STR     (optional)
  :after  STR     (optional)
  :invisible VAL  (optional)
  :prio   INT     (optional, default 0)

The mocks teach `emacs-redisplay--overlays-in' to return every defined
overlay (Phase 3.B.2 callers filter by start/end internally)."
  (declare (indent 1) (debug (form body)))
  (let ((spec (gensym "spec-")))
    `(let* ((,spec ,overlays-spec))
       (cl-letf (((symbol-function 'emacs-redisplay--overlays-in)
                  (lambda (_b _e &optional _buf)
                    (mapcar (lambda (o) (plist-get o :id)) ,spec)))
                 ((symbol-function 'emacs-redisplay--ovly-bounds)
                  (lambda (ov)
                    (cl-loop for o in ,spec
                             when (eq (plist-get o :id) ov)
                             return (cons (plist-get o :start)
                                          (plist-get o :end)))))
                 ((symbol-function 'emacs-redisplay--ovly-prop)
                  (lambda (ov prop)
                    (cl-loop for o in ,spec
                             when (eq (plist-get o :id) ov)
                             return
                             (cond
                              ((eq prop 'face)          (plist-get o :face))
                              ((eq prop 'before-string) (plist-get o :before))
                              ((eq prop 'after-string)  (plist-get o :after))
                              ((eq prop 'invisible)     (plist-get o :invisible))
                              ((eq prop 'priority)      (plist-get o :prio))
                              (t nil))))))
         ,@body))))

(ert-deftest emacs-redisplay-test-overlay-before-string-emits-glyphs ()
  "Overlay :before-string is emitted as glyphs *before* the buffer char."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov1 :start 2 :end 3 :before "[X]"))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            ;; Expected painted prefix: "a" + "[X]" + "bc"
            (should (string-match-p "^a\\[X\\]bc"
                                    (emacs-redisplay-glyph-row-text row)))
            (should (>= (emacs-redisplay-glyph-row-used row) 6))))))))

(ert-deftest emacs-redisplay-test-overlay-after-string-emits-glyphs ()
  "Overlay :after-string is emitted as glyphs *after* the buffer char.
The overlay's exclusive end is 3 (= just past `b'), so the after-string
emits between the `b' and the `c'."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov1 :start 2 :end 3 :after ">>"))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            (should (string-match-p "^ab>>c"
                                    (emacs-redisplay-glyph-row-text row)))))))))

(ert-deftest emacs-redisplay-test-overlay-before-string-face-propagates ()
  "Overlay :before-string glyphs carry the overlay's `face' property."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "ab"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov1 :start 1 :end 2
                      :before "X" :face 'highlight))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0))
                 (vec (emacs-redisplay-glyph-row-glyphs row))
                 ;; Glyph 0 is the injected before-string "X".
                 (g0 (aref vec 0)))
            (should (eq ?X (emacs-redisplay-glyph-char g0)))
            (let ((f (emacs-redisplay-glyph-face g0)))
              (should (or (eq f 'highlight)
                          (and (listp f) (memq 'highlight f)))))))))))

(ert-deftest emacs-redisplay-test-overlay-after-string-face-propagates ()
  "Overlay :after-string glyphs carry the overlay's `face' property."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "ab"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov1 :start 1 :end 2
                      :after "Z" :face 'warning))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0))
                 (vec (emacs-redisplay-glyph-row-glyphs row))
                 ;; Layout: "a" (0), "Z" injected at (1), "b" (2).
                 (g1 (aref vec 1)))
            (should (eq ?Z (emacs-redisplay-glyph-char g1)))
            (let ((f (emacs-redisplay-glyph-face g1)))
              (should (or (eq f 'warning)
                          (and (listp f) (memq 'warning f)))))))))))

(ert-deftest emacs-redisplay-test-overlay-empty-strings-are-graceful ()
  "Empty / nil :before-string and :after-string emit no glyphs."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov-empty :start 2 :end 3 :before "" :after "")
                (list :id 'ov-nil   :start 1 :end 2 :before nil :after nil))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            ;; Painted text equals raw buffer text, no extra glyphs.
            (should (string= "abc"
                             (emacs-redisplay-glyph-row-text row)))
            (should (= 3 (emacs-redisplay-glyph-row-used row)))))))))

(ert-deftest emacs-redisplay-test-overlay-invisible-suppresses-covered-text ()
  "Overlay `invisible' suppresses covered buffer glyphs."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcd"
      (emacs-redisplay-test--with-mock-overlays
          '((:id ov-hide :start 2 :end 4 :invisible t))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            (should (string= "ad" (emacs-redisplay-glyph-row-text row)))
            (should (= 2 (emacs-redisplay-glyph-row-used row)))))))))

(ert-deftest emacs-redisplay-test-overlay-multiple-before-strings-priority ()
  "Multiple :before-string overlays at the same start emit in priority
order: lower priority first → higher priority *closest* to the buffer
char.  So spec [(prio=10 \"H\") (prio=1 \"L\")] anchored at pos 1
yields painted prefix \"LHabc\"."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov-h :start 1 :end 4 :before "H" :prio 10)
                (list :id 'ov-l :start 1 :end 4 :before "L" :prio 1))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            (should (string-match-p "^LHabc"
                                    (emacs-redisplay-glyph-row-text row)))))))))

(ert-deftest emacs-redisplay-test-overlay-before-string-mid-line ()
  "Overlay :before-string anchored mid-line is emitted right before the
buffer char at the overlay's start position."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcde"
      (emacs-redisplay-test--with-mock-overlays
          (list (list :id 'ov1 :start 3 :end 4 :before "[!]"))
        (let* ((h (emacs-redisplay-init))
               (w (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (let* ((m (emacs-redisplay-redisplay-window h w))
                 (row (emacs-redisplay-glyph-row m 0)))
            (should (string-match-p "^ab\\[!\\]cde"
                                    (emacs-redisplay-glyph-row-text row)))))))))

;;; I. Phase 3.B.3 — color spec parser + realize layer for 256/truecolor

(ert-deftest emacs-redisplay-test-parse-color-spec-nil-and-unspecified ()
  "Parser returns nil for nil / `unspecified' (= no SGR override)."
  (should (eq nil (emacs-redisplay--parse-color-spec nil)))
  (should (eq nil (emacs-redisplay--parse-color-spec 'unspecified))))

(ert-deftest emacs-redisplay-test-parse-color-spec-16-symbol ()
  "Parser maps a plain symbol to a 16-color descriptor."
  (let ((p (emacs-redisplay--parse-color-spec 'red)))
    (should (eq 16 (plist-get p :type)))
    (should (eq 'red (plist-get p :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-hex-string-truecolor ()
  "Parser maps `#ff0000' to truecolor (255 0 0)."
  (let ((p (emacs-redisplay--parse-color-spec "#ff0000")))
    (should (eq 'truecolor (plist-get p :type)))
    (should (equal '(255 0 0) (plist-get p :value))))
  (let ((p (emacs-redisplay--parse-color-spec "#01ABcd")))
    ;; mixed case + non-zero G/B
    (should (eq 'truecolor (plist-get p :type)))
    (should (equal '(1 171 205) (plist-get p :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-rgb-plist ()
  "Parser maps `(:r 0 :g 255 :b 128)' to truecolor (0 255 128)."
  (let ((p (emacs-redisplay--parse-color-spec '(:r 0 :g 255 :b 128))))
    (should (eq 'truecolor (plist-get p :type)))
    (should (equal '(0 255 128) (plist-get p :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-palette-list ()
  "Parser maps `(palette 200)' and `(palette . 17)' to 256-color."
  (let ((p1 (emacs-redisplay--parse-color-spec '(palette 200)))
        (p2 (emacs-redisplay--parse-color-spec '(palette . 17))))
    (should (eq 256 (plist-get p1 :type)))
    (should (= 200 (plist-get p1 :value)))
    (should (eq 256 (plist-get p2 :type)))
    (should (= 17 (plist-get p2 :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-palette-keyword ()
  "Parser maps `:palette-42' keyword to 256-color descriptor."
  (let ((p (emacs-redisplay--parse-color-spec :palette-42)))
    (should (eq 256 (plist-get p :type)))
    (should (= 42 (plist-get p :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-clamps ()
  "Parser clamps out-of-range integers to [0,255] (graceful degrade)."
  (let ((p1 (emacs-redisplay--parse-color-spec '(palette 999)))
        (p2 (emacs-redisplay--parse-color-spec '(:r 999 :g -1 :b 128))))
    (should (= 255 (plist-get p1 :value)))
    (should (equal '(255 0 128) (plist-get p2 :value)))))

(ert-deftest emacs-redisplay-test-parse-color-spec-invalid-shapes ()
  "Parser returns nil for shapes it cannot interpret (= robust)."
  (should (eq nil (emacs-redisplay--parse-color-spec "not-a-known-name")))
  (should (eq nil (emacs-redisplay--parse-color-spec "#zzzz")))
  (should (eq nil (emacs-redisplay--parse-color-spec '(:r 1 :g 2)))) ;; missing :b
  (should (eq nil (emacs-redisplay--parse-color-spec 42)))
  (should (eq nil (emacs-redisplay--parse-color-spec '(rgb 1 2)))))  ;; arity

(ert-deftest emacs-redisplay-test-realize-face-emits-truecolor-descriptor ()
  "Realize face propagates `#ff0000' as `(:foreground . (rgb 255 0 0))'."
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face '(:foreground "#ff0000"))))
    (should (equal '(rgb 255 0 0) (cdr (assq :foreground alist))))))

(ert-deftest emacs-redisplay-test-realize-face-emits-256-descriptor ()
  "Realize face propagates `(palette 200)' as `(:foreground . (palette 200))'."
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face
                '(:foreground (palette 200)))))
    (should (equal '(palette 200) (cdr (assq :foreground alist))))))

(ert-deftest emacs-redisplay-test-realize-face-emits-rgb-plist-descriptor ()
  "Realize face propagates `(:r 0 :g 255 :b 128)' as `(rgb 0 255 128)'."
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face
                '(:foreground (:r 0 :g 255 :b 128)))))
    (should (equal '(rgb 0 255 128) (cdr (assq :foreground alist))))))

(ert-deftest emacs-redisplay-test-realize-face-keeps-16-color-symbol ()
  "Realize face keeps the bare 16-color path unchanged (= regression)."
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face '(:foreground "red"))))
    (should (eq 'red (cdr (assq :foreground alist))))))

(ert-deftest emacs-redisplay-test-realize-face-invalid-color-degrades ()
  "Invalid color spec degrades to nil / `default' rather than crashing."
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face '(:foreground "#zzzz"))))
    ;; Unknown string degrades to `default', which is filtered out by
    ;; the SGR layer (= no escape emitted).
    (should (memq (cdr (assq :foreground alist)) '(nil default))))
  (emacs-redisplay-face-cache-clear)
  (let ((alist (emacs-redisplay-realize-face '(:foreground (palette nope)))))
    ;; Non-integer palette index → drop the foreground attribute.
    (should (null (assq :foreground alist)))))

(ert-deftest emacs-redisplay-test-face-realize-contract-version-bumped ()
  "Phase 3.B.3 bumps face-realize contract version 1 → 2."
  (should (= 2 emacs-redisplay-face-realize-contract-version)))

;;; J. Phase 3.C.1 integration smoke (TUI + xdisp full pipeline, gate #6)
;;
;; Each test exercises the FULL pipeline (= window-set-buffer →
;; redisplay-window → flush-frame → backend canvas) and asserts the
;; resulting canvas cell contents directly.  This is the visual-smoke
;; surface Doc 43 §3.2 "integration smoke ~20 cases" calls for.

(defun emacs-redisplay-test--row-string (canvas row n)
  "Return first N cells of canvas ROW (0-based) as a string of chars."
  (let* ((r (aref canvas row)))
    (apply #'string
           (cl-loop for i below n collect (car (aref r i))))))

(ert-deftest emacs-redisplay-test-smoke-multi-line-canvas ()
  "Multi-line buffer paints row 0 = first line, row 1 = second line."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "alpha\nbeta"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((canvas (emacs-tui-backend-frame-canvas fr)))
            (should (string= "alpha"
                             (emacs-redisplay-test--row-string canvas 0 5)))
            (should (string= "beta"
                             (emacs-redisplay-test--row-string canvas 1 4)))))))))

(ert-deftest emacs-redisplay-test-smoke-empty-buffer-canvas-blank ()
  "Empty buffer paints row 0 as all spaces."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b ""
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((canvas (emacs-tui-backend-frame-canvas fr)))
            (should (string= "          "
                             (emacs-redisplay-test--row-string canvas 0 10)))))))))

(ert-deftest emacs-redisplay-test-smoke-tab-expands-on-canvas ()
  "TAB character expands to multiple cells on the rendered canvas."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "a\tb"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                 (s (emacs-redisplay-test--row-string canvas 0 10)))
            ;; "a" then spaces until next tab stop, then "b" — "b" must land
            ;; later than column 1 (= TAB visibly expanded).
            (should (eq ?a (aref s 0)))
            (should (eq ?\s (aref s 1)))
            (should (cl-position ?b (substring s 1)))))))))

(ert-deftest emacs-redisplay-test-smoke-invisible-textprop-hidden-on-canvas ()
  "`invisible' text-property suppresses cells in the rendered canvas."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abcde"
      (emacs-buffer-put-text-property 2 5 'invisible t b)
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                 (s (emacs-redisplay-test--row-string canvas 0 5)))
            ;; "a" + "e" + 3 spaces (b/c/d hidden).
            (should (eq ?a (aref s 0)))
            (should (eq ?e (aref s 1)))
            (should (eq ?\s (aref s 2)))))))))

(ert-deftest emacs-redisplay-test-smoke-redisplay-then-edit-then-flush-canvas ()
  "Full cycle: redisplay → flush → edit → redisplay → flush updates canvas."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "abc"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((nelisp-ec--current-buffer b))
            (nelisp-ec-goto-char 4)
            (nelisp-ec-insert "DEF"))
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                 (s (emacs-redisplay-test--row-string canvas 0 6)))
            (should (string= "abcDEF" s))))))))

(ert-deftest emacs-redisplay-test-smoke-scroll-then-flush-canvas ()
  "After scroll, flush paints the new visible region into the canvas."
  (emacs-redisplay-test--with-fresh-world
    (emacs-redisplay-test--with-buffer b "first\nsecond\nthird"
      (emacs-redisplay-test--with-capture
        (let* ((bk (emacs-tui-backend-init))
               (fr (emacs-tui-backend-frame-create bk "frm"))
               (h  (emacs-redisplay-init (list :backend bk)))
               (w  (emacs-window-selected-window)))
          (emacs-window-set-window-buffer w b)
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (emacs-window-set-window-start w 7) ;; "second"
          (emacs-redisplay-redisplay-window h w)
          (emacs-redisplay-flush-frame h fr)
          (let ((canvas (emacs-tui-backend-frame-canvas fr)))
            (should (string= "second"
                             (emacs-redisplay-test--row-string canvas 0 6)))
            (should (string= "third"
                             (emacs-redisplay-test--row-string canvas 1 5)))))))))

(ert-deftest emacs-redisplay-test-smoke-face-change-then-flush-canvas-face ()
  "After put-text-property face change, the canvas cell carries new face."
  (emacs-redisplay-test--with-fresh-face-registry
    (emacs-redisplay-test--with-fresh-world
      (emacs-redisplay-test--with-buffer b "hello"
        (emacs-redisplay-test--with-capture
          (let* ((bk (emacs-tui-backend-init))
                 (fr (emacs-tui-backend-frame-create bk "frm"))
                 (h  (emacs-redisplay-init (list :backend bk)))
                 (w  (emacs-window-selected-window)))
            (emacs-window-set-window-buffer w b)
            (emacs-redisplay-redisplay-window h w)
            (emacs-redisplay-flush-frame h fr)
            (emacs-buffer-put-text-property 2 3 'face '(:foreground "red") b)
            (emacs-redisplay-redisplay-window h w)
            (emacs-redisplay-flush-frame h fr)
            (let* ((canvas (emacs-tui-backend-frame-canvas fr))
                   (row (aref canvas 0))
                   (cell-1 (aref row 1)))
              ;; The cell at column 1 (= "e") carries SOME face information.
              (should (eq ?e (car cell-1)))
              (should (cdr cell-1)))))))))

(provide 'emacs-redisplay-test)

;;; emacs-redisplay-test.el ends here
