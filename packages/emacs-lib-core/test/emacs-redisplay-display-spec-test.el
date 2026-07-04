;;; emacs-redisplay-display-spec-test.el --- ERT for display-spec rendering (Doc 06 E3)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 06 E3: render a `display' text property that is a replacement string (or
;; an image's :string fallback) in place of the buffer char, and expose
;; raise/height attributes for backends.  Separate file (the main
;; emacs-redisplay-test.el is pre-broken by unrelated dirty edits).

;;; Code:

(require 'ert)
(require 'emacs-redisplay)

;;;; --- pure helpers ---------------------------------------------------

(ert-deftest emacs-redisplay-display-spec-test/replacement-string ()
  "`--display-replacement-string' extracts the text a display spec renders."
  (should (equal "x" (emacs-redisplay--display-replacement-string "x")))
  (should (equal "IMG"
                 (emacs-redisplay--display-replacement-string
                  '(image :type png :string "IMG"))))
  ;; A real image with no :string fallback is a backend placeholder → nil.
  (should-not (emacs-redisplay--display-replacement-string
               '(image :type png :file "/x.png")))
  ;; A list of specs → first replacement string.
  (should (equal "rep"
                 (emacs-redisplay--display-replacement-string
                  '((raise 0.5) "rep"))))
  ;; Width / attribute specs do not replace text.
  (should-not (emacs-redisplay--display-replacement-string '(space :width 3)))
  (should-not (emacs-redisplay--display-replacement-string '(raise 0.5)))
  (should-not (emacs-redisplay--display-replacement-string nil)))

(ert-deftest emacs-redisplay-display-spec-test/attribute ()
  "`--display-attribute' extracts raise / height factors."
  (should (= 0.5 (emacs-redisplay--display-attribute '(raise 0.5) 'raise)))
  (should (= 2.0 (emacs-redisplay--display-attribute '(height 2.0) 'height)))
  (should (= 0.3 (emacs-redisplay--display-attribute '((raise 0.3) "x") 'raise)))
  (should-not (emacs-redisplay--display-attribute '(space :width 3) 'raise))
  (should-not (emacs-redisplay--display-attribute '(raise 0.5) 'height)))

;;;; --- glyph-level rendering -----------------------------------------

(ert-deftest emacs-redisplay-display-spec-test/replacement-rendered-in-glyphs ()
  "A `display' string replaces the buffer char with its glyphs in the laid-out
row, all anchored at the source buffer position (Doc 06 E3)."
  (let ((b (nelisp-ec-generate-new-buffer "disp")))
    (unwind-protect
        (progn
          (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "abc"))
          ;; The char at pos 2 ("b") renders as "XY".
          (emacs-buffer-put-text-property 2 3 'display "XY" b)
          (let* ((laid (emacs-redisplay--lay-out-line "abc" 1 b nil 80))
                 (vec (car laid)))
            (should (= ?a (emacs-redisplay-glyph-char (aref vec 0))))
            (should (= ?X (emacs-redisplay-glyph-char (aref vec 1))))
            (should (= ?Y (emacs-redisplay-glyph-char (aref vec 2))))
            (should (= ?c (emacs-redisplay-glyph-char (aref vec 3))))
            ;; Both replacement glyphs anchor at the original buffer position.
            (should (= 2 (emacs-redisplay-glyph-buf-pos (aref vec 1))))
            (should (= 2 (emacs-redisplay-glyph-buf-pos (aref vec 2))))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-redisplay-display-spec-test/image-string-fallback-rendered ()
  "An image display spec with a :string fallback renders that text (TTY)."
  (let ((b (nelisp-ec-generate-new-buffer "disp-img")))
    (unwind-protect
        (progn
          (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "ab"))
          (emacs-buffer-put-text-property 1 2 'display '(image :string "I") b)
          (let* ((laid (emacs-redisplay--lay-out-line "ab" 1 b nil 80))
                 (vec (car laid)))
            (should (= ?I (emacs-redisplay-glyph-char (aref vec 0))))
            (should (= ?b (emacs-redisplay-glyph-char (aref vec 1))))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(ert-deftest emacs-redisplay-display-spec-test/raise-keeps-char ()
  "A `(raise ...)' display spec layers onto the text — the char is unchanged
and the spec rides along on the glyph for a GUI backend (Doc 06 E3)."
  (let ((b (nelisp-ec-generate-new-buffer "disp-raise")))
    (unwind-protect
        (progn
          (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert "ab"))
          (emacs-buffer-put-text-property 1 2 'display '(raise 0.5) b)
          (let* ((laid (emacs-redisplay--lay-out-line "ab" 1 b nil 80))
                 (vec (car laid)))
            (should (= ?a (emacs-redisplay-glyph-char (aref vec 0))))
            (should (equal '(raise 0.5)
                           (emacs-redisplay-glyph-display-spec (aref vec 0))))))
      (when (fboundp 'nelisp-ec-kill-buffer) (nelisp-ec-kill-buffer b)))))

(provide 'emacs-redisplay-display-spec-test)
;;; emacs-redisplay-display-spec-test.el ends here
