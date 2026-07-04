;;; emacs-redisplay-line-wrap-test.el --- ERT for continuation lines (Doc 06 E1)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Separate from emacs-redisplay-test.el so the E1 line-wrap / continuation
;; coverage runs independently.  Tests the pure wrap-point computation
;; (`emacs-redisplay--wrap-line-segments') and the visual-line expansion
;; (`emacs-redisplay--split-into-visual-lines') against Emacs char-wrap /
;; word-wrap semantics.

;;; Code:

(require 'ert)
(require 'emacs-redisplay)

(defun emacs-redisplay-line-wrap-test--seg-width (s)
  "Display width of S using the redisplay char-width (no TABs in tests)."
  (let ((w 0) (i 0) (n (length s)))
    (while (< i n)
      (setq w (+ w (max 1 (emacs-redisplay--char-width (aref s i)))))
      (setq i (1+ i)))
    w))

(ert-deftest emacs-redisplay-line-wrap-test/char-wrap-basics ()
  "Char-wrap splits at exact column boundaries (Doc 06 E1)."
  (should (equal '("abc" "def" "gh")
                 (emacs-redisplay--wrap-line-segments "abcdefgh" 3)))
  (should (equal '("abc" "d")
                 (emacs-redisplay--wrap-line-segments "abcd" 3)))
  (should (equal '("abc")
                 (emacs-redisplay--wrap-line-segments "abc" 3)))
  (should (equal '("ab")
                 (emacs-redisplay--wrap-line-segments "ab" 3)))
  (should (equal '("")
                 (emacs-redisplay--wrap-line-segments "" 3))))

(ert-deftest emacs-redisplay-line-wrap-test/reconstruct-and-bound ()
  "For every width, concatenating the segments rebuilds the line and no
segment exceeds the width (Doc 06 E1)."
  (let ((line "abcdefghij"))
    (dotimes (w0 10)
      (let* ((w (1+ w0))
             (segs (emacs-redisplay--wrap-line-segments line w)))
        (should (equal line (apply #'concat segs)))
        (dolist (s segs)
          (should (<= (emacs-redisplay-line-wrap-test--seg-width s) w)))))))

(ert-deftest emacs-redisplay-line-wrap-test/cjk-width-aware ()
  "Wide (CJK) chars are never split across a row and each segment fits."
  (let* ((line "あいうえお")
         (segs (emacs-redisplay--wrap-line-segments line 4)))
    (should (equal line (apply #'concat segs)))
    (dolist (s segs)
      (should (<= (emacs-redisplay-line-wrap-test--seg-width s) 4)))
    ;; 5 wide chars (width 2 each) at width 4 → 2+2+1 chars per row.
    (when (= 2 (emacs-redisplay--char-width ?あ))
      (should (equal '("あい" "うえ" "お") segs)))))

(ert-deftest emacs-redisplay-line-wrap-test/word-wrap ()
  "Word-wrap breaks at whitespace and never splits a word that fits a row."
  (should (equal '("aaa bbb" " ccc")
                 (emacs-redisplay--wrap-line-segments "aaa bbb ccc" 7 t)))
  ;; A word longer than the width falls back to char-wrap.
  (should (equal '("abc" "def" "gh")
                 (emacs-redisplay--wrap-line-segments "abcdefgh" 3 t)))
  ;; Reconstruction always holds for word-wrap too.
  (should (equal "aaaa bbbb cccc"
                 (apply #'concat
                        (emacs-redisplay--wrap-line-segments
                         "aaaa bbbb cccc" 6 t)))))

(ert-deftest emacs-redisplay-line-wrap-test/visual-lines-truncate ()
  "With truncate-lines, each logical line stays one (LINE NL nil) row."
  (let ((emacs-redisplay-truncate-lines t))
    (should (equal '(("abcdefgh" 0 nil))
                   (emacs-redisplay--split-into-visual-lines "abcdefgh" 3)))))

(ert-deftest emacs-redisplay-line-wrap-test/visual-lines-wrap ()
  "Without truncate-lines, an over-width line expands into continuation rows."
  (let ((emacs-redisplay-truncate-lines nil)
        (emacs-redisplay-word-wrap nil))
    (should (equal '(("abc" 0 nil) ("def" 0 t) ("gh" 0 t))
                   (emacs-redisplay--split-into-visual-lines "abcdefgh" 3)))
    ;; The newline lands on the LAST visual row of the logical line.
    (should (equal '(("abc" 0 nil) ("def" 0 t) ("gh" 1 t) ("x" 0 nil))
                   (emacs-redisplay--split-into-visual-lines "abcdefgh\nx" 3)))))

(provide 'emacs-redisplay-line-wrap-test)
;;; emacs-redisplay-line-wrap-test.el ends here
