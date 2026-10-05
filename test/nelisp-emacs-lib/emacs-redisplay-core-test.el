;;; emacs-redisplay-core-test.el --- row-painter regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-redisplay-core)

(ert-deftest emacs-redisplay-core-s22-new-window-clears-blank-rows ()
  ;; A new split may cover text painted by the formerly larger window.
  (should (equal (emacs-redisplay-core--dirty-rows nil ["" "body" ""] 3)
                 (bool-vector t t t))))

(ert-deftest emacs-redisplay-core-s22-existing-blank-row-stays-clean ()
  (let ((old (emacs-redisplay-core--make-matrix nil 80 1 [""] nil nil nil)))
    (should (equal (emacs-redisplay-core--dirty-rows old [""] 1)
                   (bool-vector nil)))))

(ert-deftest emacs-redisplay-core-s22-pads-display-columns ()
  (should (equal (emacs-redisplay-core--pad-row "日本" 6) "日本  "))
  (should (equal (emacs-redisplay-core--pad-row "" 3) "   "))
  (should (equal (emacs-redisplay-core--pad-row "abcdef" 3) "abc"))
  (should (equal (emacs-redisplay-core--pad-row "日本" 3) "日 ")))

(ert-deftest emacs-redisplay-core-s22-measuring-rows-preserves-match-data ()
  (string-match "b" "abc")
  (let ((saved (match-data)))
    (emacs-redisplay-core--pad-row "abc" 6)
    (should (equal (match-data) saved))))

(ert-deftest emacs-redisplay-core-s22-mode-line-cache-tracks-text-and-face ()
  (let ((emacs-window--root nil) (emacs-window--selected nil)
        (emacs-window--id-counter 0)
        (spans '(("status" . ((:reverse . t))))))
    (let* ((buffer (nelisp-ec-generate-new-buffer "*s22-mode-cache*"))
           (window (emacs-window-selected-window))
           (handle (emacs-redisplay-init)))
      (unwind-protect
          (cl-letf (((symbol-function 'emacs-redisplay-mode-line-spans)
                     (lambda (&rest _) spans)))
            (emacs-window-set-window-buffer window buffer)
            (emacs-redisplay-redisplay-window handle window)
            (emacs-redisplay-redisplay-window handle window)
            (let* ((matrix (emacs-redisplay-glyph-matrix handle window))
                   (row (1- (emacs-redisplay-glyph-matrix-height matrix))))
              (should-not (aref (emacs-redisplay-glyph-matrix-dirty-rows matrix) row))
              ;; Equal text with a changed face must still be repainted.
              (setq spans '(("status" . ((:bold . t)))))
              (emacs-redisplay-redisplay-window handle window)
              (should (aref (emacs-redisplay-glyph-matrix-dirty-rows
                             (emacs-redisplay-glyph-matrix handle window)) row))
              (setq spans '(("updated" . ((:bold . t)))))
              (emacs-redisplay-redisplay-window handle window)
              (should (string-prefix-p "updated"
                       (aref (emacs-redisplay-glyph-matrix-rows
                              (emacs-redisplay-glyph-matrix handle window)) row)))))
        (nelisp-ec-kill-buffer buffer)))))

(provide 'emacs-redisplay-core-test)
