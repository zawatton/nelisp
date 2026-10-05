;;; emacs-redisplay-native-layout-test.el --- Native buffer layout checks -*- lexical-binding: t; -*-

;; Host buffers exercise the same public buffer/property interface as the
;; native standalone shim.  The parity harness supplies the terminal oracle.
(require 'ert)
(require 'emacs-redisplay)

(defmacro emacs-redisplay-native-test--with-window (text width height &rest body)
  (declare (indent 3))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (setq-local mode-line-format nil)
     (let* ((window (emacs-window--make :buffer (current-buffer)
                                      :total-cols ,width :total-lines ,height))
            (emacs-window--root window)
            (emacs-window--selected window)
            (handle (emacs-redisplay-init)))
       ,@body)))

(defun emacs-redisplay-native-test--row (matrix index)
  (let ((row (aref (emacs-redisplay-glyph-matrix-rows matrix) index)))
    (mapconcat (lambda (glyph)
                 (if glyph (string (emacs-redisplay-glyph-char glyph)) " "))
               (append (emacs-redisplay-glyph-row-glyphs row) nil) "")))

(ert-deftest emacs-redisplay-native-layout/edit-invalidates-text-cache ()
  (emacs-redisplay-native-test--with-window "abc" 10 3
    (should (equal "abc       " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))
    (erase-buffer)
    (insert "xy")
    (should (equal "xy        " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/options-invalidate-layout-cache ()
  (emacs-redisplay-native-test--with-window "abcdefghijkl" 10 3
    (setq-local truncate-lines nil)
    (should (equal "abcdefghi\\" (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))
    (setq-local truncate-lines t)
    (should (equal "abcdefghi$" (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))
    (setq-local header-line-format "Header")
    (should (equal "Header    " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/narrowing-invalidates-text-cache ()
  (emacs-redisplay-native-test--with-window "ab\ncd\nef" 10 3
    (emacs-redisplay-redisplay-window handle window)
    (narrow-to-region 4 6)
    (emacs-window-set-window-start window 4)
    (emacs-window-set-window-point window 4)
    (should (equal "cd        " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))
    (should (emacs-redisplay--ml-narrowed-p (current-buffer)))
    (should (stringp (emacs-redisplay--ml-percent (current-buffer))))
    (widen)
    (emacs-window-set-window-start window 1)
    (emacs-window-set-window-point window 1)
    (should (equal "ab        " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/replacement-range-renders-once ()
  (emacs-redisplay-native-test--with-window "abcdef" 10 3
    (put-text-property 2 5 'display "XY")
    (should (equal "aXYef     " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/property-change-invalidates-cache ()
  (emacs-redisplay-native-test--with-window "abcd" 10 3
    (emacs-redisplay-redisplay-window handle window)
    (put-text-property 2 4 'display "X")
    (should (equal "aXd       " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/hidden-newline-joins-lines ()
  (emacs-redisplay-native-test--with-window "ab\ncd" 10 3
    (setq-local buffer-invisibility-spec t)
    (put-text-property 3 4 'invisible t)
    (should (equal "abcd      " (emacs-redisplay-native-test--row
                                (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/empty-overlay-strings ()
  (emacs-redisplay-native-test--with-window "abcd" 10 3
    (let ((overlay (make-overlay 3 3)))
      (overlay-put overlay 'before-string "<")
      (overlay-put overlay 'after-string ">")
      (should (equal "ab<>cd    " (emacs-redisplay-native-test--row
                                  (emacs-redisplay-redisplay-window handle window) 0))))))

(ert-deftest emacs-redisplay-native-layout/word-wrap-preserves-word ()
  (emacs-redisplay-native-test--with-window "aaa bbb ccc" 10 3
    (setq-local word-wrap t)
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      (should (equal "aaa bbb   " (emacs-redisplay-native-test--row matrix 0)))
      (should (equal "ccc       " (emacs-redisplay-native-test--row matrix 1))))))

(ert-deftest emacs-redisplay-native-layout/empty-rows-stay-sparse ()
  (emacs-redisplay-native-test--with-window "" 10 3
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      (dotimes (i 3)
        (let ((row (aref (emacs-redisplay-glyph-matrix-rows matrix) i)))
          (should (= 0 (emacs-redisplay-glyph-row-used row)))
          (should (equal (make-vector 10 nil) (emacs-redisplay-glyph-row-glyphs row))))))))

(provide 'emacs-redisplay-native-layout-test)
;;; emacs-redisplay-native-layout-test.el ends here
