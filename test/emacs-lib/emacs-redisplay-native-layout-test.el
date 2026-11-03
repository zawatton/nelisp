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


(ert-deftest emacs-redisplay-native-layout/mode-line-constructs ()
  (emacs-redisplay-native-test--with-window "body" 20 3
    (setq-local parity-field "literal %b")
    (setq-local parity-flag t)
    (should (equal "literal %b YES eval padded  abc"
                   (emacs-redisplay--mode-line-format-to-string
                    '("" parity-field " " (parity-flag "YES" "NO")
                      " " (:eval "eval") " " (8 "padded") (-3 "abcdef"))
                    (current-buffer))))))

(ert-deftest emacs-redisplay-native-layout/mode-line-flags-and-visual-column ()
  (emacs-redisplay-native-test--with-window "a\t日本" 20 3
    (setq-local tab-width 4)
    (goto-char (point-max))
    (setq-local buffer-read-only t)
    (should (equal "%* C8" (emacs-redisplay--mode-line-format-to-string
                            "%*%+ C%c" (current-buffer))))
    (set-buffer-modified-p nil)
    (should (equal "%%" (emacs-redisplay--mode-line-format-to-string
                         "%*%+" (current-buffer))))))

(ert-deftest emacs-redisplay-native-layout/header-eval-invalidates-cache ()
  (emacs-redisplay-native-test--with-window "body" 12 3
    (setq-local parity-header "first")
    (setq-local header-line-format '(:eval parity-header))
    (should (string-prefix-p "first" (emacs-redisplay-native-test--row
                                      (emacs-redisplay-redisplay-window handle window) 0)))
    (setq-local parity-header "second")
    (should (string-prefix-p "second" (emacs-redisplay-native-test--row
                                       (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/mode-line-active-and-inactive-faces ()
  (emacs-redisplay-native-test--with-window "body" 12 3
    (let ((emacs-redisplay--face-registry (make-hash-table :test 'eq))
          (emacs-redisplay--face-cache (make-hash-table :test 'equal))
          (other (emacs-window--make :buffer (current-buffer) :total-cols 12 :total-lines 3)))
      (setq-local mode-line-format "bar")
      (emacs-redisplay-defface 'mode-line '(:inverse-video t))
      (emacs-redisplay-defface 'mode-line-inactive '(:underline t :inverse-video nil))
      (let ((emacs-redisplay--mode-line-window window))
        (should (cdr (assq :reverse (emacs-redisplay-glyph-realized-face
                                    (aref (emacs-redisplay--mode-line-glyphs (current-buffer) 12) 0))))))
      (let* ((emacs-redisplay--mode-line-window other)
             (face (emacs-redisplay-glyph-realized-face
                    (aref (emacs-redisplay--mode-line-glyphs (current-buffer) 12) 11))))
        (should (cdr (assq :underline face)))
        (should-not (cdr (assq :reverse face)))))))

(ert-deftest emacs-redisplay-native-layout/line-number-wrap-gutter ()
  (emacs-redisplay-native-test--with-window "abcdefghijk" 10 3
    (setq-local display-line-numbers t)
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      (should (equal "  1 abcde\\" (emacs-redisplay-native-test--row matrix 0)))
      (should (equal "    fghij\\" (emacs-redisplay-native-test--row matrix 1)))
      (should (equal "    k     " (emacs-redisplay-native-test--row matrix 2))))))

(ert-deftest emacs-redisplay-native-layout/line-numbers-option-invalidates-cache ()
  (emacs-redisplay-native-test--with-window "one\ntwo\n" 12 3
    (emacs-redisplay-redisplay-window handle window)
    (emacs-redisplay-display-line-numbers-mode 1)
    (should (equal "  1 one     " (emacs-redisplay-native-test--row
                                  (emacs-redisplay-redisplay-window handle window) 0)))
    (emacs-redisplay-display-line-numbers-mode -1)
    (should (equal "one         " (emacs-redisplay-native-test--row
                                  (emacs-redisplay-redisplay-window handle window) 0)))))

(ert-deftest emacs-redisplay-native-layout/selective-display-folds-indented-lines ()
  (emacs-redisplay-native-test--with-window "top\n  hidden\n    hidden\nnext\n" 12 3
    (setq-local selective-display 2)
    (setq-local selective-display-ellipses t)
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      (should (equal "top...      " (emacs-redisplay-native-test--row matrix 0)))
      (should (equal "next        " (emacs-redisplay-native-test--row matrix 1))))))

(ert-deftest emacs-redisplay-native-layout/active-region-is-rendered ()
  (emacs-redisplay-native-test--with-window "abcdef" 12 3
    (let ((emacs-redisplay--face-registry (make-hash-table :test 'eq))
          (emacs-redisplay--face-cache (make-hash-table :test 'equal)))
      (emacs-redisplay-defface 'region '(:underline t))
      (set-mark 5)
      (goto-char 2)
      (setq-local transient-mark-mode t)
      (setq-local mark-active t)
      (let* ((matrix (emacs-redisplay-redisplay-window handle window))
             (glyphs (emacs-redisplay-glyph-row-glyphs
                      (aref (emacs-redisplay-glyph-matrix-rows matrix) 0))))
        (should-not (emacs-redisplay-glyph-realized-face (aref glyphs 0)))
        (should (cdr (assq :underline (emacs-redisplay-glyph-realized-face (aref glyphs 1)))))
        (should-not (emacs-redisplay-glyph-realized-face (aref glyphs 4)))))))

(ert-deftest emacs-redisplay-native-layout/window-end-follows-visible-rows ()
  (emacs-redisplay-native-test--with-window "one\ntwo\nthree\nfour\nfive" 20 2
    (emacs-redisplay-redisplay-window handle window)
    (should (= 9 (emacs-window-window-end window)))
    (emacs-window-set-window-start window 9)
    (emacs-window-set-window-point window 9)
    (emacs-redisplay-redisplay-window handle window)
    (should (= 20 (emacs-window-window-end window)))))

(ert-deftest emacs-redisplay-native-layout/minibuffer-height-reserves-frame-space ()
  (emacs-redisplay-native-test--with-window "body" 40 12
    (emacs-window-layout-frame 40 12 0 3)
    (should (= 9 (emacs-window-total-lines window)))))

(ert-deftest emacs-redisplay-native-layout/minibuffer-has-no-mode-or-header-line ()
  (emacs-redisplay-native-test--with-window "Prompt: value" 10 3
    (setq-local mode-line-format "MODE")
    (setq-local header-line-format "HEADER")
    (emacs-window-set-window-parameter window 'minibuffer t)
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      (should (equal "Prompt: v\\" (emacs-redisplay-native-test--row matrix 0)))
      (should (equal "alue      " (emacs-redisplay-native-test--row matrix 1))))))

(ert-deftest emacs-redisplay-native-layout/region-includes-newline-cell ()
  (emacs-redisplay-native-test--with-window "abc\ndef" 10 3
    (let ((emacs-redisplay--face-registry (make-hash-table :test 'eq))
          (emacs-redisplay--face-cache (make-hash-table :test 'equal)))
      (emacs-redisplay-defface 'region '(:underline t))
      (set-mark 6) (goto-char 2)
      (setq-local transient-mark-mode t)
      (setq-local mark-active t)
      (let* ((matrix (emacs-redisplay-redisplay-window handle window))
             (glyphs (emacs-redisplay-glyph-row-glyphs
                      (aref (emacs-redisplay-glyph-matrix-rows matrix) 0))))
        (should (cdr (assq :underline (emacs-redisplay-glyph-realized-face (aref glyphs 3)))))
        (should-not (aref glyphs 4))))))

(ert-deftest emacs-redisplay-native-layout/japanese-word-wrap-at-space ()
  (emacs-redisplay-native-test--with-window "本 abc 語 XYZ 日本 abc 語 XYZ 日本 abc 語" 40 3
    (setq-local word-wrap t)
    (let ((matrix (emacs-redisplay-redisplay-window handle window)))
      ;; The source ends a word exactly at the content edge.  Its following
      ;; wide character starts the next row without a continuation mark.
      (should-not (string-suffix-p "\\" (emacs-redisplay-native-test--row matrix 0)))
      (should (string-prefix-p "語" (emacs-redisplay-native-test--row matrix 1))))))

(ert-deftest emacs-redisplay-native-layout/line-numbers-remain-buffer-local ()
  (with-temp-buffer
    (let ((first (current-buffer)))
      (emacs-redisplay-display-line-numbers-mode 1)
      (with-temp-buffer
        (should-not display-line-numbers)
        (should-not display-line-numbers-mode))
      (should (buffer-local-value 'display-line-numbers first))
      (should (buffer-local-value 'display-line-numbers-mode first)))))

(ert-deftest emacs-redisplay-native-layout/japanese-word-wrap-inside-run ()
  (emacs-redisplay-native-test--with-window "abc 日本日本日本" 10 3
    (setq-local word-wrap t)
    (let* ((matrix (emacs-redisplay-redisplay-window handle window))
           (glyphs (emacs-redisplay-glyph-row-glyphs
                    (aref (emacs-redisplay-glyph-matrix-rows matrix) 0))))
      ;; CJK may break between characters.  Keep the two fitting characters
      ;; on this row instead of moving the whole run after the earlier space.
      (should (= ?日 (emacs-redisplay-glyph-char (aref glyphs 4))))
      (should (= ?本 (emacs-redisplay-glyph-char (aref glyphs 6))))
      (should (= ?\\ (emacs-redisplay-glyph-char (aref glyphs 8))))
      (should (= ?\\ (emacs-redisplay-glyph-char (aref glyphs 9)))))))

(provide 'emacs-redisplay-native-layout-test)
;;; emacs-redisplay-native-layout-test.el ends here
