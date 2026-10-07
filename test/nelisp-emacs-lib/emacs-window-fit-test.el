;;; emacs-window-fit-test.el --- Shared GNU window fitting policy -*- lexical-binding: t; -*-
(require 'ert)
(require 'emacs-window-builtins)

(defmacro emacs-window-fit-test--world (&rest body)
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer " *window-fit*"))
         (window-min-height 4) (window-min-width 10)
         (window-safe-min-height 2) (window-safe-min-width 2)
         (fit-window-to-buffer-horizontally nil) (fit-frame-to-buffer nil)
         (window-combination-resize nil))
     (emacs-window-reset)
     (setf (emacs-window-buffer (emacs-window-selected-window)) buffer)
     (with-current-buffer buffer
       (setq mode-line-format nil header-line-format nil tab-line-format nil))
     (unwind-protect (progn ,@body) (kill-buffer buffer) (emacs-window-reset))))

(ert-deftest emacs-window-fit/root-is-not-resized ()
  (emacs-window-fit-test--world
    (let ((before (emacs-window-window-height)))
      (should-not (emacs-window-fit-window-to-buffer))
      (should (= before (emacs-window-window-height)))
      (should-error (emacs-window-window-resize nil 1)))))

(ert-deftest emacs-window-fit/gnu-empty-and-terminating-newlines ()
  (emacs-window-fit-test--world
    (dolist (case '(("" 0 0) ("abc" 3 1) ("abc\n" 3 1)
                    ("abc\n\n" 3 2) ("abc\ndef\n" 3 2)))
      (with-current-buffer buffer (erase-buffer) (insert (car case)))
      (let ((size (emacs-window-text-pixel-size)))
        (should (= (car size) (* 8 (nth 1 case))))
        (should (= (cdr size) (* 16 (nth 2 case))))))
    (with-current-buffer buffer (erase-buffer) (insert "\n\nabc\n\n"))
    (should (equal (emacs-window-text-pixel-size nil nil t) '(24 . 48)))
    (should (equal (emacs-window-text-pixel-size nil t t) '(24 . 32)))))

(ert-deftest emacs-window-fit/gnu-bounds-and-conservation ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer (insert "abc\ndef\n"))
    (let* ((window (emacs-window-selected-window))
           (lower (emacs-window-split-window window nil 'below))
           (total (emacs-window-total-lines emacs-window--root)))
      (should-not (emacs-window-fit-window-to-buffer window 8 4))
      (should (= (emacs-window-total-lines window) 4))
      (should (= (+ (emacs-window-total-lines window) (emacs-window-total-lines lower)) total))
      (should (eq (emacs-window-window-resize lower -2) t))
      (should (= (emacs-window-total-lines window) 6))
      (let ((before (list (emacs-window-total-lines window) (emacs-window-total-lines lower))))
        (should-error (emacs-window-window-resize window -50))
        (should (equal before (list (emacs-window-total-lines window) (emacs-window-total-lines lower))))))))

(ert-deftest emacs-window-fit/mid-line-range-uses-display-column ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer (insert "abcdef\nghi\n"))
    ;; GNU keeps absolute line extents across rows, but reports the interval
    ;; width when both positions occupy the same display row.
    (should (equal (emacs-window-text-pixel-size nil 3 9) '(48 . 32)))
    (should (equal (emacs-window-text-pixel-size nil 2 6) '(32 . 16)))
    (should (equal (emacs-window-text-pixel-size nil 4 4) '(0 . 16)))
    (should (equal (emacs-window-text-pixel-size nil 7 7) '(-48 . 16)))
    (should (equal (emacs-window-text-pixel-size nil 8 8) '(0 . 0)))
    (with-current-buffer buffer (narrow-to-region 3 9))
    (should (equal (emacs-window-text-pixel-size nil nil nil t) '(32 . 32)))
    (with-current-buffer buffer (widen) (erase-buffer) (insert (make-string 160 ?x)))
    (should (equal (emacs-window-text-pixel-size nil 79 80) '(632 . 16)))
    (should (equal (emacs-window-text-pixel-size nil 100 110) '(80 . 16)))))

(ert-deftest emacs-window-fit/maximum-and-fixed-buffer-size ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer (dotimes (_ 40) (insert "line\n")))
    (let ((window (emacs-window-selected-window)))
      (emacs-window-split-window window nil 'below)
      (emacs-window-fit-window-to-buffer window 8 4)
      (should (= (emacs-window-total-lines window) 8))
      (with-current-buffer buffer (setq-local window-size-fixed 'height) (erase-buffer) (insert "x"))
      (emacs-window-fit-window-to-buffer window)
      (should (= (emacs-window-total-lines window) 8)))))

(ert-deftest emacs-window-fit/nested-orthogonal-resize ()
  (emacs-window-fit-test--world
    (let* ((left (emacs-window-selected-window))
           (below (emacs-window-split-window left 12 'below))
           (right (emacs-window-split-window left 30 'right)))
      (emacs-window-window-resize left 2)
      (should (= (emacs-window-total-lines left) 14))
      (should (= (emacs-window-total-lines right) 14))
      (should (= (emacs-window-total-lines below) 10))
      (should (= (emacs-window-total-lines emacs-window--root) 24)))))

(ert-deftest emacs-window-fit/width-opt-in-and-pixel-delta ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer (insert "abc"))
    (let* ((left (emacs-window-selected-window))
           (right (emacs-window-split-window left 40 'right)))
      (emacs-window-fit-window-to-buffer left)
      (should (= (emacs-window-total-cols left) 40))
      (let ((fit-window-to-buffer-horizontally t))
        (emacs-window-fit-window-to-buffer left nil nil 20 10))
      (should (= (emacs-window-total-cols left) 10))
      (should (= (emacs-window-total-cols right) 70))
      (emacs-window-window-resize left 16 t nil t)
      (should (= (emacs-window-total-cols left) 12)))))

(ert-deftest emacs-window-fit/native-buffer-visibility ()
  (emacs-window-fit-test--world
    (let ((window (emacs-window-selected-window)))
      (setf (emacs-window-total-cols window) 8 (emacs-window-total-lines window) 2)
      (with-current-buffer buffer (insert "abcd\nefgh\n"))
      (should (emacs-window-pos-visible-in-window-p 1 window))
      (should (emacs-window-pos-visible-in-window-p 6 window))
      (should-not (emacs-window-pos-visible-in-window-p 11 window))
      (should (emacs-window-pos-visible-in-window-p t window))
      (setf (emacs-window-start window) 6)
      (should-not (emacs-window-pos-visible-in-window-p 1 window))
      (should (emacs-window-pos-visible-in-window-p 11 window)))))

(ert-deftest emacs-window-fit/unbound-root-adopts-native-buffer ()
  (emacs-window-reset)
  (unwind-protect
      (with-temp-buffer
        (insert "abc")
        (let ((size (emacs-window-text-pixel-size)))
          (should (eq (emacs-window-window-buffer) (current-buffer)))
          (should (equal size '(24 . 16)))))
    (emacs-window-reset)))

(ert-deftest emacs-window-fit/preservation-is-soft-and-buffer-specific ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer (insert "abc\n"))
    (let ((window (emacs-window-selected-window)))
      (emacs-window-split-window window nil 'below)
      (should (emacs-window-fit-window-to-buffer window 8 4 nil nil t))
      (should (= (nth 2 (emacs-window-window-parameter window 'window-preserved-size)) 64))
      (should (emacs-window-window-resize window 1))
      (should (= (emacs-window-total-lines window) 5))
      (emacs-window-fit-window-to-buffer window)
      (should-not (nth 2 (emacs-window-window-parameter window 'window-preserved-size))))))

(ert-deftest emacs-window-fit/property-window-object-boundary ()
  (emacs-window-fit-test--world
    (with-current-buffer buffer
      (insert "abc")
      (put-text-property 1 3 'k3-property 'window-value))
    (let ((window (emacs-window-selected-window)))
      (should (eq (emacs-window-builtins--pos-property-window
                   #'get-pos-property 2 'k3-property window)
                  'window-value))
      (should (eq (emacs-window-builtins--pos-property-window
                   #'get-pos-property 2 'k3-property buffer)
                  'window-value)))))
