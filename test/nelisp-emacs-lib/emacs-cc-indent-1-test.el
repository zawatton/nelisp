;;; emacs-cc-indent-1-test.el --- indent C-core fallback tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst emacs-cc-indent-1-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-cc-indent-1-test--load-fallbacks ()
  (load (expand-file-name
         "packages/nelisp-emacs-foundation/src/emacs-cc-indent-1.el"
         emacs-cc-indent-1-test--root)
        nil t))

(defun emacs-cc-indent-1-test--integer-motion-results (motion)
  "Return ordinary integer and CUR-COL results using MOTION."
  (list
   (with-temp-buffer
     (insert "one\ntwo\nthree")
     (goto-char 1)
     (list (funcall motion 1) (point)))
   (with-temp-buffer
     (insert "one\ntwo\nthree")
     (goto-char 8)
     (list (funcall motion -1) (point)))
   (with-temp-buffer
     (insert "one\ntwo\nthree")
     (goto-char 8)
     (list (funcall motion 0) (point)))
   (with-temp-buffer
     (insert "one\ntwo\nthree")
     (goto-char 8)
     (list (funcall motion 1 nil 2) (point)))))

(defun emacs-cc-indent-1-test--cons-case (motion window text start lines hscroll)
  "Return MOTION's result and point for TEXT and window geometry."
  (let ((old-buffer (window-buffer window))
        (old-hscroll (window-hscroll window)))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (set-window-buffer window (current-buffer))
          (set-window-hscroll window hscroll)
          (goto-char start)
          (list (funcall motion lines window) (point)))
      (set-window-buffer window old-buffer)
      (set-window-hscroll window old-hscroll))))

(defun emacs-cc-indent-1-test--cons-motion-results (motion)
  "Return cons-form results for multiple strings, widths, and hscrolls."
  (let* ((frame (selected-frame))
         (window (selected-window))
         (old-width (frame-width frame))
         (old-height (frame-height frame)))
    (unwind-protect
        (list
         (progn
           (set-frame-size frame 24 old-height)
           (list
            (emacs-cc-indent-1-test--cons-case
             motion window "abcdefgh\nxy" 1 '(4 . 0) 0)
            (emacs-cc-indent-1-test--cons-case
             motion window "one\ntwo\nthree" 2 '(6 . 1) 0)))
         (progn
           (set-frame-size frame 70 old-height)
           (list
            (emacs-cc-indent-1-test--cons-case
             motion window "a\tb\ncd\nef" 2 '(4 . 1) 3)
            (emacs-cc-indent-1-test--cons-case
             motion window "short\nline\nend" 7 '(20 . 2) 0))))
      (set-frame-size frame old-width old-height))))

(ert-deftest emacs-cc-indent-1-fallbacks-use-host-buffer-primitives ()
  (let* ((saved-motion (and (fboundp 'compute-motion)
                            (symbol-function 'compute-motion)))
         (saved-vertical (and (fboundp 'vertical-motion)
                              (symbol-function 'vertical-motion)))
         (reference-motions
          (emacs-cc-indent-1-test--integer-motion-results saved-vertical))
         (reference-cons-motions
          (emacs-cc-indent-1-test--cons-motion-results saved-vertical))
         (buffer-names '(buffer-substring-no-properties point-min point-max
                         goto-char point))
         (buffer-functions (mapcar (lambda (name)
                                     (cons name (symbol-function name)))
                                   buffer-names)))
    (unwind-protect
        (progn
          ;; Force only the two target fallbacks; the real host buffer API stays
          ;; installed and serves the same role as the runtime primitives.
          (fmakunbound 'compute-motion)
          (fmakunbound 'vertical-motion)
          (emacs-cc-indent-1-test--load-fallbacks)
          (dolist (entry buffer-functions)
            (should (eq (symbol-function (car entry)) (cdr entry))))
          (should (equal (emacs-cc-indent-1--line-col "ab\ncd" 4) '(1 . 1)))
          (should (equal (emacs-cc-indent-1-test--integer-motion-results
                          #'vertical-motion)
                         reference-motions))
          (should (equal (emacs-cc-indent-1-test--cons-motion-results
                          #'vertical-motion)
                         reference-cons-motions))
          (with-temp-buffer
            (insert "abcdefgh\nxy")
            (goto-char 1)
            (should (= (vertical-motion '(4 . 0)) 0))
            (should (<= (point-min) (point) (point-max))))
          (with-temp-buffer
            (insert "one\ntwo")
            (goto-char 1)
            (should (equal (condition-case err (vertical-motion nil)
                             (error (list (car err) (cadr err) (caddr err))))
                           '(wrong-type-argument fixnump nil)))))
      (if saved-motion (fset 'compute-motion saved-motion)
        (fmakunbound 'compute-motion))
      (if saved-vertical (fset 'vertical-motion saved-vertical)
        (fmakunbound 'vertical-motion)))))

(provide 'emacs-cc-indent-1-test)

;;; emacs-cc-indent-1-test.el ends here
