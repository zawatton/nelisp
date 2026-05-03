;;; emacs-buffer-builtins-test.el --- ERT tests for emacs-buffer-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs buffer builtin bridge.  Under batch
;; host Emacs the host C builtins remain active, so these tests lean on
;; the `nelisp-ec-*' substrate directly to verify the contract that the
;; polyfill body is supposed to bridge to.

;;; Code:

(require 'ert)
(let ((load-path (cons "/home/madblack-21/Notes/dev/nelisp/packages/nelisp-regex/src"
                       load-path)))
  (require 'emacs-buffer-builtins))
(require 'cl-lib)

(defmacro emacs-buffer-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean NeLisp buffer registry/current-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil))
     ,@body))

(defmacro emacs-buffer-builtins-test--with-temp-buffer-polyfill (&rest body)
  "Mirror the Phase 9 `with-temp-buffer' rewrite for macroexpand checks."
  (declare (indent 0) (debug (body)))
  (let ((buf (make-symbol "buf")))
    (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                     " *temp*")))
          (list 'unwind-protect
                (cons 'nelisp-ec-with-current-buffer (cons buf body))
                (list 'nelisp-ec-kill-buffer buf)))))

;;;; A. Load cleanly

(ert-deftest emacs-buffer-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-buffer-builtins))
  (should (fboundp 'buffer-string))
  (should (fboundp 'with-current-buffer)))

;;;; B. Temp-buffer style roundtrip via nelisp-ec-*

(ert-deftest emacs-buffer-builtins-test/temp-buffer-roundtrip-via-nelisp-ec ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf1 (nelisp-ec-generate-new-buffer " *temp*"))
          (buf2 (nelisp-ec-generate-new-buffer " *temp*")))
      (should (equal "" (nelisp-ec-with-current-buffer buf1
                          (nelisp-ec-buffer-string))))
      (should (equal "alpha" (nelisp-ec-with-current-buffer buf1
                               (nelisp-ec-insert "alpha")
                               (nelisp-ec-buffer-string))))
      (should (equal 6 (nelisp-ec-with-current-buffer buf2
                         (nelisp-ec-insert "alpha")
                         (nelisp-ec-point))))
      (should (equal "alpha" (nelisp-ec-with-current-buffer buf2
                               (nelisp-ec-buffer-string)))))))

;;;; C. with-current-buffer restores selection

(ert-deftest emacs-buffer-builtins-test/with-current-buffer-restores-current-buffer ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (nelisp-ec-set-buffer a)
      (should (eq a (nelisp-ec-current-buffer)))
      (nelisp-ec-with-current-buffer b
        (should (eq b (nelisp-ec-current-buffer)))
        (should (equal "b" (nelisp-ec-buffer-name (nelisp-ec-current-buffer)))))
      (should (eq a (nelisp-ec-current-buffer))))))

;;;; D. generate-new-buffer / kill-buffer

(ert-deftest emacs-buffer-builtins-test/generate-and-kill-buffer-roundtrip ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "scratch")))
      (should (nelisp-ec-buffer-p buf))
      (should (equal "scratch" (nelisp-ec-buffer-name buf)))
      (should (eq buf (cdr (assoc "scratch" nelisp-ec--buffers))))
      (should (equal t (nelisp-ec-kill-buffer buf)))
      (should (nelisp-ec-buffer-killed-p buf))
      (should-not (assoc "scratch" nelisp-ec--buffers))
      (should-error (nelisp-ec-set-buffer buf)
                    :type 'nelisp-ec-buffer-killed))))

;;;; E. point-min / point-max

(ert-deftest emacs-buffer-builtins-test/point-min-max-on-populated-buffer ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "points")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcd")
        (should (= 1 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (should (= 5 (nelisp-ec-point)))
        (should (= 4 (nelisp-ec-buffer-size)))))))

;;;; F. buffer-substring

(ert-deftest emacs-buffer-builtins-test/buffer-substring-range-correct ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "substr")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (should (equal "bcd" (nelisp-ec-buffer-substring 2 5)))
        (should (equal "bcd" (nelisp-ec-buffer-substring 5 2)))
        (should (= 7 (nelisp-ec-point-max)))
        (should (= 1 (nelisp-ec-point-min)))))))

;;;; G. save-excursion

(ert-deftest emacs-buffer-builtins-test/save-excursion-restores-point ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "excursion")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-goto-char 3)
        (should (= 3 (nelisp-ec-point)))
        (nelisp-ec-save-excursion
          (nelisp-ec-goto-char 6)
          (should (= 6 (nelisp-ec-point))))
        (should (= 3 (nelisp-ec-point)))
        (should (eq buf (nelisp-ec-current-buffer)))))))

;;;; H. save-restriction

(ert-deftest emacs-buffer-builtins-test/save-restriction-restores-bounds ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "restrict")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-narrow-to-region 2 5)
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (nelisp-ec-save-restriction
          (nelisp-ec-widen)
          (should (= 1 (nelisp-ec-point-min)))
          (should (= 7 (nelisp-ec-point-max)))
          (nelisp-ec-narrow-to-region 3 4)
          (should (= 3 (nelisp-ec-point-min)))
          (should (= 4 (nelisp-ec-point-max))))
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))))))

;;;; I. narrow-to-region

(ert-deftest emacs-buffer-builtins-test/narrow-to-region-clips-point-min-max ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "narrow")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-goto-char 6)
        (should (= 6 (nelisp-ec-point)))
        (nelisp-ec-narrow-to-region 2 5)
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (should (= 5 (nelisp-ec-point)))
        (nelisp-ec-widen)
        (should (= 1 (nelisp-ec-point-min)))
        (should (= 7 (nelisp-ec-point-max)))))))

;;;; J. markers

(ert-deftest emacs-buffer-builtins-test/make-marker-set-marker-roundtrip ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "marker")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abc")
        (let ((m (nelisp-ec-make-marker)))
          (should (null (nelisp-ec-marker-position m)))
          (should (null (nelisp-ec-marker-buffer m)))
          (should (eq m (nelisp-ec-set-marker m 2 buf)))
          (should (= 2 (nelisp-ec-marker-position m)))
          (should (eq buf (nelisp-ec-marker-buffer m)))
          (should (eq m (nelisp-ec-set-marker m nil)))
          (should (null (nelisp-ec-marker-position m)))
          (should (null (nelisp-ec-marker-buffer m))))))))

;;;; K. macroexpand shape of with-temp-buffer rewrite

(ert-deftest emacs-buffer-builtins-test/with-temp-buffer-macroexpand-uses-ec-substrate ()
  (cl-letf (((symbol-function 'with-temp-buffer)
             (symbol-function
              'emacs-buffer-builtins-test--with-temp-buffer-polyfill)))
    (let* ((expanded (macroexpand '(with-temp-buffer
                                     (insert "x")
                                     (buffer-string))))
           (flat (flatten-tree expanded)))
      (should (eq 'let (car expanded)))
      (should (memq 'nelisp-ec-generate-new-buffer flat))
      (should (memq 'nelisp-ec-with-current-buffer flat))
      (should (memq 'nelisp-ec-kill-buffer flat)))))

;;;; L. Nested temp-buffer style buffers stay independent

(ert-deftest emacs-buffer-builtins-test/nested-buffers-preserve-separate-content ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((outer (nelisp-ec-generate-new-buffer "outer"))
          (inner (nelisp-ec-generate-new-buffer "inner")))
      (should (not (eq outer inner)))
      (nelisp-ec-with-current-buffer outer
        (nelisp-ec-insert "outer")
        (should (equal "outer" (nelisp-ec-buffer-string)))
        (nelisp-ec-with-current-buffer inner
          (nelisp-ec-insert "inner")
          (should (equal "inner" (nelisp-ec-buffer-string))))
        (should (equal "outer" (nelisp-ec-buffer-string)))
        (should (eq outer (nelisp-ec-current-buffer)))))))

(provide 'emacs-buffer-builtins-test)

;;; emacs-buffer-builtins-test.el ends here
