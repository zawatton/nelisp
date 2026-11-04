;;; emacs-buffer-stream-test.el --- T107 stream bridge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-buffer-builtins)

(ert-deftest emacs-buffer-stream-test/read-narrowed-multibyte-buffer ()
  "Read offsets are relative to the accessible, narrowed buffer text."
  (let ((buf (nelisp-ec-generate-new-buffer " *t107-stream*")))
    (unwind-protect
        (progn
          (nelisp-ec-with-current-buffer buf
            (nelisp-ec-insert "α (one) (two) tail")
            ;; The second form begins at an offset that is not a byte index.
            (nelisp-ec-narrow-to-region 3 14)
            (nelisp-ec-goto-char 9))
          (should (equal (emacs-buffer-builtins-read-dispatch buf) '(two)))
          (nelisp-ec-with-current-buffer buf
            (should (= (nelisp-ec-point) 14))))
      (ignore-errors (nelisp-ec-kill-buffer buf)))))

(ert-deftest emacs-buffer-stream-test/marker-stream-preserves-selected-buffer ()
  "Marker output changes its buffer, without changing selected buffer."
  (let ((selected (nelisp-ec-generate-new-buffer " *selected*"))
        (target (nelisp-ec-generate-new-buffer " *target*")))
    (unwind-protect
        (progn
          (nelisp-ec-with-current-buffer target (nelisp-ec-insert "ab"))
          (let ((marker (nelisp-ec-with-current-buffer target
                          (nelisp-ec-point-marker))))
            (nelisp-ec-with-current-buffer selected
              (should (eq (nelisp-ec-current-buffer) selected))
              (emacs-buffer-builtins-emit-to-stream "X" marker)
              (should (eq (nelisp-ec-current-buffer) selected)))
            (should (= (nelisp-ec-marker-position marker) 4))))
      (ignore-errors (nelisp-ec-kill-buffer selected))
      (ignore-errors (nelisp-ec-kill-buffer target)))))

(ert-deftest emacs-buffer-stream-test/read-marker-ignores-narrowing-and-restores-state ()
  "Marker reads use the complete buffer, unlike buffer reads."
  (let ((buf (nelisp-ec-generate-new-buffer " *t107-marker-read*")))
    (unwind-protect
        (progn
          (nelisp-ec-with-current-buffer buf
            (nelisp-ec-insert "α (one) (two) tail")
            (let ((marker (nelisp-ec-set-marker (nelisp-ec-make-marker) 3 buf)))
              (nelisp-ec-narrow-to-region 9 14)
              (nelisp-ec-goto-char 14)
              (should (equal (emacs-buffer-builtins-read-dispatch marker) '(one)))
              (should (= (nelisp-ec-marker-position marker) 8))
              (should (= (nelisp-ec-point-min) 9))
              (should (= (nelisp-ec-point-max) 14))
              (should (= (nelisp-ec-point) 14))))
      (ignore-errors (nelisp-ec-kill-buffer buf))))))

(ert-deftest emacs-buffer-stream-test/output-marker-outside-narrowing-does-not-mutate ()
  "Output to an inaccessible marker signals before changing anything."
  (let ((buf (nelisp-ec-generate-new-buffer " *t107-marker-output*")))
    (unwind-protect
        (nelisp-ec-with-current-buffer buf
          (nelisp-ec-insert "(a)(b)")
          (let ((marker (nelisp-ec-set-marker (nelisp-ec-make-marker) 1 buf)))
            (nelisp-ec-narrow-to-region 4 7)
            (nelisp-ec-goto-char 4)
            (should-error (emacs-buffer-builtins-emit-to-stream "X" marker)
                          :type 'error)
            (should (equal (nelisp-ec-buffer-string) "(b)"))
            (should (= (nelisp-ec-point) 4))
            (should (= (nelisp-ec-marker-position marker) 1))))
      (ignore-errors (nelisp-ec-kill-buffer buf)))))

(provide 'emacs-buffer-stream-test)

;;; emacs-buffer-stream-test.el ends here
