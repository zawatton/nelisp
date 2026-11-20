;;; nelisp-ec-expand-file-name-relative-base-test.el --- relative base parity -*- lexical-binding: t; -*-

(defun nelisp-ec-expand-file-name-relative-base-test--run ()
  "Check relative BASE, absolute PATH, omitted BASE, root clamp and buffers."
  (let ((checks 0)
        (failed nil)
        (root (make-temp-file "expand-relative-base-" t))
        (saved default-directory)
        (buffer-a (generate-new-buffer " *relative-base-a*"))
        (buffer-b (generate-new-buffer " *relative-base-b*")))
    (unwind-protect
        (condition-case err
            (let* ((one (expand-file-name "one" root))
                   (two (expand-file-name "two" root))
                   (one-child (expand-file-name "two" one)))
              (make-directory one)
              (make-directory two)
              (make-directory one-child)
              (setq default-directory (file-name-as-directory one))
              (dolist (case `(("../two" "./" ,two)
                              ("../two" "." ,two)
                              ("two" ".." ,two)
                              ("two" "" ,one-child)
                              (,two "./" ,two)
                              ("../two" nil ,two)
                              ("../../two" "/" "/two")))
                (let* ((path (nth 0 case))
                       (base (nth 1 case))
                       (want (nth 2 case))
                       (got (if base
                                (expand-file-name path base)
                              (expand-file-name path))))
                  (unless (equal got want)
                    (error "expansion mismatch: path=%S base=%S got=%S want=%S"
                           path base got want))
                  (setq checks (1+ checks))))
              (with-current-buffer buffer-a
                (setq default-directory (file-name-as-directory one))
                (unless (equal (expand-file-name "file" "./")
                               (expand-file-name "file" one))
                  (error "buffer A relative base mismatch"))
                (setq checks (1+ checks)))
              (with-current-buffer buffer-b
                (setq default-directory (file-name-as-directory two))
                (unless (equal (expand-file-name "file" "./")
                               (expand-file-name "file" two))
                  (error "buffer B relative base mismatch"))
                (setq checks (1+ checks))))
          (error (setq failed err)))
      (setq default-directory saved)
      (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b) (kill-buffer buffer-b))
      (ignore-errors (delete-directory root t)))
    (if failed
        (progn
          (princ (format "CD-RELATIVE-BASE FAIL cases=%d error=%S\n"
                         checks failed))
          2)
      (princ (format "CD-RELATIVE-BASE PASS cases=%d\n" checks))
      nil)))

(nelisp-ec-expand-file-name-relative-base-test--run)

;;; nelisp-ec-expand-file-name-relative-base-test.el ends here
