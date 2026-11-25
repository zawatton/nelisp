;;; -*- lexical-binding: t; -*-
(let* ((root (make-temp-file "par-init-dir-" t))
       (default-directory (file-name-as-directory root))
       (short-names '("one" "two\nlines" "日本語"))
       expected)
  (unwind-protect
      (progn
        (dolist (name short-names)
          (write-region "" nil name nil 'silent))
        (prin1 (list 'small
                     (equal (directory-files root nil "\\`[^.]") short-names)
                     (length (directory-files root t nil t))))
        (terpri)
        ;; 300 full-sized names plus separators cross the old 64 KiB cap.
        (dotimes (index 300)
          (let ((name (format "item-%03d-%s" index (make-string 232 ?z))))
            (write-region "" nil name nil 'silent)
            (push name expected)))
        (setq expected (nreverse expected))
        (condition-case err
            (let ((actual (directory-files root nil "\\`item-")))
              (prin1 (list 'large (length actual) (equal actual expected))))
          (error (prin1 (list 'large (car err)))))
        (terpri)
        (condition-case err
            (prin1 (list 'full
                         (equal (directory-files root t "\\`item-")
                                (mapcar (lambda (name) (concat default-directory name))
                                        expected))))
          (error (prin1 (list 'full (car err)))))
        (terpri))
    (dolist (name (append short-names expected))
      (when (file-exists-p name) (delete-file name)))
    (delete-directory root)))
