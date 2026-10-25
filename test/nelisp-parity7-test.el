;;; nelisp-parity7-test.el --- Mapping parity through native processes -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-parity7-corpus)

(defconst p7-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(defun p7-run (row mode)
  "Run one bounded ROW in MODE, preserving output for differential review."
  (let* ((binary (or (getenv "P7_BINARY") (ert-skip "Set P7_BINARY")))
         (out (expand-file-name (or (getenv "P7_OUT") "target/parity7/ert") p7-root))
         (stem (expand-file-name (format "%s-%s" mode (car row)) out))
         (fixture (concat stem ".el"))
         (err (concat stem ".stderr"))
         (expr (cadr row)))
    (make-directory out t)
    (with-temp-file fixture
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (prin1 `(progn
                (prin1 ,(if (eq mode 'interpreted) `(eval ',expr nil)
                          `(funcall ',(byte-compile `(lambda () ,expr)))))
                (terpri) (princ "PARITY7 COMPLETE\n") (kill-emacs 0)) (current-buffer)))
    (with-temp-buffer
      (let ((rc (apply #'call-process "timeout" nil (list t err) nil "--kill-after=1" "5" binary
                       (append (when (getenv "P7_GNU") '("-Q" "--batch"))
                               (list "--load" fixture)))))
        (write-region (point-min) (point-max) (concat stem ".stdout") nil 'silent)
        (should (equal rc 0))
        (should (zerop (file-attribute-size (file-attributes err))))
        (goto-char (point-min))
        (should (equal (read (current-buffer)) (nth 2 row)))
        (skip-chars-forward " \n\r")
        (should (equal (buffer-substring (point) (point-max)) "PARITY7 COMPLETE\n"))))))

(dolist (mode '(interpreted bytecode))
  (dolist (row p7-cases)
    (eval `(ert-deftest ,(intern (format "parity7-%s-%s" mode (car row))) ()
             (p7-run ',row ',mode)) t)))
(provide 'nelisp-parity7-test)
