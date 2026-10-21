;;; bytecode-gen.el --- reproduce GNU 31.1 bytecode inputs -*- lexical-binding: t; -*-
(require 'bytecomp)
(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (print-length nil) (print-level nil) (print-escape-nonascii t)
       (functions
        `((while . ,(byte-compile
                     '(lambda (n) (let ((i 0))
                                    (while (< i n) (setq i (1+ i))) i))))
          (add . ,(byte-compile '(lambda (i) (+ i 3))))
          (funcall . ,(byte-compile
                       '(lambda (n) (let ((i 0) (s 2))
                                      (while (< i n)
                                        (setq s (funcall 'cr-add i))
                                        (setq i (1+ i))) s)))))))
  (with-temp-file (expand-file-name "bytecode.eld" dir)
    (insert ";; GNU " emacs-version "; reproduce with bytecode-gen.el\n")
    (prin1 functions (current-buffer)) (insert "\n")))
