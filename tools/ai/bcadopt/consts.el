;;; -*- lexical-binding: t -*-
(setq load-prefer-newer t)
(let* ((root (file-name-as-directory (expand-file-name (getenv "BCROOT"))))
       (out (getenv "BCOUT")))
  (load (expand-file-name "scripts/nelisp-prelude-bytecode.el" root) nil t)
  (require 'bytecomp) (require 'cl-lib)
  (let* ((src (with-temp-buffer (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el" root)) (buffer-string)))
         (forms (nelisp-prelude-bytecode-source-defuns src)))
    (with-temp-file out
      (dolist (form forms)
        (let* ((name (nth 1 form))
               (c (condition-case nil
                      (with-temp-buffer
                        (setq-local lexical-binding nil)
                        (let ((byte-compile-warnings nil))
                          (byte-compile (append (list 'lambda (nth 2 form)) (cdddr form)))))
                    (error nil))))
          (when (byte-code-function-p c)
            (insert (format "%s\t%S\n" name
                            (cl-remove-if-not #'symbolp (append (aref c 2) nil))))))))))
