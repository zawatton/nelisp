;;; standalone-literal-file.el --- Exact byte insertion controls -*- lexical-binding: t; -*-
(require 'nelisp-stdlib-literal-file)
(let* ((before (symbol-function 'insert-file-contents-literally))
       (installed (eq (nelisp-stdlib-literal-file-install) 'installed))
       (file (getenv "NELISP_LITERAL_FIXTURE")))
  (unless installed
    (unless (eq before (symbol-function 'insert-file-contents-literally))
      (error "GNU literal reader changed")))
  (unless file (error "Literal fixture path required"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "XY")
    (goto-char 2)
    (unless (equal (insert-file-contents-literally file)
                   (list (expand-file-name file) 8))
      (error "Literal return metadata differs"))
    (unless (and (= (point) 2)
                 (equal (string-to-list (buffer-string)) '(88 0 128 255 65 10 13 27 66 89)))
      (error "Raw file bytes or insertion point differ")))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (unless (equal (insert-file-contents-literally file nil 1 3)
                   (list (expand-file-name file) 2))
      (error "Literal range metadata differs"))
    (unless (equal (string-to-list (buffer-string)) '(128 255))
      (error "Literal byte window differs")))
  (when installed
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "preserved")
      (dolist (arguments '((t) (nil -1) (nil nil nil t)))
        (let (refused)
          (condition-case nil
              (apply #'insert-file-contents-literally file arguments)
            (error (setq refused t)))
          (unless (and refused (equal (buffer-string) "preserved"))
            (error "Unsupported range/mode mutated buffer")))))))
(princ "PUBLIC-LITERAL-BYTES-PASS\n")
