;;; strings-layout.el --- Retained object layout fixture -*- lexical-binding: t; -*-
(defvar strings-layout-n (string-to-number (or (getenv "STRINGS_N") "4000")))
(defvar strings-layout-mode (or (getenv "STRINGS_MODE") "population"))
(defvar strings-layout-keep (make-vector strings-layout-n nil))
(defvar strings-layout-shared (make-string 8 120))
(let ((i 0))
  (while (< i strings-layout-n)
    (aset strings-layout-keep i
          (if (equal strings-layout-mode "population")
              (make-string 8 120) strings-layout-shared))
    (setq i (1+ i))))
(garbage-collect)
(princ (format "STRINGS-ROOT %d %S\n" strings-layout-n
               (eq (aref strings-layout-keep 0) (aref strings-layout-keep 1))))
;; The checker inspects its own child's retained root, then releases it.
(princ "STRINGS-HOLD\n")
(read)
nil
