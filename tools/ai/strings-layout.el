;;; strings-layout.el --- Retained strings and alias transport -*- lexical-binding: t; -*-

(defvar strings-layout-n (string-to-number (or (getenv "STRINGS_N") "4000")))
(defvar strings-layout-mode (or (getenv "STRINGS_MODE") "population"))
(defvar strings-layout-keep (make-vector strings-layout-n nil))
(defvar strings-layout-shared (make-string 8 120))
(defvar strings-layout-before nil)
(defvar strings-layout-after nil)

(defun strings-layout-snapshot ()
  (let* ((rows (garbage-collect))
         (headers (nth 2 (assq 'strings rows)))
         (bytes (nth 2 (assq 'string-bytes rows)))
         (arena (nth 4 (nelisp--arena-stats))))
    (list headers bytes arena)))

(setq strings-layout-before (strings-layout-snapshot))
(let ((i 0))
  (while (< i strings-layout-n)
    (aset strings-layout-keep i
          (if (equal strings-layout-mode "population")
              (make-string 8 120) strings-layout-shared))
    (setq i (1+ i))))
(setq strings-layout-after (strings-layout-snapshot))
(princ (format "STRINGS-LAYOUT %s %d %d %d %d %S\n"
               strings-layout-mode strings-layout-n
               (- (nth 0 strings-layout-after) (nth 0 strings-layout-before))
               (- (nth 1 strings-layout-after) (nth 1 strings-layout-before))
               (- (nth 2 strings-layout-after) (nth 2 strings-layout-before))
               (eq (aref strings-layout-keep 0) (aref strings-layout-keep 1))))
nil
