;;; strings-timing.el --- Paired wall-clock string workload -*- lexical-binding: t; -*-

(defvar strings-bench-count
  (string-to-number (or (getenv "STRINGS_N") "10000")))
(defvar strings-bench-kind (or (getenv "STRINGS_KIND") "concat"))
(defvar strings-bench-keep nil)

(defun strings-bench-loop (count)
  (let ((i 0) (a "abcdefghi") (b "xyz") (from 1) (to 7)
        (fmt "%s:%s") (index 0) (v (vector nil nil)))
    (cond
     ((equal strings-bench-kind "concat")
      (while (< i count) (setq strings-bench-keep (concat a b) i (1+ i))))
     ((equal strings-bench-kind "concat-control")
      (while (< i count) (setq strings-bench-keep (eq a b) i (1+ i))))
     ((equal strings-bench-kind "substring")
      (while (< i count) (setq strings-bench-keep (substring a from to) i (1+ i))))
     ((equal strings-bench-kind "substring-control")
      (while (< i count) (setq strings-bench-keep (aset v index to) i (1+ i))))
     ((equal strings-bench-kind "format")
      (while (< i count) (setq strings-bench-keep (format fmt a b) i (1+ i))))
     ((equal strings-bench-kind "format-control")
      (while (< i count) (setq strings-bench-keep (aset v index a) i (1+ i)))))))

(strings-bench-loop 100)
(garbage-collect)
(let ((start (float-time)))
  (strings-bench-loop strings-bench-count)
  (garbage-collect)
  (princ (format "STRINGS-TIME %.9f\n" (- (float-time) start))))
nil
