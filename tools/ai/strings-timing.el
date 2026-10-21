;;; strings-timing.el --- Paired wall-clock string workload -*- lexical-binding: t; -*-
(defvar strings-bench-count
  (string-to-number (or (getenv "STRINGS_N") "100000")))
(defvar strings-bench-kind (or (getenv "STRINGS_KIND") "concat"))
(defvar strings-bench-keep nil)

(defun strings-bench-loop (count)
  (let ((i 0) (a "abcdefghi") (b "xyz") (from 1) (to 7)
        (fmt "%s:%s") (index 0) (v (vector nil nil)) (keep nil)
        ;; Resolve the public function once, preventing a compiler from
        ;; treating a literal primitive call with constant strings as a
        ;; constant expression. The same function-value call is used by
        ;; the same-arity trivial subr control in all three runtimes.
        (function (symbol-function
                   (cond ((equal strings-bench-kind "concat") 'concat)
                         ((equal strings-bench-kind "concat-control") 'eq)
                         ((equal strings-bench-kind "substring") 'substring)
                         ((equal strings-bench-kind "substring-control") 'aset)
                         ((equal strings-bench-kind "format") 'format)
                         (t 'aset)))))
    (cond
     ((or (equal strings-bench-kind "concat")
          (equal strings-bench-kind "concat-control"))
      (while (< i count) (setq keep (funcall function a b) i (1+ i))))
     ((equal strings-bench-kind "substring")
      (while (< i count) (setq keep (funcall function a from to) i (1+ i))))
     ((equal strings-bench-kind "substring-control")
      (while (< i count) (setq keep (funcall function v index to) i (1+ i))))
     ((equal strings-bench-kind "format")
      (while (< i count) (setq keep (funcall function fmt a b) i (1+ i))))
     ((equal strings-bench-kind "format-control")
      (while (< i count) (setq keep (funcall function v index a) i (1+ i)))))
    ;; Keep one result through the final GC without timing a global-store
    ;; mutation, with a different value, at every primitive invocation.
    (setq strings-bench-keep keep)))

(strings-bench-loop 100)
(garbage-collect)
(let ((start (float-time)))
  (strings-bench-loop strings-bench-count)
  (garbage-collect)
  (princ (format "STRINGS-TIME %.9f\n" (- (float-time) start))))
nil
