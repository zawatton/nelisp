;;; nelisp-prelude-perf-parity-cases.el --- targeted prelude parity -*- lexical-binding: nil; -*-

;; Standalone cases for the prelude performance/compatibility slice.
(list
 ;; Beginning, newline, end, and out-of-range positions exercise the
 ;; optimized line counter while keeping results independent of buffer state.
 (with-temp-buffer
   (insert "a\nbc\nd\n")
   (mapcar (lambda (pos) (line-number-at-pos pos)) '(1 2 3 4 5 6 7 8)))
 ;; BEG and END are byte offsets. Check clipping at both file boundaries,
 ;; partial insertion, directory errors, and replacement with existing text.
 (let ((file (make-temp-file "nelisp-prelude-perf-parity-")))
   (unwind-protect
       (progn
         (with-temp-file file (insert "abc\n日本\n"))
         (list
          (with-temp-buffer
            (let ((result (insert-file-contents file nil 0 100)))
              (list (cadr result) (buffer-string) (point))))
          (with-temp-buffer
            (let ((result (insert-file-contents file nil 100 200)))
              (list (cadr result) (buffer-string) (point))))
          (with-temp-buffer
            (let ((result (insert-file-contents file nil 2 nil)))
              (list (cadr result) (buffer-string) (point))))
          (with-temp-buffer
            (let ((result (insert-file-contents file nil 4 7)))
              (list (cadr result) (buffer-string) (point))))
          (condition-case err
              (progn (insert-file-contents default-directory) 'no-error)
            (error err))
          (with-temp-buffer
            (insert "abcdef")
            (goto-char 4)
            (let ((result (insert-file-contents file nil nil nil t)))
              (list (cadr result) (buffer-string) (point))))))
     (delete-file file))))
