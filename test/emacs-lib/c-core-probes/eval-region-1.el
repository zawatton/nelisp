;;; eval-region-1.el --- Buffer reader and evaluator parity -*- lexical-binding: t; -*-

(eval-region
 (with-temp-buffer
   (insert "(+ 1 2)\n; trailing comment")
   (goto-char 4)
   (list (eval-region 1 (point-max)) (point)))
 (with-temp-buffer
   (insert "(goto-char 1) (setq ccore-region-result 42)")
   (goto-char 10)
   (eval-region 1 (point-max))
   (list (point) ccore-region-result))
 (with-temp-buffer
   (insert "(+ 1 2) (+ 3 4)")
   (goto-char 3)
   (let (seen)
     (eval-region 1 (point-max) nil
                  (lambda (stream)
                    (push (list (bufferp stream)
                                (eq stream (current-buffer)) (point)) seen)
                    (read stream)))
     (list (point) (nreverse seen))))
 (with-temp-buffer
   (insert "(+ 1 2) (+ 3 4)")
   (let (output)
     (eval-region 1 (point-max) (lambda (char) (push char output)))
     (nreverse output)))
 (with-temp-buffer
   (insert "(+ 1 2) (error \"outside region\")")
   (eval-region 1 8))
 (with-temp-buffer
   (insert "(+ 1")
   (condition-case err (eval-region 1 (point-max))
     (error (car err))))
 (with-temp-buffer
   (insert "(error \"evaluation failed\")")
   (goto-char 2)
   (list (condition-case err (eval-region 1 (point-max))
           (error (car err))) (point))))
