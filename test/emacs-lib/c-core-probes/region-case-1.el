;;; region-case-1.el --- region casing behavior probes  -*- lexical-binding: t; -*-
(capitalize-region
  (with-temp-buffer
    (insert "hELLO wORLD")
    (list (capitalize-region 1 (1+ (buffer-size))) (buffer-string))))
(upcase-initials-region
  (with-temp-buffer
    (insert "hELLO wORLD")
    (list (upcase-initials-region 1 (1+ (buffer-size))) (buffer-string))))
(capitalize-region
  (with-temp-buffer
    (insert "ßETA café ΟΔΥΣΣΕΎΣ")
    (list (capitalize-region 1 (1+ (buffer-size))) (buffer-string))))
(capitalize-region
  (with-temp-buffer
    (insert "ßETA")
    (goto-char 3)
    (set-mark 4)
    (list (capitalize-region 1 (1+ (buffer-size)))
          (buffer-string) (point) (mark t))))
(upcase-initials-region
  (with-temp-buffer
    (insert "ßETA café ΟΔΥΣΣΕΎΣ")
    (list (upcase-initials-region 1 (1+ (buffer-size))) (buffer-string))))
(capitalize-region
  (with-temp-buffer
    (insert "hELLO wORLD")
    (goto-char 4)
    (set-mark 9)
    (list (capitalize-region 8 2)
          (buffer-string) (point) (mark t))))
(capitalize-region
  (with-temp-buffer
    (insert "hELLO")
    (condition-case err
        (list 'returned (capitalize-region 0 4) (buffer-string))
      (error (list (car err) (buffer-string))))))
(upcase-initials-region
  (with-temp-buffer
    (insert "hELLO")
    (narrow-to-region 2 5)
    (condition-case err
        (list 'returned (upcase-initials-region 1 4) (buffer-string))
      (error (list (car err) (buffer-string))))))
(capitalize-region
  (with-temp-buffer
    (insert "hELLO")
    (put-text-property 2 4 'read-only t)
    (condition-case err
        (list 'returned (capitalize-region 1 6) (buffer-string))
      (error (list (car err) (buffer-string))))))
 (upcase-initials-region
  (with-temp-buffer
    (insert "hELLO wORLD")
    (put-text-property 1 (1+ (buffer-size)) 'face 'bold)
    (upcase-initials-region 1 (1+ (buffer-size)))
    (list (buffer-string) (get-text-property 1 'face))))
(capitalize-region
  (with-temp-buffer
    (insert "hELLO wORLD")
    (let ((region-extract-function
           (lambda (method)
             (if (eq method 'bounds) '((1 . 6) (7 . 12)) nil))))
      (list (capitalize-region 1 (1+ (buffer-size)) t) (buffer-string)))))
