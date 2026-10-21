;;; gnu-semantics-printer.el --- GNU print.c parity corpus -*- lexical-binding: t; -*-
;; Regenerate the golden output from the repository root:
;; (ulimit -v 8388608; emacs -Q --batch --load test/fixtures/gnu-semantics-printer.el)
;; Output character codes preserve literal control bytes without depending on
;; the printer's string-escape policy to serialize the result being tested.

(defun gnu-printer-emit (label value operation)
  (let ((result
         (condition-case err
             (cond ((eq operation 'prin1) (with-output-to-string (prin1 value)))
                   ((eq operation 'princ) (with-output-to-string (princ value)))
                   ((eq operation 'format) (format "%S" value))
                   ((eq operation 'number) (number-to-string value)))
           (error (list 'error err)))))
    (let ((print-length nil) (print-level nil) (print-escape-newlines nil)
          (print-escape-control-characters nil) (float-output-format nil))
      (prin1 (list label operation (if (stringp result) (append result nil) result)))
      (terpri))))

(defun gnu-printer-bool-bytes (bytes length)
  (let ((vector (make-bool-vector length nil)) (i 0))
    (while (< i length)
      (aset vector i (/= 0 (logand (nth (/ i 8) bytes) (ash 1 (% i 8)))))
      (setq i (1+ i)))
    vector))

(dolist (size '(0 1 7 8 9 15 16 17 31))
  (dolist (initial '(nil t))
    (dolist (operation '(prin1 princ format))
      (gnu-printer-emit (list 'bool-size size initial)
                        (make-bool-vector size initial) operation))))

(dolist (bytes '((0 48) (1 55) (7 56) (8 48) (9 0) (10 55) (12 0)
                 (13 49) (31 49) (34 92) (63 127) (127 128) (255 255)))
  (dolist (escape-newlines '(nil t))
    (dolist (escape-control '(nil t))
      (let ((print-escape-newlines escape-newlines)
            (print-escape-control-characters escape-control))
        (dolist (operation '(prin1 princ format))
          (gnu-printer-emit (list 'bool-escape bytes escape-newlines escape-control)
                            (gnu-printer-bool-bytes bytes 16) operation))))))

(dolist (limit '(nil -1 0 1 2 3))
  (let ((print-length limit) (print-escape-control-characters t))
    (dolist (operation '(prin1 princ format))
      (gnu-printer-emit (list 'bool-limit limit)
                        (gnu-printer-bool-bytes '(1 48 255) 19) operation))))

(let ((print-escape-nonascii t) (print-escape-multibyte t))
  (dolist (operation '(prin1 princ format))
    (gnu-printer-emit 'bool-nonascii
                      (gnu-printer-bool-bytes '(128 255) 15) operation)))

(dolist (value '(1e0 1.5e-7 1e21 -0.0 0.0 1.0e+INF -1.0e+INF
                 0.0e+NaN -0.0e+NaN 1.0e+NaN 17.0e+NaN
                 1.0e-4 1.0e-5 1.0e14 1.0e15 1.234567890123456e15
                 1.0000000000000002 0.3333333333333333 99.99
                 5.0e-324 2.2250738585072014e-308 1.7976931348623157e308))
  (dolist (operation '(prin1 princ format number))
    (gnu-printer-emit 'float-default value operation)))

(dolist (descriptor '(nil 17 "%.0f" "%.1f" "%.2e" "%.3g" "%.15g"
                     "%.f" "%.e" "%.g" "%.00f" "%.0e" "%.0g" "%.16g"
                     "%g" "%10.2f" "%.2E" "%.2fX" "%.999999999999g"))
  (let ((float-output-format descriptor))
    (dolist (value '(1.0 1.5e-7 1.25 1e21 -0.0 1.0e+INF 17.0e+NaN))
      (dolist (operation '(prin1 princ format))
        (gnu-printer-emit (list 'float-format descriptor) value operation)))))

(princ "GNU-SEMANTICS-PRINTER-PASS\n")
nil
