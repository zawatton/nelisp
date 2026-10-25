;;; nelisp-parity5-test.el --- Native remainder and marker parity -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst nelisp-parity5--root
  (file-name-directory (directory-file-name (file-name-directory
                                           (or load-file-name buffer-file-name)))))
(defvar nelisp-parity5--results nil)
(defvar nelisp-parity5--attempted nil)

;; Expectations were checked on GNU Emacs 31.1, including complete conditions.
(defconst nelisp-parity5--cases
  '((rem-big-divisor (% 13 big) (ok 13))
    (and-marker-left (logand m 3) (ok 2))
    (and-marker-right (logand 13 m) (ok 4))
    (and-marker-unary (logand m) (ok 6))
    (and-marker-ternary (logand m 15 3) (ok 2))
    (rem-boundaries
     (list (% -13 big) (% 13 (- big)) (% -13 (- big))
           (% most-negative-fixnum (- most-negative-fixnum))
           (% most-positive-fixnum (- most-negative-fixnum))
           (% big 13) (% (- big) 13) (% -17 4) (% 17 -4)
           (% m big) (% m 4) (% 13 m))
     (ok (-13 13 -13 0 2305843009213693951 8 -8 -1 1 6 2 1)))
    (and-values
     (list (logand) (logand big) (logand big m) (logand m big)
           (logand m big 15) (logand big m 15)
           (logand (- big) m) (logand m (- big)) (logand m m m))
     (ok (-1 18446744073709551608 0 0 0 0 0 0 6)))
    (rem-zero (% 17 0) (error arith-error))
    (rem-float-left (% 1.5 big) (error wrong-type-argument integer-or-marker-p 1.5))
    (rem-float-right (% big 1.5) (error wrong-type-argument integer-or-marker-p 1.5))
    (rem-bad-left (% 'bad d) (error wrong-type-argument integer-or-marker-p bad))
    (rem-detached (% 13 d) (error error "Marker does not point anywhere"))
    (and-bad-first (logand 'bad d) (error wrong-type-argument integer-or-marker-p bad))
    (and-bad-later (logand m 'bad) (error wrong-type-argument number-or-marker-p bad))
    (and-float-first (logand 1.5 m) (error wrong-type-argument integer-or-marker-p 1.5))
    (and-float-later (logand m 1.5 'bad) (error wrong-type-argument integer-or-marker-p 1.5))
    (and-big-float (logand big m 1.5) (error wrong-type-argument integer-or-marker-p 1.5))
    (and-detached (logand 7 d) (error error "Marker does not point anywhere"))))

(defun nelisp-parity5-write-fixture (file)
  "Write one batch of real runtime calls, usable by GNU and NeLisp."
  (with-temp-file file
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (prin1 '(defun parity5-printable (value m d)
              (cond ((markerp value)
                     (list 'marker (cond ((eq value m) 'm) ((eq value d) 'd) (t 'other))
                           (marker-position value)))
                    ((consp value) (cons (parity5-printable (car value) m d)
                                         (parity5-printable (cdr value) m d)))
                    (t value))) (current-buffer))
    (insert "\n")
    (insert "(let ((m (make-marker)) (d (make-marker))\n"
            "      (big (* most-positive-fixnum 8)))\n"
            "  (with-temp-buffer\n    (insert \"abcdefghijklmnop\") (set-marker m 6)\n")
    (dolist (row nelisp-parity5--cases)
      (prin1 `(progn (prin1 (list ',(car row)
                                 (parity5-printable
                                  (condition-case e (list 'ok ,(cadr row))
                                    (error (cons 'error e))) m d))) (terpri)) (current-buffer))
      (insert "\n"))
    (insert "  ) (set-marker m nil))\n(princ \"PARITY5 COMPLETE\\n\")\n(kill-emacs 0)\n")))

(defun nelisp-parity5--run ()
  "Run all cases once; preserve raw stdout and stderr for diagnosis."
  (or nelisp-parity5--results
      (let* ((binary (or (getenv "NELISP_PARITY5_BIN")
                         (ert-skip "Native parity test requires NELISP_PARITY5_BIN")))
             (out (expand-file-name (or (getenv "NELISP_PARITY5_OUT") "target/parity5/ert")
                                    nelisp-parity5--root))
             (fixture (expand-file-name "cases.el" out))
             (stderr (expand-file-name "runtime.stderr" out)))
        (when nelisp-parity5--attempted
          (ert-fail "Previous native parity batch failed; inspect its raw stdout/stderr"))
        (setq nelisp-parity5--attempted t)
        (make-directory out t)
        (nelisp-parity5-write-fixture fixture)
        (with-temp-buffer
          (let ((rc (apply #'call-process binary nil (list t stderr) nil
                           (append (when (getenv "NELISP_PARITY5_GNU") '("-Q" "--batch"))
                                   (list "--load" fixture)))))
            (write-region (point-min) (point-max) (expand-file-name "runtime.stdout" out) nil 'silent)
            (should (equal rc 0))
            (should (= (file-attribute-size (file-attributes stderr)) 0))
            (goto-char (point-min))
            (let (rows)
              (dotimes (_ (length nelisp-parity5--cases)) (push (read (current-buffer)) rows))
              (skip-chars-forward "\n\r ")
              (should (equal (buffer-substring-no-properties (point) (point-max)) "PARITY5 COMPLETE\n"))
              (setq nelisp-parity5--results (nreverse rows))))))))

(dolist (row nelisp-parity5--cases)
  (let ((id (car row)) (expected (nth 2 row)))
    (eval `(ert-deftest ,(intern (format "nelisp-parity5-%s" id)) ()
             (should (equal (cadr (assq ',id (nelisp-parity5--run))) ',expected))) t)))

(provide 'nelisp-parity5-test)
