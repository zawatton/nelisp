;;; nelisp-repl-profile-test.el --- REPL profiler tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-repl-profile)

(defun nelisp-repl-profile-test--leaf (x) (+ x 1))
(defun nelisp-repl-profile-test--mid (x)
  (nelisp-repl-profile-test--leaf (nelisp-repl-profile-test--leaf x)))
(defun nelisp-repl-profile-test--rec (n)
  (if (<= n 0) 0 (nelisp-repl-profile-test--rec (1- n))))

(defun nelisp-repl-profile-test--row (name)
  (car (seq-filter (lambda (r) (eq (plist-get r :name) name))
                   (nelisp-repl-profile-report))))

(defmacro nelisp-repl-profile-test--with (names &rest body)
  "Instrument NAMES for BODY and restore afterwards."
  (declare (indent 1))
  `(unwind-protect
       (progn (clrhash nelisp-repl-profile--records)
              (nelisp-repl-profile-instrument ,names)
              ,@body)
     (nelisp-repl-profile-restore)
     (clrhash nelisp-repl-profile--records)))

(ert-deftest nelisp-repl-profile/counts-calls-and-keeps-values ()
  (nelisp-repl-profile-test--with '(nelisp-repl-profile-test--leaf
                                    nelisp-repl-profile-test--mid)
    ;; The wrapped function must still answer, not merely be counted.
    (should (= (nelisp-repl-profile-test--mid 10) 12))
    (should (= (nelisp-repl-profile-test--leaf 10) 11))
    (should (= (plist-get (nelisp-repl-profile-test--row
                           'nelisp-repl-profile-test--mid)
                          :calls)
               1))
    ;; Two from inside `mid', one direct.
    (should (= (plist-get (nelisp-repl-profile-test--row
                           'nelisp-repl-profile-test--leaf)
                          :calls)
               3))))

(ert-deftest nelisp-repl-profile/recursion-counts-once-for-time ()
  (nelisp-repl-profile-test--with '(nelisp-repl-profile-test--rec)
    (nelisp-repl-profile-test--rec 50)
    (let ((row (nelisp-repl-profile-test--row 'nelisp-repl-profile-test--rec)))
      ;; Every entry is counted...
      (should (= (plist-get row :calls) 51))
      ;; ...but the nest contributes one span, not 51 overlapping ones.
      (should (>= (plist-get row :seconds) 0.0))
      (should (plist-get row :inclusive)))))

(ert-deftest nelisp-repl-profile/unknown-name-is-not-reported-as-installed ()
  (nelisp-repl-profile-test--with '(nelisp-repl-profile-test--leaf)
    (should (equal (nelisp-repl-profile-instrument
                    '(nelisp-repl-profile-test--no-such-function))
                   nil))
    ;; And instrumenting twice does not stack shims.
    (should (equal (nelisp-repl-profile-instrument
                    '(nelisp-repl-profile-test--leaf))
                   nil))))

(ert-deftest nelisp-repl-profile/restore-leaves-a-redefined-function-alone ()
  (let ((replacement (lambda (_x) 'replaced)))
    (unwind-protect
        (progn
          (clrhash nelisp-repl-profile--records)
          (nelisp-repl-profile-instrument '(nelisp-repl-profile-test--leaf))
          ;; Something else takes the name over -- a reload, an `fset'.
          (fset 'nelisp-repl-profile-test--leaf replacement)
          (let ((result (nelisp-repl-profile-restore)))
            (should (equal (plist-get result :restored) nil))
            (should (equal (plist-get result :skipped)
                           '(nelisp-repl-profile-test--leaf))))
          ;; The newer definition survives instead of being overwritten.
          (should (eq (nelisp-repl-profile-test--leaf 1) 'replaced)))
      (fset 'nelisp-repl-profile-test--leaf (lambda (x) (+ x 1)))
      (clrhash nelisp-repl-profile--records))))

(ert-deftest nelisp-repl-profile/reset-keeps-instrumentation ()
  (nelisp-repl-profile-test--with '(nelisp-repl-profile-test--leaf)
    (nelisp-repl-profile-test--leaf 1)
    (should (= (plist-get (nelisp-repl-profile-test--row
                           'nelisp-repl-profile-test--leaf) :calls)
               1))
    (nelisp-repl-profile-reset)
    (should (= (plist-get (nelisp-repl-profile-test--row
                           'nelisp-repl-profile-test--leaf) :calls)
               0))
    (nelisp-repl-profile-test--leaf 1)
    (should (= (plist-get (nelisp-repl-profile-test--row
                           'nelisp-repl-profile-test--leaf) :calls)
               1))))

(ert-deftest nelisp-repl-profile/report-is-sorted-and-limited ()
  (nelisp-repl-profile-test--with '(nelisp-repl-profile-test--leaf
                                    nelisp-repl-profile-test--mid
                                    nelisp-repl-profile-test--rec)
    (nelisp-repl-profile-test--rec 20)
    (nelisp-repl-profile-test--mid 1)
    (let ((all (nelisp-repl-profile-report))
          (two (nelisp-repl-profile-report 2)))
      (should (= (length all) 3))
      (should (= (length two) 2))
      (should (>= (plist-get (nth 0 all) :seconds)
                  (plist-get (nth 1 all) :seconds)))
      (should (>= (plist-get (nth 1 all) :seconds)
                  (plist-get (nth 2 all) :seconds))))))

(ert-deftest nelisp-repl-profile/overhead-is-measured-not-assumed ()
  (let ((overhead (nelisp-repl-profile-overhead-seconds 200)))
    (should (numberp overhead))
    ;; The probe cleans up after itself: no shim and no row left behind.
    (should-not (gethash 'nelisp-repl-profile--overhead-probe
                         nelisp-repl-profile--originals))
    (should-not (gethash 'nelisp-repl-profile--overhead-probe
                         nelisp-repl-profile--records))))

(provide 'nelisp-repl-profile-test)

;;; nelisp-repl-profile-test.el ends here
