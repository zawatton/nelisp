;;; thunk-test.el --- Tests for the thunk shim  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 16 breadth round 20.  On the batch host the real GNU `thunk' runs,
;; pinning the contract the NeLisp runtime shim must reproduce.

;;; Code:

(require 'ert)
(require 'thunk)

(ert-deftest thunk-test/doc16-round20-delay-force ()
  (should (= 3 (thunk-force (thunk-delay (+ 1 2)))))
  (should (eq 'v (thunk-force (thunk-delay 'v)))))

(ert-deftest thunk-test/doc16-round20-memoized ()
  (let ((count 0))
    (let ((th (thunk-delay (setq count (1+ count)) 'done)))
      (should (eq 'done (thunk-force th)))
      (should (eq 'done (thunk-force th)))
      (should (= count 1)))))

(ert-deftest thunk-test/doc16-round20-thunk-let* ()
  (should (= 30 (thunk-let* ((x 10) (y (* x 3))) y)))
  (should (= 10 (thunk-let* ((x 10)) x))))

(ert-deftest thunk-test/doc16-round20-thunk-let-is-lazy ()
  (let ((ran nil))
    (should (= 1 (thunk-let ((a 1) (b (setq ran t))) a)))
    (should (null ran))))

(provide 'thunk-test)

;;; thunk-test.el ends here
