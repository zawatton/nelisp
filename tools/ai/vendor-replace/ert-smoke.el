;;; ert-smoke.el --- vendored ert runs real tests (S2.6)  -*- lexical-binding: t; -*-

;; Usage: BIN --load tools/ai/vendor-replace/ert-smoke.el
;;        emacs --batch -Q -l tools/ai/vendor-replace/ert-smoke.el
;; Prints "ERT-SMOKE <expected>/<total> unexpected=<n>" from the stats of a
;; batch run of four tests: a pass, a should-error, an expected failure and
;; one that fails unexpectedly (so a runner that reports everything as
;; passed cannot print 3/4 unexpected=1).

;;; Code:

(require 'ert)

(ert-deftest vr-smoke-pass ()
  (should (equal (list 1 2) (list 1 2))))

(ert-deftest vr-smoke-error ()
  (should-error (car 1) :type 'wrong-type-argument))

(ert-deftest vr-smoke-expected-fail ()
  :expected-result :failed
  (should (= 1 2)))

(ert-deftest vr-smoke-unexpected-fail ()
  (should (string= "a" "b")))

(let ((stats (let ((standard-output #'ignore))
               (ert-run-tests-batch "^vr-smoke-"))))
  (princ (format "ERT-SMOKE %d/%d unexpected=%d\n"
                 (ert-stats-completed-expected stats)
                 (ert-stats-total stats)
                 (ert-stats-completed-unexpected stats))))

;;; ert-smoke.el ends here
