;;; standalone-soak-test.el --- Tests for the in-process soak diagnostic -*- lexical-binding: t; -*-

;; Doc 11 M8: soak + failure-bucket reporting + RSS probe.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'standalone-soak)

(ert-deftest standalone-soak-run-reports-all-ok ()
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil))
    (let ((report (standalone-soak-run 5)))
      (should (= 5 (plist-get report :iterations)))
      (should (= 5 (plist-get report :ok)))
      (should (= 0 (plist-get report :errors)))
      (should (null (plist-get report :buckets))))))

(ert-deftest standalone-soak-buckets-failures-by-type ()
  (cl-letf (((symbol-function 'standalone-soak--iteration)
             (lambda (n) (if (= 0 (mod n 2)) (error "even boom") nil))))
    (let ((report (standalone-soak-run 4)))
      (should (= 4 (plist-get report :iterations)))
      ;; odd iterations (1, 3) succeed; even (0, 2) fail
      (should (= 2 (plist-get report :ok)))
      (should (= 2 (plist-get report :errors)))
      (let ((bucket (assoc "error" (plist-get report :buckets))))
        (should bucket)
        (should (= 2 (cdr bucket)))))))

(ert-deftest standalone-soak-rss-probe-is-int-or-nil ()
  (let ((rss (standalone-soak-rss-kb)))
    (should (or (null rss)
                (and (integerp rss) (> rss 0))))))

(ert-deftest standalone-soak-report-string-summarizes ()
  (let ((ok-report '(:iterations 3 :ok 3 :errors 0 :buckets nil))
        (fail-report '(:iterations 3 :ok 1 :errors 2 :buckets (("error" . 2)))))
    (should (string-match-p "3 iterations, 3 ok, 0 errors"
                            (standalone-soak-report-string ok-report)))
    (should (string-match-p "error=2"
                            (standalone-soak-report-string fail-report)))))

(ert-deftest standalone-soak-large-file-builds-and-searches ()
  (let ((nelisp-ec--buffers nil)
        (nelisp-ec--current-buffer nil))
    (let ((r (standalone-soak-large-file 200)))
      (should (= 200 (plist-get r :lines)))
      (should (plist-get r :found))
      (let ((rss (plist-get r :rss-kb)))
        (should (or (null rss) (integerp rss)))))))

(provide 'standalone-soak-test)

;;; standalone-soak-test.el ends here
