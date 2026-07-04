;;; emacs-weak-table-test.el --- ERT for the weak-table approximation (Doc 06 B5)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 06 B5 option (b): an explicitly-pruned weak-table approximation.  Fully
;; host-testable (no FFI / GC dependency).

;;; Code:

(require 'ert)
(require 'emacs-weak-table)

(ert-deftest emacs-weak-table-test/put-get-remove-count ()
  "Basic get/put/remove/count semantics, including nil values."
  (let ((wt (emacs-weak-table-create 'eq)))
    (should (= 0 (emacs-weak-table-count wt)))
    (emacs-weak-table-put wt 'a 1)
    (emacs-weak-table-put wt 'b nil)        ; nil value is a real entry
    (should (= 2 (emacs-weak-table-count wt)))
    (should (= 1 (emacs-weak-table-get wt 'a)))
    (should (null (emacs-weak-table-get wt 'b)))
    (should (eq 'def (emacs-weak-table-get wt 'missing 'def)))
    ;; Re-putting an existing key must not duplicate it.
    (emacs-weak-table-put wt 'a 99)
    (should (= 2 (emacs-weak-table-count wt)))
    (should (= 99 (emacs-weak-table-get wt 'a)))
    ;; Remove.
    (should (emacs-weak-table-remove wt 'a))
    (should-not (emacs-weak-table-remove wt 'a))   ; already gone
    (should (= 1 (emacs-weak-table-count wt)))))

(ert-deftest emacs-weak-table-test/prune-by-liveness ()
  "`prune' drops entries whose key fails the liveness predicate (the explicit
weak-collection step), and reports how many were dropped (Doc 06 B5)."
  (let ((wt (emacs-weak-table-create 'eq))
        (live '(k1 k3)))
    (emacs-weak-table-put wt 'k1 1)
    (emacs-weak-table-put wt 'k2 2)
    (emacs-weak-table-put wt 'k3 3)
    (emacs-weak-table-put wt 'k4 4)
    ;; Only k1 and k3 are "live"; k2 and k4 should be pruned.
    (should (= 2 (emacs-weak-table-prune wt (lambda (k) (memq k live)))))
    (should (= 2 (emacs-weak-table-count wt)))
    (should (= 1 (emacs-weak-table-get wt 'k1)))
    (should (= 3 (emacs-weak-table-get wt 'k3)))
    (should-not (emacs-weak-table-get wt 'k2))
    (should-not (emacs-weak-table-get wt 'k4))
    ;; A second prune with everything live drops nothing.
    (should (= 0 (emacs-weak-table-prune wt (lambda (_k) t))))
    (should (= 2 (emacs-weak-table-count wt)))))

(provide 'emacs-weak-table-test)
;;; emacs-weak-table-test.el ends here
