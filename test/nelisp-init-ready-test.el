;;; nelisp-init-ready-test.el --- Tests for nelisp-init-ready  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-init-ready)

(defconst nelisp-init-ready-test--base
  '((runtime . "1.0") (prelude . "aa") (module . "bb") (compiler . "3")
    (abi . "4") (features . "x,y") (producer . "7") (regs . "r1,r2")))

(ert-deftest nelisp-init-ready-fingerprint-deterministic ()
  (should (equal (nelisp-init-ready-fingerprint nelisp-init-ready-test--base)
                 (nelisp-init-ready-fingerprint nelisp-init-ready-test--base)))
  (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                          (nelisp-init-ready-fingerprint
                           nelisp-init-ready-test--base))))

(ert-deftest nelisp-init-ready-fingerprint-order-independent ()
  (should (equal (nelisp-init-ready-fingerprint nelisp-init-ready-test--base)
                 (nelisp-init-ready-fingerprint
                  (reverse nelisp-init-ready-test--base)))))

(ert-deftest nelisp-init-ready-fingerprint-sensitive ()
  (let ((ref (nelisp-init-ready-fingerprint nelisp-init-ready-test--base)))
    (dolist (c nelisp-init-ready-test--base)
      (let ((mod (mapcar (lambda (e)
                           (if (eq (car e) (car c))
                               (cons (car e) (concat (cdr e) "!"))
                             e))
                         nelisp-init-ready-test--base)))
        (should-not (equal ref (nelisp-init-ready-fingerprint mod)))))))

(ert-deftest nelisp-init-ready-fingerprint-length-prefix ()
  (should-not (equal (nelisp-init-ready-fingerprint '((ab . "c")))
                     (nelisp-init-ready-fingerprint '((a . "bc")))))
  (should-not (equal (nelisp-init-ready-fingerprint '((a . "b") (c . "d")))
                     (nelisp-init-ready-fingerprint '((a . "bc") (d . ""))))))

(ert-deftest nelisp-init-ready-fingerprint-errors ()
  (should-error (nelisp-init-ready-fingerprint nil)
                :type 'nelisp-init-ready-error)
  (should-error (nelisp-init-ready-fingerprint '((a . "1") (a . "2")))
                :type 'nelisp-init-ready-error)
  (should-error (nelisp-init-ready-fingerprint '((a . 1)))
                :type 'nelisp-init-ready-error))

(ert-deftest nelisp-init-ready-happy-path ()
  (let ((txn (nelisp-init-ready-make)))
    (should (eq (nelisp-init-ready-state txn) 'unseen))
    (should (eq (nelisp-init-ready-begin txn "id1") t))
    (should (eq (nelisp-init-ready-state txn) 'initializing))
    (nelisp-init-ready-register txn "id1" 'd1 'p1)
    (should (eq (nelisp-init-ready-publish txn "id1" '(r)) txn))
    (should (eq (nelisp-init-ready-state txn) 'ready))
    (should (equal (nelisp-init-ready-registrations txn) '((d1 . p1))))
    (should (equal (nelisp-init-ready-root-set txn) '(r)))))

(ert-deftest nelisp-init-ready-reuse-skips-thunk ()
  (let ((txn (nelisp-init-ready-make)) (n 0))
    (nelisp-init-ready-run txn "id1" (lambda (_) (setq n (1+ n)) nil))
    (should (eq (nelisp-init-ready-run txn "id1" (lambda (_) (setq n (1+ n)) nil))
                :reuse))
    (should (= n 1))))

(ert-deftest nelisp-init-ready-stale-rejected-before-thunk ()
  (let ((txn (nelisp-init-ready-make)) (n 0))
    (nelisp-init-ready-run txn "id1" (lambda (_) nil))
    (should-error (nelisp-init-ready-run txn "id2" (lambda (_) (setq n (1+ n))))
                  :type 'nelisp-init-ready-stale)
    (should (= n 0))
    (should (eq (nelisp-init-ready-state txn) 'ready))))

(ert-deftest nelisp-init-ready-reentrancy ()
  (let ((txn (nelisp-init-ready-make)) (n 0))
    (should-error
     (nelisp-init-ready-run
      txn "id1"
      (lambda (_)
        (setq n (1+ n))
        (nelisp-init-ready-run txn "id1" (lambda (_) (setq n (+ n 100))))))
     :type 'nelisp-init-ready-reentrant)
    (should (= n 1))
    (should (eq (nelisp-init-ready-state txn) 'failed))))

(ert-deftest nelisp-init-ready-failure-exactly-once ()
  (let ((txn (nelisp-init-ready-make)) (n 0) (m 0) caught)
    (condition-case err
        (nelisp-init-ready-run txn "id1"
                               (lambda (_) (setq n (1+ n))
                                 (signal 'wrong-type-argument '(foo 42))))
      (error (setq caught err)))
    (should (equal caught '(wrong-type-argument foo 42)))
    (should (= n 1))
    (should (eq (nelisp-init-ready-state txn) 'failed))
    (should (equal (nelisp-init-ready-status txn) caught))
    (should-error (nelisp-init-ready-run txn "id1" (lambda (_) (setq m 1)))
                  :type 'nelisp-init-ready-failed)
    (should (= m 0))
    (should (= n 1))))

(ert-deftest nelisp-init-ready-register-idempotent-and-conflict ()
  (let ((txn (nelisp-init-ready-make)))
    (nelisp-init-ready-begin txn "id1")
    (nelisp-init-ready-register txn "id1" 'd 'p)
    (nelisp-init-ready-register txn "id1" 'd 'p)
    (should (equal (nelisp-init-ready-registrations txn) '((d . p))))
    (should-error (nelisp-init-ready-register txn "id1" 'd 'q)
                  :type 'nelisp-init-ready-conflict)
    (should-error (nelisp-init-ready-register txn "other" 'e 'p)
                  :type 'nelisp-init-ready-stale)))

(ert-deftest nelisp-init-ready-publish-wrong-identity ()
  (let ((txn (nelisp-init-ready-make)))
    (should-error (nelisp-init-ready-publish txn "id1" nil)
                  :type 'nelisp-init-ready-error)
    (nelisp-init-ready-begin txn "id1")
    (should-error (nelisp-init-ready-publish txn "id2" nil)
                  :type 'nelisp-init-ready-stale)
    (should (eq (nelisp-init-ready-state txn) 'initializing))
    (should-error (nelisp-init-ready-fail txn "id2" 'x)
                  :type 'nelisp-init-ready-stale)))

(ert-deftest nelisp-init-ready-partial-state-rejected ()
  ;; A transaction left INITIALIZING (as if a prior process died mid-init)
  ;; must reject a different identity and never restart for the same one.
  (let ((txn (nelisp-init-ready-make)) (n 0))
    (nelisp-init-ready-begin txn "old")
    (should-error (nelisp-init-ready-run txn "new" (lambda (_) (setq n 1)))
                  :type 'nelisp-init-ready-stale)
    (should-error (nelisp-init-ready-run txn "old" (lambda (_) (setq n 1)))
                  :type 'nelisp-init-ready-reentrant)
    (should (= n 0))))

(ert-deftest nelisp-init-ready-test-throw-records-failed ()
  "A throw out of the thunk records FAILED and is never re-run."
  (let ((txn (nelisp-init-ready-make)) (calls 0))
    (should (eq (catch 'out
                  (nelisp-init-ready-run
                   txn "id" (lambda (_) (setq calls (1+ calls)) (throw 'out 'escaped))))
                'escaped))
    (should (eq (nelisp-init-ready-state txn) 'failed))
    (should (equal (nelisp-init-ready-status txn) '(:nonlocal-exit)))
    (should-error (nelisp-init-ready-run
                   txn "id" (lambda (_) (setq calls (1+ calls)) nil))
                  :type 'nelisp-init-ready-failed)
    (should (= calls 1))))

(provide 'nelisp-init-ready-test)
;;; nelisp-init-ready-test.el ends here
