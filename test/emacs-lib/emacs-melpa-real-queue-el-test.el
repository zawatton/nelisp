;;; emacs-melpa-real-queue-el-test.el --- Phase 4 real MELPA package: queue.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 real-package onboarding 第 5 件 = queue.el (Toby
;; Cubitt、~165 LOC、`Package-Requires: cl-lib').
;;
;; queue.el は pure FIFO data structure。cl-lib 以外の依存無し、
;; host load + substrate cons / list primitive のみで動作。

;;; Code:

(require 'ert)

(defconst emacs-melpa-real-queue-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/queue/queue.el"
    "/usr/share/emacs/site-lisp/elpa/queue/queue.el"))

(defun emacs-melpa-real-queue-el-test--locate ()
  (cl-find-if #'file-readable-p emacs-melpa-real-queue-el-test--candidates))

(defmacro emacs-melpa-real-queue-el-test--skip-without-source (&rest body)
  (declare (indent 0) (debug t))
  `(let ((src (emacs-melpa-real-queue-el-test--locate)))
     (unless src (ert-skip "queue.el not found"))
     (load src nil t)
     ,@body))

(ert-deftest emacs-melpa-real-queue-el-test/loads-cleanly ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (should (fboundp 'make-queue))
    (should (fboundp 'queue-enqueue))
    (should (fboundp 'queue-dequeue))
    (should (fboundp 'queue-first))
    (should (fboundp 'queue-length))
    (should (fboundp 'queue-empty))))

(ert-deftest emacs-melpa-real-queue-el-test/enqueue-dequeue-fifo ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (let ((q (make-queue)))
      (queue-enqueue q 1)
      (queue-enqueue q 2)
      (queue-enqueue q 3)
      (should (= 3 (queue-length q)))
      (should (= 1 (queue-dequeue q)))
      (should (= 2 (queue-dequeue q)))
      (should (= 1 (queue-length q)))
      (should (= 3 (queue-dequeue q)))
      (should (queue-empty q)))))

(ert-deftest emacs-melpa-real-queue-el-test/first-and-last ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (let ((q (make-queue)))
      (queue-enqueue q "a")
      (queue-enqueue q "b")
      (queue-enqueue q "c")
      (should (string= "a" (queue-first q)))
      (should (string= "c" (queue-last  q))))))

(ert-deftest emacs-melpa-real-queue-el-test/empty-predicate ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (let ((q (make-queue)))
      (should      (queue-empty q))
      (queue-enqueue q :x)
      (should-not  (queue-empty q))
      (queue-dequeue q)
      (should      (queue-empty q)))))

(ert-deftest emacs-melpa-real-queue-el-test/all-method ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (let ((q (make-queue)))
      (queue-enqueue q 1)
      (queue-enqueue q 2)
      (queue-enqueue q 3)
      (should (equal '(1 2 3) (queue-all q))))))

(ert-deftest emacs-melpa-real-queue-el-test/append-and-prepend ()
  (emacs-melpa-real-queue-el-test--skip-without-source
    (let ((q (make-queue)))
      (queue-append q 'tail)
      (queue-prepend q 'head)
      (should (eq 'head (queue-first q)))
      (should (eq 'tail (queue-last  q))))))

(provide 'emacs-melpa-real-queue-el-test)

;;; emacs-melpa-real-queue-el-test.el ends here
