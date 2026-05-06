;;; emacs-melpa-real-a-el-test.el --- Phase 4 real MELPA package: a.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 real-package onboarding 第 6 件 = a.el (Arne Brasseur、
;; ~355 LOC、associative-list utility).  `a-get' / `a-assoc' /
;; `a-keys' / `a-vals' / `a-get-in' (= Clojure-inspired access API)。
;; cl-lib 以外の依存無し、pure cons/list 操作で動作。

;;; Code:

(require 'ert)

(defconst emacs-melpa-real-a-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/a.el/a.el"
    "/usr/share/emacs/site-lisp/elpa/a/a.el"))

(defun emacs-melpa-real-a-el-test--locate ()
  (cl-find-if #'file-readable-p emacs-melpa-real-a-el-test--candidates))

(defmacro emacs-melpa-real-a-el-test--skip-without-source (&rest body)
  (declare (indent 0) (debug t))
  `(let ((src (emacs-melpa-real-a-el-test--locate)))
     (unless src (ert-skip "a.el not found"))
     (load src nil t)
     ,@body))

(ert-deftest emacs-melpa-real-a-el-test/loads-cleanly ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (should (fboundp 'a-get))
    (should (fboundp 'a-has-key))
    (should (fboundp 'a-assoc))
    (should (fboundp 'a-keys))
    (should (fboundp 'a-vals))
    (should (fboundp 'a-get-in))))

(ert-deftest emacs-melpa-real-a-el-test/get-and-default ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (should (= 1 (a-get '((:a . 1) (:b . 2)) :a)))
    (should (null (a-get '((:a . 1)) :missing)))
    (should (eq :default (a-get '((:a . 1)) :missing :default)))))

(ert-deftest emacs-melpa-real-a-el-test/has-key ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (should      (a-has-key '((:a . 1) (:b . 2)) :a))
    (should      (a-has-key '((:a . 1) (:b . 2)) :b))
    (should-not  (a-has-key '((:a . 1)) :missing))))

(ert-deftest emacs-melpa-real-a-el-test/assoc-adds-and-overrides ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (let ((m '((:a . 1))))
      ;; Adding a new key
      (let ((r (a-assoc m :b 2)))
        (should (= 1 (a-get r :a)))
        (should (= 2 (a-get r :b))))
      ;; Overriding an existing key
      (let ((r (a-assoc m :a 99)))
        (should (= 99 (a-get r :a)))))))

(ert-deftest emacs-melpa-real-a-el-test/keys-and-vals ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (let ((m '((:a . 1) (:b . 2) (:c . 3))))
      (should (equal '(:a :b :c) (a-keys m)))
      (should (equal '(1 2 3)    (a-vals m))))))

(ert-deftest emacs-melpa-real-a-el-test/get-in-traverses-nested ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (let ((nested '((:user . ((:profile . ((:name . "alice"))))))))
      (should (string= "alice"
                       (a-get-in nested '(:user :profile :name))))
      (should (null (a-get-in nested '(:user :missing :name))))
      (should (eq :default
                  (a-get-in nested '(:user :missing :name) :default))))))

(ert-deftest emacs-melpa-real-a-el-test/reduce-kv ()
  (emacs-melpa-real-a-el-test--skip-without-source
    (let* ((m '((:a . 1) (:b . 2) (:c . 3)))
           (sum (a-reduce-kv (lambda (acc _k v) (+ acc v)) 0 m)))
      (should (= 6 sum)))))

(provide 'emacs-melpa-real-a-el-test)

;;; emacs-melpa-real-a-el-test.el ends here
