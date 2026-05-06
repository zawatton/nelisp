;;; emacs-melpa-real-ht-el-test.el --- Phase 4 real MELPA package: ht.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 real-package onboarding 第 3 件 = ht.el (Wilfred
;; Hughes、hash-table convenience、~354 LOC、`Package-Requires: dash')。
;;
;; ht.el は pure hash-table 操作なので host load + substrate primitive
;; (= make-hash-table / puthash / gethash / hash-table-keys 等) のみで
;; 動く。dash.el が prerequisite なので test 内で先 load する。

;;; Code:

(require 'ert)

(defconst emacs-melpa-real-ht-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/ht.el/ht.el"
    "/usr/share/emacs/site-lisp/elpa/ht/ht.el")
  "Candidate paths for ht.el on disk.")

(defconst emacs-melpa-real-ht-el-test--dash-candidates
  '("/home/madblack-21/.emacs.d/external-packages/dash.el/dash.el"
    "/usr/share/emacs/site-lisp/elpa/dash/dash.el")
  "Candidate paths for dash.el (= ht.el's prerequisite).")

(defun emacs-melpa-real-ht-el-test--locate (paths)
  (cl-find-if #'file-readable-p paths))

(defmacro emacs-melpa-real-ht-el-test--skip-without-source (&rest body)
  "Skip the test gracefully when ht.el or dash.el are not on disk."
  (declare (indent 0) (debug t))
  `(let ((dash (emacs-melpa-real-ht-el-test--locate
                emacs-melpa-real-ht-el-test--dash-candidates))
         (ht   (emacs-melpa-real-ht-el-test--locate
                emacs-melpa-real-ht-el-test--candidates)))
     (cond
      ((null dash) (ert-skip "dash.el not found (= ht.el prerequisite)"))
      ((null ht)   (ert-skip "ht.el not found"))
      (t (load dash nil t)
         (load ht   nil t)
         ,@body))))

;;;; A. ht.el pure subset — all hash-table operations

(ert-deftest emacs-melpa-real-ht-el-test/loads-cleanly ()
  "ht.el must load end-to-end and bind its public API."
  (emacs-melpa-real-ht-el-test--skip-without-source
    (should (fboundp 'ht-create))
    (should (fboundp 'ht-set!))
    (should (fboundp 'ht-get))
    (should (fboundp 'ht-keys))
    (should (fboundp 'ht-size))
    (should (fboundp 'ht-from-alist))))

(ert-deftest emacs-melpa-real-ht-el-test/create-set-get ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let ((h (ht-create)))
      (ht-set! h :a 1)
      (ht-set! h :b 2)
      (should (= 1 (ht-get h :a)))
      (should (= 2 (ht-get h :b)))
      (should (null (ht-get h :missing))))))

(ert-deftest emacs-melpa-real-ht-el-test/contains-and-size ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let ((h (ht-create)))
      (ht-set! h :x 10)
      (should      (ht-contains-p h :x))
      (should-not  (ht-contains-p h :y))
      (should (= 1 (ht-size h)))
      (ht-set! h :y 20)
      (should (= 2 (ht-size h))))))

(ert-deftest emacs-melpa-real-ht-el-test/remove-and-clear ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let ((h (ht-create)))
      (ht-set! h :a 1)
      (ht-set! h :b 2)
      (ht-remove! h :a)
      (should-not  (ht-contains-p h :a))
      (should      (ht-contains-p h :b))
      (ht-clear! h)
      (should (= 0 (ht-size h))))))

(ert-deftest emacs-melpa-real-ht-el-test/keys-and-values ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let* ((h (ht-from-alist '((:a . 1) (:b . 2) (:c . 3)))))
      (should (= 3 (length (ht-keys h))))
      (should (= 3 (length (ht-values h))))
      ;; Use member-style assertion since hash iteration order isn't
      ;; guaranteed.
      (should (memq :a (ht-keys h)))
      (should (memq :b (ht-keys h)))
      (should (memq :c (ht-keys h)))
      (should (member 1 (ht-values h)))
      (should (member 2 (ht-values h)))
      (should (member 3 (ht-values h))))))

(ert-deftest emacs-melpa-real-ht-el-test/from-alist-and-to-alist ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let* ((alist '((:a . 1) (:b . 2)))
           (h (ht-from-alist alist))
           (out (ht->alist h)))
      (should (= 1 (cdr (assq :a out))))
      (should (= 2 (cdr (assq :b out)))))))

(ert-deftest emacs-melpa-real-ht-el-test/update-and-merge ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let ((h1 (ht-from-alist '((:a . 1) (:b . 2))))
          (h2 (ht-from-alist '((:b . 20) (:c . 3)))))
      (ht-update! h1 h2)
      (should (= 1   (ht-get h1 :a)))
      (should (= 20  (ht-get h1 :b)))
      (should (= 3   (ht-get h1 :c))))))

(ert-deftest emacs-melpa-real-ht-el-test/copy ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let* ((h1 (ht-from-alist '((:a . 1))))
           (h2 (ht-copy h1)))
      (ht-set! h2 :b 2)
      (should-not (ht-contains-p h1 :b))
      (should      (ht-contains-p h2 :b)))))

(ert-deftest emacs-melpa-real-ht-el-test/empty-p ()
  (emacs-melpa-real-ht-el-test--skip-without-source
    (let ((h (ht-create)))
      (should      (ht-empty? h))
      (ht-set! h :a 1)
      (should-not  (ht-empty? h)))))

(provide 'emacs-melpa-real-ht-el-test)

;;; emacs-melpa-real-ht-el-test.el ends here
