;;; emacs-melpa-real-dash-el-test.el --- Phase 4 real MELPA package: dash.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 real-package onboarding 第 2 件 = dash.el (Magnar
;; Sveen、list utilities、s.el と並ぶ MELPA foundation、reverse-deps
;; 多数)。
;;
;; s.el と同じ判断: dash.el は pure cons / list / sequence 操作 なので
;; host load + substrate primitive (= car / cdr / mapcar / nth /
;; cl-remove-if / append 等) に乗ったまま動作する。`-each' 系の
;; mutation API は host buffer に触らないので shim 不要。
;;
;; 残ガード = 第 2 onboarding 候補 (= helm / org-mode core / magit) は
;; buffer + window の両方を強く使うため Phase 4 shim の protocol
;; harmonisation 完了後に着手。

;;; Code:

(require 'ert)

(defconst emacs-melpa-real-dash-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/dash.el/dash.el"
    "/usr/share/emacs/site-lisp/elpa/dash/dash.el"
    "/usr/share/emacs/site-lisp/elpa-src/dash/dash.el")
  "Candidate paths for dash.el on disk.")

(defun emacs-melpa-real-dash-el-test--locate ()
  "Return an absolute path to a usable dash.el on the host, or nil."
  (cl-find-if #'file-readable-p emacs-melpa-real-dash-el-test--candidates))

(defmacro emacs-melpa-real-dash-el-test--skip-without-source (&rest body)
  "Skip the test gracefully when dash.el is not on disk."
  (declare (indent 0) (debug t))
  `(let ((src (emacs-melpa-real-dash-el-test--locate)))
     (unless src
       (ert-skip "dash.el not found in any candidate location"))
     (load src nil t)
     ,@body))

;;;; A. pure list subset (= MVP coverage that works without the shim)

(ert-deftest emacs-melpa-real-dash-el-test/loads-cleanly ()
  "dash.el must `load' end-to-end without raising — proves the substrate
serves the cons/list primitives + lexical-binding semantics dash.el
expands its `--' macros against."
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (fboundp '-map))
    (should (fboundp '-filter))
    (should (fboundp '-reduce))
    (should (fboundp '-take))
    (should (fboundp '-concat))))

(ert-deftest emacs-melpa-real-dash-el-test/map ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '(2 3 4)   (-map #'1+ '(1 2 3))))
    (should (equal '("A" "B") (-map #'upcase '("a" "b"))))
    (should (equal '()        (-map #'1+ '())))))

(ert-deftest emacs-melpa-real-dash-el-test/filter-and-remove ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '(2 4 6)
                   (-filter (lambda (x) (zerop (mod x 2))) '(1 2 3 4 5 6))))
    (should (equal '(1 3 5)
                   (-remove (lambda (x) (zerop (mod x 2))) '(1 2 3 4 5 6))))))

(ert-deftest emacs-melpa-real-dash-el-test/reduce ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (= 15  (-reduce #'+ '(1 2 3 4 5))))
    (should (= 120 (-reduce #'* '(1 2 3 4 5))))))

(ert-deftest emacs-melpa-real-dash-el-test/take-and-drop ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '("a" "b" "c") (-take 3 '("a" "b" "c" "d"))))
    (should (equal '("c" "d")     (-drop 2 '("a" "b" "c" "d"))))
    (should (equal '()            (-take 0 '("a"))))))

(ert-deftest emacs-melpa-real-dash-el-test/concat-and-flatten ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '(1 2 3 4)        (-concat '(1 2) '(3 4))))
    (should (equal '(1 2 3 4)        (-concat '(1) '() '(2 3) '(4))))
    (should (equal '(1 2 3 4 5 6)    (-flatten '((1 2) (3 (4)) (5 6)))))))

(ert-deftest emacs-melpa-real-dash-el-test/first-last-nth ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (= 1 (-first-item '(1 2 3))))
    (should (= 3 (-last-item  '(1 2 3))))
    (should (null (-first-item '())))
    (should (null (-last-item  '())))))

(ert-deftest emacs-melpa-real-dash-el-test/partition ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '((1 2) (3 4) (5 6)) (-partition 2 '(1 2 3 4 5 6))))
    (should (equal '((1 2 3) (4 5 6))   (-partition 3 '(1 2 3 4 5 6))))
    (should (equal '()                  (-partition 2 '())))))

(ert-deftest emacs-melpa-real-dash-el-test/zip-and-interleave ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should (equal '((1 . "a") (2 . "b"))
                   (-zip '(1 2) '("a" "b"))))
    (should (equal '(1 "a" 2 "b" 3 "c")
                   (-interleave '(1 2 3) '("a" "b" "c"))))))

(ert-deftest emacs-melpa-real-dash-el-test/contains ()
  (emacs-melpa-real-dash-el-test--skip-without-source
    (should      (-contains-p '(1 2 3) 2))
    (should-not  (-contains-p '(1 2 3) 7))))

(provide 'emacs-melpa-real-dash-el-test)

;;; emacs-melpa-real-dash-el-test.el ends here
