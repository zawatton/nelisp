;;; nelisp-harness-prep-trim-test.el --- structural checks for harness prep trimming -*- lexical-binding: t; -*-
;; Structural ERT: each predicate must hold for the trimmed files and fail for
;; the pre-change originals kept under tools/ai/sbcl-impl-evidence/s2-harness-trim/orig/
;; (negative control).
(require 'ert)
(require 'cl-lib)
(defconst harness-prep-trim--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defun harness-prep-trim--read (rel &optional orig)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name rel (if orig
                               (or (getenv "NELISP_HARNESS_ORIG_ROOT")
                                   (expand-file-name "tools/ai/sbcl-impl-evidence/s2-harness-trim/orig/"
                                                     harness-prep-trim--root))
                             harness-prep-trim--root)))
    (buffer-string)))
(defconst harness-prep-trim--producer "tools/nelisp-native-application-producer.el")
(defconst harness-prep-trim--driver "tools/bench/startup-phase/benchmark-driver-native-on-phased.el")
(defconst harness-prep-trim--nested "tools/bench/startup-phase/runtime-bootstrap-on.el")
(defun harness-prep-trim--has (text re) (string-match-p re text))
(defun harness-prep-trim--prelude-hash-guarded-p (text)
  (harness-prep-trim--has text "(when hotpatch\n[^\n]*secure-hash 'sha256\n[^\n]*prelude 8388608"))
(defun harness-prep-trim--driver-ok (text)
  (and (not (harness-prep-trim--has text "(:prelude \""))
       (harness-prep-trim--has text ":selected-definitions t (unless (equal names all-names)")
       (= 1 (1- (length (split-string text "--check-candidates expected-plan"))))))
(defun harness-prep-trim--nested-ok (text)
  (and (not (harness-prep-trim--has text "(:prelude \""))
       (= 1 (1- (length (split-string text "--check-candidates expected-plan"))))))
(defun harness-prep-trim--producer-ok (text)
  (and (harness-prep-trim--prelude-hash-guarded-p text)
       (harness-prep-trim--has text "defer-baseline keep-names")
       (harness-prep-trim--has text ":deselected-definition-names")
       (harness-prep-trim--has text ":prelude-verified hotpatch")
       ;; install-candidates keeps exactly one check before binding and none duplicated after compile
       (= 3 (1- (length (split-string text "(nelisp-native-application-producer--check-candidates plan candidates"))))))
(ert-deftest harness-prep-trim-producer ()
  (should (harness-prep-trim--producer-ok (harness-prep-trim--read harness-prep-trim--producer))))
(ert-deftest harness-prep-trim-driver ()
  (should (harness-prep-trim--driver-ok (harness-prep-trim--read harness-prep-trim--driver)))
  (should (harness-prep-trim--nested-ok (harness-prep-trim--read harness-prep-trim--nested))))
(ert-deftest harness-prep-trim-negative-control ()
  (should-not (harness-prep-trim--producer-ok (harness-prep-trim--read harness-prep-trim--producer t)))
  (should-not (harness-prep-trim--driver-ok (harness-prep-trim--read harness-prep-trim--driver t)))
  (should-not (harness-prep-trim--nested-ok (harness-prep-trim--read harness-prep-trim--nested t))))
(provide 'nelisp-harness-prep-trim-test)
