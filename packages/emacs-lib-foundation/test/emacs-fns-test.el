;;; emacs-fns-test.el --- Tests for emacs-fns  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `emacs-fns' Emacs C core port (= mapcar / mapconcat /
;; mapc / nreverse / reverse / plist-{get,put,member} / provide).
;;
;; Under regular Emacs every function is already provided by the C
;; core, so the polyfill `unless (fboundp ...)' guards make our
;; definitions inert.  These tests still exercise the symbols since
;; they are bound either way, and any divergence between the polyfill
;; and the Emacs C original would surface here when the file is loaded
;; under NeLisp standalone.

;;; Code:

(require 'ert)
(require 'emacs-fns)

;;;; --- mapcar -------------------------------------------------------------

(ert-deftest emacs-fns-test/mapcar-basic ()
  (should (equal (mapcar #'1+ '(1 2 3)) '(2 3 4))))

(ert-deftest emacs-fns-test/mapcar-empty ()
  (should (equal (mapcar #'1+ nil) nil)))

(ert-deftest emacs-fns-test/mapcar-preserves-order ()
  (should (equal (mapcar #'identity '(:a :b :c :d)) '(:a :b :c :d))))


;;;; --- mapc ---------------------------------------------------------------

(ert-deftest emacs-fns-test/mapc-side-effects-and-returns-sequence ()
  (let ((collected nil))
    (let ((seq '(1 2 3)))
      (let ((ret (mapc (lambda (x) (setq collected (cons x collected))) seq)))
        ;; Mapc returns the original sequence.
        (should (eq ret seq))
        ;; Side-effects accumulated all elements.
        (should (equal (sort collected #'<) '(1 2 3)))))))


;;;; --- mapconcat ----------------------------------------------------------

(ert-deftest emacs-fns-test/mapconcat-joins-with-separator ()
  (should (equal (mapconcat #'identity '("a" "b" "c") ",") "a,b,c")))

(ert-deftest emacs-fns-test/mapconcat-empty-sequence-is-empty-string ()
  (should (equal (mapconcat #'identity nil ",") "")))

(ert-deftest emacs-fns-test/mapconcat-single-element-no-separator ()
  (should (equal (mapconcat #'identity '("only") "/") "only")))


;;;; --- reverse / nreverse -------------------------------------------------

(ert-deftest emacs-fns-test/reverse-does-not-mutate ()
  (let* ((src '(1 2 3))
         (rev (reverse src)))
    (should (equal rev '(3 2 1)))
    (should (equal src '(1 2 3)))))

(ert-deftest emacs-fns-test/nreverse-returns-reversed ()
  (should (equal (nreverse (list 1 2 3)) '(3 2 1))))

(ert-deftest emacs-fns-test/reverse-empty ()
  (should (equal (reverse nil) nil)))


;;;; --- plist-get / member / put -------------------------------------------

(ert-deftest emacs-fns-test/plist-get-finds-key ()
  (should (equal (plist-get '(:a 1 :b 2 :c 3) :b) 2)))

(ert-deftest emacs-fns-test/plist-get-missing-returns-nil ()
  (should (null (plist-get '(:a 1 :b 2) :z))))

(ert-deftest emacs-fns-test/plist-get-uses-eq ()
  ;; Symbols compare via eq; strings would NOT match (= same as Emacs default).
  (should (equal (plist-get '(:k "v") :k) "v"))
  (should (null (plist-get '("k" "v") "k"))))

(ert-deftest emacs-fns-test/plist-member-returns-tail ()
  (let ((tail (plist-member '(:a 1 :b 2 :c 3) :b)))
    (should (equal tail '(:b 2 :c 3)))))

(ert-deftest emacs-fns-test/plist-member-missing-returns-nil ()
  (should (null (plist-member '(:a 1) :z))))

(ert-deftest emacs-fns-test/plist-put-replaces-existing ()
  (let ((result (plist-put (list :a 1 :b 2) :a 99)))
    (should (equal (plist-get result :a) 99))
    (should (equal (plist-get result :b) 2))))

(ert-deftest emacs-fns-test/plist-put-appends-new ()
  (let ((result (plist-put (list :a 1) :b 2)))
    (should (equal (plist-get result :a) 1))
    (should (equal (plist-get result :b) 2))))


;;;; --- provide ------------------------------------------------------------

(ert-deftest emacs-fns-test/provide-accepts-subfeatures ()
  (let ((features (remove 'emacs-fns-test-provide-subfeatures features)))
    (should (eq (provide 'emacs-fns-test-provide-subfeatures
                         '(remote-wildcards))
                'emacs-fns-test-provide-subfeatures))
    (should (featurep 'emacs-fns-test-provide-subfeatures))))


(provide 'emacs-fns-test)

;;; emacs-fns-test.el ends here
