;;; emacs-subr-extras-test.el --- ERT for Phase B2 subr.el shims  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;; Tests cover the four subr.el primitives + 15 four-level cdr/car
;; aliases ported in `emacs-subr-extras.el'.  Behaviour mirrors host
;; Emacs so these run identically under the test harness and on
;; standalone NeLisp.

(require 'ert)
(require 'emacs-subr-extras)

;;;; number-sequence

(ert-deftest emacs-subr-extras-number-sequence-basic ()
  (should (equal (number-sequence 1 5) '(1 2 3 4 5))))

(ert-deftest emacs-subr-extras-number-sequence-step-2 ()
  (should (equal (number-sequence 0 10 2) '(0 2 4 6 8 10))))

(ert-deftest emacs-subr-extras-number-sequence-descending ()
  (should (equal (number-sequence 5 1 -1) '(5 4 3 2 1))))

(ert-deftest emacs-subr-extras-number-sequence-single-arg ()
  (should (equal (number-sequence 7) '(7))))

(ert-deftest emacs-subr-extras-number-sequence-empty-asc-when-from-gt-to ()
  (should (equal (number-sequence 5 3) nil)))

;;;; assoc-default

(ert-deftest emacs-subr-extras-assoc-default-found ()
  (should (equal (assoc-default 'b '((a . 1) (b . 2) (c . 3))) 2)))

(ert-deftest emacs-subr-extras-assoc-default-missing-returns-nil ()
  ;; Real `assoc-default' returns nil when no element matches;
  ;; DEFAULT is only used for matched-but-not-cons entries.
  (should (null (assoc-default 'z '((a . 1)) nil 'sentinel))))

(ert-deftest emacs-subr-extras-assoc-default-bare-key-uses-default ()
  ;; Bare-key match (= element is symbol, not cons) returns DEFAULT.
  (should (eq (assoc-default 'b '(a b c) nil 'sentinel) 'sentinel)))

(ert-deftest emacs-subr-extras-assoc-default-with-test-fn ()
  (should (equal (assoc-default "B" '(("a" . 1) ("b" . 2))
                                (lambda (k v) (string= (downcase k) (downcase v))))
                 2)))

(ert-deftest emacs-subr-extras-assoc-default-skips-nil-entries ()
  (should (equal (assoc-default 'a '(nil (a . 1) nil)) 1)))

;;;; string-join

(ert-deftest emacs-subr-extras-string-join-default-sep ()
  (should (string= (string-join '("foo" "bar" "baz")) "foobarbaz")))

(ert-deftest emacs-subr-extras-string-join-with-sep ()
  (should (string= (string-join '("a" "b" "c") "-") "a-b-c")))

(ert-deftest emacs-subr-extras-string-join-empty-list ()
  (should (string= (string-join nil) "")))

;;;; member-ignore-case

(ert-deftest emacs-subr-extras-member-ignore-case-found ()
  (let ((tail (member-ignore-case "BAR" '("foo" "bar" "baz"))))
    (should (equal tail '("bar" "baz")))))

(ert-deftest emacs-subr-extras-member-ignore-case-missing ()
  (should (null (member-ignore-case "qux" '("foo" "bar")))))

;;;; four-level cdr/car (spot-check 4 of 15 — full coverage in cl-lib's defaliases)

(ert-deftest emacs-subr-extras-caaaar ()
  (should (= (caaaar '((((1 . a)))) ) 1)))

(ert-deftest emacs-subr-extras-cdddar ()
  ;; cdddar = cdr(cdr(cdr(car x)))
  (should (equal (cdddar '((1 2 3 4 5) ignored)) '(4 5))))

(ert-deftest emacs-subr-extras-cddddr ()
  ;; cddddr = cdr(cdr(cdr(cdr x))) → 5th element onward
  (should (equal (cddddr '(1 2 3 4 5 6 7)) '(5 6 7))))

(ert-deftest emacs-subr-extras-cadadr ()
  ;; cadadr = car(cdr(car(cdr x)))
  ;; Walk through:
  ;;   x       = ((10) (20 99) 30)
  ;;   cdr x   = ((20 99) 30)
  ;;   car cdr = (20 99)
  ;;   cdr car cdr = (99)
  ;;   car cdr car cdr = 99
  (should (= (cadadr '((10) (20 99) 30)) 99)))

;;;; vendored cl-lib.el load smoke (= the actual symptom we are fixing)

(ert-deftest emacs-subr-extras-fboundp-after-require ()
  (should (fboundp 'number-sequence))
  (should (fboundp 'assoc-default))
  (should (fboundp 'string-join))
  (should (fboundp 'member-ignore-case))
  (dolist (s '(caaaar caaadr caadar caaddr cadaar cadadr caddar
               cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
    (should (fboundp s))))

(provide 'emacs-subr-extras-test)
;;; emacs-subr-extras-test.el ends here
