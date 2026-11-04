;;; subr-x-test.el --- ERT for lightweight subr-x facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)

(ert-deftest subr-x-test/require-loads-standard-feature ()
  (should (featurep 'subr-x))
  (dolist (sym '(thread-first thread-last hash-table-empty-p hash-table-keys
                              hash-table-values string-remove-prefix
                              string-remove-suffix string-replace
                              string-truncate-left string-limit string-pad
                              string-chop-newline named-let proper-list-p
                              mapcan))
    (should (fboundp sym))))

(ert-deftest subr-x-test/threading-macros ()
  (should (= (thread-first 5 (+ 20) (/ 25)) 1))
  (should (equal (thread-last '(1 2 3) (mapcar #'1+) reverse)
                 '(4 3 2))))

(ert-deftest subr-x-test/hash-table-values-and-empty ()
  (let ((table (make-hash-table :test 'equal)))
    (should (hash-table-empty-p table))
    (puthash "a" 1 table)
    (puthash "b" 2 table)
    (should-not (hash-table-empty-p table))
    (should (equal (sort (hash-table-keys table) #'string<)
                   '("a" "b")))
    (should (equal (sort (hash-table-values table) #'<)
                   '(1 2)))))

(ert-deftest subr-x-test/string-helpers ()
  (should (string= (string-remove-prefix "foo" "foobar") "bar"))
  (should (string= (string-remove-prefix "no" "foobar") "foobar"))
  (should (string= (string-remove-suffix "bar" "foobar") "foo"))
  (should (string= (string-replace "aa" "b" "aaaaa") "bba"))
  (should (string= (string-truncate-left "abcdef" 5) "...ef"))
  (should (string= (string-limit "abcdef" 3) "abc"))
  (should (string= (string-limit "abcdef" 3 t) "def"))
  (should (string= (string-pad "ab" 4 ?.) "ab.."))
  (should (string= (string-pad "ab" 4 ?. t) "..ab"))
  (should (string= (string-chop-newline "line\n") "line")))

(ert-deftest subr-x-test/proper-list-p-and-mapcan ()
  (should (= (proper-list-p '(1 2 3)) 3))
  (should (= (proper-list-p nil) 0))
  (should-not (proper-list-p '(1 . 2)))
  (should (equal (mapcan (lambda (x) (list x (- x))) '(1 2 3))
                 '(1 -1 2 -2 3 -3))))

(ert-deftest subr-x-test/named-let-recurses ()
  (should (= (named-let loop ((n 5) (acc 1))
              (if (= n 0)
                  acc
                (loop (1- n) (* acc n))))
             120)))

;; S2 coverage batch (2026-09-28): text-property and buffer-text helpers
;; ported verbatim from GNU Emacs 31.1's `subr-x.el'.  Values pinned
;; against real GNU Emacs 31.1.

(ert-deftest subr-x-test/s2-batch-fboundp ()
  (dolist (sym '(string-fill add-display-text-property
                 add-remove--display-text-property
                 remove-display-text-property
                 emacs-etc--hide-local-variables))
    (should (fboundp sym))))

(ert-deftest subr-x-test/string-fill-wraps-at-width ()
  (should (string= (string-fill "aaaa bbbb cccc" 9) "aaaa bbbb\ncccc"))
  ;; A single word wider than WIDTH is left alone rather than split.
  (should (string= (string-fill "aaaaaaaaaa" 4) "aaaaaaaaaa")))

(ert-deftest subr-x-test/display-text-property-add-remove ()
  ;; A single spec is stored bare (a scalar `display' value), not wrapped
  ;; in an extra list -- matches real GNU Emacs 31.1.
  (should (equal
           (with-temp-buffer
             (insert "hello world")
             (add-display-text-property 1 6 'height 2.0)
             (get-text-property 1 'display))
           '(height 2.0)))
  ;; A second spec promotes the property to a list of specs, most recent
  ;; first, retaining the earlier one.
  (should (equal
           (with-temp-buffer
             (insert "hello world")
             (add-display-text-property 1 6 'height 2.0)
             (add-display-text-property 1 6 'raise 0.1)
             (get-text-property 1 'display))
           '((raise 0.1) (height 2.0))))
  ;; `remove-display-text-property' drops only the named spec.
  (should (equal
           (with-temp-buffer
             (insert "hello world")
             (add-display-text-property 1 6 'height 2.0)
             (add-display-text-property 1 6 'raise 0.1)
             (remove-display-text-property 1 6 'height)
             (get-text-property 1 'display))
           '((raise 0.1))))
  ;; OBJECT may be a string instead of a buffer.
  (should (equal
           (let ((s (copy-sequence "hello world")))
             (add-display-text-property 1 6 'height 2.0 s)
             (get-text-property 1 'display s))
           '(height 2.0))))

(ert-deftest subr-x-test/hide-local-variables-narrows-before-marker ()
  ;; Real usage (`emacs-authors-mode' &c.) always has a blank line before
  ;; the "Local Variables:" block; `forward-line -1' from the marker then
  ;; lands right after the body, excluding the blank separator too.
  (should (equal
           (with-temp-buffer
             (insert "body text\n\nLocal Variables:\nfoo: 1\nEnd:\n")
             (emacs-etc--hide-local-variables)
             (cons (point-min) (point-max)))
           (cons 1 11)))
  ;; No "Local Variables:" marker: narrowing is a no-op (widens to eob).
  (should (equal
           (with-temp-buffer
             (insert "body text only\n")
             (emacs-etc--hide-local-variables)
             (cons (point-min) (point-max)))
           (cons 1 16))))

(provide 'subr-x-test)

;;; subr-x-test.el ends here
