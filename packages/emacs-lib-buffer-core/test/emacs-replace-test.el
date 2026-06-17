;;; emacs-replace-test.el --- ERT for emacs-replace  -*- lexical-binding: t; -*-

;;; Commentary:

;; occur / replace / line-filter tests.  All operations are pure buffer units
;; driven by `string-match' (no `re-search-forward'), so they validate the
;; Layer 2 logic independently of the reader.

;;; Code:

(require 'ert)
(require 'emacs-replace)

;;;; --- occur --------------------------------------------------------

(ert-deftest emacs-replace-test/occur-matches-line-and-pos ()
  (with-temp-buffer
    (insert "alpha 1\nbeta 2\ngamma 3\nalpha 4\n")
    (let ((m (emacs-occur-matches "alpha")))
      (should (= 2 (length m)))
      (should (= 1 (plist-get (nth 0 m) :line)))
      (should (= 1 (plist-get (nth 0 m) :pos)))
      (should (equal "alpha 1" (plist-get (nth 0 m) :text)))
      (should (= 4 (plist-get (nth 1 m) :line)))
      (should (= 24 (plist-get (nth 1 m) :pos))))))

(ert-deftest emacs-replace-test/occur-builds-buffer-and-goto ()
  (with-temp-buffer
    (insert "alpha 1\nbeta 2\ngamma 3\nalpha 4\n")
    (let ((count (emacs-occur "alpha")))
      (unwind-protect
          (progn
            (should (= 2 count))
            (with-current-buffer emacs-occur-buffer-name
              (should (string-match-p "2 matches for" (buffer-string)))
              (should (string-match-p "1:alpha 1" (buffer-string)))
              (should (string-match-p "4:alpha 4" (buffer-string))))
            ;; goto jumps to the 2nd match's source position (line 4)
            (let ((p (emacs-occur-goto 2)))
              (should (= 24 p))
              (should (= (point) p))))
        (when (get-buffer emacs-occur-buffer-name)
          (kill-buffer emacs-occur-buffer-name))))))

;;;; --- replace ------------------------------------------------------

(ert-deftest emacs-replace-test/replace-regexp-counts-and-rewrites ()
  (with-temp-buffer
    (insert "foo1 foo2 foo3")
    (let ((n (emacs-replace-regexp "foo[0-9]" "BAR")))
      (should (= 3 n))
      (should (equal "BAR BAR BAR" (buffer-string))))))

(ert-deftest emacs-replace-test/replace-regexp-no-match-leaves-buffer ()
  (with-temp-buffer
    (insert "nothing here")
    (should (= 0 (emacs-replace-regexp "zzz+" "X")))
    (should (equal "nothing here" (buffer-string)))))

(ert-deftest emacs-replace-test/replace-string-is-literal ()
  (with-temp-buffer
    (insert "a.b a.b axb")
    (let ((n (emacs-replace-string "a.b" "Z")))
      (should (= 2 n))               ; literal "a.b" only; "axb" untouched
      (should (equal "Z Z axb" (buffer-string))))))

(ert-deftest emacs-replace-test/how-many ()
  (with-temp-buffer
    (insert "x x y x")
    (should (= 3 (emacs-replace-how-many "x")))
    (should (= 0 (emacs-replace-how-many "q")))))

;;;; --- line filters -------------------------------------------------

(ert-deftest emacs-replace-test/flush-lines-drops-matching ()
  (with-temp-buffer
    (insert "keep1\ndrop me\nkeep2\ndrop me too\n")
    (let ((removed (emacs-replace-flush-lines "drop")))
      (should (= 2 removed))
      (should (equal "keep1\nkeep2\n" (buffer-string))))))

(ert-deftest emacs-replace-test/keep-lines-keeps-matching ()
  (with-temp-buffer
    (insert "yes a\nno b\nyes c\n")
    (let ((removed (emacs-replace-keep-lines "yes")))
      (should (= 1 removed))
      (should (equal "yes a\nyes c\n" (buffer-string))))))

;;;; --- query-replace ------------------------------------------------

(ert-deftest emacs-replace-test/query-replace-honours-decisions ()
  (with-temp-buffer
    (insert "x A x B x C")
    (goto-char (point-min))
    (let* ((decisions (list 'act 'skip 'act))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "x" "Z" decide)))
      (should (= 2 n))
      (should (equal "Z A x B Z C" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-act-all ()
  (with-temp-buffer
    (insert "a a a a")
    (goto-char (point-min))
    (let* ((decisions (list 'skip 'act-all))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "a" "Z" decide)))
      (should (= 3 n))               ; first skipped, the remaining three replaced
      (should (equal "a Z Z Z" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-quit-stops ()
  (with-temp-buffer
    (insert "a a a")
    (goto-char (point-min))
    (let* ((decisions (list 'act 'quit))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "a" "Z" decide)))
      (should (= 1 n))
      (should (equal "Z a a" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-regexp-backref ()
  (with-temp-buffer
    (insert "f(1) f(2)")
    (goto-char (point-min))
    (let ((n (emacs-query-replace-regexp "f(\\([0-9]\\))" "g[\\1]"
                                         (lambda (_m _b _e) 'act))))
      (should (= 2 n))
      (should (equal "g[1] g[2]" (buffer-string))))))

(provide 'emacs-replace-test)

;;; emacs-replace-test.el ends here
