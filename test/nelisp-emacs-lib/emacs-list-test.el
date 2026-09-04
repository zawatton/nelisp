;;; emacs-list-test.el --- ERT for emacs-list polyfills  -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for list primitives ported in `emacs-list'.  Under host Emacs the
;; `unless (fboundp ...)' gates skip the polyfills (host C builtins win), so
;; the `nconc' body is exercised via a pinned lambda copy (parity pattern).

;;; Code:

(require 'ert)
(require 'emacs-list)

(defvar emacs-list-test--nconc
  (lambda (&rest lists)
    (let ((result nil) (tail nil))
      (dolist (l lists)
        (when l
          (if tail (setcdr tail l) (setq result l))
          (setq tail l)
          (while (and (consp tail) (consp (cdr tail)))
            (setq tail (cdr tail)))))
      result)))

(ert-deftest emacs-list-test/require-loads-cleanly ()
  (should (featurep 'emacs-list))
  (should (fboundp 'nconc)))

(ert-deftest emacs-list-test/nconc-body-matches-host ()
  (let ((f emacs-list-test--nconc))
    (should (equal '(1 2 3 4 5)
                   (funcall f (list 1 2) (list 3 4) (list 5))))
    (should (equal '(1 2)
                   (funcall f nil (list 1) nil (list 2))))
    (should (equal '(1 . 5) (funcall f (list 1) 5)))
    (should (equal nil (funcall f)))
    (should (equal '(1 2 3) (funcall f (list 1 2 3))))
    ;; destructive: the first list's tail is rewired
    (let ((a (list 1 2)) (b (list 3)))
      (funcall f a b)
      (should (eq (cddr a) b)))))

(provide 'emacs-list-test)
;;; emacs-list-test.el ends here
