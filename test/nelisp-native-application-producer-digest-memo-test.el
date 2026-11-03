;;; nelisp-native-application-producer-digest-memo-test.el --- digest memo -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; `--form-digest' must reuse work for an unchanged form and must never serve a
;; digest for a form edited in place.  NELISP_DIGEST_MEMO_NEGATIVE=1 swaps in a
;; helper keyed by identity only (no structural recheck); the stale test must fail.
;;; Code:
(require 'ert)
(require 'cl-lib)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../lisp/nelisp-native-application-protocol.el" dir) nil t)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "../tools/nelisp-native-application-producer.el" dir))
    (goto-char (point-min))
    (re-search-forward "^(defvar nelisp-native-application-producer--digest-memo")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)
    (eval (read (current-buffer)) t)))
(when (getenv "NELISP_DIGEST_MEMO_NEGATIVE")
  (defun nelisp-native-application-producer--form-digest (form)
    (let ((hit (gethash form nelisp-native-application-producer--digest-memo)))
      (if hit (cdr hit)
        (let ((d (nelisp-native-application-protocol-digest form)))
          (puthash form (cons nil d) nelisp-native-application-producer--digest-memo)
          d)))))

(ert-deftest nelisp-digest-memo-matches-and-reuses ()
  (let ((form (list 'defun 'f '(x) '(+ x 1)))
        (calls 0))
    (cl-letf* ((orig (symbol-function 'nelisp-native-application-protocol-digest))
               ((symbol-function 'nelisp-native-application-protocol-digest)
                (lambda (d) (cl-incf calls) (funcall orig d))))
      (let ((a (nelisp-native-application-producer--form-digest form))
            (b (nelisp-native-application-producer--form-digest form)))
        (should (equal a (funcall orig form)))
        (should (equal a b))
        (should (= calls 1))))))

(ert-deftest nelisp-digest-memo-stale-after-in-place-edit ()
  (let* ((form (list 'defun 'f '(x) (list '+ 'x 1)))
         (before (nelisp-native-application-producer--form-digest form)))
    (setcar (nthcdr 1 (nth 3 form)) 'x)
    (setcar (nthcdr 2 (nth 3 form)) 2)
    (let ((after (nelisp-native-application-producer--form-digest form)))
      (should-not (equal before after))
      (should (equal after (nelisp-native-application-protocol-digest (copy-tree form)))))))

(ert-deftest nelisp-digest-memo-different-object-same-content ()
  (let ((a (list 'defun 'g '(y) 'y)) (b (list 'defun 'g '(y) 'y)))
    (should (equal (nelisp-native-application-producer--form-digest a)
                   (nelisp-native-application-producer--form-digest b)))))
(provide 'nelisp-native-application-producer-digest-memo-test)
