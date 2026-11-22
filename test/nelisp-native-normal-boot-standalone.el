;;; nelisp-native-normal-boot-standalone.el --- native rows in a normal boot  -*- lexical-binding: t; -*-

;; F43/F50: a normal boot (`exec-runtime-image') must run the harvested native
;; code of a prelude function like the cold-image boot does, and a redefinition
;; must still take effect.  Run by test/nelisp-native-normal-boot-smoke.sh.
;; Prints PASS/FAIL lines; "NNB-DONE" last.

(defvar nnb--fail 0)
(defun nnb--check (name ok)
  (unless ok (setq nnb--fail (1+ nnb--fail)))
  (princ (format "%s %s\n" (if ok "PASS" "FAIL") name)))
(defun nnb--rec (name)
  (let* ((fn (nelisp--closure-cons (symbol-function name)))
         (r (and (consp fn) (gethash (nthcdr 3 fn) nelisp--fx-table))))
    (and (vectorp r) (aref r 1))))
(defun nnb--native-p (name)
  (let ((n (nnb--rec name))) (and (integerp n) (< n -15))))

(let ((b (generate-new-buffer "nnb")))
  (set-buffer b)
  (insert "hello world")
  (put-text-property 1 3 'face 'bold)
  (let ((r0 (next-single-property-change 1 'face)))
    (nnb--check "property-change result" (eql r0 3))
    (dolist (n '(next-single-property-change nelisp--w202-property-change
                 nelisp--tp-validate nelisp--tp-scan-fwd nelisp--w202-position))
      (nnb--check (format "%s is native after first call" n) (nnb--native-p n)))
    ;; Redefinition: a callee redefined after the install must take effect in
    ;; its callers, and the original native code must come back unchanged.
    (let ((old (symbol-function 'nelisp--w202-position)) (after nil) (back nil))
      (fset 'nelisp--w202-position (lambda (&rest _) 5))
      (setq after (condition-case e (nelisp--w202-property-change 1 nil nil nil t 'face) (error e)))
      (fset 'nelisp--w202-position old)
      (setq back (nelisp--w202-property-change 1 nil nil nil t 'face))
      (nnb--check "redefined callee changes caller result" (not (equal after back)))
      (nnb--check "restored callee gives the original result" (eql back r0))
      (nnb--check "restored callee is native again" (nnb--native-p 'nelisp--w202-position)))
    (let ((old (symbol-function 'nelisp--w202-position)))
      (fset 'nelisp--w202-position (lambda (&rest _) 7))
      (nnb--check "a new body gets no native record" (not (nnb--native-p 'nelisp--w202-position)))
      (fset 'nelisp--w202-position old))))
(princ (format "NNB-DONE fail=%d\n" nnb--fail))
(kill-emacs (if (= nnb--fail 0) 0 1))
