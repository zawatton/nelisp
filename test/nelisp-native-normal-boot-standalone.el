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
;; --- F58 additions ---
(defun nnb--scan (b)
  (condition-case e (nelisp--tp-scan-fwd (aref b 9) 0 1 nil 24 t 'face) (error (list 'err e))))
(defun nnb--time (b n)
  (let ((t0 (float-time))) (dotimes (_ n) (nnb--scan b)) (- (float-time) t0)))
(let ((b (generate-new-buffer "nnb2")))
  (set-buffer b)
  (insert "hello world hello world")
  (put-text-property 1 3 'face 'bold)
  (let ((r0 (nnb--scan b)))
    ;; The chain below scan-fwd is registered when scan-fwd converts.
    (nnb--check "scan-fwd native" (nnb--native-p 'nelisp--tp-scan-fwd))
    (nnb--check "closure callee textget native" (nnb--native-p 'nelisp--tp-textget))
    (nnb--check "closure callee plists-equal native" (nnb--native-p 'nelisp--tp-plists-equal))
    ;; Native code really runs: the same record on the VM is much slower.
    (nnb--time b 200)
    (let ((nat (min (nnb--time b 2000) (nnb--time b 2000))) (vm nil))
      (nelisp-nc-select 'nelisp--tp-scan-fwd 'vm)
      (setq vm (min (nnb--time b 2000) (nnb--time b 2000)))
      (nelisp-nc-select 'nelisp--tp-scan-fwd 'native)
      (nnb--check (format "native executes (vm %.0f us vs native %.0f us per 2000)" (* 1e6 vm) (* 1e6 nat))
                  (> vm (* 1.5 nat)))
      (nnb--check "native record restored" (nnb--native-p 'nelisp--tp-scan-fwd)))
    ;; A callee registered by the recursion, redefined later, fset back.
    (let ((old (symbol-function 'nelisp--tp-textget)))
      (fset 'nelisp--tp-textget (lambda (&rest _) 'zzz))
      (let ((r1 (nnb--scan b)))
        (nnb--check "recursively registered callee redefined changes result" (not (equal r1 r0))))
      (fset 'nelisp--tp-textget old)
      (nnb--check "callee restored gives the original result" (equal (nnb--scan b) r0)))
    ;; advice-add on a callee.
    (if (not (fboundp 'advice-add))
        (nnb--check "advice-add unavailable (skipped)" t)
      (let ((adv (lambda (f &rest a) (ignore f a) 'advised)))
        (advice-add 'nelisp--tp-textget :around adv)
        (let ((r2 (nnb--scan b)))
          (advice-remove 'nelisp--tp-textget adv)
          (nnb--check "advice on a callee takes effect" (not (equal r2 r0)))
          (nnb--check "advice removed gives the original result" (equal (nnb--scan b) r0)))))
    ;; fset of the top-level row function.
    (let ((r0 (next-single-property-change 1 'face)) (old (symbol-function 'nelisp--w202-property-change)))
      (fset 'nelisp--w202-property-change (lambda (&rest _) 'top))
      (nnb--check "fset of the top-level row function takes effect"
                  (not (equal (next-single-property-change 1 'face) r0)))
      (fset 'nelisp--w202-property-change old)
      (nnb--check "top-level function restored" (equal (next-single-property-change 1 'face) r0)))
    ;; A stale `none' on a callee becomes native when its caller registers.
    (let* ((cf (nelisp--closure-cons (symbol-function 'nelisp--tp-plists-equal)))
           (pf (nelisp--closure-cons (symbol-function 'nelisp--tp-scan-fwd))))
      (remhash (nthcdr 3 pf) nelisp--fx-table)
      (puthash (nthcdr 3 cf) 'none nelisp--fx-table)
      (nelisp--fx-register 'nelisp--tp-scan-fwd)
      (nnb--check "stale none callee becomes native" (nnb--native-p 'nelisp--tp-plists-equal))
      (nnb--check "caller native again" (nnb--native-p 'nelisp--tp-scan-fwd))
      (nnb--check "result unchanged" (equal (nnb--scan b) r0)))))
(princ (format "NNB-DONE fail=%d\n" nnb--fail))
(kill-emacs (if (= nnb--fail 0) 0 1))
