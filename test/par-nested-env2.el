;;; -*- lexical-binding: t; -*-
;; Nested closure env view, review-fix rows (F299).  Spec = GNU 31.1 (par-nested-env2.gnu).
;; Rows named "KD ..." are KNOWN-DIV: they are printed, and compared with the recorded
;; old-binary output (par-nested-env2.old4983, base-4983 = pre b665d7f30), not with GNU.
;;  KD A4b/A15: a body defvar in a frame with NO skipped special: no sentinel (lmode 0) so the
;;    defvars trail the env, GNU puts them first.  Fixing it needs a sentinel on every call
;;    (one cons per call even for envs without specials) or a defvar boundary marker: not local.
;;  KD B*: regime B (a lexical pair in the creating frame): GNU's cconv trims the env, NeLisp never has.
(defvar nv-s1 1) (defvar nv-s2 2)
(defmacro row (n f) `(princ (format "%-30s %S\n" ,n (condition-case e ,f (error (list 'ERR e))))))
(defun nv (al body) (let ((o (eval `(lambda () ,body) al))) (delq t (copy-sequence (aref (funcall o) 2)))))
(row "KD A4b defvar no specials" (nv '(nv-ns1 t) '(progn (defvar nv-late) (lambda () 1))))
(row "A14 two defvars" (nv '(nv-s1 nv-ns1 t) '(progn (defvar nv-l1) (defvar nv-l2) (lambda () 1))))
(row "KD A15 two defvars no spec" (nv '(nv-ns1 t) '(progn (defvar nv-l1) (defvar nv-l2) (lambda () 1))))
(row "A16 defvar dup of defvar" (nv '(nv-s1 nv-ns1 t) '(progn (defvar nv-l1) (defvar nv-l1) (lambda () 1))))
;; GC loop around nested creation: 450 names (~90% special), collect before every creation
(defun nvg-names (n) (let (l (i 0)) (dotimes (_ n) (push (intern (format "nvg-v%d" (setq i (1+ i)))) l)) (nreverse l)))
(defvar nvg-all (let ((r nil) (i 0)) (dolist (s (nvg-names 450)) (setq i (1+ i)) (when (/= (% i 10) 0) (eval `(defvar ,s 0) t)) (push s r)) (nreverse r)))
(row "GC loop nested creation"
     (let* ((o (eval '(lambda () (garbage-collect) (lambda () 1)) (append nvg-all '(t))))
            (ok 0) (bad 0))
       (dotimes (_ 300) (let ((v (delq t (copy-sequence (aref (funcall o) 2))))) (if (equal v nvg-all) (setq ok (1+ ok)) (setq bad (1+ bad)))))
       (list ok bad (equal (delq t (copy-sequence (aref o 2))) nvg-all))))
(row "GC loop 2000 calls"
     (let* ((o (eval '(lambda () (lambda () 1)) (append nvg-all '(t)))) (bad 0))
       (dotimes (i 2000) (when (= (% i 500) 0) (garbage-collect)) (unless (equal (delq t (copy-sequence (aref (funcall o) 2))) nvg-all) (setq bad (1+ bad))))
       bad))
(row "signal caught then nested"
     (let ((o (eval '(lambda () (condition-case nil (signal 'error nil) (error nil)) (lambda () 1)) '(nv-s1 nv-ns1 t))))
       (delq t (copy-sequence (aref (funcall o) 2)))))
;; regime B: a lexical pair in the creating frame (KNOWN-DIV vs GNU, oracle = old binary)
(row "KD B1 let pair" (nv '(nv-s1 nv-ns1 t) '(let ((a 1)) (lambda () a))))
(row "KD B2 let pair, binds ns1" (nv '(nv-ns1 t) '(let ((a 1)) (lambda () (let ((nv-ns1 2)) (list a nv-ns1))))))
(row "KD B3 param call" (let* ((o (eval '(lambda (a) (lambda () a)) '(nv-s1 nv-ns1 t)))) (delq t (copy-sequence (aref (funcall o 5) 2)))))
(row "KD B4 param + let + special" (let* ((o (eval '(lambda (a) (let ((b 2)) (lambda () (+ a b nv-s1)))) '(nv-s1 nv-ns1 nv-s2 t)))) (delq t (copy-sequence (aref (funcall o 5) 2)))))
(row "B5 pair value kept" (let* ((o (eval '(lambda (a) (lambda () a)) '(nv-s1 nv-ns1 t))) (c (funcall o 5))) (funcall c)))
