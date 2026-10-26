;;; nelisp-fxir-scan.el --- fxir structure over every fx record  -*- lexical-binding: t; -*-
;; For every named closure whose live fx record is compiled code: compile it
;; again with fxir on (`nelisp-fxir-recompile') and check that the code is the
;; live record's (the fxir walk changed nothing the VM runs), that the fxir
;; exists, and that every operand index names the code word of its opcode.
;; Prints SCAN records=N same=N ir=N bad=N and the first problems.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(let ((n 0) (same 0) (irn 0) (bad nil) (none 0) (seen (make-hash-table :test 'eq)))
  (dolist (s (let ((names nil))
               (maphash (lambda (_b r) (when (and (vectorp r) (symbolp (aref r 4)) (aref r 4))
                                         (push (aref r 4) names)))
                        nelisp--fx-table)
               names))
     (let* ((fn (and (fboundp s) (symbol-function s)))
            (rec (and (eq (car-safe fn) 'closure) (gethash (nthcdr 3 fn) nelisp--fx-table))))
       (when (and (vectorp rec) (not (gethash rec seen)) (/= (aref rec 1) -1))
         (puthash rec t seen)
         (setq n (1+ n))
         (let ((c (nelisp-fxir-recompile fn)))
           (cond
            ((null c) (setq none (1+ none)) (push (list s :recompile-none) bad))
            (t
             (if (nelisp-fxir-code-same-p (car c) (aref rec 0)) (setq same (1+ same))
               (push (list s :code-differs) bad))
             (if (null (nth 2 c)) (push (list s :no-ir) bad)
               (setq irn (1+ irn))
               (let ((p (nelisp-fxir-problems (nth 2 c) (car c) (max 0 (nth 1 c)))))
                 (when p (push (cons s p) bad))))))))))
  (princ (format "SCAN records=%d same=%d ir=%d bad=%d recompile-none=%d\n" n same irn (length bad) none))
  (let ((i 0)) (dolist (b bad) (when (< i 15) (princ (format "BAD %S\n" b))) (setq i (1+ i)))))
(kill-emacs 0)
