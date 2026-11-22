;;; aot-run.el --- host helper: compile the AOT arith cases, emit an in-image run script  -*- lexical-binding: t; -*-
;; emacs --batch -L LANE/lisp -L LANE/src -l aot-run.el --eval '(overlay-aot-prepare LANE OUTDIR)'
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(defun overlay-aot-prepare (lane outdir)
  "Compile LANE's arith cases to OUTDIR/all.neln and write OUTDIR/run.el."
  (load (expand-file-name "test/nelisp-aot-lisp-arith-cases.el" lane) nil t)
  (make-directory outdir t)
  (let ((el (expand-file-name "all.el" outdir))
        (neln (expand-file-name "all.neln" outdir))
        (run (expand-file-name "run.el" outdir)))
    (write-region nelisp-aot-lisp-arith-cases-source nil el nil 'silent)
    (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln 'opportunistic)
    (write-region
     (format "(unless (featurep 'nelisp-native-load) (require 'nelisp-native-load))
(load %S nil t t)
(defvar dt-handles nil)
(defun dt-call (name args)
  (let ((h (cdr (assoc name dt-handles))))
    (unless h (setq h (nelisp-native-load-artifact %S name))
            (setq dt-handles (cons (cons name h) dt-handles)))
    (nelisp-native-load-call h args)))
(dolist (l (nelisp-aot-lisp-arith-cases-run (function dt-call))) (princ \"AOT \") (princ l) (princ \"\\n\"))
(princ \"AOT-DONE\\n\")\n"
             (expand-file-name "test/nelisp-aot-lisp-arith-cases.el" lane) neln)
     nil run nil 'silent)))
(provide 'aot-run)
