;;; nelisp-aot-fxloop-gen.el --- differential generator for fixnum loops (aot-regs)  -*- lexical-binding: t; -*-
;;
;; Host side of the in-image differential.  Usage (host Emacs, batch):
;;   emacs --batch -L lisp -L src -l test/nelisp-aot-fxloop-gen.el \
;;         -f nelisp-aot-fxloop-gen-run  OUTDIR
;; writes OUTDIR/probe.el (run it in the runtime image, it prints one line per
;; case) and OUTDIR/expected.txt (GNU Emacs answers for the same defuns and
;; arguments).  `nelisp-aot-fxloop-gen-compare' diffs the two per build.

(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nelisp-aot-fxloop-gen--source
  "(defun fx-sum (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (1+ i))) s))
(defun fx-sum2 (n) (let ((i 0) (s 0)) (while (< i n) (setq i (1+ i) s (+ s i))) s))
(defun fx-sum3 (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i) i (1+ i))) s))
(defun fx-acc (n x) (let ((s 0) (i 0)) (while (< i n) (setq s (+ s x) i (1+ i))) s))
(defun fx-two (a b n) (let ((i 0)) (while (< i n) (setq a (+ a 1) b (+ b b) i (1+ i))) (list a b i)))
(defun fx-tree4 (a n) (let ((i 0)) (while (< i n) (setq a (+ (+ a a) (+ a a)) i (1+ i))) a))
(defun fx-tree8 (a n) (let ((i 0)) (while (< i n) (setq a (+ (+ (+ a a) (+ a a)) (+ (+ a a) (+ a a))) i (1+ i))) a))
(defun fx-up (i n) (while (< i n) (setq i (1+ i))) i)
(defun fx-down (i n) (while (> i n) (setq i (1- i))) i)
(defun fx-neq (n) (let ((i n) (c 0)) (while (/= i 0) (setq i (1- i) c (+ c 2))) c))
(defun fx-and (n m) (let ((i 0)) (while (and (< i n) (< i m)) (setq i (1+ i))) i))
(defun fx-neg (n) (let ((i 0) (s 0)) (while (< i n) (setq s (- s i) i (1+ i))) s))
(defun fx-zero (n) (let ((i 5)) (while (< i n) (setq i (1+ i))) i))
(defun fx-not (n) (let ((i 0)) (while (not (>= i n)) (setq i (1+ i))) i))
(defun fx-call (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (abs i)) i (1+ i))) s))
(defun fx-let (n) (let ((i 0)) (while (< i n) (let ((j i)) (setq i (1+ j)))) i))
"
  "Test defuns; fx-call fx-let must be declined by the pass; the special-variable case is in the ERT unit.")

(defconst nelisp-aot-fxloop-gen--mpf 2305843009213693951)
(defconst nelisp-aot-fxloop-gen--mnf -2305843009213693952)

(defun nelisp-aot-fxloop-gen--cases ()
  (let ((mpf nelisp-aot-fxloop-gen--mpf) (mnf nelisp-aot-fxloop-gen--mnf) (b70 (expt 2 70)))
    (append
     (mapcan (lambda (f) (mapcar (lambda (n) (list f (list n))) '(0 1 2 5 1000)))
             '(fx-sum fx-sum2 fx-sum3 fx-neg fx-not fx-zero fx-call fx-let))
     (mapcar (lambda (n) (list 'fx-neq (list n))) '(0 1 7 100))
     `((fx-sum (3.5)) (fx-sum (nil)) (fx-sum (a)) (fx-sum2 (2.0)) (fx-zero (,(- b70)))
       (fx-zero (0.5)) (fx-zero (nil)) (fx-zero (a)) (fx-not (a)) (fx-not (1.5))
       (fx-acc (10 1)) (fx-acc (10 1152921504606846976)) (fx-acc (3 ,(1- (ash 1 61))))
       (fx-acc (5 ,mnf)) (fx-acc (4 ,mpf)) (fx-acc (2 ,b70)) (fx-acc (3 1.5)) (fx-acc (3 nil))
       (fx-two (0 1152921504606846976 5)) (fx-two (0 1 70)) (fx-two (0 1 61)) (fx-two (0 1 62))
       (fx-two (,mpf 1 3)) (fx-two (,(- mpf 2) 1 5)) (fx-two (0 ,mnf 1))
       (fx-tree4 (1 10)) (fx-tree4 (3 20)) (fx-tree4 (-1 40)) (fx-tree8 (1 6)) (fx-tree8 (3 12))
       (fx-up (,(- mpf 3) ,mpf)) (fx-up (,mpf ,(1+ mpf))) (fx-up (,(- mpf 1) ,(+ mpf 5)))
       (fx-up (0 0)) (fx-up (7 3))
       (fx-down (,(+ mnf 3) ,mnf)) (fx-down (,mnf ,(1- mnf))) (fx-down (,(+ mnf 1) ,(- mnf 3)))
       (fx-down (5 5)) (fx-down (3 -2))
       (fx-and (10 4)) (fx-and (4 10)) (fx-and (0 0)) (fx-and (5 1.5))
       (fx-up (,(- mpf 2) ,mpf)) (fx-up (,mnf ,(+ mnf 3))) (fx-sum (100)) (fx-sum2 (70)) (fx-sum3 (200))))))

(defun nelisp-aot-fxloop-gen--build (dir tag flag &optional mutation budget)
  "Compile the test defuns into DIR/TAG.neln with the pass FLAG; return the path."
  (let* ((src (expand-file-name "m.el" dir)) (art (expand-file-name (concat tag ".neln") dir))
         (nelisp-aot-compiler-fixnum-loops flag)
         (nelisp-aot-compiler--fxloop-mutation mutation)
         (nelisp-aot-compiler-fixnum-loop-budget (or budget nelisp-aot-compiler-fixnum-loop-budget))
         (nelisp-aot-compiler--fxloop-log nil))
    (with-temp-file src (insert nelisp-aot-fxloop-gen--source))
    (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
    (cons art (reverse nelisp-aot-compiler--fxloop-log))))

(defun nelisp-aot-fxloop-gen--gnu (fn args)
  (condition-case e (format "%S" (apply fn args)) (error (format "ERR %S" e))))

(defun nelisp-aot-fxloop-gen-run ()
  (let* ((dir (expand-file-name (car command-line-args-left)))
         (builds '(("on" t nil nil) ("off" nil nil nil) ("b7" t nil 7)
                   ("m-old" t old-temps nil) ("m-per" t per-setq-commit nil)
                   ("m-guard" t drop-guard nil) ("m-check" t drop-check nil)))
         (cases (nelisp-aot-fxloop-gen--cases))
         (paths nil) (logs nil))
    (setq command-line-args-left nil)
    (make-directory dir t)
    (with-temp-buffer (insert nelisp-aot-fxloop-gen--source)
                      (goto-char (point-min))
                      (condition-case nil (while t (eval (read (current-buffer)) t)) (end-of-file nil)))
    (dolist (b builds)
      (let ((r (nelisp-aot-fxloop-gen--build dir (nth 0 b) (nth 1 b) (nth 2 b) (nth 3 b))))
        (push (cons (nth 0 b) (car r)) paths)
        (push (cons (nth 0 b) (cdr r)) logs)))
    (with-temp-file (expand-file-name "expected.txt" dir)
      (dolist (c cases)
        (insert (format "%S\t%s\n" c (nelisp-aot-fxloop-gen--gnu (car c) (cadr c))))))
    (with-temp-file (expand-file-name "logs.txt" dir) (prin1 (reverse logs) (current-buffer)))
    (with-temp-file (expand-file-name "probe.el" dir)
      (insert (format "(setq load-path (append (list %S %S) load-path))\n(require 'nelisp-native-load)\n"
                      (expand-file-name "lisp" default-directory) (expand-file-name "src" default-directory)))
      (insert "(defvar fxp-handles nil)\n(defun fxp-h (art fn) (let ((k (cons art fn))) (or (cdr (assoc k fxp-handles)) (let ((h (nelisp-native-load-artifact art fn))) (push (cons k h) fxp-handles) h))))\n")
      (insert (format "(defconst fxp-builds '%S)\n(defconst fxp-cases '%S)\n"
                      (reverse paths) cases))
      (insert "(dolist (b (let ((only (getenv \"FXP_BUILD\"))) (if (and only (> (length only) 0)) (list (assoc only fxp-builds)) fxp-builds))) (dolist (c fxp-cases)
 (princ (format \"%s\\t%S\\t%s\\n\" (car b) c
   (condition-case e (format \"%S\" (nelisp-native-load-call (fxp-h (cdr b) (symbol-name (car c))) (cadr c))) (error (format \"ERR %S\" e)))))))\n"))
    (message "wrote %s" dir)))

(defun nelisp-aot-fxloop-gen-compare (dir)
  "Return an alist (BUILD . (MISMATCHES...)) comparing probe.out with expected.txt."
  (let ((exp (make-hash-table :test #'equal)) (out nil) (res nil))
    (with-temp-buffer (insert-file-contents (expand-file-name "expected.txt" dir))
      (dolist (l (split-string (buffer-string) "\n" t))
        (let ((p (split-string l "\t"))) (puthash (car p) (cadr p) exp))))
    (with-temp-buffer (insert-file-contents (expand-file-name "probe.out" dir))
      (dolist (l (split-string (buffer-string) "\n" t))
        (let ((p (split-string l "\t")))
          (when (= (length p) 3)
            (let* ((ok (equal (gethash (nth 1 p) exp) (nth 2 p)))
                   (cell (or (assoc (car p) res) (car (push (list (car p) 0 nil) res)))))
              (cl-incf (nth 1 cell) (if ok 0 1))
              (unless ok (push (list (nth 1 p) (gethash (nth 1 p) exp) (nth 2 p)) (nth 2 cell))))))))
    res))
(provide 'nelisp-aot-fxloop-gen)
