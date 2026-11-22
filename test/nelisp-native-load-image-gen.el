;;; nelisp-native-load-image-gen.el --- GNU differential for the native-load boundary  -*- lexical-binding: t; -*-
;;
;; Host side of an in-image differential for what crosses `nelisp-native-load-call':
;;  - fixnum arguments and results at the immediate-word limits (a 4N+1 word
;;    is handed to `ptr-call', whose arguments are 62-bit Lisp fixnums),
;;  - fixnum overflow to a bignum in the generic arithmetic arm,
;;  - a Lisp error raised under a native call: its condition and data must
;;    equal GNU's (the pending exit is read from the runtime's stash).
;; Usage (host Emacs, batch; the probe then runs in the runtime image):
;;   emacs --batch -L lisp -L src -l test/nelisp-native-load-image-gen.el \
;;         -f nelisp-native-load-image-gen-run OUTDIR
;; writes OUTDIR/{m.el,m.neln,probe.el,expected.txt}; `probe.el' prints one line
;; per case, and `nelisp-native-load-image-gen-compare' diffs it against GNU.

(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nelisp-native-load-image-gen--source
  "(defun nl-add (a b) (+ a b))
(defun nl-sub (a b) (- a b))
(defun nl-mul (a b) (* a b))
(defun nl-inc (a) (1+ a))
(defun nl-dec (a) (1- a))
(defun nl-id (a) a)
(defun nl-lt (a b) (< a b))
(defun nl-neg (a) (- a))
"
  "The defuns compiled to a native artifact; each runs unchanged under GNU.")

(defun nelisp-native-load-image-gen--cases ()
  (let* ((p59 (ash 1 59)) (p60 (ash 1 60)) (p61 (ash 1 61))
         (mpf (1- p61)) (mnf (- p61)))
    (append
     ;; Words at the 59-bit immediate limit, either side, both signs.
     (mapcar (lambda (v) (list 'nl-id (list v)))
             (list (1- p59) p59 (1+ p59) (- p59) (1- (- p59)) p60 (- p60) mpf mnf
                   (ash 1 62) (- (ash 1 62)) (ash 1 70) 0 1 -1))
     ;; A result computed natively and returned as an immediate word.
     (mapcar (lambda (v) (list 'nl-inc (list v)))
             (list (- p59 2) (1- p59) p59 (1- p60) (- mpf 1) mpf (1- mnf) mnf (- p59) (- (1- p59))))
     (mapcar (lambda (v) (list 'nl-dec (list v)))
             (list (- p59 1) p59 mnf (1+ mnf) mpf (- p59) (- (1+ p59))))
     ;; Overflow to a bignum in the generic arm.
     (list (list 'nl-add (list p60 p60)) (list 'nl-add (list mpf 1)) (list 'nl-add (list mpf mpf))
           (list 'nl-add (list mnf -1)) (list 'nl-add (list mnf mnf))
           (list 'nl-sub (list mnf 1)) (list 'nl-sub (list mpf -1)) (list 'nl-sub (list 0 mnf))
           (list 'nl-mul (list p60 8)) (list 'nl-mul (list mpf mpf)) (list 'nl-mul (list mnf -1))
           (list 'nl-mul (list p59 4)) (list 'nl-mul (list p59 3))
           (list 'nl-neg (list mnf)) (list 'nl-neg (list mpf))
           (list 'nl-add (list p59 p59)) (list 'nl-add (list (1- p59) 1)) (list 'nl-add (list 1 2))
           (list 'nl-lt (list p60 p59)) (list 'nl-lt (list p59 p60)) (list 'nl-lt (list (ash 1 70) p60)))
     ;; Wrong types: condition and data must equal GNU's.
     (list '(nl-add (a 1)) '(nl-add (1 b)) '(nl-add (nil 1)) '(nl-add (1 nil)) '(nl-add (1.5 2))
           '(nl-add (1 "s")) '(nl-add ("s" 1)) '(nl-inc (nil)) '(nl-inc (a)) '(nl-inc (1.5))
           '(nl-dec (a)) '(nl-lt (a 1)) '(nl-lt (1 nil)) '(nl-neg (a)) '(nl-mul (a 2))
           '(nl-add ((1 2) 1)) '(nl-add ([1] 1)) '(nl-id (a))))))

(defun nelisp-native-load-image-gen--gnu (fn args)
  (condition-case e (format "%S" (apply fn args)) (error (format "ERR %S" e))))

(defun nelisp-native-load-image-gen-run ()
  (let* ((dir (expand-file-name (car command-line-args-left)))
         (src (expand-file-name "m.el" dir))
         (art (expand-file-name "m.neln" dir))
         (cases (nelisp-native-load-image-gen--cases)))
    (setq command-line-args-left nil)
    (make-directory dir t)
    (with-temp-file src (insert nelisp-native-load-image-gen--source))
    (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
    (with-temp-buffer (insert nelisp-native-load-image-gen--source)
                      (goto-char (point-min))
                      (condition-case nil (while t (eval (read (current-buffer)) t)) (end-of-file nil)))
    (with-temp-file (expand-file-name "expected.txt" dir)
      (dolist (c cases)
        (insert (format "%S\t%s\n" c (nelisp-native-load-image-gen--gnu (car c) (cadr c))))))
    (with-temp-file (expand-file-name "probe.el" dir)
      (insert (format "(setq load-path (append (list %S %S) load-path))\n(require 'nelisp-native-load)\n"
                      (expand-file-name "lisp" default-directory) (expand-file-name "src" default-directory)))
      (insert (format "(defconst nlp-art %S)\n(defconst nlp-cases '%S)\n(defvar nlp-handles nil)\n" art cases))
      (insert "(defun nlp-h (fn) (or (cdr (assoc fn nlp-handles)) (let ((h (nelisp-native-load-artifact nlp-art fn))) (push (cons fn h) nlp-handles) h)))
(dolist (c nlp-cases)
  (princ (format \"%S\\t%s\\n\" c
    (condition-case e (format \"%S\" (nelisp-native-load-call (nlp-h (symbol-name (car c))) (cadr c)))
      (error (format \"ERR %S\" e))))))\n"))
    (message "wrote %s" dir)))

(defun nelisp-native-load-image-gen-compare (dir)
  "Return the (CASE EXPECTED ACTUAL) triples of DIR whose probe.out differs from GNU."
  (let ((exp (make-hash-table :test #'equal)) (seen (make-hash-table :test #'equal)) (bad nil))
    (with-temp-buffer (insert-file-contents (expand-file-name "expected.txt" dir))
      (dolist (l (split-string (buffer-string) "\n" t))
        (let ((p (split-string l "\t"))) (puthash (car p) (cadr p) exp))))
    (with-temp-buffer (insert-file-contents (expand-file-name "probe.out" dir))
      (dolist (l (split-string (buffer-string) "\n" t))
        (let ((p (split-string l "\t")))
          (when (= (length p) 2)
            (puthash (car p) t seen)
            (unless (equal (gethash (car p) exp) (cadr p))
              (push (list (car p) (gethash (car p) exp) (cadr p)) bad))))))
    ;; A case that never printed (the probe died or stalled) is a mismatch.
    (maphash (lambda (k v) (unless (gethash k seen) (push (list k v "MISSING") bad))) exp)
    (nreverse bad)))

(provide 'nelisp-native-load-image-gen)
;;; nelisp-native-load-image-gen.el ends here
