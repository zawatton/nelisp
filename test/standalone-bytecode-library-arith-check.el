;;; standalone-bytecode-library-arith-check.el --- bytecode arithmetic on a library's objects -*- lexical-binding: t; -*-

;; The standalone runtime hosts libraries that own object kinds the VM knows
;; nothing about, and make the arithmetic functions accept them by redefining
;; the function cells.  The interpreter calls whatever the cell holds.  The VM's
;; arithmetic and comparison opcodes compute on numbers; for an operand that is
;; not a number they call the opcode's function through its cell, as the
;; interpreter does, so a library's redefinition is honoured and the runtime's
;; own cell answers as GNU does.  (Buffer switching is bridged the same way:
;; test/standalone-buffer-function-cell-check.el.)
;;
;; The library here: [bla N] stands for the number N, and each arithmetic
;; function is redefined to coerce such objects and call the runtime's own
;; function.  The GNU-compiled functions of test/fixtures/bytecode-marker-arith.eld
;; are called with those objects mixed with fixnums, floats and markers; each
;; outcome must equal the interpreter's call of the same function, and the
;; library's cell must be called once, by the VM as by the interpreter.  This
;; output is the runtime's own (GNU cannot run it: GNU's opcodes never consult a
;; cell); the GNU-golden check is standalone-bytecode-marker-arith-check.el.

(defun bla-make (n) (vector 'bla n))
(defun bla-p (x) (and (vectorp x) (= (length x) 2) (eq (aref x 0) 'bla)))
(defun bla-value (x) (if (bla-p x) (aref x 1) x))
(defun bla-show (x)
  (cond ((bla-p x) (format "bla%d" (aref x 1)))
        ((markerp x) (format "m@%d" (marker-position x)))
        (t (format "%S" x))))

(defvar bla-recording nil)
(defvar bla-calls nil)

(defun bla-outcome (thunk)
  ;; The outcome is rendered after the library's cells stop recording: the calls the
  ;; runtime's own printing makes (a Lisp format layer, say) are not the VM's.
  (let ((result (condition-case err (list "ok" (funcall thunk)) (error (list "err" err)))))
    (setq bla-recording nil)
    (format "%s %S" (car result) (cadr result))))

(defun bla-run (name thunk)
  "Call THUNK with NAME redefined by the library; return (OUTCOME . CALLS)."
  (let ((original (symbol-function name)) (outcome nil))
    (setq bla-calls nil)
    (fset name (lambda (&rest args)
                 (when bla-recording (push name bla-calls))
                 (apply original (mapcar #'bla-value args))))
    (unwind-protect
        (let ((bla-recording t))
          (setq outcome (bla-outcome thunk)))
      (fset name original))
    (cons outcome (reverse bla-calls))))

(let* ((root (file-name-directory load-file-name))
       (compiled (or (getenv "BMA_COMPILED")
                     (expand-file-name "fixtures/bytecode-marker-arith.eld" root)))
       (binary '((bma-eql . =) (bma-gtr . >) (bma-lss . <) (bma-leq . <=) (bma-geq . >=)
                 (bma-max . max) (bma-min . min) (bma-add . +) (bma-sub . -)
                 (bma-mul . *) (bma-quo . /) (bma-rem . %)))
       (unary '((bma-inc . 1+) (bma-dec . 1-) (bma-neg . -)))
       (cases 0) (failures 0))
  (load compiled nil t t)
  (with-temp-buffer
    (insert "abcdefgh")
    (let* ((m5 (copy-marker 5))
           (operands (list 3 2.5 (bla-make 2) (bla-make 7) m5))
           (singles (list 3 2.5 (bla-make 2) m5 'a)))
      (dolist (entry binary)
        (dolist (a (append operands (list 'a)))
          (dolist (b (append operands (list 'a)))
            ;; A wrong-type value without a library object is the GNU-golden check's.
            (unless (and (or (eq a 'a) (eq b 'a)) (not (bla-p a)) (not (bla-p b)))
              (let* ((bytecode (bla-run (cdr entry) (lambda () (funcall (car entry) a b))))
                     (interpreted (bla-run (cdr entry) (lambda () (apply (cdr entry) (list a b)))))
                     (uses-library (or (bla-p a) (bla-p b))))
                (setq cases (1+ cases))
                (princ (format "%-8s %-8s %-8s %s calls=%S\n" (car entry) (bla-show a)
                               (bla-show b) (car bytecode) (cdr bytecode)))
                ;; The interpreter calls the cell for every case; the VM only for an
                ;; operand that is not a number, so compare calls just for those.
                (unless (and (equal (car bytecode) (car interpreted))
                             (or (not uses-library)
                                 (equal (cdr bytecode) (cdr interpreted))))
                  (setq failures (1+ failures))
                  (princ (format "FAIL %s interpreted %s calls=%S\n" (car entry)
                                 (car interpreted) (cdr interpreted)))))))))
      (dolist (entry unary)
        (dolist (x singles)
          (let* ((bytecode (bla-run (cdr entry) (lambda () (funcall (car entry) x))))
                 (interpreted (bla-run (cdr entry) (lambda () (apply (cdr entry) (list x)))))
                 (uses-library (bla-p x)))
            (setq cases (1+ cases))
            (princ (format "%-8s %-8s %-8s %s calls=%S\n" (car entry) (bla-show x) ""
                           (car bytecode) (cdr bytecode)))
            (unless (and (equal (car bytecode) (car interpreted))
                         (or (not uses-library) (equal (cdr bytecode) (cdr interpreted))))
              (setq failures (1+ failures))
              (princ (format "FAIL %s interpreted %s calls=%S\n" (car entry)
                             (car interpreted) (cdr interpreted)))))))))
  (princ (format "LIBRARY-ARITH %d cases, %d failures\n" cases failures))
  (kill-emacs (if (= failures 0) 0 1)))
