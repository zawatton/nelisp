;;; standalone-bytecode-marker-arith-check.el --- GNU-compiled arithmetic on markers -*- lexical-binding: t; -*-

;; GNU bytecode.c: Beqlsign, Bgtr, Blss, Bleq and Bgeq take a fixnum fast path
;; and call arithcompare, Bmax and Bmin call Fmax and Fmin, and Bplus, Bdiff,
;; Bmult, Bquo, Brem, Bnegate, Badd1 and Bsub1 call their arith functions.  All
;; of them coerce a marker through CHECK_NUMBER_COERCE_MARKER and signal
;; (wrong-type-argument number-or-marker-p X) for anything else.
;;
;; test/fixtures/bytecode-marker-arith.eld holds one function per opcode, compiled
;; by GNU 31.1.  This check calls each with fixnums, floats, markers (of the
;; runtime's own kind, which the library image uses) and values that are neither
;; numbers nor markers, and prints the outcome.  GNU Emacs 31.1 prints the same
;; text (the .expected file is its output), and on the runtime each outcome must
;; also equal the interpreter's call of the same function.  Before the VM
;; coerced markers, `<', `=', `max' and the rest signalled
;; (wrong-type-argument number-or-marker-p #<marker ...>) for a marker.

(defun bma-show (x)
  (cond ((markerp x) (if (marker-position x) (format "m@%d" (marker-position x)) "m@none"))
        (t (format "%S" x))))

(defun bma-outcome (thunk)
  (condition-case err
      (format "ok %S" (funcall thunk))
    (error (format "err %S" err))))

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
    (let* ((m2 (copy-marker 2)) (m5 (copy-marker 5)) (detached (make-marker))
           (numbers (list 3 -2 2.5 m2 m5))
           (bad (list 'a "s" nil))
           (pairs nil) (singles nil))
      ;; Every ordered pair of numbers and markers.
      (dolist (a numbers) (dolist (b numbers) (push (cons a b) pairs)))
      ;; A value that is neither, before and after a fixnum, and after a marker.
      (dolist (x bad)
        (push (cons x 3) pairs) (push (cons 3 x) pairs) (push (cons m2 x) pairs))
      ;; A marker that points nowhere.
      (push (cons detached 3) pairs) (push (cons 3 detached) pairs) (push (cons detached m2) pairs)
      (setq pairs (nreverse pairs))
      (setq singles (append numbers bad (list detached)))
      (dolist (entry binary)
        (dolist (pair pairs)
          (let* ((bytecode (bma-outcome (lambda () (funcall (car entry) (car pair) (cdr pair)))))
                 (interpreted (bma-outcome (lambda () (apply (cdr entry) (list (car pair) (cdr pair)))))))
            (setq cases (1+ cases))
            (princ (format "%-8s %-8s %-8s %s\n" (car entry) (bma-show (car pair))
                           (bma-show (cdr pair)) bytecode))
            (unless (equal bytecode interpreted)
              (setq failures (1+ failures))
              (princ (format "FAIL %s interpreted %s\n" (car entry) interpreted))))))
      (dolist (entry unary)
        (dolist (x singles)
          (let* ((bytecode (bma-outcome (lambda () (funcall (car entry) x))))
                 (interpreted (bma-outcome (lambda () (apply (cdr entry) (list x))))))
            (setq cases (1+ cases))
            (princ (format "%-8s %-8s %-8s %s\n" (car entry) (bma-show x) "" bytecode))
            (unless (equal bytecode interpreted)
              (setq failures (1+ failures))
              (princ (format "FAIL %s interpreted %s\n" (car entry) interpreted))))))))
  (princ (format "MARKER-ARITH %d cases, %d failures\n" cases failures))
  (kill-emacs (if (= failures 0) 0 1)))
