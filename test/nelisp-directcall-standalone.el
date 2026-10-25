;;; nelisp-directcall-standalone.el --- Real primitive call semantics -*- lexical-binding: t; -*-
;; The same cases run on GNU, the pinned baseline and the candidate.
(defvar ndc-checked 0)
(defvar ndc-failed 0)
(defvar ndc-events 0)
(defvar ndc-autoload-count 0)
(defvar ndc-native (not (equal (getenv "DIRECTCALL_ENGINE") "gnu")))
(defun ndc-case (name thunk)
  (setq ndc-checked (1+ ndc-checked))
  (condition-case e
      (if (funcall thunk)
          (princ (format "DIRECTCALL CASE %s PASS\n" name))
        (setq ndc-failed (1+ ndc-failed))
        (princ (format "DIRECTCALL CASE %s FAIL\n" name)))
    (error (setq ndc-failed (1+ ndc-failed))
           (princ (format "DIRECTCALL CASE %s ERROR %S\n" name e)))))
(defun ndc-condition (thunk) (condition-case e (funcall thunk) (error e)))
(fset 'ndc-bc0 #[257 "\211 \207" [] 2])
(fset 'ndc-bc1 #[514 "\1\1!\207" [] 4])
(fset 'ndc-bc2 #[771 "\2\2\2\"\207" [] 6])
(fset 'ndc-bc3 #[1028 "\3\3\3\3#\207" [] 8])

;; Group 1: generic bytecode, source, symbol/object funcall, and apply.
(ndc-case "g1-logand" (lambda () (= (ndc-bc2 'logand 7 3) 3)))
(ndc-case "g1-car" (lambda () (= (ndc-bc1 'car '(7 8)) 7)))
(ndc-case "g1-length" (lambda () (= (ndc-bc1 'length [3 5 7 11]) 4)))
(ndc-case "g1-nth" (lambda () (= (ndc-bc2 'nth 1 '(7 8 9)) 8)))
(ndc-case "g1-aset" (lambda () (let ((v (vector 3 5 7 11)))
                                (and (= (ndc-bc3 'aset v 1 9) 9) (= (aref v 1) 9)))))
(ndc-case "g1-object" (lambda () (= (ndc-bc2 (symbol-function 'logand) 7 3) 3)))
(ndc-case "g1-funcall" (lambda () (= (funcall 'logand 7 3) 3)))
(ndc-case "g1-apply" (lambda () (= (apply 'logand 7 '(3 15)) 3)))
(ndc-case "g1-bc-funcall" (lambda () (= (ndc-bc3 'funcall 'logand 7 3) 3)))
(ndc-case "g1-bc-apply" (lambda () (= (ndc-bc3 'apply 'logand 7 '(3 15)) 3)))
(ndc-case "g1-source" (lambda () (equal (eval '(list (logand 7 3) (length [1 2 3]))) '(3 3))))
(ndc-case "g1-many" (lambda () (= (apply 'logand (make-list 260 7)) 7)))

;; Group 2: warm sites, replace cells, alias, restore, invalid function/autoload.
(let ((saved (symbol-function 'logand)))
  (unwind-protect
      (progn
        (ndc-bc2 'logand 7 3)
        (fset 'logand (lambda (_a _b) 41))
        (ndc-case "g2-defun" (lambda () (= (ndc-bc2 'logand 7 3) 41)))
        (fset 'ndc-alias-target (lambda (_a _b) 52))
        (defalias 'logand 'ndc-alias-target)
        (ndc-case "g2-alias" (lambda () (= (ndc-bc2 'logand 7 3) 52)))
        (fset 'logand (symbol-function 'logior))
        (ndc-case "g2-other-primitive" (lambda () (= (ndc-bc2 'logand 7 3) 7)))
        (fset 'logand 17)
        (ndc-case "g2-invalid" (lambda () (eq (car (ndc-condition (lambda () (ndc-bc2 'logand 7 3)))) 'invalid-function))))
    (fset 'logand saved)))
(ndc-case "g2-restored" (lambda () (= (ndc-bc2 'logand 7 3) 3)))
(ndc-case "g2-void-funcall" (lambda () (equal (ndc-condition (lambda () (funcall 'ndc-absent 7)))
                                             '(void-function ndc-absent))))
(ndc-case "g2-void-apply" (lambda () (equal (ndc-condition (lambda () (apply 'ndc-absent '(7))))
                                           '(void-function ndc-absent))))
(ndc-case "g2-autoload"
          (lambda ()
            (autoload 'ndc-autoload-target (getenv "DIRECTCALL_AUTOLOAD_FILE") nil nil)
            (and (= (ndc-bc2 'ndc-autoload-target 7 3) 55) (= ndc-autoload-count 1))))

;; Group 3: load the real GNU advice implementation, never a substitute.
(condition-case e
    (progn
      (let ((inhibit-message t) (message-log-max nil))
        (if ndc-native
            (progn
              (add-to-list 'load-path (expand-file-name "lisp" (getenv "DIRECTCALL_ROOT")))
              (require 'nelisp-nadvice-substrate))
          (require 'nadvice)))
      (unless (featurep 'nadvice) (error "Real nadvice feature is absent"))
      (fset 'ndc-before (lambda (&rest _) (setq ndc-events (1+ ndc-events))))
      (fset 'ndc-after (lambda (&rest _) (setq ndc-events (1+ ndc-events))))
      (fset 'ndc-around (lambda (old &rest args) (+ 100 (apply old args))))
      (fset 'ndc-override (lambda (&rest _) 19))
      (unwind-protect
          (progn
            (advice-add 'logand :before #'ndc-before)
            (advice-add 'logand :after #'ndc-after)
            (advice-add 'logand :around #'ndc-around)
            (ndc-case "g3-around-before-after" (lambda () (and (= (ndc-bc2 'logand 7 3) 103) (= ndc-events 2))))
            (advice-add 'logand :override #'ndc-override)
            (ndc-case "g3-override" (lambda () (= (ndc-bc2 'logand 7 3) 19)))
            (advice-remove 'logand #'ndc-override))
        (advice-remove 'logand #'ndc-before)
        (advice-remove 'logand #'ndc-after)
        (advice-remove 'logand #'ndc-around)
        (advice-remove 'logand #'ndc-override))
      (ndc-case "g3-restored" (lambda () (= (ndc-bc2 'logand 7 3) 3))))
  (error (setq ndc-failed (1+ ndc-failed))
         (princ (format "DIRECTCALL ADVICE UNAVAILABLE %S\n" e))))

;; Group 4: source selection precedes argument side effects; decline evaluates once.
(let ((saved (symbol-function 'logand)))
  (unwind-protect
      (progn
        (ndc-case "g4-captured-cell"
                  (lambda () (= (eval '(logand 7 (progn (fset 'logand (lambda (_a _b) 41)) 3))) 3)))
        (ndc-case "g4-next-cell" (lambda () (= (eval '(logand 7 3)) 41))))
    (fset 'logand saved)))
(ndc-case "g4-once" (lambda () (let ((calls 0))
                                (and (= (logand (progn (setq calls (1+ calls)) (ash 1 70)) 3) 0)
                                     (= calls 1)))))

;; Group 5: arity/error data and general values that the restricted argv path declines.
(ndc-case "g5-zero" (lambda () (= (ndc-bc0 'logand) -1)))
(ndc-case "g5-arity" (lambda () (equal (ndc-condition (lambda () (ndc-bc0 'car)))
                                      (list 'wrong-number-of-arguments (symbol-function 'car) 0))))
(ndc-case "g5-source-arity" (lambda () (let ((x 0))
                                        (and (equal (condition-case e (car (setq x 1) (setq x 2)) (error e))
                                                    '(wrong-number-of-arguments car 2)) (= x 0)))))
(ndc-case "g5-dotted" (lambda () (eq (car (ndc-condition (lambda () (apply 'logand '(7 . 3))))) 'wrong-type-argument)))
(ndc-case "g5-bignum" (lambda () (= (ndc-bc2 'logand (ash 1 80) 3) 0)))
(ndc-case "g5-type" (lambda () (eq (car (ndc-condition (lambda () (ndc-bc2 'logand 7 "x")))) 'wrong-type-argument)))
(ndc-case "g5-unicode" (lambda () (= (ndc-bc1 'length "あい😀x") 4)))
(ndc-case "g5-bool-vector" (lambda () (= (ndc-bc1 'length (make-bool-vector 11 nil)) 11)))
(ndc-case "g5-seven-vector" (lambda () (let ((v (vector 1 2 3 4 5 6 7)))
                                        (and (= (ndc-bc1 'length v) 7) (= (ndc-bc3 'aset v 2 99) 99)
                                             (= (aref v 2) 99)))))

;; Group 6: roots through evaluation, allocation, collection, nesting and unwind.
(ndc-case "g6-source-gc" (lambda () (= (eval '(length (progn (garbage-collect) (vector "a" "b" "c")))) 3)))
(ndc-case "g6-result-root" (lambda () (let* ((s (make-string 4096 ?x)) (r (ndc-bc1 'car (list s))))
                                      (garbage-collect) (and (eq s r) (= (length r) 4096)))))
(ndc-case "g6-aset-root" (lambda () (let* ((v (vector nil nil nil nil)) (x (make-string 4096 ?y)))
                                    (and (eq (ndc-bc3 'aset v 1 x) x)
                                         (progn (garbage-collect) (eq (aref v 1) x))))))
(fset 'ndc-allocating-target (lambda (x) (let ((v (list x))) (garbage-collect) (car v))))
(ndc-case "g6-allocating-fallback" (lambda () (equal (ndc-bc1 'ndc-allocating-target "alive") "alive")))
(ndc-case "g6-throw" (lambda () (and (= (catch 'ndc-exit (ndc-bc2 'throw 'ndc-exit 42)) 42)
                                    (= (ndc-bc2 'logand 7 3) 3))))
(ndc-case "g6-error-unwind" (lambda () (and (consp (ndc-condition (lambda () (ndc-bc1 'car 5))))
                                           (= (ndc-bc2 'logand 7 3) 3))))

;; Group 7: preserve current public visibility and identity.
(ndc-case "g7-visibility" (lambda () (let ((f (symbol-function 'logand)))
                                      (and (subrp f) (eq (type-of f) 'subr)
                                           (eq f (symbol-function 'logand))
                                           (eq (consp f) (if ndc-native t nil))))))
(ndc-case "g7-retained-object" (lambda () (let ((f (symbol-function 'logand)))
                                           (= (ndc-bc2 f 7 3) 3))))
;; A hit must never authorize the following call after a cell change.
(let ((saved (symbol-function 'logand)))
  (unwind-protect
      (progn
        (ndc-case "dcid-redefinition-between-calls"
                  (lambda () (and (= (ndc-bc2 'logand 7 3) 3)
                                  (progn (fset 'logand (lambda (_a _b) 61))
                                         (= (ndc-bc2 'logand 7 3) 61)))))
        (fset 'logand saved)
        (ndc-case "dcid-advice-after-hit"
                  (lambda () (and (= (ndc-bc2 'logand 7 3) 3)
                                  (progn (advice-add 'logand :override #'ndc-override)
                                         (= (ndc-bc2 'logand 7 3) 19)))))
        (advice-remove 'logand #'ndc-override)
        (fset 'logand saved)
        (ndc-case "dcid-fset-lambda-after-hit"
                  (lambda () (and (= (ndc-bc2 'logand 7 3) 3)
                                  (progn (fset 'logand '(lambda (a b) 73))
                                         (= (ndc-bc2 'logand 7 3) 73))))))
    (advice-remove 'logand #'ndc-override) (fset 'logand saved)))
(when ndc-native
  (ndc-case "dcid-builtin-mutation-after-hit"
            (lambda () (let ((object (list 'builtin 'car)))
                         (and (= (ndc-bc1 object '(7 8 9)) 7)
                              (progn (setcar (cdr object) 'length)
                                     (= (ndc-bc1 object '(7 8 9)) 3)))))))
(princ (format "DIRECTCALL SUMMARY checked=%d failed=%d\n" ndc-checked ndc-failed))
(kill-emacs (if (= ndc-failed 0) 0 1))
