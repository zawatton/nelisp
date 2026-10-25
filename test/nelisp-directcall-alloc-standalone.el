;;; nelisp-directcall-alloc-standalone.el --- vector/list/concat/substring/make-* through the call routes -*- lexical-binding: t; -*-
;; dcproto: the same rows must print on GNU Emacs and on the NeLisp binaries.
;; The bytecode helpers receive the primitive as a symbol operand, so a Bcall
;; (the VM's own call) reaches it; funcall, apply and the evaluator reach it
;; through their own routes.  Every error row compares GNU's error data.
(defvar nda-checked 0)
(defvar nda-failed 0)
(defun nda-case (name thunk)
  (setq nda-checked (1+ nda-checked))
  (condition-case e
      (if (funcall thunk)
          (princ (format "NDA CASE %s PASS\n" name))
        (setq nda-failed (1+ nda-failed))
        (princ (format "NDA CASE %s FAIL\n" name)))
    (error (setq nda-failed (1+ nda-failed))
           (princ (format "NDA CASE %s ERROR %S\n" name e)))))
(defun nda-condition (thunk) (condition-case e (funcall thunk) (error e)))
;; (F A...) as one Bcall: the function is the first operand.
(fset 'nda-bc0 #[257 "\211 \207" [] 2])
(fset 'nda-bc1 #[514 "\1\1!\207" [] 4])
(fset 'nda-bc2 #[771 "\2\2\2\"\207" [] 6])
(fset 'nda-bc3 #[1028 "\3\3\3\3#\207" [] 8])
(fset 'nda-bc4 #[1285 "\4\4\4\4\4$\207" [] 10])
;; The function as a constant of the code (no opcode exists for these two).
(fset 'nda-kvec3 #[771 "\300\3\3\3#\207" [vector] 8])
(fset 'nda-kmkvec #[514 "\300\2\2\"\207" [make-vector] 5])
(fset 'nda-kmkstr #[514 "\300\2\2\"\207" [make-string] 5])
(defvar nda-multi (string 233 97))

;; vector
(nda-case "vector-bc3" (lambda () (equal (nda-bc3 'vector 1 'b "c") [1 b "c"])))
(nda-case "vector-bc0" (lambda () (equal (nda-bc0 'vector) [])))
(nda-case "vector-bc1-float" (lambda () (equal (nda-bc1 'vector 1.5) [1.5])))
(nda-case "vector-constant" (lambda () (equal (nda-kvec3 4 5 6) [4 5 6])))
(nda-case "vector-funcall" (lambda () (equal (funcall 'vector 1 2) [1 2])))
(nda-case "vector-apply" (lambda () (equal (apply 'vector 1 '(2 3)) [1 2 3])))
(nda-case "vector-eval" (lambda () (equal (eval '(vector 1 (vector 2)) t) [1 [2]])))
(nda-case "vector-many" (lambda () (= (length (apply 'vector (make-list 300 7))) 300)))
(nda-case "vector-bc4" (lambda () (equal (nda-bc4 'vector 1 2 3 4) [1 2 3 4])))
(nda-case "vector-fresh" (lambda () (let ((a (nda-bc2 'vector 1 2))) (aset a 0 9)
                                      (and (equal (nda-bc2 'vector 1 2) [1 2]) (equal a [9 2])))))
(nda-case "vector-shares-elements"
          (lambda () (let* ((x (list 1)) (v (nda-bc2 'vector x x))) (and (eq (aref v 0) x) (eq (aref v 1) x)))))
(nda-case "vector-seven-elements"
          (lambda () (equal (apply 'vector '(1 2 3 4 5 6 7)) [1 2 3 4 5 6 7])))

;; make-vector
(nda-case "make-vector-bc2" (lambda () (equal (nda-bc2 'make-vector 3 'a) [a a a])))
(nda-case "make-vector-zero" (lambda () (equal (nda-bc2 'make-vector 0 0) [])))
(nda-case "make-vector-constant" (lambda () (equal (nda-kmkvec 2 'z) [z z])))
(nda-case "make-vector-shared-init"
          (lambda () (let* ((x (list 1)) (v (nda-bc2 'make-vector 3 x))) (and (eq (aref v 0) x) (eq (aref v 2) x)))))
(nda-case "make-vector-float-init" (lambda () (equal (nda-bc2 'make-vector 2 1.5) [1.5 1.5])))
(nda-case "make-vector-negative"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'make-vector -1 0))) '(wrong-type-argument wholenump -1))))
(nda-case "make-vector-not-number"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'make-vector 'a 0))) '(wrong-type-argument wholenump a))))
(nda-case "make-vector-funcall-arity"
          (lambda () (eq (car (nda-condition (lambda () (funcall 'make-vector 1)))) 'wrong-number-of-arguments)))
(nda-case "make-vector-big" (lambda () (= (length (nda-bc2 'make-vector 70000 nil)) 70000)))

;; list
(nda-case "list-bc3" (lambda () (equal (nda-bc3 'list 1 2 3) '(1 2 3))))
(nda-case "list-bc0" (lambda () (null (nda-bc0 'list))))
(nda-case "list-float" (lambda () (equal (nda-bc2 'list 1.5 "s") '(1.5 "s"))))
(nda-case "list-fresh" (lambda () (not (eq (nda-bc1 'list 1) (nda-bc1 'list 1)))))
(nda-case "list-apply-many" (lambda () (= (length (apply 'list (make-list 400 1))) 400)))
(nda-case "list-funcall" (lambda () (equal (funcall 'list 1 2 3) '(1 2 3))))
(nda-case "list-eval" (lambda () (equal (eval '(list 1 (list 2)) t) '(1 (2)))))
(nda-case "list-shares-elements"
          (lambda () (let* ((x (list 1)) (l (nda-bc2 'list x x))) (and (eq (car l) x) (eq (cadr l) x)))))

;; concat
(nda-case "concat-bc2" (lambda () (equal (nda-bc2 'concat "ab" "cd") "abcd")))
(nda-case "concat-empty" (lambda () (equal (nda-bc0 'concat) "")))
(nda-case "concat-three" (lambda () (equal (nda-bc3 'concat "a" "" "bc") "abc")))
(nda-case "concat-unibyte-result" (lambda () (not (multibyte-string-p (nda-bc2 'concat "ab" "cd")))))
(nda-case "concat-multibyte" (lambda () (equal (nda-bc2 'concat nda-multi "x") (concat (string 233 97) "x"))))
(nda-case "concat-multibyte-flag" (lambda () (multibyte-string-p (nda-bc2 'concat nda-multi "x"))))
(nda-case "concat-unibyte-high-byte"
          (lambda () (let ((r (nda-bc2 'concat "a" "\200"))) (and (= (length r) 2) (= (aref r 1) 128)))))
(nda-case "concat-multibyte-and-high-byte"
          (lambda () (let ((r (nda-bc2 'concat nda-multi "\200"))) (and (= (length r) 3) (multibyte-string-p r)))))
(nda-case "concat-properties"
          (lambda () (eq (get-text-property 0 'face (nda-bc2 'concat (propertize "a" 'face 'bold) "b")) 'bold)))
(nda-case "concat-list-argument" (lambda () (equal (nda-bc2 'concat "a" '(98 99)) "abc")))
(nda-case "concat-vector-argument" (lambda () (equal (nda-bc2 'concat "a" [98]) "ab")))
(nda-case "concat-nil-argument" (lambda () (equal (nda-bc2 'concat "a" nil) "a")))
(nda-case "concat-bad-argument"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'concat "a" 5))) '(wrong-type-argument sequencep 5))))
(nda-case "concat-fresh" (lambda () (let ((a (nda-bc2 'concat "ab" "cd"))) (aset a 0 ?z)
                                      (equal (nda-bc2 'concat "ab" "cd") "abcd"))))
(nda-case "concat-funcall-apply"
          (lambda () (and (equal (funcall 'concat "a" "b") "ab") (equal (apply 'concat "a" '("b" "c")) "abc"))))

;; substring
(nda-case "substring-bc3" (lambda () (equal (nda-bc3 'substring "abcdef" 2 4) "cd")))
(nda-case "substring-to-end" (lambda () (equal (nda-bc2 'substring "abcdef" 2) "cdef")))
(nda-case "substring-nil-end" (lambda () (equal (nda-bc3 'substring "abcdef" 2 nil) "cdef")))
(nda-case "substring-empty" (lambda () (equal (nda-bc3 'substring "abcdef" 3 3) "")))
(nda-case "substring-negative-from" (lambda () (equal (nda-bc2 'substring "abcdef" -2) "ef")))
(nda-case "substring-negative-to" (lambda () (equal (nda-bc3 'substring "abcdef" 1 -1) "bcde")))
(nda-case "substring-one-arg" (lambda () (equal (nda-bc1 'substring "abc") "abc")))
(nda-case "substring-vector" (lambda () (equal (nda-bc3 'substring [1 2 3 4] 1 3) [2 3])))
(nda-case "substring-multibyte" (lambda () (equal (nda-bc2 'substring (concat nda-multi "bc") 1) "abc")))
(nda-case "substring-ascii-multibyte"
          (lambda () (let ((r (nda-bc3 'substring (string-to-multibyte "abcd") 1 3))) (and (equal r "bc") (multibyte-string-p r)))))
(nda-case "substring-properties"
          (lambda () (eq (get-text-property 0 'face (nda-bc3 'substring (propertize "abc" 'face 'bold) 1 2)) 'bold)))
(nda-case "substring-range-error"
          (lambda () (equal (nda-condition (lambda () (nda-bc3 'substring "abc" 0 9))) '(args-out-of-range "abc" 0 9))))
(nda-case "substring-reversed-error"
          (lambda () (equal (nda-condition (lambda () (nda-bc3 'substring "abc" 2 1))) '(args-out-of-range "abc" 2 1))))
(nda-case "substring-bad-type"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'substring 5 1))) '(wrong-type-argument arrayp 5))))
(nda-case "substring-fresh" (lambda () (let* ((s "abcdef") (r (nda-bc3 'substring s 1 3))) (aset r 0 ?z)
                                         (and (equal s "abcdef") (equal (nda-bc3 'substring s 1 3) "bc")))))

;; make-string
(nda-case "make-string-bc2" (lambda () (equal (nda-bc2 'make-string 3 ?a) "aaa")))
(nda-case "make-string-zero" (lambda () (equal (nda-bc2 'make-string 0 ?a) "")))
(nda-case "make-string-constant" (lambda () (equal (nda-kmkstr 2 ?q) "qq")))
(nda-case "make-string-unibyte" (lambda () (not (multibyte-string-p (nda-bc2 'make-string 2 ?a)))))
(nda-case "make-string-wide" (lambda () (let ((r (nda-bc2 'make-string 2 233))) (and (equal r (string 233 233)) (multibyte-string-p r)))))
(nda-case "make-string-multibyte-flag"
          (lambda () (let ((r (nda-bc3 'make-string 2 ?a t))) (and (equal r "aa") (multibyte-string-p r)))))
(nda-case "make-string-negative-count"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'make-string -1 ?a))) '(wrong-type-argument wholenump -1))))
(nda-case "make-string-bad-char"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'make-string 2 -1))) '(wrong-type-argument characterp -1))))
(nda-case "make-string-not-char"
          (lambda () (equal (nda-condition (lambda () (nda-bc2 'make-string 2 'a))) '(wrong-type-argument characterp a))))
(nda-case "make-string-fresh" (lambda () (let ((a (nda-bc2 'make-string 3 ?a))) (aset a 0 ?z)
                                           (equal (nda-bc2 'make-string 3 ?a) "aaa"))))
(nda-case "make-string-big" (lambda () (= (length (nda-bc2 'make-string 100000 ?a)) 100000)))

;; Aliases, redefinition and restoring: the cell, not the name, decides.
(nda-case "alias-of-the-cell"
          (lambda () (progn (fset 'nda-my-vector (symbol-function 'vector))
                            (and (equal (nda-bc2 'nda-my-vector 1 2) [1 2])
                                 (equal (nda-condition (lambda () (nda-bc2 'nda-my-make-vector -1 0)))
                                        '(void-function nda-my-make-vector))))))
(nda-case "alias-error-data"
          (lambda () (progn (fset 'nda-my-make-vector (symbol-function 'make-vector))
                            (equal (nda-condition (lambda () (nda-bc2 'nda-my-make-vector -1 0)))
                                   '(wrong-type-argument wholenump -1)))))
(nda-case "uninterned-same-name"
          (lambda () (let ((s (make-symbol "vector")))
                       (fset s (symbol-function 'vector))
                       (equal (nda-bc2 s 1 2) [1 2]))))
(nda-case "redefined-then-restored"
          (lambda () (let ((saved (symbol-function 'vector)))
                       (unwind-protect
                           (progn (nda-bc2 'vector 1 2)
                                  (fset 'vector (lambda (&rest a) (cons 'mine a)))
                                  (and (equal (nda-bc2 'vector 1 2) '(mine 1 2))
                                       (progn (fset 'vector saved) (equal (nda-bc2 'vector 1 2) [1 2]))))
                         (fset 'vector saved)))))
(nda-case "swapped-for-another-primitive"
          (lambda () (let ((saved (symbol-function 'list)))
                       (unwind-protect
                           (progn (nda-bc2 'list 1 2)
                                  (fset 'list (symbol-function 'vector))
                                  (equal (nda-bc2 'list 1 2) [1 2]))
                         (fset 'list saved)))))

;; Collection while results are alive and arguments are only on the VM stack.
(nda-case "gc-lists-of-strings"
          (lambda () (let ((acc nil) (ok t))
                       (dotimes (i 3000)
                         (push (nda-bc3 'list (nda-bc2 'concat "k" (number-to-string i))
                                        (nda-bc3 'substring "0123456789" 2 6) (nda-bc2 'make-string 3 ?x))
                               acc)
                         (when (= (% i 250) 0) (garbage-collect)))
                       (garbage-collect)
                       (let ((i 2999))
                         (dolist (e acc)
                           (unless (equal e (list (concat "k" (number-to-string i)) "2345" "xxx")) (setq ok nil))
                           (setq i (1- i))))
                       ok)))
(nda-case "gc-vectors"
          (lambda () (let ((acc nil) (ok t))
                       (dotimes (i 3000)
                         (push (nda-bc3 'vector i (nda-bc2 'make-vector 4 i) (nda-bc2 'list i i)) acc)
                         (when (= (% i 300) 0) (garbage-collect)))
                       (garbage-collect)
                       (let ((i 2999))
                         (dolist (e acc)
                           (unless (equal e (vector i (make-vector 4 i) (list i i))) (setq ok nil))
                           (setq i (1- i))))
                       ok)))
(nda-case "gc-in-lisp-loop-result-rooted"
          (lambda () (let ((cell (nda-bc3 'list (nda-bc2 'make-string 5 ?a) 1 2)))
                       (dotimes (_ 5) (garbage-collect) (make-list 2000 (list 1 2)))
                       (equal cell '("aaaaa" 1 2)))))

(princ (format "NDA SUMMARY checked=%d failed=%d\n" nda-checked nda-failed))
(kill-emacs (if (= nda-failed 0) 0 1))
