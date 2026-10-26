;;; nelisp-nc3-corpus.el --- transcript over the nc3 lowering  -*- lexical-binding: t; -*-
;; nativecomp N2b (nc3).  The functions of test/nelisp-nc3-fx.el (harvested
;; corpus rows 2000 and up) on the boundary inputs of the inlined GNU opcode
;; primitives (immediates, strings, objects, legacy words, bignums, floats,
;; markers, improper lists, zero divisors, overflow, records, empty strings),
;; the builtins called without the call-out (fboundp, string-search, funcall,
;; apply, symbol-value / set, `*' and `<=' with three arguments), and
;; redefinition of a builtin under a native caller (advice and fset of
;; `fboundp' / `nelisp--string-search-chars', restored).  tools/ai/nc3-check.sh
;; runs it with the VM off, with the records back on the word VM (NC_MODE=vm),
;; as built and with the mutant (NC_MODE=mutant); NC_GC=1 collects between
;; cases.  NC_INLINE=1 runs only the inline-opcode checks (GNU compiled
;; semantics: a redefined `stringp' is not seen).  Runs on GNU Emacs too.
(load (expand-file-name "test/nelisp-nc3-fx.el") nil t)
(defvar nc3-cases nil)
(defmacro nc3 (label &rest body)
  `(push (cons ,label (lambda () ,@body)) nc3-cases))
(defun nc3-gc () (when (getenv "NC_GC") (garbage-collect)))
(defmacro nc3-err (form) `(condition-case e ,form (error (list 'err (car e)))))
(defun nc3-each (f inputs) (mapcar (lambda (x) (nc3-err (funcall f x))) inputs))

(defvar nc3-objects
  (let ((cyc (list 'p 'q)))
    (setcdr (cdr cyc) cyc)
    (list nil t 0 1 -1 most-positive-fixnum most-negative-fixnum (1+ most-positive-fixnum)
          1.5 -0.0 ?a "" "abc" "été" (string-to-unibyte "ab\377") (propertize "pq" 'face 'bold)
          'sym :kw (make-symbol "unint") '(1 2) '(a . b) (vector) (vector 1 2 3) (make-vector 7 0)
          (record 'nc3r 1 2) (make-bool-vector 3 t) (make-char-table 'nc3) (make-hash-table)
          #'car (lambda (x) x) cyc)))

;; --- predicates on every class
(nc3 "pred" (mapcar (lambda (x) (nc3-pred x)) nc3-objects))
(nc3 "pred-marker" (with-temp-buffer (insert "abc") (nc3-pred (copy-marker 2))))
;; --- comparisons and arithmetic
(nc3 "cmp" (list (nc3-cmp 1 2) (nc3-cmp 2 1) (nc3-cmp 2 2) (nc3-cmp most-negative-fixnum most-positive-fixnum)
                 (nc3-cmp 1 1.5) (nc3-cmp 2.0 2) (nc3-cmp (1+ most-positive-fixnum) 1) (nc3-cmp -0.0 0)
                 (nc3-err (nc3-cmp 'a 1)) (nc3-err (nc3-cmp 1 "x")) (nc3-err (nc3-cmp nil nil))
                 (with-temp-buffer (insert "abcdef") (list (nc3-cmp (copy-marker 3) 4) (nc3-cmp 5 (copy-marker 2))))))
(nc3 "cmp3" (list (nc3-cmp3 1 2 3) (nc3-cmp3 3 2 1) (nc3-cmp3 1 3 2) (nc3-cmp3 1 1 1) (nc3-cmp3 1 2.5 3)
                  (nc3-err (nc3-cmp3 1 'x 3))))
(nc3 "mul" (list (nc3-mul 6 7) (nc3-mul -6 7) (nc3-mul 0 most-positive-fixnum) (nc3-mul 65536 65536)
                 (nc3-mul 2147483647 2147483647) (nc3-mul 4294967296 4294967296) (nc3-mul most-positive-fixnum 2)
                 (nc3-mul most-negative-fixnum -1) (nc3-mul 1.5 2) (nc3-mul (1+ most-positive-fixnum) 1)
                 (nc3-err (nc3-mul 'a 2)) (nc3-err (nc3-mul 2 nil))
                 (nc3-mul3 2 3 4) (nc3-mul3 2 1.5 2) (nc3-err (nc3-mul3 2 3 'x))))
(nc3 "rem" (list (nc3-rem 7 3) (nc3-rem -7 3) (nc3-rem 7 -3) (nc3-rem -7 -3) (nc3-rem 0 5)
                 (nc3-rem most-negative-fixnum -1) (nc3-rem most-positive-fixnum 2)
                 (nc3-err (nc3-rem 5 0)) (nc3-err (nc3-rem 5.0 2)) (nc3-err (nc3-rem 'a 2))
                 (nc3-err (nc3-rem (1+ most-positive-fixnum) 3))
                 (with-temp-buffer (insert "abcdef") (nc3-err (nc3-rem (copy-marker 5) 3)))))
(nc3 "max" (list (nc3-max 1 2) (nc3-max 2 1) (nc3-max 1 1.0) (nc3-max 1.0 1) (nc3-err (nc3-max 'a 1))))
;; --- eq / equal
(nc3 "eq" (let ((s "abc") (l (list 1 2)))
            (list (nc3-eq 1 1) (nc3-eq 1 2) (nc3-eq 'a 'a) (nc3-eq 'a 'b) (nc3-eq s s) (nc3-eq s (copy-sequence s))
                  (nc3-eq "" (copy-sequence "")) (nc3-eq l l) (nc3-eq l (copy-sequence l)) (nc3-eq nil 'nil)
                  (nc3-eq 1.5 1.5) (nc3-eq (1+ most-positive-fixnum) (1+ most-positive-fixnum))
                  (nc3-eq (make-symbol "u") (make-symbol "u")) (nc3-eq :kw :kw) (nc3-eq ?a 97))))
(nc3 "equal" (list (nc3-equal "abc" (copy-sequence "abc")) (nc3-equal "abc" "abd") (nc3-equal "" "")
                   (nc3-equal (propertize "ab" 'face 'bold) "ab") (nc3-equal '(1 (2 "x")) (list 1 (list 2 "x")))
                   (nc3-equal [1 2 (3)] (vector 1 2 (list 3))) (nc3-equal 1.5 1.5) (nc3-equal 1 1.0)
                   (nc3-equal (record 'r 1 "a") (record 'r 1 "a")) (nc3-equal (record 'r 1) (record 'r 2))
                   (nc3-equal (1+ most-positive-fixnum) (1+ most-positive-fixnum)) (nc3-equal 'a 'a)
                   (nc3-equal (make-bool-vector 3 t) (make-bool-vector 3 t)) (nc3-equal "é" "é")
                   (nc3-equal (string-to-multibyte "ab") "ab") (nc3-equal (make-char-table 'x) (make-char-table 'x))))
;; --- list walks
(nc3 "memq" (let ((l (list 'a 1 "s" nil 'b)) (s "s"))
              (list (nc3-memq 'a l) (nc3-memq 'b l) (nc3-memq 1 l) (nc3-memq nil l) (nc3-memq 'z l) (nc3-memq "s" l)
                    (nc3-memq s (list 1 s 2)) (nc3-memq 'x nil) (nc3-memq 1.5 (list 1.5)) (nc3-memq ?a (list 97))
                    (nc3-err (nc3-memq 'z '(a . b))) (nc3-memq 'a '(a . b)) (nc3-err (nc3-memq 'a 5))
                    (let ((cyc (list 'p 'q))) (setcdr (cdr cyc) cyc) (car (nc3-memq 'q cyc))))))
(nc3 "assq" (let ((al (list (cons 'a 1) 'junk nil (cons 'b 2) (cons nil 3) (list 'c))))
              (list (nc3-assq 'a al) (nc3-assq 'b al) (nc3-assq nil al) (nc3-assq 'c al) (nc3-assq 'z al)
                    (nc3-assq 1 (list (cons 1 'one))) (nc3-assq 'x nil) (nc3-err (nc3-assq 'z '((a . 1) . 5)))
                    (nc3-err (nc3-assq 'a 7)))))
(nc3 "nth" (let ((l (list 'a 'b 'c)))
             (list (nc3-nth 0 l) (nc3-nth 2 l) (nc3-nth 3 l) (nc3-nth 9 l) (nc3-nth -1 l) (nc3-nth 1 nil)
                   (nc3-err (nc3-nth 1 '(a . b))) (nc3-err (nc3-nth 'x l)) (nc3-err (nc3-nth 0 5))
                   (nc3-nth 1 (list 1 2.5)) (nc3-err (nc3-nth (1+ most-positive-fixnum) l)))))
(nc3 "nthcdr" (let ((l (list 'a 'b 'c)))
                (list (nc3-nthcdr 0 l) (nc3-nthcdr 1 l) (nc3-nthcdr 3 l) (nc3-nthcdr 5 l) (nc3-nthcdr -2 l)
                      (nc3-nthcdr 1 '(a . b)) (nc3-err (nc3-nthcdr 2 '(a . b))) (nc3-nthcdr 0 5)
                      (nc3-err (nc3-nthcdr 1 5)) (nc3-err (nc3-nthcdr 'x l)) (nc3-nthcdr 100000 l))))
(nc3 "length" (nc3-each #'nc3-length
                        (list nil "" "abc" "été" (string-to-unibyte "ab\377") (propertize "pq" 'face 'b)
                              '(1 2 3) (list 'closure t '(x) 'x) (list 'a 'b) [] [1 2 3] (make-vector 7 0)
                              (record 'r 1 2) (make-bool-vector 5 nil) '(a . b) 5 'sym 1.5 (make-char-table 'x))))
;; --- globals
(nc3 "set-symval" (list (nc3-set 'nc3-var 5) nc3-var (nc3-symval 'nc3-var) (nc3-set 'nc3-var "s") (nc3-symval 'nc3-var)
                        (nc3-err (nc3-set 5 1)) (nc3-err (nc3-set nil 1)) (nc3-err (nc3-set t 1))
                        (nc3-set :kw :kw) (nc3-err (nc3-set :kw 1)) (nc3-err (nc3-symval 'nc3-unbound-var))
                        (nc3-symval nil) (nc3-symval :kw) (nc3-err (nc3-symval 5))
                        (let ((nc3-var 'dyn)) (list (nc3-symval 'nc3-var) (nc3-set 'nc3-var 'dyn2) nc3-var))
                        nc3-var))
;; --- builtins without the call-out
(nc3 "fboundp" (nc3-each #'nc3-fboundp (list 'car 'nc3-pred 'nc3-unbound-fn 'nc3-var nil t :kw (make-symbol "u")
                                             'when 'string-search 5 "car" '(a))))
(nc3 "search" (list (nc3-search "b" "abcb" nil) (nc3-search "b" "abcb" 2) (nc3-search "z" "abc" nil) (nc3-search "" "abc" 1)
                    (nc3-search "é" "tété" nil) (nc3-search "é" "tété" 2)
                    (nc3-err (nc3-search "a" "abc" 9)) (nc3-err (nc3-search 'a "abc" nil)) (nc3-err (nc3-search "a" "abc" -1))
                    (nc3-search "b" (string-to-unibyte "ab") nil) (nc3-search "ab" (propertize "xab" 'face 'b) nil)))
(nc3 "funcall-apply" (list (nc3-funcall #'1+ 4) (nc3-funcall 'car '(x y)) (nc3-funcall (lambda (v) (list 'got v)) 3)
                           (nc3-funcall #'nc3-pred "s") (nc3-err (nc3-funcall 'nc3-unbound-fn 1)) (nc3-err (nc3-funcall 5 1))
                           (nc3-funcall0 #'list) (nc3-err (nc3-funcall0 #'car))
                           (nc3-apply #'+ '(1 2 3)) (nc3-apply #'list nil) (nc3-apply 'nc3-mul '(6 7))
                           (nc3-err (nc3-apply #'car '(1 2))) (nc3-err (nc3-apply #'+ 5))))
(nc3 "loop" (list (nc3-loop (list 1 "ab" 'a 7 nil "xyz" 'c 3)) (nc3-loop nil) (nc3-loop (list 'b))
                  (nc3-err (nc3-loop (list 'x 1.5 2)))))
(nc3 "loop-gc" (let ((acc nil) (i 0))
                 (while (< i 300)
                   (push (nc3-loop (list i (number-to-string i) 'b)) acc)
                   (when (= (% i 100) 0) (nc3-gc))
                   (setq i (1+ i)))
                 (list (length acc) (car acc) (car (last acc)))))
;; The negative control (NC_MODE=mutant: a wrong `<' in text-properties-at's
;; row) shows only as the route taken: count the full validations (nc2).
(nc3 "tp-at-route" (if (not (fboundp 'nelisp--tp-validate)) 'gnu
                     (with-temp-buffer
                       (insert "abcdef") (put-text-property 2 5 'face 'bold)
                       (let ((calls 0) r)
                         (unwind-protect
                             (progn (advice-add 'nelisp--tp-validate :before (lambda (&rest _) (setq calls (1+ calls)))
                                                '((name . nc3-tpv)))
                                    (setq r (list (text-properties-at 3) (text-properties-at 5) (get-text-property 4 'face))))
                           (advice-remove 'nelisp--tp-validate 'nc3-tpv))
                         (list r calls)))))
;; --- redefinition of builtins under a native caller (restored)
(nc3 "redef-fboundp-advice" (let (r)
                              (unwind-protect
                                  (progn (advice-add 'fboundp :around
                                                     (lambda (orig s) (if (eq s 'nc3-ghost) 'advised (funcall orig s)))
                                                     '((name . nc3-adv)))
                                         (setq r (list (nc3-fboundp 'nc3-ghost) (nc3-fboundp 'car))))
                                (advice-remove 'fboundp 'nc3-adv))
                              (list r (nc3-fboundp 'nc3-ghost))))
(nc3 "redef-search-advice" (if (not (fboundp 'nelisp--string-search-chars)) 'gnu
                             (let (r)
                               (unwind-protect
                                   (progn (advice-add 'nelisp--string-search-chars :filter-return
                                                      (lambda (v) (if (integerp v) (+ v 100) v)) '((name . nc3-s)))
                                          (setq r (nc3-search "b" "abc" nil)))
                                 (advice-remove 'nelisp--string-search-chars 'nc3-s))
                               (list r (nc3-search "b" "abc" nil)))))
(nc3 "redef-length-fset-loop" (let ((old (symbol-function 'nc3-length)) (acc nil) (i 0))
                                (unwind-protect
                                    (while (< i 4)
                                      (if (= (% i 2) 0) (fset 'nc3-length (lambda (_x) 'redef)) (fset 'nc3-length old))
                                      (push (nc3-funcall 'nc3-length "abc") acc)
                                      (setq i (1+ i)))
                                  (fset 'nc3-length old))
                                (nreverse acc)))

(when (fboundp 'nelisp--fx-register)
  ;; Register the corpus functions on the word VM (the install hook converts a
  ;; record whose fxir equals its harvested row).
  (dolist (n nc3-names) (nelisp--fx-register n)))
(when (and (getenv "NC_MODE") (boundp 'nelisp-nc--table))
  (let ((mode (intern (getenv "NC_MODE"))) (n 0))
    (dolist (row nelisp-nc--table)
      (when (and (fboundp (car row)) (not (memq :mutant row)))
        (when (nelisp-nc-select (car row) (if (eq mode 'mutant) 'native mode)) (setq n (1+ n)))))
    (when (eq mode 'mutant)
      (dolist (row nelisp-nc--table)
        (when (memq :mutant row) (nelisp-nc-select (car row) 'mutant))))
    (princ (format "MODE %s selected=%d\n" mode n))))
(when (getenv "NC_REPORT")
  (princ (format "NC3NATIVE %d\n" (length (seq-filter (lambda (n) (memq n (and (boundp 'nelisp-nc--converted) nelisp-nc--converted)))
                                                       nc3-names)))))
(if (getenv "NC_INLINE")
    ;; GNU byte code compiles `stringp' and `memq' to Bstringp / Bmemq, so a later
    ;; fset is not seen by compiled callers (GNU 31.1 byte-compiled: (t t)).
    (let ((olds (symbol-function 'stringp)) (oldm (symbol-function 'memq)) r)
      (unwind-protect
          (progn (fset 'stringp (lambda (_x) 'redef)) (fset 'memq (lambda (_x _l) 'redef))
                 (setq r (list (car (nc3-pred "s")) (and (nc3-memq 'a '(a)) t))))
        (fset 'stringp olds) (fset 'memq oldm))
      (princ (format "inline-stringp-memq-fset => %S\n" r)))
  (dolist (c (reverse nc3-cases))
    (nc3-gc)
    (princ (format "%s => %s\n" (car c)
                   (condition-case e (format "%S" (funcall (cdr c))) (error (format "ERR %S" e)))))))
