;;; nelisp-directcall-primx-test.el --- plist-get and make-hash-table on WORD argv  -*- lexical-binding: t; -*-

;; primx: `plist-get' (leaf id 100) and `make-hash-table' (id 118) are directcall
;; entries.  The model below is a word memory: a cons word is ADDR|2 with the car
;; at ADDR and the cdr at ADDR+8; a symbol word is N*16+6 named by `npx--names'.

(require 'nelisp-directcall-alloc-test)

(defvar npx--names nil)
(defvar npx--tables nil)
(defvar npx--markers nil)
(defvar npx--reserved nil)

(defun npx--sym (name)
  (or (car (rassoc name npx--names))
      (let ((w (+ (* 16 (+ 1000 (length npx--names))) 6))) (push (cons w name) npx--names) w)))

(defun npx--cons (a d)
  (let ((c (nda--alloc 16))) (puthash c a nda--mem) (puthash (+ c 8) d nda--mem) (+ c 2)))

(defun npx--plist (&rest words)
  (let ((r 3)) (dolist (w (reverse words) r) (setq r (npx--cons w r)))))

(defun npx--tag (w)
  (cond ((= (logand w 3) 1) 2) ((= w 3) 0) ((= w 7) 1)
        ((= (logand w 7) 2) 7) ((= (logand w 7) 6) 4) (t 99)))

(defun npx--with (thunk)
  (nda--with-model
   (lambda ()
     (let ((npx--names nil) (npx--tables nil) (npx--markers nil) (npx--reserved nil))
       (cl-letf (((symbol-function 'nl_val_tag) #'npx--tag)
                 ((symbol-function 'nl_cons_car_word)
                  (lambda (w) (if (= (npx--tag w) 7) (gethash (- w 2) nda--mem) 0)))
                 ((symbol-function 'nl_cons_cdr_word)
                  (lambda (w) (if (= (npx--tag w) 7) (gethash (+ (- w 2) 8) nda--mem) 0)))
                 ((symbol-function 'bf_eq2) (lambda (a b) (if (= a b) 1 0)))
                 ((symbol-function 'bf_plist_next_pair)
                  (lambda (p) (if (= (npx--tag p) 7)
                                  (let ((r (gethash (+ (- p 2) 8) nda--mem)))
                                    (if (= (npx--tag r) 7) (gethash (+ (- r 2) 8) nda--mem) r))
                                p)))
                 ((symbol-function 'bf_plist_cons_identity_w)
                  (lambda (a b) (if (and (= (npx--tag a) 7) (= (npx--tag b) 7) (= a b)) 1 0)))
                 ((symbol-function 'sexp-name-eq)
                  (lambda (w name) (if (equal (cdr (assq w npx--names)) (string-trim-left name ":")) 1
                                     (if (equal (cdr (assq w npx--names)) name) 1 0))))
                 ((symbol-function 'nl_word_canon) #'identity)
                 ((symbol-function 'nl_word_payload) (lambda (w) (ash w -2)))
                 ((symbol-function 'nl_ht_make_w) (lambda (_) (let ((tb (nda--alloc 64))) (push tb npx--tables) tb)))
                 ((symbol-function 'nl_ht_meta_slot) #'identity)
                 ((symbol-function 'nl_ht_marker_word_set)
                  (lambda (m i w) (push (list m i w) npx--markers) 0))
                 ((symbol-function 'nl_ht_reserve) (lambda (tb n) (push (list tb n) npx--reserved) 0)))
         (funcall thunk))))))

(ert-deftest npx-specs-and-leaf-ids ()
  (let ((forms (nelisp-standalone--directcall-defuns)))
    (dolist (spec '(("plist-get" 100) ("make-hash-table" 118)))
      (let ((blob (nth 2 (cl-find-if (lambda (f) (eq (car f) 'data-blob)) forms)))
            (sid (nelisp-standalone--sid (car spec))))
        (should sid)
        (should (= (aref blob (* sid 4)) (+ (cadr spec) 17)))))
    ;; plist-get is a leaf entry (below 112) and carries the leaf flag.
    (should (/= 0 (logand (ash 1 24) (nelisp-standalone--subr-desc "plist-get"))))
    (should (= 0 (logand (ash 1 24) (nelisp-standalone--subr-desc "make-hash-table"))))))

(ert-deftest npx-plist-get ()
  (npx--with
   (lambda ()
     (let* ((a (npx--sym "a")) (b (npx--sym "b")) (c (npx--sym "c"))
            (pl (npx--plist a (nda--fix 1) b (nda--fix 2) c (nda--fix 3))))
       (should (equal (nda--call 100 (list pl b)) (cons 0 (nda--fix 2))))
       (should (equal (nda--call 100 (list pl c)) (cons 0 (nda--fix 3))))
       (should (equal (nda--call 100 (list pl (npx--sym "z"))) (cons 0 3)))
       (should (equal (nda--call 100 (list 3 b)) (cons 0 3)))
       ;; odd-length plist: key present with no value cell yields nil
       (should (equal (nda--call 100 (list (npx--plist a (nda--fix 1) b) b)) (cons 0 3)))
       ;; improper tail ends the walk
       (should (equal (nda--call 100 (list (npx--cons a (nda--fix 5)) b)) (cons 0 3)))
       ;; a non-list plist is nil
       (should (equal (nda--call 100 (list (nda--fix 7) b)) (cons 0 3)))
       ;; a predicate argument declines with nothing written
       (should (equal (nda--call 100 (list pl b 3)) (cons -3 nil)))
       ;; circular plist terminates with nil
       (let* ((c1 (npx--cons a 0)) (c2 (npx--cons (nda--fix 1) c1)))
         (puthash (+ (- c1 2) 8) c2 nda--mem)
         (should (equal (nda--call 100 (list c1 b)) (cons 0 3))))
       (should (= nda--allocs 0))))))

(ert-deftest npx-make-hash-table ()
  (npx--with
   (lambda ()
     (let ((r (nda--call 118 nil)))
       (should (= (car r) 0)) (should (= (cdr r) (car npx--tables)))
       (should (null npx--reserved)))
     (setq npx--markers nil npx--reserved nil)
     (let* ((test (npx--sym ":test")) (size (npx--sym ":size")) (equal-s (npx--sym "equal"))
            (r (nda--call 118 (list test equal-s size (nda--fix 10) (npx--sym ":purecopy") 7))))
       (should (= (car r) 0))
       (should (member (list (car npx--tables) 2 equal-s) npx--markers))
       (should (equal npx--reserved (list (list (car npx--tables) 10)))))
     ;; first occurrence wins (GNU scans from the end)
     (setq npx--markers nil)
     (let* ((test (npx--sym ":test")) (r (nda--call 118 (list test (npx--sym "eq") test (npx--sym "equal")))))
       (should (= (car r) 0))
       (should (member (list (car npx--tables) 2 (npx--sym "eq")) npx--markers)))
     ;; declines before any allocation
     (dolist (bad (list (list (npx--sym ":test"))                     ; odd argc
                        (list (npx--sym ":bogus") 3)                    ; unknown keyword
                        (list (npx--sym ":test") (npx--sym "my-test"))  ; custom test
                        (list (npx--sym ":size") (nda--fix -1))
                        (list (npx--sym ":size") 3 )                     ; nil size is allowed => see below
                        (list (npx--sym ":weakness") 7)                  ; t needs normalising
                        (list (nda--fix 1) 3)))
       (setq nda--allocs 0 npx--tables nil)
       (let ((r (nda--call 118 bad)))
         (unless (equal bad (list (npx--sym ":size") 3))
           (should (equal r (cons -3 nil)))
           (should (null npx--tables))))))))

(provide 'nelisp-directcall-primx-test)
