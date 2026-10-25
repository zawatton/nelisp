;;; nelisp-w1vec-test.el --- `vector' builds the canonical boxed vector  -*- lexical-binding: t -*-

;; w1vec: bf_vector_w returns the boxed vector (kind 8, word tag 6) that the word VM's
;; aref/aset/length arms accept, unless an argument is a legacy view (low bits 0), in which
;; case it keeps the 32-byte view.  Set W1VEC_SRC to run against another copy of the generator.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-fxvm-coverage-test)

(defvar w1vec--made nil)

(defun w1vec--run (args)
  "Run production `bf_vector_w' over ARGS (words); return (:boxed WORDS) or (:view WORDS)."
  (let* ((src (or (getenv "W1VEC_SRC")
                  (expand-file-name "scripts/nelisp-standalone-build.el" fxvm--root)))
         (defs (nelisp-fx-vector--extract src '(bf_vector_w bf_vec_count bf_vec_fill)))
         (fxvm--memory (make-hash-table :test 'eql))
         (next 100000) (cells (make-hash-table :test 'eql)) (views (make-hash-table :test 'eql))
         (head 3) (alloc (lambda (n) (prog1 next (setq next (+ next (* 8 (ceiling (max n 8) 8)) 64))))))
    (dolist (w (reverse args))
      (let ((c (funcall alloc 16)))
        (fxvm--write c 0 w) (fxvm--write c 8 head) (puthash c t cells) (setq head c)))
    (dolist (d defs) (eval `(defun ,(cadr d) ,(nth 2 d) ,@(mapcar #'fxvm--lower (nthcdr 3 d))) t))
    (cl-letf (((symbol-function 'heap-sexp-tag-word) (lambda (c) (if (gethash c cells) 7 0)))
              ((symbol-function 'nl_cons_car_word) (lambda (c) (fxvm--read c 0)))
              ((symbol-function 'nl_cons_cdr_ptr) (lambda (c) (fxvm--read c 8)))
              ((symbol-function 'alloc-bytes) (lambda (n _a) (funcall alloc n)))
              ((symbol-function 'nl_alloc_vector)
               (lambda (n)
                 (should (= n 0))
                 (let ((box (funcall alloc 8))) (fxvm--write box 0 8) box)))
              ((symbol-function 'vector-make) (lambda (n out) (puthash out (list n) views) 0))
              ((symbol-function 'vector-slot-set)
               (lambda (v i x) (setcdr (last (gethash v views)) (list (cons i x))) 0)))
      (let ((r (bf_vector_w head 0)))
        (if (gethash r views)
            (list :view (mapcar #'cdr (cdr (gethash r views))))
          (should (= (logand r 7) 6))
          (let* ((box (- r 6)) (n (ash (fxvm--read box 0) -8)))
            (should (= (logand (fxvm--read box 0) 255) 8))
            (list :boxed (cl-loop for i below n collect (fxvm--read box (+ 8 (* i 8)))))))))))

(ert-deftest w1v-vector-canonical-words-are-boxed ()
  (should (equal (w1vec--run (list 5 9 13 45)) '(:boxed (5 9 13 45))))
  (should (equal (w1vec--run (list 3 7 5)) '(:boxed (3 7 5))))
  (should (equal (w1vec--run nil) '(:boxed nil))))

(ert-deftest w1v-vector-legacy-view-argument-keeps-view ()
  (should (equal (w1vec--run (list 5 4096 9)) '(:view (5 4096 9))))
  (should (equal (w1vec--run (list 4096)) '(:view (4096)))))

(provide 'nelisp-w1vec-test)
