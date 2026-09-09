;;; nelisp-applyfn-length-dispatch-test.el --- applyfn bucket tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defconst nelisp-applyfn-length-test--table
  '(((:u8 "a") . (first-a))
    ((:lit "é") . (non-ascii))
    ((:lit "a") . (second-a))))

(ert-deftest nelisp-applyfn-length-dispatch-preserves-first-match-and-byte-length ()
  "Buckets retain source order and count UTF-8 bytes rather than characters."
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
    (should
     (equal
      (nelisp-standalone--applyfn-build-dispatch
       nelisp-applyfn-length-test--table 99)
      '(let* ((nl_name_len
               (if (= (sexp-tag name_ptr) 4) (ptr-read-u64 name_ptr 24)
                 (if (= (sexp-tag name_ptr) 5) (ptr-read-u64 name_ptr 24)
                   (if (= (sexp-tag name_ptr) 14) (ptr-read-u64 name_ptr 24)
                     -1)))))
         (if (= nl_name_len 1)
             (if (= (sexp-name-eq name_ptr "a") 1) (first-a)
               (if (= (sexp-name-eq name_ptr "a") 1) (second-a) 99))
           (if (= nl_name_len 2)
               (if (= (sexp-name-eq name_ptr "é") 1) (non-ascii) 99)
             99)))))))

(ert-deftest nelisp-applyfn-flat-toggle-retains-old-dispatch-shape ()
  "The environment toggle emits the old flat first-match chain for A/B builds."
  (cl-letf (((symbol-function 'getenv)
             (lambda (name) (and (equal name "NELISP_T104_FLAT_DISPATCH") "1"))))
    (should
     (equal
      (nelisp-standalone--applyfn-build-dispatch
       nelisp-applyfn-length-test--table 99)
      '(if (= (sexp-name-eq name_ptr "a") 1) (first-a)
         (if (= (sexp-name-eq name_ptr "é") 1) (non-ascii)
           (if (= (sexp-name-eq name_ptr "a") 1) (second-a) 99)))))))

(ert-deftest nelisp-applyfn-length-dispatch-shares-reader-fallback-only ()
  "Reader assembly emits one fallback helper; baked assembly stays standalone."
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
    (let ((reader (nelisp-standalone--applyfn-assemble
                   nil nelisp-applyfn-length-test--table))
          (baked (nelisp-standalone--applyfn-assemble
                  nil nelisp-applyfn-length-test--table 99)))
      (should (= 1 (cl-count 'nl_applyfn_unsupported reader
                             :key (lambda (form) (and (consp form) (cadr form))))))
      (should-not (cl-find 'nl_applyfn_unsupported baked
                           :key (lambda (form) (and (consp form) (cadr form)))))
      (should (equal (car (last baked))
                     '(defun nelisp_apply_function (func_ptr args env out)
                        (let* ((name_ptr
                                (nl_cons_car_ptr (nl_cons_cdr_ptr func_ptr))))
                          (let* ((nl_name_len
                                  (if (= (sexp-tag name_ptr) 4)
                                      (ptr-read-u64 name_ptr 24)
                                    (if (= (sexp-tag name_ptr) 5)
                                        (ptr-read-u64 name_ptr 24)
                                      (if (= (sexp-tag name_ptr) 14)
                                          (ptr-read-u64 name_ptr 24)
                                        -1)))))
                            (if (= nl_name_len 1)
                                (if (= (sexp-name-eq name_ptr "a") 1)
                                    (first-a)
                                  (if (= (sexp-name-eq name_ptr "a") 1)
                                      (second-a) 99))
                              (if (= nl_name_len 2)
                                  (if (= (sexp-name-eq name_ptr "é") 1)
                                      (non-ascii) 99)
                                99))))))))))

(provide 'nelisp-applyfn-length-dispatch-test)
;;; nelisp-applyfn-length-dispatch-test.el ends here
