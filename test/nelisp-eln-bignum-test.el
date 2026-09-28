;;; nelisp-eln-bignum-test.el --- owned bignum projection tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(provide 'nl-ffi-memory)
(require 'nelisp-eln-abi)
(require 'nelisp-eln-bignum)

(defvar nelisp-eln-bignum-test--next 4096)
(defvar nelisp-eln-bignum-test--blocks nil)
(defvar nelisp-eln-bignum-test--releases 0)

(defun nelisp-eln-bignum-test--allocate (size)
  (let* ((base nelisp-eln-bignum-test--next)
         (block (vector base (make-vector size 0))))
    (setq nelisp-eln-bignum-test--next (+ base 4096)
          nelisp-eln-bignum-test--blocks (cons block nelisp-eln-bignum-test--blocks))
    block))

(defun nelisp-eln-bignum-test--block (address)
  (or (cl-find-if (lambda (block)
                    (<= (aref block 0) address
                        (+ (aref block 0) (length (aref block 1)))) )
                  nelisp-eln-bignum-test--blocks)
      (error "unmapped test address: %s" address)))

(defun nelisp-eln-bignum-test--write-u32 (address offset value)
  (let* ((block (nelisp-eln-bignum-test--block (+ address offset)))
         (bytes (aref block 1))
         (index (- (+ address offset) (aref block 0)))
         (i 0))
    (while (< i 4)
      (aset bytes (+ index i) (logand (ash value (* -8 i)) 255))
      (setq i (1+ i)))))

(defun nelisp-eln-bignum-test--read-u32 (address offset)
  (let* ((block (nelisp-eln-bignum-test--block (+ address offset)))
         (bytes (aref block 1))
         (index (- (+ address offset) (aref block 0)))
         (result 0) (i 0))
    (while (< i 4)
      (setq result (+ result (ash (aref bytes (+ index i)) (* 8 i)))
            i (1+ i)))
    result))

(defun nelisp-eln-bignum-test--read-u64 (address offset)
  (+ (nelisp-eln-bignum-test--read-u32 address offset)
     (ash (nelisp-eln-bignum-test--read-u32 address (+ offset 4)) 32)))

(defmacro nelisp-eln-bignum-test--with-memory (&rest body)
  (declare (indent 0) (debug t))
  `(let ((nelisp-eln-bignum-test--next 4096)
         (nelisp-eln-bignum-test--blocks nil)
         (nelisp-eln-bignum-test--releases 0))
     (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
                #'nelisp-eln-bignum-test--allocate)
               ((symbol-function 'nl-ffi-memory-address) (lambda (block) (aref block 0)))
               ((symbol-function 'nl-ffi-memory-release)
                (lambda (block)
                  (setq nelisp-eln-bignum-test--releases
                        (1+ nelisp-eln-bignum-test--releases))
                  (setq nelisp-eln-bignum-test--blocks
                        (delq block nelisp-eln-bignum-test--blocks))))
               ((symbol-function 'ptr-write-u32)
                #'nelisp-eln-bignum-test--write-u32)
               ((symbol-function 'ptr-read-u32)
                #'nelisp-eln-bignum-test--read-u32))
       ,@body)))

(ert-deftest nelisp-eln-bignum-encodes-boundary-and-multilimb-values ()
  (nelisp-eln-bignum-test--with-memory
    (dolist (value (list (ash 1 61) (1- (- (ash 1 61))) (ash 1 130)
                         (- (ash 1 130))))
      (let* ((owner (nelisp-eln-bignum-allocate value))
             (address (nelisp-eln-bignum-address owner))
             (magnitude (abs value))
             (remaining magnitude) (count 0))
        (while (> remaining 0)
          (setq remaining (ash remaining -64) count (1+ count)))
        (unwind-protect
            (progn
              (should (= (nelisp-eln-bignum-test--read-u64 address 0)
                         (+ nelisp-eln-bignum--header-flag
                            (ash nelisp-eln-bignum--pvec-bignum 24)
                            (ash 2 12))))
              (should (= (nelisp-eln-bignum-test--read-u32 address 8) count))
              (should (= (nelisp-eln-bignum-test--read-u32 address 12)
                         (logand (if (< value 0) (- count) count)
                                 #xffffffff)))
              (should (= (nelisp-eln-bignum-test--read-u64 address 16)
                         (+ address 24)))
              (dotimes (index count)
                (should (= (nelisp-eln-bignum-test--read-u64
                            address (+ 24 (* index 8)))
                           (logand (ash magnitude (* -64 index))
                                   nelisp-eln-abi-word-mask))))
              (should (= (logand (nelisp-eln-bignum-word owner) 7) 5))
              (should (eq value (nelisp-eln-bignum-source owner)))
              (should (eq value (nelisp-eln-bignum-decode
                                 owner (nelisp-eln-bignum-word owner))))
              (garbage-collect)
              (should (eq value (nelisp-eln-bignum-source owner))))
          (nelisp-eln-bignum-release owner))))))

(ert-deftest nelisp-eln-bignum-rejects-fixnums-foreign-words-and-stale-owners ()
  (nelisp-eln-bignum-test--with-memory
    (should-error (nelisp-eln-bignum-allocate nelisp-eln-abi-fixnum-max)
                  :type 'nelisp-eln-bignum-error)
    (should-error
     (nelisp-eln-bignum-word
      (vector nelisp-eln-bignum--marker (ash 1 61) nil 4096 'open nil))
     :type 'nelisp-eln-bignum-error)
    (should (= (length nelisp-eln-bignum-test--blocks) 0))
    (let ((owner (nelisp-eln-bignum-allocate (ash 1 61))))
      (should-error (nelisp-eln-bignum-decode owner
                                              (+ 8 (nelisp-eln-bignum-word owner)))
                    :type 'nelisp-eln-bignum-error)
      (should (nelisp-eln-bignum-release owner))
      (should (nelisp-eln-bignum-release owner))
      (should-error (nelisp-eln-bignum-word owner)
                    :type 'nelisp-eln-bignum-error)
      (should-error (nelisp-eln-bignum-source owner)
                    :type 'nelisp-eln-bignum-error)
      (should (= nelisp-eln-bignum-test--releases 1)))))

(ert-deftest nelisp-eln-bignum-releases-allocation-after-write-failure ()
  (nelisp-eln-bignum-test--with-memory
    (let ((writes 0))
      (cl-letf (((symbol-function 'ptr-write-u32)
                 (lambda (&rest args)
                   (setq writes (1+ writes))
                   (if (= writes 3) (error "injected write failure")
                     (apply #'nelisp-eln-bignum-test--write-u32 args)))))
        (should-error (nelisp-eln-bignum-allocate (ash 1 130)))
        (should (= nelisp-eln-bignum-test--releases 1))
        (should (= (length nelisp-eln-bignum-test--blocks) 0))))))

(provide 'nelisp-eln-bignum-test)

;;; nelisp-eln-bignum-test.el ends here
