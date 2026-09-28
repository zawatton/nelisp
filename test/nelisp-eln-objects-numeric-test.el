;;; nelisp-eln-objects-numeric-test.el --- numeric graph leases -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(provide 'nl-ffi-memory)
(require 'nelisp-eln-abi)
(require 'nelisp-eln-string)
(load (expand-file-name "../lisp/nelisp-eln-objects.el"
                        (file-name-directory load-file-name)))
(require 'nelisp-eln-bignum)
(require 'nelisp-eln-float)

(defvar nelisp-eln-objects-numeric-test--next 65536)
(defvar nelisp-eln-objects-numeric-test--blocks nil)
(defvar nelisp-eln-objects-numeric-test--allocations 0)
(defvar nelisp-eln-objects-numeric-test--releases 0)

(defun nelisp-eln-objects-numeric-test--allocate (size)
  (let* ((base nelisp-eln-objects-numeric-test--next)
         (block (vector base (make-vector size 0))))
    (setq nelisp-eln-objects-numeric-test--next (+ base 4096)
          nelisp-eln-objects-numeric-test--blocks
          (cons block nelisp-eln-objects-numeric-test--blocks)
          nelisp-eln-objects-numeric-test--allocations
          (1+ nelisp-eln-objects-numeric-test--allocations))
    block))

(defun nelisp-eln-objects-numeric-test--block (address)
  (or (cl-find-if
       (lambda (block)
         (<= (aref block 0) address
             (1- (+ (aref block 0) (length (aref block 1))))))
       nelisp-eln-objects-numeric-test--blocks)
      (error "unmapped address: %s" address)))

(defun nelisp-eln-objects-numeric-test--write-u32 (address offset value)
  (let* ((absolute (+ address offset))
         (block (nelisp-eln-objects-numeric-test--block absolute))
         (bytes (aref block 1))
         (index (- absolute (aref block 0)))
         (i 0))
    (while (< i 4)
      (aset bytes (+ index i) (logand (ash value (* -8 i)) 255))
      (setq i (1+ i)))))

(defun nelisp-eln-objects-numeric-test--read-u32 (address offset)
  (let* ((absolute (+ address offset))
         (block (nelisp-eln-objects-numeric-test--block absolute))
         (bytes (aref block 1))
         (index (- absolute (aref block 0)))
         (result 0) (i 0))
    (while (< i 4)
      (setq result (+ result (ash (aref bytes (+ index i)) (* 8 i)))
            i (1+ i)))
    result))

(defmacro nelisp-eln-objects-numeric-test--with-memory (&rest body)
  (declare (indent 0) (debug t))
  `(let ((nelisp-eln-objects-numeric-test--next 65536)
         (nelisp-eln-objects-numeric-test--blocks nil)
         (nelisp-eln-objects-numeric-test--allocations 0)
         (nelisp-eln-objects-numeric-test--releases 0))
     (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
                #'nelisp-eln-objects-numeric-test--allocate)
               ((symbol-function 'nl-ffi-memory-address)
                (lambda (block) (aref block 0)))
               ((symbol-function 'nl-ffi-memory-release)
                (lambda (block)
                  (setq nelisp-eln-objects-numeric-test--releases
                        (1+ nelisp-eln-objects-numeric-test--releases)
                        nelisp-eln-objects-numeric-test--blocks
                        (delq block nelisp-eln-objects-numeric-test--blocks))))
               ((symbol-function 'ptr-write-u32)
                #'nelisp-eln-objects-numeric-test--write-u32)
               ((symbol-function 'ptr-read-u32)
                #'nelisp-eln-objects-numeric-test--read-u32))
       ,@body)))

(ert-deftest nelisp-eln-objects-numeric-encodes-direct-bignum-and-gc-root ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value (ash 1 130))
           (unit (nelisp-eln-objects-create))
           (word (nelisp-eln-objects-encode unit value)))
      (unwind-protect
          (progn
            (should (eq (nelisp-eln-abi-classify-word word) 'vectorlike))
            (should (eq (nelisp-eln-objects-decode unit word) value))
            (garbage-collect)
            (should (eq (nelisp-eln-objects-decode unit word) value)))
        (nelisp-eln-objects-release unit)))))

(ert-deftest nelisp-eln-objects-numeric-supports-bignum-in-cons-graph ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value (ash 1 130))
           (cell (cons value nil))
           (unit (nelisp-eln-objects-create))
           (word (nelisp-eln-objects-encode unit cell))
           (address (- word 3)))
      (unwind-protect
          (progn
            (should (eq (nelisp-eln-objects-decode unit word) cell))
            (should (= (nelisp-eln-objects--read-word address 0)
                       (nelisp-eln-objects--encode-word
                        (nelisp-eln-objects--resolve unit) value)))
            (should (eq (nelisp-eln-objects-decode
                         unit (nelisp-eln-objects--read-word address 0)) value)))
        (nelisp-eln-objects-release unit)))))

(ert-deftest nelisp-eln-objects-numeric-shares-identity-and-activation-lease ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value (ash 1 130))
           (first (nelisp-eln-objects-create))
           (second (nelisp-eln-objects-create))
           (word-a (nelisp-eln-objects-encode first value))
           (word-b (nelisp-eln-objects-encode second value))
           (activation (nelisp-eln-objects-activation-acquire first)))
      (unwind-protect
          (progn
            (should (= word-a word-b))
            (should (= (aref (nelisp-eln-objects--global-record value) 5) 2))
            (nelisp-eln-objects-release first)
            (should (eq (nelisp-eln-objects-activation-decode activation word-a)
                        value))
            (nelisp-eln-objects-release second)
            (should (eq (nelisp-eln-objects-activation-decode activation word-a)
                        value))
            (nelisp-eln-objects-activation-release activation)
            (should-error (nelisp-eln-objects-activation-decode activation word-a)
                          :type 'nelisp-eln-objects-error))
        (when (assq activation nelisp-eln-objects--activations)
          (nelisp-eln-objects-activation-release activation))
        (when (assq first nelisp-eln-objects--live-units)
          (nelisp-eln-objects-release first))
        (when (assq second nelisp-eln-objects--live-units)
          (nelisp-eln-objects-release second))))))

(ert-deftest nelisp-eln-objects-numeric-rejects-foreign-bignum-word ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value (ash 1 130))
           (unit (nelisp-eln-objects-create))
           (own-word (nelisp-eln-objects-encode unit value))
           (foreign (nelisp-eln-bignum-allocate (ash 1 131))))
      (unwind-protect
            (should (eq (nelisp-eln-abi-classify-word own-word) 'vectorlike))
            (should-error (nelisp-eln-objects-decode
                         unit (nelisp-eln-bignum-word foreign))
                        :type 'nelisp-eln-objects-error)
        (nelisp-eln-bignum-release foreign)
        (nelisp-eln-objects-release unit)))))

(ert-deftest nelisp-eln-objects-numeric-rolls-back-partial-owner-setup ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let ((unit (nelisp-eln-objects-create))
          (value (ash 1 130)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'nelisp-eln-bignum-address)
                       (lambda (_owner) (error "injected address failure"))))
              (should-error (nelisp-eln-objects-encode unit value)))
            (should-not (aref (nelisp-eln-objects--resolve unit) 9))
            (should-not (nelisp-eln-objects--global-record value))
            (should-not nelisp-eln-bignum--live)
            (should (= nelisp-eln-objects-numeric-test--allocations
                       nelisp-eln-objects-numeric-test--releases)))
        (nelisp-eln-objects-release unit)))))

(ert-deftest nelisp-eln-objects-numeric-does-not-extend-activation-snapshot ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((first-value (ash 1 130))
           (later-value (ash 1 131))
           (unit (nelisp-eln-objects-create))
           (first-word (nelisp-eln-objects-encode unit first-value))
           (activation (nelisp-eln-objects-activation-acquire unit)))
      (unwind-protect
          (let ((member-count
                 (length (aref (nelisp-eln-objects--resolve-activation activation)
                               1)))
                (later-word (nelisp-eln-objects-encode unit later-value)))
            (should (= (length (aref
                                (nelisp-eln-objects--resolve-activation activation)
                                1)) member-count))
            (should (eq (nelisp-eln-objects-activation-decode
                         activation first-word) first-value))
            (should-error (nelisp-eln-objects-activation-decode
                           activation later-word)
                          :type 'nelisp-eln-objects-error))
        (when (assq activation nelisp-eln-objects--activations)
          (nelisp-eln-objects-activation-release activation))
        (when (assq unit nelisp-eln-objects--live-units)
          (nelisp-eln-objects-release unit))))))

(ert-deftest nelisp-eln-objects-numeric-retries-failed-unmap-with-quarantine ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value (ash 1 130))
           (unit (nelisp-eln-objects-create))
           (word (nelisp-eln-objects-encode unit value))
           (record (cdr (assq value nelisp-eln-objects--identity-records)))
           (owner (aref record 3))
           (real-release (symbol-function 'nl-ffi-memory-release))
           (attempts 0))
      (cl-letf (((symbol-function 'nl-ffi-memory-release)
                 (lambda (block)
                   (setq attempts (1+ attempts))
                   (if (<= attempts 2)
                       (error "injected unmap failure")
                     (funcall real-release block)))))
        (should (eq (nelisp-eln-objects-decode unit word) value))
        (should-error (nelisp-eln-objects-release unit))
        (should (eq nelisp-eln-objects--registry-state 'poisoned))
        (should (eq (aref owner 4) 'cleanup-pending))
        (should (eq (cdr (assq value nelisp-eln-objects--identity-records))
                    record))
        (should (nelisp-eln-objects-retry-pending-cleanup))
        (should (eq nelisp-eln-objects--registry-state 'poisoned))
        (should (eq (aref owner 4) 'cleanup-pending))
        (should (eq (cdr (assq value nelisp-eln-objects--identity-records))
                    record))
        (should (nelisp-eln-objects-release unit))
        (should (eq (aref owner 4) 'closed))
        (should-not (assq value nelisp-eln-objects--identity-records))
        (should (eq nelisp-eln-objects--registry-state 'open))
        (should (= attempts 3))
        (should (= nelisp-eln-objects-numeric-test--allocations
                   nelisp-eln-objects-numeric-test--releases))))))

(ert-deftest nelisp-eln-objects-numeric-mocks-float-view-api ()
  (nelisp-eln-objects-numeric-test--with-memory
    (let* ((value 1.25)
           (owner (vector 'mock-float value 0 8192 'open))
           (unit (nelisp-eln-objects-create))
           (released nil))
      (cl-letf (((symbol-function 'nelisp-eln-float-allocate)
                     (lambda (source)
                       (should (eq source value)) owner))
                    ((symbol-function 'nelisp-eln-float-source)
                     (lambda (candidate) (aref candidate 1)))
                    ((symbol-function 'nelisp-eln-float-address)
                     (lambda (_candidate) 8192))
                    ((symbol-function 'nelisp-eln-float-word)
                     (lambda (_candidate) 8199))
                    ((symbol-function 'nelisp-eln-float-release)
                     (lambda (_candidate) (setq released t)))
                    ((symbol-function 'nelisp--float-word-half)
                     (lambda (&rest _) 0)))
        (unwind-protect
            (let ((word (nelisp-eln-objects-encode unit value)))
              (should (= word 8199))
              (should (eq (nelisp-eln-objects-decode unit word) value))
              (nelisp-eln-objects-release unit)
              (should released)))
          (when (assq unit nelisp-eln-objects--live-units)
            (nelisp-eln-objects-release unit))))))

;;; nelisp-eln-objects-numeric-test.el ends here
