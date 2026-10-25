;;; nelisp-bytecode-stackset-fast-test.el --- Stack-set batching -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun sst--find (tree predicate)
  "Return the first subtree of TREE accepted by PREDICATE."
  (cond ((funcall predicate tree) tree)
        ((consp tree) (or (sst--find (car tree) predicate)
                         (sst--find (cdr tree) predicate)))))

(defun sst--function (name)
  (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun)
                                (eq (cadr form) name)))
              nelisp-standalone--applyfn-bytecode-helpers))

(defun sst--starter-mask ()
  (cadr (sst--find (sst--function 'wf_bytecode_execute_frame)
                  (lambda (form)
                    (and (eq (car-safe form) 'logand)
                         (equal (caddr form)
                                '(shl 1 (- first-op 128))))))))

(ert-deftest sst-structural-stackset-leaf ()
  (should (sst--find (sst--function 'wf_bytecode_w_fast_run)
                    (lambda (form) (equal form '(= raw 178)))))
  ;; 179 has no outer implementation and must remain unsupported.
  (should-not (sst--find (sst--function 'wf_bytecode_execute_frame)
                        (lambda (form) (equal form '(= base 179)))))
  (should-not (sst--find (sst--function 'wf_bytecode_w_fast_run)
                        (lambda (form) (equal form '(= raw 179))))))

(ert-deftest sst-structural-stackset-admission ()
  (should (/= 0 (logand (sst--starter-mask) (ash 1 50))))
  (should (= 0 (logand (sst--starter-mask) (ash 1 51))))
  ;; Other independently covered fast leaves may add starter bits; every
  ;; pre-stack-set starter must still be admitted.
  (should (= (logand (sst--starter-mask) 412316861308)
             412316861308)))

(defun sst--host-form (tree)
  "Execute the production source IR with checked host memory operations.
Only syntax and raw memory/shift primitives are adapted; opcode logic is
the actual generated definition, including its dispatch and state packing."
  (cond
   ((atom tree) tree)
   ((memq (car tree) '(if while))
    (cons (car tree)
          (cons (list 'sst--truth (sst--host-form (cadr tree)))
                (mapcar #'sst--host-form (cddr tree)))))
   (t
    (cons (pcase (car tree)
            ('seq 'progn) ('shl 'ash) ('sar 'sst--sar)
            (_ (sst--host-form (car tree))))
          (mapcar #'sst--host-form (cdr tree))))))

(defun sst--truth (value)
  "Source IR uses integer zero for false; host Elisp does not."
  (and value (not (equal value 0))))

(defun sst--sar (value shift) (ash value (- shift)))

(defun sst--run (code sp depth &optional outer)
  "Run the full fast-loop source IR, or the existing OUTER stack-set arm.
The address space checks byte fetches and word accesses, with canaries on
both sides of the stack. Return packed state, memory and stack accesses."
  (let* ((data 1024) (mem 4096) (cell 8192)
         (words (make-hash-table :test #'eql))
         (before nil) (reads nil) (writes nil)
         (pc 0)
         (operand (and (> (length code) 1) (aref code 1)))
         (top (+ mem (* (1- sp) 8))))
    (cl-loop for i from -1 to depth do
             (puthash (+ mem (* i 8)) (+ 101 (* i 4)) words))
    (puthash cell 1 words)
    (puthash (+ cell 8) (1+ (* mem 4)) words)
    (setq before (copy-hash-table words))
    (cl-labels
        ((read-word (address offset)
           (let ((at (+ address offset)))
             (unless (gethash at words) (error "Unexpected word read: %s" at))
             (push at reads) (gethash at words)))
         (write-word (address offset value)
           (let ((at (+ address offset)))
             (unless (gethash at words) (error "Unexpected word write: %s" at))
             (push (cons at value) writes) (puthash at value words)))
         (read-byte (address offset)
           (unless (and (= address data) (>= offset 0) (< offset (length code)))
             (error "Out-of-bounds operand fetch: %s %s" address offset))
           (aref code offset)))
      (cl-letf (((symbol-function 'heap-read-u8) #'read-byte)
                ((symbol-function 'heap-read-u64) #'read-word)
                ((symbol-function 'ptr-read-u64) #'read-word)
                ((symbol-function 'ptr-write-u64) #'write-word))
        (let ((state
               (if outer
                   (let ((arm (sst--find (sst--function 'wf_bytecode_execute_frame)
                                        (lambda (form)
                                          (equal (car-safe form) '(= base 178))))))
                     ;; Keep a shared lexical environment so SP and DONE
                     ;; updates can be observed after the outer arm.
                     (eval (sst--host-form
                            `(let ((sp ,sp) (depth ,depth) (operand ,operand)
                                   (mem ,mem) (top ,top) (done 0) (bad-op 0) (raw 178))
                               ,@(cdr arm) (list sp done))) t))
                 (apply (eval (cons 'lambda
                                    (cddr (sst--host-form
                                           (sst--function 'wf_bytecode_w_fast_run)))) t)
                        (list data (length code) mem depth 2048 0 pc sp 0 cell)))))
          (list :state state :before before :after words
                :stack-reads (cl-remove-if (lambda (at) (>= at cell)) reads)
                :stack-writes (cl-remove-if (lambda (pair) (>= (car pair) cell)) writes)))))))

(defun sst--same-memory-p (left right)
  (and (= (hash-table-count left) (hash-table-count right))
       (cl-loop for key being the hash-keys of left
                always (equal (gethash key left) (gethash key right)))))

(ert-deftest sst-host-ir-zero-is-false ()
  (should (= (eval (sst--host-form '(if (if (< 1 4) 0 1) 10 20)) t) 20))
  (should (= (eval (sst--host-form '(if (if (< 4 1) 0 1) 10 20)) t) 10)))

(ert-deftest sst-seed-stack-ref-invalid-convention ()
  ;; Probe the existing (< raw 6) arm before adding the new leaf. A failed
  ;; stack-ref must return its input PC/SP without touching stack memory.
  (dolist (case '((0 4 4) (0 0 4) (5 2 4)))
    (pcase-let* ((`(,opcode ,sp ,depth) case)
                 (result (sst--run (vector opcode 138) sp depth)))
      (should (= (plist-get result :state) sp))
      (should-not (plist-get result :stack-reads))
      (should-not (plist-get result :stack-writes))
      (should (sst--same-memory-p (plist-get result :before)
                                 (plist-get result :after))))))

(ert-deftest sst-semantics-valid-and-boundaries ()
  ;; GNU: ptr = top - FETCH; *ptr = POP. SP counts live slots, hence
  ;; index = sp - operand - 1. Include self-assignment and both boundaries.
  (dolist (case '((4 4 2) (4 4 3) (4 4 0) (1 4 0) (4 4 1)))
    (pcase-let* ((`(,sp ,depth ,operand) case)
                 (code (vector 178 operand 138))
                 (result (sst--run code sp depth))
                 (outer (sst--run code sp depth t))
                 (expected (copy-hash-table (plist-get result :before))))
      (puthash (+ 4096 (* (- sp operand 1) 8))
               (gethash (+ 4096 (* (1- sp) 8)) expected) expected)
      (should (= (plist-get result :state) (+ (ash 2 17) (1- sp))))
      (should (equal (plist-get outer :state) (list (1- sp) 0)))
      (should (sst--same-memory-p expected (plist-get result :after)))
      (should (sst--same-memory-p (plist-get outer :after)
                                 (plist-get result :after))))))

(ert-deftest sst-semantics-invalid-and-truncated ()
  (dolist (case '((0 4 0) (-1 4 0) (2 4 2) (2 4 255) (5 4 0)))
    (pcase-let* ((`(,sp ,depth ,operand) case)
                 (code (vector 178 operand 138))
                 (result (sst--run code sp depth))
                 (outer (sst--run code sp depth t)))
      (should (= (plist-get result :state) sp))
      (should (equal (plist-get outer :state) (list sp 2)))
      (should-not (plist-get result :stack-reads))
      (should-not (plist-get result :stack-writes))
      (should (sst--same-memory-p (plist-get result :before)
                                 (plist-get result :after)))
      (should (sst--same-memory-p (plist-get outer :after)
                                 (plist-get result :after)))))
  (let ((result (sst--run [178] 2 4)))
    (should (= (plist-get result :state) 2))
    (should-not (plist-get result :stack-reads))
    (should-not (plist-get result :stack-writes))
    (should (sst--same-memory-p (plist-get result :before)
                               (plist-get result :after)))))

(provide 'nelisp-bytecode-stackset-fast-test)
;;; nelisp-bytecode-stackset-fast-test.el ends here
