;;; nelisp-bytecode-jit.el --- narrow native lowering for byte-code  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; P5 proof slice: lower bounded GNU byte-code streams through the existing
;; AOT chain. Integer operations use the raw ABI; object predicates and
;; immutable symbol constants use the boxed ABI.

;;; Code:

(require 'nelisp-native-load)
(require 'cl-lib)
(require 'nelisp-bytecode-ir)

(defvar nelisp-bytecode-jit--handles (make-hash-table :test 'eq)
  "Published native handles keyed by byte-code function object.")
(defvar nelisp-bytecode-jit--pending-eln-closes nil
  "GNU ELN module handles awaiting safe close after callable leases expire.")
(defvar nelisp-bytecode-jit--eln-entry-counter 0
  "Monotonic suffix for unique generated GNU subr names.")
(defvar nelisp-bytecode-jit--pending-queue nil
  "FIFO queue of byte-code functions awaiting explicit native preparation.")
(defvar nelisp-bytecode-jit--pending-identities (make-hash-table :test 'eq)
  "Current queued identity for each function in `--pending-queue'.")
(defvar nelisp-bytecode-jit--compile-failures (make-hash-table :test 'eq)
  "Terminal compile failure records keyed by function and cache identity.")
(defvar nelisp-bytecode-jit--preparing-active nil
  "Non-nil while an explicit native preparation is compiling or loading.")
(defvar nelisp-bytecode-jit--draining-active nil
  "Non-nil while `nelisp-bytecode-jit-drain-pending' processes its snapshot.")
(defvar nelisp-bytecode-jit--deferred-preparation-enabled nil
  "Non-nil after the native safe-point callback is installed and active.")
(defvar nelisp-bytecode-jit--runtime-identity-cache :unset
  "Memoized runtime identity, including an unavailable identity result.")
(defconst nelisp-bytecode-jit--cache-abi "p5-jit-ir-v1"
  "Identity tag for this JIT lowering and call ABI.")
(defconst nelisp-bytecode-jit--drain-batch-size 1
  "Maximum queued compiles performed by one outer evaluation boundary.")
(defconst nelisp-bytecode-jit--pending-limit 256
  "Maximum number of distinct functions retained for preparation.")

(defvar nelisp-bytecode-jit-threshold 3
  "Number of calls before a supported unary function is native-hot.")

(defvar nelisp-bytecode-jit--hot-counts (make-hash-table :test 'eq)
  "Call count by byte-code function object.")

(defvar nelisp-bytecode-jit--native-call-count 0
  "Number of calls that entered a generated RX machine-code entry.")

(defvar nelisp-bytecode-jit--compile-count 0
  "Number of distinct byte-code functions compiled to native artifacts.")

(defvar nelisp-bytecode-jit--dispatch-attempt-count 0
  "Number of ordinary byte-code function calls reaching the P5 hook.")

(defvar nelisp-bytecode-jit--interpreter-fallback-count 0
  "Number of eligible calls that stayed in the byte-code VM.")

(defvar nelisp-bytecode-jit--dispatch-active nil
  "Non-nil while the P5 dispatch hook is inspecting or running a function.")
(defvar nelisp-bytecode-jit--native-entry-state nil
  "Dispatch-local cons whose car is non-nil once its native entry is attempted.")

(defvar nelisp-bytecode-jit--failure-phase nil)
(defvar nelisp-bytecode-jit--failure-source nil)
(defvar nelisp-bytecode-jit--failure-artifact nil)
(defvar nelisp-bytecode-jit--failure-manifest nil)
(defvar nelisp-bytecode-jit--last-runtime-failure nil
  "Most recent caught native dispatch condition and its phase.")
(defvar nelisp-bytecode-jit--bind-trace-enabled nil)
(defvar nelisp--last-jit-bind-arity-trace nil)

(defvar nelisp-bytecode-jit--timing-active nil)
(defvar nelisp-bytecode-jit--last-stage-timings nil)
(defvar nelisp-bytecode-jit--last-expression-fingerprint nil)

(defconst nelisp-bytecode-jit--unsafe :nelisp-bytecode-jit-unsafe)

(defun nelisp-bytecode-jit--record-stage-time (stage started)
  "Accumulate elapsed microseconds for STAGE since STARTED."
  (let ((elapsed (* 1000000 (- (float-time) started))))
    (setq nelisp-bytecode-jit--last-stage-timings
          (plist-put nelisp-bytecode-jit--last-stage-timings stage
                     (+ (or (plist-get nelisp-bytecode-jit--last-stage-timings stage)
                            0)
                        (max 0 (truncate elapsed)))))))

(defmacro nelisp-bytecode-jit--with-timed-stage (stage &rest body)
  "Run BODY and accumulate opt-in elapsed microseconds for STAGE."
  (declare (indent 1) (debug t))
  `(if nelisp-bytecode-jit--timing-active
       (let ((started (float-time)))
         (unwind-protect (progn ,@body)
           (nelisp-bytecode-jit--record-stage-time ,stage started)))
     (progn ,@body)))

(defun nelisp-bytecode-jit--timing-record-start (function)
  "Reset opt-in timing data for FUNCTION's current dispatch."
  (let ((enabled (let ((directory (getenv "NELISP_JIT_DIAGNOSTIC_DIR")))
                   (and directory (not (equal directory ""))))))
    (setq nelisp-bytecode-jit--last-stage-timings
          (and enabled (list :decode-ir-us 0 :source-generation-us 0
                             :artifact-compile-us 0 :artifact-load-us 0
                             :native-call-us 0))
          nelisp-bytecode-jit--last-expression-fingerprint
          (and enabled
               (byte-code-function-p function)
               (secure-hash 'sha256
                            (prin1-to-string
                             (list (aref function 0) (aref function 1)
                                   (aref function 2) (aref function 3))))))
    enabled))

(defun nelisp-bytecode-jit--predicate-only-stream-p (code constants)
  "Return non-nil for the legacy VM-only unary predicate byte-code shape."
  (and (= (length code) 2)
       (= (aref code 1) 135)
       (memq (aref code 0) '(57 58 59 60))
       (vectorp constants)))

(defun nelisp-bytecode-jit--decode-instructions (code constants)
  "Decode CODE through the structural IR, accepting only JIT-lowerable ops.
The historical object-predicate unary shape is retained for VM fallback; it is
not eligible for native execution."
  (if (nelisp-bytecode-jit--predicate-only-stream-p code constants)
      (list (vector 0 (aref code 0) 1 nil)
            (vector 1 135 2 nil))
    (let* ((decoded (nelisp-bytecode-ir-decode-result code constants))
           (rows (plist-get decoded :instructions))
           (supported '(1 2 3 4 5 8 9 57 58 59 60 61 64 65 83 84 85 86 87 88 89 90 92 95
                         129 130 131 132 133 134 135 137 168))
           (ok (not (eq (plist-get decoded :status) 'malformed))))
      (dotimes (index (length rows))
        (let* ((row (aref rows index)) (opcode (aref row 1))
               (metadata (aref row 4)))
          (unless (and (plist-get metadata :lowerable)
                       (or (memq opcode supported)
                           (and (>= opcode 192) (<= opcode 255))))
            (setq ok nil))))
      (when (and ok
                 (nelisp-bytecode-ir-cfg-valid-p (append rows nil) (length code)))
        (mapcar (lambda (row)
                  (let ((opcode (aref row 1)) (operand (aref row 3))
                        (metadata (aref row 4)))
                    (vector (aref row 0) opcode (aref row 2)
                            (cond
                             ((and (<= 8 opcode 15)
                                   (integerp operand)
                                   (< operand (length constants)))
                              (aref constants operand))
                             ((and (eq (plist-get metadata :constant-type) 'symbol)
                                   (not (memq operand '(nil t))))
                              (list 'quote operand))
                             (t operand)))))
                (append rows nil))))))

(defun nelisp-bytecode-jit--cfg-valid-p (instructions code-length)
  "Return non-nil when all branch targets are boundaries and code is reachable."
  (nelisp-bytecode-ir-cfg-valid-p instructions code-length))

(defun nelisp-bytecode-jit--lower-flow (pc stack table code-length path)
  "Lower one acyclic control-flow path with symbolic STACK."
  (let ((instruction (cdr (assq pc table))))
    (when (and instruction (not (memq pc path)))
      (let* ((op (aref instruction 1)) (next (aref instruction 2))
             (operand (aref instruction 3)) (depth (length stack))
             (path (cons pc path)) (result nil))
        (cond
         ((and (>= op 1) (<= op 5))
          (when (< operand depth)
            (setq result (nelisp-bytecode-jit--lower-flow
                          next (cons (nth operand stack) stack) table
                          code-length path))))
         ((and (>= op 192) (<= op 255))
          (setq result (nelisp-bytecode-jit--lower-flow
                        next (cons operand stack) table code-length path)))
         ((= op 129)
          (setq result (nelisp-bytecode-jit--lower-flow
                        next (cons operand stack) table code-length path)))
         ((and (<= 8 op 15) (symbolp operand))
          (setq result (nelisp-bytecode-jit--lower-flow
                        next (cons operand stack) table code-length path)))
         ((= op 137)
          (when stack
            (setq result (nelisp-bytecode-jit--lower-flow
                          next (cons (car stack) stack) table code-length path))))
         ((memq op '(83 84))
          (when stack
            (setq result (nelisp-bytecode-jit--lower-flow
                          next (cons (list (if (= op 83) '1- '1+)
                                           (car stack))
                                     (cdr stack)) table code-length path))))
         ((memq op '(57 58 59 60))
          (when stack
            (let ((operator (cdr (assq op '((57 . symbolp) (58 . consp)
                                             (59 . stringp) (60 . listp))))))
              (setq result (nelisp-bytecode-jit--lower-flow
                            next (cons (list operator (car stack)) (cdr stack))
                            table code-length path)))))
         ((and (= op 168) stack)
          (setq result (nelisp-bytecode-jit--lower-flow
                        next (cons (list 'integerp (car stack)) (cdr stack))
                        table code-length path)))
         ((memq op '(85 86 87 88 89 90 92 95))
          (when (>= depth 2)
            (let ((operator (cdr (assq op '((85 . =) (86 . >) (87 . <)
                                             (88 . <=) (89 . >=) (90 . -)
                                             (92 . +) (95 . *))))))
              (setq result (nelisp-bytecode-jit--lower-flow
                            next (cons (list operator (nth 1 stack) (car stack))
                                       (nthcdr 2 stack)) table code-length path)))))
         ((= op 61)
          (when (>= depth 2)
            (setq result
                  (nelisp-bytecode-jit--lower-flow
                   next (cons (list 'eq (nth 1 stack) (car stack))
                              (nthcdr 2 stack))
                   table code-length path))))
         ((memq op '(64 65))
          (when stack
            (setq result
                  (nelisp-bytecode-jit--lower-flow
                   next (cons (list (if (= op 64) 'car 'cdr) (car stack))
                              (cdr stack))
                   table code-length path))))
         ((memq op '(131 132 133 134))
          (when stack
            (let* ((preserve-target (memq op '(133 134)))
                   (fallthrough (nelisp-bytecode-jit--lower-flow
                                 next (cdr stack) table code-length path))
                   (target (nelisp-bytecode-jit--lower-flow
                            operand (if preserve-target stack (cdr stack))
                            table code-length path)))
              (when (and fallthrough target)
                (setq result
                      (vector (if (memq op '(131 133))
                                  (list 'if (car stack) (aref fallthrough 0)
                                        (aref target 0))
                                (if (= op 134)
                                    (list 'if (car stack) (aref target 0)
                                          (aref fallthrough 0))
                                  (list 'if (car stack) (aref target 0)
                                        (aref fallthrough 0))))
                              (max depth (aref fallthrough 1)
                                   (aref target 1))))))))
         ((= op 130)
          (setq result (nelisp-bytecode-jit--lower-flow
                        operand stack table code-length path)))
         ((= op 135)
          (when stack (setq result (vector (car stack) depth)))))
        (when (and result (not (memq op '(131 132 133 134))))
          (aset result 1 (max depth (aref result 1))))
        result))))

(defun nelisp-bytecode-jit--linear-stack-to (pc stop stack table)
  "Symbolically execute straight-line IR from PC up to STOP with STACK."
  (let ((maximum (length stack)) (ok t))
    (while (and ok (/= pc stop))
      (let ((instruction (cdr (assq pc table))))
        (if (not instruction)
            (setq ok nil)
          (let* ((op (aref instruction 1)) (next (aref instruction 2))
                 (operand (aref instruction 3)) (depth (length stack)))
            (cond
             ((and (<= 1 op 5) (< operand depth))
              (setq stack (cons (nth operand stack) stack)))
             ((and (>= op 192) (<= op 255))
              (setq stack (cons operand stack)))
             ((and (= op 137) stack) (setq stack (cons (car stack) stack)))
             ((and (memq op '(83 84)) stack)
              (setq stack (cons (list (if (= op 83) '1- '1+)
                                      (car stack))
                                (cdr stack))))
             ((and (memq op '(85 86 87 88 89 90 92 95)) (>= depth 2))
              (setq stack
                    (cons (list (cdr (assq op '((85 . =) (86 . >) (87 . <)
                                                (88 . <=) (89 . >=) (90 . -)
                                                (92 . +) (95 . *))))
                                (nth 1 stack) (car stack))
                                (nthcdr 2 stack))))
             ((and (= op 61) (>= depth 2))
              (setq stack (cons (list 'eq (nth 1 stack) (car stack))
                                (nthcdr 2 stack))))
             ((and (= op 64) stack)
              (setq stack (cons (list 'car (car stack)) (cdr stack))))
             (t (setq ok nil)))
            (when ok
              (setq maximum (max maximum (length stack))
                    pc next))))))
    (and ok (= pc stop) (vector stack maximum))))

(defun nelisp-bytecode-jit--lower-single-loop (function instructions table)
  "Lower one stack-balanced backward loop through the common bytecode IR."
  (let* ((validation (nelisp-bytecode-ir-validate
                      (aref function 1) (aref function 2)
                      (logand (aref function 0) 255)))
         (ir-rows (plist-get validation :instructions))
         (shape (nelisp-bytecode-ir-single-backedge-loop
                 ir-rows (length (aref function 1))
                 (logand (aref function 0) 255)))
         (header (plist-get shape :header))
         (condition-pc (plist-get shape :condition))
         (body-pc (plist-get shape :body))
         (backedge-pc (plist-get shape :backedge))
         (exit-pc (plist-get shape :exit)))
    (when shape
      (let* ((arguments (cl-loop for index below (logand (aref function 0) 255)
                                 collect (intern (format "x%d" index))))
             (initial (nelisp-bytecode-jit--linear-stack-to
                       0 header (reverse arguments) table))
             (initial-stack (and initial (aref initial 0)))
             (loop-vars (cl-loop for index below (length initial-stack)
                                 collect (intern (format "nelisp-bc-loop-s%d" index))))
             (test (and initial-stack
                        (nelisp-bytecode-jit--linear-stack-to
                         header condition-pc loop-vars table)))
             (test-stack (and test (aref test 0)))
             (loop-condition (car test-stack))
             (branch-stack (cdr test-stack))
             (continue-condition
              (and shape (if (plist-get shape :continue-on-true)
                             loop-condition
                           (list 'if loop-condition nil t))))
             (body (and (equal branch-stack loop-vars)
                        (nelisp-bytecode-jit--linear-stack-to
                         body-pc backedge-pc branch-stack table)))
             (body-stack (and body (aref body 0)))
             (updates nil))
        (when (and initial test (consp test-stack)
                   (= (length branch-stack) (length loop-vars))
                   body (= (length body-stack) (length loop-vars)))
          (cl-loop for variable in loop-vars
                   for value in body-stack
                   unless (equal variable value)
                   do (push (cons variable value) updates))
          (setq updates (nreverse updates))
          (when updates
            (let* ((branch-row (cdr (assq condition-pc table)))
                   (exit-state
                    (nelisp-bytecode-jit--lower-flow
                     exit-pc branch-stack table (length (aref function 1)) nil))
                   (exit-expression (and exit-state (aref exit-state 0)))
                   (initial-bindings
                    (cl-loop for variable in loop-vars
                             for value in initial-stack
                             collect (list variable value)))
                   (temps
                    (cl-loop for index from 0 below (length updates)
                             collect (intern (format "nelisp-bc-loop-next%d" index))))
                   (update-bindings
                    (cl-loop for temporary in temps
                             for update in updates
                             collect (list temporary (cdr update))))
                   (assignments
                    (cl-loop for update in updates
                             for temporary in temps
                             append (list (car update) temporary)))
                   (body-form (list 'let update-bindings
                                    (cons 'setq assignments)))
                   (expression
                    (list 'let initial-bindings
                          (list 'while continue-condition body-form)
                          exit-expression))
                   (max-depth (plist-get (plist-get validation :stack-analysis)
                                         :max-depth)))
              (when (and exit-expression
                         (= (aref function 3) max-depth))
                (list :arity (length arguments) :expression expression
                      :max-depth (aref function 3) :instructions instructions
                      :loop (list :header header :condition condition-pc
                                  :body body-pc :backedge backedge-pc
                                  :exit exit-pc :branch-op (aref branch-row 1)))))))))))

(defun nelisp-bytecode-jit--decode-ir (function)
  "Return bounded symbolic IR for FUNCTION, or nil for unsupported code."
  (when (and (byte-code-function-p function)
             (or (= (length function) 4) (= (length function) 5))
             (integerp (aref function 0)) (integerp (aref function 3))
             (stringp (aref function 1)) (vectorp (aref function 2)))
    (let* ((descriptor (aref function 0))
           (arity (logand descriptor 255))
           (maximum (ash descriptor -8))
           (code (aref function 1))
           (instructions (and (<= (length code) 64)
                              (<= arity 4) (= arity maximum)
                              (nelisp-bytecode-jit--decode-instructions
                               code (aref function 2))))
           (table nil))
      (when (and instructions
                 (nelisp-bytecode-jit--cfg-valid-p instructions (length code)))
        (dolist (instruction instructions)
          (push (cons (aref instruction 0) instruction) table))
        (let* ((arguments (cl-loop for index below arity
                                   collect (intern (format "x%d" index))))
               (depth-first (nelisp-bytecode-jit--lower-flow
                             0 (reverse arguments) table (length code) nil))
               (loop-ir (and (not depth-first)
                             (nelisp-bytecode-jit--lower-single-loop
                              function instructions table))))
          (or (and depth-first (= (aref function 3)
                                  (max (if (= arity 0) 1 2)
                                       (aref depth-first 1)))
                   (list :arity arity :expression (aref depth-first 0)
                         :max-depth (aref depth-first 1)
                         :instructions instructions))
              loop-ir))))))

(defun nelisp-bytecode-jit--decode-boxed-predicate (function)
  "Return boxed-predicate IR for one structurally valid unary FUNCTION.

The structural IR deliberately marks object predicate opcodes as unsupported
for the raw integer ABI.  This path admits only the four fixed unary object
predicates, followed immediately by RETURN, after target and stack validation.
No source form or predicate alias is inspected."
  (when (and (byte-code-function-p function)
             (or (= (length function) 4) (= (length function) 5))
             (eql (aref function 0) 257)
             (stringp (aref function 1))
             (vectorp (aref function 2))
             (= (length (aref function 2)) 0)
             (eql (aref function 3) 2))
    (let* ((code (aref function 1))
           (result (nelisp-bytecode-ir-validate code (aref function 2) 1))
           (rows (plist-get result :instructions))
           (stack (plist-get result :stack-analysis)))
      (when (and (not (eq (plist-get result :status) 'malformed))
                 (eq (plist-get stack :status) 'complete)
                 (= (plist-get stack :max-depth) 1)
                 (= (length code) 2)
                 (= (length rows) 2)
                 (= (aref (aref rows 0) 0) 0)
                 (= (aref (aref rows 0) 2) 1)
                 (memq (aref (aref rows 0) 1) '(57 58 59 60))
                 (= (aref (aref rows 1) 0) 1)
                 (= (aref (aref rows 1) 1) 135)
                 (= (aref (aref rows 1) 2) 2))
        (list :arity 1
              :opcode (aref (aref rows 0) 1)
              :predicate (cdr (assq (aref (aref rows 0) 1)
                                    '((57 . symbolp) (58 . consp)
                                      (59 . stringp) (60 . listp))))
              :instructions rows)))))

(defun nelisp-bytecode-jit--decode-unary (function)
  "Return the legacy unary classifier when FUNCTION lowers to unary IR."
  (let ((expression (plist-get (nelisp-bytecode-jit--decode-ir function)
                               :expression)))
    (cond ((equal expression '(1+ x0)) 'add1)
          ((equal expression '(1- x0)) 'sub1))))

(defun nelisp-bytecode-jit--decode-binary-add (function)
  "Classify two-argument addition after generic byte-code lowering."
  (let ((ir (nelisp-bytecode-jit--decode-ir function)))
    (and (= (or (plist-get ir :arity) 0) 2)
         (equal (plist-get ir :expression) '(+ x0 x1)) 'add)))

(defun nelisp-bytecode-jit--safe-binary-operands-p (left right)
  "Return non-nil when LEFT + RIGHT and both inputs are interior fixnums."
  (and (fixnump left) (fixnump right)
       (< most-negative-fixnum left) (< left most-positive-fixnum)
       (< most-negative-fixnum right) (< right most-positive-fixnum)
       (let ((sum (+ left right)))
         (and (fixnump sum)
              (< most-negative-fixnum sum) (< sum most-positive-fixnum)))))

(defun nelisp-bytecode-jit--safe-fixnum-operand-p (operation argument)
  "Return non-nil when unary OPERATION stays within the fixnum range."
  (and (fixnump argument)
       (if (eq operation 'add1)
           (< argument most-positive-fixnum)
         (> argument most-negative-fixnum))))

(defun nelisp-bytecode-jit--decode-branch (function)
  "Decode Emacs 31.1's exact `(if (= x 0) THEN ELSE)' byte-code shape.

Return `(branch-eq-zero THEN ELSE)' when FUNCTION has one argument, the
compiler's exact conditional stream, and fixnum result constants.  The stream
uses the compiler's local-argument, integer-zero comparison, branch, and
constant-return opcodes only.  It has no user-visible side effects."
  (when (and (byte-code-function-p function)
             (or (equal (aref function 0) '(x))
                 (equal (aref function 0) 257))
             (or (= (length function) 4) (= (length function) 5))
             (stringp (aref function 1))
             (equal (string-to-list (aref function 1))
             '(137 192 85 131 8 0 193 135 194 135))
             (vectorp (aref function 2))
             (= (length (aref function 2)) 3)
             (eql (aref (aref function 2) 0) 0)
             (= (aref function 3) 3)
             (fixnump (aref (aref function 2) 1))
             (fixnump (aref (aref function 2) 2)))
    (list 'branch-eq-zero
          (aref (aref function 2) 1)
          (aref (aref function 2) 2))))

(defun nelisp-bytecode-jit--ir-safe-form-p (form environment &optional boxed-values)
  "Evaluate the restricted arithmetic FORM; return (t . VALUE) if safe."
  (cond
   ((integerp form)
    (and (fixnump form) (< most-negative-fixnum form)
         (< form most-positive-fixnum) (cons t form)))
   ((symbolp form)
    (let* ((binding (assq form environment))
           (global (and (not binding) (boundp form)))
           (value (if binding (cdr binding)
                    (and global (symbol-value form))))
           (safe (if binding
                     (if boxed-values
                         (nelisp-bytecode-jit--boxed-argument-supported-p value)
                       (and (fixnump value) (< most-negative-fixnum value)
                            (< value most-positive-fixnum)))
                   (and global (fixnump value)))))
      (and safe (cons t value))))
   ((and (consp form) (eq (car form) 'quote) (= (length form) 2))
    (let ((value (cadr form)))
      (and (or (null value) (eq value t)
               (and (symbolp value)
                    (eq value (intern-soft (symbol-name value))))
               (and (fixnump value) (< most-negative-fixnum value)
                    (< value most-positive-fixnum)))
           (cons t value))))
   ((and (consp form) (memq (car form) '(consp symbolp stringp listp))
         (= (length form) 2))
    (let ((value (nelisp-bytecode-jit--ir-safe-form-p
                  (cadr form) environment boxed-values)))
      (and value
           (cons t (pcase (car form)
                     ('consp (consp (cdr value)))
                     ('symbolp (symbolp (cdr value)))
                     ('stringp (stringp (cdr value)))
                     ('listp (listp (cdr value))))))))
   ((and (consp form) (eq (car form) 'eq) (= (length form) 3))
    (let ((left (nelisp-bytecode-jit--ir-safe-form-p
                 (nth 1 form) environment boxed-values))
          (right (nelisp-bytecode-jit--ir-safe-form-p
                  (nth 2 form) environment boxed-values)))
      (and left right (cons t (eq (cdr left) (cdr right))))))
   ((and (consp form) (memq (car form) '(car cdr)) (= (length form) 2))
    (let ((value (nelisp-bytecode-jit--ir-safe-form-p
                  (cadr form) environment boxed-values)))
      (and value
           (cond ((null (cdr value)) (cons t nil))
                 ((consp (cdr value))
                  (cons t (if (eq (car form) 'car)
                              (car (cdr value)) (cdr (cdr value)))))
                 (t nil)))))
   ((and (consp form) (eq (car form) 'integerp) (= (length form) 2))
    (let ((value (nelisp-bytecode-jit--ir-safe-form-p
                  (cadr form) environment boxed-values)))
      (and value (cons t (integerp (cdr value))))))
   ((and (consp form) (eq (car form) 'if))
    (let ((condition (nelisp-bytecode-jit--ir-safe-form-p
                      (nth 1 form) environment boxed-values)))
      (and condition
           (nelisp-bytecode-jit--ir-safe-form-p
            (if (cdr condition) (nth 2 form) (nth 3 form))
            environment boxed-values))))
   ((consp form)
    (let* ((operator (car form))
           (evaluated (mapcar (lambda (part)
                                (nelisp-bytecode-jit--ir-safe-form-p
                                 part environment boxed-values))
                              (cdr form))))
      (when (and (memq operator '(+ - * = < > <= >= 1+ 1-))
                 (cl-every #'identity evaluated))
        (let* ((args (mapcar #'cdr evaluated))
               (numeric (cl-every #'fixnump args))
               (value (pcase operator
                        ('+ (+ (car args) (cadr args)))
                        ('- (- (car args) (cadr args)))
                        ('* (* (car args) (cadr args)))
                        ('= (= (car args) (cadr args)))
                        ('< (< (car args) (cadr args)))
                        ('> (> (car args) (cadr args)))
                        ('<= (<= (car args) (cadr args)))
                        ('>= (>= (car args) (cadr args)))
                        ('1+ (1+ (car args)))
                        ('1- (1- (car args))))))
          (when numeric
            (if (memq operator '(= < > <= >=))
                (cons t value)
              (and (fixnump value) (< most-negative-fixnum value)
                   (< value most-positive-fixnum) (cons t value))))))))
   (t nil)))

(defun nelisp-bytecode-jit--safe-ir-p (ir arguments)
  "Guard all values and arithmetic nodes represented by IR."
  (or (and (= (or (plist-get ir :arity) -1) 0)
           (null arguments)
           (null (plist-get ir :expression))
           (nelisp-bytecode-jit--boxed-value-supported-p
            (plist-get ir :expression) (list 1) 0))
      (let ((environment
         (cl-loop for index below (plist-get ir :arity)
                  collect (cons (intern (format "x%d" index))
                                (nth index arguments)))))
       (and (= (length arguments) (plist-get ir :arity))
             (or (and (nelisp-bytecode-jit--boxed-integerp-ir-p ir)
                  (= (length arguments) 1)
                  (nelisp-bytecode-jit--boxed-argument-supported-p
                   (car arguments))
                  (nelisp-bytecode-jit--ir-safe-form-p
                   (plist-get ir :expression) environment t))
             (and (nelisp-bytecode-jit--boxed-object-boolean-ir-p ir)
                  (= (length arguments) 1)
                  (nelisp-bytecode-jit--boxed-argument-supported-p
                   (car arguments))
                  (nelisp-bytecode-jit--ir-safe-form-p
                   (plist-get ir :expression) environment t))
             (and (nelisp-bytecode-jit--boxed-eq-ir-p ir)
                  (nelisp-bytecode-jit--eq-native-argument-supported-p
                   (car arguments))
                  (nelisp-bytecode-jit--eq-native-argument-supported-p
                   (cadr arguments)))
             (nelisp-bytecode-jit--boxed-projection-call-supported-p
              ir arguments)
             (nelisp-bytecode-jit--safe-counted-loop-p
              (plist-get ir :expression) environment)
             (nelisp-bytecode-jit--ir-safe-form-p
              (plist-get ir :expression) environment))))))

(defun nelisp-bytecode-jit--safe-counted-loop-p (form environment)
  "Prove the restricted zero-to-X0 increment loop stays within fixnums."
  (when (and (consp form) (eq (car form) 'let)
             (= (length form) 4) (listp (cadr form))
             (= (length (cadr form)) 2)
             (consp (nth 2 form)) (eq (car (nth 2 form)) 'while)
             (= (length (nth 2 form)) 3))
    (let* ((bindings (cadr form))
           (zero-binding (cl-find-if (lambda (binding)
                                       (and (consp binding) (= (length binding) 2)
                                            (eql (cadr binding) 0)))
                                     bindings))
           (limit-binding (cl-find-if (lambda (binding)
                                        (and (consp binding) (= (length binding) 2)
                                             (eq (cadr binding) 'x0)))
                                      bindings))
           (variable (car-safe zero-binding))
           (limit (car-safe limit-binding))
           (condition (nth 1 (nth 2 form)))
           (body (nth 2 (nth 2 form)))
           (result (nth 3 form))
           (updates (and (consp body) (eq (car body) 'let)
                         (= (length body) 3) (cadr body)
                         (= (length (cadr body)) 1)
                         (car (cadr body))))
           (update-name (car-safe updates))
           (update-value (cadr updates))
           (assignment (nth 2 body))
           (environment-value (cdr (assq 'x0 environment))))
      (and variable limit (not (eq variable limit))
           (memq (car-safe condition) '(< <=))
           (equal (cdr condition) (list variable limit))
           (equal result variable)
           (equal assignment (list 'setq variable update-name))
           (or (equal update-value (list '1+ variable))
               (equal update-value (list '+ variable 1)))
           (fixnump environment-value)
           (< most-negative-fixnum environment-value)
           (< environment-value most-positive-fixnum)))))

(defun nelisp-bytecode-jit--aot-form (form)
  "Normalize the restricted IR FORM for the raw AOT compiler."
  (if (consp form)
      (let ((operator (car form))
            (arguments (mapcar #'nelisp-bytecode-jit--aot-form (cdr form))))
        (cond ((memq operator '(let let*))
               (cons operator
                     (cons (mapcar (lambda (binding)
                                     (if (and (consp binding)
                                              (= (length binding) 2))
                                         (list (car binding)
                                               (nelisp-bytecode-jit--aot-form
                                                (cadr binding)))
                                       binding))
                                   (cadr form))
                           (mapcar #'nelisp-bytecode-jit--aot-form
                                   (cddr form)))))
              ((eq operator 'setq)
               (let ((tail (cdr form)) (result nil))
                 (while tail
                   (push (pop tail) result)
                   (when tail
                     (push (nelisp-bytecode-jit--aot-form (pop tail)) result)))
                 (cons 'setq (nreverse result))))
              ((eq operator '1+) (list '+ (car arguments) 1))
              ((eq operator '1-) (list '- (car arguments) 1))
              (t (cons operator arguments))))
    form))

(defun nelisp-bytecode-jit--nil-result-ir-p (ir)
  "Return non-nil when IR is the generic zero-argument nil constant shape."
  (and (= (or (plist-get ir :arity) -1) 0)
       (null (plist-get ir :expression))))

(defun nelisp-bytecode-jit--boolean-result-ir-p (ir)
  "Return non-nil when IR has a comparison operator as its result."
  (let ((expression (plist-get ir :expression)))
    (and (consp expression)
         (memq (car expression) '(= < > <= >=)))))

(defun nelisp-bytecode-jit--boxed-eq-ir-p (ir)
  "Return non-nil for the exact two-argument identity expression."
  (and (= (or (plist-get ir :arity) -1) 2)
       (equal (plist-get ir :expression) '(eq x0 x1))))

(defun nelisp-bytecode-jit--contains-integerp-p (form)
  "Return non-nil when FORM contains the structural `integerp' operation."
  (and (consp form)
       (or (eq (car form) 'integerp)
           (cl-some #'nelisp-bytecode-jit--contains-integerp-p (cdr form)))))

(defun nelisp-bytecode-jit--boolean-value-form-p (form)
  "Return non-nil when FORM has only Lisp-boolean result leaves."
  (cond
   ((memq form '(nil t)) t)
   ((and (consp form) (eq (car form) 'if) (= (length form) 4))
    (and (nelisp-bytecode-jit--boolean-value-form-p (nth 2 form))
         (nelisp-bytecode-jit--boolean-value-form-p (nth 3 form))))
   ((and (consp form) (= (length form) 2)
         (memq (car form) '(integerp symbolp consp stringp listp))) t)
   ((and (consp form) (= (length form) 3)
         (memq (car form) '(= < > <= >= eq))) t)
   (t nil)))

(defun nelisp-bytecode-jit--box-boolean-return-form (form)
  "Wrap predicate leaves in FORM so native returns are canonical Lisp booleans."
  (cond
   ((and (consp form) (eq (car form) 'if) (= (length form) 4))
    (list 'if (cadr form)
          (nelisp-bytecode-jit--box-boolean-return-form (nth 2 form))
          (nelisp-bytecode-jit--box-boolean-return-form (nth 3 form))))
   ((and (consp form)
         (or (and (= (length form) 2)
                  (memq (car form) '(integerp symbolp consp stringp listp)))
             (and (= (length form) 3)
                  (memq (car form) '(= < > <= >= eq)))))
    (list 'if form t nil))
   (t form)))

(defun nelisp-bytecode-jit--boxed-integerp-ir-p (ir)
  "Return non-nil when unary IR uses boxed `integerp' and returns a Lisp value."
  (and (= (or (plist-get ir :arity) -1) 1)
       (nelisp-bytecode-jit--boolean-value-form-p
        (plist-get ir :expression))
       (nelisp-bytecode-jit--contains-integerp-p (plist-get ir :expression))))

(defun nelisp-bytecode-jit--boxed-car-chain-depth (ir)
  "Return the supported unary CAR-chain depth in IR, or nil.
Only one or two CAR operations over x0 are currently admitted."
  (when (= (or (plist-get ir :arity) -1) 1)
    (let ((expression (plist-get ir :expression))
          (depth 0))
      (while (and (consp expression) (eq (car expression) 'car)
                  (= (length expression) 2))
        (setq depth (1+ depth)
              expression (cadr expression)))
      (and (memq depth '(1 2)) (eq expression 'x0) depth))))

(defun nelisp-bytecode-jit--boxed-projection-ops (ir)
  "Return the one- or two-step CAR/CDR path for unary IR, or nil."
  (when (= (or (plist-get ir :arity) -1) 1)
    (let ((expression (plist-get ir :expression))
          (ops nil))
      (while (and (consp expression)
                  (memq (car expression) '(car cdr))
                  (= (length expression) 2)
                  (< (length ops) 2))
        (push (car expression) ops)
        (setq expression (cadr expression)))
      (and ops (eq expression 'x0) ops))))

(defun nelisp-bytecode-jit--boxed-car-call-supported-p (ir arguments)
  "Return non-nil when boxed CAR evaluation and result decoding are safe.
Invalid CAR operands stay on the VM path for its original signal semantics.
Cons and string results are admitted through the pinned reference bridge."
  (let ((depth (nelisp-bytecode-jit--boxed-car-chain-depth ir))
        (value (car arguments))
        (safe (= (length arguments) 1)))
    (while (and safe (integerp depth) (> depth 0))
      (if (null value)
          (setq value nil)
        (if (consp value)
            (setq value (car value))
          (setq safe nil)))
      (setq depth (1- depth)))
    (and (integerp (nelisp-bytecode-jit--boxed-car-chain-depth ir))
         safe
         (or (consp value) (stringp value)
             (nelisp-bytecode-jit--boxed-argument-supported-p value)))))

(defun nelisp-bytecode-jit--boxed-projection-call-supported-p (ir arguments)
  "Return non-nil when a boxed CAR/CDR chain is safe for native execution."
  (let ((ops (nelisp-bytecode-jit--boxed-projection-ops ir))
        (value (car arguments))
        (safe (and (= (length arguments) 1)
                   (nelisp-bytecode-jit--boxed-argument-supported-p
                    (car arguments) t))))
    (dolist (op ops)
      (when safe
        (cond
         ((null value) nil)
         ((consp value) (setq value (if (eq op 'car) (car value) (cdr value))))
         (t (setq safe nil)))))
    (and ops safe
         (nelisp-bytecode-jit--boxed-argument-supported-p value t))))

(defun nelisp-bytecode-jit--boxed-result-ir-p (ir)
  "Return non-nil when IR uses the boxed native result ABI."
  (or (nelisp-bytecode-jit--nil-result-ir-p ir)
      (nelisp-bytecode-jit--boxed-eq-ir-p ir)
      (nelisp-bytecode-jit--boxed-integerp-ir-p ir)
      (nelisp-bytecode-jit--boxed-object-boolean-ir-p ir)
      (nelisp-bytecode-jit--boxed-projection-ops ir)))

(defun nelisp-bytecode-jit--contains-object-predicate-p (form)
  "Return non-nil when FORM contains one of the four boxed predicates."
  (and (consp form)
       (or (memq (car form) '(symbolp consp stringp listp))
           (cl-some #'nelisp-bytecode-jit--contains-object-predicate-p
                    (cdr form)))))

(defun nelisp-bytecode-jit--boxed-object-boolean-ir-p (ir)
  "Return non-nil for boolean IR containing a boxed object predicate."
  (let ((expression (plist-get ir :expression)))
    (and (= (or (plist-get ir :arity) -1) 1)
         (nelisp-bytecode-jit--boolean-value-form-p expression)
         (nelisp-bytecode-jit--contains-object-predicate-p expression))))

(defun nelisp-bytecode-jit--runtime-identity ()
  "Return current ABI tags and the memoized native binary identity."
  (when (eq nelisp-bytecode-jit--runtime-identity-cache :unset)
    (setq nelisp-bytecode-jit--runtime-identity-cache
          (if (fboundp 'nelisp-native-load--running-binary-sha256)
              (condition-case nil
                  (nelisp-native-load--running-binary-sha256)
                (error :unavailable))
            :unavailable)))
  (list :cache-abi nelisp-bytecode-jit--cache-abi
        :raw-runtime-abi
        (or (and (boundp 'nelisp-native-load-raw-runtime-abi-v2)
                 nelisp-native-load-raw-runtime-abi-v2)
            (and (boundp 'nelisp-native-load-raw-runtime-abi)
                 nelisp-native-load-raw-runtime-abi)
            :unknown)
        :binary-sha256 nelisp-bytecode-jit--runtime-identity-cache))

(defun nelisp-bytecode-jit--function-payload-fingerprint (function)
  "Return a fingerprint of FUNCTION's complete byte-code payload."
  (when (and (byte-code-function-p function)
             (or (= (length function) 4) (= (length function) 5)))
    (condition-case nil
        (secure-hash 'sha256
                     (prin1-to-string
                      (list (aref function 0) (aref function 1)
                            (aref function 2) (aref function 3))))
      (error nil))))

(defun nelisp-bytecode-jit--cache-identity (function)
  "Return FUNCTION's payload and runtime identity pair."
  (let ((fingerprint
         (nelisp-bytecode-jit--function-payload-fingerprint function)))
    (and (stringp fingerprint)
         (list fingerprint (nelisp-bytecode-jit--runtime-identity)))))

(defun nelisp-bytecode-jit--ready-handle (function ir identity)
  "Return a published handle matching FUNCTION, IR, and IDENTITY."
  (let ((record (gethash function nelisp-bytecode-jit--handles)))
    (if (and (listp record)
             (eq (plist-get record :state) 'ready)
             (equal (plist-get record :identity) identity)
             (equal (plist-get record :ir) ir))
        (plist-get record :handle)
      (when record
        (remhash function nelisp-bytecode-jit--handles)
        (nelisp-bytecode-jit--retire-handle (plist-get record :handle)))
      nil)))

(defun nelisp-bytecode-jit--publish-handle (function ir identity handle)
  "Publish validated HANDLE for FUNCTION, IR, and IDENTITY."
  (puthash function (list :state 'ready :identity identity :ir ir :handle handle)
           nelisp-bytecode-jit--handles)
  (remhash function nelisp-bytecode-jit--compile-failures)
  handle)

(defun nelisp-bytecode-jit--validated-handle-p (ir identity handle)
  "Return non-nil when HANDLE metadata agrees with IR and IDENTITY."
  (let ((arity (plist-get handle :arity))
        (boxed (nelisp-bytecode-jit--boxed-result-ir-p ir)))
    (and (consp identity)
         (stringp (car identity))
         (consp (cadr identity))
         (equal (plist-get (cadr identity) :cache-abi)
                nelisp-bytecode-jit--cache-abi)
         (integerp arity)
         (if (eq (plist-get handle :backend) 'gnu-eln-native-subr)
             (and (= arity 0) (= (plist-get ir :arity) 0)
                  (nelisp-bytecode-jit--gnu-eln-literal-p ir)
                  (subrp (plist-get handle :callable))
                  (plist-get handle :module)
                  (nelisp-eln-emitter--profile-p
                   (plist-get handle :producer-profile))
                  (equal (plist-get handle :cache-identity) identity))
            (and (= arity (plist-get ir :arity))
                 (if boxed
                     (if (nelisp-bytecode-jit--boxed-integerp-ir-p ir)
                         (and (eq (plist-get handle :abi) 'integer)
                              (eq (plist-get handle :param-repr) 'sexp-ptr)
                              (eq (plist-get handle :return-repr) 'raw-bool))
                       (and (memq (plist-get handle :abi) '(boxed integer))
                            (eq (plist-get handle :param-repr) 'sexp-ptr)
                            (memq (plist-get handle :return-repr)
                                  '(sexp-ptr raw-bool unknown))))
                   (and (eq (plist-get handle :abi) 'integer)
                        (memq (plist-get handle :param-repr) '(raw-i64 unknown))
                        (memq (plist-get handle :return-repr)
                              '(raw-i64 unknown)))))))))

(defun nelisp-bytecode-jit--compile-ir-private (function ir)
  "Compile FUNCTION's generic IR through its raw or boxed AOT compiler."
  (let* ((identity (nelisp-bytecode-jit--cache-identity function))
         (cached (nelisp-bytecode-jit--ready-handle function ir identity)))
    (if cached
        cached
      (let* ((boxed (nelisp-bytecode-jit--boxed-result-ir-p ir))
             (boxed-eq (nelisp-bytecode-jit--boxed-eq-ir-p ir))
             (boxed-integerp (nelisp-bytecode-jit--boxed-integerp-ir-p ir))
             (boxed-projection (nelisp-bytecode-jit--boxed-projection-ops ir))
             (diagnostic-dir (getenv "NELISP_JIT_DIAGNOSTIC_DIR"))
             (diagnostic-dir
              (and diagnostic-dir (not (equal diagnostic-dir ""))
                   (file-name-as-directory (expand-file-name diagnostic-dir))))
             (_ (when diagnostic-dir (make-directory diagnostic-dir t)))
             (prefix (if diagnostic-dir
                         (expand-file-name "jit-ir-" diagnostic-dir)
                       "nelisp-bc-jit-ir-"))
             (source (make-temp-file prefix nil ".el"))
             (artifact (make-temp-file prefix nil (if boxed ".neln" ".nelr")))
             (manifest (and boxed (concat artifact ".manifest.el")))
             (arguments (cl-loop for index below (plist-get ir :arity)
                                 collect (intern (format "x%d" index))))
             (compile-complete nil)
             handle)
        (setq nelisp-bytecode-jit--failure-phase 'source-generation
              nelisp-bytecode-jit--failure-source source
              nelisp-bytecode-jit--failure-artifact artifact
              nelisp-bytecode-jit--failure-manifest manifest)
        (unwind-protect
            (progn
              (nelisp-bytecode-jit--with-timed-stage :source-generation-us
                (with-temp-file source
                  (insert ";;; -*- lexical-binding: t; -*-\n")
                  (insert (format "(defun nl_bc_jit_entry (%s) %s)\n"
                                  (mapconcat #'symbol-name arguments " ")
                                  (prin1-to-string
                                   (nelisp-bytecode-jit--aot-form
                                    (if boxed-integerp
                                        (nelisp-bytecode-jit--box-boolean-return-form
                                         (plist-get ir :expression))
                                      (plist-get ir :expression))))))))
              (setq nelisp-bytecode-jit--failure-phase 'artifact-compile)
              (if boxed
                (progn
                    (nelisp-bytecode-jit--with-timed-stage :artifact-compile-us
                      (unless (fboundp 'nelisp-artifact-compile-file)
                        (require 'nelisp-artifact))
                      (let* ((trace-dir (getenv "NELISP_JIT_BIND_TRACE_DIR"))
                             (nelisp-bytecode-jit--bind-trace-enabled
                              (and trace-dir (not (equal trace-dir "")) t)))
                        (when nelisp-bytecode-jit--bind-trace-enabled
                          (setq nelisp--last-jit-bind-arity-trace nil))
                        (nelisp-artifact-compile-file
                         source artifact nil nil nil nil nil 'neln)))
                    (setq nelisp-bytecode-jit--failure-phase 'artifact-load)
                    (setq handle
                          (nelisp-bytecode-jit--with-timed-stage :artifact-load-us
                            (nelisp-native-load-artifact
                             artifact "nl_bc_jit_entry")))
                    (when (or boxed-eq boxed-integerp boxed-projection
                              (nelisp-bytecode-jit--boxed-object-boolean-ir-p ir))
                      (unless (and (= (plist-get handle :arity)
                                     (plist-get ir :arity))
                                   (eq (plist-get handle :param-repr) 'sexp-ptr)
                                   (or (eq (plist-get handle :return-repr) 'sexp-ptr)
                                       (and (nelisp-bytecode-jit--boxed-object-boolean-ir-p ir)
                                            (eq (plist-get handle :abi) 'boxed)
                                            (memq (plist-get handle :return-repr)
                                                  '(raw-bool unknown)))
                                       (and boxed-integerp
                                            (eq (plist-get handle :return-repr)
                                                'raw-bool))))
                        (error "nelisp-bytecode-jit: boxed object representation mismatch: %S"
                               (list (plist-get handle :arity)
                                     (plist-get handle :param-repr)
                                     (plist-get handle :return-repr))))))
                (nelisp-bytecode-jit--with-timed-stage :artifact-compile-us
                  (nelisp-native-load-raw-compile-file
                   source artifact nil "p5-bytecode-generic-ir"))
                (setq nelisp-bytecode-jit--failure-phase 'artifact-load)
                (setq handle
                      (nelisp-bytecode-jit--with-timed-stage :artifact-load-us
                        (nelisp-native-load-raw-artifact
                         artifact "nl_bc_jit_entry"))))
              (setq nelisp-bytecode-jit--compile-count
                    (1+ nelisp-bytecode-jit--compile-count)
                    compile-complete t))
          (unless (and diagnostic-dir (not compile-complete))
            (when (file-exists-p source) (delete-file source))
            (when (file-exists-p artifact) (delete-file artifact))
            (when (and manifest (file-exists-p manifest))
              (delete-file manifest))))
        (if nelisp-bytecode-jit--preparing-active
            handle
          (unless (and (equal identity
                              (nelisp-bytecode-jit--cache-identity function))
                       (nelisp-bytecode-jit--validated-handle-p
                        ir identity handle))
            (error "nelisp-bytecode-jit: native handle identity mismatch"))
          (nelisp-bytecode-jit--publish-handle function ir identity handle))))))

(defun nelisp-bytecode-jit--gnu-eln-literal-p (ir)
  "Return non-nil for the first pinned GNU ELN-compatible IR subset."
  (let ((value (plist-get ir :expression)))
    (and (= (plist-get ir :arity) 0)
         (integerp value) (<= 0 value) (<= value #x1fffffff))))

(defun nelisp-bytecode-jit--retry-eln-closes ()
  "Retry closing retired GNU ELN modules; keep any live leases queued."
  (let ((pending nelisp-bytecode-jit--pending-eln-closes)
        (remaining nil))
    (setq nelisp-bytecode-jit--pending-eln-closes nil)
    (dolist (entry pending)
      (condition-case nil
          (progn
            (when (and (plist-get entry :module)
                       (not (plist-get entry :closed)))
              (nelisp-eln-system-loader-close (plist-get entry :module))
              (setq entry (plist-put entry :closed t)))
            (let ((artifact (plist-get entry :artifact))
                  (directory (plist-get entry :directory)))
              (when (and artifact (file-exists-p artifact))
                (delete-file artifact))
              (when (and directory (file-directory-p directory))
                (delete-directory directory))))
        (error (push entry remaining))))
    (setq nelisp-bytecode-jit--pending-eln-closes (nreverse remaining))
    (length nelisp-bytecode-jit--pending-eln-closes)))

(defun nelisp-bytecode-jit--retire-handle (handle)
  "Queue HANDLE's GNU module for close after its callable lease is gone."
  (when (eq (plist-get handle :backend) 'gnu-eln-native-subr)
    (let ((module (plist-get handle :module))
          (artifact (plist-get handle :artifact))
          (directory (plist-get handle :artifact-directory)))
      (when (and (or module artifact directory)
                 (not (cl-some (lambda (entry)
                                 (if module
                                     (eq (plist-get entry :module) module)
                                   (and (null (plist-get entry :module))
                                        (equal (plist-get entry :artifact)
                                               artifact))))
                               nelisp-bytecode-jit--pending-eln-closes)))
        (push (list :module module :artifact artifact :directory directory
                    :closed (null module))
              nelisp-bytecode-jit--pending-eln-closes))))
  nil)

(defun nelisp-bytecode-jit--compile-eln-ir (function ir identity)
  "Emit IR as a GNU .eln leaf and prepare its managed callable."
  (require 'nelisp-eln-emitter)
  (require 'nelisp-eln-system-loader)
  (require 'nelisp-eln-native-subr)
  (let* ((entry-name
          (intern (format "nelisp-bytecode-jit-entry-%d"
                          (setq nelisp-bytecode-jit--eln-entry-counter
                                (1+ nelisp-bytecode-jit--eln-entry-counter)))))
         (aot-ir
          (nelisp-aot-compiler--parse-stmt
           (list 'defun entry-name nil (plist-get ir :expression)) nil nil nil))
         (profile nelisp-eln-emitter-gnu31-profile)
         (dir nil)
         (artifact nil)
         (module nil)
         (callable nil)
         (handle nil)
         (keep-artifact nil))
    (unwind-protect
        (progn
          (setq dir (make-temp-file "nelisp-bytecode-jit-eln-" t)
                artifact (expand-file-name "unit.eln" dir))
          (setq nelisp-bytecode-jit--failure-phase 'gnu-eln-emission)
          (nelisp-eln-emitter-write-ir aot-ir artifact profile)
          (setq nelisp-bytecode-jit--failure-phase 'gnu-eln-load)
          (setq module (nelisp-eln-system-loader-open artifact))
          (setq nelisp-bytecode-jit--failure-phase 'gnu-eln-subr)
          (let* ((root-name (nelisp-eln-emitter--symbol-name entry-name))
                 (capability
                  (nelisp-eln-system-loader-function-capability module root-name)))
            (setq callable (nelisp-eln-native-subr-create module root-name)
                  handle (list :backend 'gnu-eln-native-subr
                               :callable callable :module module
                               :entry (nth 3 capability)
                               :symbol root-name :arity 0
                               :abi 'gnu-lisp-object :param-repr 'none
                               :return-repr 'gnu-fixnum
                               :producer-profile profile
                               :cache-identity identity
                               :artifact artifact :artifact-directory dir)))
          (setq keep-artifact t)
          (setq nelisp-bytecode-jit--compile-count
                (1+ nelisp-bytecode-jit--compile-count))
          handle)
      (unless keep-artifact
        (if module
            (nelisp-bytecode-jit--retire-handle
             (list :backend 'gnu-eln-native-subr :module module
                   :artifact artifact :artifact-directory dir))
          (nelisp-bytecode-jit--retire-handle
           (list :backend 'gnu-eln-native-subr :artifact artifact
                 :artifact-directory dir)))))))

(defun nelisp-bytecode-jit--compile-ir (function ir)
  "Compile FUNCTION IR using GNU ELN for the admitted scalar leaf subset."
  (if (and nelisp-bytecode-jit--preparing-active
           (nelisp-bytecode-jit--gnu-eln-literal-p ir))
      (nelisp-bytecode-jit--compile-eln-ir
       function ir (nelisp-bytecode-jit--cache-identity function))
    (nelisp-bytecode-jit--compile-ir-private function ir)))

(defun nelisp-bytecode-jit--execute-ir (ir handle arguments)
  "Call validated HANDLE for IR with ARGUMENTS without compiling."
  (setq nelisp-bytecode-jit--failure-phase 'native-call)
  (nelisp-bytecode-jit--with-timed-stage :native-call-us
    (cond
     ((eq (plist-get handle :backend) 'gnu-eln-native-subr)
      (when nelisp-bytecode-jit--native-entry-state
        (setcar nelisp-bytecode-jit--native-entry-state t))
      (funcall (plist-get handle :callable)))
     ((nelisp-bytecode-jit--boolean-result-ir-p ir)
      (let ((raw (progn
                   (when nelisp-bytecode-jit--native-entry-state
                     (setcar nelisp-bytecode-jit--native-entry-state t))
                   (nelisp-native-load-raw-call handle arguments))))
        (cond ((eql raw 0) nil)
              ((eql raw 1) t)
              (t (error "nelisp-bytecode-jit: invalid raw boolean %S" raw)))))
     ((nelisp-bytecode-jit--boxed-result-ir-p ir)
      (when nelisp-bytecode-jit--native-entry-state
        (setcar nelisp-bytecode-jit--native-entry-state t))
      (let ((result (nelisp-native-load-call handle arguments)))
        (if (eq (plist-get handle :return-repr) 'raw-bool)
            (cond ((or (eq result t) (eql result 1)) t)
                  ((or (null result) (eql result 0)) nil)
                  (t (error "nelisp-bytecode-jit: invalid boxed boolean %S"
                            result)))
          result)))
     (t (when nelisp-bytecode-jit--native-entry-state
          (setcar nelisp-bytecode-jit--native-entry-state t))
        (nelisp-native-load-raw-call handle arguments)))))

(defun nelisp-bytecode-jit--call-ir (function ir arguments)
  "Compile FUNCTION's IR and call it with its corresponding value ABI."
  (nelisp-bytecode-jit--execute-ir
   ir (nelisp-bytecode-jit--compile-ir function ir) arguments))

(defun nelisp-bytecode-jit--record-runtime-failure (function ir error-data)
  "Write opt-in diagnostics for a swallowed generic dispatch failure."
  (let ((directory (getenv "NELISP_JIT_DIAGNOSTIC_DIR")))
    (when (and directory (not (equal directory "")))
      (make-directory directory t)
      (nelisp-bytecode-jit--write-failure-diagnostic
       directory function ir
       (and (byte-code-function-p function)
            (condition-case nil
                (nelisp-bytecode-ir-validate
                 (aref function 1) (aref function 2)
                 (if (integerp (aref function 0))
                     (logand (aref function 0) 255)
                   (length (aref function 0))))
              (error nil)))
       error-data nelisp-bytecode-jit--failure-source
       nelisp-bytecode-jit--failure-artifact
       nelisp-bytecode-jit--failure-manifest
       nelisp-bytecode-jit--failure-phase))))

(defun nelisp-bytecode-jit--record-bind-trace (error-data)
  "Write an opt-in native binder arity trace after ERROR-DATA is caught."
  (let ((directory (getenv "NELISP_JIT_BIND_TRACE_DIR")))
    (when (and directory (not (equal directory ""))
               nelisp--last-jit-bind-arity-trace)
      (make-directory directory t)
      (with-temp-file (expand-file-name "binder-arity-trace.el" directory)
        (prin1 (list :condition error-data
                     :fingerprint
                     (or nelisp-bytecode-jit--last-expression-fingerprint
                     (plist-get nelisp-bytecode-jit--last-runtime-failure
                                    :fingerprint))
                     :phase nelisp-bytecode-jit--failure-phase
                     :source nelisp-bytecode-jit--failure-source
                     :artifact nelisp-bytecode-jit--failure-artifact
                     :required-got nelisp--last-jit-bind-arity-trace)
               (current-buffer))
        (insert "\n")))))

(defun nelisp-bytecode-jit--boxed-list-supported-p (value budget depth
                                                          &optional nested)
  "Return non-nil for a bounded proper list of supported values.
NESTED permits bounded sublists for projection calls. BUDGET is shared across
walks, and DEPTH bounds recursion for cyclic or deeply nested lists."
  (let ((cursor value)
        (seen nil)
        (supported t))
    (while (and (consp cursor) supported)
      (if (memq cursor seen)
          (setq supported nil)
        (push cursor seen)
        (setcar budget (1- (car budget)))
        (if (< (car budget) 0)
            (setq supported nil)
          (let ((item (car cursor)))
            (setq supported
                  (and (or nested (not (consp item)))
                       (nelisp-bytecode-jit--boxed-value-supported-p
                        item budget (1+ depth) nested)))))
        (setq cursor (cdr cursor))))
    (and supported (null cursor))))

(defun nelisp-bytecode-jit--boxed-value-supported-p (value budget depth
                                                           &optional nested)
  "Return non-nil when VALUE and its contents are safely boxable.
BUDGET is a mutable one-element list limiting total visited cons cells;
DEPTH bounds nested list recursion."
  (cond
   ((or (null value) (eq value t)) t)
   ((and (fixnump value)
         (< most-negative-fixnum value)
         (< value most-positive-fixnum)) t)
   ((stringp value)
    (condition-case nil
        (progn (nelisp-native-load--string-bytes value) t)
      (error nil)))
   ((symbolp value)
    (and (eq value (intern-soft (symbol-name value)))
         (condition-case nil
             (progn (nelisp-native-load--string-bytes (symbol-name value)) t)
           (error nil))))
   ((and (consp value) (< depth 64))
    (nelisp-bytecode-jit--boxed-list-supported-p
     value budget depth nested))
   (t nil)))

(defun nelisp-bytecode-jit--boxed-argument-supported-p (value &optional nested)
  "Return non-nil when VALUE is safely boxable by the current native loader.
Interned symbols and finite proper lists of supported values are included;
uninterned symbols, improper or cyclic lists, and other objects stay in the VM.
List dispatch uses a temporary 256-cons budget to avoid expensive marshalling."
  (nelisp-bytecode-jit--boxed-value-supported-p value (list 256) 0 nested))

(defun nelisp-bytecode-jit--eq-native-argument-supported-p (value)
  "Return non-nil for values with tested identity in the boxed ABI.
The loader preserves aliases for rooted slots; unsupported object shapes stay
on the VM path."
  (nelisp-bytecode-jit--boxed-argument-supported-p value))

(defun nelisp-bytecode-jit--diagnostic-property (key object)
  "Find KEY recursively in a plist-like manifest OBJECT."
  (cond
   ((and (consp object) (eq (car object) key)) (cadr object))
   ((consp object)
    (or (nelisp-bytecode-jit--diagnostic-property key (car object))
        (nelisp-bytecode-jit--diagnostic-property key (cdr object))))
   ((vectorp object)
    (let ((index 0) (found nil))
      (while (and (< index (length object)) (null found))
        (setq found (nelisp-bytecode-jit--diagnostic-property
                     key (aref object index)))
        (setq index (1+ index)))
      found))))

(defun nelisp-bytecode-jit--diagnostic-manifest (path)
  "Read selected body and representation metadata from manifest PATH."
  (when (and path (file-readable-p path))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (let ((manifest (read (current-buffer))))
            (list :manifest-body-offset
                  (nelisp-bytecode-jit--diagnostic-property :body-offset manifest)
                  :manifest-param-repr
                  (nelisp-bytecode-jit--diagnostic-property :param-repr manifest)
                  :manifest-return-repr
                  (nelisp-bytecode-jit--diagnostic-property :return-repr manifest))))
      (error (list :manifest-read-error t)))))

(defun nelisp-bytecode-jit--write-failure-diagnostic
    (directory function ir validation error source artifact manifest &optional phase)
  "Write a bounded failure record under DIRECTORY."
  (let* ((path (make-temp-file (expand-file-name "failure-" directory)
                               nil ".el"))
         (code (and (byte-code-function-p function) (aref function 1)))
         (fingerprint
          (and (byte-code-function-p function)
               (secure-hash 'sha256
                            (prin1-to-string
                             (list (aref function 0) (aref function 1)
                                   (aref function 2) (aref function 3))))))
         (manifest-data (nelisp-bytecode-jit--diagnostic-manifest manifest))
         (record
          (append
           (list :kind (if (eq phase 'native-call)
                           'nelisp-bytecode-jit-runtime-failure
                         'nelisp-bytecode-jit-prepare-failure)
                 :function-fingerprint fingerprint
                 :ir-expression
                 (and ir (let ((printed (prin1-to-string ir)))
                           (substring printed 0 (min 1024 (length printed)))))
                 :compile-phase phase
                 :stage-timings nelisp-bytecode-jit--last-stage-timings
                 :expression-fingerprint
                 nelisp-bytecode-jit--last-expression-fingerprint
                 :opcode-byte-count (if (stringp code) (length code) 0)
                 :opcode-bytes
                 (and (stringp code)
                      (string-to-list (substring code 0 (min 256 (length code)))))
                 :ir-status (and (listp validation) (plist-get validation :status))
                 :ir-eligible (and ir t)
                 :predicate (plist-get ir :predicate)
                 :error (let ((message (error-message-string error)))
                          (substring message 0 (min 512 (length message))))
                 :source-path source :artifact-path artifact :manifest-path manifest)
           manifest-data)))
    (with-temp-file path (prin1 record (current-buffer)))
    path))

(defun nelisp-bytecode-jit-prepare (function)
  "Prepare and publish a validated native entry for supported FUNCTION IR.

Preparation is explicit and may compile/load.  Runtime dispatch only consumes
the resulting handle; unsupported byte code returns nil."
  (unless (or nelisp-bytecode-jit--dispatch-active
              nelisp-bytecode-jit--preparing-active)
    (let* ((ir (nelisp-bytecode-jit--decode-ir function))
           (identity (and ir (nelisp-bytecode-jit--cache-identity function)))
           (cached (and ir (nelisp-bytecode-jit--ready-handle
                            function ir identity)))
           (failure (and ir (gethash function
                                     nelisp-bytecode-jit--compile-failures))))
      (cond
       (cached cached)
       ((not ir) nil)
       ((not identity) nil)
       ((and failure (equal (plist-get failure :identity) identity))
        (let ((condition (plist-get failure :condition)))
          (signal (car condition) (cdr condition))))
       (t
        (setq nelisp-bytecode-jit--last-expression-fingerprint (car identity))
        (let ((nelisp-bytecode-jit--preparing-active t)
              (nelisp-bytecode-jit--dispatch-active t))
          (condition-case error-data
              (let ((handle (nelisp-bytecode-jit--compile-ir function ir)))
                (let ((current-identity
                       (nelisp-bytecode-jit--cache-identity function)))
                  (cond
                   ((not (equal identity current-identity))
                    ;; A mutable payload or runtime identity changed while
                    ;; compiling; discard this stale private candidate.
                    (nelisp-bytecode-jit--retire-handle handle)
                    nil)
                   ((nelisp-bytecode-jit--validated-handle-p
                     ir identity handle)
                    (nelisp-bytecode-jit--publish-handle
                     function ir identity handle))
                   (t (error "nelisp-bytecode-jit: invalid handle metadata")))))
            (error
             (puthash function (list :identity identity :condition error-data
                                     :phase nelisp-bytecode-jit--failure-phase)
                      nelisp-bytecode-jit--compile-failures)
             (let ((directory (getenv "NELISP_JIT_DIAGNOSTIC_DIR")))
               (when (and directory (not (equal directory "")))
                 (condition-case nil
                     (nelisp-bytecode-jit--write-failure-diagnostic
                      directory function ir
                      (nelisp-bytecode-ir-validate
                       (aref function 1) (aref function 2)
                       (if (integerp (aref function 0))
                           (logand (aref function 0) 255)
                         (length (aref function 0))))
                      error-data nelisp-bytecode-jit--failure-source
                      nelisp-bytecode-jit--failure-artifact
                      nelisp-bytecode-jit--failure-manifest
                      nelisp-bytecode-jit--failure-phase)
                   (error nil))))
             (when (getenv "NELISP_JIT_BIND_TRACE_DIR")
               (condition-case nil
                   (nelisp-bytecode-jit--record-bind-trace error-data)
                 (error nil)))
             (signal (car error-data) (cdr error-data))))))))))

(defun nelisp-bytecode-jit-invalidate (function)
  "Forget FUNCTION's published handle or terminal preparation failure."
  (let ((record (gethash function nelisp-bytecode-jit--handles)))
    (remhash function nelisp-bytecode-jit--handles)
    (when record
      (nelisp-bytecode-jit--retire-handle (plist-get record :handle)))
    (setq record nil))
  (remhash function nelisp-bytecode-jit--compile-failures)
  (remhash function nelisp-bytecode-jit--pending-identities)
  (setq nelisp-bytecode-jit--pending-queue
        (cl-remove-if (lambda (request) (eq (plist-get request :function) function))
                      nelisp-bytecode-jit--pending-queue))
  (when (fboundp 'nelisp-eln-system-loader-close)
    (nelisp-bytecode-jit--retry-eln-closes))
  nil)

(defun nelisp-bytecode-jit--compile-branch (function _legacy-ir)
  "Compile a branch function by lowering its validated generic IR."
  (let ((ir (nelisp-bytecode-jit--decode-ir function)))
    (unless ir (error "nelisp-bytecode-jit: invalid branch IR"))
    (nelisp-bytecode-jit--compile-ir function ir)))

(defun nelisp-bytecode-jit--branch-call (function ir argument)
  "Execute decoded branch IR for FUNCTION with fixnum ARGUMENT."
  (nelisp-native-load-raw-call
   (nelisp-bytecode-jit--compile-branch function ir) (list argument)))

(defun nelisp-bytecode-jit--compile-unary (function operation)
  "Compile unary OPERATION through the generic IR source generator."
  (let ((ir (nelisp-bytecode-jit--decode-ir function)))
    (unless (eq operation (nelisp-bytecode-jit--decode-unary function))
      (error "nelisp-bytecode-jit: unary IR mismatch"))
    (nelisp-bytecode-jit--compile-ir function ir)))

(defun nelisp-bytecode-jit--compile-binary-add (function)
  "Compile the generic IR for a byte-code addition stream."
  (let ((ir (nelisp-bytecode-jit--decode-ir function)))
    (unless (nelisp-bytecode-jit--decode-binary-add function)
      (error "nelisp-bytecode-jit: binary addition IR mismatch"))
    (nelisp-bytecode-jit--compile-ir function ir)))

(defun nelisp-bytecode-jit-call (function &rest arguments)
  "Run supported FUNCTION through its shared IR and the RX AOT entry."
  (let ((ir (nelisp-bytecode-jit--decode-ir function)))
    (unless ir (error "nelisp-bytecode-jit: unsupported byte-code shape"))
    (unless (nelisp-bytecode-jit--safe-ir-p ir arguments)
      (signal 'wrong-type-argument (list 'fixnump arguments)))
    (nelisp-bytecode-jit--call-ir function ir arguments)))

(defun nelisp-bytecode-jit--queue-preparation (function ir identity)
  "Queue FUNCTION's IR and IDENTITY once unless this identity already failed."
  (let ((failure (gethash function nelisp-bytecode-jit--compile-failures))
        (pending (gethash function nelisp-bytecode-jit--pending-identities)))
    (unless (or (null identity)
                (>= (length nelisp-bytecode-jit--pending-queue)
                    nelisp-bytecode-jit--pending-limit)
                nelisp-bytecode-jit--preparing-active
                (and failure (equal (plist-get failure :identity) identity))
                (equal pending identity))
      (puthash function identity nelisp-bytecode-jit--pending-identities)
      (setq nelisp-bytecode-jit--pending-queue
            (append nelisp-bytecode-jit--pending-queue
                    (list (list :function function :ir ir :identity identity)))))))

(defun nelisp-bytecode-jit-drain-pending ()
  "Prepare at most one function from the pending snapshot; return jobs tried.

The queue is drained only at an explicit safe boundary.  Recursive drains and
drains during VM dispatch or compilation do nothing."
  (if (or nelisp-bytecode-jit--dispatch-active
          nelisp-bytecode-jit--preparing-active
          nelisp-bytecode-jit--draining-active)
      0
    (let ((nelisp-bytecode-jit--draining-active t)
          (snapshot nil)
          (attempted 0))
      (when nelisp-bytecode-jit--pending-queue
        (setq snapshot
              (cl-subseq nelisp-bytecode-jit--pending-queue 0
                         (min nelisp-bytecode-jit--drain-batch-size
                              (length nelisp-bytecode-jit--pending-queue))))
        (setq nelisp-bytecode-jit--pending-queue
              (nthcdr (length snapshot) nelisp-bytecode-jit--pending-queue)))
      (dolist (request snapshot)
        (let* ((function (plist-get request :function))
               (identity (plist-get request :identity))
               (current (nelisp-bytecode-jit--cache-identity function)))
          (when (equal (gethash function nelisp-bytecode-jit--pending-identities)
                       identity)
            (remhash function nelisp-bytecode-jit--pending-identities)
            (when (equal current identity)
              (setq attempted (1+ attempted))
              (condition-case nil
                  (nelisp-bytecode-jit-prepare function)
                (error nil))))))
      (when (fboundp 'nelisp-eln-system-loader-close)
        (nelisp-bytecode-jit--retry-eln-closes))
      attempted)))

(defun nelisp-bytecode-jit--runtime-dispatch-deferred (function &rest arguments)
  "Return [t RESULT] for a hot prepared call; otherwise request VM fallback.

This function never compiles or loads code.  Hot unprepared functions enqueue
one request for the next explicit safe-point call to
`nelisp-bytecode-jit-drain-pending'."
  (if (or nelisp-bytecode-jit--dispatch-active
          nelisp-bytecode-jit--preparing-active)
      nil
    (let ((nelisp-bytecode-jit--dispatch-active t))
      (let ((result nil))
        (unwind-protect
            (setq result
                  (let* ((nelisp-bytecode-jit--timing-active
                  (nelisp-bytecode-jit--timing-record-start function))
                 (ir (nelisp-bytecode-jit--with-timed-stage :decode-ir-us
                       (nelisp-bytecode-jit--decode-ir function)))
                 (safe (and ir (nelisp-bytecode-jit--safe-ir-p ir arguments)))
                 (identity (and safe
                                (nelisp-bytecode-jit--cache-identity function))))
            (setq nelisp-bytecode-jit--dispatch-attempt-count
                  (1+ nelisp-bytecode-jit--dispatch-attempt-count))
            (when safe
              (let* ((calls (1+ (gethash function nelisp-bytecode-jit--hot-counts 0)))
                     (handle (nelisp-bytecode-jit--ready-handle
                              function ir identity)))
                (puthash function calls nelisp-bytecode-jit--hot-counts)
                (if (< calls nelisp-bytecode-jit-threshold)
                    (progn
                      (setq nelisp-bytecode-jit--interpreter-fallback-count
                            (1+ nelisp-bytecode-jit--interpreter-fallback-count))
                      nil)
                  (if (not handle)
                      (progn
                        (nelisp-bytecode-jit--queue-preparation
                         function ir identity)
                        (setq nelisp-bytecode-jit--interpreter-fallback-count
                              (1+ nelisp-bytecode-jit--interpreter-fallback-count))
                        nil)
                    (let ((nelisp-bytecode-jit--failure-phase nil)
                          (nelisp-bytecode-jit--failure-source nil)
                          (nelisp-bytecode-jit--failure-artifact nil)
                          (nelisp-bytecode-jit--failure-manifest nil)
                          (nelisp-bytecode-jit--native-entry-state (list nil)))
                      (condition-case error-data
                          (let ((result (nelisp-bytecode-jit--execute-ir
                                         ir handle arguments)))
                            (setq nelisp-bytecode-jit--native-call-count
                                  (1+ nelisp-bytecode-jit--native-call-count))
                            (vector t result))
                        (error
                         (setq nelisp-bytecode-jit--last-runtime-failure
                               (list :condition error-data
                                     :message (error-message-string error-data)
                                     :phase nelisp-bytecode-jit--failure-phase
                                     :fingerprint (car identity)
                                     :stage-timings
                                     nelisp-bytecode-jit--last-stage-timings
                                     :ir (plist-get ir :expression)))
                         (when (getenv "NELISP_JIT_DIAGNOSTIC_DIR")
                           (condition-case nil
                               (nelisp-bytecode-jit--record-runtime-failure
                                function ir error-data)
                             (error nil)))
                         (when (getenv "NELISP_JIT_BIND_TRACE_DIR")
                           (condition-case nil
                               (nelisp-bytecode-jit--record-bind-trace error-data)
                             (error nil)))
                         (if (car nelisp-bytecode-jit--native-entry-state)
                             (signal (car error-data) (cdr error-data))
                           (setq nelisp-bytecode-jit--interpreter-fallback-count
                                 (1+ nelisp-bytecode-jit--interpreter-fallback-count))
                           nil))))))))))
          (setq nelisp-bytecode-jit--dispatch-active nil))
        result))))

(defun nelisp-bytecode-jit--runtime-dispatch-legacy (function arguments)
  "Preserve synchronous dispatch until the native preparation hook is active."
  (if nelisp-bytecode-jit--dispatch-active
      nil
    (let ((nelisp-bytecode-jit--dispatch-active t))
      (let* ((ir (nelisp-bytecode-jit--decode-ir function))
             (safe (and ir (nelisp-bytecode-jit--safe-ir-p ir arguments)))
             (calls (and safe (1+ (gethash function
                                            nelisp-bytecode-jit--hot-counts 0)))))
        (setq nelisp-bytecode-jit--dispatch-attempt-count
              (1+ nelisp-bytecode-jit--dispatch-attempt-count))
        (if (not safe)
            nil
          (puthash function calls nelisp-bytecode-jit--hot-counts)
          (if (< calls nelisp-bytecode-jit-threshold)
              (progn
                (setq nelisp-bytecode-jit--interpreter-fallback-count
                      (1+ nelisp-bytecode-jit--interpreter-fallback-count))
                nil)
            (let ((nelisp-bytecode-jit--failure-phase nil)
                  (nelisp-bytecode-jit--failure-source nil)
                  (nelisp-bytecode-jit--failure-artifact nil)
                  (nelisp-bytecode-jit--failure-manifest nil)
                  (nelisp-bytecode-jit--native-entry-state (list nil)))
              (condition-case error-data
                  (let ((result (nelisp-bytecode-jit--call-ir
                                 function ir arguments)))
                    (setq nelisp-bytecode-jit--native-call-count
                          (1+ nelisp-bytecode-jit--native-call-count))
                    (vector t result))
                (error
                 (setq nelisp-bytecode-jit--last-runtime-failure
                       (list :condition error-data
                             :message (error-message-string error-data)
                             :phase nelisp-bytecode-jit--failure-phase
                             :fingerprint
                             (nelisp-bytecode-jit--function-payload-fingerprint
                              function)
                             :stage-timings nelisp-bytecode-jit--last-stage-timings
                             :ir (plist-get ir :expression)))
                 (when (getenv "NELISP_JIT_DIAGNOSTIC_DIR")
                   (condition-case nil
                       (nelisp-bytecode-jit--record-runtime-failure
                        function ir error-data)
                     (error nil)))
                 (when (getenv "NELISP_JIT_BIND_TRACE_DIR")
                   (condition-case nil
                       (nelisp-bytecode-jit--record-bind-trace error-data)
                     (error nil)))
                 (if (car nelisp-bytecode-jit--native-entry-state)
                     (signal (car error-data) (cdr error-data))
                   (setq nelisp-bytecode-jit--interpreter-fallback-count
                         (1+ nelisp-bytecode-jit--interpreter-fallback-count))
                   nil))))))))))

(defun nelisp-bytecode-jit--runtime-dispatch (function &rest arguments)
  "Dispatch FUNCTION synchronously until deferred preparation is enabled."
  (if nelisp-bytecode-jit--deferred-preparation-enabled
      (apply #'nelisp-bytecode-jit--runtime-dispatch-deferred
             function arguments)
    (nelisp-bytecode-jit--runtime-dispatch-legacy function arguments)))

(defun nelisp-bytecode-jit-status ()
  "Return the P5 slice's hotness and execution counters."
  (list :threshold nelisp-bytecode-jit-threshold
        :dispatch-attempts nelisp-bytecode-jit--dispatch-attempt-count
        :native-calls nelisp-bytecode-jit--native-call-count
        :compiled-functions nelisp-bytecode-jit--compile-count
        :interpreter-fallbacks nelisp-bytecode-jit--interpreter-fallback-count
        :pending-preparations (length nelisp-bytecode-jit--pending-queue)
        :preparation-failures (hash-table-count
                               nelisp-bytecode-jit--compile-failures)
        :last-runtime-failure nelisp-bytecode-jit--last-runtime-failure
        :last-expression-fingerprint
        nelisp-bytecode-jit--last-expression-fingerprint
        :last-stage-timings nelisp-bytecode-jit--last-stage-timings
        :mapped-entry (let ((handle nil))
                        (maphash (lambda (_function value)
                                   (when (eq (plist-get value :state) 'ready)
                                     (setq handle (plist-get value :handle))))
                                 nelisp-bytecode-jit--handles)
                        (and handle (plist-get handle :entry)))))

(provide 'nelisp-bytecode-jit)
;;; nelisp-bytecode-jit.el ends here
