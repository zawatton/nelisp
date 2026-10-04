;;; nelisp-bytecode-native-switch.el --- Runtime switch validation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-hash-custom)

(defun nelisp-bytecode-native-switch-reference (key table targets)
  "Look up KEY using TABLE's current test and validate against TARGETS.
The fresh sentinel distinguishes a miss from every possible stored value.
Only the compiler's boundary/depth-verified targets may enter native blocks."
  (let* ((missing (cons nil nil)) (target (gethash key table missing)))
    (cond ((eq target missing) -1)
          ((and (integerp target) (memq target targets)) target)
          (t (error "Invalid native switch target: %S" target)))))

(let ((lookup (symbol-function 'gethash))
      (pair (symbol-function 'cons)) (same (symbol-function 'eq))
      (integer (symbol-function 'integerp)) (member (symbol-function 'memq))
      (raise (symbol-function 'error)))
  (let ((function
         (lambda (key table targets)
           (let* ((missing (funcall pair nil nil))
                  (target (funcall lookup key table missing)))
             (cond ((funcall same target missing) -1)
                   ((and (funcall integer target) (funcall member target targets)) target)
                   (t (funcall raise "Invalid native switch target: %S" target)))))))
    (defun nelisp-bytecode-native-switch-function ()
      "Return the frozen Lisp lookup/validation callable used through F1."
      function)))

(defun nelisp-bytecode-native-switch-depths (rows initial-depth)
  "Solve ordinary edge depth constraints, including switch miss edges.
Disconnected arms use the reachable switch depth, raised to their safe minimum.
This defines
an explicit admission domain; runtime targets outside it are refused. Work is
bounded by the decoded instruction count, not the number of execution paths."
  (let ((graph (make-hash-table :test 'eql)) (offsets (make-hash-table :test 'eql))
        (table (nelisp-bytecode-ir--instruction-table rows)) result)
    (dolist (row (append rows nil))
      (let* ((pc (aref row 0)) (op (aref row 1)) (next (aref row 2))
             (delta (plist-get (aref row 4) :stack-delta)) edges)
        (unless (and (numberp delta) (nelisp-bytecode-frame-ir--min-inputs row)
                     (nelisp-bytecode-frame-ir--simple-kind row))
          (error "Unsupported switch depth semantics at %d" pc))
        (setq edges
              (cond ((= op 135) nil)
                    ((= op 130) (list (cons (aref row 3) delta)))
                    ((memq op '(131 132 133 134))
                     (list (cons next delta) (cons (aref row 3) (if (memq op '(133 134)) 0 delta))))
                    (t (list (cons next delta)))))
        (dolist (edge edges)
          (unless (assq (car edge) table) (error "Invalid switch depth edge at %d" pc))
          (puthash pc (cons edge (gethash pc graph)) graph)
          (puthash (car edge) (cons (cons pc (- (cdr edge))) (gethash (car edge) graph)) graph))))
    (dolist (row (append rows nil))
      (let ((start (aref row 0)))
        (unless (gethash start offsets)
          (let ((pending (list start)) (component nil) (minimum 0) anchor)
            (puthash start 0 offsets)
            (while pending
              (let* ((pc (pop pending)) (offset (gethash pc offsets))
                     (ins (cdr (assq pc table)))
                     (need (nelisp-bytecode-frame-ir--min-inputs ins)))
                (when (<= 1 (aref ins 1) 7)
                  (setq need (1+ (or (plist-get (aref ins 4) :stack-offset) (aref ins 1)))))
                (push pc component)
                (setq minimum (max minimum (- need offset)))
                (when (= pc 0) (setq anchor (- initial-depth offset)))
                (dolist (edge (gethash pc graph))
                  (let* ((to (car edge)) (expected (+ offset (cdr edge)))
                         (old (gethash to offsets :missing)))
                    (if (eq old :missing)
                        (progn (puthash to expected offsets) (push to pending))
                      (unless (= old expected) (error "Inconsistent switch depth at %d" to)))))))
            (when (and anchor (< anchor minimum)) (error "Operand underflow in switch component"))
            (let ((switch-depth nil))
              (dotimes (index (length rows))
                (let* ((ins (aref rows index)) (known (assq (aref ins 0) result)))
                  (when (and known (= (aref ins 1) 183))
                    (setq switch-depth (if switch-depth (min switch-depth (- (cdr known) 2))
                                         (- (cdr known) 2))))))
              (let ((base (or anchor (max minimum (or switch-depth 0)))))
              (dolist (pc component) (push (cons pc (+ base (gethash pc offsets))) result))))))))
    result))
(provide 'nelisp-bytecode-native-switch)
