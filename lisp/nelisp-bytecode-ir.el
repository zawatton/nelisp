;;; nelisp-bytecode-ir.el --- Structural GNU byte-code decoder -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Decode a deliberately bounded part of GNU Emacs 31.1 byte-code without
;; executing it. Instruction vectors keep the first four slots compatible
;; with the initial bytecode JIT prototype: [offset opcode next operand].

;;; Code:

(require 'cl-lib)

(defconst nelisp-bytecode-ir--max-code-size 65535)

(defun nelisp-bytecode-ir--fail (status reason &optional instructions)
  (list :status status :reason reason :instructions instructions))

(defun nelisp-bytecode-ir-decode-result (code constants)
  "Decode CODE against CONSTANTS without executing any instruction.

Return a result plist with :status `valid', `unsupported', or `malformed',
:instructions as vectors [OFFSET OPCODE NEXT OPERAND METADATA], and a
:reason for non-valid results. Branch operands are absolute byte offsets.
GNU byte-code 31.1 stack-ref opcodes 1 through 5 encode their index in the
opcode; opcode 6 consumes one index byte and opcode 7 consumes a little-endian
16-bit index. Opcode 0 is reserved and rejected. Only instruction widths and
semantics explicitly listed here are accepted."
  (cond
   ((not (stringp code)) (nelisp-bytecode-ir--fail 'malformed "code is not a string"))
   ((not (vectorp constants)) (nelisp-bytecode-ir--fail 'malformed "constants is not a vector"))
   ((> (length code) nelisp-bytecode-ir--max-code-size)
    (nelisp-bytecode-ir--fail 'malformed "code exceeds decoder size limit"))
   (t
    (let ((pc 0) (result nil) (unsupported nil) (failure nil))
      (while (and (< pc (length code)) (not failure))
        (let* ((op (aref code pc)) (width 1) (operand nil) (kind nil)
               (delta nil) (lowerable nil) (metadata nil))
          (cond
           ((= op 0) (setq failure "reserved opcode 0"))
           ((<= 1 op 5)
            (setq kind 'stack-ref delta 1 operand op lowerable t))
           ((= op 6)
            (setq kind 'stack-ref delta 1 width 2 lowerable nil)
            (if (> (+ pc width) (length code))
                (setq failure (format "truncated 8-bit stack-ref index at %d" pc))
              (setq operand (aref code (1+ pc))
                    metadata (list :stack-offset operand))
              (push (cons pc 'extended-stack-ref) unsupported)))
           ((= op 7)
            (setq kind 'stack-ref delta 1 width 3 lowerable nil)
            (if (> (+ pc width) (length code))
                (setq failure (format "truncated 16-bit stack-ref index at %d" pc))
              (setq operand (+ (aref code (1+ pc))
                               (ash (aref code (+ pc 2)) 8))
                    metadata (list :stack-offset operand))
              (push (cons pc 'extended-stack-ref) unsupported)))
           ((<= 8 op 47)
            (let* ((family (* 8 (/ op 8))) (immediate (logand op 7))
                   (operand-width (if (= immediate 6) 1
                                    (if (= immediate 7) 2 0))))
              (setq width (1+ operand-width))
              (if (> (+ pc width) (length code))
                  (setq failure
                        (format "truncated %d-bit compact operand at %d"
                                (* 8 operand-width) pc))
                (setq operand
                      (if (= operand-width 0)
                          immediate
                        (+ (aref code (1+ pc))
                           (if (= operand-width 2)
                               (ash (aref code (+ pc 2)) 8)
                             0)))
                      metadata (list :compact-index immediate
                                     :operand-width operand-width)))
              (cond
               ((= family 8)
                (setq kind 'variable-ref delta 1
                      lowerable (and (< operand (length constants))
                                     (symbolp (aref constants operand)))))
               ((= family 16) (setq kind 'variable-set delta -1))
               ((= family 24) (setq kind 'variable-bind delta 0))
               ((= family 32) (setq kind 'call delta (- (or operand immediate)))
                )
               ((= family 40) (setq kind 'unbind delta 0))
               (t (setq failure (format "unknown compact opcode %d at %d" op pc))))
              (unless (or failure lowerable)
                (push (cons pc 'unsupported-semantics) unsupported))))
           ((memq op '(57 58 59 60))
            ;; GNU Emacs object predicates replace the top stack value in-place.
            ;; Their values are preserved by the boxed native ABI.
            (setq kind 'predicate delta 0 lowerable t)
            (setq metadata (list :width 1))
            )
           ((or (= op 56) (and (<= 61 op) (<= op 82)))
            ;; Fixed-width Lisp object operations from bytecomp.el's byte-defop table.
            (let ((entry (assq op
                               '((56 nth -1) (61 eq -1) (62 memq -1) (63 not 0)
                                 (64 car 0) (65 cdr 0) (66 cons -1) (67 list1 0)
                                 (68 list2 -1) (69 list3 -2) (70 list4 -3)
                                 (71 length 0) (72 aref -1) (73 aset -2)
                                 (74 symbol-value 0) (75 symbol-function 0)
                                 (76 set -1) (77 fset -1) (78 get -1)
                                 (79 substring -2) (80 concat2 -1)
                                 (81 concat3 -2) (82 concat4 -3)))))
              (setq kind (nth 1 entry) delta (nth 2 entry)
                    lowerable (memq op '(61 64 65))
                    metadata (list :width 1))
              (unless lowerable
                (push (cons pc 'unsupported-semantics) unsupported))))
           ((and (<= 147 op) (<= op 168))
            ;; GNU Emacs 31.1 bytecomp.el fixed-width object operations.
            (let ((entry (assq op
                               '((147 set-marker -2) (148 match-beginning 0)
                                 (149 match-end 0) (150 upcase 0) (151 downcase 0)
                                 (152 string= -1) (153 string< -1) (154 equal -1)
                                 (155 nthcdr -1) (156 elt -1) (157 member -1)
                                 (158 assq -1) (159 nreverse 0) (160 setcar -1)
                                 (161 setcdr -1) (162 car-safe 0) (163 cdr-safe 0)
                                 (164 nconc -1) (165 quo -1) (166 rem -1)
                                 (167 numberp 0) (168 integerp 0)))))
              (setq kind (nth 1 entry) delta (nth 2 entry)
                    lowerable (= op 168) metadata (list :width 1))
              (unless lowerable
                (push (cons pc 'unsupported-semantics) unsupported))))
           ((and (<= 96 op) (<= op 127))
            ;; Fixed-width buffer/point operations from bytecomp.el.
            (let ((entry (assq op
                               '((96 point 1) (97 save-current-buffer 0 obsolete)
                                 (98 goto-char 0) (99 insert 0) (100 point-max 1)
                                 (101 point-min 1) (102 char-after 0)
                                 (103 following-char 1) (104 preceding-char 1)
                                 (105 current-column 1) (106 indent-to 0)
                                 (107 scan-buffer 0 obsolete) (108 eolp 1)
                                 (109 eobp 1) (110 bolp 1) (111 bobp 1)
                                 (112 current-buffer 1) (113 set-buffer 0)
                                 (114 save-current-buffer 0) (115 set-mark 0 obsolete)
                                 (116 interactive-p 1 obsolete) (117 forward-char 0)
                                 (118 forward-word 0) (119 skip-chars-forward -1)
                                 (120 skip-chars-backward -1) (121 forward-line 0)
                                 (122 char-syntax 0) (123 buffer-substring -1)
                                 (124 delete-region -1) (125 narrow-to-region -1)
                                 (126 widen 1) (127 end-of-line 0)))))
              (setq kind (nth 1 entry) delta (nth 2 entry)
                    lowerable nil metadata (list :width 1))
              (when (nth 3 entry) (setq metadata (append metadata '(:obsolete t))))
              (push (cons pc 'unsupported-semantics) unsupported)))
           ((and (<= 138 op) (<= op 145))
            (let ((entry (assq op
                               '((138 save-excursion 0)
                                 (139 save-window-excursion 0 obsolete)
                                 (140 save-restriction 0) (141 catch -1 obsolete)
                                 (142 unwind-protect -1)
                                 (143 condition-case -2 obsolete)
                                 (144 temp-output-buffer-setup 0 obsolete)
                                 (145 temp-output-buffer-show -1 obsolete)))))
              (setq kind (nth 1 entry) delta (nth 2 entry)
                    lowerable nil metadata (list :width 1))
              (when (nth 3 entry) (setq metadata (append metadata '(:obsolete t))))
              (push (cons pc 'unsupported-semantics) unsupported)))
           ((= op 136)
            (setq kind 'discard delta -1 lowerable nil
                  metadata (list :width 1))
            (push (cons pc 'unsupported-semantics) unsupported))
           ((memq op '(178 179))
            (let ((operand-width (if (= op 178) 1 2)))
              (setq width (1+ operand-width)
                    kind 'stack-set delta -1 lowerable nil)
              (if (> (+ pc width) (length code))
                  (setq failure
                        (format "truncated %d-bit stack-set operand at %d"
                                (* 8 operand-width) pc))
                (let ((offset (aref code (1+ pc))))
                  (when (= operand-width 2)
                    (setq offset (+ offset (ash (aref code (+ pc 2)) 8))))
                  (setq operand offset
                        metadata (list :width width :operand-width operand-width
                                       :stack-offset offset)))
                (push (cons pc 'unsupported-semantics) unsupported))))
           ((memq op '(175 176 177))
            (setq width 2 delta nil lowerable nil)
            (if (> (+ pc width) (length code))
                (setq failure (format "truncated 8-bit count operand at %d" pc))
              (setq operand (aref code (1+ pc))
                    kind (cdr (assq op '((175 . list-n) (176 . concat-n)
                                         (177 . insert-n))))
                    delta (- 1 operand)
                    metadata (list :width width :operand-width 1
                                   :count operand))
              (push (cons pc 'unsupported-semantics) unsupported)))
           ((= op 182)
            (setq width 2 kind 'discard-n lowerable nil)
            (if (> (+ pc width) (length code))
                (setq failure (format "truncated 8-bit discard operand at %d" pc))
              (let* ((raw (aref code (1+ pc)))
                     (count (logand raw #x7f)))
                (setq operand raw delta (- count)
                      metadata (list :width width :operand-width 1
                                     :count count :preserve-top (/= 0 (logand raw #x80))))
                (push (cons pc 'unsupported-semantics) unsupported))))
           ((= op 183)
            (setq kind 'switch delta -2 lowerable nil
                  metadata (list :width 1 :control-flow 'table-driven))
            (push (cons pc 'unsupported-semantics) unsupported))
           ((<= 129 op 134)
            (setq width 3 kind 'branch)
            (when (> (+ pc width) (length code))
              (setq failure (format "truncated 16-bit operand at %d" pc)))
            (unless failure
              (setq operand (+ (aref code (1+ pc))
                               (ash (aref code (+ pc 2)) 8)))
              (cond ((= op 129) (setq kind 'constant delta 1 lowerable t))
                    ((= op 130) (setq kind 'goto delta 0 lowerable t))
                    ((memq op '(131 132))
                     (setq kind 'conditional-branch delta -1 lowerable t))
                    ((memq op '(133 134))
                     (setq kind 'conditional-branch delta -1 lowerable t)))))
           ((<= 192 op 255)
            (setq kind 'constant delta 1 operand (- op 192)
                  metadata (list :constant-index (- op 192)))
            (if (>= operand (length constants))
                (setq failure (format "constant index %d out of range at %d" operand pc))
              (setq operand (aref constants operand)
                    lowerable (or (null (aref constants (- op 192)))
                                  (eq t (aref constants (- op 192)))
                                  (and (symbolp (aref constants (- op 192)))
                                       (eq (aref constants (- op 192))
                                           (intern-soft
                                            (symbol-name
                                             (aref constants (- op 192))))))
                                  (and (integerp (aref constants (- op 192)))
                                       (fixnump (aref constants (- op 192))))))
              (when (symbolp operand)
                (setq metadata (append metadata '(:constant-type symbol))))
              (unless lowerable (push (cons pc 'non-fixnum-constant) unsupported))))
           ((memq op '(48 49 50))
            (setq kind (pcase op (48 'pop-handler) (49 'push-condition-case)
                         (50 'push-catch))
                  delta (if (= op 48) 0 -1) lowerable nil)
            (when (memq op '(49 50))
              (setq width 3)
              (if (> (+ pc width) (length code))
                  (setq failure (format "truncated handler target at %d" pc))
                (setq operand (+ (aref code (1+ pc))
                                 (ash (aref code (+ pc 2)) 8)))))
            (push (cons pc 'handler-semantics) unsupported))
           ((memq op '(83 84 85 86 87 88 89 90 91 92 93 94 95))
            (setq kind 'arithmetic
                  delta (if (memq op '(83 84 91)) 0 -1)
                  lowerable (memq op '(83 84 85 86 87 88 89 90 92 95)))
            (unless lowerable (push (cons pc 'unsupported-arithmetic) unsupported)))
           ((= op 135) (setq kind 'return delta -1 lowerable t))
           ((= op 137) (setq kind 'dup delta 1 lowerable t))
           ((memq op '(128 146 169 170 171 172 173 174 180 181
                       184 185 186 187 188 189 190 191))
            (setq failure (format "reserved opcode %d at %d" op pc)))
           (t (setq failure (format "unsupported opcode %d at %d (width unknown)" op pc))))
          (unless failure
            (when (= op 129)
              (setq metadata (list :constant-index operand))
              (if (>= operand (length constants))
                  (setq failure (format "constant index %d out of range at %d" operand pc))
                (let ((index operand))
                  (setq operand (aref constants index)
                        lowerable (or (and (integerp (aref constants index))
                                           (fixnump (aref constants index)))
                                      (and (symbolp (aref constants index))
                                           (eq (aref constants index)
                                               (intern-soft
                                                (symbol-name
                                                 (aref constants index)))))))
                  (when (symbolp operand)
                    (setq metadata (append metadata '(:constant-type symbol)))))
              (unless failure
                (unless lowerable (push (cons pc 'non-fixnum-constant) unsupported))))))
            (unless failure
              (let ((next (+ pc width)))
                (push (vector pc op next operand
                              (append (list :kind kind :stack-delta delta
                                            :lowerable (and lowerable t)) metadata))
                      result)
                (setq pc next)))))
      (if failure
          (nelisp-bytecode-ir--fail 'malformed failure (vconcat (nreverse result)))
        (list :status (if unsupported 'unsupported 'valid)
              :reason (and unsupported (format "unsupported semantics at %d" (caar unsupported)))
              :unsupported (nreverse unsupported)
              :instructions (vconcat (nreverse result))))))))

(defun nelisp-bytecode-ir-decode-instructions (code constants)
  "Compatibility decoder returning a list of [offset op next operand] rows.

Return nil for malformed streams. Structurally valid streams with semantics
outside the current lowering subset are returned and can be inspected with
`nelisp-bytecode-ir-decode-result'."
  (let ((result (nelisp-bytecode-ir-decode-result code constants)))
    (unless (eq (plist-get result :status) 'malformed)
      (append (plist-get result :instructions) nil))))

(defalias 'nelisp-bytecode-ir--decode-instructions
  #'nelisp-bytecode-ir-decode-instructions)

(defun nelisp-bytecode-ir--instruction-table (instructions)
  (let (table)
    (dotimes (i (length instructions))
      (let ((insn (aref instructions i)))
        (push (cons (aref insn 0) insn) table)))
    table))

(defun nelisp-bytecode-ir-validate-targets (instructions code-length)
  "Return nil or a diagnostic if any branch target is not an instruction start."
  (setq instructions (vconcat instructions))
  (let ((table (nelisp-bytecode-ir--instruction-table instructions))
        failure)
    (dotimes (i (length instructions))
      (let* ((insn (aref instructions i)) (op (aref insn 1))
             (target (aref insn 3)))
        (when (memq op '(130 131 132 133 134 49 50))
          (unless (and (integerp target) (>= target 0) (< target code-length)
                       (assq target table))
            (setq failure (format "target %S at %d is not an instruction boundary"
                                  target (aref insn 0)))))))
    failure))

(defun nelisp-bytecode-ir-cfg-valid-p (instructions code-length)
  "Compatibility CFG predicate for instruction rows and CODE-LENGTH."
  (let ((target-error (nelisp-bytecode-ir-validate-targets
                       (vconcat instructions) code-length))
        (table (nelisp-bytecode-ir--instruction-table (vconcat instructions)))
        (seen nil) (pending '(0)) (valid t) (steps 0)
        (limit (* 3 (max 1 (length instructions)))))
    (when target-error (setq valid nil))
    (while (and valid pending (< steps limit))
      (setq steps (1+ steps))
      (let* ((pc (pop pending)) (insn (cdr (assq pc table))))
        (unless insn (setq valid nil))
        (when (and insn (not (memq pc seen)))
          (push pc seen)
          (let ((op (aref insn 1)) (next (aref insn 2)) (target (aref insn 3)))
            (cond ((= op 135))
                  ((= op 130) (push target pending))
                  ((memq op '(131 132 133 134))
                   (push target pending)
                   (if (assq next table) (push next pending) (setq valid nil)))
                  ((memq op '(49 50))
                   (push target pending)
                   (if (assq next table) (push next pending) (setq valid nil)))
                  ((= op 183) (setq valid nil))
                  ((assq next table) (push next pending))
                  (t (setq valid nil)))))))
    (and valid (null pending) (= (length seen) (length instructions)))))

(defun nelisp-bytecode-ir-single-backedge-loop (instructions code-length
                                                               initial-depth)
  "Describe a single natural loop with one forward conditional exit.

INSTRUCTIONS are decoded IR rows.  Return a plist naming the loop header,
conditional, body, backedge, and exit only when targets, reachability, and
fixed-point stack depths are valid.  This recognizes control-flow shape; it
does not lower loop operations or admit handler transfers."
  (setq instructions (vconcat instructions))
  (let* ((stack (nelisp-bytecode-ir-analyze-stack instructions initial-depth))
         (depths (plist-get stack :depths))
         (backedges nil) (conditional-exits nil) (valid nil))
    (when (and (eq (plist-get stack :status) 'complete)
               (nelisp-bytecode-ir-cfg-valid-p instructions code-length))
      (dotimes (i (length instructions))
        (let* ((row (aref instructions i)) (pc (aref row 0))
               (op (aref row 1)) (next (aref row 2)) (target (aref row 3)))
          (when (and (= op 130) (< target pc))
            (push row backedges))))
      (when (= (length backedges) 1)
        (let* ((backedge (car backedges))
               (header (aref backedge 3))
               (backedge-pc (aref backedge 0)))
          (dotimes (i (length instructions))
            (let* ((row (aref instructions i)) (pc (aref row 0))
                   (op (aref row 1)) (next (aref row 2))
                   (target (aref row 3)))
              (when (and (memq op '(131 132))
                         (<= header pc) (< pc backedge-pc)
                         (> target backedge-pc)
                         (<= next backedge-pc))
                (push row conditional-exits))))
          (when (= (length conditional-exits) 1)
            (let* ((condition (car conditional-exits))
                   (condition-pc (aref condition 0))
                   (op (aref condition 1)) (next (aref condition 2))
                   (target (aref condition 3))
                   (header-depth (cdr (assq header depths)))
                   (backedge-depth (cdr (assq backedge-pc depths)))
                   (linear-controls t))
              (dotimes (i (length instructions))
                (let* ((row (aref instructions i)) (pc (aref row 0))
                       (row-op (aref row 1)))
                  (when (and (<= header pc) (<= pc backedge-pc)
                             (or (and (memq row-op '(131 132))
                                      (/= pc condition-pc))
                                 (and (= row-op 130) (/= pc backedge-pc))
                                 (memq row-op '(133 134 183 49 50)))
                    (setq linear-controls nil))))
              (when (and linear-controls (= header-depth backedge-depth))
                (let* ((exit (if (> target backedge-pc) target next))
                       (body (if (= exit target) next target))
                       (continue-on-true (if (= op 131)
                                             (= body next)
                                           (= body target))))
                  (setq valid
                        (list :header header :condition condition-pc
                              :body body :backedge backedge-pc :exit exit
                              :continue-on-true continue-on-true
                              :stack-depth header-depth)))))))))
    valid)))

(defalias 'nelisp-bytecode-ir--cfg-valid-p #'nelisp-bytecode-ir-cfg-valid-p)

(defun nelisp-bytecode-ir-analyze-stack (instructions initial-depth)
  "Analyze reachable stack depths from INITIAL-DEPTH.

Return a plist with :status `complete', `unknown', or `invalid'. Handler
targets are validated structurally but excluded from ordinary CFG edges;
their value restoration makes stack depth unknown without VM handler rules."
  (if (not (and (integerp initial-depth) (>= initial-depth 0)))
      (list :status 'invalid :reason "initial stack depth must be a nonnegative integer")
    (setq instructions (vconcat instructions))
    (let ((table (nelisp-bytecode-ir--instruction-table instructions))
        (depths nil) (pending (list (cons 0 initial-depth)))
        (maximum initial-depth) (unknown nil) (failure nil) (steps 0)
        (limit (* 4 (max 1 (length instructions)))))
    (while (and pending (not failure) (< steps limit))
      (setq steps (1+ steps))
      (let* ((item (pop pending)) (pc (car item)) (depth (cdr item))
             (insn (cdr (assq pc table))))
        (if (not insn)
            (setq failure (format "control flow reaches non-instruction offset %d" pc))
          (let* ((old (assq pc depths)) (op (aref insn 1))
                 (next (aref insn 2)) (operand (aref insn 3))
                 (delta (plist-get (aref insn 4) :stack-delta))
                 (after (and (numberp delta) (+ depth delta))))
            (cond
             ((and old (/= (cdr old) depth))
              (setq failure (format "inconsistent stack depth at %d: %d vs %d"
                                    pc (cdr old) depth)))
             (old nil)
             (t
              (push (cons pc depth) depths)
              (when (memq op '(49 50 183)) (setq unknown t))
              (when (and (<= 1 op 7) (>= depth 0)
                         (>= (or (plist-get (aref insn 4) :stack-offset)
                                 (and (<= op 5) op) 0)
                            depth))
                (setq failure (format "stack reference outside depth %d at %d"
                                      depth pc)))
              (when (or (null after) (< after 0))
                (setq failure (format "stack underflow/unknown effect at %d" pc)))
              (when (and after (> after maximum)) (setq maximum after))
              (unless failure
                (pcase op
                  (135 nil)
                  (183 nil)
                  (130 (push (cons operand after) pending))
                  ((or 131 132)
                   (push (cons operand after) pending)
                   (push (cons next after) pending))
                  ((or 133 134)
                   (push (cons operand depth) pending)
                   (push (cons next after) pending))
                  (_ (if (assq next table)
                         (push (cons next after) pending)
                       (setq failure (format "execution falls off bytecode at %d" next))))))))))))
    (cond (failure (list :status 'invalid :reason failure :depths (nreverse depths)
                         :max-depth maximum))
          ((>= steps limit) (list :status 'invalid :reason "dataflow step limit exceeded"
                                  :depths (nreverse depths) :max-depth maximum))
          (t (list :status (if unknown 'unknown 'complete)
                   :reason (and unknown
                                "handler or table-driven transfer has dynamic control flow")
                   :depths (nreverse depths) :max-depth maximum))))))

(defun nelisp-bytecode-ir-validate (code constants &optional initial-depth)
  "Decode and validate CODE, CONSTANTS, and optional INITIAL-DEPTH.

Return the decode plist enriched with :stack-analysis when INITIAL-DEPTH is
provided. Bad branch/handler targets and statically invalid stack flows set
:status to `malformed'. Unsupported execution semantics remain distinguishable
as `unsupported'; this function never executes byte-code."
  (let* ((result (nelisp-bytecode-ir-decode-result code constants))
         (instructions (plist-get result :instructions)))
    (unless (eq (plist-get result :status) 'malformed)
      (let ((target-error (nelisp-bytecode-ir-validate-targets
                           instructions (length code))))
        (if target-error
            (setq result (plist-put result :status 'malformed)
                  result (plist-put result :reason target-error))
          (when (integerp initial-depth)
            (let ((stack (nelisp-bytecode-ir-analyze-stack instructions initial-depth)))
              (setq result (plist-put result :stack-analysis stack))
              (when (eq (plist-get stack :status) 'invalid)
                (setq result (plist-put result :status 'malformed)
                      result (plist-put result :reason (plist-get stack :reason)))))))))
    result))

(provide 'nelisp-bytecode-ir)
;;; nelisp-bytecode-ir.el ends here
