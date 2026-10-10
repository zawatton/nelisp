;;; nelisp-native-funcall-v2.el --- Generic rooted evaluator ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-bytecode-cleanup)
(declare-function nelisp-native-frame-v2-bank-copy-emit "nelisp-native-frame-v2" (plan inputs roots body))
(defvar nelisp-stdlib--symbol-plists)
(defvar nelisp--bytecode-lisp-providers)
(defconst nelisp-native-funcall-v2-version "nelisp-native-funcall-v2-2")
(let ((descriptor
       '(:version "nelisp-native-funcall-v2-2" :name "nl_native_funcall_v2"
         :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64
         :root-limit 256 :arguments contiguous :scratch-count 4
         :exit-offset 1 :exit-base 1024 :exit-kinds (1 2)
         :poll-entry nl_native_poll_v2 :poll-state vector-9 :ownership reauthenticate :stash publish-before-clear)))
(defun nelisp-native-funcall-v2-descriptor ()
  "Return fresh source-owned bounds, signature and exit semantics."
  (copy-tree descriptor)))
(defun nelisp-native-funcall-v2-hash ()
  "Bind all generic evaluator ABI semantics."
  (let ((print-length nil) (print-level nil))
    (secure-hash 'sha256 (prin1-to-string (list (nelisp-native-funcall-v2-descriptor) (nelisp-bytecode-legacy-source))))))
(let ((primitives '((56 nth 2) (57 symbolp 1) (58 consp 1) (59 stringp 1) (60 listp 1)
                    (61 eq 2) (62 memq 2) (63 not 1) (64 car 1) (65 cdr 1) (66 cons 2)
                    (67 list 1) (68 list 2) (69 list 3) (70 list 4) (175 list operand)
                    (71 length 1) (72 aref 2) (73 aset 3) (74 symbol-value 1)
                    (75 symbol-function 1) (76 set 2) (77 fset 2) (78 get 2)
                    (79 substring 3) (80 concat 2) (81 concat 3) (82 concat 4)
                    (176 concat operand) (177 insert operand)
                    (83 1- 1) (84 1+ 1) (85 = 2) (86 > 2) (87 < 2) (88 <= 2) (89 >= 2)
                    (90 - 2) (91 - 1) (92 + 2) (93 max 2) (94 min 2) (95 * 2)
                    (96 point 0) (98 goto-char 1) (99 insert 1) (100 point-max 0)
                    (101 point-min 0) (102 char-after 1) (103 following-char 0)
                    (104 previous-char 0) (105 current-column 0) (106 indent-to 1)
                    (108 eolp 0) (109 eobp 0) (110 bolp 0) (111 bobp 0)
                    (112 current-buffer 0) (113 set-buffer 1)
                    (116 interactive-p 0 dynamic) (117 forward-char 1) (118 forward-word 1)
                    (119 skip-chars-forward 2) (120 skip-chars-backward 2)
                    (121 forward-line 1) (122 char-syntax 1) (123 buffer-substring 2)
                    (124 delete-region 2) (125 narrow-to-region 2) (126 widen 0) (127 end-of-line 1)
                    (139 nelisp--bytecode-legacy-window 1)
                    (141 nelisp--bytecode-legacy-catch 2)
                    (143 nelisp--bytecode-legacy-condition 3)
                    (144 nelisp--bytecode-legacy-setup 1)
                    (145 nelisp--bytecode-legacy-show 2)
                    (164 nconc 2) (165 / 2) (166 % 2) (167 numberp 1) (168 integerp 1)
                    (147 set-marker 3) (148 match-beginning 1) (149 match-end 1)
                    (150 upcase 1) (151 downcase 1) (152 string-equal 2)
                    (153 string-lessp 2) (154 equal 2) (155 nthcdr 2)
                    (156 elt 2) (157 member 2) (158 assq 2)
                    (159 nreverse 1) (160 setcar 2) (161 setcdr 2)
                    (162 car-safe 1) (163 cdr-safe 1))))
(defun nelisp-native-funcall-v2-primitive (opcode)
  "Return the canonical runtime primitive and arity for OPCODE."
  (copy-tree (assq opcode primitives))))
(let ((lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq))
      (association (if (fboundp 'nelisp--eval-source-string)
                       '(builtin assq) (symbol-function 'assq)))
      (membership (symbol-function 'memq))
      (legacy-providers nelisp-bytecode-legacy-providers)
      (originals (mapcar (lambda (name) (cons name (symbol-function (if (eq name 'previous-char) 'preceding-char name)))) '(nelisp--bytecode-legacy-window nelisp--bytecode-legacy-catch
                                                                            nelisp--bytecode-legacy-condition nelisp--bytecode-legacy-setup nelisp--bytecode-legacy-show
                                                                            car cdr car-safe cdr-safe cons list nth memq length aref aset
                                                                            symbol-value symbol-function set fset get substring concat insert apply
                                                                            set-marker match-beginning match-end upcase downcase
                                                                            string-equal string-lessp equal nthcdr elt member assq
                                                                            nreverse setcar setcdr
                                                                            point goto-char insert point-max point-min char-after
                                                                            following-char previous-char current-column indent-to
                                                       eolp eobp bolp bobp current-buffer set-buffer forward-char forward-word
                                                       skip-chars-forward skip-chars-backward forward-line char-syntax
                                                       buffer-substring delete-region narrow-to-region widen end-of-line
                                                                            symbolp consp stringp listp eq not
                                                                            1- 1+ = > < <= >= - + max min * nconc / % numberp integerp)))
      (car-safe-provider
       (let ((consp-value '(builtin consp)) (car-value '(builtin car)))
         (lambda (object)
           (if (funcall consp-value object) (funcall car-value object) nil))))
      (cdr-safe-provider
       (let ((consp-value '(builtin consp)) (cdr-value '(builtin cdr)))
         (lambda (object)
           (if (funcall consp-value object) (funcall cdr-value object) nil))))
      ;; GNU Bnth's small-index path reports the reached dotted tail;
      ;; Fnth instead reports the original list.  Keep the VM condition/data
      ;; through a frozen Lisp provider, delegating the other cases to Fnth.
      (nth-provider
       (let ((nth-value '(builtin nth)) (car-value '(builtin car))
             (cdr-value '(builtin cdr)) (consp-value '(builtin consp))
             (integerp-value '(builtin integerp))
             (less-equal-value '(builtin <=)) (greater-value '(builtin >))
             (decrement-value '(builtin 1-)))
         (lambda (n list)
           (if (and (funcall integerp-value n)
                    (funcall less-equal-value 0 n) (funcall less-equal-value n 127))
               (let ((tail list))
                 (while (and (funcall greater-value n 0) (funcall consp-value tail))
                   (setq tail (funcall cdr-value tail) n (funcall decrement-value n)))
                 (funcall car-value tail))
             (funcall nth-value n list)))))
      (nreverse-provider
       (let ((null-value '(builtin null)) (consp-value '(builtin consp))
             (cdr-value '(builtin cdr)) (setcdr-value '(builtin setcdr))
             (eq-value '(builtin eq)) (signal-value '(builtin signal))
             (vectorp-value '(builtin vectorp)) (bool-vector-p-value '(builtin bool-vector-p))
             (stringp-value '(builtin stringp)) (length-value '(builtin length))
             (aref-value '(builtin aref)) (aset-value '(builtin aset))
             (substring-value '(builtin substring)) (concat-value '(builtin concat))
             (apply-value '(builtin apply)) (cons-value '(builtin cons))
             (list-value '(builtin list))
             (less-value '(builtin <)) (decrement-value '(builtin 1-))
             (increment-value '(builtin 1+)))
         (lambda (seq)
           (cond
            ((funcall null-value seq) nil)
            ((funcall consp-value seq)
             (let ((prev nil) (cur seq) next)
               (while (funcall consp-value cur)
                 (setq next (funcall cdr-value cur))
                 (when (funcall eq-value next seq)
                   (funcall signal-value 'circular-list (list seq)))
                 (funcall setcdr-value cur prev)
                 (setq prev cur cur next))
               (unless (funcall null-value cur)
                 (funcall signal-value 'wrong-type-argument (list 'listp seq)))
               prev))
            ((or (funcall vectorp-value seq) (funcall bool-vector-p-value seq))
             (let ((i 0) (j (funcall decrement-value (funcall length-value seq))) tmp)
               (while (funcall less-value i j)
                 (setq tmp (funcall aref-value seq i))
                 (funcall aset-value seq i (funcall aref-value seq j))
                 (funcall aset-value seq j tmp)
                 (setq i (funcall increment-value i) j (funcall decrement-value j)))
               seq))
            ((funcall stringp-value seq)
             (let ((n (funcall length-value seq)) (i 0)
                   (parts (funcall list-value (funcall substring-value seq 0 0))))
               ;; Reverse character slices, retaining the original byte mode.
               ;; Concatenate once; multibyte strings cannot be rewritten with
               ;; ASET, and repeated concatenation would copy quadratic data.
               (while (funcall less-value i n)
                 (setq parts (funcall cons-value
                                      (funcall substring-value seq i (funcall increment-value i))
                                      parts)
                       i (funcall increment-value i)))
               (funcall apply-value concat-value parts)))
            (t (funcall signal-value 'wrong-type-argument (list 'arrayp seq)))))))
      ;; VM MIN/MAX retain the selected object and reject mixed bignum/float
      ;; pairs. The public prelude functions have different NaN semantics.
      (max-provider
       (let ((numberp-value '(builtin numberp)) (integerp-value '(builtin integerp))
             (floatp-value '(builtin floatp)) (less-value '(builtin <))
             (greater-value '(builtin >)) (compare-value '(builtin >))
             (signal-value '(builtin signal)))
         (lambda (left right)
           (cond
            ((not (funcall numberp-value left))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p left)))
            ((not (funcall numberp-value right))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p right)))
            ((and (funcall integerp-value left)
                  (or (funcall less-value left -2305843009213693952)
                      (funcall greater-value left 2305843009213693951))
                  (funcall floatp-value right))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p right)))
            ((and (funcall integerp-value right)
                  (or (funcall less-value right -2305843009213693952)
                      (funcall greater-value right 2305843009213693951))
                  (funcall floatp-value left))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p left)))
            ((funcall compare-value right left) right)
            (t left)))))
      (min-provider
       (let ((numberp-value '(builtin numberp)) (integerp-value '(builtin integerp))
             (floatp-value '(builtin floatp)) (less-value '(builtin <))
             (greater-value '(builtin >)) (compare-value '(builtin <))
             (signal-value '(builtin signal)))
         (lambda (left right)
           (cond
            ((not (funcall numberp-value left))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p left)))
            ((not (funcall numberp-value right))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p right)))
            ((and (funcall integerp-value left)
                  (or (funcall less-value left -2305843009213693952)
                      (funcall greater-value left 2305843009213693951))
                  (funcall floatp-value right))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p right)))
            ((and (funcall integerp-value right)
                  (or (funcall less-value right -2305843009213693952)
                      (funcall greater-value right 2305843009213693951))
                  (funcall floatp-value left))
             (funcall signal-value 'wrong-type-argument (list 'number-or-marker-p left)))
            ((funcall compare-value right left) right)
            (t left)))))
      ;; The bytecode NCONC intrinsic overwrites a dotted tail and names
      ;; CONSP for a non-list first operand, bypassing the public provider.
      (nconc-provider
       (let ((null-value '(builtin null)) (consp-value '(builtin consp))
             (cdr-value '(builtin cdr)) (setcdr-value '(builtin setcdr))
             (signal-value '(builtin signal)))
         (lambda (first second)
           (cond ((funcall null-value first) second)
                 ((not (funcall consp-value first))
                  (funcall signal-value 'wrong-type-argument (list 'consp first)))
                 (t (let ((tail first))
                      (while (funcall consp-value (funcall cdr-value tail))
                        (setq tail (funcall cdr-value tail)))
                      (funcall setcdr-value tail second)
                      first))))))
      ;; REM validates both integer operands before the VM's limited bignum
      ;; divisor path. The ordinary % primitive only checks a wide dividend.
      (rem-provider
       (let ((integerp-value '(builtin integerp)) (less-value '(builtin <))
             (greater-value '(builtin >)) (rem-value '(builtin %))
             (signal-value '(builtin signal)))
         (lambda (left right)
           (cond
            ((not (funcall integerp-value left))
             (funcall signal-value 'wrong-type-argument (list 'integer-or-marker-p left)))
            ((not (funcall integerp-value right))
             (funcall signal-value 'wrong-type-argument (list 'integer-or-marker-p right)))
            ((and (or (funcall less-value left -2305843009213693952)
                      (funcall greater-value left 2305843009213693951)
                      (funcall less-value right -2305843009213693952)
                      (funcall greater-value right 2305843009213693951))
                  (or (funcall less-value right -2147483648)
                      (funcall greater-value right 2147483648)))
             (funcall signal-value 'nelisp-bignum-division-unsupported nil))
            (t (funcall rem-value left right))))))
      ;; GET is a prelude provider over the same store used by BYTE-GET.
      ;; Freeze its source-owned body and dependencies, never its public cell.
      (get-provider
       (let ((symbolp-value '(builtin symbolp)) (signal-value '(builtin signal))
             (gethash-value '(builtin gethash)) (plist-get-value '(builtin plist-get)))
         (lambda (symbol property)
           (unless (funcall symbolp-value symbol)
             (funcall signal-value 'wrong-type-argument (list 'symbolp symbol)))
           (funcall plist-get-value
                    (funcall gethash-value symbol nelisp-stdlib--symbol-plists) property)))))
(defun nelisp-native-funcall-v2-initializer (name)
  "Materialize only a canonical frozen VM primitive, never a public function cell."
  (unless (funcall association name originals) (error "Unknown funcall primitive initializer"))
  (if (funcall association name legacy-providers)
      (cdr (funcall association name legacy-providers))
    (if (fboundp 'nelisp--eval-source-string)
      ;; Builtin values are runtime evaluator tokens, not a caller certificate.
      (cond ((funcall same name 'car-safe) car-safe-provider)
            ((funcall same name 'cdr-safe) cdr-safe-provider)
            ((funcall same name 'nth) nth-provider)
            ((funcall same name 'get) get-provider)
            ((funcall same name 'nreverse) nreverse-provider)
            ((funcall same name 'max) max-provider) ((funcall same name 'min) min-provider)
            ((funcall same name 'nconc) nconc-provider) ((funcall same name '%) rem-provider)
            ;; These VM operations are Lisp-owned. Their values were frozen
            ;; by the prelude, before caller code can rebind a public cell.
            ((funcall membership name '(set-marker match-beginning match-end upcase downcase
                                                       point goto-char insert point-max point-min char-after
                                                       following-char previous-char current-column indent-to
                                                       eolp eobp bolp bobp current-buffer set-buffer forward-char forward-word
                                                       skip-chars-forward skip-chars-backward forward-line char-syntax
                                                       buffer-substring delete-region narrow-to-region widen end-of-line))
             (cdr (funcall association name nelisp--bytecode-lisp-providers)))
            ((funcall same name 'string-equal) '(builtin string=))
            ((funcall same name 'string-lessp) '(builtin string<))
            (t (list 'builtin name)))
    (cdr (funcall association name originals))))))
(defun nelisp-native-funcall-v2-reference (function arguments)
  "Lisp reference for the evaluator entry; roots are an infrastructure concern."
  (apply function arguments))
(defun nelisp-native-funcall-v2-copy-form (inputs roots body)
  "Emit authenticated full-slot copies before BODY; arguments may be phi indices."
  (let ((result body) (index (length inputs)))
    (while (> index 0)
      (setq index (1- index))
      (let ((source (intern (format "f1_source_%d" index)))
            (destination (intern (format "f1_destination_%d" index))))
        (setq result
              `(let ((,source (extern-call nl_root_pin_slot_v2 env ticket ,(nth index inputs) 0 0 0))
                     (,destination (extern-call nl_root_pin_slot_v2 env ticket ,(nth index roots) 0 0 0)))
                 (if (or (= ,source 0) (= ,destination 0)) 2
                   (progn
                     ,@(mapcar (lambda (offset)
                                 `(ptr-write-u64 ,destination ,offset (ptr-read-u64 ,source ,offset)))
                               '(0 8 16 24))
                     ,result)))))) result))
(defun nelisp-native-funcall-v2-fixnum-form (opcode inputs output success fallback)
  "Emit allocation-free fixnum arithmetic around the unchanged FALLBACK.
Full operands are read before output writes, including aliased root banks.
The multiplication guard conservatively bounds both inputs to 30 bits;
larger fixnums use the existing exact numeric operation, never wrapped math."
  (if (not (memq opcode '(83 84 85 86 87 88 89 90 91 92 95))) fallback
    (let* ((binary (not (memq opcode '(83 84 91))))
           (expression (pcase opcode
                         (83 '(- fast_a 1)) (84 '(+ fast_a 1))
                         (91 '(- 0 fast_a)) (90 '(- fast_a fast_b))
                         (92 '(+ fast_a fast_b)) (95 '(* fast_a fast_b))
                         (85 '(= fast_a fast_b)) (86 '(> fast_a fast_b))
                         (87 '(< fast_a fast_b)) (88 '(<= fast_a fast_b))
                         (89 '(>= fast_a fast_b))))
           (comparison (memq opcode '(85 86 87 88 89)))
           (bounds (if (= opcode 95) 1073741824 2305843009213693951))
           (lower (if (= opcode 95) -1073741824 -2305843009213693952))
           (store `(progn (ptr-write-u64 fast_out 0 ,(if comparison '(if fast_value 1 0) 2))
                          (ptr-write-u64 fast_out 8 ,(if comparison 0 'fast_value))
                          (ptr-write-u64 fast_out 16 0) (ptr-write-u64 fast_out 24 0) ,success)))
      `(let* ((fast_left (extern-call nl_root_pin_slot_v2 env ticket ,(car inputs) 0 0 0))
              (fast_right ,(if binary `(extern-call nl_root_pin_slot_v2 env ticket ,(cadr inputs) 0 0 0) 'fast_left))
              (fast_out (extern-call nl_root_pin_slot_v2 env ticket ,output 0 0 0)))
         (if (or (= fast_left 0) (or (= fast_right 0) (= fast_out 0))) 2
           ;; Payload reads are safe for any authenticated full Sexp slot.
           ;; Only tagged, bounded inputs can evaluate the arithmetic itself.
           (let* ((fast_a (ptr-read-u64 fast_left 8)) (fast_b (ptr-read-u64 fast_right 8))
                  (fast_valid (and (= (ptr-read-u64 fast_left 0) 2)
                                   (and (= (ptr-read-u64 fast_right 0) 2)
                                        (and (>= fast_a ,lower)
                                             (and (<= fast_a ,bounds)
                                                  (and (>= fast_b ,lower) (<= fast_b ,bounds)))))))
                  (fast_value (if fast_valid ,expression 0)))
             (if ,(if comparison 'fast_valid
                    '(and fast_valid (and (>= fast_value -2305843009213693952)
                               (<= fast_value 2305843009213693951))))
                 ,store ,fallback)))))))

(defun nelisp-native-funcall-v2-value-form (opcode inputs output success fallback)
  "Lower frozen VM predicates on tagged values, with the genuine fallback.
This is general compiler lowering, not a new evaluator primitive. EQ's
symbol and special boxed/string cases use the existing runtime implementation."
  (if (not (memq opcode '(57 58 59 60 61 63 167 168))) fallback
    (let* ((binary (= opcode 61))
           (test (pcase opcode
                   (57 '(or (= value_tag 0) (or (= value_tag 1) (or (= value_tag 4) (= value_tag 16)))))
                   (58 '(= value_tag 7))
                   (59 '(or (= value_tag 5) (or (= value_tag 6) (or (= value_tag 14) (= value_tag 15)))))
                   (60 '(or (= value_tag 0) (= value_tag 7)))
                   (63 '(= value_tag 0))
                   (167 '(or (= value_tag 2) (or (= value_tag 3) (= value_tag 13))))
                   (168 '(or (= value_tag 2) (= value_tag 13)))
                   (61 '(and (= value_tag other_tag)
                             (= (ptr-read-u64 value_left 8) (ptr-read-u64 value_right 8))))))
           (safe (if binary
                     '(or (/= value_tag other_tag)
                          (or (<= value_tag 2)
                              ;; Tag 4 stores symbol identity outside offset 8.
                              ;; Equal payload words do not mean equal names.
                              (or (= value_tag 7)
                                      (or (= value_tag 8)
                                          (or (= value_tag 12)
                                              (or (= value_tag 16)
                                                  (or (= value_tag 17) (= value_tag 18))))))))
                   '(= value_tag value_tag))))
      `(let* ((value_left (extern-call nl_root_pin_slot_v2 env ticket ,(car inputs) 0 0 0))
              (value_right ,(if binary `(extern-call nl_root_pin_slot_v2 env ticket ,(cadr inputs) 0 0 0) 'value_left))
              (value_out (extern-call nl_root_pin_slot_v2 env ticket ,output 0 0 0)))
         (if (or (= value_left 0) (or (= value_right 0) (= value_out 0))) 2
           (let ((value_tag (ptr-read-u64 value_left 0))
                 (other_tag (ptr-read-u64 value_right 0)))
             (if ,safe
                 (progn (ptr-write-u64 value_out 0 (if ,test 1 0))
                        (ptr-write-u64 value_out 8 0) (ptr-write-u64 value_out 16 0)
                        (ptr-write-u64 value_out 24 0) ,success)
               ,fallback)))))))

(defun nelisp-native-funcall-v2-emit (operation function inputs continuation &optional copy-plan)
  "Stage canonical operands with one shared continuation for fast/slow paths."
  (let* ((copy (if copy-plan
                   (lambda (sources destinations body)
                     (nelisp-native-frame-v2-bank-copy-emit copy-plan sources destinations body))
                 #'nelisp-native-funcall-v2-copy-form))
         (roots (plist-get operation :staging-roots))
         (result (plist-get operation :result-root))
         (status (intern (format "f1_status_%d" (plist-get operation :pc))))
         (slow (funcall copy inputs roots
                `(let ((,status (extern-call ,(if (plist-get operation :poll) 'nl_native_poll_v2 'nl_native_funcall_v2)
                                             env ticket ,function ,(or (car roots) 1) ,(length inputs) ,result)))
                   (if (= ,status 0)
                       ,(funcall copy (list result) (list (plist-get operation :output-root)) 0)
                     ,status)))))
    `(let ((,status ,(nelisp-native-funcall-v2-value-form
                     (plist-get operation :bytecode-opcode) inputs
                     (plist-get operation :output-root) 0
                     (nelisp-native-funcall-v2-fixnum-form
                     (plist-get operation :bytecode-opcode) inputs
                     (plist-get operation :output-root) 0 slow))))
       (if (= ,status 0) ,continuation ,status))))
(defun nelisp-native-funcall-v2-emit-list (operation function inputs continuation &optional compact)
  "Build a long list through frozen CONS using two reusable argument roots.
All source values remain rooted, and the accumulator is published after each
allocation.  Flat status guards bound form depth independently of list length."
  (let* ((status (intern (format "f1_list_status_%d" (plist-get operation :pc))))
         (output (plist-get operation :output-root))
         (runs nil) (calls nil)
         (finish (if (plist-get operation :apply-function-root)
                     (nelisp-native-funcall-v2-emit
                      operation (plist-get operation :apply-function-root)
                      (list (plist-get operation :target-function-root) output) continuation)
                   continuation)))
    ;; Consecutive aliases have identical operands but distinct allocations.
    ;; The shared CFG emitter can keep their bounded repetitions as real loops;
    ;; the older structured emitter retains an equivalent flat expansion.
    (dolist (input (reverse inputs))
      (if (and compact runs (equal input (caar runs)))
          (setcdr (car runs) (1+ (cdar runs)))
        (push (cons input 1) runs)))
    (dolist (run (nreverse runs))
      (let ((body `(if (= ,status 0)
                       (setq ,status
                             ,(nelisp-native-funcall-v2-emit
                               operation function (list (car run) output) 0))
                     ,status)))
        (push (if (> (cdr run) 1) `(cfg-repeat ,(cdr run) ,status ,body) body) calls)))
    (nelisp-native-funcall-v2-copy-form
     (list (plist-get operation :nil-root)) (list output)
     `(let ((,status 0))
        (progn ,@(nreverse calls) (if (= ,status 0) ,finish ,status))))))
(provide 'nelisp-native-funcall-v2)
