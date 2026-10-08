;;; nelisp-native-arithmetic-v2.el --- Rooted builtin numeric adapter -*- lexical-binding: t; -*-
(require 'cl-lib)
(defconst nelisp-native-arithmetic-v2--plus-body
  '(let* ((f (wf_any_float_arith args)))
     (if (= f 2)
         (bf_wrong_type_number_or_marker (wf_first_non_number_or_bignum args))
       (if (= f 1)
           ;; Copy the fold's final Float Sexp.  Its existing external-call
           ;; classifier requires bits-to-f64 to wrap refs inside the helpers.
           (let* ((sc (alloc-bytes 32 8)))
             (seq (wf_fsum args 0 sc) (wf_copy32 out sc)))
         (wf_sum args 0 out))))
  "Exact source-owned builtin addition body, shared with the standalone reader.")
(defconst nelisp-native-arithmetic-v2--direct-source
  '(seq
    (defun nl_native_add_v2_copy (to from)
      (seq (ptr-write-u64 to 0 (ptr-read-u64 from 0))
           (ptr-write-u64 to 8 (ptr-read-u64 from 8))
           (ptr-write-u64 to 16 (ptr-read-u64 from 16))
           (ptr-write-u64 to 24 (ptr-read-u64 from 24)) 0))
    (defun nl_native_add_v2_exit (kind tag value)
      (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
             (flag (ptr-read-u64 (+ base 16) 0)))
        (if (or (= flag 1) (= flag 2))
            (seq (nl_native_add_v2_copy tag (+ base 24))
                 (nl_native_add_v2_copy value (+ base 56))
                 ;; Exact Sexp::Int store from the genuine wf_write_int owner.
                 (ptr-write-u64 kind 0 2)
                 (ptr-write-u64 kind 8 flag)
                 (ptr-write-u64 kind 16 0)
                 (ptr-write-u64 kind 24 0)
                 ;; Ownership transfers to the authenticated outer roots.
                 ;; Lisp resumes the captured exit after native return.
                 (ptr-write-u64 (+ base 16) 0 0) 1)
          2)))
    (defun nl_native_add_v2_inner (env token left right output kind tag value)
      (let* ((nil-slot (nl_root_pin_reserve_v2 env token))
             (first (nl_root_pin_reserve_v2 env token))
             (second (nl_root_pin_reserve_v2 env token))
             (tail (nl_root_pin_reserve_v2 env token))
             (args (nl_root_pin_reserve_v2 env token))
             (result (nl_root_pin_reserve_v2 env token)))
        (if (or (= nil-slot 0) (= first 0) (= second 0)
                (= tail 0) (= args 0) (= result 0))
            2
          (seq
             ;; Every temporary Sexp is rooted before either cons allocation.
             (nl_native_add_v2_copy first left)
             (nl_native_add_v2_copy second right)
             (nelisp_cons_construct second nil-slot tail)
             (nelisp_cons_construct first tail args)
             (nl_native_add_v2_plus args result)
             ;; Read the current arena only after the allocating numeric body.
             (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
                    (flag (ptr-read-u64 (+ base 16) 0)))
               (if (= flag 0)
                   (seq (nl_native_add_v2_copy output result) 0)
                 (nl_native_add_v2_exit kind tag value)))))))
    (defun nl_native_add_v2 (env ticket left-index right-index output-index exit-index)
      (if (or (<= left-index 0) (<= right-index 0) (<= output-index 0)
              (<= exit-index 0)
              (and (>= left-index exit-index) (< left-index (+ exit-index 3)))
              (and (>= right-index exit-index) (< right-index (+ exit-index 3)))
              (and (>= output-index exit-index) (< output-index (+ exit-index 3))))
          2
        (let* ((left (nl_root_pin_slot_v2 env ticket left-index))
               (right (nl_root_pin_slot_v2 env ticket right-index))
               (output (nl_root_pin_slot_v2 env ticket output-index))
               (kind (nl_root_pin_slot_v2 env ticket exit-index))
               (tag (nl_root_pin_slot_v2 env ticket (+ exit-index 1)))
               (value (nl_root_pin_slot_v2 env ticket (+ exit-index 2))))
          (if (or (= left 0) (= right 0) (= output 0)
                  (= kind 0) (= tag 0) (= value 0))
              2
            (let* ((nested (nl_root_pin_begin_v2 env)))
              (if (= nested 0) 2
                (let* ((status (nl_native_add_v2_inner
                                env nested left right output kind tag value))
                       (ended (nl_root_pin_end_v2 env nested)))
                  (if (= ended 1) status 2)))))))))
  "Native source directly using the source-owned builtin binary addition body.
Status 0 writes OUTPUT; 1 captures KIND/TAG/VALUE (signal=1, throw=2);
status 2 refuses malformed root requests. All slots remain GC-visible.")
(defconst nelisp-native-arithmetic-v2--source
  '(seq
    (defun nl_native_add_v2_copy (to from)
      (seq (ptr-write-u64 to 0 (ptr-read-u64 from 0))
           (ptr-write-u64 to 8 (ptr-read-u64 from 8))
           (ptr-write-u64 to 16 (ptr-read-u64 from 16))
           (ptr-write-u64 to 24 (ptr-read-u64 from 24)) 0))
    (defun nl_native_add_v2_exit (kind tag value)
      (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
             (flag (ptr-read-u64 (+ base 16) 0)))
        (if (or (= flag 1) (= flag 2))
            (seq (nl_native_add_v2_copy tag (+ base 24))
                 (nl_native_add_v2_copy value (+ base 56))
                 ;; Exact Sexp::Int store from the genuine wf_write_int owner.
                 (ptr-write-u64 kind 0 2)
                 (ptr-write-u64 kind 8 flag)
                 (ptr-write-u64 kind 16 0)
                 (ptr-write-u64 kind 24 0)
                 ;; Ownership transfers to the authenticated outer roots.
                 ;; Lisp resumes the captured exit after native return.
                 (ptr-write-u64 (+ base 16) 0 0) 1)
          2)))
    (defun nl_native_add_v2_inner (env token left right output kind tag value)
      (let* ((nil-slot (nl_root_pin_reserve_v2 env token))
             (builtin (nl_root_pin_reserve_v2 env token))
             (plus (nl_root_pin_reserve_v2 env token))
             (inner (nl_root_pin_reserve_v2 env token))
             (function (nl_root_pin_reserve_v2 env token))
             (first (nl_root_pin_reserve_v2 env token))
             (second (nl_root_pin_reserve_v2 env token))
             (result (nl_root_pin_reserve_v2 env token)))
        (if (or (= nil-slot 0) (= builtin 0) (= plus 0) (= inner 0)
                (= function 0) (= first 0) (= second 0) (= result 0))
            2
          (let* ((name (alloc-bytes 8 1)))
            (seq
             ;; All temporary Sexp slots are precise roots before allocation.
             (nl_native_add_v2_copy first left)
             (nl_native_add_v2_copy second right)
             (ptr-write-u64 name 0 31078196194145634)
             (nl_alloc_symbol name 7 builtin)
             (ptr-write-u64 name 0 43)
             (nl_alloc_symbol name 1 plus)
             ;; Symbol construction copies the bytes on both intern paths.
             (dealloc-bytes name 8 1)
             (nelisp_cons_construct plus nil-slot inner)
             (nelisp_cons_construct builtin inner function)
             ;; The builtin wrapper bypasses mutable '+' function cells,
             ;; matching byte-plus rather than general symbol-call semantics.
             (let* ((status (wf_bytecode_call_gateway
                             env function nil-slot 5 2 result)))
               (if (= status 0)
                   (seq (nl_native_add_v2_copy output result) 0)
                 (if (= status 1) (nl_native_add_v2_exit kind tag value) 2))))))))
    (defun nl_native_add_v2 (env ticket left-index right-index output-index exit-index)
      (if (or (<= left-index 0) (<= right-index 0) (<= output-index 0)
              (<= exit-index 0)
              (and (>= left-index exit-index) (< left-index (+ exit-index 3)))
              (and (>= right-index exit-index) (< right-index (+ exit-index 3)))
              (and (>= output-index exit-index) (< output-index (+ exit-index 3))))
          2
        (let* ((left (nl_root_pin_slot_v2 env ticket left-index))
               (right (nl_root_pin_slot_v2 env ticket right-index))
               (output (nl_root_pin_slot_v2 env ticket output-index))
               (kind (nl_root_pin_slot_v2 env ticket exit-index))
               (tag (nl_root_pin_slot_v2 env ticket (+ exit-index 1)))
               (value (nl_root_pin_slot_v2 env ticket (+ exit-index 2))))
          (if (or (= left 0) (= right 0) (= output 0)
                  (= kind 0) (= tag 0) (= value 0))
              2
            (let* ((nested (nl_root_pin_begin_v2 env)))
              (if (= nested 0) 2
                (let* ((status (nl_native_add_v2_inner
                                env nested left right output kind tag value))
                       (ended (nl_root_pin_end_v2 env nested)))
                  (if (= ended 1) status 2)))))))))
  "Native source using the existing rooted VM call gateway for binary addition.
Status 0 writes OUTPUT; 1 captures KIND/TAG/VALUE (signal=1, throw=2);
status 2 refuses malformed root requests. All slots remain GC-visible.")
(let ((owners nil) (owner-checker nil) (canonical-plus nil) (canonical-direct nil)
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error)))
(defun nelisp-native-arithmetic-v2-owner-valid-p ()
  "Check original provider owners without executing any snapshot helper."
  (let ((remaining owners))
    (while remaining
      (let ((entry (funcall head remaining)))
        (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
            nil (funcall reject "native-arithmetic: source owner changed")))
      (setq remaining (funcall tail remaining)))
    t))
(defun nelisp-native-arithmetic-v2--copy-node (item ancestors depth budget)
  "Copy one source node using an explicit bounded traversal state."
  (setcar budget (1- (car budget)))
  (if (or (< (car budget) 0) (> depth 64))
      (error "native-arithmetic: source bound exceeded"))
  (cond ((consp item)
         (if (memq item ancestors) (error "native-arithmetic: cyclic source"))
         (let ((next (cons item ancestors)))
           (cons (nelisp-native-arithmetic-v2--copy-node (car item) next (1+ depth) budget)
                 (nelisp-native-arithmetic-v2--copy-node (cdr item) next (1+ depth) budget))))
        ((stringp item)
         (if (or (> (length item) 256)
                 (text-properties-at 0 item)
                 (< (or (next-property-change 0 item) (length item)) (length item)))
             (error "native-arithmetic: noncanonical source string"))
         (copy-sequence item))
        ((or (symbolp item) (integerp item)) item)
        (t (error "native-arithmetic: malformed source atom"))))
(defun nelisp-native-arithmetic-v2--snapshot (value)
  "Copy bounded source data without runtime macro expansion."
  (nelisp-native-arithmetic-v2--copy-node value nil 0 (list 4096)))
(defun nelisp-native-arithmetic-v2-source ()
  "Return an independent copy of the canonical native slow gateway source."
  (funcall owner-checker)
  (nelisp-native-arithmetic-v2--snapshot nelisp-native-arithmetic-v2--source))
(defun nelisp-native-arithmetic-v2-direct-source ()
  "Return bounded direct numeric compilation data, without runtime admission."
  (funcall owner-checker)
  (let ((source (nelisp-native-arithmetic-v2--snapshot canonical-direct)))
    (if (and (consp source) (eq (car source) 'seq))
        (cons 'seq (cons (list 'defun 'nl_native_add_v2_plus (list 'args 'out)
                              (nelisp-native-arithmetic-v2-plus-body)) (cdr source)))
      source)))
(defun nelisp-native-arithmetic-v2-plus-body ()
  "Return a bounded independent copy of the genuine builtin addition body."
  (funcall owner-checker)
  (nelisp-native-arithmetic-v2--snapshot canonical-plus))
(defun nelisp-native-arithmetic-v2-descriptor ()
  "Return the planner/emitter boundary for rooted generic numeric addition."
  (funcall owner-checker)
  (nelisp-native-arithmetic-v2--snapshot
   '(:name "nl_native_add_v2" :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64)
    :return u64 :calling-convention sysv-amd64
    :status-success 0 :status-exit 1 :status-refused 2 :exit-root-count 3
    :signal-kind 1 :throw-kind 2 :operation add :byte-opcode 92
    :exit-kind-layout (:bytes 32 :tag 2 :tag-offset 0 :payload-offset 8 :zero-offsets (16 24)))))
(defun nelisp-native-arithmetic-v2-runtime-imports ()
  "Return source-owned runtime imports, excluding all artifact-local functions."
  (funcall owner-checker)
  (nelisp-native-arithmetic-v2--snapshot
   '((:name "nl_arena_base" :kind data :size 8)
     (:name "nl_alloc_symbol" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "nelisp_cons_construct" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "nl_root_pin_begin_v2" :kind func :arity 1 :params (u64) :return u64)
     (:name "nl_root_pin_end_v2" :kind func :arity 2 :params (u64 u64) :return u64)
     (:name "nl_root_pin_reserve_v2" :kind func :arity 2 :params (u64 u64) :return u64)
     (:name "nl_root_pin_slot_v2" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "wf_bytecode_call_gateway" :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64))))
(defun nelisp-native-arithmetic-v2-direct-runtime-imports ()
  "Return direct compilation imports requiring separately certified admission."
  (funcall owner-checker)
  (nelisp-native-arithmetic-v2--snapshot
   '((:name "nl_arena_base" :kind data :size 8)
     (:name "nelisp_cons_construct" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "nl_root_pin_begin_v2" :kind func :arity 1 :params (u64) :return u64)
     (:name "nl_root_pin_end_v2" :kind func :arity 2 :params (u64 u64) :return u64)
     (:name "nl_root_pin_reserve_v2" :kind func :arity 2 :params (u64 u64) :return u64)
     (:name "nl_root_pin_slot_v2" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "wf_any_float_arith" :kind func :arity 1 :params (u64) :return u64)
     (:name "wf_first_non_number_or_bignum" :kind func :arity 1 :params (u64) :return u64)
     (:name "wf_fsum" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "wf_copy32" :kind func :arity 2 :params (u64 u64) :return u64)
     (:name "wf_sum" :kind func :arity 3 :params (u64 u64 u64) :return u64)
     (:name "bf_wrong_type_number_or_marker" :kind func :arity 1 :params (u64) :return u64))))
(defun nelisp-native-arithmetic-v2-dependency-context ()
  "Return opaque owner identities and a copied canonical source snapshot."
  (funcall owner-checker)
  (vector (symbol-function 'nelisp-native-arithmetic-v2-source)
          (symbol-function 'nelisp-native-arithmetic-v2-descriptor)
          (symbol-function 'nelisp-native-arithmetic-v2-runtime-imports)
          (symbol-function 'nelisp-native-arithmetic-v2-dependency-context)
          (symbol-function 'nelisp-native-arithmetic-v2--snapshot)
          (symbol-function 'nelisp-native-arithmetic-v2--copy-node)
          (mapcar #'symbol-function
                  '(car cdr cons list setcar memq copy-sequence text-properties-at next-property-change
                    1- < > consp 1+ stringp length symbolp integerp error
                    vector mapcar symbol-function))
          (nelisp-native-arithmetic-v2-source)
          (nelisp-native-arithmetic-v2-descriptor)
          (nelisp-native-arithmetic-v2-runtime-imports)
          (symbol-function 'nelisp-native-arithmetic-v2-plus-body)
          (nelisp-native-arithmetic-v2-plus-body)
          (symbol-function 'nelisp-native-arithmetic-v2-direct-source)
          (symbol-function 'nelisp-native-arithmetic-v2-direct-runtime-imports)
          (nelisp-native-arithmetic-v2-direct-source)
          (nelisp-native-arithmetic-v2-direct-runtime-imports)))
;; Capture independent bounded data before exposing the new accessors.  Public
;; constant rebinding or mutation must never alter their compilation snapshots.
(setq canonical-plus (nelisp-native-arithmetic-v2--snapshot nelisp-native-arithmetic-v2--plus-body)
      canonical-direct (nelisp-native-arithmetic-v2--snapshot nelisp-native-arithmetic-v2--direct-source))
(setq owner-checker (funcall lookup 'nelisp-native-arithmetic-v2-owner-valid-p)
      owners (mapcar (lambda (name) (cons name (funcall lookup name)))
                    '(nelisp-native-arithmetic-v2-owner-valid-p
                      nelisp-native-arithmetic-v2-source nelisp-native-arithmetic-v2-descriptor
                      nelisp-native-arithmetic-v2-runtime-imports nelisp-native-arithmetic-v2-dependency-context
                      nelisp-native-arithmetic-v2--snapshot nelisp-native-arithmetic-v2--copy-node
                      nelisp-native-arithmetic-v2-plus-body
                      nelisp-native-arithmetic-v2-direct-source
                      nelisp-native-arithmetic-v2-direct-runtime-imports
                      car cdr cons list setcar memq copy-sequence text-properties-at next-property-change
                      1- < > consp 1+ stringp length symbolp integerp error vector mapcar
                      symbol-function eq and or cond)))
)
(provide 'nelisp-native-arithmetic-v2)
