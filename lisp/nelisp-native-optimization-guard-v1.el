;;; nelisp-native-optimization-guard-v1.el --- Root-authenticated arithmetic guard -*- lexical-binding: t; -*-
(require 'nelisp-native-arithmetic-v2)

(defconst nelisp-native-optimization-guard-v1--arithmetic
  '(defun nl_native_add_guard_v1 (env ticket left-index right-index output-index exit-index)
     (if (or (<= left-index 0) (<= right-index 0) (<= output-index 0) (<= exit-index 0)
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
         (if (or (= left 0) (= right 0) (= output 0) (= kind 0) (= tag 0) (= value 0))
             2
           (if (and (= (ptr-read-u64 left 0) 2) (= (ptr-read-u64 right 0) 2))
               (let* ((a (ptr-read-s64 left 8)) (b (ptr-read-s64 right 8)))
                 (if (and (>= a -2305843009213693952) (<= a 2305843009213693951)
                          (>= b -2305843009213693952) (<= b 2305843009213693951))
                     ;; Two GNU fixnums sum within signed i64 before checking
                     ;; whether the result itself remains a GNU fixnum.
                     (let ((sum (+ a b)))
                       (if (and (>= sum -2305843009213693952) (<= sum 2305843009213693951))
                           (seq (ptr-write-u64 output 0 2) (ptr-write-u64 output 8 sum)
                                (ptr-write-u64 output 16 0) (ptr-write-u64 output 24 0) 0)
                         (nl_native_add_v2 env ticket left-index right-index output-index exit-index)))
                   (nl_native_add_v2 env ticket left-index right-index output-index exit-index)))
             (nl_native_add_v2 env ticket left-index right-index output-index exit-index))))))
  "Artifact-local guard; the unchanged numeric gateway owns all slow exits.
Every slot is authenticated before memory reads. Complete operand reads
precede output writes, so output/input aliasing preserves addition semantics.")

(let ((owner-checker nil) (owners nil)
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error))
      (provider-checker (symbol-function 'nelisp-native-arithmetic-v2-owner-valid-p)))
(defun nelisp-native-optimization-guard-v1-owner-valid-p ()
  "Check module-load original identities without invoking snapshot helpers."
  (let ((remaining owners))
    (while remaining
      (let ((entry (funcall head remaining)))
        (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
            nil (funcall reject "optimization-guard: source owner changed")))
      (setq remaining (funcall tail remaining)))
    (funcall provider-checker)))

(defun nelisp-native-optimization-guard-v1--copy (value active budget depth)
  "Copy bounded symbol/integer DSL data without exposing mutable source conses."
  (setcar budget (1- (car budget)))
  (if (or (< (car budget) 0) (> depth 64)) (error "optimization-guard: source bound exceeded"))
  (cond ((consp value)
         (if (memq value active) (error "optimization-guard: cyclic source"))
         (let ((next (cons value active)))
           (cons (nelisp-native-optimization-guard-v1--copy (car value) next budget (1+ depth))
                 (nelisp-native-optimization-guard-v1--copy (cdr value) next budget depth))))
        ((or (symbolp value) (integerp value)) value)
        (t (error "optimization-guard: unsupported source atom"))))

(defun nelisp-native-optimization-guard-v1-source (mode)
  "Return the genuine slow provider, adding the fixed arithmetic guard for ON.
The compiler must authenticate root origins and the complete returned source
and dependency context before publishing an executable artifact."
  (funcall owner-checker)
  (unless (memq mode '(on off)) (error "optimization-guard: unknown mode"))
  (let ((source (nelisp-native-arithmetic-v2-source)))
    (if (eq mode 'off) source
      (append source
              (list (nelisp-native-optimization-guard-v1--copy
                     nelisp-native-optimization-guard-v1--arithmetic nil (list 2048) 0))))))

(defun nelisp-native-optimization-guard-v1-descriptor (mode)
  "Describe the fixed six-root gateway ABI and its guard facts."
  (funcall owner-checker)
  (unless (memq mode '(on off)) (error "optimization-guard: unknown mode"))
  (list :name (if (eq mode 'on) "nl_native_add_guard_v1" "nl_native_add_v2")
        :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64
        :status '(0 1 2) :root-authentication 'nl_root_pin_slot_v2
        :sexp-size 32 :integer-tag 2 :payload-offset 8
        :fixnum-min -2305843009213693952 :fixnum-max 2305843009213693951
        :native-call-guard 'pending-owner-proof))

(defun nelisp-native-optimization-guard-v1-call-select (symbol expected)
  "Resolve SYMBOL once and describe an identity guard decision.
This is a decision component, not an authenticated native call capability.
An executable caller must additionally bind EXPECTED to its genuine compiled
artifact proof and retain the resolved function as a live root."
  (funcall owner-checker)
  (if (not (and (symbolp symbol) symbol (not (eq symbol t))
                (not (keywordp symbol)) (functionp expected)))
      (list :route 'refused)
    (let ((observed (symbol-function symbol)))
      (list :route (if (eq observed expected) 'fast 'slow) :callee observed
            :requires-artifact-owner-proof t))))

(defun nelisp-native-optimization-guard-v1-dependency-context ()
  "Return copied provider, source and numeric facts for compiler sealing."
  (funcall owner-checker)
  (nelisp-native-optimization-guard-v1--dependency-context
   (nelisp-native-arithmetic-v2-dependency-context)))

(defun nelisp-native-optimization-guard-v1--dependency-context (provider)
  "Bind module helpers, interpreted macro owners, source and numeric facts."
  (funcall owner-checker)
  (vector
   (mapcar #'symbol-function
           '(nelisp-native-optimization-guard-v1--copy
             nelisp-native-optimization-guard-v1-source
             nelisp-native-optimization-guard-v1-descriptor
             nelisp-native-optimization-guard-v1-call-select
             nelisp-native-optimization-guard-v1-dependency-context
             nelisp-native-optimization-guard-v1-owner-valid-p
             setcar 1- car cdr cons memq symbolp integerp error 1+ < >
             eq append list symbol-function functionp keywordp not and unless cond mapcar vector))
   provider
   ;; PROVIDER is an owned copy. Reuse its slow source rather than asking
   ;; the provider to validate and copy that identical source again.
   (append (aref provider 7)
           (list (nelisp-native-optimization-guard-v1--copy
                  nelisp-native-optimization-guard-v1--arithmetic nil (list 2048) 0)))
   (nelisp-native-optimization-guard-v1-descriptor 'on)))

(setq owner-checker (funcall lookup 'nelisp-native-optimization-guard-v1-owner-valid-p)
      owners
      (mapcar (lambda (name) (cons name (funcall lookup name)))
              '(nelisp-native-optimization-guard-v1-owner-valid-p
                nelisp-native-optimization-guard-v1--copy
                nelisp-native-optimization-guard-v1-source
                nelisp-native-optimization-guard-v1-descriptor
                nelisp-native-optimization-guard-v1-call-select
                nelisp-native-optimization-guard-v1-dependency-context
                nelisp-native-optimization-guard-v1--dependency-context
                nelisp-native-arithmetic-v2-owner-valid-p
                symbol-function eq car cdr cons setcar list 1- 1+ memq
                symbolp integerp error < > and or cond unless not
                append mapcar vector functionp keywordp aref)))
)

(provide 'nelisp-native-optimization-guard-v1)
