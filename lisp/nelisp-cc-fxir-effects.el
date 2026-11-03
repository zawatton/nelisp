;;; nelisp-cc-fxir-effects.el --- Canonical FXIR effect-class table -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; One table that gives every FXIR op keyword handled by
;; `nelisp-cc-fxir-analysis-row' an effect class (docs 231 and 234): may
;; allocate, may GC, may signal, may call Lisp, and nonlocal exit.  An op
;; that is not in the table is UNKNOWN and gets the conservative class, so
;; later passes can read one answer without a second `pcase'.
;;
;; Flags describe the node itself and ignore its children.  The flags are
;; derived from the diagnostic effect keywords the analysis pass already
;; emits for the op:
;;   :unknown-call          -> :may-call-lisp
;;   :allocation-or-gc      -> :may-allocate and :may-gc
;;   :poll-or-gc            -> :may-gc
;;   :signal-or-reentry     -> :may-signal, :may-call-lisp, :may-allocate
;;                            and :may-gc (reentry can run Lisp: debugger,
;;                            signal hooks, variable watchers on binding)
;;   :unproved-primitive    -> :may-allocate, :may-gc and :may-signal
;;                            (conservative: the inline op is not proved)
;;   :nonlocal-exit         -> :nonlocal-exit
;;   :mutable-cell-read, :call-observation, :dynamic-binding
;;                          -> no flag of their own
;; Every current diagnostic list is operand independent, so the table holds
;; the whole list; analysis keeps only operand validation.  `:store' and
;; `:varbind' are let-binding kinds, not node ops, but analysis dispatches
;; on them the same way, so they are listed too.  `:value' is the already
;; lowered value wrapper used only inside `nelisp-cc-fxir-native--lower'.

;;; Code:

(defconst nelisp-cc-fxir-effects-table
  '((:nil  :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:slot :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:const :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
            :nonlocal-exit nil :diagnostic nil)
    (:varref :may-allocate t :may-gc t :may-signal t :may-call-lisp t
             :nonlocal-exit nil :diagnostic (:mutable-cell-read :signal-or-reentry))
    (:alias :may-allocate t :may-gc t :may-signal t :may-call-lisp t
            :nonlocal-exit nil :diagnostic (:mutable-cell-read :signal-or-reentry))
    (:setslot :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
              :nonlocal-exit nil :diagnostic nil)
    (:call :may-allocate t :may-gc t :may-signal t :may-call-lisp t
           :nonlocal-exit nil
           :diagnostic (:unknown-call :allocation-or-gc :signal-or-reentry :call-observation))
    (:inline :may-allocate t :may-gc t :may-signal t :may-call-lisp t
             :nonlocal-exit nil :diagnostic (:unproved-primitive :signal-or-reentry))
    (:value :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
            :nonlocal-exit nil :diagnostic nil)
    (:seq  :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:prog1 :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
            :nonlocal-exit nil :diagnostic nil)
    (:and  :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:or   :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:if   :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:while :may-allocate nil :may-gc t :may-signal nil :may-call-lisp nil
            :nonlocal-exit nil :diagnostic (:poll-or-gc))
    (:let  :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
           :nonlocal-exit nil :diagnostic nil)
    (:store :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
            :nonlocal-exit nil :diagnostic nil)
    (:varbind :may-allocate t :may-gc t :may-signal t :may-call-lisp t
              :nonlocal-exit nil :diagnostic (:dynamic-binding :signal-or-reentry))
    (:catch :may-allocate t :may-gc t :may-signal t :may-call-lisp t
            :nonlocal-exit t :diagnostic (:nonlocal-exit :signal-or-reentry))
    (:throw :may-allocate t :may-gc t :may-signal t :may-call-lisp t
            :nonlocal-exit t :diagnostic (:nonlocal-exit :signal-or-reentry))
    (:unwind-protect :may-allocate t :may-gc t :may-signal t :may-call-lisp t
                     :nonlocal-exit t :diagnostic (:nonlocal-exit :signal-or-reentry))
    (:condition-case :may-allocate t :may-gc t :may-signal t :may-call-lisp t
                     :nonlocal-exit t :diagnostic (:nonlocal-exit :signal-or-reentry)))
  "Alist of FXIR op keyword to its effect-class plist.
Plist keys: :may-allocate :may-gc :may-signal :may-call-lisp :nonlocal-exit
(booleans) and :diagnostic, the exact list of diagnostic effect keywords
`nelisp-cc-fxir-analysis-row' emits for the op, in emission order.")

(defconst nelisp-cc-fxir-effects--unknown
  '(:may-allocate t :may-gc t :may-signal t :may-call-lisp t :nonlocal-exit t
    :diagnostic (:unknown-call :allocation-or-gc :signal-or-reentry) :unknown t)
  "Conservative effect class for an op that is not in the table.")

(defun nelisp-cc-fxir-effects-of (op)
  "Return the effect-class plist for op keyword OP.
An OP missing from `nelisp-cc-fxir-effects-table' gets the conservative
class with every flag t and `:unknown' t."
  (let ((entry (assq op nelisp-cc-fxir-effects-table)))
    (copy-sequence (if entry (cdr entry) nelisp-cc-fxir-effects--unknown))))

(defun nelisp-cc-fxir-effects-pure-p (op)
  "Return t when OP may not allocate, GC, signal, call Lisp or exit nonlocally.
An unknown OP is never pure."
  (let ((e (nelisp-cc-fxir-effects-of op)))
    (not (or (plist-get e :may-allocate) (plist-get e :may-gc)
             (plist-get e :may-signal) (plist-get e :may-call-lisp)
             (plist-get e :nonlocal-exit)))))

;; Stage 2 (doc 231): callee-refined :call effects.  The table lists only
;; callees for which `nelisp-cc-fxir-native--inline-fast' already emits an
;; inline arm built from bit tests and arithmetic (no out-of-line helper
;; call), so the inline part cannot signal.  Two shapes exist:
;;  - total fast path (`eq'): the fast path decides every case and no generic
;;    call is emitted, so the node carries no may-* flag;
;;  - guard + generic fallback (the rest): the fast path answers 0 for a
;;    case it declines and the lowering then makes the generic call, which may
;;    call Lisp.  The may-* flags stay conservative; `:fast-path-pure' t
;;    records only that the inline part is pure.
(defconst nelisp-cc-fxir-effects-callee-table
  '((eq :arity 2 :may-allocate nil :may-gc nil :may-signal nil :may-call-lisp nil
        :nonlocal-exit nil :diagnostic nil :fast-path-pure t)
    (car :arity 1 :fast-path-pure t) (cdr :arity 1 :fast-path-pure t)
    (car-safe :arity 1 :fast-path-pure t) (cdr-safe :arity 1 :fast-path-pure t)
    (null :arity 1 :fast-path-pure t) (not :arity 1 :fast-path-pure t)
    (<= :arity 2 :fast-path-pure t) (>= :arity 2 :fast-path-pure t)
    (max :arity 2 :fast-path-pure t) (min :arity 2 :fast-path-pure t)
    ;; Guard (function-cell identity) + fixnum test, then generic fallback.
    (logand :arity 2 :fast-path-pure t))
  "Alist of callee symbol to a plist overriding the `:call' effect class.
Keys not listed in an entry are inherited from `:call' in
`nelisp-cc-fxir-effects-table'.  `:arity' is the argument count of the
inline arm in `nelisp-cc-fxir-native--inline-fast'.")

;; Guarded direct-subr callees.  Unlike `nelisp-cc-fxir-effects-callee-table'
;; these have no pure inline arm: the lowering calls the primitive's directcall
;; entry (`nl_directcall_invoke') after a function-cell guard, and the generic
;; call stays on the fallback edge.  Every effect flag therefore stays
;; conservative (`gethash'/`puthash' may call a user hash test; the allocating
;; ones may GC and signal), identical to the plain `:call' class, so
;; `nelisp-cc-fxir-effects-call-of' deliberately does not consult this table.
;; :id is the directcall id (`nelisp-standalone--directcall-specs'); :min/:max
;; bound the argument counts lowered directly (the runtime re-checks the Subr
;; arity, a mismatch takes the generic call).
(defconst nelisp-cc-fxir-effects-direct-subr-table
  '((gethash :id 101 :min 2 :max 3 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp t)
    (puthash :id 102 :min 3 :max 3 :may-allocate t :may-gc t :may-signal t :may-call-lisp t)
    (make-vector :id 113 :min 2 :max 2 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil)
    (make-string :id 117 :min 2 :max 3 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil)
    (vector :id 112 :min 0 :max 8 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil)
    (list :id 114 :min 0 :max 8 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil)
    (concat :id 115 :min 0 :max 8 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil)
    (substring :id 116 :min 1 :max 3 :may-allocate t :may-gc t :may-signal t :may-call-lisp nil))
  "Alist of callee symbol to its directcall id, lowered argument-count range
and (conservative) effect flags.")

(defun nelisp-cc-fxir-effects-direct-subr (callee argc)
  "Return CALLEE's direct-subr plist when ARGC is in its lowered range, else nil."
  (let ((e (and (symbolp callee) (assq callee nelisp-cc-fxir-effects-direct-subr-table))))
    (and e (integerp argc)
         (>= argc (plist-get (cdr e) :min)) (<= argc (plist-get (cdr e) :max))
         (cdr e))))

;; Doc 237 ownership-region operations.  They have no inline arm (so they do
;; not belong in `nelisp-cc-fxir-effects-callee-table', whose entries all
;; describe pure inline fast paths): the nine region operations run in native
;; code without allocating, collecting or calling Lisp; their only effect is a
;; signal on misuse.  `nelisp--region-stats' allocates its result list.
(defconst nelisp-cc-fxir-effects-region-table
  '((nelisp--region-open :arity 1 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-close :arity 1 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-move :arity 1 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-live-p :arity 1 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-size :arity 1 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-u8-ref :arity 2 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-u8-set :arity 3 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-i64-ref :arity 2 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-i64-set :arity 3 :may-allocate nil :may-gc nil :may-signal t :may-call-lisp nil)
    (nelisp--region-stats :arity 0 :may-allocate t :may-gc t :may-signal nil :may-call-lisp nil))
  "Alist of Doc 237 region primitive to its :call effect-class override.")

(defun nelisp-cc-fxir-effects-call-of (callee)
  "Return the effect-class plist for a :call to the statically known CALLEE.
CALLEE not in `nelisp-cc-fxir-effects-callee-table' (or not a symbol) gets
exactly `(nelisp-cc-fxir-effects-of :call)'."
  (let ((entry (and (symbolp callee)
                    (or (assq callee nelisp-cc-fxir-effects-callee-table)
                        (assq callee nelisp-cc-fxir-effects-region-table))))
        (base (nelisp-cc-fxir-effects-of :call)))
    (if (null entry) base
      (let ((o (cdr entry)))
        (while o (setq base (plist-put base (car o) (cadr o)) o (cddr o)))
        base))))

(provide 'nelisp-cc-fxir-effects)
;;; nelisp-cc-fxir-effects.el ends here
