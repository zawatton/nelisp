;;; nelisp-standalone-argframe.el --- Proven indexed argv transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep admission a structural proof over the current handler body. An
;; unrecognized use of the argument list must retain the ordinary list path.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-argframe-ineligible "Argument list requires ordinary transport")

(defconst nelisp-argframe-static-symbol-suffix
  '("atom" "stringp" "integerp" "numberp" "floatp" "vectorp" "listp" "nlistp"
    "nelisp--declare-local-special" "nelisp--intern-table-symbols"
    "nelisp--format-simple" "nelisp--native-subr-live-count" "fetch-bytecode"
    "nelisp--char-table-vector-bridge-p" "nelisp--string-mark-intervals"
    "nl-current-unix-time" "nl-unix-time-usec" "syscall-direct" "atomic-fetch-add"
    "ptr-read-u8" "ptr-write-u8" "ptr-read-u32" "ptr-write-u32"
    "ptr-read-u64" "ptr-write-u64" "alloc-bytes" "nelisp-process-async-ready-p"
    "nelisp--target-os-code" "nelisp--target-arch-code" "sin" "cos"
    "nelisp--native-symbol-addr" "nelisp--native-env"
    "nelisp--native-runtime-contract-word" "nl-ffi-call" "nelisp--native-pin-eq-slots"
    ;; subr1: the handlers `wf_arg_ptr' lowering admits (append-only).
    "subrp" "symbolp" "symbol-with-pos-p" "nelisp--symbol-position-op" "bare-symbol"
    "natnump" "sxhash-eq" "nl--read-int" "nl--int-token-p" "nl--read-float-token" "zerop"
    "symbol-name" "make-symbol" "nelisp--intern-lookup" "nelisp--native-subr-arity"
    "sxhash-equal" "sxhash-eql" "ash" "lognot" "nelisp--float-word-half"
    "nelisp-process-object-p" "nelisp-process-pid" "nelisp-process-status"
    "nelisp-process-exit-status" "nelisp-process-wait" "nelisp-process-delete"
    "tan" "asin" "acos" "exp" "sqrt" "ffloor" "fceiling" "ftruncate" "fround" "log" "atan"
    "nelisp--pow" "float" "logb" "nelisp--symbol-global-cell-p" "ptr-call-typed"
    "special-form-p" "nelisp--native-pin-copy" "nelisp--native-pin-copy-v2"
    ;; callpath: the handlers admitted once helper calls are opened (append-only).
    "makunbound" "featurep" "make-record" "recordp" "byte-code-function-p"
    "nelisp--raw-aref" "nelisp--raw-aset" "char-table-p" "nelisp--char-table-slots"
    "nelisp--char-table-set-slots" "char-table-subtype" "char-table-parent"
    "set-char-table-parent" "char-table-extra-slot" "set-char-table-extra-slot"
    "char-table-range" "set-char-table-range" "map-char-table"
    "nelisp--clear-string-bytes" "nelisp--equal-leaf" "nelisp--eval-source-string"
    "ptr-call" "thread-spawn" "nelisp-spin-until" "fork-spawn"
    ;; callpath2: intern's handlers, admitted once the obarray argument is
    ;; tested by the argument count (append-only).
    "intern" "intern-soft"
    ;; wordroute: the handlers the list-walk lowering admits without an older id
    ;; (append-only; nelisp--bv-binop/bv-count were admitted without one before).
    "put" "nelisp--bv-binop" "nelisp--bv-count"
    ;; applyfn2: the other walk-admitted handlers, now that the argv entries
    ;; have link units of their own (append-only).
    "nl--nthcdr" "byte-code" "require" "unibyte-string" "make-bool-vector" "bool-vector"
    "bool-vector-p" "record" "nelisp--native-subr-create" "make-byte-code" "elt"
    "symbol-plist" "setplist" "make-char-table" "error" "load" "nl-write-file"
    "nelisp--syscall-path" "nelisp--syscall-path2" "nelisp--syscall-path-int"
    "nelisp--syscall-stat-field" "nelisp--syscall-stat-buf" "nelisp--syscall-lstat-buf"
    "nelisp--syscall-readlink" "nelisp--syscall-readdir-names" "nelisp--readdir-scan-raw"
    "nelisp--syscall-utimes" "nelisp--syscall-statx-buf" "nelisp--syscall-unshare"
    "nelisp--syscall-mount" "nelisp--syscall-pivot-root" "nelisp--write-stdout-bytes"
    "nelisp--write-stderr-line" "nelisp--read-all-from-string-native" "read-stdin-bytes"
    "float-time" "logand" "logior" "logxor" "sort" "ptr-read-bytes" "ptr-write-bytes"
    "nelisp-process-call-process" "nelisp-process-start" "nelisp-process-start-process"
    "nelisp-process-read-output" "nelisp-process-write" "nelisp-process-close-stdin"
    "nelisp-process-poll" "nelisp-portable-syscall" "nelisp-socket-listen"
    "nelisp-socket-accept" "nelisp-socket-connect" "nelisp-socket-send"
    "nelisp-socket-recv" "nelisp-socket-close" "nelisp-socket-poll"
    "nelisp-socket-connect-error" "nelisp-thread-shared-alloc" "nelisp-thread-atomic-add"
    "nelisp-thread-atomic-read" "nelisp-thread-spawn" "nelisp-thread-join"
    "nelisp-thread-gc-inhibit" "nelisp--native-unbox-reference")
  "Frozen append-only IDs needed before the first dispatch is generated.
The full reader table later ratchets this suffix against structural admission;
no old symbol ID is moved and no late mutation changes a generated dispatch.")

(defvar nelisp-argframe-inline-helpers nil
  "callpath: helper definitions the lowering may open, or nil for none.
A hash table (or a function answering one) from a helper's name to its
\=(PARAMS . BODY), for helpers whose first parameter is `args'.  A handler arm
that hands its whole argument list to such a helper, `(HELPER args E...)',
is lowered as the helper's body with `args' read through the argument array,
the other parameters renamed and bound to E..., so a handler admitted by
structure no longer depends on how its body is split into helpers (GNU: a
SUBR's C function receives its arguments, however it delegates).  A helper
whose body is not provably indexed leaves the arm on the list route.")

(defconst nelisp-argframe-inline-depth 3
  "callpath: how many helper levels the lowering opens.")

(defconst nelisp-argframe-inline-max-size 600
  "callpath: the largest helper body (in conses) the lowering opens.")

(defvar nelisp-argframe--inline-stack nil
  "callpath: the helpers being opened, innermost first.")

(defun nelisp-argframe--size (form)
  (if (consp form) (+ 1 (nelisp-argframe--size (car form)) (nelisp-argframe--size (cdr form))) 0))

(defun nelisp-argframe--helper (name)
  "The (PARAMS . BODY) of helper NAME the lowering may open, else nil."
  (let ((table (if (functionp nelisp-argframe-inline-helpers)
                   (funcall nelisp-argframe-inline-helpers)
                 nelisp-argframe-inline-helpers)))
    (and (hash-table-p table) (symbolp name)
         (< (length nelisp-argframe--inline-stack) nelisp-argframe-inline-depth)
         (not (memq name nelisp-argframe--inline-stack))
         (let ((def (gethash name table)))
           (and def (eq (car-safe (car def)) 'args)
                (<= (nelisp-argframe--size (cdr def)) nelisp-argframe-inline-max-size)
                def)))))

(defun nelisp-argframe--open (name def actuals base argc mode)
  "Lower the call (NAME args . ACTUALS) as the body of DEF opened in place."
  (let* ((params (cdar def)) (body (cdr def)) (n 0) (bindings nil))
    (when (/= (length params) (length actuals))
      (signal 'nelisp-argframe-ineligible (list name)))
    (dolist (p params)
      (let ((fresh (intern (format "nl_afi_%s_%d" name n))))
        (setq n (1+ n))
        (setq body (cl-subst fresh p body))
        (push (list fresh (nelisp-argframe--lower-indexed (car actuals) base argc mode)) bindings)
        (setq actuals (cdr actuals))))
    (let* ((nelisp-argframe--inline-stack (cons name nelisp-argframe--inline-stack))
           (lowered (nelisp-argframe--lower-indexed (if (cdr body) (cons 'seq body) (car body))
                                           base argc mode)))
      (if bindings `(let* ,(nreverse bindings) ,lowered) lowered))))

(defun nelisp-argframe--lower-indexed (form base argc &optional mode)
  "Lower proven indexed reads in FORM to BASE and ARGC.
With MODE, retain the original read when MODE is zero. Missing arguments
return zero, matching `wf_arg_word'; an explicit nil argument remains three.
Reject every other occurrence of `args', including bindings and escapes.
A call handing `args' whole to a helper of `nelisp-argframe-inline-helpers'
is opened in place (with MODE, only the argv side; the list side keeps the
call).  `(nl_argframe_list_only X)' keeps X on the list route: the argv side
declines with 2 there, which its callers answer by running the list route on
the same evaluated words; it may only wrap work that no effect precedes."
  (cond
   ((or (eq form 'args) (and mode (eq form mode)))
    (signal 'nelisp-argframe-ineligible (list form)))
   ((atom form) form)
   ((and (eq (car form) 'nl_argframe_list_only) (consp (cdr form)) (null (cddr form)))
    (if mode `(if (= ,mode 0) ,form 2) 2))
   ((and (memq (car form) '(wf_arg_word wf_argval wf_arg_ptr))
         (eq (cadr form) 'args) (integerp (caddr form))
         (>= (caddr form) 0) (null (cdddr form)))
    ;; subr1: BASE is a raw argv pointer (never a String), so its words are
    ;; raw loads; `heap-read-u64' at offsets 0-24 of a variable is the
    ;; String-aware `nl_string_read64' call.  `wf_arg_ptr' is
    ;; `nl_cons_car_ptr' of the list cell: the same view-or-materialise test
    ;; on the element WORD (`m5_fx_word_ptr'), and 0 past the end, as the
    ;; list walk reaches nil.
    (let* ((index (caddr form))
           (word `(if (< ,index ,argc) (ptr-read-u64 ,base ,(* index 8)) 0))
           (read (pcase (car form)
                   ('wf_argval `(nl_word_payload ,word))
                   ('wf_arg_ptr `(m5_fx_word_ptr ,word))
                   (_ word))))
      (if mode `(if (= ,mode 0) ,form ,read) read)))
   ((equal form '(m5_list_len args 0))
    (if mode `(if (= ,mode 0) ,form ,argc) argc))
   ((and (consp (cdr form)) (eq (cadr form) 'args) (nelisp-argframe--helper (car form)))
    (let ((opened (nelisp-argframe--open (car form) (nelisp-argframe--helper (car form))
                                         (cddr form) base argc
                                         ;; The list side keeps the original call whole.
                                         nil)))
      (if mode `(if (= ,mode 0) ,form ,opened) opened)))
   (t (cons (nelisp-argframe--lower-indexed (car form) base argc mode)
            (nelisp-argframe--lower-indexed (cdr form) base argc mode)))))

;;; wordroute: list-walk lowering.
;;
;; GNU calls every SUBR with its arguments as values: a MANY function gets
;; (nargs, args[]) and walks `for (i = 0; i < nargs; i++) args[i]' (eval.c
;; funcall_subr, data.c minmax_driver, fns.c Fappend, editfns.c Fformat).  A
;; handler here that walks its argument LIST (`(nl_cons_cdr_word rest)' until
;; the tag is not a cons, `(m5_list_len args 0)', a helper recursing on the
;; tail) is the same loop over a list view.  The walk below lowers such a body
;; onto the argument array: a CURSOR is an index I into it standing for the
;; list tail args[I..argc), so car is an indexed read, cdr is I+1, the cons
;; test is I < argc and the length is argc - I.  A helper handed a cursor is
;; opened in place, or, when it recurses or is large, specialized once into a
;; generated defun taking (BASE ARGC ...) with that parameter an index.  Any
;; other use of a cursor (a Lisp callback, a helper that keeps the list) gets
;; a fresh list of the remaining argument words, rooted for that call: what
;; the list route conses for every call, consed only where it is needed.

(define-error 'nelisp-argframe-escape "Argument list escapes to code that needs the list"
  'nelisp-argframe-ineligible)

(defvar nelisp-argframe-helper-defs nil
  "wordroute: every helper the list-walk lowering may open or specialize, or nil.
A hash table (or a function answering one) from a helper's name to its
\(PARAMS . BODY), for helpers of the link set the generated code links
against; a name defined twice is left out, so the lowering opens the
definition that links.")

(defvar nelisp-argframe-generated nil
  "wordroute: where specialized helper definitions go.
nil collects them into a scratch table that is dropped (eligibility only);
`:forbid' makes a handler that needs one ineligible; otherwise a cons
\(DEFUNS . MEMO) the caller appends to the frame leaves, DEFUNS newest first.")

(defvar nelisp-argframe-materialize-env nil
  "wordroute: the variable naming the evaluator env in lowered handler code.
Non-nil lets a cursor that reaches code needing the list itself (a Lisp
callback such as a custom hash test, a helper that keeps the list) become a
fresh list of the remaining argument words, rooted for the call (GNU hands
such a callback its arguments as values; the list route conses this same
list for every call).  Nil makes such a handler ineligible.")

(defconst nelisp-argframe-walk-depth 6
  "wordroute: how many helper levels the list-walk lowering opens in place.")

(defconst nelisp-argframe-walk-max-size 120
  "wordroute: the largest helper body (in conses) opened in place; larger ones
are specialized into a generated defun instead, shared by every handler that
reaches them.  At 400 the 357 frame leaves carried private copies of
`nl_buf_prim_w' and `wf_cmpl_w' (frame-leaf source 603 KB, the applyfn unit
16.97 MB, over the 16 MiB rooted build-input bound); at 120 it is 295 KB.")

(defconst nelisp-argframe-walk-max-spec-size 700
  "wordroute: the largest specialized helper (lowered body, in conses) kept.
A larger one (a slow path that opens much of the runtime, such as
`nl_gb_slow_w') is not duplicated: its call gets a fresh list instead.")

(defconst nelisp-argframe--tag-readers
  '(nl_val_tag sexp-tag heap-sexp-tag-word nl_word_tag64)
  "Readers of a list's type tag: 7 for a cons, 0 for nil.")

(defvar nelisp-argframe--lw-base nil)
(defvar nelisp-argframe--lw-argc nil)
(defvar nelisp-argframe--lw-mode nil)
(defvar nelisp-argframe--lw-env nil
  "The evaluator env variable where a fresh list may be rooted, nil inside a
specialized defun (which has none).")
(defvar nelisp-argframe--lw-opened nil "Helpers being opened in place, innermost first.")
(defvar nelisp-argframe--lw-gensym 0)
(defvar nelisp-argframe--lw-discard nil "Non-nil when the form's value is not used.")

(defun nelisp-argframe--lw-defs ()
  (let ((table (if (functionp nelisp-argframe-helper-defs)
                   (funcall nelisp-argframe-helper-defs)
                 nelisp-argframe-helper-defs)))
    (and (hash-table-p table) table)))

(defun nelisp-argframe--lw-def (name)
  (let ((table (nelisp-argframe--lw-defs)))
    (and table (symbolp name) (gethash name table))))

(defun nelisp-argframe--lw-fresh (prefix)
  (setq nelisp-argframe--lw-gensym (1+ nelisp-argframe--lw-gensym))
  (intern (format "nl_afw_%s_%d" prefix nelisp-argframe--lw-gensym)))

(defun nelisp-argframe--lw-ineligible (form)
  (signal 'nelisp-argframe-ineligible (list form)))

(defun nelisp-argframe--lw-elt (cursor k)
  "The WORD of argument CURSOR + K, 0 past the end (as `wf_arg_word')."
  (let ((index (if (integerp cursor) (+ cursor k) (if (= k 0) cursor `(+ ,cursor ,k)))))
    `(if (< ,index ,nelisp-argframe--lw-argc)
         (ptr-read-u64 ,nelisp-argframe--lw-base
                       ,(if (integerp index) (* index 8) `(* ,index 8)))
       0)))

(defun nelisp-argframe--lw-more (cursor)
  `(< ,cursor ,nelisp-argframe--lw-argc))

(defun nelisp-argframe--lw-end (cursor)
  `(>= ,cursor ,nelisp-argframe--lw-argc))

(defun nelisp-argframe--lw-seq (forms cenv tail)
  "Lower FORMS in order; the last one in TAIL position.  Returns (TYPE . LIST)."
  (let ((out nil) (type 'val))
    (while forms
      (let ((r (let ((nelisp-argframe--lw-discard (or (cdr forms) nelisp-argframe--lw-discard)))
                 (nelisp-argframe--lw (car forms) cenv (and tail (null (cdr forms)))))))
        (when (and (cdr forms) (eq (car r) 'cur))
          ;; A discarded cursor value: keep the form (a `setq' of a cursor
          ;; variable is its effect), it is only no longer a cursor.
          (setq r (cons 'val (cdr r))))
        (setq type (car r))
        (push (cdr r) out)
        (setq forms (cdr forms))))
    (cons type (nreverse out))))

(defun nelisp-argframe--lw-val (form cenv)
  "Lower FORM in a value position: a cursor there is an escape."
  (let ((r (let ((nelisp-argframe--lw-discard nil)) (nelisp-argframe--lw form cenv nil))))
    (if (eq (car r) 'cur) (signal 'nelisp-argframe-escape (list form)) (cdr r))))

(defun nelisp-argframe--lw-bind (var init-type cenv)
  "CENV extended by VAR bound to a value of INIT-TYPE."
  (when (eq var nelisp-argframe--lw-mode)
    (nelisp-argframe--lw-ineligible var))
  (cons (cons var (if (eq init-type 'cur) var nil)) cenv))

(defun nelisp-argframe--lw-let (form cenv tail)
  (let* ((star (eq (car form) 'let*)) (bindings (cadr form)) (body (cddr form))
         (inner cenv) (out nil))
    ;; A binding of the ARGC or BASE name (a handler's own count, or its own
    ;; list rebound to another list) is renamed, so the lowered reads keep
    ;; seeing the array.
    (dolist (b bindings)
      (let ((var (if (consp b) (car b) b)))
        (when (or (eq var nelisp-argframe--lw-argc) (eq var nelisp-argframe--lw-base)
                  (let ((old (assq var cenv))) (and (cdr old) (not (eq (cdr old) var)))))
          (let ((fresh (nelisp-argframe--lw-fresh "a")))
            (setq bindings (cl-subst fresh var bindings)
                  body (cl-subst fresh var body))))))
    (dolist (b bindings)
      (let* ((var (if (consp b) (car b) b))
             (r (if (consp b) (let ((nelisp-argframe--lw-discard nil))
                                (nelisp-argframe--lw (cadr b) (if star inner cenv) nil))
                  (cons 'val 3))))
        (setq inner (nelisp-argframe--lw-bind var (car r) inner))
        (push (list var (cdr r)) out)))
    (let ((r (nelisp-argframe--lw-seq body inner tail)))
      (cons (car r) `(,(car form) ,(nreverse out) ,@(cdr r))))))

(defun nelisp-argframe--lw-call (form cenv _tail)
  (let* ((head (car form)) (rs nil) (any-cur nil))
    (dolist (a (cdr form))
      (let ((r (let ((nelisp-argframe--lw-discard nil)) (nelisp-argframe--lw a cenv nil))))
        (when (eq (car r) 'cur) (setq any-cur t))
        (push r rs)))
    (setq rs (nreverse rs))
    (let ((args (mapcar #'cdr rs)) (types (mapcar #'car rs)))
      (cond
       ;; (= (TAG C) 7) and friends: the lowered tag read compared, as an index test.
       ((and (not any-cur) (memq head '(= /=)) (= (length args) 2)
             (let ((tag (nelisp-argframe--lw-tag-of (car args) (cadr args))))
               (and tag (setq any-cur tag))))
        (let* ((cursor (car any-cur)) (k (cdr any-cur))
               (cons-p (eq (= k 7) (eq head '=))))
          (cons 'val (if cons-p (nelisp-argframe--lw-more cursor)
                       (nelisp-argframe--lw-end cursor)))))
       ((not any-cur)
        (cons 'val (cons head args)))
       ;; Readers of a cursor in argument 0.
       ((and (eq (car types) 'cur) (null (cdr args))
             (memq head '(nl_cons_car_word nl_cons_car_ptr)))
        (let ((word (nelisp-argframe--lw-elt (car args) 0)))
          (cons 'val (if (eq head 'nl_cons_car_ptr) `(m5_fx_word_ptr ,word) word))))
       ;; `wf_cdr_has_more': the list's cdr is a cons (it reads the cell's cdr
       ;; slot); on the array, a next argument exists.
       ((and (eq head 'wf_cdr_has_more) (eq (car types) 'cur) (null (cdr args)))
        (let ((c (car args)))
          (cons 'val `(if (< ,(if (integerp c) (1+ c) `(+ ,c 1)) ,nelisp-argframe--lw-argc) 1 0))))
       ;; Immediate-ness of the list WORD: nil (3) is immediate, a cons never.
       ((and (eq head 'logand) (eq (car types) 'cur) (equal (cdr args) '(1)))
        (cons 'val `(if ,(nelisp-argframe--lw-more (car args)) 0 1)))
       ((and (eq (car types) 'cur) (null (cdr args)) (memq head '(nl_cons_cdr_word nl_cons_cdr_ptr)))
        ;; Unclamped: every reader below tests the index against ARGC, so an
        ;; index past the end reads as the end of the list.
        (let ((c (car args)))
          (cons 'cur (if (integerp c) (1+ c) `(+ ,c 1)))))
       ((and (eq (car types) 'cur) (null (cdr args)) (memq head nelisp-argframe--tag-readers))
        (cons 'val `(if ,(nelisp-argframe--lw-more (car args)) 7 0)))
       ((and (eq (car types) 'cur) (eq head 'm5_list_len) (equal (cdr args) '(0)))
        (let ((c (car args)))
          (cons 'val (if (eql c 0) nelisp-argframe--lw-argc
                       `(if (< ,c ,nelisp-argframe--lw-argc) (- ,nelisp-argframe--lw-argc ,c) 0)))))
       ((and (eq (car types) 'cur) (memq head '(wf_arg_word wf_argval wf_arg_ptr))
             (= (length args) 2) (integerp (cadr args)) (>= (cadr args) 0))
        (let ((word (nelisp-argframe--lw-elt (car args) (cadr args))))
          (cons 'val (pcase head
                       ('wf_argval `(nl_word_payload ,word))
                       ('wf_arg_ptr `(m5_fx_word_ptr ,word))
                       (_ word)))))
       ;; The list's emptiness compared against nil (3) or an absent list (0).
       ((and (memq head '(= /=)) (= (length args) 2)
             (or (and (eq (car types) 'cur) (memq (cadr args) '(0 3)) (eq (cadr types) 'val))
                 (and (eq (cadr types) 'cur) (memq (car args) '(0 3)) (eq (car types) 'val))))
        (let ((c (if (eq (car types) 'cur) (car args) (cadr args))))
          (cons 'val (if (eq head '=) (nelisp-argframe--lw-end c) (nelisp-argframe--lw-more c)))))
       ((nelisp-argframe--lw-def head)
        (nelisp-argframe--lw-helper head args types form))
       (t (nelisp-argframe--lw-materialize head args types form))))))

(defun nelisp-argframe--lw-tag-of (a b)
  "(CURSOR . K) when A or B is a lowered tag read of a cursor and the other K 0 or 7."
  (cl-flet ((tag-read (x) (pcase x
                            (`(if (< ,c ,n) 7 0)
                             (and (eq n nelisp-argframe--lw-argc) c)))))
    (cond ((and (memq b '(0 7)) (tag-read a)) (cons (tag-read a) b))
          ((and (memq a '(0 7)) (tag-read b)) (cons (tag-read b) a)))))

(defun nelisp-argframe--lw-list (cursor)
  "A fresh list of the argument words from CURSOR on (nil past the end)."
  `(m5_fx_word_list ,nelisp-argframe--lw-base ,nelisp-argframe--lw-argc
                    (- ,nelisp-argframe--lw-argc ,cursor) 0))

(defun nelisp-argframe--lw-materialize (head args types form)
  "Call HEAD with each cursor of ARGS as a fresh list rooted for the call
\(`nl_argframe_fresh_list' roots it; the call's mark releases it)."
  (unless nelisp-argframe--lw-env (signal 'nelisp-argframe-escape (list form)))
  (let* ((env nelisp-argframe--lw-env)
         (mark (nelisp-argframe--lw-fresh "m")) (result (nelisp-argframe--lw-fresh "r"))
         (lists nil) (actuals nil))
    (cl-mapc (lambda (a ty)
               (if (eq ty 'cur)
                   (let ((v (nelisp-argframe--lw-fresh "l")))
                     (push `(,v (nl_argframe_fresh_list ,env ,nelisp-argframe--lw-base
                                                        ,nelisp-argframe--lw-argc ,a))
                           lists)
                     (push v actuals))
                 (push a actuals)))
             args types)
    (setq lists (nreverse lists) actuals (nreverse actuals))
    (cons 'val
          `(let* ((,mark (nl_root_mark ,env)) ,@lists (,result (,head ,@actuals)))
             (seq (nl_root_release ,env ,mark) ,result)))))

(defun nelisp-argframe--lw-helper (name args types form)
  "Open or specialize helper NAME on the lowered ARGS, some of them cursors.
A helper whose body cannot run on the array is called with fresh lists."
  (let* ((def (nelisp-argframe--lw-def name)) (params (car def)))
    (if (or (/= (length params) (length args))
            (memq '&optional params) (memq '&rest params))
        (nelisp-argframe--lw-materialize name args types form)
      (condition-case err
          (if (and (not (memq name nelisp-argframe--lw-opened))
                   (< (length nelisp-argframe--lw-opened) nelisp-argframe-walk-depth)
                   (<= (nelisp-argframe--size (cdr def)) nelisp-argframe-walk-max-size)
                   (not (nelisp-argframe--calls-p name (cdr def))))
              (nelisp-argframe--lw-open name def args types)
            (nelisp-argframe--lw-specialize name def args types))
        (nelisp-argframe-ineligible
         (if nelisp-argframe--lw-env
             (nelisp-argframe--lw-materialize name args types form)
           (signal (car err) (cdr err))))))))

(defun nelisp-argframe--assigned-p (var forms)
  "Non-nil when FORMS may assign VAR (any `setq' naming it)."
  (cond ((atom forms) nil)
        ((and (eq (car forms) 'setq) (memq var (cdr forms))) t)
        (t (or (nelisp-argframe--assigned-p var (car forms))
               (nelisp-argframe--assigned-p var (cdr forms))))))

(defun nelisp-argframe--calls-p (name forms)
  "Non-nil when FORMS call NAME (a recursive helper is specialized, not opened)."
  (cond ((atom forms) nil)
        ((eq (car forms) name) t)
        (t (or (nelisp-argframe--calls-p name (car forms))
               (nelisp-argframe--calls-p name (cdr forms))))))

(defun nelisp-argframe--lw-open (name def args types)
  (let* ((params (car def)) (body (cdr def)) (bindings nil) (cenv nil) (i 0))
    (dolist (p params)
      (let ((fresh (intern (format "nl_afw_%s_%d" name i))))
        (setq i (1+ i))
        (if (and (integerp (car args)) (not (nelisp-argframe--assigned-p p body)))
            ;; A constant the body never assigns: substituted, so an argument
            ;; index stays a literal (`(ptr-read-u64 BASE 8)').
            (if (eq (car types) 'cur)
                (progn (setq body (cl-subst fresh p body))
                       (push (cons fresh (car args)) cenv))
              (setq body (cl-subst (car args) p body)))
          (setq body (cl-subst fresh p body))
          (push (list fresh (car args)) bindings)
          (push (cons fresh (if (eq (car types) 'cur) fresh nil)) cenv))
        (setq args (cdr args) types (cdr types))))
    (let* ((nelisp-argframe--lw-opened (cons name nelisp-argframe--lw-opened))
           (r (nelisp-argframe--lw-seq body cenv nil)))
      (cons (car r)
            (if bindings `(let* ,(nreverse bindings) ,@(cdr r))
              (if (cdr (cdr r)) (cons 'seq (cdr r)) (cadr r)))))))

(defun nelisp-argframe--lw-specialize (name def args types)
  "Call the specialization of NAME on its cursor parameters, generating it once."
  (when (eq nelisp-argframe-generated :forbid)
    (nelisp-argframe--lw-ineligible name))
  (let* ((positions (let ((k 0) (ps nil))
                      (dolist (ty types (nreverse ps))
                        (when (eq ty 'cur) (push k ps))
                        (setq k (1+ k)))))
         (key (cons name positions))
         (memo (cdr nelisp-argframe-generated))
         (spec (gethash key memo)))
    (when (eq spec :fail) (nelisp-argframe--lw-ineligible name))
    (unless spec
      (setq spec (intern (format "%s__af%s" name (mapconcat #'number-to-string positions "_"))))
      (puthash key spec memo)
      (condition-case err
          (let* ((params (car def))
                 (cenv (let ((k 0) (out nil))
                         (dolist (p params out)
                           (push (cons p (if (memq k positions) p nil)) out)
                           (setq k (1+ k)))))
                 (nelisp-argframe--lw-base 'nl_afb)
                 (nelisp-argframe--lw-argc 'nl_afn)
                 (nelisp-argframe--lw-mode nil)
                 ;; A helper taking the evaluator env (a parameter named as
                 ;; the handlers' one, not itself a cursor) roots fresh lists
                 ;; there; one without it has nowhere to root them.
                 (nelisp-argframe--lw-env
                  (and nelisp-argframe-materialize-env
                       (let ((k (cl-position nelisp-argframe-materialize-env params)))
                         (and k (not (memq k positions)) nelisp-argframe-materialize-env))))
                 (nelisp-argframe--lw-opened (list name))
                 (r (nelisp-argframe--lw-seq (cdr def) cenv t)))
            (when (or (eq (car r) 'cur)
                      (> (nelisp-argframe--size (cdr r)) nelisp-argframe-walk-max-spec-size))
              (nelisp-argframe--lw-ineligible name))
            (push `(defun ,spec (nl_afb nl_afn ,@params) ,@(cdr r))
                  (car nelisp-argframe-generated)))
        (nelisp-argframe-ineligible
         (puthash key :fail memo)
         (signal (car err) (cdr err)))))
    (cons 'val `(,spec ,nelisp-argframe--lw-base ,nelisp-argframe--lw-argc ,@args))))

(defun nelisp-argframe--lw (form cenv tail)
  "Lower FORM under CENV (VAR . CURSOR-EXPR or nil); TAIL when its value is the
handler's.  Returns (TYPE . EXPR), TYPE `cur' (EXPR an index) or `val'."
  (cond
   ((and (symbolp form) form (eq form nelisp-argframe--lw-mode))
    (nelisp-argframe--lw-ineligible form))
   ((symbolp form)
    (let ((b (assq form cenv)))
      (if (and b (cdr b)) (cons 'cur (cdr b)) (cons 'val form))))
   ((atom form) (cons 'val form))
   ((eq (car form) 'quote)
    (when (nelisp-argframe--calls-p 'args (list (cdr form))) (nelisp-argframe--lw-ineligible form))
    (cons 'val form))
   ((and (eq (car form) 'nl_argframe_list_only) (consp (cdr form)) (null (cddr form)))
    (cons 'val 2))
   ((memq (car form) '(seq progn))
    (let ((r (nelisp-argframe--lw-seq (cdr form) cenv tail)))
      (cons (car r) (cons (car form) (cdr r)))))
   ((memq (car form) '(if when unless))
    (let* ((test (nelisp-argframe--lw-val (cadr form) cenv))
           (then (if (eq (car form) 'if) (list (caddr form)) (cddr form)))
           (else (if (eq (car form) 'if) (cdddr form) nil))
           (a (nelisp-argframe--lw-seq then cenv tail))
           (b (and else (nelisp-argframe--lw-seq else cenv tail))))
      ;; Branches of different kinds: a discarded value needs nothing; the
      ;; handler's own value returns the list itself (the list route's list).
      (when (and (not nelisp-argframe--lw-discard)
                 (or (and b (not (eq (car a) (car b)))) (and (not b) (eq (car a) 'cur))))
        (unless tail (nelisp-argframe--lw-ineligible form))
        (setq a (nelisp-argframe--lw-value-of a) b (and b (nelisp-argframe--lw-value-of b))))
      (cons (if (and b (eq (car a) (car b))) (car a) 'val)
            (pcase (car form)
              ('if `(if ,test ,(nelisp-argframe--lw-one (cdr a)) ,@(and b (cdr b))))
              (_ `(,(car form) ,test ,@(cdr a)))))))
   ((eq (car form) 'cond)
    (let ((clauses nil) (type nil))
      (dolist (clause (cdr form))
        (let* ((test (nelisp-argframe--lw-val (car clause) cenv))
               (body (and (cdr clause) (nelisp-argframe--lw-seq (cdr clause) cenv tail))))
          (when body
            (when (and type (not (eq type (car body))))
              (if nelisp-argframe--lw-discard (setq body (cons 'val (cdr body)))
                (nelisp-argframe--lw-ineligible form)))
            (setq type (car body)))
          (push (cons test (and body (cdr body))) clauses)))
      (cons (or type 'val) (cons 'cond (nreverse clauses)))))
   ((memq (car form) '(and or))
    (cons 'val (cons (car form) (mapcar (lambda (f) (nelisp-argframe--lw-val f cenv)) (cdr form)))))
   ((eq (car form) 'while)
    (let* ((test (nelisp-argframe--lw-val (cadr form) cenv))
           (body (let ((nelisp-argframe--lw-discard t)) (nelisp-argframe--lw-seq (cddr form) cenv nil))))
      (cons 'val `(while ,test ,@(cdr body)))))
   ((memq (car form) '(let let*)) (nelisp-argframe--lw-let form cenv tail))
   ((eq (car form) 'setq)
    (let ((pairs (cdr form)) (out nil) (type 'val))
      (while pairs
        (let* ((var (car pairs)) (b (assq var cenv))
               (r (let ((nelisp-argframe--lw-discard nil)) (nelisp-argframe--lw (cadr pairs) cenv nil))))
          (when (or (eq var nelisp-argframe--lw-base) (eq var nelisp-argframe--lw-mode))
            (nelisp-argframe--lw-ineligible form))
          (cond ((and b (cdr b))
                 (unless (and (eq (car r) 'cur) (eq (cdr b) var)) (nelisp-argframe--lw-ineligible form)))
                ((eq (car r) 'cur) (nelisp-argframe--lw-ineligible form)))
          (setq type (car r))
          (setq out (append out (list var (cdr r))))
          (setq pairs (cddr pairs))))
      (cons type (cons 'setq out))))
   ((symbolp (car form)) (nelisp-argframe--lw-call form cenv tail))
   (t (nelisp-argframe--lw-ineligible form))))

(defun nelisp-argframe--lw-value-of (r)
  "R as a value: a cursor becomes a fresh list (the handler's result)."
  (if (eq (car r) 'cur)
      (cons 'val (let ((forms (cdr r)))
                   (append (butlast forms) (list (nelisp-argframe--lw-list (car (last forms)))))))
    r))

(defun nelisp-argframe--lw-one (forms)
  (if (cdr forms) (cons 'seq forms) (car forms)))

(defun nelisp-argframe-walk-lower (form base argc &optional mode)
  "wordroute: lower FORM's argument-list walks onto BASE and ARGC (see above).
Signals `nelisp-argframe-ineligible' when FORM cannot run on the array.  With
MODE, the result keeps FORM whole for list mode (MODE zero)."
  (let* ((nelisp-argframe--lw-base base)
         (nelisp-argframe--lw-argc argc)
         (nelisp-argframe--lw-mode mode)
         (nelisp-argframe--lw-env nelisp-argframe-materialize-env)
         (nelisp-argframe--lw-opened nil)
         (nelisp-argframe--lw-gensym 0)
         (nelisp-argframe-generated (or nelisp-argframe-generated
                                        (cons nil (make-hash-table :test #'equal))))
         (r (nelisp-argframe--lw form (list (cons 'args 0)) t)))
    (when (eq (car r) 'cur)
      ;; The handler answers the list itself: the list route's fresh list.
      (if nelisp-argframe--lw-env
          (setq r (cons 'val (nelisp-argframe--lw-list (cdr r))))
        (nelisp-argframe--lw-ineligible form)))
    (when (equal (cdr r) 2)
      ;; Declining at once: nothing runs on the array.
      (nelisp-argframe--lw-ineligible form))
    (if mode `(if (= ,mode 0) ,form ,(cdr r)) (cdr r))))

(defun nelisp-argframe-lower (form base argc &optional mode)
  "Lower FORM's argument reads onto BASE and ARGC.
The indexed pass (`nelisp-argframe--lower-indexed') first, else the
argument-list walks (`nelisp-argframe-walk-lower', wordroute).
A form the indexed pass admits is lowered exactly as before."
  (condition-case nil
      (nelisp-argframe--lower-indexed form base argc mode)
    (nelisp-argframe-ineligible
     (nelisp-argframe-walk-lower form base argc mode))))

(defvar nelisp-argframe-walk-names nil
  "wordroute: the handlers the list-walk pass may admit, or nil for all.
Each argv entry is a frame leaf (and its specialized helpers) in the reader's
applyfn unit, which the rooted build bounds at 16 MiB; a build restricts the
walk to the handlers whose list route its measurements show hot.  Handlers
the indexed pass admits are not affected.")

(defun nelisp-argframe--entry-lower (entry base argc mode)
  "Lower the guarded table ENTRY's body: the indexed pass, else the walk when
`nelisp-argframe-walk-names' allows ENTRY's name."
  (condition-case err
      (nelisp-argframe--lower-indexed (cddr entry) base argc mode)
    (nelisp-argframe-ineligible
     (if (or (null nelisp-argframe-walk-names)
             (member (cadar entry) nelisp-argframe-walk-names))
         (nelisp-argframe-walk-lower (cddr entry) base argc mode)
       (signal (car err) (cdr err))))))

(defun nelisp-argframe-mode-table (table &optional admitted-name-p)
  "Add private argv mode to TABLE without duplicating handler bodies.
The caller supplies `_pad' = argc + 1 in frame mode and zero in list mode.
An ineligible frame arm declines before any handler effect. ADMITTED-NAME-P
may impose an additional metadata proof, such as an existing frozen SID.
An arm whose walk needs a specialized helper declines here (wordroute: this
table has nowhere to put the generated definition)."
  (let ((nelisp-argframe-generated :forbid))
  (mapcar
   (lambda (entry)
     (unless (eq (cadr entry) :word)
       (error "Argument-frame transport requires a WORD arm: %S" (car entry)))
     (cons (car entry)
           (cons :word
                 (if (and admitted-name-p (not (funcall admitted-name-p (cadar entry))))
                     `(if (= _pad 0) ,(cddr entry) 2)
                   (condition-case nil
                     (nelisp-argframe--entry-lower
                      entry 'args '(- _pad 1) '_pad)
                   (nelisp-argframe-ineligible
                    `(if (= _pad 0) ,(cddr entry) 2)))))))
   table)))

(defun nelisp-argframe-admitted-names (table)
  "Return the names whose complete guarded TABLE bodies prove eligible."
  (let (names)
    (dolist (entry table (nreverse names))
      (when (eq (cadr entry) :word)
        (condition-case nil
            (progn (nelisp-argframe--entry-lower
                    entry 'args '(- _pad 1) '_pad)
                   (push (cadar entry) names))
          (nelisp-argframe-ineligible nil))))))

(provide 'nelisp-standalone-argframe)
;;; nelisp-standalone-argframe.el ends here
