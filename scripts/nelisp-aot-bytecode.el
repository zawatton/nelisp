;;; nelisp-aot-bytecode.el --- build-time AOT of GNU bytecode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Build-time AOT of GNU bytecode.  A translator turns selected GNU bytecode
;; functions (the exact objects the boot image bakes from vendor macroexp.el)
;; into build-DSL defuns `wf_aot_N' that the standalone build compiles to
;; machine code.  The VM call arm `wf_bytecode_w_function' and the VM generic
;; call arm enter them through an install table; the heap object layout and the
;; GC roots are unchanged (the operand stack lives in native locals, which the
;; collector scans conservatively on the main thread; worker threads always
;; run the VM).
;;
;; Runtime pieces:
;;  - `nelisp--aot-table' (Lisp vector, GC-rooted): slots 0..K-1 hold the
;;    installed bytecode objects, slots K..2K-1 are fixnum entry counters
;;    (observable by the regression smoke), then `nelisp--aot-lean-words' words
;;    of lean-call counters and a seal slot (`t' once every row is installed).
;;  - `nelisp--aot-install' (boot Lisp) fills slot ID only when the live
;;    object's arity, code string and constants are `equal' to the build-time
;;    values; a mismatch leaves the function on the VM.
;;  - `nl_aot_state' (208 BSS bytes) caches the table: +0 mirror entry, +8
;;    its `nl_mxcache_epoch', +16 callee miss filter, +24 filter epoch+1,
;;    +32+8i installed object words, +96 table data address, +104+8i constants
;;    data addresses, +176 lean counter region address, +184 the captured
;;    `autoload-do-load' object and +192 the symbol `autoload' (0 when not
;;    heap words), which guard the inline autoload-do-load fast path.
;;
;; Build-time switches (environment variables):
;;  - NELISP_AOT_LIST: comma separated override of the translated functions.
;;  - NELISP_AOT_PROF=1: per-site counters for the slow/generic operations
;;    (`wf_aot_prof'); the default build emits no profiling code at all.
;;  - NELISP_AOT_DEFER=1: the image keeps the table in `nelisp--aot-table-saved'
;;    and starts with `nelisp--aot-table' detached (everything runs on the VM).
;; The regression smoke is test/nelisp-aot-bytecode-standalone-smoke.sh.

;;; Code:

(require 'cl-lib)

(declare-function nelisp-boot-image-provider "nelisp-boot-image" (feature))

(defconst nelisp-aot-bytecode-build-list
  (let ((env (getenv "NELISP_AOT_LIST")))
    (if env
        (mapcar #'intern (split-string env "," t))
      '(macroexp--cons macroexp--all-forms macroexpand-1 macroexp--all-clauses
        macroexp-macroexpand macroexp--expand-all function-get)))
  "Functions the build tries to translate, in id order.
Names in `nelisp-aot-bytecode-subr-names' come from the staged GNU subr.el, the
others from vendor macroexp.el.
NELISP_AOT_LIST (comma separated, possibly empty) overrides it for measurements.")

(defconst nelisp-aot-bytecode-subr-names '(function-get)
  "Build-list functions whose bytecode object comes from staged subr.el.
They are defined after the macroexp provider, so their install forms are emitted
late (`nelisp-aot-bytecode-late-install-source').")

(defvar nelisp-aot-bytecode-subr-source-function nil
  "Function of one argument NAME returning the boot text that defines NAME in
staged subr.el, exactly as the standalone build compiles it, or nil.  The
standalone build sets it; when nil, the subr.el rows are not translated.")

(defvar nelisp-aot-bytecode--subr-texts nil)
(defun nelisp-aot-bytecode-subr-text (name)
  "Return the staged subr.el boot text defining NAME, or nil.
The text is the one the standalone build compiles."
  (let ((hit (assq name nelisp-aot-bytecode--subr-texts)))
    (if hit (cdr hit)
      (let ((text (and nelisp-aot-bytecode-subr-source-function
                       (funcall nelisp-aot-bytecode-subr-source-function name))))
        (push (cons name text) nelisp-aot-bytecode--subr-texts)
        text))))

(defun nelisp-aot-bytecode--late-name-p (name) (and (memq name nelisp-aot-bytecode-subr-names) t))

(define-error 'nelisp-aot-refused "AOT translation refused")

(defvar nelisp-aot-bytecode--macroexp-text nil "Cache of the macroexp.el boot provider text.")

(defun nelisp-aot-bytecode-macroexp-text ()
  "Return the boot provider text of macroexp.el (computed once per process)."
  (or nelisp-aot-bytecode--macroexp-text
      (setq nelisp-aot-bytecode--macroexp-text
            (nelisp-boot-image-provider 'macroexp))))

(defun nelisp-aot-bytecode--make-form (text name)
  "Return the `make-byte-code' form NAME is defined by in TEXT, or nil."
  (let ((pos (and (stringp text)
                  (string-match (format "(fset '%s (make-byte-code " (regexp-quote (symbol-name name)))
                                text))))
    (when pos
      (let ((form (car (read-from-string text pos))))
        (and (eq (car-safe form) 'fset)
             (eq (car-safe (nth 2 form)) 'make-byte-code)
             (nth 2 form))))))

(defun nelisp-aot-bytecode--object (make-form)
  "Evaluate MAKE-FORM on the host and return the byte-code object."
  (eval make-form t))

;;;; Translator

(defun nelisp-aot--refuse (fmt &rest args)
  "Signal `nelisp-aot-refused' with the formatted reason.
The refused function stays on the VM."
  (signal 'nelisp-aot-refused (list (apply #'format fmt args))))

;; Opcode classes.
(defconst nelisp-aot--ops-1 '(1 2 3 4 5 8 9 10 11 12 13 16 17 18 19 20 21 24 25 26 27 28 29
                              32 33 34 35 36 37 40 41 42 43 44 45 48 57 58 60 61 62 64 65 66
                              67 68 69 70 71 75 78 83 84 85 135 136 137 154 158 159 162 163 164 166 183)
  "Supported single-byte opcodes (constants >= 192 are added in the decoder).")

(defun nelisp-aot--decode (code)
  "Decode CODE (a unibyte string) into a vector of (OP OPERAND LEN) by pc."
  (let* ((n (length code)) (v (make-vector n nil)) (pc 0))
    (while (< pc n)
      (let* ((op (aref code pc))
             (len (cond ((or (memq op nelisp-aot--ops-1) (>= op 192)) 1)
                        ((memq op '(6 14 22 30 46 178 182)) 2)
                        ((memq op '(7 15 23 31 47 49 129 130 131 132 133)) 3)
                        (t (nelisp-aot--refuse "unsupported opcode %d at %d" op pc)))))
        (when (> (+ pc len) n) (nelisp-aot--refuse "truncated instruction at %d" pc))
        (aset v pc (list op
                         (cond ((= len 1) nil)
                               ((= len 2) (aref code (1+ pc)))
                               (t (+ (aref code (1+ pc)) (* 256 (aref code (+ pc 2))))))
                         len))
        (setq pc (+ pc len))))
    v))

(defun nelisp-aot--var-index (op arg)
  "Constant index of a varref/varset/varbind/unbind-family opcode OP (operand ARG)."
  (let ((low (logand op 7))) (if (< low 6) low arg)))

(defun nelisp-aot--analyze (insns nargs depth consts)
  "Return (SPS LEADERS MAXSP CTXS SWITCHES): static stack depth by pc, sorted
block leaders, deepest stack, the condition-case region of each pc
\(nil or (HANDLER-PC . SAVED-SP)) and an alist pc -> switch target pcs."
  (let* ((nconst (length consts))
         (sps (make-vector (length insns) nil)) (ctxs (make-vector (length insns) nil))
         (bds (make-vector (length insns) nil)) (maxbd 0)
         (work (list (list 0 nargs nil 0)))
         (leaders (list 0)) (maxsp nargs) (switches nil))
    (while work
      (let* ((item (pop work)) (pc (nth 0 item)) (sp (nth 1 item)) (ctx (nth 2 item))
             (bd (nth 3 item)) (lastc nil))
        (catch 'done
          (while t
            (when (>= pc (length insns)) (nelisp-aot--refuse "falls off the end"))
            (let ((seen (aref sps pc)))
              (when seen
                (unless (and (= seen sp) (equal (aref ctxs pc) ctx) (= (aref bds pc) bd))
                  (nelisp-aot--refuse "inconsistent depth or handler at %d" pc))
                (throw 'done nil)))
            (aset sps pc sp) (aset ctxs pc ctx) (aset bds pc bd)
            (let* ((ins (or (aref insns pc) (nelisp-aot--refuse "pc %d is not an instruction start" pc)))
                   (op (car ins)) (arg (cadr ins)) (len (nth 2 ins))
                   (next (+ pc len)) (thisc nil))
              (cond
               ((<= 1 op 7)
                (let ((n (if (<= op 5) op arg)))
                  (when (< n 1) (nelisp-aot--refuse "stack-ref 0"))
                  (when (>= n sp) (nelisp-aot--refuse "stack-ref below frame"))
                  (setq sp (1+ sp))))
               ((<= 8 op 15)
                (when (>= (nelisp-aot--var-index op arg) nconst) (nelisp-aot--refuse "varref constant range"))
                (setq sp (1+ sp)))
               ((or (<= 16 op 23) (<= 24 op 31))
                (when (>= (nelisp-aot--var-index op arg) nconst) (nelisp-aot--refuse "var constant range"))
                (when (< sp 1) (nelisp-aot--refuse "varset underflow"))
                (when (and ctx (<= 24 op 31)) (nelisp-aot--refuse "varbind inside handler region"))
                (when (<= 24 op 31) (setq bd (1+ bd)) (setq maxbd (max maxbd bd)))
                (setq sp (1- sp)))
               ((<= 40 op 47)
                (when ctx (nelisp-aot--refuse "unbind inside handler region"))
                (let ((n (nelisp-aot--var-index op arg)))
                  (when (< bd n) (nelisp-aot--refuse "unbind below binding depth"))
                  (setq bd (- bd n))))
               ((= op 137) (when (< sp 1) (nelisp-aot--refuse "dup underflow")) (setq sp (1+ sp)))
               ((= op 136) (when (< sp 1) (nelisp-aot--refuse "discard underflow")) (setq sp (1- sp)))
               ((= op 182)
                (let ((n (logand arg 127)))
                  (when (< sp (+ n (if (/= (logand arg 128) 0) 1 0))) (nelisp-aot--refuse "discardN underflow"))
                  (setq sp (- sp n))))
               ((or (= op 129) (>= op 192))
                (let ((i (if (= op 129) arg (- op 192))))
                  (when (>= i nconst) (nelisp-aot--refuse "constant index out of range"))
                  (setq thisc i)
                  (setq sp (1+ sp))))
               ((= op 178)
                (when (or (< sp 1) (> arg (1- sp))) (nelisp-aot--refuse "stack-set range"))
                (setq sp (1- sp)))
               ((memq op '(64 65 57 58 60 75 83 84 159 162 163 71))
                (when (< sp 1) (nelisp-aot--refuse "unary underflow")))
               ((memq op '(66 61 85 158 164 62 78 154 166))
                (when (< sp 2) (nelisp-aot--refuse "binary underflow")) (setq sp (1- sp)))
               ((<= 67 op 70)
                (let ((k (- op 66)))
                  (when (< sp k) (nelisp-aot--refuse "listN underflow"))
                  (setq sp (- sp (1- k)))))
               ((<= 32 op 37)
                (let ((n (- op 32)))
                  (when (< sp (1+ n)) (nelisp-aot--refuse "call underflow"))
                  (setq sp (- sp n))))
               ((= op 135)
                (when (< sp 1) (nelisp-aot--refuse "return underflow"))
                (when ctx (nelisp-aot--refuse "return inside handler region"))
                (throw 'done nil))
               ((= op 49)
                (when (< sp 1) (nelisp-aot--refuse "pushconditioncase underflow"))
                (when ctx (nelisp-aot--refuse "nested handler"))
                (setq sp (1- sp))
                (push arg leaders) (push (list arg (1+ sp) nil bd) work)
                (setq ctx (cons arg sp)))
               ((= op 48)
                (unless ctx (nelisp-aot--refuse "pophandler without handler"))
                (setq ctx nil))
               ((= op 183)
                (when (< sp 2) (nelisp-aot--refuse "switch underflow"))
                (let ((tab (and lastc (aref consts lastc))) (targets nil))
                  (unless (hash-table-p tab) (nelisp-aot--refuse "switch table is not a constant"))
                  (maphash (lambda (_k v)
                             (unless (and (integerp v) (> v pc)) (nelisp-aot--refuse "switch target"))
                             (push v targets))
                           tab)
                  (setq targets (delete-dups targets))
                  (push (cons pc targets) switches)
                  (setq sp (- sp 2))
                  (dolist (tg targets) (push tg leaders) (push (list tg sp ctx bd) work))
                  (push next leaders) (push (list next sp ctx bd) work)
                  (throw 'done nil)))
               ((memq op '(130 131 132 133))
                (when (and (/= op 130) (< sp 1)) (nelisp-aot--refuse "branch underflow"))
                (when (memq op '(131 132)) (setq sp (1- sp)))
                (push arg leaders) (push (list arg sp ctx bd) work)
                (when (= op 133) (setq sp (1- sp)))
                (if (= op 130) (throw 'done nil) (push next leaders)))
               (t (nelisp-aot--refuse "unsupported opcode %d at %d" op pc)))
              (setq lastc thisc)
              (setq maxsp (max maxsp sp))
              (when (> sp depth) (nelisp-aot--refuse "stack deeper than declared"))
              (setq pc next))))))
    (list sps (sort (delete-dups leaders) #'<) maxsp ctxs switches bds maxbd)))


(defvar nelisp-aot--direct nil
  "Alist (SYMBOL ID MANDATORY MAXARGS) of functions with a generated `wf_aot_ID'.")

(defun nelisp-aot--direct-for (sym n)
  "The id of the AOT function SYM when it accepts N arguments, else nil."
  (let ((e (assq sym nelisp-aot--direct)))
    (and e (<= (nth 2 e) n) (<= n (nth 3 e)) (nth 1 e))))

(defconst nelisp-aot--fail '(seq (setq fail 1) (setq blk -1)))


;;;; Native-locals code generation (stage 4c).
;;
;; The bytecode operand stack lives in DSL locals x0..xN (one per stack
;; depth); there is no rooted VM frame.  The collector scans the native stack
;; conservatively on the main thread, so a local is a root.  Slow operations
;; copy their operands into a small per-activation block `xb' (allocated on
;; first use, held by a local so it is scanned too) and reuse the VM helpers
;; that take a word vector.  Dynamic bindings keep {name, saved, kind} in
;; locals bn/bv/bg at the static binding depth.

(defvar nelisp-aot--ctxs nil "Per-pc condition-case region (HANDLER-PC . SAVED-SP) or nil.")
(defvar nelisp-aot--switches nil "Alist switch pc -> target pcs.")
(defvar nelisp-aot--bds nil "Per-pc dynamic binding depth.")
(defvar nelisp-aot--codelen 0)
(defvar nelisp-aot--maxarr 2 "Largest operand count a slow path put in the block.")
(defconst nelisp-aot--prof (equal (getenv "NELISP_AOT_PROF") "1")
  "Build-time profiling switch: per-site counters of the slow/generic operations.")
(defconst nelisp-aot--lw (if nelisp-aot--prof 136 8)
  "Words of the table vector between the entry counters and the seal.")
(defvar nelisp-aot--sites nil "Reversed list of (FUNCTION . LABEL) per profiled site.")
(defvar nelisp-aot--curname nil "Name of the function being translated (profiling labels).")
(defun nelisp-aot--site (label)
  "A form counting one execution of a site labelled LABEL (0 unless profiling)."
  (if (not nelisp-aot--prof) 0
    (push (cons nelisp-aot--curname label) nelisp-aot--sites)
    `(wf_aot_prof ,(1- (length nelisp-aot--sites)))))
(defun nelisp-aot--subst-l (form)
  "Substitute the table layout words %L% and %HL2% in the helper FORM."
  (cl-subst (/ nelisp-aot--lw 2) '%HL2% (cl-subst nelisp-aot--lw '%L% form)))
(defvar nelisp-aot--k 0 "Number of installed AOT rows (counter slots start after the objects).")

(defun nelisp-aot--x (i)
  "The DSL local holding operand stack slot I."
  (intern (format "x%d" i)))
;; The operand block is a rooted WORD frame on the root stack (LIFO, released at
;; the function's single exit); the 160-byte view scratch (+128 a name buffer) is a
;; separate heap block allocated only by the operations that need views.
(defconst nelisp-aot--ensure
  '(if (= xb 0)
       (seq (setq xbm (nl_root_mark env))
            (setq xb (nl_root_frame env %XBW%))
            (if (= xb 0)
                (seq (setq xbm 0) (setq xb (alloc-bytes (* %XBW% 8) 8)))
              0))
     0))
(defconst nelisp-aot--scr
  '(seq (if (= scb 0) (setq scb (wf_aot_xb 160 0)) 0) scb))
(defconst nelisp-aot--gc-poll '(if (< (ptr-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0))
(defconst nelisp-aot--back-poll
  '(seq (setq bec (+ bec 1))
        (if (>= bec 255) (seq (setq bec 0) (wf_bytecode_poll)) 0)))

(defun nelisp-aot--fail-form (ctx leaders)
  "Failure continuation: exit with the signal, or enter the handler of CTX."
  (if ctx
      `(let* ((cc (wf_aot_catch env cnd)))
         (if (/= cc 0)
             (seq (setq ,(nelisp-aot--x (cdr ctx)) cc) (setq fail 2)
                  (setq blk ,(cl-position (car ctx) leaders)))
           ,nelisp-aot--fail))
    nelisp-aot--fail))

(defun nelisp-aot--slow (sp k call failf &optional label)
  "Run CALL (0 = ok) on the K operands ending below SP copied to block words
0..K-1; the result (block word 0) replaces operand SP-K."
  (setq nelisp-aot--maxarr (max nelisp-aot--maxarr k))
  `(seq ,nelisp-aot--ensure
        ,(nelisp-aot--site (or label (let ((txt (format "%S" call))) (substring txt 0 (min 50 (length txt))))))
        ,@(let ((j -1))
            (mapcar (lambda (_e) (setq j (1+ j))
                      `(ptr-write-u64 xb ,(* j 8) ,(nelisp-aot--x (+ (- sp k) j))))
                    (make-list k nil)))
        (if (= ,call 0) (setq ,(nelisp-aot--x (- sp k)) (ptr-read-u64 xb 0)) ,failf)))

(defun nelisp-aot--named (sp k name failf)
  "Call builtin NAME on K operands through the VM's named-primitive route."
  (progn
    (nelisp-aot--slow sp k `(seq ,@(let ((i -1))
                                     (mapcar (lambda (c) (setq i (1+ i)) `(ptr-write-u8 (+ ,nelisp-aot--scr 128) ,i ,c))
                                             (string-to-list name)))
                                 (wf_bytecode_call_named env xb ,k ,k (+ ,nelisp-aot--scr 128) ,(length name)))
                      failf (concat "named:" name))))

(defun nelisp-aot--undo (j)
  "Undo dynamic binding J (newest first by the caller)."
  (let ((bn (intern (format "bn%d" j))) (bv (intern (format "bv%d" j))) (bg (intern (format "bg%d" j))))
    `(if (= ,bg 0)
         (seq (if (= (logand ,bv 1) 0) (wf_dirty) 0)
              (wf_bytecode_w_lean_store ,bn ,bv))
       (wf_bytecode_unbind (+ env 32) 1))))

(defun nelisp-aot--emit-block (insns sps leaders start consts)
  "Return the DSL form for the block beginning at START."
  (let* ((pc start) (ops nil) (term nil) (kn (make-hash-table))
         (scr nelisp-aot--scr))
    (cl-flet ((x (i) (nelisp-aot--x i)))
      (while (not term)
        (let* ((ins (aref insns pc)) (op (car ins)) (arg (cadr ins)) (len (nth 2 ins))
               (sp (aref sps pc)) (next (+ pc len)) (top (1- sp))
               (bd (aref nelisp-aot--bds pc))
               (failf (nelisp-aot--fail-form (aref nelisp-aot--ctxs pc) leaders)))
          (cl-flet ((add (form &optional may-fail) (push (list form may-fail) ops)))
            (cond
             ((<= 1 op 7)
              (let ((n (if (<= op 5) op arg)))
                (add `(setq ,(x sp) ,(x (- sp 1 n))))))
             ((= op 137) (add `(setq ,(x sp) ,(x top))))
             ((= op 136) nil)
             ((or (= op 129) (>= op 192))
              (let ((i (if (= op 129) arg (- op 192))))
                (add `(setq ,(x sp) (ptr-read-u64 cdata ,(* i 8))))))
             ((= op 178) (add `(setq ,(x (- sp 1 arg)) ,(x top))))
             ((memq op '(64 65))
              (add `(let* ((w ,(x top)))
                      (if (= (logand w 7) 2)
                          (setq ,(x top) (ptr-read-u64 (- w 2) ,(if (= op 64) 0 8)))
                        (if (= (wf_bytecode_w_consp w) 1)
                            (setq ,(x top) (,(if (= op 64) 'nl_cons_car_word 'nl_cons_cdr_word) w))
                          (if (= (wf_bytecode_w_nilp w) 1)
                              (setq ,(x top) 3)
                            (seq (bf_wrong_type_listp (wf_bytecode_w_view w (alloc-bytes 32 8)))
                                 ,failf)))))
                   t))
             ((memq op '(162 163))
              (add `(let* ((w ,(x top)))
                      (setq ,(x top)
                            (if (= (logand w 7) 2) (ptr-read-u64 (- w 2) ,(if (= op 162) 0 8))
                              (if (= (wf_bytecode_w_consp w) 1)
                                  (,(if (= op 162) 'nl_cons_car_word 'nl_cons_cdr_word) w)
                                3))))))
             ((= op 58)
              (add `(let* ((w ,(x top)))
                      (setq ,(x top) (if (= (logand w 7) 2) 7
                                       (if (= (wf_bytecode_w_consp w) 1) 7 3))))))
             ((= op 60)
              (add `(let* ((w ,(x top)))
                      (setq ,(x top) (if (= w 3) 7
                                       (if (= (logand w 7) 2) 7
                                         (if (= (wf_bytecode_w_nilp w) 1) 7
                                           (if (= (wf_bytecode_w_consp w) 1) 7 3))))))))
             ((= op 57)
              (add `(let* ((w ,(x top)))
                      (setq ,(x top)
                            (if (if (= w 3) 1 (= w 7)) 7
                              (if (= (logand w 3) 1) 3
                                (if (= (logand w 7) 6)
                                    ;; vectorlike word: kind 4/16 symbol, kind 19 positioned
                                    ;; symbol (as `wf_bytecode_predicate' answers), else not.
                                    (let* ((k (logand (ptr-read-u8 w -6) 63)))
                                      (if (if (= k 4) 1 (= k 16)) 7
                                        (if (= k 19) (if (= (nl_positioned_flag 0) 1) 7 3) 3)))
                                  (if (= (logand w 7) 2) 3
                                    (if (= (logand w 7) 0)
                                        ;; a legacy box is its own view: no copy
                                        (if (= (wf_bytecode_predicate 57 w) 1) 7 3)
                                      (if (= (wf_bytecode_predicate
                                              57 (wf_bytecode_w_view w (alloc-bytes 32 8))) 1)
                                          7 3))))))))))
             ((= op 66)
              (add `(setq ,(x (- sp 2)) (wf_bytecode_w_cons2 ,(x (- sp 2)) ,(x top)))))
             ((= op 61)
              (add `(let* ((a ,(x (- sp 2))) (b ,(x top)))
                      (setq ,(x (- sp 2)) (if (= a b) 7 (if (= (wf_aot_eq a b) 1) 7 3))))))
             ((memq op '(75 158))
              (let ((k (if (= op 75) 1 2)))
                (add `(let* ((r ,(if (= op 75)
                                     `(wf_aot_symfn env ,(x top))
                                   `(wf_aot_assq ,(x (- sp 2)) ,(x top)))))
                        (if (/= r 0) (setq ,(x (- sp k)) r)
                          ,(nelisp-aot--slow sp k `(wf_bytecode_call_primitive env xb ,k ,op) failf)))
                     t)))
             ((= op 164)
              (add `(let* ((r (wf_aot_nconc2 ,(x (- sp 2)) ,(x top))))
                      (if (/= r 0) (setq ,(x (- sp 2)) r)
                        ,(nelisp-aot--slow sp 2 `(wf_bytecode_nconc2 env xb 2 ,scr) failf)))
                   t))
             ((= op 159)
              (add `(let* ((r (wf_aot_nreverse ,(x top))))
                      (if (/= r 0) (setq ,(x top) r)
                        ,(nelisp-aot--slow sp 1 `(wf_bytecode_w_boxed env xb 1 1 159 ,scr) failf)))
                   t))
             ((memq op '(83 84))
              (add `(let* ((a ,(x top))
                           (r (if (= (logand a 3) 1)
                                  (wf_bytecode_w_fix (,(if (= op 83) '- '+) (sar a 2) 1)) -1)))
                      (if (/= r -1) (setq ,(x top) r)
                        ,(nelisp-aot--slow sp 1 `(wf_bytecode_call_primitive env xb 1 ,op) failf)))
                   t))
             ((= op 85)
              (add `(let* ((a ,(x (- sp 2))) (b ,(x top)))
                      (if (if (= (logand a 3) 1) (= (logand b 3) 1) 0)
                          (setq ,(x (- sp 2)) (if (= a b) 7 3))
                        ,(nelisp-aot--slow sp 2 `(wf_bytecode_call_primitive env xb 2 85) failf)))
                   t))
             ((= op 166)
              (add (nelisp-aot--slow sp 2 `(if (= (wf_bytecode_small_arithmetic xb 2 166) 0) 0
                                            (wf_bytecode_w_boxed env xb 2 2 166 ,scr))
                                     failf)
                   t))
             ((= op 71) (add (nelisp-aot--named sp 1 "length" failf) t))
             ((= op 154) (add (nelisp-aot--named sp 2 "equal" failf) t))
             ((= op 78)
              (add `(let* ((r (wf_bytecode_w_get_fast ,(x (- sp 2)) ,(x top))))
                      (if (/= r 0) (setq ,(x (- sp 2)) r)
                        ,(nelisp-aot--named sp 2 "get" failf)))
                   t))
             ((= op 62)
              (add `(let* ((r (wf_bytecode_w_memq_fast ,(x (- sp 2)) ,(x top))))
                      (if (/= r 0) (setq ,(x (- sp 2)) r)
                        ,(nelisp-aot--named sp 2 "memq" failf)))
                   t))
             ((<= 67 op 70)
              (let* ((k (- op 66)) (form 3))
                (dotimes (j k)
                  (setq form `(wf_bytecode_w_cons2 ,(x (- sp 1 j)) ,form)))
                (add `(setq ,(x (- sp k)) ,form))))
             ((<= 32 op 37)
              (let* ((n (- op 32)) (fi (- sp n 1)) (callee (gethash fi kn))
                     (did (and callee (nelisp-aot--direct-for callee n)))
                     (generic
                      (progn
                        (setq nelisp-aot--maxarr (max nelisp-aot--maxarr (1+ n)))
                        `(seq ,nelisp-aot--gc-poll
                              ,nelisp-aot--ensure
                              ,(nelisp-aot--site (format "call:%s" (or callee "dyn")))
                              ,@(let ((j -1))
                                  (mapcar (lambda (_e) (setq j (1+ j))
                                            `(ptr-write-u64 xb ,(* j 8) ,(x (+ fi j))))
                                          (make-list (1+ n) nil)))
                              ;; Leaf subrs (symbol-with-pos-p, fboundp, ...) first, as the VM's
                              ;; call arm does (`wf_bytecode_call_subr'): 0 done, 1 signalled, 2 not a leaf.
                              (let* ((ll (wf_bytecode_call_leaf env xb 0 ,n (ptr-read-u64 xb 0))))
                                (if (= ll 0) (setq ,(x fi) (ptr-read-u64 xb 0))
                                  (if (= ll 1) ,failf
                                    (let* ((lr (wf_aot_lean_call env xb ,n)))
                                      (if (= lr 0) (setq ,(x fi) (ptr-read-u64 xb 0))
                                        (if (= lr 1) ,failf
                                          (if (= (wf_bytecode_call env xb ,(1+ n) ,n 0 1) 0)
                                              (setq ,(x fi) (ptr-read-u64 xb 0))
                                            ,failf)))))))))))
                ;; (autoload-do-load FUNDEF ...) with the constant callee symbol: GNU's C
                ;; `Fautoload_do_load' returns FUNDEF unless (car-safe FUNDEF) is `autoload'.
                ;; `wf_aot_adl' answers 1 only when the callee's live function cell is still
                ;; the captured prelude object and FUNDEF is no autoload form; otherwise the
                ;; generic call (redefinition, advice, a real autoload) runs as before.
                (when (and (eq callee 'autoload-do-load) (<= 1 n 3))
                  (setq generic
                        `(if (= (wf_aot_adl ,(x fi) ,(x (+ fi 1))) 1)
                             (setq ,(x fi) ,(x (+ fi 1)))
                           ,generic)))
                (add (if did
                         (let* ((e (assq callee nelisp-aot--direct)) (maxargs (nth 3 e)))
                           `(if (= (wf_aot_gd ,(x fi) ,(+ 32 (* did 8))) 1)
                                (let* ((r (,(intern (format "wf_aot_%d" did)) env
                                           ,@(append (mapcar (lambda (j) (x (+ fi 1 j))) (number-sequence 0 (1- n)))
                                                     (make-list (- maxargs n) 3)))))
                                  (if (= r 0) ,failf (setq ,(x fi) r)))
                              ,generic))
                       generic)
                     t)))
             ((= op 135)
              (setq term `(seq (setq res ,(x top)) (setq blk -1))))
             ((= op 130)
              (setq term (if (<= arg pc)
                             `(seq ,nelisp-aot--back-poll (setq blk ,(cl-position arg leaders)))
                           `(setq blk ,(cl-position arg leaders)))))
             ((memq op '(131 132))
              (let* ((nilp `(= (wf_bytecode_w_nilp ,(x top)) 1))
                     (tgt (cl-position arg leaders)) (fall (cl-position next leaders))
                     (jump-when (if (= op 131) nilp `(= ,nilp 0)))
                     (jump (if (<= arg pc) `(seq ,nelisp-aot--back-poll (setq blk ,tgt)) `(setq blk ,tgt))))
                (setq term `(if ,jump-when ,jump (setq blk ,fall)))))
             ((= op 133)
              (let* ((nilp `(= (wf_bytecode_w_nilp ,(x top)) 1))
                     (tgt (cl-position arg leaders)) (fall (cl-position next leaders))
                     (jump (if (<= arg pc) `(seq ,nelisp-aot--back-poll (setq blk ,tgt)) `(setq blk ,tgt))))
                (setq term `(if ,nilp ,jump (setq blk ,fall)))))
             ((<= 8 op 15)
              (let ((i (nelisp-aot--var-index op arg)))
                (add `(let* ((nm (ptr-read-u64 cdata ,(* i 8)))
                             (g (extern-call nl_gv_fast env nm)))
                        (if (if (= g -1) 1 (= g 2))
                            (seq ,nelisp-aot--ensure
                                 (if (= (wf_bytecode_w_varref env nm xb (+ ,scr 96)) 0)
                                     (setq ,(x sp) (ptr-read-u64 xb 0))
                                   ,failf))
                          (setq ,(x sp) g)))
                     t)))
             ((<= 16 op 23)
              (let ((i (nelisp-aot--var-index op arg)))
                (add `(seq ,nelisp-aot--ensure
                           (if (= (bf_set_dynamic_value env (ptr-read-u64 cdata ,(* i 8))
                                                        (wf_bytecode_w_view ,(x top) ,scr)) 0)
                               0 ,failf))
                     t)))
             ((<= 24 op 31)
              (let* ((i (nelisp-aot--var-index op arg))
                     (bn (intern (format "bn%d" bd))) (bv (intern (format "bv%d" bd)))
                     (bg (intern (format "bg%d" bd))))
                (add `(let* ((sym (ptr-read-u64 cdata ,(* i 8))) (val ,(x top))
                             (sv (wf_aot_lean_bind env sym val)))
                        (if (/= sv 0)
                            (seq (setq ,bn sym) (setq ,bv sv) (setq ,bg 0) (setq nb ,(1+ bd)))
                          (seq ,nelisp-aot--ensure
                               (if (= (wf_bytecode_varbind env sym (wf_bytecode_w_view val ,scr)) 0)
                                   (seq (setq ,bg 1) (setq nb ,(1+ bd)))
                                 ,failf))))
                     t)))
             ((<= 40 op 47)
              (let ((n (nelisp-aot--var-index op arg)))
                (add `(seq ,@(mapcar #'nelisp-aot--undo (number-sequence (1- bd) (- bd n) -1))
                           (setq nb ,(- bd n))))))
             ((= op 182)
              (when (/= (logand arg 128) 0)
                (add `(setq ,(x (- sp 1 (logand arg 127))) ,(x top)))))
             ((= op 49) (add `(setq cnd ,(x top))))
             ((= op 48) nil)
             ((= op 183)
              (let* ((targets (cdr (assq pc nelisp-aot--switches)))
                     (fall (cl-position next leaders))
                     (form `(setq blk ,fall)))
                (setq nelisp-aot--maxarr (max nelisp-aot--maxarr 2))
                (dolist (tg targets)
                  (setq form `(if (= tg ,tg) (setq blk ,(cl-position tg leaders)) ,form)))
                (setq term `(let* ((tg (let* ((r (wf_aot_switch ,(x (- sp 2)) ,(x (- sp 1))
                                                                ,nelisp-aot--codelen)))
                                         (if (/= r -3) r
                                           (seq ,nelisp-aot--ensure
                                                (ptr-write-u64 xb 0 ,(x (- sp 2)))
                                                (ptr-write-u64 xb 8 ,(x (- sp 1)))
                                                (wf_bytecode_switch xb 2 ,nelisp-aot--codelen ,scr))))))
                              ,form))))
             (t (nelisp-aot--refuse "emit: opcode %d" op))))
          ;; Track which stack slots hold a constant symbol (call-site targets).
          (cond ((or (= op 129) (>= op 192))
                 (let ((c (aref consts (if (= op 129) arg (- op 192)))))
                   (if (and c (not (eq c t)) (symbolp c)) (puthash sp c kn) (remhash sp kn))))
                ((= op 178) (remhash (- sp 1 arg) kn))
                ((memq op '(136 130 131 132 135)) nil)
                (t (let ((ns (and (< next (length sps)) (aref sps next))))
                     (when ns (remhash (1- ns) kn)))))
          (setq pc next)
          (when (and (not term) (memq pc leaders))
            (setq term `(setq blk ,(cl-position pc leaders))))))
      (let ((form term))
        (dolist (o ops)                 ; ops is reversed: last op first
          (setq form (if (cadr o)
                         `(seq ,(car o) (if (= fail 0) ,form 0))
                       `(seq ,(car o) ,form))))
        form))))

(defun nelisp-aot-translate (id obj)
  "Return the DSL defun `wf_aot_ID' for the byte-code object OBJ, or refuse."
  (let* ((template (aref obj 0)) (code (aref obj 1)) (consts (aref obj 2)) (depth (aref obj 3)))
    (unless (and (integerp template) (stringp code) (vectorp consts) (integerp depth))
      (nelisp-aot--refuse "not a lexical byte-code object"))
    (when (/= (logand template 128) 0) (nelisp-aot--refuse "&rest"))
    (when (multibyte-string-p code) (nelisp-aot--refuse "multibyte code"))
    (let* ((nargs (ash template -8))
           (insns (nelisp-aot--decode code))
           (an (nelisp-aot--analyze insns nargs depth consts))
           (sps (nth 0 an)) (leaders (nth 1 an)) (maxsp (nth 2 an))
           (nslots (1+ (max depth maxsp nargs 1)))
           (maxbd (nth 6 an))
           (nelisp-aot--ctxs (nth 3 an)) (nelisp-aot--switches (nth 4 an))
           (nelisp-aot--bds (nth 5 an))
           (nelisp-aot--codelen (length code))
           (nelisp-aot--maxarr 2)
           (handlers (delq nil (mapcar #'car (delete-dups (delq nil (append nelisp-aot--ctxs nil))))))
           (has-handlers (and handlers t))
           (blocks nil) (i 0))
      (when (> nargs 5) (nelisp-aot--refuse "more than 5 arguments"))
      (dolist (l leaders)
        (unless (aref sps l) (nelisp-aot--refuse "leader %d unreachable" l)))
      (dolist (l leaders)
        (push (cons i (nelisp-aot--emit-block insns sps leaders l consts)) blocks)
        (setq i (1+ i)))
      (setq blocks (nreverse blocks))
      (let* ((vec (apply #'vector (mapcar #'cdr blocks)))
             ;; Balanced decision tree over the block index: O(log n) per transition.
             (dispatch
              (cl-labels ((build (lo hi)
                            (if (= lo hi) (aref vec lo)
                              (let ((mid (/ (+ lo hi 1) 2)))
                                `(if (< blk ,mid) ,(build lo (1- mid)) ,(build mid hi))))))
                (build 0 (1- (length vec)))))
             (m (1+ nelisp-aot--maxarr))
             (body
              `(let* ((rec (ptr-read-u64 env 96)))
                 (if (if (< rec 100) 0 (>= rec (nl_eval_depth_limit env rec)))
                     (seq (wf_bytecode_signal_nesting) 0)
                   (let* (,@(mapcar (lambda (j) (list (nelisp-aot--x j) 0)) (number-sequence nargs (1- nslots)))
                          (res 0) (fail 0) (blk 0) (bec 0) (nb 0) (xb 0) (xbm 0) (scb 0) (cnd 0)
                          (cdata (ptr-read-u64 (data-addr nl_aot_state) ,(+ 104 (* id 8))))
                          ,@(apply #'append
                                   (mapcar (lambda (j) (list (list (intern (format "bn%d" j)) 0)
                                                             (list (intern (format "bv%d" j)) 0)
                                                             (list (intern (format "bg%d" j)) 0)))
                                           (number-sequence 0 (1- maxbd)))))
                     (seq
                      (ptr-write-u64 env 96 (+ rec 1))
                      ;; Entry counter (4 per entry, fixnum-coded): the observable the regression
                      ;; smoke reads to prove the AOT body ran.  One load, add and store.
                      (let* ((tb (ptr-read-u64 (data-addr nl_aot_state) 96)))
                        (if (/= tb 0)
                            (let* ((ca (+ tb ,(* (+ nelisp-aot--k id) 8))))
                              (ptr-write-u64 ca 0 (+ (ptr-read-u64 ca 0) 4)))
                          0))
                      ,nelisp-aot--gc-poll
                      (while (>= blk 0)
                        ,(if has-handlers `(seq (if (= fail 2) (setq fail 0) 0) ,dispatch) dispatch))
                      ,@(mapcar (lambda (j) `(if (> nb ,j) ,(nelisp-aot--undo j) 0))
                                (number-sequence (1- maxbd) 0 -1))
                      (if (/= xbm 0) (nl_root_release env xbm) 0)
                      (ptr-write-u64 env 96 rec)
                      (if (= fail 0) res 0)))))))
        (setq body (cl-subst m '%XBW% body))
        `(defun ,(intern (format "wf_aot_%d" id)) (env ,@(mapcar #'nelisp-aot--x (number-sequence 0 (1- nargs))))
           ,body)))))

(defun nelisp-aot-bytecode--objects ()
  "Return (ID NAME MAKE-FORM OBJ DEFUN) for each translatable build-list entry."
  (let ((text (nelisp-aot-bytecode-macroexp-text)) (rows nil) (nelisp-aot--direct nil))
    ;; Pass 1 finds the translatable functions; pass 2 regenerates the code
    ;; with direct call sites to exactly those.
    (dotimes (pass 2)
      (let ((id 0))
        (setq rows nil nelisp-aot--sites nil)
        (dolist (name nelisp-aot-bytecode-build-list)
          (let* ((mf (nelisp-aot-bytecode--make-form
                      (if (nelisp-aot-bytecode--late-name-p name)
                          (nelisp-aot-bytecode-subr-text name)
                        text)
                      name))
                 (obj (and mf (nelisp-aot-bytecode--object mf))))
            (condition-case err
                (progn
                  (unless obj (nelisp-aot--refuse "no bytecode definition of %s" name))
                  (push (list id name mf obj (let ((nelisp-aot--curname name)) (nelisp-aot-translate id obj))) rows)
                  (setq id (1+ id)))
              (nelisp-aot-refused
               (when (= pass 1) (message "[aot] refused %s: %s" name (cadr err)))))))
        (setq rows (nreverse rows))
        (when (= pass 0)
          (setq nelisp-aot--k (length rows))
          (setq nelisp-aot--direct
                (mapcar (lambda (r)
                          (let ((tm (aref (nth 3 r) 0)))
                            (list (nth 1 r) (nth 0 r) (logand tm 127) (ash tm -8))))
                        rows)))))
    rows))

(defvar nelisp-aot-bytecode--rows nil "Cache of `nelisp-aot-bytecode-rows'.")
(defun nelisp-aot-bytecode-rows ()
  "The translated rows (computed once), or the symbol `none'."
  (or nelisp-aot-bytecode--rows
      (setq nelisp-aot-bytecode--rows (or (nelisp-aot-bytecode--objects) 'none))))

(defun nelisp-aot-bytecode--live-rows ()
  "The translated rows as a list (nil when nothing was translated)."
  (let ((r (nelisp-aot-bytecode-rows))) (if (eq r 'none) nil r)))

(defun nelisp-aot-bytecode--install-rows (rows)
  (mapconcat
   (lambda (row)
     (let ((mf (nth 2 row)))
       (format "(nelisp--aot-install '%s %d %S %S %S)\n"
               (nth 1 row) (nth 0 row) (nth 1 mf) (nth 2 mf) (nth 3 mf))))
   rows ""))

(defun nelisp-aot-bytecode--seal-source (k)
  "Boot text capturing `autoload-do-load' and `autoload', then sealing the table.
Lean words 6 and 7 (spare) hold the autoload-do-load object captured when it is
the prelude's byte-code function, and the symbol `autoload'; `wf_aot_mask_sync'
mirrors them for the inline call-site fast path (`wf_aot_adl')."
  (concat
   (format "(let ((f (and (fboundp 'autoload-do-load) (symbol-function 'autoload-do-load))))\n  (when (byte-code-function-p f)\n    (aset nelisp--aot-table %d f)\n    (aset nelisp--aot-table %d 'autoload)))\n"
           (+ 6 (* 2 k)) (+ 7 (* 2 k)))
   (format "(aset nelisp--aot-table %d t)\n" (+ nelisp-aot--lw (* 2 k)))))

(defun nelisp-aot-bytecode-late-install-source ()
  "Boot text installing the late rows (subr.el functions) and sealing the table.
Empty when there is no late row (the seal is then part of the install source)."
  (let* ((rows (nelisp-aot-bytecode--live-rows)) (k (length rows))
         (late (cl-remove-if-not (lambda (r) (nelisp-aot-bytecode--late-name-p (nth 1 r))) rows)))
    (if (null late) ""
      (concat "\n;; --- nelisp AOT of GNU bytecode: late rows and seal ---\n"
              (nelisp-aot-bytecode--install-rows late)
              (nelisp-aot-bytecode--seal-source k)
              (if (equal (getenv "NELISP_AOT_DEFER") "1")
                  "(defvar nelisp--aot-table-saved nelisp--aot-table)\n(setq nelisp--aot-table nil)\n"
                "")))))

(defun nelisp-aot-bytecode-install-source ()
  "Return boot Lisp text defining the install table and installing each row."
  (let* ((rows (nelisp-aot-bytecode--live-rows)) (k (length rows)))
    (concat
     "\n;; --- nelisp AOT of GNU bytecode: install table (scripts/nelisp-aot-bytecode.el) ---\n"
     (format "(defvar nelisp--aot-table (make-vector %d %s) \"Installed AOT bytecode objects, entry counters, 8 lean-call words (hits, not-bytecode declines, AOT-installed declines, other pre declines, declines in the lean checks, disable flag, 2 spare), then a seal slot.\")\n"
             (+ 1 nelisp-aot--lw (* 2 k)) 0)
     (format "(defvar nelisp--aot-lean-words %d)\n" nelisp-aot--lw)
     (if nelisp-aot--prof
         (format "(defvar nelisp--aot-site-info '%S)\n"
                 (mapcar (lambda (c) (cons (format "%s" (car c)) (cdr c))) (reverse nelisp-aot--sites)))
       "")
     "(defun nelisp--aot-equal (a b)\n"
     "  (cond ((and (hash-table-p a) (hash-table-p b))\n"
     "         (and (= (hash-table-count a) (hash-table-count b))\n"
     "              (let ((ok t))\n"
     "                (maphash (lambda (k v) (unless (equal (gethash k b 'aot-missing) v) (setq ok nil))) a)\n"
     "                ok)))\n"
     "        ((and (vectorp a) (vectorp b))\n"
     "         (and (= (length a) (length b))\n"
     "              (let ((ok t) (i 0))\n"
     "                (while (and ok (< i (length a)))\n"
     "                  (unless (nelisp--aot-equal (aref a i) (aref b i)) (setq ok nil))\n"
     "                  (setq i (1+ i)))\n"
     "                ok)))\n"
     "        (t (equal a b))))\n"
     "(defun nelisp--aot-install (name id arity code consts)\n"
     "  (let ((f (and (fboundp name) (symbol-function name))))\n"
     "    (if (and (byte-code-function-p f) (eq (aref f 0) arity) (equal (aref f 1) code)\n"
     "             (nelisp--aot-equal (aref f 2) consts))\n"
     "        (progn (aset nelisp--aot-table id f) t)\n"
     "      nil)))\n"
     ;; The counters start as fixnum 0 and the object slots as nil.
     (if rows
         (format "(let ((i 0)) (while (< i %d) (aset nelisp--aot-table i nil) (setq i (1+ i))))\n" k)
       "")
     (nelisp-aot-bytecode--install-rows
      (cl-remove-if (lambda (r) (nelisp-aot-bytecode--late-name-p (nth 1 r))) rows))
     ;; Seal: until it is t the call-arm filter stays all-maybe (see wf_aot_mask_sync).
     ;; With late rows the seal moves to `nelisp-aot-bytecode-late-install-source'.
     (if (cl-some (lambda (r) (nelisp-aot-bytecode--late-name-p (nth 1 r))) rows) ""
       (nelisp-aot-bytecode--seal-source k))
     ;; Debug aid: NELISP_AOT_DEFER=1 builds an image whose AOT table is detached
     ;; (everything runs on the VM); scripts re-enable it from `nelisp--aot-table-saved'.
     (if (and (equal (getenv "NELISP_AOT_DEFER") "1")
              (not (cl-some (lambda (r) (nelisp-aot-bytecode--late-name-p (nth 1 r))) rows)))
         "(defvar nelisp--aot-table-saved nelisp--aot-table)\n(setq nelisp--aot-table nil)\n"
       ""))))


(defun nelisp-aot-bytecode-dsl-defuns ()
  "Return the DSL defuns: fixed helpers, call chain and generated functions."
  (let* ((rows (nelisp-aot-bytecode--live-rows)) (k (length rows)))
    (append
     (nelisp-aot--subst-l '((defun wf_aot_eq (a b)
         ;; Mirrors the VM fast loop's `eq' arm (raw 61): `eq' on two words through
         ;; the allocation-free word compare (no 32-byte view is allocated for a
         ;; mixed fixnum/box pair).
         (wf_bytecode_w_eqw a b))
       (defun wf_aot_entry_vec (entry)
         ;; No VM arm.  The table vector WORD held by the cached mirror entry of
         ;; `nelisp--aot-table', or 0 (same cached-entry read as `nl_fx_tbl_cache').
         (let* ((w (nl_record_slot_word entry 0)))
           (if (= (logand w 7) 6)
               (if (= (wf_bytecode_w_tag w) 8) w 0)
             0)))
       (defun wf_aot_vec (env)
         ;; No VM arm.  The table vector WORD, or 0 when the variable is unbound.
         ;; The mirror entry is cached in `nl_aot_state' and validated by
         ;; `nl_mxcache_epoch', like `nl_fx_tbl_cache'; the miss path builds the
         ;; symbol name `nelisp--aot-table' from raw words and looks it up once.
         (let* ((entry (if (= (ptr-read-u64 (data-addr nl_aot_state) 8)
                              (ptr-read-u64 (data-addr nl_mxcache_epoch) 0))
                           (ptr-read-u64 (data-addr nl_aot_state) 0)
                         0)))
           (if (/= entry 0)
               (wf_aot_entry_vec entry)
             (let* ((buf (alloc-bytes 24 1)) (sym (alloc-bytes 32 8)) (val (alloc-bytes 32 8)))
               (seq
                (ptr-write-u64 buf 0 3255381746650998126)
                (ptr-write-u64 (+ buf 8) 0 7809911855420567393)
                (ptr-write-u64 (+ buf 16) 0 101)
                (nl_alloc_symbol buf 17 sym)
                (if (/= (nl_env_lookup_val sym env val) 0) 0
                  (let* ((e (nelisp_mirror_lookup_entry (+ env 0) sym)))
                    (if (= e 0) 0
                      (seq (ptr-write-u64 (data-addr nl_aot_state) 0 0)
                           (ptr-write-u64 (data-addr nl_aot_state) 8
                                          (ptr-read-u64 (data-addr nl_mxcache_epoch) 0))
                           (ptr-write-u64 (data-addr nl_aot_state) 0 e)
                           (wf_aot_entry_vec e))))))))))
       (defun wf_aot_lean_bind (env name valw)
         ;; Mirrors `wf_bytecode_w_lean_bind' (the varbind arms, raw 24-31).  The
         ;; saved value WORD (never 0) when bound in place; 0 when NAME needs the
         ;; general binder (nothing done).  w3spec: a shallow binding is a fixed
         ;; {NAME, SAVED} record kept in native locals by the caller (the VM keeps
         ;; it in its AUX block), no consing and no lookup: an interned symbol WORD
         ;; is its own entry and its value cell is at +16.  Declined, nothing done:
         ;; non-symbol-object names, uninterned, alias/buffer-local/forwarded/
         ;; trapped cells, watcher mode, workers, root-pin capture, keywords and
         ;; legacy view values.  The matching unbind is `wf_bytecode_w_lean_store'.
         (if (/= (logand name 7) 6) 0
           (if (= (logand valw 7) 0) 0
             (let* ((hdr (ptr-read-u64 (- name 6) 0)))
               (if (/= (logand hdr 3903) 4) 0
                 (if (/= (logand (ptr-read-u64 env 112) 1048576) 0) 0
                   (if (/= (ptr-read-u64 env 168) 0) 0
                     (if (/= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0) 0
                       (if (= (ptr-read-u64 (data-addr nl_root_pin_control) 48) 2) 0
                         (if (= (let* ((nm (ptr-read-u64 (- name 6) 8)))
                                  (if (= (logand nm 7) 4)
                                      (let* ((sb (- nm 4)) (n0 (ptr-read-u64 sb 8))
                                             (n (if (< n0 0)
                                                    (logand (ptr-read-u64 sb 0) 9223372036854775807)
                                                  n0)))
                                        (if (< n 1) 0
                                          (if (= (heap-read-u8 (ptr-read-u64 sb 24) 0) 58) 1 0)))
                                    (bf_keyword_raw name)))
                                1)
                             0
                           (let* ((saved0 (ptr-read-u64 (- name 6) 16))
                                  (saved (if (= (logand saved0 7) 0) (nl_word_canon saved0) saved0)))
                             (if (= saved 0) 0
                               (seq
                                (if (= (logand valw 1) 0) (wf_dirty) 0)
                                (ptr-write-u64 (- name 6) 16 valw)
                                saved)))))))))))))
       (defun wf_aot_find_in (v func)
         ;; No VM arm.  1 + the installed id of FUNC in table vector V, else 0
         ;; (linear scan of the at most 8 installed object slots).
         (let* ((n (- (/ (vector-len v) 2) %HL2%)) (i 0) (r 0) (p (nl_word_payload func)))
           (seq (while (if (= r 0) (< i n) 0)
                  (let* ((w (nl_vector_slot_word v i)))
                    (seq (if (= (logand w 7) 6)
                             (if (= (nl_word_payload w) p) (setq r (+ i 1)) 0)
                           0)
                         (setq i (+ i 1)))))
                r)))
       (defun wf_aot_maybe (p)
         ;; No VM arm.  Inline miss filter on the callee's payload address P: 0 =
         ;; certainly not installed (a 64-bit bloom word, state +16).  Valid only
         ;; for the compaction epoch it was built in (state +24 holds epoch+1, so
         ;; a zero-fresh state is never valid).
         (if (= (ptr-read-u64 (data-addr nl_aot_state) 24)
                (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
             (logand (shr (ptr-read-u64 (data-addr nl_aot_state) 16)
                          (logand (+ (shr p 3) (shr p 9)) 63))
                     1)
           1))
       (defun wf_aot_mask_sync (v)
         ;; No VM arm.  Once the table is sealed, rebuild the call-arm filter and the per-id
         ;; caches in nl_aot_state: +16 filter, +24 epoch+1 (valid flag), +32+8i the
         ;; installed object WORD, +96 the table data address (entry counters),
         ;; +104+8i the constants data address.  Lisp objects do not move (the
         ;; production collector is mark+sweep).
         (if (= (ptr-read-u64 (data-addr nl_aot_state) 24)
                (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
             0
           (let* ((n (- (/ (vector-len v) 2) %HL2%)) (i 0) (m 0))
             (if (if (/= (nl_vector_slot_word v (+ %L% (* 2 n))) 7) 1 (> n 8)) 0
               (seq (while (< i n)
                      (let* ((w (nl_vector_slot_word v i)))
                        (seq (if (= (logand w 7) 6)
                                 (setq m (logior m (shl 1 (logand (+ (shr (nl_word_payload w) 3)
                                                                     (shr (nl_word_payload w) 9))
                                                                  63))))
                               0)
                             (setq i (+ i 1)))))
                    (ptr-write-u64 (data-addr nl_aot_state) 16 m)
                    (ptr-write-u64 (data-addr nl_aot_state) 96 (+ (nl_word_payload v) 8))
                    (ptr-write-u64 (data-addr nl_aot_state) 176 (+ (nl_word_payload v) (+ 8 (* 16 n))))
                    ;; Lean words 6/7: the captured autoload-do-load object and `autoload',
                    ;; kept only when both are heap words (else 0: the inline path declines).
                    (let* ((aw (nl_vector_slot_word v (+ (* 2 n) 6)))
                           (sw (nl_vector_slot_word v (+ (* 2 n) 7))))
                      (seq (ptr-write-u64 (data-addr nl_aot_state) 184
                                          (if (= (logand aw 7) 6) (if (= (logand sw 7) 6) aw 0) 0))
                           (ptr-write-u64 (data-addr nl_aot_state) 192
                                          (if (= (logand sw 7) 6) sw 0))))
                    (setq i 0)
                    (while (< i n)
                      (let* ((w (nl_vector_slot_word v i)))
                        (seq (ptr-write-u64 (data-addr nl_aot_state) (+ 32 (* i 8))
                                            (if (= (logand w 7) 6) w 0))
                             (ptr-write-u64 (data-addr nl_aot_state) (+ 104 (* i 8))
                                            (if (= (logand w 7) 6)
                                                (+ (nl_word_payload
                                                    (ptr-read-u64 (+ (nl_word_payload w) 16) 16))
                                                   8)
                                              0))
                             (setq i (+ i 1)))))
                    (ptr-write-u64 (data-addr nl_aot_state) 24
                                   (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
                    0)))))
       (defun wf_aot_gd (fw off)
         ;; No VM arm; guards the direct native call of an AOT callee.  1 when FW
         ;; (a constant callee symbol word) currently has as its function cell
         ;; exactly the installed object cached at state+OFF, so redefinition or
         ;; advice of the callee falls back to the generic call.
         (if (= (logand fw 7) 6)
             (if (= (logand (ptr-read-u8 fw -6) 63) 4)
                 (let* ((slot (ptr-read-u64 fw 18)))
                   (if (= slot 0) 0
                     (if (= (ptr-read-u64 (data-addr nl_aot_state) 24)
                            (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
                         (if (= slot (ptr-read-u64 (data-addr nl_aot_state) off)) 1 0)
                       0)))
               0)
           0))
       (defun wf_aot_adl (fw a)
         ;; 1 when the call (autoload-do-load A ...) through the constant callee
         ;; symbol word FW may answer A directly: FW's function cell is exactly the
         ;; original autoload-do-load object captured at state+184 (state valid),
         ;; and A is not a cons whose car is the symbol `autoload' (state+192).
         ;; Declines (0) on anything else, including an unusual cons shape.
         (if (= (logand fw 7) 6)
             (if (= (logand (ptr-read-u8 fw -6) 63) 4)
                 (let* ((slot (ptr-read-u64 fw 18)))
                   (if (= slot 0) 0
                     (if (= slot (ptr-read-u64 (data-addr nl_aot_state) 184))
                         (if (= (ptr-read-u64 (data-addr nl_aot_state) 24)
                                (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
                             (if (= (logand a 7) 2)
                                 (if (= (ptr-read-u64 (- a 2) 0)
                                        (ptr-read-u64 (data-addr nl_aot_state) 192))
                                     0 1)
                               (if (= (logand a 1) 1) 1
                                 (if (= a 3) 1
                                   (if (= (wf_bytecode_w_consp a) 1) 0 1))))
                           0)
                       0)))
               0)
           0))
       (defun wf_aot_xb (nbytes scroff)
         ;; No VM arm (the VM keeps these on its rooted frame).  Heap block for the
         ;; view scratch of slow operations: the 128-byte view scratch at SCROFF
         ;; (first view a valid nil), then a name buffer.  The caller keeps it in a
         ;; local, so the collector scans it.
         (let* ((b (alloc-bytes nbytes 8)))
           (seq (ptr-write-u64 b scroff 0) (ptr-write-u64 b (+ scroff 8) 0)
                (ptr-write-u64 b (+ scroff 16) 0) (ptr-write-u64 b (+ scroff 24) 0)
                b)))
       (defun wf_aot_catch (env cnd)
         ;; Mirrors the condition-case handler resume of `wf_bytecode_execute_frame'
         ;; (pushconditioncase, raw 49).  The condition object (SIGNAL . DATA) when
         ;; the stashed signal matches the condition list CND (exit state cleared,
         ;; as the VM does when it resumes a handler); 0 otherwise.
         (if (= (ptr-read-u64 268435472 0) 1)
             (if (= (wf_bytecode_condition_matches env (nl_exit_tag_w) cnd) 1)
                 (let* ((c (wf_bytecode_w_cons2 (nl_exit_tag_w) (nl_exit_val_w))))
                   (seq (ptr-write-u64 268435472 0 0)
                        (ptr-write-u64 (data-addr nl_bt_snapshot) 0 0)
                        (atomic-fetch-add 268435544 1)
                        c))
               0)
           0))
       (defun wf_aot_try (func mem first argc env)
         ;; Hook at the entry of `wf_bytecode_w_function' (the VM function-call
         ;; path).  2 = FUNC is not installed (run the VM); else the result WORD,
         ;; 0 = signalled.  Worker threads always run the VM (their stacks are not
         ;; scanned).
         (if (/= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0) 2
           (let* ((v (if (= (wf_aot_maybe (nl_word_payload func)) 0) 0 (wf_aot_vec env))))
             (if (= v 0) 2
               (let* ((aid (seq (wf_aot_mask_sync v) (wf_aot_find_in v func))))
                 (if (= aid 0) 2
                   (if (= (ptr-read-u64 (data-addr nl_aot_state) 24)
                          (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
                       (wf_aot_call (- aid 1) func mem first argc env)
                     2)))))))
       (defun wf_aot_vm_call (env mem sp argc cell)
         ;; Hook in the VM fast loop's call arm, before `wf_bytecode_call_subr', for
         ;; a call of an installed callee: 2 = not handled, 0 = value stored at the
         ;; function word, 1 = signalled.
         (let* ((fi (- sp (+ argc 1))))
           (if (< fi 0) 2
             (let* ((fw (ptr-read-u64 mem (* fi 8))))
               (if (if (< fw 4096) 1 (/= (logand fw 7) 6)) 2
                 (let* ((k (logand (ptr-read-u8 fw -6) 63))
                        (func (if (= k 17) fw (if (= k 4) (ptr-read-u64 fw 18) 0))))
                   (if (if (< func 4096) 1 (/= (logand func 7) 6)) 2
                     (if (/= (logand (ptr-read-u8 func -6) 63) 17) 2
                       (if (= (wf_aot_maybe (- func 6)) 0) 2
                         (let* ((tpl (sar (ptr-read-u64 (- func 6) 16) 2)))
                           (if (if (< argc (logand tpl 127)) 1 (> argc (/ tpl 256))) 2
                             (let* ((r (wf_aot_try func mem (+ fi 1) argc env)))
                               (if (= r 2) 2
                                 (if (= r 0) 1
                                   (seq (ptr-write-u64 mem (* fi 8) r) 0)))))))))))))))
       (defun wf_aot_lean_call (env xb argc)
         ;; Stage 4d: cheap native->VM entry for the generic call arm.  XB is the
         ;; caller's rooted operand block: XB[0] the function word, XB[1..ARGC] the
         ;; arguments; the value replaces XB[0].  The callee frame is reserved on the
         ;; root stack (LIFO, released here on every exit) and run by
         ;; `wf_cb_lean_call', the mapcar callback entry, whose ROOTS layout
         ;; (+24 result, +32 arguments) XB-24 matches.  0 done, 1 signalled,
         ;; 2 not handled (nothing ran; the caller takes `wf_bytecode_call').
         ;; Declines: lean disabled (flag), a worker thread registered, the AOT
         ;; state not valid, a callee that is not a lexical byte-code closure
         ;; (redefinition, advice, subr, macro), an AOT-installed callee, a
         ;; depth outside the VM's range, or any decline of `wf_cb_lean_call'.
         ;; Counters (lean region at nl_aot_state+176): +0 hits, +8 callee not a
         ;; bytecode closure, +16 AOT-installed callee, +24 other declines before the
         ;; reservation, +32 declines in the lean checks, +40 disable flag.
         (let* ((lc (ptr-read-u64 (data-addr nl_aot_state) 176)))
           (if (if (= lc 0) 1
                 (if (/= (ptr-read-u64 lc 40) 1) 1
                   (if (/= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0) 1
                     (/= (ptr-read-u64 (data-addr nl_aot_state) 24)
                         (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1)))))
               2
             (let* ((fw (ptr-read-u64 xb 0))
                    ;; Cheap pre-filter: a symbol whose function slot is a cons or a
                    ;; non-closure object (subr, macro, interpreted function) is declined
                    ;; without the environment lookup.
                    (skip (if (/= (logand fw 7) 6) 0
                            (if (/= (logand (ptr-read-u8 fw -6) 63) 4) 0
                              (let* ((slot (ptr-read-u64 fw 18)))
                                (if (= (logand slot 7) 2) 1
                                  (if (= (logand slot 7) 6)
                                      (if (< slot 4096) 0
                                        (if (= (logand (ptr-read-u8 slot -6) 63) 17) 0 1))
                                    0))))))
                    (target (if (= skip 1) 0 (wf_cb_target env fw))))
               (if (if (= target 0) 1 (< (record-slot-count target) 4))
                   (seq (ptr-write-u64 lc 8 (+ (ptr-read-u64 lc 8) 4)) 2)
                 (let* ((rp (+ (nl_word_payload target) 16))
                        (dw (ptr-read-u64 rp 0))
                        (sw (ptr-read-u64 rp 24))
                        (depth (sar sw 2))
                        (bad (if (/= (logand dw 3) 1) 1
                               (if (/= (logand sw 3) 1) 1
                                 (if (< depth 0) 1
                                   (if (> depth 131063) 1 0)))))
                        (inst (if (= bad 1) 0
                                (if (= (wf_aot_maybe (nl_word_payload target)) 0) 0
                                  (if (/= (wf_aot_find target env) 0) 1 0)))))
                   (if (= inst 1)
                       (seq (ptr-write-u64 lc 16 (+ (ptr-read-u64 lc 16) 4)) 2)
                     (if (= bad 1)
                         (seq (ptr-write-u64 lc 24 (+ (ptr-read-u64 lc 24) 4)) 2)
                     (let* ((mark (nl_root_mark env))
                            (cell (nl_root_frame env 4))
                            (fp (if (= cell 0) 0 (nl_root_frame env (+ depth 7)))))
                       (if (if (= fp 0) 1 (/= fp (+ cell 64)))
                           (seq (nl_root_release env mark)
                                (ptr-write-u64 lc 24 (+ (ptr-read-u64 lc 24) 4))
                                2)
                         (let* ((frame (nl_n4_push_words env fw (+ xb 8) argc))
                                (lean (wf_cb_lean_call target (- xb 24) argc env cell)))
                           (if (= lean 2)
                               (seq (if (= frame 0) 0
                                      (ptr-write-u64 (ptr-read-u64 env 176) 0
                                                     (- (ptr-read-u64 (ptr-read-u64 env 176) 0) 1)))
                                    (nl_root_release env mark)
                                    (ptr-write-u64 lc 32 (+ (ptr-read-u64 lc 32) 4))
                                    2)
                             (let* ((r (if (= (nl_n4_finish_w (if (= lean 0) (ptr-read-u64 xb 0) 0)
                                                              env 0 frame) 0)
                                           1 0)))
                               (seq (nl_root_release env mark)
                                    (ptr-write-u64 lc 0 (+ (ptr-read-u64 lc 0) 4))
                                    r))))))))))))))
       (defun wf_aot_nreverse (head)
         ;; The VM fast loop's nreverse arm (stage 2a, raw 159) on a word: the
         ;; reversed head, or 0 (nothing written) for a dotted, circular, long or
         ;; non-list operand.  No allocation, no scratch.
         (if (= head 3) 3
           (if (/= (logand head 7) 2) 0
             (let* ((p head) (prev 3) (nx 0) (n 0) (st 0))
               (seq
                (while (= st 0)
                  (seq (setq nx (ptr-read-u64 (- p 2) 8))
                       (if (= (logand nx 7) 2)
                           (seq (setq p nx) (setq n (+ n 1))
                                (if (> n 4096) (setq st 2) 0))
                         (setq st (if (= nx 3) 1 2)))))
                (if (= st 1)
                    (seq (wf_dirty)
                         (setq p head)
                         (while (/= p 3)
                           (seq (setq nx (ptr-read-u64 (- p 2) 8))
                                (ptr-write-u64 (- p 2) 8 prev)
                                (setq prev p)
                                (setq p nx)))
                         prev)
                  0))))))
       (defun wf_aot_nconc2 (nfirst nsecond)
         ;; The VM fast loop's nconc arm (stage 2a, raw 164): the result word, or 0
         ;; (nothing written) when the operands are not the simple case.
         (if (= nfirst 3) nsecond
           (if (if (= (logand nfirst 7) 2) (/= (logand nsecond 7) 0) 0)
               (let* ((tail nfirst) (nx 0) (n 0) (st 0))
                 (seq
                  (while (= st 0)
                    (seq (setq nx (ptr-read-u64 (- tail 2) 8))
                         (if (= (logand nx 7) 2)
                             (seq (setq tail nx) (setq n (+ n 1))
                                  (if (> n 4096) (setq st 2) 0))
                           (setq st (if (= (logand nx 7) 0) 2 1)))))
                  (if (= st 1)
                      (seq (wf_dirty) (ptr-write-u64 (- tail 2) 8 nsecond) nfirst)
                    0)))
             0)))
       (defun wf_aot_symfn (env sa)
         ;; The VM fast loop's symbol-function arm (raw 75): the function word, or 0
         ;; (decline, nothing done).
         (if (= (logand sa 7) 6)
             (if (/= (wf_bytecode_w_symobj_p sa) 0)
                 (let* ((fv (nelisp_env_lookup_function_w (+ env 0) (+ env 64) sa 0)))
                   (if (= fv 0) 0
                     (if (= fv 2) 3 (if (= (nl_word_tag64 fv) 0) 3 fv))))
               0)
           0))
       (defun wf_aot_assq (a b)
         ;; The VM fast loop's assq arm (raw 158), bounded to 64 elements: the
         ;; matching pair or nil, or 0 (decline) for anything it does not take.
         (if (= (wf_bytecode_w_tag a) 19) 0
           (let* ((ref 64) (word 0) (cur b))
             (seq
              (while (if (= word 0) (if (> ref 0) (= (wf_bytecode_w_consp cur) 1) 0) 0)
                (let* ((el (nl_cons_car_word cur)))
                  (seq
                   (if (= (wf_bytecode_w_consp el) 1)
                       (if (= (wf_bytecode_w_tag (nl_cons_car_word el)) 19) (setq word -1)
                         (if (= (wf_bytecode_w_eqw a (nl_cons_car_word el)) 1) (setq word el) 0))
                     0)
                   (if (= word 0) (seq (setq cur (nl_cons_cdr_word cur)) (setq ref (- ref 1))) 0))))
              (if (= word -1) 0
                (if (/= word 0) word
                  (if (= (wf_bytecode_w_nilp cur) 1) 3 0)))))))
       (defun wf_aot_switch (a b codelen)
         ;; The VM fast loop's switch arm (raw 183) on words, no view scratch: the
         ;; absolute target offset, -1 for a miss, -3 when this arm declines (the
         ;; caller then takes `wf_bytecode_switch').
         (if (if (= (wf_bytecode_w_tag a) 19) 1 (= (logand b 1) 1)) -3
           (if (/= (bf_hash_table_p_raw b) 1) -3
             (let* ((mode (nl_ht_test_mode b)))
               (if (if (= mode 1) 0 (/= mode 2)) -3
                 (let* ((ref (wf_bytecode_switch_find b a mode)))
                   (if (<= ref 0) -1
                     (let* ((word (nl_ht_entry_value b ref)))
                       (if (/= (wf_bytecode_w_tag word) 2) -3
                         (let* ((off (wf_bytecode_w_int word)))
                           (if (if (< off 0) 1 (>= off codelen)) -3 off)))))))))))
       (defun wf_aot_find (func env)
         ;; No VM arm.  1 + the installed id of the callee word FUNC, else 0; used
         ;; by the VM call arms to decline the lean entry for AOT-installed targets.
         (let* ((v (wf_aot_vec env)))
           (if (= v 0) 0
             (seq (wf_aot_mask_sync v)
                  (wf_aot_find_in v func)))))
       ))
     ;; Profiling counter helper: emitted only by an NELISP_AOT_PROF=1 build, so
     ;; the default image carries no profiling code.
     (when nelisp-aot--prof
       '((defun wf_aot_prof (i)
           ;; Count one execution of profiled site I (words from lean region +64).
           (let* ((lc (ptr-read-u64 (data-addr nl_aot_state) 176)))
             (if (= lc 0) 0
               (ptr-write-u64 lc (+ 64 (* i 8)) (+ (ptr-read-u64 lc (+ 64 (* i 8))) 4)))))))
     ;; The generated call chain: `wf_aot_call' dispatches on the install id to the
     ;; per-id entry wrapper `wf_aot_ent_ID', which loads the (at most 5) argument
     ;; words from the VM argument memory (missing optionals are nil, word 3) and
     ;; calls the translated body `wf_aot_ID'.  Unknown ids answer 2 (not handled).
     (list
      (let ((chain 2))
        (dotimes (j k)
          (setq chain `(if (= id ,(- k 1 j)) (,(intern (format "wf_aot_ent_%d" (- k 1 j)))
                                               mem first argc env)
                         ,chain)))
        `(defun wf_aot_call (id func mem first argc env) ,chain)))
     (mapcar (lambda (row)
               (let* ((id (nth 0 row)) (nargs (ash (aref (nth 3 row) 0) -8)))
                 `(defun ,(intern (format "wf_aot_ent_%d" id)) (mem first argc env)
                    (,(intern (format "wf_aot_%d" id)) env
                     ,@(mapcar (lambda (j) `(if (< ,j argc) (ptr-read-u64 mem (* (+ first ,j) 8)) 3))
                               (number-sequence 0 (1- nargs)))))))
             rows)
     (mapcar (lambda (row) (nth 4 row)) rows))))

(provide 'nelisp-aot-bytecode)
;;; nelisp-aot-bytecode.el ends here
