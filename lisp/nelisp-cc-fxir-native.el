;;; nelisp-cc-fxir-native.el --- fxir -> native code for prelude closures (nativecomp N1) -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage N1 of target/nativecomp/DESIGN.md (worktree resume-4943-20261024):
;; lowering of the fx compiler's fxir (`nelisp--fx-want-ir' in
;; scripts/nelisp-stdlib-prelude.el) to the AOT dialect on runtime WORDs.
;;
;; Input: rows harvested from a running image into
;; lisp/nelisp-fxir-native-data.el (tools/ai/nc1-harvest.el):
;;   (NAME ID NLOCALS CALLEES IR)
;; CALLEES are the names the IR's call nodes call, in walk order: the
;; lowering inlines GNU's byte-opcode primitives (car, cdr, eq, aref, ...)
;; by name, so the install pass converts a record only when its fxir and its
;; callees are both the harvested ones (scripts/nelisp-fxir-native-install.el).
;;
;; Output: one dialect defun nc_ID (code env out args nparams argw wout) per
;; row -- `m5_fx_run_body''s signature minus NLOCALS; the VM runs it for a
;; record whose NLOCALS is -(16+ID) -- plus the dispatcher nc_run and a few
;; shared out-of-line slow paths.  The code shape follows DESIGN 4.4:
;;  - a tree walk over the structured fxir: real branches and loops, no pc
;;    dispatcher, no operand stack;
;;  - locals and temporaries are `let*' variables, i.e. frame slots under the
;;    compiler's stack maps; nothing tagged stays in a register across a call;
;;  - a call stages its arguments in a small rooted window (one
;;    `nl_root_frame' per activation: arguments, result word, the two words
;;    `m5_fx_callout' keeps the callee's definition in), as the VM's operand
;;    stack does;
;;  - inline fast paths with a type test for the VM's fixnum ops and GNU's
;;    opcode primitives; every other case is the generic call (the same
;;    helpers the VM uses), so results are the VM's by construction;
;;  - non-local exits are the fx status protocol: a failing helper's rc is
;;    kept in ST, the rest of the body is skipped by `(if (= st 0) ...)'
;;    guards, and the single exit unbinds the activation's dynamic bindings.
;;
;; The opcode primitives are inlined without a function-cell guard, as GNU's
;; byte compiler compiles `car' to Bcar: a later (fset 'car ...) is not seen
;; by compiled code (GNU-faithful; the VM honoured it).  Every other call goes
;; through the live function cell.
;;
;; Compiler lowering only: no reader builtin, no :lit arm, no data blob.

;;; Code:

(require 'cl-lib)

(defconst nelisp-cc-fxir-native--data-files
  (mapcar (lambda (f)
            (cons (expand-file-name (concat f ".el")
                                    (file-name-directory (or load-file-name buffer-file-name
                                                             default-directory)))
                  (intern f)))
          '("nelisp-fxir-native-data" "nelisp-fxir-native-corpus-data"))
  "Harvested rows: the prelude hot set, then the fx corpus functions
\(test/nelisp-fx-adopt-corpus.el) whose native transcripts
tools/ai/nc1-native-check.sh compares; a corpus row is dormant unless a
function of that name with the same fxir is defined.")

(defvar nelisp-cc-fxir-native-windows
  (string-prefix-p "windows" (or (getenv "NELISP_STANDALONE_TARGET") ""))
  "Non-nil generates no rows, so the word VM keeps every record: Win64 builds
\(the Win64 lowering is not validated yet).")

(defun nelisp-cc-fxir-native--read-data (file const)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((found nil) (form nil))
        (condition-case nil
            (while (not found)
              (setq form (read (current-buffer)))
              (when (and (eq (car-safe form) 'defconst) (eq (nth 1 form) const))
                (setq found (eval (nth 2 form) t))))
          (end-of-file nil))
        found))))

(defun nelisp-cc-fxir-native--rows ()
  "All harvested rows (NAME ID NLOCALS CALLEES IR ARITY); nil for Win64."
  (unless nelisp-cc-fxir-native-windows
    (apply #'append
           (mapcar (lambda (fc) (nelisp-cc-fxir-native--read-data (car fc) (cdr fc)))
                   nelisp-cc-fxir-native--data-files))))

;; ---- per-function generator state ----
(defvar nelisp-cc-fxir-native--temps nil "Temporaries of the function being generated.")
(defvar nelisp-cc-fxir-native--callees nil "Remaining callee names, in walk order.")
(defvar nelisp-cc-fxir-native--maxargc 0 "Largest staged argument count.")
(defvar nelisp-cc-fxir-native--mutant nil
  "Non-nil generates a deliberately wrong `<' (as `>'): the negative control.")

(defun nelisp-cc-fxir-native--temp ()
  "A fresh temporary variable (a stack-mapped frame slot, nil at entry)."
  (let ((v (intern (format "t%d" (length nelisp-cc-fxir-native--temps)))))
    (push v nelisp-cc-fxir-native--temps)
    v))

(defun nelisp-cc-fxir-native--local (n) (intern (format "l%d" n)))

(defun nelisp-cc-fxir-native--cv (k)
  "The code-vector word K, read where the VM reads it."
  ;; (* K 8) stays a form: a computed address keeps a physical load, as in
  ;; `m5_fx_run_body'.
  `(heap-read-u64 (+ base (* ,k 8)) 0))

;; A lowered node is (STMTS . VALUE): STMTS a list of (FALLIBLE . FORM) run
;; in order, VALUE a pure expression (a variable, an immediate or a
;; code-vector read) valid right after STMTS.  FALLIBLE forms may leave a
;; non-zero ST; everything after them runs under `(if (= st 0) ...)'.

(defun nelisp-cc-fxir-native--pure-p (v)
  "Non-nil when V needs no copy: an immediate or a code-vector read."
  (or (integerp v) (eq (car-safe v) 'heap-read-u64)))

(defun nelisp-cc-fxir-native--hold (low)
  "LOW with its value copied to a fresh temporary unless pure."
  (if (nelisp-cc-fxir-native--pure-p (cdr low)) low
    (let ((v (nelisp-cc-fxir-native--temp)))
      (cons (append (car low) (list (cons nil `(setq ,v ,(cdr low))))) v))))

(defun nelisp-cc-fxir-native--block (stmts)
  "One dialect form running STMTS, each group after a fallible form guarded."
  (let ((groups nil) (cur nil))
    ;; Split after every fallible statement.
    (dolist (s stmts)
      (push (cdr s) cur)
      (when (car s) (push (nreverse cur) groups) (setq cur nil)))
    (when cur (push (nreverse cur) groups))
    (setq groups (nreverse groups))
    (let ((out nil) (first t))
      (dolist (g groups)
        (let ((form (if (cdr g) (cons 'seq g) (car g))))
          (push (if first form `(if (= st 0) ,form 0)) out))
        (setq first nil))
      (cond ((null out) 0)
            ((null (cdr out)) (car out))
            (t (cons 'seq (nreverse out)))))))

(defun nelisp-cc-fxir-native--fallible-p (stmts)
  (cl-some #'car stmts))

(defun nelisp-cc-fxir-native--truthy (v)
  "Dialect test (1/0) that word V is not nil (a legacy view goes out of line)."
  `(if (= ,v 3) 0 (if (/= (logand ,v 7) 0) 1 (nc_truthy_slow ,v))))

(defun nelisp-cc-fxir-native--frame-forms (argc)
  (setq nelisp-cc-fxir-native--maxargc (max nelisp-cc-fxir-native--maxargc argc))
  nil)

;; The activation's rooted window, reserved on first use (`--ensure'):
;; mem[0] the result word, mem[1..2] the call-out's definition words,
;; mem[3..] staged arguments, then the parameter words of a list call.
(defconst nelisp-cc-fxir-native--ensure
  '(if (= mem 0) (setq mem (nl_root_frame env fsize)) 0)
  "Reserve the window; `nc_call*' answer -4 (root exhausted) while MEM is 0.")

;; nativecomp N2: a function that calls at all reserves its window and view
;; slot once at entry (`--eager') instead of testing both before every call:
;; per call site that is two branches less, and the AOT compiler's time grows
;; faster than linearly with the size of the function it compiles.
(defvar nelisp-cc-fxir-native--eager (not (getenv "NC2_LAZY"))
  "Non-nil reserves the window at entry when the function uses it.")
(defvar nelisp-cc-fxir-native--uses-window nil
  "Non-nil once the function being generated has used its window.")

(defun nelisp-cc-fxir-native--win ()
  "Statements making the window and view slot available before a call."
  (setq nelisp-cc-fxir-native--uses-window t)
  (unless nelisp-cc-fxir-native--eager
    (list (cons nil nelisp-cc-fxir-native--ensure)
          (cons nil '(if (= oslot 0) (if (= mem 0) 0 (setq oslot (nl_root_reserve env))) 0)))))

(defun nelisp-cc-fxir-native--memchk (form)
  "FORM, or -4 while no window could be reserved (lazy mode only)."
  (setq nelisp-cc-fxir-native--uses-window t)
  (if nelisp-cc-fxir-native--eager form `(if (= mem 0) -4 ,form)))

(defun nelisp-cc-fxir-native--call-stmts (site vals argc)
  "Statements calling SITE generically on VALS (pure expressions).
Answers (STMTS . RESULT-TEMP): the VM's CALL, through `nc_callN'."
  (nelisp-cc-fxir-native--frame-forms argc)
  (let ((r (nelisp-cc-fxir-native--temp)))
    (cons (append
           (nelisp-cc-fxir-native--win)
           (list
                (cons t (if (<= argc 3)
                            `(setq st (,(intern (format "nc_call%d" argc)) env ,site mem oslot ,@vals))
                          `(seq ,@(let ((i 0) (f nil))
                                    (dolist (v vals) (push `(ptr-write-u64 (+ mem (* ,(+ 3 i) 8)) 0 ,v) f)
                                            (setq i (1+ i)))
                                    (nreverse f))
                                (setq st (nc_call env ,site (+ mem 24) ,argc mem oslot)))))
                (cons nil `(setq ,r (heap-read-u64 mem 0)))))
          r)))

(defun nelisp-cc-fxir-native--args (nodes)
  "Lower argument NODES left to right; answers (STMTS . VALUES).
A value is copied to a temporary only when a later argument runs statements
(it may assign a slot or call)."
  (let* ((lows (mapcar #'nelisp-cc-fxir-native--lower nodes))
         (stmts nil) (vals nil) (rest lows))
    (while rest
      (let ((low (car rest)))
        (when (cl-some #'car (cdr rest))
          (setq low (nelisp-cc-fxir-native--hold low)))
        (setq stmts (append stmts (car low)))
        (push (cdr low) vals))
      (setq rest (cdr rest)))
    (cons stmts (nreverse vals))))

(defun nelisp-cc-fxir-native--inline-fast (name vals)
  "Dialect expression for the fast path of primitive NAME on VALS, answering
the result word, or 0 to take the generic call; nil when NAME is not inlined.
Every fast path is the one the runtime's own word helpers take for the
direct classes (cons 010, object 110, immediates)."
  (let ((a (nth 0 vals)) (b (nth 1 vals)) (c (nth 2 vals)) (n (length vals)))
    (pcase (cons name n)
      ;; Bcar / Bcdr on a direct cons; nil gives nil.
      (`(car . 1) `(if (= (logand ,a 7) 2) (heap-read-u64 (- ,a 2) 0) (if (= ,a 3) 3 0)))
      (`(cdr . 1) `(if (= (logand ,a 7) 2) (heap-read-u64 (- ,a 2) 8) (if (= ,a 3) 3 0)))
      (`(car-safe . 1) `(if (= (logand ,a 7) 2) (heap-read-u64 (- ,a 2) 0) (if (= (logand ,a 1) 1) 3 0)))
      (`(cdr-safe . 1) `(if (= (logand ,a 7) 2) (heap-read-u64 (- ,a 2) 8) (if (= (logand ,a 1) 1) 3 0)))
      ;; Bconsp ... Bintegerp (`wf_bytecode_predicate'): an immediate, a direct
      ;; cons (010) and a string (100) are decided by their class; an object
      ;; (110) by its header kind out of line (`nc_pred6'); a legacy view goes
      ;; out.  nc3: the string and object classes are decided here too.
      (`(consp . 1) `(if (= (logand ,a 7) 2) 7 (if (= (logand ,a 1) 1) 3
                                                (if (= (logand ,a 7) 4) 3
                                                  (if (= (logand ,a 7) 6) (nc_pred6 58 ,a) 0)))))
      ;; Bnot: nil is the word 3 (or a legacy view); every other direct word is not nil.
      ((or `(null . 1) `(not . 1)) `(if (= ,a 3) 7 (if (/= (logand ,a 7) 0) 3 0)))
      ;; Beq: identical words are eq; two different immediates are not; any
      ;; other pair is the VM's word `eq' (`wf_bytecode_w_eqw', what Beq and the
      ;; eq primitive run), out of line.  nc3: no generic call for eq.
      (`(eq . 2) `(if (= ,a ,b) 7 (if (= (logand (logand ,a ,b) 1) 1) 3
                                     (if (= (wf_bytecode_w_eqw ,a ,b) 1) 7 3))))
      (`(integerp . 1) `(if (= (logand ,a 3) 1) 7 (if (= (logand ,a 1) 1) 3
                                                    (if (= (logand ,a 7) 6) (nc_pred6 168 ,a)
                                                      (if (= (logand ,a 7) 0) 0 3)))))
      (`(numberp . 1) `(if (= (logand ,a 3) 1) 7 (if (= (logand ,a 1) 1) 3
                                                   (if (= (logand ,a 7) 6) (nc_pred6 167 ,a)
                                                     (if (= (logand ,a 7) 0) 0 3)))))
      (`(symbolp . 1) `(if (= (logand ,a 1) 1) (if (= (logand ,a 3) 1) 3 7)
                         (if (= (logand ,a 7) 6) (nc_pred6 57 ,a)
                           (if (= (logand ,a 7) 0) 0 3))))
      (`(stringp . 1) `(if (= (logand ,a 7) 4) 7
                         (if (= (logand ,a 7) 6) (nc_pred6 59 ,a)
                           (if (= (logand ,a 7) 0) 0 3))))
      (`(listp . 1) `(if (= ,a 3) 7 (if (= (logand ,a 7) 2) 7
                                     (if (= (logand ,a 7) 6) (nc_pred6 60 ,a)
                                       (if (= (logand ,a 7) 0) 0 3)))))
      ;; Bleq / Bgeq on two fixnums (GNU arithcompare's fixnum case).
      (`(<= . 2) `(if (if (= (logand ,a 3) 1) (= (logand ,b 3) 1) 0) (if (<= ,a ,b) 7 3) 0))
      (`(>= . 2) `(if (if (= (logand ,a 3) 1) (= (logand ,b 3) 1) 0) (if (>= ,a ,b) 7 3) 0))
      ;; Bmult / Brem on fixnums: out of line (overflow and zero divisor go
      ;; to the generic call, which signals as GNU does).
      (`(* . 2) `(if (if (= (logand ,a 3) 1) (= (logand ,b 3) 1) 0) (nc_mul_fast ,a ,b) 0))
      (`(% . 2) `(if (if (= (logand ,a 3) 1) (if (= (logand ,b 3) 1) (/= ,b 1) 0) 0)
                     (nc_rem_fast ,a ,b) 0))
      ;; Bequal: identical words, else `wf_eqd' (the public equal's own
      ;; decision; 2 -- records, cycles, deep nesting -- takes the generic call).
      (`(equal . 2) `(if (= ,a ,b) 7 (nc_equal_fast ,a ,b)))
      ;; Bmemq / Bassq / Bnth / Bnthcdr / Blength: list walks out of line.
      (`(memq . 2) `(nc_memq_fast ,a ,b))
      (`(assq . 2) `(nc_assq_fast ,a ,b))
      (`(nthcdr . 2) `(nc_nthcdr_fast ,a ,b 1))
      (`(nth . 2) `(nc_nthcdr_fast ,a ,b 0))
      (`(length . 1) `(nc_length_fast ,a))
      ;; Bmax / Bmin on two fixnums: tagged fixnum words order as their values.
      (`(max . 2) `(if (and (= (logand ,a 3) 1) (= (logand ,b 3) 1)) (if (< ,a ,b) ,b ,a) 0))
      (`(min . 2) `(if (and (= (logand ,a 3) 1) (= (logand ,b 3) 1)) (if (> ,a ,b) ,b ,a) 0))
      ;; Baref on a record or a plain vector object with an in-range fixnum
      ;; index (`wf_bytecode_vector_primitive'): record index 0 is the type
      ;; word (out of line), K > 0 is slot K-1; a 7-slot vector may be the
      ;; char-table bridge (out of line).
      (`(aref . 2) `(nc_aref_fast ,a ,b))
      (`(aset . 3) `(nc_aset_fast ,a ,b ,c))
      (_ nil))))

;; nativecomp N2: guarded direct calls (DESIGN 4.3, kind 3).  A call whose
;; callee is itself a harvested row J, with an argument count the row's arity
;; takes, calls the wrapper nc_d_J: when the callee's live function cell is
;; the closure the call site's cache recorded and that closure's record runs
;; nc_J, nc_J is entered directly (backtrace record, recursion budget and the
;; status protocol as `m5_fx_call_words'); anything else -- fset, advice,
;; autoload, a record switched back to the VM, a cold cache -- takes the
;; generic call, which also fills the cache.
(defvar nelisp-cc-fxir-native--row-index nil
  "Hash table NAME -> row of the rows being generated (no mutant row).")
(defvar nelisp-cc-fxir-native--direct-ids nil
  "Ids J whose wrapper nc_d_J some generated call uses.")

(defvar nelisp-cc-fxir-native--wrapper-names nil
  "The nc_d_ID wrappers the last generated source defines.")

(defun nelisp-cc-fxir-native--direct-row (name argc)
  "The row NAME's direct call with ARGC arguments may enter, or nil."
  (let ((row (and nelisp-cc-fxir-native--row-index
                  (gethash name nelisp-cc-fxir-native--row-index))))
    (when row
      (let* ((arity (nth 5 row)) (amin (nth 0 arity)) (amax (nth 1 arity)))
        (and (>= argc amin) (or (< amax 0) (<= argc amax)) (<= argc 255) row)))))

(defun nelisp-cc-fxir-native--direct-stmts (site vals argc id)
  "Statements calling row ID's wrapper on VALS staged in the window.
Answers (STMTS . RESULT-TEMP)."
  (nelisp-cc-fxir-native--frame-forms argc)
  (cl-pushnew id nelisp-cc-fxir-native--direct-ids)
  (let ((r (nelisp-cc-fxir-native--temp)))
    (cons (append
           (nelisp-cc-fxir-native--win)
           (list (cons t `(setq st ,(nelisp-cc-fxir-native--memchk
                                     `(seq ,@(let ((i 0) (f nil))
                                               (dolist (v vals) (push `(ptr-write-u64 (+ mem (* ,(+ 3 i) 8)) 0 ,v) f)
                                                       (setq i (1+ i)))
                                               (nreverse f))
                                           (,(intern (format "nc_d_%d" id)) env ,site (+ mem 24) ,argc mem oslot)))))
                 (cons nil `(setq ,r (heap-read-u64 mem 0)))))
          r)))

(defun nelisp-cc-fxir-native--wrapper (row)
  "The `defun' of nc_d_ID, the guarded direct entry of ROW's nc_ID."
  (let* ((id (nth 1 row)) (rest (nth 2 (nth 5 row)))
         ;; The record's NLOCALS word while it runs nc_ID: the fixnum -(16+ID).
         (nlw (1+ (* 4 (- -16 id))))
         (restbits (if (>= rest 0) (* 65536 (1+ rest)) 0)))
    `(defun ,(intern (format "nc_d_%d" id)) (env site stk argc resw oslot)
       (let* ((rec (nc_site_rec env site)))
         (if (if (= rec 0) 0 (= (nl_vector_slot_word rec 1) ,nlw))
             ;; `m5_fx_callout' then `m5_fx_call_words' minus the lookups the
             ;; guard replaced: the definition kept in the caller's XW word, the
             ;; backtrace record, the recursion budget (`m5_fx_run').
             (let* ((frame (seq (ptr-write-u64 (+ resw 8) 0 (nl_vector_slot_word site 3))
                                (nl_n4_push_words env (nl_vector_slot_word site 0) stk argc)))
                    (depth (heap-read-u64 (+ env 96) 0))
                    (limit (nl_eval_depth_limit env (+ depth 2)))
                    (st (if (>= (+ depth 2) limit)
                            (nelisp_eval_call_stash_excessive_lisp_nesting env limit)
                          (seq (ptr-write-u64 (+ env 96) 0 (+ depth 2))
                               (m5_fx_run_done
                                (,(intern (format "nc_%d" id)) (nl_vector_slot_word rec 0) env 0 0
                                 (+ argc ,restbits) stk resw)
                                env depth)))))
               ;; -3: the entry check declined before any effect; the generic
               ;; call runs it (and its interpreter fallback) as the VM would.
               (if (= st -3)
                   (seq (nl_n4_finish_w 0 env 0 frame) (nc_call env site stk argc resw oslot))
                 (m5_fx_call_words_done st env oslot resw frame 0)))
           (nc_call env site stk argc resw oslot))))))

(defun nelisp-cc-fxir-native--call (n)
  "Lower (:call K ARGC ARGS...)."
  (let* ((k (nth 1 n)) (argc (nth 2 n))
         (name (pop nelisp-cc-fxir-native--callees))
         (args (nelisp-cc-fxir-native--args (nthcdr 3 n)))
         (vals (cdr args))
         (fast (nelisp-cc-fxir-native--inline-fast name vals))
         (gkind (and (null fast)
                     (cond ((and (eq name 'set) (= argc 2)) 22)
                           ((and (eq name 'symbol-value) (= argc 1)) 21))))
         (drow (and (null fast) (null gkind) (nelisp-cc-fxir-native--direct-row name argc))))
    (cond
     ;; nc3: Bset / Bsymbol_value (GNU opcodes, not redefinable) run the VM's
     ;; globals arm (`m5_fx_globals_run' 22 / 21) on the staged words; a case it
     ;; declines (a non-symbol: the error) takes the generic call.
     (gkind
      (nelisp-cc-fxir-native--frame-forms argc)
      (let ((r (nelisp-cc-fxir-native--temp)))
        (cons (append (car args)
                      (nelisp-cc-fxir-native--win)
                      (list (cons t `(setq st ,(nelisp-cc-fxir-native--memchk
                                                `(nc_globals env ,(nelisp-cc-fxir-native--cv k) mem oslot ,gkind
                                                             ,(nth 0 vals) ,(or (nth 1 vals) 3)))))
                            (cons nil `(setq ,r (heap-read-u64 mem 0)))))
              r)))
     ((null fast)
        (let ((call (if drow
                        (nelisp-cc-fxir-native--direct-stmts (nelisp-cc-fxir-native--cv k) vals argc (nth 1 drow))
                      (nelisp-cc-fxir-native--call-stmts (nelisp-cc-fxir-native--cv k) vals argc))))
          (cons (append (car args) (car call)) (cdr call))))
     ;; nc3: a fast path that decides every case (Beq) needs no generic call.
     ((eq name 'eq)
      (let ((r (nelisp-cc-fxir-native--temp)))
        (cons (append (car args) (list (cons nil `(setq ,r ,fast)))) r)))
     (t
      ;; Fast path inline; the generic call (the VM's own route) out of it.
      (let* ((r (nelisp-cc-fxir-native--temp))
             (call (nelisp-cc-fxir-native--call-stmts (nelisp-cc-fxir-native--cv k) vals argc)))
        (cons (append (car args)
                      (list (cons nil `(setq ,r ,fast))
                            (cons t `(if (= ,r 0)
                                         ,(nelisp-cc-fxir-native--block
                                           (append (car call) (list (cons nil `(setq ,r ,(cdr call))))))
                                       0))))
              r))))))

(defun nelisp-cc-fxir-native--inline-op (n)
  "Lower (:inline OPCODE K ARGS...): the VM's fixnum ops 4 + 5 - 6 < 13 = 14 > 7 1+ 15 1-."
  (let* ((op (nth 1 n)) (k (nth 2 n))
         (args (nelisp-cc-fxir-native--args (nthcdr 3 n)))
         (vals (cdr args)) (a (nth 0 vals)) (b (or (nth 1 vals) 3))
         (op (if (and nelisp-cc-fxir-native--mutant (= op 6)) 14 op))
         (r (nelisp-cc-fxir-native--temp))
         (both `(if (= (logand ,a 3) 1) (= (logand ,b 3) 1) 0))
         (fast (pcase op
                 ;; Tagged fixnum words order as their values; 1+/1- stay in
                 ;; range below the largest / above the smallest fixnum word.
                 ((or 4 5) `(if ,both
                                (seq (setq ,r (,(if (= op 4) '+ '-) (sar ,a 2) (sar ,b 2)))
                                     (if (= (sar (shl ,r 2) 2) ,r) (setq ,r (+ (shl ,r 2) 1))
                                       (setq ,r -1)))
                              (setq ,r -1)))
                 (6 `(setq ,r (if ,both (if (< ,a ,b) 7 3) -1)))
                 (13 `(setq ,r (if ,both (if (= ,a ,b) 7 3) -1)))
                 (14 `(setq ,r (if ,both (if (> ,a ,b) 7 3) -1)))
                 (7 `(setq ,r (if (if (= (logand ,a 3) 1) (< ,a 9223372036854775805) 0) (+ ,a 4) -1)))
                 (15 `(setq ,r (if (if (= (logand ,a 3) 1) (> ,a -9223372036854775807) 0) (- ,a 4) -1))))))
    (nelisp-cc-fxir-native--frame-forms (length vals))
    (cons (append (car args)
                  (list (cons nil fast)
                        (cons t `(if (= ,r -1)
                                     (seq ,@(mapcar #'cdr (nelisp-cc-fxir-native--win))
                                          (setq st (nc_arith env ,(nelisp-cc-fxir-native--cv k) mem oslot
                                                             ,(+ op (* 256 (length vals))) ,a ,b))
                                          (if (= st 0) (setq ,r (heap-read-u64 mem 0)) 0))
                                   0))))
          r)))

(defun nelisp-cc-fxir-native--if (c a b)
  "Lower an `if' on node C (or a pre-lowered C) with arms A and B (nodes)."
  (let* ((lc (if (eq (car-safe c) :lowered) (cdr c) (nelisp-cc-fxir-native--lower c)))
         (la (nelisp-cc-fxir-native--lower a))
         (lb (nelisp-cc-fxir-native--lower b))
         (r (nelisp-cc-fxir-native--temp))
         (arm (lambda (l) (nelisp-cc-fxir-native--block
                           (append (car l) (list (cons nil `(setq ,r ,(cdr l)))))))))
    (cons (append (car lc)
                  (list (cons (or (nelisp-cc-fxir-native--fallible-p (car la))
                                  (nelisp-cc-fxir-native--fallible-p (car lb)))
                              `(if ,(nelisp-cc-fxir-native--truthy (cdr lc))
                                   ,(funcall arm la) ,(funcall arm lb)))))
          r)))

(defun nelisp-cc-fxir-native--lower (n)
  "Lower fxir node N to (STMTS . VALUE)."
  (pcase (car n)
    (:const (cons nil (nelisp-cc-fxir-native--cv (nth 1 n))))
    (:slot (cons nil (nelisp-cc-fxir-native--local (nth 1 n))))
    (:varref
     (let ((r (nelisp-cc-fxir-native--temp)))
       (nelisp-cc-fxir-native--frame-forms 0)
       (cons (append (butlast (nelisp-cc-fxir-native--win))
                     (list (cons t `(setq st ,(nelisp-cc-fxir-native--memchk
                                               `(wf_bytecode_w_varref env ,(nelisp-cc-fxir-native--cv (nth 1 n)) mem 0))))
                           (cons nil `(setq ,r (heap-read-u64 mem 0)))))
             r)))
    (:alias
     (let ((r (nelisp-cc-fxir-native--temp)))
       (nelisp-cc-fxir-native--frame-forms 0)
       (cons (append (butlast (nelisp-cc-fxir-native--win))
                     (list (cons t `(setq st ,(nelisp-cc-fxir-native--memchk
                                               `(m5_fx_alias_ref ,(nelisp-cc-fxir-native--cv (nth 1 n)) env mem 0))))
                           (cons nil `(setq ,r (heap-read-u64 mem 0)))))
             r)))
    (:setslot
     (let ((l (nelisp-cc-fxir-native--lower (nth 2 n))) (v (nelisp-cc-fxir-native--local (nth 1 n))))
       (cons (append (car l) (list (cons nil `(setq ,v ,(cdr l))))) v)))
    (:seq
     (let ((stmts nil) (val 3) (rest (cdr n)))
       (while rest
         (let ((l (nelisp-cc-fxir-native--lower (car rest))))
           (setq stmts (append stmts (car l)) val (cdr l)))
         (setq rest (cdr rest)))
       (cons stmts val)))
    (:prog1
     (let* ((first (nelisp-cc-fxir-native--hold (nelisp-cc-fxir-native--lower (nth 1 n))))
            (stmts (car first)))
       (dolist (x (nthcdr 2 n))
         (setq stmts (append stmts (car (nelisp-cc-fxir-native--lower x)))))
       (cons stmts (cdr first))))
    (:if (nelisp-cc-fxir-native--if (nth 1 n) (nth 2 n) (nth 3 n)))
    (:and
     (cond ((null (cdr n)) (cons nil 7))
           ((null (cddr n)) (nelisp-cc-fxir-native--lower (nth 1 n)))
           (t (nelisp-cc-fxir-native--if (nth 1 n) (cons :and (cddr n)) '(:nil)))))
    (:or
     (cond ((null (cdr n)) (cons nil 3))
           ((null (cddr n)) (nelisp-cc-fxir-native--lower (nth 1 n)))
           (t (let* ((l (nelisp-cc-fxir-native--hold (nelisp-cc-fxir-native--lower (nth 1 n))))
                     (v (cdr l)))
                (nelisp-cc-fxir-native--if (cons :lowered l) (list :value v) (cons :or (cddr n)))))))
    (:nil (cons nil 3))
    (:value (cons nil (nth 1 n)))
    (:while
     (let* ((lc (nelisp-cc-fxir-native--lower (nth 1 n)))
            (lb (nelisp-cc-fxir-native--lower (nth 2 n)))
            ;; Nothing of the test runs once the body has failed.
            (cond-form `(if (= st 0)
                            ,(nelisp-cc-fxir-native--block
                              (append (car lc)
                                      (list (cons nil `(if (= st 0) ,(nelisp-cc-fxir-native--truthy (cdr lc)) 0)))))
                          0))
            ;; GNU's Bgoto back-edge safepoint, as the VM's backward jump.
            (body (nelisp-cc-fxir-native--block
                   (append (car lb)
                           (list (cons nil '(if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0)
                                                (wf_bytecode_poll) 0)))))))
       (cons (list (cons (or (nelisp-cc-fxir-native--fallible-p (car lc))
                             (nelisp-cc-fxir-native--fallible-p (car lb)))
                         `(while ,cond-form ,body)))
             3)))
    (:let
     (let ((stmts nil))
       (dolist (it (nth 1 n))
         (let ((l (nelisp-cc-fxir-native--lower (nth 2 it))))
           (setq stmts (append stmts (car l)))
           (if (eq (car it) :store)
               (setq stmts (append stmts (list (cons nil `(setq ,(nelisp-cc-fxir-native--local (nth 1 it)) ,(cdr l))))))
             ;; VARBIND: GNU's Bvarbind through the byte-code VM's binder.
             (setq stmts (append stmts
                                 (list (cons t `(setq st (wf_bytecode_varbind env ,(nelisp-cc-fxir-native--cv (nth 1 it)) ,(cdr l))))
                                       (cons nil '(setq nb (+ nb 1)))))))))
       (let ((body (nelisp-cc-fxir-native--lower (nth 2 n))))
         (if (= (nth 3 n) 0)
             (cons (append stmts (car body)) (cdr body))
           (let ((held (nelisp-cc-fxir-native--hold body)))
             (cons (append stmts (car held)
                           (list (cons nil `(seq (wf_bytecode_unbind (+ env 32) ,(nth 3 n))
                                                 (setq nb (- nb ,(nth 3 n)))))))
                   (cdr held)))))))
    (:call (nelisp-cc-fxir-native--call n))
    (:inline (nelisp-cc-fxir-native--inline-op n))
    (_ (error "fxir-native: bad node %S" n))))

(defun nelisp-cc-fxir-native--defun (row)
  "The `defun' form of nc_ID for ROW (NAME ID NLOCALS CALLEES IR (MIN MAX REST))."
  (let* ((id (nth 1 row)) (nlocals (max 0 (nth 2 row)))
         (arity (nth 5 row)) (amin (nth 0 arity)) (amax (nth 1 arity)) (rest (nth 2 arity))
         ;; Fixed parameters: the locals before &rest, else MAX of them.
         (nfixed (if (>= rest 0) rest amax))
         (nelisp-cc-fxir-native--temps nil)
         (nelisp-cc-fxir-native--callees (copy-sequence (nth 3 row)))
         (nelisp-cc-fxir-native--maxargc 0)
         (nelisp-cc-fxir-native--uses-window nil)
         (body (nelisp-cc-fxir-native--lower (nth 1 (nth 4 row))))
         (poff (+ 3 nelisp-cc-fxir-native--maxargc))
         (fsize (+ poff nfixed))
         (locals (let ((l nil)) (dotimes (i nlocals) (push (list (nelisp-cc-fxir-native--local i) 3) l)) (nreverse l)))
         (temps (mapcar (lambda (v) (list v 3)) (reverse nelisp-cc-fxir-native--temps)))
         (entry
          (append
           ;; Eager window (`--eager'): reserved before anything else runs.
           (when (and nelisp-cc-fxir-native--eager nelisp-cc-fxir-native--uses-window)
             (list (cons t '(seq (setq mem (nl_root_frame env fsize))
                                 (if (= mem 0) (setq st -4) (setq oslot (nl_root_reserve env)))))))
           ;; &rest: the remaining arguments as a fresh list (`m5_fx_run_body').
           (when (>= rest 0)
             (list (cons nil `(if (> n ,rest)
                                  (setq ,(nelisp-cc-fxir-native--local rest)
                                        (if (/= argw 0) (m5_fx_slot_list (+ argw (* ,rest 8)) (- n ,rest))
                                          (m5_fx_list_copy (nc_nthcdr args ,rest) 0)))
                                0))))
           ;; A list of arguments (the apply route): its words into the window.
           (when (> nfixed 0)
             (list (cons t `(if (= argw 0)
                                (seq ,nelisp-cc-fxir-native--ensure
                                     (if (= mem 0) (setq st -4)
                                       (setq argw (nc_list_words args (+ mem (* ,poff 8)) (if (< n ,nfixed) n ,nfixed)))))
                              0))))
           (let ((f nil))
             (dotimes (i nfixed)
               (let ((set `(setq ,(nelisp-cc-fxir-native--local i) (heap-read-u64 (+ argw (* ,i 8)) 0))))
                 ;; Required parameters always arrive (the caller checked the arity).
                 (push (cons nil (if (< i amin) set `(if (> n ,i) ,set 0))) f)))
             (nreverse f))))
         (form
          `(defun ,(intern (format "nc_%d" id)) (code env out args nparams argw wout)
             ;; ENTRY-CHECK first: a decline (-3) happens before any effect.
             (if (/= (m5_fx_entry_check (heap-read-u64 (+ (+ (nl_word_payload code) 8) 8) 0) env) 0) -3
               (let* ((base (+ (nl_word_payload code) 8))
                      (mark (nl_root_mark env))
                      (n (logand nparams 65535))
                      (mem 0) (oslot 0) (st 0) (nb 0)
                      ,@locals ,@temps)
                 (seq
                  ,(nelisp-cc-fxir-native--block
                    (append entry (car body)
                            (list (cons nil `(if (= wout 0) (nl_abi_word_to_rc ,(cdr body) out)
                                               (ptr-write-u64 wout 0 ,(cdr body)))))))
                  ;; One exit: bindings still held (a call failed) are undone.
                  (if (> nb 0) (wf_bytecode_unbind (+ env 32) nb) 0)
                  (nl_root_release env mark)
                  (if (= st 0) -1 (if (= st -4) (wf_bytecode_root_exhausted) st))))))))
    (ignore amax)
    (cl-subst fsize 'fsize form)))

(defun nelisp-cc-fxir-native--dispatch (ids lo hi)
  "Balanced dispatch tree over IDS (sorted list) index range LO..HI."
  (if (= lo hi)
      `(,(intern (format "nc_%d" (nth lo ids))) code env out args nparams argw wout)
    (let ((mid (/ (+ lo hi 1) 2)))
      `(if (< id ,(nth mid ids))
           ,(nelisp-cc-fxir-native--dispatch ids lo (1- mid))
         ,(nelisp-cc-fxir-native--dispatch ids mid hi)))))

(defconst nelisp-cc-fxir-native--shared-source
  '((defun nc_truthy_slow (w) (if (= (nl_val_tag w) 0) 0 1))
    ;; The generic call of the call-site cache (or symbol) SITE on the ARGC
    ;; words at STK, result word in RESW: the VM's CALL (`m5_fx_run_body'
    ;; op 16) -- a primitive Subr directly, else `m5_fx_callout'.  OSLOT is
    ;; the activation's reserved Sexp view slot.
    ;; nativecomp N2: a native subr with a directcall id is called on the
    ;; staged words (`nl_directcall_invoke', the byte-code VM's Bcall route,
    ;; GNU funcall_subr), not through the call-out's argument list.
    ;; nativecomp N2b (nc3): a builtin in the cell (a Subr object or the legacy
    ;; (builtin NAME) list) never takes the call-out (`nc_subr_call').
    (defun nc_call (env site stk argc resw oslot)
      (if (= (m5_fx_call_subr env site stk argc argc resw) 0) 0
        (let* ((d (nc_subr_call env site stk argc resw oslot)))
          (if (/= d -5) d
            (m5_fx_callout env site stk argc argc resw (+ resw 8) oslot 0)))))
    ;; The call site's symbol, from its cache [SYMBOL ...] or the site itself;
    ;; 0 unless it is an interned symbol object (whose function cell is +24).
    (defun nc_site_sym (site)
      (let* ((s (if (= (logand site 7) 6)
                    (if (< site 4096) 0
                      (if (= (logand (ptr-read-u64 (- site 6) 0) 63) 8) (ptr-read-u64 (- site 6) 8) site))
                  0)))
        (if (= (logand s 7) 6)
            (if (< s 4096) 0 (if (= (logand (ptr-read-u64 (- s 6) 0) 63) 4) s 0))
          0)))
    ;; nativecomp N2: the live function cell holds a Subr object:
    ;; `nl_directcall_invoke' (0 done, 1 signalled, 2 not a directcall
    ;; primitive: nothing happened).
    ;; nc3: the call of a builtin held in the live function cell, without the
    ;; call-out's lookup, autoload and closure-record work (GNU Bcall ->
    ;; funcall_subr with the SUBR in hand, eval.c:3254).  A Subr object (header
    ;; kind 20): its directcall id (`nl_directcall_invoke'), else the primitive
    ;; id of its interpreter descriptor (header bits 33-62, as
    ;; `nl_eic_subr_call_w' reads it) on the staged words, funcall from the
    ;; site cache, else the list apply.  The legacy (builtin NAME) list: the
    ;; call site's kind cache (`m5_fx_site_kind': funcall 13, symbol-value /
    ;; set 21/22, the VM's primitive ids), else the list apply
    ;; (`m5_fx_call_list').  These are the call-out's own arms for a builtin,
    ;; in its order, so values, errors and backtraces are the call-out's.
    ;; -5: not a builtin (a closure, byte code, an alias, an autoload, unbound,
    ;; a worker): nothing happened and the call-out runs.  GNU's Bcall
    ;; safepoint first, as the call-out takes it.
    (defun nc_subr_call (env site stk argc resw oslot)
      (let* ((sym (nc_site_sym site))
             (fn (if (= sym 0) 0 (ptr-read-u64 (- sym 6) 24)))
             (cls (logand fn 7)))
        (if (if (= cls 6) (if (< fn 4096) 0 (= (logand (ptr-read-u64 (- fn 6) 0) 63) 20)) 0)
            (seq (if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0)
                 (let* ((d (nl_directcall_invoke fn sym stk argc env resw)))
                   (if (/= d 2) d
                     (if (= (nl_sb_worker_p env) 1) -5
                       (nc_desc_call env site sym fn stk argc resw oslot)))))
          (if (if (= cls 2) (= (nl_sb_worker_p env) 0) 0)
              (seq (if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0)
                   (nc_builtin_call env site sym fn stk argc resw oslot))
            -5))))
    ;; A Subr object with no directcall entry for this call: its interpreter
    ;; descriptor's primitive (`nl_eic_prim_frame_w''s dispatch), funcall (13)
    ;; through the site, else the list apply of the legacy list it wraps.  A
    ;; wrong argument count goes to the call-out, which signals it.
    (defun nc_desc_call (env site sym fn stk argc resw oslot)
      (let* ((p (shr (ptr-read-u64 (- fn 6) 0) 33))
             (legacy (ptr-read-u64 (- fn 6) 8)))
        (if (= p 0) (nc_list_call env sym legacy stk argc resw oslot)
          (let* ((desc (logior (logand p 63) (shl (shr p 6) 8))))
            (if (= (nl_subr_arity_bad desc argc) 1) -5
              (let* ((id (nl_subr_prim desc argc))
                     (st (if (= id 13) (nc_funcall env site stk argc resw)
                           (if (if (= id 0) 1 (if (= id 14) 1 (= id 28))) -3
                             (if (<= id 12) (m5_fx_primitive_run id stk argc argc resw env (+ resw 8))
                               (if (= id 27) (m5_fx_primitive_run -14 stk argc argc resw env (+ resw 8))
                                 (nl_eic_prim2_run id stk argc resw)))))))
                (if (= st -3) (nc_list_call env sym legacy stk argc resw oslot) st)))))))
    ;; The legacy (builtin NAME) cell FN: the call-out's kind arms on the site's
    ;; cached kind, then the list apply; -5 when FN is no builtin list.
    (defun nc_builtin_call (env site sym fn stk argc resw oslot)
      (let* ((cache (if (= (nl_word_tag64 site) 8) site 0))
             (kind (m5_fx_site_kind cache fn argc env)))
        (if (= kind 0)
            (if (= (m5_fx_builtin_name fn 0) 0) -5
              (nc_list_call env sym fn stk argc resw oslot))
          (let* ((st (seq (ptr-write-u64 (+ resw 8) 0 fn)
                          (if (> kind 20) (m5_fx_globals_run kind stk argc argc resw env oslot)
                            (if (= kind 13) (nc_funcall env site stk argc resw)
                              (m5_fx_primitive_run kind stk argc argc resw env (+ resw 8)))))))
            (if (= st -3) (nc_list_call env sym fn stk argc resw oslot) st)))))
    ;; `funcall' with the site's callee cache (`m5_fx_callout' kind 13).
    (defun nc_funcall (env site stk argc resw)
      (m5_fx_funcall_site (if (= (nl_word_tag64 site) 8) (if (= (nl_sb_worker_p env) 1) 0 site) 0)
                          (heap-read-u64 stk 0) stk argc (- argc 1) resw env (+ resw 8)))
    ;; The ordinary apply of the builtin FN (`m5_fx_call_list'), FN kept in the
    ;; window's definition word as the call-out keeps it.
    (defun nc_list_call (env sym fn stk argc resw oslot)
      (seq (ptr-write-u64 (+ resw 8) 0 fn)
           (m5_fx_call_list sym fn stk argc argc env resw (+ resw 16) oslot)))
    ;; The guard of a direct call: the record the call site's cache holds
    ;; (slots 3 and 4, filled by `m5_fx_site_rec'), when the symbol's live
    ;; function cell is still that cached closure, the record is not retired
    ;; and neither a worker nor the dynamic-binding mode runs (the conditions
    ;; of `m5_fx_site_rec'); else 0.
    (defun nc_site_rec (env site)
      (if (if (= (logand site 7) 6)
              (if (< site 4096) 0
                (if (= (logand (ptr-read-u64 (- site 6) 0) 63) 8) (>= (nl_vector_length site) 5) 0))
            0)
          (let* ((sym (nc_site_sym site))
                 (cell (if (= sym 0) 3 (ptr-read-u64 (- sym 6) 24))))
            (if (if (= cell 3) 0 (= cell (nl_vector_slot_word site 3)))
                (let* ((rec (nl_vector_slot_word site 4)))
                  (if (if (= (logand rec 7) 6)
                          (if (< rec 4096) 0
                            (if (>= (nl_vector_length rec) 8) (= (nl_vector_slot_word rec 7) 3) 0))
                        0)
                      (if (= (nl_sb_worker_p env) 1) 0
                        (if (/= (logand (heap-read-u64 (+ env 112) 0) 2097152) 0) 0 rec))
                    0))
              0))
        0))
    ;; The same with 0-3 arguments staged here (window MEM, 0: not reserved).
    (defun nc_call0 (env site mem oslot)
      (if (= mem 0) -4 (nc_call env site (+ mem 24) 0 mem oslot)))
    (defun nc_call1 (env site mem oslot a)
      (if (= mem 0) -4
        (seq (ptr-write-u64 (+ mem 24) 0 a)
             (nc_call env site (+ mem 24) 1 mem oslot))))
    (defun nc_call2 (env site mem oslot a b)
      (if (= mem 0) -4
        (seq (ptr-write-u64 (+ mem 24) 0 a) (ptr-write-u64 (+ mem 32) 0 b)
             (nc_call env site (+ mem 24) 2 mem oslot))))
    (defun nc_call3 (env site mem oslot a b c)
      (if (= mem 0) -4
        (seq (ptr-write-u64 (+ mem 24) 0 a) (ptr-write-u64 (+ mem 32) 0 b)
             (ptr-write-u64 (+ mem 40) 0 c)
             (nc_call env site (+ mem 24) 3 mem oslot))))
    ;; A fixnum op whose fast path failed: OPN = opcode + 256 * argument
    ;; count.  The generic arm of `m5_fx_run_body' (bignum-range words),
    ;; else its call-out of the op's function SITE.
    (defun nc_arith (env site mem oslot opn a b)
      (if (= mem 0) -4
        (let* ((op (logand opn 255))
               (r (if (> opn 511) (nc_arith_slow op a b) (nc_incdec_slow op a))))
          (if (/= r -1) (seq (ptr-write-u64 mem 0 r) 0)
            (if (> opn 511) (nc_call2 env site mem oslot a b)
              (nc_call1 env site mem oslot a))))))
    ;; The words of the first N elements of the argument list ARGS into DST
    ;; (`m5_fx_run_body''s list copy; the window's other words stay nil).
    (defun nc_list_words (args dst n)
      (let* ((i 0) (a args))
        (seq (while (< i n)
               (seq (ptr-write-u64 (+ dst (* i 8)) 0 (heap-read-u64 (nl_word_payload a) 0))
                    (setq a (heap-read-u64 (nl_word_payload a) 8))
                    (setq i (+ i 1))))
             dst)))
    (defun nc_nthcdr (args n)
      (let* ((i 0) (a args))
        (seq (while (< i n)
               (seq (setq a (heap-read-u64 (nl_word_payload a) 8)) (setq i (+ i 1))))
             a)))
    ;; Baref fast path: a record or plain vector object and an in-range fixnum
    ;; index; 0 (never a WORD) leaves the case to the generic call.
    (defun nc_aref_fast (arr index)
      (if (= (logand arr 7) 6)
          (if (< arr 4096) 0
            (if (= (logand index 3) 1)
                (let* ((hdr (heap-read-u64 (- arr 6) 0)) (k (logand hdr 63)) (i (sar index 2)))
                  (if (= k 12)
                      (if (if (< i 1) 1 (> i (sar hdr 8))) 0
                        (heap-read-u64 (- arr 6) (+ 16 (* (- i 1) 8))))
                    (if (= k 8)
                        (if (= (sar hdr 8) 7) 0
                          (if (if (< i 0) 1 (>= i (sar hdr 8))) 0
                            (heap-read-u64 (- arr 6) (+ 8 (* i 8)))))
                      0)))
              0))
        0))
    ;; Baset fast path on a record slot K > 0 or a plain vector element, as
    ;; `wf_bytecode_vector_primitive' installs it; 0 for the generic call.
    (defun nc_aset_fast (arr index value)
      (if (= (logand arr 7) 6)
          (if (< arr 4096) 0
            (if (= (logand index 3) 1)
                (let* ((hdr (heap-read-u64 (- arr 6) 0)) (k (logand hdr 63)) (i (sar index 2)))
                  (if (= k 12)
                      (if (if (< i 1) 1 (> i (sar hdr 8))) 0
                        (seq (wf_dirty)
                             (ptr-write-u64 (+ (- arr 6) (+ 16 (* (- i 1) 8))) 0
                                            (if (= (logand value 3) 1) value (nl_word_canon value)))
                             value))
                    (if (= k 8)
                        (if (= (sar hdr 8) 7) 0
                          (if (if (< i 0) 1 (>= i (sar hdr 8))) 0
                            (seq (wf_dirty)
                                 (ptr-write-u64 (+ (- arr 6) (+ 8 (* i 8))) 0 value)
                                 value)))
                      0)))
              0))
        0))
    ;; nc3: out-of-line parts of GNU's opcode primitives (see
    ;; `nelisp-cc-fxir-native--inline-fast').  Each answers the result word,
    ;; or 0 (never a WORD) for the generic call, which owns every error.
    ;; The header-kind tests of `wf_bytecode_predicate' on an object word A
    ;; (class 110): OP is GNU's opcode, 57 symbolp, 58 consp, 59 stringp,
    ;; 60 listp, 167 numberp, 168 integerp.  A positioned symbol (kind 19,
    ;; flag-dependent) goes out.
    (defun nc_pred6 (op a)
      (if (< a 4096) 0
        (let* ((k (logand (ptr-read-u64 (- a 6) 0) 63)))
          (if (= op 59) (if (if (= k 5) 1 (if (= k 6) 1 (if (= k 14) 1 (= k 15)))) 7 3)
            (if (= op 168) (if (if (= k 2) 1 (= k 13)) 7 3)
              (if (= op 167) (if (if (= k 2) 1 (if (= k 3) 1 (= k 13))) 7 3)
                (if (= op 60) (if (if (= k 0) 1 (= k 7)) 7 3)
                  (if (= op 58) (if (= k 7) 7 3)
                    (if (= k 19) 0
                      (if (if (= k 4) 1 (if (= k 16) 1 (if (= k 0) 1 (= k 1)))) 7 3))))))))))
    ;; Bmult on two fixnum words: operands inside 32 bits (`nl_eic_prim_arith'
    ;; id 17), a product inside the fixnum range.
    (defun nc_mul_fast (a b)
      (let* ((x (sar a 2)) (y (sar b 2)))
        (if (if (= (sar (shl x 33) 33) x) (= (sar (shl y 33) 33) y) 0)
            (let* ((p (* x y)))
              (if (= (sar (shl p 2) 2) p) (+ (shl p 2) 1) 0))
          0)))
    ;; Brem on two fixnum words, divisor not 0: GNU's truncating remainder
    ;; (`m5_fx_primitive_run' -17).
    (defun nc_rem_fast (a b)
      (let* ((x (sar a 2)) (y (sar b 2)))
        (+ (shl (- x (* (/ x y) y)) 2) 1)))
    ;; Bequal: `wf_eqd', the public equal's decision (`bf_equal_dispatch_w');
    ;; 2 (records, char-tables, positioned symbols, cycles, deep nesting) goes
    ;; to the generic call, which runs the Lisp reference.
    (defun nc_equal_fast (a b)
      (let* ((r (wf_eqd a b 0)))
        (if (= r 1) 7 (if (= r 0) 3 0))))
    ;; Bmemq over direct conses with the VM's word `eq'; an improper or legacy
    ;; tail, or more than 65536 elements, goes to the generic call.
    (defun nc_memq_fast (x l)
      (let* ((cur l) (res 0) (n 0))
        (seq
         (while (if (= res 0) (= (logand cur 7) 2) 0)
           (let* ((e (heap-read-u64 (- cur 2) 0)))
             (if (if (= e x) 1 (if (= (logand (logand e x) 1) 1) 0 (wf_bytecode_w_eqw e x)))
                 (setq res cur)
               (seq (setq cur (heap-read-u64 (- cur 2) 8))
                    (setq n (+ n 1))
                    (if (> n 65536) (setq res 1) 0)))))
         (if (= res 1) 0 (if (/= res 0) res (if (= cur 3) 3 0))))))
    ;; Bassq: the first element that is a cons whose car is `eq' to KEY; other
    ;; elements are skipped (GNU Fassq); a legacy element or tail goes out.
    (defun nc_assq_fast (key l)
      (let* ((cur l) (res 0) (n 0))
        (seq
         (while (if (= res 0) (= (logand cur 7) 2) 0)
           (let* ((e (heap-read-u64 (- cur 2) 0)))
             (if (= (logand e 7) 0) (setq res 1)
               (seq
                (if (= (logand e 7) 2)
                    (let* ((k (heap-read-u64 (- e 2) 0)))
                      (if (if (= k key) 1 (if (= (logand (logand k key) 1) 1) 0 (wf_bytecode_w_eqw k key)))
                          (setq res e) 0))
                  0)
                (if (= res 0)
                    (seq (setq cur (heap-read-u64 (- cur 2) 8))
                         (setq n (+ n 1))
                         (if (> n 65536) (setq res 1) 0))
                  0)))))
         (if (= res 1) 0 (if (/= res 0) res (if (= cur 3) 3 0))))))
    ;; Bnthcdr (MODE 1) / Bnth (MODE 0): a fixnum count 0..65536 over direct
    ;; conses; a tail that is neither a cons nor nil before the count is used
    ;; up (the wrong-type error) goes out.
    (defun nc_nthcdr_fast (nw l mode)
      (if (= (logand nw 3) 1)
          (let* ((i (sar nw 2)) (cur l))
            (if (if (< i 0) 1 (> i 65536)) 0
              (seq
               (while (if (> i 0) (= (logand cur 7) 2) 0)
                 (seq (setq cur (heap-read-u64 (- cur 2) 8)) (setq i (- i 1))))
               (if (= cur 3) 3
                 (if (> i 0) 0
                   (if (= mode 1) cur
                     (if (= (logand cur 7) 2) (heap-read-u64 (- cur 2) 0) 0)))))))
        0))
    ;; Blength (`bf_length_checked_w'): nil, a string word's character count, a
    ;; proper list of direct conses (up to 65536; one that may be a closure --
    ;; a symbol first -- asks `nl_closure_shape_p', whose slot count is the
    ;; generic call's), a plain vector (7 slots: the char-table bridge goes out).
    (defun nc_length_fast (a)
      (if (= a 3) 1
        (if (= (logand a 7) 4)
            (let* ((n (ptr-read-u64 (- a 4) 0)))
              (if (if (>= n 0) (< n 2305843009213693952) 0) (+ (shl n 2) 1) 0))
          (if (= (logand a 7) 2)
              (if (if (= (logand (heap-read-u64 (- a 2) 0) 7) 6) (= (nl_closure_shape_p a) 1) 0) 0
                (let* ((n 0) (cur a))
                  (seq (while (if (= (logand cur 7) 2) (<= n 65536) 0)
                         (seq (setq cur (heap-read-u64 (- cur 2) 8)) (setq n (+ n 1))))
                       (if (= cur 3) (+ (shl n 2) 1) 0))))
            (if (= (logand a 7) 6)
                (if (< a 4096) 0
                  (if (= (logand (ptr-read-u64 (- a 6) 0) 63) 8)
                      (let* ((n (vector-len a))) (if (if (>= n 0) (/= n 7) 0) (+ (shl n 2) 1) 0))
                    0))
              0)))))
    ;; Bset (KIND 22) / Bsymbol_value (21): the VM's globals arm on A (and B)
    ;; staged in the window; a case it declines takes the generic call.
    (defun nc_globals (env site mem oslot kind a b)
      (if (= mem 0) -4
        (let* ((argc (if (= kind 22) 2 1)))
          (seq (ptr-write-u64 (+ mem 24) 0 a)
               (if (= kind 22) (ptr-write-u64 (+ mem 32) 0 b) 0)
               (let* ((st (m5_fx_globals_run kind (+ mem 24) argc argc mem env oslot)))
                 (if (= st -3) (nc_call env site (+ mem 24) argc mem oslot) st))))))
    (defun nc_arith_slow (op a b)
      ;; The generic arm of `m5_fx_run_body' for operands that are not both
      ;; immediate fixnums (bignum-range words, overflow): -1 asks for a call-out.
      (if (= (+ (m5_fx_int_p a) (m5_fx_int_p b)) 2)
          (if (= op 6) (if (< (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3)
            (if (= op 13) (if (= (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3)
              (if (= op 14) (if (> (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3)
                (m5_fx_fix (if (= op 4) (+ (wf_bytecode_w_int a) (wf_bytecode_w_int b))
                             (- (wf_bytecode_w_int a) (wf_bytecode_w_int b)))))))
        -1))
    (defun nc_incdec_slow (op a)
      (if (= (m5_fx_int_p a) 1)
          (m5_fx_fix (if (= op 7) (+ (wf_bytecode_w_int a) 1) (- (wf_bytecode_w_int a) 1)))
        -1)))
  "Shared out-of-line helpers (inline boundaries, so bodies stay small).")

(defconst nelisp-cc-fxir-native--fixed-boundaries
  '(nc_truthy_slow nc_call nc_call0 nc_call1 nc_call2 nc_call3 nc_arith nc_list_words nc_nthcdr
    nc_aref_fast nc_aset_fast nc_arith_slow nc_incdec_slow nc_run
    nc_subr_call nc_site_rec
    ;; nc3
    nc_desc_call nc_builtin_call nc_funcall nc_list_call nc_pred6 nc_mul_fast nc_rem_fast
    nc_equal_fast nc_memq_fast nc_assq_fast nc_nthcdr_fast nc_length_fast nc_globals)
  "Shared helpers the unit inliner must leave as calls.")

(defun nelisp-cc-fxir-native--mutant-row (rows)
  "The negative-control row: a copy of the first row using `<' with a new id."
  (let ((src (cl-find-if (lambda (r) (string-match-p "(:inline 6 " (prin1-to-string (nth 4 r)))) rows)))
    (when src
      (list (car src) (1+ (apply #'max (mapcar #'cadr rows))) (nth 2 src) (nth 3 src) (nth 4 src)
            (nth 5 src) :mutant))))

(defun nelisp-cc-fxir-native-all-rows ()
  "Harvested rows plus the negative-control row (marked :mutant)."
  (let* ((rows (nelisp-cc-fxir-native--rows))
         (m (and rows (nelisp-cc-fxir-native--mutant-row rows))))
    (if m (append rows (list m)) rows)))

(defun nelisp-cc-fxir-native-source ()
  "AOT source `(seq (defun ...) ...)' for every row."
  (let* ((rows (nelisp-cc-fxir-native-all-rows))
         (ids (sort (mapcar #'cadr rows) #'<)))
    (if (null rows)
        '(seq (defun nc_run (id code env out args nparams argw wout) -3))
      (let* ((nelisp-cc-fxir-native--row-index (make-hash-table :test 'eq))
             (nelisp-cc-fxir-native--direct-ids nil))
        (dolist (r rows)
          (unless (nth 6 r) (puthash (car r) r nelisp-cc-fxir-native--row-index)))
        (let ((defuns (mapcar (lambda (row)
                                (let ((nelisp-cc-fxir-native--mutant (eq (nth 6 row) :mutant)))
                                  (nelisp-cc-fxir-native--defun row)))
                              rows)))
          (setq nelisp-cc-fxir-native--wrapper-names
                (mapcar (lambda (id) (intern (format "nc_d_%d" id)))
                        (sort (copy-sequence nelisp-cc-fxir-native--direct-ids) #'<)))
          `(seq
            ,@nelisp-cc-fxir-native--shared-source
            ,@defuns
            ;; The direct entries the rows call (nativecomp N2).
            ,@(mapcar (lambda (id)
                        (nelisp-cc-fxir-native--wrapper
                         (cl-find-if (lambda (r) (and (= (nth 1 r) id) (not (nth 6 r)))) rows)))
                      (sort (copy-sequence nelisp-cc-fxir-native--direct-ids) #'<))
            (defun nc_run (id code env out args nparams argw wout)
              ,(nelisp-cc-fxir-native--dispatch ids 0 (1- (length ids))))))))))

(defconst nelisp-cc-fxir-native--source (nelisp-cc-fxir-native-source)
  "WORD-ABI AOT lowering of the harvested prelude closures (its own unit).")

(defconst nelisp-cc-fxir-native--boundaries
  (append nelisp-cc-fxir-native--fixed-boundaries nelisp-cc-fxir-native--wrapper-names)
  "Names the unit inliner must leave as calls: the shared helpers and the
direct-call wrappers (inlined, each would copy its callee's entry into every
caller).")

(defun nelisp-cc-fxir-native-install-table-source ()
  "Source text of `nelisp-nc--table' for the boot image: (NAME ID CALLEES IR ARITY [:mutant])."
  (format "(defconst nelisp-nc--table '%S)\n"
          (mapcar (lambda (r) (append (list (nth 0 r) (nth 1 r) (nth 3 r) (nth 4 r) (nth 5 r))
                                      (and (nth 6 r) (list (nth 6 r)))))
                  (nelisp-cc-fxir-native-all-rows))))

(provide 'nelisp-cc-fxir-native)
;;; nelisp-cc-fxir-native.el ends here
