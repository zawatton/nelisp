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
;; compall2: loads from a raw address -- the activation's window MEM, a cons
;; or vector address (WORD minus its tag), a catch node, the staged words --
;; are `ptr-read-u64'.  The heap-accessor lowering turns `heap-read-u64' at
;; offsets 0-24 of a base it cannot classify into an out-of-line
;; `nl_string_read64' call (lisp/nelisp-heap-accessors.el), whose String test
;; can never hold for an 8-aligned address: it cost a call on every inlined
;; car/cdr and after every call-out, and one relocation each in the unit's
;; metadata (3,629 of the 9,450 relocations of fxir-native.o on v35).
;;
;; The opcode primitives are inlined without a function-cell guard, as GNU's
;; byte compiler compiles `car' to Bcar: a later (fset 'car ...) is not seen
;; by compiled code (GNU-faithful; the VM honoured it).  Every other call goes
;; through the live function cell.
;;
;; Compiler lowering only: no reader builtin, no :lit arm, no data blob.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cc-fxir-rows)
(require 'nelisp-cc-fxir-effects)

(declare-function nelisp-cc-fxir-facts-analyze "nelisp-cc-fxir-facts" (row &optional code))
(declare-function nelisp-cc-fxir-facts-verify "nelisp-cc-fxir-facts" (row code candidate))
(eval-when-compile (defvar nelisp-cc-fxir-facts--origin-schema))

(declare-function nelisp-cc-fxir-analysis-row "nelisp-cc-fxir-analysis" (row &optional code))
(declare-function nelisp-cc-fxir-admission-rows "nelisp-cc-fxir-admission" (snapshot))
(declare-function nelisp-cc-fxir-inline-projection "nelisp-cc-fxir-inline" (row))
(declare-function nelisp-cc-fxir-inline-addition "nelisp-cc-fxir-inline" (row))
(declare-function nelisp-cc-fxir-inline-form "nelisp-cc-fxir-inline" (row site argc token &optional kind rewrite))
(declare-function nelisp-cc-fxir-facts-verify-add-region "nelisp-cc-fxir-facts" (row site baseline candidate))

(defvar nelisp-cc-fxir-native-inline-projections nil
  "Experimental guarded M1a parameter splicing; nil by default.
Native qualification is required before enabling this compiler option.")
(defvar nelisp-cc-fxir-native-inline-additions nil
  "Experimental guarded two-addition leaf splicing; nil by default.")
(defvar nelisp-cc-fxir-native-inline-checked-additions nil
  "Experimental verified reuse of preflight addition results; nil by default.")
(defvar nelisp-cc-fxir-native-condition-facts nil
  "Consume recomputed row-local Boolean condition facts; default nil.")
(defvar nelisp-cc-fxir-native--inline-sites nil)
(defvar nelisp-cc-fxir-native--inline-budget 0)
(defvar nelisp-cc-fxir-native--inline-count 0)

(defvar nelisp-cc-fxir-native-diagnostics nil
  "Non-nil enables compiler-only M0 row analysis; default emits identical DSL.")

(defvar nelisp-cc-fxir-native-diagnostic-function #'ignore
  "Function receiving one structured M0 row report when diagnostics is enabled.")

;; ---- per-function generator state ----
(defvar nelisp-cc-fxir-native--temps nil "Temporaries of the function being generated.")
(defvar nelisp-cc-fxir-native--callees nil "Remaining callee names, in walk order.")
(defvar nelisp-cc-fxir-native--maxargc 0 "Largest staged argument count.")
(defvar nelisp-cc-fxir-native--nbd 0
  "Dynamic bindings the code being lowered holds at its start (static count).")
(defvar nelisp-cc-fxir-native--ncatch 0
  "Catch nodes of the function being generated (three window words each).")
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

(defun nelisp-cc-fxir-native--op-fallible-p (op)
  "Non-nil when the effect table says op keyword OP itself may leave ST non-zero.
That is: it may signal, call Lisp or exit nonlocally.  An unknown OP is
fallible (the table's conservative class)."
  (let ((e (nelisp-cc-fxir-effects-of op)))
    (and (or (plist-get e :may-signal) (plist-get e :may-call-lisp)
             (plist-get e :nonlocal-exit))
         t)))

(defun nelisp-cc-fxir-native--call-fallible-p (callee)
  "Non-nil when a :call to CALLEE may leave ST non-zero (callee-refined class)."
  (let ((e (nelisp-cc-fxir-effects-call-of callee)))
    (and (or (plist-get e :may-signal) (plist-get e :may-call-lisp)
             (plist-get e :nonlocal-exit))
         t)))

(defun nelisp-cc-fxir-native--effects-consistent-p (node lowered &optional callee)
  "Test-time check that LOWERED (from NODE) agrees with the effect table.
For a :call NODE the optional CALLEE selects the callee-refined class.
Return nil when NODE's op is unknown, or is classified non-fallible yet
LOWERED carries a fallible statement that no descendant of NODE justifies
\(a descendant op, or a `:varbind' let item, that the table calls fallible).
Not used on the lowering path."
  (let ((op (car-safe node)))
    (and (assq op nelisp-cc-fxir-effects-table)
         (or (if (and (eq op :call) callee)
                 (nelisp-cc-fxir-native--call-fallible-p callee)
               (nelisp-cc-fxir-native--op-fallible-p op))
             (not (nelisp-cc-fxir-native--fallible-p (car lowered)))
             (nelisp-cc-fxir-native--descendant-fallible-p node)))))

(defun nelisp-cc-fxir-native--descendant-fallible-p (node)
  "Non-nil when some strict descendant keyword form of NODE is fallible."
  (let ((found nil))
    (cl-labels ((walk (x)
                  (when (consp x)
                    (when (and (keywordp (car x))
                               (nelisp-cc-fxir-native--op-fallible-p (car x)))
                      (setq found t))
                    (dolist (y (if (proper-list-p x) x (list (car x) (cdr x))))
                      (walk y)))))
      (dolist (y (cdr-safe node)) (walk y)))
    found))

(defvar nelisp-cc-fxir-native--condition-sites nil)
(defvar nelisp-cc-fxir-native--condition-row nil)
(defvar nelisp-cc-fxir-native--condition-syntax nil)
(defvar nelisp-cc-fxir-native--condition-hash nil)

(defun nelisp-cc-fxir-native--condition-map (body proofs syntax-hash)
  "Return unique original condition nodes authenticated by PROOFS."
  (let ((counts (make-hash-table :test 'eq))
        (conditions (make-hash-table :test 'equal))
        (active (make-hash-table :test 'eq)) (steps 0) (ok t))
    (cl-labels
        ((walk (node path depth)
           (setq steps (1+ steps))
           (when (or (> steps 10000) (> depth 48) (not (proper-list-p node))
                     (gethash node active))
             (setq ok nil))
           (when ok
             (puthash node (1+ (gethash node counts 0)) counts)
             (puthash node t active)
             (pcase (car node)
               ((or :const :nil :slot :varref :alias) nil)
               (:setslot (walk (nth 2 node) (append path '(2)) (1+ depth)))
               ((or :seq :prog1)
                (let ((i 1))
                  (dolist (child (nthcdr 1 node))
                    (walk child (append path (list i)) (1+ depth))
                    (setq i (1+ i)))))
               (:if
                (puthash (append path '(1)) (nth 1 node) conditions)
                (walk (nth 1 node) (append path '(1)) (1+ depth))
                (walk (nth 2 node) (append path '(2)) (1+ depth))
                (walk (nth 3 node) (append path '(3)) (1+ depth)))
               (:while
                (puthash (append path '(1)) (nth 1 node) conditions)
                (walk (nth 1 node) (append path '(1)) (1+ depth))
                (walk (nth 2 node) (append path '(2)) (1+ depth)))
               (:let
                (let ((i 0))
                  (dolist (binding (nth 1 node))
                    (walk (nth 2 binding) (append path (list 1 i 2)) (1+ depth))
                    (setq i (1+ i))))
                (walk (nth 2 node) (append path '(2)) (1+ depth)))
               ((or :call :inline)
                (let ((i 3))
                  (dolist (child (nthcdr 3 node))
                    (walk child (append path (list i)) (1+ depth))
                    (setq i (1+ i)))))
               (_ (setq ok nil)))
             (remhash node active))))
      (walk body '(1) 0)
      (when ok
        (let (sites)
          (dolist (entry proofs)
            (let* ((path (car entry)) (proof (cadr entry))
                   (node (gethash path conditions)))
              (when (and node (= (gethash node counts 0) 1)
                         (equal path (plist-get proof :path))
                         (equal syntax-hash (plist-get proof :syntax-sha256)))
                (push node sites))))
          (nreverse sites))))))

(defun nelisp-cc-fxir-native--facts-candidate-bounded-p (candidate)
  "Return non-nil only for a bounded acyclic plist-shaped CANDIDATE graph.
Inspect conses iteratively so malformed analyzer output cannot recurse or
loop before the producer verifier sees its private copy."
  (let ((stack (list (vector candidate 0 nil)))
        (active (make-hash-table :test 'eq))
        (nodes 0) (string-bytes 0) (ok (consp candidate)))
    (while (and stack ok)
      (let* ((frame (pop stack))
             (node (aref frame 0))
             (depth (aref frame 1))
             (exit (aref frame 2)))
        (cond
         (exit (remhash node active))
         ((consp node)
          (setq nodes (1+ nodes))
          (if (or (> nodes 100000) (> depth 512) (gethash node active))
              (setq ok nil)
            (puthash node t active)
            (push (vector node depth t) stack)
            (push (vector (cdr node) (1+ depth) nil) stack)
            (push (vector (car node) (1+ depth) nil) stack)))
         ((stringp node)
          (setq string-bytes (+ string-bytes (string-bytes node)))
          (when (> string-bytes 1048576) (setq ok nil)))
         ((or (symbolp node) (numberp node)) nil)
         (t (setq ok nil)))))
    ok))

(defun nelisp-cc-fxir-native--facts-owned-copy (value)
  "Copy previously bounded VALUE with private cons cells and strings.
The raw metadata guard must run first. Text properties have no facts
semantics, so discard them rather than retaining property-value aliases."
  (let* ((root (list nil)) (stack (list (vector value root nil))))
    (while stack
      (let* ((frame (pop stack)) (source (aref frame 0))
             (target (aref frame 1)) (tail (aref frame 2))
             (copy (cond ((consp source) (cons nil nil))
                         ((stringp source) (substring-no-properties source))
                         (t source))))
        (if tail (setcdr target copy) (setcar target copy))
        (when (consp source)
          (push (vector (cdr source) copy t) stack)
          (push (vector (car source) copy nil) stack))))
    (car root)))

(defun nelisp-cc-fxir-native--condition-snapshot (row)
  "Recompute and verify private Boolean condition evidence for ROW."
  (condition-case nil
      (progn
        (require 'nelisp-cc-fxir-facts)
        (let ((candidate (nelisp-cc-fxir-facts-analyze row nil)))
          (when (nelisp-cc-fxir-native--facts-candidate-bounded-p candidate)
            (let* ((facts (nelisp-cc-fxir-native--facts-owned-copy candidate))
                   (analysis (nelisp-cc-fxir-analysis-row row))
                   (hash (plist-get facts :analysis-hash)))
              (when (and (plist-get facts :valid)
                         (eql (plist-get facts :schema) 2)
                         (eql (plist-get facts :origin-schema)
                              nelisp-cc-fxir-facts--origin-schema)
                         (stringp hash)
                         (equal hash (plist-get analysis :syntax-sha256))
                         (nelisp-cc-fxir-facts-verify row nil facts))
                (list :syntax (nelisp-cc-fxir-native--facts-owned-copy (nth 4 row))
                      :hash (substring-no-properties (plist-get analysis :syntax-sha256))
                      :sites (nelisp-cc-fxir-native--condition-map
                              (nth 1 (nth 4 row))
                              (plist-get facts :boolean-truth-tests) hash)))))))
    (error nil)))

(defun nelisp-cc-fxir-native--truthy (v &optional original-node)
  "Dialect test (1/0) that word V is not nil (a legacy view goes out of line)."
  (if (and original-node (memq original-node nelisp-cc-fxir-native--condition-sites)
           nelisp-cc-fxir-native--condition-row
           (equal (nth 4 nelisp-cc-fxir-native--condition-row)
                  nelisp-cc-fxir-native--condition-syntax)
           (stringp nelisp-cc-fxir-native--condition-hash))
      `(/= ,v 3)
    `(if (= ,v 3) 0 (if (/= (logand ,v 7) 0) 1 (nc_truthy_slow ,v)))))

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
                (cons (nelisp-cc-fxir-native--op-fallible-p :call) (if (<= argc 3)
                            `(setq st (,(intern (format "nc_call%d" argc)) env ,site mem oslot ,@vals))
                          `(seq ,@(let ((i 0) (f nil))
                                    (dolist (v vals) (push `(ptr-write-u64 (+ mem (* ,(+ 3 i) 8)) 0 ,v) f)
                                            (setq i (1+ i)))
                                    (nreverse f))
                                (setq st (nc_call env ,site (+ mem 24) ,argc mem oslot)))))
                (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
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

(defconst nelisp-cc-fxir-native--logand-subr-hdr-id 44
  "Header id byte (bits 8-15) of the immutable Subr object for `logand':
its directcall id 27 (`nelisp-standalone--directcall-specs') plus 17, the bias
`nelisp-standalone--subr-desc' stores.  Only that primitive's Subr object
carries it.  `logior' has no directcall id, hence no identity token: it is
deliberately not inlined.")

(defun nelisp-cc-fxir-native--subr-guard (site id)
  "Dialect test: the live function cell of SITE's symbol is the Subr object
whose header id byte is ID (answers non-zero), else 0.
GNU has no opcode for `logand', so an `fset' or advice of the symbol must be
honoured.  What is compared: the symbol's live function cell word (symbol
object + 18, read afresh on every execution as `nc_site_sym' / the site-rec
guard do), which must be a Subr object (header kind 20) whose header bits 8-15
equal ID.  `fset' to a closure, a symbol, another subr or an advice object
changes the cell, so the test fails and the generic call runs.  SITE is the
call cache vector (kind 8, symbol in slot 0) or the symbol itself."
  `(let* ((lgs ,site)
          (lgsym (if (= (logand lgs 7) 6)
                     (if (< lgs 4096) 0
                       (let* ((lgh (ptr-read-u64 (- lgs 6) 0)))
                         (if (= (logand lgh 63) 8) (ptr-read-u64 (- lgs 6) 8)
                           (if (= (logand lgh 63) 4) lgs 0))))
                   0))
          (lgsy (if (= (logand lgsym 7) 6)
                    (if (< lgsym 4096) 0
                      (if (= (logand (ptr-read-u64 (- lgsym 6) 0) 63) 4) lgsym 0))
                  0))
          (lgfn (if (= lgsy 0) 0 (ptr-read-u64 (+ lgsy 18) 0))))
     (if (= (logand lgfn 7) 6)
         (if (< lgfn 4096) 0
           (if (= (logand (ptr-read-u64 (- lgfn 6) 0) 63) 20)
               (= (logand (shr (ptr-read-u64 (- lgfn 6) 0) 8) 255) ,id)
             0))
       0)))

(defun nelisp-cc-fxir-native--nary-fast (op r vals)
  "Dialect statement setting R to the tagged fixnum chain OP over VALS
\(OP is `+' or `-', at least three VALS), or to -1 for the generic call.
Same checks as the binary `:inline' arm: every word a fixnum (tag 01) and no
partial result leaving the 62-bit range.  -1 never is a fixnum word."
  (let* ((fix (let ((e `(= (logand ,(car (last vals)) 3) 1)))
                (dolist (v (cdr (reverse vals)))
                  (setq e `(if (= (logand ,v 3) 1) ,e 0)))
                e))
         (ok `(= (sar (shl ,r 2) 2) ,r))
         ;; Innermost: tag the final result.
         (acc `(setq ,r (+ (shl ,r 2) 1))))
    (dolist (v (reverse (nthcdr 2 vals)))
      (setq acc `(seq (setq ,r (,op ,r (sar ,v 2)))
                      (if ,ok ,acc (setq ,r -1)))))
    ;; ACC now applies the third..last steps; each step checks its result.
    ;; The final tagging is wrapped by the last check; the first step follows.
    `(if ,fix
         (seq (setq ,r (,op (sar ,(nth 0 vals) 2) (sar ,(nth 1 vals) 2)))
              (if ,ok ,acc (setq ,r -1)))
       (setq ,r -1))))

(defun nelisp-cc-fxir-native--inline-fast (name vals &optional site)
  "Dialect expression for the fast path of primitive NAME on VALS, answering
the result word, or 0 to take the generic call; nil when NAME is not inlined.
Every fast path is the one the runtime's own word helpers take for the
direct classes (cons 010, object 110, immediates).  SITE is the call-site word
form, needed by the guarded (redefinable) primitives."
  (let ((a (nth 0 vals)) (b (nth 1 vals)) (c (nth 2 vals)) (n (length vals)))
    (pcase (cons name n)
      ;; `logand' has no GNU opcode: a function-cell guard, then the tagged
      ;; words' logand (tags 01 & 01 = 01 stays a fixnum word).
      (`(logand . 2) `(if (if (= (logand ,a 3) 1) (= (logand ,b 3) 1) 0)
                          (if ,(nelisp-cc-fxir-native--subr-guard
                                (or site 'site) nelisp-cc-fxir-native--logand-subr-hdr-id)
                              (logand ,a ,b) 0)
                        0))
      ;; Bcar / Bcdr on a direct cons; nil gives nil.
      (`(car . 1) `(if (= (logand ,a 7) 2) (ptr-read-u64 (- ,a 2) 0) (if (= ,a 3) 3 0)))
      (`(cdr . 1) `(if (= (logand ,a 7) 2) (ptr-read-u64 (- ,a 2) 8) (if (= ,a 3) 3 0)))
      (`(car-safe . 1) `(if (= (logand ,a 7) 2) (ptr-read-u64 (- ,a 2) 0) (if (= (logand ,a 1) 1) 3 0)))
      (`(cdr-safe . 1) `(if (= (logand ,a 7) 2) (ptr-read-u64 (- ,a 2) 8) (if (= (logand ,a 1) 1) 3 0)))
      ;; Bconsp ... Bintegerp (`wf_bytecode_predicate'): an immediate, a direct
      ;; cons (010) and a string (100) are decided by their class; an object
      ;; (110) by its header kind out of line (`nc_pred6'); a legacy view goes
      ;; out.  nc3: the string and object classes are decided here too.
      (`(consp . 1) `(if (= (logand ,a 7) 2) 7 (if (= (logand ,a 1) 1) 3
                                                (if (= (logand ,a 7) 4) 3
                                                  (if (= (logand ,a 7) 6) (nc_pred6 58 ,a) 0)))))
      ;; Bnot: nil is the word 3 (or a legacy view); every other direct word is not nil.
      ((or `(null . 1) `(not . 1)) `(if (= ,a 3) 7 (if (/= (logand ,a 7) 0) 3 0)))
      ;; Beq: identical words are eq; distinct immediates or two distinct
      ;; direct conses are not. Other representations retain the VM's word
      ;; `eq' (`wf_bytecode_w_eqw'). Direct cons WORDs own pointer identity.
      (`(eq . 2) `(if (= ,a ,b) 7 (if (= (logand (logand ,a ,b) 1) 1) 3
                                     (if (if (= (logand ,a 7) 2) (= (logand ,b 7) 2) 0) 3
                                       (if (= (wf_bytecode_w_eqw ,a ,b) 1) 7 3)))))
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

(defconst nelisp-cc-fxir-native--direct-aliases
  '((string-match . nlre--string-match-entry) (string-match-p . nlre--string-match-p-entry)
    (match-beginning . nlre-match-beginning) (match-end . nlre-match-end)
    (match-string . nlre--match-string-entry))
  "Public names whose function cell the boot source sets to a row's closure
\(`fset' in `nelisp-standalone--reader-repl-prelude-source'): a call of the
public name may enter that row; the guard still compares the live cell and
the record, so another definition takes the generic call.")

(defun nelisp-cc-fxir-native--direct-row (name argc)
  "The row NAME's direct call with ARGC arguments may enter, or nil."
  (let ((row (and nelisp-cc-fxir-native--row-index
                  (gethash (or (cdr (assq name nelisp-cc-fxir-native--direct-aliases)) name)
                           nelisp-cc-fxir-native--row-index))))
    (when row
      (let* ((arity (nth 5 row)) (amin (nth 0 arity)) (amax (nth 1 arity)))
        (and (>= argc amin) (or (< amax 0) (<= argc amax)) (<= argc 255) row)))))

(defun nelisp-cc-fxir-native--direct-stmts (site vals argc id &optional expanded)
  "Statements calling row ID's wrapper on VALS staged in the window.
Answers (STMTS . RESULT-TEMP)."
  (nelisp-cc-fxir-native--frame-forms argc)
  (cl-pushnew id nelisp-cc-fxir-native--direct-ids)
  (let ((r (nelisp-cc-fxir-native--temp)))
    (cons (append
           (nelisp-cc-fxir-native--win)
           (list (cons (nelisp-cc-fxir-native--op-fallible-p :call) `(setq st ,(nelisp-cc-fxir-native--memchk
                                     `(seq ,@(let ((i 0) (f nil))
                                               (dolist (v vals) (push `(ptr-write-u64 (+ mem (* ,(+ 3 i) 8)) 0 ,v) f)
                                                       (setq i (1+ i)))
                                               (nreverse f))
                                           ,(or expanded
                                                `(,(intern (format "nc_d_%d" id)) env ,site (+ mem 24) ,argc mem oslot))))))
                 (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
          r)))

(defconst nelisp-cc-fxir-native--site-rec-form
  ;; nc4: the guard as GNU's Ffuncall / funcall_general reads it
  ;; (eval.c: the symbol's function cell, a type test, the entry): the
  ;; site is the code vector's call cache [SYMBOL ...] the fx compiler
  ;; made (the install pass checked the code), so only word classes are
  ;; tested; the cache's slots 3 and 4 are written record first
  ;; (`m5_fx_funcall_site_fill'), so a cached closure equal to the live
  ;; cell means slot 4 holds its record.  Words: vector slot K at
  ;; WORD + 2 + 8K, a symbol's function cell at WORD + 18.
  ;; nc5: the guard inline in each nc_d_ID, as GNU's funcall_general is
  ;; inline in Ffuncall: no call on the path every direct call takes.
  '(if (= (logand site 7) 6)
       (let* ((gsym (ptr-read-u64 (+ site 2) 0)))
         (if (if (= (logand gsym 7) 6) (>= gsym 4096) 0)
             (let* ((cell (ptr-read-u64 (+ gsym 18) 0)))
               (if (if (= cell 3) 0 (= cell (ptr-read-u64 (+ site 26) 0)))
                   (let* ((grec (ptr-read-u64 (+ site 34) 0)))
                     (if (if (= (logand grec 7) 6) (= (ptr-read-u64 (+ grec 58) 0) 3) 0)
                         (if (= (heap-read-u64 env 168) 0)
                             (if (= (logand (heap-read-u64 (+ env 112) 0) 2097152) 0)
                                 (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0) grec
                                   (if (= (nl_sb_worker_p env) 1) 0 grec))
                               0)
                           0)
                       0))
                 0))
           0))
     0)
  "The record the call site's cache holds (slots 3 and 4, filled by
`m5_fx_site_rec'), when the symbol's live function cell is still that cached
closure, the record is not retired and neither a worker nor the
dynamic-binding mode runs (the conditions of `m5_fx_site_rec'); else 0.")

(defconst nelisp-cc-fxir-native--push-form
  ;; nc5: record_in_backtrace in place (lisp.h:3760): `nl_n4_push_words''s
  ;; hot body inline for the call's operator (the site's symbol) and its
  ;; staged words, with the same cold call for a missing state, the depth
  ;; limit and a view operator.  The 64-byte record and every reader are
  ;; unchanged.
  ;; Raw loads (`ptr-read-u64'): the state, the record and the frames
  ;; vector's data are runtime memory, never a String WORD, so the String-aware
  ;; `heap-read-u64' (an `nl_string_read64' call at offsets 0-24) is not used.
  '(let* ((bfn (nl_vector_slot_word site 0))
          (bstate (heap-read-u64 env 176))
          (bdepth (if (= bstate 0) 32768 (ptr-read-u64 bstate 0))))
     (if (if (>= bdepth 32768) 1 (= (logand bfn 7) 0))
         (nl_n4_push_words_cold env bfn stk argc)
       (let* ((bfr (+ bstate (+ 64 (shl bdepth 6))))
              (bfd (heap-read-u64 env 40)))
         (seq
          (ptr-write-u64 bfr 0 bfn)
          (ptr-write-u64 bfr 8 3)
          (ptr-write-u64 bfr 16 (logior 257 (shl (logand (heap-read-u64 env 112) 2097152) 3)))
          (ptr-write-u64 bfr 24 (+ (* stk 65536) argc))
          (ptr-write-u64 bfr 32 (sar (ptr-read-u64 bfd 24) 2))
          (ptr-write-u64 bfr 40 (+ bfd 6))
          (ptr-write-u64 bfr 56 (nl_n4_cut_capture env))
          (ptr-write-u64 bstate 0 (+ bdepth 1))
          bfr))))
  "`nl_n4_push_words' of the direct call's record, inline.")

(defun nelisp-cc-fxir-native--wrapper (row)
  "The `defun' of nc_d_ID, the guarded direct entry of ROW's nc_ID."
  (let* ((id (nth 1 row)) (rest (nth 2 (nth 5 row)))
         ;; The record's NLOCALS word while it runs nc_ID: the fixnum -(16+ID).
         (nlw (1+ (* 4 (- -16 id))))
         (restbits (if (>= rest 0) (* 65536 (1+ rest)) 0)))
    `(defun ,(intern (format "nc_d_%d" id)) (env site stk argc resw oslot)
       (let* ((rec ,nelisp-cc-fxir-native--site-rec-form))
         (if (if (= rec 0) 0 (= (nl_vector_slot_word rec 1) ,nlw))
             ;; `m5_fx_callout' then `m5_fx_call_words' minus the lookups the
             ;; guard replaced: the definition kept in the caller's XW word, the
             ;; backtrace record, the recursion budget (`m5_fx_run').
             ;; nc5: the record pushed in place, and the limit below 100 is 100
             ;; without reading `max-lisp-eval-depth' (`nl_eval_depth_limit').
             (let* ((frame (seq (ptr-write-u64 (+ resw 8) 0 (nl_vector_slot_word site 3))
                                ,nelisp-cc-fxir-native--push-form))
                    (depth (heap-read-u64 (+ env 96) 0))
                    (limit (if (< (+ depth 2) 100) 100 (nl_eval_depth_limit env (+ depth 2))))
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
                 ;; nc5: a plain success (`m5_fx_call_words_done' -> `nl_n4_finish_w'
                 ;; -> `m5_fx_take' with the result already in RESW) pops the
                 ;; record inline; a signal, a flagged record (debug-on-exit) or
                 ;; no record keeps the helper.
                 (if (if (= st -1)
                         (if (/= frame 0)
                             (if (/= (ptr-read-u64 resw 0) 0) (= (ptr-read-u8 frame 18) 0) 0)
                           0)
                       0)
                     (let* ((fstate (heap-read-u64 env 176)))
                       (seq (ptr-write-u64 fstate 0 (- (ptr-read-u64 fstate 0) 1)) 0))
                   (m5_fx_call_words_done st env oslot resw frame 0))))
           (nc_call env site stk argc resw oslot))))))

(defun nelisp-cc-fxir-native--call (n)
  "Lower (:call K ARGC ARGS...)."
  (let* ((k (nth 1 n)) (argc (nth 2 n))
         (name (pop nelisp-cc-fxir-native--callees))
         (origin (and (or nelisp-cc-fxir-native-inline-projections
                          nelisp-cc-fxir-native-inline-additions)
                      (pop nelisp-cc-fxir-native--inline-sites)))
         (args (nelisp-cc-fxir-native--args (nthcdr 3 n)))
         (vals (cdr args))
         (fast (nelisp-cc-fxir-native--inline-fast name vals (nelisp-cc-fxir-native--cv k)))
         (nary (and (null fast) (memq name '(+ -)) (>= argc 3)))
         (gkind (and (null fast)
                     (cond ((and (eq name 'set) (= argc 2)) 22)
                           ((and (eq name 'symbol-value) (= argc 1)) 21))))
         (drow (and (null fast) (null gkind) (nelisp-cc-fxir-native--direct-row name argc)))
         (dsub (and (null fast) (null gkind) (null drow) (not nary)
                    (nelisp-cc-fxir-effects-direct-subr name argc)))
         (kind (and drow origin
                    (< nelisp-cc-fxir-native--inline-count 2)
                    (cond ((and nelisp-cc-fxir-native-inline-projections
                                (> nelisp-cc-fxir-native--inline-budget 0)
                                (nelisp-cc-fxir-inline-projection drow)) :projection)
                          ((and nelisp-cc-fxir-native-inline-additions
                                nelisp-cc-fxir-native--eager
                                (>= nelisp-cc-fxir-native--inline-budget 4)
                                (nelisp-cc-fxir-inline-addition drow)) :addition))))
         (expanded
          (when kind
            (cl-incf nelisp-cc-fxir-native--inline-count)
            (cl-decf nelisp-cc-fxir-native--inline-budget (if (eq kind :addition) 4 1))
            (let* ((site (nelisp-cc-fxir-native--cv k))
                   (token (cons (car (plist-get origin :id)) (cadr (plist-get origin :id))))
                   (baseline (nelisp-cc-fxir-inline-form drow site argc token kind)))
              (if (and (eq kind :addition) nelisp-cc-fxir-native-inline-checked-additions
                       (require 'nelisp-cc-fxir-facts nil t)
                       (fboundp 'nelisp-cc-fxir-facts-verify-add-region))
                  (let ((candidate (nelisp-cc-fxir-inline-form drow site argc token kind t)))
                    (if (condition-case nil
                            (nelisp-cc-fxir-facts-verify-add-region drow site baseline candidate)
                          (error nil))
                        candidate baseline))
                baseline)))))
    (cond
     ;; N-ary + / -: GNU compiles them to chained Bplus / Bdiff
     ;; (byte-compile-variadic-numeric), which `fset' cannot redefine, so no
     ;; function guard is needed.  Arguments are already evaluated once, left to
     ;; right, into VALS; a non-fixnum or an overflowing partial result takes
     ;; the generic call on the SAME words.
     (nary
      (let* ((r (nelisp-cc-fxir-native--temp))
             (call (nelisp-cc-fxir-native--call-stmts (nelisp-cc-fxir-native--cv k) vals argc)))
        (cons (append (car args)
                      (list (cons nil (nelisp-cc-fxir-native--nary-fast name r vals))
                            (cons (nelisp-cc-fxir-native--op-fallible-p :call) `(if (= ,r -1)
                                         ,(nelisp-cc-fxir-native--block
                                           (append (car call) (list (cons nil `(setq ,r ,(cdr call))))))
                                       0))))
              r)))
     ;; nc3: Bset / Bsymbol_value (GNU opcodes, not redefinable) run the VM's
     ;; globals arm (`m5_fx_globals_run' 22 / 21) on the staged words; a case it
     ;; declines (a non-symbol: the error) takes the generic call.
     (gkind
      (nelisp-cc-fxir-native--frame-forms argc)
      (let ((r (nelisp-cc-fxir-native--temp)))
        (cons (append (car args)
                      (nelisp-cc-fxir-native--win)
                      (list (cons (nelisp-cc-fxir-native--op-fallible-p :call) `(setq st ,(nelisp-cc-fxir-native--memchk
                                                `(nc_globals env ,(nelisp-cc-fxir-native--cv k) mem oslot ,gkind
                                                             ,(nth 0 vals) ,(or (nth 1 vals) 3)))))
                            (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
              r)))
     ;; Guarded direct subr call: arguments are already evaluated once, left to
     ;; right, into VALS and staged in the window; a function-cell guard (the
     ;; primitive's Subr header id) selects the primitive's directcall entry,
     ;; otherwise the generic call runs on the same staged words.
     (dsub
      (nelisp-cc-fxir-native--frame-forms argc)
      (let ((r (nelisp-cc-fxir-native--temp))
            (site (nelisp-cc-fxir-native--cv k)))
        (cons (append (car args)
                      (nelisp-cc-fxir-native--win)
                      (list (cons (nelisp-cc-fxir-native--op-fallible-p :call)
                                  `(setq st ,(nelisp-cc-fxir-native--memchk
                                              `(seq ,@(let ((i 0) (f nil))
                                                        (dolist (v vals) (push `(ptr-write-u64 (+ mem (* ,(+ 3 i) 8)) 0 ,v) f)
                                                                (setq i (1+ i)))
                                                        (nreverse f))
                                                    (if ,(nelisp-cc-fxir-native--subr-guard
                                                          site (+ 17 (plist-get dsub :id)))
                                                        (nc_dsubr_call env ,site (+ mem 24) ,argc mem oslot)
                                                      (nc_call env ,site (+ mem 24) ,argc mem oslot))))))
                            (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
              r)))
     ((null fast)
        (let ((call (if drow
                        (nelisp-cc-fxir-native--direct-stmts (nelisp-cc-fxir-native--cv k) vals argc (nth 1 drow) expanded)
                      (nelisp-cc-fxir-native--call-stmts (nelisp-cc-fxir-native--cv k) vals argc))))
          (cons (append (car args) (car call)) (cdr call))))
     ;; nc3: a fast path that decides every case (Beq) needs no generic call.
     ;; The callee-refined effect class (`nelisp-cc-fxir-effects-call-of')
     ;; says this call is non-fallible; the statement flag reads it.
     ((eq name 'eq)
      (let ((r (nelisp-cc-fxir-native--temp)))
        (cons (append (car args)
                      (list (cons (nelisp-cc-fxir-native--call-fallible-p name)
                                  `(setq ,r ,fast))))
              r)))
     (t
      ;; Fast path inline; the generic call (the VM's own route) out of it.
      (let* ((r (nelisp-cc-fxir-native--temp))
             (call (nelisp-cc-fxir-native--call-stmts (nelisp-cc-fxir-native--cv k) vals argc)))
        (cons (append (car args)
                      (list (cons nil `(setq ,r ,fast))
                            (cons (nelisp-cc-fxir-native--op-fallible-p :call) `(if (= ,r 0)
                                         ,(nelisp-cc-fxir-native--block
                                           (append (car call) (list (cons nil `(setq ,r ,(cdr call))))))
                                       0))))
              r))))))

(defun nelisp-cc-fxir-native--inline-op (n)
  "Lower (:inline OPCODE K ARGS...), the VM's fixnum operations.
Opcodes: 4 +, 5 -, 6 <, 13 =, 14 >, 7 1+, 15 1-."
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
                        (cons (nelisp-cc-fxir-native--op-fallible-p :inline) `(if (= ,r -1)
                                     (seq ,@(mapcar #'cdr (nelisp-cc-fxir-native--win))
                                          (setq st (nc_arith env ,(nelisp-cc-fxir-native--cv k) mem oslot
                                                             ,(+ op (* 256 (length vals))) ,a ,b))
                                          (if (= st 0) (setq ,r (ptr-read-u64 mem 0)) 0))
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
                              `(if ,(nelisp-cc-fxir-native--truthy
                                     (cdr lc)
                                     (and (not (eq (car-safe c) :lowered)) c))
                                   ,(funcall arm la) ,(funcall arm lb)))))
          r)))

(defun nelisp-cc-fxir-native--value-block (low r)
  "One form running LOW's statements, then R set to its value."
  (nelisp-cc-fxir-native--block (append (car low) (list (cons nil `(setq ,r ,(cdr low)))))))

;; nativecomp N3: handlers.  A failing call leaves ST non-zero and the
;; statements after it are skipped up to the end of the region's block, so
;; the region's handler is the code right after that block: no handler
;; stack, as the regions nest statically (the VM's frames, `m5_fx_run_body'
;; ops 22-27, are this nesting made dynamic).  What a landing does is the
;; VM's: a catch matches a throw to its tag (`nc_catch_match'), a
;; condition-case clause a signal of its conditions, an unwind-protect keeps
;; the exit across its cleanup; the bindings made inside the region are
;; undone first (the static count `--nbd' at the region's start).  A catch
;; also registers a node on the catch chain, in three words of the window,
;; so `throw' finds it.  Callees are lowered in the order
;; `nelisp-nc--callees' walks them.
(defun nelisp-cc-fxir-native--handler (n)
  "Lower a handler node N (:catch :throw :condition-case :unwind-protect)."
  (let ((d nelisp-cc-fxir-native--nbd))
    (pcase (car n)
      (:throw
       (let ((args (nelisp-cc-fxir-native--args (cdr n))))
         (cons (append (car args)
                       (list (cons (nelisp-cc-fxir-native--op-fallible-p :throw) `(setq st (nl_ct_throw_words env ,(nth 0 (cdr args)) ,(nth 1 (cdr args)))))))
               3)))
      (:catch
       (let* ((lt (nelisp-cc-fxir-native--hold (nelisp-cc-fxir-native--lower (nth 1 n))))
              (lb (nelisp-cc-fxir-native--lower (nth 2 n)))
              (r (nelisp-cc-fxir-native--temp))
              (node `(+ mem (* (+ cbase ,(* 3 nelisp-cc-fxir-native--ncatch)) 8))))
         (setq nelisp-cc-fxir-native--ncatch (1+ nelisp-cc-fxir-native--ncatch))
         (cons (append (car lt)
                       (nelisp-cc-fxir-native--win)
                       (list (cons (nelisp-cc-fxir-native--op-fallible-p :catch) `(seq (nc_catch_push env ,node ,(cdr lt))
                                           ,(nelisp-cc-fxir-native--value-block lb r)
                                           (nc_catch_pop env ,node)
                                           (if (> st 0)
                                               (if (= (nc_catch_match ,node) 1)
                                                   (seq (setq nb (nc_unbind_to env nb ,d))
                                                        (setq ,r (nl_ct_caught_word (nl_exit_val_w) 0))
                                                        (setq st 0))
                                                 0)
                                             0)))))
               r)))
      (:condition-case
       (let* ((vs (nth 1 n))
              (var (and vs (nelisp-cc-fxir-native--local vs)))
              (lb (nelisp-cc-fxir-native--lower (nth 2 n)))
              (ls (and (nth 3 n) (nelisp-cc-fxir-native--lower (nth 3 n))))
              (hs (mapcar (lambda (c) (cons (car c) (nelisp-cc-fxir-native--lower (nth 1 c))))
                          (nthcdr 4 n)))
              (r (nelisp-cc-fxir-native--temp))
              (chain 0))
         (dolist (h (reverse hs))
           (setq chain
                 `(if (= (nc_cc_match env ,(nelisp-cc-fxir-native--cv (car h))) 1)
                      (seq (setq nb (nc_unbind_to env nb ,d))
                           (setq st 0)
                           ,(if var `(setq ,var (m5_fx_exit_condition 0 0)) '(m5_fx_exit_condition 0 0))
                           ,(nelisp-cc-fxir-native--value-block (cdr h) r))
                    ,chain)))
         (cons (list (cons (nelisp-cc-fxir-native--op-fallible-p :condition-case) `(seq ,(nelisp-cc-fxir-native--value-block lb r)
                                   (if (= st 0)
                                       ,(if ls
                                            `(seq ,(if var `(setq ,var ,r) 0)
                                                  ,(nelisp-cc-fxir-native--value-block ls r))
                                          0)
                                     (if (> st 0) ,chain 0)))))
               r)))
      (:unwind-protect
       (let* ((lb (nelisp-cc-fxir-native--lower (nth 1 n)))
              (lc (nelisp-cc-fxir-native--lower (nth 2 n)))
              (r (nelisp-cc-fxir-native--temp))
              (rc (nelisp-cc-fxir-native--temp)) (fl (nelisp-cc-fxir-native--temp))
              (tg (nelisp-cc-fxir-native--temp)) (vl (nelisp-cc-fxir-native--temp)))
         (cons (list (cons (nelisp-cc-fxir-native--op-fallible-p :unwind-protect) `(seq ,(nelisp-cc-fxir-native--value-block lb r)
                                   (if (> st 0)
                                       (seq (setq ,rc (+ (shl st 2) 1))
                                            (setq ,fl (+ (shl (heap-read-u64 268435472 0) 2) 1))
                                            (setq ,tg (nl_exit_tag_w))
                                            (setq ,vl (nl_exit_val_w))
                                            (setq nb (nc_unbind_to env nb ,d))
                                            (setq st 0))
                                     (setq ,rc 1))
                                   ,(nelisp-cc-fxir-native--block (car lc))
                                   (if (= st 0)
                                       (if (= ,rc 1) 0
                                         (seq (nc_exit_put ,tg ,vl (sar ,fl 2))
                                              (setq st (sar ,rc 2))))
                                     0))))
               r))))))

(defun nelisp-cc-fxir-native--lower (n)
  "Lower fxir node N to (STMTS . VALUE)."
  (pcase (car n)
    (:const (cons nil (nelisp-cc-fxir-native--cv (nth 1 n))))
    (:slot (cons nil (nelisp-cc-fxir-native--local (nth 1 n))))
    (:varref
     (let ((r (nelisp-cc-fxir-native--temp)))
       (nelisp-cc-fxir-native--frame-forms 0)
       (cons (append (butlast (nelisp-cc-fxir-native--win))
                     (list (cons (nelisp-cc-fxir-native--op-fallible-p :varref) `(setq st ,(nelisp-cc-fxir-native--memchk
                                               `(wf_bytecode_w_varref env ,(nelisp-cc-fxir-native--cv (nth 1 n)) mem 0))))
                           (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
             r)))
    (:alias
     (let ((r (nelisp-cc-fxir-native--temp)))
       (nelisp-cc-fxir-native--frame-forms 0)
       (cons (append (butlast (nelisp-cc-fxir-native--win))
                     (list (cons (nelisp-cc-fxir-native--op-fallible-p :alias) `(setq st ,(nelisp-cc-fxir-native--memchk
                                               `(m5_fx_alias_ref ,(nelisp-cc-fxir-native--cv (nth 1 n)) env mem 0))))
                           (cons nil `(setq ,r (ptr-read-u64 mem 0)))))
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
                                      (list (cons nil `(if (= st 0)
                                                           ,(nelisp-cc-fxir-native--truthy
                                                             (cdr lc) (nth 1 n))
                                                         0)))))
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
               (setq stmts (append stmts (list (cons (nelisp-cc-fxir-native--op-fallible-p :store) `(setq ,(nelisp-cc-fxir-native--local (nth 1 it)) ,(cdr l))))))
             ;; VARBIND: GNU's Bvarbind through the byte-code VM's binder.
             (setq stmts (append stmts
                                 (list (cons (nelisp-cc-fxir-native--op-fallible-p :varbind) `(setq st (wf_bytecode_varbind env ,(nelisp-cc-fxir-native--cv (nth 1 it)) ,(cdr l))))
                                       (cons nil '(setq nb (+ nb 1)))))))))
       (let ((body (let ((nelisp-cc-fxir-native--nbd (+ nelisp-cc-fxir-native--nbd (nth 3 n))))
                     (nelisp-cc-fxir-native--lower (nth 2 n)))))
         (if (= (nth 3 n) 0)
             (cons (append stmts (car body)) (cdr body))
           (let ((held (nelisp-cc-fxir-native--hold body)))
             (cons (append stmts (car held)
                           (list (cons nil `(seq (wf_bytecode_unbind (+ env 32) ,(nth 3 n))
                                                 (setq nb (- nb ,(nth 3 n)))))))
                   (cdr held)))))))
    (:call (nelisp-cc-fxir-native--call n))
    (:inline (nelisp-cc-fxir-native--inline-op n))
    ((or :catch :throw :condition-case :unwind-protect)
     (nelisp-cc-fxir-native--handler n))
    (_ (error "fxir-native: bad node %S" n))))

(defun nelisp-cc-fxir-native--m1-candidate-p (analysis)
  "Whether ANALYSIS has a site that may select an enabled M1 transform.
Use the existing callee certificates and initial growth budget before M2
captures provenance.  This is conservative selection, not verification;
the normal lowerer retains every live guard and checked-region verifier."
  (condition-case nil
      (let ((budget (/ (or (plist-get analysis :nodes) 0) 5)))
        (cl-some
         (lambda (site)
           (let* ((name (plist-get site :callee)) (argc (plist-get site :argc))
                  ;; Fast primitive lowering is pure and depends on name/arity,
                  ;; not on the staged argument values used in its expression.
                  (fast (nelisp-cc-fxir-native--inline-fast name (make-list argc nil)))
                  (global (or (and (eq name 'set) (= argc 2))
                              (and (eq name 'symbol-value) (= argc 1))))
                  (callee (and (null fast) (not global)
                               (nelisp-cc-fxir-native--direct-row name argc))))
             (and callee
                  (or (and nelisp-cc-fxir-native-inline-projections (> budget 0)
                           (nelisp-cc-fxir-inline-projection callee))
                      (and nelisp-cc-fxir-native-inline-additions
                           nelisp-cc-fxir-native--eager (>= budget 4)
                           (nelisp-cc-fxir-inline-addition callee))))))
         (plist-get analysis :sites)))
    ;; A malformed preflight cannot establish untransformed provenance.
    (error t)))

(defun nelisp-cc-fxir-native--defun (row &optional body-factory)
  "Generate nc_ID for ROW, optionally replacing only its lowered body.
BODY-FACTORY receives the original lowering after frame sizing facts are set."
  (when nelisp-cc-fxir-native-diagnostics
    (require 'nelisp-cc-fxir-analysis)
    (funcall nelisp-cc-fxir-native-diagnostic-function
             (nelisp-cc-fxir-analysis-row row)))
  (when (or nelisp-cc-fxir-native-inline-projections nelisp-cc-fxir-native-inline-additions)
    (require 'nelisp-cc-fxir-inline))
  (let* ((id (nth 1 row)) (nlocals (max 0 (nth 2 row)))
         (analysis (and (or nelisp-cc-fxir-native-inline-projections
                            nelisp-cc-fxir-native-inline-additions)
                        (nelisp-cc-fxir-analysis-row row)))
         (condition-snapshot
          (and nelisp-cc-fxir-native-condition-facts
               (not body-factory)
               (not (nelisp-cc-fxir-native--m1-candidate-p analysis))
               (nelisp-cc-fxir-native--condition-snapshot row)))
         (nelisp-cc-fxir-native--condition-row row)
         (nelisp-cc-fxir-native--condition-syntax
          (plist-get condition-snapshot :syntax))
         (nelisp-cc-fxir-native--condition-hash
          (plist-get condition-snapshot :hash))
         (nelisp-cc-fxir-native--condition-sites
          (plist-get condition-snapshot :sites))
         (nelisp-cc-fxir-native--inline-sites (plist-get analysis :sites))
         (nelisp-cc-fxir-native--inline-budget (/ (or (plist-get analysis :nodes) 0) 5))
         (nelisp-cc-fxir-native--inline-count 0)
         (arity (nth 5 row)) (amin (nth 0 arity)) (amax (nth 1 arity)) (rest (nth 2 arity))
         ;; Fixed parameters: the locals before &rest, else MAX of them.
         (nfixed (if (>= rest 0) rest amax))
         (nelisp-cc-fxir-native--temps nil)
         (nelisp-cc-fxir-native--callees (copy-sequence (nth 3 row)))
         (nelisp-cc-fxir-native--maxargc 0)
         (nelisp-cc-fxir-native--uses-window nil)
         (nelisp-cc-fxir-native--nbd 0)
         (nelisp-cc-fxir-native--ncatch 0)
         (body (nelisp-cc-fxir-native--lower (nth 1 (nth 4 row))))
         (emitted-body (if body-factory (funcall body-factory body) body))
         (poff (+ 3 nelisp-cc-fxir-native--maxargc))
         ;; nativecomp N3: the catch nodes follow the parameter words.
         (cbase (+ poff nfixed))
         (fsize (+ cbase (* 3 nelisp-cc-fxir-native--ncatch)))
         (locals (let ((l nil)) (dotimes (i nlocals) (push (list (nelisp-cc-fxir-native--local i) 3) l)) (nreverse l)))
         (temps (mapcar (lambda (v) (list v 3)) (reverse nelisp-cc-fxir-native--temps)))
         (entry
          (append
           ;; Eager window (`--eager'): reserved before anything else runs.
           (when (and nelisp-cc-fxir-native--eager nelisp-cc-fxir-native--uses-window)
             (list (cons t '(seq (setq mem (nc_enter env fsize))
                                 (if (= mem 0) (setq st -4) (setq oslot (+ mem fbytes)))))))
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
             ;; nc5: GNU's frame push -- nil (no lexical name to watch) answers
             ;; without the call, and the main thread's root mark and release
             ;; are the stack top itself (`nl_root_mark' / `nl_root_release').
             (let* ((ec (heap-read-u64 (+ (+ (nl_word_payload code) 8) 8) 0)))
             (if (if (= ec 3) 0
                   ;; nc5: an unchanged generation (`m5_fx_entry_check''s second
                   ;; test: slot K of a vector WORD at WORD + 2 + 8K) inline too.
                   (if (if (= (logand ec 7) 6)
                           (let* ((ecell (ptr-read-u64 (+ ec 2) 0)))
                             (if (= (logand ecell 7) 6)
                                 (= (ptr-read-u64 (+ ecell 2) 0) (ptr-read-u64 (+ ec 10) 0))
                               0))
                         0)
                       0
                     (/= (m5_fx_entry_check ec env) 0)))
                 -3
               (let* ((base (+ (nl_word_payload code) 8))
                      (mark (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
                                (heap-read-u64 (data-addr nl_rootstack_top) 0)
                              (nl_root_mark env)))
                      (n (logand nparams 65535))
                      (mem 0) (oslot 0) (st 0) (nb 0)
                      ,@locals ,@temps)
                 (seq
                  ,(nelisp-cc-fxir-native--block
                    (append entry (car emitted-body)
                            (list (cons nil `(if (= wout 0) (nl_abi_word_to_rc ,(cdr emitted-body) out)
                                               (ptr-write-u64 wout 0 ,(cdr emitted-body)))))))
                  ;; One exit: bindings still held (a call failed) are undone.
                  (if (> nb 0) (wf_bytecode_unbind (+ env 32) nb) 0)
                  (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
                      (ptr-write-u64 (data-addr nl_rootstack_top) 0 mark)
                    (nl_root_release env mark))
                  (if (= st 0) -1 (if (= st -4) (wf_bytecode_root_exhausted) st)))))))))
    (ignore amax)
    ;; Selection must remain stable between preflight and lowering.  If a
    ;; hook changes it, refuse the mixed-provenance result without replaying
    ;; checked M1 verification or publishing an M2-optimized transformed row.
    (when (and condition-snapshot (> nelisp-cc-fxir-native--inline-count 0))
      (error "fxir-native: M1 selection changed after condition preflight"))
    (if (and condition-snapshot
             (not (equal (nth 4 row) nelisp-cc-fxir-native--condition-syntax)))
        ;; M1 candidates already declined M2 before lowering.  A changed row
        ;; still has no provenance contract; retain its conservative fallback.
        (let ((nelisp-cc-fxir-native-condition-facts nil))
          (nelisp-cc-fxir-native--defun row body-factory))
      (cl-subst (logand (+ (* fsize 8) 31) -32) 'fbytes
                (cl-subst cbase 'cbase (cl-subst fsize 'fsize form))))))

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
    ;; nativecomp N3 handlers (`nelisp-cc-fxir-native--handler').  A catch
    ;; node is three window words in `nl_ct_active_prepare_w''s layout: the
    ;; fixnum 0, the previous node, the tag.
    (defun nc_catch_push (env node tag)
      (seq (ptr-write-u64 node 0 1)
           (ptr-write-u64 (+ node 8) 0 (ptr-read-u64 (nl_ct_head_slot env) 0))
           (ptr-write-u64 (+ node 16) 0 tag)
           (ptr-write-u64 (nl_ct_head_slot env) 0 node)
           0))
    (defun nc_catch_pop (env node)
      (seq (ptr-write-u64 (nl_ct_head_slot env) 0 (ptr-read-u64 (+ node 8) 0)) 0))
    ;; 1 when the pending exit is a throw to NODE's tag.
    (defun nc_catch_match (node)
      (if (= (heap-read-u64 268435472 0) 2)
          (nl_ct_tag_eq_w (nl_exit_tag_w) (ptr-read-u64 (+ node 16) 0))
        0))
    ;; 1 when the pending exit is a signal SPEC's conditions match.
    (defun nc_cc_match (env spec)
      (if (= (heap-read-u64 268435472 0) 1)
          (wf_bytecode_condition_matches env (nl_exit_tag_w) spec)
        0))
    ;; Undo the dynamic bindings above D of the NB held; answers the count left.
    (defun nc_unbind_to (env nb d)
      (if (> nb d) (seq (wf_bytecode_unbind (+ env 32) (- nb d)) d) nb))
    ;; Put a kept exit back (`nl_exit_restore' from words).
    (defun nc_exit_put (tag val flag)
      (seq (ptr-write-u64 (data-addr nl_exit_values) 0 tag)
           (ptr-write-u64 (+ (data-addr nl_exit_values) 8) 0 val)
           (ptr-write-u64 268435472 0 flag)
           0))

    ;; The generic call of the call-site cache (or symbol) SITE on the ARGC
    ;; words at STK, result word in RESW: the VM's CALL (`m5_fx_run_body'
    ;; op 16) -- a primitive Subr directly, else `m5_fx_callout'.  OSLOT is
    ;; the activation's reserved Sexp view slot.
    ;; nativecomp N2: a native subr with a directcall id is called on the
    ;; staged words (`nl_directcall_invoke', the byte-code VM's Bcall route,
    ;; GNU funcall_subr), not through the call-out's argument list.
    ;; nativecomp N2b (nc3): a builtin in the cell (a Subr object or the legacy
    ;; (builtin NAME) list) never takes the call-out (`nc_subr_call').
    ;; callpath: any Subr object with an argument-array entry is called on the
    ;; staged words too (`m5_fx_call_subr_any', GNU funcall_subr), before the
    ;; descriptor and list arms of `nc_subr_call' (subr1's split regression:
    ;; fboundp went nc_subr_call -> nc_desc_call -> nc_builtin_call).
    (defun nc_call (env site stk argc resw oslot)
      (let* ((st (m5_fx_call_subr_any env site stk argc argc resw)))
        (if (/= st -3) st
          (let* ((d (nc_subr_call env site stk argc resw oslot)))
            (if (/= d -5) d
              (m5_fx_callout env site stk argc argc resw (+ resw 8) oslot 0))))))
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
                 ;; subr1: header bits 8-15 are id+17, 0 for a builtin with no
                 ;; directcall id, for which `nl_directcall_invoke' answers 2
                 ;; at once with nothing done.
                 (let* ((d (if (= (logand (ptr-read-u64 (- fn 6) 0) 65280) 0) 2
                             (nl_directcall_invoke fn sym stk argc env resw))))
                   (if (/= d 2) d
                     (if (= (nl_sb_worker_p env) 1) -5
                       (nc_desc_call env site sym fn stk argc resw oslot)))))
          (if (if (= cls 2) (= (nl_sb_worker_p env) 0) 0)
              (seq (if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0)
                   (nc_builtin_call env site sym fn stk argc resw oslot 0))
            -5))))
    ;; Guarded direct subr call: the inline guard at the call site proved the live
    ;; function cell is the expected primitive's Subr object.  Run its directcall
    ;; entry (`nl_directcall_invoke': 0 done, 1 signalled, 2 not handled with
    ;; nothing done) on the staged words, as `nc_subr_call' does, with the same
    ;; GNU Bcall safepoint first.  The entry re-reads the live cell's header, so
    ;; a cell changed meanwhile only costs the generic call.  2 (a declined
    ;; shape, a debug policy, a bad arity) takes the full generic call on the
    ;; SAME staged words, which owns the error and its backtrace.
    (defun nc_dsubr_call (env site stk argc resw oslot)
      (let* ((sym (nc_site_sym site))
             (fn (if (= sym 0) 0 (ptr-read-u64 (- sym 6) 24))))
        (seq (if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0)
             ;; hash4: gethash (static id 101, Subr header byte 118) skips the
             ;; `nl_directcall_invoke'/`nl_directcall_id' layers with the same
             ;; checks they make for a leaf id: a Subr header re-read now, its
             ;; arity, the debug-policy bits and the backtrace depth limit.
             ;; Anything else (or any failed check) takes the unchanged entry.
             (let* ((hdr (if (= (logand fn 7) 6)
                             (if (< fn 4096) 0 (ptr-read-u64 (- fn 6) 0))
                           0))
                    (desc (sar hdr 8))
                    (st (heap-read-u64 env 176))
                    (d (if (if (= (logand hdr 63) 20)
                               (if (= (logand desc 255) 118)
                                   (if (>= argc (logand (sar desc 8) 255))
                                       (if (if (= (logand (sar desc 16) 255) 127) 1
                                             (<= argc (logand (sar desc 16) 255)))
                                           (if (= (logand (heap-read-u64 env 112) 3145728) 0)
                                               (if (= st 0) 0 (< (heap-read-u64 st 0) 32768))
                                             0)
                                         0)
                                     0)
                                 0)
                             0)
                           (let* ((r (nl_directcall_hash_get stk argc resw env)))
                             (if (= r 0) 0 (if (= r -3) 2 1)))
                         (nl_directcall_invoke fn sym stk argc env resw))))
               (if (/= d 2) d (nc_call env site stk argc resw oslot))))))
    ;; A Subr object with no directcall entry for this call: its interpreter
    ;; descriptor's primitive (`nl_eic_prim_frame_w''s dispatch), funcall (13)
    ;; through the site, else the list apply of the legacy list it wraps.  A
    ;; wrong argument count goes to the call-out, which signals it.
    ;; subr1: with no interpreter descriptor (every builtin without a
    ;; primitive id now has a Subr object) the legacy list takes the arms it
    ;; took when it was the cell itself (`nc_builtin_call'), with no
    ;; "builtin" name test: the object proves it.
    (defun nc_desc_call (env site sym fn stk argc resw oslot)
      (let* ((p (shr (ptr-read-u64 (- fn 6) 0) 33))
             (legacy (ptr-read-u64 (- fn 6) 8)))
        (if (= p 0) (nc_builtin_call env site sym legacy stk argc resw oslot 1)
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
    ;; cached kind, then the list apply; -5 when FN is no builtin list.  KNOWN 1:
    ;; FN is the list a Subr object wraps, a builtin by construction.
    (defun nc_builtin_call (env site sym fn stk argc resw oslot known)
      (let* ((cache (if (= (nl_word_tag64 site) 8) site 0))
             (kind (m5_fx_site_kind cache fn argc env)))
        (if (= kind 0)
            (if (if (= known 1) 0 (= (m5_fx_builtin_name fn 0) 0)) -5
              (nc_list_call env sym fn stk argc resw oslot))
          (let* ((st (seq (ptr-write-u64 (+ resw 8) 0 fn)
                          (if (> kind 20) (m5_fx_globals_run kind stk argc argc resw env oslot)
                            (if (= kind 13) (nc_funcall env site stk argc resw)
                              ;; callpath2: `apply' on the staged words.
                              (if (= kind 14) (m5_fx_apply_run stk argc argc resw env)
                                (m5_fx_primitive_run kind stk argc argc resw env (+ resw 8))))))))
            (if (= st -3) (nc_list_call env sym fn stk argc resw oslot) st)))))
    ;; `funcall' with the site's callee cache (`m5_fx_callout' kind 13).
    (defun nc_funcall (env site stk argc resw)
      (m5_fx_funcall_site (if (= (nl_word_tag64 site) 8) (if (= (nl_sb_worker_p env) 1) 0 site) 0)
                          (ptr-read-u64 stk 0) stk argc (- argc 1) resw env (+ resw 8)))
    ;; The ordinary apply of the builtin FN (`m5_fx_call_list'), FN kept in the
    ;; window's definition word as the call-out keeps it.
    (defun nc_list_call (env sym fn stk argc resw oslot)
      (seq (ptr-write-u64 (+ resw 8) 0 fn)
           (m5_fx_call_list sym fn stk argc argc env resw (+ resw 16) oslot)))
    ;; nc5: GNU's frame push (bytecode.c:510-525) for an activation's window:
    ;; the WORD frame of N slots (`nl_root_frame''s layout: the header, every
    ;; slot nil) and right after it the view slot (`nl_root_reserve''s zeroed
    ;; 32-byte entry), so the view slot is MEM + the frame's padded size and
    ;; one store moves the top.  The main thread with no worker registered and
    ;; its stack set up takes it inline; anything else is
    ;; `nl_root_frame' then `nl_root_reserve' (`nc_enter_slow').  0: the
    ;; region cannot hold both (the activation answers -4, root exhausted).
    (defun nc_enter (env n)
      (let* ((top (heap-read-u64 (data-addr nl_rootstack_top) 0))
             (fb (logand (+ (* n 8) 31) -32))
             (next (+ top (+ 64 fb))))
        (if (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
                (if (= top 0) 0 (<= next (+ (data-addr nl_rootstack_region) 4194304)))
              0)
            (let* ((slot (+ top 32)) (end (+ slot (* n 8))) (view (+ slot fb)))
              (seq
               (ptr-write-u64 top 0 1464226381)
               (ptr-write-u64 top 8 n)
               (ptr-write-u64 top 16 0)
               (ptr-write-u64 top 24 0)
               (while (< slot end)
                 (seq (ptr-write-u64 slot 0 3)
                      (setq slot (+ slot 8))))
               (ptr-write-u64 view 0 0)
               (ptr-write-u64 view 8 0)
               (ptr-write-u64 view 16 0)
               (ptr-write-u64 view 24 0)
               (ptr-write-u64 (data-addr nl_rootstack_top) 0 next)
               (+ top 32)))
          (nc_enter_slow env n fb))))
    ;; A worker, an uninitialised stack or a full region: the general frame
    ;; and slot; a view slot not adjacent to the frame (the region's scratch
    ;; fallback) is refused, so the caller's MEM + FB is always the slot.
    (defun nc_enter_slow (env n fb)
      (let* ((mark (nl_root_mark env)) (mem (nl_root_frame env n)))
        (if (= mem 0) 0
          (if (= (nl_root_reserve env) (+ mem fb)) mem
            (seq (nl_root_release env mark) 0)))))
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
    ;; else the canonical intrinsic provider, independent of SITE's cell.
    (defun nc_arith (env site mem oslot opn a b)
      (if (= mem 0) -4
        (let* ((op (logand opn 255))
               (r (if (> opn 511) (nc_arith_slow op a b) (nc_incdec_slow op a))))
          (if (/= r -1) (seq (ptr-write-u64 mem 0 r) 0)
            (let* ((argc (if (> opn 511) 2 1)))
              (seq (ptr-write-u64 (+ mem 24) 0 a)
                   (if (= argc 2) (ptr-write-u64 (+ mem 32) 0 b) 0)
                   (m5_fx_intrinsic_slow op env site (+ mem 24) argc argc mem (+ mem 8) oslot)))))))
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
      (if (= (logand arr 7) 4)
          (if (< arr 4096) 0
            (if (= (logand index 3) 1)
                (let* ((hdr (- arr 4)) (i (sar index 2)))
                  (if (< i 0) 0
                    (if (< (ptr-read-u64 hdr 8) 0)
                        (if (>= i (logand (ptr-read-u64 hdr 0) 9223372036854775807)) 0
                          (let* ((data (ptr-read-u64 hdr 24)))
                            (if (< data 4096) 0
                              (+ (* (ptr-read-u8 data i) 4) 1))))
                      0)))
              0))
      (if (= (logand arr 7) 6)
          (if (< arr 4096) 0
            (if (= (logand index 3) 1)
                (let* ((hdr (ptr-read-u64 (- arr 6) 0)) (k (logand hdr 63)) (i (sar index 2)))
                  (if (= k 12)
                      (if (if (< i 1) 1 (> i (sar hdr 8))) 0
                        (heap-read-u64 (- arr 6) (+ 16 (* (- i 1) 8))))
                    (if (= k 8)
                        (if (= (sar hdr 8) 7) 0
                          (if (if (< i 0) 1 (>= i (sar hdr 8))) 0
                            (heap-read-u64 (- arr 6) (+ 8 (* i 8)))))
                      0)))
              0))
        ;; Genuine BoolVector is a legacy tag-10 slot, not a vectorlike
        ;; header: its payload box owns N bits and low-bit-first bytes.
        (if (= (logand arr 7) 0)
            (if (< arr 4096) 0
              (if (= (logand index 3) 1)
                  (if (= (ptr-read-u8 arr 0) 10)
                      (let* ((box (ptr-read-u64 arr 8)) (i (sar index 2)))
                        (if (if (< box 4096) 1 (/= (logand box 7) 0)) 0
                          (if (if (< i 0) 1 (>= i (ptr-read-u64 box 0))) 0
                            (let* ((data (ptr-read-u64 box 8)))
                              (if (< data 4096) 0
                                (if (= (logand (sar (ptr-read-u8 data (sar i 3))
                                                   (logand i 7)) 1) 0)
                                    3 7))))))
                    0)
                0))
          0))))
    ;; Baset fast path on a record slot K > 0 or a plain vector element, as
    ;; `wf_bytecode_vector_primitive' installs it; 0 for the generic call.
    (defun nc_aset_fast (arr index value)
      (if (= (logand arr 7) 6)
          (if (< arr 4096) 0
            (if (= (logand index 3) 1)
                (let* ((hdr (ptr-read-u64 (- arr 6) 0)) (k (logand hdr 63)) (i (sar index 2)))
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
           (let* ((e (ptr-read-u64 (- cur 2) 0)))
             (if (if (= e x) 1 (if (= (logand (logand e x) 1) 1) 0 (wf_bytecode_w_eqw e x)))
                 (setq res cur)
               (seq (setq cur (ptr-read-u64 (- cur 2) 8))
                    (setq n (+ n 1))
                    (if (> n 65536) (setq res 1) 0)))))
         (if (= res 1) 0 (if (/= res 0) res (if (= cur 3) 3 0))))))
    ;; Bassq: the first element that is a cons whose car is `eq' to KEY; other
    ;; elements are skipped (GNU Fassq); a legacy element or tail goes out.
    (defun nc_assq_fast (key l)
      (let* ((cur l) (res 0) (n 0))
        (seq
         (while (if (= res 0) (= (logand cur 7) 2) 0)
           (let* ((e (ptr-read-u64 (- cur 2) 0)))
             (if (= (logand e 7) 0) (setq res 1)
               (seq
                (if (= (logand e 7) 2)
                    (let* ((k (ptr-read-u64 (- e 2) 0)))
                      (if (if (= k key) 1 (if (= (logand (logand k key) 1) 1) 0 (wf_bytecode_w_eqw k key)))
                          (setq res e) 0))
                  0)
                (if (= res 0)
                    (seq (setq cur (ptr-read-u64 (- cur 2) 8))
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
                 (seq (setq cur (ptr-read-u64 (- cur 2) 8)) (setq i (- i 1))))
               (if (= cur 3) 3
                 (if (> i 0) 0
                   (if (= mode 1) cur
                     (if (= (logand cur 7) 2) (ptr-read-u64 (- cur 2) 0) 0)))))))
        0))
    ;; Blength (`bf_length_checked_w'): nil, a string word's character count, a
    ;; proper list of direct conses (up to 65536; one that may be a closure --
    ;; a symbol first -- asks `nl_closure_shape_p', whose slot count is the
    ;; generic call's), a plain vector (7 slots: the char-table bridge goes out),
    ;; or a genuine legacy bool-vector's bit count. Inputs are live WORDs.
    (defun nc_length_fast (a)
      (if (= a 3) 1
        (if (= (logand a 7) 4)
            (let* ((n (logand (ptr-read-u64 (- a 4) 0) 9223372036854775807)))
              (if (if (>= n 0) (< n 2305843009213693952) 0) (+ (shl n 2) 1) 0))
          (if (= (logand a 7) 2)
              (if (if (= (logand (ptr-read-u64 (- a 2) 0) 7) 6) (= (nl_closure_shape_p a) 1) 0) 0
                (let* ((n 0) (cur a))
                  (seq (while (if (= (logand cur 7) 2) (<= n 65536) 0)
                         (seq (setq cur (ptr-read-u64 (- cur 2) 8)) (setq n (+ n 1))))
                       (if (= cur 3) (+ (shl n 2) 1) 0))))
            (if (= (logand a 7) 6)
                (if (< a 4096) 0
                  (if (= (logand (ptr-read-u64 (- a 6) 0) 63) 8)
                      (let* ((n (vector-len a))) (if (if (>= n 0) (/= n 7) 0) (+ (shl n 2) 1) 0))
                    0))
              (if (= (logand a 7) 0)
                  (if (< a 4096) 0
                    (if (= (ptr-read-u8 a 0) 10)
                        (let* ((box (ptr-read-u64 a 8)))
                          (if (if (< box 4096) 1 (/= (logand box 7) 0)) 0
                            (let* ((n (ptr-read-u64 box 0)))
                              (if (if (>= n 0) (< n 2305843009213693952) 0)
                                  (+ (shl n 2) 1) 0))))
                      0))
                0))))))
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
    nc_subr_call nc_dsubr_call
    ;; nc5
    nc_enter nc_enter_slow
    ;; nc3
    nc_desc_call nc_builtin_call nc_funcall nc_list_call nc_pred6 nc_mul_fast nc_rem_fast
    nc_equal_fast nc_memq_fast nc_assq_fast nc_nthcdr_fast nc_length_fast nc_globals
    ;; nc4
    nc_catch_push nc_catch_pop nc_catch_match nc_cc_match nc_unbind_to nc_exit_put)
  "Shared helpers the unit inliner must leave as calls.")

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

(defun nelisp-cc-fxir-native--closed-scalar-helpers (&optional forms target)
  "Return unique closed scalar providers in FORMS for supported TARGET.
This deliberately small effect summary proves only absence of calls, memory,
global references and traps.  Ordinary inlining still applies all its checks.
The source DSL is inspected before normalization: every accepted operation
already has a nontrapping scalar compiler lowering.  Unknown syntax declines."
  (when (equal (or target (getenv "NELISP_STANDALONE_TARGET") "linux-x86_64")
               "linux-x86_64")
    (let ((providers (make-hash-table :test 'eq)) result
          (catalog (or forms nelisp-cc-fxir-native--shared-source)))
      (cl-labels
          ((binder-p (name)
             (and (symbolp name) name (not (memq name '(t nil)))
                  (not (keywordp name))
                  (not (string-prefix-p "&" (symbol-name name)))
                  (not (special-variable-p name))))
           (walk (expr scope)
             (cond
              ((integerp expr) (<= (- (expt 2 63)) expr (1- (expt 2 63))))
              ((symbolp expr) (and (memq expr scope) t))
              ((not (proper-list-p expr)) nil)
              ((eq (car expr) 'let*)
               (and (= (length expr) 3) (proper-list-p (cadr expr))
                    (let ((locals scope) (ok t))
                      (dolist (binding (cadr expr))
                        (if (and (proper-list-p binding) (= (length binding) 2)
                                 (binder-p (car binding))
                                 (not (memq (car binding) locals))
                                 (walk (cadr binding) locals))
                            (push (car binding) locals)
                          (setq ok nil)))
                      (and ok (walk (caddr expr) locals)))))
              ((eq (car expr) 'if)
               (and (= (length expr) 4)
                    (cl-every (lambda (x) (walk x scope)) (cdr expr))))
              ((and (memq (car expr) '(+ - * logand logior logxor shl sar = /= < <= > >=))
                    (= (length expr) 3) (not (memq (car expr) scope)))
               (and (walk (cadr expr) scope) (walk (caddr expr) scope)))
              (t nil))))
        (when (and (proper-list-p catalog)
                   (cl-every
                    (lambda (form)
                      (and (proper-list-p form) (= (length form) 4)
                           (eq (car form) 'defun) (binder-p (cadr form))
                           (not (gethash (cadr form) providers))
                           (proper-list-p (nth 2 form))
                           (cl-every #'binder-p (nth 2 form))
                           (= (length (nth 2 form))
                              (length (delete-dups (copy-sequence (nth 2 form)))))
                           (progn (puthash (cadr form) (list form) providers) t)))
                    catalog))
        (maphash
         (lambda (name definitions)
           (when (= (length definitions) 1)
             (let* ((form (car definitions)) (args (nth 2 form)))
               (when (and (= (length form) 4) (proper-list-p args)
                          (cl-every #'binder-p args)
                          (= (length args) (length (delete-dups (copy-sequence args))))
                          (walk (nth 3 form) args))
                 (push name result)))))
         providers)))
      (nreverse result))))

(defconst nelisp-cc-fxir-native--boundaries
  (append nelisp-cc-fxir-native--fixed-boundaries
          nelisp-cc-fxir-native--wrapper-names)
  "Names the unit inliner must leave as calls: the shared helpers and the
direct-call wrappers (inlined, each would copy its callee's entry into every
caller).")

(defun nelisp-cc-fxir-native-install-table-source ()
  "Source text of `nelisp-nc--table' for the boot image: (NAME ID TEXT [:mutant]).
TEXT is the printed (CALLEES IR ARITY) the install pass compares a record
with; `nelisp-nc--row' reads it when a registration of NAME is checked.
compall2: as one string per row the table is a few hundred leaf objects
for the collector, not the cons trees of every row's fxir (live for the
whole session, they made every full collection slower)."
  (format "(defconst nelisp-nc--table '%S)\n"
          (mapcar (lambda (r)
                    (append (list (nth 0 r) (nth 1 r)
                                  (let ((print-length nil) (print-level nil))
                                    (prin1-to-string (list (nth 3 r) (nth 4 r) (nth 5 r)))))
                            (and (nth 6 r) (list (nth 6 r)))))
                  (nelisp-cc-fxir-native-all-rows))))

;; Stage 1 (doc 231): static call-cost census.  Diagnostic only; nothing on
;; the lowering path calls it.
(defun nelisp-cc-fxir-native-call-census (lowered)
  "Return a plist of static cost components of LOWERED, a (STMTS . VALUE) pair.
STMTS is the top-level statement list of the lowering; keys:
 :stmts      number of top-level statements.
 :fallible   top-level statements whose FALLIBLE flag is non-nil.
 :call-outs  forms, at any depth, whose head is a generic dispatch entry:
             `nc_call', `nc_call0'..`nc_call3' (generic call), `nc_globals'
             (Bset / Bsymbol_value arm) or `nc_d_<ID>' (guarded direct call).
 :guards     `if' forms, at any depth, whose test is not the status check
             `(= st 0)' (fast-path tag tests, the fast-path-declined test
             `(= tN 0)', truthiness tests).
 :temps      distinct temporaries `t<digits>' appearing in STMTS or VALUE.
 :forms-size cons cells of the statement forms (the FORM half of each
             statement, not the flag wrapper) plus the VALUE."
  (let ((stmts (car lowered)) (value (cdr lowered))
        (outs 0) (guards 0) (temps nil) (size 0))
    (cl-labels ((walk (x)
                  (cond
                   ((consp x)
                    (cl-incf size)
                    (when (symbolp (car x))
                      (let ((h (symbol-name (car x))))
                        (when (or (string-match-p "\\`nc_call[0-9]*\\'" h)
                                  (string-match-p "\\`nc_d_[0-9]+\\'" h)
                                  (equal h "nc_dsubr_call")
                                  (equal h "nc_globals"))
                          (cl-incf outs)))
                      (when (and (eq (car x) 'if) (not (equal (cadr x) '(= st 0))))
                        (cl-incf guards)))
                    (walk (car x))
                    (walk (cdr x)))
                   ((and x (symbolp x) (string-match-p "\\`t[0-9]+\\'" (symbol-name x)))
                    (cl-pushnew x temps)))))
      (dolist (s stmts) (walk (cdr s)))
      (walk value))
    (list :stmts (length stmts)
          :fallible (cl-count-if #'car stmts)
          :call-outs outs :guards guards
          :temps (length temps) :forms-size size)))

(provide 'nelisp-cc-fxir-native)
;;; nelisp-cc-fxir-native.el ends here
