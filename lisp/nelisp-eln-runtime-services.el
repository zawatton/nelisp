;;; nelisp-eln-runtime-services.el --- GNU .eln runtime service implementations  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Genuine GNU-compiled .eln units call a fixed set of C runtime helpers by
;; direct relocated address instead of going through ordinary Lisp dispatch
;; (see `freloc_link_table' in GNU src/comp.c).  This module supplies NeLisp
;; implementations of the subset of those helpers needed to run provenance-
;; pinned GNU 31.1 .eln bytes under the trust model chosen for this effort:
;;
;;   - The .eln bytes are pinned to a specific, hash-verified GNU Emacs 31.1
;;     build (ABI tag "ba35c031").
;;   - Every call the native code makes into the C runtime is redirected,
;;     through the callable-import adapter (`nelisp-eln-callable-import.el',
;;     owned by another lane), to a NeLisp-side implementation with the same
;;     observable semantics as the genuine GNU C function.
;;
;; This module owns two things: the IMPLEMENTATIONS (this file) and a
;; DESCRIPTOR DATA TABLE (`nelisp-eln-runtime-services-descriptors') that
;; records, per service, the authenticated freloc slot it corresponds to,
;; its calling convention, and whether this model can support it at all.
;; Wiring these descriptors into the adapter's dispatch table is explicitly
;; left to a later, separate change; nothing here calls into
;; `nelisp-eln-callable-import.el', the native-subr/tail-code/leaf-code/
;; registration modules, or `scripts/*'.
;;
;; Scope discipline: every implementation below is built only from ordinary,
;; public Elisp operations (`set', `symbol-value', `signal', `push', `pop',
;; `funcall', `apply', standard type predicates, buffer/point/window
;; accessors).  Nothing here reaches into NeLisp's GC allocator, object
;; representation, or `src/nelisp-cc-*' JIT/IR internals; where GNU's real
;; C function depends on such internals (buffer-local variable chains,
;; backtrace frames, symbols-with-position display, consing counters), the
;; simplification is called out explicitly in the relevant docstring.
;;
;; Source of truth for GNU semantics: this machine's copy of the genuine
;; GNU Emacs 31.1 C sources, found at
;; ~/Downloads/emacs-31.1/src/{eval,data,alloc,fns,editfns,comp}.c and
;; src/{lisp,buffer}.h (verified present and readable while writing this
;; module).  Each implementation's docstring cites the specific function
;; and file.  The authenticated freloc address table used to validate the
;; descriptor table against the real ABI is
;; ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (see
;; `nelisp-eln-runtime-services-freloc-tsv-file' and
;; `nelisp-eln-runtime-services-validate-descriptors').
;;
;;; Calling conventions
;;
;; Native .eln code calls these helpers with GNU's own native-comp calling
;; conventions (GNU src/comp.c `declare_runtime_imported_funcs',
;; src/lisp.h `enum maxargs'):
;;
;;   - `fixed': the callee takes exactly N register arguments (Lisp_Object
;;     or plain C int/bool), matching `ADD_IMPORTED' entries in comp.c.
;;   - `many': the callee takes GNU's MANY convention, (ptrdiff_t nargs,
;;     Lisp_Object *args).  Until the adapter is generalized to splat a
;;     native args array, this module's `many'-convention implementations
;;     accept the equivalent Lisp list (FUNCTION . ARGUMENTS) / (NUMBER
;;     . MORE-NUMBERS) as a single argument, which the adapter lane can
;;     build from (nargs, args) with a single `listify' pass.
;;
;;; Per-service status summary (see the descriptor table for detail)
;;
;;   maybe_quit, maybe_gc, specbind, helper_unbind_n, helper_unwind_protect,
;;   record_unwind_protect_excursion, record_unwind_current_buffer,
;;   set_internal, helper_PSEUDOVECTOR_TYPEP_XUNTAG, wrong_type_argument,
;;   slow_eq, Fcons, Fassq, Fsymbol_value, Ffuncall, Fapply, Feqlsign, Fleq,
;;   Fadd1, Fsub1                                              -> supported
;;   push_handler                                               -> unsupported
;;     (native code pairs push_handler with an inline sys_setjmp at the
;;      exact call site; a Lisp-callable implementation cannot supply a
;;      jmp_buf target for that call site -- see
;;      `nelisp-eln-runtime-services-push-handler').

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-eln-runtime-services-error
  "NeLisp eln-runtime-services internal error")
(define-error 'nelisp-eln-runtime-services-unsupported
  "GNU native-comp service has no supported NeLisp implementation"
  'nelisp-eln-runtime-services-error)

(defconst nelisp-eln-runtime-services-abi "ba35c031"
  "ABI tag of the provenance-pinned GNU Emacs 31.1 build this module targets.
Matches the `-ba35c031' suffix of the authenticated freloc table below and
the ABI byte the coordinator pinned for this effort.")

(defconst nelisp-eln-runtime-services--freloc-tsv-sha256
  "3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758"
  "Expected SHA-256 of the authenticated freloc table, as handed down by
the coordinator for ABI `nelisp-eln-runtime-services-abi'.  Any table on
disk that hashes to something else is untrusted and must not be used to
validate the descriptors below.")

(defcustom nelisp-eln-runtime-services-freloc-tsv-file
  (expand-file-name "tmp/slot-auth/freloc-ba35c031.tsv"
                     (or (getenv "XDG_CACHE_HOME")
                         (expand-file-name ".cache" (or (getenv "HOME") "~"))))
  "Path to the authenticated (index, offset, address, symbol) freloc table.
Resolved from $XDG_CACHE_HOME (falling back to ~/.cache) rather than
hard-coded, so this module works on any machine that has the same
coordinator-distributed table under its own cache root."
  :type 'file
  :group 'nelisp-eln)

;;; Shared unwind bookkeeping (specpdl analogue)
;;
;; GNU's real specpdl (src/eval.c specpdl_ptr / SPECPDL_INDEX) is a single
;; process-global stack private to the interpreter core; NeLisp's own
;; bytecode VM keeps an equivalent stack, but it lives per call-frame inside
;; the VM's private state vector (see src/nelisp-bytecode.el, `specpdl'
;; local / (aref vm 8)), which is exactly the GC/object-model-internal
;; territory this module is asked to stay out of.  Native .eln code needs
;; specbind/unbind semantics that span many native calls without any
;; enclosing Lisp `let', so this module keeps its own explicit stack built
;; only from `push'/`pop'/`set'/`symbol-value' -- ordinary public
;; operations.  Because the *value* mutation this stack performs is a plain
;; `set' on the real global symbol cell, its visible effect on the rest of
;; the system is identical to GNU's specbind; only the bookkeeping that
;; remembers how to undo it is private to this module.

(defvar nelisp-eln-runtime-services--specpdl nil
  "Stack of pending unwind entries pushed by this module's specbind /
helper-unwind-protect / record-unwind-protect-* implementations.
Each entry is (:let SYMBOL . OLD-VALUE) or (:unwind-protect . THUNK).
OLD-VALUE may be the sentinel `nelisp-eln-runtime-services--unbound'.")

(defconst nelisp-eln-runtime-services--unbound
  'nelisp-eln-runtime-services--unbound
  "Sentinel recorded in a :let specpdl entry when the symbol was unbound
at bind time, so unbinding can call `makunbound' instead of `set'.")

(defun nelisp-eln-runtime-services-specpdl-depth ()
  "Return the number of pending entries on this module's unwind stack.
Analogue of GNU's `SPECPDL_INDEX' (src/eval.c), scoped to entries pushed
through this module rather than the interpreter's own global specpdl."
  (length nelisp-eln-runtime-services--specpdl))

(defun nelisp-eln-runtime-services--unbind-1 ()
  "Pop and process exactly one unwind entry, GNU `do_one_unbind' style.
The entry is removed from the stack before its cleanup runs, so a
cleanup that signals leaves the remaining entries untouched and the
error propagates to the caller -- matching GNU's `unbind_to' comment in
src/eval.c: \"We decrement first so that an error in unbinding won't try
to unbind the same entry again.\""
  (let ((entry (pop nelisp-eln-runtime-services--specpdl)))
    (unless entry
      (signal 'nelisp-eln-runtime-services-error '(specpdl-underflow)))
    (pcase (car entry)
      (:let (let ((symbol (cadr entry)) (old (cddr entry)))
              (if (eq old nelisp-eln-runtime-services--unbound)
                  (makunbound symbol)
                (set symbol old))))
      (:unwind-protect (funcall (cdr entry)))
      (_ (signal 'nelisp-eln-runtime-services-error
                  (list 'unknown-specpdl-entry entry))))))

;;; maybe_quit / maybe_gc
;;
;; Source: GNU src/lisp.h `maybe_quit' / `maybe_gc' (INLINE, called from
;; nearly every C loop and from `Ffuncall'), src/eval.c `probably_quit' /
;; `process_quit_flag' / `quit'.

(defun nelisp-eln-runtime-services-maybe-quit ()
  "Poll the pending-quit flag and signal `quit' exactly like GNU.
Mirrors src/lisp.h `maybe_quit' -> src/eval.c `probably_quit' ->
`process_quit_flag' -> `quit' (`signal_or_quit (Qquit, Qnil, true)'):
the flag is cleared first (so a later quit does not re-fire the same
request), a pending `kill-emacs' request takes priority over an
ordinary quit, and otherwise a `quit' condition is signalled and does
not return.  `inhibit-quit' suppresses the whole check, matching
GNU's `if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))' guard.
`Vthrow_on_input' is GNU's rare third branch and is intentionally not
reproduced here; it is not part of the requested service list.
Returns nil when there is nothing pending.

Caveat for direct invocation on host GNU Emacs: `Ffuncall' itself polls
the real `quit-flag' as its first action (src/eval.c), so calling this
function while the genuine global `quit-flag' already holds the symbol
`kill-emacs' lets the host's own safepoint invoke the real `kill-emacs'
before this function's body runs at all.  The branch below is written
for fidelity with `process_quit_flag' and for runtimes whose call path
does not itself poll first; it is not exercised by this module's own
ERT suite for exactly that reason (see the test file's commentary)."
  (when (and (bound-and-true-p quit-flag)
             (not (bound-and-true-p inhibit-quit)))
    (let ((flag quit-flag))
      (setq quit-flag nil)
      (if (eq flag 'kill-emacs)
          (kill-emacs nil)
        (signal 'quit nil)))))

(defun nelisp-eln-runtime-services-maybe-gc ()
  "GC safepoint hook, or a documented no-op if NeLisp has none.
GNU's `maybe_gc' (src/lisp.h) collects only when a private consing
counter (`consing_until_gc') has gone negative; that counter is GC/
allocator-internal state this module is not permitted to reach into.
This implementation calls a NeLisp-provided safepoint hook named
`nelisp-runtime-gc-safepoint' if and only if that public function
exists (`fboundp'); as of this writing no such hook is exposed, so on
both host GNU Emacs and current NeLisp this is a documented no-op.
Wiring a real conditional safepoint, if NeLisp later adds a public
counter-based trigger, only requires defining that one function."
  (when (fboundp 'nelisp-runtime-gc-safepoint)
    (funcall 'nelisp-runtime-gc-safepoint))
  nil)

;;; specbind / unbind-n / record-unwind-protect family
;;
;; Source: GNU src/eval.c `specbind' (SYMBOL_PLAINVAL fast path),
;; `record_unwind_protect', `unbind_to'; src/comp.c `helper_unwind_protect'
;; / `helper_unbind_n' (the native-callable wrappers actually present in
;; the freloc table); src/eval.c `record_unwind_protect_excursion' +
;; src/editfns.c `save_excursion_save'/`save_excursion_restore';
;; src/buffer.h `record_unwind_current_buffer' + `set_buffer_if_live'.

(defun nelisp-eln-runtime-services-specbind (symbol value)
  "Dynamically bind SYMBOL to VALUE, recording how to restore it.
Mirrors GNU `specbind' (src/eval.c) for the common global-symbol case:
push the symbol's current value (or the unbound sentinel) onto this
module's unwind stack, then set the new value.  Buffer-local /
forwarded-variable redirection (GNU's SYMBOL_LOCALIZED / SYMBOL_FORWARDED
branches) is intentionally not replicated; those branches consult the
buffer-local value chain, which is object-model territory out of this
module's scope. Returns nil, like the C function."
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (push (cons :let (cons symbol (if (boundp symbol)
                                     (symbol-value symbol)
                                   nelisp-eln-runtime-services--unbound)))
        nelisp-eln-runtime-services--specpdl)
  (set symbol value)
  nil)

(defun nelisp-eln-runtime-services-helper-unbind-n (n)
  "Unwind exactly N entries pushed by this module, running unwind-protect
cleanups and restoring `let'-style bindings in LIFO order.
Mirrors src/comp.c `helper_unbind_n', which native code calls as
`unbind_to (SPECPDL_INDEX () - n, Qnil)': quit-flag is suppressed for
the duration exactly like `unbind_to' (src/eval.c) and restored
afterward only if nothing new set it meanwhile.  Always returns nil."
  (unless (and (integerp n) (>= n 0))
    (signal 'wrong-type-argument (list 'natnump n)))
  (let ((saved-quit (bound-and-true-p quit-flag)))
    (when (boundp 'quit-flag) (setq quit-flag nil))
    (dotimes (_ n) (nelisp-eln-runtime-services--unbind-1))
    (when (and (boundp 'quit-flag) (null quit-flag) saved-quit)
      (setq quit-flag saved-quit)))
  nil)

(defun nelisp-eln-runtime-services-helper-unwind-protect (handler)
  "Register HANDLER as an unwind-protect cleanup, GNU `helper_unwind_protect'
style (src/comp.c): \"Support for a function here is new in 24.4\" --
when HANDLER is a function it is called with no arguments during
unbind; a non-function HANDLER (GNU's legacy inline-progn case) is a
no-op here, matching `prog_ignore'."
  (push (cons :unwind-protect
              (if (functionp handler)
                  (lambda () (funcall handler))
                (lambda () nil)))
        nelisp-eln-runtime-services--specpdl)
  nil)

(defun nelisp-eln-runtime-services-record-unwind-protect-excursion ()
  "Push a cleanup that restores buffer, point, and window-point exactly
like `save-excursion', mirroring GNU src/eval.c
`record_unwind_protect_excursion' + src/editfns.c
`save_excursion_save'/`save_excursion_restore': capture a point marker
and the selected window now; on unbind, switch back to the marker's
buffer (doing nothing if that buffer was killed), go to the marker,
release it, and fix up the previously selected window's point if a
different window was selected in between and still shows this buffer."
  (let ((marker (point-marker))
        (window (selected-window)))
    (push (cons :unwind-protect
                (lambda ()
                  (let ((buffer (marker-buffer marker)))
                    (when buffer
                      (set-buffer buffer)
                      (goto-char marker)
                      (set-marker marker nil)
                      (when (and (window-live-p window)
                                 (not (eq window (selected-window)))
                                 (eq (window-buffer window) (current-buffer)))
                        (set-window-point window (point)))))))
          nelisp-eln-runtime-services--specpdl))
  nil)

(defun nelisp-eln-runtime-services-record-unwind-current-buffer ()
  "Push a cleanup that restores the current buffer, mirroring GNU
src/buffer.h `record_unwind_current_buffer' (`record_unwind_protect
(set_buffer_if_live, Fcurrent_buffer ())'): capture the current buffer
now, and on unbind switch back to it only if it is still live."
  (let ((buffer (current-buffer)))
    (push (cons :unwind-protect
                (lambda ()
                  (when (buffer-live-p buffer) (set-buffer buffer))))
          nelisp-eln-runtime-services--specpdl))
  nil)

;;; push_handler -- unsupported under this trust model

(defun nelisp-eln-runtime-services-push-handler (_tag-ch-val _handlertype)
  "Fail-closed stub for GNU `push_handler' (src/eval.c).
Genuine GNU native code pairs every `push_handler' call with an inline
`sys_setjmp' on the returned `struct handler''s jmp_buf, emitted at the
exact native call site (src/comp.c `emit_limple_push_handler',
`emit_setjmp'): `struct handler *c = push_handler (tag, type); if
(setjmp (c->jmp)) goto handler_bb; else goto guarded_bb;'.  A Lisp
function invoked through the callable-import adapter runs in its own
C frame and returns to native code before that native code would ever
execute its `setjmp'; there is no jmp_buf for this implementation to
populate, and NeLisp's own `condition-case'/`catch' cannot be handed a
foreign native return address to jump back to.  Supporting this would
require either the excluded `src/nelisp-cc-*' JIT emitting a real
inline setjmp at each native call site, or the excluded shared native
ABI exposing genuine per-call jmp_bufs -- both out of this module's
scope.  This always signals `nelisp-eln-runtime-services-unsupported'
rather than silently doing nothing, so a caller that does not check
the descriptor's :status first fails loudly instead of continuing
past a handler that will never fire."
  (signal 'nelisp-eln-runtime-services-unsupported
          (list 'push-handler
                "push_handler requires an inline sys_setjmp at the native call site; not representable as a callable Lisp implementation under this adapter model")))

;;; helper_PSEUDOVECTOR_TYPEP_XUNTAG
;;
;; Source: GNU src/comp.c `helper_PSEUDOVECTOR_TYPEP_XUNTAG' + src/lisp.h
;; `enum pvec_type' (ordinal values below reproduced from that enum,
;; Emacs 31.1).

(defconst nelisp-eln-runtime-services--pvec-predicates
  '((0 . vectorp)
    (2 . bignump)
    (3 . markerp)
    (4 . overlayp)
    (6 . symbol-with-pos-p)
    (9 . processp)
    (10 . framep)
    (11 . windowp)
    (12 . bool-vector-p)
    (13 . bufferp)
    (14 . hash-table-p)
    (15 . obarrayp)
    (17 . window-configuration-p)
    (18 . subrp)
    (31 . closurep)
    (32 . char-table-p)
    (34 . recordp))
  "GNU `enum pvec_type' ordinal (src/lisp.h, Emacs 31.1) to the NeLisp/
Emacs public type predicate that reproduces `PSEUDOVECTOR_TYPEP' for
that code.  Deliberately omitted: PVEC_FREE (1, never a live object),
PVEC_FINALIZER (5, no public predicate), PVEC_MISC_PTR/PVEC_USER_PTR
(7-8, opaque), PVEC_TERMINAL (16, only `terminal-live-p' exists, which
also checks liveness), PVEC_OTHER (19, \"should never be visible to
Elisp code\"), PVEC_XWIDGET/XWIDGET_VIEW (20-21, optional build
feature), PVEC_THREAD/MUTEX/CONDVAR (22-24), PVEC_MODULE_FUNCTION (25),
PVEC_NATIVE_COMP_UNIT (26), PVEC_TS_* (27-29, optional build feature),
PVEC_SQLITE (30, optional build feature), PVEC_SUB_CHAR_TABLE (33, not
normally visible to Lisp), PVEC_FONT (35, splits across several
predicates with no single 1:1 mapping).  Calling this service with any
omitted or out-of-range code signals, matching the requested
fail-closed behaviour for unknown codes.")

(defun nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag (obj code)
  "Return non-nil if OBJ is a pseudovector of GNU pvec_type ordinal CODE.
Mirrors GNU `helper_PSEUDOVECTOR_TYPEP_XUNTAG' (src/comp.c): the real
C function assumes the caller already confirmed OBJ is vectorlike and
then untags + checks the type field unconditionally, which is undefined
behaviour on a non-vectorlike OBJ; this implementation instead calls
the public predicate for CODE (see
`nelisp-eln-runtime-services--pvec-predicates'), which is safe for any
OBJ and agrees with the C function whenever its precondition holds.
Signals `nelisp-eln-runtime-services-error' for a CODE this module does
not map, rather than guessing."
  (let ((predicate (cdr (assq code nelisp-eln-runtime-services--pvec-predicates))))
    (unless predicate
      (signal 'nelisp-eln-runtime-services-error
              (list 'unknown-pvec-type-code code)))
    (and (funcall predicate obj) t)))

;;; wrong_type_argument / set_internal / slow_eq

(defun nelisp-eln-runtime-services-wrong-type-argument (predicate value)
  "Signal `wrong-type-argument' for PREDICATE and VALUE; never returns.
Mirrors GNU `wrong_type_argument' (src/data.c): `xsignal2
(Qwrong_type_argument, predicate, value)'."
  (signal 'wrong-type-argument (list predicate value)))

(defun nelisp-eln-runtime-services-set-internal (symbol newval where bindflag)
  "Set SYMBOL's dynamic value to NEWVAL, GNU `set_internal' style.
Mirrors GNU `set_internal' (src/data.c) for the parts reachable through
public Elisp: rejects a non-symbol SYMBOL exactly like `CHECK_SYMBOL';
rejects overwriting `nil', `t', or a keyword with a different value
with `setting-constant', exactly like the `SYMBOL_NOWRITE' branch
(setting a keyword to its own value is allowed, matching GNU's
comment \"Allow setting keywords to their own value\"); when BINDFLAG
is 1 (GNU's `SET_INTERNAL_BIND') this also records the old value on
this module's unwind stack via `nelisp-eln-runtime-services-specbind',
so a later `helper_unbind_n' restores it -- this is GNU's real
distinction between a plain `set_internal' write (BINDFLAG 0/2/3,
`SET_INTERNAL_SET'/`UNBIND'/`THREAD_SWITCH') and a `let'-style bind.
A non-nil WHERE selects the buffer to bind/set in via
`with-current-buffer', approximating GNU's buffer-local value-cell
chain (`Lisp_Buffer_Local_Value' lookup) rather than replicating it
exactly; variable-watcher notification
(`notify_variable_watchers'/`add-variable-watcher') is not reproduced.
Both simplifications touch buffer-local/object-model internals this
module is scoped to avoid.  Returns nil, like the C function (void)."
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (when (and (or (null symbol) (eq symbol t) (keywordp symbol))
             (not (and (boundp symbol) (eq newval (symbol-value symbol)))))
    (signal 'setting-constant (list symbol)))
  (cond
   ((eql bindflag 1)
    (if where
        (with-current-buffer where
          (nelisp-eln-runtime-services-specbind symbol newval))
      (nelisp-eln-runtime-services-specbind symbol newval)))
   (where (with-current-buffer where (set symbol newval)))
   (t (set symbol newval)))
  nil)

(defun nelisp-eln-runtime-services-slow-eq (x y)
  "Return non-nil if X and Y are `eq', GNU `slow_eq' style.
Mirrors GNU `slow_eq' (src/data.c): when `symbols-with-pos-enabled' is
non-nil, a symbol-with-position operand is compared by its underlying
bare symbol instead of by its wrapper identity."
  (eq (if (and (bound-and-true-p symbols-with-pos-enabled) (symbol-with-pos-p x))
          (bare-symbol x)
        x)
      (if (and (bound-and-true-p symbols-with-pos-enabled) (symbol-with-pos-p y))
          (bare-symbol y)
        y)))

;;; Lisp primitive redirects (fixed and MANY convention)
;;
;; These primitives already exist, Emacs-compatibly, as NeLisp built-ins;
;; the "NeLisp implementation" of the GNU C subr a native .eln unit calls
;; directly is simply that built-in, called the same way the byte-code
;; interpreter and `funcall' already call it.  Source: GNU src/alloc.c
;; `Fcons', src/fns.c `Fassq', src/data.c `Fsymbol_value'/`Feqlsign'/
;; `Fleq'/`Fadd1'/`Fsub1', src/eval.c `Ffuncall'/`Fapply'.

(defun nelisp-eln-runtime-services-fcons (car cdr)
  "NeLisp equivalent of GNU `Fcons' (src/alloc.c): build and return a
fresh cons of CAR and CDR."
  (cons car cdr))

(defun nelisp-eln-runtime-services-fassq (key alist)
  "NeLisp equivalent of GNU `Fassq' (src/fns.c): the first element of
ALIST whose car is `eq' to KEY, or nil; signals on a malformed
(non-nil, non-cons) list tail, like `CHECK_LIST_END'."
  (assq key alist))

(defun nelisp-eln-runtime-services-fmemq (elt list)
  "NeLisp equivalent of GNU `Fmemq' (src/fns.c): the tail of LIST whose
car is `eq' to ELT, or nil; signals on a malformed (non-nil, non-cons)
list tail, like `CHECK_LIST_END'."
  (memq elt list))

(defun nelisp-eln-runtime-services-fnreverse (seq)
  "NeLisp equivalent of GNU `Fnreverse' (src/fns.c): reverse SEQ
destructively and return the result."
  (nreverse seq))

(defun nelisp-eln-runtime-services-flength (sequence)
  "NeLisp equivalent of GNU `Flength' (src/fns.c): the number of elements
of SEQUENCE; signals `wrong-type-argument' for a dotted list or a
non-sequence, and `circular-list' for a circular list, like GNU."
  (length sequence))

(defun nelisp-eln-runtime-services-fnth (n list)
  "NeLisp equivalent of GNU `Fnth' (src/fns.c): the Nth element of LIST,
i.e. (car (nthcdr N LIST)); N must be an integer and LIST a list."
  (nth n list))

(defun nelisp-eln-runtime-services-fstringp (object)
  "NeLisp equivalent of GNU `Fstringp' (src/data.c): t if OBJECT is a
string, else nil."
  (stringp object))

(defun nelisp-eln-runtime-services-fcar-safe (object)
  "NeLisp equivalent of GNU `Fcar_safe' (src/data.c): the car of OBJECT
if it is a cons cell, else nil; never signals."
  (car-safe object))

(defun nelisp-eln-runtime-services-fmember (elt list)
  "NeLisp equivalent of GNU `Fmember' (src/fns.c): the tail of LIST whose
car is `equal' to ELT, or nil."
  (member elt list))

(defun nelisp-eln-runtime-services-fgethash (key table dflt)
  "NeLisp equivalent of GNU `Fgethash' (src/fns.c): the value of KEY in
hash TABLE, or DFLT when absent."
  (gethash key table dflt))

(defun nelisp-eln-runtime-services-faset (array idx newelt)
  "NeLisp equivalent of GNU `Faset' (src/data.c): store NEWELT at IDX of
ARRAY and return NEWELT."
  (aset array idx newelt))

(defun nelisp-eln-runtime-services-ftype-of (object)
  "NeLisp equivalent of GNU `Ftype_of' (src/data.c): the type symbol of OBJECT."
  (type-of object))

(defun nelisp-eln-runtime-services-fsignal (error-symbol data)
  "NeLisp equivalent of GNU `Fsignal' (src/eval.c): signal ERROR-SYMBOL with
DATA; never returns."
  (signal error-symbol data))

(defun nelisp-eln-runtime-services-fmake-closure (&rest args)
  "NeLisp equivalent of GNU `Fmake_closure' (src/alloc.c, 1 MANY).
ARGS is (PROTOTYPE . CLOSURE-VARS), spread (unlike this module's other
`many' services, which take one list): the admitted S6 dispatcher
applies a MANY service to its decoded arguments.  Returns a copy of the
byte-code function PROTOTYPE whose leading constants are CLOSURE-VARS,
via NeLisp's own `make-closure'.  Fewer than one argument, or a
PROTOTYPE that is not a byte-code function, signals like GNU."
  (unless args
    (signal 'wrong-number-of-arguments
            (list 'nelisp-eln-runtime-services-fmake-closure 0)))
  (unless (byte-code-function-p (car args))
    (signal 'wrong-type-argument (list 'byte-code-function-p (car args))))
  (apply #'make-closure args))

(defun nelisp-eln-runtime-services-fnconc (&rest args)
  "NeLisp equivalent of GNU `Fnconc' (src/fns.c, 0 MANY).
ARGS are the lists to concatenate destructively, spread like
`nelisp-eln-runtime-services-fmake-closure' (the admitted S6 dispatcher
applies a MANY service to its decoded arguments); the last one is
shared, not copied, and may be any object."
  (apply #'nconc args))

(defun nelisp-eln-runtime-services-fsymbolp (object)
  "NeLisp equivalent of GNU `Fsymbolp' (src/data.c): t if OBJECT is a
symbol (including nil and t), else nil."
  (symbolp object))

(defun nelisp-eln-runtime-services-ffboundp (symbol)
  "NeLisp equivalent of GNU `Ffboundp' (src/data.c): t if SYMBOL's
function cell is non-nil; signals `wrong-type-argument' `symbolp' for a
non-symbol, like GNU's CHECK_SYMBOL."
  (fboundp symbol))

(defun nelisp-eln-runtime-services-fsymbol-function (symbol)
  "NeLisp equivalent of GNU `Fsymbol_function' (src/data.c): SYMBOL's
function cell (nil when unbound); signals `wrong-type-argument' `symbolp'
for a non-symbol, like GNU's CHECK_SYMBOL."
  (symbol-function symbol))

(defun nelisp-eln-runtime-services-fautoload-do-load
    (fundef funname macro-only)
  "NeLisp equivalent of GNU `Fautoload_do_load' (src/eval.c): when FUNDEF
is an autoload object, load its file and return FUNNAME's new definition
\(for MACRO-ONLY non-nil, only a macro autoload is loaded, and `macro'
restricts it to those); any other FUNDEF is returned unchanged."
  (autoload-do-load fundef funname macro-only))

(defun nelisp-eln-runtime-services-fsymbol-value (symbol)
  "NeLisp equivalent of GNU `Fsymbol_value' (src/data.c): SYMBOL's
dynamic value, or a `void-variable' signal if it has none."
  (symbol-value symbol))

(defun nelisp-eln-runtime-services--coerce-number-or-marker (x)
  "Mirror GNU `check_number_coerce_marker' (src/data.c): a marker
argument is replaced by its integer position; anything else that is
not already a number signals `wrong-type-argument'."
  (cond ((markerp x) (or (marker-position x)
                          (signal 'error (list "marker points nowhere" x))))
        ((numberp x) x)
        (t (signal 'wrong-type-argument (list 'number-or-marker-p x)))))

(defun nelisp-eln-runtime-services-fadd1 (number)
  "NeLisp equivalent of GNU `Fadd1' (src/data.c, doc \"1+\"): NUMBER plus
one; a marker argument is coerced to its integer position first."
  (1+ (nelisp-eln-runtime-services--coerce-number-or-marker number)))

(defun nelisp-eln-runtime-services-fsub1 (number)
  "NeLisp equivalent of GNU `Fsub1' (src/data.c, doc \"1-\"): NUMBER minus
one; a marker argument is coerced to its integer position first."
  (1- (nelisp-eln-runtime-services--coerce-number-or-marker number)))

(defun nelisp-eln-runtime-services-ffuncall (args)
  "NeLisp equivalent of GNU `Ffuncall' (src/eval.c), MANY convention.
ARGS is (FUNCTION . ARGUMENTS), the Lisp-list form the adapter builds
from the native (nargs, args) pair.  Runs `maybe-quit' and `maybe-gc'
at entry, exactly like GNU's `Ffuncall' does before dispatching, then
calls FUNCTION on ARGUMENTS.  GNU's own excessive-recursion counter
(`lisp_eval_depth'/`max_lisp_eval_depth') and backtrace-frame recording
(`record_in_backtrace') are call-stack/debugger internals this
implementation does not reproduce; ordinary `max-lisp-eval-depth'
recursion errors from the underlying `funcall' still apply."
  (nelisp-eln-runtime-services-maybe-quit)
  (nelisp-eln-runtime-services-maybe-gc)
  (apply #'funcall args))

(defun nelisp-eln-runtime-services-fapply (args)
  "NeLisp equivalent of GNU `Fapply' (src/eval.c), MANY convention.
ARGS is (FUNCTION ARG... . FINAL-LIST), matching `apply's own calling
shape, which is what the adapter's (nargs, args) pair already listifies
to for `apply' unlike the funcall-specific reshaping `Fapply' itself
performs internally; the observable result (call FUNCTION with the
leading ARGs followed by the elements of FINAL-LIST) is identical."
  (apply #'apply args))

(defun nelisp-eln-runtime-services-feqlsign (args)
  "NeLisp equivalent of GNU `Feqlsign' (src/data.c, doc \"=\"), MANY
convention: ARGS is the list of numbers/markers to compare pairwise
for numeric equality."
  (apply #'= args))

(defun nelisp-eln-runtime-services-fleq (args)
  "NeLisp equivalent of GNU `Fleq' (src/data.c, doc \"<=\"), MANY
convention: ARGS is the list of numbers/markers to compare pairwise
with `<='."
  (apply #'<= args))

;;; Descriptor data table

(defmacro nelisp-eln-runtime-services--descriptor (&rest plist)
  "Build one descriptor plist, always tagging it with the pinned ABI."
  `(list :abi nelisp-eln-runtime-services-abi ,@plist))

(defconst nelisp-eln-runtime-services-descriptors
  (list
   (nelisp-eln-runtime-services--descriptor
    :index 14 :symbol "maybe_quit" :convention 'fixed :arity 0
    :noreturn nil :implementation #'nelisp-eln-runtime-services-maybe-quit
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 14; GNU src/lisp.h maybe_quit + src/eval.c probably_quit/process_quit_flag/quit")
   (nelisp-eln-runtime-services--descriptor
    :index 13 :symbol "maybe_gc" :convention 'fixed :arity 0
    :noreturn nil :implementation #'nelisp-eln-runtime-services-maybe-gc
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 13; GNU src/lisp.h maybe_gc (documented no-op: no public NeLisp safepoint hook exists yet)")
   (nelisp-eln-runtime-services--descriptor
    :index 12 :symbol "specbind" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-specbind
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 12; GNU src/eval.c specbind (SYMBOL_PLAINVAL fast path)")
   (nelisp-eln-runtime-services--descriptor
    :index 4 :symbol "helper_unbind_n" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-helper-unbind-n
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 4; GNU src/comp.c helper_unbind_n -> src/eval.c unbind_to")
   (nelisp-eln-runtime-services--descriptor
    :index 11 :symbol "helper_unwind_protect" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-helper-unwind-protect
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 11; GNU src/comp.c helper_unwind_protect")
   (nelisp-eln-runtime-services--descriptor
    :index 3 :symbol "record_unwind_protect_excursion" :convention 'fixed :arity 0
    :noreturn nil :implementation #'nelisp-eln-runtime-services-record-unwind-protect-excursion
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 3; GNU src/eval.c record_unwind_protect_excursion + src/editfns.c save_excursion_save/restore")
   (nelisp-eln-runtime-services--descriptor
    :index 9 :symbol "record_unwind_current_buffer" :convention 'fixed :arity 0
    :noreturn nil :implementation #'nelisp-eln-runtime-services-record-unwind-current-buffer
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 9; GNU src/buffer.h record_unwind_current_buffer + set_buffer_if_live")
   (nelisp-eln-runtime-services--descriptor
    :index 2 :symbol "push_handler" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-push-handler
    :status 'unsupported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 2; GNU src/eval.c push_handler + src/comp.c emit_limple_push_handler (inline sys_setjmp at call site, not representable as a Lisp callable)")
   (nelisp-eln-runtime-services--descriptor
    :index 1 :symbol "helper_PSEUDOVECTOR_TYPEP_XUNTAG" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-helper-pseudovector-typep-xuntag
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1; GNU src/comp.c helper_PSEUDOVECTOR_TYPEP_XUNTAG + src/lisp.h enum pvec_type")
   (nelisp-eln-runtime-services--descriptor
    :index 0 :symbol "wrong_type_argument" :convention 'fixed :arity 2
    :noreturn t :implementation #'nelisp-eln-runtime-services-wrong-type-argument
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 0; GNU src/data.c wrong_type_argument")
   (nelisp-eln-runtime-services--descriptor
    :index 10 :symbol "set_internal" :convention 'fixed :arity 4
    :noreturn nil :implementation #'nelisp-eln-runtime-services-set-internal
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 10; GNU src/data.c set_internal (buffer-local chain and variable watchers simplified, see docstring)")
   (nelisp-eln-runtime-services--descriptor
    :index 7 :symbol "slow_eq" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-slow-eq
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 7; GNU src/data.c slow_eq")
   (nelisp-eln-runtime-services--descriptor
    :index 1119 :symbol "Fcons" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fcons
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1119; GNU src/alloc.c Fcons")
   (nelisp-eln-runtime-services--descriptor
    :index 1215 :symbol "Fassq" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fassq
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1215; GNU src/fns.c Fassq")
   (nelisp-eln-runtime-services--descriptor
    :index 1217 :symbol "Fmemq" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fmemq
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1217; GNU src/fns.c Fmemq")
   (nelisp-eln-runtime-services--descriptor
    :index 1209 :symbol "Fnreverse" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fnreverse
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1209; GNU src/fns.c Fnreverse")
   (nelisp-eln-runtime-services--descriptor
    :index 1376 :symbol "Fstringp" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fstringp
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1376; GNU src/data.c Fstringp")
   (nelisp-eln-runtime-services--descriptor
    :index 1354 :symbol "Fcar_safe" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fcar-safe
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1354; GNU src/data.c Fcar_safe")
   (nelisp-eln-runtime-services--descriptor
    :index 1378 :symbol "Fsymbolp" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fsymbolp
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1378; GNU src/data.c Fsymbolp")
   (nelisp-eln-runtime-services--descriptor
    :index 1339 :symbol "Ffboundp" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-ffboundp
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1339; GNU src/data.c Ffboundp")
   (nelisp-eln-runtime-services--descriptor
    :index 1350 :symbol "Fsymbol_function" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fsymbol-function
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1350; GNU src/data.c Fsymbol_function")
   (nelisp-eln-runtime-services--descriptor
    :index 948 :symbol "Fautoload_do_load" :convention 'fixed :arity 3
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fautoload-do-load
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 948; GNU src/eval.c Fautoload_do_load")
   (nelisp-eln-runtime-services--descriptor
    :index 1335 :symbol "Fsymbol_value" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fsymbol-value
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1335; GNU src/data.c Fsymbol_value")
   (nelisp-eln-runtime-services--descriptor
    :index 945 :symbol "Ffuncall" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-ffuncall
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 945; GNU src/eval.c Ffuncall (1, MANY)")
   (nelisp-eln-runtime-services--descriptor
    :index 946 :symbol "Fapply" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fapply
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 946; GNU src/eval.c Fapply (1, MANY)")
   (nelisp-eln-runtime-services--descriptor
    :index 1320 :symbol "Feqlsign" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-feqlsign
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1320; GNU src/data.c Feqlsign (1, MANY)")
   (nelisp-eln-runtime-services--descriptor
    :index 1317 :symbol "Fleq" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fleq
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1317; GNU src/data.c Fleq (1, MANY)")
   (nelisp-eln-runtime-services--descriptor
    :index 1301 :symbol "Fadd1" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fadd1
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1301; GNU src/data.c Fadd1")
   (nelisp-eln-runtime-services--descriptor
    :index 1300 :symbol "Fsub1" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fsub1
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1300; GNU src/data.c Fsub1")
   (nelisp-eln-runtime-services--descriptor
    :index 1113 :symbol "Fmake_closure" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fmake-closure
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1113; GNU src/alloc.c Fmake_closure (1, MANY); spread arguments")
   (nelisp-eln-runtime-services--descriptor
    :index 1250 :symbol "Flength" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-flength
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1250; GNU src/fns.c Flength")
   (nelisp-eln-runtime-services--descriptor
    :index 1220 :symbol "Fnth" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fnth
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1220; GNU src/fns.c Fnth")
   (nelisp-eln-runtime-services--descriptor
    :index 1196 :symbol "Fnconc" :convention 'many :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fnconc
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1196; GNU src/fns.c Fnconc (0, MANY)")
   (nelisp-eln-runtime-services--descriptor
    :index 1218 :symbol "Fmember" :convention 'fixed :arity 2
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fmember
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1218; GNU src/fns.c Fmember")
   (nelisp-eln-runtime-services--descriptor
    :index 1263 :symbol "Fgethash" :convention 'fixed :arity 3
    :noreturn nil :implementation #'nelisp-eln-runtime-services-fgethash
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1263; GNU src/fns.c Fgethash")
   (nelisp-eln-runtime-services--descriptor
    :index 1323 :symbol "Faset" :convention 'fixed :arity 3
    :noreturn nil :implementation #'nelisp-eln-runtime-services-faset
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1323; GNU src/data.c Faset")
   (nelisp-eln-runtime-services--descriptor
    :index 1392 :symbol "Ftype_of" :convention 'fixed :arity 1
    :noreturn nil :implementation #'nelisp-eln-runtime-services-ftype-of
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 1392; GNU src/data.c Ftype_of")
   (nelisp-eln-runtime-services--descriptor
    :index 951 :symbol "Fsignal" :convention 'fixed :arity 2
    :noreturn t :implementation #'nelisp-eln-runtime-services-fsignal
    :status 'supported
    :evidence "freloc tsv sha 3e8591ab..f0758, index 951; GNU src/eval.c Fsignal"))
  "One plist per supported/unsupported GNU .eln runtime service.
See the Commentary above for the plist shape.  Cross-check against the
authenticated freloc table with
`nelisp-eln-runtime-services-validate-descriptors'.")

;;; Descriptor <-> freloc table cross-validation

(defun nelisp-eln-runtime-services--freloc-symbol-at (rows index)
  "Return the bare C symbol name at INDEX in parsed freloc ROWS, or nil."
  (let ((row (cl-find index rows :key #'car)))
    (and row
         (let ((text (nth 3 row)))
           (if (string-match "\\`\\([^ \t]+\\)" text)
               (match-string 1 text)
             text)))))

(defun nelisp-eln-runtime-services--parse-freloc-tsv (file)
  "Parse FILE (tab-separated index/offset/address/symbol, header row
first) into a list of (INDEX OFFSET ADDRESS SYMBOL-TEXT), INDEX as a
number."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((rows nil) (first t))
      (dolist (line (split-string (buffer-string) "\n" t))
        (if first
            (setq first nil)
          (let ((fields (split-string line "\t")))
            (when (>= (length fields) 4)
              (push (list (string-to-number (nth 0 fields))
                          (nth 1 fields) (nth 2 fields) (nth 3 fields))
                    rows)))))
      (nreverse rows))))

(defun nelisp-eln-runtime-services-freloc-tsv-sha256 (&optional file)
  "Return the SHA-256 hex digest of FILE (default
`nelisp-eln-runtime-services-freloc-tsv-file'), computed with the
public `secure-hash' function."
  (let ((file (or file nelisp-eln-runtime-services-freloc-tsv-file)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (buffer-string)))))

(defun nelisp-eln-runtime-services-validate-descriptors (&optional file)
  "Cross-check `nelisp-eln-runtime-services-descriptors' against the
authenticated freloc table at FILE (default
`nelisp-eln-runtime-services-freloc-tsv-file').
Returns nil if every descriptor's :abi, :index, and :symbol agree with
the table and the table's own SHA-256 matches
`nelisp-eln-runtime-services--freloc-tsv-sha256'; otherwise returns a
list of problem plists, one per mismatch, each shaped
(:reason SYMBOL :index N ...).  Never signals for a data mismatch (the
caller decides what a validation failure means); signals a file error
only if FILE itself cannot be read."
  (let* ((file (or file nelisp-eln-runtime-services-freloc-tsv-file))
         (actual-sha (nelisp-eln-runtime-services-freloc-tsv-sha256 file))
         (problems nil))
    (if (not (string-equal actual-sha
                            nelisp-eln-runtime-services--freloc-tsv-sha256))
        (push (list :reason 'tsv-sha256-mismatch
                    :expected nelisp-eln-runtime-services--freloc-tsv-sha256
                    :actual actual-sha)
              problems)
      (let ((rows (nelisp-eln-runtime-services--parse-freloc-tsv file)))
        (dolist (descriptor nelisp-eln-runtime-services-descriptors)
          (let* ((abi (plist-get descriptor :abi))
                 (index (plist-get descriptor :index))
                 (symbol (plist-get descriptor :symbol))
                 (found (nelisp-eln-runtime-services--freloc-symbol-at
                         rows index)))
            (cond
             ((not (string-equal abi nelisp-eln-runtime-services-abi))
              (push (list :reason 'abi-mismatch :index index :symbol symbol
                          :abi abi)
                    problems))
             ((null found)
              (push (list :reason 'index-not-in-tsv :index index
                          :symbol symbol)
                    problems))
             ((not (string-equal found symbol))
              (push (list :reason 'symbol-mismatch :index index
                          :expected symbol :found found)
                    problems)))))))
    (nreverse problems)))

(provide 'nelisp-eln-runtime-services)

;;; nelisp-eln-runtime-services.el ends here
