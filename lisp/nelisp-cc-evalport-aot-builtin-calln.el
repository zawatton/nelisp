;;; nelisp-cc-evalport-aot-builtin-calln.el --- native calln provider -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-cc-evalport-aot-builtin-call1' provides a native
;; `nelisp_aot_builtin_call1' for the standalone reader.  There was no
;; matching `nelisp_aot_builtin_calln': the symbol appears nowhere in the
;; reader build, so an artifact that lowers any multi-argument call
;; through the Doc 129.6 calln dispatcher has an unresolvable extern and
;; cannot be loaded in-process.
;;
;; That is every artifact compiled with
;; `nelisp-aot-compiler--dynamic-user-calls', which routes
;; otherwise-unresolvable user calls through this dispatcher precisely so
;; the extern set closes over the runtime symbols.  Without this file the
;; set closes onto a symbol the reader does not have.
;;
;; The ABI is
;;
;;   nelisp_aot_builtin_calln(mirror, frames, name, argc, out, scratch, arg...)
;;
;; Six fixed slots fill the SysV GP registers, so every user argument
;; arrives on the stack and the provider has to declare each one to read
;; it.  Declaring parameters past the sixth is supported (checked: a
;; 12-parameter AOT defun compiles), which is what makes this shape
;; possible at all.
;;
;; Like the call1 provider this builds the `(builtin NAME)' sentinel and
;; forwards to `nelisp_apply_function', so dispatch goes through the
;; runtime's own function table rather than any builtin subset.  The only
;; added work is assembling the argument list, which is built right to
;; left through one cons slot per position under an `argc' if-chain --
;; the AOT surface has no loop over a variadic tail.

;;; Code:

(defconst nelisp-cc-evalport-aot-builtin-calln--max-args 8
  "User arguments this provider can read off the stack.
`nelisp-aot-compiler--parse-aot-builtinn-call' refuses to emit a call
with more than this while lowering for dynamic load, so a unit that
compiles is a unit whose calls this provider can service.")

(defconst nelisp-cc-evalport-aot-builtin-calln--builtin-name-word
  31078196194145634
  "The seven bytes of \"builtin\" packed little-endian into one u64.
Same constant the call1 provider writes; kept spelled out rather than
computed because the source form is data consumed by the AOT compiler.")

(defun nelisp-cc-evalport-aot-builtin-calln--arg-syms ()
  "Return the declared user-argument parameter symbols, in ABI order."
  (let ((syms nil))
    (dotimes (i nelisp-cc-evalport-aot-builtin-calln--max-args)
      (push (intern (format "a%d" i)) syms))
    (nreverse syms)))

(defun nelisp-cc-evalport-aot-builtin-calln--nil-writes (slot)
  "Return forms zeroing the 32-byte Sexp at SLOT, i.e. writing nil."
  (list `(ptr-write-u64 ,slot 0 0)
        `(ptr-write-u64 (+ ,slot 8) 0 0)
        `(ptr-write-u64 (+ ,slot 16) 0 0)
        `(ptr-write-u64 (+ ,slot 24) 0 0)))

(defun nelisp-cc-evalport-aot-builtin-calln--build-list (n)
  "Return the form building an N-element argument list into `args_list'.
The list is assembled right to left: the last argument conses onto nil,
each earlier one onto the partial list before it.  Every intermediate
lands in its own slot because `nelisp_cons_construct' writes through an
out-parameter and cannot accumulate in place."
  (if (zerop n)
      (cons 'seq (nelisp-cc-evalport-aot-builtin-calln--nil-writes 'args_list))
    (let ((forms nil))
      ;; Position n-1 conses onto nil; positions n-2 .. 1 onto the slot
      ;; built one step to their right; position 0 lands in `args_list'.
      (dotimes (step n)
        (let* ((idx (- n 1 step))
               (arg (intern (format "a%d" idx)))
               (tail (if (= idx (1- n)) 'nil_slot (intern (format "c%d" (1+ idx)))))
               (dest (if (zerop idx) 'args_list (intern (format "c%d" idx)))))
          (push `(nelisp_cons_construct ,arg ,tail ,dest) forms)))
      (cons 'seq (nreverse forms)))))

(defun nelisp-cc-evalport-aot-builtin-calln--dispatch ()
  "Return the `argc' if-chain selecting an argument-list shape.

Every count from 0 to the maximum gets its own arm.  The final else is
reached only when `argc' is outside that range, which the compiler
refuses to emit, so it builds an EMPTY list rather than the longest one:
an out-of-range `argc' means the parameters past it hold whatever was on
the stack, and consing those produces a list with garbage pointers in
it.  Applying to an empty list gives a wrong answer; applying to garbage
pointers dereferences them."
  (let ((form '(seq (ptr-write-u64 args_list 0 0)
                    (ptr-write-u64 (+ args_list 8) 0 0)
                    (ptr-write-u64 (+ args_list 16) 0 0)
                    (ptr-write-u64 (+ args_list 24) 0 0)))
        (n (1+ nelisp-cc-evalport-aot-builtin-calln--max-args)))
    (while (> n 0)
      (setq n (1- n))
      (setq form `(if (= argc ,n)
                      ,(nelisp-cc-evalport-aot-builtin-calln--build-list n)
                    ,form)))
    form))

(defconst nelisp-cc-evalport-aot-builtin-calln--source
  `(defun nelisp_aot_builtin_calln
       (mirror frames name argc out scratch
               ,@(nelisp-cc-evalport-aot-builtin-calln--arg-syms))
     (let* ((nil_slot (alloc-bytes 32 8))
            (builtin_sym (alloc-bytes 32 8))
            (name_buf (alloc-bytes 8 1))
            (args_list (alloc-bytes 32 8))
            ,@(let ((slots nil))
                ;; One accumulator per interior list position; position 0
                ;; is `args_list' and the tail of the last is `nil_slot'.
                (dotimes (i (1- nelisp-cc-evalport-aot-builtin-calln--max-args))
                  (push `(,(intern (format "c%d" (1+ i))) (alloc-bytes 32 8))
                        slots))
                (nreverse slots))
            (inner (alloc-bytes 32 8))
            (func (alloc-bytes 32 8)))
       (seq
        ,@(nelisp-cc-evalport-aot-builtin-calln--nil-writes 'nil_slot)
        (ptr-write-u64 name_buf 0
                       ,nelisp-cc-evalport-aot-builtin-calln--builtin-name-word)
        (nl_alloc_symbol name_buf 7 builtin_sym)
        ,(nelisp-cc-evalport-aot-builtin-calln--dispatch)
        (nelisp_cons_construct name nil_slot inner)
        (nelisp_cons_construct builtin_sym inner func)
        (nelisp_apply_function func args_list frames out)
        out)))
  "AOT source for the native `nelisp_aot_builtin_calln' provider.")

(provide 'nelisp-cc-evalport-aot-builtin-calln)

;;; nelisp-cc-evalport-aot-builtin-calln.el ends here
