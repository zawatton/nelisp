;;; nelisp-native-load-driver.el --- in-reader check of the .neln loader  -*- lexical-binding: t; -*-

;;; Commentary:

;; The loader in `lisp/nelisp-native-load.el' only runs where mmap,
;; `ptr-call' and the runtime symbols exist, which is the standalone
;; reader and not host Emacs.  The ERT suite covers its pure parts --
;; trampoline encoding, artifact parsing, the pre-flight check -- and
;; this covers the part that has to actually execute.
;;
;; Run by `make neln-loader-test', which compiles the artifacts named
;; below with host Emacs first.  Exits non-zero on the first wrong
;; answer, so the make target fails rather than printing into the void.

;;; Code:

;; `nelisp-native-load-driver-dir' and the loader itself are supplied by a
;; generated prelude the make target writes, rather than read from the
;; environment: the reader has no `getenv', so an env-var version failed
;; every case with `void-function' and looked like a loader bug.

(defvar nelisp-native-load-driver-dir nil
  "Directory holding the compiled fixtures; set by the generated prelude.")

(defvar nelisp-native-load-driver--dir nelisp-native-load-driver-dir)
(defvar nelisp-native-load-driver--failures 0)

(defun nelisp-native-load-driver--case (name args want)
  "Load NAME, call it with ARGS and compare against WANT."
  (let* ((path (concat nelisp-native-load-driver--dir "/" name ".neln"))
         (got (condition-case e
                  (nelisp-native-load-exec path name args)
                (error (list 'error e))))
         (ok (equal got want)))
    (unless ok
      (setq nelisp-native-load-driver--failures
            (1+ nelisp-native-load-driver--failures)))
    (princ (format "%-10s %-14S -> %-12S want %-12S %s\n"
                   name args got want (if ok "ok" "WRONG")))))

(defun nelisp-native-load-driver--identity-case (name args)
  "Check that NAME returns a list whose first two items are identical."
  (let* ((path (concat nelisp-native-load-driver--dir "/" name ".neln"))
         (got (condition-case e
                  (nelisp-native-load-exec path name args)
                (error (list 'error e))))
         (ok (and (consp got) (consp (cdr got))
                  (eq (car got) (cadr got)))))
    (unless ok
      (setq nelisp-native-load-driver--failures
            (1+ nelisp-native-load-driver--failures)))
    (princ (format "%-14s identity -> %s\n" name
                   (if ok "ok" "WRONG")))))

(defvar nelisp-p5-gateway-effects 0)
(defun nelisp-p5-gateway-once (x)
  (setq nelisp-p5-gateway-effects (1+ nelisp-p5-gateway-effects))
  x)
(defun nelisp-p5-gateway-gc-identity (x)
  (garbage-collect)
  x)
(defun nelisp-p5-gateway-rebound-throw (tag value)
  (setq nelisp-p5-gateway-effects (1+ nelisp-p5-gateway-effects))
  (cons tag value))

(defun nelisp-native-load-driver--gateway-handle ()
  "Load the raw six-register gateway fixture."
  (let ((handle (nelisp-native-load-artifact
                 (concat nelisp-native-load-driver--dir "/gateway-call.neln")
                 "gateway-call")))
    ;; The DSL wrapper keeps its generated parameter representation; it
    ;; unboxes these Lisp integer values before issuing the raw extern call.
    handle))

(defun nelisp-native-load-driver--gateway-call (handle function args)
  "Call HANDLE's shared gateway through the loaded native entry."
  (let* ((env (nelisp--native-env))
         (marker (nelisp-native-load--pin-begin env))
         (fn-slot (nelisp--native-pin-copy env marker function))
         (slots fn-slot)
         (arg-slots nil)
         (rest args)
         (index 1)
         (out-slot nil)
         (env-arg nil)
         (function-arg nil)
         (slots-arg nil)
         (first-arg nil)
         (argc-arg nil)
         (out-arg nil)
         (status nil)
         (value nil)
         (out-box nil)
         (arg-boxes nil))
    (unwind-protect
        (progn
          (while rest
            (let ((slot (nelisp--native-pin-copy env marker (car rest))))
              (unless (= slot (+ slots (* index 32)))
                (error "gateway fixture input slots are not contiguous"))
              (setq arg-slots (append arg-slots (list slot)))
              (setq index (1+ index))
              (setq rest (cdr rest))))
          (setq out-slot (nelisp-native-load--pin-reserve env marker))
          (unless (= out-slot (+ slots (* index 32)))
            (error "gateway fixture result slot is not contiguous"))
          ;; The compiled DSL wrapper uses its generated Sexp-pointer
          ;; parameter ABI. Give it rooted integer boxes; it unwraps these
          ;; values before issuing the raw six-register gateway call.
          (setq env-arg (nelisp--native-pin-copy env marker env))
          (setq function-arg (nelisp--native-pin-copy env marker fn-slot))
          (setq slots-arg (nelisp--native-pin-copy env marker slots))
          (setq first-arg (nelisp--native-pin-copy env marker 1))
          (setq argc-arg (nelisp--native-pin-copy env marker (length args)))
          (setq out-arg (nelisp--native-pin-copy env marker out-slot))
          (ptr-write-u64 (nelisp-native-load-driver--gateway-arena-slot 16) 0 0)
          (setq status
                (ptr-call (plist-get handle :entry)
                          env-arg function-arg slots-arg first-arg argc-arg out-arg))
          (when (= status 0)
            (setq value (nelisp-native-load-unbox out-slot env marker)))
          (setq out-box
                (list (ptr-read-u64 out-slot 0) (ptr-read-u64 out-slot 8)
                      (ptr-read-u64 out-slot 16) (ptr-read-u64 out-slot 24)))
          (setq arg-boxes
                (mapcar (lambda (slot)
                          (list (ptr-read-u64 slot 0) (ptr-read-u64 slot 8)
                                (ptr-read-u64 slot 16) (ptr-read-u64 slot 24)))
                        arg-slots))
          (list status value out-box arg-boxes))
      (nelisp-native-load--pin-end env marker))))

(defun nelisp-native-load-driver--gateway-check (label ok)
  (unless ok
    (setq nelisp-native-load-driver--failures
          (1+ nelisp-native-load-driver--failures)))
  (princ (format "gateway %-18s %s\n" label (if ok "ok" "WRONG"))))

(defun nelisp-native-load-driver--gateway-arena-slot (offset)
  "Return an address at OFFSET in the current runtime arena chunk."
  (+ (ptr-read-u64 (nelisp-native-load--symbol-addr "nl_arena_base") 0)
     offset))

(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (status nil))
  (unwind-protect
      (progn
        (ptr-write-u64 (nelisp-native-load-driver--gateway-arena-slot 16) 0 0)
        (setq status
              (let* ((env-arg (nelisp--native-pin-copy env marker env))
                     (nil-arg (nelisp--native-pin-copy env marker 0))
                     (first-arg (nelisp--native-pin-copy env marker 0))
                     (count-arg (nelisp--native-pin-copy env marker -1)))
                (ptr-call (plist-get handle :entry)
                          env-arg nil-arg nil-arg first-arg count-arg nil-arg)))
        (nelisp-native-load-driver--gateway-check
         "pre-effect reject" (= status 2)))
    (nelisp-native-load--pin-end env marker)))

(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (sentinel 'nelisp-p5-gateway-sentinel)
       (bytecode (make-byte-code
                  0 (unibyte-string 192 193 33 135)
                  [(lambda (x) (+ x 1)) 41] 2))
       (bytecode-result (nelisp-native-load-driver--gateway-call
                         handle bytecode nil))
       (side-effect-result nil))
  (nelisp-native-load-driver--gateway-check
   "bytecode" (and (= (car bytecode-result) 0)
                    (= (cadr bytecode-result) 42)))
  (setq nelisp-p5-gateway-effects 0)
  (setq side-effect-result
        (nelisp-native-load-driver--gateway-call
         handle 'nelisp-p5-gateway-once (list sentinel)))
  (nelisp-native-load-driver--gateway-check
   "one-effect" (and (= (car side-effect-result) 0)
                      (= nelisp-p5-gateway-effects 1)
                      (eq (cadr side-effect-result) sentinel)))
  (setq nelisp-p5-gateway-effects 0)
  (let ((status (nelisp-native-load-call
                 handle (list (nelisp--native-env) 0 0 0 -1 0))))
    (nelisp-native-load-driver--gateway-check
     "pre-effect reject" (and (= status 2) (= nelisp-p5-gateway-effects 0)))))

;; The constant-return bytecode is eligible for the fixnum fast path, but its
;; maximum declared frame leaves no room for the active evaluator roots. Its
;; partial checked reservation must be released before reporting memory-full.
(let* ((bytecode (make-byte-code 0 (unibyte-string 192 135) [42] 131064))
       (caught (condition-case err
                   (funcall bytecode)
                 (error (car err))))
       (after (+ 40 2)))
  (nelisp-native-load-driver--gateway-check
   "root boundary" (and (eq caught 'memory-full) (= after 42))))

;; The same fastpath candidate must reject an extreme depth before evaluating
;; depth + the eight non-operand roots in the native integer domain.
(let* ((bytecode (make-byte-code 0 (unibyte-string 192 135) [42]
                                 most-positive-fixnum))
       (caught (condition-case err
                   (funcall bytecode)
                 (error (car err))))
       (after (+ 40 2)))
  (nelisp-native-load-driver--gateway-check
   "root depth cap" (and (eq caught 'memory-full) (= after 42))))

;; The native caller constructs the only cons value in pinned slots. The
;; gateway's callee forces GC before returning it; compare native identity
;; payloads directly so no decoded host cons keeps it alive.
(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (fn-slot (nelisp--native-pin-copy env marker
                                         'nelisp-p5-gateway-gc-identity))
       (arg-slot (nelisp-native-load--pin-reserve env marker))
       (out-slot (nelisp-native-load--pin-reserve env marker))
       (car-slot (nelisp--native-pin-copy env marker
                                         'nelisp-p5-gateway-sentinel))
       (nil-slot (nelisp--native-pin-copy env marker nil))
       (status nil)
       (same nil))
  (unwind-protect
      (progn
        (unless (and (= arg-slot (+ fn-slot 32))
                     (= out-slot (+ fn-slot 64)))
          (error "gateway GC fixture slots are not contiguous"))
        (ptr-call (nelisp-native-load--symbol-addr "nelisp_cons_construct")
                  car-slot nil-slot arg-slot 0 0 0)
        (ptr-write-u64 (nelisp-native-load-driver--gateway-arena-slot 16) 0 0)
        (setq status
              (let* ((env-arg (nelisp--native-pin-copy env marker env))
                     (function-arg (nelisp--native-pin-copy env marker fn-slot))
                     (slots-arg (nelisp--native-pin-copy env marker fn-slot))
                     (first-arg (nelisp--native-pin-copy env marker 1))
                     (count-arg (nelisp--native-pin-copy env marker 1))
                     (out-arg (nelisp--native-pin-copy env marker out-slot)))
                (ptr-call (plist-get handle :entry)
                          env-arg function-arg slots-arg first-arg count-arg out-arg)))
        (setq same (and (= (ptr-read-u64 out-slot 0)
                           (ptr-read-u64 arg-slot 0))
                        (= (ptr-read-u64 out-slot 8)
                           (ptr-read-u64 arg-slot 8))))
        (nelisp-native-load-driver--gateway-check
         "gc identity" (and (= status 0) same)))
    (nelisp-native-load--pin-end env marker)))

;; Error and throw are returned as status 1 with the existing stash intact;
;; the result slot remains untouched on these non-normal exits.
(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (error-result (nelisp-native-load-driver--gateway-call handle 'car '(7)))
       (error-ok (and (= (car error-result) 1)
                      (= (ptr-read-u64
                          (nelisp-native-load-driver--gateway-arena-slot 16) 0) 1)
                      (eq (nelisp-native-load-unbox
                           (nelisp-native-load-driver--gateway-arena-slot 24))
                          'wrong-type-argument))))
  (nelisp-native-load-driver--gateway-check "signal stash" error-ok))

(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (tag 'nelisp-p5-gateway-throw-tag)
       (value (cons 'nelisp-p5-gateway-throw-value nil))
       (throw-result (nelisp-native-load-driver--gateway-call
                      handle 'throw (list tag value)))
       (value-box (nth 1 (nth 3 throw-result)))
       (throw-ok (and (= (car throw-result) 1)
                      (= (ptr-read-u64
                          (nelisp-native-load-driver--gateway-arena-slot 16) 0) 2)
                      (eq (nelisp-native-load-unbox
                           (nelisp-native-load-driver--gateway-arena-slot 24)) tag)
                      (= (ptr-read-u64
                          (nelisp-native-load-driver--gateway-arena-slot 56) 0)
                         (nth 0 value-box))
                      (= (ptr-read-u64
                          (nelisp-native-load-driver--gateway-arena-slot 56) 8)
                         (nth 1 value-box)))))
  (nelisp-native-load-driver--gateway-check "throw stash" throw-ok))

(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (bad-arity (nelisp-native-load-driver--gateway-call handle 'throw '(tag)))
       (ok (and (= (car bad-arity) 1)
                (= (ptr-read-u64
                    (nelisp-native-load-driver--gateway-arena-slot 16) 0) 1)
                (eq (nelisp-native-load-unbox
                     (nelisp-native-load-driver--gateway-arena-slot 24))
                    'wrong-number-of-arguments))))
  (nelisp-native-load-driver--gateway-check "throw bad arity" ok))

(let* ((handle (nelisp-native-load-driver--gateway-handle))
       (old-function (symbol-function 'throw))
       (result nil)
       (ok nil))
  (unwind-protect
      (progn
        (fset 'throw (function nelisp-p5-gateway-rebound-throw))
        (setq nelisp-p5-gateway-effects 0)
        (setq result
              (nelisp-native-load-driver--gateway-call
               handle 'throw '(tag rebound-value)))
        (setq ok (and (= (car result) 0)
                      (= nelisp-p5-gateway-effects 1)
                      (eq (car (cadr result)) 'tag)
                      (eq (cdr (cadr result)) 'rebound-value)))
        (nelisp-native-load-driver--gateway-check "rebound throw" ok))
    (if old-function
        (fset 'throw old-function)
      (fmakunbound 'throw))))

;; Cloning a boxed native result into the evaluator must retain aliasing.
(nelisp-native-load-driver--identity-case "cons-alias" '(9))
(nelisp-native-load-driver--identity-case "gc-cons-alias" '(9))
(nelisp-native-load-driver--identity-case "string-alias" '("identity"))
(nelisp-native-load-driver--identity-case "gc-string-alias" '("identity"))

;; A forged result address must fail before dereference, leaving the owned
;; pin frame releasable for the next loader call.
(let* ((env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (rejected (condition-case nil
                     (progn (nelisp--native-unbox-reference 1 env marker) nil)
                   (error t))))
  (nelisp-native-load--pin-end env marker)
  (unless rejected
    (setq nelisp-native-load-driver--failures
          (1+ nelisp-native-load-driver--failures))
    (princ "forged result address was not rejected\n")))

;; An unsupported Sexp tag is rejected while the slot remains under the
;; active frame; the frame must still release cleanly after the error.
(let* ((env (nelisp--native-env))
       (marker (nelisp-native-load--pin-begin env))
       (slot (nelisp-native-load--pin-reserve env marker))
       (rejected (progn
                   (ptr-write-u64 slot 0 99)
                   (condition-case nil
                       (progn (nelisp-native-load-unbox slot env marker) nil)
                     (error t)))))
  (nelisp-native-load--pin-end env marker)
  (unless rejected
    (setq nelisp-native-load-driver--failures
          (1+ nelisp-native-load-driver--failures))
    (princ "invalid result tag was not rejected\n")))

;; Boxed boundary: one delegated call, then nesting, then a calln with a
;; literal argument -- the three shapes that each broke separately.
(nelisp-native-load-driver--case "inc1" '(41) 42)
(nelisp-native-load-driver--case "nested" '(41) 42)
(nelisp-native-load-driver--case "carlist" '(41) 42)
;; A slot retained across several interpreted loader helpers must not alias
;; the output/name/scratch slots when the evaluator restores its own root top.
(nelisp-native-load-driver--case "pinid" '(41) 41)
(nelisp-native-load-driver--case "pinid" '(nelisp-p5-pin-symbol)
                                   'nelisp-p5-pin-symbol)
(nelisp-native-load-driver--case "gcpinid" '(nelisp-p5-gc-pin-symbol)
                                   'nelisp-p5-gc-pin-symbol)
;; An error must release the frame so the immediately following call can pin.
(let ((failed (condition-case nil
                  (progn (nelisp-native-load-exec
                          (concat nelisp-native-load-driver--dir "/pinerr.neln")
                          "pinerr" '(1))
                         nil)
                (error t))))
  (unless failed
    (setq nelisp-native-load-driver--failures
          (1+ nelisp-native-load-driver--failures))
    (princ "pinerr did not signal\n")))
(nelisp-native-load-driver--case "pinid" '(43) 43)
;; Integer ABI: no externs, so raw arguments and the result in rax.
(nelisp-native-load-driver--case "add3" '(1 2 3) 6)
;; Arity 0 and 6, the ends of the register range the trampoline covers.
(nelisp-native-load-driver--case "zero" '() 7)
(nelisp-native-load-driver--case "six" '(1 2 3 4 5 6) 21)
;; Values that are not integers, in and out.
(nelisp-native-load-driver--case "strlen" '("hello") 5)
(nelisp-native-load-driver--case "symname" '(0) "abc")
(nelisp-native-load-driver--case "istrue" '(1) t)
(nelisp-native-load-driver--case "isfalse" '(1) nil)
;; Both zero and non-zero literal vector indices must be passed to the
;; native helper as raw indices, not boxed Sexp payloads.
(nelisp-native-load-driver--case "vget" '(0) 7)
(nelisp-native-load-driver--case "plainref" '(0) 8)
(nelisp-native-load-driver--case "nestvec" '(0) 7)
(nelisp-native-load-driver--case "vsetget" '(42) 42)
;; Raw loop state crosses `setq', arithmetic, comparison and `while'.
(nelisp-native-load-driver--case "rawloop" '(10) 10)
;; A dispatcher-produced Sexp integer must unbox before native arithmetic.
(nelisp-native-load-driver--case "dispatchint" '("abc") 13)
;; One shared-borrow acquisition: vector state read, raw arithmetic, write,
;; and boxed vector return, without the loop or cleanup path.
(nelisp-native-load-driver--case "cell-acquire" '(0) 7)
;; A fresh fat pointer crosses allocation, checked u8 write/read lowering,
;; and the raw-integer return boundary without relying on the benchmark.
(nelisp-native-load-driver--case "fat-roundtrip" '() 42)
;; Derived and nested slices must retain their narrowed provenance through
;; binding; the latter also proves a raw monotone loop index against it.
(nelisp-native-load-driver--case "fat-derived" '() 42)
(nelisp-native-load-driver--case "fat-derived-loop" '() 10)

(princ (format "\nfailures: %d\n" nelisp-native-load-driver--failures))
(if (> nelisp-native-load-driver--failures 0) (exit 1) (exit 0))

;;; nelisp-native-load-driver.el ends here
