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

;; Boxed boundary: one delegated call, then nesting, then a calln with a
;; literal argument -- the three shapes that each broke separately.
(nelisp-native-load-driver--case "inc1" '(41) 42)
(nelisp-native-load-driver--case "nested" '(41) 42)
(nelisp-native-load-driver--case "carlist" '(41) 42)
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
