;;; nl-safe-native-bench-fixtures.el --- section 9 pair as .neln  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 170 section 9 budgets a checked shared borrow at 1.15x on the AOT
;; native path.  Two things had to change before that could be measured.
;;
;; The unit's extern set has to close over the runtime symbols, which
;; `nelisp-aot-compiler--dynamic-user-calls' does.  Both sides are
;; compiled that way, so the mode cancels out of the ratio.
;;
;; `aot-with-fresh-shared-borrow' is the internal result of borrow analysis
;; for a syntactically fresh, non-escaping cell.  It exposes only the value
;; to its body, so the state is provably zero and acquire/release bookkeeping
;; can be removed.  Its plain counterpart has the same value-read shape.
;;
;; nl-safe also guards against borrowing a cell that is exclusively
;; borrowed.  That guard is a compare against the state already read,
;; and its false arm is likewise not entered here.
;;
;; Two more shapes the measurement depends on:
;;
;; The loop is INSIDE the compiled function.  The loader boxes integers,
;; not vectors, so a cell cannot be an argument -- and building one per
;; call would put an allocation in both sides and shrink the ratio
;; towards 1, flattering the borrow.  Built once, the timed region is
;; the borrow.
;;
;; The plain side is what `nl-with-borrow' expands to when checking is
;; disabled -- a `let' over the cell's value slot -- not a bare `aref'.

;;; Code:

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nl-safe-native-bench-fixtures
  (list
   ;; Per-iteration borrow: this is the section 9 budget.  The acquire and
   ;; release happen 2000 times, so their cost is what the ratio shows.
   ;;
   ;; The cell is a real one.  `(vector 1 V 0)' is NOT a borrow cell --
   ;; `nl-cell-p' wants `nl--cell' in slot 0 -- so a borrow over it signals
   ;; `nl-type-error'.  Measuring that shape measured a program that does
   ;; not run.
   ;;
   ;; The acquire/release are written out rather than reached through
   ;; `nl-with-borrow', for two reasons.  The macro would have to be
   ;; expanded at compile time, and its helpers are ordinary elisp
   ;; functions that the reader's `nelisp_apply_function' cannot dispatch
   ;; -- it walks a fixed if-chain over the registered builtins and writes
   ;; anything else to stderr.  In-unit calls need no dispatcher.
   ;;
   ;; What that costs in fidelity: the fast path is nl-safe's, operation
   ;; for operation -- type check, state read, increment, value read, and
   ;; the decrement on release.  What differs is the branch a passing run
   ;; never enters.  nl-safe signals there; this returns 0, because a
   ;; compiled `signal' needs `nelisp_aot_signal' and the reader has no
   ;; such symbol.  Both are a test whose false arm is not taken.
   ;; One defun, with the acquire and release written into the loop rather
   ;; than called.  Splitting them into helpers is the natural shape, but
   ;; the in-process loader gives a unit ONE boundary scratch vector, so a
   ;; callee that uses scratch corrupts its caller's -- measured: a
   ;; two-defun unit runs, and the same unit faults as soon as the callee
   ;; touches a vector.  Inlining sidesteps that; what it costs the
   ;; measurement is the call overhead of nl-safe's internal structure,
   ;; not any of the checking work, which is what the budget is about.
   (list "nl-safe-native-bench-checked"
         "(defun nl-safe-native-bench-checked ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (while (< i 2000)
      (if (and (vectorp c) (= (length c) 3) (eq (aref c 0) 'nl--cell))
          (let ((state (aref c 2)))
            (aset c 2 (+ state 1))
            (setq acc (aref (aref c 1) 0))
            (aset c 2 (- (aref c 2) 1)))
        ;; Never taken.  It assigns the same way the true arm does so both
        ;; arms leave `acc' in one representation: an else arm of `0' made
        ;; the compiler keep `acc' raw in this side and boxed in the other,
        ;; and the two sides then disagreed on their result.
        (setq acc (aref (aref c 1) 0)))
      (setq i (+ i 1)))
    acc))")
   (list "nl-safe-native-bench-plain"
         "(defun nl-safe-native-bench-plain ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (while (< i 2000)
      (setq acc (aref (aref c 1) 0))
      (setq i (+ i 1)))
    acc))")
   ;; Attribution ladder: the type check is three predicates, and they do
   ;; not cost the same.  Each rung adds one, so the deltas say which to
   ;; lower natively next rather than guessing.
   (list "nl-safe-native-bench-chk1"
         "(defun nl-safe-native-bench-chk1 ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (while (< i 2000)
      (if (vectorp c)
          (let ((state (aref c 2)))
            (aset c 2 (+ state 1))
            (setq acc (aref (aref c 1) 0))
            (aset c 2 (- (aref c 2) 1)))
        (setq acc (aref (aref c 1) 0)))
      (setq i (+ i 1)))
    acc))")
   (list "nl-safe-native-bench-chk2"
         "(defun nl-safe-native-bench-chk2 ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (while (< i 2000)
      (if (and (vectorp c) (= (length c) 3))
          (let ((state (aref c 2)))
            (aset c 2 (+ state 1))
            (setq acc (aref (aref c 1) 0))
            (aset c 2 (- (aref c 2) 1)))
        (setq acc (aref (aref c 1) 0)))
      (setq i (+ i 1)))
    acc))")
   ;; The same per-iteration borrow with the TYPE CHECK removed, keeping
   ;; only the state bookkeeping.  Splits the budget miss into its two
   ;; parts: `vectorp' / `length' / `eq' have no native lowering and each
   ;; costs a dispatcher round-trip, while `aref' / `aset' lower natively
   ;; through `nl_vector_slot_ptr'.  Without this row a reader cannot tell
   ;; whether the borrow is expensive or three predicates are.
   (list "nl-safe-native-bench-statecheck"
         "(defun nl-safe-native-bench-statecheck ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (while (< i 2000)
      (let ((state (aref c 2)))
        (aset c 2 (+ state 1))
        (setq acc (aref (aref c 1) 0))
        (aset c 2 (- (aref c 2) 1)))
      (setq i (+ i 1)))
    acc))")
   ;; The hoisted shape, kept because it is a real result about the elision
   ;; pass -- just not the borrow budget.  Reported under its own label.
   (list "nl-safe-native-bench-elided-checked"
         "(defun nl-safe-native-bench-elided-checked ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)))
    (nl-with-borrow (v c)
      (let ((i 0) (acc 0))
        (while (< i 2000)
          (setq acc (aref v 0))
          (setq i (+ i 1)))
        acc))))")
   (list "nl-safe-native-bench-elided-plain"
         "(defun nl-safe-native-bench-elided-plain ()
  (let ((c (vector 'nl--cell (vector 7 8 9) 0)) (i 0) (acc 0))
    (let ((v (aref c 1)))
      (while (< i 2000)
        (setq acc (aref v 0))
        (setq i (+ i 1)))
      acc)))")
   (list "nl-safe-native-fat-checked"
         "(defun nl-safe-native-fat-checked ()
  (let ((p (nl-ptr-make (alloc-bytes 64 8) 64 0)))
    (let ((i 0) (acc 0))
      (while (< i 2000)
        (setq acc (+ acc (nl-ptr-ref-u8 p 0)))
        (setq i (+ i 1)))
      acc)))")
   (list "nl-safe-native-fat-plain"
         "(defun nl-safe-native-fat-plain ()
  (let ((p (alloc-bytes 64 8)) (i 0) (acc 0))
    (while (< i 2000)
      (setq acc (+ acc (ptr-read-u8 p 0)))
      (setq i (+ i 1)))
    acc))")
   (list "nl-safe-native-fat-derived-checked"
         "(defun nl-safe-native-fat-derived-checked ()
  (let ((p (nl-ptr-make (alloc-bytes 2048 8) 2048 0)))
    (let ((s (nl-ptr-slice p 2 2046)))
      (let ((q (nl-ptr-slice s 0 2000)))
        (nl-ptr-set-u8 q 0 1)
        (let ((i 0) (acc 0))
          (while (< i 2000)
            (setq acc (+ acc (nl-ptr-ref-u8 q i)))
            (setq i (+ i 1)))
          acc)))))")
   (list "nl-safe-native-fat-derived-plain"
         "(defun nl-safe-native-fat-derived-plain ()
  (let ((p (alloc-bytes 2048 8)) (i 0) (acc 0))
    (ptr-write-u8 p 2 1)
    (while (< i 2000)
      (setq acc (+ acc (ptr-read-u8 p (+ 2 i))))
      (setq i (+ i 1)))
    acc))"))
  "(NAME SOURCE) for the section 9 native ratio pairs.")

(defun nl-safe-native-bench-fixtures-build (dir)
  "Compile both sides into DIR, and report the extern set of each."
  (make-directory dir t)
  (dolist (entry nl-safe-native-bench-fixtures)
    (let* ((name (car entry))
           (el (expand-file-name (concat name ".el") dir))
           (neln (expand-file-name (concat name ".neln") dir))
           (nelisp-aot-compiler--dynamic-user-calls t)
           ;; The fixture constructs only NeLisp vectors, so it supplies
           ;; the proof required for the opt-in native aref/aset lowering.
           (nelisp-aot-compiler--native-vector-primitives t)
           (nelisp-aot-compiler--aot-borrow-check-elision t)
           (nelisp-aot-compiler--aot-fat-pointer-check-elision t))
      (with-temp-file el
        (insert (cadr entry) (format "\n(provide '%s)\n" name)))
      (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln)
      (let* ((native (plist-get (nelisp-artifact--read-payload neln) :native))
             (meta (car (seq-filter
                         (lambda (m) (equal (plist-get m :name) name))
                         (plist-get native :defuns)))))
        (message "[fixture] %-30s text=%s rt=%s externs=%S"
                 name (plist-get meta :size) (plist-get meta :rt-slot-count)
                 (plist-get native :extern-symbols))))))

(defun nl-safe-native-bench-fixtures-main ()
  "Entry point: build into the directory named by NELISP_ARTIFACT_DIR."
  (let ((dir (getenv "NELISP_ARTIFACT_DIR")))
    (unless dir
      (error "nl-safe-native-bench-fixtures: NELISP_ARTIFACT_DIR is unset"))
    (nl-safe-native-bench-fixtures-build dir)))

(provide 'nl-safe-native-bench-fixtures)

;;; nl-safe-native-bench-fixtures.el ends here
