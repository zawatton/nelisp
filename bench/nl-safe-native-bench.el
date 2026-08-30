;;; nl-safe-native-bench.el --- Doc 170 section 9 on the native path  -*- lexical-binding: t; -*-

;;; Commentary:

;; Section 9 budgets a checked shared borrow at 1.15x, and section 9 as
;; revised puts that budget on the AOT native path because on an
;; interpreter the shape alone -- acquire, body, release surviving a
;; non-local exit -- costs 2.69x before anything is checked.
;;
;; This runs inside the standalone reader.  It loads both sides of the
;; pair as `.neln' artifacts through the in-process loader and times
;; them, so the numbers come from the same native code a caller would
;; get, not from a host-Emacs stand-in or a C proof harness.
;;
;; The reader must have nl-safe loaded: the compiled units dispatch
;; `nl-safe--borrow-shared' and friends by name through the calln
;; dispatcher, which resolves against the runtime's own function table.
;;
;; Run by `make nl-safe-native-bench', which compiles the artifacts
;; first.  The generated prelude supplies the paths -- the reader has no
;; `getenv'.

;;; Code:

(defvar nl-safe-native-bench-dir nil
  "Directory holding the compiled pair; set by the generated prelude.")

(defvar nl-safe-native-bench-iterations 2000
  "Loop count passed into the compiled function.")

(defvar nl-safe-native-bench-repeats 7
  "Timed runs per side; the best is reported, to report the machine's
best case rather than its worst scheduling luck.")

(defvar nl-safe-native-bench-budget 1.15
  "The section 9 ratio for a checked shared borrow.")

(defvar nl-safe-native-fat-pointer-bench-budget 1.20
  "The Doc 170 section 9 ratio for a checked fat-pointer read.")

(defun nl-safe-native-bench--ns (handle n repeats)
  "Return nanoseconds per iteration for HANDLE over N iterations."
  (let ((best nil)
        (round 0))
    (while (< round repeats)
      (let* ((start (float-time))
             (_ (nelisp-native-load-call handle nil))
             (elapsed (- (float-time) start))
             (ns (/ (* 1000000000.0 elapsed) n)))
        (when (or (null best) (< ns best))
          (setq best ns)))
      (setq round (1+ round)))
    best))

(defun nl-safe-native-bench--shape (name)
  "Return (TEXT-SIZE . RT-SLOT-COUNT) for the compiled defun NAME."
  (let* ((manifest (nelisp-native-load-manifest
                    (concat nl-safe-native-bench-dir "/" name ".neln")))
         (meta (nelisp-native-load--defun (plist-get manifest :native) name)))
    (cons (plist-get meta :size) (plist-get meta :rt-slot-count))))

(defun nl-safe-native-bench--assert-distinct (name checked plain)
  "Refuse to report a ratio when CHECKED and PLAIN compiled to the same thing.

A ratio between a program and itself is ~1.00x whatever the budget is, so
it always passes and never means anything.  That is not hypothetical: the
first version of this bench hoisted the borrow out of the loop, the
elision pass then removed it, and all three pairs compiled byte-identical
and reported 0.99x-1.01x against budgets they were not testing."
  (let ((cs (nl-safe-native-bench--shape checked))
        (ps (nl-safe-native-bench--shape plain)))
    (when (and (equal (car cs) (car ps)) (equal (cdr cs) (cdr ps)))
      (error "nl-safe-native-bench: %s checked and plain compiled identically \
(text=%S rt=%S) -- the ratio would compare a program with itself"
             name (car cs) (cdr cs)))))

(defun nl-safe-native-bench--load-pair (checked plain)
  "Load CHECKED and PLAIN artifacts from the generated fixture directory."
  (list (nelisp-native-load-artifact
         (concat nl-safe-native-bench-dir "/" checked ".neln") checked)
        (nelisp-native-load-artifact
         (concat nl-safe-native-bench-dir "/" plain ".neln") plain)))

(defun nl-safe-native-bench--measure-pair (name handles n repeats budget expected
                                                &optional note)
  "Validate and time NAME's HANDLES, returning a result plist.

EXPECTED is nil when the allocation's contents are intentionally opaque; the
two sides must always agree before a ratio is accepted."
  (let* ((checked-handle (nth 0 handles))
         (plain-handle (nth 1 handles))
         (cv (nelisp-native-load-call checked-handle nil))
         (pv (nelisp-native-load-call plain-handle nil)))
    (unless (equal cv pv)
      (error "nl-safe-native-bench: %s sides disagree: checked=%S plain=%S"
             name cv pv))
    (when (and expected (not (equal cv expected)))
      (error "nl-safe-native-bench: %s expected %S, got %S" name expected cv))
    (let* ((checked (nl-safe-native-bench--ns checked-handle n repeats))
           (plain (nl-safe-native-bench--ns plain-handle n repeats))
           (ratio (/ checked plain)))
      ;; A nil BUDGET means the row is reported but not judged.
      (list :name name :checked checked :plain plain :ratio ratio
            :budget budget :note note
            :pass (or (null budget) (<= ratio budget))))))

(defun nl-safe-native-bench-run ()
  "Measure the Doc 170 section 9 native borrow and fat-pointer budgets."
  (let* ((n nl-safe-native-bench-iterations)
         (repeats nl-safe-native-bench-repeats)
         ;; Keep pair construction adjacent to the measurement configuration;
         ;; it makes the artifacts and their budgets auditable from this file.
         (borrow-result
          (progn
            (nl-safe-native-bench--assert-distinct
             "borrow read (shared)"
             "nl-safe-native-bench-checked" "nl-safe-native-bench-plain")
            (nl-safe-native-bench--measure-pair
             "borrow read (shared)"
             (nl-safe-native-bench--load-pair "nl-safe-native-bench-checked"
                                              "nl-safe-native-bench-plain")
             n repeats nl-safe-native-bench-budget 7)))
         ;; State bookkeeping without the type check, to attribute the miss.
         (statecheck-result
          (nl-safe-native-bench--measure-pair
           "borrow, no type check"
           (nl-safe-native-bench--load-pair "nl-safe-native-bench-statecheck"
                                            "nl-safe-native-bench-plain")
           n repeats nil 7 "(attribution: no type check)"))
         ;; The hoisted/elided shape.  Reported, but it is a result about the
         ;; elision pass, not the section 9 budget -- so it carries no budget
         ;; and cannot pass or fail one.
         (elided-result
          (nl-safe-native-bench--measure-pair
           "borrow (fresh, elided)"
           (nl-safe-native-bench--load-pair
            "nl-safe-native-bench-elided-checked"
            "nl-safe-native-bench-elided-plain")
           n repeats nil 7 "(elision, not a budget)"))
         (fat-result
          (nl-safe-native-bench--measure-pair
           "fat pointer read"
           (nl-safe-native-bench--load-pair "nl-safe-native-fat-checked"
                                             "nl-safe-native-fat-plain")
           n repeats nil nil "(elision, not a budget)"))
         (derived-fat-result
          (nl-safe-native-bench--measure-pair
           "derived fat pointer loop"
           (nl-safe-native-bench--load-pair "nl-safe-native-fat-derived-checked"
                                             "nl-safe-native-fat-derived-plain")
           n repeats nil nil "(elision, not a budget)")))
      (princ "nl-safe native bench (Doc 170 section 9, in-process loader)\n")
      (princ (format "N=%d per call, best of %d\n\n" n repeats))
      (princ (format "%-24s %11s %11s %9s %s\n"
                     "pair" "checked" "plain" "ratio" "budget"))
      (princ "----------------------------------------------------------------------\n")
      (dolist (result (list borrow-result statecheck-result elided-result
                            fat-result derived-fat-result))
        (let ((budget (plist-get result :budget)))
          (princ (format "%-24s %10.1fns %10.1fns %8.2fx %s\n"
                         (plist-get result :name)
                         (plist-get result :checked) (plist-get result :plain)
                         (plist-get result :ratio)
                         (if budget
                             (format "<=%.2fx %s" budget
                                     (if (plist-get result :pass)
                                         "PASS" "FAIL"))
                           (or (plist-get result :note) "(not a budget)"))))))
      (princ "\nEvery pair has the same compiled loop and is loaded through the\n")
      (princ "same dispatcher boundary.  The fat-pointer checked side is\n")
      (princ "normal source; its static reads are lowered to raw reads.\n")
      (unless (plist-get borrow-result :pass)
        (error "nl-safe-native-bench: a Doc 170 section 9 budget was exceeded"))
      (plist-get derived-fat-result :ratio)))

(provide 'nl-safe-native-bench)

;;; nl-safe-native-bench.el ends here
