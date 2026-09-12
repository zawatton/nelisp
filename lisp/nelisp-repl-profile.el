;;; nelisp-repl-profile.el --- name-a-function timing for a live REPL -*- lexical-binding: t; -*-

;;; Commentary:

;; There is no profiler in the standalone, and the two obvious substitutes do
;; not exist either: `mapatoms' is unbound (measured 2026-09-12) so nothing
;; can sweep the obarray, and `advice.el' is absent, which
;; `nelisp-repl-code.el' already works around with an explicit `fset'.  So a
;; slow call in a live session -- `(require 'treemacs)' taking 25 minutes
;; through the consumer loader, say -- had nowhere to be looked at except by
;; guessing which function to wrap by hand, one at a time, restarting in
;; between.
;;
;; This wraps a NAMED list of functions and reports how many times each was
;; entered and how long the outermost entries took.  Naming them is the
;; point: without an obarray sweep there is no "profile everything", and a
;; list you wrote is a list you can read the results against.
;;
;; What the numbers are, exactly:
;;
;;   :calls    every entry, including recursive ones.
;;   :seconds  wall time of OUTERMOST entries only.  A function that calls
;;             itself is counted once for the whole nest, so the column sums
;;             to something a human can compare against the operation's own
;;             wall time instead of double counting recursion.
;;   inclusive Time spent in callees is included.  Two instrumented functions
;;             in a caller/callee relation therefore both count it; that is
;;             what localizes a cost to a chain, and it is why the report says
;;             `inclusive' rather than pretending to be self time.
;;
;; The shim costs something per call, so the tool measures its own overhead
;; (`nelisp-repl-profile-overhead-seconds') rather than asking anyone to
;; assume it is negligible: a function entered a million times is reported
;; with an overhead the caller can subtract.

;;; Code:

(defvar nelisp-repl-profile--records (make-hash-table :test #'eq)
  "NAME -> [CALLS SECONDS DEPTH].")

(defvar nelisp-repl-profile--originals (make-hash-table :test #'eq)
  "NAME -> the function this module replaced, for `nelisp-repl-profile-restore'.")

(defvar nelisp-repl-profile--shims (make-hash-table :test #'eq)
  "NAME -> the shim this module installed, so `restore' can tell it apart.")

(defun nelisp-repl-profile--record (name)
  (or (gethash name nelisp-repl-profile--records)
      (puthash name (vector 0 0.0 0) nelisp-repl-profile--records)))

(defun nelisp-repl-profile--wrap (name original)
  "Return a function that calls ORIGINAL and times entries under NAME."
  (lambda (&rest args)
    (let ((rec (nelisp-repl-profile--record name))
          (start nil))
      (aset rec 0 (1+ (aref rec 0)))
      (aset rec 2 (1+ (aref rec 2)))
      (setq start (and (= (aref rec 2) 1) (float-time)))
      (unwind-protect
          (apply original args)
        (aset rec 2 (1- (aref rec 2)))
        (when (and start (= (aref rec 2) 0))
          (aset rec 1 (+ (aref rec 1) (- (float-time) start))))))))

(defun nelisp-repl-profile-instrument (names)
  "Instrument NAMES, a list of function symbols.  Return the ones installed.

A name that is not `fboundp', or is already instrumented, is skipped and
left out of the returned list rather than reported as installed."
  (let ((installed nil))
    (dolist (name (if (symbolp names) (list names) names))
      (when (and (symbolp name)
                 (fboundp name)
                 (null (gethash name nelisp-repl-profile--originals)))
        (let* ((original (symbol-function name))
               (shim (nelisp-repl-profile--wrap name original)))
          (puthash name original nelisp-repl-profile--originals)
          (puthash name shim nelisp-repl-profile--shims)
          (nelisp-repl-profile--record name)
          (fset name shim)
          (setq installed (cons name installed)))))
    (nreverse installed)))

(defun nelisp-repl-profile-restore (&optional names)
  "Put back the original definitions of NAMES, or of everything instrumented.

A name whose definition is no longer this module's shim is left alone and
named in the returned list's `:skipped', because something else -- a
reload, an `fset' -- owns it now and silently overwriting that would lose
the newer definition."
  (let ((restored nil)
        (skipped nil)
        (targets nil))
    (if names
        (setq targets (if (symbolp names) (list names) names))
      (maphash (lambda (name _orig) (setq targets (cons name targets)))
               nelisp-repl-profile--originals))
    (dolist (name targets)
      (let ((original (gethash name nelisp-repl-profile--originals)))
        (cond
         ((null original) (setq skipped (cons name skipped)))
         ((not (eq (symbol-function name)
                   (gethash name nelisp-repl-profile--shims)))
          (remhash name nelisp-repl-profile--originals)
          (remhash name nelisp-repl-profile--shims)
          (setq skipped (cons name skipped)))
         (t
          (fset name original)
          (remhash name nelisp-repl-profile--originals)
          (remhash name nelisp-repl-profile--shims)
          (setq restored (cons name restored))))))
    (list :restored (nreverse restored) :skipped (nreverse skipped))))

(defun nelisp-repl-profile-reset ()
  "Zero the counters, keeping the instrumentation in place."
  (maphash (lambda (_name rec)
             (aset rec 0 0)
             (aset rec 1 0.0)
             (aset rec 2 0))
           nelisp-repl-profile--records)
  t)

(defun nelisp-repl-profile-report (&optional limit)
  "Return the instrumented functions as plists, slowest first.

Each entry is (:name NAME :calls N :seconds S :inclusive t).  LIMIT caps
the number of entries returned; nil returns all of them."
  (let ((rows nil))
    (maphash (lambda (name rec)
               (setq rows (cons (list :name name
                                      :calls (aref rec 0)
                                      :seconds (aref rec 1)
                                      :inclusive t)
                                rows)))
             nelisp-repl-profile--records)
    (setq rows (sort rows (lambda (a b)
                            (> (plist-get a :seconds)
                               (plist-get b :seconds)))))
    (if (and limit (> (length rows) limit))
        (let ((head nil) (n 0))
          (while (and rows (< n limit))
            (setq head (cons (car rows) head))
            (setq rows (cdr rows))
            (setq n (1+ n)))
          (nreverse head))
      rows)))

(defun nelisp-repl-profile-overhead-seconds (&optional iterations)
  "Measure this module's per-call overhead, in seconds.

Runs a trivial function ITERATIONS times (default 10000) uninstrumented
and instrumented, and returns the difference divided by ITERATIONS.  The
answer belongs to this machine and this session; it is measured rather
than assumed so a report on a function with a million entries can be read
with the shim's share subtracted."
  (let* ((n (or iterations 10000))
         ;; `intern' rather than a quoted symbol: with the symbol as a
         ;; constant the byte compiler resolves `(funcall name ...)' and then
         ;; warns that a function this code creates at run time is not known
         ;; to be defined.
         (name (intern "nelisp-repl-profile--overhead-probe"))
         (i 0)
         (bare 0.0)
         (wrapped 0.0))
    (fset name (lambda (x) x))
    (let ((start (float-time)))
      (while (< i n) (funcall name i) (setq i (1+ i)))
      (setq bare (- (float-time) start)))
    (nelisp-repl-profile-instrument (list name))
    (setq i 0)
    (let ((start (float-time)))
      (while (< i n) (funcall name i) (setq i (1+ i)))
      (setq wrapped (- (float-time) start)))
    (nelisp-repl-profile-restore (list name))
    (remhash name nelisp-repl-profile--records)
    (/ (- wrapped bare) (* 1.0 n))))

(provide 'nelisp-repl-profile)

;;; nelisp-repl-profile.el ends here
