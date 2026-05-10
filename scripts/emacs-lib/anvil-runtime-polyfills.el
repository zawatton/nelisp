;;; anvil-runtime-polyfills.el --- Substrate polyfills for standalone NeLisp -*- lexical-binding: t; -*-

;; Phase 7 (= 2026-05-10 follow-up to project_anvil_standalone_tools_wired):
;; Provide the substrate bits that GREEN-bucket anvil-* modules expect
;; but standalone NeLisp doesn't ship.  Loaded by
;; `scripts/anvil-runtime-shell-loop.el' between the emacs-init/stub
;; bootstrap and the tool-module load chain so that registrations and
;; tool-call handler bodies see a healthy substrate.
;;
;; Audited gaps (= host Emacs has it, NeLisp standalone doesn't):
;;   - anvil-discovery `tools-by-intent' / `usage-report'
;;       cl-copy-list, cl-remove-duplicates
;;   - anvil-sqlite `sqlite-query'
;;       cl-subseq + sqlite cursor protocol
;;       (sqlite-more-p / sqlite-next / sqlite-finalize)
;;       + `sqlite-select' return-shape override (= cursor on `'set')
;;   - anvil-bench (= deferred to Stage 2)
;;       benchmark-call / benchmark-elapse / benchmark-progn,
;;       profiler-start / profiler-stop / profiler-reset / profiler-cpu-log

;;; Code:

;; --- subr-x feature stub --------------------------------------------

;; emacs-stub-bulk already binds the subr-x bits anvil callers reach
;; for (string-empty-p / string-trim / when-let / hash-table-keys ...).
;; The package as such is missing, so `(require 'subr-x)' fails with
;; "Cannot open load file".  Provide the feature so callers requiring
;; the package see it satisfied without re-binding the helpers.

(unless (featurep 'subr-x)
  (provide 'subr-x))


;; --- cl-lib gaps ----------------------------------------------------

(unless (fboundp 'cl-copy-list)
  (defun cl-copy-list (list)
    "Polyfill: standalone NeLisp's cl-lib does not ship cl-copy-list.
`copy-sequence' on a list returns a fresh top-level cons chain
preserving any dotted-pair tail, which matches the `cl-copy-list'
contract."
    (copy-sequence list)))

(unless (fboundp 'cl-remove-duplicates)
  (defun cl-remove-duplicates (list &rest _ignored-keys)
    "Polyfill: first-occurrence-wins via `equal'.
`:test' / `:key' / `:from-end' keyword args are ignored — the only
anvil callers (= anvil-discovery aggregation) are content with
default semantics."
    (let ((seen nil) (out nil))
      (dolist (x list)
        (unless (member x seen)
          (push x seen)
          (push x out)))
      (nreverse out))))

(unless (fboundp 'cl-subseq)
  (defun cl-subseq (seq start &optional end)
    "Polyfill: subsequence on lists / vectors / strings.
For lists, walks linearly.  For vectors, builds a fresh vector via
`aset'.  For strings, delegates to `substring'."
    (let ((n (length seq)))
      (let ((e (or end n)))
        (cond
         ((listp seq)
          (let ((i 0) (cur seq) (res nil))
            (while (and cur (< i e))
              (when (>= i start)
                (push (car cur) res))
              (setq i (1+ i))
              (setq cur (cdr cur)))
            (nreverse res)))
         ((vectorp seq)
          (let ((len (- e start))
                (i 0))
            (let ((v (make-vector len nil)))
              (while (< i len)
                (aset v i (aref seq (+ start i)))
                (setq i (1+ i)))
              v)))
         ((stringp seq)
          (substring seq start e))
         (t (error "cl-subseq: unsupported sequence type")))))))


;; --- sqlite cursor wrappers ----------------------------------------

;; Standalone NeLisp's `sqlite-select' (= alias to `nelisp-sqlite-select')
;; returns the full result-set as a list of row vectors regardless of
;; the 4th arg.  anvil-sqlite drives the Emacs 30 cursor protocol:
;;
;;   (let* ((stmt (sqlite-select db sql params 'set)))
;;     (while (and stmt (sqlite-more-p stmt) (< count cap))
;;       (let ((row (sqlite-next stmt))) ...))
;;     (when stmt (sqlite-finalize stmt)))
;;
;; We wrap the result list as a tagged cons-cell `(:anvil-sqlite-cursor
;; . REMAINING-ROWS)' that the cursor primitives drive in-place.

(defvar anvil-runtime-polyfills--orig-sqlite-select
  (and (fboundp 'sqlite-select) (symbol-function 'sqlite-select))
  "Captured `sqlite-select' implementation prior to override.
Used by the override to delegate the actual SELECT execution.")

(when anvil-runtime-polyfills--orig-sqlite-select
  (defun sqlite-select (db query &optional values return-type)
    "Cursor-aware override of standalone NeLisp's `sqlite-select'.
When RETURN-TYPE is `'set' or `'full', wrap the row list as a
cursor cons consumable by `sqlite-more-p' / `sqlite-next' /
`sqlite-finalize'.  Otherwise return rows inline."
    (let ((rows (funcall anvil-runtime-polyfills--orig-sqlite-select
                         db query values return-type)))
      (cond
       ((memq return-type '(set full))
        (cons :anvil-sqlite-cursor (or rows nil)))
       (t rows)))))

(unless (fboundp 'sqlite-more-p)
  (defun sqlite-more-p (cursor)
    "Return non-nil while CURSOR has un-consumed rows."
    (and (consp cursor)
         (eq (car cursor) :anvil-sqlite-cursor)
         (cdr cursor))))

(unless (fboundp 'sqlite-next)
  (defun sqlite-next (cursor)
    "Pop and return the next row from CURSOR, advancing internal state."
    (when (and (consp cursor)
               (eq (car cursor) :anvil-sqlite-cursor)
               (cdr cursor))
      (let ((row (cadr cursor)))
        (setcdr cursor (cddr cursor))
        row))))

(unless (fboundp 'sqlite-finalize)
  (defun sqlite-finalize (cursor)
    "Release CURSOR.  For the list-backed polyfill this just clears
the remaining-rows tail."
    (when (and (consp cursor)
               (eq (car cursor) :anvil-sqlite-cursor))
      (setcdr cursor nil))))


;; --- benchmark / profiler stubs ------------------------------------

;; anvil-bench.el `(require 'benchmark) (require 'profiler)' fails
;; because the modules don't ship in standalone NeLisp.  We `provide'
;; the features and stub the surface anvil-bench actually calls.
;; benchmark-* are real timing wrappers, profiler-* are no-ops returning
;; empty results — sampling profiler is a NeLisp-side feature gap.

(unless (featurep 'benchmark)
  (defmacro benchmark-elapse (&rest body)
    "Polyfill: time BODY using `current-time'."
    `(let ((anvil-runtime-polyfills--bench-start (current-time)))
       ,@body
       (float-time (time-subtract (current-time)
                                  anvil-runtime-polyfills--bench-start))))
  (defun benchmark-call (function &optional repetitions)
    "Polyfill: call FUNCTION REPETITIONS times, return (elapsed gc-elapsed gc-count).
Elapsed-seconds is real, the GC fields are zero stubs since standalone
NeLisp doesn't expose `gcs-done' / `gc-elapsed' separately."
    (let* ((reps (or repetitions 1))
           (start (current-time))
           (i 0))
      (while (< i reps)
        (funcall function)
        (setq i (1+ i)))
      (list (float-time (time-subtract (current-time) start)) 0 0)))
  (defmacro benchmark-progn (&rest body)
    "Polyfill: time BODY and message the elapsed seconds."
    `(let ((anvil-runtime-polyfills--bp-start (current-time)))
       (prog1 (progn ,@body)
         (message "Elapsed: %fs"
                  (float-time
                   (time-subtract (current-time)
                                  anvil-runtime-polyfills--bp-start))))))
  (provide 'benchmark))

(unless (featurep 'profiler)
  ;; Sampling profiler is a NeLisp-side feature gap; stub returns an
  ;; empty cpu-log so anvil-bench's profile-driven tools degrade
  ;; gracefully (= empty :top, no crash).
  (defun profiler-start (&optional _mode) nil)
  (defun profiler-stop () nil)
  (defun profiler-reset () nil)
  (defun profiler-cpu-log () (make-hash-table :test 'equal))
  (provide 'profiler))

(provide 'anvil-runtime-polyfills)
;;; anvil-runtime-polyfills.el ends here
