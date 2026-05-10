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


;; --- sqlite FFI wire-up via emacs-sqlite-ffi + vendor sqlite.el ------

;; Standalone NeLisp ships:
;;   - `emacs-sqlite-ffi.el' (in nelisp-emacs/src/) — uses the in-process
;;     `nl-ffi-call' primitive against `libnelisp_runtime.so' to provide
;;     real `sqlite-available-p' / `sqlite-open' / `sqlite-close' /
;;     `sqlite-execute' / `sqlite-select' / `sqlitep' implementations.
;;   - `vendor/emacs-lisp/sqlite.el' — upstream Emacs sqlite.el, ships
;;     the `with-sqlite-transaction' macro and `(provide 'sqlite)' so
;;     `(require 'sqlite)' from anvil-* downstreams resolves.
;;
;; emacs-init.el unconditionally loads `emacs-sqlite' (= the forwarder
;; layer) which leaves `sqlite-*' bound to thin shims that call
;; `nelisp-sqlite-*' (= unbound on standalone, so available-p returns
;; nil and the forwarders error).  We `fmakunbound' those names so
;; emacs-sqlite-ffi's `unless fboundp' gates evaluate true and the FFI
;; implementations land.

;; Vendor paths.  Directive (2026-05-10 user): substrate polyfills must
;; prefer the upstream Emacs `*.el' shipped under
;; `nelisp-emacs/vendor/emacs-lisp/' over re-implementing or pulling
;; from sibling NeLisp packages.  We prepend the vendor dirs to
;; load-path so `(require 'sqlite)' / `(require 'url)' / `(require
;; 'jsonrpc)' etc resolve to the vendored Emacs sources.
(let* ((nelisp-emacs-root
        (or (and (boundp 'anvil-runtime-polyfills-nelisp-emacs-dir)
                 anvil-runtime-polyfills-nelisp-emacs-dir)
            "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs"))
       (vendor-base (concat nelisp-emacs-root "/vendor/emacs-lisp"))
       (vendor-dirs (list vendor-base
                          (concat vendor-base "/url"))))
  (dolist (dir vendor-dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Drop the forwarder-layer bindings so emacs-sqlite-ffi's gated
;; defuns can win.  `sqlite-pragma' / `sqlite-transaction' /
;; `sqlite-commit' / `sqlite-rollback' have no FFI counterpart yet —
;; leave them on the forwarder layer (= they'll error if called, which
;; is more honest than a silent miss).
(dolist (sym '(sqlite-available-p sqlite-open sqlite-close
               sqlite-execute sqlite-select sqlitep))
  (when (fboundp sym)
    (fmakunbound sym)))

(condition-case anvil-runtime-polyfills--sqlite-load-err
    (require 'emacs-sqlite-ffi)
  (error
   (when (fboundp 'nelisp--write-stderr-line)
     (nelisp--write-stderr-line
      (concat "[anvil-runtime-polyfills] emacs-sqlite-ffi require failed: "
              (format "%S" anvil-runtime-polyfills--sqlite-load-err)
              " (sqlite-query handler will error gracefully)")))))

;; Optional: vendor `sqlite.el' provides `with-sqlite-transaction' and
;; `(provide 'sqlite)'.  Anvil callers don't currently `require 'sqlite'
;; directly, but loading is cheap and makes future requires resolve.
(condition-case nil (require 'sqlite) (error nil))

;; anvil-sqlite drives the Emacs 30 cursor protocol:
;;
;;   (let* ((stmt (sqlite-select db sql params 'set)))
;;     (while (and stmt (sqlite-more-p stmt) (< count cap))
;;       (let ((row (sqlite-next stmt))) ...))
;;     (when stmt (sqlite-finalize stmt)))
;;
;; emacs-sqlite-ffi.el's `sqlite-select' returns the full row list
;; directly regardless of RETURN-TYPE.  We capture that FFI implementation
;; into a defvar (= done AFTER the require above runs), then redefine
;; `sqlite-select' with a cursor-aware wrapper that delegates to the
;; captured FFI function and tags the row list as a
;; `(:anvil-sqlite-cursor . REMAINING-ROWS)' cons when RETURN-TYPE is
;; `set' or `full'.

(defvar anvil-runtime-polyfills--sqlite-select-impl
  (and (fboundp 'sqlite-select) (symbol-function 'sqlite-select))
  "Captured emacs-sqlite-ffi `sqlite-select' implementation.
Used by the cursor-aware override below so the wrapper delegates
to the real FFI without infinite recursion.")

(when anvil-runtime-polyfills--sqlite-select-impl
  (defun sqlite-select (db query &optional values return-type)
    "Cursor-aware override delegating to the captured FFI `sqlite-select'.
When RETURN-TYPE is `'set' or `'full', wrap the row list as a
`(:anvil-sqlite-cursor . REMAINING-ROWS)' cons.  Otherwise return
rows inline (= matches the Emacs 30 builtin's default shape)."
    (let ((rows (funcall anvil-runtime-polyfills--sqlite-select-impl
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

;; --- post-load patches (anvil-* module compat) ---------------------

;; Fixes that depend on anvil-* having been loaded; called by the driver
;; after the ANVIL_TOOL_MODULES load+enable chain.

(defun anvil-runtime-polyfills-apply-post-load-patches ()
  "Apply fixups to anvil-* modules that need substrate-aware overrides.
Idempotent: each branch checks featurep / fboundp before redefining."

  ;; anvil-sqlite uses `(string-match-p \"\\\\`\\\\(?:SELECT\\\\|WITH\\\\|...\\\\)\\\\b\" ...)`
  ;; for its readonly guard.  Standalone NeLisp's regex engine does not
  ;; accept `\\(?:` non-capturing groups + `\\b` word-boundary in the same
  ;; pattern (= returns nil unconditionally), so every legitimate read
  ;; statement gets rejected.  Replace with a regex-free prefix check.
  (when (featurep 'anvil-sqlite)
    (defun anvil-sqlite--readonly-statement-p (sql)
      "Polyfill override: regex-free read-only-statement detector.
Returns non-nil if SQL begins (after upcase + trim) with one of
SELECT / WITH / PRAGMA / EXPLAIN.  Standalone NeLisp's regex
substrate cannot evaluate the original `(?:...)|\\b' pattern."
      (let* ((trimmed (string-trim (or sql "")))
             (up (upcase trimmed)))
        (and (not (string-empty-p up))
             (or (string-prefix-p "SELECT"  up)
                 (string-prefix-p "WITH"    up)
                 (string-prefix-p "PRAGMA"  up)
                 (string-prefix-p "EXPLAIN" up)))))))


(provide 'anvil-runtime-polyfills)
;;; anvil-runtime-polyfills.el ends here
