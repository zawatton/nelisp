;;; emacs-sqlite-ffi.el --- sqlite-* via nelisp-ffi for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 3 — Layer 2 SQLite implementation via FFI.
;;
;; Bypasses the Emacs Dynamic Module API (= which NeLisp standalone
;; does not yet expose) by calling `nelisp-sqlite-rs' extern "C"
;; symbols (`nl_sqlite_open' / `nl_sqlite_execute' / `nl_sqlite_query'
;; / `nl_sqlite_close' / `nl_sqlite_alive') directly through
;; `nelisp-ffi-call' against `libnelisp_runtime.so'.
;;
;; This makes the host-Emacs-style `sqlite-open' / `sqlite-execute' /
;; `sqlite-select' / `sqlitep' / `sqlite-available-p' usable under
;; NeLisp standalone — exactly the surface anvil-state.el and
;; anvil-memory.el reach for.
;;
;; Each polyfill is gated on `unless (fboundp ...)' so loading under
;; host Emacs (= where sqlite-* is the C builtin) is a no-op.

;;; Code:

(defvar emacs-sqlite-ffi-libpath
  (or (getenv "NELISP_RUNTIME_SO")
      "/home/madblack-21/Cowork/Notes/dev/nelisp/target/release/libnelisp_runtime.so")
  "Absolute path to `libnelisp_runtime.so' (= nelisp-sqlite-rs cdylib).
Override via `NELISP_RUNTIME_SO' env var or by `setq' before this
file loads.  Phase 3 hard-codes the dev tree path; Phase 4 will
auto-detect via NELISP_HOME / ANVIL_HOME / repo-locate-dominating.")


;;;; --- low-level FFI helpers ---------------------------------------------

(defun emacs-sqlite-ffi--open (path)
  "FFI: nl_sqlite_open(PATH) → handle (i64)."
  (require 'nelisp-ffi)
  (nelisp-ffi-call emacs-sqlite-ffi-libpath
                   "nl_sqlite_open"
                   [:sint64 :pointer]
                   path))

(defun emacs-sqlite-ffi--close (handle)
  "FFI: nl_sqlite_close(HANDLE) → 0/error."
  (require 'nelisp-ffi)
  (nelisp-ffi-call emacs-sqlite-ffi-libpath
                   "nl_sqlite_close"
                   [:sint64 :sint64]
                   handle))

(defun emacs-sqlite-ffi--alive (handle)
  "FFI: nl_sqlite_alive(HANDLE) → 1 if open, 0 otherwise."
  (require 'nelisp-ffi)
  (nelisp-ffi-call emacs-sqlite-ffi-libpath
                   "nl_sqlite_alive"
                   [:sint64 :sint64]
                   handle))

(defun emacs-sqlite-ffi--execute-raw (handle sql args-json)
  "FFI: nl_sqlite_execute(HANDLE, SQL, ARGS-JSON) → row count or error."
  (require 'nelisp-ffi)
  (nelisp-ffi-call emacs-sqlite-ffi-libpath
                   "nl_sqlite_execute"
                   [:sint64 :sint64 :pointer :pointer]
                   handle sql args-json))


;;;; --- args encoding (= minimal JSON for Phase 3) ------------------------

(defun emacs-sqlite-ffi--encode-args (values)
  "Encode VALUES (= a list of literals) into a JSON array string.
Phase 3 supports nil / strings / integers / floats only.  Booleans,
nested lists, and binary blobs are Phase 4."
  (cond
   ((null values) "[]")
   (t
    (let ((acc nil)
          (cur values))
      (while cur
        (let ((v (car cur)))
          (cond
           ((null v)         (setq acc (cons "null" acc)))
           ((eq v t)         (setq acc (cons "true" acc)))
           ((stringp v)
            ;; Naive escaping: backslash + quote.  Adequate for the
            ;; query parameters anvil-state uses (= mostly path / id
            ;; strings without quotes or backslashes).
            (setq acc (cons (concat "\""
                                    (replace-regexp-in-string
                                     "\"" "\\\\\""
                                     (replace-regexp-in-string
                                      "\\\\" "\\\\\\\\" v))
                                    "\"")
                            acc)))
           ((integerp v)     (setq acc (cons (number-to-string v) acc)))
           ((floatp v)       (setq acc (cons (number-to-string v) acc)))
           (t                (setq acc (cons (prin1-to-string v) acc)))))
        (setq cur (cdr cur)))
      (let ((reversed nil))
        (while acc (setq reversed (cons (car acc) reversed)) (setq acc (cdr acc)))
        (let ((s "["))
          (let ((first t))
            (while reversed
              (unless first (setq s (concat s ",")))
              (setq s (concat s (car reversed)))
              (setq first nil)
              (setq reversed (cdr reversed))))
          (concat s "]")))))))


;;;; --- public Emacs-API polyfills ----------------------------------------

(unless (fboundp 'sqlite-available-p)
  (defun sqlite-available-p ()
    "Return non-nil if FFI-backed SQLite can be invoked.
Probes by opening :memory: and immediately closing.  Caches the
result in `emacs-sqlite-ffi--available' on success."
    (condition-case _
        (let ((h (emacs-sqlite-ffi--open ":memory:")))
          (when (and (integerp h) (> h 0))
            (emacs-sqlite-ffi--close h)
            t))
      (error nil))))

(unless (fboundp 'sqlite-open)
  (defun sqlite-open (path)
    "Open a SQLite database at PATH; return the handle."
    (let ((h (emacs-sqlite-ffi--open path)))
      (unless (and (integerp h) (> h 0))
        (error "sqlite-open: failed for %s (handle=%S)" path h))
      h)))

(unless (fboundp 'sqlite-close)
  (defun sqlite-close (db)
    "Close DB; return non-nil on success."
    (= 0 (emacs-sqlite-ffi--close db))))

(unless (fboundp 'sqlitep)
  (defun sqlitep (object)
    "Return t when OBJECT looks like a sqlite handle (= positive integer)."
    (and (integerp object) (> object 0))))

(unless (fboundp 'sqlite-execute)
  (defun sqlite-execute (db query &optional values)
    "Execute QUERY against DB with optional VALUES.
Returns the number of affected rows on success; signals an error on
failure."
    (let* ((args-json (emacs-sqlite-ffi--encode-args values))
           (rc (emacs-sqlite-ffi--execute-raw db query args-json)))
      (when (< rc 0)
        (error "sqlite-execute: error %d for %s" rc query))
      rc)))

(unless (fboundp 'sqlite-select)
  (defun sqlite-select (db query &optional values _return-type)
    "Phase 3 stub: returns nil for SELECT queries.
Real implementation requires `nl_sqlite_query' which writes a JSON
result into a caller-supplied buffer — needs Phase 4 FFI buffer
support to surface back to Lisp."
    (ignore db query values)
    nil))


(provide 'emacs-sqlite-ffi)

;;; emacs-sqlite-ffi.el ends here
