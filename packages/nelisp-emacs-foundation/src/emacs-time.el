;;; emacs-time.el --- Time + truncate polyfills for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 6
;; write-path polyfill).  Wraps the build-tool builtins
;; `nl-current-unix-time' when present, otherwise the NeLisp
;; `nelisp--syscall' bridge for Linux `time(2)'.
;;
;; Real Emacs `current-time' returns a HIGH/LOW/MICRO list — anvil
;; callsites only pull `(truncate (float-time))' so we expose that
;; path directly without bothering with the legacy list shape.
;;
;; `truncate' is included here because its bulk-stub no-op (emitted
;; by `emacs-stub-bulk.el') was not real-integer-correct; this file's
;; version replaces it when the bulk stub fired first.
;;
;; Each definition is gated on the appropriate `unless (fboundp ...)'
;; or live-replace check.  Loading under host Emacs is a cheap no-op.

;;; Code:

(defun emacs-time--standalone-unix-time ()
  "Return Unix time from the standalone runtime, or nil if unavailable."
  (cond
   ((fboundp 'nl-current-unix-time)
    (nl-current-unix-time))
   ;; Linux x86_64/aarch64: __NR_time = 201.  Passing NULL avoids any
   ;; userspace pointer writes and returns the epoch seconds directly.
   ((fboundp 'nelisp--syscall)
    (condition-case nil
        (let ((value (nelisp--syscall 201 0)))
          (and (integerp value)
               (>= value 0)
               value))
      (error nil)))
   (t nil)))

;; Live-replace gate — same pattern as `truncate' below.  We only
;; override `float-time' / `current-time' when the host's binding is
;; missing or is the no-op bulk stub (`emacs-stub-bulk.el' returns nil).
;; Under regular Emacs the host's correct implementations are kept
;; intact so `accept-process-output' and other timing-sensitive code
;; paths continue to work during ERT runs.

(unless (and (fboundp 'float-time)
             (let ((ft (ignore-errors (float-time))))
               (and (numberp ft) (> ft 0))))
  (defun float-time (&optional time-value)
    "Return seconds since the Unix epoch.
TIME-VALUE is accepted for API compatibility but only a nil value
is supported (= read current time)."
    ;; Keep the argument for API compatibility.  Do not call `ignore'
    ;; here: raw NeLisp does not define it before `emacs-stub' loads.
    (if time-value nil nil)
    (or (emacs-time--standalone-unix-time) 0)))

(unless (and (fboundp 'current-time)
             (let ((ct (ignore-errors (current-time))))
               (and (consp ct)
                    (or (numberp (car ct))
                        (and (consp (cdr ct)) (numberp (car ct)))))))
  (defun current-time ()
    "Return current time as (HIGH LOW USEC PSEC) — Phase 6 simplified
shape that returns (T 0 0 0) where T is the Unix epoch as a single
integer.  anvil-memory only ever feeds this back into `truncate' /
`float-time' so the legacy 3-cell shape is unnecessary here."
    (list (float-time) 0 0 0)))

(unless (and (fboundp 'truncate)
             ;; If truncate is the no-op bulk stub, override with real impl.
             (not (get 'truncate 'emacs-stub-bulk)))
  (defun truncate (number &optional divisor)
    "Phase 10 (= ex-Phase 6) polyfill: integer truncation toward zero.
NUMBER may be int or float; DIVISOR optional (= NUMBER / DIVISOR)."
    (cond
     ((null number) 0)
     (divisor
      (truncate (/ number divisor)))
     ((integerp number) number)
     ((< number 0)
      (- (truncate (- number))))
     ((>= number 1)
      ;; Avoid float literals and `while' in this early bootstrap body:
      ;; standalone-reader currently segfaults while installing that shape.
      (+ 1 (truncate (- number 1))))
     (t 0)))
  (put 'truncate 'emacs-stub-bulk nil))

(provide 'emacs-time)

;;; emacs-time.el ends here
