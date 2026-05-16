;;; emacs-time.el --- Time + truncate polyfills for NeLisp standalone  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 10 — extracted from `emacs-stub.el' (= the Phase 6
;; write-path polyfill).  Wraps the build-tool builtins
;; `nl-current-unix-time' / `nl-secure-hash' (=
;; `bi_nl_current_unix_time' / `bi_nl_secure_hash' in
;; `build-tool/eval/builtins.rs').
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
    (ignore time-value)
    (if (fboundp 'nl-current-unix-time)
        (nl-current-unix-time)
      0)))

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
             (let ((t1 (truncate 3.7)))
               (and (integerp t1) (= t1 3))))
  (defun truncate (number &optional divisor)
    "Phase 10 (= ex-Phase 6) polyfill: integer truncation toward zero.
NUMBER may be int or float; DIVISOR optional (= NUMBER / DIVISOR)."
    (cond
     ((null number) 0)
     (divisor
      (truncate (/ number divisor)))
     ((integerp number) number)
     ((floatp number)
      (let ((n (if (>= number 0)
                   (- number 0.0)
                 (- 0.0 number)))
            (sign (if (>= number 0) 1 -1)))
        ;; floor-by-subtraction (no `floor' builtin available).  Adequate
        ;; for the timestamp range we care about (< 2^53 seconds).
        (let ((i 0))
          (while (>= n 1.0)
            (setq n (- n 1.0))
            (setq i (+ i 1)))
          (* sign i))))
     (t 0))))

(provide 'emacs-time)

;;; emacs-time.el ends here
