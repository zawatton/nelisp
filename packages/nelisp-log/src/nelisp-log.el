;;; nelisp-log.el --- Leveled logging with pluggable sinks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small leveled logger for NeLisp, usable on host Emacs and the
;; standalone reader:
;;
;;   - five levels: `trace' < `debug' < `info' < `warn' < `error'
;;   - a default formatter: "[LEVEL] message"
;;   - pluggable sinks: a sink is just a function of (LEVEL FORMATTED);
;;     `nelisp-log-sink-stderr' is the built-in standard-error sink, and
;;     any other function of that signature is a "function sink" --
;;     there is no separate wrapper type to construct one.
;;
;; Global mutable state is exactly one variable, `nelisp-log-default-logger',
;; which callers may replace wholesale (`nelisp-log-set-default-logger')
;; or mutate in place (`nelisp-log-set-level', or `setf' on any accessor).
;; Every other logger is an ordinary value the caller owns and threads
;; through `nelisp-log-log' itself.
;;
;; Standalone-safety: the default formatter deliberately does not stamp a
;; timestamp.  Time formatting (`current-time' / `format-time-string')
;; works in this tree's standalone runtime too, but this package's own
;; hot path -- format, then call one sink function -- has no reason to
;; depend on it.  Reach for a custom `:formatter' when a timestamp (or
;; JSON, or anything else) is wanted; see `nelisp-log-default-formatter'.
;; The built-in stderr sink uses `message', which is standalone-safe (used
;; throughout src/ and lisp/) and, in batch/standalone execution, writes
;; to standard error rather than standard output -- so log lines do not
;; land in a program's own stdout protocol traffic.

;;; Code:

(require 'cl-lib)

;; Named `nelisp-log-invalid-level' rather than the more usual
;; `nelisp-log-error', because this package's own `nelisp-log-error'
;; function (log at level `error') already claims that name for
;; something unrelated -- signaling and log-at-error-level are two
;; different operations and must not share a symbol.
(define-error 'nelisp-log-invalid-level "NeLisp log: invalid level")

(defconst nelisp-log-levels '(trace debug info warn error)
  "Severity levels, lowest first.  Each is strictly louder than the last.")

(defun nelisp-log--level-index (level)
  "Return the position of LEVEL in `nelisp-log-levels'.
Signal `nelisp-log-invalid-level' when LEVEL is not one of them."
  (let ((levels nelisp-log-levels) (index 0))
    (catch 'found
      (while levels
        (when (eq (car levels) level) (throw 'found index))
        (setq levels (cdr levels) index (1+ index)))
      (signal 'nelisp-log-invalid-level
              (list (format "unknown log level %S; must be one of %S"
                            level nelisp-log-levels)
                    level)))))

;;;###autoload
(defun nelisp-log-default-formatter (level format-string args)
  "Return LEVEL and FORMAT-STRING/ARGS rendered as \"[LEVEL] message\".
The default formatter for a logger created without an explicit
`:formatter'.  See this file's Commentary for why it carries no
timestamp."
  (format "[%s] %s" (upcase (symbol-name level)) (apply #'format format-string args)))

;;;###autoload
(defun nelisp-log-sink-stderr (level formatted)
  "Write FORMATTED to standard error via `message'.
LEVEL is accepted but unused, so a sink signature swap for
level-based routing later does not require touching call sites."
  (ignore level)
  (message "%s" formatted))

(cl-defstruct (nelisp-log-logger
               (:constructor nelisp-log--make-logger)
               (:copier nil))
  "A logger: a minimum LEVEL to emit, a SINK, and a FORMATTER.
Construct with `nelisp-log-make-logger', never this constructor directly,
so the level is validated and the defaults are applied consistently."
  (level 'info)
  (sink #'nelisp-log-sink-stderr)
  (formatter #'nelisp-log-default-formatter))

;;;###autoload
(cl-defun nelisp-log-make-logger (&key (level 'info)
                                        (sink #'nelisp-log-sink-stderr)
                                        (formatter #'nelisp-log-default-formatter))
  "Return a new logger.

LEVEL is the minimum severity that reaches SINK, one of `nelisp-log-levels'
\(default `info'\).  SINK is a function of (LEVEL FORMATTED-STRING),
called for each message that passes the level filter (default
`nelisp-log-sink-stderr').  FORMATTER is a function of (LEVEL
FORMAT-STRING ARGS) returning the string handed to SINK (default
`nelisp-log-default-formatter').

Signals `nelisp-log-invalid-level' when LEVEL is not a recognised level."
  (nelisp-log--level-index level)
  (nelisp-log--make-logger :level level :sink sink :formatter formatter))

;;;###autoload
(defvar nelisp-log-default-logger (nelisp-log-make-logger)
  "The logger every `nelisp-log-LEVEL' convenience function uses.

The one piece of global mutable state this package keeps.  Replace it
wholesale with `nelisp-log-set-default-logger', or mutate a field in
place, e.g. `(nelisp-log-set-level \\='debug)' or
`(setf (nelisp-log-logger-sink nelisp-log-default-logger) SINK)'.")

;;;###autoload
(defun nelisp-log-set-default-logger (logger)
  "Replace `nelisp-log-default-logger' with LOGGER."
  (unless (nelisp-log-logger-p logger)
    (signal 'wrong-type-argument (list 'nelisp-log-logger-p logger)))
  (setq nelisp-log-default-logger logger))

;;;###autoload
(defun nelisp-log-set-level (level &optional logger)
  "Set LOGGER's (default `nelisp-log-default-logger') minimum LEVEL."
  (nelisp-log--level-index level)
  (setf (nelisp-log-logger-level (or logger nelisp-log-default-logger)) level))

;;;###autoload
(defun nelisp-log-log (logger level format-string &rest args)
  "Emit a LEVEL message on LOGGER, if LEVEL is at or above its threshold.

FORMAT-STRING and ARGS are passed through LOGGER's formatter exactly as
given, without being evaluated or interpolated by this function first.
Return the formatted string when the message was emitted, nil when it
was filtered out by LEVEL."
  (when (>= (nelisp-log--level-index level)
            (nelisp-log--level-index (nelisp-log-logger-level logger)))
    (let ((formatted (funcall (nelisp-log-logger-formatter logger)
                               level format-string args)))
      (funcall (nelisp-log-logger-sink logger) level formatted)
      formatted)))

;;;###autoload
(defun nelisp-log-trace (format-string &rest args)
  "Log FORMAT-STRING/ARGS at level `trace' on the default logger."
  (apply #'nelisp-log-log nelisp-log-default-logger 'trace format-string args))

;;;###autoload
(defun nelisp-log-debug (format-string &rest args)
  "Log FORMAT-STRING/ARGS at level `debug' on the default logger."
  (apply #'nelisp-log-log nelisp-log-default-logger 'debug format-string args))

;;;###autoload
(defun nelisp-log-info (format-string &rest args)
  "Log FORMAT-STRING/ARGS at level `info' on the default logger."
  (apply #'nelisp-log-log nelisp-log-default-logger 'info format-string args))

;;;###autoload
(defun nelisp-log-warn (format-string &rest args)
  "Log FORMAT-STRING/ARGS at level `warn' on the default logger."
  (apply #'nelisp-log-log nelisp-log-default-logger 'warn format-string args))

;;;###autoload
(defun nelisp-log-error (format-string &rest args)
  "Log FORMAT-STRING/ARGS at level `error' on the default logger.
Only logs; does not itself signal a Lisp error."
  (apply #'nelisp-log-log nelisp-log-default-logger 'error format-string args))

(provide 'nelisp-log)

;;; nelisp-log.el ends here
