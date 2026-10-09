;;; nelisp-startup-trace.el --- Opt-in standalone form timing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Load after nelisp-standalone-build, before building a dedicated trace binary.
;; The normal runtime and proof checks are unchanged; stderr carries binary
;; records consumed by tools/ai/nelisp-startup-trace.py. Never use this binary
;; for acceptance timing. Each event identifies its source and form end offset.
(require 'cl-lib)
(defun nelisp-startup-trace--instrument (forms)
  "Instrument the existing evaluation boundary in native FORMS."
  (let ((hits 0))
    (cl-labels
        ((event (kind)
           `(let* ((trace-buffer (alloc-bytes 40 8))
                   (trace-length ,(if (= kind 0) '(if (< (nl_bi_strlen src) 96) (nl_bi_strlen src) 96) 0)))
              (seq (ptr-write-u64 trace-buffer 0 5641975213432120625)
                   (ptr-write-u64 trace-buffer 8 ,kind)
                   (ptr-write-u64 trace-buffer 16 (nl_bi_strptr src))
                   (ptr-write-u64 trace-buffer 24 (ptr-read-u64 cursor 8))
                   (ptr-write-u64 trace-buffer 32 trace-length)
                   (nl_os_write_stderr trace-buffer 40)
                   (if (> trace-length 0)
                       (nl_os_write_stderr (nl_bi_strptr src) trace-length) 0))))
         (walk (node)
           (cond
            ((and (consp node) (eq (car node) 'defun)
                  (eq (cadr node) 'nl_driver_eval_with_recorded_roots))
             (setq hits (1+ hits))
             (let ((body (cdddr node)))
               `(defun ,(cadr node) ,(caddr node)
                  (seq ,(event 0)
                       (let* ((trace-result (seq ,@body)))
                         (seq ,(event 1) trace-result))))))
            ((consp node) (mapcar #'walk node))
            (t node))))
      (let ((result (walk forms)))
        (unless (= hits 1) (error "Startup trace boundary drift: %d" hits))
        result))))
(setq nelisp-standalone--applyfn-bf-helpers
      (nelisp-startup-trace--instrument nelisp-standalone--applyfn-bf-helpers))
