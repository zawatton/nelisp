;;; nelisp-worker-sketch.el --- Phase 5-D forecast sketch -*- lexical-binding: t; -*-
;;
;; Non-compiling sketch of the anticipated shape of
;; `src/nelisp-worker.el'.  Purpose: feed the 15-audit-script.el
;; scanner with realistic call sites so the primitive gap for
;; Phase 5-D is enumerable before any real implementation.
;;
;; Every `nelisp-*' reference is internal; the scanner treats them
;; as user-defined and ignores them.  Host subr references (e.g.
;; `run-at-time', `random', `format-time-string') are what we
;; want surfaced.
;;
;; This file is NOT loaded by the real build.

;;; Code:

(defvar nelisp-worker--pools nil
  "Alist of (NAME . struct) per lane pool.")

(defvar nelisp-worker--correlation-counter 0)

(defvar nelisp-worker-classify-read-patterns '("^(buffer-\\|^(org-read-")
  "Regexp patterns routed to the read lane.")

(defvar nelisp-worker-classify-write-patterns '("^(file-\\|^(write-")
  "Regexp patterns routed to the write lane.")

(defvar nelisp-worker-classify-batch-patterns '("^(org-tangle\\|^(byte-compile-file")
  "Regexp patterns routed to the batch lane.")

(defvar nelisp-worker-batch-warmup-expressions nil)
(defvar nelisp-worker-batch-warmup-delay 2.0)
(defvar nelisp-worker-health-check-interval 30)
(defvar nelisp-worker-call-timeout 60)
(defvar nelisp-worker-latency-sample-cap 1000)

;;; Correlation id ---------------------------------------------------

(defun nelisp-worker--next-id ()
  (setq nelisp-worker--correlation-counter
        (1+ nelisp-worker--correlation-counter))
  (format "nw-%d-%d"
          (random 1000000)
          nelisp-worker--correlation-counter))

;;; Classifier -------------------------------------------------------

(defun nelisp-worker--match-any (expression patterns)
  (let ((s (format "%S" expression)))
    (cl-some (lambda (re) (string-match-p re s)) patterns)))

(defun nelisp-worker-classify (expression)
  (cond
   ((nelisp-worker--match-any expression nelisp-worker-classify-batch-patterns) :batch)
   ((nelisp-worker--match-any expression nelisp-worker-classify-read-patterns) :read)
   ((nelisp-worker--match-any expression nelisp-worker-classify-write-patterns) :write)
   (t :write)))

;;; Latency bucket ---------------------------------------------------

(defun nelisp-worker--now-ms ()
  (truncate (* 1000.0 (float-time))))

(defun nelisp-worker--percentile (samples pct)
  (let* ((sorted (sort (copy-sequence samples) '<))
         (n (length sorted))
         (idx (max 0 (min (1- n)
                          (truncate (* pct (/ n 100.0)))))))
    (and sorted (nth idx sorted))))

(defun nelisp-worker--latency-record (bucket spawn-ms wait-ms)
  (let ((spawns (plist-get bucket :spawn-samples))
        (waits  (plist-get bucket :wait-samples)))
    (plist-put bucket :spawn-samples
               (cons spawn-ms (seq-take spawns
                                        (1- nelisp-worker-latency-sample-cap))))
    (plist-put bucket :wait-samples
               (cons wait-ms (seq-take waits
                                       (1- nelisp-worker-latency-sample-cap))))))

;;; Health check -----------------------------------------------------

(defun nelisp-worker--schedule-health-check (pool)
  (run-at-time nelisp-worker-health-check-interval nil
               #'nelisp-worker--health-scan pool))

(defun nelisp-worker--health-scan (pool)
  (message "nelisp-worker: scan pool %S at %s"
           pool (format-time-string "%H:%M:%S")))

;;; Batch warmup -----------------------------------------------------

(defun nelisp-worker--maybe-warmup (worker lane)
  (when (and (eq lane :batch)
             nelisp-worker-batch-warmup-expressions)
    (run-at-time nelisp-worker-batch-warmup-delay nil
                 #'nelisp-worker--send-warmup worker)))

(defun nelisp-worker--send-warmup (worker)
  (dolist (expr nelisp-worker-batch-warmup-expressions)
    (nelisp-worker--send-line worker (prin1-to-string expr))))

(defun nelisp-worker--send-line (worker line)
  (ignore worker line))

;;; Request / response -----------------------------------------------

(defun nelisp-worker-call (expression &rest opts)
  (let* ((lane (or (plist-get opts :lane)
                   (nelisp-worker-classify expression)))
         (timeout (or (plist-get opts :timeout)
                      nelisp-worker-call-timeout))
         (id (nelisp-worker--next-id))
         (start (nelisp-worker--now-ms)))
    (ignore lane id start timeout)
    nil))

(provide 'nelisp-worker-sketch)
;;; nelisp-worker-sketch.el ends here
