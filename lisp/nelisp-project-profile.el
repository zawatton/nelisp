;;; nelisp-project-profile.el --- Interpreted application profiling -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Wrap selected function cells for one entry invocation. Inclusive elapsed
;; time uses the runtime's real-time clock; backwards intervals are discarded
;; and counted. Saved function objects and native direct calls bypass wrappers.

;;; Code:

(declare-function nelisp--json-encode "nelisp-standalone-build" (value))
(declare-function nelisp--write-stderr-line "nelisp-standalone-build" (value))
(declare-function json-encode-string "json" (string))

(defun nelisp-project-profile--quote (name)
  "Encode NAME as a JSON string on either substrate."
  (if (fboundp 'nelisp--json-encode)
      (nelisp--json-encode name)
    (require 'json nil t)
    (json-encode-string name)))

(defun nelisp-project-profile--wrapper (original row)
  "Return a function wrapping ORIGINAL and accumulating into ROW."
  (lambda (&rest args)
    (let ((started (float-time)) (completed nil))
      (aset row 1 (1+ (aref row 1)))
      (unwind-protect
          (prog1 (apply original args) (setq completed t))
        (let ((elapsed (- (float-time) started)))
          (if (< elapsed 0)
              (aset row 4 (1+ (aref row 4)))
            (aset row 3 (+ (aref row 3) (truncate (* elapsed 1000000))))))
        (when completed (aset row 2 (1+ (aref row 2))))))))

(defun nelisp-project-profile--report (rows completed)
  "Return JSON for ROWS and entry COMPLETED status."
  (concat "{\"schema_version\":1,\"scope\":\"function-cell-inclusive\","
          "\"clock\":\"realtime\",\"unit\":\"microseconds\",\"status\":"
          (if completed "\"ok\"" "\"error\"") ",\"functions\":["
          (mapconcat
           (lambda (row)
             (format "{\"name\":%s,\"calls\":%d,\"completed\":%d,\"elapsed_us\":%d,\"invalid_intervals\":%d}"
                     (nelisp-project-profile--quote (symbol-name (aref row 0)))
                     (aref row 1) (aref row 2) (aref row 3) (aref row 4)))
           rows ",") "]}"))

(defun nelisp-project-profile--write (report)
  "Write REPORT to stderr without mixing it into application stdout."
  (let ((line (concat "NELISP_PROFILE_V1 " report)))
    (if (fboundp 'nelisp--write-stderr-line)
        (nelisp--write-stderr-line line)
      (princ (concat line "\n") #'external-debugging-output))))

;;;###autoload
(defun nelisp-project-profile-run (entry symbols &optional reporter)
  "Run zero-argument ENTRY, profiling SYMBOLS; return its original value.
REPORTER receives one JSON string, including after a Lisp error. Function
cells are restored unless application code replaced them during the run.
Process exit and fatal native faults cannot produce a completion report."
  (let ((names (delete-dups (copy-sequence symbols))) rows installed completed)
    ;; Validate before changing any function cells. Core profiler operations
    ;; cannot themselves be wrapped without measuring the profiler recursively.
    (dolist (name names)
      (when (or (string-prefix-p "nelisp-project-profile-" (symbol-name name))
                (memq name '(float-time apply funcall fset symbol-function format
                             aset aref truncate symbol-name mapconcat concat
                             fboundp functionp eq princ + - * < 1+
                             car cdr cons list vector nth nreverse memq
                             delete-dups copy-sequence string-prefix-p
                             error require symbol-name json-encode-string)))
        (error "Cannot profile instrumentation dependency: %S" name))
      (unless (and (fboundp name) (functionp (symbol-function name)))
        (error "Profile target is not a function: %S" name)))
    (unwind-protect
        (progn
          (dolist (name names)
            (let* ((original (symbol-function name))
                   (row (vector name 0 0 0 0))
                   (wrapper (nelisp-project-profile--wrapper original row)))
              (push row rows)
              (push (list name original wrapper) installed)
              (fset name wrapper)))
          (prog1 (funcall entry) (setq completed t)))
      (dolist (record installed)
        (when (eq (symbol-function (car record)) (nth 2 record))
          (fset (car record) (nth 1 record))))
      (funcall (or reporter #'nelisp-project-profile--write)
               (nelisp-project-profile--report (nreverse rows) completed)))))

(provide 'nelisp-project-profile)
;;; nelisp-project-profile.el ends here
