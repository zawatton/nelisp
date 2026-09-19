;;; nl-ns-cmd.el --- Batch entry points for nl-ns -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Example:
;;
;;   emacs -Q --batch -L packages/nl-prelude/src -L packages/nl-ns/src \
;;     -l packages/nl-ns/src/nl-ns-cmd.el -- \
;;     --baseline packages/nl-ns/baseline/emacs-30.2.el \
;;     --fail-severity 3 path/to/file.el

;;; Code:

(require 'nl-ns)

(defun nl-ns-cmd--usage ()
  "Return command-line usage text."
  "nl-ns-cmd: [--baseline FILE] [--deps] [--fail-severity N] FILE...\n")

(defun nl-ns-cmd--parse-args (args)
  "Parse batch ARGS and return plist."
  (let ((baseline nil)
        (deps nil)
        (fail-severity nil)
        (paths nil))
    (while args
      (let ((arg (car args)))
        (cond
         ((string= arg "--baseline")
          (setq args (cdr args))
          (setq baseline (car args)))
         ((string= arg "--deps")
          (setq deps t))
         ((string= arg "--fail-severity")
          (setq args (cdr args))
          (setq fail-severity (string-to-number (car args))))
         ((string= arg "--help")
          (princ (nl-ns-cmd--usage))
          (kill-emacs 0))
         (t
          (setq paths (cons arg paths)))))
      (setq args (cdr args)))
    (unless paths
      (error "%s" (nl-ns-cmd--usage)))
    (list :baseline baseline
          :deps deps
          :fail-severity fail-severity
          :paths (nreverse paths))))

(defun nl-ns-cmd-run ()
  "Run nl-ns from `command-line-args-left'."
  (let* ((options (nl-ns-cmd--parse-args command-line-args-left))
         (baseline (plist-get options :baseline))
         (findings (nl-ns-check-files (plist-get options :paths)
                                      (plist-get options :deps)
                                      baseline))
         (threshold (plist-get options :fail-severity)))
    (princ (nl-ns-report findings baseline))
    (kill-emacs
     (if (and threshold findings
              (<= (nl-ns-report-max-severity findings) threshold))
         1
       0))))

(provide 'nl-ns-cmd)

;;; nl-ns-cmd.el ends here
