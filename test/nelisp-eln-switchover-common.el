;;; nelisp-eln-switchover-common.el --- S7.7 switchover smoke helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Shared by test/nelisp-eln-switchover-driver.el and
;; test/nelisp-eln-switchover-lifecycle-driver.el (both run on the NeLisp
;; binary by test/nelisp-eln-switchover-smoke.sh).

(require 'nelisp-eln-switchover)

(defvar s77-neln (getenv "NELISP_S77_NELN"))
(defvar s77-eln-dir (getenv "NELISP_S77_ELN_DIR"))
(defvar s77-gnu-identity (getenv "NELISP_S77_GNU_IDENTITY"))

(defun s77-fail (what &rest detail)
  (princ (format "S77_FAIL %s %S\n" what detail))
  (kill-emacs 1))

(defvar s77-t0 (float-time))
(defun s77-check (name ok &rest detail)
  (when (getenv "NELISP_S77_TIMING")
    (princ (format "S77_T %s %.2f\n" name (- (float-time) s77-t0))))
  (setq s77--report nil)
  (if ok
      (princ (format "S77_%s=PASS\n" name))
    (apply #'s77-fail name detail)))

(defconst s77-inputs
  '((nelisp-s77-const)
    (nelisp-s77-bad-trunc)
    (nelisp-s77-ident 5) (nelisp-s77-ident "str") (nelisp-s77-ident (1 . 2))
    (nelisp-s77-ident nil)
    ;; Outside the object codec's argument coverage (an interned symbol
    ;; with global state): refused before native entry, answered by the
    ;; previous definition through the per-call guard, and recorded.
    (nelisp-s77-ident nelisp-s77-ident)
    (nelisp-s77-bad-abi)
    (nelisp-s77-stale)
    (nelisp-s77-choose nil) (nelisp-s77-choose t) (nelisp-s77-choose 3)
    (nelisp-s77-add1 41)
    (nelisp-gnu-identity 9) (nelisp-gnu-identity "gnu")
    (nelisp-gnu-identity nelisp-s77-ident))
  "Calls whose results must not change across the switchover.")

(defun s77-results ()
  (mapcar (lambda (call)
            (condition-case err
                (apply (car call) (cdr call))
              (error (list 'error (car err)))))
          s77-inputs))

(defun s77-private-mappings ()
  "Count distinct private-copy .eln files mapped into this process.
Reads this process's /proc/PID/maps through grep: the in-process string
search over the maps text costs seconds on the standalone binary."
  (with-temp-buffer
    (unless (eq 0 (call-process
                   "sh" nil t nil "-c"
                   (format "grep -o 'nelisp-eln-private-[0-9]*/copy-[^ ]*' /proc/%d/maps | sort -u | wc -l"
                           (emacs-pid))))
      (s77-fail "maps" (buffer-string)))
    (string-to-number (buffer-string))))

(defun s77-route (symbol)
  "Return the last recorded load route for SYMBOL."
  (let ((route nil))
    (dolist (rec (nelisp-eln-switchover-log-for symbol))
      (when (eq (plist-get rec :op) 'load)
        (setq route rec)))
    route))

(defvar s77--report nil
  "The report snapshot shared by one check's arguments (cleared per check).")
(defun s77-report (key)
  (plist-get (or s77--report
                 (setq s77--report (nelisp-eln-switchover-report)))
             key))
(defun s77-report-all () (s77-report :owners) s77--report)

(defun s77-restored-p (symbols)
  (let ((ok t))
    (dolist (s symbols)
      (let ((prev (cdr (assq s s77-previous))))
        (unless (if prev (and (fboundp s) (eq (symbol-function s) prev))
                  (not (fboundp s)))
          (setq ok nil))))
    ok))


(provide 'nelisp-eln-switchover-common)

;;; nelisp-eln-switchover-common.el ends here
