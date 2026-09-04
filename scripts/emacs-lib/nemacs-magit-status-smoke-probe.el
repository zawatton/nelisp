;;; nemacs-magit-status-smoke-probe.el --- Task #17 (M2) proof form  -*- lexical-binding: t; -*-

;; Evaluated as a single `exec-runtime-image' FORM against
;; `build/nemacs-magit-runtime.nlri' (the M1 magit runtime image) by the
;; `magit-status-smoke' Makefile target.  Reads the fixture repo path from
;; the NEMACS_MAGIT_FIXTURE_DIR environment variable instead of a baked-in
;; literal so this file needs no per-invocation templating.
;;
;; This is the honest in-session re-run of Doc 33's replay proofs
;; (tmp-diag/proof-20260703-magit-status-buffer.el and
;; proof-20260703-magit-git-exec.el), generalized to a reusable fixture and
;; with the additional M2 completion-condition conjuncts from the approved
;; plan: `magit-root-section' has non-empty children, and
;; `magit-section-forward' actually advances `magit-current-section'.
;;
;; Prints exactly two lines:
;;   MAGIT-STATUS-BUFFER PASS|FAIL
;;   MAGIT-GIT-EXEC PASS|FAIL

(let* ((fixture (file-name-as-directory (getenv "NEMACS_MAGIT_FIXTURE_DIR"))))
  (let ((default-directory fixture))
    (magit-status-setup-buffer default-directory))
  (let ((buf (magit-get-mode-buffer 'magit-status-mode)))
    (if (bufferp buf)
        (with-current-buffer buf
          (nelisp--write-stdout-bytes
           (if (and (eq major-mode 'magit-status-mode)
                    (> (buffer-size) 0)
                    (get-text-property (point-min) 'magit-section)
                    (let ((children (oref magit-root-section children)))
                      (and children (> (length children) 0)))
                    (let ((sec0 (magit-current-section)))
                      (magit-section-forward)
                      (not (eq sec0 (magit-current-section)))))
               "MAGIT-STATUS-BUFFER PASS\n"
             "MAGIT-STATUS-BUFFER FAIL\n"))
          (nelisp--write-stdout-bytes
           (if (and (stringp (magit-toplevel))
                    (equal (file-name-as-directory (magit-toplevel)) fixture)
                    (let ((head (magit-git-string "rev-parse" "HEAD")))
                      (and (stringp head) (= (length head) 40))))
               "MAGIT-GIT-EXEC PASS\n"
             "MAGIT-GIT-EXEC FAIL\n")))
      (progn
        (nelisp--write-stdout-bytes "MAGIT-STATUS-BUFFER FAIL\n")
        (nelisp--write-stdout-bytes "MAGIT-GIT-EXEC FAIL\n")))))

;;; nemacs-magit-status-smoke-probe.el ends here
