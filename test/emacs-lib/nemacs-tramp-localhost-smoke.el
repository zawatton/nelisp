;;; nemacs-tramp-localhost-smoke.el --- real-ssh localhost Tramp smoke  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 37 (Tramp ssh-only lane, task #16), implementation plan section
;; 4.2.  Optional real-host verification: connects to `localhost' over a
;; genuine `ssh' binary (no fakes).  Requires passwordless key auth to
;; be configured for the current user (see the manual checklist in
;; `docs/design/11-remaining-work-roadmap.org' M11 and
;; `docs/design/37-tramp-ssh-lane.org').  When that is not set up --
;; the common case on a fresh checkout or in CI -- this test skips
;; cleanly via `ert-skip' rather than failing, exactly mirroring the
;; hermetic stub-ssh test's coverage (M2 read, M3 write round-trip, M4
;; process-file) against a genuine sshd instead of the fake `ssh'
;; double.
;;
;; Run via `make tramp-localhost-smoke', or directly:
;;   emacs --batch -L src -L test -l test/nemacs-tramp-localhost-smoke.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'emacs-file-name-handler)
(require 'nemacs-tramp)

(defun nemacs-tramp-localhost-smoke--ssh-ready-p ()
  "Return non-nil when passwordless `ssh localhost' works right now."
  (and (executable-find "ssh")
       (= 0 (call-process "ssh" nil nil nil
                          "-o" "BatchMode=yes" "-o" "ConnectTimeout=3"
                          "localhost" "true"))))

(defmacro nemacs-tramp-localhost-smoke--skip-unless-ready (&rest body)
  "Run BODY only when `nemacs-tramp-localhost-smoke--ssh-ready-p'."
  (declare (indent 0) (debug (body)))
  `(if (nemacs-tramp-localhost-smoke--ssh-ready-p)
       (progn ,@body)
     (ert-skip "passwordless `ssh -o BatchMode=yes localhost true' is not set up")))

(defun nemacs-tramp-localhost-smoke--remote (path)
  "Return a `/ssh:localhost:PATH' name."
  (concat "/ssh:localhost:" path))

(ert-deftest nemacs-tramp-localhost-smoke/round-trip ()
  (nemacs-tramp-localhost-smoke--skip-unless-ready
    (nemacs-tramp-setup)
    (let* ((local (make-temp-file "nemacs-tramp-localhost-smoke-"))
           (remote (nemacs-tramp-localhost-smoke--remote local))
           (payload "real-ssh localhost round-trip\n"))
      (unwind-protect
          (progn
            ;; M2: read.
            (write-region payload nil local)
            (should (file-exists-p remote))
            (with-temp-buffer
              (insert-file-contents remote)
              (should (equal (buffer-string) payload)))
            ;; M3: write, then verify with a plain local read.
            (let ((payload2 "real-ssh localhost write-back\n"))
              (with-temp-buffer
                (insert payload2)
                (write-region (point-min) (point-max) remote))
              (with-temp-buffer
                (insert-file-contents local)
                (should (equal (buffer-string) payload2))))
            ;; M4: process-file.
            (let ((default-directory (nemacs-tramp-localhost-smoke--remote "/tmp/")))
              (should (= (process-file "true" nil nil nil) 0))
              (should (= (process-file "false" nil nil nil) 1))))
        (ignore-errors (delete-file local))))))

(provide 'nemacs-tramp-localhost-smoke)

;;; nemacs-tramp-localhost-smoke.el ends here
