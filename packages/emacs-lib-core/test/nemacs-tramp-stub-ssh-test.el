;;; nemacs-tramp-stub-ssh-test.el --- hermetic Tramp ssh-lane ERT  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 37 (Tramp ssh-only lane, task #16), implementation plan section
;; 4.1.  A hermetic, sshd-independent round-trip test: a fake `ssh'
;; executable is put ahead of `exec-path'/PATH that ignores every ssh(1)
;; argument and execs a real local POSIX shell in its place, so
;; `tramp-sh' negotiates with a genuine shell without ever touching the
;; network.  This works because the default "ssh" Tramp method opens one
;; persistent shell session over ssh's stdin/stdout (no trailing command
;; in its `tramp-login-args'; see `tramp-sh.el') and Tramp itself sends
;; its own PS1/marker synchronisation over that pipe -- it does not
;; depend on the real ssh(1) wire protocol at all once a shell is
;; connected on the other end.  Files stay under `tramp-copy-size-limit'
;; (10KB) so Tramp uses its inline (base64-over-the-shell) transfer and
;; never shells out to a real `scp'.
;;
;; Discovery note: the fake shell MUST run interactive (`-i') with
;; stderr merged onto stdout (`2>&1').  `tramp-actions-before-shell'
;; makes `tramp-open-connection-setup-interactive-shell' wait to observe
;; a shell prompt (`shell-prompt-pattern'/`tramp-shell-prompt-pattern')
;; before it sends anything at all; a shell only emits PS1 when it
;; considers itself interactive, and POSIX shells conventionally write
;; that prompt to stderr.  Without both flags this test hangs
;; indefinitely (`tramp-process-actions' busy-polls
;; `accept-process-output' waiting for a prompt that never arrives, with
;; no bound -- confirmed via `strace' during this test's development:
;; the spawned shell sits blocked on `read(0, ...)' forever because
;; Tramp never gets past "waiting for the shell to come up" to send its
;; first command).
;;
;; Covers, in one hermetic pass:
;;   - M1: the vendored Tramp load chain + non-remote passthrough.
;;   - M1: the ssh/scp-only method guard (`nemacs-tramp-unsupported-method').
;;   - M2: `insert-file-contents' / `file-exists-p' / `file-attributes'
;;     read a remote file through the fake ssh.
;;   - M3: `write-region' round-trip: a buffer is written back through
;;     the fake ssh and then read back via a plain LOCAL read (bypassing
;;     Tramp) to prove the bytes really landed on disk.
;;   - M4: `process-file' runs "true"/"false" through the fake ssh and
;;     returns the real exit status, plus stdout capture from `echo'.
;;
;; Run via `make tramp-stub-smoke', or directly:
;;   emacs --batch -L src -L test -l test/nemacs-tramp-stub-ssh-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'emacs-file-name-handler)
(require 'nemacs-tramp)

(defvar nemacs-tramp-stub-ssh-test--bin-dir nil
  "Temp directory holding the fake `ssh' executable for this run.")

(defconst nemacs-tramp-stub-ssh-test--fake-ssh "\
#!/bin/sh
# Hermetic Tramp test double (Doc 37): ignore all ssh(1) flags/host
# arguments and exec a real local shell in their place.  The default
# \"ssh\" Tramp method opens ssh with no trailing command (a persistent
# login-less session), so this is indistinguishable to it from a real
# ssh connection to a real POSIX host -- with one wrinkle: `tramp-sh'
# waits to see a shell prompt (`shell-prompt-pattern'/
# `tramp-shell-prompt-pattern', both in `tramp-actions-before-shell')
# before it sends its own setup commands, and a shell only emits PS1 in
# *interactive* mode.  `-i' turns that on; `2>&1' matters because the
# interactive prompt is conventionally written to stderr, and Tramp only
# reads the channel it spawned the process on (stdout).  Without both,
# `tramp-open-connection-setup-interactive-shell' spins in
# `tramp-process-actions' forever waiting for a prompt that never
# arrives (found empirically -- see the Doc 37 discovery note this test
# links to in its Commentary).
exec /bin/sh -i 2>&1
"
  "Source of the fake `ssh' executable installed by this test's setup.")

(defconst nemacs-tramp-stub-ssh-test--host "stub-host"
  "Fake host name; the fake `ssh' ignores it and execs a local shell.")

(defun nemacs-tramp-stub-ssh-test--remote (path)
  "Return a `/ssh:HOST:PATH' name for the hermetic fake host."
  (format "/ssh:%s:%s" nemacs-tramp-stub-ssh-test--host path))

(defun nemacs-tramp-stub-ssh-test--setup ()
  "Install a fake `ssh' ahead of `exec-path'/PATH and configure Tramp."
  (unless nemacs-tramp-stub-ssh-test--bin-dir
    (let* ((dir (make-temp-file "nemacs-tramp-stub-ssh-" t))
           (ssh (expand-file-name "ssh" dir)))
      (with-temp-file ssh
        (insert nemacs-tramp-stub-ssh-test--fake-ssh))
      (set-file-modes ssh #o755)
      (setq nemacs-tramp-stub-ssh-test--bin-dir dir)
      (setq exec-path (cons dir exec-path))
      (setenv "PATH" (concat dir ":" (or (getenv "PATH") "")))))
  (nemacs-tramp-setup))

;;;; M1 -- load chain, non-remote passthrough, unsupported-method guard

(ert-deftest nemacs-tramp-stub-ssh-test/m1-load-chain-and-passthrough ()
  (nemacs-tramp-stub-ssh-test--setup)
  (should (featurep 'tramp))
  (should (featurep 'tramp-sh))
  (should (memq #'tramp-file-name-handler
                (mapcar #'cdr file-name-handler-alist)))
  (should (tramp-tramp-file-p (nemacs-tramp-stub-ssh-test--remote "/tmp/x")))
  (should (equal (file-remote-p (nemacs-tramp-stub-ssh-test--remote "/tmp/x"))
                (format "/ssh:%s:" nemacs-tramp-stub-ssh-test--host)))
  ;; Non-remote passthrough: local file ops are unaffected by any of the
  ;; handler-dispatch wiring this lane installs.
  (should (file-exists-p "/etc/passwd"))
  (should-not (file-remote-p "/etc/passwd")))

(ert-deftest nemacs-tramp-stub-ssh-test/m1-unsupported-method-signals ()
  (nemacs-tramp-stub-ssh-test--setup)
  (should-error
   (tramp-dissect-file-name
    (format "/smb:%s:/tmp/x" nemacs-tramp-stub-ssh-test--host))
   :type 'nemacs-tramp-unsupported-method)
  ;; "scp" is in the supported set and must NOT be blocked.
  (should (tramp-dissect-file-name
          (format "/scp:%s:/tmp/x" nemacs-tramp-stub-ssh-test--host))))

;;;; M2 -- read a remote file through the fake ssh

(ert-deftest nemacs-tramp-stub-ssh-test/m2-read-remote-file ()
  (nemacs-tramp-stub-ssh-test--setup)
  (let* ((local (make-temp-file "nemacs-tramp-stub-ssh-src"))
         (payload "hello from the stub-ssh lane\n"))
    (unwind-protect
        (progn
          (write-region payload nil local)
          (let ((remote (nemacs-tramp-stub-ssh-test--remote local)))
            (should (file-exists-p remote))
            (should (file-regular-p remote))
            (should (> (file-attribute-size (file-attributes remote)) 0))
            (with-temp-buffer
              (insert-file-contents remote)
              (should (equal (buffer-string) payload)))))
      (delete-file local))))

;;;; M3 -- write a remote file, then read it back locally

(ert-deftest nemacs-tramp-stub-ssh-test/m3-write-remote-file-round-trip ()
  (nemacs-tramp-stub-ssh-test--setup)
  (let* ((local (make-temp-file "nemacs-tramp-stub-ssh-dst"))
         (payload "written through the stub-ssh lane\n")
         (remote (nemacs-tramp-stub-ssh-test--remote local)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert payload)
            (write-region (point-min) (point-max) remote))
          ;; Read back via a plain LOCAL read (bypassing Tramp entirely)
          ;; to prove the bytes really landed on disk through the fake
          ;; ssh channel, not merely in some Tramp-side cache.
          (with-temp-buffer
            (insert-file-contents local)
            (should (equal (buffer-string) payload))))
      (delete-file local))))

;;;; M4 -- process-file exit status + stdout capture

(ert-deftest nemacs-tramp-stub-ssh-test/m4-process-file-exit-status-and-stdout ()
  (nemacs-tramp-stub-ssh-test--setup)
  (let ((default-directory (nemacs-tramp-stub-ssh-test--remote "/tmp/")))
    (should (= (process-file "true" nil nil nil) 0))
    (should (= (process-file "false" nil nil nil) 1))
    (with-temp-buffer
      (process-file "echo" nil t nil "stub-ssh-stdout")
      (should (string-match-p "stub-ssh-stdout" (buffer-string))))))

(provide 'nemacs-tramp-stub-ssh-test)

;;; nemacs-tramp-stub-ssh-test.el ends here
