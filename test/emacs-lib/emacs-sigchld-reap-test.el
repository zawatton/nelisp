;;; emacs-sigchld-reap-test.el --- ERT for SIGCHLD-fallback reaping (Doc 06 C2)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The real fork + wait4 reap is verified on the binary by
;; `test/emacs-sigchld-reap-binary-verify.el'.  Here we host-test the FFI-free
;; pieces: the wait(2) status decoding (pure bit math, matches the kernel /
;; glibc WIFEXITED / WEXITSTATUS / WTERMSIG macros) and the by-pid lookup.

;;; Code:

(require 'ert)
(require 'emacs-process-events)

(ert-deftest emacs-sigchld-reap-test/wait-status-decode ()
  "WIFEXITED / WEXITSTATUS / WTERMSIG decoding of a wait(2) status word."
  ;; Normal exit with code 0: status == 0.
  (should (emacs-process-events--wait-exited-p 0))
  (should (= 0 (emacs-process-events--wait-exit-code 0)))
  ;; Normal exit with code 42: status == (42 << 8).
  (let ((st (ash 42 8)))
    (should (emacs-process-events--wait-exited-p st))
    (should (= 42 (emacs-process-events--wait-exit-code st)))
    (should (= 0 (emacs-process-events--wait-signal st))))
  ;; Killed by signal 9 (low 7 bits = signal, no core bit): not exited.
  (should-not (emacs-process-events--wait-exited-p 9))
  (should (= 9 (emacs-process-events--wait-signal 9))))

(ert-deftest emacs-sigchld-reap-test/lookup-by-pid ()
  "`--lookup-by-pid' matches a process by its plist `:pid' (scanning the
all-list, since the standalone reader's `maphash' does not iterate)."
  (let ((emacs-process-events--all nil))
    (let ((a (emacs-process-events--make-vec
              "a" 5 'pipe-process 'run nil nil nil '(:pid 1234) nil -1 1))
          (b (emacs-process-events--make-vec
              "b" 6 'pipe-process 'run nil nil nil '(:pid 5678) nil -1 2)))
      (push a emacs-process-events--all)
      (push b emacs-process-events--all)
      (should (eq a (emacs-process-events--lookup-by-pid 1234)))
      (should (eq b (emacs-process-events--lookup-by-pid 5678)))
      (should-not (emacs-process-events--lookup-by-pid 9999)))))

(provide 'emacs-sigchld-reap-test)
;;; emacs-sigchld-reap-test.el ends here
