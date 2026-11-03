;;; emacs-pipe-process-test.el --- ERT for pipe-async filter dispatch (Doc 06 C1)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The real pipe I/O + eventloop dispatch needs `nl-ffi-call' (absent under host
;; Emacs) and is verified on the binary by
;; `test/emacs-pipe-process-binary-verify.el'.  Here we host-test the two pieces
;; that do not need the FFI: the maphash-independent fd enumeration (the bug C1
;; uncovered — the standalone reader's `maphash' never iterates) and the
;; create-time fd validation.

;;; Code:

(require 'ert)
(require 'emacs-process-events)
(require 'emacs-pipe-process)

(ert-deftest emacs-pipe-process-test/all-fds-without-maphash ()
  "`--all-fds' enumerates live process fds from the all-list (not via the
broken-on-standalone `maphash'), and excludes closed processes (Doc 06 C1)."
  (let ((emacs-process-events--all nil))
    (let ((live (emacs-process-events--make-vec
                 "live" 5 'pipe-process 'run nil nil nil nil nil -1 1))
          (dead (emacs-process-events--make-vec
                 "dead" 7 'pipe-process 'closed nil nil nil nil nil -1 2))
          (neg  (emacs-process-events--make-vec
                 "neg" -1 'pipe-process 'run nil nil nil nil nil -1 3)))
      (push live emacs-process-events--all)
      (push dead emacs-process-events--all)
      (push neg emacs-process-events--all)
      (let ((fds (emacs-process-events--all-fds)))
        (should (member 5 fds))         ; live → included
        (should-not (member 7 fds))     ; closed → excluded
        (should-not (member -1 fds)))))) ; invalid fd → excluded

(ert-deftest emacs-pipe-process-test/create-validates-fd ()
  "`emacs-pipe-process-create' rejects a missing / invalid read fd before it
touches the FFI (so the check is exercisable under host Emacs)."
  (should-error (emacs-pipe-process-create :name "x" :read-fd -1))
  (should-error (emacs-pipe-process-create :name "x"))
  (should-error (emacs-pipe-process-create :name "x" :read-fd "nope")))

(provide 'emacs-pipe-process-test)
;;; emacs-pipe-process-test.el ends here
