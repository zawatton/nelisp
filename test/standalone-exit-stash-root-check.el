;;; standalone-exit-stash-root-check.el --- the non-local exit stash is a GC root -*- lexical-binding: t -*-

;; Usage: nelisp [--cold-load-from BIN.cold] \
;;          --load test/standalone-exit-stash-root-check.el
;;
;; `throw' and `signal' leave the tag and value in a fixed stash that
;; outlives the exit: the catcher reads it, but nothing clears it, and the
;; evaluator saves and restores it around variable-watcher callbacks,
;; `unwind-protect' cleanups and bytecode handlers, cloning whatever it
;; holds.  The stash must therefore keep its value alive across a
;; collection; otherwise a thrown cons is freed once the catcher drops it
;; and every later save/restore bumps the refcount word of whatever reuses
;; the block (it rewrote block headers while packages loaded).
;;
;; With the conservative native-stack scan switched off (`nelisp--debug-
;; switch' 28; 29 restores it), only precise roots keep a value alive, so
;; live bytes after two collections (`nelisp--arena-stats' field 4) must
;; still count a dropped 20000-element list that was thrown last (about
;; 2 MB), and must stop counting it once an immediate replaces it.  Each
;; step is its own top-level form so no evaluator frame still holds the
;; list.  Prints one EXIT-STASH line and exits non-zero on failure.

(defun exit-stash--live ()
  (garbage-collect) (garbage-collect)
  (nth 4 (nelisp--arena-stats)))
(defun exit-stash--throw (value)
  (catch 'exit-stash (throw 'exit-stash value))
  nil)
(defun exit-stash--throw-list (n)
  (exit-stash--throw (make-list n 'exit-stash-payload))
  nil)
(defvar exit-stash--n 20000)
(defvar exit-stash--base nil)
(defvar exit-stash--held nil)
(defvar exit-stash--released nil)

(nelisp--debug-switch 28)
(exit-stash--throw 0)
(setq exit-stash--base (exit-stash--live))
(exit-stash--throw-list exit-stash--n)
(setq exit-stash--held (- (exit-stash--live) exit-stash--base))
(exit-stash--throw 0)
(setq exit-stash--released (- (exit-stash--live) exit-stash--base))
(nelisp--debug-switch 29)

(let ((ok (and (>= exit-stash--held 1048576)
               (< exit-stash--released 524288))))
  (princ (format "EXIT-STASH thrown list of %d: %d live bytes held after the catch, %d after an immediate replaced it (need >= 1048576, then < 524288) -- %s\n"
                 exit-stash--n exit-stash--held exit-stash--released
                 (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-exit-stash-root-check.el ends here
