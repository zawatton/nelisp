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
;; The census rows of `garbage-collect' count the conses a collection
;; reached through precise edges: a dropped 1000-element list that was
;; thrown last must still be counted, and must stop being counted once an
;; immediate replaces it.  (Live bytes would not do: the conservative
;; stack scan may keep a stale stack copy of the list alive either way.)
;; Without the census rows the check cannot run and says so.
;; Prints one EXIT-STASH line and exits non-zero on failure.

(defun exit-stash--conses ()
  (let ((row (assq 'conses (garbage-collect))))
    (and (consp row) (nth 2 row))))
(defun exit-stash--throw (value)
  (catch 'exit-stash (throw 'exit-stash value))
  nil)
(defun exit-stash--throw-list (n)
  (exit-stash--throw (make-list n 'exit-stash-payload))
  nil)

(let* ((n 1000) base held released ok)
  (exit-stash--throw 0)
  (setq base (exit-stash--conses))
  (if (not (integerp base))
      (progn (princ "EXIT-STASH SKIP: garbage-collect returns no census rows\n")
             (kill-emacs 1))
    (exit-stash--throw-list n)
    (setq held (- (exit-stash--conses) base))
    (exit-stash--throw 0)
    (setq released (- (exit-stash--conses) base))
    (setq ok (and (>= held n) (< released n)))
    (princ (format "EXIT-STASH thrown list of %d: %d conses held after the catch, %d after an immediate replaced it -- %s\n"
                   n held released (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))

;;; standalone-exit-stash-root-check.el ends here
