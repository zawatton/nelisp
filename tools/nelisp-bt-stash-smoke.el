;;; nelisp-bt-stash-smoke.el --- Doc 214 S1.4 per-thread signal stash -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S1.4 acceptance.  Two clone(2) eval workers signal, catch and
;; throw concurrently, 20000 times each, with different error data and
;; different catch tags.  Before Doc 214 the signal/throw flag, TAG and VAL
;; were one process-global stash, so one worker's `condition-case' could
;; read or clear the other's pending signal.  With per-thread blocks each
;; worker must see only its own error data and its own throw value: both
;; counts must be exactly 20000.  Prints BT-STASH-PASS on success.
;;
;; The worker bodies bind `nelisp--last-signal-data' locally: the
;; condition-case path re-stashes into that variable, and a worker may not
;; write the shared globals mirror (Doc 199 Tier 3b).

;;; Code:

(defun nl-bt-stash-smoke--form (n)
  "Return a worker form that signals and throws with datum N."
  `(let ((nelisp--last-signal-data nil)
         (i 0) (ok 0))
     (while (< i 20000)
       (when (and (eq (condition-case e
                          (signal 'error (list ,n))
                        (error (cadr e)))
                      ,n)
                  (eq (catch ',(intern (format "nl-bt-tag-%d" n))
                        (throw ',(intern (format "nl-bt-tag-%d" n)) ,n))
                      ,n))
         (setq ok (1+ ok)))
       (setq i (1+ i)))
     ok))

(if (not (member system-configuration
              '("x86_64-pc-linux-gnu" "x86_64-pc-windows-msvc")))
    (princ "BT-STASH-SKIP no native threads on this target\n")
  (let* ((shared (nelisp-thread-shared-alloc 64))
         (r0 shared) (r1 (+ shared 8)) (done (+ shared 16))
         (section nil))
    (unwind-protect
        (progn
          (unless (= (nelisp-thread-gc-inhibit 1) 1)
            (error "parallel section did not begin"))
          (setq section t)
          (unless (and (> (nelisp-thread-spawn
                           2 0 (nl-bt-stash-smoke--form 101) r0 done)
                          0)
                       (> (nelisp-thread-spawn
                           2 0 (nl-bt-stash-smoke--form 202) r1 done)
                          0))
            (error "worker spawn failed"))
          (nelisp-thread-join done 2)
          (let ((a (nelisp-thread-atomic-read r0))
                (b (nelisp-thread-atomic-read r1)))
            (nelisp-thread-gc-inhibit 0)
            (setq section nil)
            (princ (format "BT-STASH counts %S %S\n" a b))
            (if (and (= a 20000) (= b 20000))
                (princ "BT-STASH-PASS\n")
              (princ "BT-STASH-FAIL\n"))))
      (when section (nelisp-thread-gc-inhibit 0)))))

;;; nelisp-bt-stash-smoke.el ends here
