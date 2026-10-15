;;; standalone-symname-tombstone-check.el --- gensym churn keeps symbol names cheap -*- lexical-binding: t -*-

;; Usage: nelisp [--cold-load-from BIN.cold] \
;;          --load test/standalone-symname-tombstone-check.el
;;
;; Every uninterned symbol (a macro gensym) registers a row in the
;; symbol-name sidecar table, and every collection turns the rows of the
;; dead ones into tombstones.  The table must drop those tombstones: if it
;; keeps them, its empty slots run out, every new name probes the whole
;; table, and symbol creation slows down with each collection.
;; Over 40 rounds of 3000 named gensyms followed by a collection, no round
;; may cost more than 4x the first one.  A failing table degrades round by
;; round, so the check stops at the first round over the limit.
;; Prints one SYMNAME line and exits non-zero on failure.

(defun symname-tomb--round (n)
  (let ((t0 (float-time)) (i 0))
    (while (< i n)
      (symbol-name (make-symbol "symname-tomb-gensym"))
      (setq i (1+ i)))
    (- (float-time) t0)))

(garbage-collect)
(let* ((base (min (symname-tomb--round 3000) (symname-tomb--round 3000)))
       (limit (* 4 (max base 0.005)))
       (worst 0.0) (worst-round 0) (round 0) (ok t))
  (while (and ok (< round 40))
    (garbage-collect)
    (let ((cost (symname-tomb--round 3000)))
      (when (> cost worst) (setq worst cost worst-round round))
      (when (> cost limit) (setq ok nil)))
    (setq round (1+ round)))
  (princ (format "SYMNAME %d rounds of 3000 gensyms: worst %.3f s (round %d) vs first %.3f s (limit 4x)\n"
                 round worst worst-round base))
  (princ (format "SYMNAME tombstone %s\n" (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-symname-tombstone-check.el ends here
