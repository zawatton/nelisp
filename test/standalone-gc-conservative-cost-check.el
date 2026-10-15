;;; standalone-gc-conservative-cost-check.el --- the conservative scan does not re-walk the heap -*- lexical-binding: t -*-

;; Usage: nelisp [--cold-load-from BIN.cold] \
;;          --load test/standalone-gc-conservative-cost-check.el
;;
;; A collection marks the precise roots and also scans the native stack
;; conservatively (Doc 152 §11.21): every word of a block a stack word
;; reaches is treated as a candidate pointer, and the blocks it finds are
;; drained the same way.  The drain pins only unmarked blocks, so once the
;; precise pass has run it walks just what the stack alone keeps alive.
;; Run before the precise pass, it re-walked most of the live heap.  The
;; cost of the scan is measured against the same collection with it
;; switched off (`nelisp--debug-switch' 28, restored by 29): the fastest of
;; five collections with the scan on may cost at most 1.25 times the
;; fastest of five with it off.
;; Prints one GC-CONSERV line and exits non-zero on failure.

(defun gc-conserv--best (n)
  (let ((best nil) (i 0))
    (while (< i n)
      (let ((t0 (float-time)))
        (garbage-collect)
        (let ((dt (- (float-time) t0)))
          (when (or (null best) (< dt best)) (setq best dt))))
      (setq i (1+ i)))
    best))
(defvar gc-conserv--on nil)
(defvar gc-conserv--off nil)

(garbage-collect)
(garbage-collect)
(setq gc-conserv--on (gc-conserv--best 5))
(nelisp--debug-switch 28)
(setq gc-conserv--off (gc-conserv--best 5))
(nelisp--debug-switch 29)

(let* ((ratio (/ gc-conserv--on (max gc-conserv--off 0.001)))
       (ok (<= ratio 1.25)))
  (princ (format "GC-CONSERV collection %.3f s with the stack scan, %.3f s without: %.2fx (limit 1.25x) -- %s\n"
                 gc-conserv--on gc-conserv--off ratio (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-gc-conservative-cost-check.el ends here
