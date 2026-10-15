;;; standalone-gc-pressure-check.el --- nelisp--gc-pressure tracks the next collection -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-gc-pressure-check.el
;;
;; `nelisp--gc-pressure' (native under AGENTS.md clause 2, GC entry) is what
;; an idle loop reads to collect between inputs rather than in the middle of
;; the next command.  Checked here:
;;
;; - debt: it reads at most 5 right after a collection, rises once about
;;   17 MB have been allocated (200000 strings and conses), and falls back
;;   to at most 5 after the next collection.  Two explicit collections run
;;   first, so the debt trigger is armed.
;; - growth: about 100 MB allocated by `mapcar' (no `while' back edge, so no
;;   mid-form collection can run) grows the heap reservation through the
;;   mid-form growth window, and the reading reflects it (at least 20); an
;;   explicit collection then re-arms the growth triggers from the current
;;   reservation, so it reads at most 5 again.  Before explicit collections
;;   re-armed them, the crossed watermark stayed in place and the next back
;;   edge collected a second time.
;;
;; Prints one GC-PRESSURE line and exits non-zero on failure.

(defvar gc-pressure-check--keep nil)

(defun gc-pressure-check--churn (n)
  (let ((i 0) (acc nil))
    (while (< i n)
      (setq acc (cons (make-string 24 ?x) acc))
      (setq i (1+ i)))
    (setq gc-pressure-check--keep acc)
    (length acc)))

(let* ((native (fboundp 'nelisp--gc-pressure))
       (p0 (and native (progn (garbage-collect) (garbage-collect)
                              (nelisp--gc-pressure))))
       (n (gc-pressure-check--churn 200000))
       (p1 (and native (nelisp--gc-pressure)))
       (p2 (and native (progn (setq gc-pressure-check--keep nil)
                              (garbage-collect)
                              (nelisp--gc-pressure))))
       (big (mapcar (lambda (_) (make-string 1000 ?a)) (make-list 100000 nil)))
       (p3 (and native (nelisp--gc-pressure)))
       (p4 (and native (progn (setq big nil)
                              (garbage-collect)
                              (nelisp--gc-pressure))))
       (ok (and native (integerp p0) (integerp p1) (integerp p2)
                (integerp p3) (integerp p4)
                (= n 200000) (<= 0 p0 5) (> p1 p0) (<= 0 p2 5)
                (>= p3 20) (<= 0 p4 5))))
  (princ (format "GC-PRESSURE after-gc=%S after-churn=%S after-next-gc=%S after-growth=%S after-explicit-gc=%S %s\n"
                 p0 p1 p2 p3 p4 (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-gc-pressure-check.el ends here
