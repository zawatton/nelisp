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
;; - growth: three successive `mapcar' allocations of about 5 MB each (no
;;   `while' back edge, so no mid-form collection can run) must raise the
;;   reading strictly each time, to at least 10 and below 100 after the third;
;;   an explicit collection then re-arms the growth triggers from the current
;;   reservation, so it reads at most 5 again.  Before explicit collections
;;   re-armed them, the crossed watermark stayed in place and the next back
;;   edge collected a second time.
;;
;;   The check used to allocate 100 MB and require a reading of at least 20.
;;   That reading is a position in a window of roughly 60 MB of growth that
;;   wraps to 0 when crossed, so after 100 MB it depends on the live heap the
;;   image boots with: with ballast held live the same binary read
;;   b85 25/19/14/7 and b168 19/14/10/4 at +0/3/6/12 MB (about -1.7 points per
;;   MB of live heap), and the floor failed whenever boot grew by a few MB.
;;   Staying well inside the window and checking the rise, as measured from
;;   the post-collection reading, does not depend on that baseline.  Negative
;;   controls: with the allocations shrunk to 100 strings the readings stay
;;   flat and the check fails.
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
       (big1 (mapcar (lambda (_) (make-string 1000 ?a)) (make-list 5000 nil)))
       (g1 (and native (nelisp--gc-pressure)))
       (big2 (mapcar (lambda (_) (make-string 1000 ?a)) (make-list 5000 nil)))
       (g2 (and native (nelisp--gc-pressure)))
       (big3 (mapcar (lambda (_) (make-string 1000 ?a)) (make-list 5000 nil)))
       (p3 (and native (nelisp--gc-pressure)))
       (p4 (and native (progn (setq big1 nil big2 nil big3 nil)
                              (garbage-collect)
                              (nelisp--gc-pressure))))
       (ok (and native (integerp p0) (integerp p1) (integerp p2)
                (integerp g1) (integerp g2) (integerp p3) (integerp p4)
                (= n 200000) (<= 0 p0 5) (> p1 p0) (<= 0 p2 5)
                (< p2 g1) (< g1 g2) (< g2 p3) (>= p3 10) (< p3 100)
                (<= 0 p4 5))))
  (princ (format "GC-PRESSURE after-gc=%S after-churn=%S after-next-gc=%S after-growth=%S after-explicit-gc=%S %s\n"
                 p0 p1 p2 p3 p4 (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-gc-pressure-check.el ends here
