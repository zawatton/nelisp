;;; gc-port-budget.el --- GNU's consing_until_gc: the allocation budget a VM poll tests  -*- lexical-binding: t; -*-

;; Doc 223, "Bytecode VM safepoints".  The allocation fast paths subtract their bytes from the signed word
;; `nl_gc_until'; a poll site tests it for a negative value and only then calls `nl_gc_vm_poll', which decides
;; exactly as the debt, reservation, force and backstop triggers always did and derives the word again.
;; Debug switches: 76 answers the word, 77 its base (the value it was last set to), 78 the exact debt
;; (the folded debt plus base - word), 72 the number of mid-form collections, 70/71 the force hook.
;; The VM under test polls with (if (< nl_gc_until 0) (nl_gc_vm_poll) 0) at every Bcall
;; (tools/ai/gc-port-vm-poll-reference.patch), so a bytecode function that calls another polls once per call.
;; Every debug-switch call and every interpreted form allocates a little itself, so the words are compared
;; with a tolerance of a few KiB.  GCB_MODE selects:
;;   run     the armed state (three collections first): the word is nonnegative and the debt is the base
;;           less the word, the debt is exact across allocations, a nonnegative word stays so below its
;;           slice, the first poll after the slice refills without collecting, a collection comes when the
;;           debt reaches the limit (and then the debt starts again), the force hook makes the word negative
;;           and every poll collects, a mapped chunk makes it negative and the next poll collects;
;;   fresh   the unarmed state (no collection yet): a poll collects no later than the 64 MiB backstop.
;; Prints "GCB MODE checks=N failures=F PASS|FAIL" last.
(defvar gcb-mode nil)
(defvar gcb-checks 0)
(defvar gcb-failures 0)
(defvar gcb-keep nil)
(setq gcb-mode (or (getenv "GCB_MODE") "run") gcb-checks 0 gcb-failures 0)
;; GNU-compiled by bytecode-gen.el (test/gc-port-vm-poll/funcall.eld, GCB_ELD): CR-ADD is (lambda (i) (+ i 3))
;; and GCB-LOOP calls it N times, each call a Bcall and so a poll of the VM under test.  The functions are
;; read from the file as the workload driver does (a string literal with octal escapes read by `load' is not
;; the same object).
(let ((data (with-temp-buffer (insert-file-contents (getenv "GCB_ELD"))
                              (goto-char (point-min)) (read (current-buffer)))))
  (fset 'cr-add (cdr (assq 'add data)))
  (fset 'gcb-loop (cdr (assq 'funcall data))))
(defun gcb-poll (n) (gcb-loop n))
(defun gcb-check (ok label)
  (setq gcb-checks (1+ gcb-checks))
  (unless ok
    (setq gcb-failures (1+ gcb-failures))
    (princ (format "GCB FAIL %s\n" label))))
(defun gcb-u () (nelisp--debug-switch 76))
(defun gcb-b () (nelisp--debug-switch 77))
(defun gcb-d () (nelisp--debug-switch 78))
(defun gcb-n () (nelisp--debug-switch 72))
(defun gcb-mib () (setq gcb-keep (make-vector 131072 0)) nil)
(defconst gcb-slack 4096)

(defun gcb-run ()
  (dotimes (_ 3) (garbage-collect))
  ;; The armed state right after a collection.
  (let ((u (gcb-u)) (b (gcb-b)) (d (gcb-d)))
    (gcb-check (>= u 0) (format "budget after a collection u=%d" u))
    ;; The debt is what was folded at the last refill plus base - word; the collection's own tail
    ;; allocates a little between its two refills, so it can exceed base - word by that.
    (gcb-check (and (>= d (- (- b u) gcb-slack)) (< (- d (- b u)) 1048576))
               (format "the debt is the folded part plus base - word: u=%d b=%d d=%d" u b d))
    (gcb-check (< d 1048576) (format "debt after a collection %d" d)))
  ;; The debt is exact across allocations, and the word falls by what the debt rises by.
  (let ((d0 (gcb-d)) (u0 (gcb-u)))
    (gcb-mib)
    (let ((dd (- (gcb-d) d0)) (du (- u0 (gcb-u))))
      (gcb-check (and (>= dd 1048576) (< dd (+ 1048576 262144))) (format "one MiB charged %d" dd))
      (gcb-check (< (abs (- dd du)) gcb-slack) (format "the word fell by the debt: %d %d" dd du)))
    (gcb-check (>= (gcb-u) 0) (format "a nonnegative word below its slice %d" (gcb-u))))
  ;; A poll with a nonnegative word does not decide, and does not refill.
  (let ((n0 (gcb-n)) (b0 (gcb-b)))
    (gcb-poll 5)
    (gcb-check (= (gcb-n) n0) "no collection while the word is nonnegative")
    (gcb-check (= (gcb-b) b0) "no refill while the word is nonnegative"))
  ;; Past the slice the word is negative; the debt stays exact; the next poll refills without collecting.
  (let ((d0 (gcb-d)) (n0 (gcb-n)) (i 0))
    (while (and (>= (gcb-u) 0) (< i 64)) (gcb-mib) (setq i (1+ i)))
    (gcb-check (< (gcb-u) 0) (format "the slice ran out u=%d" (gcb-u)))
    (let ((spent (- (gcb-d) d0)))
      (gcb-check (>= spent (* i 1048576)) (format "debt after %d MiB: %d" i spent))
      (gcb-poll 1)
      (gcb-check (>= (gcb-u) 0) (format "the poll refilled u=%d" (gcb-u)))
      (gcb-check (< (abs (- (gcb-u) (gcb-b))) gcb-slack) "base equals the word after the poll")
      (gcb-check (= (gcb-n) n0) "the debt was still below the limit: no collection")
      (gcb-check (< (abs (- (gcb-d) (+ d0 spent))) (* 4 gcb-slack))
                 (format "the debt survived the refill: %d -> %d" (+ d0 spent) (gcb-d)))))
  ;; A collection comes when the debt reaches the limit, and the debt starts again.
  (let ((n0 (gcb-n)) (i 0) (peak 0))
    (while (and (= (gcb-n) n0) (< i 1500))
      (gcb-mib) (gcb-poll 1)
      (setq peak (max peak (gcb-d)) i (1+ i)))
    (gcb-check (> (gcb-n) n0) (format "a collection by debt within %d MiB" i))
    (gcb-check (< (gcb-d) 4194304) (format "the debt restarted after the collection %d" (gcb-d)))
    (gcb-check (>= (gcb-u) 0) "the budget is armed after the collection")
    (princ (format "GCB INFO limit reached after %d MiB, debt before the collection %d\n" i peak)))
  ;; The force hook: the word is negative, every poll collects, and 71 restores the triggers.
  (nelisp--debug-switch 70)
  (gcb-check (< (gcb-u) 0) (format "force makes the word negative (%d)" (gcb-u)))
  (let ((n0 (gcb-n)))
    (gcb-poll 3)
    (gcb-check (= (- (gcb-n) n0) 3) (format "three forced polls, three collections (%d)" (- (gcb-n) n0)))
    (gcb-check (< (gcb-u) 0) "the word is negative after a forced collection"))
  (nelisp--debug-switch 71)
  (gcb-check (>= (gcb-u) 0) (format "71 restores the budget u=%d" (gcb-u)))
  (let ((n0 (gcb-n))) (gcb-poll 3)
    (gcb-check (= (gcb-n) n0) "no collection once the hook is off"))
  ;; A mapped chunk makes the word negative at once, with the debt kept, and the next poll collects (the
  ;; reservation passed the mid-form trigger).
  (let ((d0 (gcb-d)) (n0 (gcb-n)))
    (setq gcb-keep (make-string 150000000 ?x))
    (gcb-check (< (gcb-u) 0) (format "a chunk mapping makes the word negative u=%d" (gcb-u)))
    (gcb-check (>= (- (gcb-d) d0) 150000000) (format "the big string is charged %d" (- (gcb-d) d0)))
    (gcb-poll 1)
    (gcb-check (> (gcb-n) n0) "the poll after the mapping collected")
    (setq gcb-keep nil))
  (garbage-collect))

(defun gcb-fresh ()
  ;; No collection has run: the debt trigger is not armed, so the poll's backstop opens at 64 MiB.
  (gcb-poll 1)
  (gcb-check (>= (gcb-u) 0) (format "a poll arms the budget u=%d" (gcb-u)))
  (let ((n0 (gcb-n)) (i 0) (peak 0))
    (while (and (= (gcb-n) n0) (< i 200))
      (gcb-mib) (gcb-poll 1)
      (setq peak (max peak (gcb-d)) i (1+ i)))
    ;; The growth trigger (a chunk mapped) may come first; either way a poll collected before 200 MiB.
    (gcb-check (> (gcb-n) n0) (format "the first collection came within %d MiB" i))
    (gcb-check (<= peak (* 72 1048576)) (format "no later than the backstop: %d" peak))
    (princ (format "GCB INFO first collection after %d MiB, debt %d\n" i peak))))

(if (equal gcb-mode "fresh") (gcb-fresh) (gcb-run))
(princ (format "GCB %s checks=%d failures=%d %s\n" gcb-mode gcb-checks gcb-failures
               (if (= gcb-failures 0) "PASS" "FAIL")))
nil
