;;; nelisp-bt-gc-smoke.el --- Doc 214 S6 allocation and GC from any thread -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S6 acceptance, run from the repository root:
;;   nelisp --load tools/nelisp-bt-gc-smoke.el
;; 8 threads each build and drop short-lived lists, 600 iterations of
;; 200 conses and 200 vectors apiece, while each keeps a growing list of
;; live samples.  Mid-form collections fall due while they run; the main
;; thread runs them, at its loop back-edges, with every worker parked.
;; Every thread also calls `garbage-collect' itself every 200 iterations,
;; which asks the main thread to collect and waits for it.  Passing
;; requires:
;; - every thread's live samples are intact afterwards (their sum is
;;   recomputed and compared), so no collection freed a live object;
;; - at least one parked collection ran while the threads did.
;; Prints BT-GC-PASS.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(defun nl-bt-gc-smoke--parked-collects ()
  (nth 20 (nelisp--debug-switch 0)))

(defconst nl-bt-gc-smoke--iterations 600
  "Per-thread loop count; each iteration allocates 400 short-lived objects.")

(let* ((before (nl-bt-gc-smoke--parked-collects))
       (t0 (float-time))
       (threads
        (mapcar
         (lambda (id)
           (nl-bt-make-thread
            (lambda ()
              (let ((i 0) (keep nil) (expect 0))
                (while (< i nl-bt-gc-smoke--iterations)
                  ;; ~16 KB of short-lived conses, strings and vectors.
                  (let ((junk (make-list 200 (cons i "x"))))
                    (setq junk (mapcar (lambda (c) (vector (car c) (cdr c))) junk))
                    (when (= 0 (% i 50))
                      (push (list id i (length junk) (format "%d-%d" id i)) keep)
                      (setq expect (+ expect i))))
                  (when (= 0 (% i 200)) (garbage-collect))
                  (setq i (1+ i)))
                ;; Recompute from the live samples only.
                (let ((sum 0) (ok t))
                  (dolist (k keep)
                    (setq sum (+ sum (nth 1 k)))
                    (unless (and (= (nth 0 k) id) (= (nth 2 k) 200)
                                 (equal (nth 3 k) (format "%d-%d" id (nth 1 k))))
                      (setq ok nil)))
                  (list ok (= sum expect) (length keep)))))))
         '(1 2 3 4 5 6 7 8)))
       (results (mapcar #'nl-bt-join-thread threads))
       (collects (- (nl-bt-gc-smoke--parked-collects) before)))
  (princ (format "BT-GC results=%S parked-collects=%d secs=%.1f\n"
                 results collects (- (float-time) t0)))
  (princ (if (and (equal results (make-list 8 (list t t 12)))
                  (> collects 0))
             "BT-GC-PASS\n" "BT-GC-FAIL\n")))

;;; nelisp-bt-gc-smoke.el ends here
