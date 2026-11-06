;;; nelisp-service-pool-smoke.el --- End-to-end pool smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213.  Run with `nelisp --load FILE' (NeLisp workers) or
;; `emacs --batch -Q -l FILE' (Emacs workers).  Prints one ok/FAIL line
;; per check and ends with SMOKE-PASS or SMOKE-FAIL; the exit status is
;; not relied on (see AI.md).

;;; Code:

(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'nelisp-service-worker)

(defvar nelisp-service-smoke--failed nil)
(defun nelisp-service-smoke--check (name ok)
  "Report check NAME as OK or not."
  (princ (format "%s %s\n" (if ok "ok" "FAIL") name))
  (unless ok (setq nelisp-service-smoke--failed t)))

(let ((pool (nelisp-service-pool-create :name "smoke" :max-workers 2 :recycle-after 3)))
  (nelisp-service-smoke--check
   "call" (equal 3 (nelisp-service-pool-call pool '(+ 1 2) 120)))
  (nelisp-service-smoke--check
   "unicode" (equal "a\nb日本"
                    (nelisp-service-pool-call pool '(concat "a\nb" "日本") 60)))
  (nelisp-service-smoke--check
   "error" (condition-case nil
               (progn (nelisp-service-pool-call pool '(car 5) 60) nil)
             (error t)))
  (let ((done 0) (peak 0))
    (dotimes (i 6)
      (nelisp-service-pool-submit pool `(* ,i 10)
                                  (lambda (_s _v) (setq done (1+ done)))))
    (nelisp-service-wait-until
     (lambda ()
       (setq peak (max peak (length (nelisp-service-pool-processes pool))))
       (= done 6))
     180)
    (nelisp-service-smoke--check "burst" (= done 6))
    (nelisp-service-smoke--check "ceiling" (<= peak 2)))
  (let ((result nil))
    (nelisp-service-pool-submit
     pool '(progn (while t (accept-process-output nil 0.1)))
     (lambda (s v) (setq result (list s v))))
    (nelisp-service-wait-until
     (lambda ()
       (let ((busy nil))
         (dolist (w (nelisp-service-get pool :workers))
           (when (numberp (nelisp-service-get w :busy)) (setq busy t)))
         busy))
     120)
    (dolist (w (nelisp-service-get pool :workers))
      (when (numberp (nelisp-service-get w :busy))
        (delete-process (nelisp-service-get w :process))))
    (nelisp-service-wait-until (lambda () result) 30)
    (nelisp-service-smoke--check "crash" (eq (car result) 'crashed)))
  (nelisp-service-smoke--check
   "after-crash" (equal 42 (nelisp-service-pool-call pool '(+ 40 2) 120)))
  (nelisp-service-pool-set-version pool 2)
  (nelisp-service-smoke--check
   "after-version" (equal 2 (nelisp-service-pool-call pool '(+ 1 1) 120)))
  (nelisp-service-smoke--check
   "recycled" (>= (plist-get (nelisp-service-pool-stats pool) :retired) 2))
  (nelisp-service-pool-shutdown pool)
  (nelisp-service-smoke--check
   "shutdown" (null (nelisp-service-pool-processes pool))))

(princ (if nelisp-service-smoke--failed "SMOKE-FAIL\n" "SMOKE-PASS\n"))

;;; nelisp-service-pool-smoke.el ends here
