;;; nelisp-bt-global-smoke.el --- Doc 214 S5 global writes and interning on threads -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S5 acceptance, run from the repository root:
;;   nelisp --load tools/nelisp-bt-global-smoke.el
;; - BT-GLOBAL-PASS: from 8 threads at once, each thread
;;     * creates and assigns a fresh global with `set',
;;     * defines a function with `defalias' and a variable with `defvar',
;;     * runs `string-match' without disturbing the main thread's match
;;       data,
;;   and all 8 increment one existing global 1000 times under an nl-bt
;;   lock (8000).  The main thread then sees every value and calls every
;;   function.
;; - BT-INTERN-PASS 40000: 8 threads intern 5000 fresh names each; every
;;   name is then found by `intern-soft', so 40000 distinct symbols exist.
;;   The 8 threads also intern the same 500 names concurrently, and every
;;   thread must get `eq' symbols for each.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(defvar nl-bt-smoke-counter 0)

(princ (format "BT-GLOBAL backend %S\n" nl-bt-backend))

(string-match "\\(b+\\)" "aabbbcc")
(let* ((main-match (match-beginning 1))
       (lock (nl-bt-make-lock "counter"))
       (threads
        (mapcar
         (lambda (i)
           (nl-bt-make-thread
            (lambda ()
              (set (intern (format "nl-bt-smoke-global-%d" i)) (* i 10))
              (defalias (intern (format "nl-bt-smoke-fn-%d" i))
                (lambda () (+ i 100)))
              (eval `(defvar ,(intern (format "nl-bt-smoke-dv-%d" i)) ,i) t)
              (let ((m (and (string-match "x\\(y+\\)" "zzxyyy") (match-beginning 1)))
                    (k 0))
                (while (< k 1000)
                  (nl-bt-with-lock-held lock
                    (setq nl-bt-smoke-counter (1+ nl-bt-smoke-counter)))
                  (setq k (1+ k)))
                m))))
         '(1 2 3 4 5 6 7 8)))
       (matches (mapcar #'nl-bt-join-thread threads))
       (ok t))
  (dolist (i '(1 2 3 4 5 6 7 8))
    (unless (and (equal (symbol-value (intern (format "nl-bt-smoke-global-%d" i))) (* i 10))
                 (equal (funcall (intern (format "nl-bt-smoke-fn-%d" i))) (+ i 100))
                 (equal (symbol-value (intern (format "nl-bt-smoke-dv-%d" i))) i)
                 (special-variable-p (intern (format "nl-bt-smoke-dv-%d" i))))
      (setq ok nil)
      (princ (format "BT-GLOBAL thread %d state missing\n" i))))
  (princ (format "BT-GLOBAL counter=%S matches=%S main-match=%S/%S\n"
                 nl-bt-smoke-counter matches main-match (match-beginning 1)))
  (princ (if (and ok (= nl-bt-smoke-counter 8000)
                  (equal matches '(3 3 3 3 3 3 3 3))
                  (equal main-match 2) (equal (match-beginning 1) 2))
             "BT-GLOBAL-PASS\n" "BT-GLOBAL-FAIL\n")))

(let* ((threads
        (mapcar
         (lambda (i)
           (nl-bt-make-thread
            (lambda ()
              (let ((k 0) (shared nil))
                (while (< k 5000)
                  (intern (format "nl-bt-fresh-%d-%d" i k))
                  (setq k (1+ k)))
                (setq k 0)
                (while (< k 500)
                  (push (intern (format "nl-bt-shared-%d" k)) shared)
                  (setq k (1+ k)))
                (nreverse shared)))))
         '(1 2 3 4 5 6 7 8)))
       (shared (mapcar #'nl-bt-join-thread threads))
       (found 0)
       (same t))
  (dolist (i '(1 2 3 4 5 6 7 8))
    (let ((k 0))
      (while (< k 5000)
        (when (intern-soft (format "nl-bt-fresh-%d-%d" i k))
          (setq found (1+ found)))
        (setq k (1+ k)))))
  (let ((first (car shared)))
    (dolist (lst (cdr shared))
      (let ((a first) (b lst))
        (while (and a b)
          (unless (eq (car a) (car b)) (setq same nil))
          (setq a (cdr a) b (cdr b))))))
  (princ (format "BT-INTERN found=%d shared-eq=%S\n" found same))
  (princ (if (and (= found 40000) same) "BT-INTERN-PASS 40000\n" "BT-INTERN-FAIL\n")))

;;; nelisp-bt-global-smoke.el ends here
