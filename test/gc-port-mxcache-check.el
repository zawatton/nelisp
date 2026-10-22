;;; gc-port-mxcache-check.el --- the macro-expansion cache survives collections  -*- lexical-binding: t; -*-

;; The interpreter caches a macro call site's expansion keyed by the call form's address (nl_mxcache_*).  A collection
;; must keep the expansion of every used entry alive: the cache is the only reference to it.  N functions each hold
;; one macro call site; every site is expanded once, called again (a hit: no new expansion), then collections run with
;; allocation churn between them that reuses freed memory, and every function is called again.  A skipped entry would
;; return a freed (reused) expansion: a wrong value or a crash.  Prints one summary line
;;   MXC n=N expansions first=A second=B after-gc=C wrong=W <PASS|FAIL>
;; and PASS needs wrong=0 and second=A=C (the cache hit before and after the collections).
(defvar mxc-expansions 0)
(defmacro mxc-m (x) (setq mxc-expansions (1+ mxc-expansions)) (list 'list x x))
(defconst mxc-n 400)
(let ((i 0))
  (while (< i mxc-n)
    (eval (list 'defun (intern (format "mxc-f%d" i)) '(v) (list 'mxc-m (list '+ 'v i))))
    (setq i (1+ i))))
(defun mxc-call-all (round)
  (let ((i 0) (wrong 0))
    (while (< i mxc-n)
      (let ((r (funcall (intern (format "mxc-f%d" i)) round)))
        (unless (equal r (list (+ round i) (+ round i))) (setq wrong (1+ wrong))))
      (setq i (1+ i)))
    wrong))
(defun mxc-churn ()
  (let ((keep nil) (i 0))
    (while (< i 4000)
      (setq keep (cons (list i (number-to-string i) (make-vector 6 i)) (if (> (length keep) 50) nil keep)))
      (setq i (1+ i)))
    keep))
(let* ((w1 (mxc-call-all 1))
       (first mxc-expansions)
       (w2 (mxc-call-all 2))
       (second mxc-expansions)
       (w3 0))
  (dotimes (_ 4)
    (mxc-churn)
    (garbage-collect)
    (mxc-churn)
    (setq w3 (+ w3 (mxc-call-all 3))))
  (let ((after mxc-expansions) (wrong (+ w1 w2 w3)))
    (princ (format "MXC n=%d expansions first=%d second=%d after-gc=%d wrong=%d %s\n" mxc-n first second after wrong
                   (if (and (= wrong 0) (= first second) (= second after)) "PASS" "FAIL")))))
nil
