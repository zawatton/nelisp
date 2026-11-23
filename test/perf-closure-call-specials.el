;;; -*- lexical-binding: t; -*-
;; F293 per-call cost gate: a closure made under an eval alist of N>=400 names (as inside
;; eval-and-compile with 400+ defvars; ~90% special) must cost <= 2x a plain closure call.
;; The body calls an undefined function (as perf-b1.el) so the call takes the evaluator path that installs the env.
;; usage: target/nelisp --cold-load-from <rt.dump> --eval '(load "this.el" nil t t)'
;; prints "PERFGATE ratio=R plain=P us big=B us n=N OK|FAIL"; GNU has no such cost (prints too, informational).
(defun pcs-names (n) (let (l (i 0)) (dotimes (_ n) (push (intern (format "pcs-v%d" (setq i (1+ i)))) l)) l))
(dolist (s (pcs-names 400)) (eval `(defvar ,s 0) t))
(defun pcs-tm (al) (let ((f (eval '(lambda () (if (pcs-nonexistent-fn-p) 1 2)) al)) (n 3000))
  (dotimes (_ 200) (ignore-errors (funcall f))) (garbage-collect)
  (let ((s (float-time))) (dotimes (_ n) (ignore-errors (funcall f))) (/ (* 1e6 (- (float-time) s)) n))))
;; Two shapes: markers only (share path, nelisp_frame_captured_decls) and one lexical pair + markers
;; (nl_push_captured_walk); each is compared with the same shape without the 400 names.
(defun pcs-row (label base al)
  (let ((r nil))
    (dotimes (_ 5) (let ((p (pcs-tm base)) (b (pcs-tm al))) (push (cons p b) r)))
    (setq r (sort r (lambda (a b) (< (/ (cdr a) (car a)) (/ (cdr b) (car b))))))
    (let* ((m (nth 2 r)) (ratio (/ (cdr m) (car m))))
      (princ (format "PERFGATE %s ratio=%.2f plain=%.3f us big=%.3f us n=%d %s\n" label ratio (car m) (cdr m) (length al) (if (<= ratio 2.0) "OK" "FAIL"))))))
(pcs-row "markers-only" '(t) (append (pcs-names 400) '(pcs-ns1 pcs-ns2 t)))
(pcs-row "pair+markers" '((x . 1) t) (append '((x . 1)) (pcs-names 400) '(pcs-ns1 pcs-ns2 t)))
;; F298 nested-create row (informational, regime A): a 600-name closure whose body creates (lambda () 1) per call.
;; Creation expands the env sentinel, O(|env|) conses as before b665d7f30, so expect ~80-90 us/call, only on creation.
(let* ((names (pcs-names 600))
       (f (eval '(lambda () (lambda () 1)) (append names '(t))))
       (n 3000))
  (dotimes (_ 100) (funcall f)) (garbage-collect)
  (let ((s (float-time)))
    (dotimes (_ n) (funcall f))
    (princ (format "PERFINFO nested-create names=600 %.3f us/call view-len=%d\n"
                   (/ (* 1e6 (- (float-time) s)) n) (length (aref (funcall f) 2))))))
;; F299 closure+let row: a closure whose env is ONLY special names (400, so the call install skips them all
;; and slot 3 holds only the env-view sentinel) and whose body is (let ((x 1)) x).  nl_eic_let_scope_clean must
;; accept the lone sentinel so the let fast arm stays on.  Post-build gate: the big/plain ratio (and the absolute
;; big us) must be no worse than the same file run on a b665d7f30 binary, where slot 3 was nil.
(defun pcs-let-tm (al) (let ((f (eval '(lambda () (let ((x 1)) x)) al)) (n 20000))
  (dotimes (_ 500) (funcall f)) (garbage-collect)
  (let ((s (float-time))) (dotimes (_ n) (funcall f)) (/ (* 1e6 (- (float-time) s)) n))))
(let ((r nil))
  (dotimes (_ 5) (let ((p (pcs-let-tm '(t))) (b (pcs-let-tm (append (pcs-names 400) '(t))))) (push (cons p b) r)))
  (setq r (sort r (lambda (a b) (< (/ (cdr a) (car a)) (/ (cdr b) (car b))))))
  (let ((m (nth 2 r)))
    (princ (format "PERFINFO closure-let all-specials plain=%.3f us big=%.3f us ratio=%.2f\n" (car m) (cdr m) (/ (cdr m) (car m))))))
