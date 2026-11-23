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
