;;; standalone-bytecode-poll-check.el --- live values across VM safepoints -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-bytecode-poll-check.el
;;
;; Replays test/bytecode-poll-corpus.eld (functions byte-compiled by host GNU
;; Emacs, see test/bytecode-poll-corpus-src.el) twice: once with the
;; allocator's own triggers, once with a collection at every VM safepoint
;; (debug switch 70 makes every `nl_gc_vm_poll' collect, switch 71 restores the
;; triggers, switch 72 answers the number of mid-form collections so far).
;; Every answer must equal GNU's `%S' text, and the forced run must have
;; collected: a VM without safepoints, or a binary without the hook, answers 0
;; collections and fails.

(let* ((file (expand-file-name "bytecode-poll-corpus.eld"
                               (file-name-directory load-file-name)))
       (data (with-temp-buffer
               (insert-file-contents file)
               (car (read-from-string (buffer-string)))))
       (count (lambda () (let ((n (nelisp--debug-switch 72))) (if (integerp n) n -1))))
       (fails 0) (n 0) (polls 0))
  (dolist (p (nth 1 data))
    (eval (list 'defvar (car p)) t)
    (set (car p) (cdr p)))
  (dolist (p (nth 2 data))
    (fset (car p) (cdr p)))
  (dolist (pass '(plain forced))
    (when (eq pass 'forced) (nelisp--debug-switch 70))
    (let ((before (funcall count)))
      (dolist (c (nth 3 data))
        (setq n (1+ n))
        (let ((got (format "%S" (condition-case e (apply (car c) (copy-tree (cadr c) t))
                                  (error (list 'error (car e)))))))
          (unless (equal got (nth 2 c))
            (setq fails (1+ fails))
            (princ (format "BYTECODE-POLL FAIL %s %S %S got %s want %s\n" pass
                           (car c) (cadr c) got (nth 2 c))))))
      (when (eq pass 'forced)
        (nelisp--debug-switch 71)
        (setq polls (- (funcall count) before)))))
  (princ (format "BYTECODE-POLL %s %d/%d answers match GNU %s, %d forced collections\n"
                 (if (and (= fails 0) (> polls 0)) "PASS" "FAIL")
                 (- n fails) n (car data) polls))
  (kill-emacs (if (and (= fails 0) (> polls 0)) 0 1)))

;;; standalone-bytecode-poll-check.el ends here
