;;; force-check.el --- the GNU bytecode corpus with a collection at every VM safepoint  -*- lexical-binding: t; -*-
;; Usage: nelisp --load test/gc-port-vm-poll/force-check.el
;; Debug switch 70 makes every `nl_gc_vm_poll' collect (Doc 223, "Bytecode VM safepoints").
;; Replays test/bytecode-word-corpus.eld exactly as test/standalone-bytecode-word-check.el
;; does, then runs the allocating loop of loop.eld for 5000 iterations.  Passes when every
;; answer equals GNU's and the VM polled (collections > 0): a VM without polls, or a
;; binary without the hook, answers 0 collections and fails.
(let* ((dir (file-name-directory load-file-name))
       (data (with-temp-buffer
               (insert-file-contents (expand-file-name "../bytecode-word-corpus.eld" dir))
               (car (read-from-string (buffer-string)))))
       (loop (cdr (assq 'loop (with-temp-buffer
                                (insert-file-contents (expand-file-name "loop.eld" dir))
                                (goto-char (point-min)) (read (current-buffer))))))
       (count (lambda () (let ((n (nelisp--debug-switch 72))) (if (integerp n) n -1))))
       (fails 0) (n 0))
  (nelisp--debug-switch 70)
  (let ((before (funcall count)))
    (dolist (p (nth 1 data))
      (eval (list 'defvar (car p)) t)
      (set (car p) (cdr p)))
    (dolist (p (nth 2 data))
      (fset (car p) (cdr p)))
    (dolist (c (nth 3 data))
      (setq n (1+ n))
      (let ((got (format "%S" (condition-case e (apply (car c) (copy-tree (cadr c) t))
                                (error (list 'error (car e)))))))
        (unless (equal got (nth 2 c))
          (setq fails (1+ fails))
          (princ (format "VM-POLL-FORCE FAIL %S %S got %s want %s\n"
                         (car c) (cadr c) got (nth 2 c))))))
    (let ((loop-answer (funcall loop 5000)))
      (unless (equal loop-answer 4999)
        (setq fails (1+ fails))
        (princ (format "VM-POLL-FORCE FAIL loop got %S want 4999\n" loop-answer))))
    (nelisp--debug-switch 71)
    (let ((polls (- (funcall count) before)))
      (princ (format "VM-POLL-FORCE %s cases=%d/%d collections=%d\n"
                     (if (and (= fails 0) (> polls 0)) "PASS" "FAIL")
                     (- n fails) n polls))
      (kill-emacs (if (and (= fails 0) (> polls 0)) 0 1)))))
