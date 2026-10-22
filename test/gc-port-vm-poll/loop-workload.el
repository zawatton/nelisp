;;; loop-workload.el --- the call-free allocating bytecode loop, n iterations  -*- lexical-binding: t; -*-
;; Environment: VM_POLL_N.  Prints "S5-WORK loop N ANSWER SECONDS" like the codegen workload.
(let* ((file (expand-file-name "loop.eld" (file-name-directory load-file-name)))
       (data (with-temp-buffer (insert-file-contents file)
                               (goto-char (point-min)) (read (current-buffer))))
       (n (string-to-number (getenv "VM_POLL_N")))
       (fn (cdr (assq 'loop data))))
  (unless (= (funcall fn 1000) 999) (error "VM-POLL loop warmup failed"))
  (garbage-collect)
  (let* ((start (float-time)) (answer (funcall fn n)) (seconds (- (float-time) start)))
    (unless (= answer (1- n)) (error "VM-POLL loop wrong result %s" answer))
    (princ (format "S5-WORK loop %d %d %.9f\n" n answer seconds))
    (princ (format "VM-POLL-COLLECTIONS %S\n"
                   (let ((stats (nelisp--debug-switch 2))) (and (consp stats) (nth 7 stats)))))))
