;;; batch-f-vm-funcall-18m.el --- the codegen lane's 18.3 million funcall workload -*- lexical-binding: t; -*-

;; The GNU-calibrated bytecode funcall loop of the codegen lane (S5.13):
;; 18300000 Bcalls of test/perf/codegen-runtime/bytecode.eld.  A bytecode VM
;; that never polls the garbage collector grows about 0.67 KB per call here
;; and exits 88 at the 8 GiB address-space cap; one that polls like GNU
;; bytecode.c (maybe_gc at Bcall entry, :797) finishes.

(let* ((file (expand-file-name "../perf/codegen-runtime/bytecode.eld"
                               (file-name-directory load-file-name)))
       (data (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (read (current-buffer))))
       (fn (cdr (assq 'funcall data))))
  (fset 'cr-add (cdr (assq 'add data)))
  (princ (format "result %S\n" (funcall fn 18300000))))
(kill-emacs 0)
