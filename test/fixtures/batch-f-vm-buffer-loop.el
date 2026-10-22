;;; batch-f-vm-buffer-loop.el --- a long GNU-compiled with-current-buffer loop -*- lexical-binding: t; -*-

;; 100000 iterations of the buffer-bridge lane's GNU 31.1 byte code fbb-loop
;; (with-current-buffer in a while loop).  This runtime's set-buffer allocates
;; about 90 KB per iteration, which is 9 GB in all: a bytecode VM that never
;; polls the garbage collector hits the 8 GiB address-space cap (exit 88) near
;; 75000 iterations, one that polls like GNU bytecode.c finishes.

(defvar fbb-local nil)
(load (expand-file-name "buffer-function-cell.eld" (file-name-directory load-file-name)) nil t t)
(let ((b (get-buffer-create "fbb-b")))
  (princ (format "result %S\n" (fbb-loop b 100000))))
(kill-emacs 0)
