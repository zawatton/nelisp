(princ (format "selftest73=%S control74=%S\n" (nelisp--debug-switch 73) (nelisp--debug-switch 74)))
(let ((c0 (nelisp--debug-switch 75)))
  (garbage-collect)
  (princ (format "scan_regs-calls-delta=%S\n" (- (nelisp--debug-switch 75) c0))))
