;;; nconc-opcode.el --- NCONC source-VM fixture -*- lexical-binding: t; -*-

(defun nelisp-gnu-bytecode-vm-nconc-two (left right)
  (nconc left right))

(defun nelisp-gnu-bytecode-vm-nconc-three (first second third)
  (nconc first second third))

(provide 'nelisp-gnu-bytecode-vm-nconc-opcode)
;;; nconc-opcode.el ends here
