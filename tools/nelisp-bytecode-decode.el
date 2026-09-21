;;; nelisp-bytecode-decode.el --- Decode host Emacs bytecode -*- lexical-binding: t; -*-

;; Encoding verified against Emacs 31.1 disassemble and disassemble-offset.
;; Only bytes 0..47 use the low-three-bit operand family.  listN,
;; concatN, insertN, stack-set and discardN have fixed-width operands.

(require 'bytecomp)

(defun nelisp-bytecode-decode (bytecode-string)
  "Decode BYTECODE-STRING into (OFFSET OPCODE-SYMBOL OPERAND-OR-NIL).
Offsets and jump operands are byte positions.  Names follow disassemble:
constant2 and stack-set2 become constant and stack-set; discardN's high
bit selects discardN-preserve-tos.  Constant operands remain indices.
Signal an error for unknown opcodes, truncated operands or non-byte input.
Opcode zero is decoded as stack-ref, as in the host disassembler."
  (unless (and (stringp bytecode-string)
               (not (multibyte-string-p bytecode-string)))
    (error "Expected a unibyte code string"))
  (let ((pc 0) (end (length bytecode-string)) result)
    (while (< pc end)
      (let* ((start pc)
             (raw (aref bytecode-string pc))
             (base raw)
             (width 0)
             operand op)
        (setq pc (1+ pc))
        (cond
         ((< raw byte-pophandler)
          (setq base (logand raw #xf8)
                operand (logand raw 7))
          (when (>= operand 6)
            (setq width (- operand 5))))
         ((>= raw byte-constant)
          (setq base byte-constant operand (- raw byte-constant)))
         ((or (<= byte-constant2 raw byte-goto-if-not-nil-else-pop)
              (memq raw (list byte-stack-set2 byte-pushcatch
                              byte-pushconditioncase)))
          (setq width 2))
         ((<= byte-listN raw byte-discardN)
          (setq width 1)))
        (setq op (aref byte-code-vector base))
        (unless op
          (error "Unknown opcode %d at byte %d" raw start))
        (when (> (+ pc width) end)
          (error "Truncated operand at byte %d" start))
        (when (> width 0)
          (setq operand (aref bytecode-string pc))
          (when (= width 2)
            (setq operand (+ operand
                             (ash (aref bytecode-string (1+ pc)) 8))))
          (setq pc (+ pc width)))
        (cond
         ((eq op 'byte-constant2) (setq op 'byte-constant))
         ((eq op 'byte-stack-set2) (setq op 'byte-stack-set))
         ((and (eq op 'byte-discardN) (>= operand 128))
          (setq op 'byte-discardN-preserve-tos operand (- operand 128))))
        (push (list start op operand) result)))
    (nreverse result)))

(provide 'nelisp-bytecode-decode)
;;; nelisp-bytecode-decode.el ends here
