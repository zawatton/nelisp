;;; nelisp-eln-leaf-code.el --- Verify bounded GNU leaf code -*- lexical-binding: t; -*-

;;; Code:

(defun nelisp-eln-leaf-code--u32 (bytes start)
  "Read a little-endian unsigned word from BYTES at START."
  (let ((n 0))
    (dotimes (i 4 n)
      (setq n (logior n (ash (aref bytes (+ start i)) (* 8 i)))))))

(defun nelisp-eln-leaf-code--s32 (bytes start)
  "Read a little-endian signed word from BYTES at START."
  (let ((n (nelisp-eln-leaf-code--u32 bytes start)))
    (if (>= n #x80000000) (- n #x100000000) n)))

(defun nelisp-eln-leaf-code--match (bytes offset octets)
  "Return non-nil when BYTES at OFFSET starts with OCTETS."
  (and (<= (+ offset (length octets)) (length bytes))
       (let ((i 0))
         (while (and (< i (length octets))
                     (= (aref bytes (+ offset i)) (nth i octets)))
           (setq i (1+ i)))
         (= i (length octets)))))

(defun nelisp-eln-leaf-code--merge-state (old new)
  "Merge must-properties OLD and NEW at a control-flow join."
  (if old
      (cons (and (car old) (car new))
            (and (cdr old) (cdr new)))
    new))

(defun nelisp-eln-leaf-code-valid-p (bytes)
  "Return non-nil iff BYTES is a safe supported GNU x86-64 unary leaf.

This recognizes only the emitter's register move, nil xor, tagged immediate,
test, forward conditional/unconditional branches, and final return."
  (catch 'invalid
    (unless (and (stringp bytes) (> (length bytes) 0))
      (throw 'invalid nil))
    (setq bytes (string-as-unibyte bytes))
    (let* ((size (length bytes)) (offset 0) (instructions nil)
           (boundaries (make-hash-table :test #'eql))
           (targets (make-hash-table :test #'eql)))
      ;; Decode the complete stream.  Immediates are consumed as operands.
      (while (< offset size)
        (puthash offset t boundaries)
        (let ((start offset) (kind nil) (length 0))
          (cond
           ((nelisp-eln-leaf-code--match bytes offset '(#x48 #x89 #xf8))
            (setq kind 'move length 3))
           ((nelisp-eln-leaf-code--match bytes offset '(#x31 #xc0))
            (setq kind 'nil length 2))
           ((nelisp-eln-leaf-code--match bytes offset '(#x48 #xb8))
            (when (< (- size offset) 10) (throw 'invalid nil))
            (let ((word 0))
              (dotimes (i 8)
                (setq word (logior word (ash (aref bytes (+ offset 2 i)) (* 8 i)))))
              (unless (or (= word 0) (= (logand word 3) 2))
                (throw 'invalid nil)))
            (setq kind 'immediate length 10))
           ((nelisp-eln-leaf-code--match bytes offset '(#x48 #x85 #xc0))
            (setq kind 'test length 3))
           ((nelisp-eln-leaf-code--match bytes offset '(#x0f #x84))
            (when (< (- size offset) 6) (throw 'invalid nil))
            (setq kind 'jz length 6))
           ((= (aref bytes offset) #xe9)
            (when (< (- size offset) 5) (throw 'invalid nil))
            (setq kind 'jmp length 5))
           ((= (aref bytes offset) #xc3)
            (setq kind 'ret length 1))
           (t (throw 'invalid nil)))
          (setq offset (+ offset length))
          (when (and (eq kind 'ret) (/= offset size))
            (throw 'invalid nil))
          (when (memq kind '(jz jmp))
            (let* ((disp (nelisp-eln-leaf-code--s32
                          bytes (if (eq kind 'jz) (+ start 2) (1+ start))))
                   (target (+ offset disp)))
              (unless (and (> target start) (>= target 0) (< target size))
                (throw 'invalid nil))
              (puthash start target targets)))
          (push (list start offset kind) instructions)))
      (unless (and instructions (eq (nth 2 (car instructions)) 'ret))
        (throw 'invalid nil))
      (maphash (lambda (_branch target)
                 (unless (gethash target boundaries) (throw 'invalid nil)))
               targets)
      ;; Forward-only edges make this ascending worklist a single pass.
      (let ((states (make-hash-table :test #'eql)))
        (puthash 0 (cons nil nil) states)
        (dolist (insn (nreverse instructions))
          (let* ((start (nth 0 insn)) (end (nth 1 insn))
                 (kind (nth 2 insn)) (state (gethash start states)))
            (when state
              (let ((initialized (car state)) (tested (cdr state)))
                (pcase kind
                  ((or 'move 'immediate)
                   (setq initialized t))
                  ('nil (setq initialized t tested nil))
                  ('test
                   (unless initialized (throw 'invalid nil))
                   (setq tested t))
                  ('jz
                   (unless tested (throw 'invalid nil))
                   (let* ((target (gethash start targets))
                          (next-state (cons initialized nil)))
                     (puthash target
                              (nelisp-eln-leaf-code--merge-state
                               (gethash target states) next-state) states)))
                  ('jmp
                   (let* ((target (gethash start targets))
                          (next-state (cons initialized tested)))
                     (puthash target
                              (nelisp-eln-leaf-code--merge-state
                               (gethash target states) next-state) states)))
                  ('ret
                   (unless initialized (throw 'invalid nil))))
                (unless (memq kind '(jmp ret))
                  (when (>= end size) (throw 'invalid nil))
                  (puthash end
                           (nelisp-eln-leaf-code--merge-state
                            (gethash end states) (cons initialized tested))
                           states))))))
      t))))

(provide 'nelisp-eln-leaf-code)

;;; nelisp-eln-leaf-code.el ends here
