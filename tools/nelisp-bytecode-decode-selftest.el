;;; nelisp-bytecode-decode-selftest.el --- Isolated decoder evidence -*- lexical-binding: t; -*-

(require 'ert)
(require 'disass)
(let ((directory (file-name-directory load-file-name)))
  (load (expand-file-name "nelisp-bytecode-decode.el" directory) nil t)
  (load (expand-file-name "nelisp-bytecode-histogram.el" directory) nil t))

(defun nelisp-bytecode-test-disassembly (object)
  "Capture disassemble output for OBJECT as (OFFSET OPCODE) pairs.
Only column-zero instructions belong to OBJECT; nested code is indented."
  (let ((text (with-output-to-string
                (with-temp-buffer
                  (disassemble object (current-buffer))
                  (princ (buffer-string)))))
        result)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([0-9]+\\)\\(?::[0-9]+\\)?[ \t]+\\([^ \t\n]+\\)"
              nil t)
        (push (list (string-to-number (match-string 1))
                    (intern (concat "byte-" (match-string 2))))
              result)))
    (nreverse result)))

(ert-deftest nelisp-bytecode-decode-handwritten ()
  ;; No fixture is executed: arbitrary stack depths and indices are intentional.
  (dolist (base '(0 8 16 24 32 40))
    (let ((op (aref byte-code-vector base)))
      (dotimes (operand 6)
        (should (equal (nelisp-bytecode-decode
                        (unibyte-string (+ base operand)))
                       (list (list 0 op operand)))))
      (should (equal (nelisp-bytecode-decode
                      (unibyte-string (+ base 6) 254))
                     (list (list 0 op 254))))
      (should (equal (nelisp-bytecode-decode
                      (unibyte-string (+ base 7) 2 1))
                     (list (list 0 op 258))))))
  (should (equal (nelisp-bytecode-decode (unibyte-string 192 255 129 2 1))
                 '((0 byte-constant 0) (1 byte-constant 63)
                   (2 byte-constant 258))))
  (dolist (opcode '(49 50 130 131 132 133 134))
    (should (equal (nelisp-bytecode-decode (unibyte-string opcode 2 1))
                   (list (list 0 (aref byte-code-vector opcode) 258)))))
  (should
   (equal (nelisp-bytecode-decode
           (unibyte-string 175 7 176 8 177 9 178 10 179 2 1 182 131 182 4 135))
          '((0 byte-listN 7) (2 byte-concatN 8) (4 byte-insertN 9)
            (6 byte-stack-set 10) (8 byte-stack-set 258)
            (11 byte-discardN-preserve-tos 3) (13 byte-discardN 4)
            (15 byte-return nil))))
  ;; Host disassembly independently confirms widths and normalized names.
  (dolist (bytes (list (unibyte-string 8 9 10 11 12 13 14 42 15 2 1 135)
                      (unibyte-string 192 255 129 2 1 135)
                      (unibyte-string 175 7 176 8 177 9 178 10
                                      179 2 1 182 131 135)))
    (should
     (equal (mapcar (lambda (row) (list (car row) (cadr row)))
                    (nelisp-bytecode-decode bytes))
            (nelisp-bytecode-test-disassembly
             (make-byte-code 0 bytes (make-vector 300 'fixture) 300)))))
  (dolist (opcode '(49 50 130 131 132 133 134))
    (let ((bytes (unibyte-string opcode 3 0 135)))
      (should
       (equal (mapcar (lambda (row) (list (car row) (cadr row)))
                      (nelisp-bytecode-decode bytes))
              (nelisp-bytecode-test-disassembly
               (make-byte-code 0 bytes [] 1)))))))

(ert-deftest nelisp-bytecode-decode-rejects-malformed ()
  (should (equal (nelisp-bytecode-decode (unibyte-string)) nil))
  (should-error (nelisp-bytecode-decode 42))
  (should-error (nelisp-bytecode-decode (string #x100)))
  (should-error (nelisp-bytecode-decode (unibyte-string 184)))
  (dolist (opcode '(6 7 14 15 22 23 30 31 38 39 46 47
                     49 50 129 130 131 132 133 134 175 176 177 178 179 182))
    (should-error (nelisp-bytecode-decode (unibyte-string opcode))))
  (dolist (opcode '(7 15 23 31 39 47 49 50 129 130 131 132 133 134 179))
    (should-error (nelisp-bytecode-decode (unibyte-string opcode 1)))))

(ert-deftest nelisp-bytecode-decode-source-parity ()
  (let* ((files (nelisp-bytecode-source-paths))
         (entries (nelisp-bytecode-compile-sources files))
         (objects 0))
    (should (>= (length entries) 20))
    (dolist (file files)
      (should (assoc file entries)))
    (dolist (entry entries)
      (dolist (object (nelisp-bytecode-objects (nth 2 entry)))
        (ert-info ((format "%s: %s" (car entry) (cadr entry)))
          (let* ((bytes (aref object 1))
                 (decoded (nelisp-bytecode-decode bytes))
                 (expected (nelisp-bytecode-test-disassembly object)))
            (should expected)
            ;; Compare names AND every instruction boundary to the host.
            (should (equal (mapcar (lambda (row) (list (car row) (cadr row)))
                                   decoded)
                           expected))
            ;; A sentinel must start exactly at the original end.
            (should (equal (nelisp-bytecode-decode
                            (concat bytes (unibyte-string byte-return)))
                           (append decoded
                                   (list (list (length bytes)
                                               'byte-return nil)))))
            ;; Every cut inside a host-observed operand must be rejected.
            (cl-loop for tail on expected
                     for start = (caar tail)
                     for end = (if (cdr tail) (caadr tail) (length bytes))
                     do (cl-loop for cut from (1+ start) below end
                                 do (should-error
                                     (nelisp-bytecode-decode
                                      (substring bytes 0 cut)))))
            (cl-incf objects)))))
    (message "Parity: %d source functions, %d bytecode objects, %d files"
             (length entries) objects (length files))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-decode-selftest.el ends here
