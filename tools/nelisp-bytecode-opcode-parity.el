;;; nelisp-bytecode-opcode-parity.el --- Ordered opcode parity cases -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-corpus-parity)

(defconst nelisp-bytecode-opcode-parity-cases
  '((call (192 193 194 34 135) [list 1 2] 4)
    (car (192 64 135) [(1 . 2)] 2)
    (cdr (192 65 135) [(1 . 2)] 2)
    (varref (193 24 8 41 135) [bytecode-x 7] 2)
    (goto-if-not-nil (192 132 8 0 193 130 9 0 194 135) [t no yes] 2)
    (goto-if-nil-else-pop (192 133 5 0 193 135) [nil bad] 2)
    (cons (192 193 66 135) [1 2] 3)
    (memq (192 193 62 135) [b (a b c)] 3)
    (eq (192 193 61 135) [same same] 3)
    (goto-if-not-nil-else-pop (192 134 5 0 193 135) [t bad] 2)
    (car-safe (192 162 135) [4] 2)
    (not (192 63 135) [nil] 2)
    (discardN (192 193 182 1 135) [1 2] 3)
    (list1 (192 67 135) [1] 2)
    (length (192 71 135) [(a b c)] 2)
    (consp (192 58 135) [(a)] 2)
    (unbind (193 24 192 41 135) [bytecode-x 7] 2)
    (eqlsign (192 193 85 135) [2 2] 3)
    (varbind (193 24 8 41 135) [bytecode-x 7] 2)
    (sub1 (192 83 135) [3] 2)
    (gtr (192 193 86 135) [3 2] 3)
    (nth (192 193 56 135) [1 (a b)] 3)
    (setcar (192 193 160 135) [(1 . 2) 9] 3)
    (diff (192 193 90 135) [7 2] 3)
    (varset (193 24 194 16 8 41 135) [bytecode-x 1 2] 2))
  "One focused case per requested opcode, in implementation order.")

(defconst nelisp-bytecode-regression-forms
  '((plus . (+ 1 2))
    (if . (if (< 3 2) 'yes 'no))
    (string . "abc")
    (negative-plus . (+ -5 3))
    (loop-five . (let ((i 0))
                   (while (< i 5) (setq i (1+ i))) i))
    (loop-sum . (let ((s 0) (i 0))
                  (while (< i 10)
                    (setq s (+ s i)) (setq i (1+ i))) s))
    (carry-symbol . (let ((i 0) (x 'carried-symbol))
                      (while (< i 2) (setq i (1+ i))) x))
    (carry-string . (let ((i 0) (x "carried-string"))
                      (while (< i 2) (setq i (1+ i))) x))
    (carry-cons . (let ((i 0) (x '(left . right)))
                    (while (< i 2) (setq i (1+ i))) x))
    (carry-list . (let ((i 0) (x '(left right)))
                    (while (< i 2) (setq i (1+ i))) x)))
  "Pre-existing byte-code values and loop-root regressions.")

(defun nelisp-bytecode-opcode-parity-run ()
  (let* ((root (nelisp-bytecode-corpus--root))
         (default-directory root)
         (output (expand-file-name "target/bytecode-opcode-parity/" root))
         (standalone (expand-file-name "target/nelisp" root))
         (host (expand-file-name invocation-name invocation-directory))
         (passed 0) (regression-passed 0) failures)
    (make-directory output t)
    (cl-loop
     for case in nelisp-bytecode-opcode-parity-cases
     for index from 1
     for name = (nth 0 case)
     for bytes = (nth 1 case)
     for constants = (nth 2 case)
     for depth = (nth 3 case)
     for form = `(byte-code (unibyte-string ,@bytes) ',constants ,depth)
     for printed = (prin1-to-string form)
     for stem = (expand-file-name (format "%02d-%s" index name) output)
     for host-out = (concat stem ".host.out")
     for host-err = (concat stem ".host.err")
     for standalone-out = (concat stem ".standalone.out")
     for standalone-err = (concat stem ".standalone.err")
     for host-status = (nelisp-bytecode-corpus--run
                        host (list "--batch" "-Q" "--eval"
                                   (format "(prin1 %s)" printed))
                        host-out host-err)
     for standalone-status = (nelisp-bytecode-corpus--run
                              standalone (list "--eval" printed)
                              standalone-out standalone-err)
     for host-value = (nelisp-bytecode-corpus--result-text host-out)
     for standalone-value = (nelisp-bytecode-corpus--result-text
                              standalone-out)
     do
     (nelisp-bytecode-corpus--write (concat stem ".expr") printed)
     (nelisp-bytecode-corpus--write
      (concat stem ".status")
      (format "host=%s\nstandalone=%s\n" host-status standalone-status))
     (if (and (equal host-status 0) (equal standalone-status 0)
              (equal host-value standalone-value))
         (cl-incf passed)
       (push (list name host-status standalone-status
                   host-value standalone-value)
             failures)))
    (cl-loop
     for case in nelisp-bytecode-regression-forms
     for index from 1
     for name = (car case)
     for object = (byte-compile `(lambda () ,(cdr case)))
     for form = (nelisp-bytecode-corpus--call-form object)
     for printed = (let ((print-escape-newlines t)
                         (print-escape-control-characters t)
                         (print-escape-nonascii t))
                     (prin1-to-string form))
     for stem = (expand-file-name (format "reg-%02d-%s" index name) output)
     for host-out = (concat stem ".host.out")
     for host-err = (concat stem ".host.err")
     for standalone-out = (concat stem ".standalone.out")
     for standalone-err = (concat stem ".standalone.err")
     for host-status = (nelisp-bytecode-corpus--run
                        host (list "--batch" "-Q" "--eval"
                                   (format "(prin1 %s)" printed))
                        host-out host-err)
     for standalone-status = (nelisp-bytecode-corpus--run
                              standalone (list "--eval" printed)
                              standalone-out standalone-err)
     for host-value = (nelisp-bytecode-corpus--result-text host-out)
     for standalone-value = (nelisp-bytecode-corpus--result-text
                              standalone-out)
     do
     (nelisp-bytecode-corpus--write (concat stem ".expr") printed)
     (nelisp-bytecode-corpus--write
      (concat stem ".status")
      (format "host=%s\nstandalone=%s\n" host-status standalone-status))
     (if (and (equal host-status 0) (equal standalone-status 0)
              (equal host-value standalone-value))
         (cl-incf regression-passed)
       (push (list name host-status standalone-status
                   host-value standalone-value)
             failures)))
    (setq failures (nreverse failures))
    (let ((report (expand-file-name "report.txt" output)))
      (with-temp-file report
        (insert (format "opcode-cases=%d\nopcode-passed=%d\n"
                        (length nelisp-bytecode-opcode-parity-cases)
                        passed))
        (insert (format "regression-cases=%d\nregression-passed=%d\nfailed=%d\n"
                        (length nelisp-bytecode-regression-forms)
                        regression-passed (length failures)))
        (dolist (row failures)
          (insert (format "%S host-status=%S standalone-status=%S host=%S standalone=%S\n"
                          (nth 0 row) (nth 1 row) (nth 2 row)
                          (nth 3 row) (nth 4 row)))))
      (princ (with-temp-buffer
               (insert-file-contents report)
               (buffer-string))))))

(provide 'nelisp-bytecode-opcode-parity)
;;; nelisp-bytecode-opcode-parity.el ends here
