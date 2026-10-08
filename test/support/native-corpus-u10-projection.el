;;; native-corpus-u10-projection.el --- Exact GNU fixture projections -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)

(defun u10-projection-validate (actual expected)
  "Reject any decoded projection different from its canonical forms."
  (unless (equal actual expected)
    (error "U10 projection differs from canonical fixture"))
  t)

(defun u10-projection-write (directory rows protected oracle)
  "Write authenticated projections directly from final canonical objects."
  (make-directory directory t)
  (let ((units (mapcar (lambda (row)
                        (cons (number-to-string (plist-get row :opcode))
                              (list (list 'setq 'u10-fixtures
                                          (list 'cons (list 'quote row) 'u10-fixtures)))))
                      rows))
        metadata)
    (push (cons "protected" (list (list 'setq 'u10-protected-function (list 'quote protected)
                                       'u10-protected-oracle (list 'quote oracle)))) units)
    (dolist (unit units)
      (let ((file (expand-file-name (concat (car unit) ".el") directory)))
        (with-temp-file file
          (insert ";;; GNU U10 projection -*- lexical-binding: t; -*-\n")
          (let ((print-length nil) (print-level nil) (print-circle nil) (print-escape-newlines t))
            (dolist (form (cdr unit))
              (prin1 form (current-buffer)) (insert "\n"))))
        (with-temp-buffer
          (insert-file-contents file)
          (let (forms)
            (while (progn (skip-chars-forward " \t\n\r") (not (eobp)))
              (push (read (current-buffer)) forms))
            (u10-projection-validate (nreverse forms) (cdr unit))))
        (push (cons (car unit)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally file)
                      (secure-hash 'sha256 (current-buffer)))) metadata)))
    (nreverse metadata)))

(provide 'native-corpus-u10-projection)
