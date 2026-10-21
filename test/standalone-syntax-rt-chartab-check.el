;;; standalone-syntax-rt-chartab-check.el --- Doc 225 GNU 31.1 fixture as a standalone check -*- lexical-binding: t; -*-

;; Runs test/fixtures/syntax-rt-chartab.el; its output must equal
;; standalone-syntax-rt-chartab-check.expected, GNU 31.1's own output for the
;; same file (test/fixtures/syntax-rt-chartab.gnu-31.1.expected).
(load (expand-file-name "fixtures/syntax-rt-chartab.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; standalone-syntax-rt-chartab-check.el ends here
