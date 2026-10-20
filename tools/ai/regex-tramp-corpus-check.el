;;; regex-tramp-corpus-check.el --- public results and all captures -*- lexical-binding: nil; -*-
(when (getenv "RT_MODULE")
  (load (getenv "RT_MODULE"))
  (nlre--compiled-cache-clear))
(load (expand-file-name "test/regex-tramp-corpus.el"))
(let ((count 0))
  (dolist (row rt-corpus)
    (dolist (text (nth 2 row))
      (let ((result (condition-case e
                        (let ((hit (string-match (nth 1 row) text)))
                          (list hit (and hit (match-data t))))
                      (error (list :error e)))))
        (princ (format "%S %S %S\n" (car row) text result))
        (setq count (1+ count)))))
  (princ (format "RT-CORPUS-CHECK %d\n" count)))
nil
