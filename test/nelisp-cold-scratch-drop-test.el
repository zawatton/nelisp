;;; nelisp-cold-scratch-drop-test.el --- Doc 241 S2 matcher scratch not dumped  -*- lexical-binding: t; -*-

;;; Commentary:

;; The native regexp matcher's 3,200,000-byte scratch (`nlre--nat-scratch',
;; created lazily, ~100% zero) must not be carried by the cold image: both
;; image-dump forms of the generator reset it before `garbage-collect'.

;;; Code:

(require 'ert)

(defconst nelisp-cold-scratch-drop-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest nelisp-cold-scratch-drop-both-dump-forms ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "scripts/nelisp-standalone-build.el"
                       nelisp-cold-scratch-drop-test--root))
    (let ((n 0) (pos (point-min)))
      (while (string-match
              "(setq nlre--nat-scratch nil) nil)[ ]?(garbage-collect) (nelisp--arena-dump-image-stream"
              (buffer-string) pos)
        (setq n (1+ n) pos (match-end 0)))
      (should (= n 2)))))

(provide 'nelisp-cold-scratch-drop-test)
;;; nelisp-cold-scratch-drop-test.el ends here
