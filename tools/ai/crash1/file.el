;;; crash1/file.el --- B1 corpus: files and process output larger than 8 MiB  -*- lexical-binding: t -*-
;; Usage: CRASH1_DATA=DIR CRASH1_KIND=ifc|literal|callproc EMACS --load file.el
;; Each line: NAME KIND SIZE MD5, from the buffer contents after the read.
(let* ((dir (getenv "CRASH1_DATA")) (kind (getenv "CRASH1_KIND"))
       (names (sort (directory-files dir nil "\\`\\(ascii\\|utf8\\|bin\\)-") #'string<)))
  (dolist (name names)
    (let ((file (expand-file-name name dir)))
      (when (eq (and (string= kind "literal") t) (and (string-suffix-p ".bin" name) t))
        (message "%s %s %S" name kind
                 (condition-case e
                     (with-temp-buffer
                       (cond ((string= kind "ifc") (insert-file-contents file))
                             ((string= kind "literal")
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally file))
                             (t (call-process "cat" nil t nil file)))
                       (let ((str (buffer-substring-no-properties (point-min) (point-max))))
                         (list (string-bytes str) (secure-hash 'md5 str))))
                   (error (list 'ERR (car e) (cdr e)))))))))
