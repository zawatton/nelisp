;;; corpus-hash-bytes.el --- secure-hash/md5 over bytes vs GNU 31.1  -*- lexical-binding: t; -*-
;; Run from the repository root with tools/ai/cl-gnu/run-corpus.el.
(progn (defvar shb-dir (expand-file-name "tools/ai/secure-hash/")) t)
(with-temp-buffer (insert-file-contents-literally (concat shb-dir "nonascii.txt")) (list (buffer-size) enable-multibyte-characters (secure-hash 'sha256 (current-buffer))))
(with-temp-buffer (insert-file-contents-literally (concat shb-dir "rawbytes.bin")) (list (buffer-size) (secure-hash 'sha256 (current-buffer)) (md5 (current-buffer))))
(with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally (concat shb-dir "nonascii.txt")) (list enable-multibyte-characters (secure-hash 'sha256 (current-buffer))))
(with-temp-buffer (insert-file-contents (concat shb-dir "nonascii.txt")) (list (buffer-size) (secure-hash 'sha256 (current-buffer))))
(with-temp-buffer (insert-file-contents-literally (concat shb-dir "nonascii.txt")) (secure-hash 'sha256 (current-buffer) 5 9))
(with-temp-buffer (insert-file-contents-literally (concat shb-dir "nonascii.txt")) (secure-hash 'sha256 (buffer-string)))
(secure-hash 'sha256 "abc é")
(secure-hash 'sha256 (encode-coding-string "abc é" 'utf-8))
(secure-hash 'sha256 (string-to-unibyte "\377\200a"))
(secure-hash 'sha256 (string 4194303 4194176 97))
(md5 "abc é")
(md5 (string 4194303 97))
(secure-hash 'sha1 "’")
(secure-hash 'sha256 "abc é" nil nil t)
;;; corpus-hash-bytes.el ends here
