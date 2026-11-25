;;; par-load-source-bytes.el --- Streaming source load offsets -*- lexical-binding: t; -*-

(defvar par-load-source-bytes-values nil)
(defvar par-load-source-bytes-closure nil)

(let ((file (make-temp-file "par-load-source-bytes-" nil ".el"))
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (concat
          (encode-coding-string
           (concat ";;; -*- lexical-binding: t; coding: utf-8 -*-\n"
                   "(require 'par-load-source-bytes-missing nil t)\n"
                   "(push (length \"" (make-string 501 ?x) "日本\") par-load-source-bytes-values)\n"
                   "(push (string-to-list \"é") 'utf-8)
          (unibyte-string #xff)
          "\") par-load-source-bytes-values)\n"
          "(push (string-to-list \""
          (unibyte-string #xff)
          "\") par-load-source-bytes-values)\n"
          "(push 42 par-load-source-bytes-values)\n")
         nil file nil 'silent)
        (prin1 (list 'source-byte-offsets (load file nil t t)
                     (nreverse par-load-source-bytes-values)))
        (terpri))
    (delete-file file)))

(let ((file (make-temp-file "par-load-source-script-" nil ".el")))
  (unwind-protect
      (progn
        (write-region
         (concat "#!/usr/bin/emacs --script\n"
                 ";;; -*- lexical-binding: t -*-\n"
                 "(require 'par-load-source-script-missing nil t)\n"
                 "(setq par-load-source-bytes-closure\n"
                 "      (let ((answer 41)) (lambda () (1+ answer))))\n")
         nil file nil 'silent)
        (prin1 (list 'source-script-lexical-binding (load file nil t t)
                     (funcall par-load-source-bytes-closure)))
        (terpri))
    (delete-file file)))

(let ((file (make-temp-file "par-load-source-external-" nil ".el"))
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (dolist (incremental '(nil t))
        (write-region
         (concat ";;; -*- lexical-binding: t; coding: utf-8 -*-\n"
                 (if incremental "(require 'cl-lib)\n" "")
                 "(setq par-load-source-bytes-values (string-to-list \""
                 (unibyte-string 193 191) "\"))\n")
         nil file nil 'silent)
        (prin1 (list 'source-external-byte-pair incremental (load file nil t t)
                     par-load-source-bytes-values))
        (terpri))
    (delete-file file)))
