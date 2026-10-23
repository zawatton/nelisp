;;; integ-wh-raw-file.el --- literal file reads keep every byte  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1 with IWH_RAW_DIR naming a directory that holds raw256.bin (the
;; bytes 0..255), utf8.txt (UTF-8 text with CRLF) and mixed.bin (written by integ-wh-check.sh raw-file); both
;; must print the same bytes.  A literal read (insert-file-contents-literally, insert-file-contents under
;; coding-system-for-read no-conversion) inserts the file's bytes into a unibyte buffer.  The RAW argument of the
;; read-file primitive rewrote a legacy view's tag byte on a canonical String, so it had no effect: any file with
;; a byte in 80..BF came back as a multibyte string whose length and aref disagree, and insert-file-contents-literally
;; signalled args-out-of-range or inserted decoded characters instead of bytes.

(defun iwh-rf (tag x) (princ (format "RAWF %-26s %S\n" tag x)))
(defmacro iwh-rfs (tag &rest body)
  `(condition-case e (progn ,@body) (error (iwh-rf ,tag (list 'SIGNAL (car e))))))
(defun iwh-rf-file (f)
  (iwh-rf "file" (file-name-nondirectory f))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((r (condition-case e (insert-file-contents-literally f) (error (list 'ERR (car e))))))
      (iwh-rf "ifcl result length" (cadr r))
      (iwh-rf "ifcl buffer-size" (buffer-size))
      (iwh-rf "ifcl multibyte" enable-multibyte-characters)
      (let ((s (buffer-string)))
        (iwh-rf "ifcl string len/mb" (list (length s) (multibyte-string-p s)))
        (iwh-rfs "ifcl byte sum" (iwh-rf "ifcl byte sum" (apply #'+ (append s nil))))
        (iwh-rfs "ifcl last 3" (iwh-rf "ifcl last 3" (let ((n (length s)))
                                                       (and (> n 3) (list (aref s (- n 3)) (aref s (- n 2)) (aref s (- n 1)))))))
        (iwh-rfs "ifcl sxhash" (iwh-rf "ifcl equal copy" (equal s (copy-sequence s)))))))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'no-conversion)
           (r (condition-case e (insert-file-contents f) (error (list 'ERR (car e))))))
      (iwh-rf "ifc/no-conv result length" (cadr r))
      (iwh-rf "ifc/no-conv buffer-size" (buffer-size))
      (iwh-rfs "ifc/no-conv byte sum"
               (iwh-rf "ifc/no-conv byte sum" (apply #'+ (append (buffer-string) nil))))))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((r (condition-case e (insert-file-contents-literally f nil 5 40) (error (list 'ERR (car e))))))
      (iwh-rf "ifcl 5..40 result" (cadr r))
      (iwh-rfs "ifcl 5..40 sum"
               (iwh-rf "ifcl 5..40 sum" (apply #'+ (append (buffer-string) nil)))))))
(dolist (n '("raw256.bin" "utf8.txt" "mixed.bin"))
  (iwh-rf-file (expand-file-name n (getenv "IWH_RAW_DIR"))))
;; default decoding of valid UTF-8 text (invalid byte sequences decode differently from GNU and are not compared here)
(with-temp-buffer
  (let ((r (insert-file-contents (expand-file-name "utf8.txt" (getenv "IWH_RAW_DIR")))))
    (iwh-rf "ifc default utf8 result" (cadr r))
    (iwh-rf "ifc default utf8 text" (buffer-string))))
(princ "INTEG-WH-RAW-FILE DONE\n")
