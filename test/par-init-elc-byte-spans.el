;;; par-init-elc-byte-spans.el --- Compiled reader raw bytes -*- lexical-binding: t; -*-

(defvar par-init-elc-byte-spans-value nil)
(let ((file (make-temp-file "par-init-elc-byte-spans-" nil ".elc"))
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (concat ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n"
                 "(setq par-init-elc-byte-spans-value "
                 "#(\"" "abcdefghijklmnopq" (unibyte-string 193 191)
                 "rstuvwxyz12345678" (unibyte-string 227 129 130)
                 "tail\" 0 1 (par-init-elc-byte-spans t)))\n")
         nil file nil 'silent)
        (let* ((loaded (load file nil t t))
               (value par-init-elc-byte-spans-value))
          (prin1 (list 'elc-byte-spans loaded (length value) (string-bytes value)
                       (get-text-property 0 'par-init-elc-byte-spans value)
                       (aref value 17)
                       (append (substring value -5) nil)))
          (terpri)))
    (delete-file file)))
