;;; emacs-cc-byteorder-1.el --- Byte order primitive -*- lexical-binding: t; -*-

(defun emacs-cc-byteorder-1--decode-htonl (value)
  "Decode the result of libc htonl applied to one."
  (cond
   ((equal value 1) ?B)
   ((equal value #x01000000) ?l)
   (t (error "byteorder: unexpected htonl result %S" value))))

(defun emacs-cc-byteorder-1--observe ()
  "Return GNU's byteorder code using the actual system libc."
  (require 'nl-ffi)
  (require 'emacs-network-ffi)
  (let ((libc emacs-network-ffi-libc-path))
    (unless (and (stringp libc) (> (length libc) 0))
      (error "byteorder: system libc path is unavailable"))
    (ffi:library libc)
    (emacs-cc-byteorder-1--decode-htonl
     (nl-ffi--invoke 'byteorder-htonl "htonl" '(:uint32) :uint32 '(1)))))

(unless (fboundp 'byteorder)
  (defun byteorder (&rest arguments)
    "Return 66 for big-endian machines or 108 for little-endian machines."
    (when arguments
      (signal 'wrong-number-of-arguments
              (list 'byteorder (length arguments))))
    (emacs-cc-byteorder-1--observe)))

(provide 'emacs-cc-byteorder-1)
;;; emacs-cc-byteorder-1.el ends here
