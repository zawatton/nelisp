;;; hex-util.el --- lightweight hexadecimal string helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Small pure-Elisp implementation of the standard `hex-util' feature.
;; Several network/auth vendor modules use this for byte-string encoding
;; and decoding, so keeping it standalone widens the Class-A vendor lane.

;;; Code:

(defun hex-util--char-to-number (char)
  "Return numeric value for hexadecimal digit CHAR."
  (cond
   ((and (<= ?0 char) (<= char ?9)) (- char ?0))
   ((and (<= ?a char) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (<= ?A char) (<= char ?F)) (+ 10 (- char ?A)))
   (t (error "Invalid hexadecimal digit `%c'" char))))

(defun hex-util--number-to-char (number)
  "Return lowercase hexadecimal digit for NUMBER."
  (aref "0123456789abcdef" number))

(defun decode-hex-string (string)
  "Decode hexadecimal STRING to an octet string."
  (let* ((len (length string))
         (dst (make-string (/ len 2) 0))
         (idx 0)
         (pos 0))
    (while (< pos len)
      (aset dst idx
            (+ (* (hex-util--char-to-number (aref string pos)) 16)
               (hex-util--char-to-number (aref string (1+ pos)))))
      (setq idx (1+ idx)
            pos (+ pos 2)))
    dst))

(defun encode-hex-string (string)
  "Encode octet STRING to a lowercase hexadecimal string."
  (let* ((len (length string))
         (dst (make-string (* len 2) 0))
         (idx 0)
         (pos 0))
    (while (< pos len)
      (let ((char (aref string pos)))
        (aset dst idx (hex-util--number-to-char (/ char 16)))
        (setq idx (1+ idx))
        (aset dst idx (hex-util--number-to-char (% char 16)))
        (setq idx (1+ idx)
              pos (1+ pos))))
    dst))

(provide 'hex-util)

;;; hex-util.el ends here
