;;; -*- lexical-binding: t; -*-
;; Every coding system of type `charset' (coding.c decode_coding_charset /
;; encode_coding_charset): decoding all 256 bytes (characters, raw bytes,
;; `charset' property runs), encoding the result back, an unencodable
;; character, EOL variants and `last-coding-system-used'.  Spec = GNU 31.1
;; (par-coding-charset.gnu).
;; Keep charset annotations visible even when every decoded char is ASCII.
(defmacro row (n f)
  `(let ((print-charset-text-property t))
     (princ (format "%-28s %S\n" ,n
                    (condition-case e ,f
                      (error (list 'ERR (car e) (cadr e))))))))
(defvar pcc-all (let ((s (make-string 256 0))) (dotimes (i 256) (aset s i i)) (string-to-unibyte s)))
(defun pcc-runs (s)
  (let ((pos 0) (out nil))
    (while (< pos (length s))
      (let ((next (or (next-single-property-change pos 'charset s) (length s))))
        (push (list pos next (get-text-property pos 'charset s)) out)
        (setq pos next)))
    (nreverse out)))
(defun pcc-row (cs)
  (let* ((d (decode-coding-string pcc-all cs))
         (lcs last-coding-system-used)
         (e (encode-coding-string d cs)))
    (list (secure-hash 'md5 (format "%S" (string-to-list d)))
          (length d) (pcc-runs d) lcs (string= e pcc-all)
          (encode-coding-string "a一b" cs)
          (decode-coding-string "x\r\ny" cs) last-coding-system-used
          (encode-coding-string "x\ny" (coding-system-change-eol-conversion cs 'dos)))))
(defvar pcc-systems
  (sort (seq-filter (lambda (c) (and (eq (coding-system-type c) 'charset) (eq c (coding-system-base c))))
                    coding-system-list)
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
(row "S0 systems" pcc-systems)
(dolist (cs pcc-systems) (row (format "S %s" cs) (pcc-row cs)))
(row "A1 aliases" (list (decode-coding-string "abc\351" 'iso-8859-15) (encode-coding-string "abcé" 'iso-8859-15) (decode-coding-string "\351" 'latin-1) (decode-coding-string "\301" 'cp1251)))
(row "L1 load-path subdirs" (mapcar (lambda (d) (file-name-nondirectory (directory-file-name d)))
                                    (seq-filter (lambda (d) (string-match-p "/\\(language\\|international\\|leim\\)/?\\'" d)) load-path)))
(row "L2 japan-util" (and (locate-library "japan-util") t))

(defun pcc-eol-row (cs bytes)
  (let* ((d (decode-coding-string bytes cs))
         (decoded-with last-coding-system-used)
         (encoded (encode-coding-string "A\r\nB" cs)))
    (list (string-to-list d) (pcc-runs d) decoded-with
          (string-to-list encoded) last-coding-system-used)))

;; EBCDIC's LF is byte 37.  Byte 10 decodes to a different character.
(dolist (cs pcc-systems)
  (dolist (eol '(nil unix dos mac))
    (let ((coding (if eol (coding-system-change-eol-conversion cs eol) cs)))
      (row (format "E %s %s" cs (or eol 'auto))
           (mapcar (lambda (bytes) (pcc-eol-row coding bytes))
                   (list "abc" "x\r\ny" (unibyte-string 193 13 37 194)
                         (unibyte-string 193 13)))))))
(row "E inhibited" (let ((inhibit-eol-conversion t))
                     (pcc-eol-row 'ebcdic-us-dos
                                  (unibyte-string 193 13 37 194))))
