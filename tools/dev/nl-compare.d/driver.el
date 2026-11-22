;;; driver.el --- nl-compare record driver, loaded unchanged on NeLisp and GNU  -*- lexical-binding: t; -*-
;; Reads every top-level form of `nlc-forms-file' (the engine's own reader), evaluates each
;; one at a time (each in its own fresh with-temp-buffer, so *scratch* mode cannot leak in) and prints exactly one machine-readable line per form:
;;   NLC<TAB>INDEX<TAB>ok|err<TAB>ESCAPED-PRIN1-VALUE<TAB>ESCAPED-PRIN1-FORM
;; ok value = the form's value; err value = (ERROR-SYMBOL . DATA).  Escaping makes every record
;; one physical line of pure ASCII (\\ \n \t \r, everything else outside 32..126 -> \x{HEX}),
;; so no result can split a record or shift later rows.  The hex digits are built from a string
;; table, never (format "%02x"), so the escaper cannot itself diverge.
(defvar nlc-forms-file nil)
(defconst nlc--hex "0123456789abcdef")
(defun nlc--hexstr (n)
  (let ((r nil))
    (if (= n 0) (setq r (list ?0)))
    (while (> n 0) (setq r (cons (aref nlc--hex (% n 16)) r) n (/ n 16)))
    (concat r)))
(defun nlc--esc (s)
  (let ((parts nil) (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (setq parts
              (cons (cond ((= c 92) "\\\\") ((= c 10) "\\n") ((= c 9) "\\t") ((= c 13) "\\r")
                          ((or (< c 32) (> c 126)) (concat "\\x{" (nlc--hexstr c) "}"))
                          (t (char-to-string c)))
                    parts)))
      (setq i (1+ i)))
    (apply #'concat (nreverse parts))))
(defun nlc--p1 (v)
  (let ((print-length nil) (print-level nil) (print-circle nil) (print-escape-newlines nil))
    (prin1-to-string v)))
(defun nlc-run ()
  (let ((forms nil) (idx 0))
    (with-temp-buffer
      (insert-file-contents nlc-forms-file)
      (goto-char (point-min))
      (let ((go t))
        (while go
          (condition-case nil
              (setq forms (cons (read (current-buffer)) forms))
            (end-of-file (setq go nil))))))
    (setq forms (nreverse forms))
    (dolist (f forms)
      (let ((r (condition-case e (list 'ok (with-temp-buffer (eval f t))) (t (list 'err e)))))
        (princ (concat "NLC\t" (number-to-string idx) "\t" (symbol-name (car r)) "\t"
                       (nlc--esc (nlc--p1 (car (cdr r)))) "\t" (nlc--esc (nlc--p1 f)) "\n")))
      (setq idx (1+ idx)))
    (princ (concat "NLC-DONE\t" (number-to-string idx) "\n"))))
