;;; standalone-base64-decode-parity.el --- native base64 decode parity -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-base64-decode-parity.el
;;
;; Compares the reader's native `nelisp--base64-decode' (falling back to
;; `base64-decode-string' when it answers nil, as the native loader
;; does) with `base64-decode-string' over random byte strings encoded
;; with and without line breaks and over malformed inputs, then times a
;; 533K-character decode.  Prints one B64-PARITY line and exits non-zero
;; on any difference, when the builtin is missing, or when the decode
;; takes 0.5 s or more.

(defvar b64-n 0) (defvar b64-bad 0)
(defun b64-one (enc)
  (setq b64-n (1+ b64-n))
  (let ((want (condition-case e (base64-decode-string enc) (error (list 'err (car e)))))
        (got (or (and (fboundp (quote nelisp--base64-decode)) (nelisp--base64-decode enc)) (condition-case e (base64-decode-string enc) (error (list 'err (car e)))))))
    (unless (and (equal want got) (or (consp want) (equal (multibyte-string-p want) (multibyte-string-p got))))
      (setq b64-bad (1+ b64-bad))
      (when (< b64-bad 40) (princ (format "DIFF %S: %S vs %S\n" enc want got))))))
(let ((seed 7))
  (dotimes (len 70)
    (let ((l nil) bytes)
      (dotimes (_ len) (setq seed (% (+ (* seed 1103515245) 12345) 2147483648)) (push (% (/ seed 65536) 256) l))
      (setq bytes (apply #'unibyte-string l))
      (let ((enc (base64-encode-string bytes t)))
        (b64-one enc)
        (b64-one (base64-encode-string bytes))
        (when (> (length enc) 4) (b64-one (concat (substring enc 0 4) "\n" (substring enc 4))))))))
(dolist (enc '("" "=" "==" "QQ" "QQ=" "QQ==" "QUI" "QUI=" "QUJD" "QU=I" "Q!==" "QQ==QQ==" "Q Q=" "QUJD\r\nREVG" "-_-_" "QUJD===" "4oCU"))
  (b64-one enc))
(let* ((raw (make-string 400000 ?z))
       (enc (base64-encode-string raw t))
       (_ (garbage-collect))
       (t0 (float-time))
       (decoded (and (fboundp 'nelisp--base64-decode) (nelisp--base64-decode enc)))
       (secs (- (float-time) t0))
       (ok (and (fboundp 'nelisp--base64-decode) (= b64-bad 0)
                (equal decoded raw) (< secs 0.5))))
  (princ (format "B64-PARITY cases=%d diffs=%d decode533k=%.4fs %s\n"
                 b64-n b64-bad secs (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-base64-decode-parity.el ends here
