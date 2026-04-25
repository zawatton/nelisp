;;; nelisp-base64.el --- Pure NeLisp RFC 4648 base64  -*- lexical-binding: t; -*-

;; T62 / T54 Wave 1 agent E deliverable.
;; anvil-disk / anvil-server / anvil-data secure-hash + base64 unblock for
;; the standalone path without host `base64-*' dependency.

;;; Commentary:

;; Pure Elisp base64 encoder/decoder for the NeLisp standalone path.
;; Public API mirrors `base64-encode-string' / `base64-decode-string'
;; for unibyte data, plus explicit byte-oriented helpers.  Multibyte
;; input is rejected on the string API to match host Emacs behavior.

;;; Code:

(require 'cl-lib)

(defconst nelisp-b64--alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "RFC 4648 base64 alphabet.")

(defconst nelisp-b64--padding ?=
  "RFC 4648 base64 padding character.")

(defvar nelisp-b64--decode-table nil
  "Char-table mapping base64 alphabet chars to 6-bit values.")

(defun nelisp-b64--ensure-decode-table ()
  "Initialize `nelisp-b64--decode-table' once."
  (unless nelisp-b64--decode-table
    (let ((table (make-char-table 'nelisp-b64--decode-table nil))
          (index 0))
      (while (< index (length nelisp-b64--alphabet))
        (aset table (aref nelisp-b64--alphabet index) index)
        (setq index (1+ index)))
      (setq nelisp-b64--decode-table table)))
  nelisp-b64--decode-table)

(defun nelisp-b64--byte-list (bytes)
  "Normalize BYTES to a list of byte integers."
  (cond
   ((stringp bytes)
    (when (multibyte-string-p bytes)
      (error "Multibyte character in data for base64 encoding"))
    (mapcar (lambda (byte) (logand byte #xFF)) (append bytes nil)))
   ((listp bytes)
    (mapcar (lambda (byte)
              (unless (and (integerp byte) (>= byte 0) (< byte 256))
                (signal 'wrong-type-argument (list 'integerp byte)))
              byte)
            bytes))
   (t
    (signal 'wrong-type-argument (list 'sequencep bytes)))))

(defun nelisp-b64--wrap-lines (encoded)
  "Insert MIME line breaks every 76 chars in ENCODED."
  (let ((length (length encoded))
        (start 0)
        (chunks '()))
    (while (< start length)
      (push (substring encoded start (min length (+ start 76))) chunks)
      (setq start (+ start 76)))
    (mapconcat #'identity (nreverse chunks) "\n")))

(defun nelisp-b64-encode-bytes (bytes)
  "Encode BYTES to RFC 4648 base64.
BYTES must be a unibyte string or a list of integers in [0,255]."
  (let ((input (nelisp-b64--byte-list bytes))
        (chunks '()))
    (while input
      (let* ((b0 (pop input))
             (b1 (when input (pop input)))
             (b2 (when input (pop input)))
             (word (logior (ash b0 16)
                           (ash (or b1 0) 8)
                           (or b2 0)))
             (c0 (aref nelisp-b64--alphabet (logand (ash word -18) #x3F)))
             (c1 (aref nelisp-b64--alphabet (logand (ash word -12) #x3F)))
             (c2 (if b1
                     (aref nelisp-b64--alphabet (logand (ash word -6) #x3F))
                   nelisp-b64--padding))
             (c3 (if b2
                     (aref nelisp-b64--alphabet (logand word #x3F))
                   nelisp-b64--padding)))
        (push (string c0 c1 c2 c3) chunks)))
    (apply #'concat (nreverse chunks))))

(defun nelisp-b64-encode-string (string &optional no-line-break)
  "Encode unibyte STRING to base64.
Default output inserts MIME line breaks every 76 characters.  With
NO-LINE-BREAK non-nil, output is emitted as one line."
  (when (multibyte-string-p string)
    (error "Multibyte character in data for base64 encoding"))
  (let ((encoded (nelisp-b64-encode-bytes string)))
    (if no-line-break
        encoded
      (nelisp-b64--wrap-lines encoded))))

(defun nelisp-b64--decode-char (char)
  "Decode one base64 CHAR to its 6-bit value or nil."
  (let ((table (nelisp-b64--ensure-decode-table)))
    (aref table char)))

(defun nelisp-b64--base64-char-p (char)
  "Return non-nil if CHAR is in the RFC 4648 base64 alphabet."
  (not (null (nelisp-b64--decode-char char))))

(defun nelisp-b64--whitespace-char-p (char)
  "Return non-nil if CHAR is ignorable ASCII whitespace."
  (memq char '(9 10 13 32 12 11)))

(defun nelisp-b64--sanitize-input (base64-string)
  "Strip ASCII whitespace from BASE64-STRING and validate characters."
  (let ((chars '())
        (index 0)
        (length (length base64-string)))
    (while (< index length)
      (let ((char (aref base64-string index)))
        (cond
         ((nelisp-b64--whitespace-char-p char))
         ((or (= char nelisp-b64--padding)
              (nelisp-b64--base64-char-p char))
          (push char chars))
         (t
          (error "Invalid base64 character at %d: %c" index char))))
      (setq index (1+ index)))
    (apply #'string (nreverse chars))))

(defun nelisp-b64--decoded-length-adjustment (c2 c3)
  "Return how many bytes the quartet with C2/C3 contributes."
  (cond
   ((= c2 nelisp-b64--padding) 1)
   ((= c3 nelisp-b64--padding) 2)
   (t 3)))

(defun nelisp-b64-decode-to-bytes (base64-string)
  "Decode BASE64-STRING to a unibyte string of raw bytes.
ASCII whitespace is ignored.  Non-alphabet characters signal an error."
  (let* ((clean (nelisp-b64--sanitize-input base64-string))
         (length (length clean))
         (bytes '())
         (index 0))
    (unless (= (mod length 4) 0)
      (error "Invalid base64 length: %d" length))
    (while (< index length)
      (let* ((c0 (aref clean index))
             (c1 (aref clean (1+ index)))
             (c2 (aref clean (+ index 2)))
             (c3 (aref clean (+ index 3)))
             (v0 (nelisp-b64--decode-char c0))
             (v1 (nelisp-b64--decode-char c1))
             (v2 (if (= c2 nelisp-b64--padding) 0 (nelisp-b64--decode-char c2)))
             (v3 (if (= c3 nelisp-b64--padding) 0 (nelisp-b64--decode-char c3))))
        (when (or (null v0) (null v1)
                  (and (/= c2 nelisp-b64--padding) (null v2))
                  (and (/= c3 nelisp-b64--padding) (null v3)))
          (error "Invalid base64 quartet at %d" index))
        (when (and (= c2 nelisp-b64--padding)
                   (/= c3 nelisp-b64--padding))
          (error "Invalid base64 padding at %d" index))
        (when (and (< (+ index 4) length)
                   (or (= c2 nelisp-b64--padding)
                       (= c3 nelisp-b64--padding)))
          (error "Padding only allowed in final quartet"))
        (let* ((word (logior (ash v0 18)
                             (ash v1 12)
                             (ash v2 6)
                             v3))
               (count (nelisp-b64--decoded-length-adjustment c2 c3)))
          (push (logand (ash word -16) #xFF) bytes)
          (when (> count 1)
            (push (logand (ash word -8) #xFF) bytes))
          (when (> count 2)
            (push (logand word #xFF) bytes))))
      (setq index (+ index 4)))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-b64-decode-string (base64-string)
  "Decode BASE64-STRING and return the resulting unibyte string."
  (nelisp-b64-decode-to-bytes base64-string))

(provide 'nelisp-base64)

;;; nelisp-base64.el ends here
