;;; emacs-cc-coding-4.el --- coding.c charset-type converters -*- lexical-binding: t; -*-

;; Port of GNU Emacs 31.1 src/coding.c for coding systems of type
;; `charset' (iso-8859-N, windows-125x, koi8, ...): the `charset-valids'
;; dispatch vector Fdefine_coding_system_internal builds, decode_coding_charset
;; with its charset annotations (produce_charset: the `charset' text
;; property), encode_coding_charset, and the EOL handling of
;; decode_coding / encode_coding for these systems.  The registry is
;; emacs-cc-coding-1.el; the charsets are emacs-cc-charset-*.el.

;;; Code:

(defvar emacs-cc-charset--id-ascii)
(defvar emacs-cc-charset--slots)
(defvar emacs-cc-coding--hash)
(defvar emacs-cc-coding--attr-slots)
(declare-function emacs-cc-charset--from-id "emacs-cc-charset-1")
(declare-function emacs-cc-charset--decode "emacs-cc-charset-1")
(declare-function emacs-cc-charset--encode "emacs-cc-charset-1")
(declare-function emacs-cc-charset--name "emacs-cc-charset-1")
(declare-function emacs-cc-charset--char-charset "emacs-cc-charset-3")

;; Function forms of the struct accessor macros of emacs-cc-charset-1.el and
;; emacs-cc-coding-1.el, so this file compiles without loading them.
(defun emacs-cc-coding-4--attr (attrs slot)
  (aref attrs (cdr (assq slot emacs-cc-coding--attr-slots))))
(defun emacs-cc-coding-4--cs (cs slot)
  (aref cs (cdr (assq slot emacs-cc-charset--slots))))

(defvar emacs-cc-coding-4--valids (make-hash-table :test 'eq)
  "Coding attribute vector -> its 256-entry `charset-valids' vector.")

(defun emacs-cc-coding-4--attrs (coding)
  "Return the attribute vector of CODING when its type is `charset', or nil."
  (and (symbolp coding) coding
       (boundp 'emacs-cc-coding--hash)
       (let ((spec (gethash coding emacs-cc-coding--hash)))
         (and spec
              (let ((attrs (aref spec 0)))
                (and (eq (emacs-cc-coding-4--attr attrs 'type) 'charset) attrs))))))

(defun emacs-cc-coding-4--dim (id)
  (emacs-cc-coding-4--cs (emacs-cc-charset--from-id id) 'dimension))

(defun emacs-cc-coding-4--charset-valids (attrs)
  "coding.c Fdefine_coding_system_internal: the `charset-valids' vector.
Element N is nil (byte N cannot start a character), a charset ID, or a
list of IDs sorted by dimension (smaller first)."
  (or (gethash attrs emacs-cc-coding-4--valids)
      (let ((val (make-vector 256 nil)))
        (dolist (id (emacs-cc-coding-4--attr attrs 'charset-list))
          (let* ((cs (emacs-cc-charset--from-id id))
                 (dim (emacs-cc-coding-4--cs cs 'dimension))
                 (sp (emacs-cc-coding-4--cs cs 'code-space))
                 (idx (* (1- dim) 4))
                 (i (aref sp idx)))
            (while (<= i (aref sp (1+ idx)))
              (let ((tmp (aref val i)))
                (setq tmp
                      (cond
                       ((null tmp) id)
                       ((integerp tmp)
                        (if (< dim (emacs-cc-coding-4--dim tmp))
                            (list id tmp)
                          (list tmp id)))
                       (t
                        (let ((tail tmp))
                          (while (and tail (>= dim (emacs-cc-coding-4--dim (car tail))))
                            (setq tail (cdr tail)))
                          (if (null tail)
                              (nconc tmp (list id))
                            (setcdr tail (cons (car tail) (cdr tail)))
                            (setcar tail id)
                            tmp)))))
                (aset val i tmp))
              (setq i (1+ i)))))
        (puthash attrs val emacs-cc-coding-4--valids))))

(defun emacs-cc-coding-4--eol (coding bytes)
  "EOL type of CODING for decoding BYTES: `unix', `dos', `mac', or nil.
An undecided EOL (a vector) is detected as detect_eol and
adjust_coding_eol_type do: a bare LF means unix, else CR LF means dos,
else a CR means mac; nil when BYTES has no EOL at all."
  (let ((eol (coding-system-eol-type coding)))
    (cond
     ((eq eol 1) 'dos)
     ((eq eol 2) 'mac)
     ((integerp eol) 'unix)
     ((string-match-p "\\(?:\\`\\|[^\r]\\)\n" bytes) 'unix)
     ((string-match-p "\r\n" bytes) 'dos)
     ((string-match-p "\r" bytes) 'mac)
     (t nil))))

(defun emacs-cc-coding-4--decode-bytes (bytes attrs)
  "decode_coding_charset over the unibyte string BYTES.
Return (CHARS . RUNS): the decoded characters and the charset runs
\((FROM TO . CHARSET-NAME) ...) produce_charset turns into properties."
  (let* ((valids (emacs-cc-coding-4--charset-valids attrs))
         (n (length bytes)) (i 0) (chars nil) (runs nil)
         (offset 0) (last-id emacs-cc-charset--id-ascii) (last-offset 0))
    (while (< i n)
      (let* ((b (aref bytes i)) (val (aref valids b)) (c -1) (charset nil) (len 1))
        (dolist (id (if (integerp val) (list val) val))
          (when (< c 0)
            (let* ((cs (emacs-cc-charset--from-id id))
                   (dim (emacs-cc-coding-4--cs cs 'dimension)))
              (when (<= (+ i dim) n)
                (let ((code 0) (k 0))
                  (while (< k dim)
                    (setq code (logior (ash code 8) (aref bytes (+ i k))) k (1+ k)))
                  (setq c (emacs-cc-charset--decode cs code))
                  (when (>= c 0) (setq charset cs len dim)))))))
        (if (< c 0)
            ;; invalid_code: the byte itself, as a raw-byte character.
            (progn (push (if (< b 128) b (+ b #x3FFF00)) chars)
                   (setq i (1+ i)))
          (let ((id (emacs-cc-coding-4--cs charset 'id)))
            (when (and (/= id emacs-cc-charset--id-ascii) (/= last-id id))
              (unless (= last-id emacs-cc-charset--id-ascii)
                (push (cons last-offset (cons offset last-id)) runs))
              (setq last-id id last-offset offset)))
          (push c chars)
          (setq i (+ i len)))
        (setq offset (1+ offset))))
    (unless (= last-id emacs-cc-charset--id-ascii)
      (push (cons last-offset (cons offset last-id)) runs))
    (cons (nreverse chars) (nreverse runs))))

(defun emacs-cc-coding-4--ascii-p (bytes)
  "Return non-nil when the unibyte string BYTES has no byte above 127."
  (let ((i 0) (n (length bytes)) (ok t))
    (while (and ok (< i n))
      (when (> (aref bytes i) 127) (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun emacs-cc-coding-4--decode (string coding attrs)
  "Decode STRING with the charset-type CODING (attribute vector ATTRS).
Return (TEXT . EOL-USED)."
  (let* ((bytes (if (multibyte-string-p string)
                    (encode-coding-string string 'raw-text)
                  string))
         (eol (emacs-cc-coding-4--eol coding bytes)))
    ;; decode_coding_charset treats CR LF (dos) and CR (mac) as one LF; the
    ;; EOL bytes are ASCII in every charset coding system.
    (cond ((eq eol 'dos) (setq bytes (replace-regexp-in-string "\r\n" "\n" bytes t t)))
          ((eq eol 'mac) (setq bytes (replace-regexp-in-string "\r" "\n" bytes t t))))
    (let* ((res (if (and (emacs-cc-coding-4--attr attrs 'ascii-compat)
                         (emacs-cc-coding-4--ascii-p bytes))
                    ;; decode_coding_object: ASCII-only text of an
                    ;; ASCII-compatible system is produced as is, without
                    ;; the decoder and so without charset annotations.
                    (cons (string-to-list bytes) nil)
                  (emacs-cc-coding-4--decode-bytes bytes attrs)))
           (text (if (car res) (apply #'string (car res)) (make-string 0 0 t))))
      (dolist (run (cdr res))
        (put-text-property (car run) (cadr run) 'charset
                           (emacs-cc-charset--name
                            (emacs-cc-charset--from-id (cddr run)))
                           text))
      (cons text eol))))

(defun emacs-cc-coding-4--encode (string coding attrs)
  "encode_coding_charset: encode STRING with charset-type CODING."
  (let* ((eol (coding-system-eol-type coding))
         (text (cond ((eq eol 1) (replace-regexp-in-string "\n" "\r\n" string t t))
                     ((eq eol 2) (replace-regexp-in-string "\n" "\r" string t t))
                     (t string)))
         (ascii-compat (emacs-cc-coding-4--attr attrs 'ascii-compat))
         (clist (emacs-cc-coding-4--attr attrs 'charset-list))
         (default (emacs-cc-coding-4--attr attrs 'default-char))
         (out nil))
    (dolist (c (string-to-list text))
      (cond
       ((and ascii-compat (< c 128)) (push c out))
       ((>= c #x3FFF80) (push (- c #x3FFF00) out))
       (t
        (let ((cs (emacs-cc-charset--char-charset c clist)))
          (if (null cs)
              (push default out)
            (let ((code (emacs-cc-charset--encode cs c))
                  (dim (emacs-cc-coding-4--cs cs 'dimension)))
              (while (> dim 0)
                (setq dim (1- dim))
                (push (logand (ash code (- (* 8 dim))) 255) out))))))))
    (apply #'unibyte-string (nreverse out))))

(defun emacs-cc-coding-4--eol-name (coding eol)
  "CODING's subsidiary for EOL, as `last-coding-system-used' reports it."
  (let ((type (coding-system-eol-type coding)))
    (if (and eol (vectorp type))
        (aref type (cond ((eq eol 'dos) 1) ((eq eol 'mac) 2) (t 0)))
      coding)))

(when (and (fboundp 'nelisp--repr) (fboundp 'decode-coding-string))
  (unless (fboundp 'emacs-cc-coding-4--decode-next)
    (defalias 'emacs-cc-coding-4--decode-next
      (symbol-function 'decode-coding-string))
    (defalias 'emacs-cc-coding-4--encode-next
      (symbol-function 'encode-coding-string)))
  (defun emacs-cc-coding-4--decode-coding-string (string coding-system &rest rest)
    "`decode-coding-string' with the charset-type systems of coding.c."
    (let ((attrs (and (stringp string) (emacs-cc-coding-4--attrs coding-system))))
      (if (not attrs)
          (apply #'emacs-cc-coding-4--decode-next string coding-system rest)
        (let* ((buffer (nth 1 rest))
               (res (emacs-cc-coding-4--decode string coding-system attrs))
               (text (car res)))
          (setq last-coding-system-used
                (emacs-cc-coding-4--eol-name coding-system (cdr res)))
          (if (not buffer)
              text
            (with-current-buffer buffer
              (save-excursion (insert text)))
            (length text))))))
  (defun emacs-cc-coding-4--encode-coding-string (string coding-system &rest rest)
    "`encode-coding-string' with the charset-type systems of coding.c."
    (let ((attrs (and (stringp string) (emacs-cc-coding-4--attrs coding-system))))
      (if (not attrs)
          (apply #'emacs-cc-coding-4--encode-next string coding-system rest)
        (let ((buffer (nth 1 rest))
              (bytes (emacs-cc-coding-4--encode string coding-system attrs)))
          (setq last-coding-system-used coding-system)
          (if (not buffer)
              bytes
            (with-current-buffer buffer
              (save-excursion (insert bytes)))
            (length bytes))))))
  (defalias 'decode-coding-string #'emacs-cc-coding-4--decode-coding-string)
  (defalias 'encode-coding-string #'emacs-cc-coding-4--encode-coding-string))

(provide 'emacs-cc-coding-4)
;;; emacs-cc-coding-4.el ends here
