;;; standalone-unibyte-aset-check.el --- GNU 31.1 byte mutation parity -*- lexical-binding: t; -*-

;; Golden output: standalone-unibyte-aset-check.expected, generated with
;; GNU Emacs 31.1: emacs -Q --batch -l test/standalone-unibyte-aset-check.el
;; The standalone runner compares the complete output and rejects stderr.

(defun unibyte-aset-observe (name thunk)
  (prin1 (list name (condition-case err (funcall thunk) (error err))))
  (terpri))

(defun unibyte-aset-snapshot (s)
  (list (length s) (string-bytes s) (multibyte-string-p s)
        (append s nil)))

(defun unibyte-aset-producer (name s)
  (unibyte-aset-observe
   name
   (lambda ()
     (let ((before (unibyte-aset-snapshot s)) (i 0) (sum 0))
       ;; Every byte, including C0/C1 and UTF-8 continuation bytes, must
       ;; round-trip as one element without changing the representation.
       (while (< i 256)
         (unless (= (aset s 0 i) i) (error "aset return value"))
         (unless (and (= (aref s 0) i)
                      (= (length s) (car before))
                      (= (string-bytes s) (nth 1 before))
                      (not (multibyte-string-p s)))
           (error "raw byte or string representation changed"))
         (setq sum (+ sum (aref s 0)) i (1+ i)))
       (aset s 0 233)
       (let ((promoted (string-to-multibyte s))
             (mixed (concat s (string-to-multibyte "Z"))))
         (list before sum (unibyte-aset-snapshot s)
               (unibyte-aset-snapshot promoted)
               (unibyte-aset-snapshot mixed)
               ;; Character codes keep this observation ASCII-only while
               ;; checking the actual prin1 output, including octal bytes.
               (append (prin1-to-string s) nil)))))))

(unibyte-aset-producer 'as-unibyte (string-as-unibyte (make-string 1 0)))
(unibyte-aset-producer 'to-unibyte (string-to-unibyte (make-string 1 0)))
(unibyte-aset-producer 'as-unibyte-text-tag (string-as-unibyte (format "%d" 0)))
(unibyte-aset-producer 'to-unibyte-text-tag (string-to-unibyte (format "%d" 0)))
(unibyte-aset-producer 'encode-ascii (encode-coding-string (make-string 1 0) 'utf-8))
(unibyte-aset-producer 'encode-utf8 (encode-coding-string "é" 'utf-8))
(unibyte-aset-producer 'unibyte-string (unibyte-string 0))
(unibyte-aset-producer 'make-string (make-string 1 0))
(unibyte-aset-producer 'substring (substring (unibyte-string 0 255) 0 1))
(unibyte-aset-producer 'concat (concat (unibyte-string 0) (unibyte-string 255)))
(unibyte-aset-producer 'copy-sequence (copy-sequence (unibyte-string 0)))
(unibyte-aset-producer 'reader-octal (copy-sequence "\351"))
(unibyte-aset-producer 'reader-octal-direct "\351")
(unibyte-aset-producer 'reader-ascii "a")
(unibyte-aset-producer 'char-to-string (char-to-string 0))
(unibyte-aset-producer 'integer-to-string (number-to-string 0))
(unibyte-aset-producer 'float-to-string (number-to-string 1.25))
(unibyte-aset-producer 'bignum-to-string (number-to-string 18446744073709551616))
(unibyte-aset-producer 'secure-hash (secure-hash 'sha256 "x"))

(defun unibyte-aset-symbol-producer (name symbol)
  ;; Restore canonical names before printing: altering nil/t's spelling can
  ;; otherwise alter the syntax emitted for booleans elsewhere in this check.
  (unibyte-aset-observe
   name
   (lambda ()
     (let* ((s (symbol-name symbol)) (old (aref s 0))
            (before (unibyte-aset-snapshot s)) (i 0) (sum 0))
       (unwind-protect
           (progn
             (while (< i 256)
               (unless (and (= (aset s 0 i) i) (= (aref s 0) i)
                            (= (length s) (car before))
                            (= (string-bytes s) (nth 1 before))
                            (not (multibyte-string-p s)))
                 (error "symbol name byte representation changed"))
               (setq sum (+ sum (aref s 0)) i (1+ i)))
             (aset s 0 233)
             (list before sum (unibyte-aset-snapshot s)
                   (eq s (symbol-name symbol)) (aref (symbol-name symbol) 0)))
         (aset s 0 old))))))

(unibyte-aset-symbol-producer
 'read-unibyte-symbol-name
 (read (string-as-unibyte "unibyte-aset-read-raw-name-unique")))
(unibyte-aset-symbol-producer
 'read-unibyte-uninterned-symbol-name
 (read (string-as-unibyte "#:unibyte-aset-read-raw-uninterned-name")))
(unibyte-aset-symbol-producer 'nil-symbol-name nil)
(unibyte-aset-symbol-producer 't-symbol-name t)

(unibyte-aset-observe
 'multibyte-symbol-names
 (lambda ()
   (list (unibyte-aset-snapshot (symbol-name 'é))
         (unibyte-aset-snapshot
          (symbol-name (make-symbol (string-to-multibyte "forced-name")))))))

(unibyte-aset-observe
 'read-multibyte-symbol-names
 (lambda ()
   (mapcar
    (lambda (source)
      (let* ((symbol (read (string-to-multibyte source)))
             (name (symbol-name symbol)))
        (list (unibyte-aset-snapshot name)
              (condition-case err (aset name 0 233) (error err))
              (eq name (symbol-name symbol)))))
    '("unibyte-aset-read-multi-name-unique"
      "#:unibyte-aset-read-multi-uninterned-name"))))

(unibyte-aset-observe
 'aliases
 (lambda ()
   (let* ((s "a") (alias s) (container (list s)))
     (aset s 0 233)
     (list (eq s alias) (eq s (car container))
           (unibyte-aset-snapshot alias)
           (unibyte-aset-snapshot (car container))))))

(unibyte-aset-observe
 'prin1
 (lambda ()
   (let ((s (string-as-unibyte (make-string 1 0))))
     (aset s 0 233)
     s)))

(let ((print-escape-nonascii t))
  (unibyte-aset-observe 'prin1-escaped (lambda () (unibyte-string 233))))

(unibyte-aset-observe
 'prin1-multibyte-byte8
 (lambda () (string-to-multibyte (unibyte-string 233))))

(let ((print-escape-multibyte t))
  (unibyte-aset-observe
   'prin1-multibyte-byte8-escaped
   (lambda () (string-to-multibyte (unibyte-string 233)))))

(unibyte-aset-observe
 'make-string-byte8
 (lambda () (unibyte-aset-snapshot (make-string 1 4194281))))

(dolist (spec '(("ab" 0 90) ("ab" 0 233) ("é" 0 97)
                ("é" 0 234) ("é" 0 12354) ("ab" 0 -1)))
  (unibyte-aset-observe
   (list 'multibyte spec)
   (lambda ()
     (let* ((s (string-to-multibyte (car spec)))
            (before (unibyte-aset-snapshot s))
            (result (condition-case err (aset s (nth 1 spec) (nth 2 spec))
                      (error err))))
       (list before result (unibyte-aset-snapshot s))))))

(dolist (value '(-1 256 4194281 bad))
  (unibyte-aset-observe
   (list 'invalid-byte value)
   (lambda () (aset (unibyte-string 0) 0 value))))

(unibyte-aset-observe
 'forced-make-string
 (lambda ()
   (let ((s (make-string 1 0 t)))
     (list (unibyte-aset-snapshot s)
           (condition-case err (aset s 0 233) (error err))))))

;; anvil-worklog-search -> sqlite-select -> emacs-sqlite-ffi--read:
;; a zero-filled string is converted to unibyte and filled with ptr-read-u8
;; values.  Reproduce the byte loop with a deterministic SQLite column;
;; avoid opening or writing the user's real worklog database.
(unibyte-aset-observe
 'anvil-sqlite-column
 (lambda ()
   (let* ((column (encode-coding-string "é日本" 'utf-8))
          (size (length column))
          (bytes (string-as-unibyte (make-string size 0)))
          (index 0))
     (while (< index size)
       (aset bytes index (aref column index))
       (setq index (1+ index)))
     (list (equal bytes column) (unibyte-aset-snapshot bytes)
           (unibyte-aset-snapshot (concat bytes "!"))))))

(kill-emacs 0)

;;; standalone-unibyte-aset-check.el ends here
