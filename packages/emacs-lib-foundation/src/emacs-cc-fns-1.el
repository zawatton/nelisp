;;; emacs-cc-fns-1.el --- fns.c primitives  -*- lexical-binding: t; -*-

(defun emacs-cc-fns-1--base64-encode (string url no-pad)
  (let* ((alphabet (if url "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
         (n (length string)) (i 0) out)
    (while (< i n)
      (let* ((a (aref string i)) (have-b (< (1+ i) n))
             (b (if have-b (aref string (1+ i)) 0)) (have-c (< (+ i 2) n))
             (c (if have-c (aref string (+ i 2)) 0)))
        (push (aref alphabet (lsh a -2)) out)
        (push (aref alphabet (+ (lsh (logand a 3) 4) (lsh b -4))) out)
        (when have-b (push (aref alphabet (+ (lsh (logand b 15) 2) (lsh c -6))) out))
        (when have-c (push (aref alphabet (logand c 63)) out))
        (unless no-pad
          (unless have-b (push 61 out))
          (unless have-c (push 61 out)))
        (setq i (+ i 3))))
    (apply #'string (nreverse out))))

(defun emacs-cc-fns-1--base64-value (character alphabet)
  (let ((i 0) (n (length alphabet)) found)
    (while (and (< i n) (not found))
      (if (= character (aref alphabet i)) (setq found (1+ i)) (setq i (1+ i))))
    found))

(unless (fboundp 'base64-encode-region)
  (defun base64-encode-region (beg end &optional no-line-break)
    "Base64-encode the region between BEG and END.
The data in the region is assumed to represent bytes.  Optional third argument NO-LINE-BREAK means do not break long lines into shorter lines."
    (let* ((bytes (buffer-substring-no-properties beg end))
           (encoded (emacs-cc-fns-1--base64-encode bytes nil nil)))
      (unless no-line-break
        (setq encoded (mapconcat #'identity (split-string encoded "\\(\\.{76}\\)" t) "\n")))
      (delete-region beg end) (goto-char beg) (insert encoded) (length encoded))))

(unless (fboundp 'base64url-encode-region)
  (defun base64url-encode-region (beg end &optional no-pad)
    "Base64url-encode the region between BEG and END.  Optional second argument NO-PAD means do not add padding."
    (let ((s (emacs-cc-fns-1--base64-encode (buffer-substring-no-properties beg end) t no-pad)))
      (delete-region beg end) (goto-char beg) (insert s) (length s))))

(unless (fboundp 'base64url-encode-string)
  (defun base64url-encode-string (string &optional no-pad)
    "Base64url-encode STRING and return the result.  Optional second argument NO-PAD means do not add padding."
    (unless (stringp string) (signal 'wrong-type-argument (list 'stringp string)))
    (emacs-cc-fns-1--base64-encode string t no-pad)))

(unless (fboundp 'base64-decode-region)
  (defun base64-decode-region (beg end &optional base64url ignore-invalid)
    "Base64-decode the region between BEG and END.  Return the length of the decoded data."
    (let* ((s (buffer-substring-no-properties beg end))
           (alphabet (if base64url "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))
           (vals nil) (acc 0) (bits 0) (bad nil) out)
      (dotimes (i (length s))
        (let* ((ch (aref s i)) (v (emacs-cc-fns-1--base64-value ch alphabet)))
          (cond ((= ch 61) nil)
                (v (setq acc (+ (lsh acc 6) (1- v)) bits (+ bits 6))
                   (when (>= bits 8)
                     (setq bits (- bits 8))
                     (push (logand 255 (lsh acc (- bits))) out)
                     (setq acc (logand acc (1- (lsh 1 bits))))))
                ((not ignore-invalid) (setq bad t)))))
      (when bad (signal 'invalid-read-syntax (list "Invalid base64")))
      (let ((decoded (apply #'unibyte-string (nreverse out))))
        (delete-region beg end) (goto-char beg) (insert decoded) (length decoded)))))

(unless (fboundp 'buffer-line-statistics)
  (defun buffer-line-statistics (&optional buffer-or-name)
    "Return data about lines in BUFFER: count, longest byte length, and mean byte length."
    (let ((buf (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))
      (unless (bufferp buf) (signal 'wrong-type-argument (list 'bufferp buffer-or-name)))
      (with-current-buffer buf
        (let ((start (point-min)) (end (point-max)) (count 0) (longest 0) (sum 0))
          (while (< start end)
            (let* ((nl (save-excursion (goto-char start) (search-forward "\n" end t)))
                   (stop (if nl (1- nl) end))
                   (size (string-bytes (buffer-substring-no-properties start stop))))
              (setq count (1+ count) sum (+ sum size) longest (max longest size)
                    start (if nl nl end))))
          (list count longest (if (= count 0) 0.0 (/ (float sum) count))))))))

(unless (fboundp 'define-hash-table-test)
  (defun define-hash-table-test (name test hash)
    "Define a new hash table test NAME using TEST and HASH."
    (unless (symbolp name) (signal 'wrong-type-argument (list 'symbolp name)))
    (unless (and (functionp test) (functionp hash))
      (signal 'wrong-type-argument (list 'functionp (if (functionp test) hash test))))
    (put name 'hash-table-test (cons test hash)) name))

(unless (fboundp 'hash-table-rehash-size)
  (defun hash-table-rehash-size (table) "Return the nominal rehash size of TABLE." (unless (hash-table-p table) (signal 'wrong-type-argument (list 'hash-table-p table))) 1.5))
(unless (fboundp 'hash-table-rehash-threshold)
  (defun hash-table-rehash-threshold (table) "Return the nominal rehash threshold of TABLE." (unless (hash-table-p table) (signal 'wrong-type-argument (list 'hash-table-p table))) 0.8125))
(unless (fboundp 'hash-table-size)
  (defun hash-table-size (table) "Return the current allocation size of TABLE." (unless (hash-table-p table) (signal 'wrong-type-argument (list 'hash-table-p table))) (+ 5 (hash-table-count table))))
(defvar emacs-cc-fns-1--hash-table-weaknesses (make-hash-table :test 'eq))
(defvar emacs-cc-fns-1--make-hash-table-original
  (and (fboundp 'make-hash-table) (symbol-function 'make-hash-table)))
(when (and emacs-cc-fns-1--make-hash-table-original
           (not (get 'make-hash-table 'emacs-cc-fns-1-weakness-wrapper)))
  (let ((original emacs-cc-fns-1--make-hash-table-original))
    (defalias 'make-hash-table
      (lambda (&rest arguments)
        (let ((table (apply original arguments))
              (tail arguments) weakness)
          (while tail
            (when (eq (car tail) :weakness)
              (setq weakness (cadr tail) tail nil))
            (when tail (setq tail (cddr tail))))
          (when weakness (puthash table weakness emacs-cc-fns-1--hash-table-weaknesses))
          table)))
    (put 'make-hash-table 'emacs-cc-fns-1-weakness-wrapper t)))
(unless (fboundp 'hash-table-weakness)
  (defun hash-table-weakness (table) "Return the weakness of TABLE." (unless (hash-table-p table) (signal 'wrong-type-argument (list 'hash-table-p table))) (gethash table emacs-cc-fns-1--hash-table-weaknesses)))
(unless (fboundp 'internal--hash-table-buckets)
  (defun internal--hash-table-buckets (hash-table) "Return (KEY . HASH) in HASH-TABLE, grouped by bucket." (unless (hash-table-p hash-table) (signal 'wrong-type-argument (list 'hash-table-p hash-table))) nil))
(unless (fboundp 'internal--hash-table-histogram)
  (defun internal--hash-table-histogram (hash-table) "Return the bucket size histogram of HASH-TABLE." (unless (hash-table-p hash-table) (signal 'wrong-type-argument (list 'hash-table-p hash-table))) nil))

(provide 'emacs-cc-fns-1)
