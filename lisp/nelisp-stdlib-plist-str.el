;;; nelisp-stdlib-plist-str.el --- Sweep 9 G4 plist + simple string  -*- lexical-binding: t; -*-

(defun plist-member (plist key)
  (let ((cur plist) (found nil))
    (while (and cur (not found))
      (if (eq (car cur) key)
          (setq found cur)
        (setq cur (cdr (cdr cur)))))
    found))

(defun plist-get (plist key)
  (let ((tail (plist-member plist key)))
    (if tail (car (cdr tail)) nil)))

(defun plist-put (plist key value)
  (let ((tail (plist-member plist key)))
    (if tail
        (progn (setcar (cdr tail) value) plist)
      ;; absent — append "key value" by walking to the end
      (if (null plist)
          (cons key (cons value nil))
        (let ((cur plist))
          (while (cdr (cdr cur))
            (setq cur (cdr (cdr cur))))
          (setcdr (cdr cur) (cons key (cons value nil)))
          plist)))))

(defun string-empty-p (s)
  (= (length s) 0))

;; Rust-min (2026-05-06): compare-strings as elisp.  Emacs primitive
;; signature: (compare-strings STR1 START1 END1 STR2 START2 END2
;; &optional IGNORE-CASE).  Returns:
;;   t                       — STR1[start1..end1] equals STR2[start2..end2]
;;   positive integer (1+pos) — STR1 > STR2 at that 1-based offset
;;   negative integer        — STR1 < STR2 at that offset
;; nil starts default to 0; nil ends default to (length STR).
;; IGNORE-CASE non-nil downcases each char before comparing.
(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  (let* ((s1 str1) (s2 str2)
         (a (or start1 0))
         (b (or end1 (length s1)))
         (c (or start2 0))
         (d (or end2 (length s2)))
         (len1 (- b a))
         (len2 (- d c))
         (n (if (< len1 len2) len1 len2))
         (i 0)
         (result t))
    (while (and (< i n) (eq result t))
      (let* ((ch1 (aref s1 (+ a i)))
             (ch2 (aref s2 (+ c i)))
             (k1 (if ignore-case (downcase ch1) ch1))
             (k2 (if ignore-case (downcase ch2) ch2)))
        (cond
         ((< k1 k2) (setq result (- (1+ i))))
         ((> k1 k2) (setq result (1+ i)))
         (t (setq i (1+ i))))))
    (cond
     ((not (eq result t)) result)
     ((= len1 len2) t)
     ((< len1 len2) (- (1+ n)))
     (t (1+ n)))))

;; Rust-min (2026-05-06): regexp-quote — pure char-by-char escape of
;; the GNU Emacs regex meta-charset.  Migrated from build-tool/src/
;; eval/builtins.rs `bi_regexp_quote'.
(defun regexp-quote (s)
  (let ((out nil)
        (i 0)
        (n (length s)))
    (while (< i n)
      (let ((ch (aref s i)))
        (when (or (eq ch ?.) (eq ch ?*) (eq ch ?+) (eq ch ??)
                  (eq ch ?\[) (eq ch ?\]) (eq ch ?^) (eq ch ?$)
                  (eq ch ?\\) (eq ch ?\() (eq ch ?\))
                  (eq ch ?\{) (eq ch ?\}) (eq ch ?|))
          (setq out (cons ?\\ out)))
        (setq out (cons ch out)))
      (setq i (1+ i)))
    (concat (nreverse out))))

;; Rust-min (2026-05-06): file-name-* — pure path string slicing.
;; Migrated from build-tool/src/eval/builtins.rs `bi_file_name_*'.

(defun file-name-directory (path)
  "Return the directory part of PATH, or nil if PATH has no slash.
Result keeps the trailing slash."
  (let ((idx -1)
        (i 0)
        (n (length path)))
    (while (< i n)
      (when (eq (aref path i) ?/)
        (setq idx i))
      (setq i (1+ i)))
    (if (< idx 0)
        nil
      (substring path 0 (1+ idx)))))

(defun file-name-nondirectory (path)
  "Return the non-directory part of PATH (= last `/'-delimited component)."
  (let ((idx -1)
        (i 0)
        (n (length path)))
    (while (< i n)
      (when (eq (aref path i) ?/)
        (setq idx i))
      (setq i (1+ i)))
    (if (< idx 0)
        path
      (substring path (1+ idx)))))

(defun file-name-as-directory (path)
  "Return PATH with a trailing `/' appended if not already present."
  (cond
   ((zerop (length path)) "/")
   ((eq (aref path (1- (length path))) ?/) path)
   (t (concat path "/"))))

(defun directory-file-name (path)
  "Return PATH with a single trailing `/' stripped (= keeps `/' for root)."
  (let ((n (length path)))
    (cond
     ((<= n 1) path)
     ((eq (aref path (1- n)) ?/) (substring path 0 (1- n)))
     (t path))))

;; Rust-min (2026-05-06): string-trim family + string-prefix-p /
;; string-suffix-p — pure string slicing.  Migrated from
;; build-tool/src/eval/builtins.rs.

(defun nelisp-stdlib--whitespace-p (ch)
  "Return non-nil when CH (= integer codepoint) is ASCII whitespace.
Matches the Emacs default whitespace class for `string-trim'."
  (or (eq ch ?\s) (eq ch ?\t) (eq ch ?\n) (eq ch ?\r)
      (eq ch ?\f) (eq ch 11)))                ; 11 = ?\v

(defun string-trim-left (s &optional _regexp)
  "Strip leading whitespace from S.  REGEXP arg accepted for API
parity but ignored — use the polyfill in `replace-regexp-in-string'
when a custom pattern is needed."
  (let ((i 0)
        (n (length s)))
    (while (and (< i n) (nelisp-stdlib--whitespace-p (aref s i)))
      (setq i (1+ i)))
    (if (zerop i) s (substring s i))))

(defun string-trim-right (s &optional _regexp)
  "Strip trailing whitespace from S."
  (let ((n (length s))
        (i (length s)))
    (while (and (> i 0) (nelisp-stdlib--whitespace-p (aref s (1- i))))
      (setq i (1- i)))
    (if (= i n) s (substring s 0 i))))

(defun string-trim (s &optional _trim-left _trim-right)
  "Strip leading and trailing whitespace from S."
  (string-trim-left (string-trim-right s)))

(defun string-prefix-p (prefix s &optional ignore-case)
  "Return non-nil when S starts with PREFIX.
IGNORE-CASE non-nil → case-insensitive comparison."
  (let ((plen (length prefix))
        (slen (length s)))
    (if (> plen slen)
        nil
      (eq t (compare-strings prefix 0 plen s 0 plen ignore-case)))))

(defun string-suffix-p (suffix s &optional ignore-case)
  "Return non-nil when S ends with SUFFIX.
IGNORE-CASE non-nil → case-insensitive comparison."
  (let* ((suflen (length suffix))
         (slen (length s))
         (start (- slen suflen)))
    (if (< start 0)
        nil
      (eq t (compare-strings suffix 0 suflen s start slen ignore-case)))))

;; nelisp-stdlib-plist-str.el ends here
