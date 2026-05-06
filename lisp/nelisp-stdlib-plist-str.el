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

;; nelisp-stdlib-plist-str.el ends here
