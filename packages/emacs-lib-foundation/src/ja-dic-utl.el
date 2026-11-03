;;; ja-dic-utl.el --- lightweight Japanese dictionary helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Compact compatibility facade for GNU Emacs' ja-dic-utl.el.  It keeps
;; the public SKK dictionary variables and lookup helper available for
;; kkc.el-style callers.  Full SKK-JISYO data is intentionally not loaded
;; during startup.

;;; Code:

(defvar skkdic-okuri-ari nil
  "Nested alist/hash table for OKURI-ARI entries.")

(defvar skkdic-postfix nil
  "Nested alist/hash table for postfix entries.")

(defvar skkdic-prefix nil
  "Nested alist/hash table for prefix entries.")

(defvar skkdic-okuri-nasi nil
  "Nested alist/hash table for OKURI-NASI entries.")

(defconst skkdic-okurigana-table
  '((#x3041 . ?a) (#x3042 . ?a) (#x3043 . ?i) (#x3044 . ?i)
    (#x3045 . ?u) (#x3046 . ?u) (#x3047 . ?e) (#x3048 . ?e)
    (#x3049 . ?o) (#x304a . ?o) (#x304b . ?k) (#x304c . ?g)
    (#x304d . ?k) (#x304e . ?g) (#x304f . ?k) (#x3050 . ?g)
    (#x3051 . ?k) (#x3052 . ?g) (#x3053 . ?k) (#x3054 . ?g)
    (#x3055 . ?s) (#x3056 . ?z) (#x3057 . ?s) (#x3058 . ?j)
    (#x3059 . ?s) (#x305a . ?z) (#x305b . ?s) (#x305c . ?z)
    (#x305d . ?s) (#x305e . ?z) (#x305f . ?t) (#x3060 . ?d)
    (#x3061 . ?t) (#x3062 . ?d) (#x3063 . ?t) (#x3064 . ?t)
    (#x3065 . ?d) (#x3066 . ?t) (#x3067 . ?d) (#x3068 . ?t)
    (#x3069 . ?d) (#x306a . ?n) (#x306b . ?n) (#x306c . ?n)
    (#x306d . ?n) (#x306e . ?n) (#x306f . ?h) (#x3070 . ?b)
    (#x3071 . ?p) (#x3072 . ?h) (#x3073 . ?b) (#x3074 . ?p)
    (#x3075 . ?h) (#x3076 . ?b) (#x3077 . ?p) (#x3078 . ?h)
    (#x3079 . ?b) (#x307a . ?p) (#x307b . ?h) (#x307c . ?b)
    (#x307d . ?p) (#x307e . ?m) (#x307f . ?m) (#x3080 . ?m)
    (#x3081 . ?m) (#x3082 . ?m) (#x3083 . ?y) (#x3084 . ?y)
    (#x3085 . ?y) (#x3086 . ?y) (#x3087 . ?y) (#x3088 . ?y)
    (#x3089 . ?r) (#x308a . ?r) (#x308b . ?r) (#x308c . ?r)
    (#x308d . ?r) (#x308f . ?w) (#x3090 . ?w) (#x3091 . ?w)
    (#x3092 . ?w) (#x3093 . ?n))
  "Alist of okuriganas vs trailing ASCII letters in OKURI-ARI entries.")

(defconst skkdic-jisx0208-hiragana-block
  (cons #x3041 #x309e)
  "Unicode range of the Hiragana block used by the lightweight helper.")

(defun skkdic-merge-head-and-tail (heads tails postfix)
  "Return strings made by joining HEADS and TAILS.
When POSTFIX is nil, one-character tails are skipped, matching the
upstream helper's short-entry filter."
  (let ((min-len 2)
        out)
    (while heads
      (let ((head (car heads)))
      (when (or postfix
                (>= (length head) min-len))
          (let ((tail-list tails))
            (while tail-list
              (let ((tail (car tail-list)))
                (when (or postfix
                          (>= (length tail) min-len))
                  (setq out (cons (concat head tail) out))))
              (setq tail-list (cdr tail-list)))))
        (setq heads (cdr heads))))
    (nreverse out)))

(defun skkdic--seq-key (seq len)
  "Return a string key from SEQ's first LEN characters."
  (let ((out ""))
    (dotimes (i len out)
      (setq out (concat out (char-to-string (aref seq i)))))))

(defun skkdic--table-lookup (table key)
  "Look up KEY in lightweight SKK TABLE."
  (cond
   ((hash-table-p table)
    (gethash key table))
   ((and (consp table) (stringp key))
    (cdr (assoc key table)))
   (t nil)))

(defun skkdic--ensure-dictionary ()
  "Try to load bundled Japanese dictionary data when available."
  (unless skkdic-okuri-nasi
    (condition-case nil
        (load-library "ja-dic/ja-dic")
      (error nil))))

(defun skkdic-lookup-key (seq len &optional postfix prefer-noun)
  "Return conversion strings for SEQ of length LEN.
This lightweight version supports hash-table or string-key alist
dictionaries in `skkdic-okuri-nasi', `skkdic-prefix',
`skkdic-postfix', and `skkdic-okuri-ari'."
  (skkdic--ensure-dictionary)
  (let* ((key (skkdic--seq-key seq len))
         (entry (copy-sequence
                 (or (skkdic--table-lookup skkdic-okuri-nasi key)
                     nil))))
    (when postfix
      (let ((i 1))
        (while (< i len)
          (let* ((head-key (substring key 0 i))
                 (tail-key (substring key i))
                 (heads (skkdic--table-lookup skkdic-okuri-nasi head-key))
                 (tails (skkdic--table-lookup skkdic-postfix tail-key)))
            (when (and heads tails)
              (setq entry
                    (nconc entry
                           (skkdic-merge-head-and-tail heads tails t)))))
          (setq i (1+ i)))))
    (let ((i (1- len)))
      (while (> i 0)
        (let* ((head-key (substring key 0 i))
               (tail-key (substring key i))
               (heads (skkdic--table-lookup skkdic-prefix head-key))
               (tails (skkdic--table-lookup skkdic-okuri-nasi tail-key)))
          (when (and heads tails)
            (setq entry
                  (nconc entry
                         (skkdic-merge-head-and-tail heads tails nil)))))
        (setq i (1- i))))
    (let* ((last (aref seq (1- len)))
           (okuri (cdr (assq last skkdic-okurigana-table)))
           (okuri-key (and okuri
                           (concat (substring key 0 (1- len))
                                   (char-to-string okuri))))
           (okuri-entry (and okuri-key
                             (skkdic--table-lookup skkdic-okuri-ari
                                                   okuri-key))))
      (when okuri-entry
        (let (with-okuri)
          (dolist (candidate okuri-entry)
            (push (concat candidate (char-to-string last)) with-okuri))
          (setq with-okuri (nreverse with-okuri))
          (setq entry
                (if prefer-noun
                    (nconc entry with-okuri)
                  (nconc with-okuri entry))))))
    entry))

(provide 'ja-dic-utl)

;;; ja-dic-utl.el ends here
