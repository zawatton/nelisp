;;; nemacs-runtime-stdlib-extra.el --- bridge-runtime stdlib extras  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Small stdlib functions baked into the bridge runtime image after the
;; pure-elisp regexp matcher (`nelisp-stdlib-regexp.el', the `nlre-*'
;; family).  Two groups:
;;
;;   1. The `string-match' / `replace-regexp-in-string' family aliases over
;;      `nlre-*' -- the same aliases the standalone REPL prelude installs
;;      (`nelisp-standalone--reader-repl-prelude-source'), which the
;;      bridge's source-v1 image did not get.  Runtime-loaded packages
;;      (e.g. google-ime-server.el cleaning a transliterate JSON response)
;;      need these.
;;
;;   2. `url-hexify-string' -- url-util.el aborts the source-v1 replay, so a
;;      self-contained percent-encoder is provided here instead.  Works on
;;      raw byte arrays (the standalone string model), so multibyte UTF-8
;;      (e.g. CJK yomi) is encoded byte-by-byte, matching real Emacs.
;;
;; Each is gated on `(not (fboundp ...))' so host Emacs / a fuller runtime
;; that already provides them is a no-op.

;;; Code:

(unless (fboundp 'string-match)
  (when (fboundp 'nlre-string-match)
    (defun string-match (re s &optional start) (nlre-string-match re s start))
    (defun string-match-p (re s &optional start) (nlre-string-match re s start))
    (defun match-beginning (n) (nlre-match-beginning n))
    (defun match-end (n) (nlre-match-end n))
    (defun match-string (n &optional str)
      (let ((b (nlre-match-beginning n)) (e (nlre-match-end n)))
        (if (and str b e) (substring str b e) nil)))
    (defun split-string (s &optional sep omit trim) (nlre-split-string s sep omit))
    (defun replace-regexp-in-string (re rep s &optional fc lit subexp start)
      (nlre-replace-regexp-in-string re rep s))))

;; `substring-no-properties' = `substring' here (strings carry no text
;; properties in the standalone reader); many packages (e.g. ddskk's cdb.el)
;; use it.  `%' (integer modulo) is not a reader builtin -- only `mod' is --
;; and calling the undefined `%' segfaults, so alias it to `mod' (they agree
;; for the non-negative indices cdb/hashing use).
(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (s &optional from to) (substring s from to)))
(unless (fboundp '%)
  (defun % (a b) (mod a b)))

(unless (fboundp 'url-hexify-string)
  (defun url-hexify-string (string)
    "Percent-encode STRING (RFC 3986 unreserved set kept).
Operates on raw bytes, so multibyte UTF-8 is encoded byte-by-byte
(e.g. a CJK yomi for a google-ime transliterate query)."
    (let ((result "")
          (i 0)
          (n (length string)))
      (while (< i n)
        (let ((c (aref string i)))
          (setq result
                (concat result
                        (if (or (and (>= c ?A) (<= c ?Z))
                                (and (>= c ?a) (<= c ?z))
                                (and (>= c ?0) (<= c ?9))
                                (memq c '(?- ?_ ?. ?~)))
                            (char-to-string c)
                          ;; `format' has no width support in the reader
                          ;; (%02X yields the literal "%02X"), so pad by hand.
                          (let ((h (format "%X" c)))
                            (concat "%" (if (= (length h) 1) (concat "0" h) h)))))))
        (setq i (1+ i)))
      result)))

;;; --- init-parity stdlib: functions a typical Emacs config requires --------
;;
;; The bridge runtime already carries most of cl-lib / seq / subr-x, but a few
;; ubiquitous helpers are absent (probed: when-let/if-let, mapcan, assoc-default,
;; cl-reduce, cl-destructuring-bind).  These are pure and self-contained, so
;; bake them here -- gated on `(not (fboundp ...))' so a host Emacs / fuller
;; runtime that already provides them is a no-op.  This lets a user config that
;; `(require 'subr-x)' / `(require 'cl-lib)' and uses these load on the
;; standalone GUI runtime.

(unless (fboundp 'mapcan)
  (defun mapcan (func sequence)
    "Apply FUNC to each element of SEQUENCE; nconc the results."
    (apply #'nconc (mapcar func sequence))))

(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find the first ALIST element whose car matches KEY; return its cdr.
TEST defaults to `equal'.  A non-cons element matches KEY directly and
yields DEFAULT.  Returns nil on no match."
    (let ((tst (or test #'equal)) (result nil) (found nil) (l alist))
      (while (and l (not found))
        (let ((elt (car l)))
          (if (consp elt)
              (when (funcall tst key (car elt))
                (setq result (cdr elt) found t))
            (when (funcall tst key elt)
              (setq result default found t))))
        (setq l (cdr l)))
      result)))

(unless (fboundp 'cl-reduce)
  (defun cl-reduce (function sequence &rest keys)
    "Reduce SEQUENCE (a list) with FUNCTION.  Supports :initial-value and :key.
A pragmatic subset of cl-reduce (left fold) sufficient for typical config
code; :from-end is not implemented.  The keyword scan is done by hand so this
depends on no `plist-*' (absent from the bare prelude)."
    (let ((key nil) (init nil) (has-init nil) (k keys))
      (while k
        (cond ((eq (car k) :key)
               (when (car (cdr k)) (setq key (car (cdr k)))))
              ((eq (car k) :initial-value)
               (setq init (car (cdr k)) has-init t)))
        (setq k (cdr (cdr k))))
      ;; Apply :key only when given -- avoids depending on `identity', which
      ;; the bare prelude does not provide.
      (let ((lst (if key (mapcar key sequence) sequence)) acc rest)
        (if has-init
            (setq acc init rest lst)
          (if lst
              (setq acc (car lst) rest (cdr lst))
            (setq acc (funcall function))))
        (while rest
          (setq acc (funcall function acc (car rest)))
          (setq rest (cdr rest)))
        acc))))

;; `when-let' / `if-let' (and the canonical `*' forms): nested let/if expansion
;; so each binding's symbol is bound and truth-tested in turn.  No gensym
;; needed -- the user's own symbols are the bindings, exactly as in subr-x.
(unless (fboundp 'if-let*)
  (defun nemacs--if-let-normalize (bindings)
    "Normalize BINDINGS to a list of (SYM VALUE) specs.
Accepts the single-binding `(SYM VALUE)' form and the list-of-bindings form."
    (cond
     ((null bindings) nil)
     ;; single (sym val): car is a non-cons symbol
     ((and (consp bindings) (symbolp (car bindings)) (not (null (car bindings))))
      (list (list (car bindings) (car (cdr bindings)))))
     (t (mapcar (lambda (b)
                  (if (consp b) (list (car b) (car (cdr b))) (list b b)))
                bindings))))

  (defun nemacs--if-let-expand (specs then else)
    "Expand SPECS (list of (SYM VALUE)) into nested let/if around THEN/ELSE."
    (if (null specs)
        then
      (let ((s (car specs)))
        (list 'let (list (list (car s) (car (cdr s))))
              (list 'if (car s)
                    (nemacs--if-let-expand (cdr specs) then else)
                    else)))))

  (defmacro if-let* (bindings then &rest else)
    "Bind BINDINGS in turn; eval THEN if all are non-nil, else ELSE."
    (nemacs--if-let-expand (nemacs--if-let-normalize bindings)
                           then (if else (cons 'progn else) nil)))

  (defmacro when-let* (bindings &rest body)
    "Bind BINDINGS in turn; eval BODY only if all are non-nil."
    (nemacs--if-let-expand (nemacs--if-let-normalize bindings)
                           (cons 'progn body) nil))

  (defmacro if-let (bindings then &rest else)
    "Compatibility alias for `if-let*'."
    (nemacs--if-let-expand (nemacs--if-let-normalize bindings)
                           then (if else (cons 'progn else) nil)))

  (defmacro when-let (bindings &rest body)
    "Compatibility alias for `when-let*'."
    (nemacs--if-let-expand (nemacs--if-let-normalize bindings)
                           (cons 'progn body) nil)))

(provide 'nemacs-runtime-stdlib-extra)

;;; nemacs-runtime-stdlib-extra.el ends here
