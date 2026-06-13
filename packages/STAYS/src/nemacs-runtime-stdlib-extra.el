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

(provide 'nemacs-runtime-stdlib-extra)

;;; nemacs-runtime-stdlib-extra.el ends here
