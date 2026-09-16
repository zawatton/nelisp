;;; nelisp-uuid.el --- UUID generation and parsing for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure Lisp UUID (RFC 4122) support for NeLisp, both host Emacs and the
;; standalone reader:
;;
;;   - `nelisp-uuid-v4'    -- random UUID
;;   - `nelisp-uuid-v5'    -- name-based UUID (SHA-1)
;;   - `nelisp-uuid-parse' -- validate and canonicalize a UUID string
;;   - `nelisp-uuid-p'     -- non-signaling predicate
;;
;; Design constraints:
;;   - pure Lisp only, no new external dependency
;;   - `nelisp-uuid-v5' reuses `nelisp-hash-sha1' from the existing
;;     `nelisp-secure-hash' package rather than adding a second SHA-1
;;     implementation to the tree
;;   - text form is always the canonical lowercase 8-4-4-4-12 layout
;;
;; Randomness quality: `nelisp-uuid-v4' draws from Emacs' `random', which
;; is a fast general-purpose PRNG (glibc `random(3)' on the host build),
;; NOT a cryptographically secure random source.  Do not use v4 UUIDs
;; from this package as secrets (session tokens, password-reset links,
;; API keys); use them only as identifiers where collision resistance,
;; not unpredictability, is what matters.

;;; Code:

(require 'nelisp-secure-hash)
(require 'nelisp-coding)

(define-error 'nelisp-uuid-error "NeLisp UUID error")
(define-error 'nelisp-uuid-parse-error "NeLisp UUID parse error" 'nelisp-uuid-error)

;; RFC 4122 Appendix C standard namespace UUIDs, provided for `nelisp-uuid-v5'
;; callers that need one of the predefined namespaces.
(defconst nelisp-uuid-namespace-dns "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
  "RFC 4122 namespace UUID for fully-qualified domain names.")
(defconst nelisp-uuid-namespace-url "6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  "RFC 4122 namespace UUID for URLs.")
(defconst nelisp-uuid-namespace-oid "6ba7b812-9dad-11d1-80b4-00c04fd430c8"
  "RFC 4122 namespace UUID for ISO OIDs.")
(defconst nelisp-uuid-namespace-x500 "6ba7b814-9dad-11d1-80b4-00c04fd430c8"
  "RFC 4122 namespace UUID for X.500 distinguished names.")

(defconst nelisp-uuid--nil "00000000-0000-0000-0000-000000000000"
  "The nil UUID (RFC 4122 Section 4.1.7).")

(defconst nelisp-uuid--canonical-regexp
  (concat "\\`"
          "[0-9a-fA-F]\\{8\\}-"
          "[0-9a-fA-F]\\{4\\}-"
          "[0-9a-fA-F]\\{4\\}-"
          "[0-9a-fA-F]\\{4\\}-"
          "[0-9a-fA-F]\\{12\\}"
          "\\'")
  "Canonical 8-4-4-4-12 UUID text form.")

(defun nelisp-uuid--hex-digit-value (char)
  "Return the numeric value of hexadecimal CHAR, or nil if invalid."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?a) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (>= char ?A) (<= char ?F)) (+ 10 (- char ?A)))
   (t nil)))

(defun nelisp-uuid--string-to-bytes (string)
  "Return the 16 raw bytes of canonical UUID text STRING.
Signal `nelisp-uuid-parse-error' when STRING is not a well-formed
canonical UUID."
  (unless (and (stringp string) (string-match-p nelisp-uuid--canonical-regexp string))
    (signal 'nelisp-uuid-parse-error (list "malformed UUID text" string)))
  (let ((hex (concat (substring string 0 8)
                      (substring string 9 13)
                      (substring string 14 18)
                      (substring string 19 23)
                      (substring string 24 36)))
        (bytes nil))
    (dotimes (i 16)
      (let* ((hi (nelisp-uuid--hex-digit-value (aref hex (* i 2))))
             (lo (nelisp-uuid--hex-digit-value (aref hex (1+ (* i 2))))))
        (push (logior (ash hi 4) lo) bytes)))
    (nreverse bytes)))

(defun nelisp-uuid--bytes-to-string (bytes)
  "Return canonical 8-4-4-4-12 text for the 16-byte list BYTES."
  (let ((hex (mapconcat (lambda (b) (format "%02x" b)) bytes "")))
    (concat (substring hex 0 8) "-"
            (substring hex 8 12) "-"
            (substring hex 12 16) "-"
            (substring hex 16 20) "-"
            (substring hex 20 32))))

(defun nelisp-uuid--set-version-and-variant (bytes version)
  "Return BYTES (a 16-byte list) with the RFC 4122 VERSION and variant set."
  (let ((out (copy-sequence bytes)))
    (setf (nth 6 out) (logior (logand (nth 6 out) #x0F) (ash version 4)))
    (setf (nth 8 out) (logior (logand (nth 8 out) #x3F) #x80))
    out))

;;;###autoload
(defun nelisp-uuid-v4 ()
  "Return a random (version 4) UUID as a canonical lowercase string.

Randomness comes from Emacs' `random', an ordinary PRNG -- see this
file's Commentary for why that is not appropriate for secrets."
  (let ((bytes (let (acc) (dotimes (_ 16 (nreverse acc)) (push (random 256) acc)))))
    (nelisp-uuid--bytes-to-string (nelisp-uuid--set-version-and-variant bytes 4))))

;;;###autoload
(defun nelisp-uuid-v5 (namespace name)
  "Return a name-based (version 5, SHA-1) UUID for NAME under NAMESPACE.

NAMESPACE is a canonical UUID string, e.g. `nelisp-uuid-namespace-url' or
another v3/v5 UUID; NAME is an arbitrary string.  Deterministic: the same
NAMESPACE and NAME always produce the same UUID, per RFC 4122 Section 4.3."
  (let* ((namespace-bytes (nelisp-uuid--string-to-bytes namespace))
         (name-bytes (nelisp-coding-utf8-encode name))
         (input (apply #'unibyte-string (append namespace-bytes name-bytes)))
         (digest (nelisp-hash-sha1 input))
         (digest-bytes nil))
    (dotimes (i 16)
      (push (logior (ash (nelisp-uuid--hex-digit-value (aref digest (* i 2))) 4)
                    (nelisp-uuid--hex-digit-value (aref digest (1+ (* i 2)))))
            digest-bytes))
    (nelisp-uuid--bytes-to-string
     (nelisp-uuid--set-version-and-variant (nreverse digest-bytes) 5))))

;;;###autoload
(defun nelisp-uuid-p (value)
  "Return non-nil if VALUE is a canonical UUID string.  Never signals."
  (and (stringp value) (string-match-p nelisp-uuid--canonical-regexp value) t))

;;;###autoload
(defun nelisp-uuid-parse (string)
  "Validate STRING as a canonical UUID and return it lower-cased.
Signal `nelisp-uuid-parse-error' (never a silent nil) when STRING is not
a well-formed 8-4-4-4-12 UUID."
  (unless (and (stringp string) (string-match-p nelisp-uuid--canonical-regexp string))
    (signal 'nelisp-uuid-parse-error (list "malformed UUID text" string)))
  (downcase string))

;;;###autoload
(defun nelisp-uuid-nil-p (value)
  "Return non-nil if VALUE is the nil UUID (all zero bits)."
  (and (nelisp-uuid-p value) (string-equal (downcase value) nelisp-uuid--nil)))

(provide 'nelisp-uuid)

;;; nelisp-uuid.el ends here
