;;; standalone-json-compat-smoke.el --- standalone-compat/json.el smoke  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-segC-prelude, item 2: neither the Makefile nor
;; test/ has ever had a "standalone-compat" hit (`grep -rn
;; "standalone-compat" Makefile test/' is empty on this tree) -- the
;; wrappers in `standalone-compat/json.el' had no test of their own at
;; all, host or standalone.  This is the standalone smoke that fills
;; that gap, in the same `--load SCRIPT' + "CASES=N mismatches=M" shape
;; as the sibling smokes in this directory
;; (`standalone-number-token-smoke.el', `standalone-bignum-smoke.el')
;; that `make standalone-reader-number-token-smoke' etc. already wire
;; up as Makefile targets.  This file is NOT wired into the Makefile:
;; segment C1's file scope forbids editing it.  Run directly:
;;
;;   target/nelisp --load scripts/standalone-json-compat-smoke.el
;;
;; Covers the item 2 fix: `json-parse-string'/`json-parse-buffer' with
;; `:object-type \='alist' now intern their keys to symbols (previously
;; string keys, a documented divergence from real Emacs -- see
;; `standalone-compat/json.el''s Commentary); `plist'/`hash-table'
;; object types are unaffected; `json-read'/`json-read-from-string' now
;; honor the live `json-object-type'/`json-key-type' bindings instead
;; of a hardcoded alist assumption.  Every expected value below was
;; verified against host Emacs 31.1 with the identical forms.

;;; Code:

(require 'json)

(defvar nelisp--json-compat-smoke-cases 0)
(defvar nelisp--json-compat-smoke-mismatches 0)

(defun nelisp--json-compat-smoke-check (label actual expected)
  (setq nelisp--json-compat-smoke-cases (1+ nelisp--json-compat-smoke-cases))
  (unless (equal actual expected)
    (setq nelisp--json-compat-smoke-mismatches
          (1+ nelisp--json-compat-smoke-mismatches))
    (princ (format "[json-compat-smoke] MISMATCH %s: got %S want %S\n"
                   label actual expected))))

;; `json-parse-string' / `json-parse-buffer', new API -------------------

(nelisp--json-compat-smoke-check
 "parse-string alist symbol keys"
 (json-parse-string "{\"a\":1,\"b\":2}" :object-type 'alist)
 '((a . 1) (b . 2)))

(nelisp--json-compat-smoke-check
 "parse-string alist nested object+array"
 (json-parse-string "{\"a\":{\"b\":2},\"c\":[{\"d\":3}]}" :object-type 'alist)
 '((a (b . 2)) (c . [((d . 3))])))

(nelisp--json-compat-smoke-check
 "parse-string plist unaffected (keyword keys)"
 (json-parse-string "{\"a\":1}" :object-type 'plist)
 '(:a 1))

(nelisp--json-compat-smoke-check
 "parse-string hash-table unaffected (string keys)"
 (gethash "a" (json-parse-string "{\"a\":1}" :object-type 'hash-table))
 1)

;; Regression coverage: with NO `:object-type' argument at all,
;; `json-parse-string' must default to `hash-table', matching real
;; Emacs's own default (verified against Emacs 31.1: `(hash-table-p
;; (json-parse-string "{\"a\":1}"))' is t there too).  `type-of' is NOT
;; used here as the probe: the standalone's fallback `type-of'
;; (`scripts/nelisp-stdlib-prelude.el', "A13") pre-dates hash tables and
;; reports `cons' for every hash table on this substrate, including one
;; made directly with `(make-hash-table)' -- `hash-table-p'/`gethash'
;; are the correct structural/behavioral probes and both agree with
;; Emacs.
(nelisp--json-compat-smoke-check
 "parse-string default object-type is hash-table (no keyword arg)"
 (hash-table-p (json-parse-string "{\"a\":1}"))
 t)

(nelisp--json-compat-smoke-check
 "parse-string default object-type hash-table value"
 (gethash "a" (json-parse-string "{\"a\":1}"))
 1)

(with-temp-buffer
  (insert "{\"a\":1,\"b\":{\"c\":2}}")
  (goto-char (point-min))
  (nelisp--json-compat-smoke-check
   "parse-buffer alist symbol keys"
   (json-parse-buffer :object-type 'alist)
   '((a . 1) (b (c . 2)))))

;; `json-read' / `json-read-from-string', classic API --------------------

(nelisp--json-compat-smoke-check
 "read-from-string default (alist, symbol keys)"
 (json-read-from-string "{\"a\":1,\"b\":[1,2]}")
 '((a . 1) (b . [1 2])))

(nelisp--json-compat-smoke-check
 "read-from-string honors json-object-type=plist"
 (let ((json-object-type 'plist))
   (json-read-from-string "{\"a\":1}"))
 '(:a 1))

(nelisp--json-compat-smoke-check
 "read-from-string honors json-object-type=hash-table"
 (let ((json-object-type 'hash-table))
   (gethash "a" (json-read-from-string "{\"a\":1}")))
 1)

(nelisp--json-compat-smoke-check
 "read-from-string honors json-key-type=string override on alist"
 (let ((json-object-type 'alist) (json-key-type 'string))
   (json-read-from-string "{\"a\":1}"))
 '(("a" . 1)))

(nelisp--json-compat-smoke-check
 "read-from-string honors json-key-type=symbol override on hash-table"
 (let ((json-object-type 'hash-table) (json-key-type 'symbol))
   (gethash 'a (json-read-from-string "{\"a\":1}")))
 1)

(with-temp-buffer
  (insert "{\"a\":1}")
  (goto-char (point-min))
  (nelisp--json-compat-smoke-check
   "read (buffer) default alist symbol keys"
   (json-read)
   '((a . 1))))

(princ (format "JSON-COMPAT-SMOKE cases=%d mismatches=%d\n"
               nelisp--json-compat-smoke-cases
               nelisp--json-compat-smoke-mismatches))

(when (> nelisp--json-compat-smoke-mismatches 0)
  (kill-emacs 1))

;;; standalone-json-compat-smoke.el ends here
