;;; nelisp-json.el --- Pure Lisp JSON parser + encoder for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T54 standalone gap analysis Wave 1 agent D, anvil-server JSON-RPC
;; unblock.
;;
;; This module provides the minimal `json.el' compatible surface that
;; NeLisp standalone needs before `anvil-server.el' can drop its host
;; Emacs JSON dependency:
;;
;;   - `nelisp-json-encode'
;;   - `nelisp-json-encode-string'
;;   - `nelisp-json-parse-string'
;;   - `nelisp-json-serialize'
;;   - `nelisp-json-read-from-string'
;;   - `nelisp-json-pretty-print-string'
;;
;; Design constraints:
;;   - pure Lisp only
;;   - zero dependency on host Emacs `json.el'
;;   - standard JSON only (no comments, trailing commas, or streams)
;;   - object default = hash-table with string keys
;;
;; Sentinel mapping used by the parser:
;;   - JSON null  -> `:null'
;;   - JSON false -> `:false'
;;   - JSON true  -> `t'
;;
;; Encoding intentionally keeps one long-standing `json.el' trap visible:
;; `nil' encodes to JSON null, not {}.  Therefore an empty object must be
;; represented explicitly, e.g. with `(make-hash-table :test 'equal)'.
;; A plist containing a nil-valued key is still an object because the
;; plist structure itself is non-empty; only the empty list is ambiguous
;; and is encoded as null.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-json-error "NeLisp JSON error")
(define-error 'nelisp-json-parse-error "NeLisp JSON parse error" 'nelisp-json-error)
(define-error 'nelisp-json-encode-error "NeLisp JSON encode error" 'nelisp-json-error)

(defconst nelisp-json--whitespace-chars '(?\s ?\t ?\n ?\r)
  "Characters treated as JSON whitespace.")

(defconst nelisp-json--literal-table
  '(("true" . t)
    ("false" . :false)
    ("null" . :null))
  "JSON literal lookup table.")

(defun nelisp-json--parse-error (json-string pos format-string &rest args)
  "Signal `nelisp-json-parse-error' at POS in JSON-STRING.
FORMAT-STRING and ARGS shape the human-readable message."
  (signal 'nelisp-json-parse-error
          (list (format "%s at position %d"
                        (apply #'format format-string args)
                        pos)
                json-string
                pos)))

(defun nelisp-json--encode-error (format-string &rest args)
  "Signal `nelisp-json-encode-error' using FORMAT-STRING and ARGS."
  (signal 'nelisp-json-encode-error
          (list (apply #'format format-string args))))

(defun nelisp-json--skip-whitespace (json-string pos)
  "Return first non-whitespace position in JSON-STRING at or after POS."
  (let ((len (length json-string)))
    (while (and (< pos len)
                (memq (aref json-string pos) nelisp-json--whitespace-chars))
      (setq pos (1+ pos)))
    pos))

(defun nelisp-json--hex-digit-value (char)
  "Return numeric value of hexadecimal CHAR, or nil if invalid."
  (cond
   ((and (>= char ?0) (<= char ?9)) (- char ?0))
   ((and (>= char ?a) (<= char ?f)) (+ 10 (- char ?a)))
   ((and (>= char ?A) (<= char ?F)) (+ 10 (- char ?A)))
   (t nil)))

(defun nelisp-json--parse-hex4 (json-string pos)
  "Parse four hexadecimal digits from JSON-STRING at POS.
Return (CODEPOINT . NEXT-POS)."
  (let ((value 0)
        (idx pos)
        digit)
    (dotimes (_ 4)
      (when (>= idx (length json-string))
        (nelisp-json--parse-error json-string idx "truncated unicode escape"))
      (setq digit (nelisp-json--hex-digit-value (aref json-string idx)))
      (unless digit
        (nelisp-json--parse-error json-string idx "invalid unicode escape"))
      (setq value (+ (* value 16) digit))
      (setq idx (1+ idx)))
    (cons value idx)))

(defun nelisp-json--high-surrogate-p (codepoint)
  "Return non-nil if CODEPOINT is a UTF-16 high surrogate."
  (and (>= codepoint #xD800) (<= codepoint #xDBFF)))

(defun nelisp-json--low-surrogate-p (codepoint)
  "Return non-nil if CODEPOINT is a UTF-16 low surrogate."
  (and (>= codepoint #xDC00) (<= codepoint #xDFFF)))

(defun nelisp-json--combine-surrogates (high low)
  "Combine HIGH and LOW UTF-16 surrogates into one codepoint."
  (+ #x10000
     (ash (- high #xD800) 10)
     (- low #xDC00)))

(defun nelisp-json--string-from-codepoint (codepoint)
  "Return a string containing CODEPOINT."
  (char-to-string codepoint))

(defun nelisp-json--parse-string-token (json-string pos)
  "Parse JSON string token in JSON-STRING starting at quote POS.
Return (VALUE . NEXT-POS) where NEXT-POS is after the closing quote."
  (let ((idx (1+ pos))
        (len (length json-string))
        (chunks nil)
        start escape codepoint low-info low)
    (catch 'done
      (while (< idx len)
        (setq start idx)
        (while (and (< idx len)
                    (not (memq (aref json-string idx) '(?\" ?\\)))
                    (>= (aref json-string idx) #x20))
          (setq idx (1+ idx)))
        (when (> idx start)
          (push (substring json-string start idx) chunks))
        (when (>= idx len)
          (nelisp-json--parse-error json-string idx "unterminated string"))
        (cond
         ((= (aref json-string idx) ?\")
          (setq idx (1+ idx))
          (throw 'done (cons (apply #'concat (nreverse chunks)) idx)))
         ((< (aref json-string idx) #x20)
          (nelisp-json--parse-error json-string idx
                                    "unescaped control character in string"))
         (t
          (setq idx (1+ idx))
          (when (>= idx len)
            (nelisp-json--parse-error json-string idx "truncated escape sequence"))
          (setq escape (aref json-string idx))
          (setq idx (1+ idx))
          (push
           (pcase escape
             (?\" "\"")
             (?\\ "\\")
             (?/ "/")
             (?b "\b")
             (?f "\f")
             (?n "\n")
             (?r "\r")
             (?t "\t")
             (?u
              (setq codepoint (nelisp-json--parse-hex4 json-string idx))
              (setq idx (cdr codepoint))
              (setq codepoint (car codepoint))
              (cond
               ((nelisp-json--high-surrogate-p codepoint)
                (when (or (>= (+ idx 1) len)
                          (/= (aref json-string idx) ?\\)
                          (/= (aref json-string (1+ idx)) ?u))
                  (nelisp-json--parse-error json-string idx
                                            "missing low surrogate after high surrogate"))
                (setq low-info (nelisp-json--parse-hex4 json-string (+ idx 2)))
                (setq idx (cdr low-info))
                (setq low (car low-info))
                (unless (nelisp-json--low-surrogate-p low)
                  (nelisp-json--parse-error json-string (- idx 4)
                                            "invalid low surrogate"))
                (nelisp-json--string-from-codepoint
                 (nelisp-json--combine-surrogates codepoint low)))
               ((nelisp-json--low-surrogate-p codepoint)
                (nelisp-json--parse-error json-string (- idx 4)
                                          "unexpected low surrogate"))
               (t
                (nelisp-json--string-from-codepoint codepoint))))
             (_
              (nelisp-json--parse-error json-string (1- idx)
                                        "invalid escape character %c" escape)))
           chunks))))
      (nelisp-json--parse-error json-string idx "unterminated string"))))

(defun nelisp-json--digit-char-p (char)
  "Return non-nil if CHAR is an ASCII digit."
  (and (>= char ?0) (<= char ?9)))

(defun nelisp-json--parse-number-token (json-string pos)
  "Parse a JSON number token from JSON-STRING at POS.
Return (NUMBER . NEXT-POS)."
  (let ((idx pos)
        (len (length json-string))
        has-fraction
        has-exponent
        number-text)
    (when (= (aref json-string idx) ?-)
      (setq idx (1+ idx))
      (when (>= idx len)
        (nelisp-json--parse-error json-string idx "truncated number")))
    (cond
     ((= (aref json-string idx) ?0)
      (setq idx (1+ idx))
      (when (and (< idx len) (nelisp-json--digit-char-p (aref json-string idx)))
        (nelisp-json--parse-error json-string idx
                                  "leading zero is not allowed")))
     ((nelisp-json--digit-char-p (aref json-string idx))
      (while (and (< idx len) (nelisp-json--digit-char-p (aref json-string idx)))
        (setq idx (1+ idx))))
     (t
      (nelisp-json--parse-error json-string idx "invalid number")))
    (when (and (< idx len) (= (aref json-string idx) ?.))
      (setq has-fraction t)
      (setq idx (1+ idx))
      (when (or (>= idx len) (not (nelisp-json--digit-char-p (aref json-string idx))))
        (nelisp-json--parse-error json-string idx
                                  "fraction requires at least one digit"))
      (while (and (< idx len) (nelisp-json--digit-char-p (aref json-string idx)))
        (setq idx (1+ idx))))
    (when (and (< idx len) (memq (aref json-string idx) '(?e ?E)))
      (setq has-exponent t)
      (setq idx (1+ idx))
      (when (and (< idx len) (memq (aref json-string idx) '(?+ ?-)))
        (setq idx (1+ idx)))
      (when (or (>= idx len) (not (nelisp-json--digit-char-p (aref json-string idx))))
        (nelisp-json--parse-error json-string idx
                                  "exponent requires at least one digit"))
      (while (and (< idx len) (nelisp-json--digit-char-p (aref json-string idx)))
        (setq idx (1+ idx))))
    (setq number-text (substring json-string pos idx))
    (cons (if (or has-fraction has-exponent)
              (string-to-number number-text)
            (string-to-number number-text))
          idx)))

(defun nelisp-json--parse-literal-token (json-string pos)
  "Parse a JSON literal from JSON-STRING at POS.
Return (VALUE . NEXT-POS)."
  (let ((table nelisp-json--literal-table)
        literal)
    (or
     (catch 'done
       (while table
         (setq literal (car table))
         (when (and (<= (+ pos (length (car literal))) (length json-string))
                    (string=
                     (substring json-string pos (+ pos (length (car literal))))
                     (car literal)))
           (throw 'done (cons (cdr literal) (+ pos (length (car literal))))))
         (setq table (cdr table)))
       nil)
     (nelisp-json--parse-error json-string pos "unexpected token"))))

(defun nelisp-json--object-key->plist-symbol (key)
  "Convert JSON object KEY string to a plist symbol."
  (intern (concat ":" key)))

(defun nelisp-json--plist-symbol->object-key (symbol)
  "Convert plist SYMBOL key into a JSON object key string."
  (let ((name (symbol-name symbol)))
    (if (and (> (length name) 0)
             (= (aref name 0) ?:))
        (substring name 1)
      name)))

(defun nelisp-json--normalize-object-key (key)
  "Return JSON object key string for KEY or signal."
  (cond
   ((stringp key) key)
   ((keywordp key) (nelisp-json--plist-symbol->object-key key))
   ((symbolp key) (symbol-name key))
   (t (nelisp-json--encode-error "invalid JSON object key: %S" key))))

(defun nelisp-json--parse-object-as-hash (pairs)
  "Return hash-table object built from PAIRS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair pairs table)
      (puthash (car pair) (cdr pair) table))))

(defun nelisp-json--parse-object-as-alist (pairs)
  "Return alist object built from PAIRS."
  pairs)

(defun nelisp-json--parse-object-as-plist (pairs)
  "Return plist object built from PAIRS."
  (let (plist)
    (dolist (pair pairs plist)
      (setq plist
            (append plist
                    (list (nelisp-json--object-key->plist-symbol (car pair))
                          (cdr pair)))))))

(defun nelisp-json--apply-array-type (items array-type)
  "Shape parsed array ITEMS according to ARRAY-TYPE."
  (pcase array-type
    ('list items)
    (_ (vconcat items))))

(defun nelisp-json--apply-object-type (pairs object-type)
  "Shape parsed object PAIRS according to OBJECT-TYPE."
  (pcase object-type
    ('alist (nelisp-json--parse-object-as-alist pairs))
    ('plist (nelisp-json--parse-object-as-plist pairs))
    (_ (nelisp-json--parse-object-as-hash pairs))))

(defun nelisp-json--parse-array-token (json-string pos options)
  "Parse JSON array in JSON-STRING at POS using OPTIONS.
Return (VALUE . NEXT-POS)."
  (let ((idx (nelisp-json--skip-whitespace json-string (1+ pos)))
        (items nil)
        parsed)
    (if (and (< idx (length json-string))
             (= (aref json-string idx) ?\]))
        (cons (nelisp-json--apply-array-type
               nil
               (plist-get options :array-type))
              (1+ idx))
      (catch 'done
        (while t
          (setq parsed (nelisp-json--parse-value json-string idx options))
          (push (car parsed) items)
          (setq idx (nelisp-json--skip-whitespace json-string (cdr parsed)))
          (when (>= idx (length json-string))
            (nelisp-json--parse-error json-string idx "unterminated array"))
          (pcase (aref json-string idx)
            (?, (setq idx (nelisp-json--skip-whitespace json-string (1+ idx))))
            (?\] (throw 'done
                        (cons (nelisp-json--apply-array-type
                               (nreverse items)
                               (plist-get options :array-type))
                              (1+ idx))))
            (_ (nelisp-json--parse-error json-string idx
                                         "expected ',' or ']' in array"))))))))

(defun nelisp-json--parse-object-token (json-string pos options)
  "Parse JSON object in JSON-STRING at POS using OPTIONS.
Return (VALUE . NEXT-POS)."
  (let ((idx (nelisp-json--skip-whitespace json-string (1+ pos)))
        (pairs nil)
        key-info key value-info)
    (if (and (< idx (length json-string))
             (= (aref json-string idx) ?\}))
        (cons (nelisp-json--apply-object-type
               nil
               (plist-get options :object-type))
              (1+ idx))
      (catch 'done
        (while t
          (unless (and (< idx (length json-string))
                       (= (aref json-string idx) ?\"))
            (nelisp-json--parse-error json-string idx
                                      "object key must be a string"))
          (setq key-info (nelisp-json--parse-string-token json-string idx))
          (setq key (car key-info))
          (setq idx (nelisp-json--skip-whitespace json-string (cdr key-info)))
          (unless (and (< idx (length json-string))
                       (= (aref json-string idx) ?:))
            (nelisp-json--parse-error json-string idx "expected ':' after object key"))
          (setq idx (nelisp-json--skip-whitespace json-string (1+ idx)))
          (setq value-info (nelisp-json--parse-value json-string idx options))
          (push (cons key (car value-info)) pairs)
          (setq idx (nelisp-json--skip-whitespace json-string (cdr value-info)))
          (when (>= idx (length json-string))
            (nelisp-json--parse-error json-string idx "unterminated object"))
          (pcase (aref json-string idx)
            (?, (setq idx (nelisp-json--skip-whitespace json-string (1+ idx))))
            (?\} (throw 'done
                        (cons (nelisp-json--apply-object-type
                               (nreverse pairs)
                               (plist-get options :object-type))
                              (1+ idx))))
            (_ (nelisp-json--parse-error json-string idx
                                         "expected ',' or '}' in object"))))))))

(defun nelisp-json--parse-value (json-string pos options)
  "Parse JSON value from JSON-STRING at POS using OPTIONS.
Return (VALUE . NEXT-POS)."
  (setq pos (nelisp-json--skip-whitespace json-string pos))
  (when (>= pos (length json-string))
    (nelisp-json--parse-error json-string pos "unexpected end of input"))
  (let ((parsed
         (pcase (aref json-string pos)
           (?\" (nelisp-json--parse-string-token json-string pos))
           (?\[ (nelisp-json--parse-array-token json-string pos options))
           (?\{ (nelisp-json--parse-object-token json-string pos options))
           ((or ?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
            (nelisp-json--parse-number-token json-string pos))
           ((or ?t ?f ?n)
            (nelisp-json--parse-literal-token json-string pos))
           (_ (nelisp-json--parse-error json-string pos "unexpected token")))))
    (cons
     (pcase (car parsed)
       (:null (plist-get options :null-object))
       (:false (plist-get options :false-object))
       (value value))
     (cdr parsed))))

(defun nelisp-json--json-false-p (value)
  "Return non-nil if VALUE should encode as JSON false."
  (memq value '(:false :json-false)))

(defun nelisp-json--alist-p (value)
  "Return non-nil if VALUE looks like an alist JSON object."
  (and (listp value)
       value
       (catch 'no
         (dolist (entry value t)
           (unless (and (consp entry)
                        (not (null (car entry)))
                        (or (stringp (car entry))
                            (symbolp (car entry))))
             (throw 'no nil))))))

(defun nelisp-json--plist-p (value)
  "Return non-nil if VALUE looks like a plist JSON object."
  (and (listp value)
       value
       (let ((cursor value)
             ok)
         (setq ok t)
         (while cursor
           (unless (and (consp cursor)
                        (or (keywordp (car cursor))
                            (stringp (car cursor))
                            (symbolp (car cursor)))
                        (consp (cdr cursor)))
             (setq ok nil
                   cursor nil))
           (when cursor
             (setq cursor (cddr cursor))))
         ok)))

(defun nelisp-json--hash-entries (table)
  "Return TABLE entries as a list of (KEY . VALUE) cons cells."
  (let (entries)
    (maphash
     (lambda (key value)
       (push (cons key value) entries))
     table)
    (nreverse entries)))

(defun nelisp-json--encode-string-contents (string)
  "Return escaped JSON string contents for STRING without quotes."
  (let ((chunks nil)
        (start 0)
        (len (length string))
        ch)
    (dotimes (idx len)
      (setq ch (aref string idx))
      (when (or (< ch #x20)
                (= ch ?\")
                (= ch ?\\))
        (when (> idx start)
          (push (substring string start idx) chunks))
        (push
         (pcase ch
           (?\" "\\\"")
           (?\\ "\\\\")
           (?\b "\\b")
           (?\f "\\f")
           (?\n "\\n")
           (?\r "\\r")
           (?\t "\\t")
           (_ (format "\\u%04X" ch)))
         chunks)
        (setq start (1+ idx))))
    (when (< start len)
      (push (substring string start len) chunks))
    (apply #'concat (nreverse chunks))))

(defun nelisp-json-encode-string (string)
  "Return STRING encoded as a quoted JSON string."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (concat "\"" (nelisp-json--encode-string-contents string) "\""))

(defun nelisp-json--encode-number (number)
  "Return NUMBER rendered as a JSON number."
  (unless (numberp number)
    (signal 'wrong-type-argument (list 'numberp number)))
  (if (floatp number)
      (let ((rendered (format "%.15g" number)))
        (unless (string-match-p
                 "\\`-?\\(?:0\\|[1-9][0-9]*\\)\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'"
                 rendered)
          (nelisp-json--encode-error
           "JSON does not support NaN or infinity: %S" number))
        rendered)
    (number-to-string number)))

(defun nelisp-json--join-with-comma (strings)
  "Join STRINGS with commas."
  (mapconcat #'identity strings ","))

(defun nelisp-json--encode-array (items)
  "Return JSON array string for ITEMS."
  (concat "[" (nelisp-json--join-with-comma
               (mapcar #'nelisp-json-encode items))
          "]"))

(defun nelisp-json--encode-object-pairs (pairs)
  "Return JSON object string for PAIRS."
  (concat
   "{"
   (nelisp-json--join-with-comma
    (mapcar
     (lambda (pair)
       (concat (nelisp-json-encode-string
                (nelisp-json--normalize-object-key (car pair)))
               ":"
               (nelisp-json-encode (cdr pair))))
     pairs))
   "}"))

(defun nelisp-json--value->object-pairs (value)
  "Return VALUE as a list of (KEY . VALUE) pairs if it is an object.
Signal if VALUE is not representable as a JSON object."
  (cond
   ((hash-table-p value)
    (nelisp-json--hash-entries value))
   ((nelisp-json--alist-p value)
    value)
   ((nelisp-json--plist-p value)
    (let ((cursor value)
          pairs)
      (while cursor
        (push (cons (car cursor) (cadr cursor)) pairs)
        (setq cursor (cddr cursor)))
      (nreverse pairs)))
   ((and (listp value)
         value
         (cl-every #'consp value))
    (nelisp-json--encode-error "invalid JSON object key in alist: %S" value))
   (t
    (nelisp-json--encode-error "value is not a JSON object: %S" value))))

(defun nelisp-json--encode-pretty (value depth)
  "Return pretty-printed JSON for VALUE at indentation DEPTH."
  (cond
   ((null value) "null")
   ((eq value :null) "null")
   ((eq value t) "true")
   ((nelisp-json--json-false-p value) "false")
   ((numberp value) (nelisp-json--encode-number value))
   ((stringp value) (nelisp-json-encode-string value))
   ((vectorp value)
    (if (= (length value) 0)
        "[]"
      (let ((indent (make-string (* depth 2) ?\s))
            (child-indent (make-string (* (1+ depth) 2) ?\s)))
        (concat
         "[\n"
         (mapconcat
          (lambda (item)
            (concat child-indent
                    (nelisp-json--encode-pretty item (1+ depth))))
          (append value nil)
          ",\n")
         "\n" indent "]"))))
   ((or (hash-table-p value)
        (nelisp-json--alist-p value)
        (nelisp-json--plist-p value))
    (let ((pairs (nelisp-json--value->object-pairs value)))
      (if (null pairs)
          "{}"
        (let ((indent (make-string (* depth 2) ?\s))
              (child-indent (make-string (* (1+ depth) 2) ?\s)))
          (concat
           "{\n"
           (mapconcat
            (lambda (pair)
              (concat
               child-indent
               (nelisp-json-encode-string
                (nelisp-json--normalize-object-key (car pair)))
               ": "
               (nelisp-json--encode-pretty (cdr pair) (1+ depth))))
            pairs
            ",\n")
           "\n" indent "}")))))
   ((listp value)
    (nelisp-json--encode-pretty (vconcat value) depth))
   (t
    (nelisp-json--encode-error "unsupported JSON value: %S" value))))

;;;###autoload
(defun nelisp-json-encode (value)
  "Encode VALUE as a compact JSON string.

Supported inputs:
- `nil'                    -> JSON null
- `t'                      -> JSON true
- `:false' / `:json-false' -> JSON false
- numbers, strings
- vectors                  -> JSON arrays
- hash tables              -> JSON objects
- alists / plists          -> JSON objects

Important empty-object note: `nil' encodes to JSON null, not {}.  Use
an explicit empty hash table to encode an empty object."
  (cond
   ((null value) "null")
   ((eq value t) "true")
   ((nelisp-json--json-false-p value) "false")
   ((numberp value) (nelisp-json--encode-number value))
   ((stringp value) (nelisp-json-encode-string value))
   ((vectorp value) (nelisp-json--encode-array (append value nil)))
   ((or (hash-table-p value)
        (nelisp-json--alist-p value)
        (nelisp-json--plist-p value))
    (nelisp-json--encode-object-pairs
     (nelisp-json--value->object-pairs value)))
   ((and (listp value)
         value
         (cl-every #'consp value))
    (nelisp-json--encode-error "invalid JSON object key in alist: %S" value))
   ((listp value)
    (nelisp-json--encode-array value))
   (t
    (nelisp-json--encode-error "unsupported JSON value: %S" value))))

(defun nelisp-json--normalize-parse-options (args)
  "Normalize parser keyword ARGS to an internal plist."
  (let ((object-type (or (plist-get args :object-type) 'hash-table))
        (array-type (or (plist-get args :array-type) 'vector))
        (null-object :null)
        (false-object :false))
    (when (memq :null-object args)
      (setq null-object (plist-get args :null-object)))
    (when (memq :false-object args)
      (setq false-object (plist-get args :false-object)))
    (list :object-type object-type
          :array-type array-type
          :null-object null-object
          :false-object false-object)))

;;;###autoload
(defun nelisp-json-parse-string (json-string &rest args)
  "Parse JSON-STRING and return the corresponding Lisp value.

Defaults:
- object type = hash-table
- array type  = vector
- null        = `:null'
- false       = `:false'

Recognised keyword arguments:
- `:object-type' => `hash-table' / `alist' / `plist'
- `:array-type'  => `vector' / `list'
- `:null-object'
- `:false-object'"
  (unless (stringp json-string)
    (signal 'wrong-type-argument (list 'stringp json-string)))
  (let* ((options (nelisp-json--normalize-parse-options args))
         (parsed (nelisp-json--parse-value json-string 0 options))
         (end (nelisp-json--skip-whitespace json-string (cdr parsed))))
    (when (< end (length json-string))
      (nelisp-json--parse-error json-string end "trailing data"))
    (car parsed)))

;;;###autoload
(defun nelisp-json-serialize (value &rest _args)
  "Alias for `nelisp-json-encode'."
  (nelisp-json-encode value))

;;;###autoload
(defun nelisp-json-read-from-string (json-string &rest args)
  "Alias for `nelisp-json-parse-string'."
  (apply #'nelisp-json-parse-string json-string args))

;;;###autoload
(defun nelisp-json-pretty-print-string (json-string)
  "Return JSON-STRING reformatted with 2-space indentation."
  (nelisp-json--encode-pretty
   (nelisp-json-parse-string json-string)
   0))

(provide 'nelisp-json)

;;; nelisp-json.el ends here
