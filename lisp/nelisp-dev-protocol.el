;;; nelisp-dev-protocol.el --- bounded DEV protocol envelopes -*- lexical-binding: t; -*-
(require 'json)
(require 'cl-lib)
(defconst nelisp-dev-protocol-version "1")
(defconst nelisp-dev-protocol-budget 16384)
(defvar nelisp-dev-protocol--details (make-hash-table :test #'equal))
(defvar nelisp-dev-protocol--detail-bytes 0)
(defconst nelisp-dev-protocol-detail-limit 64)
(defconst nelisp-dev-protocol-detail-max-bytes (* 1024 1024))
(defvar nelisp-dev-protocol--epoch 0)

(defun nelisp-dev-protocol-value (value)
  "Convert legacy plists into wire values, preserving null and booleans.
Use this for bounded diagnostic records, never for arbitrary application heaps."
  (cond
   ((memq value '(nil :null :unknown :unavailable)) :null)
   ((eq value :false) :false)
   ((eq value t) t)
   ((vectorp value) (vconcat (mapcar #'nelisp-dev-protocol-value value)))
   ((and (consp value) (keywordp (car value)))
    (unless (and (proper-list-p value) (= 0 (% (length value) 2)))
      (error "Malformed legacy diagnostic plist"))
    (let (out)
      (while value
        (let ((key (pop value)))
          (unless (keywordp key) (error "Malformed legacy diagnostic key"))
          (push (cons (substring (symbol-name key) 1)
                      (nelisp-dev-protocol-value (pop value))) out)))
      (nreverse out)))
   ((consp value)
    (unless (proper-list-p value) (error "Malformed legacy diagnostic list"))
    (if (cl-every (lambda (entry) (and (consp entry) (stringp (car entry)))) value)
        (mapcar (lambda (entry) (cons (car entry) (nelisp-dev-protocol-value (cdr entry)))) value)
      (vconcat (mapcar #'nelisp-dev-protocol-value value))))
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   (t value)))

(defun nelisp-dev-protocol--expire ()
  "Release expired entries, including entries that are never queried again."
  (let ((now (float-time)) (expired nil))
    (maphash (lambda (key entry)
               (when (>= (- now (car entry)) 900) (push key expired)))
             nelisp-dev-protocol--details)
    (dolist (key expired)
      (setq nelisp-dev-protocol--detail-bytes
            (- nelisp-dev-protocol--detail-bytes
               (cadr (gethash key nelisp-dev-protocol--details))))
      (remhash key nelisp-dev-protocol--details))))

(defun nelisp-dev-protocol-detail-put (id value)
  "Store one bounded detail VALUE under ID; return ID or nil."
  (nelisp-dev-protocol--expire)
  (let* ((bytes (string-bytes (prin1-to-string value)))
         (old (gethash id nelisp-dev-protocol--details)))
   (when (and (stringp id) (<= bytes nelisp-dev-protocol-detail-max-bytes)
             (<= (+ (- nelisp-dev-protocol--detail-bytes (or (cadr old) 0))
                    bytes) (* 16 1024 1024))
             (or old (< (hash-table-count nelisp-dev-protocol--details)
                        nelisp-dev-protocol-detail-limit)))
    (puthash id (list (float-time) bytes value) nelisp-dev-protocol--details)
    (setq nelisp-dev-protocol--detail-bytes
          (+ (- nelisp-dev-protocol--detail-bytes (or (cadr old) 0)) bytes)) id)))

(defun nelisp-dev-protocol-detail-get (id)
  "Return a non-expired detail, or nil."
  (nelisp-dev-protocol--expire)
  (nth 2 (gethash id nelisp-dev-protocol--details)))

(defun nelisp-dev-protocol-detail-clear ()
  "Release all protocol detail references and return the count."
  (let ((n (hash-table-count nelisp-dev-protocol--details)))
    (clrhash nelisp-dev-protocol--details)
    (setq nelisp-dev-protocol--detail-bytes 0
          nelisp-dev-protocol--epoch (1+ nelisp-dev-protocol--epoch)) n))

(defun nelisp-dev-protocol--json-value (value)
  (cond
   ((eq value :null) nil)
   ((null value) (make-hash-table :test #'equal))
   ((eq value :false) :json-false)
   ((vectorp value) (apply #'vector (mapcar #'nelisp-dev-protocol--json-value value)))
   ((consp value)
    (if (and (consp (car value)) (stringp (caar value)))
        (mapcar (lambda (pair)
                  (cons (car pair) (nelisp-dev-protocol--json-value (cdr pair)))) value)
      (if (stringp (car value))
          (cons (car value) (nelisp-dev-protocol--json-value (cdr value)))
        (mapcar #'nelisp-dev-protocol--json-value value))))
   (t value)))

(defun nelisp-dev-protocol-envelope (operation request-id status identity summary
                                                diagnostics data limitations)
  (list (cons "schema_version" nelisp-dev-protocol-version)
        (cons "operation" operation) (cons "request_id" request-id)
        (cons "status" status) (cons "identity" (or identity :null))
        (cons "summary" summary)
        (cons "diagnostics"
              (vconcat
               (mapcar
                (lambda (diagnostic)
                  (if (assoc "id" diagnostic) diagnostic
                    (cons
                     (cons "id" (secure-hash
                                 'sha256
                                 (prin1-to-string
                                  (list (cdr (assoc "code" diagnostic))
                                        (cdr (assoc "phase" diagnostic)) identity
                                        (cdr (assoc "span" diagnostic))
                                        (cdr (assoc "path" diagnostic))
                                        (cdr (assoc "subject" diagnostic))))))
                     diagnostic)))
                diagnostics)))
        (cons "data" (or data :null))
        (cons "limitations" (or limitations [])) (cons "next_cursor" :null)))

(defun nelisp-dev-protocol-string-keys (value)
  "Normalize JSON alist keys from either symbols or strings."
  (cond
   ((vectorp value) (apply #'vector (mapcar #'nelisp-dev-protocol-string-keys value)))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (pair)
              (cons (if (symbolp (car pair)) (symbol-name (car pair)) (car pair))
                    (nelisp-dev-protocol-string-keys (cdr pair)))) value))
   ((consp value) (mapcar #'nelisp-dev-protocol-string-keys value))
   (t value)))

(defun nelisp-dev-protocol--encode (value)
  "Encode VALUE independently of the caller's JSON reader bindings."
  (let* ((json-encoding-pretty-print nil)
         (json-null nil)
         (json-false :json-false)
         (json (json-encode (nelisp-dev-protocol--json-value value))))
    json))

(defun nelisp-dev-protocol-json (value)
  (let ((json (nelisp-dev-protocol--encode value)))
    (if (<= (string-bytes json) nelisp-dev-protocol-budget)
        json
      (error "NELISP-DEV result exceeds encoded budget"))))

(defun nelisp-dev-protocol-page (result request context)
  "Page RESULT diagnostics with a cursor bound to the complete current result.
The cursor is reproducible across source-only CLI workers.  Live cursors also
bind the session and explicit clear epoch.  Oversized single rows stay omitted
with an explicit limitation; callers never receive truncated JSON bytes."
  (let* ((limits (cdr (assoc "limits" request)))
         (budget (or (cdr (assoc "bytes" limits)) nelisp-dev-protocol-budget))
         (size (or (cdr (assoc "page_size" limits)) 50))
         (cursor (cdr (assoc "cursor" limits)))
         (rows (cdr (assoc "diagnostics" result)))
         (total (length rows))
         (base (copy-tree result t))
         (summary (copy-tree (cdr (assoc "summary" base))))
         (fingerprint
          (secure-hash 'sha256
                       (prin1-to-string
                        (list (cdr (assoc "operation" request))
                              (cdr (assoc "arguments" request))
                              (cdr (assoc "identity" result)) rows
                              (plist-get context :session-id)
                              nelisp-dev-protocol--epoch))))
         (offset 0) (expiry (+ (floor (float-time)) 900)))
    (when (and cursor (not (eq cursor :null)))
      (let ((parts (and (stringp cursor) (split-string cursor ":"))))
        (unless (and (= (length parts) 3)
                     (equal (car parts) fingerprint)
                     (string-match-p "\\`[0-9]+\\'" (nth 1 parts))
                     (string-match-p "\\`[0-9]+\\'" (nth 2 parts))
                     (> (string-to-number (nth 2 parts)) (float-time))
                     (< (string-to-number (nth 1 parts)) total))
          (error "NELISP-DEV-STALE-CURSOR"))
        (setq offset (string-to-number (nth 1 parts))
              expiry (string-to-number (nth 2 parts)))))
    (let ((count (min size (- total offset))) (done nil))
      (while (not done)
        (setf (alist-get "diagnostics" base nil nil #'equal)
              (cl-subseq rows offset (+ offset count))
              (alist-get "next_cursor" base nil nil #'equal)
              (if (< (+ offset count) total)
                  (format "%s:%d:%d" fingerprint (+ offset count) expiry) :null)
              (alist-get "returned" summary nil nil #'equal) count
              (alist-get "omitted" summary nil nil #'equal) (- total count)
              (alist-get "summary" base nil nil #'equal) summary)
        (setq done (<= (string-bytes (nelisp-dev-protocol--encode base)) budget))
        (unless done
          (if (> count 0) (setq count (1- count))
            (setq base
                  (nelisp-dev-protocol-envelope
                   (cdr (assoc "operation" result))
                   (cdr (assoc "request_id" result)) "inconclusive"
                   (cdr (assoc "identity" result))
                   (list (cons "returned" 0) (cons "omitted" total)) [] nil
                   ["Result exceeds byte budget; narrow the request."]))
            (setq done t))))
      ;; No progress cursor: a single diagnostic cannot fit this budget.
      (when (and (= count 0) (> total 0))
        (setf (alist-get "next_cursor" base nil nil #'equal) :null
              (alist-get "status" base nil nil #'equal) "inconclusive"
              (alist-get "limitations" base nil nil #'equal)
              ["No diagnostic fits the byte budget; narrow the request."]))
      (when (> (string-bytes (nelisp-dev-protocol--encode base)) budget)
        (setf (alist-get "identity" base nil nil #'equal) :null
              (alist-get "data" base nil nil #'equal) :null
              (alist-get "diagnostics" base nil nil #'equal) []
              (alist-get "next_cursor" base nil nil #'equal) :null
              (alist-get "status" base nil nil #'equal) "inconclusive"
              (alist-get "limitations" base nil nil #'equal)
              ["Result metadata exceeds the byte budget; narrow the request."]))
      base)))

(provide 'nelisp-dev-protocol)
