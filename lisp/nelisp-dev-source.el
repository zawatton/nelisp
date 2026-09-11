;;; nelisp-dev-source.el --- bounded nonexecuting source queries -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-dev-protocol)

(defconst nelisp-dev-source--max-file-bytes (* 2 1024 1024))
(defconst nelisp-dev-source--max-files 32)

(defun nelisp-dev-source--arg (request key)
  (cdr (assoc key (cdr (assoc "arguments" request)))))

(defun nelisp-dev-source--position (text offset)
  "Return one-based Unicode coordinates and a separate zero-based UTF-8 offset."
  (let* ((prefix (substring text 0 offset))
         (last (or (cl-position ?\n prefix :from-end t) -1)))
    (list (cons "line" (1+ (cl-count ?\n prefix)))
          (cons "column" (- offset last))
          (cons "byte_offset" (string-bytes (encode-coding-string prefix 'utf-8-unix)))
          (cons "encoding" "utf-8"))))

(defun nelisp-dev-source--parse (text)
  "Read forms with exact reader spans, without evaluating any form."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (forms problem start)
      (condition-case err
          (while (progn
                   (skip-chars-forward " \t\r\n")
                   (while (eq (char-after) ?\;)
                     (forward-line 1) (skip-chars-forward " \t\r\n"))
                   (not (eobp)))
            (setq start (1- (point)))
            ;; EOF here is inside a form, not normal EOF between forms.
            (let ((form (read (current-buffer))))
              (push (list :form form :start start :end (1- (point))) forms)))
        (error (setq problem (list :message (error-message-string err)
                                  :start (or start 0) :end (1- (point))))))
      (list :forms (nreverse forms) :problem problem))))

(defun nelisp-dev-source--read (root path)
  "Read a bounded regular UTF-8 file inside ROOT and preserve its byte hash."
  (unless (and (stringp path) (> (length path) 0))
    (error "arguments.path (or each files entry) must be a nonempty string"))
  (let ((file (expand-file-name path root)))
    (unless (and (file-in-directory-p file root) (file-regular-p file)
                 (file-readable-p file))
      (error "Source must be a readable regular file inside the project"))
    (when (> (file-attribute-size (file-attributes file)) nelisp-dev-source--max-file-bytes)
      (error "Source exceeds 2 MiB input limit"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 (1+ nelisp-dev-source--max-file-bytes))
      (when (> (buffer-size) nelisp-dev-source--max-file-bytes)
        (error "Source grew beyond input limit"))
      (let* ((raw (buffer-string))
             (text (decode-coding-string raw 'utf-8-unix))
             (parsed (nelisp-dev-source--parse text)))
        ;; Reject malformed UTF-8 instead of reporting byte-valued characters
        ;; as Unicode scalar columns.
        (when (cl-some (lambda (ch) (> ch #x10ffff)) (string-to-list text))
          (error "Source is not valid UTF-8"))
        (list :path (file-relative-name file root) :text text
              :hash (secure-hash 'sha256 raw)
              :forms (plist-get parsed :forms) :problem (plist-get parsed :problem))))))

(defun nelisp-dev-source--definition (record text)
  (let ((form (plist-get record :form)))
    (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
               (symbolp (cadr form)) (proper-list-p (nth 2 form)))
      (list (cons "symbol" (symbol-name (cadr form)))
            (cons "kind" (symbol-name (car form)))
            (cons "signature" (prin1-to-string (nth 2 form)))
            (cons "start" (nelisp-dev-source--position text (plist-get record :start)))
            (cons "end" (nelisp-dev-source--position text (plist-get record :end)))
            (cons "provenance" "known: reader definition")
            (cons "effects" ["unknown"])))))

(defun nelisp-dev-source--calls (form)
  "Return syntactic call-head candidates, excluding quoted data.
Unknown macros and local function shadowing prevent these candidates from
establishing a complete runtime dependency graph."
  (when (and (consp form) (proper-list-p form))
    (let ((head (car form)))
      (cond
       ((memq head '(quote function)) nil)
       ((memq head '(let let*))
        (append
         (cl-mapcan (lambda (binding)
                      (when (consp binding) (nelisp-dev-source--calls (cadr binding))))
                    (and (proper-list-p (cadr form)) (cadr form)))
         (cl-mapcan #'nelisp-dev-source--calls (cddr form))))
       ((memq head '(setq setq-default))
        (let ((rest (cdr form)) calls)
          (while (consp rest)
            (setq calls (append calls (nelisp-dev-source--calls (cadr rest)))
                  rest (cddr rest))) calls))
       (t (append (and (symbolp head) (list head))
                  (cl-mapcan #'nelisp-dev-source--calls (cdr form))))))))

(defun nelisp-dev-source--diagnostic (code source start end message subject)
  (let* ((span (list (cons "start" (nelisp-dev-source--position (plist-get source :text) start))
                     (cons "end" (nelisp-dev-source--position (plist-get source :text) end))))
         (id (secure-hash 'sha256
                          (prin1-to-string (list code (plist-get source :hash)
                                                (plist-get source :path) span subject)))))
    (list (cons "id" id) (cons "code" code) (cons "phase" "source")
          (cons "path" (plist-get source :path)) (cons "span" span)
          (cons "message" message))))

(defun nelisp-dev-source-dispatch (request context)
  "Describe, parse-check or inspect call candidates in explicitly named files.
This is a bounded in-memory index fallback, not a replacement repository index.
It does not run application forms, macros, source loaders, or dependency builds."
  (let* ((op (cdr (assoc "operation" request)))
         (root (file-name-as-directory (expand-file-name
                                       (or (plist-get context :root) default-directory))))
         (paths (or (nelisp-dev-source--arg request "files")
                    (vector (nelisp-dev-source--arg request "path"))))
         (symbol (nelisp-dev-source--arg request "symbol"))
         sources diagnostics definitions edges errors)
    (condition-case err
        (progn
          (unless (and (vectorp paths) (< 0 (length paths))
                       (<= (length paths) nelisp-dev-source--max-files))
            (error "Explicit path or files array of 1..32 paths is required"))
          (unless (or (equal op "check") (and (stringp symbol) (> (length symbol) 0)))
            (error "arguments.symbol is required"))
          (dolist (path (delete-dups (sort (append paths nil) #'string<)))
            (let* ((source (nelisp-dev-source--read root path))
                   (text (plist-get source :text))
                   (problem (plist-get source :problem)))
              (push source sources)
              (when problem
                (setq errors (1+ (or errors 0)))
                (push (nelisp-dev-source--diagnostic
                       "NELISP-CHECK-SYNTAX" source (plist-get problem :start)
                       (plist-get problem :end) (plist-get problem :message) "reader")
                      diagnostics))
              (dolist (record (plist-get source :forms))
                (let* ((definition (nelisp-dev-source--definition record text))
                       (form (plist-get record :form)))
                  (when definition
                    (when (equal symbol (cdr (assoc "symbol" definition)))
                      (push (append (list (cons "path" (plist-get source :path))) definition)
                            definitions))
                    (when (and (equal op "impact")
                               (cl-some (lambda (head) (equal (symbol-name head) symbol))
                                        (cl-mapcan #'nelisp-dev-source--calls (cdddr form))))
                      (push (list (cons "caller" (cdr (assoc "symbol" definition)))
                                  (cons "path" (plist-get source :path))
                                  (cons "kind" "syntactic-call-candidate")
                                  (cons "provenance" "inferred: unexpanded reader form"))
                            edges))))))))
      (error
       (setq errors (1+ (or errors 0)))
       (push (list (cons "code" "NELISP-SOURCE-INPUT")
                   (cons "message" (error-message-string err))) diagnostics)))
    (setq sources (nreverse sources) definitions (nreverse definitions)
          edges (nreverse edges) diagnostics (nreverse diagnostics))
    (let ((identity
           (list (cons "target" (or (plist-get context :target) "host-emacs"))
                 (cons "source_revision" :null)
                 (cons "source_content_hash"
                       (cond ((null sources) :null)
                             ((null (cdr sources)) (concat "sha256:" (plist-get (car sources) :hash)))
                             (t (concat "sha256:"
                                        (secure-hash 'sha256
                                         (prin1-to-string
                                          (mapcar (lambda (s) (cons (plist-get s :path)
                                                                    (plist-get s :hash))) sources)))))))
                 (cons "runtime_artifact_hash" :null)
                 (cons "session_id" (or (plist-get context :session-id) :null))
                 (cons "generation" :null))))
      (nelisp-dev-protocol-envelope
       op (cdr (assoc "request_id" request))
       (cond (errors "failed")
             ((and (equal op "describe") (= (length definitions) 1)) "ok")
             (t "inconclusive"))
       identity
       (list (cons "errors" (or errors 0)) (cons "files" (length sources))
             (cons "matches" (length definitions)) (cons "callers" (length edges)))
       (vconcat diagnostics)
       (list (cons "definitions" (vconcat definitions)) (cons "callers" (vconcat edges))
             (cons "coverage" "Explicit files only; syntax and parsed definitions"))
       ["No application code or macro was executed."
        "Arity, dynamic calls, macro expansion and complete dependency closure are not proven."
        "Loaded artifact identity, runtime contracts and effects remain unknown."
        "Multiple source definitions do not identify which definition is currently loaded."]))))

(provide 'nelisp-dev-source)
;;; nelisp-dev-source.el ends here
