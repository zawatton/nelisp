;;; nelisp-dev-cli.el --- machine JSON frontend for nelisp-dev -*- lexical-binding: t; -*-
(require 'json)
(require 'nelisp-dev)

(defun nelisp-dev-cli--error (code message)
  (nelisp-dev-protocol-envelope
   "unknown" "request-invalid" "failed" nil '(("errors" . 1))
   (vector (list (cons "code" code) (cons "message" message))) nil []))

(defun nelisp-dev-cli--exit-code (result)
  (let* ((status (cdr (assoc "status" result)))
         (diagnostics (cdr (assoc "diagnostics" result)))
         (first (and (vectorp diagnostics) (> (length diagnostics) 0)
                     (aref diagnostics 0)))
         (invalid (member (cdr (assoc "code" first))
                          '("NELISP-DEV-INVALID-REQUEST"
                            "NELISP-DEV-UNKNOWN-OPERATION"))))
    (cond (invalid 2) ((equal status "ok") 0) ((equal status "failed") 1)
          ((equal status "inconclusive") 3) ((equal status "unsupported") 4)
          ((equal status "cancelled") 5) (t 2))))

(defun nelisp-dev-cli-main ()
  "Read one request file, write exactly one JSON response, and exit." 
  (let ((file (getenv "NELISP_DEV_REQUEST_FILE")) result)
    (let ((json-null :null) (json-false :false))
    (condition-case error-data
        (progn
          (unless (and file (file-readable-p file))
            (error "request file is not readable"))
          (unless (and (file-regular-p file)
                       (<= (file-attribute-size (file-attributes file)) 1048576))
            (error "request file exceeds input budget or is not a regular file"))
          (let ((request
                 (nelisp-dev-protocol-string-keys
                  (json-parse-string
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8-unix))
                       (insert-file-contents file))
                     (buffer-string))
                   :object-type 'alist :array-type 'array
                   :null-object :null :false-object :false))))
            (setq result (nelisp-dev-dispatch
                          request (nelisp-dev-context default-directory)))))
      (error
       (message "DEV request rejected: %s" (error-message-string error-data))
       (setq result (nelisp-dev-cli--error "NELISP-DEV-INVALID-REQUEST"
                                           (error-message-string error-data)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (princ (nelisp-dev-protocol-json result))
      (princ "\n"))
    (kill-emacs (nelisp-dev-cli--exit-code result)))))

(provide 'nelisp-dev-cli)
