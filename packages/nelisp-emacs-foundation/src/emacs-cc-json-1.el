;;; emacs-cc-json-1.el --- JSON insertion fallback -*- lexical-binding: t; -*-

;;; Code:

(require 'json)
(defvar inhibit-read-only nil)

(defun emacs-cc-json-1-insert (object &rest args)
  "Insert OBJECT as compact JSON, forwarding serializer keyword ARGS.
An empty object is represented by nil, following `json-insert'.  Encode
before insertion so serialization errors leave the current buffer alone."
  (when (/= 0 (% (length args) 2))
    (signal 'wrong-type-argument (list 'plistp args)))
  (let ((tail args))
    (while tail
      (unless (memq (car tail) '(:null-object :false-object))
        (signal 'error
                (list "One of :null-object or :false-object should be specified")))
      (setq tail (cddr tail))))
  (let* ((null-object (if (memq :null-object args)
                          (plist-get args :null-object) :null))
         (false-object (if (memq :false-object args)
                           (plist-get args :false-object) :false))
         (value (if (and (null object)
                         (not (or (and (memq :null-object args)
                                       (eq object null-object))
                                  (and (memq :false-object args)
                                       (eq object false-object)))))
                    (make-hash-table)
                  object))
         (encoded
          (if (and (fboundp 'emacs-json--to-encodable)
                   (or (and (memq :null-object args)
                            (not (eq null-object :null)))
                       (and (memq :false-object args)
                            (not (eq false-object :false)))))
              (json-encode
               (emacs-json--to-encodable value null-object false-object))
            (apply #'json-serialize value args))))
    (when (and (boundp 'buffer-read-only) buffer-read-only
               (not (and (boundp 'inhibit-read-only) inhibit-read-only)))
      (signal 'buffer-read-only (list (current-buffer))))
    ;; The native serializer returns UTF-8 bytes, while the Lisp fallback
    ;; may return characters.  JSON insertion keeps bytes in unibyte
    ;; buffers and decodes them into characters in multibyte buffers.
    (insert
     (if (and (boundp 'enable-multibyte-characters)
              (not enable-multibyte-characters))
         (if (multibyte-string-p encoded)
             (encode-coding-string encoded 'utf-8)
           encoded)
       (if (multibyte-string-p encoded)
           encoded
         (decode-coding-string encoded 'utf-8))))
    nil))

(unless (fboundp 'json-insert)
  (defalias 'json-insert #'emacs-cc-json-1-insert))

(provide 'emacs-cc-json-1)
;;; emacs-cc-json-1.el ends here
