;;; par-url-vendor.el --- GNU URL autoload and retrieval -*- lexical-binding: t; -*-

;; This test uses a data URL.  It does not need S/MIME discovery or a network
;; connection when the URL library loads.
(defvar smime-openssl-program nil)

(prin1 (cons 'autoload
             (mapcar (lambda (name)
                       (let ((file (symbol-file name 'defun)))
                         (and (autoloadp (symbol-function name))
                              file
                              (equal (file-name-base file) "url")
                              (equal (file-name-nondirectory
                                      (directory-file-name (file-name-directory file)))
                                     "url"))))
                     '(url-retrieve url-retrieve-synchronously))))
(terpri)

(let ((buffer (url-retrieve-synchronously "data:text/plain,GNU%20URL" t t)))
  (unwind-protect
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "\n\n")
        (prin1 (list 'data-body (buffer-substring-no-properties (point) (point-max))))
        (terpri))
    (kill-buffer buffer)))
