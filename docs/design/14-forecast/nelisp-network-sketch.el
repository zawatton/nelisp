;;; nelisp-network-sketch.el --- Phase 5-C.2 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)
(require 'url)

(cl-defstruct nelisp-network
  id
  host-conn
  actor
  (status 'open)
  (props nil))

(defvar nelisp-network--registry nil)

(defun nelisp-open-network (&rest args)
  "Wrap host `make-network-process' / `open-network-stream'."
  (let* ((name (or (plist-get args :name) "nelisp-net"))
         (host (plist-get args :host))
         (service (plist-get args :service))
         (type (or (plist-get args :type) 'tcp))
         (buffer (plist-get args :buffer))
         (host-sentinel (plist-get args :sentinel))
         (host-filter (plist-get args :filter))
         (conn
          (cond
           ((eq type 'tls)
            (open-network-stream name buffer host service :type 'tls))
           (t
            (make-network-process
             :name name :host host :service service
             :buffer buffer
             :sentinel host-sentinel
             :filter host-filter))))
         (wrap (make-nelisp-network :id name :host-conn conn :status 'open)))
    (push wrap nelisp-network--registry)
    wrap))

(defun nelisp-network-send-string (conn s)
  (process-send-string (nelisp-network-host-conn conn) s))

(defun nelisp-network-close (conn)
  (delete-process (nelisp-network-host-conn conn))
  (setq nelisp-network--registry (delq conn nelisp-network--registry))
  (setf (nelisp-network-status conn) 'closed))

(defun nelisp-network-live-p (conn)
  (process-live-p (nelisp-network-host-conn conn)))

(defun nelisp-network-list ()
  (copy-sequence nelisp-network--registry))

;;; HTTP client (thin) ------------------------------------------------

(defun nelisp-http-get (url &rest _plist)
  "Fetch URL via host `url-retrieve-synchronously'.
Returns (STATUS-CODE . BODY-STRING)."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (let ((code (and (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                     (string-to-number (match-string 1))))
          (body-start (and (re-search-forward "^$" nil t)
                           (1+ (point)))))
      (cons code
            (when body-start
              (buffer-substring-no-properties body-start (point-max)))))))

(defun nelisp-http-head (url &rest _plist)
  "Fetch URL HEAD via host url library."
  (let ((url-request-method "HEAD"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'nelisp-network-sketch)
;;; nelisp-network-sketch.el ends here
