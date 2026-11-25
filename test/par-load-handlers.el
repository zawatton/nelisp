;;; -*- lexical-binding: t; -*-
(require 'jka-compr)
(require 'epa-file)
(require 'tramp)

(defvar par-load-handler-value nil)

(defun par-load-handlers-row (name value)
  (prin1 (list name value))
  (terpri))

(defun par-load-handlers-short (root file)
  (and file (if (string-prefix-p root file) (substring file (length root)) file)))

(defun par-load-handlers-virtual (operation file &optional directory &rest _)
  (cond
   ((eq operation 'expand-file-name)
    (if (string-prefix-p "/" file) file
      (concat (or directory default-directory) file)))
   ((eq operation 'file-readable-p)
    (or (string-suffix-p "/magic.el.gpg" file)
        (equal file "/ssh:example.invalid:/virtual/magic.el")))
   ((eq operation 'load) (setq par-load-handler-value 'handler) t)))

(defvar par-load-handlers-expanded-target nil)

(defun par-load-handlers-expand-candidate (operation _file &rest _)
  (when (eq operation 'expand-file-name)
    par-load-handlers-expanded-target))

(let* ((root (file-name-as-directory (make-temp-file "par-load-handlers-" t)))
       (load-path (cons root load-path))
       (load-suffixes '(".elc" ".el"))
       (load-file-rep-suffixes '("" ".gz"))
       (gzip (concat root "compressed.el.gz"))
       (cipher (concat root "cipher.el.gpg"))
       (scratch (concat root "raw"))
       (plain (concat root "plain.el"))
       (tramp-regexp (car (rassq 'tramp-file-name-handler file-name-handler-alist)))
       (epa-regexp (car (rassq 'epa-file-handler file-name-handler-alist))))
  (unwind-protect
      (progn
        ;; Write the compressed fixture without invoking the write handler.
        (let ((coding-system-for-write 'no-conversion))
          (with-temp-file scratch
            (set-buffer-multibyte nil)
            (insert (base64-decode-string
                     "H4sIAAAAAAAC/9MoTi0pVChILNLNyU9M0c1IzEvJSS3SLUvMKU1VUE/Ozy0oSi0uTk3R5AIAG5Gp5CoAAAA="))))
        (rename-file scratch gzip)
        (par-load-handlers-row
         'compressed-search
         (list (par-load-handlers-short root (locate-library "compressed" nil (list root)))
               (load "compressed" nil t) par-load-handler-value))
        ;; An explicit .gz name exercises jka-compr-handler's load operation.
        (setq par-load-handler-value nil)
        (par-load-handlers-row 'compressed-load
                               (list (load gzip nil t) par-load-handler-value))
        (let ((operations (get 'jka-compr-handler 'operations))
              (decoder (get 'load 'jka-compr))
              (calls 0))
          (unwind-protect
              (progn
                ;; Let openp use the handler for the suffixed candidate.
                (put 'jka-compr-handler 'operations nil)
                (put 'load 'jka-compr
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (apply decoder args)))
                (setq par-load-handler-value nil)
                (par-load-handlers-row
                 'compressed-candidate-handler
                 (list (load "compressed" nil t) par-load-handler-value calls)))
            (put 'load 'jka-compr decoder)
            (put 'jka-compr-handler 'operations operations)))
        (with-temp-file scratch (insert "ciphertext"))
        (rename-file scratch cipher)
        (par-load-handlers-row
         'encrypted-search
         (par-load-handlers-short root (locate-file "cipher" (list root) '(".el.gpg"))))
        (with-temp-file plain (insert ";; empty library\n"))
        (par-load-handlers-row
         'quoted-directory
         (par-load-handlers-short root (locate-library "plain" nil (list (concat "/:" root)))))
        (par-load-handlers-row
         'dot-slash-local
         (par-load-handlers-short root (locate-library "./plain" nil (list root))))
        (let ((file-name-handler-alist
               (list (cons epa-regexp #'par-load-handlers-expand-candidate)))
              (par-load-handlers-expanded-target plain))
          (par-load-handlers-row
           'readable-after-expansion
           (par-load-handlers-short root (locate-file "magic" (list root) '(".el.gpg"))))
          (par-load-handlers-row
           'absolute-candidate-handler
           (par-load-handlers-short
            root (locate-file-internal (concat root "magic.el.gpg") nil '(""))))
          (let ((par-load-handlers-expanded-target gzip))
            (par-load-handlers-row
             'absolute-directory-handler
             (par-load-handlers-short root
                                      (locate-file-internal plain (list cipher) '(""))))))
        (let* ((file-name-handler-alist nil)
               (name (propertize plain 'par-load-handlers-property t))
               (result (locate-file-internal name (list root) '(""))))
          (par-load-handlers-row
           'absolute-result (list (eq result name) (text-properties-at 0 result))))
        ;; Use the real registrations' regexps with an in-process handler.
        ;; Neither test needs a passphrase, remote host or remote process.
        (let ((file-name-handler-alist (list (cons tramp-regexp #'par-load-handlers-virtual)))
              (load-path (list root "/ssh:example.invalid:/virtual/")))
          (setq par-load-handler-value nil)
          (par-load-handlers-row 'tramp-directory
                                 (list (locate-library "magic")
                                       (load "magic" nil t)
                                       par-load-handler-value
                                       (locate-library "./magic"))))
        (let* ((entry (cons epa-regexp #'par-load-handlers-virtual))
               (file-name-handler-alist (list entry))
               (load-path (list root))
               (load-suffixes '(".el" ".el.gpg"))
               (load-file-rep-suffixes '("")))
          (setq par-load-handler-value nil)
          (par-load-handlers-row 'encrypted-candidate
                                 (list (par-load-handlers-short root (locate-library "magic"))
                                       (load "magic" nil t)
                                       par-load-handler-value
                                       (locate-file "magic" load-path '(".el.gpg") 0)))
          (let ((inhibit-file-name-handlers '(par-load-handlers-virtual))
                (inhibit-file-name-operation 'file-exists-p))
            (par-load-handlers-row 'inhibited (locate-library "magic")))
          (put 'par-load-handlers-virtual 'operations '(file-readable-p))
          (par-load-handlers-row 'excluded-operation (locate-library "magic"))
          (put 'par-load-handlers-virtual 'operations nil)
          (setcar entry "\\`/x-this-prefix-does-not-exist/")
          (par-load-handlers-row 'changed-regexp (locate-library "magic"))
          (setcar entry "\\(?:字\\|\\.[[:alnum:]]+\\)\\.gpg\\'")
          (par-load-handlers-row
           'multibyte-regexp
           (par-load-handlers-short root (locate-library "magic")))))
    (delete-directory root t)))
