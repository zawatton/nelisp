;;; corpus-buffer-dir.el --- new buffers inherit default-directory (GNU 31.1)  -*- lexical-binding: t; -*-
;; Run with tools/ai/cl-gnu/run-corpus.el.
(let ((x (get-buffer-create " bd1"))) (with-current-buffer x (set (make-local-variable 'default-directory) "/tmp/") (with-temp-buffer default-directory)))
(let ((x (get-buffer-create " bd2"))) (with-current-buffer x (setq default-directory "/var/") (with-current-buffer (generate-new-buffer " bd2b") default-directory)))
(let ((x (get-buffer-create " bd3"))) (with-current-buffer x (setq default-directory "/usr/") (with-current-buffer (get-buffer-create " bd3-new") default-directory)))
(let ((x (get-buffer-create " bd4"))) (with-current-buffer x (setq default-directory "/etc/") (let ((y (get-buffer-create " bd4-new"))) (setq default-directory "/opt/") (buffer-local-value 'default-directory y))))
(let ((x (get-buffer-create " bd5"))) (with-current-buffer x (setq default-directory "/srv/") (with-temp-buffer (list (local-variable-p 'default-directory) default-directory))))
(let ((x (get-buffer-create " bd6"))) (with-current-buffer x (setq default-directory "/mnt/") (with-temp-buffer (with-temp-buffer default-directory))))
(let ((x (get-buffer-create " bd7"))) (with-current-buffer x (setq default-directory "/home/") (with-temp-buffer (setq default-directory "/tmp/z/") (buffer-local-value 'default-directory x))))
(let ((d (default-value 'default-directory)) (x (get-buffer-create " bd8"))) (with-current-buffer x (setq default-directory "/root/") (with-temp-buffer (equal (default-value 'default-directory) d))))
(local-variable-if-set-p 'default-directory)
(with-temp-buffer (local-variable-p 'default-directory))
;;; corpus-buffer-dir.el ends here
