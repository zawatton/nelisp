;;; -*- lexical-binding: t; -*-
(let ((entry (current-buffer)))
  (unwind-protect
      (progn
        (unless (eq (load (expand-file-name "test/fixtures/abi-word-load/select-buffer.el") nil t) t)
          (error "buffer-switching load result changed"))
        (unless (eq (current-buffer) entry) (error "load did not restore current buffer"))
        (unless (buffer-live-p entry) (error "entry buffer did not survive GC"))
        (princ "ABI-LOAD-BUFFER checked=3 PASS\n"))
    (set-buffer entry)
    (when (get-buffer " *abi-load-target*") (kill-buffer " *abi-load-target*"))))
