;;; buffer-filename-1.el --- buffer filename ownership probes -*- lexical-binding: t; -*-

;; Preserve the original census inputs verbatim.
(buffer-file-name
 (with-temp-buffer (buffer-file-name))
 (with-temp-buffer (buffer-file-name (current-buffer)))
 (with-temp-buffer
   (setq buffer-file-name "canonical-probe.txt")
   (buffer-file-name))
 (condition-case e (buffer-file-name 42) (error e)))

(buffer-file-name
 (with-temp-buffer
   (setq buffer-file-name "canonical-probe.txt")
   (list (buffer-file-name) (buffer-file-name (current-buffer))))
 (with-temp-buffer
   (setq buffer-file-name "old-name.txt")
   (setq buffer-file-name nil)
   (buffer-file-name))
 (with-temp-buffer
   (setq buffer-file-name "local-name.txt")
   (let ((buffer-file-name "dynamic-name.txt"))
     (list (buffer-file-name) (buffer-file-name (current-buffer)))))
 (let ((old-default (default-value 'buffer-file-name)))
   (unwind-protect
       (progn
         (setq-default buffer-file-name "default-name.txt")
         (with-temp-buffer
           (list (default-value 'buffer-file-name)
                 buffer-file-name (buffer-file-name))))
     (setq-default buffer-file-name old-default)))
 (let* ((base (generate-new-buffer " *filename-base*"))
        (child (make-indirect-buffer base " *filename-child*")))
   (unwind-protect
       (progn
         (with-current-buffer base (setq buffer-file-name "base-name.txt"))
         (list (buffer-file-name base) (buffer-file-name child)))
     (when (buffer-live-p child) (kill-buffer child))
     (when (buffer-live-p base) (kill-buffer base))))
 (let ((dead (generate-new-buffer " *filename-dead*")))
   (kill-buffer dead)
   (condition-case e (buffer-file-name dead) (error e))))

;; Preserve both original lookup forms, including the empty lookup.
(get-file-buffer
 (let ((f (make-temp-file "canonical-file-")))
   (unwind-protect
       (with-temp-buffer
         (setq buffer-file-name f)
         (eq (get-file-buffer f) (current-buffer)))
     (delete-file f)))
 (let ((f (make-temp-file "canonical-file-")))
   (unwind-protect (get-file-buffer f) (delete-file f)))
 (condition-case e (get-file-buffer 42) (error e))
 (let* ((f (make-temp-file "canonical-relative-"))
        (dir (file-name-directory f))
        (name (file-name-nondirectory f))
        (b (generate-new-buffer " *filename-relative*")))
   (unwind-protect
       (progn
         (with-current-buffer b
           (setq default-directory dir)
           (set-visited-file-name name))
         (list (eq (get-file-buffer f) b)
               (with-temp-buffer
                 (setq default-directory dir)
                 (eq (get-file-buffer name) b))))
     (when (buffer-live-p b) (kill-buffer b))
     (delete-file f)))
 (let* ((f (make-temp-file "canonical-duplicate-"))
        (a (generate-new-buffer " *filename-duplicate-a*"))
        (b (generate-new-buffer " *filename-duplicate-b*")))
   (unwind-protect
       (progn
         (with-current-buffer a (setq buffer-file-name f))
         (with-current-buffer b (setq buffer-file-name f))
         (cond ((eq (get-file-buffer f) a) 'a)
               ((eq (get-file-buffer f) b) 'b)
               (t nil)))
     (when (buffer-live-p b) (kill-buffer b))
     (when (buffer-live-p a) (kill-buffer a))
     (delete-file f))))

(get-file-buffer
 (let* ((old-file (make-temp-file "canonical-old-"))
        (new-file (make-temp-file "canonical-new-"))
        (buffer (generate-new-buffer " *filename-save*")))
   (unwind-protect
       (with-current-buffer buffer
         (insert "buffer-filename-save-probe")
         (set-visited-file-name old-file)
         (let ((old-found (eq (get-file-buffer old-file) buffer)))
           (set-visited-file-name new-file)
           (let ((new-found (eq (get-file-buffer new-file) buffer))
                 (old-gone (null (get-file-buffer old-file))))
             (save-buffer)
             (let ((saved (with-temp-buffer
                            (insert-file-contents new-file)
                            (equal (buffer-string) "buffer-filename-save-probe"))))
               (setq buffer-file-name nil)
               (list old-found new-found old-gone saved
                     (buffer-file-name) (get-file-buffer new-file))))))
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (delete-file new-file)
     (delete-file old-file))))
