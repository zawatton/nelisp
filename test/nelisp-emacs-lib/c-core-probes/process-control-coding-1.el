;;; process-control-coding-1.el --- Shared coding and actual child I/O -*- lexical-binding: t; -*-
(set-process-coding-system
 (let* ((output "")
        (process (make-process :name "ccore-coded-child" :command '("cat")
                               :connection-type 'pipe :coding 'utf-8-unix
                               :filter (lambda (_p text) (setq output (concat output text)))
                               :sentinel #'ignore :noquery t)))
   (unwind-protect
       (let ((initial (process-coding-system process))
             (result (set-process-coding-system process 'utf-8-unix 'utf-8-unix)))
         (process-send-string process "hello-日本語\n")
         (let ((deadline (+ (float-time) 2)))
           (while (and (not (string-match-p "日本語" output)) (< (float-time) deadline))
             (accept-process-output process 0.02)))
         (list initial result (process-coding-system process)
               (equal output "hello-日本語\n")))
     (ignore-errors (delete-process process))))
 (let ((process (if (fboundp 'emacs-process-events--make-vec)
                    (emacs-process-events--register
                     (emacs-process-events--make-vec "ccore-coded-event" -1 'pipe 'open
                                                    nil #'ignore nil nil nil nil nil))
                  (make-pipe-process :name "ccore-coded-event" :noquery t))))
   (unwind-protect
       (list (set-process-coding-system process 'utf-8-unix 'no-conversion)
             (process-coding-system process)
             (condition-case err (set-process-coding-system process 'ccore-invalid-coding)
               (error (car err)))
             (process-coding-system process))
     (ignore-errors (delete-process process))))
 (let ((before (length (process-list))))
   (list (condition-case err
             (make-process :name "ccore-invalid-child" :command '("cat")
                           :coding 'ccore-invalid-coding :noquery t)
           (error (car err)))
         (= before (length (process-list))))))
