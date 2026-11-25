;;; par-async-process-cwd.el --- Async child directory -*- lexical-binding: t; -*-

(defun par-async-process-cwd--run (program &rest args)
  (with-temp-buffer
    (let* ((process (apply #'start-file-process "par-async-cwd" (current-buffer)
                           program args))
           (deadline (+ (float-time) 5)))
      (set-process-sentinel process #'ignore)
      (unwind-protect
          (progn
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05))
            (accept-process-output process 0.05)
            (list (process-status process) (process-exit-status process)
                  (buffer-substring-no-properties (point-min) (point-max))))
        (delete-process process)))))

(let ((directory (make-temp-file "par-async-cwd " t)))
  (unwind-protect
      (let* ((default-directory (file-name-as-directory (file-truename directory)))
             (process-environment (cons "PWD=/stale-parent-directory"
                                        process-environment)))
        (write-region "" nil "child-marker" nil 'silent)
        (dolist (process-connection-type '(nil t))
          (let ((cwd (par-async-process-cwd--run
                      "/bin/sh" "-c" "test -f child-marker && printf '%s' \"$1\""
                      "sh" "argument with spaces"))
                (pwd (par-async-process-cwd--run "/usr/bin/printenv" "-0" "PWD")))
            (prin1 (list process-connection-type cwd (car pwd) (cadr pwd)
                         (equal (nth 2 pwd)
                                (concat (directory-file-name default-directory) "\0"))))
            (terpri))))
    (delete-directory directory t)))
