;;; soak.el --- Long-lived mixed editing workload -*- lexical-binding: t; -*-

(load (expand-file-name "driver.el" (getenv "NELISP_SMOKE_SUITE")) nil t t)

(defvar smoke-soak-completed 0)
(defvar smoke-soak-errors 0)
(defvar smoke-soak-collections 0)
(defvar smoke-soak-edits 0)

(defun smoke-soak-heartbeat ()
  (let* ((sample (getenv "NELISP_SOAK_SAMPLE"))
         (pending (concat sample ".next")))
    (with-temp-buffer
      (insert (format "%s\t%d\t%d\t%d\t%d\t%d\n"
                      (if (and (boundp 'gcs-done) (integerp gcs-done))
                          gcs-done "NA")
                      smoke-soak-collections smoke-soak-completed
                      smoke-soak-errors smoke-soak-edits (truncate (float-time))))
      (write-region nil nil pending nil 'silent))
    (rename-file pending sample t)))

(defun smoke-soak-edit (buffer)
  (with-current-buffer buffer
    (dotimes (_ 64)
      (goto-char (+ (point-min) (random (1+ (buffer-size)))))
      (if (and (not (eobp))
               (or (> (buffer-size) 4096)
                   (= (random 3) 0)))
          (delete-region (point) (1+ (point)))
        (insert (aref "abc xyz\n日本語" (random (length "abc xyz\n日本語")))))
      (setq smoke-soak-edits (1+ smoke-soak-edits)))
    (unless (= (length (buffer-substring-no-properties (point-min) (point-max)))
               (buffer-size))
      (error "Buffer size changed after reading the text"))))

(defun smoke-soak-reset (scenario)
  (let ((seed (expand-file-name scenario (getenv "NELISP_SOAK_SEED")))
        (fixture (expand-file-name scenario smoke-fixtures)))
    ;; Fixtures include binary Git objects and compressed files.
    (unless (and (equal 0 (call-process "/bin/rm" nil nil nil "-rf" "--" fixture))
                 (equal 0 (call-process "/bin/cp" nil nil nil "-a" "--" seed fixture)))
      (error "Could not reset fixture %s" scenario))))

(defun smoke-soak-run ()
  (let ((cases (split-string (getenv "NELISP_SOAK_CASES") " " t))
        (stop (getenv "NELISP_SOAK_STOP"))
        (buffer (generate-new-buffer " *smoke-soak-edits*"))
        (smoke-before-step-hook '(smoke-soak-heartbeat)))
    (random "nelisp-smoke-soak")
    (smoke-soak-heartbeat)
    (while (not (file-exists-p stop))
      (dolist (scenario cases)
        (unless (file-exists-p stop)
          (condition-case fault
              (progn
                (smoke-soak-reset scenario)
                (setq smoke-soak-errors (+ smoke-soak-errors (smoke-run-case scenario))))
            ((error quit)
             (setq smoke-soak-errors (1+ smoke-soak-errors))
             (smoke-emit "ERROR fixture" (list scenario fault))))
          (setq smoke-soak-completed (1+ smoke-soak-completed))
          (smoke-soak-edit buffer)
          (smoke-soak-heartbeat)))
      (garbage-collect)
      (setq smoke-soak-collections (1+ smoke-soak-collections))
      (smoke-soak-heartbeat))
    (kill-buffer buffer)
    (smoke-emit "SOAK-DONE" (list smoke-soak-completed smoke-soak-errors
                                 smoke-soak-collections smoke-soak-edits))
    (kill-emacs 0)))

(smoke-soak-run)
