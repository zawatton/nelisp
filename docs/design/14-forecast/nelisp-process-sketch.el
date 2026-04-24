;;; nelisp-process-sketch.el --- Phase 5-C.1 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct nelisp-process
  id
  host-proc       ; underlying host process object
  actor           ; owning nelisp-actor
  (status 'idle)  ; 'spawning / 'alive / 'dead / 'crashed
  (exit-code nil)
  (buffer nil)
  (props nil))

(defvar nelisp-process--registry nil
  "All live NeLisp process wrappers.")

(defun nelisp-make-process (&rest args)
  "Wrap host `make-process' with per-process actor plumbing."
  (let* ((name (or (plist-get args :name) "nelisp-proc"))
         (command (plist-get args :command))
         (connection-type (or (plist-get args :connection-type) 'pipe))
         (coding (plist-get args :coding))
         (host-sentinel (plist-get args :sentinel))
         (host-filter (plist-get args :filter))
         (host-proc
          (make-process
           :name name
           :command command
           :connection-type connection-type
           :coding (or coding 'utf-8)
           :sentinel (lambda (p ev)
                       (when host-sentinel (funcall host-sentinel p ev)))
           :filter (lambda (p s)
                     (when host-filter (funcall host-filter p s)))))
         (proc (make-nelisp-process :id name :host-proc host-proc
                                    :status 'alive)))
    (push proc nelisp-process--registry)
    proc))

(defun nelisp-process-send-string (proc s)
  (process-send-string (nelisp-process-host-proc proc) s))

(defun nelisp-process-send-eof (proc)
  (process-send-eof (nelisp-process-host-proc proc)))

(defun nelisp-process-live-p (proc)
  (process-live-p (nelisp-process-host-proc proc)))

(defun nelisp-process-status (proc)
  (process-status (nelisp-process-host-proc proc)))

(defun nelisp-process-exit-status (proc)
  (process-exit-status (nelisp-process-host-proc proc)))

(defun nelisp-kill-process (proc)
  (kill-process (nelisp-process-host-proc proc)))

(defun nelisp-delete-process (proc)
  (delete-process (nelisp-process-host-proc proc))
  (setq nelisp-process--registry (delq proc nelisp-process--registry)))

(defun nelisp-process-list ()
  (copy-sequence nelisp-process--registry))

(defun nelisp-process-get (proc key)
  (plist-get (nelisp-process-props proc) key))

(defun nelisp-process-put (proc key value)
  (setf (nelisp-process-props proc)
        (plist-put (nelisp-process-props proc) key value)))

(defun nelisp-process-accept-output (proc &optional seconds)
  (accept-process-output (nelisp-process-host-proc proc) seconds))

(defun nelisp-process-id (proc)
  (process-id (nelisp-process-host-proc proc)))

(defun nelisp-process-name (proc)
  (process-name (nelisp-process-host-proc proc)))

(defun nelisp-process-command (proc)
  (process-command (nelisp-process-host-proc proc)))

(defun nelisp-processp (obj)
  (and obj (nelisp-process-p obj)))

(provide 'nelisp-process-sketch)
;;; nelisp-process-sketch.el ends here
