;;; par-subr-key-process.el --- GNU keymap and process helper parity -*- lexical-binding: t; -*-

(defvar par-prefix-command)
(defvar par-prefix-map)

(defun par-subr-key-process-row (label thunk)
  (prin1 (cons label (condition-case err (funcall thunk)
                      (error (list 'error (car err) (cadr err))))))
  (terpri))

(par-subr-key-process-row
 'owners
 (lambda ()
   (mapcar (lambda (name)
             (let ((file (symbol-file name 'defun)))
               (and file (equal (file-name-base file) "subr") t)))
           '(define-prefix-command suppress-keymap global-set-key
             global-key-binding local-set-key global-unset-key local-unset-key
             process-lines-handling-status process-lines process-lines-ignore-status
             call-process-shell-command))))

(par-subr-key-process-row
 'prefix-mapvar
 (lambda ()
   (let ((par-prefix-command 'original)
         (par-prefix-map nil))
     (unwind-protect
         (list (define-prefix-command 'par-prefix-command 'par-prefix-map "Prefix")
               par-prefix-command
               (eq par-prefix-map (symbol-function 'par-prefix-command))
               (keymap-prompt par-prefix-map))
       (fmakunbound 'par-prefix-command)))))

(par-subr-key-process-row
 'global-bindings
 (lambda ()
   (let ((original (current-global-map)))
     (unwind-protect
         (progn
           (use-global-map (make-sparse-keymap))
           (global-set-key [f6] 'ignore)
           (let ((before (global-key-binding [f6])))
             (global-unset-key [f6])
             (list before (global-key-binding [f6])
                   (condition-case err
                       (global-set-key '(f7) 'ignore)
                     (wrong-type-argument (cadr err))))))
       (use-global-map original)))))

(par-subr-key-process-row
 'local-bindings
 (lambda ()
   (with-temp-buffer
     (local-set-key [f6] 'ignore)
     (let ((before (lookup-key (current-local-map) [f6])))
       (local-unset-key [f6])
       (list before (lookup-key (current-local-map) [f6])
             (condition-case err
                 (local-set-key '(f7) 'ignore)
               (wrong-type-argument (cadr err))))))))

(par-subr-key-process-row
 'suppress
 (lambda ()
   (let ((map (make-sparse-keymap)))
     (suppress-keymap map)
     (list (lookup-key map [remap self-insert-command])
           (lookup-key map "-") (lookup-key map "9")))))

(par-subr-key-process-row
 'process-lines
 (lambda ()
   (let (status)
     (list (process-lines "sh" "-c" "printf 'first\\n\\nlast'")
           (process-lines-ignore-status "sh" "-c" "printf 'output\\n'; exit 6")
           (process-lines-handling-status "sh" (lambda (code) (setq status code))
                                          "-c" "printf 'handled\\n'; exit 7")
           status))))

(par-subr-key-process-row
 'shell-process
 (lambda ()
   (with-temp-buffer
     (list (call-process-shell-command "printf 'shell output'; exit 7" nil t)
           (buffer-string)))))
