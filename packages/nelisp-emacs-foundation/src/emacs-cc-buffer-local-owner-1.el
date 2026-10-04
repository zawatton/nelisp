;;; emacs-cc-buffer-local-owner-1.el --- Public buffer local ownership -*- lexical-binding: t; -*-

(require 'emacs-buffer)

(defun emacs-buffer-owner--set-buffer (&rest arguments)
  "Select BUFFER through the preserved owner and swap its local cells."
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments (list 'set-buffer (length arguments))))
  (let* ((old (current-buffer))
         (new (funcall #'emacs-buffer-owner--original-set-buffer (car arguments))))
    (emacs-buffer-switch-current-buffer old new)
    new))

(defmacro emacs-buffer-owner--with-current-buffer (buffer &rest body)
  "Evaluate BODY with BUFFER selected through the shared local-cell bridge."
  (let ((old (make-symbol "owner-old")))
    (list 'let (list (list old '(current-buffer)))
          (list 'unwind-protect
                (cons 'progn (cons (list 'set-buffer buffer) body))
                (list 'when (list 'buffer-live-p old)
                      (list 'set-buffer old))))))

(defun emacs-buffer-owner--kill-buffer (&rest arguments)
  "Kill BUFFER through its preserved owner and restore fallback local cells."
  (when (> (length arguments) 1)
    (signal 'wrong-number-of-arguments (list 'kill-buffer (length arguments))))
  (let* ((old (current-buffer))
         (argument (if arguments (car arguments) old))
         (target (get-buffer (or argument old)))
         (active (eq target old))
         (symbols (when active (emacs-buffer--swap-active-symbols old nil))))
    (when active (emacs-buffer--swap-out old symbols))
    (let ((result (funcall #'emacs-buffer-owner--original-kill-buffer argument)))
      (when (and result target (not (buffer-live-p target)))
        (when active
          ;; The standalone owner can leave no buffer selected after killing
          ;; its current object. Select a surviving public buffer first.
          (unless (current-buffer)
            (funcall #'emacs-buffer-owner--original-set-buffer
                     (or (car (buffer-list)) (get-buffer-create "*scratch*"))))
          (emacs-buffer--swap-in
           (current-buffer)
           (emacs-buffer--swap-active-symbols old (current-buffer))))
        (emacs-buffer--forget target))
      result)))

(defmacro emacs-buffer-owner--save-current-buffer (&rest body)
  "Evaluate BODY and restore the live original buffer and its local cells."
  (let ((old (make-symbol "owner-old")))
    (list 'let (list (list old '(current-buffer)))
          (list 'unwind-protect (cons 'progn body)
                (list 'when (list 'buffer-live-p old)
                      (list 'set-buffer old))))))

(defmacro emacs-buffer-owner--save-excursion (&rest body)
  "Restore the public buffer, its local cells and a moving point marker."
  (let ((old (make-symbol "owner-excursion-buffer"))
        (marker (make-symbol "owner-excursion-marker")))
    (list 'let (list (list old '(current-buffer))
                     (list marker '(point-marker)))
          (list 'unwind-protect (cons 'progn body)
                (list 'unwind-protect
                      (list 'when (list 'buffer-live-p old)
                            (list 'set-buffer old)
                            (list 'goto-char (list 'marker-position marker)))
                      (list 'set-marker marker nil))))))

(defmacro emacs-buffer-owner--setq-default (&rest pairs)
  "Set each default through the shared owner without changing a local cell."
  (let ((forms nil))
    (while pairs
      (let ((variable (pop pairs)) (value (pop pairs)))
        (push (list 'emacs-buffer-setq-default-1 (list 'quote variable) value) forms)))
    (cons 'progn (nreverse forms))))

(defmacro emacs-buffer-owner--with-temp-buffer (&rest body)
  "Evaluate BODY in a fresh public buffer and remove its local-cell sidecar."
  (let ((temporary (make-symbol "owner-temp")))
    ;; Construct the expansion directly.  Backquote walks this fixed template
    ;; at every interpreted invocation, before the buffer work even starts.
    (list 'let (list (list temporary '(generate-new-buffer " *temp*")))
          (list 'unwind-protect
                (cons 'with-current-buffer (cons temporary body))
                (list 'kill-buffer temporary)
                (list 'emacs-buffer--forget temporary)))))

(when (fboundp 'nelisp--repr)
  (unless (fboundp 'emacs-buffer-owner--original-set-buffer)
    (fset 'emacs-buffer-owner--original-set-buffer (symbol-function 'set-buffer)))
  (unless (fboundp 'emacs-buffer-owner--original-kill-buffer)
    (fset 'emacs-buffer-owner--original-kill-buffer (symbol-function 'kill-buffer)))
  (fset 'set-buffer (symbol-function 'emacs-buffer-owner--set-buffer))
  (fset 'kill-buffer (symbol-function 'emacs-buffer-owner--kill-buffer))
  (fset 'with-current-buffer (symbol-function 'emacs-buffer-owner--with-current-buffer))
  (fset 'save-current-buffer (symbol-function 'emacs-buffer-owner--save-current-buffer))
  (fset 'save-excursion (symbol-function 'emacs-buffer-owner--save-excursion))
  (fset 'setq-default (symbol-function 'emacs-buffer-owner--setq-default))
  (fset 'with-temp-buffer (symbol-function 'emacs-buffer-owner--with-temp-buffer)))

(provide 'emacs-cc-buffer-local-owner-1)
;;; emacs-cc-buffer-local-owner-1.el ends here
