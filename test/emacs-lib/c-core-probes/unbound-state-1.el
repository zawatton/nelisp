;;; -*- lexical-binding: t; -*-
(lossage-size
 (lossage-size)
 (let ((old (lossage-size)))
   (unwind-protect (list (lossage-size 101) (lossage-size))
     (lossage-size old)))
 (lossage-size 99)
 (lossage-size -1)
 (lossage-size t))
(set-input-interrupt-mode
 (set-input-interrupt-mode t)
 (set-input-interrupt-mode nil))
(set-input-meta-mode
 (set-input-meta-mode t)
 (set-input-meta-mode 'encoded)
 (set-input-meta-mode nil 3))
(set-output-flow-control
 (set-output-flow-control t)
 (set-output-flow-control nil 3))
(set-quit-char
 (set-quit-char 3)
 (set-quit-char "ignored in batch"))
(waiting-for-user-input-p
 (waiting-for-user-input-p))
(set--this-command-keys
 (progn (set--this-command-keys "xy")
        (prog1 (list (this-command-keys) (this-command-keys-vector))
          (set--this-command-keys "")))
 (set--this-command-keys [1 2]))
(insert-special-event
 (insert-special-event 'foo)
 (insert-special-event '(unknown-event argument)))
(indirect-variable
 (indirect-variable 12)
 (indirect-variable 'unaliased-variable)
 (let ((target (make-symbol "target")) (alias (make-symbol "alias")))
   (set target 12)
   (funcall 'defvaralias alias target)
   (unwind-protect (eq (indirect-variable alias) target)
     (internal-delete-indirect-variable alias))))
(internal-delete-indirect-variable
 (internal-delete-indirect-variable 3)
 (let ((target (make-symbol "target")) (alias (make-symbol "alias")))
   (set target 12)
   (funcall 'defvaralias alias target)
   (list (eq (internal-delete-indirect-variable alias) alias)
         (eq (indirect-variable alias) alias) (boundp alias)
         (symbol-value target))))
(lread--substitute-object-in-subtree
 (let* ((placeholder (make-symbol "placeholder"))
        (root (list 1 placeholder)))
   (list (lread--substitute-object-in-subtree root placeholder t)
         (eq (cadr root) root) (car root)))
 (let* ((placeholder (make-symbol "placeholder"))
        (root (vector (cons placeholder placeholder))))
   (lread--substitute-object-in-subtree root placeholder
                                       (make-hash-table :test 'eq))
   (list (eq (car (aref root 0)) root)
         (eq (cdr (aref root 0)) root))))
(tool-bar-pixel-width
 (tool-bar-pixel-width)
 (tool-bar-pixel-width (selected-frame))
 (tool-bar-pixel-width 3))
(capitalize-region
 (with-temp-buffer
   (insert "hELLO wORLD foo-bar 12abc")
   (let ((position (point)))
     (list (capitalize-region 2 8) (buffer-string) (= (point) position))))
 (with-temp-buffer
   (insert "abCD efGH")
   (capitalize-region 9 1)
   (buffer-string))
 (with-temp-buffer
   (insert "ABC DEF")
   (put-text-property 1 4 'sample t)
   (capitalize-region 1 8)
   (list (buffer-substring-no-properties 1 8) (get-text-property 2 'sample))))
(upcase-initials-region
 (with-temp-buffer
   (insert "hELLO wORLD foo-bar 12abc")
   (list (upcase-initials-region 2 8) (buffer-string)))
 (with-temp-buffer
   (insert "abCD efGH")
   (upcase-initials-region 9 1)
   (buffer-string)))
