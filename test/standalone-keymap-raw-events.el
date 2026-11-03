;;; standalone-keymap-raw-events.el --- Raw key event regression -*- lexical-binding: t; -*-
;; Run in GNU Emacs or the standalone runtime after its keymap provider loads.
;; This focused component check does not establish package acceptance.
(let ((map (make-sparse-keymap)) results)
  (define-key map (kbd "TAB") 'tab-command)
  (define-key map (kbd "RET") 'ret-command)
  (push (lookup-key map "\t") results)
  (push (lookup-key map "\r") results)
  (push (lookup-key map [9]) results)
  (define-key map "T" (make-sparse-keymap))
  (define-key map "TA" (make-sparse-keymap))
  (define-key map "TAB" 'literal-command)
  (push (lookup-key map [84 65 66]) results)
  (push (lookup-key map [9]) results)
  (define-key map (unibyte-string 200) 'meta-command)
  (push (lookup-key map (unibyte-string 200)) results)
  (push (lookup-key map [134217800]) results)
  (push (lookup-key map (kbd "M-H")) results)
  (define-key map (string 200) 'multibyte-command)
  (push (lookup-key map [200]) results)
  (push (lookup-key map [134217800]) results)
  (dolist (operation '(define lookup))
    (push (condition-case err
              (progn (if (eq operation 'define)
                         (define-key map '(9) 'invalid)
                       (lookup-key map '(9)))
                     'unexpected-success)
            (wrong-type-argument (list (car err) (cadr err)))) results))
  (setq results (nreverse results))
  (unless (equal results
                 '(tab-command ret-command tab-command literal-command
                   tab-command meta-command meta-command meta-command
                   multibyte-command meta-command
                   (wrong-type-argument arrayp)
                   (wrong-type-argument arrayp)))
    (error "Raw key event regression: %S" results))
  (princ "KEYMAP-RAW-CONTROL-META-PASS\n"))
