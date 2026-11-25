;;; completion.el --- Completing-read and minibuffer history -*- lexical-binding: t; -*-

(defvar smoke-completion-history nil)
(defvar smoke-completion-result nil)

(defun smoke-completion-read ()
  (interactive)
  (setq smoke-completion-result
        (completing-read "Pick: " '("alpha beta" "alpha gamma" "delta")
                         nil t nil 'smoke-completion-history)))

(defun smoke-completion ()
  (let ((smoke-completion-result nil)
        (smoke-completion-history nil)
        (completion-styles '(basic))
        (completion-category-overrides nil)
        (completion-category-defaults nil)
        (backend 'default))
    (when (and (getenv "NELISP_SMOKE_VERTICO") (getenv "NELISP_SMOKE_ORDERLESS"))
      (setq backend
            (smoke-step "completion-packages"
              (require 'vertico)
              (require 'orderless)
              (setq completion-styles '(orderless))
              (vertico-mode 1)
              'vertico/orderless)))
    (unwind-protect
        (progn
          (smoke-step "completion-candidates"
            (list backend
                  (substring-no-properties
                   (car (completion-all-completions
                         (if (eq backend 'vertico/orderless) "bet alp" "alpha b")
                         '("alpha beta" "alpha gamma" "delta") nil
                         7)))))
          (smoke-step "completing-read"
            (let ((map (copy-keymap (current-global-map)))
                  (noninteractive nil))
              (define-key map [f8] #'smoke-completion-read)
              (let ((previous (current-global-map)))
                (unwind-protect
                    (progn
                      (use-global-map map)
                      (execute-kbd-macro (kbd "<f8> d e l t a RET"))
                      (unless (equal smoke-completion-result "delta")
                        (error "First completion returned %S" smoke-completion-result))
                      (setq smoke-completion-result nil)
                      (execute-kbd-macro (kbd "<f8> M-p RET"))
                      (list smoke-completion-result smoke-completion-history))
                  (use-global-map previous))))))
      (when (and (boundp 'vertico-mode) vertico-mode)
        (vertico-mode -1)))))
