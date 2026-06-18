;;; emacs-calc.el --- minimal RPN calculator  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) calculator: Calc's signature interface is the RPN
;; (reverse-Polish) stack, and that is what this minimal greenfield provides --
;; a `*Calculator*' buffer over a number stack, the four arithmetic operators
;; that pop two operands and push the result, and an `emacs-calc-eval' RPN
;; string evaluator.  The full Calc (algebraic mode, units, matrices, ...) is a
;; later target; this is the daily-driver "quick stack calculator" slice.
;;
;; Pure Elisp -- no subprocess -- so it runs unchanged on the standalone reader.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the stack, the operators, the eval, and the buffer.
;;   - nelisp-gui OWNS: rendering + key transport.

;;; Code:

(defvar emacs-calc-buffer-name "*Calculator*"
  "Buffer name used by `emacs-calc'.")

(defvar emacs-calc--stack nil
  "The calculator stack; the car is the top of the stack.")

(defconst emacs-calc--operators
  '(("+" . +) ("-" . -) ("*" . *) ("/" . /))
  "Mapping of operator tokens to their Elisp functions.")

;;;; --- RPN string evaluator (pure, no buffer/global state) ----------

(defun emacs-calc-eval (string)
  "Evaluate STRING as a space-separated RPN expression; return the result.
Tokens are numbers or one of + - * /.  Returns the final top of the
working stack (nil for an empty expression)."
  (let ((stack nil))
    (dolist (token (split-string (or string "")))
      (let ((op (assoc token emacs-calc--operators)))
        (if op
            (let ((b (pop stack))
                  (a (pop stack)))
              (push (funcall (cdr op) (or a 0) (or b 0)) stack))
          (push (string-to-number token) stack))))
    (car stack)))

;;;; --- stack + operators (buffer-backed) ----------------------------

(defun emacs-calc--render ()
  "Render the stack into the `*Calculator*' buffer (top shown last, as `1:')."
  (let ((buf (get-buffer emacs-calc-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (i (length emacs-calc--stack)))
          (erase-buffer)
          (insert "--- Emacs Calculator ---\n")
          (dolist (v (reverse emacs-calc--stack))
            (insert (format "%d:  %s\n" i v))
            (setq i (1- i)))
          (goto-char (point-max)))))))

(defun emacs-calc-push (n)
  "Push number N onto the stack and refresh the display.  Returns N."
  (push n emacs-calc--stack)
  (emacs-calc--render)
  n)

(defun emacs-calc-pop ()
  "Pop and return the top of the stack, refreshing the display."
  (prog1 (pop emacs-calc--stack)
    (emacs-calc--render)))

(defun emacs-calc-top ()
  "Return the top of the stack without removing it."
  (car emacs-calc--stack))

(defun emacs-calc--binop (fn)
  "Pop two operands, push (FN A B), and refresh the display."
  (when (< (length emacs-calc--stack) 2)
    (error "emacs-calc: stack underflow"))
  (let ((b (pop emacs-calc--stack))
        (a (pop emacs-calc--stack)))
    (emacs-calc-push (funcall fn a b))))

(defun emacs-calc-plus () "Replace the top two numbers with their sum."
  (interactive) (emacs-calc--binop #'+))
(defun emacs-calc-minus () "Replace the top two numbers with their difference."
  (interactive) (emacs-calc--binop #'-))
(defun emacs-calc-times () "Replace the top two numbers with their product."
  (interactive) (emacs-calc--binop #'*))
(defun emacs-calc-divide () "Replace the top two numbers with their quotient."
  (interactive) (emacs-calc--binop #'/))

(defun emacs-calc-reset ()
  "Clear the calculator stack."
  (interactive)
  (setq emacs-calc--stack nil)
  (emacs-calc--render)
  nil)

;;;; --- mode + entry point -------------------------------------------

(defun emacs-calc-mode ()
  "Major mode for the RPN calculator buffer."
  (interactive)
  (when (fboundp 'kill-all-local-variables)
    (kill-all-local-variables))
  (setq major-mode 'calc-mode
        mode-name "Calculator")
  nil)

(defun emacs-calc ()
  "Open or switch to the `*Calculator*' buffer; return it."
  (interactive)
  (let ((buf (get-buffer-create emacs-calc-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'calc-mode)
        (emacs-calc-mode))
      (emacs-calc--render))
    (if (fboundp 'switch-to-buffer)
        (switch-to-buffer buf)
      (set-buffer buf))
    buf))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-calc-install ()
  "Bind the standard Calc command names to the `emacs-calc' implementations.
Not run on `require' (keeps a bare load from touching shared symbols)."
  (defalias 'calc #'emacs-calc)
  (defalias 'calc-mode #'emacs-calc-mode)
  (defalias 'calc-eval #'emacs-calc-eval))

(provide 'emacs-calc)

;;; emacs-calc.el ends here
