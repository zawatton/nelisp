;;; emacs-parity-shims-test.el --- ERT tests for T87 emacs-parity-shims additions  -*- lexical-binding: t; -*-

;;; Commentary:

;; `src/emacs-parity-shims.el' guards every definition with `unless
;; (fboundp ...)', so under host Emacs -- which already provides real
;; `subr.el'/`window.el' implementations of every macro this file
;; covers -- the guarded `defmacro' forms never install: there is
;; nothing for host ERT to shadow or call directly.
;;
;; To still exercise the exact ported code (not host's own
;; definition), each test reads the `(defmacro NAME ...)' form
;; straight out of the source file, `eval's it under a private
;; uninterned name, and checks the resulting macro's expansion shape
;; and/or runtime behavior.  Expected shapes were captured from a
;; clean `emacs -Q --batch' (Emacs 31.1) during T87 (see the parallel
;; commentary in `src/emacs-parity-shims.el').

;;; Code:

(require 'ert)
(require 'emacs-parity-shims)

(defun emacs-parity-shims-test--file ()
  (let* ((file (locate-library "emacs-parity-shims"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (substring file 0 (- (length file) 1))
                 file)))
    (should (and file (file-exists-p file)))
    file))

(defun emacs-parity-shims-test--extract-defmacro (name)
  "Read the `(defmacro NAME ...)' form out of emacs-parity-shims.el,
renamed to a private uninterned symbol.  Returns (RENAMED-SYMBOL . FORM)."
  (with-temp-buffer
    (insert-file-contents (emacs-parity-shims-test--file))
    (goto-char (point-min))
    (should (re-search-forward (format "(defmacro %s " (regexp-quote name)) nil t))
    (goto-char (match-beginning 0))
    (let* ((start (point))
           (end (progn (forward-sexp) (point)))
           (form (read (buffer-substring-no-properties start end)))
           (renamed (make-symbol (format "emacs-parity-shims-test--%s" name))))
      (setcar (cdr form) renamed)
      (cons renamed form))))

;;;; A. static presence -- both guards exist in source

(ert-deftest emacs-parity-shims-test/t87-guards-present-in-source ()
  (with-temp-buffer
    (insert-file-contents (emacs-parity-shims-test--file))
    (dolist (sym '(save-selected-window with-current-buffer-window))
      (goto-char (point-min))
      (should (search-forward
               (format "(unless (fboundp '%s)" sym)
               nil t)))))

;;;; B. `save-selected-window' -- macroexpansion shape parity with host

(ert-deftest emacs-parity-shims-test/save-selected-window-macroexpansion-shape ()
  (let* ((extracted (emacs-parity-shims-test--extract-defmacro "save-selected-window"))
         (sym (car extracted)))
    (eval (cdr extracted) t)
    (let* ((expansion (macroexpand-1 (list sym '(foo) '(bar))))
           (wsym (cadr (car (cadr expansion)))))
      ;; (let ((W (selected-window)))
      ;;   (save-current-buffer
      ;;     (unwind-protect (progn (foo) (bar))
      ;;       (when (window-live-p W) (select-window W 'norecord)))))
      (should (eq 'let (car expansion)))
      (should (equal '(selected-window) wsym))
      (let ((scb (car (cddr expansion))))
        (should (eq 'save-current-buffer (car scb)))
        (let ((up (cadr scb))
              (w (car (car (cadr expansion)))))
          (should (eq 'unwind-protect (car up)))
          (should (equal '(progn (foo) (bar)) (nth 1 up)))
          (should (equal (list 'when (list 'window-live-p w)
                               (list 'select-window w ''norecord))
                         (nth 2 up))))))))

;;;; C. `save-selected-window' -- behavior: restores selection + return value

(ert-deftest emacs-parity-shims-test/save-selected-window-restores-and-returns ()
  (let* ((extracted (emacs-parity-shims-test--extract-defmacro "save-selected-window"))
         (sym (car extracted)))
    (eval (cdr extracted) t)
    (save-window-excursion
      (delete-other-windows)
      (split-window)
      (let* ((w0 (selected-window))
             (probe (eval `(lambda ()
                             (,sym (other-window 1) 'probe-value))
                          t)))
        (should (eq 'probe-value (funcall probe)))
        (should (eq w0 (selected-window)))))))

;;;; D. `with-current-buffer-window' -- macroexpansion shape parity with host

(ert-deftest emacs-parity-shims-test/with-current-buffer-window-macroexpansion-shape ()
  (let* ((extracted (emacs-parity-shims-test--extract-defmacro "with-current-buffer-window"))
         (sym (car extracted)))
    (eval (cdr extracted) t)
    (let* ((expansion (macroexpand-1 (list sym "*t87-cbw-shape*" nil nil '(foo)))))
      ;; (let* ((BUF (temp-buffer-window-setup "*t87-cbw-shape*"))
      ;;        (standard-output BUF) WIN VAL)
      ;;   (with-current-buffer BUF
      ;;     (setq VAL (progn (foo)))
      ;;     (setq WIN (temp-buffer-window-show BUF nil)))
      ;;   (if (functionp nil) (funcall nil WIN VAL) VAL))
      (should (eq 'let* (car expansion)))
      (let* ((bindings (cadr expansion))
             (buffer-sym (car (nth 0 bindings)))
             (window-sym (nth 2 bindings))
             (value-sym (nth 3 bindings))
             (body (cddr expansion)))
        (should (equal (list 'temp-buffer-window-setup "*t87-cbw-shape*")
                       (cadr (nth 0 bindings))))
        (should (equal (list 'standard-output buffer-sym) (nth 1 bindings)))
        (should (symbolp window-sym))
        (should (symbolp value-sym))
        (should (equal (list 'with-current-buffer buffer-sym
                             (list 'setq value-sym '(progn (foo)))
                             (list 'setq window-sym
                                   (list 'temp-buffer-window-show buffer-sym nil)))
                       (nth 0 body)))
        (should (equal (list 'if '(functionp nil)
                             (list 'funcall nil window-sym value-sym)
                             value-sym)
                       (nth 1 body)))))))

;;;; E. `with-current-buffer-window' -- behavior: runs BODY in buffer,
;;;;    returns its value, buffer holds inserted content.

(ert-deftest emacs-parity-shims-test/with-current-buffer-window-behavior ()
  (let* ((extracted (emacs-parity-shims-test--extract-defmacro "with-current-buffer-window"))
         (sym (car extracted)))
    (eval (cdr extracted) t)
    (unwind-protect
        (let* ((probe (eval `(lambda ()
                                (,sym "*t87-cbw-behavior*" nil nil
                                      (insert "hello-t87")
                                      (buffer-name)))
                             t)))
          (should (equal "*t87-cbw-behavior*" (funcall probe)))
          (should (equal "hello-t87"
                         (with-current-buffer "*t87-cbw-behavior*" (buffer-string)))))
      (when (get-buffer "*t87-cbw-behavior*")
        (kill-buffer "*t87-cbw-behavior*")))))

(provide 'emacs-parity-shims-test)

;;; emacs-parity-shims-test.el ends here
