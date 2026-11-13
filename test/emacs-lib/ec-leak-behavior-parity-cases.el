;;; ec-leak-behavior-parity-cases.el --- fixed inputs for the 13 formerly leaking commands  -*- lexical-binding: t; -*-

;; Loaded by ec-leak-behavior-parity.sh on host GNU Emacs (the reference) and
;; on the standalone after the bootstrap bundle.  Prints one row per case:
;;   CASE <name> <observable>
;; where the observable is (point . buffer-string), or for font-lock the
;; `face' property at fixed positions.  Rows from both sides must be equal.

(setq print-escape-newlines t print-escape-control-characters t)
(defvar ecp--text "aaa bbb ccc.  Ddd eee.\n\nfff ggg\n")

(defun ecp--row (name thunk)
  (princ (format "CASE %s %S\n" name
                 (condition-case err (funcall thunk)
                   (error (list 'error (car err)))))))

(defun ecp--edit (name text pt form &optional setup)
  "Run FORM in a fresh buffer holding TEXT with point at PT."
  (ecp--row name
            (lambda ()
              (let ((buf (generate-new-buffer "ecp")))
                (unwind-protect
                    (with-current-buffer buf
                      (insert text)
                      (when setup (funcall setup))
                      (goto-char pt)
                      (funcall form)
                      (cons (point) (buffer-string)))
                  (kill-buffer buf))))))

(defun ecp--fill-setup () (setq-local fill-column 20))

(ecp--edit "forward-paragraph" ecp--text 5 (lambda () (forward-paragraph)))
(ecp--edit "forward-paragraph-2" ecp--text 1 (lambda () (forward-paragraph 2)))
(ecp--edit "backward-paragraph" ecp--text 28 (lambda () (backward-paragraph)))
(ecp--edit "forward-sentence" ecp--text 1 (lambda () (forward-sentence)))
(ecp--edit "backward-sentence" ecp--text 20 (lambda () (backward-sentence)))
(ecp--edit "kill-sentence" ecp--text 5 (lambda () (kill-sentence)))
(ecp--edit "backward-kill-sentence" ecp--text 20 (lambda () (backward-kill-sentence)))
(ecp--edit "fill-paragraph"
           "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm\n\nnext para\n"
           3 (lambda () (fill-paragraph)) #'ecp--fill-setup)
(ecp--edit "tab-to-tab-stop" ecp--text 5 (lambda () (tab-to-tab-stop)))
(ecp--edit "delete-backward-char" ecp--text 5 (lambda () (delete-backward-char 1)))
(ecp--edit "delete-backward-char-3" ecp--text 8 (lambda () (delete-backward-char 3)))
(ecp--edit "backward-sexp" "(a (b c) d) e" 14 (lambda () (backward-sexp)))
(ecp--edit "backward-sexp-in-list" "(a (b c) d) e" 11 (lambda () (backward-sexp 2)))

(defun ecp--font-lock (name steps)
  (ecp--row name
            (lambda ()
              (let ((buf (generate-new-buffer "ecp")) (out nil))
                (unwind-protect
                    (with-current-buffer buf
                      (insert "foo \"bar foo\" ; baz foo\nfoo\n")
                      (let ((st (make-syntax-table)))
                        (modify-syntax-entry ?\" "\"" st)
                        (modify-syntax-entry ?\; "<" st)
                        (modify-syntax-entry ?\n ">" st)
                        (set-syntax-table st))
                      (setq-local font-lock-defaults
                                  '((("foo" . font-lock-keyword-face)) nil))
                      (dolist (step steps) (funcall step))
                      (dolist (p '(1 6 15 20 25 27))
                        (push (get-text-property p 'face) out))
                      (nreverse out))
                  (kill-buffer buf))))))

(ecp--font-lock "font-lock-fontify-buffer" (list #'font-lock-fontify-buffer))
(ecp--font-lock "font-lock-unfontify-buffer"
                (list #'font-lock-fontify-buffer #'font-lock-unfontify-buffer))
;; Batch Emacs refuses to turn the mode on (`noninteractive'); both sides must
;; agree on that and on the resulting faces.
(ecp--font-lock "font-lock-mode"
                (list (lambda () (font-lock-mode 1))
                      (lambda () (princ (format "MODE %S\n" font-lock-mode)))
                      #'font-lock-fontify-buffer))
;;; ec-leak-behavior-parity-cases.el ends here
