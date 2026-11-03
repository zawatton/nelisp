;;; emacs-cc-minibuf-1.el --- GNU minibuf.c primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-minibuf-1--buffer-names ()
  "Return the names of live buffers in buffer-list order."
  (let (names)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (let ((name (buffer-name buffer)))
          (when name (push name names)))))
    (nreverse names)))

(defun emacs-cc-minibuf-1--flex-match (pat str)
  "Match PAT as a subsequence of STR, returning GNU-like cost and positions."
  (unless (stringp pat) (signal 'wrong-type-argument (list 'stringp pat)))
  (unless (stringp str) (signal 'wrong-type-argument (list 'stringp str)))
  (let ((plen (length pat)) (slen (length str)))
    (when (and (> plen 0) (<= plen slen))
      (let ((positions (make-vector plen 0)) (cost 0) (last -1) (found t))
        (dotimes (i plen)
          (let ((j (1+ last)) (ch (aref pat i)))
            (while (and (< j slen)
                        (/= ch (aref str j)))
              (setq j (1+ j)))
            (if (= j slen)
                (setq found nil)
              (aset positions i j)
              (setq cost (+ cost (if (= last -1) (if (> j 0) 5 0)
                                   (if (> (- j last 1) 0)
                                       (+ 9 (- j last 1)) 0)
                                 )))
              (unless (= ch (aref str j)) (setq cost (1+ cost)))
              (setq last j))))
        (when found
          (cons cost (append positions nil)))))))

(unless (fboundp 'abort-minibuffers)
  (defun abort-minibuffers ()
    "Abort the current minibuffer."
    (if (and (fboundp 'minibufferp) (minibufferp))
        (abort-recursive-edit)
      (error "Not in a minibuffer"))))

(unless (fboundp 'completion--flex-cost-gotoh)
  (defun completion--flex-cost-gotoh (pat str)
    "Compute cost of PAT matching STR using modified Gotoh algorithm."
    (emacs-cc-minibuf-1--flex-match pat str)))

(unless (fboundp 'innermost-minibuffer-p)
  (defun innermost-minibuffer-p (&optional buffer)
    "Return t if BUFFER is the most nested active minibuffer."
    (let ((target (or buffer (current-buffer))))
      (condition-case nil
          (and (minibufferp target) (eq target (current-buffer)) t)
        (error nil)))))

(unless (fboundp 'internal-complete-buffer)
  (defun internal-complete-buffer (string predicate flag)
    "Perform completion on buffer names."
    (let ((collection (emacs-cc-minibuf-1--buffer-names)))
      (cond ((null flag) (try-completion string collection predicate))
            ((eq flag t) (all-completions string collection predicate))
            (t (test-completion string collection predicate))))))

(unless (fboundp 'minibuffer-contents-no-properties)
  (defun minibuffer-contents-no-properties ()
    "Return user input in a minibuffer, without text-properties."
    (buffer-substring-no-properties (point-min) (point-max))))

(unless (fboundp 'minibuffer-innermost-command-loop-p)
  (defun minibuffer-innermost-command-loop-p (&optional buffer)
    "Return t if BUFFER is a minibuffer at the current command loop level."
    (let ((target (or buffer (current-buffer))))
      nil)))

(unless (fboundp 'read-variable)
  (defun read-variable (prompt &optional default-value)
    "Read the name of a user option and return it as a symbol."
    (unless (stringp prompt) (signal 'wrong-type-argument (list 'stringp prompt)))
    (let* ((default (if (consp default-value) (car default-value) default-value))
           (answer (condition-case err
                       (completing-read prompt obarray
                                        (lambda (symbol)
                                          (and (custom-variable-p symbol)
                                               (stringp symbol)))
                                        t nil nil default)
                     (emacs-minibuffer-no-input
                      (signal 'end-of-file (list "Error reading from stdin"))))))
      (intern answer))))

(unless (fboundp 'set-minibuffer-window)
  (defun set-minibuffer-window (window)
    "Specify which minibuffer window to use for the minibuffer."
    (unless (windowp window) (signal 'wrong-type-argument (list 'windowp window)))
    (unless (window-minibuffer-p window)
      (signal 'error (list "Window is not a minibuffer window")))
    (setq minibuffer-window window)))

;; GNU keymap.c `apropos-internal'; eat's package chain calls it at load.
(unless (fboundp 'apropos-internal)
  (defun apropos-internal (regexp &optional predicate)
    "Show all symbols whose names contain match for REGEXP.
If optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done
for each symbol and a symbol is mentioned only if that returns non-nil.
Return list of symbols found."
    (unless (stringp regexp)
      (signal 'wrong-type-argument (list 'stringp regexp)))
    (let ((found nil))
      (mapatoms (lambda (symbol)
                  (when (and (string-match-p regexp (symbol-name symbol))
                             (or (null predicate) (funcall predicate symbol)))
                    (push symbol found))))
      (sort found #'string-lessp))))

(provide 'emacs-cc-minibuf-1)
;;; emacs-cc-minibuf-1.el ends here
