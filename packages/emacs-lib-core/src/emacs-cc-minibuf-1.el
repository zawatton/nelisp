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

;; minibuf.c DEFVAR_LISP: GNU `read-passwd' (auth-source.el) binds it.
(unless (boundp 'read-hide-char)
  (defvar read-hide-char nil
    "Whether to hide input characters in noninteractive mode."))

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

(defun emacs-cc-minibuf-1--innermost-p (buffer)
  "Return t if BUFFER (default current) is the most nested active minibuffer.
This is minibuf.c's notion: the minibuffer stack lives in
`emacs-minibuffer--buffers', innermost first."
  (let ((target (or buffer (current-buffer))))
    (and (bufferp target)
         (boundp 'emacs-minibuffer--buffers)
         (eq target (car emacs-minibuffer--buffers))
         t)))

(unless (fboundp 'innermost-minibuffer-p)
  (defun innermost-minibuffer-p (&optional buffer)
    "Return t if BUFFER is the most nested active minibuffer."
    (emacs-cc-minibuf-1--innermost-p buffer)))

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
    (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))

(unless (fboundp 'minibuffer-innermost-command-loop-p)
  (defun minibuffer-innermost-command-loop-p (&optional buffer)
    "Return t if BUFFER is a minibuffer at the current command loop level.
Minibuffers here never start a nested command loop, so this is the
innermost-minibuffer test."
    (emacs-cc-minibuf-1--innermost-p buffer)))

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

;; Variables GNU defines in minibuf.c (DEFVAR_*), read by the genuine GNU
;; minibuffer.el.  Defaults match GNU Emacs 31.1.
(defvar minibuffer-completing-file-name nil
  "Non-nil means completing file names.")
(defvar minibuffer-allow-text-properties nil
  "Non-nil means `read-from-minibuffer' should not discard text properties.")
(defvar minibuffer-help-form nil
  "Value that `help-form' takes on inside the minibuffer.")
(defvar history-add-new-input t
  "Non-nil means to add new elements in history.")
(defvar read-buffer-function nil
  "If this is non-nil, `read-buffer' does its work by calling this function.")
(defvar read-buffer-completion-ignore-case nil
  "Non-nil means completion ignores case when reading a buffer name.")
(defvar read-expression-history nil
  "A history list for arguments that are Lisp expressions to evaluate.")
(defvar minibuffer-follows-selected-frame t
  "t means the active minibuffer always displays on the selected frame.")
(defvar read-minibuffer-restore-windows t
  "Non-nil means restore window configurations on exit from minibuffer.")

;; keyboard.c DEFVARs read by `minibuffer-setup-on-screen-keyboard'.
(defvar last-event-frame nil
  "The frame on which the last input event was received.")
(defvar last-event-device nil
  "The name of the input device that generated the last event.")

(provide 'emacs-cc-minibuf-1)
;;; emacs-cc-minibuf-1.el ends here
