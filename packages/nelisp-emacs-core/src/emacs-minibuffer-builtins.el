;;; emacs-minibuffer-builtins.el --- Unprefixed minibuffer.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track C (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* minibuffer + completion
;; builtins (= `read-from-minibuffer', `read-string', `completing-read',
;; `yes-or-no-p', `y-or-n-p', `read-number', ...) to the existing
;; `emacs-minibuffer-*' prefixed implementation in `emacs-minibuffer.el',
;; mirroring the Phase 11.C'' / J / L1 / D bridge pattern.
;;
;; Why this exists: until Track C the unprefixed names were nil-stubs in
;; `emacs-stub-bulk.el', so standalone NeLisp callers like
;; `(read-string "Filename: ")' silently returned nil even though
;; `emacs-minibuffer.el' provides a full pluggable reader (=
;; `emacs-minibuffer-feed-input' lets tests / future command-loop
;; inject deterministic input).
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with the real minibuffer substrate.  Variables are still gated
;; on `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridgeable today (= covered by `emacs-minibuffer.el'):
;;
;;   - read-from-minibuffer / read-string / read-no-blanks-input
;;   - read-key / read-buffer / read-file-name / read-directory-name
;;   - read-passwd / read-number
;;   - y-or-n-p / yes-or-no-p
;;   - completing-read
;;   - minibufferp / active-minibuffer-window / minibuffer-window
;;   - minibuffer-prompt / minibuffer-contents
;;   - minibuffer-prompt-end / minibuffer-prompt-width
;;   - exit-minibuffer / abort-recursive-edit / minibuffer-message
;;
;; Plus history defvars: `minibuffer-history' / `command-history' /
;; `file-name-history' / `read-string-history' / `buffer-name-history' /
;; `regexp-history' / `extended-command-history'.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- minibuffer reads go through the nemacs minibuffer.
;;; Code:

(require 'emacs-minibuffer)

;;;; --- core readers ----------------------------------------------------

(defun emacs-minibuffer-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge.
The NeLisp reader binds `emacs-version', so use its private stdout
primitive to distinguish standalone execution from host Emacs."
  (or (fboundp 'nelisp--write-stdout-bytes)
      (get symbol 'emacs-stub-bulk)
      (not (fboundp symbol))))

(when (emacs-minibuffer-builtins--install-function-p 'read-from-minibuffer)
  (defalias 'read-from-minibuffer #'emacs-minibuffer-read-from-minibuffer))

(when (emacs-minibuffer-builtins--install-function-p 'read-string)
  (defalias 'read-string #'emacs-minibuffer-read-string))

(when (emacs-minibuffer-builtins--install-function-p 'read-no-blanks-input)
  (defalias 'read-no-blanks-input #'emacs-minibuffer-read-no-blanks-input))

(when (emacs-minibuffer-builtins--install-function-p 'read-key)
  (defalias 'read-key #'emacs-minibuffer-read-key))

;;;; --- typed readers ---------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'read-buffer)
  (defalias 'read-buffer #'emacs-minibuffer-read-buffer))

(when (emacs-minibuffer-builtins--install-function-p 'read-file-name)
  (defalias 'read-file-name #'emacs-minibuffer-read-file-name))

(when (emacs-minibuffer-builtins--install-function-p 'read-directory-name)
  (defalias 'read-directory-name #'emacs-minibuffer-read-directory-name))

(when (emacs-minibuffer-builtins--install-function-p 'read-passwd)
  (defalias 'read-passwd #'emacs-minibuffer-read-passwd))

(when (emacs-minibuffer-builtins--install-function-p 'read-number)
  (defalias 'read-number #'emacs-minibuffer-read-number))

;;;; --- confirmation ---------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'y-or-n-p)
  (defalias 'y-or-n-p #'emacs-minibuffer-y-or-n-p))

(when (emacs-minibuffer-builtins--install-function-p 'yes-or-no-p)
  (defalias 'yes-or-no-p #'emacs-minibuffer-yes-or-no-p))

;;;; --- completion -----------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'completing-read)
  (defalias 'completing-read #'emacs-minibuffer-completing-read))

(defun emacs-minibuffer-builtins--check-arity (name arguments minimum maximum)
  "Check the number of ARGUMENTS against NAME's primitive arity."
  (let ((count (length arguments)))
    (unless (and (>= count minimum) (<= count maximum))
      (signal 'wrong-number-of-arguments (list name count)))))

(defun emacs-minibuffer-builtins--completion-name (item)
  "Return ITEM's string or symbol key as a string, ignoring other keys."
  (let ((key (if (consp item) (car item) item)))
    (cond ((stringp key) key)
          ((symbolp key) (symbol-name key)))))

(defun emacs-minibuffer-builtins--completion-match-p (string candidate)
  "Check CANDIDATE's prefix and the completion regular expressions."
  (let ((ignore-case (and (boundp 'completion-ignore-case)
                          completion-ignore-case)))
    (and (>= (length candidate) (length string))
         (eq t (compare-strings string 0 nil candidate 0 (length string)
                                ignore-case))
         (let ((patterns (and (boundp 'completion-regexp-list)
                              completion-regexp-list))
               (case-fold-search ignore-case)
               (accepted t))
           (while (and patterns accepted)
             (unless (string-match-p (car patterns) candidate)
               (setq accepted nil))
             (setq patterns (cdr patterns)))
           accepted))))

(defun emacs-minibuffer-builtins--completion-candidates (string collection predicate)
  "Collect matching names, passing original entries to PREDICATE."
  (let (matches)
    (cond
     ((hash-table-p collection)
      (maphash
       (lambda (key value)
         (let ((name (and (or (stringp key) (symbolp key))
                          (emacs-minibuffer-builtins--completion-name key))))
           (when (and name
                      (emacs-minibuffer-builtins--completion-match-p string name)
                      (or (null predicate) (funcall predicate key value)))
             (setq matches (cons name matches)))))
       collection))
     ((obarrayp collection)
      (mapatoms
       (lambda (symbol)
         (let ((name (symbol-name symbol)))
           (when (and (emacs-minibuffer-builtins--completion-match-p string name)
                      (or (null predicate) (funcall predicate symbol)))
             (setq matches (cons name matches)))))
       collection))
     ((listp collection)
      ;; GNU ignores improper tails and keys other than strings or symbols.
      (while (consp collection)
        (let* ((item (car collection))
               (name (emacs-minibuffer-builtins--completion-name item)))
          (when (and name
                     (emacs-minibuffer-builtins--completion-match-p string name)
                     (or (null predicate) (funcall predicate item)))
            (setq matches (cons name matches))))
        (setq collection (cdr collection))))
     ((vectorp collection)
      (signal 'wrong-type-argument (list 'obarrayp collection))))
    (nreverse matches)))

(defun emacs-minibuffer-builtins--completion-result (string matches)
  "Return GNU's common prefix or unique exact-match result for MATCHES."
  (cond
   ((null matches) nil)
   ((let ((rest matches) (exact t))
      (while (and rest exact)
        (unless (string= string (car rest)) (setq exact nil))
        (setq rest (cdr rest)))
      exact)
    t)
   (t
    (let* ((best (car matches))
           (common (length best))
           (ignore-case (and (boundp 'completion-ignore-case)
                            completion-ignore-case)))
      (dolist (candidate (cdr matches))
        (let ((i 0) (limit (min common (length candidate))))
          (while (and (< i limit)
                      (eq t (compare-strings best i (1+ i)
                                             candidate i (1+ i) ignore-case)))
            (setq i (1+ i)))
          (setq common i))
        ;; Prefer a candidate that is itself the common prefix, then
        ;; a spelling whose prefix agrees with the input's case.
        (when (and ignore-case
                   (or (and (= (length candidate) common)
                            (> (length best) common))
                       (and (or (> (length best) common)
                                (= (length candidate) common))
                            (not (eq t (compare-strings
                                        string 0 nil best 0 (length string))))
                            (eq t (compare-strings
                                   string 0 nil candidate 0 (length string))))))
          (setq best candidate)))
      (substring best 0 common)))))

(when (emacs-minibuffer-builtins--install-function-p 'try-completion)
  (defun try-completion (&rest arguments)
    "Return the common prefix of completions of STRING in COLLECTION.
The optional PREDICATE receives original collection entries.  Function
collections receive STRING, PREDICATE and nil.  Case folding and regular
expression filtering follow the standard completion variables."
    (emacs-minibuffer-builtins--check-arity 'try-completion arguments 2 3)
    (let ((string (car arguments))
          (collection (cadr arguments))
          (predicate (car (cddr arguments))))
      (unless (stringp string)
        (signal 'wrong-type-argument (list 'stringp string)))
      (if (or (functionp collection)
              (not (or (listp collection) (hash-table-p collection)
                       (obarrayp collection) (vectorp collection))))
          (funcall collection string predicate nil)
        (emacs-minibuffer-builtins--completion-result
         string (emacs-minibuffer-builtins--completion-candidates
                 string collection predicate))))))

(when (emacs-minibuffer-builtins--install-function-p 'all-completions)
  (defun all-completions (&rest arguments)
    "Return all completions of STRING in COLLECTION.
The optional PREDICATE receives original list entries, obarray symbols,
or hash keys and values.  Function collections receive STRING, PREDICATE
and t.  Matching honors the standard completion variables."
    (emacs-minibuffer-builtins--check-arity 'all-completions arguments 2 3)
    (let ((string (car arguments))
          (collection (cadr arguments))
          (predicate (car (cddr arguments))))
      (unless (stringp string)
        (signal 'wrong-type-argument (list 'stringp string)))
      (if (or (functionp collection)
              (not (or (listp collection) (hash-table-p collection)
                       (obarrayp collection) (vectorp collection))))
          (funcall collection string predicate t)
        (emacs-minibuffer-builtins--completion-candidates
         string collection predicate)))))

(when (emacs-minibuffer-builtins--install-function-p 'test-completion)
  (defalias 'test-completion #'emacs-minibuffer-test-completion))

;;;; --- minibuffer state / control --------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'minibufferp)
  (defun minibufferp (&rest arguments)
    "Return whether BUFFER is a minibuffer; nil means the current buffer.
BUFFER may be a buffer or a buffer name.  Optional LIVE restricts the
result to an active minibuffer."
    (emacs-minibuffer-builtins--check-arity 'minibufferp arguments 0 2)
    (let* ((argument (car arguments))
           (live (cadr arguments))
           (buffer (cond
                    ((null argument) (current-buffer))
                    ((stringp argument)
                     (or (get-buffer argument)
                         (cdr (assoc argument nelisp-ec--buffers))))
                    ((or (bufferp argument) (nelisp-ec-buffer-p argument))
                     argument)
                    (t (signal 'wrong-type-argument
                               (list 'bufferp argument)))))
           (active nil)
           (stack emacs-minibuffer--buffers)
           (depth emacs-minibuffer--depth))
      (while (and stack (> depth 0))
        (when (eq buffer (car stack)) (setq active t))
        (setq stack (cdr stack) depth (1- depth)))
      (and buffer
           (or (and (bufferp buffer) (buffer-live-p buffer))
               (and (nelisp-ec-buffer-p buffer)
                    (not (nelisp-ec-buffer-killed-p buffer))))
           (if live active
             (or active
                 (and (emacs-window-p emacs-minibuffer--window)
                      (not (emacs-window-deleted-p emacs-minibuffer--window))
                      (eq buffer (emacs-window-buffer emacs-minibuffer--window)))))))))

(when (emacs-minibuffer-builtins--install-function-p 'active-minibuffer-window)
  (defalias 'active-minibuffer-window #'emacs-minibuffer-active-minibuffer-window))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-window)
  (defun minibuffer-window (&optional frame)
    "Return the minibuffer window belonging to FRAME."
    (emacs-cc-census-display-b34window01--minibuffer-window frame)))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt)
  (defalias 'minibuffer-prompt #'emacs-minibuffer-minibuffer-prompt))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-contents)
  (defun minibuffer-contents (&rest arguments)
    "Return the accessible contents of the current buffer after its prompt.
In an ordinary buffer return its entire accessible contents, retaining
text properties."
    (emacs-minibuffer-builtins--check-arity 'minibuffer-contents arguments 0 0)
    (buffer-substring (minibuffer-prompt-end) (point-max))))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt-end)
  (defun minibuffer-prompt-end (&rest arguments)
    "Return the end of the current minibuffer's prompt field.
Return `point-min' in an ordinary buffer or when no prompt field exists."
    (emacs-minibuffer-builtins--check-arity 'minibuffer-prompt-end arguments 0 0)
    (let ((start (point-min)))
      (if (minibufferp)
          (or (next-single-property-change start 'field)
              (if (get-text-property start 'field) (point-max) start))
        start))))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt-width)
  (defalias 'minibuffer-prompt-width #'emacs-minibuffer-minibuffer-prompt-width))

(when (emacs-minibuffer-builtins--install-function-p 'exit-minibuffer)
  (defalias 'exit-minibuffer #'emacs-minibuffer-exit-minibuffer))

(when (emacs-minibuffer-builtins--install-function-p 'abort-recursive-edit)
  (defalias 'abort-recursive-edit #'emacs-minibuffer-abort-recursive-edit))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-message)
  (defalias 'minibuffer-message #'emacs-minibuffer-minibuffer-message))

;;;; --- history defvars ------------------------------------------------

;; Most callers expect these defvars to exist as the HIST symbol they
;; pass to read-from-minibuffer.  Pre-defining them prevents void-variable
;; under standalone NeLisp.

(unless (boundp 'minibuffer-history)
  (defvar minibuffer-history nil
    "Track C bridge: alias for `emacs-minibuffer-history'."))

(unless (boundp 'command-history)
  (defvar command-history nil
    "Track C bridge: list of commands previously executed."))

(unless (boundp 'file-name-history)
  (defvar file-name-history nil
    "Track C bridge: history list for file-name reads."))

(unless (boundp 'read-string-history)
  (defvar read-string-history nil
    "Track C bridge: default history list for `read-string'."))

(unless (boundp 'buffer-name-history)
  (defvar buffer-name-history nil
    "Track C bridge: history list for `read-buffer'."))

(unless (boundp 'regexp-history)
  (defvar regexp-history nil
    "Track C bridge: history list for regexp prompts."))

(unless (boundp 'extended-command-history)
  (defvar extended-command-history nil
    "Track C bridge: history list for `M-x' / `execute-extended-command'."))

;;;; --- completion compatibility defvars --------------------------------

;; Upstream `minibuffer.el' / `crm.el' assume these specials exist even when
;; the current runtime does not implement visible *Completions* navigation.
;; Keep the default behavior disabled, but make the symbols available so
;; vendor callers such as Magit can execute their real source unchanged.

(unless (boundp 'minibuffer-visible-completions)
  (defvar minibuffer-visible-completions nil
    "When non-nil, visible *Completions* navigation is enabled.
The standalone substrate currently keeps this disabled by default."))

(unless (boundp 'minibuffer-visible-completions--always-bind)
  (defvar minibuffer-visible-completions--always-bind nil
    "Force visible completion bindings on when non-nil."))

(unless (boundp 'minibuffer-completion-predicate)
  (defvar minibuffer-completion-predicate nil
    "Predicate for the active minibuffer completion session."))

(unless (boundp 'completion-list-insert-choice-function)
  (defvar completion-list-insert-choice-function nil
    "Function used to insert the selected completion choice."))

(unless (boundp 'minibuffer-visible-completions-map)
  (defvar minibuffer-visible-completions-map (make-sparse-keymap)
    "Fallback keymap for visible completion navigation.
The standalone substrate does not yet install the upstream navigation
bindings here, but vendor code can safely compose this map."))

(provide 'emacs-minibuffer-builtins)

;;; emacs-minibuffer-builtins.el ends here
