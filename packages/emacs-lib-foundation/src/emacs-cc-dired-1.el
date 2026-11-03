;;; emacs-cc-dired-1.el --- Dired C-core replacements  -*- lexical-binding: t; -*-

(defun emacs-cc-dired-1--attributes-lessp (left right)
  "Compare attribute fields LEFT and RIGHT in order."
  (cond
   ((equal left right) nil)
   ((and (numberp left) (numberp right)) (< left right))
   ((and (stringp left) (stringp right)) (string-lessp left right))
   ((and (consp left) (consp right))
    (or (emacs-cc-dired-1--attributes-lessp (car left) (car right))
        (and (not (equal (car left) (car right))) nil)
        (emacs-cc-dired-1--attributes-lessp (cdr left) (cdr right))))
   (t (string-lessp (format "%s" left) (format "%s" right)))))

(unless (fboundp 'file-attributes-lessp)
  (defun file-attributes-lessp (f1 f2)
    "Return t if first arg file attributes list is less than second.
Comparison is in lexicographic order and case is significant.

(fn F1 F2)"
    (unless (listp f1) (signal 'wrong-type-argument (list 'listp f1)))
    (unless (listp f2) (signal 'wrong-type-argument (list 'listp f2)))
    (let ((a f1) (b f2) result done)
      (while (and a b (not done))
        (unless (equal (car a) (car b))
          (setq result (emacs-cc-dired-1--attributes-lessp (car a) (car b)) done t))
        (setq a (cdr a) b (cdr b)))
      (if done result (and (null a) b)))))

(defun emacs-cc-dired-1--completion-filter (names file directory)
  "Filter NAMES by FILE and `completion-regexp-list' relative to DIRECTORY."
  (let ((regs completion-regexp-list))
    (delq nil
          (mapcar (lambda (name)
                    (when (and (string-prefix-p file name)
                               (or (null regs)
                                   (let ((completion-regexp-list regs) ok)
                                     (while (and completion-regexp-list (not ok))
                                       (when (string-match-p (car completion-regexp-list) name)
                                         (setq ok t))
                                       (setq completion-regexp-list (cdr completion-regexp-list)))
                                     ok)))
                      name))
                  (sort (mapcar (lambda (name)
                                  (if (file-directory-p (expand-file-name name directory))
                                      (concat name "/") name))
                                (append '("." "..") names))
                        (lambda (a b)
                          (if (eq (string-suffix-p "/" a) (string-suffix-p "/" b))
                              (string-lessp a b)
                            (string-suffix-p "/" a))))))))

(unless (fboundp 'file-name-all-completions)
  (defun file-name-all-completions (file directory)
    "Return a list of all completions of file name FILE in directory DIRECTORY.
These are all file names in directory DIRECTORY which begin with FILE.

This function ignores some of the possible completions as determined
by `completion-regexp-list', which see.  `completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.

(fn FILE DIRECTORY)"
    (unless (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
    (unless (stringp directory) (signal 'wrong-type-argument (list 'stringp directory)))
    (emacs-cc-dired-1--completion-filter
     (directory-files directory nil nil nil) file directory)))

(unless (fboundp 'file-name-completion)
  (defun file-name-completion (file directory &optional predicate)
    "Complete file name FILE in directory DIRECTORY.
Returns the longest string
common to all file names in DIRECTORY that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIRECTORY contains no name starting with FILE.

If PREDICATE is non-nil, call PREDICATE with each possible
completion (in absolute form) and ignore it if PREDICATE returns nil.

This function ignores some of the possible completions as determined
by the variables `completion-regexp-list' and
`completion-ignored-extensions', which see.  `completion-regexp-list'
is matched against file and directory names relative to DIRECTORY.

(fn FILE DIRECTORY &optional PREDICATE)"
    (unless (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
    (unless (stringp directory) (signal 'wrong-type-argument (list 'stringp directory)))
    (let ((names (emacs-cc-dired-1--completion-filter
                  (directory-files directory nil nil nil) file directory))
          (ignore completion-ignored-extensions))
      (when predicate
        (setq names (delq nil (mapcar (lambda (name)
                                        (when (funcall predicate
                                                       (expand-file-name name directory)) name))
                                      names))))
      (when ignore
        (setq names (delq nil (mapcar (lambda (name)
                                        (let ((tail ignore) match)
                                          (while (and tail (not match))
                                            (when (string-suffix-p (car tail) name) (setq match t))
                                            (setq tail (cdr tail)))
                                          (unless match name))) names))))
      (cond ((null names) nil)
            ((and (null (cdr names)) (string= file (car names))) t)
            (t (let ((prefix (car names)))
                 (dolist (name (cdr names))
                   (while (and (> (length prefix) (length file))
                               (not (string-prefix-p prefix name)))
                     (setq prefix (substring prefix 0 -1))))
                 prefix))))))

(unless (fboundp 'system-groups)
  (defun system-groups ()
    "Return a list of user group names currently registered in the system.
The value may be nil if not supported on this platform.

(fn)"
    (when (and (fboundp 'insert-file-contents) (file-readable-p "/etc/group"))
      (with-temp-buffer
        (insert-file-contents "/etc/group")
        (let (result)
          (dolist (line (split-string (buffer-string) "\n" t))
            (let ((name (car (split-string line ":"))))
              (when (and name (not (string-empty-p name))) (push name result))))
          (nreverse result))))))

(unless (fboundp 'system-users)
  (defun system-users ()
    "Return a list of user names currently registered in the system.
If we don't know how to determine that on this platform, just
return a list with one element, taken from `user-real-login-name'.

(fn)"
    (if (and (fboundp 'insert-file-contents) (file-readable-p "/etc/passwd"))
        (with-temp-buffer
          (insert-file-contents "/etc/passwd")
          (let (result)
            (dolist (line (split-string (buffer-string) "\n" t))
              (let ((name (car (split-string line ":"))))
                (when (and name (not (string-empty-p name))) (push name result))))
            (nreverse result)))
      (list (user-real-login-name)))))

(provide 'emacs-cc-dired-1)
