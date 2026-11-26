;;; emacs-parity-subdirs.el --- local startup subdirectory walk -*- lexical-binding: t; -*-

(require 'files-runtime)

(defvar normal-top-level-add-subdirs-inode-list nil)
(defvar emacs-parity-subdirs--gnu-walk nil)
(defvar emacs-parity-subdirs--gnu-adder nil)
(defvar emacs-parity-subdirs--local-operations nil)

(defun emacs-parity-subdirs--local-operations-p ()
  (let ((operations emacs-parity-subdirs--local-operations)
        (local t))
    (while (and operations local)
      (let ((entry (pop operations)))
        (setq local (and (eq (symbol-function (car entry)) (cdr entry))
                         (not (nelisp--file-name-handler-possible-p (car entry) t))))))
    local))

(defun emacs-parity-subdirs--add-absolute-dirs (directories)
  ;; The local walker already made these paths absolute.
  ;; Preserve advised or replaced GNU helpers and use their usual entry point.
  (if (eq (symbol-function 'normal-top-level-add-to-load-path)
          emacs-parity-subdirs--gnu-adder)
      (let ((tail load-path)
            (root (directory-file-name default-directory)))
        (while (and (cdr tail) (not (equal root (car tail))))
          (setq tail (cdr tail)))
        (when tail
          (setcdr tail (nconc directories (cdr tail)))))
    (normal-top-level-add-to-load-path directories)))

(defun emacs-parity-subdirs--local-walk ()
  "Add local descendants to `load-path' in GNU's breadth-first order."
  (let ((pending (list (expand-file-name default-directory)))
        (visited (make-hash-table :test 'equal))
        dirs matched)
    (dolist (id normal-top-level-add-subdirs-inode-list)
      (puthash id t visited))
    (while pending
      (let (next)
        (dolist (directory pending)
          (push directory dirs)
          (let* ((raw (nelisp--syscall-readdir-names directory t))
                 (contents (if raw (nelisp--readdir-scan-raw raw t t)
                             (directory-files directory nil nil t)))
                 (stat (nelisp--syscall-lstat-buf directory))
                 (id (if (> stat 0)
                         (list (ptr-read-u64 stat 8) (ptr-read-u64 stat 0))
                       (file-attribute-file-identifier (file-attributes directory))))
                 (prefix (if (eq (aref directory (1- (length directory))) ?/)
                             directory (concat directory "/")))
                 children)
            (unless (gethash id visited)
              (puthash id t visited)
              (push id normal-top-level-add-subdirs-inode-list)
              (dolist (name contents)
                ;; Only directories survive the filter.  Sort those below
                ;; instead of sorting the complete readdir result first.
                (let ((first (aref name 0)))
                  (when (if (< first 128)
                            (or (and (>= first ?0) (<= first ?9))
                                (and (>= (logior first 32) ?a)
                                     (<= (logior first 32) ?z)))
                          (string-match "\\`[[:alnum:]]" name))
                    (setq matched t)
                    (unless (member name '("RCS" "CVS" "rcs" "cvs"))
                      (let ((child (concat prefix name)))
                        (when (and
                               (= (logand (nelisp--syscall-stat-field child 24) #o170000)
                                  #o040000)
                               (/= 0 (nelisp--syscall-path-int 21
                                                               (concat child "/.nosearch") 0)))
                          (push child children)))))))
              (dolist (child (sort children #'string-lessp))
                (push child next)))))
        (setq pending (nreverse next))))
    (when matched (set-match-data '(0 1)))
    (emacs-parity-subdirs--add-absolute-dirs (cdr (nreverse dirs)))))

(defun emacs-parity-subdirs--walk ()
  "Use the local startup walk where no file operation can dispatch."
  (if (and (eq system-type 'gnu/linux)
           (not (fboundp 'w32-untranslated-canonical-name))
           (fboundp 'nelisp--syscall-stat-field)
           (fboundp 'nelisp--syscall-path-int)
           (fboundp 'nelisp--syscall-lstat-buf)
           (fboundp 'nelisp--syscall-readdir-names)
           (fboundp 'nelisp--file-name-absolute-posix-p)
           (fboundp 'nelisp--file-name-handler-possible-p)
           (stringp default-directory)
           (nelisp--file-name-absolute-posix-p default-directory)
           (not (nelisp--string-search ":" default-directory 0))
           (emacs-parity-subdirs--local-operations-p))
      (emacs-parity-subdirs--local-walk)
    (funcall emacs-parity-subdirs--gnu-walk)))

(defun emacs-parity-subdirs-install ()
  "Install the standalone walk after GNU startup has been loaded."
  (when (and (files-standalone-runtime-p)
             (fboundp 'normal-top-level-add-to-load-path)
             (not (eq (symbol-function 'normal-top-level-add-subdirs-to-load-path)
                      #'emacs-parity-subdirs--walk)))
    (setq emacs-parity-subdirs--gnu-walk
          (symbol-function 'normal-top-level-add-subdirs-to-load-path))
    (setq emacs-parity-subdirs--gnu-adder
          (symbol-function 'normal-top-level-add-to-load-path))
    (setq emacs-parity-subdirs--local-operations
          (mapcar (lambda (operation) (cons operation (symbol-function operation)))
                  '(expand-file-name directory-files directory-file-name
                    file-attributes file-directory-p file-exists-p)))
    (defalias 'normal-top-level-add-subdirs-to-load-path #'emacs-parity-subdirs--walk)))

(provide 'emacs-parity-subdirs)
;;; emacs-parity-subdirs.el ends here
