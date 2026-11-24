;;; -*- lexical-binding: t; -*-
(defun par-load-find-file-row (name value)
  (prin1 (list name value))
  (terpri))

(defun par-load-find-file-name (root file)
  (and file (if (string-prefix-p root file)
                (substring file (length root))
              file)))

(defun par-load-find-file-write (file &optional value)
  (with-temp-file file
    ;; A portable .elc fixture with the header recognized by both loaders.
    (if (string-suffix-p ".elc" file)
        (insert ";ELC\1\0\0\0\n;;; in Emacs version 31.1\n")
      (insert ";; -*- lexical-binding: t; -*-\n"))
    (when value
      (insert (format "(setq par-load-find-file-value '%S)\n" value)))))

(defvar par-load-find-file-value nil)
(defun par-load-find-file-load (file &optional nosuffix must-suffix)
  (setq par-load-find-file-value nil)
  (list (load file t t nosuffix must-suffix) par-load-find-file-value))

(defun par-load-find-file-handler (operation &rest args)
  (let ((name (car args)))
    (cond
     ((eq operation 'expand-file-name)
      (if (string-prefix-p "/probe:" name) name
        (concat (or (cadr args) default-directory) name)))
     ((memq operation '(file-exists-p file-readable-p))
      (and (member name '("/probe:/virtual.el"
                          "/probe:/par-load-find-file-virtual.el")) t))
     ((eq operation 'file-directory-p) nil)
     ((eq operation 'load)
      (setq par-load-find-file-value name)
      (when (string-suffix-p "/par-load-find-file-virtual.el" name)
        (provide 'par-load-find-file-virtual))
      t))))

(let* ((root (file-name-as-directory (make-temp-file "par-load-find-file-" t)))
       (a (concat root "a/"))
       (b (concat root "b/"))
       (default-directory root)
       (load-path (list a b))
       (load-suffixes '(".elc" ".el"))
       (load-file-rep-suffixes '(""))
       (load-prefer-newer nil))
  (unwind-protect
      (progn
        (make-directory (concat a "org") t)
        (make-directory b)
        (par-load-find-file-write (concat a "foo.elc") 'first-elc)
        (par-load-find-file-write (concat a "foo.el") 'first-el)
        (par-load-find-file-write (concat b "foo.el") 'second-el)
        (set-file-times (concat a "foo.elc") '(27000 0 0 0))
        (set-file-times (concat a "foo.el") '(27000 1 0 0))
        (set-file-times (concat b "foo.el") '(27000 2 0 0))
        (par-load-find-file-row 'suffix-order
                               (par-load-find-file-name root (locate-library "foo")))
        (par-load-find-file-row 'explicit-suffix-order
                               (par-load-find-file-name
                                root (locate-file "foo" load-path '(".el" ".elc"))))
        (par-load-find-file-row 'older-elc (par-load-find-file-load "foo"))
        (let ((load-prefer-newer t))
          (par-load-find-file-row 'locate-library-newer
                                 (par-load-find-file-name root (locate-library "foo")))
          (par-load-find-file-row 'newer-el (par-load-find-file-load "foo"))
          (set-file-times (concat a "foo.elc") '(27000 1 0 0))
          (par-load-find-file-row 'mtime-tie (par-load-find-file-load "foo")))
        (delete-file (concat a "foo.el"))
        (make-directory (concat a "foo.el"))
        (par-load-find-file-row 'directory-excluded
                               (par-load-find-file-name
                                root (locate-file "foo" load-path '(".el"))))
        (par-load-find-file-row 'directory-predicate
                               (list (par-load-find-file-name
                                      root (locate-file "foo" load-path '(".el")
                                                        #'file-directory-p))
                                     (par-load-find-file-name
                                      root (locate-file "foo" load-path '(".el")
                                                        (lambda (_) 'dir-ok)))
                                     (par-load-find-file-name
                                      root (locate-file "foo" load-path '(".el") 'exists))))
        (par-load-find-file-write (concat root "here.el") 'here)
        (par-load-find-file-row 'path-elements
                               (mapcar (lambda (path)
                                         (par-load-find-file-name
                                          root (locate-library "here" nil path)))
                                       (list (list nil a) (list "" a) (list "b" nil))))
        (par-load-find-file-row 'relative-path
                               (par-load-find-file-name
                                root (locate-library "foo" nil '("b"))))
        (par-load-find-file-row 'literal-absolute
                               (par-load-find-file-name
                                root (locate-file (concat a "../here.el") nil)))
        (par-load-find-file-write (concat a "org/item.el") 'nested)
        (par-load-find-file-row 'names
                               (mapcar (lambda (name)
                                         (par-load-find-file-name
                                          root (locate-library name)))
                                       (list "org/item" (concat a "org/item")
                                             (concat b "foo"))))
        (let ((home (getenv "HOME")))
          (unwind-protect
              (progn
                (setenv "HOME" root)
                (par-load-find-file-row 'tilde
                                       (par-load-find-file-name
                                        root (locate-library "~/a/org/item"))))
            (setenv "HOME" home)))
        (par-load-find-file-write (concat b "fresh.el"))
        (par-load-find-file-row 'mutation
                               (list (par-load-find-file-name root (locate-library "fresh"))
                                     (progn (par-load-find-file-write (concat a "fresh.el"))
                                            (par-load-find-file-name root (locate-library "fresh")))
                                     (progn (delete-file (concat a "fresh.el"))
                                            (par-load-find-file-name root (locate-library "fresh")))))
        (par-load-find-file-write (concat a "bare") 'bare)
        (par-load-find-file-write (concat a "both.el") 'with-suffix)
        (par-load-find-file-write (concat a "both") 'without-suffix)
        (par-load-find-file-row 'load-suffix-flags
                               (list (par-load-find-file-load "bare" nil t)
                                     (par-load-find-file-load "bare" t t)
                                     (let ((load-path '(nil)))
                                       (par-load-find-file-load "a/bare" nil t))
                                     (par-load-find-file-load "both")
                                     (par-load-find-file-load "both" t)
                                     (par-load-find-file-load "org/item.el" nil t)))
        (par-load-find-file-row 'absolute-no-bare-representation
                               (let ((load-file-rep-suffixes '(".gz")))
                                 (par-load-find-file-load (concat a "bare"))))
        (let ((file-name-handler-alist '(("\\`/probe:" . par-load-find-file-handler)))
              (load-path (cons "/probe:/" load-path))
              (load-prefer-newer t))
          (par-load-find-file-row 'handler
                                 (list (locate-library "virtual")
                                       (par-load-find-file-load "virtual")
                                       (par-load-find-file-load "/probe:/direct")
                                       (locate-file "virtual" load-path '(".el") 'exists)
                                       (require 'par-load-find-file-virtual nil t)))
          (let ((default-directory "/probe:/"))
            (par-load-find-file-row 'handler-relative-path
                                   (list (locate-library "virtual" nil '(nil))
                                         (locate-library "virtual" nil '("")))))))
    (delete-directory root t)))
