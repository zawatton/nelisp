;;; package.el --- Minimal package facade for nelisp-emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun package-desc-create (&rest plist)
  "Create a minimal package descriptor from keyword PLIST."
  (cons 'package-desc plist))

(defun package-desc-p (object)
  "Return non-nil when OBJECT is a minimal package descriptor."
  (and (consp object) (eq (car object) 'package-desc)))

(defun package-desc-name (desc)
  "Return DESC's package name."
  (plist-get (cdr desc) :name))

(defun package-desc-version (desc)
  "Return DESC's version."
  (plist-get (cdr desc) :version))

(defun package-desc-summary (desc)
  "Return DESC's summary."
  (plist-get (cdr desc) :summary))

(defvar package-alist nil
  "Alist of installed packages for the minimal package facade.")

(defvar package-activated-list nil
  "List of package names activated by the minimal package facade.")

(defvar package-user-dir
  (expand-file-name "elpa" user-emacs-directory)
  "Package directory used by the minimal package facade.")

(defvar package-archives nil
  "Archive list placeholder.  Network archive operations are not implemented.")

(defun package--name-symbol (pkg)
  "Return PKG as a symbol."
  (cond
   ((symbolp pkg) pkg)
   ((stringp pkg) (intern pkg))
   ((package-desc-p pkg) (package-desc-name pkg))
   (t (signal 'wrong-type-argument (list 'symbolp pkg)))))

(defun package--registered-p (name)
  "Return non-nil when NAME is present in `package-alist'."
  (let ((entry (assq name package-alist)))
    (and entry (cdr entry))))

(defun package-installed-p (pkg &optional _min-version)
  "Return non-nil when PKG is available in the minimal package facade."
  (let ((name (package--name-symbol pkg)))
    (or (featurep name)
        (package--registered-p name))))

(defun package--desc-dir (name)
  "Return the on-disk directory recorded for package NAME, or nil."
  (let ((desc (car (cdr (assq name package-alist)))))
    (and (package-desc-p desc) (plist-get (cdr desc) :dir))))

(defun package-activate (pkg &optional _force)
  "Activate PKG if it is installed, returning non-nil on success.
Activation adds the package's recorded directory (if any) to `load-path',
loads its feature when not already loaded, and records it in
`package-activated-list'.  A missing feature file is non-fatal: the package
is still recorded as activated (matching the registry-only facade)."
  (let ((name (package--name-symbol pkg)))
    (when (package-installed-p name)
      (let ((dir (package--desc-dir name)))
        (when (and dir (file-directory-p dir))
          (add-to-list 'load-path dir)))
      (unless (featurep name)
        (condition-case nil
            (require name)
          (error nil)))
      (cl-pushnew name package-activated-list)
      t)))

(defun package-activate-all ()
  "Activate every package registered in `package-alist'.
Returns the list of activated package names."
  (dolist (entry package-alist)
    (package-activate (car entry)))
  package-activated-list)

(defun package-initialize (&optional no-activate)
  "Initialize the minimal package facade.
Unless NO-ACTIVATE is non-nil, activate all registered packages."
  (unless no-activate
    (package-activate-all))
  t)

(defun package-refresh-contents ()
  "Signal that archive refresh is outside the minimal facade."
  (interactive)
  (user-error "package archive refresh requires URL/process support"))

(defun package-install (pkg &optional _dont-select)
  "Signal that installing PKG is outside the minimal facade."
  (interactive "SPackage: ")
  (user-error "package install requires URL/process support: %s" pkg))

(provide 'package)

;;; package.el ends here
