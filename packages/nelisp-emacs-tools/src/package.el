;;; package.el --- ELPA activation support for nelisp-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the activation subset of Emacs package.el implemented locally for
;; the NeLisp substrate.  It supports the real ELPA activation model:
;; scanning `package-user-dir' and `package-directory-list' for
;; NAME-VERSION/NAME-pkg.el descriptors, populating `package-alist',
;; activating dependencies first, and loading NAME-autoloads.el files.  The
;; generated autoload files are responsible for adding their package
;; directories to `load-path', matching package.el's installation contract.
;;
;; Archive refresh and installation are intentionally stubbed until
;; URL/process/GPG paths are promoted as runtime features.

;;; Code:

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which package.el stores user packages."))

(unless (fboundp 'locate-user-emacs-file)
  (defun locate-user-emacs-file (new-name &optional _old-name)
    "Return NEW-NAME under `user-emacs-directory'."
    (expand-file-name new-name user-emacs-directory)))

(defvar package-enable-at-startup t
  "Non-nil means startup should activate installed packages.")

(defvar package-user-dir (locate-user-emacs-file "elpa")
  "Directory containing per-user ELPA package directories.")

(defvar package-directory-list nil
  "Additional system package roots scanned for installed packages.")

(defvar package-load-list '(all)
  "List controlling which packages may be activated.")

(defvar package-quickstart nil
  "Placeholder quickstart toggle.  Quickstart files are not implemented.")

(defvar package-alist nil
  "Alist of installed package descriptors keyed by package symbol.")

(defvar package-activated-list nil
  "List of package names activated in the current session.")

(defvar package--activated nil
  "Non-nil after `package-activate-all' has run.")

(defvar package--initialized nil
  "Non-nil after `package-initialize' has run.")

(defvar package-archive-contents nil
  "Archive contents placeholder for activation-only package support.")

(defun package--version-to-list (version)
  "Return VERSION as a list of integers."
  (cond
   ((null version) nil)
   ((listp version) version)
   ((and (fboundp 'version-to-list) (stringp version))
    (version-to-list version))
   ((stringp version)
    (mapcar #'string-to-number (split-string version "[.-]" t)))
   (t nil)))

(defun package-desc-create (&rest plist)
  "Create a package descriptor from keyword PLIST."
  (list 'package-desc
        :name (plist-get plist :name)
        :version (package--version-to-list (plist-get plist :version))
        :summary (plist-get plist :summary)
        :reqs (plist-get plist :reqs)
        :dir (plist-get plist :dir)
        :extras (plist-get plist :extras)))

(defun package-desc-p (object)
  "Return non-nil when OBJECT is a package descriptor."
  (and (consp object) (eq (car object) 'package-desc)))

(defun package--desc-plist (desc)
  "Return DESC's plist."
  (unless (package-desc-p desc)
    (signal 'wrong-type-argument (list 'package-desc-p desc)))
  (cdr desc))

(defun package-desc-name (desc)
  "Return DESC's package name."
  (plist-get (package--desc-plist desc) :name))

(defun package-desc-version (desc)
  "Return DESC's package version list."
  (plist-get (package--desc-plist desc) :version))

(defun package-desc-summary (desc)
  "Return DESC's summary."
  (plist-get (package--desc-plist desc) :summary))

(defun package-desc-reqs (desc)
  "Return DESC's dependency list."
  (plist-get (package--desc-plist desc) :reqs))

(defun package-desc-dir (desc)
  "Return DESC's installed directory."
  (plist-get (package--desc-plist desc) :dir))

(defun package--set-desc-dir (desc dir)
  "Set DESC's installed directory to DIR."
  (setcdr desc (plist-put (cdr desc) :dir dir))
  desc)

(defun package-desc-full-name (desc)
  "Return DESC's NAME-VERSION string."
  (let ((version (package-desc-version desc)))
    (format "%s-%s" (package-desc-name desc)
            (if version (mapconcat #'number-to-string version ".") ""))))

(defun package--name-symbol (pkg)
  "Return PKG as a package symbol."
  (cond
   ((symbolp pkg) pkg)
   ((stringp pkg) (intern pkg))
   ((package-desc-p pkg) (package-desc-name pkg))
   (t (signal 'wrong-type-argument (list 'symbolp pkg)))))

(defun package--normalize-reqs (reqs)
  "Return REQS with dependency names interned and versions listified."
  (let ((items (if (eq (car-safe reqs) 'quote) (cadr reqs) reqs)))
    (mapcar (lambda (req)
              (list (package--name-symbol (car req))
                    (package--version-to-list (cadr req))))
            items)))

(defun package-desc-from-define (name-string version-string
                                             &optional summary requirements
                                             &rest extras)
  "Create a descriptor from a `define-package' form."
  (package-desc-create
   :name (intern name-string)
   :version version-string
   :summary summary
   :reqs (package--normalize-reqs requirements)
   :extras extras))

(defun package-process-define-package (exp)
  "Process descriptor expression EXP and add it to `package-alist'."
  (when (eq (car-safe exp) 'define-package)
    (let* ((desc (apply #'package-desc-from-define (cdr exp)))
           (name (package-desc-name desc))
           (entry (assq name package-alist)))
      (if entry
          (setcdr entry (append (cdr entry) (list desc)))
        (push (list name desc) package-alist))
      desc)))

(defun package--description-file (pkg-dir)
  "Return the package descriptor file in PKG-DIR, or nil."
  (let ((files (and (file-directory-p pkg-dir)
                    (directory-files pkg-dir nil "-pkg\\.el\\'"))))
    (car files)))

(defun package-load-descriptor (pkg-dir)
  "Load the package description file in directory PKG-DIR."
  (let ((pkg-file (package--description-file pkg-dir)))
    (when pkg-file
      (with-temp-buffer
        (insert-file-contents (expand-file-name pkg-file pkg-dir))
        (goto-char (point-min))
        (let ((desc (or (package-process-define-package
                         (read (current-buffer)))
                        (error "Can't find define-package in %s" pkg-file))))
          (package--set-desc-dir desc pkg-dir)
          desc)))))

(defun package-load-all-descriptors ()
  "Load descriptors from `package-user-dir' and `package-directory-list'."
  (dolist (dir (cons package-user-dir package-directory-list))
    (when (file-directory-p dir)
      (dolist (pkg-dir (directory-files dir t "\\`[^.]"))
        (when (file-directory-p pkg-dir)
          (package-load-descriptor pkg-dir))))))

(defun package--alist ()
  "Return `package-alist', computing it from disk when needed."
  (or package-alist
      (progn
        (package-load-all-descriptors)
        package-alist)))

(defun package--registered-p (name)
  "Return non-nil when NAME is present in `package-alist'."
  (let ((entry (assq name (package--alist))))
    (and entry (cdr entry))))

(defun package-installed-p (pkg &optional _min-version)
  "Return non-nil when PKG is installed or already provided."
  (let ((name (package--name-symbol pkg)))
    (or (featurep name)
        (package--registered-p name))))

(defun package-disabled-p (pkg-name _version)
  "Return non-nil when PKG-NAME is disabled by `package-load-list'."
  (let ((entry (assq pkg-name package-load-list)))
    (cond
     ((memq 'all package-load-list) nil)
     ((null entry) t)
     ((null (cadr entry)) t)
     (t nil))))

(defun package--get-activatable-pkg (pkg-name)
  "Return the descriptor for PKG-NAME, or nil."
  (let ((descs (cdr (assq pkg-name (package--alist)))))
    (catch 'found
      (dolist (desc descs)
        (unless (package-disabled-p pkg-name (package-desc-version desc))
          (throw 'found desc)))
      nil)))

(defun package--autoloads-file-name (pkg-desc)
  "Return PKG-DESC's autoload file name without extension."
  (expand-file-name
   (format "%s-autoloads" (package-desc-name pkg-desc))
   (package-desc-dir pkg-desc)))

(defun package-activate-1 (pkg-desc &optional _reload deps)
  "Activate PKG-DESC.  When DEPS is non-nil, activate dependencies first."
  (let ((name (package-desc-name pkg-desc)))
    (when deps
      (dolist (req (package-desc-reqs pkg-desc))
        (unless (package-activate (car req))
          (error "Unable to activate package `%s'; required package `%s' is unavailable"
                 name (car req)))))
    (condition-case err
        (load (package--autoloads-file-name pkg-desc) nil t)
      (error
       (when (fboundp 'message)
         (message "Error loading autoloads for %s: %s"
                  name (error-message-string err)))))
    (unless (memq name package-activated-list)
      (push name package-activated-list))
    t))

(defun package-activate (package &optional force)
  "Activate PACKAGE, returning non-nil on success."
  (let* ((name (package--name-symbol package))
         (desc (package--get-activatable-pkg name)))
    (cond
     ((null desc) (featurep name))
     ((and (memq name package-activated-list) (not force)) t)
     (t (package-activate-1 desc nil 'deps)))))

(defun package-activate-all ()
  "Activate all installed packages."
  (setq package--activated t)
  (dolist (entry (package--alist))
    (condition-case err
        (package-activate (car entry))
      (error
       (when (fboundp 'message)
         (message "%s" (error-message-string err))))))
  package-activated-list)

(defun package-initialize (&optional no-activate)
  "Load package descriptors and, unless NO-ACTIVATE, activate packages."
  (interactive)
  (setq package-alist nil)
  (package-load-all-descriptors)
  (setq package--initialized t)
  (unless no-activate
    (package-activate-all))
  t)

(defun package-refresh-contents (&optional _async)
  "Signal that archive refresh is outside current package support."
  (interactive)
  (user-error "package archive refresh requires URL/process/GPG support"))

(defun package-install (pkg &optional _dont-select)
  "Signal that installing PKG is outside current package support."
  (interactive "SPackage: ")
  (user-error "package install requires URL/process/GPG support: %s" pkg))

(defun package-install-selected-packages (&optional _noconfirm)
  "Signal that installing selected packages is outside current support."
  (interactive)
  (user-error "package install requires URL/process/GPG support"))

(defun define-package (_name-string _version-string
                                    &optional _docstring _requirements
                                    &rest _extra-properties)
  "Descriptor marker used only inside package descriptor files."
  (error "Don't call define-package directly"))

(provide 'package)

;;; package.el ends here
