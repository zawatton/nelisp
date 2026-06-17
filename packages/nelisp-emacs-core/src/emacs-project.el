;;; emacs-project.el --- Minimal project.el subset for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 §3.4.3 defines a compact `project.el' subset for the daily
;; driver MVP: detect a VC-backed project by walking upward for `.git',
;; `.hg', or `.svn'; expose `project-current' / `project-root'; offer
;; `project-find-file' through `completing-read' + `find-file'; and keep
;; a minimal persisted history of known project roots.

;;; Code:

(require 'cl-lib)
(require 'emacs-fileio-builtins)
(require 'emacs-minibuffer-builtins)

(defcustom project-list-file
  (expand-file-name "projects" user-emacs-directory)
  "Flat file used to persist known project roots.
Each line stores one absolute project root."
  :type 'file
  :group 'convenience)

(defvar project--list nil
  "Cached list of known project roots.")

(defconst project--vc-markers '(".git" ".hg" ".svn")
  "Directory or file names that identify a project root.")

(defun project--normalize-dir (dir)
  "Return DIR as an absolute directory name."
  (file-name-as-directory (expand-file-name dir)))

(defun project--project (root)
  "Wrap ROOT in the MVP project representation."
  (list 'project-vc (project--normalize-dir root)))

(defun project--project-p (object)
  "Return non-nil when OBJECT is an MVP project struct."
  (and (consp object)
       (eq (car object) 'project-vc)
       (stringp (cadr object))))

(defun project--vc-root-p (dir)
  "Return non-nil when DIR contains a supported VC marker."
  (cl-some (lambda (marker)
             (file-exists-p (expand-file-name marker dir)))
           project--vc-markers))

(defun project--find-root (start-dir)
  "Walk upward from START-DIR and return the enclosing project root."
  (let ((dir (project--normalize-dir start-dir))
        parent)
    (while (and dir (not (project--vc-root-p dir)))
      (setq parent (file-name-directory (directory-file-name dir)))
      (setq dir (unless (or (null parent) (equal parent dir))
                  parent)))
    dir))

(defun project--ensure-list-loaded ()
  "Load `project--list' from `project-list-file' once."
  (unless project--list
    (setq project--list
          (if (file-exists-p project-list-file)
              (with-temp-buffer
                (insert-file-contents project-list-file)
                (let (roots)
                  (dolist (line (split-string (buffer-string) "\n" t))
                    (push (project--normalize-dir line) roots))
                  (delete-dups (nreverse roots))))
            nil))))

(defun project--append-known-project (root)
  "Append ROOT to `project-list-file' if not already known in memory."
  (let ((normalized (project--normalize-dir root)))
    (project--ensure-list-loaded)
    (unless (member normalized project--list)
      (setq project--list (append project--list (list normalized)))
      ;; Persistence is best-effort: a write failure (e.g. the standalone
      ;; reader's restricted file I/O) must not abort project detection, which
      ;; only needs the in-memory list updated above.
      (condition-case nil
          (progn
            (make-directory (file-name-directory project-list-file) t)
            (with-temp-buffer
              (insert normalized "\n")
              (write-region (point-min) (point-max) project-list-file t 'silent)))
        (error nil)))))

(defun project--files-recursive (dir)
  "Return all files under DIR recursively.
Falls back to a `directory-files' walk when `directory-files-recursively'
is unavailable (the standalone reader lacks it)."
  (if (fboundp 'directory-files-recursively)
      (directory-files-recursively dir ".*" nil)
    (let ((out nil) (stack (list (directory-file-name dir))))
      (while stack
        (let ((d (pop stack)))
          (dolist (name (directory-files d t nil t))
            (let ((bn (file-name-nondirectory name)))
              (unless (member bn '("." ".."))
                (if (file-directory-p name)
                    (push name stack)
                  (push name out)))))))
      (nreverse out))))

(defun project--project-files (root include-all)
  "Return project files beneath ROOT.
When INCLUDE-ALL is nil, exclude files inside VC admin directories."
  (let* ((dir (project--normalize-dir root))
         (all-files (project--files-recursive dir))
         ;; `regexp-quote' per marker + `\\|' alternation rather than
         ;; `regexp-opt': the latter's optimized output does not match on the
         ;; standalone reader's regexp engine.
         (vc-dir-pattern
          (mapconcat (lambda (m) (concat "/" (regexp-quote m) "/"))
                     project--vc-markers "\\|")))
    (cl-remove-if
     (lambda (path)
       (or (file-directory-p path)
           (and (not include-all)
                (string-match-p vc-dir-pattern path))))
     all-files)))

(defun project--relative-candidates (root include-all)
  "Return completion candidates for ROOT.
The result is a list of relative file names.  The project files all live
under ROOT, so strip the ROOT prefix directly -- `file-relative-name'
returns nil on the standalone reader."
  (let ((root (project--normalize-dir root)))
    (mapcar (lambda (path)
              (if (string-prefix-p root path)
                  (substring path (length root))
                (or (file-relative-name path root) path)))
            (project--project-files root include-all))))

(defun project--read-project-root ()
  "Prompt for a project root from the persisted history."
  (project--ensure-list-loaded)
  (unless (fboundp 'completing-read)
    (user-error "No completing-read available"))
  (let ((choice (completing-read "Switch to project: " project--list nil t)))
    (unless (and (stringp choice) (not (string-empty-p choice)))
      (user-error "No project selected"))
    (project--normalize-dir choice)))

;;;###autoload
(defun project-current (&optional maybe-prompt directory)
  "Return the current project object for DIRECTORY or `default-directory'.
When MAYBE-PROMPT is non-nil and no project is detected, prompt once
for a directory and retry detection there."
  (let* ((start (or directory default-directory))
         (root (and start (project--find-root start))))
    (cond
     (root
      (project--append-known-project root)
      (project--project root))
     ((and maybe-prompt (fboundp 'read-directory-name))
      (let ((prompted (read-directory-name "Project directory: " start nil t)))
        (when prompted
          (project-current nil prompted))))
     (t nil))))

;;;###autoload
(defun project-root (project)
  "Return the root directory string for PROJECT."
  (unless (project--project-p project)
    (signal 'wrong-type-argument (list 'project--project-p project)))
  (cadr project))

;;;###autoload
(defun project-find-file (&optional include-all)
  "Find a file inside the current project.
When INCLUDE-ALL is non-nil, include files under VC admin directories."
  (interactive "P")
  (let* ((project (project-current t))
         (root (and project (project-root project))))
    (unless root
      (user-error "No current project"))
    (unless (fboundp 'completing-read)
      (user-error "No completing-read available"))
    (unless (fboundp 'find-file)
      (user-error "No find-file available"))
    (let* ((candidates (project--relative-candidates root include-all))
           (choice (completing-read "Project file: " candidates nil t)))
      (unless (and (stringp choice) (not (string-empty-p choice)))
        (user-error "No project file selected"))
      (find-file (expand-file-name choice root)))))

;;;###autoload
(defun project-switch-project (dir)
  "Switch `default-directory' to DIR and open a project file there.
Interactively, select DIR from the known project history."
  (interactive (list (project--read-project-root)))
  (let* ((root (project-root
                (or (project-current nil dir)
                    (project--project dir)))))
    (setq default-directory root)
    (project--append-known-project root)
    (project-find-file)))

(provide 'emacs-project)

;;; emacs-project.el ends here
