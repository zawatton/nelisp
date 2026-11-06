;;; nemacs-test-paths.el --- Repo path helpers for nelisp-emacs-lib tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; The tests used to live beside `src/', `scripts/' and `vendor/' in a
;; standalone repository.  They now live in test/nelisp-emacs-lib/ while the
;; library sources are spread over packages/*/src/.  These helpers resolve
;; real repository files without depending on `load-path' order.

;;; Code:

(defconst nemacs-test-paths-repo-root
  (expand-file-name
   "../.."
   (file-name-directory (or load-file-name buffer-file-name
                            default-directory)))
  "Absolute repository root (no trailing slash), two directories above this file.")

(defun nemacs-test-paths-source-dirs ()
  "Return the list of packages/*/src/ directories under the repo root."
  (let ((packages (expand-file-name "packages" nemacs-test-paths-repo-root))
        dirs)
    (when (file-directory-p packages)
      (dolist (pkg (directory-files packages t "\\`[^.]"))
        (let ((src (expand-file-name "src" pkg)))
          (when (file-directory-p src)
            (push src dirs)))))
    (nreverse dirs)))

(defun nemacs-test-paths-source (name)
  "Return the absolute path of library source or data file NAME.
Search packages/*/src/ directly under the repo root.  Signal an error when
NAME is not found."
  (or (catch 'found
        (dolist (dir (nemacs-test-paths-source-dirs))
          (let ((candidate (expand-file-name name dir)))
            (when (file-exists-p candidate)
              (throw 'found candidate)))))
      (error "nemacs-test-paths: source file not found under packages/*/src/: %s"
             name)))

(defun nemacs-test-paths-source-owners (name)
  "Return every packages/*/src/NAME path under the repo root, in order.
A library source has exactly one owner package; more than one entry means a
stale duplicate that `nemacs-test-paths-source' and the doc211 bootstrap root
would resolve arbitrarily."
  (let (found)
    (dolist (dir (nemacs-test-paths-source-dirs))
      (let ((candidate (expand-file-name name dir)))
        (when (file-exists-p candidate)
          (push candidate found))))
    (nreverse found)))

(defun nemacs-test-paths-script (name)
  "Return the absolute path of repo-root scripts/NAME, or signal an error."
  (let ((path (expand-file-name name
                                (expand-file-name "scripts"
                                                  nemacs-test-paths-repo-root))))
    (unless (file-exists-p path)
      (error "nemacs-test-paths: script not found: %s" path))
    path))

(defun nemacs-test-paths-vendor (name &optional noerror)
  "Return the absolute path of repo-root vendor/NAME.
Signal an error when it does not exist, unless NOERROR is non-nil, in which
case the (possibly missing) path is returned for a `load' with NOERROR."
  (let ((path (expand-file-name name
                                (expand-file-name "vendor"
                                                  nemacs-test-paths-repo-root))))
    (unless (or noerror (file-exists-p path))
      (error "nemacs-test-paths: vendor file not found: %s" path))
    path))

(provide 'nemacs-test-paths)

;;; nemacs-test-paths.el ends here
