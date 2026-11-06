;;; nelisp-boot-libraries.el --- Relocatable, demand-loaded boot libraries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(defvar nelisp-boot-library-sources nil
  "Entries (NAME ORIGINAL-PATH SOURCE), authenticated with the executable.")
(defvar nelisp-boot-library--base-load nil)
(defvar nelisp-boot-library-autoloads nil)
(declare-function nelisp--core-bytecode-load "nelisp-standalone-build")
(declare-function nelisp--after-load-feature "nelisp-standalone-build")
(declare-function nl-afset "nelisp-stdlib-prelude")

(defun nelisp-boot-library-known-entry (file)
  "Return FILE's registered provider, without intercepting unrelated paths."
  (when (or (stringp file) (symbolp file))
    (let* ((file (if (symbolp file) (symbol-name file) file))
           (base (file-name-nondirectory file))
           (name (if (string-suffix-p ".el" base) (substring base 0 -3) base))
           (entry (assoc name nelisp-boot-library-sources)))
      (when (and entry (or (equal file name) (equal file (concat name ".el"))
                           (equal file (nth 1 entry))))
        entry))))

(defvar nelisp-boot-library-authoritative '("cl-lib" "help-mode")
  "Providers that win over any same-named file found on `load-path'.
GNU cl-lib.el is the implementation; a bundle copy of the name (the nelisp-emacs
foundation shim) must not answer `(require \'cl-lib)'.  An explicit path still
loads that file.")

(defun nelisp-boot-library-entry (file)
  "Return FILE's bundled provider when no ordinary source resolves.
Authoritative providers (see `nelisp-boot-library-authoritative') answer a bare
name even when `load-path' has a file of that name."
  (let ((entry (nelisp-boot-library-known-entry file))
        (name (if (symbolp file) (symbol-name file) file)))
    (when (and entry (not (file-readable-p name))
               (or (and (not (file-name-directory name))
                        (member name nelisp-boot-library-authoritative))
                   (not (and (not (file-name-directory name))
                             (locate-file name load-path '("" ".el"))))))
      entry)))

(defvar nelisp-boot-library-gnu-preloaded '(("lisp-mode" abbrev prog-mode))
  "Alist (LIBRARY . FEATURES): FEATURES GNU's loadup.el preloads before LIBRARY.
GNU lisp-mode.el does not `require' abbrev or prog-mode (it uses
`define-abbrev-table' and `prog-mode-map' directly) because they are dumped
into every Emacs.  This image defers them, so they load before LIBRARY does.")

(defun nelisp-boot-library-load (file &rest arguments)
  "Load FILE normally, or restore its executable-pinned provider on demand."
  (when (stringp file)
    (let* ((base (file-name-nondirectory file))
           (name (if (string-suffix-p ".el" base) (substring base 0 -3) base)))
      (dolist (feature (cdr (assoc name nelisp-boot-library-gnu-preloaded)))
        (require feature))))
  (let ((known (nelisp-boot-library-known-entry file))
        (entry (nelisp-boot-library-entry file))
        restore)
    ;; Compatibility providers contain (unless (fboundp NAME) ...). Our own
    ;; autoload must not suppress that definition. The prelude's nl-afset
    ;; publication helper records the old cell in the autoload rollback queue;
    ;; raw fset deliberately does not participate in that transaction.
    (when known
      (dolist (spec nelisp-boot-library-autoloads)
        (when (and (equal (nth 1 spec) (car known)) (fboundp (car spec)))
          (let ((function (symbol-function (car spec))))
            (when (and (autoloadp function) (equal (nth 1 function) (car known)))
              (push (cons (car spec) function) restore)
              (nl-afset (car spec) nil))))))
    (unwind-protect
        (if (not entry)
            (apply nelisp-boot-library--base-load file arguments)
          (let ((load-file-name
                 (expand-file-name (concat "boot-libraries/" (car entry) ".el")
                                   invocation-directory))
                (buffer (current-buffer)))
            (unwind-protect
                (progn
                  (nelisp--core-bytecode-load load-file-name (nth 2 entry))
                  (when (fboundp 'nelisp--after-load-feature)
                    (nelisp--after-load-feature load-file-name))
                  t)
              (when (buffer-live-p buffer) (set-buffer buffer)))))
      (dolist (saved restore)
        (unless (fboundp (car saved)) (fset (car saved) (cdr saved)))))))

(defun nelisp-boot-library-install (sources autoloads)
  "Install SOURCES and AUTOLOADS without reading or evaluating a provider."
  (setq nelisp-boot-library-sources sources)
  (setq nelisp-boot-library-autoloads autoloads)
  (unless nelisp-boot-library--base-load
    (setq nelisp-boot-library--base-load (symbol-function 'load)))
  (fset 'load #'nelisp-boot-library-load)
  (dolist (entry autoloads)
    ;; A guarded definition is a fallback, not an advertised boot entry point.
    ;; Publishing its autoload changes fboundp and suppresses consumers' fallbacks.
    (unless (or (featurep (intern (nth 1 entry))) (nth 3 entry))
      (fset (car entry) (list 'autoload (nth 1 entry) nil nil (nth 2 entry)))))
  t)

(provide 'nelisp-boot-libraries)
