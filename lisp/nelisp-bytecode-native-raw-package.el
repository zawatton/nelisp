;;; nelisp-bytecode-native-raw-package.el --- narrow raw package admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; V1 packages only pinned one-argument GNU CAR/CDR entries.  This is a
;; separate format from boxed .neln packages; it never broadens their reader.

;;; Code:
(require 'cl-lib)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-native-load)

(defconst nelisp-bytecode-native-raw-package--format
  'nelisp-bytecode-native-raw-package-v1)
(defvar nelisp-bytecode-native-raw-package--calls (make-hash-table :test 'eq))
(defvar nelisp-bytecode-native-raw-package--handles (make-hash-table :test 'eq))
(defvar nelisp-bytecode-native-raw-package--verified-features
  (make-hash-table :test 'eq))

(defun nelisp-bytecode-native-raw-package-compile-elc
    (elc-path feature function-name package-directory)
  "Compile one exact CAR/CDR entry from ELC-PATH into PACKAGE-DIRECTORY.
ELC is read as data and must end in (provide FEATURE).  The package target
must not exist.  Only a verified unary raw-v2 entry is admitted."
  (unless (and (stringp elc-path) (string-suffix-p ".elc" elc-path)
               (file-readable-p elc-path) (symbolp feature)
               (symbolp function-name) (stringp package-directory)
               (not (file-exists-p package-directory)))
    (error "raw-package: invalid compile contract"))
  (let* ((forms (nelisp-bytecode-native-package-raw-read-elc-forms elc-path))
         (function (nelisp-bytecode-native-package-read-elc-function
                    elc-path function-name))
         (input (and function (nelisp-bytecode-compiler-input-build function)))
         (operation (and input
                         (nelisp-bytecode-native-compiler-unary-template-operation input)))
         (binary (nelisp-native-load-running-binary-sha256))
         (dialect (nelisp-bytecode-compiler-input-dialect))
         (abi (nelisp-bytecode-native-package-abi-fingerprint))
         (source-hash (nelisp-bytecode-native-package-raw-file-sha256 elc-path))
         (target (expand-file-name package-directory))
         (parent (file-name-directory target))
         (name (file-name-nondirectory (directory-file-name target)))
         (temp (expand-file-name (concat "." name ".staging-"
                                         (number-to-string (random 1000000000))) parent))
         (artifact "entry.nelr")
         (entry (format "nl_native_%s_probe" operation))
         result)
    (unless (and (nelisp-bytecode-native-package-raw-final-provider-p forms feature)
                 function input (eq (plist-get input :status) 'complete)
                 (memq operation '(car cdr))
                 (equal (plist-get input :argument-min) 1)
                 (equal (plist-get input :argument-max) 1)
                 (stringp binary) (equal (plist-get dialect :status) 'pinned)
                 (stringp (nelisp-native-load--runtime-abi-v2))
                 (nelisp-native-load-raw-v2-contract)
                 (fboundp 'nelisp-runtime-reload-contract-matches-p)
                 (nelisp-runtime-reload-contract-matches-p))
      (error "raw-package: source is not the pinned unary CAR/CDR provider"))
    ;; The complete source/template identity is established before directory
    ;; or backend effects.  mkdir claims the absent destination against races;
    ;; package.nraw is written last and is the admission/commit marker.
    (make-directory parent t)
    (make-directory target)
    (unwind-protect
        (progn
          (make-directory temp)
          (setq result (nelisp-bytecode-native-compiler-build
                        function (expand-file-name artifact temp) entry))
          (unless (and (eq (plist-get result :status) 'complete)
                       (file-readable-p (expand-file-name artifact temp)))
            (error "raw-package: backend refused entry: %s"
                   (plist-get result :reason)))
          (nelisp-bytecode-native-package-raw-copy-file-bytes
           elc-path (expand-file-name "module.elc" temp))
          (dolist (file (directory-files temp t "[^.]"))
            (rename-file file (expand-file-name (file-name-nondirectory file) target)))
          (let* ((manifest
                 (list :format nelisp-bytecode-native-raw-package--format
                       :abi abi :runtime-binary-sha256 binary
                       :raw-runtime-abi (nelisp-native-load--runtime-abi-v2)
                       :dialect (plist-get dialect :dialect)
                       :input-fingerprint
                       (secure-hash 'sha256 (prin1-to-string input))
                       :source-path (file-truename elc-path)
                       :source-sha256 source-hash :feature feature
                       :elc "module.elc"
                       :elc-sha256 (nelisp-bytecode-native-package-raw-file-sha256
                                    (expand-file-name "module.elc" target))
                       :name function-name :arity 1 :operation operation
                       :entry entry :artifact artifact
                       :artifact-sha256
                       (nelisp-bytecode-native-package-raw-file-sha256
                        (expand-file-name artifact target))))
                 (manifest-temp (expand-file-name "package.nraw.tmp" target))
                 (manifest-final (expand-file-name "package.nraw" target)))
            (with-temp-file manifest-temp (prin1 manifest (current-buffer)))
            (rename-file manifest-temp manifest-final))
          (list :status 'complete :manifest (expand-file-name "package.nraw" target)))
      (when (file-exists-p temp) (delete-directory temp t))
      (unless (file-exists-p (expand-file-name "package.nraw" target))
        (delete-directory target t)))))

(defun nelisp-bytecode-native-raw-package-open (manifest-path)
  "Validate every package identity before evaluating its copied .elc once."
  (let* ((path (expand-file-name manifest-path))
         (dir (file-name-directory path))
         (m (with-temp-buffer (insert-file-contents-literally path)
              (goto-char (point-min)) (read (current-buffer))))
         (elc (expand-file-name "module.elc" dir))
         (artifact-name (plist-get m :artifact))
         (artifact (and (stringp artifact-name)
                        (string-match-p "\\`[A-Za-z0-9_-]+\\.nelr\\'" artifact-name)
                        (expand-file-name artifact-name dir)))
         (raw-manifest (and (file-readable-p artifact)
                            (nelisp-native-load-manifest artifact)))
         (forms (and (file-readable-p elc)
                     (nelisp-bytecode-native-package-raw-read-elc-forms elc)))
         (name (plist-get m :name))
         (function (and forms (nelisp-bytecode-native-package-read-elc-function elc name)))
         (input (and function (nelisp-bytecode-compiler-input-build function)))
         (dialect (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get m :format) nelisp-bytecode-native-raw-package--format)
                 (equal (plist-get m :abi) (nelisp-bytecode-native-package-abi-fingerprint))
                 (equal (plist-get m :runtime-binary-sha256)
                        (nelisp-native-load-running-binary-sha256))
                 (equal (plist-get m :raw-runtime-abi)
                        (nelisp-native-load--runtime-abi-v2))
                 (fboundp 'nelisp-runtime-reload-contract-matches-p)
                 (nelisp-runtime-reload-contract-matches-p)
                 (equal (plist-get m :dialect) (plist-get dialect :dialect))
                 (stringp (plist-get m :source-path))
                 (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'"
                                 (plist-get m :source-sha256))
                 (equal (plist-get m :elc) "module.elc")
                 (file-readable-p elc)
                 (equal (plist-get m :elc-sha256)
                        (nelisp-bytecode-native-package-raw-file-sha256 elc))
                 (stringp artifact-name)
                 (string-match-p "\\`[A-Za-z0-9_-]+\\.nelr\\'" artifact-name)
                 (symbolp name) (= (or (plist-get m :arity) -1) 1)
                 (equal (plist-get m :entry)
                        (format "nl_native_%s_probe" (plist-get m :operation)))
                 (file-readable-p artifact)
                 (equal (plist-get m :artifact-sha256)
                        (nelisp-bytecode-native-package-raw-file-sha256 artifact))
                 (equal (plist-get m :input-fingerprint)
                        (secure-hash 'sha256 (prin1-to-string input)))
                 (eq (plist-get m :operation)
                     (nelisp-bytecode-native-compiler-unary-template-operation input))
                 (memq (plist-get m :operation) '(car cdr))
                 (nelisp-bytecode-native-package-raw-final-provider-p
                  forms (plist-get m :feature))
                 (null (nelisp-native-load-raw-v2-check
                        raw-manifest (plist-get m :entry))))
      (error "raw-package: manifest or package identity rejected"))
    (let* ((feature (plist-get m :feature))
           (originals (gethash feature
                               nelisp-bytecode-native-raw-package--verified-features))
           (identity (list name (plist-get m :elc-sha256)
                           (plist-get m :input-fingerprint)))
           (original (and (hash-table-p originals)
                          (gethash identity originals))))
      ;; A feature/name pair alone does not identify the function template.
      ;; Reuse native eligibility only for the exact validated ELC identity.
      (unless (featurep feature)
        (nelisp-bytecode-native-package-raw-eval-elc-forms forms)
        (setq original (symbol-function name))
        (unless (hash-table-p originals)
          (setq originals (make-hash-table :test 'equal)))
        (puthash identity original originals)
        (puthash feature originals
                 nelisp-bytecode-native-raw-package--verified-features))
      (unless (and (featurep feature) (fboundp name))
        (error "raw-package: provider did not install %S and provide %S"
               name feature))
      (let ((handle (list nelisp-bytecode-native-raw-package--format
                          path m original nil 0 nil)))
        (puthash handle t nelisp-bytecode-native-raw-package--handles)
        handle))))

(defun nelisp-bytecode-native-raw-package-call (handle argument)
  "Call HANDLE's package function, lazily selecting its fixed raw entry."
  (unless (and (consp handle) (eq (car handle)
                                  nelisp-bytecode-native-raw-package--format)
               (gethash handle nelisp-bytecode-native-raw-package--handles)
               (not (nth 6 handle)))
    (error "raw-package: invalid handle"))
  (let* ((m (nth 2 handle)) (name (plist-get m :name))
         (original (nth 3 handle))
         (function (symbol-function name))
         (mapping (nth 4 handle)))
    (setcar (nthcdr 5 handle) (1+ (nth 5 handle)))
    (unwind-protect
        (if (or (not original)
                (not nelisp-bytecode-native-package-native-enabled)
                (not (eq function original)))
            (funcall function argument)
          (unless mapping
            (let ((artifact
                   (expand-file-name (plist-get m :artifact)
                                     (file-name-directory (nth 1 handle)))))
              (unless (and (file-readable-p artifact)
                           (equal (plist-get m :artifact-sha256)
                                  (nelisp-bytecode-native-package-raw-file-sha256
                                   artifact)))
                (error "raw-package: artifact changed after package admission")))
            (setq mapping
                  (nelisp-native-load-raw-v2-artifact
                   (expand-file-name (plist-get m :artifact)
                                     (file-name-directory (nth 1 handle)))
                   (plist-get m :entry) (plist-get m :runtime-binary-sha256)))
            (setcar (nthcdr 4 handle) mapping))
          (let ((result
                 (funcall (if (eq (plist-get m :operation) 'car)
                              #'nelisp-native-load-raw-v2-car-call
                            #'nelisp-native-load-raw-v2-cdr-call)
                          mapping argument)))
            (puthash handle
                     (1+ (gethash handle nelisp-bytecode-native-raw-package--calls 0))
                     nelisp-bytecode-native-raw-package--calls)
            result))
      (setcar (nthcdr 5 handle) (1- (nth 5 handle))))))

(defun nelisp-bytecode-native-raw-package-close (handle)
  "Close HANDLE, refusing active calls; repeated close is harmless.
The accepted native-call count remains readable after close."
  (unless (and (consp handle)
               (eq (car handle) nelisp-bytecode-native-raw-package--format)
               (gethash handle nelisp-bytecode-native-raw-package--handles))
    (error "raw-package: invalid handle"))
  (when (> (nth 5 handle) 0)
    (error "raw-package: cannot close an active call"))
  (unless (nth 6 handle)
    (when (nth 4 handle)
      (nelisp-native-load-unload (nth 4 handle))
      (setcar (nthcdr 4 handle) nil))
    (setcar (nthcdr 6 handle) t))
  t)

(defun nelisp-bytecode-native-raw-package-native-call-count (handle)
  "Return HANDLE's accepted native dispatch count."
  (gethash handle nelisp-bytecode-native-raw-package--calls 0))

(provide 'nelisp-bytecode-native-raw-package)
;;; nelisp-bytecode-native-raw-package.el ends here
