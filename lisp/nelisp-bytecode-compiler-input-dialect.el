;;; nelisp-bytecode-compiler-input-dialect.el --- Bytecode identity without compiler loading -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Shared identity owner for metadata, printers and the optional compiler.
(require 'cl-lib)
(declare-function json-read-file "json")
(defvar byte-code-vector)
(defvar byte-stack+-info)
(defvar nelisp-bytecode-runtime-dialect-id)
(defvar nelisp-bytecode-runtime-opcode-inventory)

(defconst nelisp-bytecode-compiler-input--inventory-sha256
  "147da590c9f5bdcf190b5b410c6af878c793ac89e07eafa4ae9f05a9b7aa7bcb")

(defun nelisp-bytecode-compiler-input-inventory-sha256 ()
  "Return the pinned opcode inventory SHA-256 used by this producer."
  nelisp-bytecode-compiler-input--inventory-sha256)

(defconst nelisp-bytecode-compiler-input--root
  (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))

(defconst nelisp-bytecode-compiler-input--runtime-source-evaluator
  (and (fboundp 'nelisp--eval-source-string)
       (symbol-function 'nelisp--eval-source-string)))

(defconst nelisp-bytecode-compiler-input--runtime-probe-primitives
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(subrp funcall eq equal secure-hash)))

(defun nelisp-bytecode-compiler-input--standalone-runtime-p ()
  "Verify the original native reader independently of public metadata.
The quote witness uses the already interned :status keyword; it does not
claim uninterned object identity or create a declaration or variable value."
  (and nelisp-bytecode-compiler-input--runtime-source-evaluator
       ;; Each guard uses the other original primitive: replacing either
       ;; funcall or eq cannot make its own identity check accept itself.
       (eq (cdr (assq 'funcall nelisp-bytecode-compiler-input--runtime-probe-primitives))
           (symbol-function 'funcall))
       (funcall (cdr (assq 'eq nelisp-bytecode-compiler-input--runtime-probe-primitives))
                (cdr (assq 'eq nelisp-bytecode-compiler-input--runtime-probe-primitives))
                (symbol-function 'eq))
       (fboundp 'nelisp--eval-source-string)
       (eq nelisp-bytecode-compiler-input--runtime-source-evaluator
           (symbol-function 'nelisp--eval-source-string))
       (cl-every (lambda (entry)
                   (funcall (cdr (assq 'eq nelisp-bytecode-compiler-input--runtime-probe-primitives))
                            (cdr entry) (symbol-function (car entry))))
                 nelisp-bytecode-compiler-input--runtime-probe-primitives)
       (subrp nelisp-bytecode-compiler-input--runtime-source-evaluator)
       (condition-case nil
           (eq (funcall nelisp-bytecode-compiler-input--runtime-source-evaluator
                        "(quote :status)") :status)
         (error nil))))

(defconst nelisp-bytecode-compiler-input--runtime-probe-function
  (symbol-function 'nelisp-bytecode-compiler-input--standalone-runtime-p))

(defun nelisp-bytecode-compiler-input-root ()
  "Return the repository root used by this producer."
  nelisp-bytecode-compiler-input--root)

(defun nelisp-bytecode-compiler-input--sha256-file (path)
  "Return SHA-256 of PATH read as literal bytes."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defconst nelisp-bytecode-compiler-input--gnu-source-content-sha256
  '((bytecomp . "094fa608bed9d9feffd4364b8df3288c1eb2bf1efd5dfa57fcd6d9bd13cdd099")
    (comp . "a5301de34ef5768d36044f4ffb4e4136966487435c3c33431912d4b36ff00eb8"))
  "SHA-256 of the decompressed GNU 31.1 bytecomp.el and comp.el sources.
GNU's Windows zip installs plain .el files, while Unix installs .el.gz
files whose gzip bytes vary between builds, so the content is pinned too.")

(defun nelisp-bytecode-compiler-input--gnu-source-content-sha256 (dir name)
  "Return SHA-256 of the decompressed NAME.el source in DIR, or nil."
  (let ((plain (expand-file-name (concat name ".el") dir))
        (gz (expand-file-name (concat name ".el.gz") dir)))
    (cond ((file-readable-p plain)
           (nelisp-bytecode-compiler-input--sha256-file plain))
          ((file-readable-p gz)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (let ((coding-system-for-read 'no-conversion)
                   (inhibit-message t))
               (insert-file-contents gz))
             (secure-hash 'sha256 (current-buffer)))))))

(defun nelisp-bytecode-compiler-input--dialect ()
  "Return pinned GNU 31.1 evidence, or a reason identity is unsupported."
  (let* ((native-runtime
          (and (boundp 'nelisp-bytecode-runtime-dialect-id)
           (eq (cdr (assq 'funcall nelisp-bytecode-compiler-input--runtime-probe-primitives))
               (symbol-function 'funcall))
           (funcall (cdr (assq 'eq nelisp-bytecode-compiler-input--runtime-probe-primitives))
                    (cdr (assq 'eq nelisp-bytecode-compiler-input--runtime-probe-primitives))
                    (symbol-function 'eq))
           (eq nelisp-bytecode-compiler-input--runtime-probe-function
               (symbol-function 'nelisp-bytecode-compiler-input--standalone-runtime-p))
           (funcall nelisp-bytecode-compiler-input--runtime-probe-function)))
         (inventory-path
          (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-opcodes.json"
                            nelisp-bytecode-compiler-input--root))
         (inventory-hash
          (if (and native-runtime (boundp 'nelisp-bytecode-runtime-opcode-inventory))
              (and (stringp nelisp-bytecode-runtime-opcode-inventory)
                   ;; This byte count belongs to the pinned inventory digest.
                   (= (length nelisp-bytecode-runtime-opcode-inventory) 4066)
                   (= (string-bytes nelisp-bytecode-runtime-opcode-inventory) 4066)
                   (secure-hash 'sha256 nelisp-bytecode-runtime-opcode-inventory))
            (and (file-readable-p inventory-path)
                 (nelisp-bytecode-compiler-input--sha256-file inventory-path)))))
    (cond
     ((and native-runtime
           (equal nelisp-bytecode-runtime-dialect-id
                  (concat "GNU Emacs 31.1; inventory-sha256="
                          nelisp-bytecode-compiler-input--inventory-sha256))
           (equal inventory-hash
                  nelisp-bytecode-compiler-input--inventory-sha256))
      (list :status 'pinned :dialect "GNU Emacs 31.1"
            :inventory-sha256 inventory-hash
            :runtime-evidence 'standalone-build-verified))
     ((and (boundp 'nelisp-bytecode-runtime-dialect-id)
           nelisp-bytecode-compiler-input--runtime-source-evaluator)
      (list :status 'unsupported
            :reason "standalone runtime byte-code dialect or opcode inventory identity differs from pinned GNU 31.1"))
     ((not (boundp 'emacs-version))
      (list :status 'unsupported
            :reason "runtime byte-code dialect identity is unavailable (emacs-version is unbound)"))
     ((not (and (equal emacs-version "31.1")
                (equal inventory-hash
                       nelisp-bytecode-compiler-input--inventory-sha256)))
      (list :status 'unsupported
            :reason "installed GNU byte-code dialect is not pinned Emacs 31.1"))
     (t
      ;; GNU needs its compiler tables only when host identity is requested.
      ;; The standalone branch above already carries build-pinned evidence.
      (require 'bytecomp)
      (require 'json)
      (let* ((inventory (json-read-file inventory-path))
             (source (alist-get 'source inventory))
             (library (locate-library "bytecomp"))
             (library-dir (and library (file-name-directory library)))
             (bytecomp (and library-dir
                            (expand-file-name "bytecomp.el.gz" library-dir)))
             (comp (and library-dir (expand-file-name "comp.el.gz" library-dir)))
             (bytecomp-hash (and bytecomp (file-readable-p bytecomp)
                                 (nelisp-bytecode-compiler-input--sha256-file bytecomp)))
             (comp-hash (and comp (file-readable-p comp)
                             (nelisp-bytecode-compiler-input--sha256-file comp)))
             (opcodes (append (alist-get 'opcodes inventory) nil))
             (stack-adjust (append (alist-get 'stack-adjust inventory) nil))
             (runtime-opcodes (mapcar (lambda (name)
                                        (and name (symbol-name name)))
                                      (append byte-code-vector nil))))
        (if (and (equal (alist-get 'dialect inventory) "GNU Emacs 31.1")
                 (= (length opcodes) 256) (= (length stack-adjust) 256)
                 (equal opcodes runtime-opcodes)
                 (equal stack-adjust (append byte-stack+-info nil))
                 (or (and (equal bytecomp-hash (alist-get 'bytecomp.el.gz source))
                          (equal comp-hash (alist-get 'comp.el.gz source)))
                     (and library-dir
                          (equal (nelisp-bytecode-compiler-input--gnu-source-content-sha256
                                  library-dir "bytecomp")
                                 (alist-get 'bytecomp nelisp-bytecode-compiler-input--gnu-source-content-sha256))
                          (equal (nelisp-bytecode-compiler-input--gnu-source-content-sha256
                                  library-dir "comp")
                                 (alist-get 'comp nelisp-bytecode-compiler-input--gnu-source-content-sha256)))))
            (list :status 'pinned :dialect "GNU Emacs 31.1"
                  :inventory-sha256 inventory-hash
                  :bytecomp-sha256 bytecomp-hash :comp-sha256 comp-hash)
          (list :status 'unsupported
                :reason "installed byte-code tables or source hashes differ from pinned GNU 31.1")))))))

(defun nelisp-bytecode-compiler-input-dialect ()
  "Return pinned byte-code dialect evidence for package producers."
  (nelisp-bytecode-compiler-input--dialect))

(defun nelisp-bytecode-compiler-input-native-package-runtime-context ()
  "Return copied embedded inventory state for every sealed package check."
  (when (and nelisp-bytecode-compiler-input--runtime-source-evaluator
             (boundp 'nelisp-bytecode-runtime-opcode-inventory))
    (list :embedded-opcode-inventory
          (if (and (stringp nelisp-bytecode-runtime-opcode-inventory)
                   (= (length nelisp-bytecode-runtime-opcode-inventory) 4066)
                   (= (string-bytes nelisp-bytecode-runtime-opcode-inventory) 4066))
              (copy-sequence nelisp-bytecode-runtime-opcode-inventory)
            :invalid))))

(provide 'nelisp-bytecode-compiler-input-dialect)
