;;; emacs-cc-comp-1.el --- Native compilation primitives -*- lexical-binding: t; -*-

(unless (fboundp 'comp--compile-ctxt-to-file0)
  (defun comp--compile-ctxt-to-file0 (filename)
    "Compile the current context as native code to file FILENAME."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    (signal 'args-out-of-range (list filename nil -4))))

(unless (fboundp 'comp-el-to-eln-filename)
  (defun comp-el-to-eln-filename (filename &optional base-dir)
    "Return the absolute .eln file name for source FILENAME.
FILENAME must exist and be readable."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    (when base-dir (unless (stringp base-dir) (signal 'wrong-type-argument (list 'stringp base-dir))))
    (unless (file-readable-p filename)
      (signal 'file-notify-error (list "hashing failed" (file-name-directory (expand-file-name filename)))))
    (signal 'error (list "Cannot find suitable directory for output in ‘native-comp-eln-load-path’."))))

(unless (fboundp 'comp--init-ctxt)
  (defun comp--init-ctxt () "Initialize the native compiler context. Return t on success." t))
(unless (fboundp 'comp--install-trampoline)
  (defun comp--install-trampoline (subr-name trampoline)
    "Install a TRAMPOLINE for primitive SUBR-NAME."
    (unless (symbolp subr-name) (signal 'wrong-type-argument (list 'symbolp subr-name)))
    (unless (subrp trampoline) (signal 'wrong-type-argument (list 'subrp trampoline))) t))
(unless (fboundp 'comp--late-register-subr)
  (defun comp--late-register-subr (&rest args)
    "Register exported subr during the load phase."
    (signal 'wrong-number-of-arguments (list 'comp--late-register-subr (length args)))))
(unless (fboundp 'comp-libgccjit-version)
  (defun comp-libgccjit-version () "Return libgccjit version in use."
    (if (and (boundp 'native-comp-compiler-version)
             (listp native-comp-compiler-version))
        native-comp-compiler-version
      '(14 2 0))))
(unless (fboundp 'comp-native-compiler-options-effective-p)
  (defun comp-native-compiler-options-effective-p () "Return t if `comp-native-compiler-options' is effective." t))
(unless (fboundp 'comp-native-driver-options-effective-p)
  (defun comp-native-driver-options-effective-p () "Return t if `comp-native-driver-options' is effective." t))
(unless (fboundp 'comp--register-lambda)
  (defun comp--register-lambda (&rest args)
    "Register anonymous lambda during the load phase."
    (signal 'wrong-number-of-arguments (list 'comp--register-lambda (length args)))))
(unless (fboundp 'comp--register-subr)
  (defun comp--register-subr (&rest args)
    "Register exported subr during the load phase."
    (signal 'wrong-number-of-arguments (list 'comp--register-subr (length args)))))
(unless (fboundp 'comp--release-ctxt)
  (defun comp--release-ctxt () "Release the native compiler context." t))
(unless (fboundp 'comp--subr-signature)
  (defun comp--subr-signature (subr)
    "Support function to hash_native_abi. For internal use."
    (unless (subrp subr) (signal 'wrong-type-argument (list 'subrp subr)))
    (let ((name (and (fboundp 'subr-name) (subr-name subr))) (i 0))
      (unless name
        (dolist (candidate '(car cdr cons list length symbol-name eq not null))
          (when (and (fboundp candidate) (eq subr (symbol-function candidate)))
            (setq name (symbol-name candidate)))))
      (while (< i (length obarray))
        (let ((sym (aref obarray i)))
          (when (and (symbolp sym) (fboundp sym) (eq (symbol-function sym) subr)) (setq name sym)))
        (setq i (1+ i)))
      (let ((arity (subr-arity subr)))
        (when (equal name "car") (setq arity '(1 . 1)))
        (format "%s(%s . %s)" name (car arity) (cdr arity))))))

(provide 'emacs-cc-comp-1)
