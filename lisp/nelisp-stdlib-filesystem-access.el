;;; nelisp-stdlib-filesystem-access.el --- Actual POSIX directory search -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-filesystem-access--installed-owner nil)
(defun nelisp-stdlib-filesystem-access--live ()
  "Validate unchanged native filesystem operations before use."
  (dolist (owner nelisp-stdlib-filesystem-access--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Filesystem search owner changed: %S" (car owner))))
  (dolist (name '(syscall-direct ptr-write-u8 nelisp--target-os-code nelisp--target-arch-code))
    (unless (subrp (symbol-function name)) (error "Filesystem native owner absent")))
  (unless (and (= (nelisp--target-os-code) 0) (= (nelisp--target-arch-code) 0))
    (error "Filesystem search protocol requires Linux x86_64")))
(defun nelisp-stdlib-filesystem-access--accessible (filename)
  "Implement the GNU fileio.c POSIX searchable-directory branch.
File handlers keep their genuine operation dispatch.  Native local paths use
effective-ID F_OK on DIR/./; unsupported path encoding or kernel API refuses."
  (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
  (unless (eq (symbol-function 'nelisp-stdlib-filesystem-access--live)
              (cdr (assq 'nelisp-stdlib-filesystem-access--live
                         nelisp-stdlib-filesystem-access--owners)))
    (error "Filesystem validator owner changed"))
  (nelisp-stdlib-filesystem-access--live)
  (let* ((absolute (expand-file-name filename))
         (handler (find-file-name-handler absolute 'file-accessible-directory-p)))
    (if handler (funcall handler 'file-accessible-directory-p absolute)
      (let* ((path (concat absolute (if (= (aref absolute (1- (length absolute))) ?/) "./" "/./")))
             (length (length path)) (index 0) scratch result)
        (unless (<= 1 length 4095) (error "Filesystem search path bound rejected"))
        (while (< index length)
          (unless (<= 1 (aref path index) 127)
            (error "Filesystem search path encoding is unsupported"))
          (setq index (1+ index)))
        (setq scratch (syscall-direct 9 0 (1+ length) 3 34 -1 0))
        (unless (> scratch 0) (error "Filesystem search mapping failed"))
        (unwind-protect
            (progn
              (setq index 0)
              (while (< index length)
                (ptr-write-u8 scratch index (aref path index))
                (setq index (1+ index)))
              (ptr-write-u8 scratch length 0)
              ;; Linux x86_64 faccessat2, AT_FDCWD, F_OK, AT_EACCESS.
              (setq result (syscall-direct 439 -100 scratch 0 512 0 0))
              (when (memq result '(-38 -22))
                (error "Filesystem effective-access syscall is unavailable"))
              (= result 0))
          (unless (= (syscall-direct 11 scratch (1+ length) 0 0 0 0) 0)
            (error "Filesystem search cleanup failed")))))))
(defvar nelisp-stdlib-filesystem-access--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            nelisp-stdlib-filesystem-access--accessible
            nelisp-stdlib-filesystem-access--live
            syscall-direct ptr-write-u8 nelisp--target-os-code nelisp--target-arch-code
            expand-file-name find-file-name-handler secure-hash file-attributes)))
;;;###autoload
(defun nelisp-stdlib-filesystem-access-install (&optional source-root)
  "Install the actual GNU POSIX directory-search semantics on standalone.
Authenticate SOURCE-ROOT's fileio.c provenance.  Preserve genuine GNU hosts."
  (unless (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
              (cdr (assq 'nelisp-bytecode-compiler-input-dialect
                         nelisp-stdlib-filesystem-access--owners)))
    (error "Filesystem dialect owner changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Filesystem search requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless (eq (symbol-function 'nelisp-stdlib-filesystem-access--live)
                  (cdr (assq 'nelisp-stdlib-filesystem-access--live
                             nelisp-stdlib-filesystem-access--owners)))
        (error "Filesystem validator owner changed"))
      (nelisp-stdlib-filesystem-access--live)
      (when (and (fboundp 'file-accessible-directory-p)
                 (not (eq (symbol-function 'file-accessible-directory-p)
                          nelisp-stdlib-filesystem-access--installed-owner)))
        (error "Foreign standalone filesystem search provider is prebound"))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Filesystem source root is unconfigured"))
      (let* ((path (expand-file-name "src/fileio.c" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 228666)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 228666))
                            "d9a386ba8e221aeefba006c81a061610e09502b075812ae1fc5fae7b5d8746a6"))
          (error "Filesystem GNU source pin differs")))
      (fset 'file-accessible-directory-p #'nelisp-stdlib-filesystem-access--accessible)
      (setq nelisp-stdlib-filesystem-access--installed-owner
            (symbol-function 'file-accessible-directory-p))
      'installed)))
(provide 'nelisp-stdlib-filesystem-access)
