;;; nelisp-stdlib-literal-file.el --- Undecoded standalone file insertion -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-literal-file--dialect-owner
  (symbol-function 'nelisp-bytecode-compiler-input-dialect))
(defvar nelisp-stdlib-literal-file--owners nil)
(defun nelisp-stdlib-literal-file--live-p ()
  "Require the captured real byte reader and file metadata owners."
  (dolist (owner nelisp-stdlib-literal-file--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Literal file owner changed: %S" (car owner)))))
(defun nelisp-stdlib-literal-file--insert (filename &optional visit beg end replace)
  "Insert exact raw file bytes, preserving point and returning file metadata.
This standalone slice supports unibyte buffers without VISIT or REPLACE.
Unsupported modes are refused before changing the buffer."
  (nelisp-stdlib-literal-file--live-p)
  (unless (stringp filename)
    (signal 'wrong-type-argument (list 'stringp filename)))
  (when (or visit replace enable-multibyte-characters)
    (error "Literal file mode unavailable: VISIT, REPLACE, or multibyte buffer"))
  ;; Respect real public bindings without inventing missing buffer state.
  ;; The underlying runtime's broader per-buffer read-only ownership is separate.
  (when (and (boundp 'buffer-read-only) (symbol-value 'buffer-read-only)
             (not (and (boundp 'inhibit-read-only)
                       (symbol-value 'inhibit-read-only))))
    (signal 'buffer-read-only (list (current-buffer))))
  (dolist (offset (list beg end))
    (unless (or (null offset) (and (integerp offset) (>= offset 0)))
      (signal 'wrong-type-argument (list 'natnump offset))))
  (let* ((absolute (expand-file-name filename))
         (resolved (file-truename absolute))
         (attributes (file-attributes resolved))
         (size (nth 7 attributes))
         (mode (nth 8 attributes)))
    (unless attributes
      (signal 'file-missing (list "Opening input file" "No such file or directory" absolute)))
    (unless (and (null (car attributes)) (stringp mode) (> (length mode) 0)
                 (= (aref mode 0) ?-) (integerp size) (<= 0 size 134217728))
      (error "Literal file requires a bounded regular file"))
    (let* ((start (min (or beg 0) size))
           (limit (max start (min (or end size) size)))
           (offset start) chunks)
      ;; Read every window before insertion: a failed read leaves the buffer intact.
      (while (< offset limit)
        (let* ((count (min 1048576 (- limit offset)))
               (bytes (nelisp-native-raw-file-read resolved offset count)))
          (unless (and (stringp bytes) (not (multibyte-string-p bytes))
                       (= (length bytes) count) (= (string-bytes bytes) count))
            (error "Literal reader returned decoded or truncated bytes"))
          (push bytes chunks)
          (setq offset (+ offset count))))
      (let ((position (point)))
        (unwind-protect (dolist (bytes (nreverse chunks)) (insert bytes))
          (goto-char position)))
      (list absolute (- limit start)))))
(defun nelisp-stdlib-literal-file-install ()
  "Install the real standalone literal reader, preserving genuine GNU hosts."
  (unless (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
              nelisp-stdlib-literal-file--dialect-owner)
    (error "Literal file evidence owner changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Literal file requires verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless nelisp-stdlib-literal-file--owners
        (setq nelisp-stdlib-literal-file--owners
              (mapcar (lambda (name) (cons name (symbol-function name)))
                      '(nelisp-native-raw-file-read file-attributes file-truename))))
      (nelisp-stdlib-literal-file--live-p)
      (fset 'insert-file-contents-literally #'nelisp-stdlib-literal-file--insert)
      'installed)))
(provide 'nelisp-stdlib-literal-file)
