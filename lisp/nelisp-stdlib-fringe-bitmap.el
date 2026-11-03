;;; nelisp-stdlib-fringe-bitmap.el --- Genuine GNU non-GUI fringe fallback -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(require 'nelisp-stdlib-terminal-output)
(defvar nelisp-stdlib-fringe-bitmap--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            nelisp-stdlib-terminal-output-install
            display-graphic-p selected-frame window-system secure-hash
            file-attributes expand-file-name eval fmakunbound fset symbol-function)))
;; Exact defun from pinned GNU fringe.el's (unless (fboundp ...)) branch.
;; It exists only in non-GUI builds; graphical bitmap storage is src/fringe.c.
(defconst nelisp-stdlib-fringe-bitmap--form
  '(defun define-fringe-bitmap (_bitmap _bits &optional _height _width _align)
     "Define fringe bitmap BITMAP from BITS of size HEIGHT x WIDTH.
BITMAP is a symbol identifying the new fringe bitmap.
BITS is either a string or a vector of integers.
HEIGHT is height of bitmap.  If HEIGHT is nil, use length of BITS.
WIDTH must be an integer between 1 and 16, or nil which defaults to 8.
Optional fifth arg ALIGN may be one of `top', `center', or `bottom',
indicating the positioning of the bitmap relative to the rows where it
is used; the default is to center the bitmap.  Fifth arg may also be a
list (ALIGN PERIODIC) where PERIODIC non-nil specifies that the bitmap
should be repeated.
If BITMAP already exists, the existing definition is replaced."))
(defvar nelisp-stdlib-fringe-bitmap--sealed-form
  (copy-tree nelisp-stdlib-fringe-bitmap--form))
(defvar nelisp-stdlib-fringe-bitmap--installed-owner nil)
(defvar nelisp-stdlib-fringe-bitmap--terminal-font-owner nil)
;;;###autoload
(defun nelisp-stdlib-fringe-bitmap-install (&optional source-root)
  "Install GNU31's exact non-GUI fallback on the verified standalone.
Authenticate SOURCE-ROOT's fringe.el and terminal substrate.  Preserve GNU
and foreign public providers; this module implements no graphical storage."
  (if (and (fboundp 'define-fringe-bitmap)
           (not (eq (symbol-function 'define-fringe-bitmap)
                    nelisp-stdlib-fringe-bitmap--installed-owner)))
      'preserved
    (dolist (owner nelisp-stdlib-fringe-bitmap--owners)
      (unless (eq (symbol-function (car owner)) (cdr owner))
        (error "Fringe bitmap dependency owner changed: %S" (car owner))))
    (unless (equal nelisp-stdlib-fringe-bitmap--form
                   nelisp-stdlib-fringe-bitmap--sealed-form)
      (error "Fringe bitmap source payload changed"))
    (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
      (unless (and (eq (plist-get evidence :status) 'pinned)
                   (eq (plist-get evidence :runtime-evidence)
                       'standalone-build-verified))
        (error "Fringe bitmap requires verified standalone runtime")))
    (unless (and (stringp source-root) (> (length source-root) 0))
      (error "Fringe bitmap source root is unconfigured"))
    (let* ((path (expand-file-name "lisp/fringe.el" source-root))
           (attributes (file-attributes path)))
      (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 12900)
                   (equal (secure-hash 'sha256
                                       (nelisp-native-raw-file-read path 0 12900))
                          "4944265e40f8a74986b5654a162a23a44e54943acb1579f26b32ab9b073f63ce"))
        (error "Fringe bitmap GNU source pin differs")))
    (unless nelisp-stdlib-fringe-bitmap--terminal-font-owner
      ;; A preserved public provider is not proof of terminal ownership.
      ;; Force the authenticated public installer through registration once,
      ;; then seal its public owner.  Preserve any prebound foreign function.
      (let ((previous (and (fboundp 'internal-char-font)
                           (symbol-function 'internal-char-font)))
            (admitted nil))
        (unwind-protect
            (progn
              (fmakunbound 'internal-char-font)
              (nelisp-stdlib-terminal-output-install)
              (unless (and (fboundp 'internal-char-font)
                           (or (null previous)
                               (eq previous (symbol-function 'internal-char-font))))
                (error "Fringe bitmap terminal font owner differs"))
              (internal-char-font nil 65)
              (setq nelisp-stdlib-fringe-bitmap--terminal-font-owner
                    (symbol-function 'internal-char-font))
              (setq admitted t))
          (unless admitted
            (if previous (fset 'internal-char-font previous)
              (fmakunbound 'internal-char-font))))))
    (unless (and (fboundp 'internal-char-font)
                 (eq (symbol-function 'internal-char-font)
                     nelisp-stdlib-fringe-bitmap--terminal-font-owner))
      (error "Fringe bitmap terminal font owner differs"))
    (internal-char-font nil 65)
    (unless (and (null (selected-frame)) (null (window-system))
                 (not (display-graphic-p)))
      (error "Fringe bitmap requires the non-graphical substrate"))
    (if (and (fboundp 'define-fringe-bitmap)
             (eq (symbol-function 'define-fringe-bitmap)
                 nelisp-stdlib-fringe-bitmap--installed-owner))
        'preserved
      (eval nelisp-stdlib-fringe-bitmap--form t)
      (setq nelisp-stdlib-fringe-bitmap--installed-owner
            (symbol-function 'define-fringe-bitmap))
      'installed)))
(provide 'nelisp-stdlib-fringe-bitmap)
