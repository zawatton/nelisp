;;; nelisp-stdlib-display-images.el --- Genuine GNU image capability predicate -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(require 'nelisp-stdlib-terminal-output)
(defvar nelisp-stdlib-display-images--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            nelisp-stdlib-terminal-output-install display-graphic-p
            selected-frame secure-hash file-attributes)))
(defconst nelisp-stdlib-display-images--form
  '(defun display-images-p (&optional display)
     "Return non-nil if DISPLAY can display images.

DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
     (and (display-graphic-p display)
          (fboundp 'image-mask-p)
          (fboundp 'image-size))))
(defvar nelisp-stdlib-display-images--sealed-form
  (copy-tree nelisp-stdlib-display-images--form))
(defvar nelisp-stdlib-display-images--installed-owner nil)
;;;###autoload
(defun nelisp-stdlib-display-images-install (&optional source-root)
  "Install the exact GNU31 frame predicate on the verified standalone.
Authenticate SOURCE-ROOT's frame source and the existing non-graphical output
owner.  Preserve an existing public provider.  No graphical backend is created."
  (unless (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
              (cdr (assq 'nelisp-bytecode-compiler-input-dialect
                         nelisp-stdlib-display-images--owners)))
    (error "Image capability dialect owner changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
  (if (and (eq (plist-get evidence :status) 'pinned)
           (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified)))
      'preserved
    (unless (equal nelisp-stdlib-display-images--form
                   nelisp-stdlib-display-images--sealed-form)
      (error "Image capability source payload changed"))
    (dolist (owner nelisp-stdlib-display-images--owners)
      (unless (eq (symbol-function (car owner)) (cdr owner))
        (error "Image capability owner changed: %S" (car owner))))
    (when (and (fboundp 'display-images-p)
               (not (eq (symbol-function 'display-images-p)
                        nelisp-stdlib-display-images--installed-owner)))
      (error "Foreign standalone image predicate is prebound"))
    (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
      (unless (and (eq (plist-get evidence :status) 'pinned)
                   (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        (error "Image capability requires verified standalone runtime")))
    (unless (and (stringp source-root) (> (length source-root) 0))
      (error "Image capability source root is unconfigured"))
    (let* ((path (expand-file-name "frame.el.gz" source-root))
           (attributes (file-attributes path)))
      (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 39268)
                   (equal (secure-hash 'sha256
                                       (nelisp-native-raw-file-read path 0 39268))
                          "35aa844e3c0ebaf6e7e3e936a719c5851ce5dbc73fcda434b1bffd90c548f3f0"))
        (error "Image capability GNU frame source pin differs")))
    (nelisp-stdlib-terminal-output-install)
    ;; This public operation verifies that the actual output device is live.
    (internal-char-font nil 65)
    (unless (and (null (selected-frame)) (not (display-graphic-p)))
      (error "Image capability non-graphical owner is unavailable"))
    (eval nelisp-stdlib-display-images--form t)
    (setq nelisp-stdlib-display-images--installed-owner
          (symbol-function 'display-images-p))
    'installed)))
(provide 'nelisp-stdlib-display-images)
