;;; nelisp-stdlib-desktop-handlers.el --- Genuine GNU autoload variable owners -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-desktop-handlers--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name eval)))
(defconst nelisp-stdlib-desktop-handlers--forms
'((defvar desktop-buffer-mode-handlers nil "Alist of major mode specific functions to restore a desktop buffer.
Functions listed are called by `desktop-create-buffer' when `desktop-read'
evaluates the desktop file.  List elements must have the form

   (MAJOR-MODE . RESTORE-BUFFER-FUNCTION).

Buffers with a major mode not specified here, are restored by the default
handler `desktop-restore-file-buffer'.

Handlers are called with argument list

   (DESKTOP-BUFFER-FILE-NAME DESKTOP-BUFFER-NAME DESKTOP-BUFFER-MISC)

Furthermore, they may use the following variables:

   `desktop-file-version'
   `desktop-buffer-major-mode'
   `desktop-buffer-minor-modes'
   `desktop-buffer-point'
   `desktop-buffer-mark'
   `desktop-buffer-read-only'
   `desktop-buffer-locals'

If a handler returns a buffer, then the saved mode settings
and variable values for that buffer are copied into it.

Modules that define a major mode that needs a special handler should contain
code like

   (defun foo-restore-desktop-buffer
   ...
   (add-to-list \\='desktop-buffer-mode-handlers
                \\='(foo-mode . foo-restore-desktop-buffer))

The major mode function must either be autoloaded, or of the form
\"foobar-mode\" and defined in library \"foobar\", so that desktop
can guess how to load the mode's definition.") (put 'desktop-buffer-mode-handlers 'risky-local-variable t) (defvar desktop-minor-mode-handlers nil "Alist of functions to restore non-standard minor modes.
Functions are called by `desktop-create-buffer' to restore minor modes.
List elements must have the form

   (MINOR-MODE . RESTORE-FUNCTION).

Minor modes not specified here, are restored by the standard minor mode
function.

Handlers are called with argument list

   (DESKTOP-BUFFER-LOCALS)

Furthermore, they may use the following variables:

   `desktop-file-version'
   `desktop-buffer-file-name'
   `desktop-buffer-name'
   `desktop-buffer-major-mode'
   `desktop-buffer-minor-modes'
   `desktop-buffer-point'
   `desktop-buffer-mark'
   `desktop-buffer-read-only'
   `desktop-buffer-misc'

When a handler is called, the buffer has been created and the major mode has
been set, but local variables listed in desktop-buffer-locals has not yet been
created and set.

Modules that define a minor mode that needs a special handler should contain
code like

   (defun foo-desktop-restore
   ...
   (add-to-list \\='desktop-minor-mode-handlers
                \\='(foo-mode . foo-desktop-restore))

The minor mode function must either be autoloaded, or of the form
\"foobar-mode\" and defined in library \"foobar\", so that desktop
can guess how to load the mode's definition.

See also `desktop-minor-mode-table'.") (put 'desktop-minor-mode-handlers 'risky-local-variable t))
)
(defvar nelisp-stdlib-desktop-handlers--sealed-forms
  (copy-tree nelisp-stdlib-desktop-handlers--forms))
;;;###autoload
(defun nelisp-stdlib-desktop-handlers-install (&optional source-root)
  "Install exact GNU desktop autoload declarations at verified SOURCE-ROOT.
Preserve GNU host state and standalone user lists.  No desktop feature or
restore behavior is supplied by these variable and property declarations."
  (dolist (owner nelisp-stdlib-desktop-handlers--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Desktop initializer owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-desktop-handlers--forms
                 nelisp-stdlib-desktop-handlers--sealed-forms)
    (error "Desktop initializer payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Desktop initializer requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Desktop initializer source is unconfigured"))
      (let* ((path (expand-file-name "lisp/desktop.el" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 71369)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 71369))
                            "da58e8c7bc9e33990d5a7642155c018b6c619d24131e8fdf05518850f77398a2"))
          (error "Desktop initializer GNU source pin differs")))
      (dolist (form nelisp-stdlib-desktop-handlers--forms) (eval form t))
      'installed)))
(provide 'nelisp-stdlib-desktop-handlers)
