;;; string-properties-regression.el --- focused standalone property parity -*- lexical-binding: t; -*-

(defconst string-properties-regression--root
  (or (getenv "CCORE_ROOT")
      (expand-file-name "../../.."
                        (file-name-directory (or load-file-name buffer-file-name)))))

(when (fboundp 'nelisp--buffer-multibyte-p)
  (dolist (dir '("packages/nelisp-emacs-foundation/src"
                 "packages/nelisp-emacs-core/src"
                 "packages/nelisp-emacs-buffer-core/src"
                 "packages/nelisp-emacs-text-core/src"))
    (add-to-list 'load-path (expand-file-name dir string-properties-regression--root)))
  (dolist (file '("packages/nelisp-emacs-foundation/src/emacs-stub.el"
                  "packages/nelisp-emacs-core/src/emacs-frame.el"
                  "packages/nelisp-emacs-core/src/emacs-frame-builtins.el"
                  "packages/nelisp-emacs-core/src/emacs-window.el"
                  "packages/nelisp-emacs-core/src/emacs-window-builtins.el"
                  "packages/nelisp-emacs-buffer-core/src/nelisp-emacs-compat.el"
                  "packages/nelisp-emacs-buffer-core/src/emacs-buffer.el"
                  "packages/nelisp-emacs-buffer-core/src/emacs-buffer-builtins.el"
                  "packages/nelisp-emacs-foundation/src/emacs-string.el"))
    (load (expand-file-name file string-properties-regression--root) nil t)))

(let* ((styled (propertize (copy-sequence "abcd") 'face 'bold))
       (removed (propertize "abcd" 'face 'bold))
       (cleared (propertize "abcd" 'face 'bold))
       (end-boundaries
        (list (get-text-property 4 'face "abcd")
              (text-properties-at 4 "abcd")
              (get-text-property 0 'face "")
              (text-properties-at 0 ""))))
  (remove-text-properties 0 4 '(face) removed)
  (set-text-properties 0 4 nil cleared)
  (let ((insert-result
         (with-temp-buffer
           (insert "A" styled "B")
           (list (buffer-string)
                 (get-text-property 1 'face)
                 (get-text-property 2 'face)
                 (get-text-property 5 'face)
                 (get-text-property 6 'face))))
        (explicit-buffer-result
         (let ((buffer (get-buffer-create " *string-properties-explicit*")))
           (with-current-buffer buffer
             (erase-buffer)
             (insert styled))
           (prog1 (list (buffer-live-p buffer)
                        (get-text-property 1 'face buffer))
             (kill-buffer buffer)))))
    (princ "STRING-PROPERTIES-RESULT ")
    (prin1 (list end-boundaries
                 (list (get-text-property 0 'face removed)
                       (text-properties-at 0 removed))
                 (list (get-text-property 0 'face cleared)
                       (text-properties-at 0 cleared))
                 insert-result
                 explicit-buffer-result
                 (get-text-property 0 'face styled)))
    (terpri)))

;;; string-properties-regression.el ends here
