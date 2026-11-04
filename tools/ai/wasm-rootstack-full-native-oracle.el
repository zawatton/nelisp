;;; wasm-rootstack-full-native-oracle.el --- Isolated native layout oracle -*- lexical-binding: t; -*-

(require 'cl-lib)
(defvar nelisp-standalone--target)

(defun wasm-rootstack-full-native-oracle (file names)
  "Load FILE as a native build and print actual BSS offsets for NAMES."
  (condition-case err
      (progn
        ;; Load by name like the native build, so load-prefer-newer picks a
        ;; newer .elc of the build script when one exists.
        (load (file-name-sans-extension file) nil t)
        ;; Evaluate under the explicit layout input target itself, so a
        ;; target-dependent redefinition cannot agree on Linux only.  The
        ;; parent removes NELISP_RUNTIME_RELOAD, so reload and callsite are
        ;; off exactly as the explicit inputs state.
        (let* ((nelisp-standalone--target 'wasm32)
               (unit (nelisp-standalone--arena-base-slot-unit))
               ;; nelisp-static-linker.el: nelisp-link-unit-make stores :symbols;
               ;; nelisp-link-symbol stores section-relative offsets in :value.
               (symbols (plist-get unit :symbols))
               (values
                (mapcar
                 (lambda (name)
                   (let ((matches (cl-remove-if-not
                                   (lambda (entry) (equal (plist-get entry :name) name))
                                   symbols)))
                     (unless (and (= (length matches) 1)
                                  (eq (plist-get (car matches) :section) 'bss)
                                  (integerp (plist-get (car matches) :value)))
                       (error "Invalid native boundary: %s" name))
                     (cons name (plist-get (car matches) :value))))
                 names))
               ;; Include the complete file-backed history, including dumped
               ;; libraries and this helper; nil entries have no backing file.
               (files (sort (delete-dups (delq nil (mapcar #'car load-history))) #'string<))
               (hashes
                (mapcar (lambda (path)
                          (cons path
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally path)
                                  (secure-hash 'sha256 (current-buffer)))))
                        files))
               (print-length nil) (print-level nil) (print-circle nil)
               (print-escape-newlines t))
          (princ "WASM-ROOTSTACK-NATIVE=")
          (prin1 (list :values values :files hashes))
          (terpri)))
    (error
     (princ (format "native-oracle-error=%s\n"
                    (truncate-string-to-width (error-message-string err) 300)))
     (kill-emacs 1))))

(provide 'wasm-rootstack-full-native-oracle)
