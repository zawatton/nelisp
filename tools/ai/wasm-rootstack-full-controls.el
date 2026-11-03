;;; wasm-rootstack-full-controls.el --- Genuine ownership controls -*- lexical-binding: t; -*-
(require 'wasm-rootstack-compose)

(defun wasm-rootstack-full-control (name)
  "Run the genuine check with one invalid input selected by NAME."
  (let* ((owners (copy-tree (plist-get (wasm-rootstack-authority) :owners)))
         (control (cl-find 'nl_root_pin_control owners :key (lambda (row) (plist-get row :name))))
         (region (cl-find 'nl_root_pin_region owners :key (lambda (row) (plist-get row :name)))))
    (pcase name
      ('ownership
       (nelisp-aot-compile-to-link-unit
        (wasm-rootstack-compose :owners (delq control owners) :full t)
        :arch 'wasm32 :format 'wasm))
      ('undersize
       (setf (plist-get control :size) 32)
       (wasm-rootstack-compose :owners owners :full t))
      ('alignment
       (cl-incf (plist-get region :offset) 8)
       (wasm-rootstack-compose :owners owners :full t))
      ('overlap
       (cl-incf (plist-get control :size) 8)
       (cl-incf (plist-get control :end) 8)
       (wasm-rootstack-compose :owners owners :full t))
      ('unresolved-input
       (wasm-rootstack-authority :layout-inputs nil))
      (_ (error "Unknown full-rootstack control: %S" name)))))

(defun wasm-rootstack-full-controls-run ()
  "Report one bounded rejection selected by WASM_ROOTSTACK_CONTROL."
  (let ((name (intern (or (getenv "WASM_ROOTSTACK_CONTROL") "ownership")))
        (print-length 6) (print-level 3))
    (condition-case err
        (progn
          (wasm-rootstack-full-control name)
          (princ (format "full-rootstack-control=%s unexpectedly accepted\n" name))
          (kill-emacs 2))
      (error
       (princ (format "full-rootstack-control=%s rejected=%s\n"
                      name (truncate-string-to-width (prin1-to-string err) 300)))
       (kill-emacs 1)))))

(provide 'wasm-rootstack-full-controls)
