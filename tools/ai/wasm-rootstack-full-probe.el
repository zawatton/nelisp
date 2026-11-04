;;; wasm-rootstack-full-probe.el --- Compact full-rootstack diagnostic -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'wasm-rootstack-compose)
(let ((print-length 6) (print-level 3))
  (condition-case err
      (let* ((authority (wasm-rootstack-authority))
             (_oracle-line
              (let ((print-length nil) (print-level nil) (print-escape-newlines t))
                (princ (format "full-rootstack-native-oracle=%S\n"
                               (plist-get authority :native-oracle)))))
             (owners (plist-get authority :owners))
             (source
              (progn
                (dolist (summary (plist-get (plist-get authority :usage) :dynamic-accesses))
                  (princ (format "full-rootstack-dynamic owner=%s count=%d (bounds unproved)\n"
                                 (plist-get summary :owner) (plist-get summary :count))))
                (wasm-rootstack-compose :owners owners :full t)))
             (layout (wasm-rootstack-layout source))
             (out (or (getenv "WASM_ROOTSTACK_FULL_OUTPUT") "target/wasm-rootstack/full.wasm")))
        (make-directory (file-name-directory out) t)
        (let ((pin (plist-get layout :pin-region-alignment)))
          (princ (format "full-rootstack-layout pin-region-address=%d required=32 remainder=%d absolute-aligned=%s; %s\n"
                         (plist-get pin :address) (plist-get pin :remainder)
                         (plist-get pin :absolute-aligned) (plist-get pin :limit))))
        (nelisp-aot-compile-to-object source out :arch 'wasm32 :format 'wasm)
        (princ (format "full-rootstack=compiled %s\n" out)))
    (error
     (princ (format "full-rootstack-diagnostic=%s\n"
                    (truncate-string-to-width (prin1-to-string err) 300)))
     (kill-emacs 1))))
