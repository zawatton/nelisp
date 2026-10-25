;;; nelisp-codegen-state-production-test.el --- Producer context -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(defvar nelisp-state-production--out nil)
(defvar nelisp-state-production--objects nil)
(defun nelisp-state-production--object (original owner enabled)
  "Emit OWNER with the producer's live facts; relocate omitted callees.
Only unrelated emission is omitted.  These diagnostic ET_REL objects are
not runnable runtime closures and do not replace final linked validation."
  (let* ((buf (nelisp-asm-x86_64-make-buffer 'sysv))
         (nelisp-asm-x86_64-label-index (cons buf (make-hash-table :test #'eq)))
         (nelisp-aot-compiler--x86-assignment-prefix-caches enabled)
         (nelisp-aot-compiler--label-counter nelisp-aot-compiler--label-counter)
         (file (expand-file-name (if enabled "production-on.o" "production-off.o")
                                 nelisp-state-production--out))
         local external)
    (funcall original owner buf)
    (dolist (fix (aref buf 3))
      (if (assq (cdr fix) (nelisp-asm-x86_64-buffer-labels buf))
          (push fix local)
        ;; Never turn an unresolved private branch into a relocation.
        (should (member (symbol-name (cdr fix))
                        nelisp-aot-compiler--x86-trace-unit-names))
        (push (list :offset (car fix) :type 'pc32 :symbol (symbol-name (cdr fix))
                    :addend -4 :section 'text) external)))
    (aset buf 3 local)
    (let* ((text (nelisp-asm-x86_64-resolve-fixups buf))
           (name (symbol-name (nelisp-aot-compiler--ir-get owner :name)))
           (symbols (list (list :name name :value 0 :size (length text)
                                :section 'text :bind 'global :type 'func)))
           (relocs (append (nelisp-asm-x86_64-extract-relocs buf) external)))
      (dolist (reloc relocs)
        (let ((external-name (plist-get reloc :symbol)))
          (unless (cl-find external-name symbols :key (lambda (s) (plist-get s :name))
                           :test #'equal)
            (push (list :name external-name :value 0 :size 0
                        :section 'undef :bind 'global :type 'notype) symbols))))
      (nelisp-elf-write-binary file
                               (list :e-type 'rel :machine 'x86_64 :text text
                                     :symbols symbols :relocs relocs))
      (push (cons enabled text) nelisp-state-production--objects))))
(defun nelisp-state-production--emit (original owner _buf)
  (when (eq (nelisp-aot-compiler--ir-get owner :name) 'wf_bytecode_w_fast_run)
    (with-temp-file (expand-file-name "production-owner.el" nelisp-state-production--out)
      (prin1 owner (current-buffer)))
    (nelisp-state-production--object original owner nil)
    (nelisp-state-production--object original owner t)
    (throw 'nelisp-state-production-complete t)))
(ert-deftest nelisp-codegen-state-production/actual-owner-register-dispatch ()
  (let* ((retained (getenv "NELISP_STATE_PRODUCTION_OUT"))
         (nelisp-state-production--out
          (or retained (make-temp-file "nelisp-state-production-" t)))
         (nelisp-state-production--objects nil)
         (max-lisp-eval-depth (max max-lisp-eval-depth 40000))
         (process-environment (copy-sequence process-environment))
         (nelisp-aot-compiler--x86-direct-only-defuns
          nelisp-standalone--direct-only-reader-defuns)
         ;; This test pins the SAFE compare-tree contract; the ordered table
         ;; form is asserted in nelisp-bytecode-core-ordered-test.el.
         (nelisp-aot-compiler--x86-ordered-jump-tables-enabled nil))
    (setenv "NELISP_READER_DYNAMIC" "1")
    (setenv "NELISP_STANDALONE_TARGET" "linux-x86_64")
    (make-directory nelisp-state-production--out t)
    (advice-add 'nelisp-aot-compiler--emit-defun :around #'nelisp-state-production--emit)
    (unwind-protect
        (progn
          (should
           (catch 'nelisp-state-production-complete
             (nelisp-standalone--compile-to-unit
              "applyfn-reader.o"
              (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source)))
             nil))
          (let ((on (cdr (assq t nelisp-state-production--objects)))
                (off (cdr (assq nil nelisp-state-production--objects)))
                ;; mov rax,r8; cmp r8,93: the first opcode dispatch guard.
                (pattern (regexp-quote (unibyte-string #x49 #x89 #xc0 #x49 #x83 #xf8 #x5d))))
            (should on) (should off)
            (should (string-match-p pattern on))
            (should-not (string-match-p pattern off))))
      (advice-remove 'nelisp-aot-compiler--emit-defun #'nelisp-state-production--emit)
      (unless retained (delete-directory nelisp-state-production--out t)))))
(provide 'nelisp-codegen-state-production-test)
;;; nelisp-codegen-state-production-test.el ends here
