;;; nelisp-native-template-generate.el --- Offline labelled stencils -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-asm-x86_64)
(require 'nelisp-native-template-stencils)
(require 'nelisp-bytecode-compiler-input)
(defun nelisp-native-template-generate ()
  "Generate bounded protocol fragments, with positions recorded during emission."
  (let ((fragments nil) (root (expand-file-name ".." (file-name-directory load-file-name))))
    (dolist (family '(prologue copy call frame status-save status-restore status-branch nil-branch nonnull-branch jump switch return bad epilogue))
      (let ((buf (nelisp-asm-x86_64-make-buffer)) (holes nil))
        (cl-labels
            ((hole (name kind offset)
               (push (list :name name :kind kind :offset offset :width 4 :signed t :mask '(0 0 0 0)) holes))
             (imm (reg name)
               (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
                 (nelisp-asm-x86_64-mov-imm32 buf reg 0) (hole name 'imm32 (+ start 3))))
             (external (name)
               (nelisp-asm-x86_64-emit-bytes buf (unibyte-string 232))
               (hole name 'import-rel32 (nelisp-asm-x86_64-buffer-pos buf))
               (nelisp-asm-x86_64-reloc-plt32-here buf (symbol-name name) -4))
             (jump (zero target)
               (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
                 (if zero (nelisp-asm-x86_64-jz-rel32 buf target)
                   (nelisp-asm-x86_64-jnz-rel32 buf target))
                 (hole target 'rel32 (+ start 2))))
             (slot (name reg)
               ;; The authenticated root bank is nonmoving native storage.
               ;; Certificate bounds and the wrapper's complete slot checks
               ;; prove each constant root offset; no safepoint occurs here.
               (nelisp-asm-x86_64-mov-reg-reg buf reg 'r14)
               (imm 'rdx name)
               (nelisp-asm-x86_64-shl-reg-imm8 buf 'rdx 5)
               (nelisp-asm-x86_64-add-reg-reg buf reg 'rdx))
             (memory (opcode reg base name)
               ;; Fixed mod=10 disp32, explicitly labelled, never searched in bytes.
               (nelisp-asm-x86_64-emit-bytes
                buf (apply #'unibyte-string (list (nelisp-asm-x86_64--rex 1 (nelisp-asm-x86_64--reg-ext reg) 0
                                                (nelisp-asm-x86_64--reg-ext base))
                          opcode (nelisp-asm-x86_64--modrm 2 (nelisp-asm-x86_64--reg-low3 reg)
                                                        (nelisp-asm-x86_64--reg-low3 base)))))
               (hole name 'disp32 (nelisp-asm-x86_64-buffer-pos buf))
               (nelisp-asm-x86_64-emit-bytes buf (unibyte-string 0 0 0 0))))
          (pcase family
            ('prologue
             (dolist (r '(rbp rbx r12 r13 r14 r15)) (nelisp-asm-x86_64-push buf r))
             (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8)
             (nelisp-asm-x86_64-mov-reg-reg buf 'r12 'rdi)
             (nelisp-asm-x86_64-mov-reg-reg buf 'r13 'rsi)
             (dolist (r '(rdx rcx r8 r9)) (nelisp-asm-x86_64-mov-imm32 buf r 0))
             (external 'nl_root_pin_slot_v2)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0) (jump t 'bad)
             (nelisp-asm-x86_64-mov-reg-reg buf 'r14 'rax))
            ('copy
             (dotimes (i 4)
               (memory 139 'rax 'r14 (intern (format "read%d" i)))
               (memory 137 'rax 'r14 (intern (format "write%d" i)))))
            ('call
             (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'r12)
             (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'r13)
             (imm 'rdx 'function) (imm 'rcx 'arguments) (imm 'r8 'argc) (imm 'r9 'result)
             (external 'nl_native_funcall_v2)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0) (jump nil 'epilogue))
            ('frame
             (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'r12)
             (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'r13)
             (imm 'rdx 'state) (imm 'rcx 'action) (imm 'r8 'arguments) (imm 'r9 'result)
             (external 'nl_native_frame_v2)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0) (jump nil 'failure))
            ('status-save (nelisp-asm-x86_64-mov-reg-reg buf 'rbx 'rax))
            ('status-restore (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rbx))
            ('status-branch
             (imm 'rcx 'status)
             (nelisp-asm-x86_64-cmp-reg-reg buf 'rbx 'rcx) (jump nil 'target))
            ((or 'nil-branch 'nonnull-branch)
             (slot 'source 'r15)
             (memory 139 'rax 'r15 'tag-offset)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
             (jump (eq family 'nil-branch) 'target))
            ('jump
             (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
               (nelisp-asm-x86_64-jmp-rel32 buf 'target) (hole 'target 'rel32 (1+ start))))
            ('switch
             (slot 'source 'r15)
             (memory 139 'rax 'r15 'payload-offset)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax -1) (jump t 'target)
             ;; RIP-relative table of signed deltas from its own base. The
             ;; frozen switch provider already checked the PC/depth whitelist.
             (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x4c #x8d #x3d))
             (hole 'table 'rel32 (nelisp-asm-x86_64-buffer-pos buf))
             (nelisp-asm-x86_64-emit-bytes buf (unibyte-string 0 0 0 0 #x49 #x63 #x04 #x87 #x4c #x01 #xf8 #xff #xe0)))
            ('return
             (imm 'rax 'status)
             (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
               (nelisp-asm-x86_64-jmp-rel32 buf 'epilogue) (hole 'epilogue 'rel32 (1+ start))))
            ('bad (nelisp-asm-x86_64-mov-imm32 buf 'rax 2))
            ('epilogue
             (nelisp-asm-x86_64-add-imm32 buf 'rsp 8)
             (dolist (r '(r15 r14 r13 r12 rbx rbp)) (nelisp-asm-x86_64-pop buf r))
             (nelisp-asm-x86_64-ret buf)))
          (let ((fragment (list :abi nelisp-native-template-fragment-abi
                                :bytes (nelisp-asm-x86_64-buffer-bytes buf)
                                :holes (nreverse holes))))
            (nelisp-native-template-check-holes fragment)
            (push (cons family fragment) fragments)))))
    (let* ((source-key
            (nelisp-native-template--hash
             (mapcar (lambda (name)
                       (cons name (with-temp-buffer
                                    (set-buffer-multibyte nil)
                                    (insert-file-contents-literally (expand-file-name name root))
                                    (secure-hash 'sha256 (current-buffer)))))
                     '("scripts/nelisp-native-template-generate.el" "lisp/nelisp-native-template-stencils.el"
                       "lisp/nelisp-asm-x86_64.el" "lisp/nelisp-native-funcall-v2.el"
                       "lisp/nelisp-native-frame-v2.el" "lisp/nelisp-bytecode-cleanup.el"))))
           (compiler-key
            (nelisp-native-template--hash
             (mapcar (lambda (name)
                       (cons name (with-temp-buffer
                                    (set-buffer-multibyte nil)
                                    (insert-file-contents-literally (expand-file-name name root))
                                    (secure-hash 'sha256 (current-buffer)))))
                     '("lisp/nelisp-native-template.el" "lisp/nelisp-native-template-stencils.el"
                       "lisp/nelisp-bytecode-native-rooted-cfg-contract.el" "lisp/nelisp-native-cache.el"
                       "lisp/nelisp-native-load.el" "lisp/nelisp-runtime-reload-abi.el"
                       "lisp/nelisp-native-poll.el" "lisp/nelisp-bytecode-native-switch.el"
                       "lisp/nelisp-bytecode-compiler-input-dialect.el"))))
           (inventory (nelisp-bytecode-compiler-input-inventory-sha256))
           (library (list :abi (nelisp-native-template-stencil-abi) :source-key source-key
                          :opcode-inventory inventory :fragments (nreverse fragments)))
           (snapshot (concat (nelisp-native-template--print library) "\n"))
           (path (expand-file-name "templates/nelisp-native-template.nelst" root)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region snapshot nil path nil 'silent))
      (set-file-modes path #o600)
      (with-temp-file (expand-file-name "lisp/nelisp-native-template-pin.el" root)
        (insert ";;; nelisp-native-template-pin.el --- Generated stencil receipt -*- lexical-binding: t; -*-\n;; SPDX-License-Identifier: GPL-3.0-or-later\n")
        (dolist (pair `((nelisp-native-template-library-sha256 . ,(secure-hash 'sha256 snapshot))
                        (nelisp-native-template-library-source-key . ,source-key)
                        (nelisp-native-template-library-inventory . ,inventory)
                        (nelisp-native-template-compiler-source-key . ,compiler-key)))
          (insert (format "(defconst %s %S)\n" (car pair) (cdr pair))))
        (insert "(provide 'nelisp-native-template-pin)\n"))
      (princ (format "TEMPLATE-LIBRARY-GENERATED bytes=%d digest=%s\n" (length snapshot)
                     (secure-hash 'sha256 snapshot))))))
(nelisp-native-template-generate)
