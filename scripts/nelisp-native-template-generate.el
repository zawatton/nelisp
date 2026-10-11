;;; nelisp-native-template-generate.el --- Offline labelled stencils -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-asm-x86_64)
(require 'nelisp-native-template-stencils)
(require 'nelisp-bytecode-compiler-input)
(defun nelisp-native-template-generate (&optional target check)
  "Generate TARGET fragments; CHECK proves receipts without writing artifacts.
Regenerate Win64: emacs -Q --batch -L lisp -L src -l this-file
--target windows-x86_64. Add --check for a bounded host-only development loop.
Linux generation preserves the historical library and pin byte for byte."
  (let* ((nelisp-native-load--build-target target)
         (win64 (eq target 'windows-x86_64))
         ;; The generator constructs the ABI without requiring an existing pin.
         (nelisp-native-template--selected-target (nelisp-native-load--target-v2))
         (nelisp-native-template-stencil-version
          (if win64 "template-x86_64-win64-v1" nelisp-native-template-stencil-version))
         (nelisp-native-template-fragment-abi
          (if win64 nelisp-native-template-win64-fragment-abi nelisp-native-template-fragment-abi))
         (fragments nil) (root (expand-file-name ".." (file-name-directory load-file-name))))
    (dolist (family '(prologue copy call poll fixnum-add fixnum-sub fixnum-mul fixnum-inc fixnum-dec fixnum-neg fixnum-eq fixnum-lt fixnum-gt fixnum-le fixnum-ge frame status-save status-restore status-branch nil-branch nonnull-branch jump switch return bad epilogue))
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
             (bridge (names)
               (if win64
                   (progn
                     ;; Same six-word ABI as the COFF in-house emitter: four
                     ;; registers, then two stack words above 32-byte shadow.
                     (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'r12)
                     (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'r13)
                     (imm 'r8 (nth 0 names)) (imm 'r9 (nth 1 names))
                     (imm 'rax (nth 2 names))
                     (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 32 'rax)
                     (imm 'rax (nth 3 names))
                     (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 40 'rax))
                 (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'r12)
                 (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'r13)
                 (cl-loop for reg in '(rdx rcx r8 r9) for name in names do (imm reg name))))
             (jump (zero target)
               (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
                 (if zero (nelisp-asm-x86_64-jz-rel32 buf target)
                   (nelisp-asm-x86_64-jnz-rel32 buf target))
                 (hole target 'rel32 (+ start 2))))
             (cc (code target)
               (nelisp-asm-x86_64-emit-bytes buf (unibyte-string 15 code))
               (hole target 'rel32 (nelisp-asm-x86_64-buffer-pos buf))
               (nelisp-asm-x86_64-emit-bytes buf (unibyte-string 0 0 0 0)))
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
             ;; Six pushes leave RSP=8 mod 16; 56 bytes provide alignment,
             ;; 32-byte shadow and two outgoing words. RDI/RSI and XMM6-15
             ;; are never written, so all Win64 nonvolatile registers survive.
             (nelisp-asm-x86_64-sub-imm32 buf 'rsp (if win64 56 8))
             (nelisp-asm-x86_64-mov-reg-reg buf 'r12 (if win64 'rcx 'rdi))
             (nelisp-asm-x86_64-mov-reg-reg buf 'r13 (if win64 'rdx 'rsi))
             (if win64
                 (progn
                   (dolist (r '(r8 r9 rax)) (nelisp-asm-x86_64-mov-imm32 buf r 0))
                   (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 32 'rax)
                   (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 40 'rax))
               (dolist (r '(rdx rcx r8 r9)) (nelisp-asm-x86_64-mov-imm32 buf r 0)))
             (external 'nl_root_pin_slot_v2)
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0) (jump t 'bad)
             (nelisp-asm-x86_64-mov-reg-reg buf 'r14 'rax))
            ('copy
             (dotimes (i 4)
               (memory 139 'rax 'r14 (intern (format "read%d" i)))
               (memory 137 'rax 'r14 (intern (format "write%d" i)))))
            ((or 'call 'poll)
             (bridge '(function arguments argc result))
             (external (if (eq family 'poll) 'nl_native_poll_v2 'nl_native_funcall_v2))
             (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0) (jump nil 'epilogue))
            ((or 'fixnum-add 'fixnum-sub 'fixnum-mul 'fixnum-inc 'fixnum-dec 'fixnum-neg
                 'fixnum-eq 'fixnum-lt 'fixnum-gt 'fixnum-le 'fixnum-ge)
             (let* ((binary (not (memq family '(fixnum-inc fixnum-dec fixnum-neg))))
                    (compare (memq family '(fixnum-eq fixnum-lt fixnum-gt fixnum-le fixnum-ge))))
               (memory 139 'rax 'r14 'left-tag)
               (nelisp-asm-x86_64-cmp-imm32 buf 'rax 2) (cc #x85 'slow)
               (memory 139 'rax 'r14 'left-value)
               (if binary
                   (progn (memory 139 'rcx 'r14 'right-tag)
                          (nelisp-asm-x86_64-cmp-imm32 buf 'rcx 2) (cc #x85 'slow)
                          (memory 139 'rcx 'r14 'right-value))
                 (nelisp-asm-x86_64-mov-imm32 buf 'rcx 1))
               (dolist (r (if binary '(rax rcx) '(rax)))
                 (nelisp-asm-x86_64-mov-imm64 buf 'rdx (if (eq family 'fixnum-mul) -1073741824 -2305843009213693952))
                 (nelisp-asm-x86_64-cmp-reg-reg buf r 'rdx) (cc #x8c 'slow)
                 (nelisp-asm-x86_64-mov-imm64 buf 'rdx (if (eq family 'fixnum-mul) 1073741824 2305843009213693951))
                 (nelisp-asm-x86_64-cmp-reg-reg buf r 'rdx) (cc #x8f 'slow))
               (pcase family
                 ((or 'fixnum-add 'fixnum-inc) (nelisp-asm-x86_64-add-reg-reg buf 'rax 'rcx))
                 ((or 'fixnum-sub 'fixnum-dec) (nelisp-asm-x86_64-sub-reg-reg buf 'rax 'rcx))
                 ('fixnum-neg (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x48 #xf7 #xd8)))
                 ('fixnum-mul (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x48 #x0f #xaf #xc1)))
                 (_ (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'rcx)))
               (if compare
                   (progn
                     ;; SETcc is a raw 0/1, matching the Nil/T Sexp tags.
                     (nelisp-asm-x86_64-emit-bytes buf
                       (unibyte-string 15 (pcase family ('fixnum-eq #x94) ('fixnum-lt #x9c)
                                            ('fixnum-gt #x9f) ('fixnum-le #x9e) (_ #x9d)) #xc2
                                       #x48 #x0f #xb6 #xd2))
                     (nelisp-asm-x86_64-mov-imm32 buf 'rax 0))
                 (nelisp-asm-x86_64-mov-imm64 buf 'rdx -2305843009213693952)
                 (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'rdx) (cc #x8c 'slow)
                 (nelisp-asm-x86_64-mov-imm64 buf 'rdx 2305843009213693951)
                 (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'rdx) (cc #x8f 'slow)
                 (nelisp-asm-x86_64-mov-imm32 buf 'rdx 2))
               (memory 137 'rdx 'r14 'output-tag) (memory 137 'rax 'r14 'output-value)
               (nelisp-asm-x86_64-mov-imm32 buf 'rdx 0)
               (memory 137 'rdx 'r14 'output-pad1) (memory 137 'rdx 'r14 'output-pad2)
               (let ((start (nelisp-asm-x86_64-buffer-pos buf)))
                 (nelisp-asm-x86_64-jmp-rel32 buf 'done) (hole 'done 'rel32 (1+ start)))))
            ('frame
             (bridge '(state action arguments result))
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
             (nelisp-asm-x86_64-add-imm32 buf 'rsp (if win64 56 8))
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
           (path (expand-file-name (if win64 "templates/nelisp-native-template-win64.nelst"
                                    "templates/nelisp-native-template.nelst") root)))
      ;; The historical SysV library and receipt are immutable. Regeneration
      ;; proves byte equality for EVERY fragment, then refreshes only the
      ;; compiler-source receipt; changing the compiler need not rewrite code.
      (unless win64
        (let* ((old (with-temp-buffer (set-buffer-multibyte nil)
                      (insert-file-contents-literally path) (buffer-string)))
               (library-old (nelisp-native-template--read-library old)))
          (unless (and (equal (secure-hash 'sha256 old) nelisp-native-template-library-sha256)
                       (equal (plist-get library-old :abi) (plist-get library :abi))
                       (equal (plist-get library-old :fragments) (plist-get library :fragments)))
            (error "SysV stencil non-regression refused"))))
      (if check
          (progn
            (require 'nelisp-native-template-source-pin)
            (unless (equal compiler-key nelisp-native-template-current-compiler-source-key)
              (error "Template compiler source receipt changed; regenerate"))
            (when win64
              (require 'nelisp-native-template-win64-pin)
              (unless (and (equal (secure-hash 'sha256 snapshot)
                                  nelisp-native-template-win64-library-sha256)
                           (equal snapshot (with-temp-buffer (set-buffer-multibyte nil)
                                             (insert-file-contents-literally path) (buffer-string))))
                (error "Win64 stencil/pin regeneration mismatch"))))
        (with-temp-file (expand-file-name "lisp/nelisp-native-template-source-pin.el" root)
          (insert ";;; nelisp-native-template-source-pin.el --- Generated compiler receipt -*- lexical-binding: t; -*-\n;; SPDX-License-Identifier: GPL-3.0-or-later\n")
          (insert (format "(defconst nelisp-native-template-current-compiler-source-key %S)\n" compiler-key))
          (insert "(provide 'nelisp-native-template-source-pin)\n"))
        (when win64
          (let ((coding-system-for-write 'no-conversion))
            (write-region snapshot nil path nil 'silent))
          (set-file-modes path #o600)
          (with-temp-file (expand-file-name "lisp/nelisp-native-template-win64-pin.el" root)
            (insert ";;; nelisp-native-template-win64-pin.el --- Generated Win64 stencil receipt -*- lexical-binding: t; -*-\n;; SPDX-License-Identifier: GPL-3.0-or-later\n")
            (dolist (pair `((nelisp-native-template-win64-library-sha256 . ,(secure-hash 'sha256 snapshot))
                            (nelisp-native-template-win64-library-source-key . ,source-key)
                            (nelisp-native-template-win64-library-inventory . ,inventory)))
              (insert (format "(defconst %s %S)\n" (car pair) (cdr pair))))
            (insert "(provide 'nelisp-native-template-win64-pin)\n"))))
      (princ (format "TEMPLATE-STENCILS-%s target=%s fragments=%d bytes=%d\n"
                     (if check "CHECKED" "GENERATED") (or target 'linux-x86_64)
                     (length (plist-get library :fragments)) (length snapshot))))))
(let ((target (when (equal (car command-line-args-left) "--target")
                (pop command-line-args-left)
                (intern (or (pop command-line-args-left) "missing")))))
  (unless (memq target '(nil linux-x86_64 windows-x86_64))
    (error "Unsupported stencil generation target %S" target))
  (let ((check (equal (car command-line-args-left) "--check")))
    (when check (pop command-line-args-left))
    (nelisp-native-template-generate target check)))
