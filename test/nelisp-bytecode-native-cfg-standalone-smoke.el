;;; nelisp-bytecode-native-cfg-standalone-smoke.el --- Native CFG loader smoke -*- lexical-binding: t; -*-

;;; Code:

(require 'nelisp-bytecode-native-cfg)
(require 'nelisp-native-unit)

(defconst nelisp-bytecode-native-cfg-standalone-smoke--source
  (or load-file-name buffer-file-name))

;; The supplied standalone binary may already provide an older compiler
;; feature. Load the checkout's source explicitly so this smoke exercises the
;; implementation under test while retaining the binary's native loader.
(load (expand-file-name "../lisp/nelisp-bytecode-native-cfg.el"
                        (file-name-directory
                         nelisp-bytecode-native-cfg-standalone-smoke--source))
      nil t)

(defun nelisp-bytecode-native-cfg-standalone-smoke--one
    (directory condition-constant export-name expected)
  "Compile, stage, publish and call one nil/t CFG fixture."
  (let* ((code (unibyte-string 192 131 8 0 193 130 9 0 194 135))
         (constants (vector condition-constant 1 2))
         (frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (source-form
          (funcall (make-byte-code nil code constants
                                   (plist-get frame-ir :max-stack-depth) nil)))
         (artifact (expand-file-name (concat export-name ".nelr") directory))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact
           nelisp-bytecode-native-cfg-standalone-smoke--source export-name))
         (staged (nelisp-native-unit-stage artifact nil (list export-name))))
    (unless (eq (plist-get lowered :status) 'complete)
      (error "native-cfg: lower refused fixture: %S" (plist-get lowered :reason)))
    (unless (equal (nelisp-native-load-raw-check manifest export-name) nil)
      (error "native-cfg: generated raw-v1 manifest failed validation"))
    (unless (eq (plist-get staged :status) 'staged)
      (error "native-cfg: stage failed: %S" staged))
    (let* ((unit-id (plist-get staged :unit-id))
           (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
      (unless (eq (plist-get published :status) 'published)
        (error "native-cfg: publish failed: %S" published))
      (unless (= (nelisp-native-unit-call unit-id export-name nil) expected)
        (error "native-cfg: native result mismatch for %s" export-name)))
    (unless (= source-form expected)
      (error "native-cfg: VM/native disagreement for %s: VM=%S expected=%S"
             export-name source-form expected))
    (list :export export-name :native expected
          :artifact-sha256 (plist-get manifest :artifact-sha256))))

(defun nelisp-bytecode-native-cfg-standalone-smoke--multi-slot
    (directory condition-constant export-name expected)
  "Compile, stage, publish and call a multi-slot branch/join fixture."
  (let* ((code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (constants (vector 5 10 condition-constant 20 30))
         (frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (artifact (expand-file-name (concat export-name ".nelr") directory))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact nelisp-bytecode-native-cfg-standalone-smoke--source
           export-name))
         (staged (nelisp-native-unit-stage artifact nil (list export-name))))
    (unless (eq (plist-get lowered :status) 'complete)
      (error "native-cfg: multi-slot lower refused fixture: %S"
             (plist-get lowered :reason)))
    (unless (eq (plist-get staged :status) 'staged)
      (error "native-cfg: multi-slot stage failed: %S" staged))
    (message "native-cfg: multi-slot staged %s" export-name)
    (let* ((unit-id (plist-get staged :unit-id))
           (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
      (unless (eq (plist-get published :status) 'published)
        (error "native-cfg: multi-slot publish failed: %S" published))
      (unless (= (nelisp-native-unit-call unit-id export-name nil) expected)
        (error "native-cfg: multi-slot native mismatch for %s" export-name)))
    (message "native-cfg: multi-slot native call passed %s" export-name)
    (let ((source-form
           (funcall (make-byte-code nil code constants
                                    (plist-get frame-ir :max-stack-depth) nil))))
      (unless (= source-form expected)
        (error "native-cfg: multi-slot VM/native disagreement: VM=%S native=%S"
               source-form expected))
      (list :export export-name :vm source-form :native expected
            :artifact-sha256 (plist-get manifest :artifact-sha256)))))

(defun nelisp-bytecode-native-cfg-standalone-smoke--conditional-pop
    (directory opcode condition-constant export-name expected-native expected-vm)
  "Compile and compare one conditional-else-pop outcome."
  (let* ((code (unibyte-string 192 opcode 5 0 193 135))
         (constants (vector condition-constant 7))
         (frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (artifact (expand-file-name (concat export-name ".nelr") directory))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact nelisp-bytecode-native-cfg-standalone-smoke--source
           export-name))
         (staged (nelisp-native-unit-stage artifact nil (list export-name))))
    (unless (and (eq (plist-get lowered :status) 'complete)
                 (eq (plist-get staged :status) 'staged))
      (error "native-cfg: conditional-pop lower/stage failed: %S %S"
             (plist-get lowered :reason) staged))
    (let* ((unit-id (plist-get staged :unit-id))
           (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
      (unless (eq (plist-get published :status) 'published)
        (error "native-cfg: conditional-pop publish failed: %S" published))
      (unless (= (nelisp-native-unit-call unit-id export-name nil) expected-native)
        (error "native-cfg: conditional-pop native mismatch for %s" export-name)))
    (let ((source-form
           (funcall (make-byte-code nil code constants
                                    (plist-get frame-ir :max-stack-depth) nil))))
      (unless (equal source-form expected-vm)
        (error "native-cfg: conditional-pop VM mismatch for %s: %S"
               export-name source-form))
      (list :export export-name :vm source-form :native expected-native
            :artifact-sha256 (plist-get manifest :artifact-sha256)))))

(defun nelisp-bytecode-native-cfg-standalone-smoke--stack-op
    (directory code constants export-name expected &optional compare-vm)
  "Compile and execute one raw stack-ref/dup/discard fixture."
  (let* ((frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (stack-depth (plist-get frame-ir :max-stack-depth))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (artifact (expand-file-name (concat export-name ".nelr") directory))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact nelisp-bytecode-native-cfg-standalone-smoke--source
           export-name))
         (staged (nelisp-native-unit-stage artifact nil (list export-name))))
    (unless (and (eq (plist-get lowered :status) 'complete)
                 (eq (plist-get staged :status) 'staged))
      (error "native-cfg: stack-op lower/stage failed: %S %S"
             (plist-get lowered :reason) staged))
    (let* ((unit-id (plist-get staged :unit-id))
           (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
      (unless (eq (plist-get published :status) 'published)
        (error "native-cfg: stack-op publish failed: %S" published))
      (unless (= (nelisp-native-unit-call unit-id export-name nil) expected)
        (error "native-cfg: stack-op native mismatch for %s" export-name)))
    (let ((source-form (and compare-vm
                            (funcall (make-byte-code nil code constants
                                                     stack-depth nil)))))
      (when (and compare-vm (/= source-form expected))
        (error "native-cfg: stack-op VM/native mismatch: VM=%S native=%S"
               source-form expected))
      (list :export export-name :vm source-form :native expected
            :artifact-sha256 (plist-get manifest :artifact-sha256)))))

(defun nelisp-bytecode-native-cfg-standalone-smoke--raw-arguments
    (directory code arity arguments expected export-name)
  "Stage and compare a raw-i64 function with explicit incoming arguments."
  (let* ((constants [])
         (argument-reprs (make-list arity 'raw-i64))
         (frame-ir (nelisp-bytecode-frame-ir-build code constants arity))
         (lowered (nelisp-bytecode-native-cfg-lower
                   code constants arity argument-reprs))
         (stack-depth (max (1+ arity) (plist-get frame-ir :max-stack-depth)))
         (function (make-byte-code (* arity 257) code constants stack-depth nil))
         (vm-result (apply function arguments))
         (artifact (expand-file-name (concat export-name ".nelr") directory))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact
           nelisp-bytecode-native-cfg-standalone-smoke--source export-name))
         (staged (nelisp-native-unit-stage artifact nil (list export-name))))
    (unless (and (eq (plist-get lowered :status) 'complete)
                 (= (plist-get lowered :arity) arity)
                 (= (plist-get (car (plist-get (plist-get manifest :native)
                                               :exports))
                               :arity)
                    arity))
      (error "native-cfg: argument lower/manifest mismatch: %S" lowered))
    (unless (eq (plist-get staged :status) 'staged)
      (error "native-cfg: argument stage failed: %S" staged))
    (let* ((unit-id (plist-get staged :unit-id))
           (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
      (unless (eq (plist-get published :status) 'published)
        (error "native-cfg: argument publish failed: %S" published))
      (let ((native-result (nelisp-native-unit-call unit-id export-name arguments)))
        (unless (and (= native-result vm-result) (= vm-result expected))
          (error "native-cfg: argument VM/native mismatch: VM=%S native=%S"
                 vm-result native-result))
        (list :export export-name :arity arity :vm vm-result :native native-result
              :artifact-sha256 (plist-get manifest :artifact-sha256))))))

(defun nelisp-bytecode-native-cfg-standalone-smoke--terminating-loops (directory)
  "Stage and compare nested bytecode loops that each take one backedge."
  (let* ((code (unibyte-string 192 193 137 131 20 0
                               194 137 131 15 0 63 130 7 0
                               136 63 130 2 0 136 135))
         (constants [42 t t])
         (frame-ir (nelisp-bytecode-frame-ir-build code constants))
         (function (make-byte-code nil code constants
                                   (plist-get frame-ir :max-stack-depth) nil))
         (vm-result (funcall function))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (targets (mapcar (lambda (fixup) (plist-get fixup :target-pc))
                          (plist-get lowered :branch-fixups)))
         (artifact (expand-file-name "bc_cfg_nested_loops.nelr" directory)))
    (unless (and (eq (plist-get frame-ir :status) 'complete)
                 (= vm-result 42)
                 (eq (plist-get lowered :status) 'complete)
                 (memq 2 targets) (memq 7 targets))
      (error "native-cfg: terminating-loop VM/lower/fixup check failed: VM=%S lower=%S"
             vm-result lowered))
    (let* ((manifest
            (nelisp-bytecode-native-cfg-write-raw-v1
             lowered artifact
             nelisp-bytecode-native-cfg-standalone-smoke--source
             "bc_cfg_nested_loops"))
           (staged
            (nelisp-native-unit-stage artifact nil '("bc_cfg_nested_loops"))))
      (unless (eq (plist-get staged :status) 'staged)
        (error "native-cfg: terminating-loop stage failed: %S" staged))
      (let* ((unit-id (plist-get staged :unit-id))
             (published (nelisp-native-unit-publish (plist-get staged :candidate-id))))
        (unless (eq (plist-get published :status) 'published)
          (error "native-cfg: terminating-loop publish failed: %S" published))
        (let ((native-result
               (nelisp-native-unit-call unit-id "bc_cfg_nested_loops" nil)))
          (unless (= native-result vm-result)
            (error "native-cfg: terminating-loop VM/native mismatch: VM=%S native=%S"
                   vm-result native-result))
          (list :export "bc_cfg_nested_loops" :vm vm-result :native native-result
                :backedge-targets '(2 7)
                :artifact-sha256 (plist-get manifest :artifact-sha256)))))))

(let ((directory (make-temp-file "nelisp-native-cfg-smoke-" t)))
  (unwind-protect
      (condition-case err
          (let ((results
                 (if (getenv "NELISP_NATIVE_CFG_LOOP_ONLY")
                     (list
                      (nelisp-bytecode-native-cfg-standalone-smoke--terminating-loops
                       directory))
                   (list
                  (nelisp-bytecode-native-cfg-standalone-smoke--one
                   directory t "bc_cfg_true" 1)
                  (nelisp-bytecode-native-cfg-standalone-smoke--one
                   directory nil "bc_cfg_false" 2)
                  (nelisp-bytecode-native-cfg-standalone-smoke--multi-slot
                   directory t "bc_cfg_slots_true" 20)
                  (nelisp-bytecode-native-cfg-standalone-smoke--multi-slot
                   directory nil "bc_cfg_slots_false" 30)
                  (nelisp-bytecode-native-cfg-standalone-smoke--conditional-pop
                   directory 133 t "bc_cfg_133_true" 7 7)
                  (nelisp-bytecode-native-cfg-standalone-smoke--conditional-pop
                   directory 133 nil "bc_cfg_133_nil" 0 nil)
                  (nelisp-bytecode-native-cfg-standalone-smoke--conditional-pop
                   directory 134 t "bc_cfg_134_true" 1 t)
                  (nelisp-bytecode-native-cfg-standalone-smoke--conditional-pop
                   directory 134 nil "bc_cfg_134_nil" 7 7)
                  (nelisp-bytecode-native-cfg-standalone-smoke--stack-op
                   directory (unibyte-string 192 193 1 135) [5 10]
                   "bc_cfg_stack_ref" 5 t)
                  (nelisp-bytecode-native-cfg-standalone-smoke--stack-op
                   directory (unibyte-string 192 137 136 135) [5]
                   "bc_cfg_dup_discard" 5 t)
                  (nelisp-bytecode-native-cfg-standalone-smoke--raw-arguments
                   directory (unibyte-string 135) 1 '(11) 11 "bc_cfg_arg1_return")
                  (nelisp-bytecode-native-cfg-standalone-smoke--raw-arguments
                   directory (unibyte-string 1 137 135) 2 '(11 22)
                   11 "bc_cfg_arg2_stack_ref_dup")
                  (nelisp-bytecode-native-cfg-standalone-smoke--raw-arguments
                   directory (unibyte-string 135) 6 '(11 22 33 44 55 66)
                   66 "bc_cfg_arg6_return")
                  (nelisp-bytecode-native-cfg-standalone-smoke--terminating-loops
                   directory)))))
            (message "native-bytecode-cfg: PASS %S" results))
        (error
         (message "native-bytecode-cfg: FAIL %S" err)
         (kill-emacs 1)))
    (when (file-directory-p directory)
      (delete-directory directory t))))

;;; nelisp-bytecode-native-cfg-standalone-smoke.el ends here
