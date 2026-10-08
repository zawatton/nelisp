;;; nelisp-bytecode-native-cfg.el --- Native CFG proof slice -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lower the existing byte-code frame CFG directly to x86-64 instruction
;; bytes.  This proof slice admits constants, nil/t conditional edges, gotos
;; and returns.  It emits a freestanding raw-i64 body; it does not load or
;; publish that body as a Lisp callable.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-asm-x86_64)
(eval-when-compile (require 'nelisp-native-load))
(declare-function nelisp-native-load-raw-check "nelisp-native-load"
                  (manifest &optional name))

(defun nelisp-bytecode-native-cfg--unsupported (reason frame-ir)
  (list :status 'unsupported :reason reason :frame-ir frame-ir
        :execution 'not-loaded))

(defun nelisp-bytecode-native-cfg--label (pc)
  (intern (format "nelisp-bc-pc-%d" pc)))

(defun nelisp-bytecode-native-cfg--imm32-bytes (value)
  "Encode VALUE as little-endian signed/unsigned x86-64 imm32 bytes."
  (let ((word (logand value #xffffffff)))
    (unibyte-string (logand word #xff)
                    (logand (ash word -8) #xff)
                    (logand (ash word -16) #xff)
                    (logand (ash word -24) #xff))))

(defun nelisp-bytecode-native-cfg--constant (instruction constants)
  (let* ((index (plist-get instruction :constant-index))
         (value (and (integerp index) (<= 0 index) (< index (length constants))
                     (aref constants index))))
    (if (or (null value) (eq value t)
            (and (integerp value) (<= most-negative-fixnum value)
                 (<= value most-positive-fixnum)
                 (<= (- (expt 2 31)) value) (< value (expt 2 31))))
        (cons t (cond ((null value) 0) ((eq value t) 1) (t value)))
      (cons nil nil))))

(defun nelisp-bytecode-native-cfg--static-condition-p (input block constants)
  "Whether INPUT is locally derived from a nil/t constant in BLOCK."
  (let ((pc (and (consp input) (eq (car input) :value) (nth 1 input)))
        (seen nil) result)
    (while (and (integerp pc) (not (memq pc seen)) (not result))
      (push pc seen)
      (let ((producer
             (cl-find pc (append (plist-get block :instructions) nil)
                      :key (lambda (ins) (plist-get ins :pc)))))
        (cond
         ((null producer) (setq pc nil))
         ((eq (plist-get producer :kind) 'constant)
          (let* ((index (plist-get producer :constant-index))
                 (object (and (integerp index) (<= 0 index)
                              (< index (length constants))
                              (aref constants index))))
            (setq result
                  (and (car (nelisp-bytecode-native-cfg--constant producer constants))
                       (or (null object) (eq object t))))))
         ((memq (plist-get producer :kind) '(stack-ref dup))
          (let ((source (car (plist-get producer :inputs))))
            (setq pc (and (consp source) (eq (car source) :value)
                          (nth 1 source)))))
         (t (setq pc nil)))))
    result))

(defun nelisp-bytecode-native-cfg--edge-value-p (value)
  (and (consp value) (eq (car value) :value) (= (length value) 3)))

(defun nelisp-bytecode-native-cfg--boolean-values (blocks constants)
  "Return value IDs proven to contain only nil/t throughout BLOCKS.

The analysis starts from the optimistic solution and removes values whose
producer or any incoming edge is not boolean. This fixed point handles loop
backedges while keeping initial raw arguments unproven."
  (let ((known (make-hash-table :test #'equal))
        (incoming (make-hash-table :test #'equal)))
    (dolist (block (append blocks nil))
      (let ((start (plist-get block :start)))
        (cl-loop for slot below (plist-get block :entry-stack-depth)
                 for id = (list :entry start slot)
                 do (puthash id (/= start 0) known))
        (dolist (instruction (append (plist-get block :instructions) nil))
          (let* ((kind (plist-get instruction :kind))
                 (output (car (plist-get instruction :outputs)))
                 (initial
                  (pcase kind
                    ('constant
                     (let* ((index (plist-get instruction :constant-index))
                            (object (and (integerp index) (<= 0 index)
                                         (< index (length constants))
                                         (aref constants index))))
                       (or (null object) (eq object t))))
                    ((or 'stack-ref 'dup) t)
                    ('primitive (and (= (plist-get instruction :opcode) 63) t))
                    (_ nil))))
            (when output (puthash output initial known)))))
      (dolist (edge (append (plist-get block :successors) nil))
        (cl-mapc (lambda (source target)
                   (puthash target (cons source (gethash target incoming)) incoming))
                 (append (plist-get edge :slots) nil)
                 (append (plist-get edge :target-slots) nil))))
    (let ((changed t))
      (while changed
        (setq changed nil)
        (dolist (block (append blocks nil))
          (let ((start (plist-get block :start)))
            (dolist (instruction (append (plist-get block :instructions) nil))
              (let* ((kind (plist-get instruction :kind))
                     (output (car (plist-get instruction :outputs)))
                     (input (car (plist-get instruction :inputs))))
                (when (and output (gethash output known)
                           (or (not (memq kind '(constant stack-ref dup primitive)))
                               (and (eq kind 'primitive)
                                    (/= (plist-get instruction :opcode) 63))
                               (and (memq kind '(stack-ref dup primitive))
                                    (not (gethash input known)))))
                  (puthash output nil known)
                  (setq changed t))))
            (unless (= start 0)
              (cl-loop for slot below (plist-get block :entry-stack-depth)
                       for id = (list :entry start slot)
                       for sources = (gethash id incoming)
                       when (and (gethash id known)
                                 (or (null sources)
                                     (not (cl-every (lambda (source)
                                                      (gethash source known))
                                                    sources))))
                       do (puthash id nil known) (setq changed t))))))
    known)))

(defun nelisp-bytecode-native-cfg--file-sha256 (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (buffer-string))))

(defun nelisp-bytecode-native-cfg--current-binary-sha256 ()
  "Return the loader's identity for this process's running executable."
  (unless (eq system-type 'gnu/linux)
    (error "native-cfg: raw-v1 artifacts require Linux x86_64"))
  (require 'nelisp-native-load)
  (or (nelisp-native-load-running-binary-sha256)
      (error "native-cfg: running executable identity is unavailable")))

(defun nelisp-bytecode-native-cfg--switch-table-input-p (value block)
  "Whether VALUE is the table operand consumed by a switch in BLOCK."
  (cl-some (lambda (instruction)
            (and (eq (plist-get instruction :kind) 'switch)
                 (equal value (cadr (plist-get instruction :inputs)))))
          (append (plist-get block :instructions) nil)))

(defun nelisp-bytecode-native-cfg--integer-input-p (value block constants arity)
  "Prove VALUE comes from a raw argument or an integer constant."
  (let ((producer (cl-find value (append (plist-get block :instructions) nil)
                           :key (lambda (instruction)
                                  (car (plist-get instruction :outputs))))))
    (cond
     (producer
      (pcase (plist-get producer :kind)
        ('constant (let ((index (plist-get producer :constant-index)))
                     (and (integerp index) (<= 0 index) (< index (length constants))
                          (integerp (aref constants index)))))
        ((or 'stack-ref 'dup)
         (nelisp-bytecode-native-cfg--integer-input-p
          (car (plist-get producer :inputs)) block constants arity))
        (_ nil)))
     ((and (consp value) (eq (car value) :entry)
           (= (nth 1 value) 0) (integerp (nth 2 value))
           (< (nth 2 value) arity)) t)
     (t nil))))

(defun nelisp-bytecode-native-cfg--validate-frame
    (blocks constants boolean-values arity)
  "Return a reason if BLOCKS exceeds the raw-i64 slot proof slice."
  (let ((failure nil))
    (dolist (block (append blocks nil))
      (when (> (plist-get block :entry-stack-depth) 32)
        (setq failure "frame depth exceeds the 32-slot native frame limit"))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (pcase (plist-get instruction :kind)
          ('constant
           (unless (or (car (nelisp-bytecode-native-cfg--constant instruction constants))
                       (and (nelisp-bytecode-native-cfg--switch-table-input-p
                             (car (plist-get instruction :outputs)) block)
                            (let ((index (plist-get instruction :constant-index)))
                              (and (integerp index) (<= 0 index)
                                   (< index (length constants))
                                   (hash-table-p (aref constants index))))))
             (setq failure "boxed or out-of-range constant is unsupported")))
          ('switch
           (setq failure "Bswitch requires runtime table lookup in the rooted shared lane"))
          ((or 'stack-ref 'dup 'discard 'branch 'return) nil)
          ('primitive
           (unless (and (= (plist-get instruction :opcode) 63)
                        (gethash (car (plist-get instruction :inputs)) boolean-values))
             (setq failure "byte-not input is not proven nil/t")))
          (_ (setq failure
                   (format "boxed or effectful frame operation %S is unsupported"
                           (plist-get instruction :kind)))))
        (when (and (memq (plist-get instruction :opcode) '(131 132 133 134))
                   (not (gethash (car (plist-get instruction :inputs)) boolean-values)))
          (setq failure "conditional slot is not proven nil/t"))))
    failure))

(defun nelisp-bytecode-native-cfg--emit-block (buffer block constants frame-size)
  "Emit validated frame BLOCK using rsp-relative slots; return a failure reason."
  (let* ((instructions (append (plist-get block :instructions) nil))
         (last-ins (car (last instructions)))
         (op (and last-ins (plist-get last-ins :opcode)))
         (failure nil)
         (stack (cl-loop for i below (plist-get block :entry-stack-depth) collect i))
         (values (cl-loop for i below (plist-get block :entry-stack-depth)
                          collect (cons (list :entry (plist-get block :start) i) i))))
    (nelisp-asm-x86_64-define-label
     buffer (nelisp-bytecode-native-cfg--label (plist-get block :start)))
    (dolist (instruction instructions)
      (let* ((kind (plist-get instruction :kind))
             (input (car (plist-get instruction :inputs)))
             (source (cdr (assoc input values)))
             (output (car (plist-get instruction :outputs)))
             (slot (length stack)))
        (pcase kind
          ('constant
           (let* ((index (plist-get instruction :constant-index))
                  (object (and (integerp index) (<= 0 index)
                               (< index (length constants)) (aref constants index)))
                  (value (nelisp-bytecode-native-cfg--constant instruction constants)))
             ;; The table object is retained in a dead raw slot to preserve
             ;; the CFG stack layout; dispatch reads its frozen integer cases.
             (nelisp-asm-x86_64-mov-imm32 buffer 'rax (if (hash-table-p object) 0 (cdr value)))
             (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buffer (* 8 slot) 'rax)
             (setq values (cl-remove slot values :key #'cdr :test #'=))
             (setq stack (append stack (list slot)))
             (push (cons output slot) values)))
          ((or 'stack-ref 'dup)
           (unless (and (integerp source) (memq source stack))
             (setq failure "frame input does not resolve to a live slot"))
           (when (and (eq kind 'dup)
                      (not (equal source (car (last stack)))))
             (setq failure "dup input is not the top live slot"))
           (when (integerp source)
             (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buffer 'rax (* 8 source))
             (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buffer (* 8 slot) 'rax)
             (setq values (cl-remove slot values :key #'cdr :test #'=))
             (setq stack (append stack (list slot)))
             (push (cons output slot) values)))
          ('primitive
           (let ((top (car (last stack))))
             (unless (and (= (plist-get instruction :opcode) 63)
                          (integerp source) (equal source top))
               (setq failure "byte-not requires a live top boolean slot"))
             (when (integerp source)
               (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buffer 'rax (* 8 source))
               (nelisp-asm-x86_64-cmp-imm32 buffer 'rax 0)
               (nelisp-asm-x86_64-sete-al buffer)
               (nelisp-asm-x86_64-movzx-eax-al buffer)
               (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buffer (* 8 top) 'rax)
               (setq values (cl-remove top values :key #'cdr :test #'=))
               (push (cons output top) values))))
          ('discard
           (let ((discarded (car (last stack))))
             (setq stack (butlast stack)
                   values (cl-remove discarded values :key #'cdr :test #'=))))
          ('branch
           (unless (memq (plist-get instruction :opcode) '(130 131 132 133 134))
             (setq failure "branch opcode outside raw-i64 CFG slice"))
           (when (memq (plist-get instruction :opcode) '(131 132 133 134))
             (unless (and (integerp source) (memq source stack))
               (setq failure "branch input does not resolve to a live slot"))
             (unless (equal source (car (last stack)))
               (setq failure "conditional does not consume the top live slot"))
             (when (integerp source)
               (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buffer 'rax (* 8 source)))
             (when (memq (plist-get instruction :opcode) '(131 132))
               (let ((discarded (car (last stack))))
                 (setq stack (butlast stack)
                       values (cl-remove discarded values :key #'cdr :test #'=))))))
          ('switch
           (let* ((inputs (plist-get instruction :inputs))
                  (selector-slot (cdr (assoc (car inputs) values)))
                  (table-slot (cdr (assoc (cadr inputs) values)))
                  (switch-edges (cl-remove-if-not
                                 (lambda (edge) (eq (plist-get edge :kind) 'switch))
                                 (append (plist-get block :successors) nil)))
                  (index 0))
             (unless (and (integerp selector-slot) (integerp table-slot)
                          (memq selector-slot stack) (memq table-slot stack))
               (setq failure "Bswitch inputs do not resolve to live raw slots"))
             (unless failure
               (dolist (edge switch-edges)
                 (dolist (key (plist-get edge :keys))
                   (let ((next-label
                          (intern (format "nelisp-bc-switch-%d-%d"
                                          (plist-get instruction :pc) index))))
                     (nelisp-asm-x86_64-mov-reg-mem-rsp-disp
                      buffer 'rax (* 8 selector-slot))
                     (nelisp-asm-x86_64-cmp-imm32 buffer 'rax key)
                     (nelisp-asm-x86_64-jnz-rel32 buffer next-label)
                     (nelisp-asm-x86_64-jmp-rel32
                      buffer (nelisp-bytecode-native-cfg--label
                              (plist-get edge :target)))
                     (nelisp-asm-x86_64-define-label buffer next-label)
                     (setq index (1+ index)))))
               (let ((default (cl-find 'fallthrough
                                       (append (plist-get block :successors) nil)
                                       :key (lambda (edge) (plist-get edge :kind)))))
                 (unless default (setq failure "Bswitch default edge is unavailable"))
                 (when default
                   (nelisp-asm-x86_64-jmp-rel32
                    buffer (nelisp-bytecode-native-cfg--label
                            (plist-get default :target)))))
               (setq stack (butlast stack 2)
                     values (cl-remove selector-slot values :key #'cdr :test #'=)
                     values (cl-remove table-slot values :key #'cdr :test #'=)))))
          ('return
           (unless (and (integerp source) (memq source stack))
             (setq failure "return input does not resolve to a live slot"))
           (unless (equal source (car (last stack)))
             (setq failure "return does not consume the top live slot"))
           (when (integerp source)
             (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buffer 'rax (* 8 source))))
          (_ (setq failure (format "unsupported frame operation %S" kind))))))
    (unless failure
      (cond
       ((= op 130)
        (let ((edge (car (append (plist-get block :successors) nil))))
          (nelisp-asm-x86_64-jmp-rel32
           buffer (nelisp-bytecode-native-cfg--label (plist-get edge :target)))))
       ((memq op '(131 132 133 134))
        (let* ((successors (append (plist-get block :successors) nil))
               (taken (cl-find 'taken successors :key (lambda (edge) (plist-get edge :kind))))
               (fallthrough
                (cl-find 'fallthrough successors :key (lambda (edge) (plist-get edge :kind)))))
          (nelisp-asm-x86_64-cmp-imm32 buffer 'rax 0)
          (if (memq op '(131 133))
              (nelisp-asm-x86_64-jz-rel32
               buffer (nelisp-bytecode-native-cfg--label (plist-get taken :target)))
            (nelisp-asm-x86_64-jnz-rel32
             buffer (nelisp-bytecode-native-cfg--label (plist-get taken :target))))
          (nelisp-asm-x86_64-jmp-rel32
           buffer (nelisp-bytecode-native-cfg--label (plist-get fallthrough :target)))))
       ((= op 135)
        (nelisp-asm-x86_64-emit-bytes
         buffer (concat (unibyte-string #x48 #x81 #xc4)
                        (nelisp-bytecode-native-cfg--imm32-bytes frame-size)))
        (nelisp-asm-x86_64-pop buffer 'rbp)
        (nelisp-asm-x86_64-ret buffer))
       ((plist-get block :successors)
        (let ((edge (aref (plist-get block :successors) 0)))
          (nelisp-asm-x86_64-jmp-rel32
           buffer (nelisp-bytecode-native-cfg--label (plist-get edge :target)))))))
    failure))

(defun nelisp-bytecode-native-cfg-lower
    (code constants &optional initial-arity argument-reprs)
  "Lower supported CODE/CONSTANTS through frame IR to x86-64 bytes.

The result is an inspectable backend artifact, not an executable Lisp
function. Constants, stack-ref, dup, discard, nil/t conditional branches,
gotos, and returns use rsp-relative raw-i64 slots. INITIAL-ARITY names up to
six incoming integer registers; ARGUMENT-REPRS must explicitly be a list of
`raw-i64' entries for nonzero arity. Bswitch is limited to eq/eql tables
with signed-i32 integer keys and a proven raw integer selector. Boxed objects,
calls, and effectful operations remain unsupported."
  (cl-block nelisp-bytecode-native-cfg-lower
  (let* ((arity (or initial-arity 0))
         (arity-valid (and (integerp arity) (<= 0 arity) (<= arity 6)))
         (reprs-valid
          (or (and arity-valid (= arity 0) (null argument-reprs))
              (and arity-valid (> arity 0) (proper-list-p argument-reprs)
                   (= (length argument-reprs) arity)
                   (cl-every (lambda (repr) (eq repr 'raw-i64)) argument-reprs))))
         (frame-ir (nelisp-bytecode-frame-ir-build
                    code constants (if arity-valid arity 0)))
         (blocks (plist-get frame-ir :blocks))
         (boolean-values (and (eq (plist-get frame-ir :status) 'complete)
                              (nelisp-bytecode-native-cfg--boolean-values
                               blocks constants)))
         (buffer (nelisp-asm-x86_64-make-buffer))
         (unsupported nil)
         (frame-size (max 16 (* 16 (ceiling
                                    (or (plist-get frame-ir :max-stack-depth) 0)
                                    2)))))
    (unless arity-valid
      (setq unsupported "incoming raw-i64 arity must be an integer from 0 through 6"))
    (when (and arity-valid (not reprs-valid))
      (setq unsupported "incoming arguments require an explicit raw-i64 contract"))
    (unless (eq (plist-get frame-ir :status) 'complete)
      (cl-return-from nelisp-bytecode-native-cfg-lower
        (nelisp-bytecode-native-cfg--unsupported
         (or (plist-get frame-ir :reason) "frame CFG unavailable") frame-ir)))
    (unless unsupported
      (setq unsupported (nelisp-bytecode-native-cfg--validate-frame
                         blocks constants boolean-values arity)))
    (when (and (not unsupported)
               (> (plist-get frame-ir :max-stack-depth) 32))
      (setq unsupported "frame exceeds the 32-slot native frame limit"))
    (unless unsupported
      (nelisp-asm-x86_64-push buffer 'rbp)
      (nelisp-asm-x86_64-mov-reg-reg buffer 'rbp 'rsp)
      (nelisp-asm-x86_64-emit-bytes
       buffer (concat (unibyte-string #x48 #x81 #xec)
                      (nelisp-bytecode-native-cfg--imm32-bytes frame-size)))
      (cl-loop for index from 0 below arity
               for register in '(rdi rsi rdx rcx r8 r9)
               do (nelisp-asm-x86_64-mov-mem-rsp-disp-reg
                   buffer (* 8 index) register)))
    (dolist (block (append blocks nil))
      (unless unsupported
        (setq unsupported
              (nelisp-bytecode-native-cfg--emit-block buffer block constants frame-size))))
    (if unsupported
        (nelisp-bytecode-native-cfg--unsupported unsupported frame-ir)
      (let* ((bytes (nelisp-asm-x86_64-resolve-fixups buffer))
             (labels (nelisp-asm-x86_64-buffer-labels buffer))
             (pc-labels (mapcar (lambda (block)
                                  (cons (plist-get block :start)
                                        (nelisp-bytecode-native-cfg--label
                                         (plist-get block :start))))
                                (append blocks nil)))
             (pc-offsets (mapcar (lambda (entry)
                                   (cons (car entry) (cdr (assq (cdr entry) labels))))
                                 pc-labels))
             (branch-fixups
              (mapcar (lambda (fixup)
                        (let* ((slot (car fixup))
                               (label (cdr fixup))
                               (instruction-offset
                                (if (= (aref bytes (1- slot)) #xe9)
                                    (1- slot) (- slot 2)))
                               (target-pc
                                (car (rassq label pc-labels))))
                          (list :instruction-offset instruction-offset
                                :target-pc target-pc
                                :target-offset (cdr (assq target-pc pc-offsets)))))
                      (aref buffer 3))))
        (list :status 'complete :arity arity :argument-reprs argument-reprs
              :frame-ir frame-ir
              :backend (list :kind 'x86_64-cfg :value-repr 'raw-i64-frame-slots
                             :instructions (nelisp-asm-x86_64-buffer-pos buffer)
                             :labels pc-labels :branch-fixups branch-fixups)
              :pc-labels pc-labels :pc-offsets pc-offsets
              :branch-fixups branch-fixups :machine-bytes bytes
              :execution 'not-loaded :vm-trampoline nil))))))

(defun nelisp-bytecode-native-cfg-write-raw-v1
    (lowered artifact-path source-path export-name)
  "Write LOWERED as a closed raw-v1 artifact for SOURCE-PATH.

The artifact is pinned to the current runtime binary and exposes one
raw-i64 function with the arity proved by LOWERED, named EXPORT-NAME.
SOURCE-PATH is retained and
hashed for the loader's source-identity check."
  (unless (and (eq (plist-get lowered :status) 'complete)
               (stringp (plist-get lowered :machine-bytes))
               (stringp artifact-path) (stringp source-path)
               (file-readable-p source-path) (stringp export-name)
               (> (length export-name) 0)
               (integerp (plist-get lowered :arity))
               (<= 0 (plist-get lowered :arity))
               (<= (plist-get lowered :arity) 6))
    (error "native-cfg: invalid raw-v1 artifact inputs"))
  (require 'nelisp-native-load)
  (let* ((text (plist-get lowered :machine-bytes))
         (binary (nelisp-bytecode-native-cfg--current-binary-sha256))
         (exports (list (list :name export-name :value 0 :size (string-bytes text)
                              :type 'func :abi nelisp-native-load-raw-runtime-abi
                              :arity (plist-get lowered :arity) :return 'u64)))
         (base (list :format nelisp-native-load-raw-artifact-format
                     :kind 'raw-runtime
                     :runtime-abi nelisp-native-load-raw-runtime-abi
                     :layout-id nelisp-native-load-raw-layout-id
                     :arch nelisp-native-load-raw-supported-arch
                     :build-id "nelisp-bytecode-native-cfg-v1"
                     :binary-sha256 binary
                     :source (expand-file-name source-path)
                     :source-sha256
                     (nelisp-bytecode-native-cfg--file-sha256 source-path)
                     :runtime-opt-in t
                     :native (list :raw-abi nelisp-native-load-raw-runtime-abi
                                   :object-format nelisp-native-load-raw-object-format
                                   :text-size (string-bytes text)
                                   :text-base64 (base64-encode-string text t)
                                   :object-sha256 (secure-hash 'sha256 text)
                                   :object-size (string-bytes text)
                                   :exports exports :symbols exports
                                   :imports nil :relocs nil
                                   :data-size 0 :bss-size 0)))
         (manifest (append base
                           (list :artifact-sha256
                                 (secure-hash 'sha256 (prin1-to-string base))))))
    (unless binary
      (error "native-cfg: current runtime binary identity is unavailable"))
    (when (nelisp-native-load-raw-check manifest export-name)
      (error "native-cfg: raw-v1 manifest rejected: %S"
             (nelisp-native-load-raw-check manifest export-name)))
    (let ((parent (file-name-directory (expand-file-name artifact-path))))
      (when parent (make-directory parent t)))
    (with-temp-file artifact-path
      (insert ";;; NeLisp bytecode CFG raw-v1 artifact\n")
      (prin1 manifest (current-buffer))
      (insert "\n"))
    manifest))

(provide 'nelisp-bytecode-native-cfg)
;;; nelisp-bytecode-native-cfg.el ends here
