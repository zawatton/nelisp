;;; nelisp-bytecode-native-boxed-op.el --- boxed bytecode operation gateway contract -*- lexical-binding: t; -*-

;;; Commentary:

;; The general boxed-op gateway is deliberately a contract checkpoint.  The
;; current runtime has no authenticated import with a rooted-Sexp input/output
;; ABI and an explicit status/exit result, so this module plans eligible
;; operations but never emits native code for them.

;;; Code:

(defconst nelisp-bytecode-native-boxed-op-gateway-version 2
  "Version of the proposed rooted boxed-operation gateway ABI.")

(defconst nelisp-bytecode-native-boxed-op-gateway-contract
  '(:version 2
    :entry-symbol "nelisp_bytecode_boxed_op_v2"
    :signature "uint32_t gateway(uint32_t abi, uint32_t opcode, void *frame_token, const uint32_t *input_slots, uint32_t input_count, uint32_t output_slot, uint32_t exit_slot)"
    :frame-layout ((uint32_t version)
                   (uint32_t slot_count)
                   (uint64_t generation)
                   (uint64_t runtime_cookie)
                   (NelnSexp **registered_slots))
    :frame-token (:runtime-issued t :active t :gc-registered t
                  :generation-checked t :cookie-checked t)
    :inputs (:representation root-slot-index :count-explicit t
              :object-pointers-escape nil)
    :output (:representation root-slot-index :preinitialized nil
             :read-only-on-nonzero-status t)
    :exit (:representation root-slot-index :contains-runtime-exit-record t
           :rooted-until-vm-handoff t)
    :statuses ((0 . success)
               (1 . wrong-type)
               (2 . malformed-op-or-frame)
               (3 . condition)
               (4 . quit)
               (5 . throw)
               (6 . unwind))
    :exit-record (:root-slot "exit_slot"
                   :condition "condition symbol and data"
                   :quit "quit condition data"
                   :throw "throw tag and value"
                   :unwind "pending unwind target")
    :malformed-bytecode compile-time-refusal
    :caller-on-exit vm-unwinder-required
    :allocation (:all-inputs-rooted-before-entry t
                 :output-root-reserved-before-entry t
                 :raw-moving-pointers-across-safepoints nil))
  "Proposed contract; it is not an implementation or runtime capability.

FRAME-TOKEN addresses a runtime-issued frame record with the exact fields in
`:frame-layout'. Its generation and cookie must match the active runtime
generation and live pinned frame. REGISTERED-SLOTS is an indexable table of
stable root-slot addresses; its values may move and must be reread after every
safepoint. Input, output, and exit references are indices into that table,
never raw object addresses. The gateway validates ABI/version, opcode arity,
frame identity, and every index before touching slots. The caller reserves and
registers all input, output, and exit slots before entry. It initializes the
output slot to nil and reads it only after status 0. A wrong-type status carries
the standard wrong-type condition in EXIT; condition/quit/throw/unwind statuses
carry the corresponding rooted exit record for the VM unwinder. Malformed
bytecode is rejected before native emission; status 2 is reserved for an
invalid runtime request. Allocation is allowed only through the runtime while
the registered roots remain active. The current loader does not authenticate
this entry or implement its exit handoff, so no operation is emitted yet.")

(defconst nelisp-bytecode-native-boxed-op--specs
  '((64 :name car :arity 1 :accepted-tags (nil cons)
        :wrong-type-status 1 :allocates nil)
    (65 :name cdr :arity 1 :accepted-tags (nil cons)
        :wrong-type-status 1 :allocates nil)
    (66 :name cons :arity 2 :accepted-tags any
        :wrong-type-status nil :allocates t))
  "Pinned GNU bytecode operation ids and their minimum gateway metadata.")

(defun nelisp-bytecode-native-boxed-op--instruction-spec (instruction)
  "Return the known boxed operation spec for INSTRUCTION, or nil."
  (when (eq (plist-get instruction :kind) 'primitive)
    (assq (plist-get instruction :opcode)
          nelisp-bytecode-native-boxed-op--specs)))

(defun nelisp-bytecode-native-boxed-op-plan (input)
  "Classify boxed operations in verified compiler INPUT without emitting code.

Malformed input remains `malformed'.  Known or unknown boxed primitives
remain `unsupported' until the authenticated v2 gateway is provided by the
runtime and its native-call/unwind path is verified."
  (cond
   ((not (and (listp input) (memq (plist-get input :status)
                                  '(complete malformed unsupported))))
    (list :status 'malformed :reason "invalid bytecode compiler input"))
   ((eq (plist-get input :status) 'malformed)
    (list :status 'malformed :reason (plist-get input :reason)))
   ((not (eq (plist-get input :status) 'complete))
    (list :status 'unsupported
          :reason (or (plist-get input :reason)
                      "bytecode verifier did not complete")))
   (t
    (let* ((blocks (plist-get (plist-get input :frame-result) :blocks))
           (operations nil)
           (unknown nil))
      (when (vectorp blocks)
        (dotimes (i (length blocks))
          (dolist (instruction
                   (append (plist-get (aref blocks i) :instructions) nil))
            (when (eq (plist-get instruction :kind) 'primitive)
              (let ((spec (nelisp-bytecode-native-boxed-op--instruction-spec
                           instruction)))
                (if spec
                    (push (list :opcode (car spec)
                                :name (plist-get (cdr spec) :name)
                                :arity (plist-get (cdr spec) :arity)
                                :accepted-tags
                                (plist-get (cdr spec) :accepted-tags)
                                :wrong-type-status
                                (plist-get (cdr spec) :wrong-type-status)
                                :allocates (plist-get (cdr spec) :allocates))
                          operations)
                  (push (plist-get instruction :opcode) unknown)))))))
      (setq operations (nreverse operations)
            unknown (nreverse unknown))
      (cond
       (unknown
        (list :status 'unsupported :reason "unknown boxed primitive opcode"
              :unknown-opcodes unknown
              :gateway-version nelisp-bytecode-native-boxed-op-gateway-version
              :contract nelisp-bytecode-native-boxed-op-gateway-contract))
       (operations
        (list :status 'unsupported
              :reason "authenticated rooted boxed-op v2 runtime gateway is unavailable"
              :operations operations
              :gateway-version nelisp-bytecode-native-boxed-op-gateway-version
              :contract nelisp-bytecode-native-boxed-op-gateway-contract))
       (t
        (list :status 'unsupported :reason "no boxed primitive operation found")))))))

(provide 'nelisp-bytecode-native-boxed-op)
;;; nelisp-bytecode-native-boxed-op.el ends here
