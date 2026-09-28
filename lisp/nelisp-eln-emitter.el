;;; nelisp-eln-emitter.el --- pinned GNU .eln leaf emitter -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; This emitter targets one explicit GNU Emacs 31.1 x86-64 ABI profile. It
;; lowers an existing NeLisp AOT defun IR node in the supported subset
;; (immediate fixnum return, one-argument identity, or bounded one-argument
;; conditionals over argument/fixnum/nil values) into GNU Lisp_Object ABI
;; code and a real native-comp registration unit. This is not a general
;; compiler or portable ELN ABI.

;;; Code:

;; `--symbol-name' and `--top-level-code' now live in
;; `nelisp-eln-emitter-templates.el' (pure byte/string construction, no
;; dependency on either require below); required here so this file keeps
;; defining them under their original names for `nelisp-eln-emitter-write-ir'
;; and friends. See that file's commentary for why the split exists: it lets
;; `nelisp-eln-registration.el' authenticate a genuine GNU artifact's c-name
;; mangling and recognize a self-emitted fixture's top_level_run shape
;; without ever loading the two heavy requires immediately below.
(require 'nelisp-eln-emitter-templates)
(require 'nelisp-aot-compiler)
(require 'nelisp-elf-write)

(defconst nelisp-eln-emitter-gnu31-profile
  '(:producer-version "31.1"
    :target "x86_64-linux"
    :abi-hash "ba35c031"
    :register-subr-slot 1030)
  "Pinned profile observed from GNU Emacs 31.1 x86-64 native compilation.
The hash and import slot jointly identify the producer ABI; they are not
inferred from the ELF machine field and do not imply compatibility with
other Emacs builds.")

(defconst nelisp-eln-emitter--data-layout
  '((d_reloc 0 8)
    (d_reloc_eph 8 32)
    (current_thread_reloc 40 8)
    (f_symbols_with_pos_enabled_reloc 48 8)
    (freloc_link_table 56 8)
    (comp_unit 64 8))
  "Disjoint GNU ELF data cells used by this registration unit.")

(defun nelisp-eln-emitter--validate-data-layout ()
  "Signal if exported writable data-cell ranges overlap."
  (let ((end 0))
    (dolist (cell nelisp-eln-emitter--data-layout)
      (let ((start (nth 1 cell)) (size (nth 2 cell)))
        (unless (and (= start end) (> size 0))
          (error "ELN emitter: invalid or overlapping data cell %S" cell))
        (setq end (+ start size))))
    end))

(defun nelisp-eln-emitter--u64 (n)
  "Return N as eight little-endian bytes."
  (unless (and (integerp n) (<= 0 n) (< n 18446744073709551616))
    (error "ELN emitter: out-of-range u64 %S" n))
  (let ((bytes nil) (i 0))
    (while (< i 8)
      (push (logand (ash n (* -8 i)) 255) bytes)
      (setq i (1+ i)))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-eln-emitter--repeat-byte (byte count)
  "Return COUNT copies of raw BYTE."
  (let ((bytes nil))
    (while (> count 0)
      (push byte bytes)
      (setq count (1- count)))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-eln-emitter--static-object (value)
  "Serialize VALUE in GNU comp.c static_obj_t format."
  (let* ((payload (string-as-unibyte (prin1-to-string value)))
         (nul-payload (concat payload (unibyte-string 0))))
    (concat (nelisp-eln-emitter--u64 (length nul-payload)) nul-payload)))

(defun nelisp-eln-emitter--bytecode (value)
  "Return x86-64 SysV function bytes returning GNU fixnum VALUE."
  (unless (and (integerp value) (<= 0 value) (<= value #x1fffffff))
    (error "ELN emitter: only nonnegative 29-bit fixnums are supported"))
  (let ((word (logior (ash value 2) 2)))
    (apply #'unibyte-string
           (append '(#xb8)
                   (list (logand word 255)
                         (logand (ash word -8) 255)
                         (logand (ash word -16) 255)
                         (logand (ash word -24) 255))
                   '(#xc3)))))

(defun nelisp-eln-emitter--u64-immediate (value)
  "Return little-endian bytes for the GNU Lisp_Object fixnum VALUE."
  (unless (and (integerp value)
               (<= (- (ash 1 61)) value)
               (< value (ash 1 61)))
    (error "ELN emitter: fixnum outside GNU 64-bit range: %S" value))
  (nelisp-eln-emitter--u64
   (logand (logior (ash value 2) 2) #xffffffffffffffff)))

(defun nelisp-eln-emitter--patch-rel32 (code field target)
  "Patch CODE's signed rel32 at FIELD to point to TARGET."
  (let ((disp (- target (+ field 4))))
    (unless (and (<= (- (ash 1 31)) disp) (< disp (ash 1 31)))
      (error "ELN emitter: conditional branch displacement is out of range"))
    (concat (substring code 0 field)
            (apply #'unibyte-string
                   (mapcar (lambda (shift)
                             (logand (ash disp (- shift)) 255))
                           '(0 8 16 24)))
            (substring code (+ field 4)))))

(defun nelisp-eln-emitter--expression-bytecode (node parameter)
  "Emit bounded GNU Lisp_Object code for NODE and PARAMETER.
Keep the incoming argument in RDI while every expression result uses RAX."
  (unless (and (vectorp node) (> (length node) 0))
    (error "ELN emitter: expected expression IR node"))
  (pcase (nelisp-aot-compiler--ir-kind node)
    ('ref
     (unless (and (eq (nelisp-aot-compiler--ir-get node :var) parameter)
                  (eq (nelisp-aot-compiler--ir-get node :reg) 'rdi)
                  (equal (nelisp-aot-compiler--ir-get node :slot) 0)
                  (eq (nelisp-aot-compiler--ir-get node :class) 'gp))
       (error "ELN emitter: unsupported argument reference IR"))
     (unibyte-string #x48 #x89 #xf8))
    ('imm
     (unless (and (> (length node) 1) (eq (aref node 1) :value))
       (error "ELN emitter: immediate IR has no explicit value"))
     (let ((value (nelisp-aot-compiler--ir-get node :value)))
       (if (null value)
           (unibyte-string #x31 #xc0)
         (concat (unibyte-string #x48 #xb8)
                 (nelisp-eln-emitter--u64-immediate value)))))
    ('if
     (let* ((test (nelisp-eln-emitter--expression-bytecode
                   (nelisp-aot-compiler--ir-get node :test) parameter))
            (then (nelisp-eln-emitter--expression-bytecode
                   (nelisp-aot-compiler--ir-get node :then) parameter))
            (else (nelisp-eln-emitter--expression-bytecode
                   (nelisp-aot-compiler--ir-get node :else) parameter))
            (prefix (concat test (unibyte-string #x48 #x85 #xc0
                                                 #x0f #x84 0 0 0 0)
                             then (unibyte-string #xe9 0 0 0 0)))
            (else-start (length prefix))
            (end (+ else-start (length else)))
            (patched (nelisp-eln-emitter--patch-rel32 prefix
                                                       (+ (length test) 5)
                                                       else-start)))
       (setq patched
             (nelisp-eln-emitter--patch-rel32
              patched (+ (length test) 10 (length then)) end))
       (concat patched else)))
    (_ (error "ELN emitter: unsupported expression IR kind %S"
              (nelisp-aot-compiler--ir-kind node)))))

(defun nelisp-eln-emitter--profile-p (profile)
  "Validate PROFILE against the one pinned GNU producer ABI."
  (and (equal (plist-get profile :producer-version) "31.1")
       (equal (plist-get profile :target) "x86_64-linux")
       (equal (plist-get profile :abi-hash) "ba35c031")
       (= (plist-get profile :register-subr-slot) 1030)))

(defun nelisp-eln-emitter--validate-ir (ir)
  "Return (NAME ARITY KIND VALUE) for the supported AOT defun leaf IR."
  (unless (and (vectorp ir)
               (eq (nelisp-aot-compiler--ir-kind ir) 'defun)
               (symbolp (nelisp-aot-compiler--ir-get ir :name)))
    (error "ELN emitter: expected an AOT defun IR node"))
  (let* ((name (nelisp-aot-compiler--ir-get ir :name))
         (params (nelisp-aot-compiler--ir-get ir :params))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (cond
     ((and (null params)
           (not (nelisp-aot-compiler--ir-get ir :rest-p))
           (not (nelisp-aot-compiler--ir-get ir :variadic))
           (vectorp body)
           (eq (nelisp-aot-compiler--ir-kind body) 'imm)
           (integerp (nelisp-aot-compiler--ir-get body :value)))
      (list name 0 'constant (nelisp-aot-compiler--ir-get body :value)))
     ((and (consp params) (null (cdr params)) (symbolp (car params))
           (not (nelisp-aot-compiler--ir-get ir :rest-p))
           (not (nelisp-aot-compiler--ir-get ir :variadic))
           (equal (nelisp-aot-compiler--ir-get ir :param-regs) '(rdi))
           (eq (nelisp-aot-compiler--ir-get ir :param-class) 'gp)
           (equal (nelisp-aot-compiler--ir-get ir :param-classes) '(gp))
           (= (nelisp-aot-compiler--ir-get ir :fixed-param-count) 1)
           (vectorp body)
           (eq (nelisp-aot-compiler--ir-kind body) 'ref)
           (eq (nelisp-aot-compiler--ir-get body :var) (car params))
           (eq (nelisp-aot-compiler--ir-get body :reg) 'rdi)
           (= (nelisp-aot-compiler--ir-get body :slot) 0)
           (eq (nelisp-aot-compiler--ir-get body :class) 'gp))
     (list name 1 'identity nil))
     ((and (consp params) (null (cdr params)) (symbolp (car params))
           (not (nelisp-aot-compiler--ir-get ir :rest-p))
           (not (nelisp-aot-compiler--ir-get ir :variadic))
           (equal (nelisp-aot-compiler--ir-get ir :param-regs) '(rdi))
           (eq (nelisp-aot-compiler--ir-get ir :param-class) 'gp)
           (equal (nelisp-aot-compiler--ir-get ir :param-classes) '(gp))
           (= (nelisp-aot-compiler--ir-get ir :fixed-param-count) 1)
           (memq (nelisp-aot-compiler--ir-kind body) '(if imm ref)))
      ;; Validate by emitting once before the artifact-writing path begins.
      (list name 1 'expression
            (nelisp-eln-emitter--expression-bytecode body (car params))))
     (t
      (error "ELN emitter: unsupported AOT leaf IR shape")))))

(defun nelisp-eln-emitter-write-ir (ir output &optional profile)
  "Emit supported AOT defun IR as a genuine GNU 31.1 .eln at OUTPUT.
The pinned PROFILE must describe GNU Emacs 31.1 x86_64-linux with ABI
hash ba35c031.  The output contains an ELF ET_DYN, real comp static
metadata, the registered native subr, and top_level_run.  No Host Emacs
compiler is invoked.  OUTPUT must not already exist."
  (let* ((abi (or profile nelisp-eln-emitter-gnu31-profile))
         (entry (nelisp-eln-emitter--validate-ir ir))
         (name (nth 0 entry))
         (arity (nth 1 entry))
         (kind (nth 2 entry))
         (value (nth 3 entry))
         (c-name (nelisp-eln-emitter--symbol-name name))
         (output (expand-file-name output))
         (dir (file-name-directory output))
         (ld (or (executable-find "ld")
                 (error "ELN emitter: GNU ld is required")))
         (object nil)
         (linked nil)
         (eph (nelisp-eln-emitter--static-object
               (vector 0 name c-name '(0 nil nil))))
         (reloc (nelisp-eln-emitter--static-object [nil]))
         (hash (nelisp-eln-emitter--static-object "ba35c031"))
         (quality (nelisp-eln-emitter--static-object
                   '((native-comp-speed . 2)
                     (native-comp-debug . 0))))
         (fdoc (nelisp-eln-emitter--static-object [nil]))
         (rodata "")
         (blob-symbols nil)
         (data (make-string 72 0))
         (function-bytes (if (eq kind 'identity)
                             (unibyte-string #x48 #x89 #xf8 #xc3)
                           (if (eq kind 'expression)
                               (concat value (unibyte-string #xc3))
                             (nelisp-eln-emitter--bytecode value))))
         (top-offset (* 16 (ceiling (length function-bytes) 16)))
         (top (nelisp-eln-emitter--top-level-code arity))
         (text (concat function-bytes
                       (nelisp-eln-emitter--repeat-byte
                        #x90 (- top-offset (length function-bytes)))
                       (car top)))
         (relocs (cadr top))
         (ro-offset 0)
         (success nil))
    (unless (nelisp-eln-emitter--profile-p abi)
      (error "ELN emitter: unsupported GNU ABI profile %S" abi))
    (nelisp-eln-emitter--validate-data-layout)
    (when (file-exists-p output)
      (error "ELN emitter: refusing to overwrite %s" output))
    (unless (file-directory-p dir)
      (error "ELN emitter: output directory does not exist: %s" dir))
    ;; Patch the top-level call's registration-table displacement from the
    ;; profile's link-table slot.  The profile is hash-pinned above.
    (let ((slot (plist-get abi :register-subr-slot)))
      ;; The machine sequence carries 0x2030 for the pinned slot; fail if
      ;; layout changes rather than silently emitting a different call.
      (unless (= slot 1030)
        (error "ELN emitter: unsupported registration slot %S" slot))
      nil)
    ;; Symbol offsets are explicit and .rodata blobs are namespaced exactly
    ;; as comp.c's static-object convention requires.
    (dolist (spec `(("freloc_hash_blob" ,hash)
                    ("text_data_reloc_blob" ,reloc)
                    ("text_data_reloc_eph_blob" ,eph)
                    ("text_optim_qly_blob" ,quality)
                    ("text_data_fdoc_blob" ,fdoc)))
      (let* ((payload (cadr spec))
             (padding (mod (- 8 (mod ro-offset 8)) 8)))
        (when (> padding 0)
          (setq rodata
                (concat rodata (nelisp-eln-emitter--repeat-byte 0 padding))))
        (setq ro-offset (+ ro-offset padding))
        (push (list :name (car spec) :value ro-offset :size (length payload)
                    :section 'rodata :bind 'global :type 'object)
              blob-symbols)
        (setq rodata (concat rodata payload))
        (setq ro-offset (+ ro-offset (length payload)))))
    (setq relocs (mapcar (lambda (rel)
                           (plist-put rel :offset
                                      (+ top-offset (plist-get rel :offset))))
                         relocs))
    (let* ((symbols
            (append
             (list (list :name c-name :value 0 :size (length function-bytes)
                         :section 'text :bind 'global :type 'func)
                   (list :name "top_level_run" :value top-offset
                         :size (length (car top)) :section 'text
                         :bind 'global :type 'func)
                   (list :name "d_reloc" :value 0 :size 8
                         :section 'data :bind 'global :type 'object)
                   (list :name "d_reloc_eph" :value 8 :size 32
                         :section 'data :bind 'global :type 'object)
                   (list :name "current_thread_reloc" :value 40 :size 8
                         :section 'data :bind 'global :type 'object)
                   (list :name "f_symbols_with_pos_enabled_reloc" :value 48 :size 8
                         :section 'data :bind 'global :type 'object)
                   (list :name "freloc_link_table" :value 56 :size 8
                         :section 'data :bind 'global :type 'object)
                   (list :name "comp_unit" :value 64 :size 8
                         :section 'data :bind 'global :type 'object))
             (nreverse blob-symbols))))
      (unwind-protect
          (progn
            (setq object (make-temp-file (expand-file-name ".nelisp-eln-unit-" dir))
                  linked (make-temp-file (expand-file-name ".nelisp-eln-linked-" dir)))
            (nelisp-elf-write-binary
             object
             (list :e-type 'rel :machine 'x86_64
                   :text text :rodata rodata :data data
                   :symbols symbols :relocs relocs))
            (with-temp-buffer
              (unless (eq 0 (call-process ld nil t nil
                                          "-shared" "-Bsymbolic" "-z" "noexecstack"
                                          "--build-id=none"
                                          "-o" linked object))
                (error "ELN emitter: ld -shared failed: %s"
                       (buffer-string))))
            (rename-file linked output nil)
            (setq success t)
            output)
        (when object (ignore-errors (delete-file object)))
        (when (and linked (not success))
          (ignore-errors (delete-file linked)))))))

(provide 'nelisp-eln-emitter)

;;; nelisp-eln-emitter.el ends here
