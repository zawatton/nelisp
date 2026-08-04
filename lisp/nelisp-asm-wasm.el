;;; nelisp-asm-wasm.el --- wasm32 macro assembler primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 164 §2.2 / §6 P0 — minimal wasm encoder for the AOT object path.
;; The buffer discipline mirrors `nelisp-asm-x86_64.el': emit into an
;; opaque grow-only byte buffer, then finalize once.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-asm-wasm-error
  "nelisp-asm-wasm invariant violated")

(defconst nelisp-asm-wasm--i64 #x7e)
(defconst nelisp-asm-wasm--f64 #x7c)
(defconst nelisp-asm-wasm--i32 #x7f)
(defconst nelisp-asm-wasm--func #x60)
(defconst nelisp-asm-wasm--funcref #x70)
(defconst nelisp-asm-wasm--empty-block-type #x40)
(defconst nelisp-asm-wasm--extern-func-kind #x00)
(defconst nelisp-asm-wasm--table-kind #x01)
(defconst nelisp-asm-wasm--mem-kind #x02)
(defconst nelisp-asm-wasm--global-kind #x03)

(defvar nelisp-asm-wasm--eh-dialect 'legacy
  "WebAssembly exception-handling dialect to encode.")

(defun nelisp-asm-wasm-make-buffer ()
  "Return a fresh wasm byte buffer."
  (vector nil 0))

(defsubst nelisp-asm-wasm--chunks (buf)
  (aref buf 0))

(defsubst nelisp-asm-wasm--length (buf)
  (aref buf 1))

(defun nelisp-asm-wasm-buffer-pos (buf)
  "Return BUF's current byte length."
  (nelisp-asm-wasm--length buf))

(defun nelisp-asm-wasm-buffer-bytes (buf)
  "Finalize BUF into one unibyte string."
  (apply #'concat (nreverse (nelisp-asm-wasm--chunks buf))))

(defun nelisp-asm-wasm--push-bytes (buf bytes)
  "Append BYTES to BUF."
  (aset buf 0 (cons bytes (nelisp-asm-wasm--chunks buf)))
  (aset buf 1 (+ (nelisp-asm-wasm--length buf) (length bytes))))

(defun nelisp-asm-wasm-emit-byte (buf byte)
  "Append BYTE to BUF."
  (nelisp-asm-wasm--push-bytes buf (unibyte-string (logand byte #xff))))

(defun nelisp-asm-wasm-emit-bytes (buf bytes)
  "Append BYTES to BUF."
  (nelisp-asm-wasm--push-bytes buf bytes))

(defun nelisp-asm-wasm-uleb128-bytes (value)
  "Encode VALUE as unsigned LEB128."
  (unless (and (integerp value) (>= value 0))
    (signal 'nelisp-asm-wasm-error (list :uleb128-out-of-range value)))
  (let ((n value)
        (bytes nil)
        (done nil))
    (while (not done)
      (let ((byte (logand n #x7f)))
        (setq n (ash n -7))
        (when (> n 0)
          (setq byte (logior byte #x80)))
        (push byte bytes)
        (setq done (zerop n))))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-asm-wasm-sleb128-bytes (value)
  "Encode VALUE as signed LEB128."
  (unless (integerp value)
    (signal 'nelisp-asm-wasm-error (list :sleb128-not-integer value)))
  (let ((n value)
        (bytes nil)
        done)
    (while (not done)
      (let* ((byte (logand n #x7f))
             (sign-bit (not (zerop (logand byte #x40)))))
        (setq n (ash n -7))
        (setq done (or (and (zerop n) (not sign-bit))
                       (and (= n -1) sign-bit)))
        (unless done
          (setq byte (logior byte #x80)))
        (push byte bytes)))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-asm-wasm-emit-uleb128 (buf value)
  "Append unsigned LEB128 VALUE to BUF."
  (nelisp-asm-wasm-emit-bytes buf (nelisp-asm-wasm-uleb128-bytes value)))

(defun nelisp-asm-wasm-emit-sleb128 (buf value)
  "Append signed LEB128 VALUE to BUF."
  (nelisp-asm-wasm-emit-bytes buf (nelisp-asm-wasm-sleb128-bytes value)))

(defun nelisp-asm-wasm-emit-name (buf name)
  "Append wasm NAME string to BUF."
  (let ((bytes (encode-coding-string name 'utf-8 t)))
    (nelisp-asm-wasm-emit-uleb128 buf (length bytes))
    (nelisp-asm-wasm-emit-bytes buf bytes)))

(defun nelisp-asm-wasm-emit-func-type (buf params results)
  "Append one function type with PARAMS and RESULTS to BUF."
  (nelisp-asm-wasm-emit-byte buf nelisp-asm-wasm--func)
  (nelisp-asm-wasm-emit-uleb128 buf (length params))
  (dolist (param params)
    (nelisp-asm-wasm-emit-byte buf param))
  (nelisp-asm-wasm-emit-uleb128 buf (length results))
  (dolist (result results)
    (nelisp-asm-wasm-emit-byte buf result)))

(defun nelisp-asm-wasm-emit-section (buf id payload)
  "Append section ID with PAYLOAD to BUF."
  (nelisp-asm-wasm-emit-byte buf id)
  (nelisp-asm-wasm-emit-uleb128 buf (length payload))
  (nelisp-asm-wasm-emit-bytes buf payload))

(defun nelisp-asm-wasm-emit-local-decls (buf local-types)
  "Append local declarations for LOCAL-TYPES to BUF."
  (if (null local-types)
      (nelisp-asm-wasm-emit-uleb128 buf 0)
    (let ((groups nil)
          (cur-type nil)
          (cur-count 0))
      (dolist (type local-types)
        (if (eq type cur-type)
            (setq cur-count (1+ cur-count))
          (when cur-type
            (push (cons cur-count cur-type) groups))
          (setq cur-type type
                cur-count 1)))
      (when cur-type
        (push (cons cur-count cur-type) groups))
      (setq groups (nreverse groups))
      (nelisp-asm-wasm-emit-uleb128 buf (length groups))
      (dolist (group groups)
        (nelisp-asm-wasm-emit-uleb128 buf (car group))
        (nelisp-asm-wasm-emit-byte buf (cdr group))))))

(defun nelisp-asm-wasm-make-function-body (local-types expr-bytes)
  "Build one function body from LOCAL-TYPES and EXPR-BYTES."
  (let ((payload (nelisp-asm-wasm-make-buffer))
        (body (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-local-decls payload local-types)
    (nelisp-asm-wasm-emit-bytes payload expr-bytes)
    (let ((payload-bytes (nelisp-asm-wasm-buffer-bytes payload)))
      (nelisp-asm-wasm-emit-uleb128 body (length payload-bytes))
      (nelisp-asm-wasm-emit-bytes body payload-bytes))
    (nelisp-asm-wasm-buffer-bytes body)))

(defun nelisp-asm-wasm-op-i64-const (buf value)
  "Emit `i64.const VALUE'."
  (nelisp-asm-wasm-emit-byte buf #x42)
  (nelisp-asm-wasm-emit-sleb128 buf value))

(defun nelisp-asm-wasm-op-i64-add (buf)
  "Emit `i64.add'."
  (nelisp-asm-wasm-emit-byte buf #x7c))

(defun nelisp-asm-wasm-op-i64-sub (buf)
  "Emit `i64.sub'."
  (nelisp-asm-wasm-emit-byte buf #x7d))

(defun nelisp-asm-wasm-op-i64-mul (buf)
  "Emit `i64.mul'."
  (nelisp-asm-wasm-emit-byte buf #x7e))

(defun nelisp-asm-wasm-op-local-get (buf index)
  "Emit `local.get INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x20)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-local-set (buf index)
  "Emit `local.set INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x21)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-local-tee (buf index)
  "Emit `local.tee INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x22)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-call (buf index)
  "Emit `call INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x10)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-unreachable (buf)
  "Emit `unreachable'."
  (nelisp-asm-wasm-emit-byte buf #x00))

(defun nelisp-asm-wasm-op-call-indirect (buf type-index &optional table-index)
  "Emit `call_indirect TYPE-INDEX TABLE-INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x11)
  (nelisp-asm-wasm-emit-uleb128 buf type-index)
  (nelisp-asm-wasm-emit-uleb128 buf (or table-index 0)))

(defun nelisp-asm-wasm-op-drop (buf)
  "Emit `drop'."
  (nelisp-asm-wasm-emit-byte buf #x1a))

(defun nelisp-asm-wasm-op-block (buf &optional result-type)
  "Emit `block' with RESULT-TYPE or empty block type."
  (nelisp-asm-wasm-emit-byte buf #x02)
  (nelisp-asm-wasm-emit-byte
   buf (or result-type nelisp-asm-wasm--empty-block-type)))

(defun nelisp-asm-wasm-op-loop (buf &optional result-type)
  "Emit `loop' with RESULT-TYPE or empty block type."
  (nelisp-asm-wasm-emit-byte buf #x03)
  (nelisp-asm-wasm-emit-byte
   buf (or result-type nelisp-asm-wasm--empty-block-type)))

(defun nelisp-asm-wasm-op-if (buf &optional result-type)
  "Emit `if' with RESULT-TYPE or empty block type."
  (nelisp-asm-wasm-emit-byte buf #x04)
  (nelisp-asm-wasm-emit-byte
   buf (or result-type nelisp-asm-wasm--empty-block-type)))

(defun nelisp-asm-wasm-op-try (buf &optional result-type)
  "Emit legacy `try' with RESULT-TYPE or empty block type."
  (nelisp-asm-wasm-emit-byte buf #x06)
  (nelisp-asm-wasm-emit-byte
   buf (or result-type nelisp-asm-wasm--empty-block-type)))

(defun nelisp-asm-wasm-op-catch (buf tag-index)
  "Emit legacy `catch TAG-INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x07)
  (nelisp-asm-wasm-emit-uleb128 buf tag-index))

(defun nelisp-asm-wasm-op-throw (buf tag-index)
  "Emit legacy `throw TAG-INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x08)
  (nelisp-asm-wasm-emit-uleb128 buf tag-index))

(defun nelisp-asm-wasm-op-rethrow (buf depth)
  "Emit legacy `rethrow DEPTH'."
  (nelisp-asm-wasm-emit-byte buf #x09)
  (nelisp-asm-wasm-emit-uleb128 buf depth))

(defun nelisp-asm-wasm-op-throw-ref (buf)
  "Emit standardized `throw_ref'."
  (nelisp-asm-wasm-emit-byte buf #x0a))

(defun nelisp-asm-wasm-op-else (buf)
  "Emit `else'."
  (nelisp-asm-wasm-emit-byte buf #x05))

(defun nelisp-asm-wasm-op-br (buf depth)
  "Emit `br DEPTH'."
  (nelisp-asm-wasm-emit-byte buf #x0c)
  (nelisp-asm-wasm-emit-uleb128 buf depth))

(defun nelisp-asm-wasm-op-br-if (buf depth)
  "Emit `br_if DEPTH'."
  (nelisp-asm-wasm-emit-byte buf #x0d)
  (nelisp-asm-wasm-emit-uleb128 buf depth))

(defun nelisp-asm-wasm-op-try-table (buf &optional result-type catches)
  "Emit standardized `try_table' with RESULT-TYPE and CATCHES."
  (nelisp-asm-wasm-emit-byte buf #x1f)
  (nelisp-asm-wasm-emit-byte
   buf (or result-type nelisp-asm-wasm--empty-block-type))
  (nelisp-asm-wasm-emit-uleb128 buf (length catches))
  (dolist (clause catches)
    (nelisp-asm-wasm-emit-byte buf (plist-get clause :kind))
    (when (plist-member clause :tag-index)
      (nelisp-asm-wasm-emit-uleb128 buf (plist-get clause :tag-index)))
    (nelisp-asm-wasm-emit-uleb128 buf (plist-get clause :label-depth))))

(defun nelisp-asm-wasm-op-return (buf)
  "Emit `return'."
  (nelisp-asm-wasm-emit-byte buf #x0f))

(defun nelisp-asm-wasm-op-delegate (buf depth)
  "Emit legacy `delegate DEPTH'."
  (nelisp-asm-wasm-emit-byte buf #x18)
  (nelisp-asm-wasm-emit-uleb128 buf depth))

(defun nelisp-asm-wasm-op-catch-all (buf)
  "Emit legacy `catch_all'."
  (nelisp-asm-wasm-emit-byte buf #x19))

(defun nelisp-asm-wasm-op-i32-wrap-i64 (buf)
  "Emit `i32.wrap_i64'."
  (nelisp-asm-wasm-emit-byte buf #xa7))

(defun nelisp-asm-wasm-op-memory-size (buf)
  "Emit `memory.size'."
  (nelisp-asm-wasm-emit-byte buf #x3f)
  (nelisp-asm-wasm-emit-byte buf 0))

(defun nelisp-asm-wasm-op-memory-grow (buf)
  "Emit `memory.grow'."
  (nelisp-asm-wasm-emit-byte buf #x40)
  (nelisp-asm-wasm-emit-byte buf 0))

(defun nelisp-asm-wasm-op-i32-const (buf value)
  "Emit `i32.const VALUE'."
  (nelisp-asm-wasm-emit-byte buf #x41)
  (nelisp-asm-wasm-emit-sleb128 buf value))

(defun nelisp-asm-wasm-op-i32-eq (buf)
  "Emit `i32.eq'."
  (nelisp-asm-wasm-emit-byte buf #x46))

(defun nelisp-asm-wasm-op-i32-gt-u (buf)
  "Emit `i32.gt_u'."
  (nelisp-asm-wasm-emit-byte buf #x4b))

(defun nelisp-asm-wasm-op-i32-add (buf)
  "Emit `i32.add'."
  (nelisp-asm-wasm-emit-byte buf #x6a))

(defun nelisp-asm-wasm-op-i32-sub (buf)
  "Emit `i32.sub'."
  (nelisp-asm-wasm-emit-byte buf #x6b))

(defun nelisp-asm-wasm-op-i32-and (buf)
  "Emit `i32.and'."
  (nelisp-asm-wasm-emit-byte buf #x71))

(defun nelisp-asm-wasm-op-i32-shl (buf)
  "Emit `i32.shl'."
  (nelisp-asm-wasm-emit-byte buf #x74))

(defun nelisp-asm-wasm-op-global-get (buf index)
  "Emit `global.get INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x23)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-global-set (buf index)
  "Emit `global.set INDEX'."
  (nelisp-asm-wasm-emit-byte buf #x24)
  (nelisp-asm-wasm-emit-uleb128 buf index))

(defun nelisp-asm-wasm-op-i64-extend-i32-u (buf)
  "Emit `i64.extend_i32_u'."
  (nelisp-asm-wasm-emit-byte buf #xad))

(defun nelisp-asm-wasm-op-i64-eqz (buf)
  "Emit `i64.eqz'."
  (nelisp-asm-wasm-emit-byte buf #x50))

(defun nelisp-asm-wasm-op-i64-eq (buf)
  "Emit `i64.eq'."
  (nelisp-asm-wasm-emit-byte buf #x51))

(defun nelisp-asm-wasm-op-i64-ne (buf)
  "Emit `i64.ne'."
  (nelisp-asm-wasm-emit-byte buf #x52))

(defun nelisp-asm-wasm-op-i64-lt-s (buf)
  "Emit `i64.lt_s'."
  (nelisp-asm-wasm-emit-byte buf #x53))

(defun nelisp-asm-wasm-op-i64-gt-s (buf)
  "Emit `i64.gt_s'."
  (nelisp-asm-wasm-emit-byte buf #x55))

(defun nelisp-asm-wasm-op-i64-le-s (buf)
  "Emit `i64.le_s'."
  (nelisp-asm-wasm-emit-byte buf #x57))

(defun nelisp-asm-wasm-op-i64-ge-s (buf)
  "Emit `i64.ge_s'."
  (nelisp-asm-wasm-emit-byte buf #x59))

(defun nelisp-asm-wasm-op-i64-div-s (buf)
  "Emit `i64.div_s'."
  (nelisp-asm-wasm-emit-byte buf #x7f))

(defun nelisp-asm-wasm-op-i64-rem-s (buf)
  "Emit `i64.rem_s'."
  (nelisp-asm-wasm-emit-byte buf #x81))

(defun nelisp-asm-wasm-op-select (buf)
  "Emit `select'."
  (nelisp-asm-wasm-emit-byte buf #x1b))

(defun nelisp-asm-wasm-op-i64-load (buf &optional align offset)
  "Emit `i64.load' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x29)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 3))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i32-load (buf &optional align offset)
  "Emit `i32.load' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x28)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 2))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-load8-u (buf &optional align offset)
  "Emit `i64.load8_u' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x31)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 0))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-load16-u (buf &optional align offset)
  "Emit `i64.load16_u' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x33)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 1))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-load32-u (buf &optional align offset)
  "Emit `i64.load32_u' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x35)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 2))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-store (buf &optional align offset)
  "Emit `i64.store' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x37)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 3))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i32-store (buf &optional align offset)
  "Emit `i32.store' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x36)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 2))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-store8 (buf &optional align offset)
  "Emit `i64.store8' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x3c)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 0))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-store16 (buf &optional align offset)
  "Emit `i64.store16' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x3d)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 1))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-i64-store32 (buf &optional align offset)
  "Emit `i64.store32' with ALIGN and OFFSET."
  (nelisp-asm-wasm-emit-byte buf #x3e)
  (nelisp-asm-wasm-emit-uleb128 buf (or align 2))
  (nelisp-asm-wasm-emit-uleb128 buf (or offset 0)))

(defun nelisp-asm-wasm-op-f64-sqrt (buf)
  "Emit `f64.sqrt'."
  (nelisp-asm-wasm-emit-byte buf #x9f))

(defun nelisp-asm-wasm-op-f64-reinterpret-i64 (buf)
  "Emit `f64.reinterpret_i64'."
  (nelisp-asm-wasm-emit-byte buf #xbf))

(defun nelisp-asm-wasm-op-i64-reinterpret-f64 (buf)
  "Emit `i64.reinterpret_f64'."
  (nelisp-asm-wasm-emit-byte buf #xbd))

(defun nelisp-asm-wasm-op-end (buf)
  "Emit `end'."
  (nelisp-asm-wasm-emit-byte buf #x0b))

(provide 'nelisp-asm-wasm)

;;; nelisp-asm-wasm.el ends here
