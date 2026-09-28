;;; nelisp-eln-emitter-templates.el --- pure self-emitter byte shapes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `nelisp-eln-emitter--symbol-name' and `nelisp-eln-emitter--top-level-code'
;; -- the two self-emitter functions `nelisp-eln-registration.el' actually
;; consults, to authenticate a genuine artifact's `c-name' mangling and to
;; recognize a self-emitted fixture's own top_level_run shape -- are pure
;; byte/string construction with no dependency on `nelisp-aot-compiler' or
;; `nelisp-elf-write'. `nelisp-eln-emitter.el' requires both of those
;; unconditionally at its top for its OTHER functions (`nelisp-eln-emitter
;; -write-ir' and friends, which actually assemble and link a real .eln), so
;; requiring the full `nelisp-eln-emitter' feature just to reach these two
;; functions drags in ~24,600 lines / ~1MB of source neither of them touches
;; (S7.7.4 corpus-gate investigation).
;;
;; This file holds exactly those two functions (moved here, not duplicated:
;; `nelisp-eln-emitter.el' requires this file and keeps calling them under
;; the same names). `nelisp-eln-registration.el' requires only this file,
;; never the full `nelisp-eln-emitter', so admitting a genuine GNU artifact
;; -- or recognizing a self-emitted one -- never loads the AOT compiler or
;; the ELF writer; only actually EMITTING a new self-emitted .eln (creating
;; the corpus-gate's own fixture, done once per gate run in its own process,
;; never inside the (artifact, check) worker pool) still does, through
;; `nelisp-eln-emitter-write-ir'.

;;; Code:

(defun nelisp-eln-emitter--symbol-name (name)
  "Return GNU comp-c-func-name for NAME in the pinned no-collision unit."
  (let ((source (symbol-name name)))
    (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_-]*\\'" source)
      (error "ELN emitter: unsupported function name %S" name))
    (concat
     "F"
     (mapconcat (lambda (char) (format "%02x" char))
                (string-to-list source) "")
     "_"
     (replace-regexp-in-string "-" "_" source)
     "_0")))

(defun nelisp-eln-emitter--top-level-code (arity)
  "Return top_level_run code and its ET_REL PC32/PLT32 relocations."
  (unless (memq arity '(0 1))
    (error "ELN emitter: unsupported registered-subr arity %S" arity))
  (let* ((arg-word (logior (ash arity 2) 2))
         (arg-immediate (list (logand arg-word 255)
                              (logand (ash arg-word -8) 255)
                              (logand (ash arg-word -16) 255)
                              (logand (ash arg-word -24) 255)))
         (code (append
                  '(#x53                         ; push rbx
                    #x48 #x89 #xfb               ; mov rbx,rdi (comp-unit)
                    #x48 #x8d #x05 0 0 0 0       ; lea rax,[rip+d_reloc_eph]
                    #x48 #x8b #x78 #x08          ; mov rdi,[rax+8] (name)
                    #x48 #x8b #x70 #x10          ; mov rsi,[rax+16] (c-name)
                    #xba)                        ; mov edx,minarg
                  arg-immediate
                  '(#xb9)                        ; mov ecx,maxarg
                  arg-immediate
                  '(
                    #x48 #x8d #x05 0 0 0 0       ; lea rax,[rip+d_reloc]
                    #x4c #x8b #x00               ; mov r8,[rax] (nil type)
                    #x48 #x8d #x05 0 0 0 0       ; lea rax,[rip+d_reloc_eph]
                    #x4c #x8b #x48 #x18          ; mov r9,[rax+24] (rest)
                    #x48 #x83 #xec #x10          ; align rsp, reserve 16B
                    #x48 #x89 #x1c #x24           ; [rsp]=arg7 comp-unit
                    #x48 #x8b #x05 0 0 0 0       ; mov rax,[rip+freloc_link_table]
                    #xff #x90 #x30 #x20 0 0       ; call [rax+slot*8]
                    #x48 #x83 #xc4 #x10           ; add rsp,16
                    #x5b #xc3)))                  ; pop rbx; ret
         (relocs nil))
    ;; Displacement fields are the four bytes beginning at these offsets.
    (dolist (entry '((7 . "d_reloc_eph")
                     (32 . "d_reloc")
                     (42 . "d_reloc_eph")
                     (61 . "freloc_link_table")))
      (push (list :section 'text :offset (car entry)
                  :symbol (cdr entry) :type 'pc32 :addend -4)
            relocs))
    (list (apply #'unibyte-string code) relocs)))

(provide 'nelisp-eln-emitter-templates)

;;; nelisp-eln-emitter-templates.el ends here
