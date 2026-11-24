;;; pe-dll-sample.el --- S1 sample DLL and loader EXE for the PE DLL writer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds a tiny Win64 DLL (add2 / answer / via_ptr) by linking one
;; hand-assembled COFF-style unit with `nelisp-link-units-pe32-dll', plus
;; a generic-spec EXE that LoadLibraryA's it, resolves the exports through
;; GetProcAddress, calls them and exits with add2(40,2) (= 42) on success.
;;
;; The machine code is hand-assembled raw bytes (the AOT/nelisp-asm path
;; is not used in S1).  via_ptr loads a pointer from .data that holds the
;; absolute address of `answer' (an abs64 relocation), so the DLL only
;; works when the loader applies base relocations after a rebase.
;;
;; Negative-control hooks (environment variables):
;;   NELISP_PE_DLL_DROP_EXPORT=NAME   drop one export from the table
;;   NELISP_PE_DLL_NO_RELOC=1         omit the .reloc section

;;; Code:

(require 'cl-lib)
(require 'nelisp-pe-write)
(require 'nelisp-static-linker)

(defconst nelisp-pe-dll-sample-dll-name "pedllsample.dll")

(defun nelisp-pe-dll-sample--unit ()
  "Return the hand-assembled link unit."
  (let* ((text (concat
                ;; add2: lea rax,[rcx+rdx] ; ret            (offset 0)
                (unibyte-string #x48 #x8d #x04 #x11 #xc3)
                (apply #'unibyte-string (make-list 11 #xcc))
                ;; answer: mov eax,42 ; ret                 (offset 16)
                (unibyte-string #xb8 #x2a #x00 #x00 #x00 #xc3)
                (apply #'unibyte-string (make-list 10 #xcc))
                ;; via_ptr: mov rax,[rip+g_ptr] ; jmp rax   (offset 32)
                (unibyte-string #x48 #x8b #x05 0 0 0 0 #xff #xe0)))
         (data (make-string 8 0))
         (pc32 (append (nelisp-link-reloc 35 'pc32 "g_ptr" 0) (list :section 'text)))
         (abs64 (append (nelisp-link-reloc 0 'abs64 "answer" 0) (list :section 'data))))
    (nelisp-link-unit-make
     "pedll-sample.o"
     (list (cons 'text text) (cons 'data data))
     (list (nelisp-link-symbol "add2" 0 :section 'text)
           (nelisp-link-symbol "answer" 16 :section 'text)
           (nelisp-link-symbol "via_ptr" 32 :section 'text)
           (nelisp-link-symbol "g_ptr" 0 :section 'data :type 'object))
     (list pc32 abs64))))

(defun nelisp-pe-dll-sample-build-dll (path &optional image-base)
  "Link the sample DLL to PATH, honouring the negative-control env hooks."
  (let* ((drop (getenv "NELISP_PE_DLL_DROP_EXPORT"))
         (exports (cl-remove-if (lambda (n) (and drop (string= n drop)))
                                ;; Spec order differs from sorted order so
                                ;; the ordinal table is non-trivial.
                                '("via_ptr" "add2" "answer"))))
    (nelisp-link-units-pe32-dll
     path (list (nelisp-pe-dll-sample--unit)) exports nil
     (list :dll-name nelisp-pe-dll-sample-dll-name
           :image-base image-base
           :no-reloc (equal (getenv "NELISP_PE_DLL_NO_RELOC") "1")))))

(defun nelisp-pe-dll-sample--le (n bytes)
  (let (out) (dotimes (i bytes) (push (logand (ash n (- (* 8 i))) 255) out))
       (nreverse out)))

(defun nelisp-pe-dll-sample-exe-spec (&optional reserve-base)
  "Return the generic EXE spec for the loader.
When RESERVE-BASE is an integer the EXE first VirtualAlloc's 64 KiB there so
the loader cannot honour the DLL's preferred base, and fails (exit 12) if
the DLL's HMODULE still equals RESERVE-BASE."
  (let* ((text-rva #x1000) (rdata-rva #x2000) (idata-rva #x3000)
         (imports '(("KERNEL32.dll" "LoadLibraryA" "GetProcAddress"
                     "ExitProcess" "VirtualAlloc")))
         (iat (plist-get (nelisp-pe--build-idata idata-rva imports) :iat-rva-alist))
         (strings '("pedllsample.dll" "add2" "answer" "via_ptr"))
         (str-off nil) (rdata "") (code nil) (labels nil) (fix nil))
    (dolist (s strings)
      (push (cons s (length rdata)) str-off)
      (setq rdata (concat rdata s "\0")))
    (cl-labels ((pos () (length code))
                (b (&rest bytes) (setq code (append code bytes)))
                (rip32 (rva) ;; disp32 relative to end of the 4 disp bytes
                  (apply #'b (nelisp-pe-dll-sample--le
                              (- rva (+ text-rva (pos) 4)) 4)))
                (call-iat (name) (b #xff #x15) (rip32 (cdr (assoc name iat))))
                (lea (opcode-modrm str)
                  (apply #'b opcode-modrm)
                  (rip32 (+ rdata-rva (cdr (assoc str str-off)))))
                (jcc (op2 label) (b #x0f op2) (push (list (pos) label) fix)
                     (b 0 0 0 0))
                (getproc (name store fail)
                  (b #x48 #x89 #xd9)                      ; mov rcx,rbx
                  (lea '(#x48 #x8d #x15) name)            ; lea rdx,[rip+name]
                  (call-iat "GetProcAddress")
                  (b #x48 #x85 #xc0) (jcc #x84 fail)      ; test rax,rax ; jz
                  (apply #'b store)))
      (b #x48 #x83 #xec #x28)                             ; sub rsp,40
      (when reserve-base
        (b #x48 #xb9) (apply #'b (nelisp-pe-dll-sample--le reserve-base 8))
        (b #xba 0 0 1 0)                                  ; edx=0x10000
        (b #x41 #xb8 0 #x30 0 0)                          ; r8d=MEM_RESERVE|COMMIT
        (b #x41 #xb9 4 0 0 0)                             ; r9d=PAGE_READWRITE
        (call-iat "VirtualAlloc")
        (b #x48 #x85 #xc0) (jcc #x84 'f10))
      (lea '(#x48 #x8d #x0d) "pedllsample.dll")           ; lea rcx,[rip+dll]
      (call-iat "LoadLibraryA")
      (b #x48 #x85 #xc0) (jcc #x84 'f11)
      (b #x48 #x89 #xc3)                                  ; mov rbx,rax
      (when reserve-base
        (b #x48 #xb9) (apply #'b (nelisp-pe-dll-sample--le reserve-base 8))
        (b #x48 #x39 #xcb)                                ; cmp rbx,rcx
        (jcc #x84 'f12))                                  ; je -> not rebased
      (getproc "add2" '(#x48 #x89 #xc6) 'f13)             ; rsi
      (getproc "answer" '(#x48 #x89 #xc7) 'f14)           ; rdi
      (getproc "via_ptr" '(#x48 #x89 #xc5) 'f15)          ; rbp
      (b #xb9 40 0 0 0) (b #xba 2 0 0 0) (b #xff #xd6)    ; add2(40,2)
      (b #x41 #x89 #xc5)                                  ; mov r13d,eax
      (b #xff #xd7) (b #x41 #x89 #xc6)                    ; answer() -> r14d
      (b #xff #xd5) (b #x41 #x89 #xc7)                    ; via_ptr() -> r15d
      (b #x41 #x83 #xfe 42) (jcc #x85 'f16)
      (b #x41 #x83 #xff 42) (jcc #x85 'f17)
      (b #x44 #x89 #xe9)                                  ; mov ecx,r13d
      (call-iat "ExitProcess")
      (dolist (n '(10 11 12 13 14 15 16 17))
        (push (cons (intern (format "f%d" n)) (pos)) labels)
        (b #xb9 n 0 0 0) (call-iat "ExitProcess")))
    (dolist (f fix)
      (let* ((at (nth 0 f)) (target (cdr (assq (nth 1 f) labels)))
             (disp (nelisp-pe-dll-sample--le (- target (+ at 4)) 4)))
        (dotimes (i 4) (setcar (nthcdr (+ at i) code) (nth i disp)))))
    (list :text (apply #'unibyte-string code)
          :rodata rdata :imports imports)))

(defun nelisp-pe-dll-sample-build-all (dir)
  "Build pedllsample.dll, loader.exe and loader-rebase.exe into DIR."
  (make-directory dir t)
  (nelisp-pe-dll-sample-build-dll (expand-file-name nelisp-pe-dll-sample-dll-name dir))
  (nelisp-pe-write-exe-binary (expand-file-name "loader.exe" dir)
                              (nelisp-pe-dll-sample-exe-spec))
  (nelisp-pe-write-exe-binary (expand-file-name "loader-rebase.exe" dir)
                              (nelisp-pe-dll-sample-exe-spec nelisp-pe-write-dll-image-base))
  dir)

(provide 'pe-dll-sample)
;;; pe-dll-sample.el ends here
