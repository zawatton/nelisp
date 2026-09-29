;;; nelisp-eln-handler-substrate-driver.el --- Doc 210 S8.1/S8.3 driver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runs on a NeLisp standalone binary (NELISP_READER_DYNAMIC flavor).
;; Environment: NELISP_ROOT (repo), NELISP_S8_ELN (the pinned probe),
;; NELISP_S8_BODY_SHA256 (its declared native body), NELISP_S8_MODE (`chain'
;; = S8.1, `jump' = S8.3).  Prints `S8_<CHECK>=PASS' lines and a final PASS
;; line; any failure signals (non-zero exit, message on stderr).

;;; Code:

(let ((repo (getenv "NELISP_ROOT")))
  (add-to-list 'load-path (expand-file-name "lisp" repo))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" repo)))
(require 'cl-lib)
(require 'nelisp-eln-handler-substrate)

(defvar s8-driver--eln (getenv "NELISP_S8_ELN"))
(defvar s8-driver--mode (getenv "NELISP_S8_MODE"))

(defun s8-driver--pass (name)
  (princ (format "S8_%s=PASS\n" name)))

(defun s8-driver--check (name condition)
  (unless condition (error "S8 check failed: %s" name))
  (s8-driver--pass name))

(defun s8-driver--signals (name thunk reason)
  "Check that THUNK signals `nelisp-eln-handler-substrate-error' REASON."
  (let ((got (condition-case failure (progn (funcall thunk) nil)
               (nelisp-eln-handler-substrate-error (cadr failure)))))
    (unless (eq got reason)
      (error "S8 negative %s: wanted %S, got %S" name reason got))
    (s8-driver--pass name)))

(defun s8-driver--open ()
  (setq nelisp-eln-registration--setjmp-declared-body-sha256s
        (list (getenv "NELISP_S8_BODY_SHA256")))
  (nelisp-eln-system-loader-open s8-driver--eln))

;;;; S8.1 -----------------------------------------------------------

(defun s8-driver--chain ()
  ;; Control: with no declared body the artifact is refused before dlopen.
  (setq nelisp-eln-registration--setjmp-declared-body-sha256s nil)
  (s8-driver--check
   "CHAIN_UNDECLARED_REFUSED_PREOPEN"
   (eq 'preopen-executable-region-not-admitted
       (condition-case failure
           (progn (nelisp-eln-system-loader-open s8-driver--eln) nil)
         (nelisp-eln-registration-error (cadr failure)))))
  (let* ((handle (s8-driver--open))
         (sentinel (nelisp-eln-handler-substrate-sentinel-address))
         (thread (nelisp-eln-handler-substrate-thread-address)))
    ;; Substrate shape: handlerlist at +0x68 is the sentinel, whose next is 0.
    (s8-driver--check "CHAIN_SENTINEL_AT_0X68"
                      (and (= (nelisp-eln-handler-substrate-handlerlist) sentinel)
                           (= (ptr-read-u64 thread #x68) sentinel)
                           (= (ptr-read-u64 sentinel #x20) 0)))
    ;; Negative: before the loader writes the cell the artifact's chain is dead.
    (s8-driver--signals
     "CHAIN_UNBOUND_IS_DEAD"
     (lambda () (nelisp-eln-handler-substrate-artifact-handlerlist handle))
     'chain-current-thread-mismatch)
    (nelisp-eln-handler-substrate-bind-chain handle)
    ;; The three dependent loads of the native pop sequence reach the sentinel.
    (s8-driver--check "CHAIN_THREE_LOADS_REACH_SENTINEL"
                      (= (nelisp-eln-handler-substrate-artifact-handlerlist handle)
                         sentinel))
    ;; The loader verifies the cell was zero before writing (no second bind).
    (s8-driver--signals "CHAIN_SECOND_BIND_REFUSED"
                        (lambda () (nelisp-eln-handler-substrate-bind-chain handle))
                        'current-thread-reloc-not-zero)
    ;; Negative: a tampered current_thread cell is caught by the chain check.
    (let* ((cell (nelisp-eln-handler-substrate-current-thread-cell))
           (saved (ptr-read-u64 cell 0)))
      (ptr-write-u64 cell 0 (+ saved 16))
      (unwind-protect
          (s8-driver--signals
           "CHAIN_TAMPERED_THREAD_DETECTED"
           (lambda () (nelisp-eln-handler-substrate-artifact-handlerlist handle))
           'chain-thread-mismatch)
        (ptr-write-u64 cell 0 saved)))
    ;; Negative: a moved handlerlist is not the sentinel.
    (ptr-write-u64 thread #x68 (+ sentinel #x140))
    (s8-driver--check "CHAIN_MOVED_HANDLERLIST_DETECTED"
                      (/= (nelisp-eln-handler-substrate-artifact-handlerlist handle)
                          sentinel))
    (ptr-write-u64 thread #x68 sentinel)
    (s8-driver--check "CHAIN_RESTORED"
                      (= (nelisp-eln-handler-substrate-artifact-handlerlist handle)
                         sentinel))
    (nelisp-eln-system-loader-close handle))
  (princ "NELISP-ELN-HANDLER-SUBSTRATE-CHAIN-PASS 1\n"))

;;;; S8.3 -----------------------------------------------------------

(defconst s8-driver--canaries
  '((rbx . #x01234567) (rbp . #x11234567) (r12 . #x21234567)
    (r13 . #x31234567) (r14 . #x41234567) (r15 . #x51234567)))

(defun s8-driver--emit-canary-checks (buf fail)
  (dolist (pair s8-driver--canaries)
    (nelisp-asm-x86_64-mov-imm64 buf 'rax (cdr pair))
    (nelisp-asm-x86_64-cmp-reg-reg buf (car pair) 'rax)
    (nelisp-asm-x86_64-jnz-rel32 buf fail)))

(defun s8-driver--emit-returns-twice (buf name value corrupt)
  "Emit ENTRY(rdi=buffer, rsi=&GOT-slot, rdx=longjmp).
It sets canaries in the six callee-saved registers, calls the function stored
in *rsi as `setjmp' on rdi, clobbers every saved register, calls longjmp with
VALUE and, when control returns a second time, checks the result value (zero
becomes one), rsp and all canaries.  Returns 42 on success, 12 value, 13 rsp,
99 canary.  CORRUPT smashes the saved rbx in the buffer before the jump."
  (let ((fail (intern (format "%s-fail" name)))
        (done (intern (format "%s-done" name)))
        (resumed (intern (format "%s-resumed" name)))
        (value-fail (intern (format "%s-value-fail" name)))
        (stack-fail (intern (format "%s-stack-fail" name)))
        (value-ok (intern (format "%s-value-ok" name)))
        (expected (if (= value 0) 1 value)))
    (nelisp-asm-x86_64-define-label buf name)
    (dolist (reg '(rbp rbx r12 r13 r14 r15))
      (nelisp-asm-x86_64-push buf reg))
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp 40)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 0 'rdi)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsp)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 8 'rax)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 16 'rsi)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 24 'rdx)
    (dolist (pair s8-driver--canaries)
      (nelisp-asm-x86_64-mov-imm64 buf (car pair) (cdr pair)))
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'rax 'rsi 0)
    (nelisp-asm-x86_64-call-reg buf 'rax)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jnz-rel32 buf resumed)
    (when corrupt
      (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx 0)
      (nelisp-asm-x86_64-mov-imm32 buf 'rax #x5a5a)
      (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rcx 0 'rax))
    (dolist (reg '(rbx rbp r12 r13 r14 r15))
      (nelisp-asm-x86_64-mov-imm32 buf reg 0))
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rdi 0)
    (nelisp-asm-x86_64-mov-imm32 buf 'rsi value)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rax 24)
    (nelisp-asm-x86_64-call-reg buf 'rax)
    (nelisp-asm-x86_64-define-label buf resumed)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax expected)
    (nelisp-asm-x86_64-jnz-rel32 buf value-fail)
    (nelisp-asm-x86_64-define-label buf value-ok)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx 8)
    (nelisp-asm-x86_64-cmp-reg-reg buf 'rsp 'rcx)
    (nelisp-asm-x86_64-jnz-rel32 buf stack-fail)
    (s8-driver--emit-canary-checks buf fail)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 42)
    (nelisp-asm-x86_64-jmp-rel32 buf done)
    (nelisp-asm-x86_64-define-label buf value-fail)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 12)
    (nelisp-asm-x86_64-jmp-rel32 buf done)
    (nelisp-asm-x86_64-define-label buf stack-fail)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 13)
    (nelisp-asm-x86_64-jmp-rel32 buf done)
    (nelisp-asm-x86_64-define-label buf fail)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 99)
    (nelisp-asm-x86_64-define-label buf done)
    (nelisp-asm-x86_64-add-imm32 buf 'rsp 40)
    (dolist (reg '(r15 r14 r13 r12 rbx rbp))
      (nelisp-asm-x86_64-pop buf reg))
    (nelisp-asm-x86_64-ret buf)))

(defun s8-driver--emit-setjmp-only (buf name)
  "Emit ENTRY(rdi=buffer, rsi=&GOT-slot): call *rsi once and return its result."
  (nelisp-asm-x86_64-define-label buf name)
  (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8)
  (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'rax 'rsi 0)
  (nelisp-asm-x86_64-call-reg buf 'rax)
  (nelisp-asm-x86_64-add-imm32 buf 'rsp 8)
  (nelisp-asm-x86_64-ret buf))

(defun s8-driver--harness ()
  "Assemble, map RWX and return a plist of entry addresses and code range."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (s8-driver--emit-returns-twice buf 'h-seven 7 nil)
    (s8-driver--emit-returns-twice buf 'h-zero 0 nil)
    (s8-driver--emit-returns-twice buf 'h-corrupt 7 t)
    (s8-driver--emit-setjmp-only buf 'h-once)
    (let* ((bytes (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (size (nelisp-eln-handler-substrate--page-round (length bytes)))
           (page (nelisp-native-load-map-anonymous size t)))
      (nelisp-eln-handler-substrate--write-bytes page bytes)
      (list :start page :end (+ page (length bytes))
            :seven (+ page (cdr (assq 'h-seven labels)))
            :zero (+ page (cdr (assq 'h-zero labels)))
            :corrupt (+ page (cdr (assq 'h-corrupt labels)))
            :once (+ page (cdr (assq 'h-once labels)))))))

(defun s8-driver--maps-perms (address)
  "Return the kernel's permission string of the mapping containing ADDRESS."
  (let ((maps (with-temp-buffer (insert-file-contents "/proc/self/maps")
                                (buffer-string)))
        (found nil))
    (dolist (line (split-string maps "\n" t))
      (when (string-match "\\`\\([0-9a-f]+\\)-\\([0-9a-f]+\\) \\([-rwxps]+\\) " line)
        (let ((start (string-to-number (match-string 1 line) 16))
              (end (string-to-number (match-string 2 line) 16)))
          (when (and (>= address start) (< address end))
            (setq found (match-string 3 line))))))
    found))

(defun s8-driver--jump ()
  (setq nelisp-eln-registration--setjmp-declared-body-sha256s nil)
  (let* ((harness (s8-driver--harness))
         (buffer-owner (nl-ffi-memory-allocate 512))
         (buffer (nl-ffi-memory-address buffer-owner))
         (handle (s8-driver--open))
         (slot (nelisp-eln-handler-substrate--setjmp-slot handle))
         (libc (nelisp-eln-handler-substrate--libc-setjmp-address))
         (longjmp (nelisp-eln-handler-substrate--stub-address 'longjmp))
         (stub (nelisp-eln-handler-substrate--stub-address 'setjmp)))
    (unwind-protect
        (progn
          ;; Before binding, ld.so (RTLD_NOW) has bound the word to libc.
          (s8-driver--check "JUMP_LD_SO_BOUND_TO_LIBC"
                            (and (> libc 0) (= (ptr-read-u64 slot 0) libc)))
          ;; Control: the glibc `_setjmp' does not produce the private layout
          ;; (its saved rip is pointer-guard mangled), so the private-layout
          ;; check below is meaningful.
          (dotimes (i 8) (ptr-write-u64 buffer (* i 8) 0))
          (ptr-call (plist-get harness :once) buffer slot 0 0 0 0)
          (let ((rip (ptr-read-u64 buffer 56)))
            (s8-driver--check "JUMP_CONTROL_GLIBC_LAYOUT_DIFFERS"
                              (not (and (>= rip (plist-get harness :start))
                                        (< rip (plist-get harness :end))))))
          ;; Refuse to bind when nothing is declared for the body.
          (let ((nelisp-eln-registration--setjmp-declared-body-sha256s nil))
            (s8-driver--signals
             "JUMP_UNDECLARED_BIND_REFUSED"
             (lambda () (nelisp-eln-handler-substrate-bind-setjmp handle))
             'setjmp-surface-not-admitted))
          (nelisp-eln-handler-substrate-bind-setjmp handle)
          (s8-driver--check "JUMP_GOT_SLOT_READBACK"
                            (and (= (ptr-read-u64 slot 0) stub) (/= stub libc)))
          ;; The kernel agrees the GOT word's page is writable after load.
          (s8-driver--check "JUMP_GOT_PAGE_RW_PER_PROC_MAPS"
                            (let ((perms (s8-driver--maps-perms slot)))
                              (and perms (string-prefix-p "rw" perms))))
          ;; The real PLT entry still jumps through this very word.
          (let ((st (nelisp-eln-system-loader-layout handle)))
            (s8-driver--check
             "JUMP_SLOT_IS_FOURTH_GOT_PLT_WORD"
             (= slot (+ (plist-get st :bias)
                        (nth 0 (nelisp-eln-registration--elf-section-in-bytes
                                (plist-get st :file-bytes) ".got.plt"))
                        24))))
          ;; The probe body calls setjmp through the PLT: its buffer now has
          ;; the private layout (saved rip inside the caller).
          (dotimes (i 8) (ptr-write-u64 buffer (* i 8) 0))
          (ptr-call (plist-get harness :once) buffer slot 0 0 0 0)
          (let ((rip (ptr-read-u64 buffer 56)))
            (s8-driver--check "JUMP_PRIVATE_LAYOUT_RIP_IN_CALLER"
                              (and (>= rip (plist-get harness :start))
                                   (< rip (plist-get harness :end)))))
          ;; Returns twice, callee-saved registers restored, rsp restored.
          (s8-driver--check "JUMP_RETURNS_TWICE_VALUE_7"
                            (= 42 (ptr-call (plist-get harness :seven)
                                            buffer slot longjmp 0 0 0)))
          (s8-driver--check "JUMP_ZERO_NORMALIZED_TO_ONE"
                            (= 42 (ptr-call (plist-get harness :zero)
                                            buffer slot longjmp 0 0 0)))
          ;; Negative: a clobbered saved register is caught by the canaries.
          (s8-driver--check "JUMP_CLOBBERED_CANARY_DETECTED"
                            (= 99 (ptr-call (plist-get harness :corrupt)
                                            buffer slot longjmp 0 0 0)))
          ;; Negative: a tampered GOT word fails the read-back verification.
          (ptr-write-u64 slot 0 libc)
          (s8-driver--signals
           "JUMP_TAMPERED_GOT_READBACK_REFUSED"
           (lambda () (nelisp-eln-handler-substrate-verify-setjmp-binding handle))
           'setjmp-slot-readback)
          (ptr-write-u64 slot 0 stub)
          (s8-driver--check "JUMP_REBOUND_OK"
                            (eql slot (nelisp-eln-handler-substrate-verify-setjmp-binding
                                       handle)))
          ;; The pair is unchanged and still passes after the tamper cycle.
          (s8-driver--check "JUMP_STILL_RETURNS_TWICE"
                            (= 42 (ptr-call (plist-get harness :seven)
                                            buffer slot longjmp 0 0 0))))
      (nelisp-eln-system-loader-close handle)
      (nelisp-eln-system-loader--release-owner buffer-owner)))
  (princ "NELISP-ELN-HANDLER-SUBSTRATE-JUMP-PASS 1\n"))

(cond ((equal s8-driver--mode "chain") (s8-driver--chain))
      ((equal s8-driver--mode "jump") (s8-driver--jump))
      (t (error "NELISP_S8_MODE must be chain or jump: %S" s8-driver--mode)))
