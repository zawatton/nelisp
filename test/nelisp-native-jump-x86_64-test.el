;;; nelisp-native-jump-x86_64-test.el --- private jump tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-native-jump-x86_64)

(defconst nelisp-native-jump-x86_64-test--binary
  (getenv "NELISP_OWNED_JUMP_TEST_BINARY"))
(defconst nelisp-native-jump-x86_64-test--root
  (getenv "NELISP_OWNED_JUMP_TEST_ROOT"))

(defun nelisp-native-jump-x86_64-test--canaries (buf fail)
  "Emit canary checks into BUF, branching to FAIL on any mismatch."
  (dolist (pair '((rbx . #x01234567)
                  (rbp . #x11234567)
                  (r12 . #x21234567)
                  (r13 . #x31234567)
                  (r14 . #x41234567)
                  (r15 . #x51234567)))
    (nelisp-asm-x86_64-mov-imm64 buf 'rax (cdr pair))
    (nelisp-asm-x86_64-cmp-reg-reg buf (car pair) 'rax)
    (nelisp-asm-x86_64-jnz-rel32 buf fail)))

(defun nelisp-native-jump-x86_64-test--emit-one (buf name buffer-label)
  "Emit a harness that checks a zero/nonzero longjmp through BUFFER-LABEL."
  (let ((fail (intern (format "%s-fail" name)))
        (done (intern (format "%s-done" name)))
        (resumed (intern (format "%s-resumed" name))))
    (let (
          (value-fail (intern (format "%s-value-fail" name)))
          (stack-fail (intern (format "%s-stack-fail" name))))
    (nelisp-asm-x86_64-define-label buf name)
    (dolist (reg '(rbp rbx r12 r13 r14 r15))
      (nelisp-asm-x86_64-push buf reg))
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp 24)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 0 'rdi)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsp)
    (nelisp-asm-x86_64-mov-mem-rsp-disp-reg buf 8 'rax)
    (dolist (pair '((rbx . #x01234567)
                    (rbp . #x11234567)
                    (r12 . #x21234567)
                    (r13 . #x31234567)
                    (r14 . #x41234567)
                    (r15 . #x51234567)))
      (nelisp-asm-x86_64-mov-imm64 buf (car pair) (cdr pair)))
    (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi buffer-label)
    (nelisp-asm-x86_64-call-rel32 buf 'jump-setjmp)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jnz-rel32 buf resumed)
    ;; Make successful restoration observable by clobbering every saved GPR.
    (dolist (reg '(rbx rbp r12 r13 r14 r15))
      (nelisp-asm-x86_64-mov-imm32 buf reg 0))
    (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi buffer-label)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rsi 0)
    (nelisp-asm-x86_64-call-rel32 buf 'jump-longjmp)
    (nelisp-asm-x86_64-define-label buf resumed)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rdx 0)
    ;; C longjmp takes an int: compare only its low 32 bits.
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string #x89 #xD2))
    (nelisp-asm-x86_64-cmp-imm32 buf 'rdx 0)
    (nelisp-asm-x86_64-jnz-rel32 buf (intern (format "%s-value" name)))
    (nelisp-asm-x86_64-mov-imm32 buf 'rdx 1)
    (nelisp-asm-x86_64-define-label buf (intern (format "%s-value" name)))
    (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'rdx)
    (nelisp-asm-x86_64-jnz-rel32 buf value-fail)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx 8)
    (nelisp-asm-x86_64-cmp-reg-reg buf 'rsp 'rcx)
    (nelisp-asm-x86_64-jnz-rel32 buf stack-fail)
    (nelisp-native-jump-x86_64-test--canaries buf fail)
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
    (nelisp-asm-x86_64-add-imm32 buf 'rsp 24)
    (dolist (reg '(r15 r14 r13 r12 rbx rbp))
      (nelisp-asm-x86_64-pop buf reg))
    (nelisp-asm-x86_64-ret buf))))

(defun nelisp-native-jump-x86_64-test--emit-nested (buf)
  "Emit a harness which jumps inner then outer using distinct buffers."
  (nelisp-asm-x86_64-define-label buf 'jump-nested)
  (dolist (reg '(rbp rbx r12 r13 r14 r15))
    (nelisp-asm-x86_64-push buf reg))
  (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8)
  (dolist (pair '((rbx . #x01234567)
                  (rbp . #x11234567)
                  (r12 . #x21234567)
                  (r13 . #x31234567)
                  (r14 . #x41234567)
                  (r15 . #x51234567)))
    (nelisp-asm-x86_64-mov-imm64 buf (car pair) (cdr pair)))
  (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi 'jump-buffer-outer)
  (nelisp-asm-x86_64-call-rel32 buf 'jump-setjmp)
  (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
  (nelisp-asm-x86_64-jnz-rel32 buf 'jump-nested-outer-return)
  (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi 'jump-buffer-inner)
  (nelisp-asm-x86_64-call-rel32 buf 'jump-setjmp)
  (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
  (nelisp-asm-x86_64-jnz-rel32 buf 'jump-nested-inner-return)
  (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi 'jump-buffer-inner)
  (nelisp-asm-x86_64-mov-imm32 buf 'rsi 7)
  (nelisp-asm-x86_64-call-rel32 buf 'jump-longjmp)
  (nelisp-asm-x86_64-define-label buf 'jump-nested-inner-return)
  (nelisp-asm-x86_64-cmp-imm32 buf 'rax 7)
  (nelisp-asm-x86_64-jnz-rel32 buf 'jump-nested-fail)
  (nelisp-native-jump-x86_64-test--canaries buf 'jump-nested-fail)
  (nelisp-asm-x86_64-lea-reg-rip-label buf 'rdi 'jump-buffer-outer)
  (nelisp-asm-x86_64-mov-imm32 buf 'rsi 9)
  (nelisp-asm-x86_64-call-rel32 buf 'jump-longjmp)
  (nelisp-asm-x86_64-define-label buf 'jump-nested-outer-return)
  (nelisp-asm-x86_64-cmp-imm32 buf 'rax 9)
  (nelisp-asm-x86_64-jnz-rel32 buf 'jump-nested-fail)
  (nelisp-native-jump-x86_64-test--canaries buf 'jump-nested-fail)
  (nelisp-asm-x86_64-mov-imm32 buf 'rax 42)
  (nelisp-asm-x86_64-jmp-rel32 buf 'jump-nested-done)
  (nelisp-asm-x86_64-define-label buf 'jump-nested-fail)
  (nelisp-asm-x86_64-mov-imm32 buf 'rax 99)
  (nelisp-asm-x86_64-define-label buf 'jump-nested-done)
  (nelisp-asm-x86_64-add-imm32 buf 'rsp 8)
  (dolist (reg '(r15 r14 r13 r12 rbx rbp))
    (nelisp-asm-x86_64-pop buf reg))
  (nelisp-asm-x86_64-ret buf))

(defun nelisp-native-jump-x86_64-test--fixture-bytes ()
  "Build test entries and return their complete machine-code bytes."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (nelisp-native-jump-x86_64-emit-setjmp buf 'jump-setjmp)
    (nelisp-native-jump-x86_64-emit-longjmp buf 'jump-longjmp)
    (nelisp-native-jump-x86_64-test--emit-one
     buf 'jump-one-zero 'jump-buffer-one-zero)
    (nelisp-native-jump-x86_64-test--emit-one
     buf 'jump-one-value 'jump-buffer-one-value)
    (nelisp-native-jump-x86_64-test--emit-nested buf)
    (dolist (label '(jump-buffer-one-zero jump-buffer-one-value
                     jump-buffer-outer jump-buffer-inner))
      (nelisp-asm-x86_64-define-label buf label)
      (nelisp-asm-x86_64--append-bytes
       buf (make-string nelisp-native-jump-x86_64-buffer-size 0)))
    (let ((bytes (nelisp-asm-x86_64-resolve-fixups buf)))
      (list :bytes bytes
            :one-zero (cdr (assq 'jump-one-zero
                                 (nelisp-asm-x86_64-buffer-labels buf)))
            :one-value (cdr (assq 'jump-one-value
                                  (nelisp-asm-x86_64-buffer-labels buf)))
            :nested (cdr (assq 'jump-nested
                               (nelisp-asm-x86_64-buffer-labels buf)))))))

(defun nelisp-native-jump-x86_64-test--packed-u32 (bytes)
  "Return little-endian unsigned 32-bit words containing BYTES."
  (let* ((padded (* 4 (ceiling (length bytes) 4)))
         (words nil))
    (dotimes (base padded)
      (when (zerop (% base 4))
        (let ((value 0))
          (dotimes (offset 4)
            (let ((index (+ base offset)))
              (when (< index (length bytes))
                (setq value
                      (+ value (ash (aref bytes index) (* 8 offset)))))))
          (push value words))))
    (nreverse words)))

(defun nelisp-native-jump-x86_64-test--first-mismatch (actual expected)
  "Return offset/expected/actual for the first differing u32."
  (let ((index 0))
    (while (and (< index (length expected))
                (= (nth index actual) (nth index expected)))
      (setq index (1+ index)))
    (unless (= index (length expected))
      (list :offset (* index 4)
            :expected (nth index expected)
            :actual (nth index actual)))))

(defun nelisp-native-jump-x86_64-test--run-on-binary (fixture &optional corrupt)
  "Read back and conditionally execute FIXTURE in one mapped page.
When CORRUPT is non-nil, flip its first u32 to test the no-entry guard."
  (let* ((expected (nelisp-native-jump-x86_64-test--packed-u32
                    (plist-get fixture :bytes)))
         (words (copy-sequence expected))
         (_ (when corrupt (setcar words (logxor (car words) 1))))
         (writes (mapconcat
                  (lambda (pair)
                    (format "(ptr-write-u32 page %d %d)"
                            (* 4 (car pair)) (cdr pair)))
                  (cl-loop for word in words for index from 0
                           collect (cons index word))
                  " "))
         (read-exprs (mapconcat
                      (lambda (pair)
                        (format "(ptr-read-u32 page %d)" (* 4 (car pair))))
                      (cl-loop for word in expected for index from 0
                               collect (cons index word))
                      " "))
         (calls (format
                 "(list (ptr-call (+ page %d) 0 0 0 0 0 0) (ptr-call (+ page %d) 5 0 0 0 0 0) (ptr-call (+ page %d) -7 0 0 0 0 0) (ptr-call (+ page %d) 4294967296 0 0 0 0 0) (ptr-call (+ page %d) 0 0 0 0 0 0))"
                 (plist-get fixture :one-zero)
                 (plist-get fixture :one-value)
                 (plist-get fixture :one-value)
                 (plist-get fixture :one-value)
                 (plist-get fixture :nested)))
         (expr (format
                "(let ((page (syscall-direct 9 0 4096 7 34 -1 0))) (if (< page 4096) 'mmap-failed (unwind-protect (progn %s (let ((readback (list %s))) (if (equal readback '%s) (list 'executed %s) (list 'readback-mismatch readback)))) (syscall-direct 11 page 4096 0 0 0 0))))"
                writes read-exprs (prin1-to-string expected) calls))
         (stdout (generate-new-buffer " *owned-jump-output*"))
         (stderr (make-temp-file "owned-jump-stderr-"))
         status output error-output)
    (unwind-protect
        (let ((default-directory nelisp-native-jump-x86_64-test--root))
          (setq status
                (process-file nelisp-native-jump-x86_64-test--binary
                              nil (list stdout stderr) nil "--eval" expr))
          (setq output (with-current-buffer stdout (buffer-string))
                error-output (with-temp-buffer
                               (insert-file-contents stderr)
                               (buffer-string)))
          (list status output error-output expr))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (delete-file stderr))))

(ert-deftest nelisp-native-jump-x86_64-buffer-abi-and-target-execution ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)
                    nelisp-native-jump-x86_64-test--root
                    nelisp-native-jump-x86_64-test--binary
                    (file-executable-p nelisp-native-jump-x86_64-test--binary)))
  (let* ((fixture (nelisp-native-jump-x86_64-test--fixture-bytes))
         (run (nelisp-native-jump-x86_64-test--run-on-binary fixture))
         (bad (nelisp-native-jump-x86_64-test--run-on-binary fixture t))
         (expected (nelisp-native-jump-x86_64-test--packed-u32
                    (plist-get fixture :bytes))))
    (should (<= nelisp-native-jump-x86_64-buffer-size 200))
    (should (= (car run) 0))
    (should (equal (nth 2 run) ""))
    (should (equal (car (read-from-string (nth 1 run)))
                   '(executed (42 42 42 42 42))))
    (should (= (car bad) 0))
    (should (equal (nth 2 bad) ""))
    (let* ((value (car (read-from-string (nth 1 bad))))
           (actual (cadr value)))
      (should (eq (car value) 'readback-mismatch))
      (should (equal
               (nelisp-native-jump-x86_64-test--first-mismatch actual expected)
               (list :offset 0 :expected (car expected)
                     :actual (logxor (car expected) 1)))))))

(provide 'nelisp-native-jump-x86_64-test)

;;; nelisp-native-jump-x86_64-test.el ends here
