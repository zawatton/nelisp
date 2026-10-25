;;; nelisp-win64-stack-probe-test.el --- Guard-page regression -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-aot-compiler)

(ert-deftest nelisp-win64-stack-probe/page-boundaries ()
  "Probe exact pages and tails without clobbering argument registers."
  (dolist (bytes '(0 16 4080 4095 4096 4097 4112 8192 8208))
    (let ((buf (nelisp-asm-x86_64-make-buffer))
          (offset 0)
          (reserved 0)
          (probes 0))
      (nelisp-aot-compiler--win64-reserve-stack-frame buf bytes)
      ;; Decode the complete sequence, accepting only SUB RSP,imm32 and
      ;; TEST byte [RSP],0.  This also rejects register-clobbering probes.
      (let ((code (nelisp-asm-x86_64-buffer-bytes buf)))
        (while (< offset (length code))
          (should (equal (substring code offset (+ offset 3))
                         (unibyte-string #x48 #x81 #xec)))
          (let ((step (+ (aref code (+ offset 3))
                         (ash (aref code (+ offset 4)) 8)
                         (ash (aref code (+ offset 5)) 16)
                         (ash (aref code (+ offset 6)) 24))))
            (should (<= 0 step 4096))
            (setq reserved (+ reserved step)))
          (setq offset (+ offset 7))
          (when (>= bytes 4096)
            (should (equal (substring code offset (+ offset 4))
                           (unibyte-string #xf6 #x04 #x24 #x00)))
            (setq probes (1+ probes) offset (+ offset 4))))
        (should (= reserved bytes))
        (should (= probes (if (< bytes 4096) 0 (/ (+ bytes 4095) 4096))))))))

(ert-deftest nelisp-win64-stack-probe/sysv-does-not-probe ()
  "The Windows fix must not change the SysV prologue path."
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler--abi 'sysv)
        (buf (nelisp-asm-x86_64-make-buffer)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--win64-reserve-stack-frame)
               (lambda (&rest _) (ert-fail "Windows probe used by SysV"))))
      (nelisp-aot-compiler--emit-defun
       (nelisp-aot-compiler--make-ir
        'defun :name 'sysv-probe :param-regs nil :rt-slot-count 3918
        :body (nelisp-aot-compiler--make-ir 'imm :value 0)) buf))))

(ert-deftest nelisp-win64-stack-probe/large-runtime-frame ()
  "Cold boot must not skip a Windows guard page in a large prologue."
  (dolist (param-class '(gp f64 mixed))
    (let* ((nelisp-aot-compiler--arch 'x86_64)
           (nelisp-aot-compiler--abi 'win64)
           (buf (nelisp-asm-x86_64-make-buffer))
           ;; bf_n2_direct_arity's failing cold-boot frame was 31344 bytes.
           (ir (nelisp-aot-compiler--make-ir
                'defun :name 'guard-probe
                :param-regs (pcase param-class
                              ('f64 '(xmm0 xmm1 xmm2 xmm3))
                              ('mixed '(rcx xmm1 r8 xmm3))
                              (_ '(rcx rdx r8 r9)))
                :param-class param-class
                :param-classes (if (eq param-class 'mixed)
                                   '(gp f64 gp f64) '(gp gp gp gp))
                :rt-slot-count 3918
                :body (nelisp-aot-compiler--make-ir 'imm :value 0))))
      (nelisp-aot-compiler--emit-defun ir buf)
      (let ((bytes (nelisp-asm-x86_64-buffer-bytes buf))
            (probe (unibyte-string #xf6 #x04 #x24 #x00))
            (start 0)
            (count 0))
        (while (setq start (string-match (regexp-quote probe) bytes start))
          (setq count (1+ count) start (+ start 4)))
        (should (= count 8))
        (should (string-match-p
                 (regexp-quote
                  (unibyte-string #x48 #x81 #xec #x00 #x10 #x00 #x00
                                  #xf6 #x04 #x24 #x00))
                 bytes))))))

(provide 'nelisp-win64-stack-probe-test)
;;; nelisp-win64-stack-probe-test.el ends here
