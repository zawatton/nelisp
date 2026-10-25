;;; nelisp-aot-vmreg-test.el --- Raw VM state in callee-saved registers (cgreg) -*- lexical-binding: t; -*-
;; The compiler may keep the raw variables of an opted-in function (by name) in r12-r15 for the whole
;; body, calls included.  Opt-in is `nelisp-aot-compiler-vm-register-functions', a list of (NAME VAR...).
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-compiler-vm-register-functions)
(defvar nelisp-aot-compiler--vm-reg-decisions)
(defvar nelisp-aot-compiler--x86-frame-copy-aliases)

(defun vmreg-test--emit (source &optional abi)
  "Compile SOURCE (defuns) and return the resolved text bytes of all functions."
  (let* ((nelisp-aot-compiler--x86-frame-copy-aliases nil)
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--abi (or abi 'sysv))
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--label-counter 0)
         (ir (nelisp-aot-compiler--parse source nil))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer (or abi 'sysv))))
    (dolist (d defuns) (nelisp-aot-compiler--emit-defun d buf))
    (nelisp-asm-x86_64-resolve-fixups buf)))

(defun vmreg-test--disasm (bytes)
  "Return objdump's Intel disassembly of BYTES, or nil without objdump."
  (when (executable-find "objdump")
    (let ((f (make-temp-file "vmreg" nil ".bin")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'no-conversion))
              (write-region (concat bytes) nil f nil 'silent))
            (with-temp-buffer
              (call-process "objdump" nil t nil "-D" "-b" "binary" "-mi386:x86-64" "-M" "intel"
                            "--no-show-raw-insn" f)
              (buffer-string)))
        (delete-file f)))))

(defconst vmreg-test--src
  '(seq
    (defun vmt_hv (x) (+ x 1))
    (defun vmt_leaf (pc sp) (let* ((k 0)) (seq (while (< k 3) (seq (setq pc (vmt_hv pc)) (setq k (+ k 1)))) (+ pc sp))))
    (defun vmt_run (pc sp n)
      (let* ((i 0) (acc 0))
        (seq (while (< i n)
               (seq (setq acc (+ acc (vmt_leaf pc sp)))
                    (setq pc (+ pc 1))
                    (setq sp (vmt_hv sp))
                    (setq i (+ i 1))))
             acc)))))

(defconst vmreg-test--optin '((vmt_run pc sp acc i) (vmt_leaf pc sp k)))

(defun vmreg-test--expected (pc sp n)
  (let ((acc 0) (i 0))
    (while (< i n)
      (setq acc (+ acc (+ (+ pc 3) sp)) pc (1+ pc) sp (1+ sp) i (1+ i)))
    acc))

(defun vmreg-test--run-elf (optin)
  "Compile the e2e program with OPTIN and return its exit code."
  (let ((path (make-temp-file "vmreg-e2e"))
        (nelisp-aot-compiler--x86-frame-copy-aliases nil)
        (nelisp-aot-compiler-vm-register-functions optin))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           `(seq ,@(cdr vmreg-test--src) (exit (vmt_run 1 2 5))) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest vmreg-e2e-callee-saved-survive-calls ()
  "Promoted variables survive calls to other promoted functions; result equals the unpromoted one."
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (let ((want (mod (vmreg-test--expected 1 2 5) 256)))
    (should (= (vmreg-test--run-elf nil) want))
    (should (= (vmreg-test--run-elf vmreg-test--optin) want))))

(ert-deftest vmreg-admits-and-pools-r12-r15 ()
  (let ((nelisp-aot-compiler-vm-register-functions vmreg-test--optin)
        (nelisp-aot-compiler--vm-reg-decisions nil))
    (vmreg-test--emit vmreg-test--src)
    (let ((d (cdr (assq 'vmt_run nelisp-aot-compiler--vm-reg-decisions))))
      (should (eq (car d) :admit))
      (should (= (length (cdr d)) 4))
      (should (cl-subsetp (mapcar #'cdr (cdr d)) '(r12 r13 r14 r15))))))

(ert-deftest vmreg-no-frame-access-to-promoted-slots ()
  "After the prologue spill and register load, no instruction addresses a promoted parameter's word."
  (skip-unless (executable-find "objdump"))
  (let* ((nelisp-aot-compiler-vm-register-functions '((vmt_run pc sp)))
         (nelisp-aot-compiler--vm-reg-decisions nil)
         (asm (vmreg-test--disasm (vmreg-test--emit (nth 3 vmreg-test--src))))
         (lines (split-string asm "\n"))
         ;; vmt_run: pc is slot 0 -> [rbp-0x8], sp slot 1 -> [rbp-0x10]
         (pc-refs (cl-count-if (lambda (l) (string-match-p "rbp-0x8\\]" l)) lines))
         (sp-refs (cl-count-if (lambda (l) (string-match-p "rbp-0x10\\]" l)) lines)))
    ;; one prologue spill + one load into the register, nothing in the loop
    (should (<= pc-refs 2))
    (should (<= sp-refs 2))))

(ert-deftest vmreg-declines-without-optin-and-bytes-unchanged ()
  (let ((a (let ((nelisp-aot-compiler-vm-register-functions nil)) (vmreg-test--emit vmreg-test--src)))
        (b (let ((nelisp-aot-compiler-vm-register-functions '((no_such_fn pc))))
             (vmreg-test--emit vmreg-test--src))))
    (should (equal a b))))

(ert-deftest vmreg-declines-address-taken-or-observer ()
  (let ((nelisp-aot-compiler-vm-register-functions '((vmt_obs pc)))
        (nelisp-aot-compiler--vm-reg-decisions nil))
    (vmreg-test--emit '(defun vmt_obs (pc) (seq (aot-current-sp) (+ pc 1))))
    (should (eq (car (cdr (assq 'vmt_obs nelisp-aot-compiler--vm-reg-decisions))) :decline))))

(ert-deftest vmreg-win64-uses-callee-saved-pool-only ()
  (let* ((nelisp-aot-compiler-vm-register-functions '((vmt_run pc sp acc i)))
         (nelisp-aot-compiler--vm-reg-decisions nil)
         (text (vmreg-test--emit vmreg-test--src 'win64))
         (d (cdr (assq 'vmt_run nelisp-aot-compiler--vm-reg-decisions))))
    (should (eq (car d) :admit))
    (should-not (cl-intersection (mapcar #'cdr (cdr d)) '(r8 r9 rbx rdi rsi)))
    (when (executable-find "objdump")
      (let ((asm (vmreg-test--disasm text)))
        ;; the four callee-saved registers are saved and restored
        (dolist (r '("r12" "r13" "r14" "r15"))
          (should (string-match-p (format "mov +QWORD PTR \\[rbp-0x[0-9a-f]+\\],%s" r) asm))
          (should (string-match-p (format "mov +%s,QWORD PTR \\[rbp-0x[0-9a-f]+\\]" r) asm)))))))

(provide 'nelisp-aot-vmreg-test)
;;; nelisp-aot-vmreg-test.el ends here
