;;; nelisp-aot-vmreg-test.el --- Raw VM state in callee-saved registers (cgreg) -*- lexical-binding: t; -*-
;; The compiler may keep the raw variables of an opted-in function (by name) in r12-r15 for the whole
;; body, calls included.  Opt-in is `nelisp-aot-compiler-vm-register-functions', a list of (NAME VAR...).
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-compiler-vm-register-functions)
(defvar nelisp-aot-compiler--vm-reg-decisions)
(defvar nelisp-aot-compiler--vm-reg-dropped)
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

(defconst vmreg-test--optin '((vmt_run pc sp acc i n) (vmt_leaf pc sp k)))

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
        (nelisp-aot-compiler--vm-reg-decisions nil)
        (nelisp-aot-compiler--vm-reg-dropped nil))
    (vmreg-test--emit vmreg-test--src)
    (let ((d (cdr (assq 'vmt_run nelisp-aot-compiler--vm-reg-decisions))))
      (should (eq (car d) :admit))
      ;; regraw: ACC accumulates call results (words), so the raw proof refuses it
      (should (equal (cdr (assq 'vmt_run nelisp-aot-compiler--vm-reg-dropped)) '(acc)))
      (should (= (length (cdr d)) 4))
      (should (cl-subsetp (mapcar #'cdr (cdr d)) '(r12 r13 r14 r15 rbx))))))

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
  (let* ((nelisp-aot-compiler-vm-register-functions '((vmt_run pc sp i n)))
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

;; ---- regraw: raw-value proof, rbx as fifth register, regions beside the map ----

(defvar nelisp-aot-compiler--vm-reg-dropped)
(defvar nelisp-aot-compiler--vm-reg-refusals)

(defun vmreg-test--decision (src optin &optional abi)
  "Compile SRC with OPTIN and return (DECISION . DROPPED) for its first opted-in function."
  (let ((nelisp-aot-compiler-vm-register-functions optin)
        (nelisp-aot-compiler--vm-reg-decisions nil)
        (nelisp-aot-compiler--vm-reg-dropped nil)
        (nelisp-aot-compiler--vm-reg-refusals nil))
    (vmreg-test--emit src abi)
    (cons (cdr (assq (car (car optin)) nelisp-aot-compiler--vm-reg-decisions))
          (cdr (assq (car (car optin)) nelisp-aot-compiler--vm-reg-dropped)))))

(ert-deftest regraw-proof-refuses-planted-reference ()
  "Negative control: a parameter that receives a Lisp-typed argument is refused; without it, admitted."
  (let* ((bad '(seq (defun vmp_h (x) (+ x 1))
                    (defun vmp_bad (pc w) (seq (setq pc w) (vmp_h pc) (+ pc 1)))))
         (good '(seq (defun vmp_h (x) (+ x 1))
                     (defun vmp_bad (pc w) (seq (setq pc (+ pc 1)) (vmp_h pc) (+ pc w)))))
         (rb (vmreg-test--decision bad '((vmp_bad pc))))
         (rg (vmreg-test--decision good '((vmp_bad pc)))))
    (should (eq (car (car rb)) :decline))
    (should (equal (cdr rb) '(pc)))
    (should (eq (car (car rg)) :admit))
    (should (null (cdr rg)))))

(ert-deftest regraw-proof-let-variable-needs-raw-definitions ()
  "A let variable named in the list is admitted for byte reads and refused for 64-bit word loads."
  (let* ((word '(seq (defun vmp_h (x) (+ x 1))
                     (defun vmp_let (p) (let* ((x 0) (y 0)) (seq (setq x (ptr-read-u64 p 0)) (setq y (+ y 1)) (vmp_h y) (+ x y))))))
         (byte '(seq (defun vmp_h (x) (+ x 1))
                     (defun vmp_let (p) (let* ((x 0) (y 0)) (seq (setq x (ptr-read-u8 p 0)) (setq y (+ y 1)) (vmp_h y) (+ x y))))))
         (rw (vmreg-test--decision word '((vmp_let x y))))
         (rb (vmreg-test--decision byte '((vmp_let x y)))))
    (should (equal (cdr rw) '(x)))
    (should (eq (car (car rw)) :admit))
    (should (= (length (cdr (car rw))) 1))
    (should (null (cdr rb)))
    (should (= (length (cdr (car rb))) 2))))

(ert-deftest regraw-proof-shift-back-is-not-raw ()
  "A word shifted right is unwrapped; shifting it left again rebuilds the word and is refused."
  (let* ((src '(seq (defun vmp_h (x) (+ x 1))
                    (defun vmp_sh (p) (let* ((u 0) (v 0))
                                        (seq (setq u (sar (ptr-read-u64 p 0) 2))
                                             (setq v (shl u 2))
                                             (vmp_h v)
                                             (+ u v))))))
         (r (vmreg-test--decision src '((vmp_sh u v)))))
    (should (equal (cdr r) '(v)))
    (should (= (length (cdr (car r))) 1))))

(defconst vmreg-test--five
  '(seq
    (defun rg_h (x) (+ x 1))
    (defun rg_run (a b c d e n)
      (let* ((i 0) (acc 0) (t1 0) (t2 0))
        (seq (while (< i n)
               (seq (setq acc (+ acc (rg_h a)))
                    (if (> acc 0)
                        (seq (setq t1 (+ t1 (* acc 3)))
                             (setq t2 (+ t2 (logand t1 255)))
                             (setq t1 (+ t1 t2))
                             (setq t2 (+ t2 t1))
                             (setq t1 (+ t1 t2)))
                      0)
                    (setq a (+ a 1)) (setq b (+ b a)) (setq c (+ c b)) (setq d (+ d c)) (setq e (+ e d))
                    (setq i (+ i 1))))
             (+ acc (+ t1 (+ t2 (+ a (+ b (+ c (+ d e)))))))))))
  "Five raw parameters across calls, with a call-free region (t1, t2) inside the loop.")

(defun vmreg-test--five-expected (a b c d e n)
  (let ((i 0) (acc 0) (t1 0) (t2 0))
    (while (< i n)
      (setq acc (+ acc (1+ a)))
      (when (> acc 0)
        (setq t1 (+ t1 (* acc 3)) t2 (+ t2 (logand t1 255)) t1 (+ t1 t2) t2 (+ t2 t1) t1 (+ t1 t2)))
      (setq a (1+ a) b (+ b a) c (+ c b) d (+ d c) e (+ e d) i (1+ i)))
    (+ acc t1 t2 a b c d e)))

(ert-deftest regraw-fifth-register-is-rbx-on-sysv-only ()
  (let* ((optin '((rg_run a b c d e)))
         (sysv (vmreg-test--decision vmreg-test--five optin 'sysv))
         (win (vmreg-test--decision vmreg-test--five optin 'win64)))
    (should (eq (car (car sysv)) :admit))
    (should (equal (mapcar #'cdr (cdr (car sysv))) '(r12 r13 r14 r15 rbx)))
    (should (eq (car (car win)) :admit))
    (should (equal (mapcar #'cdr (cdr (car win))) '(r12 r13 r14 r15)))))

(ert-deftest regraw-region-runs-beside-the-map ()
  "A call-free region inside a vm-register function keeps its variables in r8/r9 and leaves the
map's slots alone; forbidding the coexistence leaves strictly more frame traffic."
  (skip-unless (executable-find "objdump"))
  (let* ((nelisp-aot-compiler-vm-register-functions '((rg_run a b c d e)))
         (nelisp-aot-compiler--vm-reg-decisions nil)
         (count-refs
          (lambda ()
            (let* ((asm (vmreg-test--disasm (vmreg-test--emit (nth 2 vmreg-test--five))))
                   (lines (split-string asm "\n")))
              (list (cl-count-if (lambda (l) (string-match-p "rbp-0x" l)) lines)
                    (cl-count-if (lambda (l) (string-match-p "rbp-0x8\\]" l)) lines)
                    (cl-count-if (lambda (l) (string-match-p "\\br[89]\\b" l)) lines)))))
         (with (funcall count-refs))
         (without (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-region-map-free-p)
                             (lambda () (null nelisp-aot-compiler--promoted-slots))))
                    (funcall count-refs))))
    (should (> (nth 2 with) (nth 2 without)))   ; the region uses r8/r9
    (should (< (nth 0 with) (nth 0 without)))   ; and the frame sees less traffic
    (should (<= (nth 1 with) 2))))              ; slot 0 (a): prologue spill and register load only

(ert-deftest regraw-e2e-five-registers-and-region ()
  "Native run: the result is the same with and without the opt-in."
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (let ((want (mod (vmreg-test--five-expected 1 2 3 4 5 6) 256))
        (prog `(seq ,@(cdr vmreg-test--five) (exit (rg_run 1 2 3 4 5 6)))))
    (dolist (optin '(nil ((rg_run a b c d e))))
      (let ((path (make-temp-file "regraw-e2e"))
            (nelisp-aot-compiler--x86-frame-copy-aliases nil)
            (nelisp-aot-compiler-vm-register-functions optin))
        (unwind-protect
            (progn (nelisp-aot-compile-sexp prog path)
                   (should (= (call-process path nil nil nil) want)))
          (when (file-exists-p path) (delete-file path)))))))

(defun vmreg-test--smap-masks (src optin)
  "Return the live masks of every call site of SRC's functions, compiled with OPTIN opted in."
  (let* ((nelisp-aot-compiler-vm-register-functions optin)
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--label-counter 0)
         (nelisp-aot-compiler--x86-frame-copy-aliases nil)
         (ir (nelisp-aot-compiler--parse src nil))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer 'sysv))
         (cell (list nil))
         (nelisp-aot-compiler--stackmap-records cell))
    (dolist (d defuns) (nelisp-aot-compiler--emit-defun d buf))
    (let* ((fixups (copy-sequence (aref buf 3)))
           (relocs (nelisp-asm-x86_64-extract-relocs buf))
           (text (nelisp-asm-x86_64-resolve-fixups buf)))
      (mapcar (lambda (rec) (cons (plist-get rec :name) (mapcar #'cdr (plist-get rec :sites))))
              (nelisp-aot-compiler--stackmap-finish (car cell) text relocs fixups)))))

(ert-deftest regraw-stackmap-masks-unchanged-by-promotion ()
  "The stack maps list the same live words with and without the register plan: promoted slots stay
described (their words are stale but scanned) and no reference slot is dropped."
  (should (equal (vmreg-test--smap-masks vmreg-test--five nil)
                 (vmreg-test--smap-masks vmreg-test--five '((rg_run a b c d e)))))
  (should (cdr (assoc "rg_run" (vmreg-test--smap-masks vmreg-test--five '((rg_run a b c d e)))))))

(provide 'nelisp-aot-vmreg-test)
;;; nelisp-aot-vmreg-test.el ends here
