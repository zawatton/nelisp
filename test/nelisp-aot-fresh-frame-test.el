;;; nelisp-aot-fresh-frame-test.el --- Fresh runtime slots -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun nelisp-fresh-frame-test--scalar-slots (buf slots)
  "Retain the reviewed scalar loop as the matched performance control."
  (when (> slots 0)
    (nelisp-asm-x86_64-emit-bytes
     buf (apply #'unibyte-string
                (append '(#x31 #xc0 #x49 #x89 #xe2 #x41 #xbb)
                        (cl-loop for shift from 0 below 32 by 8
                                 collect (logand (ash slots (- shift)) 255))
                        '(#x49 #x89 #x02 #x49 #x83 #xc2 #x08
                          #x49 #x83 #xeb #x01 #x75 #xf3))))))

(defun nelisp-fresh-frame-test--definition (source slots)
  (nelisp-aot-compiler--ir-with
   (car (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse source)))
   :rt-slot-count slots))

(defun nelisp-fresh-frame-test--emit (ir selected &optional abi arch)
  (let* ((nelisp-aot-compiler--abi (or abi 'sysv))
         (nelisp-aot-compiler--arch (or arch 'x86_64))
         (nelisp-aot-compiler-zero-fresh-frame-functions (and selected '(fresh_probe)))
         (buf (nelisp-asm-x86_64-make-buffer nelisp-aot-compiler--abi)))
    (nelisp-aot-compiler--emit-defun ir buf)
    (nelisp-asm-x86_64-resolve-fixups buf)))

(ert-deftest nelisp-fresh-frame-loop-is-bounded-and-exact ()
  (let ((zero (nelisp-asm-x86_64-make-buffer))
        (large (nelisp-asm-x86_64-make-buffer)))
    (nelisp-aot-compiler--emit-zero-fresh-rt-slots zero 0)
    (should (= (length (nelisp-asm-x86_64-resolve-fixups zero)) 0))
    (nelisp-aot-compiler--emit-zero-fresh-rt-slots large 7438)
    (should (= (length (nelisp-asm-x86_64-resolve-fixups large)) 26))
    (should-error (nelisp-aot-compiler--emit-zero-fresh-rt-slots large 4294967296)
                  :type 'nelisp-aot-compiler-error)))

(ert-deftest nelisp-fresh-frame-preserves-unselected-and-rounds-only-runtime-slots ()
  (let* ((ir (nelisp-fresh-frame-test--definition '(defun fresh_probe (a b c) a) 3))
         (plain (nelisp-fresh-frame-test--emit ir nil))
         (selected (nelisp-fresh-frame-test--emit ir t))
         (loop (nelisp-asm-x86_64-make-buffer))
         (nelisp-aot-compiler-zero-fresh-frame-functions '(other_function)))
    (nelisp-aot-compiler--emit-zero-fresh-rt-slots loop 4)
    (should (string-match-p (regexp-quote (nelisp-asm-x86_64-resolve-fixups loop)) selected))
    (should-not (string-match-p (regexp-quote (nelisp-asm-x86_64-resolve-fixups loop)) plain))
    (should (equal plain (nelisp-fresh-frame-test--emit ir nil)))
    ;; Three parameter spills and their pad precede the runtime reservation.
    (should (string-prefix-p (unibyte-string #x55 #x48 #x89 #xe5 #x57 #x56 #x52
                                             #x48 #x81 #xec 8 0 0 0
                                             #x48 #x81 #xec 32 0 0 0) selected))))

(ert-deftest nelisp-fresh-frame-zeroes-region-and-pad-of-stack-parameter-entry ()
  ;; m5_fx_run_body has eight parameters (two stack-passed).  Selection must not
  ;; fail, must zero the runtime region and the RBX pad, and must leave the
  ;; unselected prologue bytes untouched.
  (let* ((ir (nelisp-fresh-frame-test--definition
              '(defun fresh_probe (a b c d e f g h) (+ a h)) 4))
         (plain (nelisp-fresh-frame-test--emit ir nil))
         (selected (nelisp-fresh-frame-test--emit ir t))
         (loop (nelisp-asm-x86_64-make-buffer))
         ;; Pad slot: 8 * (2 + 8 parameter slots + 4 runtime slots) below RBP.
         (pad (concat (unibyte-string #x48 #xc7 #x85)
                      (unibyte-string #x90 #xff #xff #xff) (unibyte-string 0 0 0 0))))
    (nelisp-aot-compiler--emit-zero-fresh-rt-slots loop 4)
    (should (string-match-p (regexp-quote (nelisp-asm-x86_64-resolve-fixups loop)) selected))
    (should (string-match-p (regexp-quote pad) selected))
    (should-not (string-match-p (regexp-quote (nelisp-asm-x86_64-resolve-fixups loop)) plain))
    (should-not (string-match-p (regexp-quote pad) plain))
    (should (equal plain (nelisp-fresh-frame-test--emit ir nil)))))

(ert-deftest nelisp-fresh-frame-rejects-unqualified-entry-paths ()
  (let ((ir (nelisp-fresh-frame-test--definition '(defun fresh_probe (a b) a) 2)))
    (should-error (nelisp-fresh-frame-test--emit ir t 'win64) :type 'nelisp-aot-compiler-error)
    (should-error (nelisp-fresh-frame-test--emit ir t 'sysv 'aarch64) :type 'nelisp-aot-compiler-error)
    (dolist (field '(:internal-entry :self-tco-raw :variadic :gc-root-slots))
      (should-error (nelisp-fresh-frame-test--emit
                     (nelisp-aot-compiler--ir-with ir field t) t)
                    :type 'nelisp-aot-compiler-error))))

(ert-deftest nelisp-fresh-frame-disables-bypass-and-prefix-plans ()
  (let ((ir (nelisp-fresh-frame-test--definition '(defun fresh_probe (a b) (+ a b)) 4)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-local-prefix-plan)
               (lambda (&rest _) (error "prefix bypass")))
              ((symbol-function 'nelisp-aot-compiler--x86-self-tco-entry-plan)
               (lambda (&rest _) (error "TCO bypass")))
              ((symbol-function 'nelisp-aot-compiler--x86-leaf-entry-plan)
               (lambda (&rest _) (error "leaf bypass")))
              ((symbol-function 'nelisp-aot-compiler--x86-terminal-entry-plan)
               (lambda (&rest _) (error "terminal bypass")))
              ((symbol-function 'nelisp-aot-compiler--x86-shrink-wrap-plan)
               (lambda (&rest _) (error "shrink-wrap bypass"))))
      (should (> (length (nelisp-fresh-frame-test--emit ir t)) 0)))))

(ert-deftest nelisp-fresh-frame-owner-selects-linux-only-and-cache-discriminates ()
  (dolist (target '(linux-x86_64 windows-x86_64 linux-aarch64))
    (let ((nelisp-standalone--target target))
      (should (equal (nelisp-standalone--zero-fresh-frame-functions)
                     (and (eq target 'linux-x86_64)
                          '(nelisp_apply_function_w nl_apply_builtin_w nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w
                            nl_apply_builtin_frame_group_0_w nl_apply_builtin_frame_group_1_w
                            nl_apply_builtin_frame_w nl_eic_argframe_call_w
                            bf_garbage_collect_w bf_garbage_collect_snapshot_w nl_gc_stats_run
                     m5_fx_run_body))))))
  (let* ((source '(seq (defun fresh_probe (a) a)))
         (nelisp-standalone--target 'linux-x86_64)
         (normal (nelisp-standalone--unit-cache-key source)))
    (cl-letf (((symbol-function 'nelisp-standalone--zero-fresh-frame-functions) (lambda () nil)))
      (should-not (equal normal (nelisp-standalone--unit-cache-key source))))))

(ert-deftest nelisp-fresh-frame-native-production-prologue-control ()
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (let* ((directory (make-temp-file "fresh-frame-native-" t))
         (source '(defun fresh_probe (mode a b c)
                    (if (= mode 1)
                        (seq (ptr-write-u64 (+ (aot-current-sp) 16) 0 3735928559)
                             (+ a (+ b c)))
                      (ptr-read-u64 (+ (aot-current-sp) 16) 0))))
         (ir (nelisp-fresh-frame-test--definition source 7437))
         (runner (expand-file-name "test/nelisp-aot-fresh-frame-native.py"
                                    nelisp-standalone--repo-root)))
    (unwind-protect
        (progn
          (let ((buf (nelisp-asm-x86_64-make-buffer))
                (coding-system-for-write 'binary))
            (nelisp-aot-compiler--emit-zero-fresh-rt-slots buf 4)
            (with-temp-file (expand-file-name "loop.bin" directory)
              (set-buffer-multibyte nil)
              (insert (nelisp-asm-x86_64-resolve-fixups buf))))
          (let ((buf (nelisp-asm-x86_64-make-buffer))
                (coding-system-for-write 'binary))
            (nelisp-fresh-frame-test--scalar-slots buf 4)
            (with-temp-file (expand-file-name "scalar-loop.bin" directory)
              (set-buffer-multibyte nil)
              (insert (nelisp-asm-x86_64-resolve-fixups buf))))
          (dolist (kind '(control initialized scalar))
            (let ((coding-system-for-write 'binary)
                  (bytes (pcase kind
                           ('control
                            (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-zero-fresh-rt-slots)
                                       (lambda (&rest _) nil)))
                              (nelisp-fresh-frame-test--emit ir t)))
                           ('scalar
                            (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-zero-fresh-rt-slots)
                                       #'nelisp-fresh-frame-test--scalar-slots))
                              (nelisp-fresh-frame-test--emit ir t)))
                           (_ (nelisp-fresh-frame-test--emit ir t)))))
              (with-temp-file (expand-file-name (format "%s.bin" kind) directory)
                (set-buffer-multibyte nil) (insert bytes))))
          (with-temp-buffer
            (should (= 0 (call-process "python3" nil t nil runner directory)))
            (princ (buffer-string))))
      (delete-directory directory t))))
