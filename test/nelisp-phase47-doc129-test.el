;;; nelisp-phase47-doc129-test.el --- Doc 129 Phase47 frontend tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-elf-write)
(require 'nelisp-phase47-compiler)

(defun nelisp-phase47-doc129-test--linux-p ()
  "Return non-nil when this host can exec x86_64 ELF64 binaries."
  (and (eq system-type 'gnu/linux)
       (stringp system-configuration)
       (string-match-p "x86_64\\|amd64" system-configuration)))

(defun nelisp-phase47-doc129-test--tmp-binary (suffix)
  "Return a fresh temporary binary path using SUFFIX."
  (make-temp-file (format "nelisp-doc129-%s-" suffix)))

(defun nelisp-phase47-doc129-test--run-binary (path)
  "Exec PATH and return its process exit code."
  (call-process path nil nil nil))

(defun nelisp-phase47-doc129-test--extern-call-names (ir)
  "Return extern-call names found while walking IR."
  (let (names)
    (nelisp-phase47-compiler--walk-ir
     ir
     (lambda (node)
       (when (eq (nelisp-phase47-compiler--ir-kind node) 'extern-call)
         (push (nelisp-phase47-compiler--ir-get node :name) names))))
    (nreverse names)))

(ert-deftest nelisp-phase47-doc129/value-seq-from-when-progn ()
  "Doc 129.1: multi-form macro body becomes a value-seq branch."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun f (x)
                 (when (> x 0)
                   (+ x 1)
                   (+ x 2)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (then-branch (nelisp-phase47-compiler--ir-get body :then)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (eq (nelisp-phase47-compiler--ir-kind then-branch) 'value-seq))
    (should (= (length (nelisp-phase47-compiler--ir-get then-branch :forms))
               2))))

(ert-deftest nelisp-phase47-doc129/e2e-value-seq-from-when-progn ()
  "Doc 129.1: value-seq returns the final child value."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "value-seq")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun f (x)
               (when (> x 0)
                 (+ x 1)
                 (+ x 2)))
             (exit (f 3)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 5)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/e2e-user-macro-progn ()
  "Doc 129.1: user macros that emit multi-form progn compile."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "user-progn")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defmacro plus-two-last (x)
               (list 'progn (list '+ x 1) (list '+ x 2)))
             (exit (plus-two-last 8)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 10)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-defun-sexp-int ()
  "Doc 129.2 MVP: `defun-sexp-int' lowers to unwrap + int-make."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-int add (out a b) (+ a b))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (val (nelisp-phase47-compiler--ir-get body :val)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-int-make))
    (should (eq (nelisp-phase47-compiler--ir-kind val) 'arith))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get val :a))
                'sexp-int-unwrap))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get val :b))
                'sexp-int-unwrap))))

(ert-deftest nelisp-phase47-doc129/e2e-defun-sexp-int-add ()
  "Doc 129.2 MVP: boxed-boundary Int add writes Sexp::Int result."
  (skip-unless (and (executable-find "ld")
                    (nelisp-phase47-doc129-test--linux-p)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-sexp-int-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-sexp-int-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-sexp-int-bin-" nil ""))
         (int-a (concat (unibyte-string 2 0 0 0 0 0 0 0)
                        (unibyte-string 5 0 0 0 0 0 0 0)))
         (int-b (concat (unibyte-string 2 0 0 0 0 0 0 0)
                        (unibyte-string 8 0 0 0 0 0 0 0)))
         (slot (make-string 32 #xFF))
         (data-bytes (concat slot int-a int-b))
         (host-text
          (concat
           ;; lea rdi, [rip + slot]
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; lea rsi, [rip + int_a]
           (unibyte-string #x48 #x8D #x35 0 0 0 0)
           ;; lea rdx, [rip + int_b]
           (unibyte-string #x48 #x8D #x15 0 0 0 0)
           ;; call add
           (unibyte-string #xE8 0 0 0 0)
           ;; mov rdi, [rax + 8]
           (unibyte-string #x48 #x8B #x78 #x08)
           ;; mov eax, 60; syscall
           (unibyte-string #xB8 #x3C 0 0 0 #x0F #x05))))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun-sexp-int add (out a b) (+ a b))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "slot" :value 0 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "int_a" :value 32 :size 16
                                 :section 'data :bind 'global :type 'object)
                           (list :name "int_b" :value 48 :size 16
                                 :section 'data :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size (length host-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "add" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 10
                                :symbol "int_a" :type 'pc32 :addend -4)
                          (list :section 'text :offset 17
                                :symbol "int_b" :type 'pc32 :addend -4)
                          (list :section 'text :offset 22
                                :symbol "add" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          (should (= (nelisp-phase47-doc129-test--run-binary bin-path) 13)))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-doc129/parse-defun-sexp-int-setq ()
  "Doc 129.3A: boxed Int setq lowers to env_set_value delegation."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-int-setq set_x
                   (out mirror frames scratch name a)
                 (+ a 16))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (make-node (nth 0 forms))
         (set-node (nth 1 forms))
         (ret-node (nth 2 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind make-node) 'sexp-int-make))
    (should (eq (nelisp-phase47-compiler--ir-kind set-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get set-node :name)
                'nelisp_env_set_value))
    (should (eq (nelisp-phase47-compiler--ir-kind ret-node) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get ret-node :var) 'out))))

(ert-deftest nelisp-phase47-doc129/e2e-defun-sexp-int-setq-env-call ()
  "Doc 129.3A: setq wrapper boxes Int, calls env setter, returns OUT."
  (skip-unless (and (executable-find "ld")
                    (nelisp-phase47-doc129-test--linux-p)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-setq-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-setq-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-setq-bin-" nil ""))
         (slot (make-string 32 #xFF))
         (zero-slot (make-string 32 0))
         (int-a (concat (unibyte-string 2 0 0 0 0 0 0 0)
                        (unibyte-string 5 0 0 0 0 0 0 0)))
         (data-bytes (concat slot zero-slot zero-slot zero-slot zero-slot int-a))
         (start-text
          (concat
           ;; lea rdi, [rip + slot]
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; lea rsi, [rip + mirror]
           (unibyte-string #x48 #x8D #x35 0 0 0 0)
           ;; lea rdx, [rip + frames]
           (unibyte-string #x48 #x8D #x15 0 0 0 0)
           ;; lea rcx, [rip + scratch]
           (unibyte-string #x48 #x8D #x0D 0 0 0 0)
           ;; lea r8, [rip + name]
           (unibyte-string #x4C #x8D #x05 0 0 0 0)
           ;; lea r9, [rip + int_a]
           (unibyte-string #x4C #x8D #x0D 0 0 0 0)
           ;; call set_x
           (unibyte-string #xE8 0 0 0 0)
           ;; mov rdi, [rax + 8]
           (unibyte-string #x48 #x8B #x78 #x08)
           ;; mov eax, 60; syscall
           (unibyte-string #xB8 #x3C 0 0 0 #x0F #x05)))
         (setter-offset (length start-text))
         (host-text
          (concat
           start-text
           ;; Test stub for nelisp_env_set_value:
           ;; add qword ptr [rcx + 8], 1; xor eax, eax; ret
           ;; The increment proves the extern call saw VAL-PTR = OUT.
           (unibyte-string #x48 #x83 #x41 #x08 #x01 #x31 #xC0 #xC3))))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun-sexp-int-setq set_x
                (out mirror frames scratch name a)
              (+ a 16))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "slot" :value 0 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "mirror" :value 32 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "frames" :value 64 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "scratch" :value 96 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "name" :value 128 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "int_a" :value 160 :size 16
                                 :section 'data :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size setter-offset
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nelisp_env_set_value"
                                 :value setter-offset
                                 :size (- (length host-text) setter-offset)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "set_x" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 10
                                :symbol "mirror" :type 'pc32 :addend -4)
                          (list :section 'text :offset 17
                                :symbol "frames" :type 'pc32 :addend -4)
                          (list :section 'text :offset 24
                                :symbol "scratch" :type 'pc32 :addend -4)
                          (list :section 'text :offset 31
                                :symbol "name" :type 'pc32 :addend -4)
                          (list :section 'text :offset 38
                                :symbol "int_a" :type 'pc32 :addend -4)
                          (list :section 'text :offset 43
                                :symbol "set_x" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          ;; BODY makes 21; the stub env setter increments OUT to 22.
          (should (= (nelisp-phase47-doc129-test--run-binary bin-path) 22)))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-doc129/parse-defun-sexp-int-setq-symbol ()
  "Doc 129.3B: setq wrapper can materialize a symbol literal."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-int-setq-symbol set_x x
                   (out mirror frames scratch name-slot a)
                 (+ a 16))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (sym-node (nth 0 forms))
         (make-node (nth 1 forms))
         (set-node (nth 2 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind sym-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get sym-node :bytes)
                   (string-to-list "x")))
    (should (eq (nelisp-phase47-compiler--ir-kind make-node) 'sexp-int-make))
    (should (eq (nelisp-phase47-compiler--ir-kind set-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get set-node :name)
                'nelisp_env_set_value))))

(ert-deftest nelisp-phase47-doc129/e2e-defun-sexp-int-setq-symbol ()
  "Doc 129.3B: symbol literal is materialized before env_set_value."
  (skip-unless (and (executable-find "ld")
                    (nelisp-phase47-doc129-test--linux-p)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-setq-sym-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-setq-sym-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-setq-sym-bin-" nil ""))
         (slot (make-string 32 #xFF))
         (zero-slot (make-string 32 0))
         (int-a (concat (unibyte-string 2 0 0 0 0 0 0 0)
                        (unibyte-string 5 0 0 0 0 0 0 0)))
         (data-bytes (concat slot zero-slot zero-slot zero-slot zero-slot int-a))
         (start-text
          (concat
           ;; lea rdi, [rip + slot]
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; lea rsi, [rip + mirror]
           (unibyte-string #x48 #x8D #x35 0 0 0 0)
           ;; lea rdx, [rip + frames]
           (unibyte-string #x48 #x8D #x15 0 0 0 0)
           ;; lea rcx, [rip + scratch]
           (unibyte-string #x48 #x8D #x0D 0 0 0 0)
           ;; lea r8, [rip + name_slot]
           (unibyte-string #x4C #x8D #x05 0 0 0 0)
           ;; lea r9, [rip + int_a]
           (unibyte-string #x4C #x8D #x0D 0 0 0 0)
           ;; call set_x
           (unibyte-string #xE8 0 0 0 0)
           ;; mov rdi, [rax + 8]
           (unibyte-string #x48 #x8B #x78 #x08)
           ;; mov eax, 60; syscall
           (unibyte-string #xB8 #x3C 0 0 0 #x0F #x05)))
         (alloc-offset (length start-text))
         (alloc-text
          (concat
           ;; nl_alloc_symbol stub:
           ;; mov byte ptr [rdx], 4
           (unibyte-string #xC6 #x02 #x04)
           ;; movzx rax, byte ptr [rdi]
           (unibyte-string #x48 #x0F #xB6 #x07)
           ;; mov [rdx + 8], rax
           (unibyte-string #x48 #x89 #x42 #x08)
           ;; mov rax, rdx; ret
           (unibyte-string #x48 #x89 #xD0 #xC3)))
         (setter-offset (+ alloc-offset (length alloc-text)))
         (setter-text
          (concat
           ;; nelisp_env_set_value stub:
           ;; mov rax, [rdx + 8]    ; first byte materialized by alloc stub
           (unibyte-string #x48 #x8B #x42 #x08)
           ;; add [rcx + 8], rax    ; OUT += byte('x') = 120
           (unibyte-string #x48 #x01 #x41 #x08)
           ;; xor eax, eax; ret
           (unibyte-string #x31 #xC0 #xC3)))
         (host-text (concat start-text alloc-text setter-text)))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun-sexp-int-setq-symbol set_x x
                (out mirror frames scratch name_slot a)
              (+ a 16))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "slot" :value 0 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "mirror" :value 32 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "frames" :value 64 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "scratch" :value 96 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "name_slot" :value 128 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "int_a" :value 160 :size 16
                                 :section 'data :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size (length start-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nl_alloc_symbol"
                                 :value alloc-offset
                                 :size (length alloc-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nelisp_env_set_value"
                                 :value setter-offset
                                 :size (length setter-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "set_x" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 10
                                :symbol "mirror" :type 'pc32 :addend -4)
                          (list :section 'text :offset 17
                                :symbol "frames" :type 'pc32 :addend -4)
                          (list :section 'text :offset 24
                                :symbol "scratch" :type 'pc32 :addend -4)
                          (list :section 'text :offset 31
                                :symbol "name_slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 38
                                :symbol "int_a" :type 'pc32 :addend -4)
                          (list :section 'text :offset 43
                                :symbol "set_x" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          ;; BODY makes 21; setter adds materialized byte('x') = 120.
          (should (= (nelisp-phase47-doc129-test--run-binary bin-path) 141)))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-doc129/parse-defun-sexp-int-defvar-symbol ()
  "Doc 129.3C: defvar wrapper conditionally delegates value-cell set."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-int-defvar-symbol ensure_x x
                   (out mirror frames scratch name-slot a)
                 (+ a 16))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (sym-node (nth 0 forms))
         (if-node (nth 1 forms))
         (test-node (nelisp-phase47-compiler--ir-get if-node :test))
         (bound-node (nelisp-phase47-compiler--ir-get test-node :a))
         (bound-args (nelisp-phase47-compiler--ir-get bound-node :args))
         (else-node (nelisp-phase47-compiler--ir-get if-node :else))
         (else-forms (nelisp-phase47-compiler--ir-get else-node :forms))
         (set-node (nth 1 else-forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind sym-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get sym-node :bytes)
                   (string-to-list "x")))
    (should (eq (nelisp-phase47-compiler--ir-kind if-node) 'if))
    (should (eq (nelisp-phase47-compiler--ir-get bound-node :name)
                'nelisp_mirror_is_bound))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 2 bound-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get if-node :then))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-kind else-node) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 0 else-forms))
                'sexp-int-make))
    (should (eq (nelisp-phase47-compiler--ir-kind set-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get set-node :name)
                'nelisp_env_set_value))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 2 else-forms))
                'ref))))

(ert-deftest nelisp-phase47-doc129/e2e-defun-sexp-int-defvar-symbol ()
  "Doc 129.3C: defvar symbol helper sets only when unbound."
  (skip-unless (and (executable-find "ld")
                    (nelisp-phase47-doc129-test--linux-p)))
  (dolist (case '((0 . 121) (1 . 120)))
    (let* ((is-bound (car case))
           (expected (cdr case))
           (probe-path (make-temp-file "nelisp-doc129-defvar-sym-probe-" nil ".o"))
           (host-path (make-temp-file "nelisp-doc129-defvar-sym-host-" nil ".o"))
           (bin-path (make-temp-file "nelisp-doc129-defvar-sym-bin-" nil ""))
           (slot (make-string 32 #xFF))
           (zero-slot (make-string 32 0))
           (int-a (concat (unibyte-string 2 0 0 0 0 0 0 0)
                          (unibyte-string 5 0 0 0 0 0 0 0)))
           (data-bytes (concat slot zero-slot zero-slot zero-slot
                               zero-slot int-a))
           (start-text
            (concat
             ;; lea rdi, [rip + slot]
             (unibyte-string #x48 #x8D #x3D 0 0 0 0)
             ;; lea rsi, [rip + mirror]
             (unibyte-string #x48 #x8D #x35 0 0 0 0)
             ;; lea rdx, [rip + frames]
             (unibyte-string #x48 #x8D #x15 0 0 0 0)
             ;; lea rcx, [rip + scratch]
             (unibyte-string #x48 #x8D #x0D 0 0 0 0)
             ;; lea r8, [rip + name_slot]
             (unibyte-string #x4C #x8D #x05 0 0 0 0)
             ;; lea r9, [rip + int_a]
             (unibyte-string #x4C #x8D #x0D 0 0 0 0)
             ;; call ensure_x
             (unibyte-string #xE8 0 0 0 0)
             ;; mov rdi, [rax + 8]
             (unibyte-string #x48 #x8B #x78 #x08)
             ;; mov eax, 60; syscall
             (unibyte-string #xB8 #x3C 0 0 0 #x0F #x05)))
           (alloc-offset (length start-text))
           (alloc-text
            (concat
             ;; nl_alloc_symbol stub:
             ;; mov byte ptr [rdx], 4
             (unibyte-string #xC6 #x02 #x04)
             ;; movzx rax, byte ptr [rdi]
             (unibyte-string #x48 #x0F #xB6 #x07)
             ;; mov [rdx + 8], rax
             (unibyte-string #x48 #x89 #x42 #x08)
             ;; mov rax, rdx; ret
             (unibyte-string #x48 #x89 #xD0 #xC3)))
           (bound-offset (+ alloc-offset (length alloc-text)))
           (bound-text
            (concat
             ;; nelisp_mirror_is_bound stub: mov eax, IS_BOUND; ret
             (unibyte-string #xB8 is-bound 0 0 0 #xC3)))
           (setter-offset (+ bound-offset (length bound-text)))
           (setter-text
            (concat
             ;; nelisp_env_set_value stub:
             ;; add qword ptr [rdx + 8], 1 ; NAME-SLOT byte changes only here
             (unibyte-string #x48 #x83 #x42 #x08 #x01)
             ;; xor eax, eax; ret
             (unibyte-string #x31 #xC0 #xC3)))
           (host-text (concat start-text alloc-text bound-text setter-text)))
      (unwind-protect
          (progn
            (nelisp-phase47-compile-to-object
             '(defun-sexp-int-defvar-symbol ensure_x x
                  (out mirror frames scratch name_slot a)
                (+ a 16))
             probe-path)
            (nelisp-elf-write-binary
             host-path
             (list :e-type 'rel
                   :text host-text
                   :data data-bytes
                   :symbols (list
                             (list :name "slot" :value 0 :size 32
                                   :section 'data :bind 'global :type 'object)
                             (list :name "mirror" :value 32 :size 32
                                   :section 'data :bind 'global :type 'object)
                             (list :name "frames" :value 64 :size 32
                                   :section 'data :bind 'global :type 'object)
                             (list :name "scratch" :value 96 :size 32
                                   :section 'data :bind 'global :type 'object)
                             (list :name "name_slot" :value 128 :size 32
                                   :section 'data :bind 'global :type 'object)
                             (list :name "int_a" :value 160 :size 16
                                   :section 'data :bind 'global :type 'object)
                             (list :name "_start" :value 0
                                   :size (length start-text)
                                   :section 'text :bind 'global :type 'func)
                             (list :name "nl_alloc_symbol"
                                   :value alloc-offset
                                   :size (length alloc-text)
                                   :section 'text :bind 'global :type 'func)
                             (list :name "nelisp_mirror_is_bound"
                                   :value bound-offset
                                   :size (length bound-text)
                                   :section 'text :bind 'global :type 'func)
                             (list :name "nelisp_env_set_value"
                                   :value setter-offset
                                   :size (length setter-text)
                                   :section 'text :bind 'global :type 'func)
                             (list :name "ensure_x" :section 'undef
                                   :bind 'global :type 'notype))
                   :relocs (list
                            (list :section 'text :offset 3
                                  :symbol "slot" :type 'pc32 :addend -4)
                            (list :section 'text :offset 10
                                  :symbol "mirror" :type 'pc32 :addend -4)
                            (list :section 'text :offset 17
                                  :symbol "frames" :type 'pc32 :addend -4)
                            (list :section 'text :offset 24
                                  :symbol "scratch" :type 'pc32 :addend -4)
                            (list :section 'text :offset 31
                                  :symbol "name_slot" :type 'pc32 :addend -4)
                            (list :section 'text :offset 38
                                  :symbol "int_a" :type 'pc32 :addend -4)
                            (list :section 'text :offset 43
                                  :symbol "ensure_x" :type 'plt32 :addend -4))))
            (let ((ld-status
                   (call-process "ld" nil nil nil
                                 "-o" bin-path probe-path host-path)))
              (should (zerop ld-status)))
            (set-file-modes bin-path #o755)
            (should (= (nelisp-phase47-doc129-test--run-binary bin-path)
                       expected)))
        (ignore-errors (delete-file probe-path))
        (ignore-errors (delete-file host-path))
        (ignore-errors (delete-file bin-path))))))

(ert-deftest nelisp-phase47-doc129/parse-defun-sexp-int-defconst-symbol ()
  "Doc 129.3D: defconst wrapper writes value and marks constant."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-int-defconst-symbol const_x x
                   (out mirror frames scratch name-slot a)
                 (+ a 16))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (clear-node (nth 2 forms))
         (set-node (nth 4 forms))
         (mark-node (nth 6 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 0 forms))
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get (nth 0 forms) :bytes)
                   (string-to-list "x")))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 1 forms))
                'sexp-write-nil))
    (should (eq (nelisp-phase47-compiler--ir-kind clear-node)
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get clear-node :name)
                'nelisp_mirror_set_constant))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 3 forms))
                'sexp-int-make))
    (should (eq (nelisp-phase47-compiler--ir-kind set-node)
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get set-node :name)
                'nelisp_env_set_value))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 5 forms))
                'sexp-write-t))
    (should (eq (nelisp-phase47-compiler--ir-kind mark-node)
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get mark-node :name)
                'nelisp_mirror_set_constant))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 forms))
                'ref))))

(ert-deftest nelisp-phase47-doc129/e2e-defun-sexp-int-defconst-symbol ()
  "Doc 129.3D: defconst symbol helper clears, writes, and marks constant."
  (skip-unless (and (executable-find "ld")
                    (nelisp-phase47-doc129-test--linux-p)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-defconst-sym-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-defconst-sym-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-defconst-sym-bin-" nil ""))
         (slot (make-string 32 #xFF))
         (zero-slot (make-string 32 0))
         (int-a (concat (unibyte-string 2 0 0 0 0 0 0 0)
                        (unibyte-string 5 0 0 0 0 0 0 0)))
         (data-bytes (concat slot zero-slot zero-slot zero-slot zero-slot int-a))
         (start-text
          (concat
           ;; lea rdi, [rip + slot]
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; lea rsi, [rip + mirror]
           (unibyte-string #x48 #x8D #x35 0 0 0 0)
           ;; lea rdx, [rip + frames]
           (unibyte-string #x48 #x8D #x15 0 0 0 0)
           ;; lea rcx, [rip + scratch]
           (unibyte-string #x48 #x8D #x0D 0 0 0 0)
           ;; lea r8, [rip + name_slot]
           (unibyte-string #x4C #x8D #x05 0 0 0 0)
           ;; lea r9, [rip + int_a]
           (unibyte-string #x4C #x8D #x0D 0 0 0 0)
           ;; call const_x
           (unibyte-string #xE8 0 0 0 0)
           ;; mov rdi, [rax + 8]
           (unibyte-string #x48 #x8B #x78 #x08)
           ;; mov eax, 60; syscall
           (unibyte-string #xB8 #x3C 0 0 0 #x0F #x05)))
         (alloc-offset (length start-text))
         (alloc-text
          (concat
           ;; nl_alloc_symbol stub:
           ;; mov byte ptr [rdx], 4
           (unibyte-string #xC6 #x02 #x04)
           ;; movzx rax, byte ptr [rdi]
           (unibyte-string #x48 #x0F #xB6 #x07)
           ;; mov [rdx + 8], rax
           (unibyte-string #x48 #x89 #x42 #x08)
           ;; mov rax, rdx; ret
           (unibyte-string #x48 #x89 #xD0 #xC3)))
         (constant-offset (+ alloc-offset (length alloc-text)))
         (constant-text
          (concat
           ;; nelisp_mirror_set_constant stub:
           ;; add qword ptr [rsi + 8], 2 ; count each constant write
           (unibyte-string #x48 #x83 #x46 #x08 #x02)
           ;; movzx rax, byte ptr [rdx] ; Nil=0 for clear, T=1 for mark
           (unibyte-string #x48 #x0F #xB6 #x02)
           ;; add [rsi + 8], rax
           (unibyte-string #x48 #x01 #x46 #x08)
           ;; mov eax, 1; ret
           (unibyte-string #xB8 #x01 0 0 0 #xC3)))
         (setter-offset (+ constant-offset (length constant-text)))
         (setter-text
          (concat
           ;; nelisp_env_set_value stub:
           ;; mov rax, [rcx + 8] ; boxed BODY payload = 21
           (unibyte-string #x48 #x8B #x41 #x08)
           ;; add [rdx + 8], rax
           (unibyte-string #x48 #x01 #x42 #x08)
           ;; xor eax, eax; ret
           (unibyte-string #x31 #xC0 #xC3)))
         (host-text (concat start-text alloc-text constant-text setter-text)))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun-sexp-int-defconst-symbol const_x x
                (out mirror frames scratch name_slot a)
              (+ a 16))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "slot" :value 0 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "mirror" :value 32 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "frames" :value 64 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "scratch" :value 96 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "name_slot" :value 128 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "int_a" :value 160 :size 16
                                 :section 'data :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size (length start-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nl_alloc_symbol"
                                 :value alloc-offset
                                 :size (length alloc-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nelisp_mirror_set_constant"
                                 :value constant-offset
                                 :size (length constant-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "nelisp_env_set_value"
                                 :value setter-offset
                                 :size (length setter-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "const_x" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 10
                                :symbol "mirror" :type 'pc32 :addend -4)
                          (list :section 'text :offset 17
                                :symbol "frames" :type 'pc32 :addend -4)
                          (list :section 'text :offset 24
                                :symbol "scratch" :type 'pc32 :addend -4)
                          (list :section 'text :offset 31
                                :symbol "name_slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 38
                                :symbol "int_a" :type 'pc32 :addend -4)
                          (list :section 'text :offset 43
                                :symbol "const_x" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          ;; name byte 120 + clear call 2 + BODY 21 + mark call 3.
          (should (= (nelisp-phase47-doc129-test--run-binary bin-path) 146)))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-doc129/parse-top-level-var-init-helpers ()
  "Doc 129.3E/F: top-level var declarations lower to AOT init helpers."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defvar x 42 "doc")
                (defconst y 7 "doc")
                (defcustom z 9 "doc" :type 'integer))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (defvar-ir (nth 0 forms))
         (defconst-ir (nth 1 forms))
         (defcustom-ir (nth 2 forms))
         (defvar-body (nelisp-phase47-compiler--ir-get defvar-ir :body))
         (defconst-body (nelisp-phase47-compiler--ir-get defconst-ir :body))
         (defcustom-body (nelisp-phase47-compiler--ir-get defcustom-ir :body))
         (defvar-forms (nelisp-phase47-compiler--ir-get defvar-body :forms))
         (defconst-forms (nelisp-phase47-compiler--ir-get defconst-body :forms))
         (defcustom-forms (nelisp-phase47-compiler--ir-get defcustom-body :forms)))
    (should (= (length forms) 3))
    (should (eq (nelisp-phase47-compiler--ir-get defvar-ir :name)
                'nelisp_aot_var_0_x))
    (should (eq (nelisp-phase47-compiler--ir-get defconst-ir :name)
                'nelisp_aot_const_1_y))
    (should (eq (nelisp-phase47-compiler--ir-get defcustom-ir :name)
                'nelisp_aot_custom_2_z))
    (should (equal (nelisp-phase47-compiler--ir-get defvar-ir :params)
                   '(out mirror frames scratch name_slot)))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 0 defvar-forms))
                'sexp-write-symbol-lit))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 1 defvar-forms))
                'if))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 0 defconst-forms))
                'sexp-write-symbol-lit))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 1 defconst-forms))
                'sexp-write-nil))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 defconst-forms))
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 0 defcustom-forms))
                'sexp-write-symbol-lit))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 1 defcustom-forms))
                'if))
    (should (equal (nelisp-phase47-compiler--ir-get defcustom-ir :params)
                   '(out mirror frames scratch name_slot)))))

(ert-deftest nelisp-phase47-doc129/top-level-defcustom-requires-docstring ()
  "Doc 129.3F: top-level defcustom validates its docstring."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defcustom z 9 :type 'integer)))))

(ert-deftest nelisp-phase47-doc129/top-level-defcustom-metadata-descriptor ()
  "Doc 129.3G: top-level defcustom exposes metadata for Doc 99."
  (let ((descriptors
         (nelisp-phase47-compiler--custom-metadata-descriptors
          '(seq
            (defvar x 42 "doc")
            (defcustom z 9 "doc" :type 'integer :group 'nelisp)))))
    (should
     (equal descriptors
            '((:name z
               :helper nelisp_aot_custom_1_z
               :standard 9
               :docstring "doc"
               :options (:type (quote integer) :group (quote nelisp))))))))

(ert-deftest nelisp-phase47-doc129/top-level-var-init-descriptors ()
  "Doc 129.3H: top-level var init helpers expose scheduling metadata."
  (let ((descriptors
         (nelisp-phase47-compiler--init-helper-descriptors
          '(seq
            (defvar no-init)
            (defvar x 42 "doc")
            (defconst y 7 "doc")
            (defcustom z 9 "doc" :type 'integer)))))
    (should
     (equal descriptors
            '((:kind defvar
               :name x
               :helper nelisp_aot_var_1_x
               :index 1)
              (:kind defconst
               :name y
               :helper nelisp_aot_const_2_y
               :index 2)
              (:kind defcustom
               :name z
               :helper nelisp_aot_custom_3_z
               :index 3))))))

(ert-deftest nelisp-phase47-doc129/top-level-defcustom-rejects-bad-options ()
  "Doc 129.3G: defcustom metadata options are keyword/value pairs."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defcustom z 9 "doc" :type))))
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defcustom z 9 "doc" type 'integer)))))

(ert-deftest nelisp-phase47-doc129/object-top-level-var-init-helpers ()
  "Doc 129.3E/F: object output accepts top-level var declaration forms."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-top-var-init-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defvar x 42 "doc")
             (defconst y 7 "doc")
             (defcustom z 9 "doc" :type 'integer))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_var_0_x" out))
            (should (string-match-p "nelisp_aot_const_1_y" out))
            (should (string-match-p "nelisp_aot_custom_2_z" out))
            (should (string-match-p "nelisp_env_set_value" out))
            (should (string-match-p "nelisp_mirror_set_constant" out))))
      (ignore-errors (delete-file path)))))


(ert-deftest nelisp-phase47-doc129/parse-multi-let-rt ()
  "Doc 129.4: multi-binding runtime `let' lowers to `let-rt-n'."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x y)
                      (let ((a (id x))
                            (b (+ y 10)))
                        (+ a b))))))
         (f-ir (nth 1 (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get f-ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt-n))
    (should (= (length (nelisp-phase47-compiler--ir-get body :bindings)) 2))
    (should (= (nelisp-phase47-compiler--ir-get f-ir :rt-slot-count) 2))))

(ert-deftest nelisp-phase47-doc129/e2e-multi-let-rt ()
  "Doc 129.4: execute a multi-binding runtime `let'."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "multi-let")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (defun id (x) x)
                 (defun f (x y)
                   (let ((a (id x))
                         (b (+ y 10)))
                     (+ a b)))
                 (exit (f 5 7)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 22)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/top-level-special-var-descriptors ()
  "Doc 129.4B: top-level vars are tracked as special declarations."
  (let ((extracted
         (nelisp-phase47-compiler--extract-defmacros
          '(seq
            (defvar dyn)
            (defconst c 1 "doc")
            (defcustom opt 2 "doc")))))
    (should (equal (plist-get extracted :special-vars)
                   '(dyn c opt)))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-mixed-let-normal-exit ()
  "Doc 129.4F: mixed lexical/special `let' lowers to temp/push/body/pop."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defvar dyn)
                (defun bind_mixed
                    ((out :type sexp)
                     (mirror :type sexp)
                     (frames :type sexp)
                     (scratch :type sexp)
                     (name-slot :type sexp)
                     (value-a :type sexp)
                     (value-b :type sexp))
                  (let (((a :type sexp) value-a)
                        (dyn value-b))
                    a)))))
         (defun-ir (car (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get defun-ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt-n))
    (should (= (cl-count 'nelisp_aot_push_special externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 1))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-multi-let-normal-exit ()
  "Doc 129.4E: all-special multi-binding `let' lowers to push/body/pop."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defvar dyn-a)
                (defvar dyn-b)
                (defun bind_special
                    ((out :type sexp)
                     (mirror :type sexp)
                     (frames :type sexp)
                     (scratch :type sexp)
                     (name-slot :type sexp)
                     (value-a :type sexp)
                     (value-b :type sexp))
                  (let ((dyn-a value-a)
                        (dyn-b value-b))
                    value-b)))))
         (defun-ir (car (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get defun-ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt-n))
    (should (= (cl-count 'nelisp_aot_push_special externs) 2))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 2))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-let-normal-exit ()
  "Doc 129.4D: source special `let' lowers to push/body/pop."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defvar dyn)
                (defun bind_special
                    ((out :type sexp)
                     (mirror :type sexp)
                     (frames :type sexp)
                     (scratch :type sexp)
                     (name-slot :type sexp)
                     (value :type sexp))
                  (let ((dyn value))
                    value)))))
         (defun-ir (car (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get defun-ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (member 'nelisp_aot_push_special externs))
    (should (member 'nelisp_aot_pop_special externs))))

(ert-deftest nelisp-phase47-doc129/source-special-let-nonlocal-still-pending ()
  "Doc 129.4D: special `let' bodies with non-local exits need landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defvar dyn)
      (defun bind_special
          ((out :type sexp)
           (mirror :type sexp)
           (frames :type sexp)
           (scratch :type sexp)
           (name-slot :type sexp)
           (value :type sexp))
        (let ((dyn value))
          (throw 'tag value)))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/source-special-multi-let-nonlocal-still-pending ()
  "Doc 129.4E: multi-special `let' bodies with non-local exits need landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defvar dyn-a)
      (defvar dyn-b)
      (defun bind_special
          ((out :type sexp)
           (mirror :type sexp)
           (frames :type sexp)
           (scratch :type sexp)
           (name-slot :type sexp)
           (value-a :type sexp)
           (value-b :type sexp))
        (let ((dyn-a value-a)
              (dyn-b value-b))
          (throw 'tag value-b)))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/source-special-mixed-let-nonlocal-still-pending ()
  "Doc 129.4F: mixed special `let' bodies with non-local exits need landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defvar dyn)
      (defun bind_mixed
          ((out :type sexp)
           (mirror :type sexp)
           (frames :type sexp)
           (scratch :type sexp)
           (name-slot :type sexp)
           (value-a :type sexp)
           (value-b :type sexp))
        (let (((a :type sexp) value-a)
              (dyn value-b))
          (throw 'tag a)))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/parse-aot-special-push-pop ()
  "Doc 129.4C: explicit special binding push/pop forms lower to bridges."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun bind_special
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name-slot :type sexp)
                    (value :type sexp)
                    (handle :type sexp))
                 (seq
                  (aot-push-special 'dyn value)
                  (aot-pop-special handle)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_special externs))
    (should (member 'nelisp_aot_pop_special externs))))

(ert-deftest nelisp-phase47-doc129/aot-special-push-requires-boundary ()
  "Doc 129.4C: special binding push lowering requires boxed boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun bind_special ((value :type sexp))
       (aot-push-special 'dyn value)))
   :type 'nelisp-phase47-compiler-error)
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun bind_special
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (value :type sexp))
       (aot-push-special 'dyn value)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-aot-special-push-pop ()
  "Doc 129.4C: object output exposes special binding bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun bind_special
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name-slot :type sexp)
                 (value :type sexp)
                 (handle :type sexp))
              (seq
               (aot-push-special 'dyn value)
               (aot-pop-special handle)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-source-special-let-normal-exit ()
  "Doc 129.4D: object output exposes source special let bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-let-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defvar dyn)
             (defun bind_special
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name-slot :type sexp)
                  (value :type sexp))
               (let ((dyn value))
                 value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-source-special-multi-let-normal-exit ()
  "Doc 129.4E: object output exposes multi-special let bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-multi-let-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defvar dyn-a)
             (defvar dyn-b)
             (defun bind_special
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name-slot :type sexp)
                  (value-a :type sexp)
                  (value-b :type sexp))
               (let ((dyn-a value-a)
                     (dyn-b value-b))
                 value-b)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-source-special-mixed-let-normal-exit ()
  "Doc 129.4F: object output exposes mixed special let bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-mixed-let-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defvar dyn)
             (defun bind_mixed
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name-slot :type sexp)
                  (value-a :type sexp)
                  (value-b :type sexp))
               (let (((a :type sexp) value-a)
                     (dyn value-b))
                 a)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-static-gc-root-map ()
  "Doc 129.5A: allocating defuns expose annotated Sexp root slots."
  (let ((ir (nelisp-phase47-compiler--parse
             '(defun make-str ((slot :type sexp) bytes len)
                (sexp-write-str slot bytes len)))))
    (should (equal (nelisp-phase47-compiler--ir-get ir :gc-root-slots)
                   '(0)))))

(ert-deftest nelisp-phase47-doc129/gc-root-descriptor-for-defun ()
  "Doc 129.5C: root slots are exposed as call-boundary descriptors."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str ((slot :type sexp) bytes len)
                 (sexp-write-str slot bytes len))))
         (descriptor
          (nelisp-phase47-compiler--gc-root-descriptor-for-defun ir)))
    (should (equal descriptor
                   '(:name make-str
                     :slots (0)
                     :param-count 3
                     :rt-slot-count 0)))))

(ert-deftest nelisp-phase47-doc129/gc-root-descriptors-skip-empty-defuns ()
  "Doc 129.5C: non-allocating defuns do not produce root descriptors."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defun id ((x :type sexp)) x)
                (defun make-str ((slot :type sexp) bytes len)
                  (sexp-write-str slot bytes len)))))
         (descriptors
          (nelisp-phase47-compiler--gc-root-descriptors ir)))
    (should (equal descriptors
                   '((:name make-str
                      :slots (0)
                      :param-count 3
                      :rt-slot-count 0))))))

(ert-deftest nelisp-phase47-doc129/runtime-let-sexp-root-slot ()
  "Doc 129.5D: annotated runtime `let' locals enter the static root map."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defun id (x) x)
                (defun make-str (raw)
                  (let (((slot :type sexp) (id raw)))
                    (sexp-write-str slot raw raw))))))
         (make-str-ir (nth 1 (nelisp-phase47-compiler--ir-get ir :forms)))
         (descriptor
          (nelisp-phase47-compiler--gc-root-descriptor-for-defun
           make-str-ir)))
    (should (equal (nelisp-phase47-compiler--ir-get make-str-ir
                                                     :gc-root-slots)
                   '(1)))
    (should (equal descriptor
                   '(:name make-str
                     :slots (1)
                     :param-count 1
                     :rt-slot-count 1)))))

(ert-deftest nelisp-phase47-doc129/parse-aot-root-push-pop ()
  "Doc 129.5E: explicit native root push/pop forms lower to bridges."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun root_frame
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (roots :type sexp))
                 (seq
                  (aot-push-roots roots)
                  (aot-pop-roots roots)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_roots externs))
    (should (member 'nelisp_aot_pop_roots externs))))

(ert-deftest nelisp-phase47-doc129/aot-root-push-pop-requires-boundary ()
  "Doc 129.5E: root push/pop lowering requires boxed boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun root_frame
         ((roots :type sexp))
       (aot-push-roots roots)))
   :type 'nelisp-phase47-compiler-error)
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun root_frame
         ((roots :type sexp))
       (aot-pop-roots roots)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-aot-root-push-pop ()
  "Doc 129.5E: object output exposes root push/pop bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-roots-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun root_frame
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (roots :type sexp))
              (seq
               (aot-push-roots roots)
               (aot-pop-roots roots)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_roots" out))
            (should (string-match-p "nelisp_aot_pop_roots" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-auto-aot-root-scope ()
  "Doc 129.5F: boundary defuns with root slots get automatic root scope."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (roots :type sexp)
                    bytes
                    len)
                 (sexp-write-str out bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'aot-root-scope))
    (should (equal (nelisp-phase47-compiler--ir-get body :root-slots)
                   '(0)))
    (should (equal (nelisp-phase47-compiler--ir-get body :root-symbols)
                   '(out)))
    (should (member 'nelisp_aot_materialize_roots externs))
    (should (member 'nelisp_aot_push_roots externs))
    (should (member 'nelisp_aot_pop_roots externs))))

(ert-deftest nelisp-phase47-doc129/parse-auto-aot-root-scope-requires-roots ()
  "Doc 129.5F: automatic root scope is skipped without root-vector input."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str ((slot :type sexp) bytes len)
                 (sexp-write-str slot bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-write-str))
    (should (equal (nelisp-phase47-compiler--ir-get ir :gc-root-slots)
                   '(0)))))

(ert-deftest nelisp-phase47-doc129/object-auto-aot-root-scope ()
  "Doc 129.5F: object output auto-emits balanced root bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-auto-roots-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun make-str
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (roots :type sexp)
                 bytes
                 len)
              (sexp-write-str out bytes len))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_roots" out))
            (should (string-match-p "nelisp_aot_materialize_roots" out))
            (should (string-match-p "nelisp_aot_pop_roots" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/top-level-require-provide-stripped ()
  "Doc 129.6A: top-level module forms are compile-time-only."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (require 'cl-lib)
                (provide 'nelisp-phase47-doc129-test-feature)
                (exit 9))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms)))
    (should (= (length forms) 1))
    (should (eq (nelisp-phase47-compiler--ir-kind (car forms)) 'exit))))

(ert-deftest nelisp-phase47-doc129/top-level-require-noerror-missing ()
  "Doc 129.6A: `(require FEATURE nil t)' can be stripped when absent."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (require 'nelisp-phase47-doc129-missing-feature nil t)
                (exit 3))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms)))
    (should (= (length forms) 1))
    (should (eq (nelisp-phase47-compiler--ir-kind (car forms)) 'exit))))

(ert-deftest nelisp-phase47-doc129/top-level-require-missing-signals ()
  "Doc 129.6A: missing top-level `require' without NOERROR still signals."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (require 'nelisp-phase47-doc129-missing-feature)
      (exit 3)))))

(ert-deftest nelisp-phase47-doc129/parse-builtin1-delegation-helper ()
  "Doc 129.6B: builtin1 helpers delegate through the runtime dispatcher."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun-sexp-builtin1-symbol call_symbol_name symbol-name
                   (out mirror frames scratch name_slot arg))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (symbol-node (nth 0 forms))
         (call-node (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-get ir :name)
                'call_symbol_name))
    (should (equal (nelisp-phase47-compiler--ir-get ir :params)
                   '(out mirror frames scratch name_slot arg)))
    (should (eq (nelisp-phase47-compiler--ir-kind symbol-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                   (string-to-list "symbol-name")))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node)
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_call1))))

(ert-deftest nelisp-phase47-doc129/object-builtin1-delegation-helper ()
  "Doc 129.6B: object output exposes builtin delegation relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-builtin1-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun-sexp-builtin1-symbol call_symbol_name symbol-name
                (out mirror frames scratch name_slot arg))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_symbol_name" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtin1-user-call ()
  "Doc 129.6D: direct one-arg builtin calls lower to dispatcher sequence."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_symbol_name
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (arg :type sexp))
                 (symbol-name arg))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (symbol-node (nth 0 forms))
         (call-node (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind symbol-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                   (string-to-list "symbol-name")))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node)
                'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_call1))))

(ert-deftest nelisp-phase47-doc129/direct-builtin1-user-call-requires-boundary ()
  "Doc 129.6D: direct builtin lowering requires explicit boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun call_symbol_name ((arg :type sexp))
       (symbol-name arg)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-direct-builtin1-user-call ()
  "Doc 129.6D: object output for direct builtin calls exposes dispatcher relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-direct-builtin1-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_symbol_name
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (arg :type sexp))
              (symbol-name arg))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_symbol_name" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-user-call ()
  "Doc 129.6F: direct vararg builtin calls lower to calln."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_list
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (a :type sexp)
                    (b :type sexp))
                 (list a b))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (symbol-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind symbol-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                   (string-to-list "list")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (= (nelisp-phase47-compiler--ir-get (nth 3 call-args)
                                                :value)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-expanded-table ()
  "Doc 129.6G: calln lowering covers common fixed-arity builtins."
  (dolist (builtin '(cons eq equal nth assq string=))
    (let* ((ir (nelisp-phase47-compiler--parse
                `(defun call_builtin
                     ((out :type sexp)
                      (mirror :type sexp)
                      (frames :type sexp)
                      (scratch :type sexp)
                      (name_slot :type sexp)
                      (a :type sexp)
                      (b :type sexp))
                   (,builtin a b))))
           (body (nelisp-phase47-compiler--ir-get ir :body))
           (forms (nelisp-phase47-compiler--ir-get body :forms))
           (symbol-node (nth 0 forms))
           (call-node (nth 1 forms)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
      (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                     (string-to-list (symbol-name builtin))))
      (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                  'nelisp_aot_builtin_calln)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-string-table ()
  "Doc 129.6H: calln lowering covers string and format builtins."
  (dolist (case '((format 2 (format a b))
                  (message 1 (message a))
                  (string-match 2 (string-match a b))
                  (string-match-p 2 (string-match-p a b))
                  (substring 3 (substring a b c))
                  (string-prefix-p 2 (string-prefix-p a b))
                  (string-suffix-p 2 (string-suffix-p a b))
                  (replace-regexp-in-string 3
                                            (replace-regexp-in-string a b c))))
    (pcase-let ((`(,builtin ,argc ,form) case))
      (let* ((ir (nelisp-phase47-compiler--parse
                  `(defun call_builtin
                       ((out :type sexp)
                        (mirror :type sexp)
                        (frames :type sexp)
                        (scratch :type sexp)
                        (name_slot :type sexp)
                        (a :type sexp)
                        (b :type sexp)
                        (c :type sexp))
                     ,form)))
             (body (nelisp-phase47-compiler--ir-get ir :body))
             (forms (nelisp-phase47-compiler--ir-get body :forms))
             (symbol-node (nth 0 forms))
             (call-node (nth 1 forms))
             (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
        (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
        (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                       (string-to-list (symbol-name builtin))))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))
        (should (= (nelisp-phase47-compiler--ir-get (nth 3 call-args)
                                                    :value)
                   argc))))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-higher-order-table ()
  "Doc 129.6I: calln lowering covers dynamic higher-order builtins."
  (dolist (case '((mapcar 2 (mapcar fn xs))
                  (mapc 2 (mapc fn xs))
                  (mapconcat 3 (mapconcat fn xs sep))
                  (sort 2 (sort xs fn))))
    (pcase-let ((`(,builtin ,argc ,form) case))
      (let* ((ir (nelisp-phase47-compiler--parse
                  `(defun call_builtin
                       ((out :type sexp)
                        (mirror :type sexp)
                        (frames :type sexp)
                        (scratch :type sexp)
                        (name_slot :type sexp)
                        (fn :type sexp)
                        (xs :type sexp)
                        (sep :type sexp))
                     ,form)))
             (body (nelisp-phase47-compiler--ir-get ir :body))
             (forms (nelisp-phase47-compiler--ir-get body :forms))
             (symbol-node (nth 0 forms))
             (call-node (nth 1 forms))
             (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
        (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
        (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                       (string-to-list (symbol-name builtin))))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))
        (should (= (nelisp-phase47-compiler--ir-get (nth 3 call-args)
                                                    :value)
                   argc))))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-map-designator ()
  "Doc 129.6J: map builtins materialize quoted/function designators."
  (dolist (form '((mapcar #'foo xs)
                  (mapc 'foo xs)
                  (mapconcat #'foo xs sep)))
    (let* ((ir (nelisp-phase47-compiler--parse
                `(defun call_builtin
                     ((out :type sexp)
                      (mirror :type sexp)
                      (frames :type sexp)
                      (scratch :type sexp)
                      (name_slot :type sexp)
                      (xs :type sexp)
                      (sep :type sexp))
                   ,form)))
           (body (nelisp-phase47-compiler--ir-get ir :body))
           (forms (nelisp-phase47-compiler--ir-get body :forms))
           (builtin-symbol (nth 0 forms))
           (fn-symbol (nth 1 forms))
           (call-node (nth 2 forms))
           (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
      (should (eq (nelisp-phase47-compiler--ir-kind builtin-symbol)
                  'sexp-write-symbol-lit))
      (should (eq (nelisp-phase47-compiler--ir-kind fn-symbol)
                  'sexp-write-symbol-lit))
      (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                     (string-to-list "foo")))
      (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                  'nelisp_aot_builtin_calln))
      (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 call-args))
                  'ref))
      (should (eq (nelisp-phase47-compiler--ir-get (nth 6 call-args) :var)
                  'scratch)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-sort-designator ()
  "Doc 129.6K: `sort' materializes quoted/function predicate designators."
  (dolist (form '((sort xs #'string<)
                  (sort xs 'lessp)))
    (let* ((ir (nelisp-phase47-compiler--parse
                `(defun call_sort
                     ((out :type sexp)
                      (mirror :type sexp)
                      (frames :type sexp)
                      (scratch :type sexp)
                      (name_slot :type sexp)
                      (xs :type sexp))
                   ,form)))
           (body (nelisp-phase47-compiler--ir-get ir :body))
           (forms (nelisp-phase47-compiler--ir-get body :forms))
           (builtin-symbol (nth 0 forms))
           (fn-symbol (nth 1 forms))
           (call-node (nth 2 forms))
           (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
      (should (equal (nelisp-phase47-compiler--ir-get builtin-symbol :bytes)
                     (string-to-list "sort")))
      (should (eq (nelisp-phase47-compiler--ir-kind fn-symbol)
                  'sexp-write-symbol-lit))
      (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                  'nelisp_aot_builtin_calln))
      (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 call-args))
                  'ref))
      (should (eq (nelisp-phase47-compiler--ir-get (nth 7 call-args) :var)
                  'scratch)))))

(ert-deftest nelisp-phase47-doc129/parse-map-lambda-lift ()
  "Doc 129.7M: map-family literal lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (mapcar (lambda (x) (+ x 1)) xs))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (fn-symbol (nth 1 body-forms))
         (call-node (nth 2 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind lambda-ir) 'defun))
    (should (string-prefix-p
             "nelisp_aot_lambda_"
             (symbol-name (nelisp-phase47-compiler--ir-get lambda-ir :name))))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get lambda-ir :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 call-args) :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/map-lambda-lift-capture-still-pending ()
  "Doc 129.7M: map lambda lifting still rejects captured variables."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun caller
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (name_slot :type sexp)
          (cap :type sexp)
          (xs :type sexp))
       (mapcar (lambda (x) (+ x cap)) xs)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/direct-builtinn-user-call-requires-boundary ()
  "Doc 129.6F: vararg builtin lowering requires explicit boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun call_list ((a :type sexp) (b :type sexp))
       (list a b)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/direct-builtinn-defun-shadow-wins ()
  "Doc 129.6F: same-unit defuns shadow vararg builtin delegation."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defun list (a b) (+ a b))
                (defun caller (a b) (list a b)))))
         (caller (nth 1 (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get caller :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'call))
    (should (eq (nelisp-phase47-compiler--ir-get body :name) 'list))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-user-call ()
  "Doc 129.6F: object output exposes builtin calln dispatcher relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-direct-builtinn-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_list
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (a :type sexp)
                 (b :type sexp))
              (list a b))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_list" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-map-designator ()
  "Doc 129.6J: object output exposes map designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-map-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_mapcar
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (mapcar #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_mapcar" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-sort-designator ()
  "Doc 129.6K: object output exposes sort predicate materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-sort-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_sort
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (sort xs #'string<))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_sort" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-map-lambda-lift ()
  "Doc 129.7M: object output exposes map lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-map-lambda-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (mapcar (lambda (x) (+ x 1)) xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtin1-table ()
  "Doc 129.6E: direct builtin1 lowering covers the shipped unary table."
  (dolist (builtin '(identity length car cdr symbolp stringp number-to-string))
    (let* ((ir (nelisp-phase47-compiler--parse
                `(defun call_builtin
                     ((out :type sexp)
                      (mirror :type sexp)
                      (frames :type sexp)
                      (scratch :type sexp)
                      (name_slot :type sexp)
                      (arg :type sexp))
                   (,builtin arg))))
           (body (nelisp-phase47-compiler--ir-get ir :body))
           (forms (nelisp-phase47-compiler--ir-get body :forms))
           (symbol-node (nth 0 forms))
           (call-node (nth 1 forms)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
      (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                     (string-to-list (symbol-name builtin))))
      (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                  'nelisp_aot_builtin_call1)))))

(ert-deftest nelisp-phase47-doc129/direct-builtin1-defun-shadow-wins ()
  "Doc 129.6E: a same-named Phase 47 defun is not captured by the builtin table."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defun identity ((x :type sexp))
                  x)
                (defun call_identity
                    ((out :type sexp)
                     (mirror :type sexp)
                     (frames :type sexp)
                     (scratch :type sexp)
                     (name_slot :type sexp)
                     (arg :type sexp))
                  (identity arg)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (caller (cadr forms))
         (body (nelisp-phase47-compiler--ir-get caller :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'call))
    (should (eq (nelisp-phase47-compiler--ir-get body :name) 'identity))))

(ert-deftest nelisp-phase47-doc129/parse-funcall1-delegation ()
  "Doc 129.7A: `(funcall FN ARG)' lowers to the runtime dispatcher."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_fn
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (fn :type sexp)
                    (arg :type sexp))
                 (funcall fn arg))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall1))))

(ert-deftest nelisp-phase47-doc129/funcall1-delegation-requires-boundary ()
  "Doc 129.7A: funcall delegation requires explicit boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun call_fn ((fn :type sexp) (arg :type sexp))
       (funcall fn arg)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-funcall1-delegation ()
  "Doc 129.7A: object output exposes the funcall dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-funcall1-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_fn
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (fn :type sexp)
                 (arg :type sexp))
              (funcall fn arg))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_fn" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-funcall2-delegation ()
  "Doc 129.7B: `(funcall FN ARG0 ARG1)' lowers to the two-arg dispatcher."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_fn2
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (fn :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp))
                 (funcall fn arg0 arg1))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall2))))

(ert-deftest nelisp-phase47-doc129/parse-funcall3-delegation ()
  "Doc 129.7E: `(funcall FN ARG0 ARG1 ARG2)' lowers to funcall3."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_fn3
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (fn :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp)
                    (arg2 :type sexp))
                 (funcall fn arg0 arg1 arg2))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall3))))

(ert-deftest nelisp-phase47-doc129/parse-funcall4-calln-delegation ()
  "Doc 129.7H: `(funcall FN ARG0 ARG1 ARG2 ARG3)' lowers to calln."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_fn4
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (fn :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp)
                    (arg2 :type sexp)
                    (arg3 :type sexp))
                 (funcall fn arg0 arg1 arg2 arg3))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcalln))
    (should (= (length (nelisp-phase47-compiler--ir-get call-node :args))
               10))))

(ert-deftest nelisp-phase47-doc129/object-funcall2-delegation ()
  "Doc 129.7B: object output exposes the funcall2 dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-funcall2-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_fn2
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (fn :type sexp)
                 (arg0 :type sexp)
                 (arg1 :type sexp))
              (funcall fn arg0 arg1))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_fn2" out))
            (should (string-match-p "nelisp_aot_funcall2" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-funcall3-delegation ()
  "Doc 129.7E: object output exposes the funcall3 dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-funcall3-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_fn3
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (fn :type sexp)
                 (arg0 :type sexp)
                 (arg1 :type sexp)
                 (arg2 :type sexp))
              (funcall fn arg0 arg1 arg2))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_fn3" out))
            (should (string-match-p "nelisp_aot_funcall3" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-funcall4-calln-delegation ()
  "Doc 129.7H: object output exposes the funcalln dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-funcall4-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_fn4
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (fn :type sexp)
                 (arg0 :type sexp)
                 (arg1 :type sexp)
                 (arg2 :type sexp)
                 (arg3 :type sexp))
              (funcall fn arg0 arg1 arg2 arg3))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_fn4" out))
            (should (string-match-p "nelisp_aot_funcalln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-apply-delegation ()
  "Doc 129.7C: `(apply FN ARGS-LIST)' lowers to the apply dispatcher."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_apply
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (fn :type sexp)
                    (args :type sexp))
                 (apply fn args))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_apply))))

(ert-deftest nelisp-phase47-doc129/parse-apply-splicing-delegation ()
  "Doc 129.7I: `(apply FN ARG... ARGS-LIST)' lowers to applyn."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_applyn
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (fn :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp)
                    (args :type sexp))
                 (apply fn arg0 arg1 args))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (car forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'extern-call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_applyn))
    (should (equal (mapcar #'nelisp-phase47-compiler--ir-kind call-args)
                   '(ref ref ref imm ref ref ref ref ref)))))

(ert-deftest nelisp-phase47-doc129/apply-delegation-requires-boundary ()
  "Doc 129.7C: apply delegation requires explicit boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun call_apply ((fn :type sexp) (args :type sexp))
       (apply fn args)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-apply-delegation ()
  "Doc 129.7C: object output exposes the apply dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-apply-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_apply
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (fn :type sexp)
                 (args :type sexp))
              (apply fn args))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_apply" out))
            (should (string-match-p "nelisp_aot_apply" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-apply-splicing-delegation ()
  "Doc 129.7I: object output exposes the applyn dispatcher reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-applyn-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_applyn
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (fn :type sexp)
                 (arg0 :type sexp)
                 (arg1 :type sexp)
                 (args :type sexp))
              (apply fn arg0 arg1 args))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_applyn" out))
            (should (string-match-p "nelisp_aot_applyn" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-rest-param-call-listn ()
  "Doc 129.7J: source &rest defuns receive a constructed rest list."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defun collect
                    ((out :type sexp)
                     &rest
                     (args :type sexp))
                  args)
                (defun call_collect
                    ((out :type sexp)
                     (mirror :type sexp)
                     (frames :type sexp)
                     (scratch :type sexp)
                     (a :type sexp)
                     (b :type sexp))
                  (collect out a b)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (collect-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (caller-body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (nelisp-phase47-compiler--ir-get collect-ir :rest-p))
    (should (= (nelisp-phase47-compiler--ir-get collect-ir
                                                :fixed-param-count)
               1))
    (should (equal (nelisp-phase47-compiler--ir-get collect-ir :params)
                   '(out args)))
    (should (eq (nelisp-phase47-compiler--ir-kind caller-body)
                'value-seq))
    (should (member 'nelisp_aot_listn externs))))

(ert-deftest nelisp-phase47-doc129/rest-param-call-requires-boundary ()
  "Doc 129.7J: rest call lowering needs caller listn boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq
      (defun collect ((out :type sexp) &rest (args :type sexp))
        args)
      (defun call_collect ((out :type sexp) (a :type sexp))
        (collect out a))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-rest-param-call-listn ()
  "Doc 129.7J: object output exposes rest-param list construction."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-rest-param-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defun collect
                 ((out :type sexp)
                  &rest
                  (args :type sexp))
               args)
             (defun call_collect
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (a :type sexp)
                  (b :type sexp))
               (collect out a b)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "collect" out))
            (should (string-match-p "call_collect" out))
            (should (string-match-p "nelisp_aot_listn" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-funcall-lambda-lift ()
  "Doc 129.7K: literal lambda funcall lifts to a synthetic defun."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (x)
                 (funcall (lambda (y) (+ y 1)) x))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (call-node (nelisp-phase47-compiler--ir-get caller-ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind lambda-ir) 'defun))
    (should (string-prefix-p
             "nelisp_aot_lambda_"
             (symbol-name (nelisp-phase47-compiler--ir-get lambda-ir :name))))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))))

(ert-deftest nelisp-phase47-doc129/parse-function-lambda-lift ()
  "Doc 129.7K: `(function (lambda ...))' funcall also lambda-lifts."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (x)
                 (funcall (function (lambda (y) (* y 2))) x))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (call-node (nelisp-phase47-compiler--ir-get caller-ir :body)))
    (should (string-prefix-p
             "nelisp_aot_lambda_"
             (symbol-name (nelisp-phase47-compiler--ir-get lambda-ir :name))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))))

(ert-deftest nelisp-phase47-doc129/lambda-lift-capture-still-pending ()
  "Doc 129.7K: captured lexical variables still wait for closures."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun caller (x)
       (let ((cap 1))
         (funcall (lambda (y) (+ y cap)) x))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/e2e-funcall-lambda-lift ()
  "Doc 129.7K: execute a non-capturing lambda-lifted funcall."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "lambda-lift")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun caller (x)
               (funcall (lambda (y) (+ y 1)) x))
             (exit (caller 41)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 42)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-funcall-lambda-lift ()
  "Doc 129.7K: object output exposes synthetic lambda defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-lift-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller (x)
              (funcall (lambda (y) (+ y 1)) x))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-lambda-lift ()
  "Doc 129.7L: direct literal lambda application also lambda-lifts."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (x)
                 ((lambda (y) (+ y 1)) x))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (call-node (nelisp-phase47-compiler--ir-get caller-ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))))

(ert-deftest nelisp-phase47-doc129/e2e-direct-lambda-lift ()
  "Doc 129.7L: execute a direct non-capturing lambda application."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "direct-lambda-lift")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun caller (x)
               ((lambda (y) (* y 2)) x))
             (exit (caller 21)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 42)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-lambda-lift ()
  "Doc 129.7L: object output exposes direct lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-direct-lambda-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller (x)
              ((lambda (y) (+ y 1)) x))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-quoted-funcall-designator ()
  "Doc 129.7D: quoted function symbols materialize through NAME-SLOT."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_quoted
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (arg :type sexp))
                 (funcall 'identity arg))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (fn-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "identity")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall1))
    (should (eq (nelisp-phase47-compiler--ir-get fn-arg :var)
                'name_slot))))

(ert-deftest nelisp-phase47-doc129/parse-function-funcall2-designator ()
  "Doc 129.7D: `#'symbol' funcall2 lowers through the same name slot."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_function
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (name-slot :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp))
                 (funcall #'concat arg0 arg1))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "concat")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall2))))

(ert-deftest nelisp-phase47-doc129/parse-quoted-funcall4-calln-designator ()
  "Doc 129.7H: quoted function symbols also materialize for calln."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_quoted4
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (arg0 :type sexp)
                    (arg1 :type sexp)
                    (arg2 :type sexp)
                    (arg3 :type sexp))
                 (funcall '+ arg0 arg1 arg2 arg3))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (fn-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "+")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcalln))
    (should (equal (mapcar #'nelisp-phase47-compiler--ir-kind call-args)
                   '(ref ref ref imm ref ref ref ref ref ref)))
    (should (eq (nelisp-phase47-compiler--ir-get fn-arg :var)
                'name_slot))))

(ert-deftest nelisp-phase47-doc129/quoted-apply-designator-requires-name-slot ()
  "Doc 129.7D: quoted apply designators require caller-owned NAME-SLOT."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun call_apply
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (args :type sexp))
       (apply '+ args)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-quoted-apply-designator ()
  "Doc 129.7D: object output exposes symbol materialization + apply reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-quoted-apply-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_apply_symbol
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (args :type sexp))
              (apply '+ args))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_apply_symbol" out))
            (should (string-match-p "nelisp_aot_apply" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-throw-literal-tag ()
  "Doc 129.8B: `(throw 'TAG VALUE)' lowers through the throw bridge."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun throw_tag
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (throw 'done value))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (tag-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "done")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_throw))
    (should (eq (nelisp-phase47-compiler--ir-get tag-arg :var)
                'name_slot))))

(ert-deftest nelisp-phase47-doc129/parse-signal-dynamic-tag ()
  "Doc 129.8B: dynamic signal tags use the supplied boxed tag value."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun signal_tag
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (tag :type sexp)
                    (data :type sexp))
                 (signal tag data))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 0 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (tag-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_signal))
    (should (eq (nelisp-phase47-compiler--ir-get tag-arg :var)
                'tag))))

(ert-deftest nelisp-phase47-doc129/throw-literal-tag-requires-name-slot ()
  "Doc 129.8B: literal throw tags require caller-owned NAME-SLOT."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun throw_tag
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (value :type sexp))
       (throw 'done value)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-throw-signal-bridges ()
  "Doc 129.8B: object output exposes throw/signal bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-exception-bridges-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defun throw_tag
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (value :type sexp))
               (throw 'done value))
             (defun signal_tag
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (tag :type sexp)
                  (data :type sexp))
               (signal tag data)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "throw_tag" out))
            (should (string-match-p "signal_tag" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-error-bridge ()
  "Doc 129.8H: `(error DATA)' lowers as `(signal 'error DATA)'."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun error_value
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (error value))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "error")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_signal))))

(ert-deftest nelisp-phase47-doc129/parse-error-varargs-bridge ()
  "Doc 129.8I: formatted `(error FMT ARG...)' lowers to errorn."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun error_fmt
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (error "%s" value))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-str-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "%s")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_errorn))
    (should (equal (mapcar #'nelisp-phase47-compiler--ir-kind call-args)
                   '(ref ref imm ref ref ref ref)))))

(ert-deftest nelisp-phase47-doc129/object-error-bridge ()
  "Doc 129.8H: object output exposes error-as-signal bridge reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-error-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun error_value
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (error value))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "error_value" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-error-varargs-bridge ()
  "Doc 129.8I: object output exposes formatted errorn bridge reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-errorn-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun error_fmt
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (error "%s" value))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "error_fmt" out))
            (should (string-match-p "nelisp_aot_errorn" out))
            (should (string-match-p "nl_alloc_str" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-push-catch-handler ()
  "Doc 129.8C: explicit catch handler push lowers to runtime bridge."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun push_catch
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp))
                 (aot-push-catch 'done 4096 64))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "done")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_push_catch))))

(ert-deftest nelisp-phase47-doc129/parse-push-unwind-handler ()
  "Doc 129.8C: explicit unwind handler push accepts dynamic cleanup."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun push_unwind
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cleanup :type sexp))
                 (aot-push-unwind cleanup 8192 64))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 0 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (cleanup-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_push_unwind))
    (should (eq (nelisp-phase47-compiler--ir-get cleanup-arg :var)
                'cleanup))))

(ert-deftest nelisp-phase47-doc129/push-catch-requires-boundary ()
  "Doc 129.8C: handler push lowering requires boxed boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun push_catch
         ((name_slot :type sexp)
          (landing :type sexp)
          (saved :type sexp))
       (aot-push-catch 'done landing saved)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-push-handler-bridges ()
  "Doc 129.8C: object output exposes push handler bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-push-handlers-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defun push_catch
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp))
               (aot-push-catch 'done 4096 64))
             (defun push_condition
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (condition :type sexp))
               (aot-push-condition condition 8192 64))
             (defun push_unwind
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (cleanup :type sexp))
               (aot-push-unwind cleanup 12288 64)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-pop-handler ()
  "Doc 129.8D: explicit handler pop lowers to runtime bridge."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun pop_catch
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp))
                 (aot-pop-handler 'catch))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (write-node (nth 0 forms))
         (call-node (nth 1 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (kind-arg (nth 2 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind write-node)
                'sexp-write-symbol-lit))
    (should (equal (nelisp-phase47-compiler--ir-get write-node :bytes)
                   (string-to-list "catch")))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_pop_handler))
    (should (eq (nelisp-phase47-compiler--ir-get kind-arg :var)
                'name_slot))))

(ert-deftest nelisp-phase47-doc129/pop-handler-requires-boundary ()
  "Doc 129.8D: pop handler lowering requires boxed boundary params."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun pop_catch
         ((name_slot :type sexp))
       (aot-pop-handler 'catch)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-pop-handler-bridge ()
  "Doc 129.8D: object output exposes pop handler bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-pop-handler-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun pop_catch
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp))
              (aot-pop-handler 'catch))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-catch-normal-exit ()
  "Doc 129.8E: source `catch' normal exit lowers to push/body/pop."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_value
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done value))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (push-node (nth 0 forms))
         (save-node (nth 1 forms))
         (save-body (nelisp-phase47-compiler--ir-get save-node :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind push-node) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind save-node) 'let-rt))
    (should (eq (nelisp-phase47-compiler--ir-kind save-body) 'value-seq))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_pop_handler externs))))

(ert-deftest nelisp-phase47-doc129/catch-nonlocal-body-still-pending ()
  "Doc 129.8E: catch bodies with explicit throw wait for landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun catch_throw
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (name_slot :type sexp)
          (value :type sexp))
       (catch 'done
         (throw 'done value))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-catch-normal-exit ()
  "Doc 129.8E: source `catch' exposes push/pop bridge relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-normal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun catch_value
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done value))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "catch_value" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-normal-exit ()
  "Doc 129.8F: source condition-case normal exit lowers to push/body/pop."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_value
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     value
                   (error out)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (push-node (nth 0 forms))
         (save-node (nth 1 forms))
         (save-body (nelisp-phase47-compiler--ir-get save-node :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind push-node) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind save-node) 'let-rt))
    (should (eq (nelisp-phase47-compiler--ir-kind save-body) 'value-seq))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_pop_handler externs))))

(ert-deftest nelisp-phase47-doc129/condition-case-nonlocal-body-still-pending ()
  "Doc 129.8F: condition-case bodies with signal wait for landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun cc_signal
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (name_slot :type sexp)
          (value :type sexp))
       (condition-case err
           (signal 'error value)
         (error out))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-list-spec-normal-exit ()
  "Doc 129.8J: list condition specs push one handler per selector."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_list_spec
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     value
                   ((error quit) out)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should (= (cl-count 'nelisp_aot_pop_handler externs) 2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-multiple-handlers-normal-exit ()
  "Doc 129.8K: normal-exit condition-case walks every handler clause."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_multi_handler
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     value
                   (error out)
                   (quit out)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should (= (cl-count 'nelisp_aot_pop_handler externs) 2))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-normal-exit ()
  "Doc 129.8F: source condition-case exposes condition push/pop relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-normal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_value
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  value
                (error out)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_value" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-list-spec-normal-exit ()
  "Doc 129.8J: list condition specs compile through condition push/pop."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-list-normal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_list_spec
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  value
                ((error quit) out)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_list_spec" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-multiple-handlers-normal-exit ()
  "Doc 129.8K: multiple handler clauses compile through condition push/pop."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-multi-normal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_multi_handler
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  value
                (error out)
                (quit out)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_multi_handler" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-normal-exit ()
  "Doc 129.8G: source unwind-protect saves BODY, runs cleanup, returns BODY."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_value
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     value
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (save-body (nelisp-phase47-compiler--ir-get body :body))
         (forms (nelisp-phase47-compiler--ir-get save-body :forms))
         (cleanup-node (nth 0 forms))
         (return-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt))
    (should (eq (nelisp-phase47-compiler--ir-kind save-body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind cleanup-node) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind return-node) 'ref))
    (should (member 'nelisp_aot_builtin_call1 externs))))

(ert-deftest nelisp-phase47-doc129/unwind-protect-nonlocal-still-pending ()
  "Doc 129.8G: non-local forms crossing unwind-protect wait for landing pads."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun unwind_throw
         ((out :type sexp)
          (mirror :type sexp)
          (frames :type sexp)
          (scratch :type sexp)
          (name_slot :type sexp)
          (value :type sexp))
       (unwind-protect
           (throw 'done value)
         (identity value))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-normal-exit ()
  "Doc 129.8G: source unwind-protect normal path compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-normal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_value
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  value
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_value" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/e2e-top-level-require-provide ()
  "Doc 129.6A: stripped module forms do not block binary emission."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary "require-provide")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (require 'cl-lib)
             (provide 'nelisp-phase47-doc129-test-feature)
             (exit 17))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 17)))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-phase47-doc129-test)

;;; nelisp-phase47-doc129-test.el ends here
