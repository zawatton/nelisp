;;; nelisp-phase47-doc129-test.el --- Doc 129 Phase47 frontend tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'nelisp-elf-write)
(require 'nelisp-cc-runtime)
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

(defun nelisp-phase47-doc129-test--ir-nodes (ir kind)
  "Return IR nodes of KIND found while walking IR."
  (let (nodes)
    (nelisp-phase47-compiler--walk-ir
     ir
     (lambda (node)
       (when (eq (nelisp-phase47-compiler--ir-kind node) kind)
         (push node nodes))))
    (nreverse nodes)))

(defun nelisp-phase47-doc129-test--assert-single-landing-metadata
    (ir extern-name label-prefix)
  "Assert IR pushes EXTERN-NAME with one landing label named LABEL-PREFIX."
  (let* ((push-call
          (car (seq-filter
                (lambda (node)
                  (eq (nelisp-phase47-compiler--ir-get node :name)
                      extern-name))
                (nelisp-phase47-doc129-test--ir-nodes ir 'extern-call))))
         (push-args (nelisp-phase47-compiler--ir-get push-call :args))
         (landing-arg (nth 3 push-args))
         (saved-sp-arg (nth 4 push-args))
         (landing-labels
          (nelisp-phase47-doc129-test--ir-nodes ir 'aot-landing-label))
         (landing-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get (car landing-labels) :label)))
         (symbol-writes
          (mapcar (lambda (node)
                    (nelisp-phase47-compiler--ir-get node :bytes))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'sexp-write-symbol-lit))))
    (should (= (length landing-labels) 1))
    (should (string-prefix-p label-prefix landing-name))
    (should (member (string-to-list landing-name) symbol-writes))
    (should (eq (nelisp-phase47-compiler--ir-get landing-arg :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-kind saved-sp-arg)
                'aot-current-sp))))

(defun nelisp-phase47-doc129-test--assert-landing-metadata-count
    (ir extern-name label-prefix count)
  "Assert IR pushes EXTERN-NAME with COUNT landing labels named LABEL-PREFIX."
  (let* ((push-calls
          (seq-filter
           (lambda (node)
             (eq (nelisp-phase47-compiler--ir-get node :name)
                 extern-name))
           (nelisp-phase47-doc129-test--ir-nodes ir 'extern-call)))
         (landing-labels
          (nelisp-phase47-doc129-test--ir-nodes ir 'aot-landing-label))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  landing-labels))
         (symbol-writes
          (mapcar (lambda (node)
                    (nelisp-phase47-compiler--ir-get node :bytes))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'sexp-write-symbol-lit))))
    (should (= (length push-calls) count))
    (should (= (length landing-labels) count))
    (dolist (name landing-names)
      (should (string-prefix-p label-prefix name))
      (should (member (string-to-list name) symbol-writes)))
    (dolist (call push-calls)
      (let* ((args (nelisp-phase47-compiler--ir-get call :args))
             (landing-arg (nth 3 args))
             (saved-sp-arg (nth 4 args)))
        (should (eq (nelisp-phase47-compiler--ir-get landing-arg :var)
                    'scratch))
        (should (eq (nelisp-phase47-compiler--ir-kind saved-sp-arg)
                    'aot-current-sp))))))

(defun nelisp-phase47-doc129-test--assert-landing-label-count
    (ir label-prefix count)
  "Assert IR contains COUNT landing labels named LABEL-PREFIX."
  (let* ((landing-labels
          (nelisp-phase47-doc129-test--ir-nodes ir 'aot-landing-label))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  landing-labels)))
    (should (= (length landing-labels) count))
    (dolist (name landing-names)
      (should (string-prefix-p label-prefix name)))))

(defun nelisp-phase47-doc129-test--capturing-callback-closure-ir
    (form callback-arg-index)
  "Assert FORM lowers a captured callback through make-closure.
CALLBACK-ARG-INDEX is the calln argument index that should receive the
materialized closure temporary."
  (let* ((ir (nelisp-phase47-compiler--parse
              `(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (cap :type sexp)
                    (xs :type sexp)
                    (table :type sexp))
                 ,form)))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (make-closure (nth 2 forms))
         (call-node (nth 3 forms))
         (make-args (nelisp-phase47-compiler--ir-get make-closure :args))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get make-closure :name)
                'nelisp_aot_make_closure))
    (should (= (nelisp-phase47-compiler--ir-get (nth 3 make-args) :value)
               1))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 make-args) :var)
                'cap))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth callback-arg-index call-args) :var)
                'out))
    ir))

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

(ert-deftest nelisp-phase47-doc129/module-init-plan-combines-descriptors ()
  "Doc 129.7W: compiler exposes a normalized module-init plan."
  (let* ((plan
          (nelisp-phase47-compiler--module-init-plan
           '(seq
             (defvar x 42 "doc")
             (defcustom z 9 "doc" :type 'integer)
             (defun make_str ((slot :type sexp) bytes len)
               (sexp-write-str slot bytes len))
             (defun caller
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (cap :type sexp)
                  (xs :type sexp))
               (mapcar (lambda (item) (+ item cap)) xs)))))
         (closure (car (plist-get plan :closure-descriptors)))
         (root-descriptors (plist-get plan :root-descriptors)))
    (should (equal (plist-get plan :helper-order)
                   '(nelisp_aot_var_0_x nelisp_aot_custom_1_z)))
    (should (equal (plist-get plan :custom-by-helper)
                   '((nelisp_aot_custom_1_z
                      :name z
                      :helper nelisp_aot_custom_1_z
                      :standard 9
                      :docstring "doc"
                      :options (:type (quote integer))))))
    (should (cl-find-if
             (lambda (descriptor)
               (equal descriptor
                      '(:name make_str
                        :slots (0)
                        :param-count 3
                        :rt-slot-count 0)))
             root-descriptors))
    (should (cl-find-if
             (lambda (descriptor)
               (eq (plist-get descriptor :name) 'caller))
             root-descriptors))
    (should (eq (plist-get closure :name) 'nelisp_aot_closure_0))
    (should (equal (plist-get closure :arglist) '(item)))
    (should (equal (plist-get closure :captures) '(cap)))))

(ert-deftest nelisp-phase47-doc129/object-module-init-metadata ()
  "Doc 129.7AI: object metadata serializes the module-init plan."
  (let* ((metadata
          (nelisp-phase47-compiler--object-module-init-metadata
           '(seq
             (defcustom z 9 "doc" :type 'integer)
             (defun caller
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (cap :type sexp)
                  (xs :type sexp))
               (mapcar (lambda (item) (+ item cap)) xs)))))
         (bytes (plist-get metadata :bytes))
         (text (decode-coding-string
                (substring bytes 0 (1- (length bytes)))
                'utf-8)))
    (should (equal (plist-get metadata :symbol)
                   "nelisp_aot_module_init_plan"))
    (should (= (aref bytes (1- (length bytes))) 0))
    (should (string-match-p ":helper-order" text))
    (should (string-match-p "nelisp_aot_custom_0_z" text))
    (should (string-match-p ":closure-descriptors" text))))

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

(ert-deftest nelisp-phase47-doc129/parse-source-special-let-direct-throw ()
  "Doc 129.4G: special `let' direct throw pops before non-local exit."
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
                    (throw 'tag value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_special externs))
    (should (member 'nelisp_aot_pop_special externs))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-multi-let-direct-throw ()
  "Doc 129.4G: multi-special `let' direct throw pops all bindings."
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
                    (throw 'tag value-b))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_special externs) 2))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 2))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-mixed-let-direct-throw ()
  "Doc 129.4G: mixed special/lexical `let' direct throw preserves lexical value."
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
                    (throw 'tag a))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_special externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 1))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-let-conditional-throw ()
  "Doc 129.4H: special `let' conditional throw cleans up both branches."
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
                    (if value
                        (throw 'tag value)
                      value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_special externs))
    (should (member 'nelisp_aot_pop_special externs))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-multi-let-conditional-throw ()
  "Doc 129.4H: multi-special `let' conditional throw pops every binding."
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
                    (if value-a
                        (throw 'tag value-b)
                      value-a))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_special externs) 2))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 4))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-mixed-let-conditional-throw ()
  "Doc 129.4H: mixed special `let' conditional throw keeps lexical aliases."
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
                    (if a
                        (throw 'tag a)
                      value-b))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_special externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 2))
    (should (member 'nelisp_aot_throw externs))))

(ert-deftest nelisp-phase47-doc129/parse-source-special-let-nested-throw ()
  "Doc 129.4I: nested special `let' throw trees clean up every leaf."
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
                    (if value
                        (if value
                            (throw 'tag value)
                          value)
                      value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_special externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_special externs) 3))
    (should (member 'nelisp_aot_throw externs))))

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

(ert-deftest nelisp-phase47-doc129/object-source-special-let-direct-throw ()
  "Doc 129.4G: object output exposes special cleanup before throw."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-let-throw-" nil ".o")))
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
                 (throw 'tag value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))
            (should (string-match-p "nelisp_aot_throw" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-source-special-let-conditional-throw ()
  "Doc 129.4H: object output exposes branch special cleanup before throw."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-special-let-if-throw-" nil ".o")))
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
                 (if value
                     (throw 'tag value)
                   value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_push_special" out))
            (should (string-match-p "nelisp_aot_pop_special" out))
            (should (string-match-p "nelisp_aot_throw" out))))
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
  "Doc 129.5F: automatic root scope is skipped without AOT handles."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str ((slot :type sexp) bytes len)
                 (sexp-write-str slot bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-write-str))
    (should (equal (nelisp-phase47-compiler--ir-get ir :gc-root-slots)
                   '(0)))))

(ert-deftest nelisp-phase47-doc129/parse-auto-aot-root-scope-frame-roots-auto ()
  "Doc 129.5J: loader-selected hash-FRAMES root scope uses stack roots."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    auto_frame_roots
                    bytes
                    len)
                 (sexp-write-str out bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'aot-root-scope))
    (should (equal (nelisp-phase47-compiler--ir-get body :root-slots)
                   '(0)))
    (should (eq (nelisp-phase47-compiler--ir-get body :root-storage)
                'stack))
    (should (eq (nelisp-phase47-compiler--ir-get body :materialize-kind)
                'frames))
    (should (member 'nelisp_aot_materialize_frame_roots externs))))

(ert-deftest nelisp-phase47-doc129/select-auto-frame-roots-from-metadata ()
  "Doc 129.5K: object loader metadata selects auto frame roots."
  (let* ((source
          '(seq
            (defun make-str
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 bytes
                 len)
              (sexp-write-str out bytes len))
            (defun plain ((slot :type sexp) bytes len)
              (sexp-write-str slot bytes len))))
         (selected
          (nelisp-phase47-compiler--select-auto-frame-roots source)))
    (should
     (equal selected
            '(seq
              (defun make-str
                  ((out :type sexp)
                   (mirror :type sexp)
                   (frames :type sexp)
                   (scratch :type sexp)
                   bytes
                   len
                   auto_frame_roots)
                (sexp-write-str out bytes len))
              (defun plain ((slot :type sexp) bytes len)
                (sexp-write-str slot bytes len)))))))

(ert-deftest nelisp-phase47-doc129/parse-auto-aot-root-scope-stack-roots ()
  "Doc 129.5I: root scope can use hash-FRAMES roots without a roots slot."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    frame_roots
                    bytes
                    len)
                 (sexp-write-str out bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'aot-root-scope))
    (should (equal (nelisp-phase47-compiler--ir-get body :root-slots)
                   '(0)))
    (should (eq (nelisp-phase47-compiler--ir-get body :root-storage)
                'stack))
    (should (eq (nelisp-phase47-compiler--ir-get body :materialize-kind)
                'frames))
    (should (member 'nelisp_aot_materialize_frame_roots externs))))

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

(ert-deftest nelisp-phase47-doc129/object-auto-aot-root-scope-frame-roots-auto ()
  "Doc 129.5J: object output supports loader-selected frame roots."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-auto-frame-roots-selected-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun make-str
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 auto_frame_roots
                 bytes
                 len)
              (sexp-write-str out bytes len))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_materialize_frame_roots" out))
            (should (string-match-p "nelisp_aot_push_roots" out))
            (should (string-match-p "nelisp_aot_pop_roots" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-auto-aot-root-scope-loader-selected ()
  "Doc 129.5K: compile-to-object can select frame roots from metadata."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-auto-frame-roots-loader-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun make-str
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 bytes
                 len)
              (sexp-write-str out bytes len))
           path
           :auto-frame-roots t)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_materialize_frame_roots" out))
            (should (string-match-p "nelisp_aot_push_roots" out))
            (should (string-match-p "nelisp_aot_pop_roots" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-auto-aot-root-scope-stack-roots ()
  "Doc 129.5I: object output uses frame-root materialization without roots."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-auto-frame-roots-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun make-str
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 frame_roots
                 bytes
                 len)
              (sexp-write-str out bytes len))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_materialize_frame_roots" out))
            (should (string-match-p "nelisp_aot_push_roots" out))
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

(ert-deftest nelisp-phase47-doc129/object-direct-builtin1-cl-lib-accessor ()
  "Doc 129.6AQ: object output exposes cl-lib accessor aliases."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-accessor-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_cadddr
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (arg :type sexp))
              (cl-cadddr arg))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_cadddr" out))
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
  "Doc 129.6G/U: calln lowering covers common fixed-arity builtins."
  (dolist (case '((cons (cons a b))
                  (eq (eq a b))
                  (equal (equal a b))
                  (nth (nth a b))
                  (assq (assq a b))
                  (string= (string= a b))
                  (plist-get (plist-get a b))
                  (plist-member (plist-member a b))
                  (gethash (gethash a b))
                  (remhash (remhash a b))
                  (plist-put (plist-put a b c))
                  (puthash (puthash a b c))
                  (seq-first (seq-first a))
                  (seq-rest (seq-rest a))
                  (seq-elt (seq-elt a b))
                  (seq-length (seq-length a))
                  (seq-empty-p (seq-empty-p a))
                  (seq-min (seq-min a))
                  (seq-max (seq-max a))
                  (seq-copy (seq-copy a))
                  (seq-reverse (seq-reverse a))
                  (seq-take (seq-take a b))
                  (seq-drop (seq-drop a b))
                  (seq-subseq (seq-subseq a b c))
                  (seq-partition (seq-partition a b))
                  (seq-split (seq-split a b))
                  (seq-remove-at-position (seq-remove-at-position a b))
                  (seq-positions (seq-positions a b c))
                  (seq-contains (seq-contains a b c))
                  (seq-into (seq-into a b))
                  (seq-concatenate (seq-concatenate a b c))
                  (seq-random-elt (seq-random-elt a))
                  (cl-first (cl-first a))
                  (cl-second (cl-second a))
                  (cl-third (cl-third a))
                  (cl-rest (cl-rest a))
                  (cl-copy-list (cl-copy-list a))
                  (cl-list* (cl-list* a b c))
                  (cl-acons (cl-acons a b c))
                  (cl-pairlis (cl-pairlis a b c))
                  (cl-adjoin (cl-adjoin a b))
                  (cl-endp (cl-endp a))
                  (cl-list-length (cl-list-length a))
                  (cl-subseq (cl-subseq a b c))
                  (cl-concatenate (cl-concatenate a b c))
                  (cl-revappend (cl-revappend a b))
                  (cl-nreconc (cl-nreconc a b))
                  (cl-tailp (cl-tailp a b))
                  (cl-ldiff (cl-ldiff a b))
                  (cl-union (cl-union a b))
                  (cl-intersection (cl-intersection a b))
                  (cl-nunion (cl-nunion a b))
                  (cl-nintersection (cl-nintersection a b))
                  (cl-set-difference (cl-set-difference a b))
                  (cl-set-exclusive-or (cl-set-exclusive-or a b))
                  (cl-nset-difference (cl-nset-difference a b))
                  (cl-nset-exclusive-or (cl-nset-exclusive-or a b))
                  (cl-subsetp (cl-subsetp a b))
                  (cl-member (cl-member a b))
                  (cl-assoc (cl-assoc a b))
                  (cl-rassoc (cl-rassoc a b))
                  (cl-tree-equal (cl-tree-equal a b))
                  (cl-position (cl-position a b))
                  (cl-find (cl-find a b))
                  (cl-count (cl-count a b))
                  (cl-mismatch (cl-mismatch a b))
                  (cl-search (cl-search a b))
                  (cl-remove (cl-remove a b))
                  (cl-delete (cl-delete a b))
                  (cl-substitute (cl-substitute a b c))
                  (cl-nsubstitute (cl-nsubstitute a b c))
                  (cl-subst (cl-subst a b c))
                  (cl-nsubst (cl-nsubst a b c))
                  (cl-sublis (cl-sublis a b))
                  (cl-nsublis (cl-nsublis a b))
                  (cl-remove-duplicates (cl-remove-duplicates a))
                  (cl-delete-duplicates (cl-delete-duplicates a))
                  (cl-fill (cl-fill a b))
                  (cl-replace (cl-replace a b))))
    (pcase-let ((`(,builtin ,form) case))
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
             (call-node (nth 1 forms)))
        (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
        (should (equal (nelisp-phase47-compiler--ir-get symbol-node :bytes)
                       (string-to-list (symbol-name builtin))))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))))))

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
                  (mapcan 2 (mapcan fn xs))
                  (maphash 2 (maphash fn xs))
                  (map-char-table 2 (map-char-table fn xs))
                  (map-keymap 2 (map-keymap fn xs))
                  (map-pairs 1 (map-pairs xs))
                  (map-keys 1 (map-keys xs))
                  (map-values 1 (map-values xs))
                  (map-length 1 (map-length xs))
                  (map-elt 2 (map-elt xs seed))
                  (map-contains-key 2 (map-contains-key xs seed))
                  (map-copy 1 (map-copy xs))
                  (map-into 2 (map-into xs type))
                  (map-merge 3 (map-merge type xs ys))
                  (map-insert 3 (map-insert xs seed pred))
                  (map-delete 2 (map-delete xs seed))
                  (map-put! 3 (map-put! xs seed pred))
                  (seq-map 2 (seq-map fn xs))
                  (seq-do 2 (seq-do fn xs))
                  (seq-each 2 (seq-each fn xs))
                  (seq-filter 2 (seq-filter fn xs))
                  (seq-remove 2 (seq-remove fn xs))
                  (seq-find 2 (seq-find fn xs))
                  (seq-some 2 (seq-some fn xs))
                  (seq-every-p 2 (seq-every-p fn xs))
                  (seq-count 2 (seq-count fn xs))
                  (seq-reduce 3 (seq-reduce fn xs seed))
                  (seq-mapcat 2 (seq-mapcat fn xs))
                  (seq-keep 2 (seq-keep fn xs))
                  (seq-mapn 3 (seq-mapn fn xs ys))
                  (seq-map-indexed 2 (seq-map-indexed fn xs))
                  (seq-sort 2 (seq-sort fn xs))
                  (seq-uniq 2 (seq-uniq xs fn))
                  (seq-position 3 (seq-position xs seed fn))
                  (seq-contains-p 3 (seq-contains-p xs seed fn))
                  (seq-set-equal-p 3 (seq-set-equal-p xs ys fn))
                  (seq-difference 3 (seq-difference xs ys fn))
                  (seq-intersection 3 (seq-intersection xs ys fn))
                  (seq-union 3 (seq-union xs ys fn))
                  (seq-take-while 2 (seq-take-while fn xs))
                  (seq-drop-while 2 (seq-drop-while fn xs))
                  (seq-do-indexed 2 (seq-do-indexed fn xs))
                  (seq-group-by 2 (seq-group-by fn xs))
                  (seq-sort-by 3 (seq-sort-by fn pred xs))
                  (cl-mapcar 2 (cl-mapcar fn xs))
                  (cl-mapc 2 (cl-mapc fn xs))
                  (cl-mapcan 2 (cl-mapcan fn xs))
                  (cl-maplist 2 (cl-maplist fn xs))
                  (cl-mapl 2 (cl-mapl fn xs))
                  (cl-mapcon 2 (cl-mapcon fn xs))
                  (cl-some 2 (cl-some fn xs))
                  (cl-every 2 (cl-every fn xs))
                  (cl-notany 2 (cl-notany fn xs))
                  (cl-notevery 2 (cl-notevery fn xs))
                  (cl-count-if 2 (cl-count-if fn xs))
                  (cl-count-if-not 2 (cl-count-if-not fn xs))
                  (cl-find-if 2 (cl-find-if fn xs))
                  (cl-find-if-not 2 (cl-find-if-not fn xs))
                  (cl-remove-if 2 (cl-remove-if fn xs))
                  (cl-remove-if-not 2 (cl-remove-if-not fn xs))
                  (cl-position-if 2 (cl-position-if fn xs))
                  (cl-position-if-not 2 (cl-position-if-not fn xs))
                  (cl-reduce 2 (cl-reduce fn xs))
                  (cl-delete-if 2 (cl-delete-if fn xs))
                  (cl-delete-if-not 2 (cl-delete-if-not fn xs))
                  (cl-member-if 2 (cl-member-if fn xs))
                  (cl-member-if-not 2 (cl-member-if-not fn xs))
                  (cl-assoc-if 2 (cl-assoc-if fn xs))
                  (cl-assoc-if-not 2 (cl-assoc-if-not fn xs))
                  (cl-rassoc-if 2 (cl-rassoc-if fn xs))
                  (cl-rassoc-if-not 2 (cl-rassoc-if-not fn xs))
                  (cl-substitute-if 3 (cl-substitute-if seed fn xs))
                  (cl-substitute-if-not 3 (cl-substitute-if-not seed fn xs))
                  (cl-nsubstitute-if 3 (cl-nsubstitute-if seed fn xs))
                  (cl-nsubstitute-if-not 3
                                         (cl-nsubstitute-if-not seed fn xs))
                  (cl-subst-if 3 (cl-subst-if seed fn xs))
                  (cl-subst-if-not 3 (cl-subst-if-not seed fn xs))
                  (cl-nsubst-if 3 (cl-nsubst-if seed fn xs))
                  (cl-nsubst-if-not 3 (cl-nsubst-if-not seed fn xs))
                  (cl-map 3 (cl-map type fn xs))
                  (cl-sort 2 (cl-sort xs fn))
                  (cl-stable-sort 2 (cl-stable-sort xs fn))
                  (cl-merge 4 (cl-merge type xs ys fn))
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
                        (ys :type sexp)
                        (type :type sexp)
                        (pred :type sexp)
                        (seed :type sexp)
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
                  (mapconcat #'foo xs sep)
                  (mapcan #'foo xs)
                  (maphash #'foo xs)
                  (map-char-table #'foo xs)
                  (map-keymap #'foo xs)
                  (map-apply #'foo xs)
                  (map-do #'foo xs)
                  (map-filter #'foo xs)
                  (map-some #'foo xs)
                  (map-every-p #'foo xs)
                  (map-remove #'foo xs)
                  (map-keys-apply #'foo xs)
                  (map-values-apply #'foo xs)
                  (seq-map #'foo xs)
                  (seq-do #'foo xs)
                  (seq-each #'foo xs)
                  (seq-filter #'foo xs)
                  (seq-remove #'foo xs)
                  (seq-find #'foo xs)
                  (seq-some #'foo xs)
                  (seq-every-p #'foo xs)
                  (seq-count #'foo xs)
                  (seq-reduce #'foo xs seed)
                  (seq-mapcat #'foo xs)
                  (seq-keep #'foo xs)
                  (seq-mapn #'foo xs ys)
                  (seq-map-indexed #'foo xs)
                  (seq-sort #'foo xs)
                  (seq-take-while #'foo xs)
                  (seq-drop-while #'foo xs)
                  (seq-do-indexed #'foo xs)
                  (seq-group-by #'foo xs)
                  (seq-sort-by #'foo pred xs)
                  (cl-mapcar #'foo xs)
                  (cl-mapc #'foo xs)
                  (cl-mapcan #'foo xs)
                  (cl-maplist #'foo xs)
                  (cl-mapl #'foo xs)
                  (cl-mapcon #'foo xs)
                  (cl-some #'foo xs)
                  (cl-every #'foo xs)
                  (cl-notany #'foo xs)
                  (cl-notevery #'foo xs)
                  (cl-count-if #'foo xs)
                  (cl-count-if-not #'foo xs)
                  (cl-find-if #'foo xs)
                  (cl-find-if-not #'foo xs)
                  (cl-remove-if #'foo xs)
                  (cl-remove-if-not #'foo xs)
                  (cl-position-if #'foo xs)
                  (cl-position-if-not #'foo xs)
                  (cl-reduce #'foo xs)))
    (let* ((ir (nelisp-phase47-compiler--parse
                `(defun call_builtin
                     ((out :type sexp)
                      (mirror :type sexp)
                      (frames :type sexp)
                      (scratch :type sexp)
                      (name_slot :type sexp)
                      (xs :type sexp)
                      (ys :type sexp)
                      (pred :type sexp)
                      (seed :type sexp)
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

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-extended-designator-indexes ()
  "Doc 129.6R/W: additional higher-order builtins materialize designators."
  (dolist (case '(((seq-uniq xs #'foo) 7 "seq-uniq")
                  ((seq-position xs seed #'foo) 8 "seq-position")
                  ((seq-contains-p xs seed #'foo) 8 "seq-contains-p")
                  ((seq-set-equal-p xs ys #'foo) 8 "seq-set-equal-p")
                  ((seq-difference xs ys #'foo) 8 "seq-difference")
                  ((seq-intersection xs ys #'foo) 8 "seq-intersection")
                  ((seq-union xs ys #'foo) 8 "seq-union")
                  ((seq-positions xs seed #'foo) 8 "seq-positions")
                  ((seq-contains xs seed #'foo) 8 "seq-contains")
                  ((cl-map type #'foo xs) 7 "cl-map")
                  ((cl-sort xs #'foo) 7 "cl-sort")
                  ((cl-stable-sort xs #'foo) 7 "cl-stable-sort")
                  ((cl-merge type xs ys #'foo) 9 "cl-merge")
                  ((cl-subst-if seed #'foo xs) 7 "cl-subst-if")
                  ((cl-subst-if-not seed #'foo xs) 7 "cl-subst-if-not")
                  ((cl-nsubst-if seed #'foo xs) 7 "cl-nsubst-if")
                  ((cl-nsubst-if-not seed #'foo xs) 7 "cl-nsubst-if-not")
                  ((map-merge-with type #'foo xs ys) 7 "map-merge-with")))
    (pcase-let ((`(,form ,arg-index ,builtin-name) case))
      (let* ((ir (nelisp-phase47-compiler--parse
                  `(defun call_builtin
                       ((out :type sexp)
                        (mirror :type sexp)
                        (frames :type sexp)
                        (scratch :type sexp)
                        (name_slot :type sexp)
                        (type :type sexp)
                        (xs :type sexp)
                        (ys :type sexp)
                        (seed :type sexp))
                     ,form)))
             (body (nelisp-phase47-compiler--ir-get ir :body))
             (forms (nelisp-phase47-compiler--ir-get body :forms))
             (builtin-symbol (nth 0 forms))
             (fn-symbol (nth 1 forms))
             (call-node (nth 2 forms))
             (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
        (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
        (should (equal (nelisp-phase47-compiler--ir-get builtin-symbol :bytes)
                       (string-to-list builtin-name)))
        (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                       (string-to-list "foo")))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))
        (should (eq (nelisp-phase47-compiler--ir-kind (nth arg-index call-args))
                    'ref))
        (should (eq (nelisp-phase47-compiler--ir-get (nth arg-index call-args)
                                                     :var)
                    'scratch))))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-keyword-designator ()
  "Doc 129.6AF: keyword callback options materialize literal symbols."
  (dolist (case '(((cl-find item xs :test #'eq)
                   keyword_slot 8 9 ":test" "eq")
                  ((cl-position item xs :key #'car)
                   keyword-slot 8 9 ":key" "car")
                  ((cl-adjoin item xs :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-member item xs :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-assoc item xs :key #'car)
                   keyword-slot 8 9 ":key" "car")
                  ((cl-rassoc item xs :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-tree-equal item xs :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-remove-duplicates xs :test #'eq)
                   keyword-slot 7 8 ":test" "eq")
                  ((cl-delete-duplicates xs :key #'car)
                   keyword-slot 7 8 ":key" "car")
                  ((cl-nunion xs ys :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-nintersection xs ys :key #'car)
                   keyword-slot 8 9 ":key" "car")
                  ((cl-nset-difference xs ys :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-nset-exclusive-or xs ys :key #'car)
                   keyword-slot 8 9 ":key" "car")
                  ((cl-subst item xs ys :test #'eq)
                   keyword-slot 9 10 ":test" "eq")
                  ((cl-nsubst item xs ys :key #'car)
                   keyword-slot 9 10 ":key" "car")
                  ((cl-subst-if item xs ys :key #'car)
                   keyword-slot 9 10 ":key" "car")
                  ((cl-nsubst-if item xs ys :key #'car)
                   keyword-slot 9 10 ":key" "car")
                  ((cl-sublis xs ys :test #'eq)
                   keyword-slot 8 9 ":test" "eq")
                  ((cl-nsublis xs ys :key #'car)
                   keyword-slot 8 9 ":key" "car")))
    (pcase-let ((`(,form ,keyword-slot ,keyword-arg-index
                         ,fn-arg-index ,keyword-name ,fn-name)
                 case))
      (let* ((ir (nelisp-phase47-compiler--parse
                  `(defun call_builtin
                       ((out :type sexp)
                        (mirror :type sexp)
                        (frames :type sexp)
                        (scratch :type sexp)
                        (name_slot :type sexp)
                        (,keyword-slot :type sexp)
                        (item :type sexp)
                        (xs :type sexp)
                        (ys :type sexp))
                     ,form)))
             (body (nelisp-phase47-compiler--ir-get ir :body))
             (forms (nelisp-phase47-compiler--ir-get body :forms))
             (keyword-symbol (nth 1 forms))
             (fn-symbol (nth 2 forms))
             (call-node (nth 3 forms))
             (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
        (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
        (should (equal (nelisp-phase47-compiler--ir-get keyword-symbol :bytes)
                       (string-to-list keyword-name)))
        (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                       (string-to-list fn-name)))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))
        (should (eq (nelisp-phase47-compiler--ir-get
                     (nth keyword-arg-index call-args)
                     :var)
                    keyword-slot))
        (should (eq (nelisp-phase47-compiler--ir-get
                     (nth fn-arg-index call-args)
                     :var)
                    'scratch))))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-multiple-keywords ()
  "Doc 129.6AF: multiple literal keywords use indexed keyword slots."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_builtin
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot_0 :type sexp)
                    (keyword_slot_1 :type sexp)
                    (item :type sexp)
                    (xs :type sexp)
                    (test :type sexp)
                    (key :type sexp))
                 (cl-find item xs :test test :key key))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw0-symbol (nth 1 forms))
         (kw1-symbol (nth 2 forms))
         (call-node (nth 3 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (equal (nelisp-phase47-compiler--ir-get kw0-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get kw1-symbol :bytes)
                   (string-to-list ":key")))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 10 call-args)
                 :var)
                'keyword_slot_1))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'test))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 11 call-args)
                 :var)
                'key))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-sequence-helper-keywords ()
  "Doc 129.6AP: cl-lib sequence helpers materialize literal keywords."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_builtin
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot_0 :type sexp)
                    (keyword_slot_1 :type sexp)
                    (xs :type sexp)
                    (item :type sexp)
                    (start :type sexp)
                    (end :type sexp))
                 (cl-fill xs item :start start :end end))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw0-symbol (nth 1 forms))
         (kw1-symbol (nth 2 forms))
         (call-node (nth 3 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (equal (nelisp-phase47-compiler--ir-get kw0-symbol :bytes)
                   (string-to-list ":start")))
    (should (equal (nelisp-phase47-compiler--ir-get kw1-symbol :bytes)
                   (string-to-list ":end")))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 10 call-args)
                 :var)
                'keyword_slot_1))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'start))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 11 call-args)
                 :var)
                'end))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtinn-multiple-keyword-designators ()
  "Doc 129.6AG: multiple keyword callback designators use callback slots."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_builtin
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot_0 :type sexp)
                    (keyword_slot_1 :type sexp)
                    (callback_slot_0 :type sexp)
                    (callback_slot_1 :type sexp)
                    (item :type sexp)
                    (xs :type sexp))
                 (cl-find item xs :test #'eq :key #'car))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw0-symbol (nth 1 forms))
         (kw1-symbol (nth 2 forms))
         (test-symbol (nth 3 forms))
         (key-symbol (nth 4 forms))
         (call-node (nth 5 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (equal (nelisp-phase47-compiler--ir-get kw0-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get kw1-symbol :bytes)
                   (string-to-list ":key")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list "eq")))
    (should (equal (nelisp-phase47-compiler--ir-get key-symbol :bytes)
                   (string-to-list "car")))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'callback_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 10 call-args)
                 :var)
                'keyword_slot_1))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 11 call-args)
                 :var)
                'callback_slot_1))))

(ert-deftest nelisp-phase47-doc129/parse-seq-sort-by-dual-designators ()
  "Doc 129.6AG: `seq-sort-by' can materialize both callback designators."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun call_builtin
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (callback_slot_0 :type sexp)
                    (callback_slot_1 :type sexp)
                    (xs :type sexp))
                 (seq-sort-by #'car #'string< xs))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (key-symbol (nth 1 forms))
         (pred-symbol (nth 2 forms))
         (call-node (nth 3 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (equal (nelisp-phase47-compiler--ir-get key-symbol :bytes)
                   (string-to-list "car")))
    (should (equal (nelisp-phase47-compiler--ir-get pred-symbol :bytes)
                   (string-to-list "string<")))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 6 call-args)
                 :var)
                'callback_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 7 call-args)
                 :var)
                'callback_slot_1))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'xs))))

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

(ert-deftest nelisp-phase47-doc129/parse-mapcan-lambda-lift ()
  "Doc 129.7N: `mapcan' literal lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (mapcan (lambda (x) x) xs))))
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

(ert-deftest nelisp-phase47-doc129/parse-maphash-lambda-lift ()
  "Doc 129.7O: `maphash' literal lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (table :type sexp))
                 (maphash (lambda (k v) k) table))))
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

(ert-deftest nelisp-phase47-doc129/parse-seq-lambda-lift ()
  "Doc 129.7P: seq.el literal lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp)
                    (seed :type sexp))
                 (seq-reduce (lambda (acc x) (+ acc x)) xs seed))))
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

(ert-deftest nelisp-phase47-doc129/parse-seq-prefix-group-lambda-lift ()
  "Doc 129.6X/129.7P: new seq.el callbacks participate in lambda lifting."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (seq-group-by (lambda (x) x) xs))))
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

(ert-deftest nelisp-phase47-doc129/parse-seq-sort-by-lambda-lift ()
  "Doc 129.6Y/129.7P: `seq-sort-by' transform lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (pred :type sexp)
                    (xs :type sexp))
                 (seq-sort-by (lambda (x) x) pred xs))))
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
    (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get lambda-ir :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 call-args) :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 7 call-args) :var)
                'pred))))

(ert-deftest nelisp-phase47-doc129/parse-seq-sort-by-dual-lambda-lift ()
  "Doc 129.7AN: `seq-sort-by' lifts both static callback lambdas."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (callback_slot_0 :type sexp)
                    (callback_slot_1 :type sexp)
                    (xs :type sexp))
                 (seq-sort-by (lambda (x) x)
                              (lambda (a b) a)
                              xs))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (key-lambda (nth 0 forms))
         (pred-lambda (nth 1 forms))
         (caller-ir (nth 2 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (key-symbol (nth 1 body-forms))
         (pred-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind key-lambda) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind pred-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get key-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get key-lambda :name)))))
    (should (equal (nelisp-phase47-compiler--ir-get pred-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get pred-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 6 call-args)
                 :var)
                'callback_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 7 call-args)
                 :var)
                'callback_slot_1))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-keyword-dual-lambda-lift ()
  "Doc 129.7AN: cl-lib keyword callback lambdas lift together."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot_0 :type sexp)
                    (keyword_slot_1 :type sexp)
                    (callback_slot_0 :type sexp)
                    (callback_slot_1 :type sexp)
                    (item :type sexp)
                    (xs :type sexp))
                 (cl-find item xs
                          :test (lambda (a b) a)
                          :key (lambda (x) x)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (key-lambda (nth 1 forms))
         (caller-ir (nth 2 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw0-symbol (nth 1 body-forms))
         (kw1-symbol (nth 2 body-forms))
         (test-symbol (nth 3 body-forms))
         (key-symbol (nth 4 body-forms))
         (call-node (nth 5 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-kind key-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw0-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get kw1-symbol :bytes)
                   (string-to-list ":key")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (equal (nelisp-phase47-compiler--ir-get key-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get key-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'callback_slot_0))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 10 call-args)
                 :var)
                'keyword_slot_1))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 11 call-args)
                 :var)
                'callback_slot_1))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-list-keyword-lambda-lift ()
  "Doc 129.6AJ: cl-lib list helper keyword lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot :type sexp)
                    (item :type sexp)
                    (xs :type sexp))
                 (cl-adjoin item xs :test (lambda (a b) a)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw-symbol (nth 1 body-forms))
         (test-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-tree-equal-keyword-lambda-lift ()
  "Doc 129.6AL: cl-tree-equal keyword lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot :type sexp)
                    (left :type sexp)
                    (right :type sexp))
                 (cl-tree-equal left right :test (lambda (a b) a)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw-symbol (nth 1 body-forms))
         (test-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-duplicates-keyword-lambda-lift ()
  "Doc 129.6AM: cl-lib duplicate helpers keyword lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot :type sexp)
                    (xs :type sexp))
                 (cl-remove-duplicates xs :test (lambda (a b) a)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw-symbol (nth 1 body-forms))
         (test-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 7 call-args)
                 :var)
                'keyword_slot))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-destructive-set-keyword-lambda-lift ()
  "Doc 129.6AN: cl-lib destructive set keyword lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot :type sexp)
                    (xs :type sexp)
                    (ys :type sexp))
                 (cl-nunion xs ys :test (lambda (a b) a)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw-symbol (nth 1 body-forms))
         (test-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 8 call-args)
                 :var)
                'keyword_slot))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-tree-substitution-keyword-lambda-lift ()
  "Doc 129.6AO: cl-lib tree substitution keyword lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (keyword_slot :type sexp)
                    (new :type sexp)
                    (old :type sexp)
                    (tree :type sexp))
                 (cl-subst new old tree :test (lambda (a b) a)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (test-lambda (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (kw-symbol (nth 1 body-forms))
         (test-symbol (nth 2 body-forms))
         (call-node (nth 3 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind test-lambda) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get kw-symbol :bytes)
                   (string-to-list ":test")))
    (should (equal (nelisp-phase47-compiler--ir-get test-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get test-lambda :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 9 call-args)
                 :var)
                'keyword_slot))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 10 call-args)
                 :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-lambda-lift ()
  "Doc 129.7Q: cl-lib literal lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (cl-remove-if (lambda (x) x) xs))))
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

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-predicate-lambda-lift ()
  "Doc 129.7S: extended cl-lib predicate lambdas lift to defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (cl-member-if (lambda (x) x) xs))))
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

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-substitute-lambda-lift ()
  "Doc 129.7S: cl-substitute-if predicate position lambda-lifts."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (new :type sexp)
                    (xs :type sexp))
                 (cl-substitute-if new (lambda (x) x) xs))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 2 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind lambda-ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 call-args) :var)
                'new))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 7 call-args) :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-cl-lib-tree-predicate-lambda-lift ()
  "Doc 129.6AS: cl-subst-if predicate position lambda-lifts."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (new :type sexp)
                    (tree :type sexp))
                 (cl-subst-if new (lambda (x) x) tree))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (body (nelisp-phase47-compiler--ir-get caller-ir :body))
         (body-forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 2 body-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (eq (nelisp-phase47-compiler--ir-kind lambda-ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 6 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 call-args) :var)
                'new))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 7 call-args) :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/parse-extended-higher-order-lambda-lift ()
  "Doc 129.7Z: extended higher-order literal lambdas lift to defuns."
  (dolist (case '(((seq-each (lambda (x) x) xs) 6)
                  ((seq-map-indexed (lambda (x i) x) xs) 6)
                  ((seq-uniq xs (lambda (a b) a)) 7)
                  ((seq-positions xs seed (lambda (a b) a)) 8)
                  ((seq-contains xs seed (lambda (a b) a)) 8)
                  ((map-char-table (lambda (k v) k) xs) 6)
                  ((map-keymap (lambda (k v) k) xs) 6)
                  ((map-filter (lambda (k v) k) xs) 6)
                  ((map-merge-with type (lambda (a b) a) xs ys) 7)
                  ((map-some (lambda (k v) k) xs) 6)
                  ((map-values-apply (lambda (v) v) xs) 6)
                  ((cl-subst-if seed (lambda (x) x) xs) 7)
                  ((cl-map type (lambda (x) x) xs) 7)
                  ((cl-merge type xs ys (lambda (a b) a)) 9)))
    (pcase-let ((`(,form ,arg-index) case))
      (let* ((ir (nelisp-phase47-compiler--parse
                  `(defun caller
                       ((out :type sexp)
                        (mirror :type sexp)
                        (frames :type sexp)
                        (scratch :type sexp)
                        (name_slot :type sexp)
                        (type :type sexp)
                        (xs :type sexp)
                        (ys :type sexp)
                        (seed :type sexp))
                     ,form)))
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
        (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                       (string-to-list
                        (symbol-name
                         (nelisp-phase47-compiler--ir-get lambda-ir :name)))))
        (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                    'nelisp_aot_builtin_calln))
        (should (eq (nelisp-phase47-compiler--ir-kind
                     (nth arg-index call-args))
                    'ref))
        (should (eq (nelisp-phase47-compiler--ir-get
                     (nth arg-index call-args) :var)
                    'scratch))))))

(ert-deftest nelisp-phase47-doc129/parse-sort-lambda-lift ()
  "Doc 129.7N: `sort' predicate lambdas lift to synthetic defuns."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (xs :type sexp))
                 (sort xs (lambda (a b) (< a b))))))
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
    (should (equal (nelisp-phase47-compiler--ir-get fn-symbol :bytes)
                   (string-to-list
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get lambda-ir :name)))))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_builtin_calln))
    (should (eq (nelisp-phase47-compiler--ir-kind (nth 7 call-args))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 7 call-args) :var)
                'scratch))))

(ert-deftest nelisp-phase47-doc129/map-lambda-closure-capture ()
  "Doc 129.7U: map callbacks with captures materialize heap closures."
  (nelisp-phase47-doc129-test--capturing-callback-closure-ir
   '(mapcar (lambda (x) (+ x cap)) xs)
   6))

(ert-deftest nelisp-phase47-doc129/map-lambda-closure-descriptor ()
  "Doc 129.7U: captured callbacks expose closure descriptors."
  (let* ((descriptors
          (nelisp-phase47-compiler--closure-descriptors
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (cap :type sexp)
                 (xs :type sexp))
              (mapcar (lambda (x) (+ x cap)) xs))))
         (descriptor (car descriptors)))
    (should (= (length descriptors) 1))
    (should (eq (plist-get descriptor :name) 'nelisp_aot_closure_0))
    (should (equal (plist-get descriptor :arglist) '(x)))
    (should (equal (plist-get descriptor :body) '((+ x cap))))
    (should (equal (plist-get descriptor :captures) '(cap)))))

(ert-deftest nelisp-phase47-doc129/map-lambda-closure-module-plan ()
  "Doc 129.7V: captured callback descriptors enter module init plans."
  (let* ((descriptors
          (nelisp-phase47-compiler--closure-descriptors
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (cap :type sexp)
                 (xs :type sexp))
              (mapcar (lambda (x) (+ x cap)) xs))))
         (plan (nelisp-cc-runtime-aot-module-init-plan
                nil nil nil descriptors)))
    (should (equal (plist-get plan :closure-descriptors)
                   descriptors))))

(ert-deftest nelisp-phase47-doc129/function-lambda-closure-value ()
  "Doc 129.7X: escaping literal lambda values materialize heap closures."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make_closure
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp))
                 (function (lambda (x) (+ x cap))))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (make-closure (nth 1 forms))
         (make-args (nelisp-phase47-compiler--ir-get make-closure :args))
         (result (nth 2 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get make-closure :name)
                'nelisp_aot_make_closure))
    (should (= (nelisp-phase47-compiler--ir-get (nth 3 make-args) :value)
               1))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 6 make-args) :var)
                'cap))
    (should (eq (nelisp-phase47-compiler--ir-kind result) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get result :var) 'out))))

(ert-deftest nelisp-phase47-doc129/raw-lambda-closure-value ()
  "Doc 129.7X: raw escaping lambda values materialize heap closures."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make_closure
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp))
                 (lambda (x) (+ x cap)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (make-closure (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get make-closure :name)
                'nelisp_aot_make_closure))))

(ert-deftest nelisp-phase47-doc129/function-lambda-closure-descriptor ()
  "Doc 129.7X: escaping lambda values expose closure descriptors."
  (let* ((descriptors
          (nelisp-phase47-compiler--closure-descriptors
           '(defun make_closure
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp))
              (function (lambda (x) (+ x cap))))))
         (descriptor (car descriptors)))
    (should (= (length descriptors) 1))
    (should (eq (plist-get descriptor :name) 'nelisp_aot_closure_0))
    (should (equal (plist-get descriptor :arglist) '(x)))
    (should (equal (plist-get descriptor :body) '((+ x cap))))
    (should (equal (plist-get descriptor :captures) '(cap)))))

(ert-deftest nelisp-phase47-doc129/sort-lambda-closure-capture ()
  "Doc 129.7U: `sort' predicates with captures materialize closures."
  (nelisp-phase47-doc129-test--capturing-callback-closure-ir
   '(sort xs (lambda (a b) (< (+ a cap) b)))
   7))

(ert-deftest nelisp-phase47-doc129/maphash-lambda-closure-capture ()
  "Doc 129.7U: `maphash' callbacks with captures materialize closures."
  (nelisp-phase47-doc129-test--capturing-callback-closure-ir
   '(maphash (lambda (k v) (+ cap k)) table)
   6))

(ert-deftest nelisp-phase47-doc129/seq-lambda-closure-capture ()
  "Doc 129.7U: seq.el callbacks with captures materialize closures."
  (nelisp-phase47-doc129-test--capturing-callback-closure-ir
   '(seq-filter (lambda (x) (eq x cap)) xs)
   6))

(ert-deftest nelisp-phase47-doc129/cl-lib-lambda-closure-capture ()
  "Doc 129.7U: cl-lib callbacks with captures materialize closures."
  (nelisp-phase47-doc129-test--capturing-callback-closure-ir
   '(cl-find-if (lambda (x) (eq x cap)) xs)
   6))

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

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-mapcan-designator ()
  "Doc 129.6L: object output exposes mapcan designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-mapcan-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_mapcan
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (mapcan #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_mapcan" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-maphash-designator ()
  "Doc 129.6M: object output exposes maphash designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-maphash-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_maphash
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (table :type sexp))
              (maphash #'foo table))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_maphash" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-map-el-designator ()
  "Doc 129.6Z: object output exposes map.el designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-map-el-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_map_filter
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (map-filter #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_map_filter" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-seq-designator ()
  "Doc 129.6N: object output exposes seq.el designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-seq-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_seq
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (seq-filter #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_seq" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-extra-callback-designators ()
  "Doc 129.6AH: object output exposes remaining callback designators."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-extra-callback-designators-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defun call_seq_each
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (xs :type sexp))
               (seq-each #'foo xs))
             (defun call_map_keymap
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (keymap :type sexp))
               (map-keymap #'foo keymap))
             (defun call_seq_positions
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (xs :type sexp)
                  (seed :type sexp))
               (seq-positions xs seed #'foo))
             (defun call_seq_contains
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (xs :type sexp)
                  (seed :type sexp))
               (seq-contains xs seed #'foo)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_seq_each" out))
            (should (string-match-p "call_map_keymap" out))
            (should (string-match-p "call_seq_positions" out))
            (should (string-match-p "call_seq_contains" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-designator ()
  "Doc 129.6O: object output exposes cl-lib designator materialization."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (xs :type sexp))
              (cl-find-if #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-predicate-designator ()
  "Doc 129.6P: object output exposes extended cl-lib predicates."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-pred-designator-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_pred
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (new :type sexp)
                 (xs :type sexp))
              (cl-substitute-if new #'foo xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_pred" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-list-keyword-designator ()
  "Doc 129.6AJ: object output exposes cl-lib list keyword callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-list-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_adjoin
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot :type sexp)
                 (item :type sexp)
                 (xs :type sexp))
              (cl-adjoin item xs :test #'foo))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_adjoin" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-member-keyword-designator ()
  "Doc 129.6AK: object output exposes cl-lib member/assoc callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-member-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_member
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot :type sexp)
                 (item :type sexp)
                 (xs :type sexp))
              (cl-member item xs :test #'foo))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_member" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-tree-equal-keyword-designator ()
  "Doc 129.6AL: object output exposes cl-tree-equal keyword callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-tree-equal-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_tree_equal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot :type sexp)
                 (left :type sexp)
                 (right :type sexp))
              (cl-tree-equal left right :test #'foo))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_tree_equal" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-duplicates-keyword-designator ()
  "Doc 129.6AM: object output exposes duplicate-removal callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-duplicates-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_remove_duplicates
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot :type sexp)
                 (xs :type sexp))
              (cl-remove-duplicates xs :test #'foo))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_remove_duplicates" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-destructive-set-keyword-designator ()
  "Doc 129.6AN: object output exposes destructive set callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-destructive-set-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_nunion
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot :type sexp)
                 (xs :type sexp)
                 (ys :type sexp))
              (cl-nunion xs ys :test #'foo))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_nunion" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-tree-substitution-keyword-designator ()
  "Doc 129.6AO/AS: object output exposes tree substitution callbacks."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-tree-substitution-keyword-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_subst_if
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (new :type sexp)
                 (tree :type sexp))
              (cl-subst-if new #'foo tree))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_subst_if" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-direct-builtinn-cl-lib-sequence-helper-keywords ()
  "Doc 129.6AP: object output exposes cl-lib sequence helper keywords."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-sequence-helper-keywords-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_cl_fill
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (keyword_slot_0 :type sexp)
                 (keyword_slot_1 :type sexp)
                 (xs :type sexp)
                 (item :type sexp)
                 (start :type sexp)
                 (end :type sexp))
              (cl-fill xs item :start start :end end))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_cl_fill" out))
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

(ert-deftest nelisp-phase47-doc129/object-map-lambda-closure-capture ()
  "Doc 129.7U: object output exposes captured callback closure bridge."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-map-closure-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun call_mapcar
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (cap :type sexp)
                 (xs :type sexp))
              (mapcar (lambda (x) (+ x cap)) xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "call_mapcar" out))
            (should (string-match-p "nelisp_aot_make_closure" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-module-init-plan-embedded ()
  "Doc 129.7AI: object output embeds module-init metadata in rodata."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-module-init-plan-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defcustom z 9 "doc" :type 'integer)
             (defun call_mapcar
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (cap :type sexp)
                  (xs :type sexp))
               (mapcar (lambda (x) (+ x cap)) xs)))
           path)
          (let ((symbols (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process "readelf" nil t nil
                                           "--wide" "-s" path))))
                (contents (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally path)
                            (buffer-string))))
            (should (string-match-p "nelisp_aot_module_init_plan" symbols))
            (should (string-match-p "OBJECT" symbols))
            (should (string-match-p "LOCAL" symbols))
            (should (string-match-p ":closure-descriptors" contents))
            (should (string-match-p "nelisp_aot_custom_0_z" contents))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-module-init-plan-loader-consumes ()
  "Doc 129.7AJ: standalone loader consumes embedded object metadata."
  (let ((path (make-temp-file "nelisp-doc129-module-init-loader-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq
             (defcustom z 9 "doc" :type 'integer)
             (defun call_mapcar
                 ((out :type sexp)
                  (mirror :type sexp)
                  (frames :type sexp)
                  (scratch :type sexp)
                  (name_slot :type sexp)
                  (cap :type sexp)
                  (xs :type sexp))
               (mapcar (lambda (x) (+ x cap)) xs)))
           path)
          (let* ((payload
                  (nelisp-elf-read-symbol-bytes
                   path "nelisp_aot_module_init_plan"))
                 (plan
                  (nelisp-cc-runtime-read-aot-object-module-init-plan path))
                 (closure (car (plist-get plan :closure-descriptors))))
            (should (= (aref payload (1- (length payload))) 0))
            (should (equal (plist-get plan :helper-order)
                           '(nelisp_aot_custom_0_z)))
            (should (eq (plist-get closure :name) 'nelisp_aot_closure_0))
            (unwind-protect
                (let ((result
                       (nelisp-cc-runtime-run-aot-standalone-loader-from-object
                        path
                        :allow-host-stub t
                        :resolver (lambda (_symbol)
                                    (cons :host-stub #x810000)))))
                  (should (plist-get result :module-init))
                  (should (equal
                           (plist-get
                            (nelisp-cc-runtime-aot-custom-metadata 'z)
                            :helper)
                           'nelisp_aot_custom_0_z))
                  (should (equal
                           (plist-get
                            (nelisp-cc-runtime-aot-closure-descriptor
                             'nelisp_aot_closure_0)
                            :captures)
                           '(cap))))
              (nelisp-cc-runtime-clear-aot-c-abi-exports)
              (nelisp-cc-runtime-clear-aot-custom-table)
              (nelisp-cc-runtime-clear-aot-closure-descriptors))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-module-init-loader-native-thunk ()
  "Doc 129.3Q: object loader can run an external native-call thunk."
  (skip-unless (file-executable-p "/bin/sh"))
  (let ((path (make-temp-file "nelisp-doc129-module-init-thunk-" nil ".o"))
        (script (make-temp-file "nelisp-doc129-native-thunk-" nil ".sh")))
    (unwind-protect
        (progn
          (write-region
           "#!/bin/sh\nprintf '%s|%s|%s\\n' \"$2\" \"$3\" \"$4\"\n"
           nil script nil 'silent)
          (set-file-modes script #o755)
          (nelisp-phase47-compile-to-object
           '(seq
             (defvar z 9)
             (defconst k 11))
           path)
          (unwind-protect
              (let* ((native-call
                      (nelisp-cc-runtime-aot-native-call-entry
                       (nelisp-cc-runtime-aot-executable-native-thunk
                        script)))
                     (result
                      (nelisp-cc-runtime-run-aot-standalone-loader-from-object
                       path
                       :resolver
                       (lambda (_symbol) (cons :resolved #x810000))
                       :native-call native-call))
                     (module-init (plist-get result :module-init))
                     (init-results (plist-get module-init :init-results)))
                (should (equal (mapcar #'car init-results)
                               '(nelisp_aot_var_0_z
                                 nelisp_aot_const_1_k)))
                (should (equal
                         (plist-get (cdr (nth 0 init-results)) :stdout)
                         "nelisp_aot_var_0_z|defvar|5\n"))
                (should (equal
                         (plist-get (cdr (nth 1 init-results)) :stdout)
                         "nelisp_aot_const_1_k|defconst|5\n")))
            (nelisp-cc-runtime-clear-aot-c-abi-exports)
            (nelisp-cc-runtime-clear-aot-custom-table)
            (nelisp-cc-runtime-clear-aot-closure-descriptors)))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file script)))))

(ert-deftest nelisp-phase47-doc129/object-function-lambda-closure-value ()
  "Doc 129.7X: object output exposes escaping lambda closure bridge."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-value-closure-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun make_closure
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp))
              (function (lambda (x) (+ x cap))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "make_closure" out))
            (should (string-match-p "nelisp_aot_make_closure" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-mapcan-lambda-lift ()
  "Doc 129.7N: object output exposes mapcan lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-mapcan-lambda-" nil ".o")))
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
              (mapcan (lambda (x) x) xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-maphash-lambda-lift ()
  "Doc 129.7O: object output exposes maphash lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-maphash-lambda-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (table :type sexp))
              (maphash (lambda (k v) k) table))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-seq-lambda-lift ()
  "Doc 129.7P: object output exposes seq.el lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-seq-lambda-" nil ".o")))
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
              (seq-filter (lambda (x) x) xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-cl-lib-lambda-lift ()
  "Doc 129.7Q: object output exposes cl-lib lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cl-lib-lambda-" nil ".o")))
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
              (cl-find-if (lambda (x) x) xs))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-sort-lambda-lift ()
  "Doc 129.7N: object output exposes sort predicate lambda-lift defuns."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-sort-lambda-" nil ".o")))
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
              (sort xs (lambda (a b) (< a b))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_lambda_0" out))
            (should (string-match-p "nelisp_aot_builtin_calln" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/parse-direct-builtin1-table ()
  "Doc 129.6E/U: direct builtin1 lowering covers the shipped unary table."
  (dolist (builtin '(identity length car cdr symbolp stringp
                              hash-table-p hash-table-count
                              number-to-string
                              cl-caaar cl-caadr cl-cadar cl-caddr
                              cl-cdaar cl-cdadr cl-cddar cl-cdddr
                              cl-caaaar cl-caaadr cl-caadar cl-caaddr
                              cl-cadaar cl-cadadr cl-caddar cl-cadddr
                              cl-cdaaar cl-cdaadr cl-cdadar cl-cdaddr
                              cl-cddaar cl-cddadr cl-cdddar cl-cddddr
                              cl-fourth cl-fifth cl-sixth cl-seventh
                              cl-eighth cl-ninth cl-tenth
                              cl-copy-seq cl-evenp cl-oddp
                              cl-plusp cl-minusp cl-functionp
                              cl-floatp-safe cl-type-of))
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

(ert-deftest nelisp-phase47-doc129/parse-funcall-lambda-lift-capture ()
  "Doc 129.7R: direct funcall lambda captures thread as leading args."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (cap x)
                 (funcall (lambda (y) (+ y cap)) x))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (call-node (nelisp-phase47-compiler--ir-get caller-ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (equal (nelisp-phase47-compiler--ir-get lambda-ir :params)
                   '(cap y)))
    (should (eq (nelisp-phase47-compiler--ir-kind call-node) 'call))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))
    (should (equal (mapcar (lambda (arg)
                             (nelisp-phase47-compiler--ir-get arg :var))
                           (nelisp-phase47-compiler--ir-get call-node :args))
                   '(cap x)))))

(ert-deftest nelisp-phase47-doc129/parse-funcall-lambda-lift-let-capture ()
  "Doc 129.7R: direct funcall lambda captures runtime let slots."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (x)
                 (let ((cap x))
                   (funcall (lambda (y) (+ y cap)) x)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (let-node (nelisp-phase47-compiler--ir-get caller-ir :body))
         (call-node (nelisp-phase47-compiler--ir-get let-node :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind let-node) 'let-rt))
    (should (equal (nelisp-phase47-compiler--ir-get lambda-ir :params)
                   '(cap y)))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))
    (should (equal (mapcar (lambda (arg)
                             (nelisp-phase47-compiler--ir-get arg :var))
                           (nelisp-phase47-compiler--ir-get call-node :args))
                   '(cap x)))))

(ert-deftest nelisp-phase47-doc129/parse-funcall-lambda-captured-setq-closure ()
  "Doc 129.7Y: captured mutation funcalls materialize heap closures."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (funcall (lambda (y) (setq cap y)) x))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (make-closure (nth 1 forms))
         (call-node (nth 2 forms))
         (make-args (nelisp-phase47-compiler--ir-get make-closure :args))
         (capture-cell (nth 6 make-args))
         (capture-forms (nelisp-phase47-compiler--ir-get capture-cell :forms))
         (capture-call (nth 1 capture-forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get make-closure :name)
                'nelisp_aot_make_closure))
    (should (= (nelisp-phase47-compiler--ir-get (nth 3 make-args) :value)
               1))
    (should (eq (nelisp-phase47-compiler--ir-kind capture-cell)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get capture-call :name)
                'nelisp_aot_capture_cell))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nth 3 (nelisp-phase47-compiler--ir-get capture-call :args))
                 :var)
                'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_funcall1))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 call-args) :var)
                'out))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read ()
  "Doc 129.7AH: captured mutation reads use the frame-slot ABI."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (seq
                  (funcall (lambda (y) (setq cap y)) x)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (slot-args (nelisp-phase47-compiler--ir-get slot-call :args))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 slot-args) :var)
                'scratch))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-if ()
  "Doc 129.7AK: both-branch captured mutation selects frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp)
                    (y :type sexp))
                 (seq
                  (if flag
                      (funcall (lambda (v) (setq cap v)) x)
                    (funcall (lambda (v) (setq cap v)) y))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-if-partial ()
  "Doc 129.7AK: one-branch captured mutation keeps ordinary reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (if flag
                      (funcall (lambda (v) (setq cap v)) x)
                    0)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (read-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind read-node) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get read-node :var) 'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should-not (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-cond ()
  "Doc 129.7AL: exhaustive cond mutation selects frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp)
                    (y :type sexp))
                 (seq
                  (cond
                   (flag
                    (funcall (lambda (v) (setq cap v)) x))
                   (t
                    (funcall (lambda (v) (setq cap v)) y)))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-cond-partial ()
  "Doc 129.7AL: non-exhaustive cond keeps ordinary reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (cond
                   (flag
                    (funcall (lambda (v) (setq cap v)) x)))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (read-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind read-node) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get read-node :var) 'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should-not (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-and-leading ()
  "Doc 129.7AM: leading short-circuit mutation selects frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (and
                   (funcall (lambda (v) (setq cap v)) x)
                   flag)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-and-trailing ()
  "Doc 129.7AM: trailing short-circuit mutation keeps ordinary reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (and
                   flag
                   (funcall (lambda (v) (setq cap v)) x))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (read-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind read-node) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get read-node :var) 'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should-not (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-or-leading ()
  "Doc 129.7AP: leading `or' mutation selects frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (or
                   (funcall (lambda (v) (setq cap v)) x)
                   flag)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-or-trailing ()
  "Doc 129.7AP: trailing `or' mutation keeps ordinary reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (flag :type sexp)
                    (x :type sexp))
                 (seq
                  (or
                   flag
                   (funcall (lambda (v) (setq cap v)) x))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (read-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind read-node) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get read-node :var) 'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should-not (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-inside-and ()
  "Doc 129.7AQ: later `and' operands see guaranteed captured mutation."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (and
                  (funcall (lambda (v) (setq cap v)) x)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'logic))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-inside-or ()
  "Doc 129.7AQ: executed later `or' operands see prior captured mutation."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (or
                  (funcall (lambda (v) (setq cap v)) x)
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'logic))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-inside-if-condition ()
  "Doc 129.7AR: `if' branches see captured mutation from condition."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (if (funcall (lambda (v) (setq cap v)) x)
                     cap
                   cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (then-ref (nelisp-phase47-compiler--ir-get body :then))
         (else-ref (nelisp-phase47-compiler--ir-get body :else))
         (then-call (nth 1 (nelisp-phase47-compiler--ir-get
                            then-ref :forms)))
         (else-call (nth 1 (nelisp-phase47-compiler--ir-get
                            else-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (eq (nelisp-phase47-compiler--ir-kind then-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind else-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get then-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (eq (nelisp-phase47-compiler--ir-get else-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-let ()
  "Doc 129.7AO: captured mutation inside `let' selects later frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (seq
                  (let ((tmp x))
                    (funcall (lambda (v) (setq cap v)) tmp))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-captured-setq-frame-slot-read-after-let-star ()
  "Doc 129.7AO: captured mutation inside `let*' selects later frame-slot reads."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp))
                 (seq
                  (let* ((tmp x)
                         (tmp2 tmp))
                    (funcall (lambda (v) (setq cap v)) tmp2))
                  cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (slot-ref (nth 1 forms))
         (slot-call (nth 1 (nelisp-phase47-compiler--ir-get
                            slot-ref :forms)))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind slot-ref)
                'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get slot-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (member 'nelisp_aot_capture_cell externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/parse-frame-slot-rewrite-respects-let-shadow ()
  "Doc 129.7AO: frame-slot rewrite does not cross lexical `let' shadowing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp)
                    (x :type sexp)
                    (y :type sexp))
                 (seq
                  (funcall (lambda (v) (setq cap v)) x)
                  (let ((cap y))
                    cap)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (let-node (nth 1 forms))
         (let-body (nelisp-phase47-compiler--ir-get let-node :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind let-node) 'let-rt))
    (should (eq (nelisp-phase47-compiler--ir-kind let-body) 'ref))
    (should (eq (nelisp-phase47-compiler--ir-get let-body :var) 'cap))
    (should (member 'nelisp_aot_capture_cell externs))
    (should-not (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/funcall-lambda-captured-setq-descriptor ()
  "Doc 129.7Y: captured mutation closures expose descriptors."
  (let* ((descriptors
          (nelisp-phase47-compiler--closure-descriptors
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (funcall (lambda (y) (setq cap y)) x))))
         (descriptor (car descriptors)))
    (should (= (length descriptors) 1))
    (should (eq (plist-get descriptor :name) 'nelisp_aot_closure_0))
    (should (equal (plist-get descriptor :arglist) '(y)))
    (should (equal (plist-get descriptor :body) '((setq cap y))))
    (should (equal (plist-get descriptor :captures) '(cap)))))

(ert-deftest nelisp-phase47-doc129/funcall-lambda-captured-setq-requires-boundary ()
  "Doc 129.7Y: captured mutation closure lowering needs boundary slots."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun caller (cap x)
       (funcall (lambda (y) (setq cap y)) x)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-doc129/parse-aot-capture-cell ()
  "Doc 129.7AB: capture-cell materialization lowers to the runtime bridge."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp))
                 (aot-capture-cell 'cap cap))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 1 forms))
         (args (nelisp-phase47-compiler--ir-get call-node :args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_capture_cell))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 args) :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 3 args) :var)
                'cap))))

(ert-deftest nelisp-phase47-doc129/parse-aot-frame-slot-ref-set ()
  "Doc 129.7AG: frame-slot forms lower to the runtime ABI bridges."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (cap :type sexp))
                 (seq
                  (aot-frame-slot-set 'cap cap)
                  (aot-frame-slot-ref 'cap)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (set-seq (nth 0 forms))
         (ref-seq (nth 1 forms))
         (set-call (nth 1 (nelisp-phase47-compiler--ir-get
                           set-seq :forms)))
         (ref-call (nth 1 (nelisp-phase47-compiler--ir-get
                           ref-seq :forms)))
         (set-args (nelisp-phase47-compiler--ir-get set-call :args))
         (ref-args (nelisp-phase47-compiler--ir-get ref-call :args))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind set-seq) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get set-call :name)
                'nelisp_aot_frame_slot_set))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 set-args) :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 3 set-args) :var)
                'cap))
    (should (eq (nelisp-phase47-compiler--ir-get ref-call :name)
                'nelisp_aot_frame_slot_ref))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 ref-args) :var)
                'scratch))
    (should (member 'nelisp_aot_frame_slot_set externs))
    (should (member 'nelisp_aot_frame_slot_ref externs))))

(ert-deftest nelisp-phase47-doc129/aot-frame-slot-requires-boundary ()
  "Doc 129.7AG: frame-slot forms require boxed boundary slots."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun caller (cap)
       (aot-frame-slot-set 'cap cap)))
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

(ert-deftest nelisp-phase47-doc129/e2e-funcall-lambda-lift-capture ()
  "Doc 129.7R: execute direct funcall lambda capture threading."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary
               "lambda-lift-capture")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun caller (cap x)
               (funcall (lambda (y) (+ y cap)) x))
             (exit (caller 8 34)))
           path)
          (should (= (nelisp-phase47-doc129-test--run-binary path) 42)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/e2e-funcall-lambda-lift-let-capture ()
  "Doc 129.7R: execute runtime let capture threading."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary
               "lambda-lift-let-capture")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun caller (x)
               (let ((cap x))
                 (funcall (lambda (y) (+ y cap)) x)))
             (exit (caller 21)))
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

(ert-deftest nelisp-phase47-doc129/object-funcall-lambda-captured-setq ()
  "Doc 129.7Y: object output exposes captured mutation closure dispatch."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (funcall (lambda (y) (setq cap y)) x))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "caller" out))
            (should (string-match-p "nelisp_aot_make_closure" out))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_funcall1" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read ()
  "Doc 129.7AH: object output selects frame-slot ABI after captured setq."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (seq
               (funcall (lambda (y) (setq cap y)) x)
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-if ()
  "Doc 129.7AK: object output selects frame-slot ABI after both branches."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-if-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (flag :type sexp)
                 (x :type sexp)
                 (y :type sexp))
              (seq
               (if flag
                   (funcall (lambda (v) (setq cap v)) x)
                 (funcall (lambda (v) (setq cap v)) y))
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-cond ()
  "Doc 129.7AL: object output selects frame-slot ABI after exhaustive cond."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-cond-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (flag :type sexp)
                 (x :type sexp)
                 (y :type sexp))
              (seq
               (cond
                (flag
                 (funcall (lambda (v) (setq cap v)) x))
                (t
                 (funcall (lambda (v) (setq cap v)) y)))
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-and-leading ()
  "Doc 129.7AM: object output selects frame-slot ABI after leading and."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-and-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (flag :type sexp)
                 (x :type sexp))
              (seq
               (and
                (funcall (lambda (v) (setq cap v)) x)
                flag)
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-or-leading ()
  "Doc 129.7AP: object output selects frame-slot ABI after leading or."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-or-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (flag :type sexp)
                 (x :type sexp))
              (seq
               (or
                (funcall (lambda (v) (setq cap v)) x)
                flag)
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-inside-and ()
  "Doc 129.7AQ: object output selects frame-slot ABI inside `and'."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-and-intra-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (and
               (funcall (lambda (v) (setq cap v)) x)
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-inside-if-condition ()
  "Doc 129.7AR: object output selects frame-slot ABI inside `if' branches."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-if-condition-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (if (funcall (lambda (v) (setq cap v)) x)
                  cap
                cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-let ()
  "Doc 129.7AO: object output selects frame-slot ABI after lexical let."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-let-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (seq
               (let ((tmp x))
                 (funcall (lambda (v) (setq cap v)) tmp))
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-captured-setq-frame-slot-read-after-let-star ()
  "Doc 129.7AP: object output selects frame-slot ABI after lexical let*."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-lambda-setq-let-star-read-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp)
                 (x :type sexp))
              (seq
               (let* ((tmp x)
                      (tmp2 tmp))
                 (funcall (lambda (v) (setq cap v)) tmp2))
               cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nelisp_aot_funcall1" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-aot-capture-cell ()
  "Doc 129.7AB: object output exposes capture-cell bridge relocation."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-capture-cell-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp))
              (aot-capture-cell 'cap cap))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_capture_cell" out))
            (should (string-match-p "nl_alloc_symbol" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-aot-frame-slot-ref-set ()
  "Doc 129.7AG: object output exposes frame-slot ABI relocations."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-frame-slot-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun caller
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (cap :type sexp))
              (seq
               (aot-frame-slot-set 'cap cap)
               (aot-frame-slot-ref 'cap)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "nelisp_aot_frame_slot_set" out))
            (should (string-match-p "nelisp_aot_frame_slot_ref" out))
            (should (string-match-p "nl_alloc_symbol" out))))
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

(ert-deftest nelisp-phase47-doc129/parse-direct-lambda-lift-capture ()
  "Doc 129.7R: direct literal lambda captures thread as leading args."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun caller (cap x)
                 ((lambda (y) (* y cap)) x))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (lambda-ir (nth 0 forms))
         (caller-ir (nth 1 forms))
         (call-node (nelisp-phase47-compiler--ir-get caller-ir :body)))
    (should (equal (nelisp-phase47-compiler--ir-get lambda-ir :params)
                   '(cap y)))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                (nelisp-phase47-compiler--ir-get lambda-ir :name)))
    (should (equal (mapcar (lambda (arg)
                             (nelisp-phase47-compiler--ir-get arg :var))
                           (nelisp-phase47-compiler--ir-get call-node :args))
                   '(cap x)))))

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

(ert-deftest nelisp-phase47-doc129/e2e-direct-lambda-lift-capture ()
  "Doc 129.7R: execute direct literal lambda capture threading."
  (unless (nelisp-phase47-doc129-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-doc129-test--tmp-binary
               "direct-lambda-lift-capture")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defun caller (cap x)
               ((lambda (y) (* y cap)) x))
             (exit (caller 2 21)))
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

(ert-deftest nelisp-phase47-doc129/parse-push-catch-handler-landing-label ()
  "Doc 129.8V: quoted handler landing labels materialize as metadata."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun push_catch_label
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp))
                 (aot-push-catch 'done
                                 'doc129_catch_landing
                                 (aot-current-sp)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (tag-write (nth 0 forms))
         (landing-write (nth 1 forms))
         (call-node (nth 2 forms))
         (call-args (nelisp-phase47-compiler--ir-get call-node :args))
         (landing-arg (nth 3 call-args))
         (saved-sp-arg (nth 4 call-args)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (equal (nelisp-phase47-compiler--ir-get tag-write :bytes)
                   (string-to-list "done")))
    (should (equal (nelisp-phase47-compiler--ir-get landing-write :bytes)
                   (string-to-list "doc129_catch_landing")))
    (should (eq (nelisp-phase47-compiler--ir-get landing-arg :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-kind saved-sp-arg)
                'aot-current-sp))))

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

(ert-deftest nelisp-phase47-doc129/parse-catch-direct-throw ()
  "Doc 129.8L/V: direct catch throw extracts from a labelled landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (throw 'done value)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (push-call
          (car (seq-filter
                (lambda (node)
                  (eq (nelisp-phase47-compiler--ir-get node :name)
                      'nelisp_aot_push_catch))
                (nelisp-phase47-doc129-test--ir-nodes ir 'extern-call))))
         (push-args (nelisp-phase47-compiler--ir-get push-call :args))
         (landing-arg (nth 3 push-args))
         (saved-sp-arg (nth 4 push-args))
         (landing-label
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-landing-label)))
         (machine-jump
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-machine-landing-jump)))
         (landing-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get landing-label :label)))
         (symbol-writes
          (mapcar (lambda (node)
                    (nelisp-phase47-compiler--ir-get node :bytes))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'sexp-write-symbol-lit))))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should (string-prefix-p "aot-catch-landing-" landing-name))
    (should (eq (nelisp-phase47-compiler--ir-get machine-jump :target)
                (nelisp-phase47-compiler--ir-get landing-label :label)))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get machine-jump :saved-sp))
                'aot-current-sp))
    (should (member (string-to-list landing-name) symbol-writes))
    (should (eq (nelisp-phase47-compiler--ir-get landing-arg :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-kind saved-sp-arg)
                'aot-current-sp))))

(ert-deftest nelisp-phase47-doc129/parse-catch-dynamic-throw-descriptor-route ()
  "Doc 129.8AN: direct dynamic catch throw resumes via landing descriptor."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (throw tag value)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (landing-label
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-landing-label)))
         (landing-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get landing-label :label))))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (should (string-prefix-p "aot-catch-landing-" landing-name))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_catch "aot-catch-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-aot-landing-jump ()
  "Doc 129.8S: landing-jump form lowers to the native jump ABI."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun landing_jump
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (landing :type sexp))
                 (aot-landing-jump landing))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (call-node (nth 0 forms))
         (args (nelisp-phase47-compiler--ir-get call-node :args))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-get call-node :name)
                'nelisp_aot_landing_jump))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 2 args) :var)
                'landing))
    (should (member 'nelisp_aot_landing_jump externs))))

(ert-deftest nelisp-phase47-doc129/parse-aot-machine-landing-jump ()
  "Doc 129.8T: machine landing jump names a label and saved SP value."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun machine_landing
                   ((saved_sp :type gp)
                    (value :type gp))
                 (seq
                  (aot-machine-landing-jump saved_sp doc129_landing_pad)
                  (aot-landing-label doc129_landing_pad value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (jump (nth 0 forms))
         (label (nth 1 forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind jump)
                'aot-machine-landing-jump))
    (should (eq (nelisp-phase47-compiler--ir-get jump :target)
                'doc129_landing_pad))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nelisp-phase47-compiler--ir-get jump :saved-sp)
                 :var)
                'saved_sp))
    (should (eq (nelisp-phase47-compiler--ir-kind label)
                'aot-landing-label))
    (should (eq (nelisp-phase47-compiler--ir-get label :label)
                'doc129_landing_pad))))

(ert-deftest nelisp-phase47-doc129/parse-aot-current-sp ()
  "Doc 129.8U: current stack pointer is a value form."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun current_sp
                   ((value :type gp))
                 (seq
                  (aot-current-sp)
                  value))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (current-sp (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind current-sp)
                'aot-current-sp))))

(ert-deftest nelisp-phase47-doc129/parse-catch-conditional-throw ()
  "Doc 129.8N: conditional direct catch throw lowers both branches."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_if_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (if value
                       (throw 'done value)
                     value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (if-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind if-node) 'if))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_catch "aot-catch-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-catch-cond-throw ()
  "Doc 129.8AO: cond catch throw trees reuse the static landing route."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_cond_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (cond
                    (value (throw 'done value))
                    (t value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_catch "aot-catch-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-catch-and-throw ()
  "Doc 129.8AO: simple and catch throw trees lower through if routing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_and_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (and value (throw 'done value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))))

(ert-deftest nelisp-phase47-doc129/parse-catch-nested-throw ()
  "Doc 129.8R: nested catch throw trees dispatch every throwing leaf."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_nested
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (if value
                       (if value
                           (throw 'done value)
                         value)
                     value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (if-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind if-node) 'if))
    (should (= (cl-count 'nelisp_aot_push_catch externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_handler externs) 2))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_catch "aot-catch-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-catch-multi-leaf-throw-labels ()
  "Doc 129.8X: multi-leaf catch trees assign one landing per throw leaf."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun catch_two_throws
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (if value
                       (throw 'done value)
                     (throw 'done value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               2))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_catch "aot-catch-landing-" 2)))

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

(ert-deftest nelisp-phase47-doc129/object-catch-direct-throw ()
  "Doc 129.8L: source `catch' direct throw exposes landing-value reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun catch_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (throw 'done value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "catch_throw" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_value" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-dynamic-throw-descriptor-route ()
  "Doc 129.8AN: direct dynamic catch throw compiles to descriptor route."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-dynamic-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun catch_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (catch 'done
                (throw tag value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "catch_throw" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))
            (should (string-match-p "nelisp_aot_landing_value" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-aot-landing-jump ()
  "Doc 129.8S: object output exposes landing-jump relocation."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-landing-jump-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun landing_jump
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (landing :type sexp))
              (aot-landing-jump landing))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "landing_jump" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-aot-machine-landing-jump ()
  "Doc 129.8T: object output emits stack restore and label jump bytes."
  (skip-unless (executable-find "readelf"))
  (skip-unless (executable-find "objdump"))
  (let ((path (make-temp-file "nelisp-doc129-machine-landing-jump-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun machine_landing
                ((value :type gp))
              (seq
               (aot-machine-landing-jump (aot-current-sp) doc129_landing_pad)
               (aot-landing-label doc129_landing_pad value)))
           path)
          (let ((symbols (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process "readelf" nil t nil
                                           "--wide" "-s" path))))
                (disasm (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "objdump" nil t nil "-d" path)))))
            (should (string-match-p "machine_landing" symbols))
            (should (string-match-p "48 89 e0" disasm))
            (should (string-match-p "49 89 c2" disasm))
            (should (string-match-p "4c 89 d4" disasm))
            (should (string-match-p "\\be9\\b" disasm))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-conditional-throw ()
  "Doc 129.8N: source conditional catch throw exposes both branch relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-if-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun catch_if_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (if value
                    (throw 'done value)
                  value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "catch_if_throw" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_value" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-cond-throw ()
  "Doc 129.8AO: source cond catch throw compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-cond-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun catch_cond_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (cond
                 (value (throw 'done value))
                 (t value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "catch_cond_throw" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_value" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))))
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

(ert-deftest nelisp-phase47-doc129/parse-condition-case-direct-signal ()
  "Doc 129.8M/V: direct condition-case signal uses a labelled landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (signal 'error value)
                   (error err)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (landing-label
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-landing-label)))
         (machine-jump
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-machine-landing-jump)))
         (handler-let (nelisp-phase47-compiler--ir-get landing-label :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (push-call
          (car (seq-filter
                (lambda (node)
                  (eq (nelisp-phase47-compiler--ir-get node :name)
                      'nelisp_aot_push_condition))
                (nelisp-phase47-doc129-test--ir-nodes ir 'extern-call))))
         (push-args (nelisp-phase47-compiler--ir-get push-call :args))
         (landing-arg (nth 3 push-args))
         (saved-sp-arg (nth 4 push-args))
         (landing-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get landing-label :label)))
         (symbol-writes
          (mapcar (lambda (node)
                    (nelisp-phase47-compiler--ir-get node :bytes))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'sexp-write-symbol-lit))))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind landing-label)
                'aot-landing-label))
    (should (eq (nelisp-phase47-compiler--ir-kind handler-let) 'let-rt))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should (eq (nelisp-phase47-compiler--ir-get machine-jump :target)
                (nelisp-phase47-compiler--ir-get landing-label :label)))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get machine-jump :saved-sp))
                'aot-current-sp))
    (should (string-prefix-p "aot-condition-landing-" landing-name))
    (should (member (string-to-list landing-name) symbol-writes))
    (should (eq (nelisp-phase47-compiler--ir-get landing-arg :var)
                'scratch))
    (should (eq (nelisp-phase47-compiler--ir-kind saved-sp-arg)
                'aot-current-sp))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-dynamic-signal-descriptor-route ()
  "Doc 129.8AN: direct dynamic condition signal resumes via descriptor."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (signal tag value)
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (landing-label
          (car (nelisp-phase47-doc129-test--ir-nodes
                ir 'aot-landing-label)))
         (landing-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get landing-label :label))))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (should (string-prefix-p "aot-condition-landing-" landing-name))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_condition "aot-condition-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-direct-signal ()
  "Doc 129.8AE: condition-targeted cleanup jumps to static landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (signal 'error value)
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-condition-landing-" target-name))
    (should (member target-name landing-names))
    (should (cl-some
             (lambda (name)
               (string-prefix-p "aot-unwind-cleanup-" name))
             landing-names))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-conditional-mixed ()
  "Doc 129.8AF: mixed condition cleanup pops normal or jumps to landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (signal 'error value)
                           value)
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-condition-landing-" target-name))
    (should (member target-name landing-names))
    (should (cl-some
             (lambda (name)
               (string-prefix-p "aot-unwind-cleanup-" name))
             landing-names))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-mixed-handled-unhandled ()
  "Doc 129.8AK: condition cleanup mixes static handled and dynamic routes."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (signal 'error value)
                           (signal 'quit value))
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_condition externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_signal externs) 2))
    (should (member 'nelisp_aot_landing_error externs))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-condition-landing-" target-name))
    (should (member target-name landing-names))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-mixed-handled-dynamic ()
  "Doc 129.8AK: dynamic condition leaves resume through the descriptor route."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (signal 'error value)
                           (signal tag value))
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump)))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_condition externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_signal externs) 2))
    (should (member 'nelisp_aot_landing_error externs))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-mixed-condition-normal ()
  "Doc 129.8AK: mixed condition cleanup keeps ordinary leaves."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (signal 'error value)
                           (if value
                               (signal 'quit value)
                             value))
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump)))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 3))
    (should (member 'nelisp_aot_push_condition externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_signal externs) 2))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-multi-handled ()
  "Doc 129.8AL: multiple static condition cleanup leaves route separately."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (signal 'error value)
                           (signal 'quit value))
                       (identity value))
                   (error err)
                   (quit err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :target)))
                  machine-jumps))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_signal externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_error externs) 2))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 2))
    (should (= (length (delete-dups (copy-sequence target-names))) 2))
    (dolist (target-name target-names)
      (should (string-prefix-p "aot-condition-landing-" target-name))
      (should (member target-name landing-names)))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-multi-handled-dynamic ()
  "Doc 129.8AM: multi-handler condition cleanup keeps dynamic leaves dynamic."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (if value
                                 (signal 'error value)
                               (signal 'quit value))
                           (signal tag value))
                       (identity value))
                   (error err)
                   (quit err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :target)))
                  machine-jumps)))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 3))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 3))
    (should (= (cl-count 'nelisp_aot_signal externs) 3))
    (should (= (cl-count 'nelisp_aot_landing_error externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 2))
    (should (= (length (delete-dups (copy-sequence target-names))) 2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-multi-handled-other-normal ()
  "Doc 129.8AM: multi-handler condition cleanup preserves other and normal leaves."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (if value
                             (if value
                                 (signal 'file-error value)
                               (signal 'quit value))
                           (if value
                               (signal 'error value)
                             value))
                       (identity value))
                   (file-error err)
                   (quit err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump)))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 4))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 3))
    (should (= (cl-count 'nelisp_aot_signal externs) 3))
    (should (= (cl-count 'nelisp_aot_landing_error externs) 2))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-conditional-signal ()
  "Doc 129.8Q: conditional condition-case signal dispatches one branch."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (if value
                         (signal 'error value)
                       value)
                   (error err)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (branch (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind branch) 'if))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_condition "aot-condition-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-cond-signal ()
  "Doc 129.8AO: cond condition-case signal trees reuse static routing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_cond_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (cond
                      (value (signal 'error value))
                      (t value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_condition "aot-condition-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-or-signal ()
  "Doc 129.8AO: simple or condition-case signal trees lower through if routing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_or_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (or value (signal 'error value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (member 'nelisp_aot_push_condition externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-nested-signal ()
  "Doc 129.8R: nested condition-case signal trees dispatch throwing leaves."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (if value
                         (if value
                             (signal 'error value)
                           value)
                       value)
                   (error out)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (branch (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind branch) 'if))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 1))
    (should (= (cl-count 'nelisp_aot_pop_handler externs) 2))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               1))
    (nelisp-phase47-doc129-test--assert-single-landing-metadata
     ir 'nelisp_aot_push_condition "aot-condition-landing-")))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-multi-leaf-labels ()
  "Doc 129.8X: multi-leaf condition trees assign one landing per signal leaf."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_two_signals
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (if value
                         (signal 'error value)
                       (error value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (= (cl-count 'nelisp_aot_push_condition externs) 2))
    (should-not (member 'nelisp_aot_pop_handler externs))
    (should (member 'nelisp_aot_landing_error externs))
    (should (= (length (nelisp-phase47-doc129-test--ir-nodes
                        ir 'aot-machine-landing-jump))
               2))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_condition "aot-condition-landing-" 2)))

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

(ert-deftest nelisp-phase47-doc129/object-condition-case-direct-signal ()
  "Doc 129.8M: direct condition-case signal exposes landing-error reloc."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (signal 'error value)
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_signal" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-dynamic-signal-descriptor-route ()
  "Doc 129.8AN: direct dynamic condition signal compiles to descriptor route."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-dynamic-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (condition-case err
                  (signal tag value)
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_signal" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))
            (should (string-match-p "nelisp_aot_landing_error" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-direct-signal ()
  "Doc 129.8AE: static condition-targeted cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (signal 'error value)
                    (identity value))
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-conditional-mixed ()
  "Doc 129.8AF: mixed condition-targeted cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-if-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (if value
                          (signal 'error value)
                        value)
                    (identity value))
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-mixed-handled-unhandled ()
  "Doc 129.8AK: mixed handled/unhandled condition cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-if-unhandled-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (if value
                          (signal 'error value)
                        (signal 'quit value))
                    (identity value))
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-multi-handled ()
  "Doc 129.8AL: multiple static condition cleanup leaves compile to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-if-multi-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (if value
                          (signal 'error value)
                        (signal 'quit value))
                    (identity value))
                (error err)
                (quit err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-multi-handled-dynamic ()
  "Doc 129.8AM: multi-handler condition cleanup with dynamic leaf compiles."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-if-multi-dyn-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (if value
                          (if value
                              (signal 'error value)
                            (signal 'quit value))
                        (signal tag value))
                    (identity value))
                (error err)
                (quit err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-conditional-signal ()
  "Doc 129.8Q: conditional condition-case signal compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-if-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (if value
                      (signal 'error value)
                    value)
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_signal" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-cond-signal ()
  "Doc 129.8AO: source cond condition-case signal compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-cond-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_cond_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (cond
                   (value (signal 'error value))
                   (t value))
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_cond_signal" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))))
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

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-direct-throw ()
  "Doc 129.8Z: source unwind-protect hands off through cleanup landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (throw 'done value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (push-node (nth 0 forms))
         (save-node (nth 1 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind push-node) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind save-node) 'let-rt))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-direct-signal ()
  "Doc 129.8AG: standalone unwind-protect direct signal uses cleanup landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (signal 'error value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (forms (nelisp-phase47-compiler--ir-get body :forms))
         (push-node (nth 0 forms))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (eq (nelisp-phase47-compiler--ir-kind push-node) 'value-seq))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-direct-throw ()
  "Doc 129.8AB: catch-targeted unwind cleanup jumps to static landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (throw 'done value)
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-catch-landing-" target-name))
    (should (member target-name landing-names))
    (should (cl-some
             (lambda (name)
               (string-prefix-p "aot-unwind-cleanup-" name))
             landing-names))))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-conditional-all-throw ()
  "Doc 129.8AC: all-throw unwind branches jump to one catch landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (if value
                           (throw 'done value)
                         (throw 'done value))
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :target)))
                  machine-jumps))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_catch externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (member 'nelisp_aot_landing_value externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 2))
    (should (= (length (delete-dups (copy-sequence target-names))) 1))
    (should (string-prefix-p "aot-catch-landing-" (car target-names)))
    (should (member (car target-names) landing-names))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-conditional-mixed ()
  "Doc 129.8AD: mixed unwind branches pop normal or jump to catch landing."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (if value
                           (throw 'done value)
                         value)
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_catch externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_value externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-catch-landing-" target-name))
    (should (member target-name landing-names))
    (should (cl-some
             (lambda (name)
               (string-prefix-p "aot-unwind-cleanup-" name))
             landing-names))))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-mixed-static-dynamic-throw ()
  "Doc 129.8AJ: catch cleanup can mix static and dynamic throw targets."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (if value
                           (throw 'done value)
                         (throw tag value))
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (target-name
          (symbol-name
           (nelisp-phase47-compiler--ir-get
            (car machine-jumps) :target)))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_catch externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))
    (should (string-prefix-p "aot-catch-landing-" target-name))
    (should (member target-name landing-names))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-mixed-static-other-throw ()
  "Doc 129.8AJ: catch cleanup routes nonmatching quoted throws dynamically."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (if value
                           (throw 'done value)
                         (throw 'other value))
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (member 'nelisp_aot_push_catch externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-mixed-dynamic-normal ()
  "Doc 129.8AJ: catch cleanup keeps ordinary leaves with dynamic throws."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (if value
                           (throw 'done value)
                         (if value
                             (throw tag value)
                           value))
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 3))
    (should (member 'nelisp_aot_push_catch externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (member 'nelisp_aot_pop_handler externs))
    (should (= (length machine-jumps) 1))
    (should-not (member 'nelisp_aot_signal externs))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-cond-throw ()
  "Doc 129.8AP: standalone cleanup accepts cond throw trees."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_cond_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (cond
                      (value (throw 'done value))
                      (t value))
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 1))
    (should (= (cl-count 'nelisp_aot_throw externs) 1))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-or-signal ()
  "Doc 129.8AP: standalone cleanup accepts simple or signal trees."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_or_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (or value (signal 'error value))
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 1))
    (should (= (cl-count 'nelisp_aot_signal externs) 1))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-dynamic-throw ()
  "Doc 129.8AQ: standalone cleanup accepts dynamic throw tags."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_dynamic_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (throw tag value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-dynamic-signal ()
  "Doc 129.8AQ: standalone cleanup accepts dynamic signal tags."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_dynamic_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (signal tag value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'value-seq))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-catch-unwind-protect-cond-mixed ()
  "Doc 129.8AP: catch cleanup cond trees keep static and dynamic routes."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_cond_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (tag :type sexp)
                    (value :type sexp))
                 (catch 'done
                   (unwind-protect
                       (cond
                        (value (throw 'done value))
                        (t (throw tag value)))
                     (identity value))))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_catch externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (member 'nelisp_aot_landing_value externs))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 1))
    (should (= (length machine-jumps) 1))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               2))))

(ert-deftest nelisp-phase47-doc129/parse-condition-case-unwind-protect-cond-signal ()
  "Doc 129.8AP: condition cleanup cond trees keep static condition routes."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun cc_unwind_cond_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (condition-case err
                     (unwind-protect
                         (cond
                          (value (signal 'error value))
                          (t value))
                       (identity value))
                   (error err)))))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir))
         (machine-jumps
          (nelisp-phase47-doc129-test--ir-nodes
           ir 'aot-machine-landing-jump))
         (landing-names
          (mapcar (lambda (node)
                    (symbol-name
                     (nelisp-phase47-compiler--ir-get node :label)))
                  (nelisp-phase47-doc129-test--ir-nodes
                   ir 'aot-landing-label))))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (member 'nelisp_aot_push_condition externs))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 1))
    (should (= (cl-count 'nelisp_aot_signal externs) 1))
    (should (member 'nelisp_aot_landing_error externs))
    (should (member 'nelisp_aot_pop_handler externs))
    (should-not (member 'nelisp_aot_landing_jump externs))
    (should (= (length machine-jumps) 1))
    (should (= (cl-count-if
                (lambda (name)
                  (string-prefix-p "aot-unwind-cleanup-" name))
                landing-names)
               1))))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-conditional-throw ()
  "Doc 129.8P: source unwind-protect conditional throw cleans up both branches."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (throw 'done value)
                       value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-conditional-signal ()
  "Doc 129.8AH: standalone conditional signal cleans up both branches."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_signal
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (signal 'error value)
                       value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (member 'nelisp_aot_builtin_call1 externs))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_signal externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-mixed-nonlocal ()
  "Doc 129.8AI: standalone unwind cleanup can mix throw and signal leaves."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_mixed
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (throw 'done value)
                       (signal 'error value))
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 1))
    (should (= (cl-count 'nelisp_aot_signal externs) 1))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 2)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-mixed-nonlocal-normal ()
  "Doc 129.8AI: mixed standalone cleanup preserves ordinary leaves."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_mixed
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (throw 'done value)
                       (if value
                           (signal 'error value)
                         value))
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 3))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 1))
    (should (= (cl-count 'nelisp_aot_signal externs) 1))
    (should-not (nelisp-phase47-doc129-test--ir-nodes
                 ir 'aot-machine-landing-jump))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 2)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-nested-throw ()
  "Doc 129.8R: nested unwind-protect throw trees clean up every leaf."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_throw
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (if value
                             (throw 'done value)
                           value)
                       value)
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 3))
    (should (member 'nelisp_aot_push_unwind externs))
    (should (member 'nelisp_aot_throw externs))
    (should (member 'nelisp_aot_landing_jump externs))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 1)))

(ert-deftest nelisp-phase47-doc129/parse-unwind-protect-multi-leaf-cleanup-labels ()
  "Doc 129.8Y: multi-leaf unwind-protect trees label every cleanup leaf."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun unwind_two_throws
                   ((out :type sexp)
                    (mirror :type sexp)
                    (frames :type sexp)
                    (scratch :type sexp)
                    (name_slot :type sexp)
                    (value :type sexp))
                 (unwind-protect
                     (if value
                         (throw 'done value)
                       (throw 'done value))
                   (identity value)))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (externs (nelisp-phase47-doc129-test--extern-call-names ir)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'if))
    (should (= (cl-count 'nelisp_aot_builtin_call1 externs) 2))
    (should (= (cl-count 'nelisp_aot_push_unwind externs) 2))
    (should (= (cl-count 'nelisp_aot_throw externs) 2))
    (should (= (cl-count 'nelisp_aot_landing_jump externs) 2))
    (nelisp-phase47-doc129-test--assert-landing-metadata-count
     ir 'nelisp_aot_push_unwind "aot-unwind-cleanup-" 2)))

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

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-direct-throw ()
  "Doc 129.8O: source unwind-protect direct throw compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (throw 'done value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-direct-signal ()
  "Doc 129.8AG: standalone unwind-protect direct signal compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (signal 'error value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-unwind-protect-direct-throw ()
  "Doc 129.8AB: static catch-targeted unwind cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-unwind-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (unwind-protect
                    (throw 'done value)
                  (identity value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-unwind-protect-conditional-all-throw ()
  "Doc 129.8AC: conditional static catch-targeted cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-unwind-if-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (unwind-protect
                    (if value
                        (throw 'done value)
                      (throw 'done value))
                  (identity value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-unwind-protect-conditional-mixed ()
  "Doc 129.8AD: mixed catch-targeted cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-unwind-if-mixed-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (catch 'done
                (unwind-protect
                    (if value
                        (throw 'done value)
                      value)
                  (identity value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_pop_handler" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-unwind-protect-mixed-static-dynamic-throw ()
  "Doc 129.8AJ: mixed static/dynamic catch cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-unwind-if-dynamic-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (catch 'done
                (unwind-protect
                    (if value
                        (throw 'done value)
                      (throw tag value))
                  (identity value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-conditional-throw ()
  "Doc 129.8P: source unwind-protect conditional throw compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-if-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (if value
                      (throw 'done value)
                    value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-conditional-signal ()
  "Doc 129.8AH: standalone conditional signal compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-if-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (if value
                      (signal 'error value)
                    value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-mixed-nonlocal ()
  "Doc 129.8AI: standalone mixed throw/signal cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-mixed-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_mixed
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (if value
                      (throw 'done value)
                    (signal 'error value))
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_mixed" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-cond-throw ()
  "Doc 129.8AP: standalone cond throw cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-cond-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_cond_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (cond
                   (value (throw 'done value))
                   (t value))
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_cond_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-dynamic-throw ()
  "Doc 129.8AQ: standalone dynamic throw cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-dynamic-throw-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_dynamic_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (throw tag value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_dynamic_throw" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-unwind-protect-dynamic-signal ()
  "Doc 129.8AQ: standalone dynamic signal cleanup compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-unwind-dynamic-signal-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_dynamic_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (unwind-protect
                  (signal tag value)
                (identity value)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_dynamic_signal" out))
            (should (string-match-p "nelisp_aot_builtin_call1" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-catch-unwind-protect-cond-mixed ()
  "Doc 129.8AP: catch cleanup cond mixed routes compile to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-catch-unwind-cond-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun unwind_cond_throw
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (tag :type sexp)
                 (value :type sexp))
              (catch 'done
                (unwind-protect
                    (cond
                     (value (throw 'done value))
                     (t (throw tag value)))
                  (identity value))))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "unwind_cond_throw" out))
            (should (string-match-p "nelisp_aot_push_catch" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_throw" out))
            (should (string-match-p "nelisp_aot_landing_jump" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-doc129/object-condition-case-unwind-protect-cond-signal ()
  "Doc 129.8AP: condition cleanup cond signal compiles to object."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc129-cc-unwind-cond-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun cc_unwind_cond_signal
                ((out :type sexp)
                 (mirror :type sexp)
                 (frames :type sexp)
                 (scratch :type sexp)
                 (name_slot :type sexp)
                 (value :type sexp))
              (condition-case err
                  (unwind-protect
                      (cond
                       (value (signal 'error value))
                       (t value))
                    (identity value))
                (error err)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "--wide" "-s" path)))))
            (should (string-match-p "cc_unwind_cond_signal" out))
            (should (string-match-p "nelisp_aot_push_condition" out))
            (should (string-match-p "nelisp_aot_push_unwind" out))
            (should (string-match-p "nelisp_aot_signal" out))
            (should (string-match-p "nelisp_aot_landing_error" out))
            (should-not (string-match-p "nelisp_aot_landing_jump" out))))
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
