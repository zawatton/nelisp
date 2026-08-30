;;; nelisp-standalone-target-test.el --- tests for standalone target selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for standalone target/ABI selection.  These guard the
;; Windows-native path against mixing Win64 object cache entries with the
;; existing Linux/SysV standalone cache.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-standalone-build)
(require 'nelisp-cc-rootstack)
(require 'nelisp-cc-evalport-combiner-apply)
(require 'nelisp-cc-sf-unwind-protect)
(require 'nelisp-cc-env-set-value)
(require 'nelisp-cc-mirror-lookup-entry)
(require 'nelisp-cc-mirror-bucket-prepend)
(require 'nelisp-cc-mirror-set-value)
(require 'nelisp-cc-mirror-set-function)
(require 'nelisp-cc-mirror-clear-value)
(require 'nelisp-cc-mirror-clear-function)
(require 'nelisp-cc-mirror-set-constant)
(require 'nelisp-cc-mirror-install-entry)
(require 'nelisp-cc-mirror-set-value-or-insert)
(require 'nelisp-cc-mirror-set-function-or-insert)
(require 'nelisp-cc-mirror-set-constant-or-insert)
(require 'nelisp-cc-mirror-install-entry-or-insert)

(ert-deftest nelisp-standalone-target-stage3-rootstack-abi-shape ()
  "Doc 152 Stage 3 roots the audited eval/apply GAP slots by handle.
The checks here are deliberately structural: runtime poison/collection and
non-local-exit behaviour are covered by the standalone reader smoke."
  (cl-labels
      ((defun-form
        (name forms)
        (cl-find-if (lambda (form)
                      (and (consp form) (eq (car form) 'defun)
                           (eq (cadr form) name)))
                    (if (eq (car-safe forms) 'seq) (cdr forms) forms)))
       (tree-symbol-p
        (needle tree)
        (cond
         ((eq needle tree) t)
         ((consp tree)
          (or (tree-symbol-p needle (car tree))
              (tree-symbol-p needle (cdr tree))))))
       (tree-member-p
        (needle tree)
        (cond
         ((equal needle tree) t)
         ((consp tree)
          (or (tree-member-p needle (car tree))
              (tree-member-p needle (cdr tree)))))))
    ;; Stage 2's write and mark sides both exist, and the scan applies the
    ;; ordinary tagged-slot marker to every 32-byte entry in [region, top).
    (dolist (name '(nl_root_mark nl_root_reserve nl_root_release
                    nl_thread_registry_add nl_thread_registry_clear
                    nl_gc_mark_rootstack nl_gc_mark_thread_roots
                    nl_thread_park_request_begin
                    nl_thread_park_request_end
                    nl_thread_park_safepoint))
      (should (defun-form name nelisp-cc-rootstack--source)))
    (let ((walk (defun-form 'nl_gc_mark_rootstack_walk
                            nelisp-cc-rootstack--source)))
      (should (tree-member-p '(extern-call nl_gc_mark_slot p) walk))
      (should (tree-symbol-p 'nl_gc_mark_slot walk)))

    ;; The public diagnostic tuple keeps the live registry count at index 16,
    ;; followed by current/last parked, missed, and successful-collect counts.
    (let ((diag (defun-form 'bf_debug_switch
                            nelisp-standalone--applyfn-core-helpers)))
      (should
       (tree-member-p
        '(wf_cons_int (ptr-read-u64 (data-addr nl_thread_registry) 0)
                      s17 s16)
        diag))
      (should (tree-symbol-p 'atomic-fetch-add diag))
      (should (tree-member-p
               '(ptr-read-u64 (data-addr nl_thread_parallel_ctx) 48)
               diag)))

    ;; Doc 199 Tier 3b: the bounded registry stores {env, published-top}
    ;; entries at +16+i*16, rejects worker 65, and reuses the ordinary 32-byte
    ;; rootstack walker for each private [env+4096, top) reserve.  Publication
    ;; is a SeqCst CAS store because the AOT DSL has no separate atomic-store
    ;; primitive; the marker's fetch-add by zero is its SeqCst atomic load.
    (let ((entry (defun-form 'nl_thread_registry_entry
                             nelisp-cc-rootstack--source))
          (add (defun-form 'nl_thread_registry_add_at
                           nelisp-cc-rootstack--source))
          (publish (defun-form 'nl_thread_registry_store_top
                               nelisp-cc-rootstack--source))
          (walk-workers (defun-form 'nl_gc_mark_thread_roots_one
                                    nelisp-cc-rootstack--source))
          (park (defun-form 'nl_thread_park_safepoint_at
                            nelisp-cc-rootstack--source))
          (wait (defun-form 'nl_thread_park_wait_bounded
                            nelisp-cc-rootstack--source))
          (request-wait (defun-form 'nl_thread_park_request_wait
                                    nelisp-cc-rootstack--source)))
      (should (tree-member-p '(>= i 64) add))
      (should (tree-member-p
               '(+ (data-addr nl_thread_registry) (+ 16 (* i 16))) entry))
      (should (tree-symbol-p 'atomic-compare-exchange publish))
      (should (tree-member-p
               '(nl_gc_mark_rootstack_walk (+ env 4096) top)
               walk-workers))
      (should (tree-member-p
               '(extern-call nl_gc_mark_recorded_env env)
               walk-workers))
      (should (tree-symbol-p 'atomic-fetch-add park))
      (should (tree-symbol-p 'while park))
      (should (tree-member-p '(nl_thread_park_request_store 0) request-wait))
      (should (tree-member-p '(nl_thread_park_missed) request-wait))
      (should (tree-member-p '(let ((status 16777216))
                               (seq
                                (while
                                    (if
                                        (<
                                         (atomic-fetch-add
                                          (+ (data-addr nl_thread_parallel_ctx)
                                             32)
                                          0)
                                         expected)
                                        (> status 0)
                                      0)
                                  (setq status (- status 1)))
                                (if
                                    (>=
                                     (atomic-fetch-add
                                      (+ (data-addr nl_thread_parallel_ctx) 32)
                                      0)
                                     expected)
                                    1
                                  0)))
                             wait)))

    ;; 3b/3c/3d: the reserved slot address and saved top are threaded only as
    ;; helper arguments.  No generated helper introduces a runtime let local.
    (let ((rooted-forms
           (append
            (mapcar (lambda (name)
                      (defun-form name nelisp-standalone--arglist-source))
                    '(nl_eval_arg_list_after_rest nl_eval_arg_list_recurse
                      nl_eval_arg_list_after_eval nl_eval_arg_list_with_slot
                      nl_eval_arg_list_with_mark nl_eval_arg_list_dispatch
                      nl_eval_arg_list_walk))
            nelisp-standalone--mxcache-eval-inner-cons-rooted
            nelisp-standalone--reader-do-apply-rooted)))
      (should (tree-symbol-p 'nl_root_reserve rooted-forms))
      (should (tree-symbol-p 'nl_root_release rooted-forms))
      (should-not (tree-symbol-p 'let rooted-forms))
      (should-not (tree-symbol-p 'let* rooted-forms)))

    ;; The two former comparator GAPs are eliminated rather than rooted:
    ;; compare symbol bytes in place, and use immediate-word predicates.
    (let ((bytes (defun-form 'nl_apply_sym_eq_bytes
                             nelisp-cc-evalport-combiner-apply--source)))
      (should bytes)
      (should-not (tree-symbol-p 'alloc-bytes bytes))
      (should-not (tree-symbol-p 'nl_alloc_symbol bytes))
      (should-not (tree-symbol-p 'nelisp_eq_symbol bytes)))
    (should (tree-symbol-p 'nl_apply_sym_eq_w
                           nelisp-cc-evalport-combiner-apply--source))))

(ert-deftest nelisp-standalone-target-thread-mirror-mutation-guard-shape ()
  "Doc 199 workers read the shared mirror but cannot mutate it."
  (cl-labels
      ((defun-form
        (name forms)
        (cl-find-if (lambda (form)
                      (and (consp form) (eq (car form) 'defun)
                           (eq (cadr form) name)))
                    (if (eq (car-safe forms) 'seq) (cdr forms) forms)))
       (tree-symbol-p
        (needle tree)
        (cond
         ((eq needle tree) t)
         ((consp tree)
          (or (tree-symbol-p needle (car tree))
              (tree-symbol-p needle (cdr tree))))))
       (tree-member-p
        (needle tree)
        (cond
         ((equal needle tree) t)
         ((consp tree)
          (or (tree-member-p needle (car tree))
              (tree-member-p needle (cdr tree)))))))
    ;; Option (a) is sound in both layouts: globals_record is env+0, frames are
    ;; env+32, and a worker registers the private region itself as its env.
    (let ((thread-forms (nelisp-standalone--thread-forms)))
      (should (tree-member-p '(nl_thread_copy_words region penv 0 4)
                             thread-forms))
      (should (tree-member-p
               '(nl_thread_private_frames_init
                 penv region type_slot backing_slot depth_slot 0)
               thread-forms))
      (should (tree-member-p '(record-make type_slot 2 (+ env 32))
                             thread-forms))
      (should (tree-member-p '(nl_thread_registry_add region) thread-forms)))

    ;; With no registered worker, the guard is exactly one count load/compare.
    ;; On a non-zero count it uses mirror-ptr directly as the registry env key.
    (let ((guard (defun-form 'nl_thread_mirror_mutation_guard
                             nelisp-cc-rootstack--source))
          (signal (defun-form 'nl_thread_mirror_mutation_signal_at
                              nelisp-cc-rootstack--source))
          (add (defun-form 'nl_thread_registry_add
                           nelisp-cc-rootstack--source)))
      (should (tree-member-p
               '(if (= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0)
                    0
                  (nl_thread_mirror_mutation_guard_registered
                   (nl_thread_registry_find mirror-ptr) name-ptr))
               guard))
      (should (tree-member-p '(nl_thread_mirror_condition_init) add))
      (should (tree-member-p
               '(extern-call nelisp_cons_construct
                             name-ptr nil-slot 268435512)
               signal))
      (should (tree-member-p '(ptr-write-u64 268435472 0 1) signal))
      (should (tree-member-p '(extern-call nl_alloc_symbol
                                           tag-buf 29 268435480)
                             signal)))

    ;; Every public mirror mutation entry point guards before its hit/miss
    ;; implementation.  The unpublished alloc-entry constructor is excluded.
    (dolist (case
             `((nelisp_mirror_set_value
                ,nelisp-cc-mirror-set-value--source)
               (nelisp_mirror_set_function
                ,nelisp-cc-mirror-set-function--source)
               (nelisp_mirror_clear_value
                ,nelisp-cc-mirror-clear-value--source)
               (nelisp_mirror_clear_function
                ,nelisp-cc-mirror-clear-function--source)
               (nelisp_mirror_set_constant
                ,nelisp-cc-mirror-set-constant--source)
               (nelisp_mirror_install_entry
                ,nelisp-cc-mirror-install-entry--source)
               (nelisp_mirror_bucket_prepend
                ,nelisp-cc-mirror-bucket-prepend--source)
               (nelisp_mirror_set_value_or_insert
                ,nelisp-cc-mirror-set-value-or-insert--source)
               (nelisp_mirror_set_function_or_insert
                ,nelisp-cc-mirror-set-function-or-insert--source)
               (nelisp_mirror_set_constant_or_insert
                ,nelisp-cc-mirror-set-constant-or-insert--source)
               (nelisp_mirror_install_entry_or_insert
                ,nelisp-cc-mirror-install-entry-or-insert--source)))
      (let ((form (defun-form (car case) (cadr case))))
        (should form)
        (should (tree-member-p
                 '(extern-call nl_thread_mirror_mutation_guard
                               mirror-ptr sym-ptr)
                 form))
        (should (tree-member-p '(- 0 4) form))))

    ;; `_or_insert' miss paths use the already-admitted internal prepend, so
    ;; the ordinary single-thread path pays only one guard load+compare.
    (dolist (source (list nelisp-cc-mirror-set-value-or-insert--source
                          nelisp-cc-mirror-set-function-or-insert--source
                          nelisp-cc-mirror-set-constant-or-insert--source
                          nelisp-cc-mirror-install-entry-or-insert--source))
      (should (tree-symbol-p 'nelisp_mirror_bucket_prepend_unchecked source)))

    ;; setq turns the mirror's -4 refusal into evaluator rc=1.  The reader's
    ;; condition-case boundary also recovers a stashed worker signal from old
    ;; low-level wrappers that normalise the mutation return to rc=0; the outer
    ;; worker publisher likewise refuses to publish such a call as success.
    (should
     (tree-member-p
      '(if (= set-rv (- 0 4)) 1 0)
      (defun-form 'nelisp_env_setv_mirror_finish
                  nelisp-cc-env-set-value--source)))
    (should (tree-member-p '(nl_thread_registry_find env)
                           nelisp-standalone--reader-errstub-source))
    (should (tree-member-p '(ptr-read-u64 268435472 0)
                           (nelisp-standalone--thread-forms)))
    (should (tree-member-p
             '(extern-call nl_thread_mirror_mutation_guard
                           mirror-ptr alias-sym-ptr)
             nelisp-standalone--sf-defvaralias))
    (should (tree-member-p
             '(extern-call nl_thread_mirror_mutation_guard (+ env 0) a)
             nelisp-standalone--applyfn-bf-arms))

    ;; The read entry point is source-identical in shape: no guard symbol at
    ;; all.  Only mutation sources above depend on the new guard.
    (should-not (tree-symbol-p 'nl_thread_mirror_mutation_guard
                               nelisp-cc-mirror-lookup-entry--source))))

(ert-deftest nelisp-standalone-target-defaults-to-linux-sysv ()
  "The default target remains Linux/SysV for compatibility on every host."
  (should (eq nelisp-standalone--target 'linux-x86_64))
  (should (eq (nelisp-standalone--target-abi 'linux-x86_64) 'sysv)))

(ert-deftest nelisp-standalone-target-windows-uses-win64 ()
  "The Windows-native target maps to the Microsoft x64 ABI."
  (should (eq (nelisp-standalone--target-abi 'windows-x86_64) 'win64)))

(ert-deftest nelisp-standalone-target-macos-uses-aarch64-darwin ()
  "The macOS standalone target maps to arm64/Darwin code generation."
  (should (eq (nelisp-standalone--target-abi 'macos-aarch64) 'aapcs64))
  (should (eq (nelisp-standalone--target-arch 'macos-aarch64) 'aarch64))
  (should (eq (nelisp-standalone--target-os 'macos-aarch64) 'darwin)))

(ert-deftest nelisp-standalone-target-object-name-is-platform-specific ()
  "Windows build logs/cache use .obj names; Linux keeps the historical .o."
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'linux-x86_64)
                 "driver.o"))
  (should (equal (nelisp-standalone--target-object-name
                  "driver.o" 'windows-x86_64)
                 "driver.obj"))
  (should (equal (nelisp-standalone--target-object-name
                  "already.obj" 'windows-x86_64)
                 "already.obj")))

(ert-deftest nelisp-standalone-target-cache-is-target-qualified ()
  "Unit cache paths include the target name to avoid ABI mixing."
  (let ((base (file-name-as-directory nelisp-standalone--cache-dir))
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (string-prefix-p
             base
             (nelisp-standalone--target-cache-dir 'linux-x86_64)))
    (should (string-suffix-p
             "linux-x86_64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'linux-x86_64))))
    (should (string-suffix-p
             "windows-x86_64-arena-70000000"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'windows-x86_64))))
    (should (string-suffix-p
             "macos-aarch64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'macos-aarch64))))))

(ert-deftest nelisp-standalone-target-cache-preserves-section-bytes ()
  "Standalone unit cache stores raw section bytes independent of host coding."
  (let* ((text (unibyte-string #x00 #x7f #x80 #x90 #xe8 #xff))
         (unit (nelisp-link-unit-make
                "probe.o"
                (list (cons 'text text))
                (list (list :name "probe" :section 'text :value 0))
                (list (list :offset 1 :type 'pc32 :symbol "ext"
                            :addend 0 :section 'text))))
         (encoded (nelisp-standalone--unit-cache-encode unit))
         (decoded (nelisp-standalone--unit-cache-decode encoded))
         (decoded-text (cdr (assq 'text (plist-get decoded :sections)))))
    (should (not (multibyte-string-p decoded-text)))
    (should (= (string-bytes decoded-text) (length text)))
    (should (equal decoded-text text))
    (should (equal (plist-get decoded :symbols) (plist-get unit :symbols)))
    (should (equal (plist-get decoded :relocs) (plist-get unit :relocs)))))

(ert-deftest nelisp-standalone-target-rejects-unknown-target ()
  "Unsupported targets fail before producing a mixed-ABI object cache."
  (should-error (nelisp-standalone--target-abi 'plan9-x86_64)
                :type 'error))

(ert-deftest nelisp-standalone-target-windows-output-uses-exe ()
  "Windows-native standalone outputs use a PE-friendly .exe path."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path nil)))
    (should (string-suffix-p ".exe" (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-reader-cli-name-is-short ()
  "The user-facing standalone reader is target/nelisp(.exe)."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t))))
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (string-suffix-p "target/nelisp.exe"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-midform-gc-default-boot-state ()
  "Doc 152 Stage 5 arms mid-form GC only after the boot watermark is valid."
  (let* ((forms (nelisp-standalone--reader-driver-source))
         (driver (cl-find-if (lambda (form)
                               (and (consp form)
                                    (eq (car form) 'defun)
                                    (eq (cadr form) 'driver)))
                             forms))
         (driver-let (nth 3 driver))
         (driver-seq (nth 2 driver-let))
         (body (cdr driver-seq))
         (watermark
          '(ptr-write-u64 268435664 0
                          (+ 268435456 (ptr-read-u64 268435456 0))))
         (watermark-pos (cl-position watermark body :test #'equal)))
    (should driver)
    (should (eq (car driver-let) 'let*))
    (should (eq (car driver-seq) 'seq))
    (should watermark-pos)
    ;; These must remain adjacent and after the watermark: enabling earlier
    ;; permits a while backedge to collect boot objects before the permanent
    ;; generation is frozen.  The trigger reads the live reserved-byte counter;
    ;; a zero BSS trigger would otherwise collect on every backedge.
    (should (equal (nth (+ watermark-pos 1) body)
                   '(ptr-write-u64 (data-addr nl_gc_loop_ctx) 8 1)))
    (should (equal (nth (+ watermark-pos 2) body)
                   '(ptr-write-u64 (data-addr nl_gc_loop_ctx) 40
                                   (+ (ptr-read-u64 268436184 0) 16777216))))
    (should (equal (nth (+ watermark-pos 3) body)
                   '(ptr-write-u64 (data-addr nl_gc_loop_ctx) 32 0)))))

(ert-deftest nelisp-standalone-target-reader-cli-uses-long-options ()
  "The standalone reader exposes Lisp-like no-args REPL plus long options."
  (let* ((forms (nelisp-standalone--reader-driver-source))
         (flat (flatten-tree forms)))
    (cl-labels ((defun-source
                  (name)
                  (prin1-to-string
                   (cl-find-if
                    (lambda (form)
                      (and (consp form)
                           (eq (car form) 'defun)
                           (eq (cadr form) name)))
                    forms)))
                (starts-with-dash-dash-p
                  (name)
                  (let ((source (defun-source name)))
                    (and (string-match-p "(ptr-read-u8 ptr 0) 45" source)
                         (string-match-p "(ptr-read-u8 ptr 1) 45" source)))))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_eval))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_load))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_neln_selftest))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_repl))
      (should (starts-with-dash-dash-p 'nl_cstr_eq_embedded))
      (should (memq 'nl_cstr_eq_help flat))
      (should-not (memq 'nl_cstr_eq_dash_e flat))
      (should-not (memq 'nl_cstr_eq_dash_h flat)))))

(ert-deftest nelisp-standalone-target-reader-cli-dispatches-neln-selftest ()
  "The standalone reader dispatch table includes `--neln-selftest'."
  (let ((source (prin1-to-string (nelisp-standalone--reader-driver-source))))
    (should (string-match-p "nl_cstr_eq_neln_selftest" source))
    (should (string-match-p "(nl_neln_demo_exec ctx 41)" source))))

(ert-deftest nelisp-standalone-target-reader-neln-demo-addresses-real-helpers ()
  "The embedded native demo points each stub at the real runtime symbol.

It used to route through one bridge defun per extern because `addr-of'
only resolves an intra-object label.  `data-addr' records a cross-unit
pc32 the static linker patches against the symbol's section VA, and that
works for a FUNC symbol too -- checked in both directions, the demo
returns 42 through the right symbol and takes SIGSEGV through a wrong
one.  Removing the bridges also removed the one that would have been
awkward to write: a calln forwarder has to redeclare and pass on every
stack argument."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source (prin1-to-string
                  (nelisp-standalone--reader-neln-demo-source))))
    (should (string-match-p "(data-addr nelisp_aot_builtin_call1)" source))
    (should (string-match-p "(data-addr nl_alloc_symbol)" source))
    ;; No forwarders, and above all no local definition of a runtime
    ;; symbol -- that would shadow the one the reader links.
    (should-not (string-match-p "nl_neln_demo_alloc_symbol_bridge" source))
    (should-not (string-match-p "nl_neln_demo_call1_bridge" source))
    (should-not (string-match-p "(defun nelisp_aot_builtin_call1" source))
    (should-not (string-match-p "(defun nl_alloc_symbol" source))))

(ert-deftest nelisp-standalone-target-reader-neln-demo-stubs-clear-the-text ()
  "Stub placement and page sizes follow the artifact, not a fixed offset.

The stubs used to sit at a hardcoded 256 bytes, which held only because
the demo compiled to 122.  Any longer function would have had the stub
it needs overwrite its own code."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (spec (nelisp-standalone--reader-neln-demo-spec))
         (text-length (length (plist-get spec :text-bytes)))
         (stubs (plist-get spec :stub-specs))
         (code-size (plist-get spec :code-size)))
    ;; The demo has to be big enough to have caught the old layout.
    (should (> text-length 256))
    (should stubs)
    (dolist (stub stubs)
      (should (>= (plist-get stub :offset) text-length)))
    ;; Offsets are distinct and the page holds all of them.
    (let ((offsets (mapcar (lambda (s) (plist-get s :offset)) stubs)))
      (should (equal offsets (delete-dups (copy-sequence offsets))))
      (should (<= (+ (apply #'max offsets)
                     nelisp-standalone--reader-neln-stub-bytes)
                  code-size)))
    (should (zerop (% code-size 4096)))))

(ert-deftest nelisp-standalone-target-reader-native-addr-arm-covers-the-set ()
  "The symbol-address arm indexes exactly the bridgeable symbol list.

Interpreted code asks for a runtime symbol's address by index, because
`data-addr' is a compile-time form and the chain of them is fixed when
the reader is built.  Nothing links the index a caller passes to the
order of that chain, so inserting a name in the middle of the list would
silently repoint every later index at a different function -- and a stub
aimed at the wrong function is a jump, not a diagnosable error."
  (let* ((arms (nelisp-standalone--reader-native-addr-arms))
         (arm (car arms))
         (chain (nth 2 (cdr arm)))
         (seen nil))
    (should (equal (mapcar (lambda (a) (cadr (car a))) arms)
                   '("nelisp--native-symbol-addr" "nelisp--native-env")))
    (should (equal (car arm) '(:u8 "nelisp--native-symbol-addr")))
    ;; Walk the if-chain, collecting (INDEX . SYMBOL) in emitted order.
    (while (and (consp chain) (eq (car chain) 'if))
      (let ((test (nth 1 chain))
            (then (nth 2 chain)))
        (should (eq (car test) '=))
        (should (eq (car then) 'data-addr))
        (push (cons (nth 2 test) (symbol-name (nth 1 then))) seen)
        (setq chain (nth 3 chain))))
    ;; The chain ends in 0: an out-of-range index is not an address.
    (should (equal chain 0))
    (setq seen (nreverse seen))
    (should (equal (mapcar #'car seen)
                   (number-sequence
                    0 (1- (length
                           nelisp-standalone--reader-neln-bridgeable-symbols)))))
    (should (equal (mapcar #'cdr seen)
                   nelisp-standalone--reader-neln-bridgeable-symbols))))

(ert-deftest nelisp-standalone-target-reader-native-addr-is-registered ()
  "Every loader arm is reachable: its name is installed as a builtin.
A dispatch arm nothing installs is dead code that still links."
  (dolist (arm (nelisp-standalone--reader-native-addr-arms))
    (should (member (cadr (car arm)) nelisp-standalone--reader-builtins))))

(ert-deftest nelisp-standalone-target-reader-neln-demo-externs-are-bridgeable ()
  "Every extern the demo needs is one the loader can address."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (spec (nelisp-standalone--reader-neln-demo-spec)))
    (dolist (stub (plist-get spec :stub-specs))
      (should (member (plist-get stub :name)
                      nelisp-standalone--reader-neln-bridgeable-symbols)))))

(ert-deftest nelisp-standalone-target-macos-reader-cli-name-is-short ()
  "The macOS user-facing standalone reader is target/nelisp."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (string-suffix-p "target/nelisp"
                             (nelisp-standalone--output-path t)))))

(ert-deftest nelisp-standalone-target-macos-start-is-main ()
  "The macOS eval start unit exports _main and calls driver."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 16))
    (should (= svc-count 1))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-macos-reader-start-uses-native-stack ()
  "The macOS reader start unit switches onto an explicit native stack."
  (let* ((nelisp-standalone--target 'macos-aarch64)
         (unit (nelisp-standalone--target-start-unit t))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs))
         (svc80 (unibyte-string #x01 #x10 #x00 #xd4))
         (svc-count 0)
         (pos 0))
    (should (equal (plist-get unit :name) "start.o"))
    (should (cl-find "_main" (plist-get unit :symbols)
                     :key (lambda (s) (plist-get s :name))
                     :test #'equal))
    (while (string-match (regexp-quote svc80) text pos)
      (setq svc-count (1+ svc-count)
            pos (match-end 0)))
    (should (> (length text) 80))
    (should (= svc-count 2))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find 'b26-pc relocs
                     :key (lambda (r) (plist-get r :type))))))

(ert-deftest nelisp-standalone-target-windows-start-imports-exitprocess ()
  "The Windows start unit calls driver, then KERNEL32!ExitProcess."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (unit (nelisp-standalone--target-start-unit))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (relocs (plist-get unit :relocs)))
    (should (equal (plist-get unit :name) "start.obj"))
    (should (equal (substring text 0 6)
                   (unibyte-string #x48 #x83 #xe4 #xf0 #x48 #x83)))
    (should (equal (substring text 6 10)
                   (unibyte-string #xec #x20 #x31 #xc9)))
    (should (= (aref text 10) #xe8))
    (should (= (aref text 17) #xe8))
    (should (cl-find "driver" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))
    (should (cl-find "ExitProcess" relocs
                     :key (lambda (r) (plist-get r :symbol))
                     :test #'equal))))

(ert-deftest nelisp-standalone-target-windows-reader-uses-wide-file-api ()
  "Windows reader opens files with CreateFileW and UTF-8/UTF-16 conversion."
  (let* ((nelisp-standalone--target 'windows-x86_64)
         (imports (cdr (assoc "KERNEL32.dll"
                              nelisp-standalone--windows-reader-imports)))
         (source-tree (flatten-tree
                       (nelisp-standalone--reader-os-source-forms))))
    (should (member "CreateFileW" imports))
    (should (member "WideCharToMultiByte" imports))
    (should (member "MultiByteToWideChar" imports))
    (should-not (member "CreateFileA" imports))
    (should (memq 'CreateFileW source-tree))
    (should (memq 'WideCharToMultiByte source-tree))
    (should (memq 'MultiByteToWideChar source-tree))
    (should-not (memq 'CreateFileA source-tree))))

(ert-deftest nelisp-standalone-target-macos-reader-uses-darwin-syscalls ()
  "macOS reader file/stdin/stdout helpers use Darwin syscall numbers."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 5 path 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 5 path 1537 420 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 6 fd 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 3 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 4 fd ptr len 0 0 0) forms))
        (should (tree-member-p '(ptr-write-u32 mib 4 49) forms))
        (should (tree-member-p '(syscall-direct 202 mib 3 buf lenp 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 2 path 0 0 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 0 fd ptr len 0 0 0) forms))
        (should-not (tree-member-p '(syscall-direct 1 fd ptr len 0 0 0) forms))))))

(ert-deftest nelisp-standalone-target-reader-installs-process-builtin ()
  "The reader exposes the synchronous process substrate primitive."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-object-p"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-portable-syscall"
                    nelisp-standalone--reader-builtins))
    (should (member "nelisp-process-call-process"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-start"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (member "nelisp-process-async-ready-p"
                    nelisp-standalone--applyfn-bf-builtins))
    (should (tree-member-p
             '((:lit "nelisp-process-call-process") .
               (nl_bi_process_call_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-process-start") .
               (nl_bi_process_start_process args out))
             nelisp-standalone--applyfn-bf-arms))
    (should (tree-member-p
             '((:lit "nelisp-portable-syscall") .
               (wf_write_int out (nl_bi_portable_syscall args)))
             nelisp-standalone--applyfn-bf-arms))
    ;; Stale-literal note (2026-06-10): assert the load-bearing SHAPES of
    ;; nl_bi_process_call_process instead of the full defun literal — the
    ;; exact-form assertion went stale when 41ea76d7 added the M11
    ;; env-inherit branch and broke CI for every later commit.
    (should (tree-member-p
             '(setq envp (ptr-read-u64 268435600 0))
             (nelisp-standalone--fileio-source)))
    (should (tree-member-p
             '(nl_os_process_execve path argv envp)
             (nelisp-standalone--fileio-source)))
    (should (tree-member-p
             '(wf_write_int out (nl_bi_process_wait_exit_code pid))
             (nelisp-standalone--fileio-source)))
    (should (tree-member-p
             '(defun nl_bi_process_make_object (pid outfd infd out)
                (seq
                 (vector-make 6 out)
                 (nl_bi_process_set_int out 0 1886547811)
                 (nl_bi_process_set_int out 1 pid)
                 (nl_bi_process_set_int out 2 outfd)
                 (nl_bi_process_set_int out 3 0)
                 (nl_bi_process_set_int out 4 -1)
                 (nl_bi_process_set_int out 5 infd)
                 0))
             (nelisp-standalone--fileio-source)))
    ;; Same stale-literal note as call-process above: assert the
    ;; load-bearing shapes, not the full defun (the 41ea76d7 env-inherit
    ;; branch invalidated the old exact literal).
    (should (tree-member-p
             '(setq pipe_rc (nl_os_process_pipe pipev))
             (nelisp-standalone--fileio-source)))
    (should (tree-member-p
             '(nl_os_process_set_nonblock readfd)
             (nelisp-standalone--fileio-source)))
    (should (tree-member-p
             '(nl_bi_process_make_object pid readfd stdin_writefd out)
             (nelisp-standalone--fileio-source)))))

(ert-deftest nelisp-standalone-target-reader-process-syscalls-are-targeted ()
  "Process helper syscall numbers stay target-specific."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 57 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 61 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 33 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 22 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 72 fd 4 2048 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 62 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 60 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(syscall-direct 2 0 0 0 0 0 0) forms))
        (should (tree-member-p '(syscall-direct 59 path argv envp 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 7 pid statusp options 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 90 oldfd newfd 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 42 pipev 0 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 92 fd 4 4 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 37 pid sig 0 0 0 0)
                               forms))
        (should (tree-member-p '(syscall-direct 1 127 0 0 0 0 0)
                               forms))))
    (let ((nelisp-standalone--target 'windows-x86_64))
      (let ((forms (nelisp-standalone--reader-os-source-forms)))
        (should (tree-member-p '(defun nl_os_process_fork nil -1) forms))
        (should (tree-member-p '(defun nl_os_process_wait4
                                  (pid statusp options) -1)
                               forms))
        (should (tree-member-p '(defun nl_os_process_pipe (pipev) -1)
                               forms))))))

(ert-deftest nelisp-standalone-target-reader-detects-shifted-argv ()
  "The reader driver can detect and normalize macOS LC_MAIN argv+1."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((forms (nelisp-standalone--reader-driver-source)))
        (should (tree-member-p
                 '(defun nl_cli_argv_shifted_p (argc slot0 slot1)
                    (if (> argc 1)
                        (if (= slot1 0)
                            1
                          (nl_cli_command_p slot0))
                      0))
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 16 slot0)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 24 slot1)
                 forms))
        (should (tree-member-p
                 '(ptr-write-u64 sp0 32 slot2)
                 forms))))))

(ert-deftest nelisp-standalone-target-reader-repl-prelude-avoids-stack-literal ()
  "REPL prelude is copied through the arena buffer, not a huge stack literal."
  (let* ((forms (nelisp-standalone--reader-repl-prelude-forms
                 'fbuf 'src 'cursor 'result 'pool 'out 'ctx 'builtin_sym))
         (flat (flatten-tree forms))
         (copy-def (nelisp-standalone--copy-lit-u64-defun 'probe "abcdefghi"))
         (copy-flat (flatten-tree copy-def))
         (chunk-defs (nelisp-standalone--copy-lit-u64-defuns
                      'big-probe "abcdefghijklmnopqr" 8))
         (chunk-flat (flatten-tree chunk-defs)))
    (should (memq 'nl_repl_prelude_source flat))
    (should (memq 'nl_alloc_str flat))
    (should-not (memq 'sexp-write-str-lit flat))
    (should (memq 'ptr-write-u64 copy-flat))
    (should (memq 'ptr-write-u8 copy-flat))
    (should (memq 'big-probe chunk-flat))
    (should (memq 'big-probe_chunk_000 chunk-flat))
    (should (memq 'big-probe_chunk_001 chunk-flat))
    (should (memq 'big-probe_chunk_002 chunk-flat))))

(ert-deftest nelisp-standalone-target-reader-load-uses-direct-source-printer ()
  "`--load' evaluates raw file source and prints the resulting value."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source)))
      (should (tree-member-p
               '(nl_alloc_str fbuf n src)
               forms))
      (should (tree-member-p
               '(nl_cli_write_value fbuf out)
               forms))
      (should (tree-member-p
               '(defun nl_cli_value_to_buf (fbuf off out)
                  (let* ((tag (ptr-read-u64 out 0)))
                    (cond
                     ((= tag 0) (nl_cli_put_nil fbuf off))
                     ((= tag 1) (nl_cli_put_byte fbuf off 116))
                     ((= tag 2) (nl_cli_put_dec fbuf off (ptr-read-u64 out 8)))
                     ((= tag 4) (nl_cli_put_string_value fbuf off out 0))
                     ((= tag 5) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 6) (nl_cli_put_string_value fbuf off out 1))
                     ((= tag 14) (nl_cli_put_unibyte_string_value fbuf off out))
                     ((= tag 15) (nl_cli_put_unibyte_string_value fbuf off out))
                     ((= tag 7) (nl_cli_put_list_tail fbuf
                                                       (nl_cli_put_byte fbuf off 40)
                                                       out 1))
                     ((= tag 8) (nl_cli_put_vector_loop fbuf
                                                        (nl_cli_put_byte fbuf off 91)
                                                        out 0 (vector-len out)))
                     (t (nl_cli_put_object fbuf off)))))
               forms))
      (should-not (tree-member-p
                   '(nl_cli_wrap_source_at fbuf (+ off n) src)
                   forms)))))

(ert-deftest nelisp-standalone-target-unwind-cleanup-errors-propagate ()
  "Cleanup uses stashing eval; body nonlocal exits preserve the M6 kind flag."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms nelisp-cc-sf-unwind-protect--source))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup (car cdr body-rc env out _pad6)
                  (if (= body-rc 0)
                      (let* ((scratch (alloc-bytes 32 8)))
                        (nl_sf_uw_do_cleanup_preserve scratch car cdr body-rc env out))
                    (let* ((flag-save (ptr-read-u64 268435472 0))
                           (tag-save (alloc-bytes 32 8))
                           (val-save (alloc-bytes 32 8)))
                      (seq
                       (nl_sexp_clone_into 268435480 tag-save)
                       (nl_sexp_clone_into 268435512 val-save)
                       (nl_sf_uw_do_cleanup_body_exit
                        flag-save tag-save val-save car cdr body-rc env out)))))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup_preserve (scratch car cdr body-rc env out)
                  (nl_sf_uw_cleanup_evaled
                   (extern-call nelisp_eval_call car env scratch)
                   cdr body-rc env out 0))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_cleanup_evaled (cleanup-rc cdr body-rc env out _pad6)
                  (if (= cleanup-rc 0)
                      (nl_sf_uw_cleanup cdr body-rc env out 0 0)
                    cleanup-rc))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_cleanup_after_body_exit
                    (cleanup-rc flag-save tag-save val-save cdr body-rc env out)
                  (if (= cleanup-rc 0)
                      (seq
                       (nl_sexp_clone_into tag-save 268435480)
                       (nl_sexp_clone_into val-save 268435512)
                       (ptr-write-u64 268435472 0 flag-save)
                       (dealloc-bytes tag-save 32 8)
                       (dealloc-bytes val-save 32 8)
                       (nl_sf_uw_cleanup cdr body-rc env out 0 0))
                    (seq
                     (dealloc-bytes tag-save 32 8)
                     (dealloc-bytes val-save 32 8)
                     cleanup-rc)))
               forms))
      (should (tree-member-p
               '(defun nl_sf_uw_do_cleanup_body_exit
                    (flag-save tag-save val-save car cdr body-rc env out)
                  (let* ((scratch (alloc-bytes 32 8)))
                    (nl_sf_uw_cleanup_after_body_exit
                     (extern-call nelisp_eval_call car env scratch)
                     flag-save tag-save val-save cdr body-rc env out)))
               forms)))))

(ert-deftest nelisp-standalone-target-reader-boundary-reclaim-is-conservative ()
  "Doc 140 Stage 5 reclaims only safe immediate non-mutating boundaries."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((forms (nelisp-standalone--reader-driver-source))
          (boundary nelisp-standalone--reader-boundary-source)
          (eval-source nelisp-standalone--reader-eval-source-source))
      (should (tree-member-p
               '(defun nl_boundary_immediate_result_p (out)
                  (if (<= (ptr-read-u64 out 0) 3) 1 0))
               boundary))
      (should (tree-member-p
               '(if (= (ptr-read-u64 268436216 0) 1)
                    (if (= (nl_boundary_immediate_result_p out) 1)
                        (if (= (ptr-read-u64 268435544 0) epoch0)
                            (if (= (ptr-read-u64 268435472 0) 0)
                                (if (= (ptr-read-u64 268435464 0) 0)
                                    (nl_boundary_reclaim mark_chunk mark_cursor)
                                  0)
                              0)
                          0)
                      0)
                  0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 0)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268436216 0 1)
               forms))
      (should (tree-member-p
               '(ptr-write-u64 268435552 0 0)
               boundary))
      (should (tree-member-p
               '(ptr-write-u64 268436168 0 mark_chunk)
               boundary))
      (should (tree-member-p
               '(nl_boundary_maybe_reclaim mark_chunk mark_cursor epoch0 out)
               eval-source)))))

(ert-deftest nelisp-standalone-target-reader-repl-suffix-uses-runtime-base ()
  "REPL runtime wrapper must read the quit slot via the live RUNTIME arena base
on EVERY target (Doc 140 Stage 8), never a baked fixed arena-base immediate.
The runtime-PARSED suffix cannot use `data-addr', and the pre-Stage-8
windows/macos fixed bases (e.g. #x70000000 + 8) point at unmapped VA after the
rebase -> SIGSEGV in the REPL print path's quit-flag check."
  (dolist (target '(macos-aarch64 windows-x86_64 linux-x86_64))
    (let* ((nelisp-standalone--target target)
           (suffix (nelisp-standalone--reader-repl-eval-suffix)))
      ;; Reads the quit flag via the live runtime base.
      (should (string-match-p
               (regexp-quote "(ptr-read-u64 (+ (car (nelisp--arena-stats)) 8) 0)")
               suffix))
      ;; Embeds NO fixed arena-base metadata immediate for any target.
      (should-not (string-match-p
                   (number-to-string
                    (nelisp-standalone--target-arena-metadata-address 8))
                   suffix)))))

(ert-deftest nelisp-standalone-target-windows-arena-init-uses-null-virtualalloc ()
  "Windows chunk-0 init uses VirtualAlloc(NULL, ...) and stores `nl_arena_base'."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc base 4096 4096 4)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (+ base 704) 0 (+ base 768))
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc #x70000000 #x4000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-arena-reserves-64m ()
  "Windows chunk-0 keeps a bounded 64 MiB reservation without committing it up front."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x4000000)
                 arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 268435456 #x10000000 12288 4)
                     arena))
        (should-not (tree-member-p
                     '(extern-call VirtualAlloc 0 #x40000000 12288 4)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-stage8-rewrites-arena-slots ()
  "Windows Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--windows-arena-base #x70000000))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-linux-arena-uses-anonymous-mmap ()
  "Doc 140 Stage 8: linux reserves chunk 0 with mmap(NULL) — no fixed base.
The kernel-chosen base is stored in the driver-owned `nl_arena_base' bss slot
and reached at run time through it; there is no MAP_FIXED / MAP_FIXED_NOREPLACE
reservation at 0x10000000 left in the normal runtime path."
  (let ((nelisp-standalone--target 'linux-x86_64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        ;; chunk 0 is reserved through the anonymous chunk allocator.
        (should (tree-member-p '(nl_os_alloc_chunk #x10000000) arena))
        ;; its kernel-chosen base is stored in the driver-owned bss slot.
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base) arena))
        ;; `nl_os_alloc_chunk' uses MAP_PRIVATE|MAP_ANONYMOUS mmap at NULL.
        (should (tree-member-p '(syscall-direct 9 0 size 3 34 -1 0) arena))
        ;; the OOM path still exits cleanly.
        (should (tree-member-p '(syscall-direct 60 88 0 0 0 0 0) arena))
        ;; NO fixed-base reservation remains in the normal runtime path.
        (should-not (tree-member-p
                     '(syscall-direct 9 #x10000000 #x10000000 3 #x100022 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-linux-arena-size-stays-pressure-visible ()
  "Linux must not hide arena pressure by growing the fixed virtual reservation.
Doc 140 Stage 7: the fixed first chunk is 256 MiB (=#x10000000=), not the
historical 8 GiB — pressure beyond it is handled by chunk growth."
  (should (= (nelisp-standalone--target-arena-size 'linux-x86_64)
             #x10000000)))

(ert-deftest nelisp-standalone-target-arena-size-slot-is-initialized ()
  "All native standalone targets expose reservation size through arena metadata."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; Doc 140 Stage 8: linux seeds the reservation-size slot relative to the
    ;; runtime mmap base (`(+ base 216)') rather than a fixed immediate.
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x10000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x4000000)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(ptr-write-u64 (+ base 216) 0 #x20000000)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-registers-first-chunk ()
  "Registers chunk 0's descriptor + control slots at init.
Doc 140 Stage 8 (linux): the writes are relative to the runtime mmap base
(`(+ base OFF)') instead of a fixed immediate — chunk 0 is no longer pinned at
0x10000000.  windows/macos now follow the same runtime-base scheme through the
shared `nl_arena_base' slot."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 base 0 #x400) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 712) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 768) 0 base) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 776) 0 #x10000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 816) 0 0) arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x4000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(ptr-write-u64 (+ base 704) 0 (+ base 768)) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 720) 0 1) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 728) 0 #x20000000) arena))
        (should (tree-member-p '(ptr-write-u64 (+ base 792) 0 (+ base #x400)) arena))))))

(ert-deftest nelisp-standalone-target-stage8-arena-base-slot-unit ()
  "Doc 140 Stage 8: chunked native targets export driver-owned bss globals.
Windows uses the target-correct `.obj' unit name; linux/macOS keep `.o'."
  (dolist (case '((linux-x86_64 "arena-base.o")
                  (windows-x86_64 "arena-base.obj")
                  (macos-aarch64 "arena-base.o")))
    (pcase-let ((`(,target ,name) case))
      (let* ((nelisp-standalone--target target)
             (u (nelisp-standalone--arena-base-slot-unit))
             (syms (plist-get u :symbols))
             (by-name (mapcar (lambda (sym)
                                (cons (plist-get sym :name) sym))
                              syms)))
        (should (equal name (plist-get u :name)))
        ;; Doc 152 Stage 3 enlarged the root-stack region 1 MiB -> 4 MiB
        ;; (32768 -> 131072 slots) so rooting's root-depth 3N+6 per non-tail
        ;; recursion stays clear of rec_max; every symbol after the region
        ;; shifts with it, so these are written as (+ OFFSET 4194304).
        (dolist (expected (list (cons "nl_arena_base" 0)
                                (cons "nl_rootstack_top" 8)
                                (cons "nl_rootstack_region" 16)
                                (cons "nl_gc_diag" (+ 16 4194304))
                                (cons "nl_gc_loop_ctx" (+ 80 4194304))
                                (cons "nl_fa_tbl_base" (+ 57488 4194304))
                                (cons "nl_thread_parallel_ctx" (+ 57888 4194304))
                                (cons "nl_alloc_diag" (+ 57952 4194304))
                                (cons "nl_gc_reclaim_scratch" (+ 58008 4194304))
                                (cons "nl_thread_registry" (+ 58048 4194304))))
          (let ((sym (cdr (assoc (car expected) by-name))))
            (should sym)
            (should (equal (cdr expected) (plist-get sym :value)))
            (should (eq 'bss (plist-get sym :section)))))
        ;; Doc 170 Stage 2: +96 bytes for the `nl_alloc_check' checked-
        ;; allocator control block appended after `nl_fvcache_*'.  Doc 180
        ;; Phase 2 item 3 (2026-08-23): +176 more bytes for `nl_bt_snapshot'
        ;; (the bounded backtrace capture buffer) appended after that.  Doc 199
        ;; Tier 3a/Tier 3b append 64 bytes of bounded section + park state. Tier 3b
        ;; appends the 1040-byte registry (16-byte header + 64*16 entries).
        (should (equal (+ 57616 4194304 96 176 64 56 40 1040)
                       (cdr (assq 'bss (plist-get u :sections)))))))))

(ert-deftest nelisp-standalone-target-stage8-build-appends-arena-base-slot-unit ()
  "Doc 140 Stage 8: standalone link units append the `nl_arena_base' slot unit."
  (dolist (target '(linux-x86_64 windows-x86_64 macos-aarch64))
    (let ((nelisp-standalone--target target)
          (nelisp-standalone--manifest '(("probe.o" :helper nil)))
          captured)
      (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
                 (lambda (_entry)
                   (nelisp-link-unit-make "probe.o" nil nil nil)))
                ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
                 (lambda ()
                   (nelisp-link-unit-make
                    (nelisp-standalone--target-object-name "arena-base.o")
                    (list (cons 'bss 8)) nil nil)))
                ;; Fixed 2026-08-24 (integration/wave6 full-battery run,
                ;; standalone-eval-test's bounded-backtrace dependency
                ;; fix): `nelisp-standalone-build' now ALSO appends
                ;; `nelisp-standalone--eval-extra-manifest''s units
                ;; (mapped through the already-mocked `unit-for' above,
                ;; harmlessly producing more fake "probe.o" units) and
                ;; one real `nelisp-standalone--compile-to-unit' call
                ;; for "bt-extra-eval.o" -- mocked here too, so this
                ;; test still exercises only the arena-base-slot-unit
                ;; behavior it names, not a real AOT compile of
                ;; unrelated new source.
                ((symbol-function 'nelisp-standalone--compile-to-unit)
                 (lambda (name &optional _source _abi)
                   (nelisp-link-unit-make name nil nil nil)))
                ((symbol-function 'nelisp-standalone--output-path)
                 (lambda (&optional _reader-p) "/tmp/nelisp-target-test"))
                ((symbol-function 'nelisp-link-units)
                 (lambda (_out units &rest _)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-pe32)
                 (lambda (_out units _entry _imports &optional _opts)
                   (setq captured units)))
                ((symbol-function 'nelisp-link-units-macho-exec)
                 (lambda (_out units _entry _arch)
                   (setq captured units)))
                ((symbol-function 'set-file-modes)
                 (lambda (&rest _) nil))
                ((symbol-function 'nelisp-standalone--codesign-macos-adhoc)
                 (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (nelisp-standalone-build)
        (should captured)
        ;; Membership, not tail position: fixed 2026-08-24 (integration/
        ;; wave6 full-battery run) -- `nelisp-standalone-build' now
        ;; appends `nelisp-standalone--eval-extra-manifest''s units and
        ;; "bt-extra-eval.o" AFTER the arena-base-slot-unit append this
        ;; test names, so arena-base.o is no longer the last element,
        ;; only a member. The feature this test verifies (the slot unit
        ;; gets appended at all, once per dynamic-arena-base target) is
        ;; unaffected by where in the list it ends up.
        (should (cl-find (nelisp-standalone--target-object-name "arena-base.o")
                          captured
                          :key (lambda (u) (plist-get u :name))
                          :test #'equal))))))

(ert-deftest nelisp-standalone-target-stage8-chunk-arena-rewrite-cross-platform ()
  "Doc 140 Stage 8: chunk-arena rewrite fires for linux, windows, and macOS.
It leaves the base-establishing `nl_arena_init' untouched while rewriting
rebased fixed metadata immediates to `nl_arena_base' loads + offsets."
  ;; linux: free-list-head (arena-base + 96) -> runtime base load + 96.
  (let ((nelisp-standalone--target 'linux-x86_64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun f () (ptr-read-u64 268435552 0)))
             '(defun f ()
                (ptr-read-u64
                 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0))))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))
             '(defun nl_arena_init () (nl_os_alloc_chunk 268435456)))))
  ;; windows/macOS: rebase first, then rewrite the target-relative immediates.
  (dolist (case '((windows-x86_64 #x70000000)
                  (macos-aarch64 #x800000000)))
    (pcase-let ((`(,target ,base) case))
      (let ((nelisp-standalone--target target))
        (should (equal
                 (nelisp-standalone--chunk-arena-rewrite
                  (nelisp-standalone--rebase-arena-source
                   '(defun f ()
                      (seq (ptr-read-u64 268435552 0)
                           (ptr-read-u64 268435456 0)))))
                 '(defun f ()
                    (seq
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 96) 0)
                     (ptr-read-u64
                      (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0) 0)))))
        (should (equal
                 (nelisp-standalone--rebase-arena-source
                  '(defun f () (ptr-read-u64 268435552 0)))
                 `(defun f () (ptr-read-u64 ,(+ base 96) 0))))))))

(ert-deftest nelisp-standalone-target-stage6-generation-split ()
  "Doc 140 Stage 6: chunk 0 (boot generation) is tagged persistent and the
top-level boundary reclaimer skips persistent chunks — only temporary per-form
scratch chunks have their cursor reset."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; the reclaimer gates the reset on the persistent flag bit.
    (should (tree-member-p '(logand flags 2)
                           nelisp-standalone--reader-boundary-source))
    ;; chunk-0 init writes desc.flags = (logior 1 persistent) = 3 on every
    ;; dynamic-base chunk-0 init path.
    (let ((dyn (nelisp-standalone--arena-init-metadata-forms-dynamic 'base 256)))
      (should (tree-member-p
               (list 'ptr-write-u64
                     (list '+ 'base (+ nelisp-standalone--arena-chunk0-desc-offset
                                       nelisp-standalone--arena-chunk-desc-flags-offset))
                     0 3)
               dyn)))
    (should (= 3 (logior 1 nelisp-standalone--arena-chunk-flag-persistent)))))

(ert-deftest nelisp-standalone-target-gc-walks-chunk-descriptors ()
  "GC membership uses the current-chunk fast path and chunk-list fallback."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree nelisp-standalone--gc-source)))
      (should (memq 'nl_gc_chunk_contains_any flat))
      (should (memq 'nl_gc_sweep_chunks flat))
      (should (member 268436160 flat))
      ;; `nl_gc_in_arena' probes the current chunk before walking the list.
      ;; This superseded the old recursive helper-only implementation.
      (should (tree-member-p '(ptr-read-u64 268436168 0)
                             nelisp-standalone--gc-source))
      (should (tree-member-p '(setq chunk (ptr-read-u64 (+ chunk 48) 0))
                             nelisp-standalone--gc-source))
      ;; Doc 152 Stage 5 made tag-9 reader char tables reachable by mid-form
      ;; collection.  Pin the complete owned-edge walk: two inline Sexps,
      ;; 40-byte entry rows, the parent box, 32-byte extra slots, and the
      ;; analogous raw data buffer owned by tag-10 bool vectors.
      (should (tree-member-p
               '(nl_gc_mark_char_table_box (ptr-read-u64 sp 8))
               nelisp-standalone--gc-source))
      (should (tree-member-p '(nl_gc_mark_slot (+ box 32))
                             nelisp-standalone--gc-source))
      (should (tree-member-p
               '(nl_gc_mark_char_table_slots entries 0 entries_len 40 8)
               nelisp-standalone--gc-source))
      (should (tree-member-p
               '(if (= parent 0) 0 (nl_gc_mark_char_table_box parent))
               nelisp-standalone--gc-source))
      (should (tree-member-p
               '(nl_gc_mark_char_table_slots extra 0 extra_len 32 0)
               nelisp-standalone--gc-source))
      (should (tree-member-p
               '(nl_gc_mark_bool_vector_box (ptr-read-u64 sp 8))
               nelisp-standalone--gc-source))
      (should-not
       (tree-member-p
        '(defun nl_gc_sweep
             nil
           (let ((hdr (ptr-read-u64 268435568 0))
                 (end (+ 268435456 (ptr-read-u64 268435456 0))))
             (while (and (> hdr 0) (< hdr end))
               (setq hdr (nl_gc_sweep_step hdr end)))
             0))
        nelisp-standalone--gc-source)))))

(ert-deftest nelisp-standalone-target-gc-slot-walks-are-bounded-by-the-block ()
  "Sexp-slot walkers clamp LEN to what the buffer's own block can hold.

Measured 2026-08-29: an uninitialised reader parse-pool slot presented as
tag 9, so `nl_gc_mark_char_table_box' read `entries_len' out of a block
that was not a char-table box and got back 0x7fffacd2d6f0 -- a pointer.
`nl_gc_mark_char_table_slots' believed it and walked 31,478,841 slots,
1.26 GB, past the end of the arena, dying in `nl_gc_mark_slot' on an
address 0x18 beyond the last mapped byte.  The allocator block header is
the authority on how many elements a buffer holds, and a live Vec never
has len > cap, so clamping to it cannot truncate a real object."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    ;; The shared capacity helper: payload bytes / STRIDE, refusing a block
    ;; whose claimed end is not itself in the arena.
    (should (tree-member-p
             '(defun nl_gc_block_elem_cap (ptr stride)
                (let ((bt (nl_hdr_bt (- ptr 8))))
                  (if (< bt 16) 0
                    (if (= (nl_gc_in_arena (+ ptr (- bt 9))) 0) 0
                      (/ (- bt 8) stride)))))
             nelisp-standalone--gc-source))
    ;; Both slot walkers take the clamp.
    (should (tree-member-p '(cap (nl_gc_block_elem_cap base stride))
                           nelisp-standalone--gc-source))
    (should (tree-member-p '(cap (nl_gc_block_elem_cap data_ptr 8))
                           nelisp-standalone--gc-source))
    (should (tree-member-p '(n (if (< len cap) len cap))
                           nelisp-standalone--gc-source))
    ;; A tag-9 box is only read at char-table offsets when the block is
    ;; actually big enough to be one (`nl_char_table_alloc' takes 128 bytes,
    ;; so BLOCK_TOTAL is 136).
    (should (tree-member-p '(< (nl_hdr_bt (- box 8)) 136)
                           nelisp-standalone--gc-source))
    ;; And the unbounded shapes are gone.  These two `should-not's fail if
    ;; either walker goes back to trusting LEN.
    (should-not (tree-member-p
                 '(defun nl_gc_mark_char_table_slots (base i len stride off)
                    (if (= (nl_gc_in_arena base) 0) 0
                      (let ((k i))
                        (while (< k len)
                          (nl_seq2 (nl_gc_mark_slot (+ base (+ off (* k stride))))
                                   (setq k (+ k 1))))
                        0)))
                 nelisp-standalone--gc-source))
    (should-not (tree-member-p
                 '(defun nl_gc_mark_vec_slots (data_ptr i len)
                    (if (= (nl_gc_in_arena data_ptr) 0) 0
                      (let ((k i))
                        (while (< k len)
                          (nl_seq2
                           (let ((vw (ptr-read-u64 (+ data_ptr (* k 8)) 0)))
                             (if (= (logand vw 1) 1) 0
                               (if (= (nl_gc_mark_block vw) 0) 0
                                 (nl_gc_mark_slot vw))))
                           (setq k (+ k 1))))
                        0)))
                 nelisp-standalone--gc-source))))

(ert-deftest nelisp-standalone-target-recorded-pool-uses-its-own-cap ()
  "A recorded frame's parse pool is walked with ITS cap, not the global word.

The cap word @268436448 names the pool of the load running now.  Nested
loads size their pools from their own source length, so an outer frame's
pool was walked with an inner load's cap: too large and the walk ran into
unrelated live blocks (`nl_fa_pool' REWRITES pointer edges through the
same shape), too small and a live pool's tail went unmarked.  The
2026-06-11 nested-cap restore fixed only the after-return face of this.
Measured 2026-08-29 over ten process layouts: 0/10 runs of anvil's
standalone MCP fast handshake completed before this change, 20/20 after."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (should (tree-member-p
             '(defun nl_gc_pool_cap_of (pool)
                (if (= pool 0) 0
                  (let ((c (nl_gc_block_elem_cap pool 32)))
                    (if (= c 0) (nl_gc_pool_cap) c))))
             nelisp-standalone--gc-source))
    ;; Cap 0 stays the form-boundary "stale slots are not roots" mode flag, and
    ;; `nl_gc_diag'+56 suppresses the slot walk for one mark pass -- the switch
    ;; `precise-root-coverage' uses to run the workload without this arm.
    (should (tree-member-p
             '(defun nl_gc_mark_recorded_pool (pool)
                (if (= pool 0) 0
                  (nl_seq2 (nl_gc_mark_block pool)
                           (if (= (ptr-read-u64 (data-addr nl_gc_diag) 56) 1) 0
                             (if (= (nl_gc_pool_cap) 0) 0
                               (nl_gc_mark_pool pool (nl_gc_pool_cap_of pool)))))))
             nelisp-standalone--gc-source))
    ;; The rewriting arm takes the same per-frame cap.
    (should (tree-member-p '(nl_gc_pool_cap_of (ptr-read-u64 base 24))
                           nelisp-standalone--applyfn-fa-file-helpers))
    (should-not (tree-member-p
                 '(defun nl_gc_mark_recorded_pool (pool)
                    (if (= pool 0) 0
                      (nl_seq2 (nl_gc_mark_block pool)
                               (nl_gc_mark_pool pool (nl_gc_pool_cap)))))
                 nelisp-standalone--gc-source))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-allocator ()
  "Doc 140 Stage 4 adds target-specific non-fixed chunk allocation."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 9 0 size 3 34 -1 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 size 8192 4)
                 arena))
        (should (tree-member-p
                 '(nl_os_commit_range base old new)
                 arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (should (tree-member-p
               '(syscall-direct 197 0 size 3 4098 -1 0)
               (nelisp-standalone--target-arena-source))))))

(ert-deftest nelisp-standalone-target-arena-adds-target-chunk-reclaimer ()
  "Growth chunk reclamation is wired on every standalone native target."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (should (tree-member-p
               '(syscall-direct 11 base size 0 0 0 0)
               (nelisp-standalone--target-arena-source))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(extern-call VirtualFree base 0 32768)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(syscall-direct 73 base size 0 0 0 0)
                 arena))
        (should-not (tree-member-p
                     '(defun nl_os_free_chunk (_base _size) 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-chunk-reclaim-invalidates-pointer-caches ()
  "Empty-chunk release invalidates both raw arena-pointer caches first."
  (cl-labels ((defun-form
               (name forms)
               (cl-find-if (lambda (form)
                             (and (consp form) (eq (car form) 'defun)
                                  (eq (cadr form) name)))
                           (if (eq (car-safe forms) 'seq) (cdr forms) forms)))
              (tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((ready (defun-form 'nl_gc_reclaim_empty_ready
                             nelisp-standalone--gc-source)))
      (should ready)
      (should
       (equal
        (cadddr ready)
        '(nl_seq2
          (nl_gc_freelist_purge_chunk base size)
          (nl_seq2
           (ptr-write-u64
            (data-addr nl_mxcache_epoch) 0
            (+ (ptr-read-u64 (data-addr nl_mxcache_epoch) 0) 1))
           (nl_gc_reclaim_empty_unlink prev chunk next base size))))))
    ;; The macro-expansion and function-value caches reject a row on an epoch
    ;; mismatch before comparing or returning either stored arena pointer.
    (dolist (name '(nl_mxcache_lookup nl_fvcache_lookup))
      (let ((lookup (defun-form name nelisp-standalone--gc-source)))
        (should lookup)
        (should
         (tree-member-p
          '(ptr-read-u64 (data-addr nl_mxcache_epoch) 0)
          lookup))))
    ;; nl_gc_loop_ctx is the other BSS owner of live arena pointers.  A full
    ;; boundary collection may run inside a nested load, so it must retain the
    ;; outer recorded driver frames just as the mid-form collector does.
    (let ((roots (defun-form 'nl_gc_mark_roots
                             nelisp-standalone--gc-source)))
      (should roots)
      (should (tree-member-p '(nl_gc_mark_recorded_contexts) roots))
      (should
       (tree-member-p
        '(nl_seq2 (nl_gc_mark_rootstack)
                  (nl_seq2 (nl_gc_mark_thread_roots)
                           (nl_gc_mark_recorded_contexts)))
        roots)))
    ;; Explicit `(garbage-collect)' uses the recorded-roots collector, so it
    ;; must cover private worker roots too, not only full boundary collection.
    (let ((recorded (defun-form 'nl_gc_collect_from_recorded_roots
                                nelisp-standalone--gc-source))
          (recorded-parked
           (defun-form 'nl_gc_collect_recorded_parked
                       nelisp-standalone--gc-source))
          (full (defun-form 'nl_gc_collect
                            nelisp-standalone--gc-source))
          (full-parked
           (defun-form 'nl_gc_collect_while_workers_parked
                       nelisp-standalone--gc-source))
          (eval-call (defun-form 'nelisp_eval_call
                                 nelisp-standalone--shim-source))
          (eval-done (defun-form 'nelisp_eval_call_done
                                 nelisp-standalone--shim-source))
          (eval-recorded-done
           (defun-form 'nelisp_eval_call_recorded_done
                       nelisp-standalone--shim-source)))
      (should recorded)
      (should (tree-member-p '(nl_gc_collect_recorded_parked mode) recorded))
      (should (tree-member-p '(nl_thread_park_request_begin) recorded-parked))
      (should (tree-member-p '(nl_thread_park_request_end) recorded-parked))
      (should (tree-member-p '(nl_thread_park_collect_succeeded)
                             recorded-parked))
      (should (tree-member-p
               '(nl_gc_collect_while_workers_parked
                 ctx result out pool src cursor bsym)
               full))
      (should (tree-member-p '(nl_thread_park_request_begin) full-parked))
      (should (tree-member-p '(nl_thread_park_request_end) full-parked))
      (should (tree-member-p '(nl_thread_park_safepoint env) eval-call))
      (should (tree-member-p '(nl_thread_park_safepoint env) eval-done))
      (should (tree-member-p '(nl_thread_park_safepoint env)
                             eval-recorded-done)))))

(ert-deftest nelisp-standalone-target-pointer-cache-slots-publish-atomically ()
  "Shared pointer-cache rows use an emitted CAS claim/publish protocol.
The runtime race is scheduler-dependent, so this structural assertion is the
deterministic against-the-bug gate for both emitted cache implementations."
  (cl-labels ((defun-form
               (name forms)
               (cl-find-if (lambda (form)
                             (and (consp form) (eq (car form) 'defun)
                                  (eq (cadr form) name)))
                           (if (eq (car-safe forms) 'seq) (cdr forms) forms)))
              (tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree))))))
              (tree-symbol-p
               (needle tree)
               (cond
                ((eq needle tree) t)
                ((consp tree)
                 (or (tree-symbol-p needle (car tree))
                     (tree-symbol-p needle (cdr tree)))))))
    (dolist (spec '((nl_mxcache_lookup nl_mxcache_lookup_claim
                     nl_mxcache_lookup_release nl_mxcache_store
                     nl_mxcache_store_claim form_ptr expansion_ptr)
                    (nl_fvcache_lookup nl_fvcache_lookup_claim
                     nl_fvcache_lookup_release nl_fvcache_store
                     nl_fvcache_store_claim args_ptr filter_ptr)))
      (cl-destructuring-bind
          (lookup-name lookup-claim-name lookup-release-name store-name
                       store-claim-name key value)
          spec
        (let ((lookup (defun-form lookup-name nelisp-standalone--gc-source))
              (lookup-claim
               (defun-form lookup-claim-name nelisp-standalone--gc-source))
              (lookup-release
               (defun-form lookup-release-name nelisp-standalone--gc-source))
              (store (defun-form store-name nelisp-standalone--gc-source))
              (store-claim
               (defun-form store-claim-name nelisp-standalone--gc-source)))
          (dolist (form (list lookup lookup-claim lookup-release store
                              store-claim))
            (should form))
          ;; Readers take exclusive ownership before the payload snapshot and
          ;; restore the key only after the epoch has been revalidated.
          (should (tree-member-p
                   `(,lookup-claim-name slot ,key 0 0) lookup))
          (should (tree-member-p
                   `(atomic-compare-exchange slot ,key 1) lookup-claim))
          (should (tree-member-p
                   `(,lookup-release-name
                     (ptr-read-u64 (+ slot 8) 0) slot ,key 0)
                   lookup-claim))
          (should (tree-member-p
                   `(atomic-compare-exchange slot 1 ,key) lookup-release))
          (should (tree-member-p
                   '(ptr-read-u64 (data-addr nl_mxcache_epoch) 0)
                   lookup-claim))
          ;; Writers first CAS any published key to the impossible pointer 1,
          ;; fill value+epoch while owned, and atomically publish the key last.
          (should (tree-member-p
                   `(,store-claim-name
                     (ptr-read-u64 slot 0) slot ,key ,value)
                   store))
          (should (tree-member-p
                   '(atomic-compare-exchange slot observed_key 1)
                   store-claim))
          (should
           (tree-member-p
            `(seq
              (ptr-write-u64 (+ slot 8) 0 ,value)
              (ptr-write-u64 (+ slot 16) 0
                             (ptr-read-u64
                              (data-addr nl_mxcache_epoch) 0))
              (if (= (atomic-compare-exchange slot 1 ,key) 1)
                  (seq (atomic-fetch-add 268435544 1) 0)
                0))
            store-claim))
          ;; All across-call runtime state is carried by helper parameters.
          (dolist (form (list lookup-claim lookup-release store-claim))
            (should-not (tree-symbol-p 'let form))
            (should-not (tree-symbol-p 'let* form))))))
    (let ((evict (defun-form 'nl_mxcache_evict
                             nelisp-standalone--gc-source)))
      (should evict)
      (should (tree-member-p
               '(atomic-compare-exchange slot addr 0) evict)))))

(ert-deftest nelisp-standalone-target-intern-region-is-target-aware ()
  "Symbol-name intern region setup uses each target's allocation surface."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((nelisp-standalone--target 'linux-x86_64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 9 0 67108864 3 34 -1 0)
                 arena))))
    (let ((nelisp-standalone--target 'windows-x86_64)
          (nelisp-standalone--windows-arena-base #x70000000))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(extern-call VirtualAlloc 0 67108864 12288 4)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))
    (let ((nelisp-standalone--target 'macos-aarch64))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p '(nl_intern_region_init) arena))
        (should (tree-member-p
                 '(syscall-direct 197 0 67108864 3 4098 -1 0)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 9 0 67108864 3 34 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-arena-allocation-is-chunk-aware ()
  "Doc 140 Stage 4 routes allocation through current chunk descriptors."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let ((flat (flatten-tree (nelisp-standalone--target-arena-source))))
      (should (memq 'nl_chunk_alloc_new flat))
      (should (memq 'nl_chunk_try_alloc flat))
      (should (memq 'atomic-compare-exchange flat))
      (should (member 268436168 flat))
      (should (tree-member-p
               '(= (atomic-compare-exchange cursor_addr old new) 1)
               (nelisp-standalone--target-arena-source)))
      (should (tree-member-p
               '(defun nl_os_alloc_chunk (size)
                  (let ((p (syscall-direct 9 0 size 3 34 -1 0)))
                    (if (< p 4096) 0 p)))
               (let ((nelisp-standalone--target 'linux-x86_64))
                 (nelisp-standalone--target-arena-source)))))))

(ert-deftest nelisp-standalone-target-macos-stage8-rewrites-arena-slots ()
  "macOS Stage 8 rewrites rebased arena metadata to `nl_arena_base' loads."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (should (equal
             (nelisp-standalone--chunk-arena-rewrite
              (nelisp-standalone--rebase-arena-source
               '(seq (ptr-write-u64 268435472 0 1)
                     (atomic-fetch-add 268435544 1)
                     (ptr-write-u64 4096 0 268435456))))
             '(seq
               (ptr-write-u64
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 16) 0 1)
               (atomic-fetch-add
                (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 88) 1)
               (ptr-write-u64
                4096 0 (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)))))))

(ert-deftest nelisp-standalone-target-macos-arena-init-uses-null-mmap ()
  "macOS chunk-0 init uses mmap(NULL, ...) and stores `nl_arena_base'."
  (let ((nelisp-standalone--target 'macos-aarch64))
    (cl-labels ((tree-member-p
                 (needle tree)
                 (cond
                  ((equal needle tree) t)
                  ((consp tree)
                   (or (tree-member-p needle (car tree))
                       (tree-member-p needle (cdr tree)))))))
      (let ((arena (nelisp-standalone--target-arena-source)))
        (should (tree-member-p
                 '(nl_os_alloc_chunk #x20000000)
                 arena))
        (should (tree-member-p
                 '(ptr-write-u64 (data-addr nl_arena_base) 0 base)
                 arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 8589934592 3 4114 -1 0)
                     arena))
        (should-not (tree-member-p
                     '(syscall-direct 197 #x800000000 #x20000000 3 4114 -1 0)
                     arena))))))

(ert-deftest nelisp-standalone-target-windows-reserves-1g-stack ()
  "Windows standalone reserves a Linux-trampoline-sized native stack."
  (should (= nelisp-standalone--windows-stack-reserve #x40000000)))

(ert-deftest nelisp-standalone-target-windows-imports-virtualfree ()
  "Windows eval and reader link paths import VirtualFree for chunk release."
  (let ((nelisp-standalone--target 'windows-x86_64)
        (nelisp-standalone--manifest '(("probe.o" :helper nil)))
        captured-imports)
    (cl-letf (((symbol-function 'nelisp-standalone--unit-for)
               (lambda (_entry)
                 (nelisp-link-unit-make "probe.obj" nil nil nil)))
              ((symbol-function 'nelisp-standalone--arena-base-slot-unit)
               (lambda ()
                 (nelisp-link-unit-make "arena-base.obj"
                                        (list (cons 'bss 8)) nil nil)))
              ((symbol-function 'nelisp-standalone--output-path)
               (lambda (&optional _reader-p) "/tmp/nelisp-target-test.exe"))
              ((symbol-function 'nelisp-link-units-pe32)
               (lambda (_out _units _entry imports &optional _opts)
                 (setq captured-imports imports)))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (nelisp-standalone-build)
      (should (member "VirtualFree" captured-imports))))
  (should (member "VirtualFree"
                  (cdr (assoc "KERNEL32.dll"
                              (nelisp-standalone--reader-pe-imports))))))

(ert-deftest nelisp-standalone-target-macos-uses-bounded-native-stack ()
  "macOS standalone uses an explicit stack that Darwin can mmap reliably."
  (should (= nelisp-standalone--macos-native-stack-size #x20000000))
  (should (< nelisp-standalone--macos-native-stack-size
             nelisp-standalone--native-stack-size)))

;; Doc 194 S5.3/P3 exit criterion: the eight `nelisp-socket-*' names (six
;; Phase 1 primitives + `nelisp-socket-poll'/`nelisp-socket-connect-error',
;; added this phase) must raise the catchable `nelisp-unsupported-
;; primitive' form -- not compile a real (wrong) syscall, not silently fail
;; to link -- on every target other than `linux-x86_64'.  This is "the
;; existing target-swap harness Phase 1's own gate uses" the design doc
;; refers to: `nelisp-standalone--target' let-bound per case and the
;; GENERATED dispatch-arm forms inspected directly at the source level, no
;; cross-arch binary build/execution needed (a Windows PE or aarch64 ELF
;; built on this x86_64 Linux host could not run here anyway).  Phase 1
;; itself never had this ERT-level proof for its own six names (a
;; pre-existing gap, not this phase's own regression) -- verified before
;; this test existed: `linux-x86_64' returns the six real `nl_socket_*_impl'
;; call forms and `windows-x86_64'/`macos-aarch64'/`linux-aarch64' each
;; returned the empty native-forms list (`nelisp-standalone--socket-forms')
;; while STILL wiring six dispatch arms (`nelisp-standalone--socket-
;; dispatch-arms' does not consult `-forms' at all for its non-linux-x86_64
;; branch) -- so this test covers all eight names on every target in one
;; pass, closing that gap for the family as a whole, not only its own two
;; new members.
(ert-deftest nelisp-standalone-target-socket-dispatch-linux-x86-64-real ()
  "linux-x86_64 gets real native call forms for all eight socket primitives."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (arms (nelisp-standalone--socket-dispatch-arms))
         (names (mapcar (lambda (a) (cadr (car a))) arms)))
    (should (equal names '("nelisp-socket-listen" "nelisp-socket-accept"
                            "nelisp-socket-connect" "nelisp-socket-send"
                            "nelisp-socket-recv" "nelisp-socket-close"
                            "nelisp-socket-poll" "nelisp-socket-connect-error")))
    (should (equal (cdr (nth 6 arms)) '(nl_socket_poll_impl args out)))
    (should (equal (cdr (nth 7 arms)) '(nl_socket_connect_error_impl args out)))))

(ert-deftest nelisp-standalone-target-socket-dispatch-non-linux-x86-64-unsupported ()
  "Every socket primitive -- including the two P3 additions -- raises the
catchable `nelisp-unsupported-primitive' signal form on every non-
linux-x86_64 target, never a real (wrong-syscall) call form.

`nelisp-standalone--applyfn-unsupported-primitive-form' is NOT a pure
function returning an `equal'-stable constant across separate calls (it
builds fresh gensym-named `let*' bindings each time, confirmed by a first
version of this test that computed the comparison value via its OWN
independent call and got a structurally-different-but-semantically-
identical form back) -- so this test instead asserts, per target, that
ALL EIGHT dispatch arms share the exact same (`eq'-identical, since
`nelisp-standalone--socket-dispatch-arms' computes `sig' exactly ONCE per
call and closes over it for every arm) signal form, and that this shared
form never mentions any of the eight real `nl_socket_*_impl' native call
targets -- the two properties that together mean \"every name maps to the
one shared unsupported-primitive signal, not to a real (or partially
real) native call\"."
  (let ((real-impls '(nl_socket_listen_impl nl_socket_accept_impl
                       nl_socket_connect_impl nl_socket_send_impl
                       nl_socket_recv_impl nl_socket_close_impl
                       nl_socket_poll_impl nl_socket_connect_error_impl))
        (expected-names '("nelisp-socket-listen" "nelisp-socket-accept"
                           "nelisp-socket-connect" "nelisp-socket-send"
                           "nelisp-socket-recv" "nelisp-socket-close"
                           "nelisp-socket-poll" "nelisp-socket-connect-error")))
    (dolist (target '(windows-x86_64 windows-aarch64 macos-aarch64 linux-aarch64))
      (let* ((nelisp-standalone--target target)
             (arms (nelisp-standalone--socket-dispatch-arms))
             (names (mapcar (lambda (a) (cadr (car a))) arms))
             (shared (cdr (car arms))))
        (should (equal names expected-names))
        (dolist (arm arms)
          (should (eq (cdr arm) shared))
          (should-not (memq (car-safe (cdr arm)) real-impls)))))))

(ert-deftest nelisp-standalone-target-thread-builtins-installed ()
  "All Doc 199 Tier-2 names are installed for uniform `fboundp' behavior."
  (dolist (name '("nelisp-thread-shared-alloc"
                  "nelisp-thread-atomic-add"
                  "nelisp-thread-atomic-read"
                  "nelisp-thread-spawn"
                  "nelisp-thread-join"
                  "nelisp-thread-gc-inhibit"))
    (should (member name nelisp-standalone--reader-builtins))))

(ert-deftest nelisp-standalone-target-thread-linux-x86-64-shape-b-forms ()
  "Linux x86_64 keeps Tier 2 registry worker ID 1 strictly GC-free."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree))))))
              (forbidden-worker-symbol-p
               (tree)
               (cond
                ((symbolp tree)
                 (or (eq tree 'alloc-bytes)
                     (string-prefix-p "nl_alloc_" (symbol-name tree))
                     (eq tree 'nelisp_eval_call)))
                ((consp tree)
                 (or (forbidden-worker-symbol-p (car tree))
                     (forbidden-worker-symbol-p (cdr tree)))))))
    (let* ((nelisp-standalone--target 'linux-x86_64)
           (forms (nelisp-standalone--thread-forms))
           (worker-names '(nl_thread_worker_sum_range nl_thread_worker_sum))
           (worker-forms
            (cl-remove-if-not
             (lambda (form) (memq (cadr form) worker-names)) forms)))
      (should (= (length worker-forms) 2))
      (should (tree-member-p
               '(syscall-direct 56 1792 launch 0 0 0 0) forms))
      (should (tree-member-p '(syscall-direct 60 0 0 0 0 0 0) forms))
      (should (tree-member-p '(atomic-fetch-add done 1) worker-forms))
      (should-not (forbidden-worker-symbol-p worker-forms)))))

(ert-deftest nelisp-standalone-target-thread-linux-x86-64-tier3a-forms ()
  "Tier 3a emits a private env and bounded no-GC allocating worker ID 2."
  (cl-labels ((tree-member-p
               (needle tree)
               (cond
                ((equal needle tree) t)
                ((consp tree)
                 (or (tree-member-p needle (car tree))
                     (tree-member-p needle (cdr tree)))))))
    (let* ((nelisp-standalone--target 'linux-x86_64)
           (forms (nelisp-standalone--thread-forms)))
      (should (tree-member-p '(nelisp_eval_call form env out) forms))
      (should (tree-member-p
               '(syscall-direct 9 0 1052672 3 34 (- 0 1) 0) forms))
      (should (tree-member-p '(record-make type_slot 2 (+ env 32)) forms))
      (should (tree-member-p '(nl_thread_registry_add region) forms))
      (should (tree-member-p '(nl_thread_registry_clear) forms))
      (should (tree-member-p '(nl_thread_park_safepoint env) forms))
      (should (tree-member-p
               '(defun nl_thread_join_impl (args env out)
                  (nl_thread_join_finish
                   (nl_thread_join_wait
                    (wf_argval args 0) (wf_argval args 1) env 0)
                   out 0 0))
               forms))
      (should (tree-member-p
               '(defun nl_thread_gc_inhibit_begin ()
                  (ptr-write-u64 (data-addr nl_gc_loop_ctx) 24 1))
               forms))
      (should (tree-member-p '(ptr-write-u64 268435624 0 1) forms))
      (should (tree-member-p '(atomic-fetch-add done 1) forms)))))

(ert-deftest nelisp-standalone-target-thread-dispatch-linux-x86-64-real ()
  "Linux x86_64 maps all five public names to real native units."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (arms (nelisp-standalone--thread-dispatch-arms)))
    (should
     (equal (mapcar (lambda (arm) (cadr (car arm))) arms)
            '("nelisp-thread-shared-alloc" "nelisp-thread-atomic-add"
              "nelisp-thread-atomic-read" "nelisp-thread-spawn"
              "nelisp-thread-join" "nelisp-thread-gc-inhibit")))
    (should (equal (mapcar (lambda (arm) (car-safe (cdr arm))) arms)
                   '(nl_thread_shared_alloc_impl nl_thread_atomic_add_impl
                     nl_thread_atomic_read_impl nl_thread_spawn_impl
                     nl_thread_join_impl nl_thread_gc_inhibit_impl)))))

(ert-deftest nelisp-standalone-target-thread-non-linux-x86-64-unsupported ()
  "Every non-Linux-x86_64 target installs only unsupported-signal arms."
  (let ((real-impls '(nl_thread_shared_alloc_impl nl_thread_atomic_add_impl
                      nl_thread_atomic_read_impl nl_thread_spawn_impl
                      nl_thread_join_impl nl_thread_gc_inhibit_impl))
        (expected-names '("nelisp-thread-shared-alloc"
                          "nelisp-thread-atomic-add"
                          "nelisp-thread-atomic-read"
                          "nelisp-thread-spawn"
                          "nelisp-thread-join"
                          "nelisp-thread-gc-inhibit")))
    (dolist (target '(windows-x86_64 windows-aarch64 macos-aarch64
                     linux-aarch64))
      (let* ((nelisp-standalone--target target)
             (arms (nelisp-standalone--thread-dispatch-arms))
             (shared (cdr (car arms))))
        (should-not (nelisp-standalone--thread-forms))
        (should (equal (mapcar (lambda (arm) (cadr (car arm))) arms)
                       expected-names))
        (dolist (arm arms)
          (should (eq (cdr arm) shared))
          (should-not (memq (car-safe (cdr arm)) real-impls)))))))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
