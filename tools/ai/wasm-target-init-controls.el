;;; wasm-target-init-controls.el --- Executed rejecting controls -*- lexical-binding: t; -*-
(require 'wasm-target-init)
(defvar wasm-target-init--library-only t)
(require 'wasm-target-init-probe)

(defun wasm-target-init-control-entry (name)
  "Execute one syntax control NAME, returning a real rejecting process status."
  (condition-case err
      (progn
        (pcase name
          ('no-rewrite (wasm-target-init--assert-rewritten nelisp-cc-rootstack--source))
          ('unknown-form
           (let ((contract (wasm-target-init-contract)))
             (wasm-target-init--validate
              wasm-target-init-inputs
              (append (plist-get contract :forms) '((ptr-write-u32 base 0 0)))
              (plist-get contract :data-start))))
          ('undersize (wasm-target-init-contract '(:target wasm32 :arena-size 8 :intern-region excluded)))
          (_ (error "Unknown control: %s" name)))
        (princ "CONTROL-UNEXPECTED-PASS\n")
        (kill-emacs 0))
    (error (princ (format "%s\n" (error-message-string err))) (kill-emacs 1))))

(defun wasm-target-init--syntax-control (name)
  "Run one syntax control NAME in a capped child inside the parent slot."
  (with-temp-buffer
    (let* ((print-length nil) (print-level nil)
           (expression (prin1-to-string `(wasm-target-init-control-entry ',name)))
           (command
            (append (list "-c" "ulimit -v 8388608; exec \"$@\"" "control" "timeout" "120"
                          (expand-file-name invocation-name invocation-directory) "-Q" "--batch")
                    (cl-loop for dir in '("lisp" "src" "test" "tools/ai")
                             append (list "-L" dir))
                    (list "--eval" "(setq load-prefer-newer t)" "-l" "wasm-target-init-controls"
                          "--eval" expression)))
           (rc (apply #'call-process "bash" nil t nil command)))
      (cons rc (buffer-string)))))

(defun wasm-target-init--write-manifest (path manifest)
  "Write control MANIFEST to PATH."
  (with-temp-file path (insert (json-encode manifest)))
  path)

(defun wasm-target-init--audit4-edit (manifest)
  "Replay exactly AUDIT-4's four-leaf edit in MANIFEST."
  (let* ((oracle (alist-get 'oracle_data_start manifest))
         (text (alist-get 'definition oracle)))
    (unless (string-match "1024" text) (error "Missing genuine data-start scalar"))
    (setq text (replace-match "824" t t text))
    (setf (alist-get 'data_start manifest) "824"
          (alist-get 'value oracle) "824"
          (alist-get 'definition oracle) text
          (alist-get 'sha256 oracle) (secure-hash 'sha256 text)))
  manifest)

(defun wasm-target-init--audit5-edit (manifest field code)
  "Replay AUDIT-5's exact text/hash pair edit for FIELD using CODE."
  (let* ((seed (eq field 'seed_forms))
         (needle (if seed "ptr-write-u64 base" "(base-sym size)"))
         (replacement (if seed (concat "ptr-write-u64" (string code) "base")
                        (concat "(base-sym" (string code) "size)")))
         (text (alist-get field manifest)))
    (unless (string-match (regexp-quote needle) text) (error "Missing AUDIT-5 anchor: %s" field))
    (setq text (replace-match replacement t t text))
    (setf (alist-get field manifest) text
          (alist-get (if seed 'seed_forms_sha256 'generator_sha256) manifest)
          (secure-hash 'sha256 text)))
  manifest)

(defun wasm-target-init--fault-build (name)
  "Build a deliberate adapter fault NAME without changing genuine seed records."
  (let ((original (symbol-function 'wasm-target-init--source)))
    (cl-letf (((symbol-function 'wasm-target-init--source)
               (lambda (contract owners &optional skip)
                 (let ((source (funcall original contract owners skip)))
                   ;; Replace only the chosen definition in the composed fixture.
                   (mapcar (lambda (form)
                             (cond
                              ((and (eq name 'rootstack-stub)
                                    (eq (car-safe form) 'defun) (eq (cadr form) 'nl_rootstack_init))
                               '(defun nl_rootstack_init () 0))
                              ((and (eq name 'wrong-first-seed)
                                    (eq (car-safe form) 'defun) (eq (cadr form) 'nl_wasm_arena_init))
                               (let* ((copy (copy-tree form)) (seq (nth 2 (nth 3 copy))))
                                 (setf (nth 3 (nth 2 seq)) 1025)
                                 copy))
                              ((and (eq name 'stray-write)
                                    (eq (car-safe form) 'defun) (eq (cadr form) 'nl_wasm_arena_init))
                               (let* ((copy (copy-tree form)) (seq (nth 2 (nth 3 copy))))
                                 (setcdr (last seq 2) '((ptr-write-u64 (+ base 1016) 0 1) base))
                                 copy))
                              ((and (eq name 'arena-tail-write)
                                    (eq (car-safe form) 'defun) (eq (cadr form) 'nl_wasm_arena_init))
                               (let* ((copy (copy-tree form)) (seq (nth 2 (nth 3 copy))))
                                 (setcdr (last seq 2) '((ptr-write-u64 (+ base 1048568) 0 1) base))
                                 copy))
                              ((and (eq name 'bss-stray-write)
                                    (eq (car-safe form) 'defun) (eq (cadr form) 'nl_wasm_arena_init))
                               (let* ((copy (copy-tree form)) (seq (nth 2 (nth 3 copy))))
                                 (setcdr (last seq 2) '((ptr-write-u64 (data-addr nl_thread_parallel_ctx) 56 1) base))
                                 copy))
                              (t form))) source)))))
      (wasm-target-init-build nil (format "target/wasm-target-init/control-%s.wasm" name) t))))

(defun wasm-target-init-controls ()
  "Execute original and revision controls with exact rejection output."
  (let* ((good (wasm-target-init-build nil "target/wasm-target-init/control-good.wasm" t))
         (skip (wasm-target-init-build t "target/wasm-target-init/control-skip.wasm" t))
         (stray (wasm-target-init--fault-build 'stray-write))
         (stub (wasm-target-init--fault-build 'rootstack-stub))
         (wrong (wasm-target-init--fault-build 'wrong-first-seed))
         (bss (wasm-target-init--fault-build 'bss-stray-write))
         (tail (wasm-target-init--fault-build 'arena-tail-write))
         (json-object-type 'alist) (json-array-type 'vector) results)
    (let ((baseline (wasm-target-init--node good)))
      (unless (and (equal (car baseline) 0)
                   (string-match-p "GATE-COUNT checked=[1-9][0-9]* findings=0" (cdr baseline)))
        (error "Control baseline failed: rc=%s %s" (car baseline) (cdr baseline)))
      (princ (format "control-baseline rc=0 %s" (cdr baseline))))
    (dolist (spec `((skip-init ,skip nil "target-init-check: init returned nonzero base")
                    (contract-mutation ,good expected "target-init-check: seed contract disagreement row 0")
                    (wrong-first-seed ,wrong nil "target-init-check: contract word arena+0")
                    (seed-disagreement ,wrong expected "target-init-check: seed contract disagreement row 0")
                    (stray-write ,stray nil "target-init-check: unseeded arena word +1016")
                    (shortened-data-start ,stray data-start "target-init-check: data-start disagrees with oracle defconst")
                    (audit4-four-leaf ,stray audit4 "target-init-check: data-start authority text disagrees with hashed build source")
                    (generator-authority-text ,good generator "target-init-check: generator authority text disagrees with hashed build source")
                    (audit5-seed-feff ,good seed-feff "target-init-check: unsupported character U+FEFF in Lisp syntax")
                    (audit5-generator-feff ,good generator-feff "target-init-check: unsupported character U+FEFF in Lisp syntax")
                    (audit5-seed-2028 ,good seed-2028 "target-init-check: unsupported character U+2028 in Lisp syntax")
                    (audit5-generator-2028 ,good generator-2028 "target-init-check: unsupported character U+2028 in Lisp syntax")
                    (arena-tail-write ,tail nil "target-init-check: unseeded arena word +1048568")
                    (rootstack-stub ,stub nil "target-init-check: rootstack top = region")
                    (empty-rootstack-postcondition ,stub empty "target-init-check: required rootstack postcondition schema")
                    (replaced-rootstack-postcondition ,stub replaced "target-init-check: required rootstack postcondition schema")
                    (seed-row-count ,good row-count "target-init-check: seed contract row count")
                    (seed-row-owner ,good owner "target-init-check: seed contract disagreement row 0")
                    (seed-row-offset ,good offset "target-init-check: seed contract disagreement row 0")
                    (seed-row-expression ,good expression "target-init-check: seed contract disagreement row 0")
                    (numeric-operand ,good number "target-init-check: expected operand: decimal string required")
                    (bss-stray-write ,bss nil "target-init-check: post-arena BSS nl_thread_parallel_ctx+56")
                    (bss-truncated-size ,bss bss-size "target-init-check: manifest size disagrees with module extent nl_thread_parallel_ctx")
                    (bss-two-field ,bss bss-two-field "target-init-check: manifest size disagrees with module extent nl_thread_parallel_ctx")
                    (bss-missing-owner ,bss bss-removed "target-init-check: required owner set")
                    (no-rewrite nil nil "raw-arena-atoms=4 offsets=(16 24 56 88)")
                    (unknown-form nil nil "Unresolved init form: (ptr-write-u32 base 0 0)")
                    (undersize nil nil "Undersize arena: size=8 required=")))
      (pcase-let* ((`(,name ,module ,mutation ,expected) spec)
                   (copy (and mutation (json-read-file (concat module ".json"))))
                   (row (and mutation (aref (alist-get 'contract_words copy) 0))))
        (pcase mutation
          ('expected (aset (alist-get 'expected row) 1
                           (number-to-string (1+ (string-to-number (aref (alist-get 'expected row) 1))))))
          ('data-start (setf (alist-get 'data_start copy) "824"))
          ('audit4 (wasm-target-init--audit4-edit copy))
          ('seed-feff (wasm-target-init--audit5-edit copy 'seed_forms #xfeff))
          ('generator-feff (wasm-target-init--audit5-edit copy 'generator_definition #xfeff))
          ('seed-2028 (wasm-target-init--audit5-edit copy 'seed_forms #x2028))
          ('generator-2028 (wasm-target-init--audit5-edit copy 'generator_definition #x2028))
          ('generator
           (let ((text (replace-regexp-in-string "(b base-sym)" "(b size)" (alist-get 'generator_definition copy) t t)))
             (setf (alist-get 'generator_definition copy) text
                   (alist-get 'generator_sha256 copy) (secure-hash 'sha256 text))))
          ('empty (setf (alist-get 'post_rootstack_words copy) []))
          ('replaced (setf (alist-get 'post_rootstack_words copy)
                           [((owner . "nl_rootstack_region") (offset . "0") (expected . ["integer" "0"]))]))
          ('row-count (setf (alist-get 'contract_words copy) (cl-subseq (alist-get 'contract_words copy) 1)))
          ('owner (setf (alist-get 'owner row) "nl_arena_base"))
          ('offset (setf (alist-get 'offset row) "8"))
          ('expression (setf (alist-get 'expression row) "1025"))
          ('number (aset (alist-get 'expected row) 1 1024)))
        (pcase mutation
          ((or 'bss-size 'bss-two-field)
           (let ((owner (cl-find "nl_thread_parallel_ctx" (alist-get 'owners copy)
                                 :key (lambda (row) (alist-get 'name row)) :test #'equal)))
             (setf (alist-get 'size owner) "56")
             (when (eq mutation 'bss-two-field)
               ;; Replay AUDIT-3: an obsolete free boundary cannot shorten coverage.
               (setf (alist-get 'end_expression owner) "(+ 57616 4194304 96 176 56)"))))
          ('bss-removed
           (setf (alist-get 'owners copy)
                 (vconcat (cl-remove "nl_thread_parallel_ctx" (alist-get 'owners copy)
                                     :key (lambda (owner) (alist-get 'name owner)) :test #'equal)))))
        (let* ((path (and mutation (wasm-target-init--write-manifest
                                    (format "target/wasm-target-init/%s.json" name) copy)))
               (result (if module (if path (wasm-target-init--node module path) (wasm-target-init--node module))
                         (wasm-target-init--syntax-control name))))
          (unless (and (equal (car result) 1) (string-match-p (regexp-quote expected) (cdr result)))
            (error "Control harness failure %s rc=%S output=%s" name (car result)
                   (truncate-string-to-width (cdr result) 1000)))
          (push `((name . ,(symbol-name name)) (rc . ,(car result)) (message . ,(cdr result))) results)
          (princ (format "control=%s rc=%s rejection=%s\n" name (car result) (string-trim (cdr result)))))))
    (setq results (nreverse results))
    (wasm-target-init--write-manifest "target/wasm-target-init/controls.json" (vconcat results))
    results))
(provide 'wasm-target-init-controls)
