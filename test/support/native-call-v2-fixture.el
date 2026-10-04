;;; native-call-v2-fixture.el --- Source-pinned static diagnostic builder -*- lexical-binding: t; -*-

;; This dedicated executable links the provider beside immutable runtime units.
;; It is not a loader artifact, proof owner, capability, or public CALL entry.
(require 'json)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-standalone-build)
(require 'nelisp-native-call-v2)

(defun nelisp-call-fixture--callees (output)
  "Serialize genuine GNU bytecode as explicit source-free VM recipes."
  (unless (equal emacs-version "31.1") (error "Diagnostic requires GNU Emacs 31.1"))
  (load (expand-file-name "test/fixtures/native-bytecode/native-call-v2.el") nil t)
  (let ((print-length (if (getenv "NELISP_CALL_PRINTER_MUTANT") 8 nil)) (print-level nil))
   (with-temp-file (expand-file-name "native-call-callees.el" output)
    (insert ";;; Generated GNU Emacs 31.1 source-free bytecode recipes.\n")
    (dolist (name '(nelisp-call-fixture-vm0 nelisp-call-fixture-vm1 nelisp-call-fixture-vm2
                    nelisp-call-fixture-vm3 nelisp-call-fixture-vm4 nelisp-call-fixture-vm5
                    nelisp-call-fixture-gc nelisp-call-fixture-signal nelisp-call-fixture-throw
                    nelisp-call-fixture-quit))
      (let* ((fn (byte-compile (symbol-function name)))
             (form (list 'fset (list 'quote name)
                         (list 'make-byte-code (aref fn 0)
                               (cons 'unibyte-string (append (aref fn 1) nil))
                               (list 'quote (aref fn 2)) (aref fn 3)))))
        (unless (byte-code-function-p fn) (error "Fixture is not genuine GNU bytecode"))
        ;; A syntactically valid ellipsis is still a broken VM recipe. Detect
        ;; bounded-printer truncation on the host, before a native launch.
        (let ((text (prin1-to-string form)))
          (unless (equal (car (read-from-string text)) form)
            (error "Source-free recipe round-trip assertion failed"))
          (insert text "\n")))))))

(when (getenv "NELISP_CALL_CALLEES_ONLY")
  (nelisp-call-fixture--callees (getenv "NELISP_CALL_FIXTURE_OUTPUT"))
  (kill-emacs 0))

(defconst nelisp-call-fixture--pins
  '(("scripts/nelisp-standalone-build.el" . "e5642d5a645b6c0fae5b727525020094e18842783aa0bb82b7e90e4daee7dedd")
    ("lisp/nelisp-cc-rootstack.el" . "ee50e7fc22e7053cbdc38be2b8a90e72cfdd81f8a01f537e11f40e662ad3018f")
    ("lisp/nelisp-aot-compiler.el" . "1913835c500b9dcc931818b77c193a481b5c1b7da4f511364e3d9d0daf6cc062")
    ("lisp/nelisp-static-linker.el" . "b00b70ee0cc78d179a32552a5c2cdbe890b4ea05a5205aec01e5ce2c2b1ac785")))

(defun nelisp-call-fixture--hash (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-call-fixture--mutate (node variant)
  "Return an independent variant, with only the named semantic defect."
  (cond
   ((atom node) node)
   ((and (equal variant "no-staging")
         (equal node '(nl_native_call_v2_copy (+ first (* i 32)) (+ function (* i 32)))))
    '(if (= i 0) (nl_native_call_v2_copy first function) 0))
   ((and (equal variant "no-exit-copy")
         (equal node '(nl_native_call_v2_copy outer-value value))) 0)
   (t (mapcar (lambda (child) (nelisp-call-fixture--mutate child variant)) node))))

(defconst nelisp-call-fixture--entry
  '(defun nl_call_diag_entry (env ticket call-base argc result-index exit-base)
     (let* ((control (data-addr nl_root_pin_control))
            (root-top (ptr-read-u64 (data-addr nl_rootstack_top) 0))
            (owner (ptr-read-u64 control 8))
            (marker (ptr-read-u64 control 16))
            (top (ptr-read-u64 control 24))
            (token (ptr-read-u64 control 32))
            (mode (ptr-read-u64 control 48))
            (status (nl_native_call_v2 env ticket call-base argc result-index exit-base)))
       (if (and (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) root-top)
                (= (ptr-read-u64 control 8) owner)
                (= (ptr-read-u64 control 16) marker)
                (= (ptr-read-u64 control 24) top)
                (= (ptr-read-u64 control 32) token)
                (= (ptr-read-u64 control 48) mode))
           status 99)))
  "Diagnostic witness compares root state without intervening Lisp callbacks.")

(let* ((frozen (or (getenv "NELISP_CALL_FROZEN_UNITS") (error "Missing frozen units")))
       (output (or (getenv "NELISP_CALL_FIXTURE_OUTPUT") (error "Missing output directory")))
       (variant (or (getenv "NELISP_CALL_VARIANT") "good"))
       (metadata (expand-file-name "active-unit-metadata.json" frozen))
       (generation (expand-file-name "startup-generation.json" frozen))
       (json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
       (units nil) (records nil))
  ;; These two final-link units are intentionally outside the proof manifest.
  ;; Borrow their frozen bytes too; never regenerate startup evidence or pins.
  (setq records
        '(((path . "arena-base.o.unit")
           (unit-sha256 . "faa9fadc302101bd47e3a127f68f910ec413b1be4f4f07b94cc4691cf3bf1939"))
          ((path . "../standalone-units/linux-x86_64/driver.o.7f32005e6e417e2846c65fa3ae9dc01545ca0a7b.unit")
           (unit-sha256 . "23e1f1f5e65e1603e204e69d9cf4ee66a658a8ac628af2da893861cfcc5af2e2"))))
  (unless (member variant '("good" "no-staging" "no-exit-copy"))
    (error "Unknown diagnostic variant"))
  (unless (equal (nelisp-call-fixture--hash metadata)
                 "3a40385cdb93077a51d64b77787ca5f1e1c3063a06eaa9d8358786ca09b955f0")
    (error "Frozen manifest changed"))
  (unless (equal (nelisp-call-fixture--hash generation)
                 "54d776bfe16516d0db87e18e2415e45e4f0459a666b901439d6352942e559316")
    (error "Frozen generation changed"))
  (dolist (pin nelisp-call-fixture--pins)
    (unless (equal (nelisp-call-fixture--hash (car pin)) (cdr pin))
      (error "Diagnostic source pin changed: %s" (car pin))))
  (dolist (source (alist-get 'sources (json-read-file generation)))
    (unless (equal (nelisp-call-fixture--hash (alist-get 'path source))
                   (alist-get 'sha256 source))
      (error "Frozen source changed: %s" (alist-get 'path source))))
  (setq records (append records (json-read-file metadata)))
  (dolist (record records)
    (let ((file (expand-file-name (alist-get 'path record) frozen)))
      (unless (equal (nelisp-call-fixture--hash file) (alist-get 'unit-sha256 record))
        (error "Frozen unit changed: %s" file))
      (push (with-temp-buffer
              (insert-file-contents file)
              (nelisp-standalone--unit-cache-decode (read (current-buffer)))) units)))
  (setq units (nreverse units))
  (nelisp-call-fixture--callees output)
  (let* ((source (nelisp-call-fixture--mutate (nelisp-native-call-v2-source) variant))
         (provider (nelisp-standalone--compile-to-unit "native-call-provider.o" source))
         (locals (mapcar (lambda (form) (symbol-name (cadr form))) (cdr source)))
         (expected (mapcar (lambda (record) (plist-get record :name))
                           (nelisp-native-call-v2-runtime-imports)))
         (imports (delete-dups
                   (cl-remove-if (lambda (name) (member name locals))
                                 (mapcar (lambda (r) (plist-get r :symbol))
                                         (plist-get provider :relocs)))))
         (witness (nelisp-standalone--compile-to-unit "native-call-witness.o"
                                                     nelisp-call-fixture--entry)))
    (unless (equal (sort imports #'string<) (sort (copy-sequence expected) #'string<))
      (error "Provider imports differ from the exact source-owned ABI: %S" imports))
    (setq units (append units (list provider witness)))
    (let* ((combined (nelisp-link-combine-sections units))
           (layout (nelisp-link--compute-layout combined))
           (linked (nelisp-link-units-2pass units layout))
           (symbols (plist-get linked :symtab))
           (image (expand-file-name (concat "nelisp-call-" variant) output))
           (mapfile (expand-file-name (concat variant "-addresses.el") output))
           (addresses nil))
      (dolist (name '("nl_call_diag_entry" "nl_root_pin_begin_v2" "nl_root_pin_end_v2"
                      "nl_root_pin_reserve_v2" "nl_root_pin_slot_v2" "nl_arena_base"
                      "nl_root_pin_control" "nl_thread_registry"))
        (let ((symbol (nelisp-link-symtab-lookup symbols name)))
          (unless symbol (error "Missing genuine static symbol: %s" name))
          (push (cons name (plist-get symbol :value)) addresses)))
      (nelisp-link-units image units)
      (set-file-modes image #o755)
      (with-temp-file mapfile
        (insert ";;; Generated addresses for a dedicated static diagnostic only.\n")
        (prin1 (list 'setq 'nelisp-call-diag-addresses (list 'quote addresses)) (current-buffer))
        (insert "\n"))
      ;; Detect concurrent replacement of any borrowed frozen unit.
      (dolist (record records)
        (unless (equal (nelisp-call-fixture--hash
                        (expand-file-name (alist-get 'path record) frozen))
                       (alist-get 'unit-sha256 record))
          (error "Frozen unit changed during diagnostic link")))
      (message "native-call fixture: %s; %d frozen units; imports=%S; sha256=%s"
               variant (length records) imports (nelisp-call-fixture--hash image)))))

;;; native-call-v2-fixture.el ends here
