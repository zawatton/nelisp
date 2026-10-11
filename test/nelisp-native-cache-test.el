;;; nelisp-native-cache-test.el --- Host cache controls -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-cache)

(defconst nelisp-native-cache-test--repository-root
  (expand-file-name "../../../.." (file-name-directory (or load-file-name buffer-file-name))))

;; Host Emacs has no standalone runtime.  Only the ptr-call-level primitives
;; below are simulated: ptr-call/read/write, syscall-direct, native env,
;; symbol addresses, runtime contract words, and pin-copy.  The compiler,
;; validators, trusted mapper, boxing, unboxing, file IO and publication are
;; real.  Fixture text is structural test data, never executable code.

(defmacro nelisp-native-cache-test--runtime (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((nelisp-bytecode-compiler-input--root nelisp-native-cache-test--repository-root)
          (memory (make-hash-table :test 'equal))
          (next-map #x100000) (next-root 0) (end-result 1)
          (entry-status 513) (begin-result 7) (copy-bad nil) (end-calls 0)
          (slots-bad nil) (symbol-bad nil)
          (reserve-bad nil) (protect-bad nil) (unmaps 0)
          (contract-words (nelisp-runtime-reload-contract-hash))
          (nelisp-native-load--running-binary-sha256-cache (make-string 64 ?a))
          (nelisp-native-load-raw-mappings nil)
          (nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
          (nelisp-native-load--raw-v2-check-count 0)
          (nelisp-native-load--rooted-cfg-outer-validation-count 0)
          (nelisp-bytecode-native-rooted-cfg-contract--validation-count 0)
          (nelisp-native-load--trusted-map-count 0)
          (nelisp-native-cache--abi :unset)
          (nelisp-native-cache--compiler-revision :unset)
          (nelisp-native-cache--addresses nil)
          (nelisp-native-cache--disabled-reason nil))
     (cl-letf (((symbol-function 'ptr-write-u8)
                (lambda (addr offset value) (puthash (list addr offset) value memory)))
               ((symbol-function 'ptr-write-u32)
                (lambda (addr offset value) (puthash (list addr offset) value memory)))
               ((symbol-function 'ptr-write-u64)
                (lambda (addr offset value) (puthash (list addr offset) value memory)))
               ((symbol-function 'ptr-read-u64)
                (lambda (addr offset) (gethash (list addr offset) memory 0)))
               ((symbol-function 'syscall-direct)
                (lambda (number &rest _)
                  (cond ((= number 9) (setq next-map (+ next-map #x10000)))
                        ((= number 11) (setq unmaps (1+ unmaps)) 0)
                        ((= number 10) (if protect-bad -1 0))
                        (t (error "Unexpected syscall")))))
               ((symbol-function 'nelisp--native-env) (lambda () #x8000))
               ((symbol-function 'nelisp--native-runtime-contract-word)
                (lambda (i) (string-to-number
                             (substring contract-words (* i 8) (* (1+ i) 8)) 16)))
               ((symbol-function 'nelisp--native-symbol-addr)
                (lambda (i) (if symbol-bad 0 (+ #x10000 (* 16 i)))))
               ((symbol-function 'nelisp--native-runtime-symbol-addr)
                (lambda (i) (+ #x20000 (* 16 i))))
               ((symbol-function 'nelisp--native-pin-copy-v2)
                (lambda (_env _ticket index value)
                  (let ((slot (+ #x40000 (* 32 index))))
                    (nelisp-native-load-box slot value)
                    (if copy-bad 0 slot))))
               ((symbol-function 'ptr-call)
                (lambda (addr _env _ticket index &rest _)
                  (let ((addresses nelisp-native-cache--addresses))
                    (cond ((eql addr (plist-get addresses :begin))
                           (setq next-root 0) begin-result)
                          ((eql addr (plist-get addresses :reserve))
                           (prog1 (if reserve-bad 0 (+ #x40000 (* 32 next-root)))
                             (setq next-root (1+ next-root))))
                          ((eql addr (plist-get addresses :slot))
                           (+ #x40000 (* 32 index) (if slots-bad 1 0)))
                          ((eql addr (plist-get addresses :end))
                           (setq end-calls (1+ end-calls)) end-result)
                          (t entry-status))))))
       ,@body)))

(defmacro nelisp-native-cache-test--directory (&rest body)
  (declare (indent 0) (debug t))
  `(let ((directory (make-temp-file "nelisp-cache-test-" t))
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn (set-file-modes directory #o700)
                (setenv "NELISP_NATIVE_CACHE" directory)
                ,@body)
       (delete-directory directory t))))

(defun nelisp-native-cache-test--function ()
  (make-byte-code 514 (unibyte-string 192 135) [1] 2))

(defun nelisp-native-cache-test--derive ()
  "Return (FUNCTION INPUT PLAN EMITTED CONTRACT) derived outside any runtime stub.
Inside `nelisp-native-cache-test--runtime' the stubbed native symbols make the
compiler input refuse host bytecode, so the genuine contract is derived first."
  (let* ((function (byte-compile (lambda (left right) (cons left right))))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                   plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry)))
    (list function input plan emitted
          (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted))))

(defvar nelisp-native-cache-test--derived (nelisp-native-cache-test--derive)
  "Host-derived fixture, computed at load time before any stub is installed.")

(defun nelisp-native-cache-test--fixture ()
  "Build a genuine shared contract with fake text for host mapping controls."
  (let* ((function (nth 0 nelisp-native-cache-test--derived))
         (input (nth 1 nelisp-native-cache-test--derived))
         (plan (nth 2 nelisp-native-cache-test--derived))
         (emitted (nth 3 nelisp-native-cache-test--derived))
         (contract (copy-tree (nth 4 nelisp-native-cache-test--derived)))
         (imports
          (mapcar (lambda (name)
                    (list :name name :kind 'func :abi nelisp-native-load-raw-runtime-abi-v2
                          :index (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal)
                          :address-mode (if (equal name "nl_root_pin_slot_v2")
                                            'conditional-root-slot-v1 'native-bridgeable-v1)
                          :arity 6 :params '(u64 u64 u64 u64 u64 u64) :return 'u64))
                  (plist-get contract :imports)))
         (gc (nelisp-native-load--raw-v2-contract))
         (exports nil) (entries nil) (index 0))
    (should contract)
    (dolist (spec (append gc (list (cons (plist-get contract :entry) 4))))
      (push (list :name (car spec) :value 0 :size 1 :type 'func
                  :abi nelisp-native-load-raw-runtime-abi-v2 :arity (cdr spec)
                  :params (make-list (cdr spec) 'u64) :return 'u64) exports))
    (dolist (spec gc)
      (push (list :name (car spec) :index index :arity (cdr spec)
                  :abi nelisp-native-load-raw-runtime-abi-v2) entries)
      (setq index (1+ index)))
    (let ((manifest
           (list :kind 'raw-runtime :format nelisp-native-load-raw-artifact-format-v2
                 :runtime-kind 'gc-arena :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                 :runtime-opt-in t :layout-id nelisp-native-load-raw-layout-id-v2
                 :arch nelisp-native-load-raw-supported-arch :binary-sha256 (make-string 64 ?a)
                 :resolver-contract-version nelisp-native-load-raw-v2-import-contract-version
                 :resolver-contract-hash (nelisp-native-load--raw-v2-import-contract-hash
                                          (nelisp-native-load--raw-v2-symbols))
                 :gc-entries (nreverse entries) :gc-table-count (length gc)
                 :gc-table-magic nelisp-native-load-raw-gc-table-magic
                 :gc-contract-hash (nelisp-native-load--raw-v2-contract-hash)
                 :native-rooted-cfg-contract-version (plist-get contract :version)
                 :native-rooted-cfg-contract contract
                 :native-rooted-cfg-import-descriptors imports
                 :native (list :raw-abi nelisp-native-load-raw-runtime-abi-v2
                               :object-format 'nelisp-aot-raw-unit-v2
                               :text-size 1 :object-size 1 :text-base64 "ww=="
                               :object-sha256 (nelisp-native-load--raw-digest (unibyte-string 195))
                               :imports imports :exports (nreverse exports)
                               :relocs nil :data-size 0 :bss-size 0))))
      (setq manifest (plist-put manifest :artifact-sha256
                               (nelisp-native-load--sha256 (prin1-to-string manifest))))
      (list function manifest
            (list :nelisp-native-cache 1 :abi (nelisp-native-cache-abi-hash)
                  :input (nelisp-native-cache--input-hash function)
                  :entry (plist-get contract :entry)
                  :arity (plist-get plan :arity) :root-count (plist-get plan :required-root-count)
                  :initializers (append (plist-get emitted :constant-initializers)
                                        (plist-get emitted :immediate-initializers))
                  :exit-root-base (plist-get plan :exit-root-base))))))

(defun nelisp-native-cache-test--write (file header manifest)
  (write-region (concat (nelisp-native-cache--print header) "\n"
                        (nelisp-native-cache--print manifest) "\n") nil file nil 'silent))

(defun nelisp-native-cache-test--counts ()
  (list nelisp-native-load--raw-v2-check-count
        nelisp-native-load--rooted-cfg-outer-validation-count
        nelisp-bytecode-native-rooted-cfg-contract--validation-count))

(ert-deftest nelisp-native-cache/name-content-and-modes ()
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (let* ((fn (nelisp-native-cache-test--function))
             (name (nelisp-native-cache-file fn)))
        (should (equal name (nelisp-native-cache-file fn)))
        (dolist (changed
                 (list (make-byte-code 514 (unibyte-string 193 135) [1] 2)
                       (make-byte-code 514 (unibyte-string 192 135) [2] 2)
                       (make-byte-code 257 (unibyte-string 192 135) [1] 2)
                       (make-byte-code 514 (unibyte-string 192 135) [1] 3)
                       (make-byte-code 514 (unibyte-string 192 135) [1] 2 "doc")
                       (make-byte-code 514 (unibyte-string 192 135) [1] 2 nil "p")))
          (should-not (equal name (nelisp-native-cache-file changed))))
        (let ((nelisp-native-cache-mode 'other))
          (should-not (equal name (nelisp-native-cache-file fn))))
        (let ((nelisp-native-cache-guard-mode 'other))
          (should-not (equal name (nelisp-native-cache-file fn))))))))

(ert-deftest nelisp-native-cache/name-every-abi-component ()
  (should (member (nelisp-native-funcall-v2-descriptor) (nelisp-native-cache--abi-components)))
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (let* ((fn (nelisp-native-cache-test--function))
             (components (nelisp-native-cache--abi-components))
             (revision (nelisp-native-cache-compiler-revision-hash))
             (nelisp-native-cache--abi (nelisp-native-cache--hash (list components revision)))
             (name (nelisp-native-cache-file fn)))
        (dotimes (i (length components))
          (let ((changed (copy-tree components)))
            (setcar (nthcdr i changed) (list :changed (nth i changed)))
            (let ((nelisp-native-cache--abi (nelisp-native-cache--hash (list changed revision))))
              (should-not (equal name (nelisp-native-cache-file fn))))))))))

(ert-deftest nelisp-native-cache/name-every-compiler-source ()
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (let* ((source-dir (expand-file-name "sources" directory))
             (load-path (cons source-dir load-path))
             (fn (nelisp-native-cache-test--function))
             (modules nelisp-native-cache--compiler-modules))
        (make-directory source-dir)
        (dolist (module modules)
          (write-region "a" nil (expand-file-name (concat (symbol-name module) ".el") source-dir)
                        nil 'silent))
        (let* ((revision (nelisp-native-cache-compiler-revision-hash))
               (components (nelisp-native-cache--abi-components))
               (nelisp-native-cache--abi (nelisp-native-cache--hash (list components revision)))
               (name (nelisp-native-cache-file fn)))
          (dolist (module modules)
            (let ((path (expand-file-name (concat (symbol-name module) ".el") source-dir))
                  (nelisp-native-cache--compiler-revision :unset))
              (write-region "b" nil path nil 'silent)
              (let* ((changed (nelisp-native-cache-compiler-revision-hash))
                     (nelisp-native-cache--abi (nelisp-native-cache--hash (list components changed))))
                (should-not (equal changed revision))
                (should-not (equal name (nelisp-native-cache-file fn))))
              (write-region "a" nil path nil 'silent))))))))

(ert-deftest nelisp-native-cache/missing-module-disables-once ()
  (let ((nelisp-native-cache--compiler-revision :unset)
        (nelisp-native-cache--abi :unset)
        (nelisp-native-cache--compiler-modules '(nelisp-cache-deliberately-missing)))
    (should-not (nelisp-native-cache-abi-hash))
    (let ((nelisp-native-cache--compiler-modules '(nelisp-native-load)))
      (should-not (nelisp-native-cache-abi-hash)))))

(ert-deftest nelisp-native-cache/name-dialect-and-opcode-inventory ()
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (let* ((fn (nelisp-native-cache-test--function))
             (revision (nelisp-native-cache-compiler-revision-hash))
             (components (nelisp-native-cache--abi-components))
             (nelisp-native-cache--abi (nelisp-native-cache--hash (list components revision)))
             (name (nelisp-native-cache-file fn)))
        (dolist (symbol '(nelisp-bytecode-runtime-dialect-id nelisp-bytecode-runtime-opcode-inventory))
          (cl-progv (list symbol) '("changed")
            (let* ((nelisp-native-cache--compiler-revision :unset)
                   (changed (nelisp-native-cache-compiler-revision-hash))
                   (nelisp-native-cache--abi (nelisp-native-cache--hash (list components changed))))
              (should-not (equal revision changed))
              (should-not (equal name (nelisp-native-cache-file fn))))))))))

(ert-deftest nelisp-native-cache/process-checks-once ()
  (nelisp-native-cache-test--runtime
    (let* ((checks 0)
          (observer (lambda (&rest _) (setq checks (1+ checks)))))
      (advice-add 'nelisp-runtime-reload-contract-matches-p :before observer)
      (unwind-protect
          (progn (should (nelisp-native-cache-abi-hash))
                 (should (nelisp-native-cache-abi-hash))
                 (should (= checks 1)))
        (advice-remove 'nelisp-runtime-reload-contract-matches-p observer)))))

(ert-deftest nelisp-native-cache/publication-first-writer-wins ()
  (nelisp-native-cache-test--directory
    (let ((first (make-temp-file (expand-file-name "first-" directory)))
          (second (make-temp-file (expand-file-name "second-" directory)))
          (final (expand-file-name "unit.nelr" directory)))
      (write-region "first" nil first nil 'silent)
      (write-region "second" nil second nil 'silent)
      (should (nelisp-native-cache--publish first final))
      (should-not (nelisp-native-cache--publish second final))
      (should-not (file-exists-p first))
      (should-not (file-exists-p second))
      (should (equal (with-temp-buffer (insert-file-contents final) (buffer-string)) "first")))))

(ert-deftest nelisp-native-cache/directory-native-fallbacks ()
  (nelisp-native-cache-test--directory
    (let ((uid (user-uid)) (calls 0)
          (created (expand-file-name "private" directory)))
      (cl-letf (((symbol-function 'user-uid) nil)
                ((symbol-function 'default-file-modes) nil)
                ((symbol-function 'set-default-file-modes) nil)
                ((symbol-function 'syscall-direct)
                 (lambda (&rest args)
                   (should (equal args '(102 0 0 0 0 0 0)))
                   (setq calls (1+ calls))
                   uid)))
        (should (equal created (nelisp-native-cache--private-directory created)))
        (should (= (logand (file-modes created) #o7777) #o700))
        (should (= calls 1))
        (set-file-modes created #o755)
        (should-error (nelisp-native-cache--private-directory created))
        (should (= (file-modes created) #o755))))))

(ert-deftest nelisp-native-cache/publication-native-fallback ()
  (nelisp-native-cache-test--directory
    (let ((link (symbol-function 'add-name-to-file))
          (memory (make-hash-table)) (next-address 0) (calls 0)
          (gc-inhibited nil)
          (forced-result nil)
          (final (expand-file-name "公開.nelr" directory)))
      ;; Simulate only allocation, byte stores and link(2); the syscall shim
      ;; decodes the actual NUL-terminated buffers and makes real hard links.
      (cl-letf (((symbol-function 'add-name-to-file) nil)
                ((symbol-function 'nl-ffi--string-to-cstring) nil)
                ((symbol-function 'nelisp--debug-switch)
                 (lambda (switch)
                   (should (memq switch '(5 6)))
                   (setq gc-inhibited (= switch 6))))
                ((symbol-function 'alloc-bytes)
                 (lambda (size alignment)
                   (should gc-inhibited)
                   (should (= alignment 1))
                   (setq next-address (1+ next-address))
                   (puthash next-address (make-vector size nil) memory)
                   next-address))
                ((symbol-function 'ptr-write-u8)
                 (lambda (address offset byte)
                   (should (<= 0 byte 255))
                   (aset (gethash address memory) offset byte)))
                ((symbol-function 'syscall-direct)
                 (lambda (number source target &rest padding)
                   (should gc-inhibited)
                   (should (= number 86))
                   (should (equal padding '(0 0 0 0)))
                   (setq calls (1+ calls))
                   (let ((paths
                          (mapcar
                           (lambda (address)
                             (let ((bytes (gethash address memory)))
                               (should (= (aref bytes (1- (length bytes))) 0))
                               (should-not (memq nil (append bytes nil)))
                               (decode-coding-string
                                (apply #'unibyte-string
                                       (butlast (append bytes nil))) 'utf-8-unix)))
                           (list source target))))
                     (if forced-result
                         (if (eq forced-result 'error)
                             (error "Injected link failure")
                           forced-result)
                       (condition-case nil
                           (progn (funcall link (car paths) (cadr paths) nil) 0)
                         (file-already-exists -17)))))))
        (dolist (value '("first" "second"))
          (let ((temporary (make-temp-file (expand-file-name "一時-" directory))))
            (write-region value nil temporary nil 'silent)
            (should (eq (nelisp-native-cache--publish temporary final)
                        (equal value "first")))
            (should-not gc-inhibited)
            (should-not (file-exists-p temporary))))
        (should (= calls 2))
        (should (equal (with-temp-buffer (insert-file-contents final) (buffer-string)) "first"))
        (dolist (result '(-13 error))
          (setq forced-result result)
          (let ((temporary (make-temp-file (expand-file-name "failure-" directory))))
            (if (eq result 'error)
                (should-error (nelisp-native-cache--publish temporary final))
              (should-not (nelisp-native-cache--publish temporary final)))
            (should-not gc-inhibited)
            (should-not (file-exists-p temporary))))))))

(ert-deftest nelisp-native-cache/directory-permissions-and-owner ()
  (nelisp-native-cache-test--directory
    (should (nelisp-native-cache--private-directory directory))
    (dolist (mode '(#o755 #o770 #o777 #o750 #o1700))
      (set-file-modes directory mode)
      (should-error (nelisp-native-cache--private-directory directory)))
    (set-file-modes directory #o700)
    (let ((link (concat directory "-link")))
      (unwind-protect
          (progn (make-symbolic-link directory link)
                 (should-error (nelisp-native-cache--private-directory link)))
        (delete-file link)))))

(ert-deftest nelisp-native-cache/header-refusal-before-mapping ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (pcase-let* ((`(,fn ,manifest ,header) (nelisp-native-cache-test--fixture))
                   (file (nelisp-native-cache-file fn)))
        (dolist (key '(:nelisp-native-cache :abi :input :root-count :entry))
          (let ((bad (copy-tree header)))
            (plist-put bad key :corrupt)
            (nelisp-native-cache-test--write file bad manifest)
            (should-error (nelisp-native-cache-load fn))
            (should (= nelisp-native-load--trusted-map-count 0))))
        (write-region "(" nil file nil 'silent)
        (should-error (nelisp-native-cache-load fn))
        (should (= nelisp-native-load--trusted-map-count 0))))))

(ert-deftest nelisp-native-cache/load-zero-validations-and-positive-controls ()
  (skip-unless (equal emacs-version "31.1"))
  ;; The trusted load runs under the native-runtime stubs.  The positive
  ;; controls run afterwards without them, because the stubbed primitives make
  ;; the host compiler input refuse to reconstruct the contract.
  (let (manifest header counts)
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (pcase-let* ((`(,fn ,fixture-manifest ,fixture-header) (nelisp-native-cache-test--fixture))
                   (file (nelisp-native-cache-file fn)))
        (setq manifest fixture-manifest header fixture-header
              counts (nelisp-native-cache-test--counts))
        (nelisp-native-cache-test--write file header manifest)
        (let ((callable (nelisp-native-cache-load fn)))
          (should-not (featurep 'nelisp-native-load-rooted-cfg-admission))
          (should (= (funcall callable 12 34) 12))
          (should-error (funcall callable 12) :type 'wrong-number-of-arguments)
          (should (equal counts (nelisp-native-cache-test--counts)))
          (should (= nelisp-native-load--trusted-map-count 1))))))
        ;; Fresh snapshots and a cold memo make every real counter reachable.
        (setq nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
        (should (nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p (copy-tree manifest)))
        (should (> nelisp-native-load--rooted-cfg-outer-validation-count (nth 1 counts)))
        (should (> nelisp-bytecode-native-rooted-cfg-contract--validation-count (nth 2 counts)))
        (should-not (nelisp-native-load-raw-v2-check (copy-tree manifest) (plist-get header :entry)))
        (should (> nelisp-native-load--raw-v2-check-count (car counts)))
        ;; The checked mapper validates before it needs the native runtime, so on
        ;; the host it stops at the binary-identity step; its validation still runs.
        (let ((before (nelisp-native-cache-test--counts)))
          (setq nelisp-native-load--raw-v2-rooted-cfg-validation-cache nil)
          (condition-case nil
              (nelisp-native-load-raw-v2-artifact
               (copy-tree manifest) (plist-get header :entry) (make-string 64 ?a))
            (error nil))
          (cl-mapc (lambda (after old) (should (> after old)))
                   (nelisp-native-cache-test--counts) before))))

(ert-deftest nelisp-native-cache/snapshot-read-once ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (pcase-let* ((`(,fn ,manifest ,header) (nelisp-native-cache-test--fixture))
                   (file (nelisp-native-cache-file fn))
                   (reads 0)
                   (observer (lambda (path &rest _)
                               (when (equal path file)
                                 (setq reads (1+ reads))))))
        (nelisp-native-cache-test--write file header manifest)
        (advice-add 'insert-file-contents :after observer)
        (unwind-protect
            (progn (should (functionp (nelisp-native-cache-load fn))) (should (= reads 1)))
          (advice-remove 'insert-file-contents observer))))))

(ert-deftest nelisp-native-cache/frame-failures-and-broken-unit ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (pcase-let* ((`(,fn ,manifest ,header) (nelisp-native-cache-test--fixture))
                   (file (nelisp-native-cache-file fn)))
        (nelisp-native-cache-test--write file header manifest)
        (let ((callable (nelisp-native-cache-load fn)))
          (setq begin-result 0)
          (should-error (funcall callable 1 2))
          (setq begin-result 7 reserve-bad t)
          (should-error (funcall callable 1 2))
          (setq reserve-bad nil copy-bad t)
          (should-error (funcall callable 1 2))
          (setq copy-bad nil slots-bad t)
          (should-error (funcall callable 1 2))
          (setq slots-bad nil entry-status 257)
          (should-error (funcall callable 1 2) :type 'wrong-type-argument)
          (setq entry-status 0)
          (should-error (funcall callable 1 2))
          (setq entry-status 513 end-result 0)
          (should-error (funcall callable 1 2))
          (setq end-result 1)
          (should-error (funcall callable 1 2)))))))

(ert-deftest nelisp-native-cache/trusted-memory-safety-refusals-and-cleanup ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (pcase-let ((`(,_ ,manifest ,header) (nelisp-native-cache-test--fixture)))
      (dolist (mutation
               (list (lambda (m) (plist-put (plist-get m :native) :text-size 2))
                     (lambda (m) (plist-put (car (plist-get (plist-get m :native) :exports)) :value 100))
                     (lambda (m) (plist-put (car (plist-get m :gc-entries)) :index -1))
                     (lambda (m) (plist-put m :gc-table-count 0))
                     (lambda (m) (plist-put (plist-get m :native) :relocs
                                           '((:offset 1 :type pc32 :symbol "missing" :addend 0))))))
        (let ((bad (copy-tree manifest)))
          (funcall mutation bad)
          (should-error (nelisp-native-load-raw-v2-artifact-trusted
                         bad (plist-get header :entry) "fixture"))
          (should (= nelisp-native-load--trusted-map-count 0))))
      (setq protect-bad t)
      (should-error (nelisp-native-load-raw-v2-artifact-trusted
                     manifest (plist-get header :entry) "fixture"))
      (should (= unmaps 2)))))

(ert-deftest nelisp-native-cache/trusted-symbol-and-displacement-cleanup ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (pcase-let ((`(,_ ,manifest ,header) (nelisp-native-cache-test--fixture)))
      (setq symbol-bad t)
      (should-error (nelisp-native-load-raw-v2-artifact-trusted
                     manifest (plist-get header :entry) "fixture"))
      (should (= unmaps 1))
      (setq symbol-bad nil unmaps 0)
      (let ((native (plist-get manifest :native)))
        (plist-put native :text-base64 (base64-encode-string (make-string 8 0) t))
        (plist-put native :text-size 8)
        (plist-put native :object-size 8)
        (plist-put native :relocs
                   (list (list :offset 0 :type 'pc32 :symbol "nl_native_cons_v2"
                               :addend (expt 2 31)))))
      ;; Re-sign the fixture so refusal exercises relocation shape validation.
      (plist-put manifest :object-sha256
                 (nelisp-native-load--sha256 (make-string 8 0)))
      (plist-put manifest :artifact-sha256
                 (nelisp-native-load--sha256
                  (prin1-to-string
                   (nelisp-native-load--raw-plist-without manifest :artifact-sha256))))
      (should-error (nelisp-native-load-raw-v2-artifact-trusted
                     manifest (plist-get header :entry) "fixture"))
      ;; The Win64 merge checks displacement bounds before allocating pages.
      (should (= unmaps 0))
      ;; A valid relocation must still clean up a failed executable publication.
      (plist-put (car (plist-get (plist-get manifest :native) :relocs)) :addend 0)
      (plist-put manifest :artifact-sha256
                 (nelisp-native-load--sha256
                  (prin1-to-string
                   (nelisp-native-load--raw-plist-without manifest :artifact-sha256))))
      (setq protect-bad t)
      (should-error (nelisp-native-load-raw-v2-artifact-trusted
                     manifest (plist-get header :entry) "fixture"))
      (should (= unmaps 2)))))

(ert-deftest nelisp-native-cache/exit-protocol-keeps-roots-until-cleanup ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-native-cache-test--runtime
    (nelisp-native-cache-test--directory
      (pcase-let* ((`(,fn ,manifest ,header) (nelisp-native-cache-test--fixture))
                   (file (nelisp-native-cache-file fn)))
        (plist-put header :root-count 6)
        (plist-put header :exit-root-base 3)
        (plist-put header :initializers
                   '((:root 3 :value 2) (:root 4 :value 123) (:root 5 :value 456)))
        (nelisp-native-cache-test--write file header manifest)
        (setq entry-status 1027)
        (let ((callable (nelisp-native-cache-load fn))
              (before (nelisp-native-cache-test--counts)))
          (should (eql (catch 123 (funcall callable 1 2)) 456))
          (should (= end-calls 1))
          (should (equal before (nelisp-native-cache-test--counts))))))))

(provide 'nelisp-native-cache-test)
;;; nelisp-native-cache-test.el ends here
