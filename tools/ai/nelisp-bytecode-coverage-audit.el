;;; nelisp-bytecode-coverage-audit.el --- GNU byte-code coverage inventory -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Audit the pinned GNU 31.1 opcode inventory against the decoder, frame IR,
;; and tested native-lowering paths.  This reads repository data only; it does
;; not require GNU bytecomp sources or execute byte-code.

;;; Code:

(require 'cl-lib)
(require 'json)
(defvar nelisp-bytecode-coverage-audit--root
  (expand-file-name "../.." (file-name-directory
                               (or load-file-name buffer-file-name))))
(defconst nelisp-bytecode-coverage-audit--inventory-sha256
  "147da590c9f5bdcf190b5b410c6af878c793ac89e07eafa4ae9f05a9b7aa7bcb")
(defconst nelisp-bytecode-coverage-audit--reserved-manifest-sha256
  "0d1232d1411750ded03240b7d30daac2295b64951ba9cf394b4311591b1c3358")
(defconst nelisp-bytecode-coverage-audit--gnu-bytecomp-sha256
  "094fa608bed9d9feffd4364b8df3288c1eb2bf1efd5dfa57fcd6d9bd13cdd099")
(defconst nelisp-bytecode-coverage-audit--gnu-bytecode-c-sha256
  "97fb8f41758f88c02684da4f766d7694e3c3274077f4be46a3163949bb6431ab")
(defconst nelisp-bytecode-coverage-audit--build-probe-sha256
  "d6955d16d6a28bc072113bc2764e0ecb57e2ebfae1fc8e8616cf49d9dd01c8b2")
(defconst nelisp-bytecode-coverage-audit--valid-fixtures-sha256
  "d9e7725d77fb4c61c69d84905fdc9f5b7f1bc1d7f9867c8fb9c4c92aef8f41c8")
(defconst nelisp-bytecode-coverage-audit--build-emacs-sha256
  "7a73e7e5db25275f09753fba330b96ecd44c64b3af9c5298fe343a44084bf5d7")
(defconst nelisp-bytecode-coverage-audit--gnu-reserved-invalid-opcodes
  '(0))
(defconst nelisp-bytecode-coverage-audit--gnu-unused-unassigned-opcodes
  '(51 52 53 54 55 107 115 128 146 169 170 171 172 173 174 180 181
    184 185 186 187 188 189 190 191))
(defconst nelisp-bytecode-coverage-audit--malformed-diagnostic-opcodes
  '(41 42 43 44 45 50 183))
(defconst nelisp-bytecode-coverage-audit--backends '(in-house gccjit)
  "Independent execution authorities; historical N/L labels are not these.")
(defconst nelisp-bytecode-coverage-audit--raw-i64-cases
  '((1 "lowers-stack-ref-dup-and-discard" (192 193 1 135) (5 10))
    (130 "lowers-a-backedge-without-calling-it" (130 0 0) nil)
    (131 "emits-a-real-cfg-branch-artifact"
     (192 131 8 0 193 130 9 0 194 135) (t 1 2))
    (133 "lowers-edge-dependent-conditional-pop" (192 133 5 0 193 135) (nil 7))
    (134 "lowers-edge-dependent-conditional-pop" (192 134 5 0 193 135) (nil 7))
    (135 "emits-a-real-cfg-branch-artifact"
     (192 131 8 0 193 130 9 0 194 135) (t 1 2))
    (136 "lowers-stack-ref-dup-and-discard" (192 137 136 135) (5 10))
    (137 "lowers-stack-ref-dup-and-discard" (192 137 136 135) (5 10))
    (192 "emits-a-real-cfg-branch-artifact"
     (192 131 8 0 193 130 9 0 194 135) (t 1 2))
    (193 "emits-a-real-cfg-branch-artifact"
     (192 131 8 0 193 130 9 0 194 135) (t 1 2))
    (194 "emits-a-real-cfg-branch-artifact"
     (192 131 8 0 193 130 9 0 194 135) (t 1 2))
    (195 "lowers-live-slots-across-branch-join"
     (192 193 194 131 10 0 195 130 11 0 196 135) (5 10 t 20 30))
    (196 "lowers-live-slots-across-branch-join"
     (192 193 194 131 10 0 195 130 11 0 196 135) (5 10 t 20 30))))
(defconst nelisp-bytecode-coverage-audit--legacy-jit-opcodes
  '(1 57 58 60 61 85 92 131 135 137 192 193 194))
(defconst nelisp-bytecode-coverage-audit--statuses
  '(gnu-reserved-invalid gnu31.1-pinned-build-invalid decoder-rejected structurally-decoded
    frame-represented native-raw-i64-slice
    legacy-jit-only runtime-op-pending unknown-pending))

(defun nelisp-bytecode-coverage-audit--sha256-file (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-bytecode-coverage-audit--inventory (&optional root)
  "Read the pinned inventory and reject changed or incomplete data."
  (let* ((path (expand-file-name "test/fixtures/native-bytecode/gnu-31.1-opcodes.json"
                                 (or root nelisp-bytecode-coverage-audit--root)))
         (hash (nelisp-bytecode-coverage-audit--sha256-file path))
         (inventory (json-read-file path)))
    (unless (and (equal hash nelisp-bytecode-coverage-audit--inventory-sha256)
                 (equal (alist-get 'dialect inventory) "GNU Emacs 31.1")
                 (= (alist-get 'opcode-count inventory) 256)
                 (= (length (alist-get 'opcodes inventory)) 256))
      (error "Pinned GNU 31.1 opcode inventory is missing or changed"))
    inventory))

(defun nelisp-bytecode-coverage-audit--gnu-source-dispositions (root)
  "Verify ROOT's pinned GNU source evidence and return source dispositions."
  (let* ((manifest-path
          (expand-file-name
           "test/fixtures/native-bytecode/gnu-31.1-reserved-opcodes.json" root))
         (manifest-sha (nelisp-bytecode-coverage-audit--sha256-file manifest-path))
         (manifest (json-read-file manifest-path))
         (bytecomp-path
          (expand-file-name "vendor/emacs-lisp/emacs-lisp/bytecomp.el" root))
         (bytecomp-sha (nelisp-bytecode-coverage-audit--sha256-file bytecomp-path))
         (sources (append (alist-get 'sources manifest) nil))
         (bytecomp-source (car sources))
         (c-source (cadr sources))
         (reserved (append (alist-get 'reserved_invalid_opcodes manifest) nil))
         (unused (append (alist-get 'unused_unassigned_opcodes manifest) nil)))
    (unless (and
             (equal manifest-sha
                    nelisp-bytecode-coverage-audit--reserved-manifest-sha256)
             (equal (alist-get 'dialect manifest) "GNU Emacs 31.1")
             (equal (alist-get 'inventory_sha256 manifest)
                    nelisp-bytecode-coverage-audit--inventory-sha256)
             (equal (nelisp-bytecode-coverage-audit--sha256-file
                     (expand-file-name
                      "test/fixtures/native-bytecode/gnu-31.1-opcodes.json" root))
                    (alist-get 'inventory_sha256 manifest))
             (equal bytecomp-sha
                    nelisp-bytecode-coverage-audit--gnu-bytecomp-sha256)
             (equal (alist-get 'sha256 bytecomp-source)
                    nelisp-bytecode-coverage-audit--gnu-bytecomp-sha256)
             (equal (alist-get 'gnu_path bytecomp-source)
                    "lisp/emacs-lisp/bytecomp.el")
             (equal (alist-get 'sha256 c-source)
                    nelisp-bytecode-coverage-audit--gnu-bytecode-c-sha256)
             (equal (alist-get 'tag c-source) "emacs-31.1")
             (equal reserved
                    nelisp-bytecode-coverage-audit--gnu-reserved-invalid-opcodes)
             (equal unused
                    nelisp-bytecode-coverage-audit--gnu-unused-unassigned-opcodes))
      (error "Pinned GNU 31.1 reserved-opcode source evidence changed"))
    (append (mapcar (lambda (opcode) (cons opcode 'gnu-reserved-invalid)) reserved)
            (mapcar (lambda (opcode) (cons opcode 'gnu-unused-unassigned)) unused))))

(defun nelisp-bytecode-coverage-audit--pinned-build-dispositions (root)
  "Verify ROOT's isolated GNU build probes and return build dispositions."
  (let* ((path (expand-file-name
                "test/fixtures/native-bytecode/gnu-31.1-installed-build-unused-opcodes.json"
                root))
         (hash (nelisp-bytecode-coverage-audit--sha256-file path))
         (data (json-read-file path))
         (executable (alist-get 'executable data))
         (probe (alist-get 'probe data))
         (control (alist-get 'valid_control data))
         (results (append (alist-get 'results data) nil)))
    (unless (and
             (equal hash nelisp-bytecode-coverage-audit--build-probe-sha256)
             (equal (alist-get 'dialect data) "GNU Emacs 31.1")
             (equal (alist-get 'scope data)
                    "installed-build-only; does not generalize to all GNU builds")
             (equal (alist-get 'sha256 executable)
                    nelisp-bytecode-coverage-audit--build-emacs-sha256)
             (equal (alist-get 'version executable) "GNU Emacs 31.1")
             (equal (alist-get 'resolved_basename executable) "emacs-31.1")
             (= (alist-get 'timeout_seconds probe) 2)
             (equal (alist-get 'isolation probe)
                    "one independent GNU subprocess per opcode")
             (equal (append (alist-get 'bytecode control) nil) '(192 135))
             (equal (append (alist-get 'constants control) nil) '(42))
             (= (alist-get 'exit_code control) 0)
             (equal (alist-get 'stdout control) "42")
             (equal (mapcar (lambda (row) (alist-get 'opcode row)) results)
                    nelisp-bytecode-coverage-audit--gnu-unused-unassigned-opcodes)
             (cl-every
              (lambda (row)
                (and (= (alist-get 'exit_code row) 255)
                     (eq (alist-get 'timed_out row) :json-false)
                     (equal (alist-get 'error row)
                            (format "Invalid byte opcode: op=%d, ptr=0"
                                    (alist-get 'opcode row)))))
              results))
      (error "Pinned GNU 31.1 installed-build opcode probe evidence changed"))
    (mapcar (lambda (row)
              (cons (alist-get 'opcode row) 'gnu31.1-pinned-build-invalid))
            results)))

(defun nelisp-bytecode-coverage-audit--probe-code (opcode)
  "Return a complete one-opcode stream for OPCODE plus a return, if needed."
  (let ((operands
         (cond ((<= 1 opcode 5) nil)
               ((= opcode 6) '(0))
               ((= opcode 7) '(0 0))
               ((and (<= 8 opcode) (<= opcode 47))
                (pcase (logand opcode 7) (6 '(0)) (7 '(0 0)) (_ nil)))
               ((memq opcode '(49 50)) '(3 0))
               ((<= 129 opcode 134) '(3 0))
               ((memq opcode '(175 176 177 182)) '(0))
               ((= opcode 178) '(0))
               ((= opcode 179) '(0 0))
               (t nil)))
        (suffix (if (= opcode 135) nil '(135))))
    (if (= opcode 183)
        ;; Diagnostic table operand is explicitly nil; dynamic tables now have
        ;; a general runtime lane, so an unknown entry operand is no longer bad.
        (unibyte-string 192 183 135)
      (apply #'unibyte-string (append (list opcode) operands suffix)))))

(defun nelisp-bytecode-coverage-audit--valid-fixtures (root)
  "Read ROOT's VALID fixtures without replacing historical diagnostic probes."
  (let* ((path (expand-file-name
                "test/fixtures/native-bytecode/gnu-31.1-valid-fixtures.json" root))
         (manifest (json-read-file path))
         (rows (append (alist-get 'valid manifest) nil))
         (excluded (append nelisp-bytecode-coverage-audit--gnu-reserved-invalid-opcodes
                           nelisp-bytecode-coverage-audit--gnu-unused-unassigned-opcodes))
         (expected (cl-loop for opcode below 256
                            unless (memq opcode excluded) collect opcode)))
    (unless (and (equal (nelisp-bytecode-coverage-audit--sha256-file path)
                        nelisp-bytecode-coverage-audit--valid-fixtures-sha256)
                 (= (alist-get 'schema manifest) 1)
                 (equal (alist-get 'dialect manifest) "GNU Emacs 31.1")
                 (equal (alist-get 'inventory_sha256 manifest)
                        nelisp-bytecode-coverage-audit--inventory-sha256)
                 (equal (mapcar (lambda (row) (alist-get 'opcode row)) rows) expected)
                 (equal (append (alist-get 'malformed_opcodes
                                           (alist-get 'diagnostic manifest)) nil)
                        nelisp-bytecode-coverage-audit--malformed-diagnostic-opcodes))
      (error "Pinned GNU 31.1 VALID fixture manifest changed"))
    (dolist (row rows)
      (unless (and (equal (alist-get 'id row)
                          (format "gnu31-valid-%03d" (alist-get 'opcode row)))
                   (= (alist-get 'initial_depth row) 0)
                   (> (alist-get 'declared_stack_depth row) 0)
                   (vectorp (alist-get 'constants row))
                   (vectorp (alist-get 'bytecode row))
                   (cl-every (lambda (byte) (and (integerp byte) (<= 0 byte 255)))
                             (append (alist-get 'bytecode row) nil)))
        (error "Invalid VALID fixture for opcode %s" (alist-get 'opcode row))))
    rows))

(defun nelisp-bytecode-coverage-audit--fixture-constants (fixture)
  "Materialize FIXTURE's data literals and explicit object descriptors.
No fixture expression is evaluated.  Buffers refer to the caller's current
buffer, and mutable constants are freshly constructed for each proof."
  (vconcat
   (mapcar
    (lambda (value)
      (if (stringp value)
          (let ((parsed (read-from-string value)))
            (unless (string-match-p "\\`[ \t\n]*\\'" (substring value (cdr parsed)))
              (error "Trailing fixture constant data"))
            (car parsed))
        (pcase (alist-get 'kind value)
          ("current-buffer" (current-buffer))
          ("marker" (make-marker))
          ("hash-table"
           (let ((test (alist-get 'test value)))
             (unless (member test '("eq" "eql" "equal"))
               (error "Unsupported fixture table test %S" test))
             (let ((table (make-hash-table :test (intern test))))
               (dolist (entry (append (alist-get 'entries value) nil))
                 (puthash (aref entry 0) (aref entry 1) table))
               table)))
          (_ (error "Unknown fixture constant descriptor %S" value)))))
    (append (alist-get 'constants fixture) nil))))

(defun nelisp-bytecode-coverage-audit--fixture-proof (fixture)
  "Return frame evidence for FIXTURE, without asserting native execution."
  (let* ((code (apply #'unibyte-string (append (alist-get 'bytecode fixture) nil)))
         (constants (nelisp-bytecode-coverage-audit--fixture-constants fixture))
         (decoded (nelisp-bytecode-ir-decode-result code constants))
         (frame (nelisp-bytecode-frame-ir-build code constants
                                               (alist-get 'initial_depth fixture))))
    (unless (cl-find (alist-get 'opcode fixture) (plist-get decoded :instructions)
                     :key (lambda (row) (aref row 1)))
      (error "VALID fixture does not decode its advertised opcode"))
    (append
     (list :id (alist-get 'id fixture) :frame-status (plist-get frame :status)
           :reason (plist-get frame :reason))
     (when (or (= (alist-get 'opcode fixture) 183) (<= 40 (alist-get 'opcode fixture) 47))
       (require 'nelisp-bytecode-native-rooted-cfg-contract)
       (let* ((fn (make-byte-code 0 code constants (alist-get 'declared_stack_depth fixture)))
              (input (nelisp-bytecode-compiler-input-build fn))
              (plan (nelisp-bytecode-native-rooted-cfg-plan input))
              (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                        plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry)))
         (list :shared-form-status (if (eq (plist-get emitted :status) 'complete) 'proved 'pending)))))))

(defun nelisp-bytecode-coverage-audit--backend-proof (backend fixture)
  "Return independent execution evidence for BACKEND and FIXTURE.
U0 has no authenticated backend execution receipts.  Compilation, ERT source
presence, frame verification and historical N/L labels cannot set executed."
  (list :backend backend :fixture-id (plist-get fixture :id)
        :executed-native nil :evidence nil))

(defun nelisp-bytecode-coverage-audit--ert-test-present-p (root test-name)
  (let ((path (expand-file-name "test/nelisp-bytecode-native-cfg-test.el" root)))
    (and (file-readable-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (re-search-forward
            (format "^(ert-deftest nelisp-bytecode-native-cfg/%s[ \n]"
                    (regexp-quote test-name)) nil t)))))

(defun nelisp-bytecode-coverage-audit--load-paths (root)
  "Return ROOT's Lisp and package source directories."
  (let ((packages (expand-file-name "packages" root)) (paths nil))
    (push (expand-file-name "lisp" root) paths)
    (when (file-directory-p packages)
      (dolist (package (directory-files packages t "\\`[^.]"))
        (let ((src (expand-file-name "src" package)))
          (when (file-directory-p src) (push src paths)))))
    paths))

(defun nelisp-bytecode-coverage-audit--raw-i64-proof-p (root case)
  "Run CASE through ROOT's raw-i64 CFG lowerer; its ERT name is traceability."
  (let* ((test-name (nth 1 case))
         (source (expand-file-name "lisp/nelisp-bytecode-native-cfg.el" root))
         (lowered nil))
    (when (and (file-readable-p source)
               (nelisp-bytecode-coverage-audit--ert-test-present-p root test-name))
      (let ((load-path (append (nelisp-bytecode-coverage-audit--load-paths root)
                               load-path)))
        (condition-case nil
            (progn
              (unless (featurep 'nelisp-bytecode-native-cfg)
                (load source nil nil t))
              (setq lowered
                    (nelisp-bytecode-native-cfg-lower
                     (apply #'unibyte-string (nth 2 case))
                     (vconcat (nth 3 case))))
              (and (eq (plist-get lowered :status) 'complete)
                   (stringp (plist-get lowered :machine-bytes))
                   (eq (plist-get (plist-get lowered :backend) :kind) 'x86_64-cfg)
                   (eq (plist-get (plist-get lowered :backend) :value-repr)
                       'raw-i64-frame-slots)))
          (error nil))))))

(defun nelisp-bytecode-coverage-audit--pc-class (opcode)
  "Return the decoder's structural opcode family for OPCODE."
  (cond ((= opcode 0) "reserved-or-stack-ref-base")
        ((<= 1 opcode 7) "stack-reference")
        ((<= 8 opcode 47) "compact-variable-call")
        ((<= 48 opcode 50) "handler-control")
        ((or (= opcode 56) (<= 57 opcode 82)) "object-operation")
        ((<= 83 opcode 95) "arithmetic")
        ((<= 96 opcode 127) "buffer-operation")
        ((<= 129 opcode 134) "branch-or-constant2")
        ((<= 135 opcode 137) "return-stack")
        ((<= 138 opcode 145) "unwind-control")
        ((<= 147 opcode 168) "extended-object-operation")
        ((<= 175 opcode 183) "count-switch-stack-set")
        ((<= 192 opcode 255) "compact-constant")
        (t "unnamed-or-reserved")))

(defun nelisp-bytecode-coverage-audit--classify
    (opcode name constants root source-dispositions build-dispositions fixture)
  "Classify OPCODE NAME using decoder, frame verifier, and native CFG proof."
  (let* ((code (nelisp-bytecode-coverage-audit--probe-code opcode))
           (decoded (nelisp-bytecode-ir-decode-result code constants))
           (rows (plist-get decoded :instructions))
           (row (cl-find opcode rows :key (lambda (instruction) (aref instruction 1))))
           (structural (and row (= (aref row 1) opcode)))
           (frame (and structural
                       (nelisp-bytecode-frame-ir-build code constants 32)))
           (frame-status (and frame (plist-get frame :status)))
           (raw-case (cl-find opcode
                              nelisp-bytecode-coverage-audit--raw-i64-cases
                              :key #'car))
           (source-status (cdr (assq opcode source-dispositions)))
           (build-status (cdr (assq opcode build-dispositions)))
           (valid-proof (and fixture (nelisp-bytecode-coverage-audit--fixture-proof fixture)))
           (status (cond
                    (build-status build-status)
                    (source-status source-status)
                    ((and (null name) (not structural)) 'decoder-rejected)
                    ((and name (not structural)) 'unknown-pending)
                    ((not structural) 'unknown-pending)
                    ((and raw-case
                          (nelisp-bytecode-coverage-audit--raw-i64-proof-p root raw-case))
                     'native-raw-i64-slice)
                    ((and (memq opcode nelisp-bytecode-coverage-audit--legacy-jit-opcodes)
                          (eq frame-status 'complete))
                     'legacy-jit-only)
                    ((eq frame-status 'complete) 'frame-represented)
                    ((eq frame-status 'unsupported) 'runtime-op-pending)
                    (t 'structurally-decoded))))
      (list :opcode opcode :name name :pc-class
            (nelisp-bytecode-coverage-audit--pc-class opcode) :status status
            :source-disposition source-status
            :decoder-status (plist-get decoded :status)
            :frame-status frame-status
            :valid-fixture valid-proof
            :backend-execution
            (mapcar (lambda (backend)
                      (cons backend (nelisp-bytecode-coverage-audit--backend-proof
                                     backend valid-proof)))
                    nelisp-bytecode-coverage-audit--backends)
            :reason (or (and (eq status 'gnu-reserved-invalid)
                             "GNU 31.1 explicitly reserves byte 0 and errors on it")
                        (and (eq status 'gnu31.1-pinned-build-invalid)
                             (format "GNU BYTE_CODES leaves this slot unassigned; pinned Emacs binary sha256 %s rejected it in an isolated probe"
                                     nelisp-bytecode-coverage-audit--build-emacs-sha256))
                        (and (eq status 'decoder-rejected)
                             "decoder rejected audit probe; GNU validity not established")
                        (and (not structural) (plist-get decoded :reason))
                        (and frame (plist-get frame :reason)))
            :evidence (cond
                       ((eq status 'native-raw-i64-slice)
                        (format "executed nelisp-bytecode-native-cfg-lower raw-i64; ERT traceability #%s"
                                (nth 1 raw-case)))
                       ((eq status 'gnu31.1-pinned-build-invalid)
                        (format "gnu-31.1-installed-build-unused-opcodes.json sha256 %s"
                                nelisp-bytecode-coverage-audit--build-probe-sha256))))))

(defun nelisp-bytecode-coverage-audit-validate (report &optional root)
  "Signal an error unless REPORT disposes every pinned opcode slot exactly once."
  (let* ((rows (plist-get report :opcodes)) (seen nil)
         (source-dispositions
          (nelisp-bytecode-coverage-audit--gnu-source-dispositions
           (or root nelisp-bytecode-coverage-audit--root)))
         (build-dispositions
          (nelisp-bytecode-coverage-audit--pinned-build-dispositions
           (or root nelisp-bytecode-coverage-audit--root)))
         (expected-names
          (append (alist-get 'opcodes
                             (nelisp-bytecode-coverage-audit--inventory root)) nil)))
    (unless (and (vectorp rows) (= (length rows) 256))
      (error "Audit must contain exactly 256 opcode slots"))
    (unless (= (plist-get report :named-opcode-count)
               (cl-count-if #'identity expected-names))
      (error "Audit named-opcode count differs from pinned inventory"))
    (dotimes (index 256)
      (let* ((row (aref rows index)) (opcode (plist-get row :opcode))
             (name (plist-get row :name)) (status (plist-get row :status)))
        (unless (and (= opcode index) (not (memq opcode seen)))
          (error "Missing or duplicate opcode disposition at %d" index))
        (push opcode seen)
        (unless (memq status nelisp-bytecode-coverage-audit--statuses)
          (error "Missing disposition for opcode %d (%s)" index name))
        (unless (equal name (nth index expected-names))
          (error "Opcode %d name does not match pinned inventory" index))
        (let* ((source-status (cdr (assq index source-dispositions)))
               (build-status (cdr (assq index build-dispositions)))
               (expected-status (or build-status source-status)))
          (when (and source-status
                     (not (eq source-status (plist-get row :source-disposition))))
            (error "Opcode %d source classification disagrees with pinned GNU source evidence" index))
          (when (and expected-status (not (eq expected-status status)))
            (error "Opcode %d disposition disagrees with pinned GNU source/build evidence" index)))
        (when (and name (eq status 'decoder-rejected))
          (error "Named opcode %d (%s) cannot be marked decoder-rejected" index name))
        (let* ((excluded (assq index source-dispositions))
               (fixture (plist-get row :valid-fixture))
               (proofs (plist-get row :backend-execution)))
          (unless (if excluded
                      (null fixture)
                    (and (equal (plist-get fixture :id) (format "gnu31-valid-%03d" index))
                         (memq (plist-get fixture :frame-status) '(complete unsupported malformed))))
            (error "Opcode %d has missing or ineligible VALID fixture evidence" index))
          (unless (equal (mapcar #'car proofs) nelisp-bytecode-coverage-audit--backends)
            (error "Opcode %d has missing or duplicate backend execution fields" index))
          (dolist (entry proofs)
            (let ((proof (cdr entry)))
              (unless (and (eq (plist-get proof :backend) (car entry))
                           (equal (plist-get proof :fixture-id) (plist-get fixture :id))
                           (plist-member proof :executed-native)
                           (null (plist-get proof :executed-native))
                           (plist-member proof :evidence)
                           (null (plist-get proof :evidence)))
                (error "Opcode %d has unauthenticated backend execution evidence" index)))))))
    (unless (and (= (plist-get report :excluded-opcode-count) (length source-dispositions))
                 (= (plist-get report :valid-opcode-count) (- 256 (length source-dispositions)))
                 (equal (plist-get report :executed-native-counts)
                        (mapcar (lambda (backend) (cons backend 0))
                                nelisp-bytecode-coverage-audit--backends)))
      (error "Audit fixture/backend accounting disagrees with opcode rows"))
    (unless (equal (mapcar #'car (plist-get report :counts))
                   nelisp-bytecode-coverage-audit--statuses)
      (error "Audit diagnostic counts have missing or duplicate classes"))
    (dolist (entry (plist-get report :counts))
      (unless (= (cdr entry) (cl-count (car entry) rows :key (lambda (row) (plist-get row :status))))
        (error "Audit diagnostic count disagrees with opcode rows")))
    (unless (equal (mapcar #'car (plist-get report :valid-fixture-frame-counts))
                   '(complete unsupported malformed))
      (error "Audit VALID frame counts have missing or duplicate classes"))
    (dolist (entry (plist-get report :valid-fixture-frame-counts))
      (unless (= (cdr entry)
                 (cl-count (car entry) rows
                           :key (lambda (row)
                                  (plist-get (plist-get row :valid-fixture) :frame-status))))
        (error "Audit VALID frame count disagrees with opcode rows")))
    (let ((rebasing (append (plist-get report :malformed-probe-rebasing) nil)))
      (unless (equal (mapcar (lambda (row) (plist-get row :opcode)) rebasing)
                     nelisp-bytecode-coverage-audit--malformed-diagnostic-opcodes)
        (error "Audit diagnostic rebasing omitted or duplicated probes"))
      (dolist (entry rebasing)
        (let ((row (aref rows (plist-get entry :opcode))))
          (unless (and (eq (plist-get entry :diagnostic-status) (plist-get row :status))
                       (eq (plist-get entry :diagnostic-frame-status) (plist-get row :frame-status))
                       (eq (plist-get entry :valid-frame-status)
                           (plist-get (plist-get row :valid-fixture) :frame-status)))
            (error "Audit diagnostic rebasing disagrees with opcode rows")))))
    report))

(defun nelisp-bytecode-coverage-audit-run (&optional root)
  "Return the complete pinned GNU 31.1 opcode audit as a plist.
ROOT defaults to this tool's repository and permits auditing an integrated tree."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root nelisp-bytecode-coverage-audit--root))))
         (nelisp-bytecode-coverage-audit--root root)
         (load-path (append (nelisp-bytecode-coverage-audit--load-paths root)
                            load-path))
         (_decoder (require 'nelisp-bytecode-ir))
         (_frame (require 'nelisp-bytecode-frame-ir))
         (inventory (nelisp-bytecode-coverage-audit--inventory root))
         (source-dispositions
          (nelisp-bytecode-coverage-audit--gnu-source-dispositions root))
         (build-dispositions
          (nelisp-bytecode-coverage-audit--pinned-build-dispositions root))
         (fixtures (nelisp-bytecode-coverage-audit--valid-fixtures root))
         (names (append (alist-get 'opcodes inventory) nil))
         (constants (make-vector 256 nil))
         (rows (vconcat
                (cl-loop for opcode below 256
                         for name in names
                         collect (nelisp-bytecode-coverage-audit--classify
                                  opcode name constants root source-dispositions
                                  build-dispositions
                                  (cl-find opcode fixtures
                                           :key (lambda (fixture)
                                                  (alist-get 'opcode fixture)))))))
         (counts (let ((result nil))
                   (dolist (status nelisp-bytecode-coverage-audit--statuses)
                     (push (cons status
                                 (cl-count status rows
                                           :key (lambda (row)
                                                  (plist-get row :status))))
                           result))
                   (nreverse result)))
         (gaps (cl-remove-if-not
                (lambda (row) (memq (plist-get row :status)
                                    '(decoder-rejected
                                      structurally-decoded runtime-op-pending
                                      unknown-pending native-raw-i64-slice
                                      legacy-jit-only)))
                (append rows nil)))
         (gap-classes (let ((result nil))
                        (dolist (row gaps)
                          (let* ((class (plist-get row :pc-class))
                                 (entry (assoc class result)))
                            (if entry
                                (setcdr entry (1+ (cdr entry)))
                              (push (cons class 1) result))))
                        (nreverse result)))
         (valid-counts
          (mapcar (lambda (status)
                    (cons status
                          (cl-count status rows
                                    :key (lambda (row)
                                           (plist-get (plist-get row :valid-fixture)
                                                      :frame-status)))))
                  '(complete unsupported malformed)))
         (backend-counts
          (mapcar
           (lambda (backend)
             (cons backend
                   (cl-count-if
                    (lambda (row)
                      (plist-get (cdr (assq backend (plist-get row :backend-execution)))
                                 :executed-native))
                    rows)))
           nelisp-bytecode-coverage-audit--backends))
         (rebasing
          (vconcat
           (mapcar (lambda (opcode)
                     (let ((row (aref rows opcode)))
                       (list :opcode opcode :diagnostic-status (plist-get row :status)
                             :diagnostic-frame-status (plist-get row :frame-status)
                             :valid-frame-status
                             (plist-get (plist-get row :valid-fixture) :frame-status))))
                   nelisp-bytecode-coverage-audit--malformed-diagnostic-opcodes)))
         (report (list :schema 2 :dialect "GNU Emacs 31.1"
                       :inventory-sha256
                       nelisp-bytecode-coverage-audit--inventory-sha256
                       :opcode-count 256 :named-opcode-count
                       (cl-count-if #'identity names)
                       :counts counts
                       :excluded-opcode-count (length source-dispositions)
                       :valid-opcode-count (length fixtures)
                       :valid-fixture-frame-counts valid-counts
                       :executed-native-counts backend-counts
                       :malformed-probe-rebasing rebasing
                       :native-raw-i64-count
                       (cdr (assq 'native-raw-i64-slice counts))
                       :legacy-jit-only-count
                       (cdr (assq 'legacy-jit-only counts))
                       :s4-admitted-native-count 0
                       :gap-classes gap-classes
                       :gaps (vconcat gaps) :opcodes rows)))
    (nelisp-bytecode-coverage-audit-validate report root)))

(defun nelisp-bytecode-coverage-audit--json-object (report)
  (let ((result (make-hash-table :test 'equal)))
    (dolist (key '(:schema :dialect :inventory-sha256 :opcode-count :named-opcode-count
                   :native-raw-i64-count :legacy-jit-only-count
                   :s4-admitted-native-count :excluded-opcode-count :valid-opcode-count))
      (puthash (substring (symbol-name key) 1) (plist-get report key) result))
    (puthash "counts"
             (mapcar (lambda (entry)
                       (cons (symbol-name (car entry)) (cdr entry)))
                     (plist-get report :counts))
             result)
    (dolist (field '(("valid-fixture-frame-counts" . :valid-fixture-frame-counts)
                     ("executed-native-counts" . :executed-native-counts)))
      (puthash (car field)
               (mapcar (lambda (entry) (cons (symbol-name (car entry)) (cdr entry)))
                       (plist-get report (cdr field))) result))
    (puthash "gap-classes"
             (mapcar (lambda (entry) (cons (car entry) (cdr entry)))
                     (plist-get report :gap-classes))
             result)
    (dolist (field '(("gaps" . :gaps) ("opcodes" . :opcodes)
                     ("malformed-probe-rebasing" . :malformed-probe-rebasing)))
      (puthash
       (car field)
       (vconcat
        (mapcar (lambda (row)
                  (let ((object (make-hash-table :test 'equal)))
                    (dolist (key '(:opcode :name :pc-class :status :source-disposition :decoder-status
                                   :frame-status :reason :evidence :diagnostic-status
                                   :diagnostic-frame-status :valid-frame-status))
                      (when (plist-member row key)
                        (puthash (substring (symbol-name key) 1)
                                 (let ((value (plist-get row key)))
                                   (cond ((null value) nil)
                                         ((symbolp value) (symbol-name value))
                                         (t value)))
                                 object)))
                    (when (plist-member row :valid-fixture)
                      (let* ((proof (plist-get row :valid-fixture))
                             (valid (and proof (make-hash-table :test 'equal))))
                        (when valid
                          (puthash "id" (plist-get proof :id) valid)
                          (puthash "frame-status" (symbol-name (plist-get proof :frame-status)) valid)
                          (puthash "reason" (plist-get proof :reason) valid)
                          (when (plist-get proof :shared-form-status)
                            (puthash "shared-form-status"
                                     (symbol-name (plist-get proof :shared-form-status)) valid)))
                        (puthash "valid-fixture" valid object)))
                    (when (plist-member row :backend-execution)
                      (let ((backends (make-hash-table :test 'equal)))
                        (dolist (entry (plist-get row :backend-execution))
                          (let ((proof (cdr entry)) (backend (make-hash-table :test 'equal)))
                            (puthash "backend" (symbol-name (plist-get proof :backend)) backend)
                            (puthash "fixture-id" (plist-get proof :fixture-id) backend)
                            (puthash "executed-native"
                                     (if (plist-get proof :executed-native) t :json-false) backend)
                            (puthash "evidence" (plist-get proof :evidence) backend)
                            (puthash (symbol-name (car entry)) backend backends)))
                        (puthash "backend-execution" backends object)))
                    object))
                (append (plist-get report (cdr field)) nil)))
       result))
    result))

(defun nelisp-bytecode-coverage-audit-main (&optional root)
  "Print bounded summary; pass `--json' and optional `--root PATH'."
  (let (cli-root jsonp)
    ;; Emacs resumes parsing this list after -f returns.  Consume our options
    ;; rather than merely inspecting them, or successful JSON ends in exit 255.
    (while command-line-args-left
      (pcase (pop command-line-args-left)
        ("--" nil)
        ("--json" (setq jsonp t))
        ("--root"
         (when cli-root (error "Duplicate --root option"))
         (unless (and command-line-args-left
                      (not (string-prefix-p "--" (car command-line-args-left))))
           (error "--root requires a repository path"))
         (setq cli-root (pop command-line-args-left)))
        (arg (error "Unknown audit option: %s" arg))))
    (let* ((report (nelisp-bytecode-coverage-audit-run (or root cli-root)))
         (counts (plist-get report :counts))
           (rebasing (plist-get report :malformed-probe-rebasing)))
    (if jsonp
        (princ (concat (json-encode (nelisp-bytecode-coverage-audit--json-object report)) "\n"))
      (princ (format "GNU Emacs 31.1 opcode audit: slots=%d named=%d counts=%S gaps=%d\n"
                     (plist-get report :opcode-count)
                     (plist-get report :named-opcode-count) counts
                     (length (plist-get report :gaps))))
      (princ (format "  gap-pc-classes=%S\n" (plist-get report :gap-classes)))
      (princ (format "  exclusion accounting: R=89->%d excluded=24->%d VALID=%d (original diagnostics retained)\n"
                     (cdr (assq 'runtime-op-pending counts))
                     (plist-get report :excluded-opcode-count)
                     (plist-get report :valid-opcode-count)))
      (princ (format "  VALID frame counts=%S executed-native=%S\n"
                     (plist-get report :valid-fixture-frame-counts)
                     (plist-get report :executed-native-counts)))
      (princ (format "  malformed-probe rebasing (separate): %S\n"
                     (mapcar (lambda (row)
                               (list (plist-get row :opcode)
                                     (plist-get row :diagnostic-frame-status)
                                     (plist-get row :valid-frame-status)))
                             (append rebasing nil))))
      (cl-loop for row in (append (plist-get report :gaps) nil)
               for index from 0 below 20
               do (princ (format "  %d %-24s %s\n"
                                 (plist-get row :opcode) (plist-get row :name)
                                 (plist-get row :status))))
      (when (> (length (plist-get report :gaps)) 20)
        (princ "  ... use --json for full gap list\n"))))))

(provide 'nelisp-bytecode-coverage-audit)
;;; nelisp-bytecode-coverage-audit.el ends here
