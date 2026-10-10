;;; nelisp-native-rooted-startup-evidence.el --- Render build-bound proof startup -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'cl-seq)
(require 'cl-macs)
(require 'cl-extra)
(require 'json)
(require 'nelisp-native-rooted-build-evidence)
(require 'nelisp-native-load)
(require 'nelisp-native-compiler-startup-evidence)

(let ((original-owners nil) (original-checker nil)
      (lookup (symbol-function 'symbol-function))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error))
      (same (symbol-function 'eq)))
  (defun nelisp-native-rooted-startup-evidence--owners-valid-p ()
    "Check module-load owner identities before invoking generation helpers."
    (let ((remaining original-owners) (valid t))
      (while remaining
        (let* ((entry (funcall head remaining)) (name (funcall head entry)))
          (setq valid (funcall same (funcall tail entry) (funcall lookup name)))
          (if valid nil (funcall reject "Rooted generation owner changed: %S" name)))
        (setq remaining (funcall tail remaining)))
      valid))

(defun nelisp-native-rooted-startup-evidence--data-bytes (value)
  "Serialize bounded acyclic evidence without graph labels or truncation."
  (let ((work (list (cons value 0))) (seen (make-hash-table :test #'eq))
        (nodes 0) (bytes 0))
    (while work
      (let* ((item (pop work)) (object (car item)) (depth (cdr item)))
        (setq nodes (+ nodes 1))
        (unless (and (<= nodes 65536) (<= depth 64))
          (error "Root evidence node/depth bound exceeded"))
        (cond
         ((consp object)
          (when (gethash object seen) (error "Cyclic/shared root evidence rejected"))
          (puthash object t seen)
          (push (cons (car object) (+ depth 1)) work)
          (push (cons (cdr object) depth) work))
         ((stringp object)
          (setq bytes (+ bytes (string-bytes object)))
          (unless (and (<= (string-bytes object) 4096) (<= bytes 1048576))
            (error "Root evidence string bound exceeded")))
         ((or (symbolp object) (integerp object)) nil)
         (t (error "Invalid root evidence type")))))
    (let ((print-circle nil) (print-length nil) (print-level nil)
          (print-escape-newlines t) (print-escape-control-characters t)
          (print-escape-nonascii nil) (print-escape-multibyte nil)
          (print-quoted nil) (print-gensym nil))
      (prin1-to-string value))))

(defun nelisp-native-rooted-startup-evidence--data-hash (value)
  "Hash the bounded canonical evidence protocol, independent of printers."
  (secure-hash 'sha256 (nelisp-native-rooted-startup-evidence--data-bytes value)))

(defun nelisp-native-rooted-startup-evidence--context-equal-p (left right)
  "Compare bounded context data while preserving opaque function identity."
  (let ((nodes 0))
    (cl-labels ((walk (a b depth)
                  (setq nodes (1+ nodes))
                  (and (<= nodes 16384) (<= depth 64)
                       (cond ((or (functionp a) (functionp b)) (eq a b))
                             ((and (consp a) (consp b))
                              (and (walk (car a) (car b) (1+ depth))
                                   (walk (cdr a) (cdr b) depth)))
                             ((or (consp a) (consp b)) nil)
                             ((and (vectorp a) (vectorp b))
                              (and (= (length a) (length b)) (<= (length a) 4096)
                                   (let ((index 0) (valid t))
                                     (while (and valid (< index (length a)))
                                       (setq valid (walk (aref a index) (aref b index) (1+ depth))
                                             index (1+ index)))
                                     valid)))
                             (t (equal a b))))))
      (walk left right 0))))

(defun nelisp-native-rooted-startup-evidence--json (path limit)
  "Parse and hash the same bounded literal JSON bytes."
  (let ((size (file-attribute-size (file-attributes path))))
    (unless (and (integerp size) (<= 0 size limit))
      (error "Oversized generated JSON input"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (unless (= (buffer-size) size) (error "Generated JSON size changed"))
      (let ((bytes (buffer-string))
            (json-array-type 'list) (json-object-type 'alist) (json-key-type 'symbol))
        (cons (json-read-from-string (decode-coding-string bytes 'utf-8))
              (secure-hash 'sha256 bytes))))))

(defun nelisp-native-rooted-startup-evidence--forms (path)
  "Return parsed forms and digest from the same bounded template bytes."
  (unless (<= (file-attribute-size (file-attributes path)) 4194304)
    (error "Oversized proof template"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (unless (<= (buffer-size) 4194304) (error "Oversized proof template"))
    (let ((digest (secure-hash 'sha256 (current-buffer))) forms)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (emacs-lisp-mode)
      (check-parens)
      (goto-char (point-min))
      (forward-comment (point-max))
      (while (< (point) (point-max))
        (push (read (current-buffer)) forms)
        (forward-comment (point-max)))
      (cons (nreverse forms) digest))))

(defun nelisp-native-rooted-startup-evidence--source (path expected-hash)
  "Decode only the exact bounded bytes authenticated for embedding."
  (with-temp-buffer
    (unless (<= (file-attribute-size (file-attributes path)) 4194304)
      (error "Oversized startup source"))
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (unless (and (<= (buffer-size) 4194304)
                 (equal expected-hash (secure-hash 'sha256 (current-buffer))))
      (error "Embedded startup bytes differ from authenticated source"))
    (decode-coding-string (buffer-string) 'utf-8)))

(defun nelisp-native-rooted-startup-evidence--numeric-union-p (closure roots binding)
  "Authenticate a selected union certificate without granting numeric admission."
  (and (equal roots '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
                     "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots" "nl_gc_mark_thread_roots"
                     "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow" "nl_native_cons_v2" "nl_alloc_symbol"))
       (stringp binding) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" binding)
       (equal binding (alist-get 'source_binding_sha256 closure))
       (= (or (alist-get 'helper_count_policy closure) 0) 192)
       (null (alist-get 'operation_eligibility closure))
       (equal (alist-get 'roots closure)
              (append roots '("wf_any_float_arith" "wf_first_non_number_or_bignum"
                              "wf_fsum" "wf_copy32" "wf_sum" "bf_wrong_type_number_or_marker")))))

(defun nelisp-native-rooted-startup-evidence-render
    (capture closure-path closure-sha256 template layout &optional selection reviewed-binding)
  "Render evidence and literal proof pins from an authenticated active build.
CAPTURE comes from the actual active-unit writer. CLOSURE-SHA256 must be the
digest returned by the verified prelink subprocess. LAYOUT is the genuine
public loader owner's build-time snapshot, never a runtime caller certificate.
Return startup source and its manifest; constructor/numeric/call remain refused."
  (unless (funcall original-checker)
    (error "Rooted generation helper ownership changed"))
  (let* ((manifest-path (plist-get capture :manifest))
         (manifest-input (nelisp-native-rooted-startup-evidence--json manifest-path 65536))
         (manifest (car manifest-input))
         (metadata-path (plist-get capture :metadata))
         (data-path (plist-get capture :generated-data))
         (metadata-input (nelisp-native-rooted-startup-evidence--json metadata-path 4194304))
         (data-input (nelisp-native-rooted-startup-evidence--json data-path 1048576))
         (closure-input (nelisp-native-rooted-startup-evidence--json closure-path 4194304))
         (metadata (car metadata-input)) (data (car data-input)) (closure (car closure-input))
         (roots '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
                  "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots"
                  "nl_gc_mark_thread_roots" "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow"))
         (builder-hash (alist-get 'builder-sha256 manifest))
         (template-input (nelisp-native-rooted-startup-evidence--forms template))
         (template-hash (cdr template-input)) (forms (car template-input))
         records units data-targets offsets expected pin)
    (unless (or (null selection) (eq selection 'numeric-union))
      (error "Unknown closure selection"))
    (unless (and (equal closure-sha256 (cdr closure-input))
                 (if selection
                     (and (equal (alist-get 'domain closure) "nelisp-compiler-numeric-prelink-v1")
                          (nelisp-native-rooted-startup-evidence--numeric-union-p closure roots reviewed-binding))
                   (equal (alist-get 'domain closure) "nelisp-rooted-prelink-text-v1"))
                 (equal (alist-get 'status closure) "PRELINK_DIRECT_CLOSURE_PASS")
                 (or selection (equal (alist-get 'roots closure) roots))
                 (equal (alist-get 'active_manifest_sha256 closure)
                        (cdr manifest-input))
                 (equal (alist-get 'metadata-sha256 manifest)
                        (cdr metadata-input))
                 (equal (alist-get 'generated-data-sha256 manifest)
                        (cdr data-input))
                 (equal (alist-get 'owner-source-sha256 data) builder-hash)
                 (<= 1 (length (alist-get 'records closure)) 128)
                 (<= (alist-get 'total_bytes closure) 524288)
                 (if (nelisp-native-load--windows-p)
                     (and (equal (plist-get layout :domain) "nelisp-rooted-pe-v2")
                          (eq (plist-get layout :target) 'x86_64-windows)
                          (eq (plist-get layout :calling-convention) 'win64))
                   (and (equal (plist-get layout :domain) "nelisp-rooted-elf-v2")
                        (eq (plist-get layout :target) 'x86_64-linux)))
                 (= (plist-get layout :sexp-bytes) 32)
                 (= (length (plist-get layout :exports)) 17))
      (error "Unauthenticated protocol generation inputs"))
    (dolist (record (alist-get 'records closure))
      (let* ((name (alist-get 'name record))
             (unit-name (alist-get 'unit record))
             (unit (cl-find unit-name metadata :key (lambda (item) (alist-get 'name item)) :test #'equal))
             (start (alist-get 'unit_offset record)) (size (alist-get 'size record)) relocations)
        (unless (and unit (not (alist-get 'indirect record)) (<= 1 size 65536))
          (error "Unknown protocol helper ownership"))
        (dolist (relocation (alist-get 'relocations unit))
          (let ((offset (alist-get 'offset relocation)))
            (when (and (<= start offset) (< offset (+ start size)))
              (push (list :offset (- offset start) :width 4
                          :type (intern (alist-get 'type relocation))
                          :symbol (alist-get 'symbol relocation)
                          :addend (alist-get 'addend relocation)) relocations))))
        (push (list :name name :size size :sha256 (alist-get 'normalized_sha256 record)
                    :relocations (nreverse relocations) :direct (alist-get 'direct record) :indirect nil) records)
        (cl-pushnew (list :name unit-name :sha256 (alist-get 'unit-sha256 unit)) units :test #'equal)))
    (dolist (record records)
      (dolist (relocation (plist-get record :relocations))
        (unless (or (member (plist-get relocation :symbol) (alist-get 'os_imports closure))
                    (cl-find (plist-get relocation :symbol) records
                         :key (lambda (item) (plist-get item :name)) :test #'equal))
          (cl-pushnew (plist-get relocation :symbol) data-targets :test #'equal))))
    (dolist (name (sort data-targets #'string<))
      (let ((symbol (cl-find name (alist-get 'symbols data)
                            :key (lambda (item) (alist-get 'name item)) :test #'equal)))
        (unless (and symbol (equal (alist-get 'section symbol) "bss")
                     (<= 0 (alist-get 'value symbol))
                     (< (alist-get 'value symbol) (alist-get 'bss-size data)))
          (error "Unknown generated protocol data target"))
        (push (cons name (alist-get 'value symbol)) offsets)))
    (setq expected
          (list :version 1 :layout layout :environment-arena-offset nil
                :bss-size (alist-get 'bss-size data) :bss-offsets (nreverse offsets)
                :units (nreverse units) :functions (nreverse records)
                :bss-owner (list :unit (alist-get 'unit data) :origin 'generated-data-unit
                                 :builder-sha256 builder-hash
                                 :generated-forms-sha256 (alist-get 'owner-forms-sha256 data))
                :protocol-domain 'ticket-gc-memory-v1 :protocol-roots roots
                :operation-eligibility '(ticket gc)
                :arena-protocol (list :header-domain 'single-u64-low32-size-low3-mark
                                       :initial-chunk-bytes (if (nelisp-native-load--windows-p)
                                                                (alist-get 'initial-chunk-bytes data) 268435456)
                                      :builder-sha256 builder-hash)))
    (when (nelisp-native-load--windows-p)
      (unless (equal (alist-get 'os_imports closure) '("ExitProcess" "VirtualAlloc" "VirtualFree"))
        (error "Win64 kernel terminal policy differs"))
      (setq expected (append expected (list :os-imports (alist-get 'os_imports closure)))))
    (when selection
      (setq expected (append expected
                             (list :numeric-union-certificate-sha256 closure-sha256
                                   :numeric-source-binding-sha256 reviewed-binding))))
    (setq pin (nelisp-native-rooted-startup-evidence--data-hash expected))
    ;; Rewrite only the four literal source pins in the reviewed template.
    ;; Unknown template shapes refuse rather than producing weaker guards.
    (let ((digest-count 0) (builder-count 0) (count-count 0) (bytes-count 0))
      (cl-labels ((rewrite (object)
                    (cond
                     ((equal (car-safe object) 'equal)
                      (cond
                       ((and (eq (cadr object) 'captured-evidence-hash) (stringp (nth 2 object)))
                        (setq digest-count (1+ digest-count)) (setcar (cddr object) pin))
                       ((and (equal (cadr object) '(plist-get arena :builder-sha256))
                             (stringp (nth 2 object)))
                        (setq builder-count (1+ builder-count)) (setcar (cddr object) builder-hash))))
                     ((and (eq (car-safe object) '=) (equal (cadr object) '(length functions)))
                      (setq count-count (1+ count-count))
                      (setcar (cddr object) (length (plist-get expected :functions))))
                     ((and (eq (car-safe object) '=) (eq (cadr object) 'total))
                      (setq bytes-count (1+ bytes-count))
                      (setcar (cddr object) (alist-get 'total_bytes closure))))
                    (when (consp object) (rewrite (car object)) (rewrite (cdr object)))))
        (dolist (form forms) (rewrite form)))
      (unless (equal (list digest-count builder-count count-count bytes-count) '(1 1 1 1))
        (error "Unknown proof template pin structure")))
    (unless (and (funcall original-checker)
                 (equal template-hash (nelisp-native-rooted-build-evidence-source-hash template 4194304)))
      (error "Proof template changed during generation"))
    (dolist (input (list (list manifest-path 65536 (cdr manifest-input))
                        (list metadata-path 4194304 (cdr metadata-input))
                        (list data-path 1048576 (cdr data-input))
                        (list closure-path 4194304 (cdr closure-input))))
      (unless (equal (nth 2 input)
                     (nelisp-native-rooted-build-evidence-source-hash (car input) (cadr input)))
        (error "Generated JSON changed during rendering")))
    (list :evidence-sha256 pin :template-sha256 template-hash
          :active-manifest-sha256 (alist-get 'active_manifest_sha256 closure)
          :startup-source
          (with-temp-buffer
            (let ((print-circle nil) (print-length nil) (print-level nil))
              (prin1 `(defun nelisp-native-rooted-abi-evidence () (quote ,expected)) (current-buffer))
              (insert "\n(provide 'nelisp-native-rooted-abi-evidence)\n")
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
            (buffer-string)))))

(defun nelisp-native-rooted-startup-evidence-build (units builder root directory &optional compiler-boot)
  "Generate startup source from actual UNITS before compiling their driver.
The loader supplies the genuine public layout and owner context. Source
dependencies, subprocess result and source owners are checked before return."
  (unless (funcall original-checker)
    (error "Rooted generation helper ownership changed"))
  (unless (and (fboundp 'nelisp-native-load-rooted-production-contract)
               (fboundp 'nelisp-native-load-rooted-runtime-dependency-context))
    (error "Missing genuine rooted production layout owner"))

  (when (and compiler-boot
             (not (assq 'nelisp-native-optimizer-bytecode--project-form original-owners)))
    (error "Compiler projection must precede generation owner capture"))
  (let* ((layout-owner (symbol-function 'nelisp-native-load-rooted-production-contract))
         (context-owner (symbol-function 'nelisp-native-load-rooted-runtime-dependency-context))
         (context (funcall context-owner))
         (layout (car (funcall layout-owner)))
         (script (expand-file-name "scripts/nelisp-native-rooted-prelink-closure.py" root))
         (owner-script (expand-file-name "scripts/nelisp-native-rooted-direct-closure.py" root))
         (template (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in" root))
         (loader (expand-file-name "lisp/nelisp-native-load.el" root))
         (python (executable-find (if (eq system-type 'windows-nt) "python" "python3")))
         (source-paths (list (expand-file-name "lisp/nelisp-native-windows.el" root)
                             (expand-file-name "lisp/nelisp-native-pe-symbols.el" root)
                             script owner-script template loader
                             (expand-file-name "lisp/nelisp-native-funcall-v2.el" root)
                             (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root)
                             (expand-file-name "lisp/nelisp-native-raw-file.el" root)))
         (sources (mapcar (lambda (path)
                            (cons path (nelisp-native-rooted-build-evidence-source-hash path 4194304)))
                          source-paths)))
    (unless (and python
                 (equal (file-truename (or (symbol-file 'nelisp-native-load-rooted-production-contract 'defun) ""))
                        (file-truename loader)))
      (error "Unknown rooted build tool or loader source owner"))
    (let* ((capture (nelisp-native-rooted-build-evidence-write units builder root directory))
           (closure (expand-file-name "prelink-closure.json" directory))
           (arguments (list script "--manifest" (plist-get capture :manifest)
                            "--source-root" root "--metadata" (plist-get capture :metadata)
                            "--unit-directory" directory "--generated-data" (plist-get capture :generated-data)
                            "--output" closure))
           result startup)
      (dolist (name '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
                      "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots"
                      "nl_gc_mark_thread_roots" "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow"))
        (setq arguments (append arguments (list "--root" name))))
      (with-temp-buffer
        (unless (and (eq (apply #'call-process python nil (list (current-buffer) t) nil arguments) 0)
                     (string-match-p "\\`[0-9a-f]\\{64\\}\n\\'" (buffer-string)))
          (error "Authenticated prelink closure generation failed: %s"
                 (substring (buffer-string) 0 (min 2000 (buffer-size)))))
        (setq result (nelisp-native-rooted-startup-evidence-render
                      capture closure (substring (buffer-string) 0 64) template layout)))
      (dolist (relative (append (when (nelisp-native-load--windows-p)
                                  '("lisp/nelisp-native-windows.el" "lisp/nelisp-native-pe-symbols.el"))
                                '("lisp/nelisp-native-funcall-v2.el" "lisp/nelisp-runtime-reload-abi.el" "lisp/nelisp-native-load.el"
                          "lisp/nelisp-native-raw-file.el")))
        (when (and compiler-boot (equal relative "lisp/nelisp-native-load.el"))
          ;; This closed source list is derived and hash-checked by the build
          ;; owner; no caller-supplied prefix or generated file is adopted.
          (push (nelisp-native-compiler-startup-evidence--early-boot root) startup))
        (let* ((path (expand-file-name relative root))
               (expected (cdr (assoc path sources))))
          (push
           (if (and compiler-boot (equal relative "lisp/nelisp-native-load.el"))
               ;; Derive the genuine initializer at its original load slot,
               ;; before any proof captures its owner.  Never replace a sealed
               ;; owner later or adopt a generated projection's source text.
               (nelisp-native-compiler-startup-evidence--emit
                'nelisp-native-load (list (list :path relative :sha256 expected)) root)
             (let ((source (nelisp-native-rooted-startup-evidence--source path expected)))
               (format "\n(let ((load-file-name %S) (buffer-file-name nil))\n (nelisp--eval-source-string %S))\n"
                       relative source)))
           startup)))
      (setq startup (concat (mapconcat #'identity (nreverse startup) "")
                            (format "\n(let ((load-file-name %S) (buffer-file-name nil))\n (nelisp--eval-source-string %S))\n"
                                    "lisp/nelisp-native-rooted-abi-proof.el"
                                    (plist-get result :startup-source))))
      (unless (and (funcall original-checker)
                   (eq layout-owner (symbol-function 'nelisp-native-load-rooted-production-contract))
                   (eq context-owner (symbol-function 'nelisp-native-load-rooted-runtime-dependency-context))
                   (nelisp-native-rooted-startup-evidence--context-equal-p context (funcall context-owner))
                   (cl-every (lambda (source)
                               (equal (cdr source)
                                      (nelisp-native-rooted-build-evidence-source-hash (car source) 4194304)))
                             sources))
        (error "Rooted startup source or layout ownership changed"))
      (with-temp-file (expand-file-name "startup-generation.json" directory)
        (insert (json-encode
                 (list :domain "nelisp-rooted-startup-generation-v1"
                       :active-manifest-sha256 (plist-get result :active-manifest-sha256)
                       :evidence-sha256 (plist-get result :evidence-sha256)
                       :template-sha256 (plist-get result :template-sha256)
                       :sources (vconcat (mapcar (lambda (item) (list :path (file-relative-name (car item) root)
                                                                      :sha256 (cdr item))) sources)))) "\n"))
      startup)))

  ;; Issuance authority is the module-load owner set, never a sampled current set.
  (setq original-checker (funcall lookup 'nelisp-native-rooted-startup-evidence--owners-valid-p)
        original-owners
        (mapcar (lambda (name) (cons name (funcall lookup name)))
                (append
                 (when (fboundp 'nelisp-native-optimizer-bytecode--project-form)
                   '(nelisp-native-optimizer-bytecode--project-form
                     nelisp-native-optimizer-bytecode--compile
                     nelisp-native-optimizer-bytecode--source
                     nelisp-native-optimizer-bytecode--normalize))
                 '(nelisp-native-compiler-startup-evidence--source-path
                  nelisp-native-compiler-startup-evidence--early-boot
                  nelisp-native-compiler-startup-evidence--tier-emit
                  nelisp-native-compiler-startup-evidence--emit
                  nelisp-native-rooted-startup-evidence--owners-valid-p
                  nelisp-native-rooted-startup-evidence--context-equal-p
                  nelisp-native-rooted-startup-evidence--json
                  nelisp-native-rooted-startup-evidence--forms
                  nelisp-native-rooted-startup-evidence--source
                  nelisp-native-rooted-startup-evidence--data-bytes
                  nelisp-native-rooted-startup-evidence--data-hash
                  nelisp-native-rooted-startup-evidence-render
                  nelisp-native-rooted-startup-evidence--numeric-union-p
                  nelisp-native-rooted-startup-evidence-build
                  nelisp-native-rooted-build-evidence-source-hash
                  nelisp-native-rooted-build-evidence-write
                  nelisp-native-load-rooted-production-contract
                  nelisp-native-load-rooted-runtime-dependency-context
                  symbol-function eq equal functionp secure-hash error
                  = <= < + - 1+ setcar fboundp buffer-size set-buffer-multibyte null append
                  json-read-from-string decode-coding-string
                  insert-file-contents-literally file-attributes file-attribute-size
                  call-process cl-every mapcar plist-get alist-get
                  car cdr caar cdar cadr cddr nth car-safe consp vectorp assoc
                  aref length integerp stringp symbolp string-bytes
                  make-hash-table gethash puthash cons list nreverse push
                  cl-labels cond and or when unless pop dolist cl-find cl-pushnew
                  read check-parens forward-comment json-encode assq
                  insert-file-contents decode-coding-region emacs-lisp-mode
                  file-truename symbol-file executable-find
                  file-relative-name expand-file-name substring string-match-p
                  prin1-to-string prin1 sort string< mapconcat identity format concat)))))

(provide 'nelisp-native-rooted-startup-evidence)
