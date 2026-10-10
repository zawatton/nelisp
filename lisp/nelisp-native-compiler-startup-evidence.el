;;; nelisp-native-compiler-startup-evidence.el --- Constructor proof derivation -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'nelisp-native-rooted-build-evidence)
(declare-function nelisp-native-compiler-derived-startup-evidence-build nil
                  (units builder root directory))

(defconst nelisp-native-compiler-startup-evidence--roots
  '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
    "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots" "nl_gc_mark_thread_roots"
    "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow" "nl_native_cons_v2" "nl_alloc_symbol"))
(defconst nelisp-native-compiler-startup-evidence--exports
  '(("nl_arena_base" data 8) ("nl_alloc_symbol" func 3)
    ("nelisp_cons_construct" func 3) ("nl_native_cons_v2" func 5)
    ("nl_root_pin_begin_v2" func 1) ("nl_root_pin_end_v2" func 2)
    ("nl_root_pin_reserve_v2" func 2) ("nl_root_pin_slot_v2" func 3)))
(defvar nelisp-native-compiler-startup-evidence--template nil)
(defvar nelisp-native-compiler-startup-evidence--source-root nil)
(defvar nelisp-native-compiler-startup-evidence--boot-sources nil)
(defconst nelisp-native-compiler-startup-evidence--boot-modules
  '(nelisp-stdlib-fast-hash nelisp-env nelisp-lexframe nelisp-native-frame-v2
    nelisp-native-funcall-v2 nelisp-bytecode-ir nelisp-hash-custom nelisp-bytecode-native-switch
    nelisp-bytecode-frame-ir nelisp-bytecode-handlers-u8 nelisp-bytecode-compiler-input-dialect nelisp-native-poll nelisp-bytecode-compiler-input
    nelisp-bytecode-native-rooted-cfg nelisp-native-arithmetic-v2
    nelisp-bytecode-native-arithmetic-lowering nelisp-native-optimization-guard-v1
    nelisp-bytecode-native-guarded-lowering nelisp-bytecode-native-rooted-cfg-plan
    nelisp-bytecode-native-rooted-cfg-emit nelisp-bytecode-native-rooted-cfg-postdom
    nelisp-bytecode-native-rooted-cfg-shared-emit nelisp-bytecode-native-rooted-cfg-contract
    nelisp-bytecode-native-rooted-cfg-safe-contract
    nelisp-bytecode-native-rooted-cfg-constructor-contract))
(defconst nelisp-native-compiler-startup-evidence--post-modules
  '(nelisp-native-compiler-constructor-loader
    nelisp-bytecode-native-rooted-cfg-native))

(defconst nelisp-native-compiler-startup-evidence--tier1-modules
  '(nelisp-bytecode-ir nelisp-bytecode-frame-ir nelisp-bytecode-handlers-u8
    nelisp-bytecode-compiler-input nelisp-bytecode-native-rooted-cfg
    nelisp-native-arithmetic-v2 nelisp-bytecode-native-arithmetic-lowering
    nelisp-native-optimization-guard-v1 nelisp-bytecode-native-guarded-lowering
    nelisp-bytecode-native-rooted-cfg-plan nelisp-bytecode-native-rooted-cfg-emit
    nelisp-bytecode-native-rooted-cfg-postdom nelisp-bytecode-native-rooted-cfg-shared-emit
    nelisp-bytecode-native-rooted-cfg-native)
  "Pinned source dependencies loaded on demand, absent from Tier 0 startup.")

(defun nelisp-native-compiler-startup-evidence--source-path (feature)
  "Resolve the canonical loader's derived startup template separately."
  (if (eq feature 'nelisp-native-compiler-constructor-loader)
      "templates/nelisp-native-load-constructor-startup.el.in"
    (format "lisp/%s.el" feature)))

(defun nelisp-native-compiler-startup-evidence--loader-template-valid (root)
  "Verify reviewed identities and three exact canonical loader gate changes."
  (let* ((source (expand-file-name "lisp/nelisp-native-load.el" root))
         (template (expand-file-name "templates/nelisp-native-load-constructor-startup.el.in" root))
         (identity (expand-file-name "templates/nelisp-native-load-constructor-startup.el.identity.json" root))
         (names '(nelisp-native-load-root-v2-addresses nelisp-native-load-root-v2-copy nelisp-native-load-raw-v2-artifact))
         (json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
         (declaration (progn (nelisp-native-rooted-build-evidence-source-hash identity 65536) (json-read-file identity)))
         (canonical (nelisp-native-compiler-startup-evidence--forms source))
         (derived (nelisp-native-compiler-startup-evidence--forms template)) found)
    (unless (and (equal (nelisp-native-rooted-build-evidence-source-hash source 4194304)
                        "15f03e43723758043414e8778e28ff5813f0d75857c97e3b966e6d0e34877d32")
                 (equal (nelisp-native-rooted-build-evidence-source-hash template 4194304)
                        "a8449a1867585a0f388b9ba963ebffc9591309456ee8411cfa4f091e4ef67563")
                 (equal (alist-get 'source declaration) "lisp/nelisp-native-load.el")
                 (equal (alist-get 'source_sha256 declaration)
                        "15f03e43723758043414e8778e28ff5813f0d75857c97e3b966e6d0e34877d32")
                 (equal (alist-get 'output_sha256 declaration)
                        "a8449a1867585a0f388b9ba963ebffc9591309456ee8411cfa4f091e4ef67563")
                 (equal (alist-get 'definitions declaration) (mapcar #'symbol-name names))
                 (= (alist-get 'exact_runtime_gate_transforms declaration) 3))
      (error "Canonical constructor loader template provenance rejected"))
    (cl-labels ((collect (node)
                  (when (consp node)
                    (if (and (eq (car node) 'defun) (memq (cadr node) names)) (push node found)
                      (collect (car node)) (collect (cdr node)))))) (collect derived))
    (unless (= (length found) 3) (error "Unexpected constructor loader definition count"))
    (dolist (name names)
      (let ((original (cl-remove-if-not (lambda (form) (and (eq (car-safe form) 'defun) (eq (cadr form) name))) canonical))
            (published (cl-remove-if-not (lambda (form) (eq (cadr form) name)) found)) (changes 0))
        (cl-labels ((rewrite (node)
                      (cond ((equal node '(nelisp-runtime-reload-contract-matches-p))
                             (setq changes (1+ changes))
                             '(or (nelisp-runtime-reload-contract-matches-p) (manifest-runtime-p manifest)))
                            ((consp node) (cons (rewrite (car node)) (rewrite (cdr node)))) (t node))))
          (unless (and (= (length original) 1) (= (length published) 1)
                       (equal (rewrite (car original)) (car published)) (= changes 1))
            (error "Constructor loader gate differs from canonical owner: %S" name))))) t))

(defun nelisp-native-compiler-startup-evidence--emit (feature snapshot root &optional skip-feature)
  "Embed exact source-owned FEATURE bytes, optionally preserving an existing owner."
  (let* ((relative (nelisp-native-compiler-startup-evidence--source-path feature))
         (record (cl-find relative snapshot :key (lambda (item) (plist-get item :path)) :test #'equal)))
    (unless record (error "Missing authenticated compiler boot dependency"))
    (with-temp-buffer
      (set-buffer-multibyte nil) (insert-file-contents-literally (expand-file-name relative root))
      (unless (and (<= (buffer-size) 4194304)
                   (equal (plist-get record :sha256) (secure-hash 'sha256 (current-buffer))))
        (error "Compiler boot source changed during derivation: %s" relative))
      (let* ((original (decode-coding-string (buffer-string) 'utf-8))
             ;; Cold compiler builds derive each genuine initializer here,
             ;; before the proof and capability factories capture its owner.
             ;; Read canonical authenticated source, never adopt a generated
             ;; file's contents or a caller-supplied compiled representation.
             (derived
              (if (and (equal (getenv "NELISP_STANDALONE_NATIVE_COMPILER_COLD") "1")
                       (fboundp 'nelisp-native-optimizer-bytecode--project-form)
                       (memq feature nelisp-native-cache--compiler-modules))
                  (with-temp-buffer
                    (insert original) (goto-char (point-min))
                    (let (forms)
                      (condition-case nil
                          (while t
                            (push (car (nelisp-native-optimizer-bytecode--project-form
                                        (read (current-buffer)))) forms))
                        (end-of-file nil))
                      (erase-buffer)
                      (insert (substring original 0
                                         (or (string-match "\n" original)
                                             (length original))) "\n")
                      (let ((print-length nil) (print-level nil) (print-circle t) (print-gensym t)
                            (print-escape-newlines t) (print-escape-nonascii t))
                        (dolist (form (nreverse forms))
                          (prin1 form (current-buffer)) (insert "\n")))
                      (buffer-string)))
                original))
             (source (format "(let ((load-file-name %S) (buffer-file-name nil))\n (nelisp--eval-source-string %S))"
                             relative derived)))
        (if skip-feature (format "\n(unless (featurep '%S)\n %s)\n" feature source)
          (concat "\n" source "\n"))))))

(defun nelisp-native-compiler-startup-evidence--forms (path)
  "Read bounded source forms without entering the ticket module's private API."
  (unless (and (file-regular-p path)
               (<= (file-attribute-size (file-attributes path)) 4194304))
    (error "Missing or oversized constructor derivation source"))
  (with-temp-buffer
    (insert-file-contents path)
    (emacs-lisp-mode) (check-parens) (goto-char (point-min))
    (forward-comment (point-max))
    (let (forms)
      (while (< (point) (point-max))
        (push (read (current-buffer)) forms)
        (forward-comment (point-max)))
      (nreverse forms))))

(defun nelisp-native-compiler-startup-evidence--tier-emit (feature snapshot root)
  "Emit the authenticated owner before sealing; choose the tier once at startup."
  (let ((source (nelisp-native-compiler-startup-evidence--emit feature snapshot root t)))
    (if (memq feature nelisp-native-compiler-startup-evidence--tier1-modules)
        (format "\n(unless nelisp-startup-template-only %s)\n" source)
      source)))

(defun nelisp-native-compiler-startup-evidence--early-boot (root)
  "Derive the closed canonical boot list, never adopt caller source text."
  (let ((snapshot
         (mapcar (lambda (feature)
                   (let ((relative (nelisp-native-compiler-startup-evidence--source-path feature)))
                     (list :path relative :sha256
                           (nelisp-native-rooted-build-evidence-source-hash
                            (expand-file-name relative root) 4194304))))
                 nelisp-native-compiler-startup-evidence--boot-modules)))
    (mapconcat (lambda (feature)
                 (nelisp-native-compiler-startup-evidence--tier-emit feature snapshot root))
               nelisp-native-compiler-startup-evidence--boot-modules "")))

(defun nelisp-native-compiler-startup-evidence--rename (value)
  "Derive a separate namespace without borrowing the ticket issuance registry."
  (cond
   ((consp value)
    (cons (nelisp-native-compiler-startup-evidence--rename (car value))
          (nelisp-native-compiler-startup-evidence--rename (cdr value))))
   ((or (symbolp value) (stringp value))
    (let ((text (if (symbolp value) (symbol-name value) value)))
      (dolist (pair '(("nelisp-native-rooted-startup-evidence" . "nelisp-native-compiler-derived-startup-evidence")
                      ("nelisp-native-rooted-abi-proof" . "nelisp-native-compiler-runtime-proof")
                      ("nelisp-native-rooted-abi-evidence" . "nelisp-native-compiler-runtime-evidence")))
        (setq text (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) text t t)))
      (if (symbolp value) (intern text) text)))
   (t value)))

(defconst nelisp-native-compiler-startup-evidence--serializer-source
  (expand-file-name "../templates/nelisp-native-rooted-abi-proof.el.in"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-native-compiler-startup-evidence-serializer-form (name)
  "Return the template's owner-checked serializer, published under NAME."
  (unless (symbolp name) (error "Serializer definition name must be a symbol"))
  (let* ((original 'nelisp-native-rooted-abi-proof--data-bytes)
         (forms (nelisp-native-compiler-startup-evidence--forms
                 nelisp-native-compiler-startup-evidence--serializer-source))
         (matches
          (cl-remove-if-not
           (lambda (form)
             (and (eq (car-safe form) 'let*) (= (length form) 5)
                  (equal (car (cadr form)) '(lookup (symbol-function 'symbol-function)))
                  (eq (car-safe (nth 3 form)) 'defun)
                  (eq (cadr (nth 3 form)) original)
                  (equal (nth 4 form) `(setq self (funcall lookup ',original)))))
           forms)))
    (unless (= (length matches) 1) (error "Unknown template serializer structure"))
    (cl-labels ((rename (node)
                  (cond ((eq node original) name)
                        ((consp node) (cons (rename (car node)) (rename (cdr node))))
                        (t node))))
      (rename (car matches)))))

(defun nelisp-native-compiler-startup-evidence--rewrite (forms generator)
  "Apply exact structural constructor policy changes to parsed source FORMS."
  (let ((roots 0) (operations 0) (bounds 0) (offsets 0) (expected 0)
        (domains 0) (embeds 0) (serializers 0) (serializer-definitions 0) (proof-pins 0)
        (serializer (unless generator
                      (nelisp-native-compiler-startup-evidence-serializer-form
                       'nelisp-native-compiler-runtime-proof--data-bytes))))
    (cl-labels
        ((walk (object)
           (cond
            ((and generator
                  (equal object '(equal (list digest-count builder-count count-count bytes-count)
                                        '(1 1 1 1))))
             ;; Independent eligibility and the private transaction both carry pins.
             (setq proof-pins (1+ proof-pins))
             '(equal (list digest-count builder-count count-count bytes-count) '(2 1 1 1)))
            ((and serializer (equal object serializer))
             (setq serializers (1+ serializers))
             (cons (walk (car object)) (walk (cdr object))))
            ((and (consp object) (eq (car object) 'defun)
                  (eq (cadr object) 'nelisp-native-compiler-runtime-proof--data-bytes))
             (setq serializer-definitions (1+ serializer-definitions)) object)
            ((equal object '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2" "nl_root_pin_end_v2"
                             "nl_root_pin_slot_v2" "nl_gc_mark_pinned_roots"
                             "nl_gc_mark_thread_roots" "nl_gc_mark_recorded_env" "nl_cold_grow_chunk0" "nl_gc_conserv_owner_slow"))
             (setq roots (1+ roots)) (copy-sequence nelisp-native-compiler-startup-evidence--roots))
            ((equal object '(ticket gc)) (setq operations (1+ operations)) '(constructor))
            ((eq object 'ticket-gc-memory-v1)
             (setq domains (1+ domains)) 'compiler-constructor-memory-v1)
            ((equal object "nelisp-rooted-prelink-text-v1") "nelisp-compiler-constructor-prelink-v1")
            ((equal object "scripts/nelisp-native-rooted-prelink-closure.py")
             "scripts/nelisp-native-compiler-constructor-prelink.py")
            ((equal object "nelisp-rooted-elf-proof") "nelisp-compiler-constructor-proof")
            ((equal object "Require the reviewed ticket+GC closure; other operation eligibility is refused.")
             "Require the reviewed constructor closure; numeric and call eligibility remain refused.")
            ((equal object "Create an opaque ticket+GC memory proof after actual ownership checks.\nThis does not grant constructor, numeric or call operation eligibility.")
             "Create a separate opaque constructor proof after actual ownership checks.\nNumeric and call operation eligibility remain refused.")
            ((and generator (equal object '(template (expand-file-name "templates/nelisp-native-compiler-runtime-proof.el.in" root))))
             '(template nelisp-native-compiler-startup-evidence--template))
            ((equal object '(<= 1 (length (alist-get 'records closure)) 128))
             (setq bounds (1+ bounds)) '(<= 1 (length (alist-get 'records closure)) 192))
            ((equal object '(<= 15 (length functions) 128))
             (setq bounds (1+ bounds)) '(<= 15 (length functions) 192))
            ((equal object '(= (length offsets) 13))
             (setq offsets (1+ offsets)) '(= (length offsets) 14))
            ((and generator (consp object) (eq (car object) 'dolist)
                  (equal (cadr object)
                         '(relative '("lisp/nelisp-native-funcall-v2.el" "lisp/nelisp-runtime-reload-abi.el" "lisp/nelisp-native-load.el"
                                      "lisp/nelisp-native-raw-file.el"))))
             ;; The ticket startup has already established these owners. Re-evaluating
             ;; them would invalidate its captured owner identities before user code.
             (setq embeds (1+ embeds)) nil)
            ((and generator (equal object '(list script owner-script template loader
                    (expand-file-name "lisp/nelisp-native-funcall-v2.el" root)
                                                (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root)
                                                (expand-file-name "lisp/nelisp-native-raw-file.el" root))))
             '(list script owner-script template loader
                    (expand-file-name "lisp/nelisp-native-funcall-v2.el" root)
                    (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root)
                    (expand-file-name "lisp/nelisp-native-raw-file.el" root)
                    (expand-file-name "lisp/nelisp-native-compiler-startup-evidence.el" root)
                    (expand-file-name "lisp/nelisp-native-rooted-startup-evidence.el" root)
                    (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in" root)
                    (expand-file-name "scripts/nelisp-native-rooted-prelink-closure.py" root)))
            ((and generator (consp object) (eq (car object) 'list)
                  (equal (cl-subseq object 0 (min 5 (length object))) '(list :version 1 :layout layout)))
             (setq expected (1+ expected))
             (append (mapcar #'walk object)
                     '(:domain 'compiler-runtime-v1 :abi-sha256 (nelisp-runtime-reload-contract-hash)
                       :active-manifest-sha256 (alist-get 'active_manifest_sha256 closure)
                       :boot-sources nelisp-native-compiler-startup-evidence--boot-sources
                       :derivation-sources
                       (mapcar (lambda (relative)
                                 (list :path relative :sha256
                                       (nelisp-native-rooted-build-evidence-source-hash
                                        (expand-file-name relative nelisp-native-compiler-startup-evidence--source-root) 4194304)))
                               '("lisp/nelisp-native-compiler-startup-evidence.el"
                                 "lisp/nelisp-native-rooted-startup-evidence.el"
                                 "templates/nelisp-native-rooted-abi-proof.el.in"
                                 "scripts/nelisp-native-rooted-prelink-closure.py"
                                 "scripts/nelisp-native-compiler-constructor-prelink.py"
                                 "lisp/nelisp-native-compiler-runtime-capability.el"))
                       :compiler-exports nelisp-native-compiler-startup-evidence--exports)))
            ((consp object) (cons (walk (car object)) (walk (cdr object))))
            (t object))))
      (setq forms (mapcar #'walk forms)))
    (unless (equal (list roots operations bounds offsets expected domains embeds serializers serializer-definitions proof-pins)
                   (if generator '(2 1 1 0 1 1 1 0 0 1) '(1 1 1 1 0 1 0 1 1 0)))
      (error "Unknown constructor derivation source structure: %S"
             (list roots operations bounds offsets expected domains embeds serializers serializer-definitions proof-pins)))
    forms))

(defun nelisp-native-compiler-startup-evidence--checked-context (forms)
  "Fuse only compiler transaction checks, retaining independent public checks."
  (let ((definitions (make-hash-table :test #'eq)) (counts (make-hash-table :test #'eq)))
    (cl-labels
        ((collect (node)
           (when (consp node)
             (when (eq (car node) 'defun) (puthash (cadr node) node definitions))
             (collect (car node)) (collect (cdr node))))
         (replace (node old new key)
           (cond ((equal node old)
                  (puthash key (1+ (gethash key counts 0)) counts) new)
                 ((consp node) (cons (replace (car node) old new key)
                                    (replace (cdr node) old new key)))
                 (t node))))
      (collect forms)
      ;; Pin complete normalized definitions, not just rewritten expressions.
      (cl-mapc
       (lambda (name digest)
         (let ((definition (gethash name definitions))
               (print-length nil) (print-level nil) (print-circle nil))
           (unless (and definition (equal digest (secure-hash 'sha256 (prin1-to-string definition))))
             (error "Compiler checked-context definition drift: %S" name))))
       '(nelisp-native-compiler-runtime-proof--eligible-p
         nelisp-native-compiler-runtime-proof-dependency-context
         nelisp-native-compiler-runtime-proof-create nelisp-native-compiler-runtime-proof-valid-p)
       '("1fd91ab77de3671c255621175f978e323ca4e73a44cf39f8825abc80799a53c6"
         "d92f1faffec409da2260f525ed818e083219a6e423f8eb34e5ae356645bd8e70"
         "d826850d8b589e0699bfc9b696f6dff497f8dc4ba0b5ddb0b8a666c9dfbe9e23"
         "7513f39d103743d6941b23ec44a5f283151258141c07119de1dcd6c43a95f3c9"))
      (let* ((eligible (gethash 'nelisp-native-compiler-runtime-proof--eligible-p definitions))
             (context (gethash 'nelisp-native-compiler-runtime-proof-dependency-context definitions))
             (create (gethash 'nelisp-native-compiler-runtime-proof-create definitions))
             (valid (gethash 'nelisp-native-compiler-runtime-proof-valid-p definitions))
             (hash '(nelisp-native-compiler-runtime-proof--data-hash captured-evidence))
             (eligible-body (replace (nthcdr 4 eligible) hash
                                     `(setq compiler-transaction-hash ,hash) 'eligibility-hash))
             (context-body (replace (nthcdr 4 context) hash
                                    'compiler-transaction-hash 'context-hash))
             (initialization
              `(setq captured-checked-context
                     (lambda ()
                       (let ((compiler-transaction-hash nil))
                         (and (progn ,@eligible-body) (progn ,@context-body))))))
             (new-create
              (replace create
                       '(unless (funcall captured-eligibility)
                          (error "Root proof native/evidence ownership rejected"))
                       '(unless compiler-transaction-context
                          (error "Root proof native/evidence ownership rejected")) 'creation-eligibility))
             (new-valid (replace valid '(funcall captured-eligibility)
                                 '(setq compiler-transaction-context
                                        (funcall captured-checked-context)) 'validation-eligibility)))
        (unless (and eligible context create valid) (error "Missing compiler transaction definition"))
        ;; Bind before the original rejection and reuse only the initial context.
        (unless (and (eq (car (nth 5 new-create)) 'let*)
                     (equal (car (cadr (nth 5 new-create)))
                            '(context (nelisp-native-compiler-runtime-proof-dependency-context))))
          (error "Compiler initial context binding drift"))
        (setcar (cadr (nth 5 new-create)) '(context compiler-transaction-context))
        (puthash 'creation-context 1 counts)
        (setq new-create
              (append (cl-subseq new-create 0 4)
                      (list `(let ((compiler-transaction-context (funcall captured-checked-context)))
                               ,@(nthcdr 4 new-create)))))
        (setq new-valid
              (replace new-valid '(nelisp-native-compiler-runtime-proof-dependency-context)
                       'compiler-transaction-context 'validation-context))
        (setq new-valid
              (append (cl-subseq new-valid 0 4)
                      (list `(let ((compiler-transaction-context nil)) ,@(nthcdr 4 new-valid)))))
        (dolist (key '(eligibility-hash context-hash creation-eligibility creation-context
                      validation-eligibility validation-context))
          (unless (= (gethash key counts 0) 1)
            (error "Compiler transaction source drift: %S (%S)" key (gethash key counts 0))))
        (let ((scope-count 0))
          (cl-labels ((walk (node)
                        (cond
                         ((eq node create) new-create)
                         ((eq node valid) new-valid)
                         ((and (consp node) (eq (car node) 'let)
                               (assq 'captured-eligibility (cadr node)))
                          (setq scope-count (1+ scope-count))
                          (append (list 'let (append (cadr node) '((captured-checked-context nil))))
                                  (mapcar #'walk (cddr node)) (list initialization)))
                         ((consp node) (cons (walk (car node)) (walk (cdr node))))
                         (t node))))
            (let ((result (mapcar #'walk forms)))
              (unless (= scope-count 1) (error "Compiler owner scope drift"))
              result)))))))

(defun nelisp-native-compiler-startup-evidence--proof-api (forms)
  "Adapt the private derived registry to the operation-specific compiler API."
  (let ((valid-count 0) (registry-count 0) (owners-count 0))
    (cl-labels
        ((walk (object)
           (cond
            ((and (consp object) (eq (car object) 'defun)
                  (eq (cadr object) 'nelisp-native-compiler-runtime-proof-valid-p))
             (setq valid-count (1+ valid-count))
             (append (list 'defun (cadr object) '(proof &optional expected-layout) (nth 3 object)
                           '(setq expected-layout
                                  (or expected-layout (plist-get nelisp-native-compiler-runtime-proof--expected :layout))))
                     (nthcdr 4 object)))
            ((and (consp object) (eq (car object) 'let)
                  (equal (cadr object) '((issued-records (make-hash-table :test #'eq)))))
             (setq registry-count (1+ registry-count))
             (append (mapcar #'walk object)
                     '((defun nelisp-native-compiler-runtime-proof-owners-valid-p ()
                         "Authenticate the constructor boot schema and owners without issuing a proof."
                         (and (funcall captured-eligibility)
                              (nelisp-native-compiler-runtime-proof--protocol-p)
                              (eq (plist-get nelisp-native-compiler-runtime-proof--expected :domain)
                                  'compiler-runtime-v1)
                              (equal (plist-get nelisp-native-compiler-runtime-proof--expected
                                                :operation-eligibility) '(constructor))))
                       (defun nelisp-native-compiler-runtime-proof-metadata (proof)
                         "Return fresh scalar metadata for a genuine registered constructor proof."
                         (unless (nelisp-native-compiler-runtime-proof-valid-p proof)
                           (error "Invalid compiler constructor proof"))
                         (let* ((record (gethash proof issued-records))
                                (verified (plist-get record :verified))
                                (symbols (plist-get verified :symbols))
                                (expected nelisp-native-compiler-runtime-proof--expected)
                                exports)
                           (dolist (spec (plist-get expected :compiler-exports))
                             (let* ((name (car spec)) (symbol (gethash name symbols))
                                    (size (if (eq (cadr spec) 'data) 8
                                            (let ((items (plist-get expected :functions)) found)
                                              (while items
                                                (when (equal name (plist-get (car items) :name))
                                                  (setq found (plist-get (car items) :size)))
                                                (setq items (cdr items))) found))))
                               (unless (and symbol (integerp size) (> size 0))
                                 (error "Missing constructor export provenance"))
                               (push (append (list :name (substring name 0) :kind (cadr spec)
                                                   :address (plist-get symbol :address) :size size)
                                             (when (eq (cadr spec) 'func) (list :arity (nth 2 spec)))) exports)))
                           (list :version 1 :domain 'compiler-runtime-v1 :operation-eligibility '(constructor)
                                 :abi-sha256 (substring (plist-get expected :abi-sha256) 0)
                                 :binary-sha256 (substring (plist-get record :image) 0)
                                 :active-manifest-sha256 (substring (plist-get expected :active-manifest-sha256) 0)
                                 :exports (nreverse exports)))))))
            ((and (consp object) (eq (car object) 'setq)
                  (eq (cadr object) 'captured-module-owners))
             (setq owners-count (1+ owners-count))
             ;; The registry's new metadata reader must share the original owner seal.
             (let ((copy (copy-tree object)))
               (cl-labels ((add (node)
                             (when (consp node)
                               (when (and (eq (car node) 'quote)
                                          (memq 'nelisp-native-compiler-runtime-proof-create (cadr node)))
                                 (setcar (cdr node) (append (cadr node)
                                                          '(nelisp-native-compiler-runtime-proof-metadata
                                                            nelisp-native-compiler-runtime-proof-owners-valid-p
                                                            nth cadr cdr dolist nreverse append))))
                               (add (car node)) (add (cdr node)))))
                 (add copy)) copy))
            ((consp object) (cons (walk (car object)) (walk (cdr object))))
            (t object))))
      (setq forms (mapcar #'walk forms)))
    (unless (equal (list valid-count registry-count owners-count) '(1 1 1))
      (error "Unknown derived constructor registry structure"))
    (setq forms (nelisp-native-compiler-startup-evidence--checked-context forms))
    (let (reader (valid-mode 0) (metadata-mode 0) (registry-mode 0))
      (cl-labels
          ((collect (node)
             (when (consp node)
               (when (and (eq (car node) 'defun)
                          (eq (cadr node) 'nelisp-native-compiler-runtime-proof-metadata))
                 (unless (equal (nth 4 node)
                                '(unless (nelisp-native-compiler-runtime-proof-valid-p proof)
                                   (error "Invalid compiler constructor proof")))
                   (error "Compiler metadata validation drift"))
                 (setq reader `(lambda (proof) ,@(nthcdr 5 node))))
               (collect (car node)) (collect (cdr node))))
           (validator-result (node)
             (cond
              ((and (consp node) (eq (car node) 'and) (eq (cadr node) 'record))
               (unless (eq (car (last node)) t) (error "Compiler validation result drift"))
               (setq valid-mode (1+ valid-mode))
               (append '(and record (memq result-mode '(nil :metadata)))
                       (butlast (cddr node))
                       '((if (eq result-mode :metadata)
                             (funcall captured-proof-metadata proof) t))))
              ((consp node) (cons (validator-result (car node)) (validator-result (cdr node))))
              (t node)))
           (atomic (node)
             (cond
              ((and (consp node) (eq (car node) 'defun)
                    (eq (cadr node) 'nelisp-native-compiler-runtime-proof-valid-p))
               (unless (equal (nth 2 node) '(proof &optional expected-layout))
                 (error "Compiler validator signature drift"))
               (append (list 'defun (cadr node) '(proof &optional expected-layout result-mode)
                             (nth 3 node)) (validator-result (nthcdr 4 node))))
              ((and (consp node) (eq (car node) 'defun)
                    (eq (cadr node) 'nelisp-native-compiler-runtime-proof-metadata))
               (setq metadata-mode (1+ metadata-mode))
               `(defun ,(cadr node) (proof) ,(nth 3 node)
                  (or (nelisp-native-compiler-runtime-proof-valid-p proof nil :metadata)
                      (error "Invalid compiler constructor proof"))))
              ((and (consp node) (eq (car node) 'let)
                    (equal (cadr node) '((issued-records (make-hash-table :test #'eq)))))
               (setq registry-mode (1+ registry-mode))
               (append (list 'let (append (cadr node) '((captured-proof-metadata nil)))
                             `(setq captured-proof-metadata ,reader))
                       (mapcar #'atomic (cddr node))))
              ((consp node) (cons (atomic (car node)) (atomic (cdr node))))
              (t node))))
        (collect forms)
        (unless reader (error "Compiler metadata reader absent"))
        (setq forms (mapcar #'atomic forms)))
      (unless (equal (list valid-mode metadata-mode registry-mode) '(1 1 1))
        (error "Compiler atomic metadata source drift"))
      forms)))

(defun nelisp-native-compiler-startup-evidence-build (units builder root directory)
  "Generate a separate source-pinned constructor proof from current active UNITS."
  (nelisp-native-compiler-startup-evidence--loader-template-valid root)
  (let* ((template-path (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in" root))
         (generator-path (expand-file-name "lisp/nelisp-native-rooted-startup-evidence.el" root))
         (temporary (make-temp-file (expand-file-name "target/compiler-proof-template-" root) nil ".el.in"))
         (nelisp-native-compiler-startup-evidence--template temporary)
         (nelisp-native-compiler-startup-evidence--source-root root)
         (nelisp-native-compiler-startup-evidence--boot-sources
          (mapcar (lambda (feature)
                    (let ((relative (nelisp-native-compiler-startup-evidence--source-path feature)))
                      (list :path relative :sha256
                            (nelisp-native-rooted-build-evidence-source-hash
                             (expand-file-name relative root) 4194304))))
                  (append nelisp-native-compiler-startup-evidence--boot-modules
                          '(nelisp-native-compiler-runtime-capability)
                          nelisp-native-compiler-startup-evidence--post-modules)))
         (template (nelisp-native-compiler-startup-evidence--proof-api
                    (nelisp-native-compiler-startup-evidence--rewrite
                     (nelisp-native-compiler-startup-evidence--rename
                      (nelisp-native-compiler-startup-evidence--forms template-path)) nil)))
         (generator (nelisp-native-compiler-startup-evidence--rewrite
                     (nelisp-native-compiler-startup-evidence--rename
                      (nelisp-native-compiler-startup-evidence--forms generator-path)) t)))
    (let ((relative "templates/nelisp-native-load-constructor-startup.el.identity.json"))
      (setq nelisp-native-compiler-startup-evidence--boot-sources
            (append nelisp-native-compiler-startup-evidence--boot-sources
                    (list (list :path relative :sha256
                                (nelisp-native-rooted-build-evidence-source-hash
                                 (expand-file-name relative root) 65536))))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil) (print-circle nil))
              (dolist (form template) (prin1 form (current-buffer)) (insert "\n"))))
          (dolist (form generator) (eval form t))
          (let* ((wrapper (expand-file-name "lisp/nelisp-native-compiler-runtime-capability.el" root))
                 (wrapper-hash (nelisp-native-rooted-build-evidence-source-hash wrapper 4194304))
                 (source (nelisp-native-compiler-derived-startup-evidence-build units builder root directory))
                 boot)
            (dolist (record nelisp-native-compiler-startup-evidence--boot-sources)
              (let* ((relative (plist-get record :path))
                     (path (expand-file-name relative root))
                     (feature (intern (file-name-base relative))))
                (with-temp-buffer
                  (set-buffer-multibyte nil) (insert-file-contents-literally path)
                  (unless (equal (plist-get record :sha256) (secure-hash 'sha256 (current-buffer)))
                    (error "Compiler boot source changed during derivation: %s" relative))
                  (unless (or (and (not (equal (getenv "NELISP_STANDALONE_NATIVE_COMPILER_COLD") "1"))
                                   (memq feature nelisp-native-compiler-startup-evidence--tier1-modules))
                              (equal relative "templates/nelisp-native-load-constructor-startup.el.in")
                              (string-suffix-p ".json" relative)
                              (memq feature (cons 'nelisp-native-compiler-runtime-capability
                                                  nelisp-native-compiler-startup-evidence--post-modules)))
                    (push (nelisp-native-compiler-startup-evidence--tier-emit
                           feature nelisp-native-compiler-startup-evidence--boot-sources root)
                          boot)))))
            (with-temp-buffer
              (set-buffer-multibyte nil) (insert-file-contents-literally wrapper)
              (unless (equal wrapper-hash (secure-hash 'sha256 (current-buffer)))
                (error "Compiler capability changed during startup derivation"))
              (concat (mapconcat #'identity (nreverse boot) "") source
                      (format "\n(let ((load-file-name %S) (buffer-file-name nil))\n (nelisp--eval-source-string %S))\n"
                              "lisp/nelisp-native-compiler-runtime-capability.el"
                              (decode-coding-string (buffer-string) 'utf-8))
                      (mapconcat
                       (lambda (feature)
                         (nelisp-native-compiler-startup-evidence--emit
                          feature nelisp-native-compiler-startup-evidence--boot-sources root
                          (eq feature 'nelisp-bytecode-native-rooted-cfg-safe-contract)))
                       (cl-remove-if (lambda (feature) (memq feature nelisp-native-compiler-startup-evidence--tier1-modules))
                                     nelisp-native-compiler-startup-evidence--post-modules) "")))))
      (delete-file temporary))))

(provide 'nelisp-native-compiler-startup-evidence)
