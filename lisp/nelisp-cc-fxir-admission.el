;;; nelisp-cc-fxir-admission.el --- Optional harvested application rows -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Host-only, opt-in source admission.  A producer receipt records the source
;; files and real harvest transcript supplied by the caller.  It is provenance,
;; not a semantic proof, native execution witness, or resource qualification.
;; The ordinary runtime installer still compares code, FXIR, callees and arity.
;; Extra files are parsed as literal data once; no extra file is loaded/evaled.

;;; Code:
(require 'cl-lib)
(require 'lisp-mode)
(require 'nelisp-cc-fxir-analysis)
(require 'nelisp-cc-fxir-native)

(defconst nelisp-cc-fxir-admission-schema 1)
(defconst nelisp-cc-fxir-admission-generation-policy
  '(:version 1 :max-code-index 1048575
    :per-row ((nlocals . 512) (nfixed . 64) (maxargc . 64) (fsize-upper . 192)
              (nodes . 2048) (calls . 256) (handlers . 32) (bindings . 64)
              (windows . 512) (staged-args . 2048))
    :aggregate ((nlocals . 4096) (nfixed . 512) (maxargc . 512) (fsize-upper . 4096)
                (nodes . 16384) (calls . 2048) (handlers . 128) (bindings . 256)
                (windows . 4096) (staged-args . 8192)))
  "Conservative optional-admission generation ceilings, not measured build caps.
Only added rows are charged. Handler/window overcharging is deliberate;
passing this policy supplies no runtime, code-size or speed qualification.")
(defconst nelisp-cc-fxir-admission--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defconst nelisp-cc-fxir-admission--cache-consumers
  '((:consumer nelisp-standalone--unit-cache-key
     :inputs (:generated-source :toolchain-digest :compile-mode :inline-settings :imports)
     :limit "Receipt identity is not an independent unit-cache key input.")
    (:consumer nelisp-cc-fxir-native-install-table-source
     :inputs (:same-validated-row-snapshot)
     :limit "Source install table is not proof of native linkage or execution.")
    (:consumer nelisp-native-rooted-build-evidence-write
     :inputs (:active-unit-bytes :builder-source :symbols :relocations)
     :limit "Post-build evidence must be regenerated; admission supplies none.")
    (:consumer nelisp-standalone--artifact-runtime-cache-path
     :inputs (:separate-runtime-bundle)
     :limit "Existing .nelc runtime caches are outside this admission contract.")))

(defun nelisp-cc-fxir-admission--print (data)
  "Print DATA canonically without truncation or pointer identities."
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-quoted t) (print-gensym t) (print-escape-nonascii t))
    (prin1-to-string data)))

(defun nelisp-cc-fxir-admission--digest (data)
  (secure-hash 'sha256 (nelisp-cc-fxir-admission--print data)))

(defun nelisp-cc-fxir-admission--bytes (file &optional limit)
  "Read bounded FILE once, as bytes."
  (unless (and (file-regular-p file)
               (<= (file-attribute-size (file-attributes file)) (or limit 1048576)))
    (error "Missing or oversized admission input: %s" file))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (unless (<= (buffer-size) (or limit 1048576)) (error "Admission input grew"))
    (buffer-string)))

(defun nelisp-cc-fxir-admission--pin (file &optional limit)
  (list :file (file-truename file) :sha256
        (secure-hash 'sha256 (nelisp-cc-fxir-admission--bytes file limit))
        :limit (or limit 1048576)))

(defun nelisp-cc-fxir-admission--check-pins (pins)
  (dolist (pin pins)
    (unless (equal (plist-get pin :sha256)
                   (secure-hash 'sha256
                                (nelisp-cc-fxir-admission--bytes
                                 (plist-get pin :file) (plist-get pin :limit))))
      (error "Stale admission input: %s" (plist-get pin :file)))))

(defun nelisp-cc-fxir-admission--forms (bytes)
  "Read forms from BYTES without evaluation or circular reader syntax."
  (with-temp-buffer
    (insert (decode-coding-string bytes 'utf-8-unix))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let ((read-circle nil) (read-symbol-shorthands nil) forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))

(defun nelisp-cc-fxir-admission--literal (data)
  "Reject nonliteral, shared, circular, deep or oversized row data."
  (let ((todo (list (cons data 0))) (seen (make-hash-table :test 'eq)) (n 0))
    (while todo
      (pcase-let* ((`(,value . ,depth) (pop todo)))
        (setq n (1+ n))
        (when (or (> n 100000) (> depth 64)) (error "Admission literal budget"))
        (cond ((or (null value) (symbolp value) (integerp value)) nil)
              ((consp value)
               (when (gethash value seen) (error "Shared admission literal"))
               (puthash value t seen)
               (push (cons (car value) (1+ depth)) todo)
               ;; List spines do not charge semantic nesting depth.
               (push (cons (cdr value) depth) todo))
              (t (error "Nonliteral admission row value")))))))

(defun nelisp-cc-fxir-admission--generation-budget (rows)
  "Validate ROWS before generation and return deterministic budget evidence.
Never call the native lowerer. Fsize upper bound is 3 + MAXARGC + NFIXED
+ 3 * HANDLERS. Unlike the current lowerer's actual catch allocation, every
catch, condition-case and unwind-protect is conservatively charged. Calls,
inline arithmetic, mutable reads and fixed parameters charge window sites."
  (unless (and (proper-list-p rows) (<= 1 (length rows) 256))
    (error "Application row count bound"))
  (let (records totals)
    (dolist (row rows)
      (unless (and (proper-list-p row) (= (length row) 6)
                   (integerp (cadr row)) (<= 0 (cadr row) 1073741823)
                   (integerp (nth 2 row)) (<= 0 (nth 2 row) 512)
                   (proper-list-p (nth 5 row)) (= (length (nth 5 row)) 3)
                   (cl-every #'integerp (nth 5 row)))
        (error "Admission generation scalar bound"))
      (let* ((arity (nth 5 row))
             (nfixed (if (>= (nth 2 arity) 0) (nth 2 arity) (nth 1 arity)))
             (metrics (list (cons 'nlocals (nth 2 row)) (cons 'nfixed nfixed)))
             (todo (list (nth 4 row))))
        (unless (and (<= 0 nfixed 64)
                     (plist-get (nelisp-cc-fxir-analysis-row row) :valid))
          (error "Invalid or oversized admission row"))
        (cl-labels ((charge (key amount)
                      (setf (alist-get key metrics) (+ (or (alist-get key metrics) 0) amount))))
          (while todo
            (let ((item (pop todo)))
              (cond
               ((integerp item)
                (unless (<= 0 item (plist-get nelisp-cc-fxir-admission-generation-policy :max-code-index))
                  (error "Admission code operand bound")))
               ((consp item)
                (when (keywordp (car item)) (charge 'nodes 1))
                (pcase (car item)
                  ((or :call :inline)
                   (let ((argc (if (eq (car item) :call) (nth 2 item) (- (length item) 3))))
                     (unless (and (integerp argc) (<= 0 argc 64))
                       (error "Admission call argument bound"))
                     (setf (alist-get 'maxargc metrics) (max argc (or (alist-get 'maxargc metrics) 0)))
                     (charge 'staged-args argc) (charge 'windows 1)
                     (when (eq (car item) :call) (charge 'calls 1))))
                  ((or :varref :alias)
                   ;; Conservative allowance for current/adjacent callout shapes.
                   (setf (alist-get 'maxargc metrics) (max 2 (or (alist-get 'maxargc metrics) 0)))
                   (charge 'windows 1))
                  ((or :catch :condition-case :unwind-protect)
                   (charge 'handlers 1) (charge 'windows 1)
                   (when (eq (car item) :condition-case)
                     (charge 'handlers (length (nthcdr 4 item)))))
                  (:varbind (charge 'bindings 1)))
                (dolist (child item) (when (or (consp child) (integerp child)) (push child todo)))))))
          (when (> nfixed 0) (charge 'windows 1))
          (charge 'fsize-upper (+ 3 nfixed (or (alist-get 'maxargc metrics) 0)
                                  (* 3 (or (alist-get 'handlers metrics) 0))))
          (dolist (bound (plist-get nelisp-cc-fxir-admission-generation-policy :per-row))
            (let* ((key (car bound)) (value (or (alist-get key metrics) 0)))
              (when (> value (cdr bound)) (error "Admission per-row %S budget" key))
              (setf (alist-get key totals) (+ value (or (alist-get key totals) 0)))))
          (push (list (car row) metrics) records))))
    (dolist (bound (plist-get nelisp-cc-fxir-admission-generation-policy :aggregate))
      (when (> (or (alist-get (car bound) totals) 0) (cdr bound))
        (error "Admission aggregate %S budget" (car bound))))
    (list :policy (copy-tree nelisp-cc-fxir-admission-generation-policy)
          :rows (nreverse records) :totals totals)))

(defun nelisp-cc-fxir-admission--read-rows (file constant expected)
  "Read only the harvester's literal defconst/provide format."
  (let* ((bytes (nelisp-cc-fxir-admission--bytes file))
         (forms (nelisp-cc-fxir-admission--forms bytes))
         (definition (car forms)))
    (unless (and (equal expected (secure-hash 'sha256 bytes))
                 (symbolp constant) constant (not (keywordp constant))
                 (proper-list-p definition) (= (length definition) 3)
                 (eq (car definition) 'defconst) (eq (cadr definition) constant)
                 (proper-list-p (nth 2 definition)) (= (length (nth 2 definition)) 2)
                 (eq (car (nth 2 definition)) 'quote)
                 (or (= (length forms) 1)
                     (and (= (length forms) 2)
                          (equal (cadr forms) (list 'provide (list 'quote constant))))))
      (error "Expected pinned literal harvested rows"))
    (let ((rows (cadr (nth 2 definition))))
      (nelisp-cc-fxir-admission--literal rows)
      (unless (and (proper-list-p rows) (<= 1 (length rows) 256))
        (error "Application row count bound"))
      (dolist (row rows)
        (unless (and (proper-list-p row) (= (length row) 6)
                     (integerp (cadr row)) (<= 0 (cadr row) 1073741823)
                     (plist-get (nelisp-cc-fxir-analysis-row row) :valid))
          (error "Invalid application FXIR row: %S" (car-safe row))))
      (nelisp-cc-fxir-admission--generation-budget rows)
      rows)))

(defun nelisp-cc-fxir-admission--arity (args)
  "Derive exact (MIN MAX REST) from an ordinary lexical defun arglist."
  (unless (proper-list-p args) (error "Nonordinary source arglist"))
  (let ((mode :required) (minimum 0) (fixed 0) (rest -1) names)
    (dolist (arg args)
      (cond ((eq arg '&optional)
             (unless (eq mode :required) (error "Invalid optional arglist"))
             (setq mode :optional))
            ((eq arg '&rest)
             (unless (memq mode '(:required :optional)) (error "Invalid rest arglist"))
             (setq mode :rest))
            (t
             (unless (and (symbolp arg) arg (not (eq arg t))
                          (not (keywordp arg)) (not (memq arg names))
                          (not (string-prefix-p "&" (symbol-name arg))))
               (error "Unsupported source argument"))
             (push arg names)
             (pcase mode
               (:required (setq minimum (1+ minimum) fixed (1+ fixed)))
               (:optional (setq fixed (1+ fixed)))
               (:rest (setq rest fixed mode :done))
               (_ (error "Argument after rest parameter"))))))
    (when (eq mode :rest) (error "Missing rest parameter"))
    (list minimum (if (< rest 0) fixed -1) rest)))

(defun nelisp-cc-fxir-admission--source-bindings (sources rows)
  "Bind every ROW to exactly one literal top-level lexical source defun."
  (unless (and (proper-list-p sources) (<= 1 (length sources) 32))
    (error "Missing source-binding files"))
  (let (definitions pins bindings)
    (dolist (file (sort (delete-dups (mapcar #'file-truename sources)) #'string<))
      (let* ((bytes (nelisp-cc-fxir-admission--bytes file))
             (pin (list :file file :sha256 (secure-hash 'sha256 bytes) :limit 1048576)))
        (unless (string-match-p "lexical-binding:[ \t]*t" (substring bytes 0 (min 256 (length bytes))))
          (error "Source module must declare lexical binding"))
        (push pin pins)
        (dolist (form (nelisp-cc-fxir-admission--forms bytes))
          (when (and (eq (car-safe form) 'defun) (assq (cadr form) rows))
            (when (assq (cadr form) definitions) (error "Duplicate source definition"))
            (unless (equal (nelisp-cc-fxir-admission--arity (nth 2 form))
                           (nth 5 (assq (cadr form) rows)))
              (error "Source/row arity mismatch"))
            (push (list (cadr form) pin (nelisp-cc-fxir-admission--digest form)) definitions)))))
    (dolist (row rows)
      (let ((definition (assq (car row) definitions)))
        (unless definition (error "Missing source definition for %S" (car row)))
        (push (list :name (car row) :id (cadr row)
                    :row-sha256 (nelisp-cc-fxir-admission--digest row)
                    :source (nth 1 definition) :form-sha256 (nth 2 definition)) bindings)))
    (list (nreverse pins) (nreverse bindings))))

(defun nelisp-cc-fxir-admission--binding-receipt (file constant sources producer)
  "Capture the supplied producer evidence; never execute the producer."
  (let* ((row-pin (nelisp-cc-fxir-admission--pin file))
         (rows (nelisp-cc-fxir-admission--read-rows file constant (plist-get row-pin :sha256)))
         (source (nelisp-cc-fxir-admission--source-bindings sources rows))
         pins)
    (dolist (role '(:binary :cold :build-manifest :stdout))
      (let ((path (plist-get producer role)))
        (unless (stringp path) (error "Missing producer evidence %S" role))
        (push (list role (nelisp-cc-fxir-admission--pin path 134217728)) pins)))
    (let ((stdout (nelisp-cc-fxir-admission--bytes (plist-get producer :stdout))))
      (dolist (row rows)
        (let ((pattern (format "^HARVEST %s ok id=%d words=[1-9][0-9]*$"
                               (regexp-quote (symbol-name (car row))) (cadr row))))
          (unless (and (string-match pattern stdout)
                       (not (string-match pattern stdout (match-end 0))))
            (error "Missing or duplicate successful harvest line")))))
    (list :schema nelisp-cc-fxir-admission-schema :kind :producer-source-binding
          :row-file row-pin :constant constant :sources (car source)
          :bindings (cadr source) :producer (nreverse pins)
          :limits '(:provenance-only :no-semantic-equivalence-proof :no-native-qualification))))

(defun nelisp-cc-fxir-admission-capture-receipt (file constant sources producer output)
  "Write a deterministic provenance receipt for existing harvester output.
SOURCES are the actual lexical source modules.  PRODUCER supplies :binary,
:cold, :build-manifest and :stdout artifact paths from the completed harvest.
This captures supplied evidence, and does not attest that a command ran.
Return the (:file FILE :sha256 HASH) descriptor accepted by `prepare'."
  (let ((receipt (nelisp-cc-fxir-admission--binding-receipt file constant sources producer)))
    (with-temp-file output (insert (nelisp-cc-fxir-admission--print receipt) "\n"))
    (list :file (file-truename output)
          :sha256 (plist-get (nelisp-cc-fxir-admission--pin output) :sha256))))

(defun nelisp-cc-fxir-admission--unique (rows baseline)
  (let ((ids (make-hash-table :test 'eql)) (names (make-hash-table :test 'eq)))
    (dolist (row baseline)
      (puthash (cadr row) t ids)
      (unless (nth 6 row) (puthash (car row) t names)))
    (dolist (row rows)
      (when (or (gethash (cadr row) ids) (gethash (car row) names))
        (error "Duplicate application row ID or ordinary name"))
      (puthash (cadr row) t ids) (puthash (car row) t names))))

(defun nelisp-cc-fxir-admission-prepare (file constant expected-sha256 source-bindings)
  "Prepare literal FILE rows, refusing absent or stale SOURCE-BINDINGS.
SOURCE-BINDINGS is a pinned receipt descriptor returned by capture-receipt.
The receipt retains producer evidence; runtime code/IR comparison remains
authoritative.  No source generation or global state changes occur here."
  (when nelisp-cc-fxir-native--additional-rows (error "One admission bundle per build"))
  (unless (and (stringp (plist-get source-bindings :file))
               (stringp (plist-get source-bindings :sha256)))
    (error "Missing pinned producer source-binding receipt"))
  (let* ((rows (nelisp-cc-fxir-admission--read-rows file constant expected-sha256))
         (receipt-pin (list :file (file-truename (plist-get source-bindings :file))
                            :sha256 (plist-get source-bindings :sha256) :limit 1048576))
         (receipt-forms (progn (nelisp-cc-fxir-admission--check-pins (list receipt-pin))
                              (nelisp-cc-fxir-admission--forms
                               (nelisp-cc-fxir-admission--bytes (plist-get receipt-pin :file)))))
         (receipt (car receipt-forms))
         (producer (apply #'append (mapcar (lambda (p) (list (car p) (plist-get (cadr p) :file)))
                                           (plist-get receipt :producer))))
         (baseline (nelisp-cc-fxir-native-all-rows))
         pins manifest)
    (unless (and (= (length receipt-forms) 1)
                 (equal receipt (nelisp-cc-fxir-admission--binding-receipt
                                 file constant (mapcar (lambda (p) (plist-get p :file))
                                                        (plist-get receipt :sources)) producer)))
      (error "Invalid producer source-binding receipt"))
    (nelisp-cc-fxir-admission--unique rows baseline)
    (setq pins (append (list receipt-pin (plist-get receipt :row-file))
                       (plist-get receipt :sources)
                       (mapcar #'cadr (plist-get receipt :producer))))
    (dolist (relative '("lisp/nelisp-cc-fxir-native.el" "lisp/nelisp-cc-fxir-analysis.el"
                        "lisp/nelisp-cc-fxir-admission.el" "scripts/nelisp-standalone-build.el"
                        "scripts/nelisp-fxir-native-install.el" "scripts/nelisp-fxir-native-install-tail.el"
                        "tools/ai/nc1-harvest.el" "scripts/nelisp-stdlib-prelude.el" "test/nelisp-fxir-eval.el"))
      (push (nelisp-cc-fxir-admission--pin (expand-file-name relative nelisp-cc-fxir-admission--root) 8388608) pins))
    (dolist (fc nelisp-cc-fxir-native--data-files)
      (push (nelisp-cc-fxir-admission--pin (car fc)) pins))
    (setq manifest (list :schema nelisp-cc-fxir-admission-schema :kind :optional-source-admission
                         :native-qualified nil :rows-sha256 (nelisp-cc-fxir-admission--digest rows)
                         :baseline-sha256 (nelisp-cc-fxir-admission--digest baseline)
                         :baseline-data-files (copy-tree nelisp-cc-fxir-native--data-files)
                         :generation-budget (nelisp-cc-fxir-admission--generation-budget rows)
                         :receipt receipt-pin :pins pins
                         :cache-consumers nelisp-cc-fxir-admission--cache-consumers))
    (list :schema nelisp-cc-fxir-admission-schema :rows (copy-tree rows)
          :manifest manifest :manifest-sha256 (nelisp-cc-fxir-admission--digest manifest))))

(defun nelisp-cc-fxir-admission-rows (snapshot)
  "Return copied rows after checking SNAPSHOT and all source/build pins.
The extra row file is never reread for parsing or evaluation.  Its bytes
are hashed only, along with source, producer and compiler provenance."
  (let ((rows (plist-get snapshot :rows)) (manifest (plist-get snapshot :manifest)))
    (nelisp-cc-fxir-admission--literal rows)
    (unless (and (eql (plist-get snapshot :schema) nelisp-cc-fxir-admission-schema)
                 (eql (plist-get manifest :schema) nelisp-cc-fxir-admission-schema)
                 (eq (plist-get manifest :kind) :optional-source-admission)
                 (not (plist-get manifest :native-qualified))
                 (plist-get manifest :receipt)
                 (member (plist-get manifest :receipt) (plist-get manifest :pins))
                 (equal (plist-get manifest :baseline-data-files) nelisp-cc-fxir-native--data-files)
                 (equal (plist-get manifest :generation-budget)
                        (nelisp-cc-fxir-admission--generation-budget rows))
                 (equal (plist-get snapshot :manifest-sha256) (nelisp-cc-fxir-admission--digest manifest))
                 (equal (plist-get manifest :rows-sha256) (nelisp-cc-fxir-admission--digest rows)))
      (error "Modified admission snapshot"))
    (nelisp-cc-fxir-admission--check-pins (plist-get manifest :pins))
    (copy-tree rows)))

(defun nelisp-cc-fxir-admission-activate (snapshot)
  "Transactionally activate SNAPSHOT before loading the standalone builder.
Regenerate eager native source, direct-wrapper boundaries and install table
under the same snapshot.  Publish globals only after generation succeeds."
  (when (or (featurep 'nelisp-standalone-build) nelisp-cc-fxir-native-windows
            nelisp-cc-fxir-native--additional-rows)
    (error "Admission requires an unactivated non-Windows compiler before builder load"))
  (let* ((rows (nelisp-cc-fxir-admission-rows snapshot))
         (baseline (nelisp-cc-fxir-native-all-rows))
         (manifest (plist-get snapshot :manifest)) source wrappers boundaries table result committed)
    (unless (equal (plist-get manifest :baseline-sha256) (nelisp-cc-fxir-admission--digest baseline))
      (error "Baseline rows changed after admission prepare"))
    (nelisp-cc-fxir-admission--unique rows baseline)
    (let ((nelisp-cc-fxir-native--additional-rows (copy-tree snapshot))
          (nelisp-cc-fxir-native--wrapper-names nil))
      (setq source (nelisp-cc-fxir-native-source)
            wrappers nelisp-cc-fxir-native--wrapper-names
            boundaries (append nelisp-cc-fxir-native--fixed-boundaries wrappers)
            table (nelisp-cc-fxir-native-install-table-source))
      (nelisp-cc-fxir-admission-rows snapshot))
    ;; Finish every fallible copy/digest before publishing any global.
    (setq committed (copy-tree snapshot)
          result (list :manifest-sha256 (plist-get snapshot :manifest-sha256)
                       :generated-source-sha256 (nelisp-cc-fxir-admission--digest source)
                       :install-table-sha256 (secure-hash 'sha256 table) :native-qualified nil))
    (setq nelisp-cc-fxir-native--additional-rows committed
          nelisp-cc-fxir-native--source source
          nelisp-cc-fxir-native--wrapper-names wrappers
          nelisp-cc-fxir-native--boundaries boundaries)
    result))

(provide 'nelisp-cc-fxir-admission)
;;; nelisp-cc-fxir-admission.el ends here
