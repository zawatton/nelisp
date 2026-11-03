;;; wasm-rootstack-compose.el --- Genuine rootstack static ownership -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-rootstack)

(defconst wasm-rootstack--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))
(defconst wasm-rootstack--names
  '(nl_rootstack_init nl_root_depth nl_rootstack_end nl_root_reserve_slot))

(defun wasm-rootstack--hash (file)
  "Return the SHA256 of FILE bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defvar wasm-rootstack--definition-cache nil
  "Dynamically scoped (FILE . INDEX) for one authority derivation only.")

(defun wasm-rootstack--read-definition (file kind name)
  "Read the unique reviewed top-level KIND/NAME form; never evaluate FILE."
  (let* ((index (if (equal file (car wasm-rootstack--definition-cache))
                    (cdr wasm-rootstack--definition-cache)
                  (wasm-rootstack--definition-index file)))
         (definition (cdr (assq name index))))
    (unless (eq (car-safe definition) kind)
      (error "Unreviewed ownership generator change: %s (definition context)" name))
    definition))

(defun wasm-rootstack--forms (tree predicate)
  "Collect syntax nodes in TREE satisfying PREDICATE."
  (when (consp tree)
    (append (when (funcall predicate tree) (list tree))
            (cl-mapcan (lambda (child) (wasm-rootstack--forms child predicate)) tree))))

(defconst wasm-rootstack--layout-inputs
  '(:target wasm32 :runtime-reload nil :callsite nil))

(defconst wasm-rootstack--boundaries
  '((nl_rootstack_top . nl_rootstack_region)
    (nl_rootstack_region . nl_gc_diag)
    (nl_thread_parallel_ctx . nl_alloc_diag)
    (nl_thread_registry . nl_gc_stats)
    (nl_root_pin_control . nl_root_pin_region)
    (nl_root_pin_region . nl_eln_callback_context)))

(defconst wasm-rootstack--oracle-helper
  (expand-file-name "tools/ai/wasm-rootstack-full-native-oracle.el" wasm-rootstack--root))

(defvar wasm-rootstack--oracle-cache (make-hash-table :test #'equal)
  "Native results keyed by (build path, build SHA256, loaded-file hashes).")

(defun wasm-rootstack--oracle-files-current-p (files &optional explicit-paths)
  "Whether FILES retain bytes and selection, exempting EXPLICIT-PATHS selection.
Full filenames passed to -l bypass sibling selection, never byte hashes."
  (cl-every
   (lambda (entry)
     (let* ((file (car entry))
            (sibling (cond ((string-suffix-p ".elc" file) (substring file 0 -1))
                           ((string-suffix-p ".el" file) (concat file "c")))))
       (and (file-readable-p file)
            (equal (cdr entry) (wasm-rootstack--hash file))
            (not (and (not (member file explicit-paths)) sibling (file-exists-p sibling)
                      (if (string-suffix-p ".elc" file)
                          (file-newer-than-file-p sibling file)
                        ;; Extensionless loading selects bytecode on a tie.
                        (not (file-newer-than-file-p file sibling)))
                      ;; A compiled loader can explicitly load its source.
                      ;; Both are then in the closure: no new candidate appeared.
                      (or (string-suffix-p ".elc" file)
                          (not (assoc sibling files))))))))
   files))

(defun wasm-rootstack--oracle-read (output names)
  "Parse exactly one native record in OUTPUT, requiring every boundary NAMES."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (unless (re-search-forward "^WASM-ROOTSTACK-NATIVE=" nil t)
      (error "missing output record"))
    (let* ((start (point))
           (finish (line-end-position))
           (record (read (current-buffer))))
      (unless (and (<= (point) finish)
                   (string-match-p "\\`[ \t]*\\'" (buffer-substring (point) finish))
                   (not (re-search-forward "^WASM-ROOTSTACK-NATIVE=" nil t))
                   (proper-list-p record) (= (length record) 4)
                   (eq (nth 0 record) :values) (eq (nth 2 record) :files))
        (error "invalid output record at %d" start))
      (let ((values (plist-get record :values)) (files (plist-get record :files)))
        (unless (and (proper-list-p values)
                     (equal (mapcar #'car-safe values) names)
                     (cl-every (lambda (entry) (and (consp entry) (integerp (cdr entry)))) values)
                     (proper-list-p files) files
                     (cl-every
                      (lambda (entry)
                        (and (consp entry) (stringp (car entry)) (file-name-absolute-p (car entry))
                             (stringp (cdr entry))
                             (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" (cdr entry)))) files)
                     (equal (mapcar #'car files)
                            (sort (delete-dups (mapcar #'car files)) #'string<)))
          (error "invalid boundary values or loaded-file hashes")))
      record)))

(defun wasm-rootstack--native-oracle (file)
  "Cross-check FILE in a separate native Emacs; cache its complete load closure.
Never evaluate build definitions in this process or fall back on failure."
  (condition-case err
      (let* ((file (expand-file-name file))
             (build-hash (wasm-rootstack--hash file))
             (names (delete-dups
                     (cl-loop for (start . end) in wasm-rootstack--boundaries
                              append (list (symbol-name start) (symbol-name end)))))
             cached)
        (maphash (lambda (key value)
                   (when (and (equal (nth 0 key) file) (equal (nth 1 key) build-hash)
                              (wasm-rootstack--oracle-files-current-p
                               (nth 2 key) (list wasm-rootstack--oracle-helper)))
                     (setq cached value)))
                 wasm-rootstack--oracle-cache)
        (or cached
            (let ((buffer (generate-new-buffer " *rootstack-native-oracle*")) process)
              (unwind-protect
                  (let* ((process-environment (copy-sequence process-environment))
                         (default-directory (file-name-directory (directory-file-name
                                                                  (file-name-directory file))))
                         ;; Printers in the full probe deliberately abbreviate;
                         ;; protocol arguments and records must never do so.
                         (print-length nil) (print-level nil)
                         (expression (prin1-to-string
                                      `(wasm-rootstack-full-native-oracle ,file ',names))))
                    (setenv "NELISP_RUNTIME_RELOAD" nil)
                    (setq process
                          (make-process
                           :name "rootstack-native-oracle" :buffer buffer :stderr buffer
                           :connection-type 'pipe :noquery t
                           :command
                           (append
                            (list "bash" "-c" "ulimit -v 8388608; exec \"$@\"" "oracle"
                                  "timeout" "120"
                                  (expand-file-name invocation-name invocation-directory)
                                  "-Q" "--batch")
                            (cl-loop for directory in '("lisp" "src" "scripts")
                                     append (list "-L" (expand-file-name directory default-directory)))
                            ;; The native build (Makefile) sets load-prefer-newer;
                            ;; without it a stale .elc could diverge from native.
                            (list "--eval" "(setq load-prefer-newer t)"
                                  "-l" wasm-rootstack--oracle-helper "--eval" expression))))
                    ;; The short child runs inside the caller's heavy-run slot;
                    ;; queueing it again could deadlock when every slot is held
                    ;; by a parent waiting for its child.  GNU timeout bounds it.
                    (while (process-live-p process) (accept-process-output process 0.1))
                    (accept-process-output process 0.1)
                    (let ((output (with-current-buffer buffer (buffer-string))))
                      (unless (and (eq (process-status process) 'exit)
                                   (= (process-exit-status process) 0))
                        (error "exit=%s %s" (process-exit-status process)
                               (truncate-string-to-width output 300)))
                      (let* ((record
                              (condition-case parse-error
                                  (wasm-rootstack--oracle-read output names)
                                (error (error "%s; output=%s" (error-message-string parse-error)
                                              (truncate-string-to-width output 300)))))
                             (files (plist-get record :files)))
                        (unless (and (equal build-hash (cdr (assoc file files)))
                                     (assoc wasm-rootstack--oracle-helper files)
                                     (wasm-rootstack--oracle-files-current-p
                                      files (list wasm-rootstack--oracle-helper)))
                          (error "loaded-file hashes missing or changed during oracle run"))
                        (puthash (list file build-hash files) record wasm-rootstack--oracle-cache)
                        record)))
                (when (and process (process-live-p process)) (delete-process process))
                (kill-buffer buffer)))))
    (error (error "Native layout oracle failed: %s"
                  (truncate-string-to-width (error-message-string err) 500)))))

(defun wasm-rootstack--check-native-oracle (owners oracle)
  "Require every syntactic OWNERS offset and size to match ORACLE."
  (let ((values (plist-get oracle :values)))
    (dolist (pair wasm-rootstack--boundaries)
      (let* ((owner (cl-find (car pair) owners :key (lambda (row) (plist-get row :name))))
             (offset (cdr (assoc (symbol-name (car pair)) values)))
             (end (cdr (assoc (symbol-name (cdr pair)) values)))
             (syntactic (cons (plist-get owner :offset) (plist-get owner :size)))
             (native (cons offset (- end offset))))
        (unless (equal syntactic native)
          (error "Native layout oracle disagreement: %s syntactic=%S native=%S"
                 (car pair) syntactic native))))))

(defconst wasm-rootstack--reviewed-generators
  '((defun . nelisp-standalone--arena-base-slot-unit)
    (defun . nelisp-standalone--driver-bss-base-size)
    (defun . nelisp-standalone--root-pin-region-offset)
    (defconst . nelisp-standalone--root-pin-slots)
    (defun . nelisp-standalone--runtime-reload-enabled-p)
    (defun . nelisp-standalone--callsite-enabled-p)))

(defun wasm-rootstack--definition-index (file)
  "Read all FILE forms in order and validate every reviewed target occurrence.
Walk quoted and unevaluated syntax too.  Each name must occur exactly once as
a definition or assignment target, in its reviewed top-level definition kind."
  (let (occurrences)
    (cl-labels
        ((quoted-name (form)
           (when (and (proper-list-p form) (= (length form) 2)
                      (eq (car form) 'quote) (symbolp (cadr form)))
             (cadr form)))
         (record (name form top)
           (when (rassq name wasm-rootstack--reviewed-generators)
             (push (list name form top) occurrences)))
         (walk (form top)
           (cond
            ((consp form)
             (pcase (car form)
               ((or 'defun 'defmacro 'defsubst 'cl-defun 'defconst 'defvar 'defcustom)
                (record (cadr form) form top))
               ((or 'defalias 'fset 'set)
                (record (quoted-name (cadr form)) form top))
               ((or 'setq 'setf 'setq-default 'setq-local)
                (let ((pairs (cdr form)))
                  (while (consp pairs)
                    (let ((place (car pairs)))
                      (record (if (symbolp place) place
                                (when (and (eq (car form) 'setf)
                                           (memq (car-safe place)
                                                 '(symbol-value symbol-function default-value)))
                                  (quoted-name (cadr place))))
                              form top))
                    (setq pairs (cddr pairs))))))
             ;; Visit elements, not list tails masquerading as separate forms.
             (let ((children form))
               (while (consp children)
                 (walk (car children) nil)
                 (setq children (cdr children)))))
            ((vectorp form) (mapc (lambda (child) (walk child nil)) form)))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer)) t))
          (end-of-file nil))))
    (mapcar
     (lambda (spec)
       (let ((matches (cl-remove-if-not (lambda (row) (eq (car row) (cdr spec))) occurrences)))
         (unless (and (= (length matches) 1) (nth 2 (car matches))
                      (eq (car-safe (nth 1 (car matches))) (car spec)))
           (error "Unreviewed ownership generator change: %s (definition context)" (cdr spec)))
         (cons (cdr spec) (nth 1 (car matches)))))
     wasm-rootstack--reviewed-generators)))

(defconst wasm-rootstack--authority-file
  (expand-file-name "tools/ai/wasm-rootstack-full-authority.eld" wasm-rootstack--root))

(defun wasm-rootstack--definition-hash (definition)
  "Hash the complete printed DEFINITION independently of diagnostic limits."
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-quoted t) (print-escape-newlines t))
    (secure-hash 'sha256 (prin1-to-string definition))))

(defun wasm-rootstack--check-reviewed-definition (name definition)
  "Reject DEFINITION unless its complete syntax is reviewed for NAME."
  (let ((hash (wasm-rootstack--definition-hash definition))
        (reviewed
         (with-temp-buffer
           (insert-file-contents wasm-rootstack--authority-file)
           (let ((entries (read (current-buffer))))
             (unless (and (proper-list-p entries)
                          (equal (mapcar #'car-safe entries)
                                 (mapcar #'cdr wasm-rootstack--reviewed-generators))
                          (cl-every (lambda (entry)
                                      (and (consp entry) (stringp (cdr entry))
                                           (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" (cdr entry))))
                                    entries)
                          (eq (condition-case nil (read (current-buffer))
                                (end-of-file :eof)) :eof))
               (error "Invalid reviewed ownership generator authority"))
             entries))))
    (unless (equal hash (cdr (assq name reviewed)))
      (error "Unreviewed ownership generator change: %s" name))
    hash))

(defun wasm-rootstack--build-definition (kind name)
  "Read KIND/NAME from the native build authority."
  (wasm-rootstack--read-definition
   (expand-file-name "scripts/nelisp-standalone-build.el" wasm-rootstack--root)
   kind name))

(defun wasm-rootstack--predicate-p (definition name args body)
  "Check the complete predicate DEFINITION, allowing one docstring."
  (let ((forms (nthcdr 3 definition)))
    (when (stringp (car forms)) (setq forms (cdr forms)))
    (and (eq (car-safe definition) 'defun)
         (eq (cadr definition) name)
         (equal (nth 2 definition) args)
         (equal forms (list body)))))

(defun wasm-rootstack--input-authority (inputs)
  "Validate literal input fields and the complete native predicates."
  (unless (and (proper-list-p inputs) (= (length inputs) 6)
               (equal (cl-loop for (key _value) on inputs by #'cddr collect key)
                      '(:target :runtime-reload :callsite))
               (eq (plist-get inputs :target) 'wasm32)
               (null (plist-get inputs :runtime-reload))
               (null (plist-get inputs :callsite)))
    (error "Unresolved ownership generator: layout inputs %S" inputs))
  (let ((reload (wasm-rootstack--build-definition
                 'defun 'nelisp-standalone--runtime-reload-enabled-p))
        (callsite (wasm-rootstack--build-definition
                   'defun 'nelisp-standalone--callsite-enabled-p)))
    (wasm-rootstack--check-reviewed-definition
     'nelisp-standalone--runtime-reload-enabled-p reload)
    (wasm-rootstack--check-reviewed-definition
     'nelisp-standalone--callsite-enabled-p callsite)
    (unless (and
             (wasm-rootstack--predicate-p
              reload 'nelisp-standalone--runtime-reload-enabled-p '(&optional target)
              '(and (eq (or target (nelisp-standalone-arena-rewrite-target))
                        'linux-x86_64)
                    (equal (getenv "NELISP_RUNTIME_RELOAD") "1")))
             (wasm-rootstack--predicate-p
              callsite 'nelisp-standalone--callsite-enabled-p nil
              '(and (nelisp-standalone--runtime-reload-enabled-p)
                    (nelisp-standalone--callsite-declared-entries) t)))
      (error "Unresolved ownership generator: native layout predicate changed"))
    (mapcar (lambda (definition)
              (cons (cadr definition) (secure-hash 'sha256 (prin1-to-string definition))))
            (list reload callsite))))

(defun wasm-rootstack--generator-value (kind name)
  "Read and validate the complete supported generator KIND/NAME definition."
  (let* ((definition (wasm-rootstack--build-definition kind name))
         (forms (and (proper-list-p definition) (nthcdr 3 definition))))
    (wasm-rootstack--check-reviewed-definition name definition)
    (unless (and (proper-list-p definition)
                 (eq (car definition) kind) (eq (cadr definition) name)
                 (pcase kind
                   ('defun
                    (when (stringp (car forms)) (setq forms (cdr forms)))
                    (and (null (nth 2 definition)) (= (length forms) 1)
                         (not (stringp (car forms)))))
                   ('defconst
                    (and (memq (length definition) '(3 4))
                         (or (null forms)
                             (and (= (length forms) 1) (stringp (car forms))))))))
      (error "Unresolved ownership generator: definition shape %s" name))
    (if (eq kind 'defun) (car forms) (nth 2 definition))))

(defun wasm-rootstack--number (form &optional inputs)
  "Resolve whitelisted native layout syntax using explicit INPUTS."
  (cond
   ((integerp form) form)
   ((eq form 'nelisp-standalone--root-pin-slots)
    (wasm-rootstack--number
     (wasm-rootstack--generator-value 'defconst form) inputs))
   ((memq (car-safe form) '(+ * logand))
    (let ((numbers (mapcar (lambda (child) (wasm-rootstack--number child inputs))
                           (cdr form))))
      (pcase (car form)
        ('+ (apply #'+ numbers))
        ('* (apply #'* numbers))
        ('logand (apply #'logand numbers)))))
   ((and (memq (car-safe form) '(nelisp-standalone--driver-bss-base-size
                                nelisp-standalone--root-pin-region-offset))
         (null (cdr form)))
    (wasm-rootstack--number (wasm-rootstack--generator-value 'defun (car form)) inputs))
   ((and (eq (car-safe form) 'if) (= (length form) 4)
         (member (cadr form)
                 '((eq (nelisp-standalone-arena-rewrite-target) 'windows-x86_64)
                   (nelisp-standalone--runtime-reload-enabled-p)
                   (nelisp-standalone--callsite-enabled-p))))
    (wasm-rootstack--input-authority inputs)
    (let ((selected
           (pcase (car (cadr form))
             ('eq (eq (plist-get inputs :target) 'windows-x86_64))
             ('nelisp-standalone--runtime-reload-enabled-p
              (plist-get inputs :runtime-reload))
             ('nelisp-standalone--callsite-enabled-p (plist-get inputs :callsite)))))
      (wasm-rootstack--number (if selected (nth 2 form) (nth 3 form)) inputs)))
   (t (error "Unresolved ownership generator: %S" form))))

(defconst wasm-rootstack--fixed-owners
  '(nl_rootstack_top nl_root_pin_control nl_thread_parallel_ctx))
;; nl_thread_registry is a 16-byte header plus 64 indexed 16-byte entries
;; (`nl_thread_registry_entry'), so it is an array owner, not a fixed record.
(defconst wasm-rootstack--array-owners
  '(nl_rootstack_region nl_root_pin_region nl_thread_registry))

(cl-defstruct (wasm-rootstack--site (:constructor wasm-rootstack--site-create))
  form parent position function binding initializer sense children declaration)

(cl-defstruct (wasm-rootstack--binding (:constructor wasm-rootstack--binding-create))
  name initializer references assigned)

(defun wasm-rootstack--constant (form)
  "Resolve source integer arithmetic FORM, never native layout names or calls.
Only integer literals and +, -, *, logand over those literals are constants.
Lexical integer variables retain the existing escape rule."
  (condition-case nil
      (cond
       ((integerp form) form)
       ((and (proper-list-p form) (memq (car-safe form) '(+ - * logand)))
        (let ((numbers (mapcar #'wasm-rootstack--constant (cdr form))))
          (when (cl-every #'integerp numbers)
            (pcase (car form)
              ('+ (apply #'+ numbers))
              ('- (apply #'- numbers))
              ('* (apply #'* numbers))
              ('logand (apply #'logand numbers)))))))
    (error nil)))

(defun wasm-rootstack--literal-count (tree)
  "Count every data-addr literal in TREE by plain car/cdr recursion.
This inventory is independent of the executable-position syntax index."
  (if (not (consp tree)) 0
    (+ (if (eq (car tree) 'data-addr) 1 0)
       (wasm-rootstack--literal-count (car tree))
       (wasm-rootstack--literal-count (cdr tree)))))

(defun wasm-rootstack--syntax (source)
  "Index SOURCE occurrences and lexical binding identities, without evaluation."
  (let (sites literals definitions comparisons)
    (cl-labels
        ((walk (form parent position env function sense)
           (let ((site (wasm-rootstack--site-create
                        :form form :parent parent :position position
                        :function function :sense sense)))
             (push site sites)
             (when parent (push site (wasm-rootstack--site-children parent)))
             (cond
              ((symbolp form)
               (let ((binding (cdr (assq form env))))
                 (when binding
                   (setf (wasm-rootstack--site-binding site) binding)
                   (push site (wasm-rootstack--binding-references binding)))))
              ((not (consp form)))
              ((or (not (proper-list-p form)) (not (symbolp (car form))))
               (error "Unsupported owner syntax: %S" form))
              ((eq (car form) 'quote)
               (when (> (wasm-rootstack--literal-count form) 0)
                 (error "Unsupported owner syntax: %S" form)))
              ((eq (car form) 'data-addr)
               (unless (and (= (length form) 2) (symbolp (cadr form)))
                 (error "Unsupported owner syntax: %S" form))
               (push site literals))
              ((eq (car form) 'defun)
               (unless (and (symbolp (cadr form)) (proper-list-p (nth 2 form))
                            (cl-every (lambda (x) (and (symbolp x)
                                                      (not (memq x '(&optional &rest &key &aux)))))
                                      (nth 2 form)))
                 (error "Unsupported owner syntax: %S" form))
               (setf (wasm-rootstack--site-function site) (cadr form))
               (let ((scope nil) (parameters nil))
                 (dolist (name (nth 2 form))
                   (let ((binding (wasm-rootstack--binding-create :name name)))
                     (push (cons name binding) scope)
                     (push binding parameters)))
                 (push (list (cadr form) site (nreverse parameters)) definitions)
                 (cl-loop for child in (nthcdr 3 form) for index from 3 do
                          (walk child site index scope (cadr form) 'valid))))
              ((memq (car form) '(let let*))
               (unless (and (proper-list-p (cadr form))
                            (cl-every (lambda (entry)
                                        (or (symbolp entry)
                                            (and (proper-list-p entry)
                                                 (memq (length entry) '(1 2))
                                                 (symbolp (car entry)))))
                                      (cadr form)))
                 (error "Unsupported owner syntax: %S" form))
               (let ((scope env))
                 (dolist (entry (cadr form))
                   (let* ((binding (wasm-rootstack--binding-create
                                    :name (if (consp entry) (car entry) entry)))
                          (declaration (wasm-rootstack--site-create
                                        :form entry :parent site :position 1
                                        :function function :sense sense :declaration t))
                          (init (walk (and (consp entry) (cadr entry)) declaration 1
                                      (if (eq (car form) 'let*) scope env) function sense)))
                     (push declaration sites)
                     (push declaration (wasm-rootstack--site-children site))
                     (setf (wasm-rootstack--binding-initializer binding) init
                           (wasm-rootstack--site-initializer init) binding)
                     (push (cons (wasm-rootstack--binding-name binding) binding) scope)))
                 (cl-loop for child in (cddr form) for index from 2 do
                          (walk child site index scope function sense))))
              ((eq (car form) 'cond)
               (cl-loop for clause in (cdr form) for index from 1 do
                        (unless (and (consp clause) (proper-list-p clause))
                          (error "Unsupported owner syntax: %S" form))
                        (let ((group (wasm-rootstack--site-create
                                      :form clause :parent site :position index
                                      :function function :sense sense :declaration t)))
                          (push group sites)
                          (push group (wasm-rootstack--site-children site))
                          (cl-loop for expression in clause for slot from 0 do
                                   (walk expression group slot env function
                                         (if (and (= slot 0) (equal (cdr clause) '(0)))
                                             (if (eq sense 'invalid) 'valid 'invalid)
                                           sense))))))
              ((eq (car form) 'setq)
               (unless (and (= (% (length (cdr form)) 2) 0)
                            (cl-loop for (name _value) on (cdr form) by #'cddr
                                     always (symbolp name)))
                 (error "Unsupported owner syntax: %S" form))
               (cl-loop for (name value) on (cdr form) by #'cddr for index from 1 by 2 do
                        (let ((binding (cdr (assq name env))))
                          (when binding (setf (wasm-rootstack--binding-assigned binding) t))
                          (walk name site index env function sense)
                          (walk value site (1+ index) env function sense))))
              ((and (or (special-form-p (car form)) (macrop (car form)))
                    (not (memq (car form) '(if while seq progn and or catch throw))))
               (error "Unsupported owner syntax: %S" form))
              (t
               ;; These special forms and ordinary calls evaluate each argument.
               (when (memq (car form) '(< <= > >= = /=)) (push site comparisons))
               (cl-loop for child in (cdr form) for index from 1 do
                        (walk child site index env function
                              (cond
                               ((and (eq (car form) 'if) (= index 1))
                                (if (eql (nth 2 form) 0)
                                    (if (eq sense 'invalid) 'valid 'invalid)
                                  sense))
                               ;; Boolean wrappers retain the inner comparison's polarity.
                               ((and (eq (car form) '=) (eql (nth 2 form) 0))
                                (if (eq sense 'invalid) 'valid 'invalid))
                               (t sense))))))
             site)))
      (if (and (listp source) (consp (car source)))
          (dolist (form source) (walk form nil 0 nil 'toplevel 'valid))
        (walk source nil 0 nil 'toplevel 'valid)))
    (list :sites (nreverse sites) :literals (nreverse literals)
          :definitions definitions :comparisons comparisons)))

(defun wasm-rootstack--sorted-escapes (entries)
  "Return ENTRIES in stable readable syntax order, retaining multiplicity."
  (let ((print-length nil) (print-level nil) (print-circle nil) (print-quoted t))
    (sort entries
          (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))))

(defun wasm-rootstack--usage (source)
  "Classify owner literal occurrences in SOURCE with a closed syntax rule.
A drift detector over the genuine source with a pinned, reviewed escape list.
It is not a bounds verifier. Within the supported grammar, new escape expressions
and increased multiplicities are rejected as unreviewed. Changes outside the
pinned expressions that keep their multiplicities are an explicitly stated limit.
Supported grammar: fixed-argument defun bodies; let/let* initializers and bodies;
cond tests and bodies; setq pairs; if, while, seq, progn, and, or, catch, throw;
and symbol-headed calls with evaluated arguments. Quoted data must not contain
owner literals; other special forms/macros and non-symbol call heads reject.
Comparisons are escapes; their pin end bounds are checked independently.
No return values, loop fixed points, or stored pointers are modelled. Helper
parameter aliases are inspected only one call deep, with a visited set keyed
by (callee parameter owner). Lexical references use binding identity. An unused
literal-initialized binding is an escape, since it has no checked access.
Source constants are integer literals and +, -, *, logand over those literals."
  (let* ((syntax (wasm-rootstack--syntax source))
         (definitions (plist-get syntax :definitions))
         (aliases (make-hash-table :test #'eq))
         (visited (make-hash-table :test #'equal))
         extents negative escapes capacities classifications)
    (cl-labels
        ((child (site index)
           (cl-find index (wasm-rootstack--site-children site)
                    :key #'wasm-rootstack--site-position))
         (escape (owner site depth)
           (let* ((form (wasm-rootstack--site-form site))
                  (function (wasm-rootstack--site-function site)))
             (push (list owner function form) escapes)
             ;; Only an actual argument occurrence can seed a parameter alias.
             (when (and (= depth 0) (not (wasm-rootstack--site-declaration site)))
               (let ((definition (assq (car-safe form) definitions)))
                 (when definition
                   (cl-loop for argument in (cdr form) for index from 0 do
                            (let ((address (address (child site (1+ index)) nil)))
                              (when (and address (eq (car address) owner))
                                (let* ((parameter (nth index (nth 2 definition)))
                                       (key (list (car definition) index owner)))
                                  (when (and parameter (not (gethash key visited)))
                                    (puthash key t visited)
                                    (puthash parameter address aliases)
                                    (references parameter owner (cdr address) 1)))))))))))
         (extent (owner offset site)
           (if (< offset 0)
               (push (list owner (wasm-rootstack--site-function site)
                           (wasm-rootstack--site-form site) offset) negative)
             (let ((entry (assq owner extents)))
               (if entry (setcdr entry (max (cdr entry) (+ offset 8)))
                 (push (cons owner (+ offset 8)) extents)))))
         (address (site seen)
           ;; Resolve only literal/immutable lexical syntax in comparison operands.
           (when site
             (let* ((form (wasm-rootstack--site-form site))
                    (binding (wasm-rootstack--site-binding site)))
               (cond
                ((eq (car-safe form) 'data-addr) (cons (cadr form) 0))
                ((and binding (not (wasm-rootstack--binding-assigned binding))
                      (not (memq binding seen)))
                 (or (gethash binding aliases)
                     (address (wasm-rootstack--binding-initializer binding) (cons binding seen))))
                ((eq (car-safe form) '+)
                 (let ((base nil) (offset 0) (valid t))
                   (cl-loop for term in (cdr form) for index from 1 do
                            (let ((n (wasm-rootstack--constant term)))
                              (if n (cl-incf offset n)
                                (let ((a (address (child site index) seen)))
                                  (if (and a (null base)) (setq base a) (setq valid nil))))))
                   (when (and valid base) (cons (car base) (+ offset (cdr base))))))
                ;; Sentinel-bearing end declarations are inspected syntactically.
                ((eq (car-safe form) 'if)
                 (let ((yes (address (child site 2) seen))
                       (no (address (child site 3) seen)))
                   (cond ((and yes (eql (nth 3 form) 0)) yes)
                         ((and no (eql (nth 2 form) 0)) no))))))))
         (end-expression-p (site seen)
           "Recognise additive end declarations, including zero/negative bounds."
           (when site
             (let ((form (wasm-rootstack--site-form site))
                   (binding (wasm-rootstack--site-binding site)))
               (cond
                ((eq (car-safe form) '+) t)
                ((and binding (not (wasm-rootstack--binding-assigned binding))
                      (not (memq binding seen)))
                 (end-expression-p (wasm-rootstack--binding-initializer binding) (cons binding seen)))
                ((eq (car-safe form) 'if)
                 (or (end-expression-p (child site 2) seen)
                     (end-expression-p (child site 3) seen)))))))
         (references (binding owner offset depth)
           (puthash binding (cons owner offset) aliases)
           (let ((direct t))
             (cond
              ((null (wasm-rootstack--binding-references binding))
               ;; An unused initializer has no checked access; pin its binding.
               (let ((init (wasm-rootstack--binding-initializer binding)))
                 (when init (escape owner (wasm-rootstack--site-parent init) depth)))
               (setq direct nil))
              ((wasm-rootstack--binding-assigned binding)
               (escape owner
                       (if (wasm-rootstack--binding-initializer binding)
                           (wasm-rootstack--site-parent
                            (wasm-rootstack--site-parent
                             (wasm-rootstack--binding-initializer binding)))
                         (wasm-rootstack--site-parent
                          (car (wasm-rootstack--binding-references binding)))) depth)
               (setq direct nil))
              (t
               (dolist (ref (wasm-rootstack--binding-references binding))
                 (unless (classify ref owner offset depth) (setq direct nil)))))
             direct))
         (classify (site owner offset depth &optional literal)
           (let ((cursor site) (n offset) (continue t))
             ;; Exactly one address term plus constant arithmetic terms is direct.
             (while (and continue
                         (wasm-rootstack--site-parent cursor)
                         (not (wasm-rootstack--site-declaration (wasm-rootstack--site-parent cursor)))
                         (eq (car-safe (wasm-rootstack--site-form
                                        (wasm-rootstack--site-parent cursor))) '+))
               (let* ((parent (wasm-rootstack--site-parent cursor))
                      (terms (cdr (wasm-rootstack--site-form parent)))
                      (position (wasm-rootstack--site-position cursor))
                      (constants (cl-loop for term in terms for index from 1
                                          unless (= index position)
                                          collect (wasm-rootstack--constant term))))
                 (if (cl-every #'integerp constants)
                     (setq n (+ n (apply #'+ constants)) cursor parent)
                   (setq continue nil))))
             (let* ((parent (wasm-rootstack--site-parent cursor))
                    (form (and parent (wasm-rootstack--site-form parent)))
                    (op (car-safe form))
                    (position (wasm-rootstack--site-position cursor))
                    (binding (wasm-rootstack--site-initializer cursor)))
               (cond
                (binding (if literal (references binding owner n depth)
                           (escape owner parent depth) nil))
                ((and (= position 1)
                      ;; Exact lowering arms in lisp/nelisp-aot-compiler.el:
                      ;; atomic-fetch-add:11175, atomic-compare-exchange:11187,
                      ;; ptr-read-u64:11201, ptr-write-u64:11212.
                      (memq op '(ptr-read-u64 ptr-write-u64
                                 atomic-fetch-add atomic-compare-exchange))
                      continue)
                 (let ((extra (if (memq op '(ptr-read-u64 ptr-write-u64))
                                  (wasm-rootstack--constant (nth 2 form)) 0)))
                   (if extra (progn (extent owner (+ n extra) parent) t)
                     (escape owner parent depth) nil)))
                (t
                 ;; Include the first unsupported operation, never its result.
                 (escape owner (or parent cursor) depth) nil))))))
      (dolist (literal (plist-get syntax :literals))
        (let* ((owner (cadr (wasm-rootstack--site-form literal)))
               (direct (classify literal owner 0 0 t)))
          (push (list owner (wasm-rootstack--site-function literal)
                      (if direct 'direct 'escape)) classifications)))
      (dolist (comparison (plist-get syntax :comparisons))
        (let* ((form (wasm-rootstack--site-form comparison)) (op (car form))
               (sense (wasm-rootstack--site-sense comparison)))
          (dolist (index '(1 2))
            (let ((a (address (child comparison index) nil)))
              (when (and (eq (car-safe a) 'nl_root_pin_region)
                         (end-expression-p (child comparison index) nil)
                         (or (memq op '(= /=))
                             (if (eq sense 'invalid)
                                 (if (= index 2) (memq op '(> >=)) (memq op '(< <=)))
                               (if (= index 2) (memq op '(< <=)) (memq op '(> >=))))))
                (push (list :function (wasm-rootstack--site-function comparison)
                            :expression (wasm-rootstack--site-form (child comparison index))
                            :size (cdr a)) capacities)))))))
    ;; Pin immediate call-site context for literal-escape functions. This is
    ;; a syntax-only review footprint, not a summary of their return values.
    ;; In particular it pins registry entry field offsets without claiming
    ;; per-entry bounds for the 16-byte header + 64 x 16-byte entry contract.
    (let ((origins (delete-dups
                    (cl-loop for (owner function class) in classifications
                             when (eq class 'escape) collect (cons owner function)))))
      (dolist (site (plist-get syntax :sites))
        (let ((form (wasm-rootstack--site-form site)))
          (when (and (consp form) (not (wasm-rootstack--site-declaration site))
                     (not (memq (car form) '(defun quote))))
            (dolist (origin origins)
              (when (eq (car form) (cdr origin))
                (let ((context (or (wasm-rootstack--site-parent site) site)))
                  (push (list (car origin) (wasm-rootstack--site-function site)
                              (wasm-rootstack--site-form context)) escapes))))))))
    (setq escapes (wasm-rootstack--sorted-escapes escapes))
    (list :extents extents :negative negative :escapes escapes
          :classifications (nreverse classifications)
          :owner-classes (list :fixed wasm-rootstack--fixed-owners :arrays wasm-rootstack--array-owners)
          :pin-capacities (delete-dups capacities)
          :dynamic-accesses
          (mapcar (lambda (owner)
                    (let ((entries (cl-remove-if-not (lambda (x) (eq (car x) owner)) escapes)))
                      (list :owner owner :count (length entries)
                            :accesses (mapcar (lambda (x) (cons (nth 1 x) (nth 2 x))) entries))))
                  wasm-rootstack--array-owners))))

(defconst wasm-rootstack--escapes-file
  (expand-file-name "tools/ai/wasm-rootstack-full-escapes.eld" wasm-rootstack--root))

(defun wasm-rootstack--pinned-escapes ()
  "Read the reviewed escape multiset as data only."
  (with-temp-buffer
    (insert-file-contents wasm-rootstack--escapes-file)
    (let ((entries (read (current-buffer))))
      (unless (and (listp entries) (equal entries (wasm-rootstack--sorted-escapes (copy-tree entries))))
        (error "Invalid pinned owner escape list"))
      (unless (eq (condition-case nil (read (current-buffer)) (end-of-file :eof)) :eof)
        (error "Trailing pinned owner escape data"))
      entries)))

(defun wasm-rootstack--check-escapes (usage source &optional full)
  "Compare escape multisets, rejecting unreviewed and stale SOURCE occurrences."
  (let* ((actual (plist-get usage :escapes))
         (functions (mapcar #'cadr (wasm-rootstack--forms source (lambda (x) (eq (car-safe x) 'defun)))))
         (pinned (cl-remove-if-not (lambda (x) (or full (memq (nth 1 x) functions)))
                                   (wasm-rootstack--pinned-escapes))))
    (dolist (entry actual)
      (let ((match (member entry pinned)))
        (unless match
          (error "Unreviewed owner escape: %s %s %S" (nth 0 entry) (nth 1 entry) (nth 2 entry)))
        (setq pinned (cl-delete entry pinned :test #'equal :count 1))))
    (when pinned
      (let ((entry (car pinned)))
        (error "Stale pinned owner escape: %s %s %S" (nth 0 entry) (nth 1 entry) (nth 2 entry))))))

(defun wasm-rootstack--check-coverage (owners source &optional full)
  "Check constant extents, direct pin end bounds, and the reviewed escape set."
  (let* ((usage (wasm-rootstack--usage source))
         (region (cl-find 'nl_root_pin_region owners :key (lambda (row) (plist-get row :name)))))
    (dolist (row owners)
      (let ((required (cdr (assq (plist-get row :name) (plist-get usage :extents)))))
        (when (and required (< (plist-get row :size) required))
          (error "Rootstack usage coverage: %s size=%d requires=%d"
                 (plist-get row :name) (plist-get row :size) required))))
    (when (plist-get usage :negative)
      (pcase-let ((`(,owner ,function ,expression ,offset) (car (plist-get usage :negative))))
        (error "Rootstack usage coverage: %s negative offset=%d function=%s access=%S"
               owner offset function expression)))
    (when region
      (unless (plist-get usage :pin-capacities)
        (error "Genuine pin region capacity contract missing"))
      (dolist (capacity (plist-get usage :pin-capacities))
        (unless (equal (plist-get region :size) (plist-get capacity :size))
          (error "Genuine pin region capacity mismatch: size=%d source=%S function=%s"
                 (plist-get region :size) (plist-get capacity :expression)
                 (plist-get capacity :function)))))
    (wasm-rootstack--check-escapes usage source full)
    usage))

(defun wasm-rootstack--check-native-layout (owners)
  "Check resolved driver-relative ownership ranges and pin alignment."
  (let ((previous-end 0))
    (dolist (row (sort (copy-sequence owners)
                      (lambda (a b) (< (plist-get a :offset) (plist-get b :offset)))))
      (let ((offset (plist-get row :offset)) (size (plist-get row :size))
            (name (plist-get row :name)))
        (when (and (eq name 'nl_root_pin_region) (/= (% offset 32) 0))
          (error "Native pin region alignment: offset=%d is not 32-byte aligned to driver BSS origin" offset))
        (when (< offset previous-end)
          (error "Native rootstack ownership overlap: %s offset=%d previous-end=%d"
                 name offset previous-end))
        (unless (and (= (% offset 8) 0) (= (% size 8) 0) (> size 0))
          (error "Unsafe native rootstack ownership: %s" name))
        (setq previous-end (+ offset size))))))

(defun wasm-rootstack-definitions ()
  "Select exactly four unchanged genuine provider definitions."
  (mapcar (lambda (name)
            (or (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun) (eq (cadr form) name)))
                            (cdr nelisp-cc-rootstack--source))
                (error "Missing genuine rootstack definition: %s" name)))
          wasm-rootstack--names))

(defun wasm-rootstack--boundary-declarations (definition names)
  "Collect exactly one unconditional declaration per boundary in NAMES.
Only the enclosing defun body and seq/progn/list/append/link-unit constructors
are supported declaration paths. Other enclosing forms reject, even if inactive."
  (let (declarations)
    (cl-labels
        ((walk (tree sequential)
           (when (consp tree)
             (when (and (eq (car tree) 'nelisp-link-symbol)
                        (member (cadr tree) names))
               (unless sequential
                 (error "Unsupported ownership declaration context: %s %S" (cadr tree) tree))
               (push tree declarations))
             (let ((allowed (and sequential
                                 (memq (car tree) '(seq progn list append nelisp-link-unit-make)))))
               ;; Inspect all syntax, including non-executable positions.
               (dolist (child tree) (walk child allowed))))))
      (dolist (body (nthcdr 3 definition)) (walk body t)))
    (dolist (name names)
      (let ((matches (cl-remove-if-not (lambda (x) (equal (cadr x) name)) declarations)))
        (unless (= (length matches) 1)
          (error "Ownership boundary declaration count: %s count=%d" name (length matches)))
        (unless (equal (plist-get (nthcdr 3 (car matches)) :section) '(quote bss))
          (error "Invalid ownership boundary declaration: %s" name))))
    declarations))

(cl-defun wasm-rootstack-authority (&key (layout-inputs wasm-rootstack--layout-inputs))
  "Derive BSS extents from syntax and cross-check native load under LAYOUT-INPUTS."
  (let* ((file (expand-file-name "scripts/nelisp-standalone-build.el" wasm-rootstack--root))
         (wasm-rootstack--definition-cache
          (cons file (condition-case err (wasm-rootstack--definition-index file)
                       (invalid-read-syntax
                        ;; Preserve the revision-7 context gates; a malformed
                        ;; file must also get the native process failure report.
                        (wasm-rootstack--native-oracle file)
                        (signal (car err) (cdr err))))))
         (predicate-hashes (wasm-rootstack--input-authority layout-inputs))
         (definition (wasm-rootstack--read-definition file 'defun 'nelisp-standalone--arena-base-slot-unit))
         (reviewed-hash (wasm-rootstack--check-reviewed-definition
                         'nelisp-standalone--arena-base-slot-unit definition))
         (boundaries wasm-rootstack--boundaries)
         (symbols (wasm-rootstack--boundary-declarations
                   definition (delete-dups (cl-loop for (start . end) in boundaries
                                                     append (list (symbol-name start) (symbol-name end))))))
         (owners
          (mapcar
           (lambda (pair)
             (let* ((start (cl-find (symbol-name (car pair)) symbols :key #'cadr :test #'equal))
                    (end (cl-find (symbol-name (cdr pair)) symbols :key #'cadr :test #'equal))
                    (a (nth 2 start)) (b (nth 2 end)))
               (unless (and start end) (error "Missing ownership boundary: %S" pair))
               (let ((offset (wasm-rootstack--number a layout-inputs))
                     (end (wasm-rootstack--number b layout-inputs)))
                 (list :name (car pair) :section 'bss :owner 'standalone-driver
                       :offset-expression a :end-expression b
                       :offset offset :end end :size (- end offset)))))
           boundaries))
         (oracle (wasm-rootstack--native-oracle file)))
    (wasm-rootstack--check-native-oracle owners oracle)
    (unless (equal (mapcar (lambda (row) (plist-get row :size)) (cl-subseq owners 0 2)) '(8 4194304))
      (error "Rootstack ownership contract changed"))
    (unless (and (member '(+ (data-addr nl_rootstack_region) 4194304)
                         (wasm-rootstack--forms (wasm-rootstack-definitions) (lambda (_) t)))
                 (member '(ptr-write-u64 (+ slot 24) 0 0)
                         (wasm-rootstack--forms (wasm-rootstack-definitions) (lambda (_) t))))
      (error "Genuine rootstack capacity/slot contract changed"))
    (wasm-rootstack--check-native-layout owners)
    (list :file file :file-sha256 (wasm-rootstack--hash file)
          :definition-sha256 (secure-hash 'sha256 (prin1-to-string definition))
          :reviewed-definition-sha256 reviewed-hash
          :authority-sha256 (wasm-rootstack--hash wasm-rootstack--authority-file)
          :layout-inputs (copy-tree layout-inputs) :predicate-sha256 predicate-hashes
          :native-oracle oracle
          :generator-sha256
          (mapcar (lambda (spec)
                    (cons (cdr spec)
                          (secure-hash 'sha256 (prin1-to-string
                                               (wasm-rootstack--build-definition (car spec) (cdr spec))))))
                  '((defun . nelisp-standalone--driver-bss-base-size)
                    (defun . nelisp-standalone--root-pin-region-offset)
                    (defconst . nelisp-standalone--root-pin-slots)))
          :escapes-sha256 (wasm-rootstack--hash wasm-rootstack--escapes-file)
          :owners owners :usage (wasm-rootstack--usage nelisp-cc-rootstack--source)
          :target (plist-get layout-inputs :target)
          :limit "Native syntax cross-checked against isolated native load; explicit wasm32 inputs exclude reload and callsite.")))

(cl-defun wasm-rootstack-compose (&key (owners 'derived) full)
  "Compose unchanged source with explicit OWNERS; nil means no declarations."
  (let* ((authority (wasm-rootstack-authority))
         (known (plist-get authority :owners))
         (declarations (if (eq owners 'derived) (cl-subseq known 0 2) owners))
         (seen nil) (blobs nil) (classified nil)
         (definitions (if full (cdr nelisp-cc-rootstack--source) (wasm-rootstack-definitions))))
    (setq classified
          (length (plist-get (wasm-rootstack--check-coverage declarations definitions full)
                             :classifications)))
    (wasm-rootstack--check-native-layout declarations)
    (dolist (row declarations)
      (let* ((name (plist-get row :name)) (size (plist-get row :size))
             (contract (cl-find name known :key (lambda (x) (plist-get x :name)))))
        (when (memq name seen) (error "Duplicate rootstack ownership: %s" name))
        (unless (and contract (eq (plist-get row :section) 'bss)
                     (eq (plist-get row :owner) 'standalone-driver)
                     (integerp size) (> size 0) (= (% size 8) 0)
                     (equal size (plist-get contract :size))
                     (equal (plist-get row :offset) (plist-get contract :offset))
                     (equal (plist-get row :end) (plist-get contract :end)))
          (error "Invalid or unresolved rootstack ownership: %s" name))
        (push name seen)
        (push `(data-blob ,name ,(make-string size 0) bss) blobs)))
    (let* ((source `(seq ,@(nreverse blobs) ,@definitions))
           (inventory (wasm-rootstack--literal-count source)))
      (unless (= inventory classified)
        (error "Unsupported owner syntax: literal inventory=%d classified=%d" inventory classified))
      (wasm-rootstack-layout source)
      source)))

(defun wasm-rootstack-layout (source)
  "Verify static owner alignment and disjoint managed cursor in SOURCE."
  (let* ((data-source `(seq ,@(cl-remove-if-not
                              (lambda (form) (eq (car-safe form) 'data-blob)) (cdr source))))
         (blobs (nelisp-aot-compiler--collect-data-blobs (nelisp-aot-compiler--parse data-source)))
         (layout (nelisp-aot-compiler--wasm-layout-data-blobs blobs))
         (cursor (plist-get layout :stack-top)) (previous-end 0))
    (dolist (entry (plist-get layout :bss-layout))
      (let ((address (+ (plist-get layout :bss-base) (plist-get entry :offset))))
        (unless (and (> address 0) (= (% address 8) 0)
                     (>= address previous-end) (<= (+ address (plist-get entry :len)) cursor))
          (error "Unsafe rootstack static layout"))
        (setq previous-end (+ address (plist-get entry :len)))))
    (let ((pin (cdr (assoc "nl_root_pin_region" (plist-get layout :symbol-addrs)))))
      (when pin
        (setq layout
              (plist-put layout :pin-region-alignment
                         (list :address pin :required 32 :remainder (% pin 32)
                               :absolute-aligned (= (% pin 32) 0)
                               :limit "Wasm BSS layout guarantees 8-byte alignment only.")))))
    layout))

(provide 'wasm-rootstack-compose)
