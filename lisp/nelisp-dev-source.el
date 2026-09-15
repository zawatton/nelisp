;;; nelisp-dev-source.el --- bounded nonexecuting source queries -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-dev-protocol)

(defconst nelisp-dev-source--max-file-bytes (* 2 1024 1024))
(defconst nelisp-dev-source--max-files 32)
(defvar nelisp-dev-source--diagnostic-sequence 0)

(defun nelisp-dev-source--ensure-lisp-mode ()
  "Load the optional host syntax support needed by source queries.
Signal clearly when this host does not provide it; source analysis must not
silently degrade to a different parser contract."
  (unless (and (require 'lisp-mode nil t)
               (boundp 'emacs-lisp-mode-syntax-table))
    (error "Source queries require the host lisp-mode syntax table")))

(defun nelisp-dev-source--arg (request key)
  (cdr (assoc key (cdr (assoc "arguments" request)))))

(defun nelisp-dev-source--position (text offset)
  "Return one-based Unicode coordinates and a separate zero-based UTF-8 offset."
  (let* ((prefix (substring text 0 offset))
         (last (or (cl-position ?\n prefix :from-end t) -1)))
    (list (cons "line" (1+ (cl-count ?\n prefix)))
          (cons "column" (- offset last))
          (cons "byte_offset" (string-bytes (encode-coding-string prefix 'utf-8-unix)))
          (cons "encoding" "utf-8"))))

(defun nelisp-dev-source--parse (text)
  "Read forms with exact reader spans, without evaluating any form."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (forms problem start)
      (condition-case err
          (while (progn
                   (skip-chars-forward " \t\r\n")
                   (while (eq (char-after) ?\;)
                     (forward-line 1) (skip-chars-forward " \t\r\n"))
                   (not (eobp)))
            (setq start (1- (point)))
            ;; EOF here is inside a form, not normal EOF between forms.
            (let ((form (read (current-buffer))))
              (push (list :form form :start start :end (1- (point))) forms)))
        (error (setq problem (list :message (error-message-string err)
                                  :start (or start 0) :end (1- (point))))))
      (list :forms (nreverse forms) :problem problem))))

(defun nelisp-dev-source--read (root path)
  "Read a bounded regular UTF-8 file inside ROOT and preserve its byte hash."
  (unless (and (stringp path) (> (length path) 0))
    (error "arguments.path (or each files entry) must be a nonempty string"))
  (let ((file (expand-file-name path root)))
    (unless (and (file-in-directory-p file root) (file-regular-p file)
                 (file-readable-p file))
      (error "Source must be a readable regular file inside the project"))
    (when (> (file-attribute-size (file-attributes file)) nelisp-dev-source--max-file-bytes)
      (error "Source exceeds 2 MiB input limit"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 (1+ nelisp-dev-source--max-file-bytes))
      (when (> (buffer-size) nelisp-dev-source--max-file-bytes)
        (error "Source grew beyond input limit"))
      (let* ((raw (buffer-string))
             (text (decode-coding-string raw 'utf-8-unix))
             (parsed (nelisp-dev-source--parse text)))
        ;; Reject malformed UTF-8 instead of reporting byte-valued characters
        ;; as Unicode scalar columns.
        (when (cl-some (lambda (ch) (> ch #x10ffff)) (string-to-list text))
          (error "Source is not valid UTF-8"))
        (list :path (file-relative-name file root) :text text
              :hash (secure-hash 'sha256 raw)
              :forms (plist-get parsed :forms) :problem (plist-get parsed :problem))))))

(defun nelisp-dev-source--definition (record text)
  (let ((form (plist-get record :form)))
    (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
               (symbolp (cadr form)) (proper-list-p (nth 2 form)))
      (list (cons "symbol" (symbol-name (cadr form)))
            (cons "kind" (symbol-name (car form)))
            (cons "signature" (prin1-to-string (nth 2 form)))
            (cons "documentation" (if (stringp (nth 3 form)) (nth 3 form) ""))
            (cons "start" (nelisp-dev-source--position text (plist-get record :start)))
            (cons "end" (nelisp-dev-source--position text (plist-get record :end)))
            (cons "provenance" "known: reader definition")
            (cons "effects" ["unknown"])))))

;;;###autoload
(defun nelisp-dev-source-symbols (text)
  "Return top-level source declarations in TEXT without evaluation or expansion.
Function and variable documentation is literal reader data.  Positions share
the development protocol's Unicode coordinates and UTF-8 byte offsets."
  (let* ((parsed (nelisp-dev-source--parse text))
         (problem (plist-get parsed :problem)))
    (when problem (error "Source syntax: %s" (plist-get problem :message)))
    (nelisp-dev-source--declarations (plist-get parsed :forms) text)))

;;;###autoload
(defun nelisp-dev-source-test-symbols (text)
  "Return literal top-level ert-deftest declarations in TEXT without execution.
Quoted, nested and macro-generated tests are not source declarations here.
Malformed source is rejected, and duplicate names retain their last location."
  (let* ((parsed (nelisp-dev-source--parse text))
         (problem (plist-get parsed :problem))
         names)
    (when problem (error "Source syntax: %s" (plist-get problem :message)))
    (dolist (record (plist-get parsed :forms))
      (let ((form (plist-get record :form)))
        (when (and (proper-list-p form) (eq (car-safe form) 'ert-deftest)
                   (symbolp (cadr form)) (cadr form) (>= (length form) 3)
                   (null (nth 2 form)))
          (let ((name (symbol-name (cadr form))))
            (setq names (cl-remove name names :key (lambda (item) (cdr (assoc "name" item))) :test #'equal))
            (push (list (cons "name" name)
                        (cons "line" (cdr (assoc "line" (nelisp-dev-source--position text (plist-get record :start))))))
                  names)))))
    (vconcat (nreverse names))))

;;;###autoload
(defun nelisp-dev-source-reader-builtins (text)
  "Read unconditional reader builtin names from build source TEXT as data.
Never load the build driver or evaluate conditional target/reload expressions."
  (let* ((parsed (nelisp-dev-source--parse text))
         (records (cl-remove-if-not
                   (lambda (record)
                     (let ((form (plist-get record :form)))
                       (and (eq (car-safe form) 'defconst)
                            (eq (cadr form) 'nelisp-standalone--reader-builtins))))
                   (plist-get parsed :forms))))
    (unless (and (not (plist-get parsed :problem)) (= (length records) 1))
      (error "Expected one complete reader builtin registration table"))
    (let* ((value (nth 2 (plist-get (car records) :form)))
           (quoted (and (eq (car-safe value) 'append) (cadr value)))
           (names (and (eq (car-safe quoted) 'quote) (cadr quoted))))
      (unless (and (proper-list-p names) names (<= (length names) 4096)
                   (cl-every (lambda (name) (and (stringp name) (< 0 (length name) 257))) names)
                   (= (length names) (length (delete-dups (copy-sequence names)))))
        (error "Unsupported literal reader builtin registration table"))
      (vconcat (mapcar (lambda (name)
                        (list (cons "symbol" name) (cons "kind" "builtin")
                              (cons "signature" "") (cons "documentation" "NeLisp reader builtin.")
                              (cons "insert_text" (prin1-to-string (intern name))))) names)))))

;;;###autoload
(defun nelisp-dev-source-completion-symbols (text)
  "Return declarations read before the first syntax error in TEXT.
This editor completion path deliberately accepts incomplete input; strict
source inspection continues to use `nelisp-dev-source-symbols'. The insert_text
field prints the symbol with reader escapes, without evaluating source."
  (let ((symbols (nelisp-dev-source--declarations
                  (plist-get (nelisp-dev-source--parse text) :forms) text)))
    (vconcat
     (mapcar (lambda (item)
               (append item
                       (list (cons "insert_text"
                                   (prin1-to-string (intern (cdr (assoc "symbol" item))))))))
             symbols))))

(defun nelisp-dev-source--declarations (records text)
  "Extract literal declarations from already-read RECORDS in TEXT."
  (let (symbols)
    (dolist (record records)
      (let* ((form (plist-get record :form))
             (definition (nelisp-dev-source--definition record text)))
        (when (and (not definition) (consp form)
                   (memq (car form) '(defvar defconst defcustom))
                   (symbolp (cadr form)))
          (setq definition
                (list (cons "symbol" (symbol-name (cadr form)))
                      (cons "kind" (symbol-name (car form)))
                      (cons "signature" "")
                      (cons "documentation" (if (stringp (nth 3 form)) (nth 3 form) ""))
                      (cons "start" (nelisp-dev-source--position text (plist-get record :start)))
                      (cons "end" (nelisp-dev-source--position text (plist-get record :end))))))
        (when definition (push definition symbols))))
    (vconcat (nreverse symbols))))

(defun nelisp-dev-source--children (record)
  "Read immediate children of an unprefixed list RECORD in the current buffer."
  (save-excursion
    (goto-char (1+ (plist-get record :start)))
    (when (and (eq (char-after) ?\()
               (proper-list-p (plist-get record :form)))
      (forward-char 1)
      (let (children)
        (while (progn
                 (skip-chars-forward " \t\r\n")
                 (while (eq (char-after) ?\;)
                   (forward-line 1) (skip-chars-forward " \t\r\n"))
                 (not (eq (char-after) ?\))))
          (let* ((start (1- (point))) (form (read (current-buffer))))
            (push (list :form form :start start :end (1- (point))) children)))
        (nreverse children)))))

(defun nelisp-dev-source--name-at (record offset namespace)
  "Describe a symbol token RECORD at OFFSET in NAMESPACE."
  (when (and record (symbolp (plist-get record :form))
             (<= (plist-get record :start) offset)
             (or (< offset (plist-get record :end))
                 (and (= offset (plist-get record :end))
                      (memq (char-after (1+ offset)) '(nil ?\s ?\t ?\r ?\n ?\))))))
    (list :symbol (symbol-name (plist-get record :form)) :namespace namespace
          :start (plist-get record :start) :end (plist-get record :end))))

(defun nelisp-dev-source--function-argument (record)
  "Return the symbol token of a literal function quote RECORD, if any."
  (let ((form (plist-get record :form)))
    (when (and (eq (car-safe form) 'function) (proper-list-p form)
               (= (length form) 2) (symbolp (cadr form))
               (eq (intern-soft (symbol-name (cadr form))) (cadr form)))
      (save-excursion
        (goto-char (1+ (plist-get record :start)))
        (if (looking-at "#'")
            (progn
              (forward-char 2)
              (skip-chars-forward " \t\r\n")
              (while (eq (char-after) ?\;)
                (forward-line 1) (skip-chars-forward " \t\r\n"))
              ;; The outer reader already identified the symbol and its end.
              (list :form (cadr form) :start (1- (point)) :end (plist-get record :end)))
          (nth 1 (nelisp-dev-source--children record)))))))

(defvar nelisp-dev-source--scope-observer nil
  "Optional internal callback observing the scope at a queried variable token.")

(defun nelisp-dev-source--lookup-variable (record offset variables)
  "Resolve a variable token against enclosing source VARIABLES first."
  (let ((found (nelisp-dev-source--name-at record offset "variable")) binding blocked)
    (when (and found nelisp-dev-source--scope-observer)
      (funcall nelisp-dev-source--scope-observer variables))
    (while (and variables (not binding) (not blocked))
      (let ((entry (pop variables)))
        (cond ((eq (car entry) :unknown-bindings) (setq blocked t))
              ((eq (car entry) (plist-get record :form)) (setq binding (cdr entry))))))
    (cond ((and found binding)
           (setq found (plist-put found :binding binding))
           (plist-put found :declaration (= (plist-get record :start) (plist-get binding :start))))
          (blocked nil)
          (t found))))

(defun nelisp-dev-source--parameter-scope (record offset variables)
  "Extend VARIABLES with simple parameters and inspect their declaration names."
  (if (not (and record (nelisp-dev-source--plain-arity (plist-get record :form))))
      (list :variables (cons '(:unknown-bindings . t) variables))
    (let (parameters found)
      (dolist (argument (nelisp-dev-source--children record))
        (unless (memq (plist-get argument :form) '(&optional &rest))
          (push (cons (plist-get argument :form)
                      (append argument '(:binding-kind "parameter"))) parameters)))
      (setq variables (append parameters variables))
      (dolist (binding parameters)
        (unless found
          (setq found (nelisp-dev-source--lookup-variable (cdr binding) offset variables))))
      (list :variables variables :found found))))

(defun nelisp-dev-source--lookup-form (record offset functions macros &optional top variables)
  "Find a declaration, variable reference, or call in a known evaluation context.
FUNCTIONS are unique plain source functions. MACROS prevent argument descent.
VARIABLES maps enclosing source bindings. Unknown binding syntax blocks global
variable fallback. Local function scopes and unrecognised forms remain opaque."
  (when (and (<= (plist-get record :start) offset)
             (<= offset (plist-get record :end)))
    (if (symbolp (plist-get record :form))
        (nelisp-dev-source--lookup-variable record offset variables)
      (when (and (consp (plist-get record :form))
                 (proper-list-p (plist-get record :form)))
        (let* ((quoted-function (nelisp-dev-source--function-argument record))
               (children (unless quoted-function (nelisp-dev-source--children record)))
               (head (plist-get (car children) :form))
               (declaration (memq head '(defun cl-defun defmacro defvar defconst defcustom)))
               found scan)
          (cond
           (quoted-function
            (setq found (nelisp-dev-source--name-at quoted-function offset "function")))
           ((and top declaration)
            (setq found (nelisp-dev-source--name-at
                         (nth 1 children) offset
                         (if (memq head '(defvar defconst defcustom)) "variable" "function")))
            (when found (setq found (plist-put found :declaration t)))
            (setq scan (if (memq head '(defvar defconst defcustom))
                           (and (nth 2 children) (list (nth 2 children)))
                         (let ((scope (nelisp-dev-source--parameter-scope (nth 2 children) offset variables)))
                           (setq variables (plist-get scope :variables)
                                 found (or found (plist-get scope :found)))
                           (nthcdr 3 children)))))
           ((or (null children) declaration
                (memq head '(quote function flet labels cl-flet cl-labels macrolet cl-macrolet))) nil)
           (t
            (setq found (nelisp-dev-source--name-at (car children) offset "function"))
            (unless (memq head macros)
              (cond
               ((or (memq head '(if and or progn prog1 prog2 when unless while))
                    (memq head functions))
                (setq scan (cdr children)))
               ((eq head 'lambda)
                (let ((scope (nelisp-dev-source--parameter-scope (nth 1 children) offset variables)))
                  (setq variables (plist-get scope :variables)
                        found (or found (plist-get scope :found)) scan (cddr children))))
               ((memq head '(let let*))
                (let ((outer variables)
                      (bindings (nth 1 children)))
                  (unless (and bindings (proper-list-p (plist-get bindings :form)))
                    (push '(:unknown-bindings . t) variables))
                  (dolist (binding (and bindings (nelisp-dev-source--children bindings)))
                    (let* ((parts (nelisp-dev-source--children binding))
                           (name (if (symbolp (plist-get binding :form)) binding (car parts)))
                           (symbol (plist-get name :form))
                           (value (nth 1 parts)))
                      (if (not (and name (symbolp symbol) (not (memq symbol '(nil t)))
                                    (<= (length parts) 2)))
                          (push '(:unknown-bindings . t) variables)
                        ;; Parallel let initializers see only the outer scope.
                        (when (and value (not found))
                          (setq found (nelisp-dev-source--lookup-form
                                       value offset functions macros nil
                                       (if (eq head 'let*) variables outer))))
                        (push (cons symbol (append name '(:binding-kind "binding"))) variables)
                        (unless found
                          (setq found (nelisp-dev-source--lookup-variable name offset variables))))))
                  (setq scan (cddr children))))
               ((eq head 'setq)
                (let ((rest (cdr children)))
                  (while (cdr rest)
                    (unless found
                      (setq found (nelisp-dev-source--lookup-variable (car rest) offset variables)))
                    (push (cadr rest) scan) (setq rest (cddr rest)))))))))
          (while (and scan (not found))
            (setq found (nelisp-dev-source--lookup-form (pop scan) offset functions macros nil variables)))
          found)))))

(defvar nelisp-dev-source--lookup-context nil
  "Dynamically scoped classification shared within one source reference query.")

(defun nelisp-dev-source--prepare-lookup-context (records text external)
  "Classify RECORDS and EXTERNAL once for unchanged TEXT."
  (let ((definitions (nelisp-dev-source--declarations records text))
        (counts (make-hash-table :test 'eq))
        (local-names (and external (make-hash-table :test 'equal)))
        (custom-names (and external (make-hash-table :test 'equal))) functions macros)
    (when external
      (mapc (lambda (item)
              (when (member (cdr (assoc "kind" item)) '("defun" "cl-defun" "defmacro"))
                (puthash (cdr (assoc "symbol" item)) t local-names))) definitions))
    (let ((visible (if external
                       (append definitions
                               (cl-remove-if (lambda (item) (gethash (cdr (assoc "symbol" item)) local-names)) external) nil)
                     definitions)))
      (when external
        (mapc (lambda (item)
                (when (member (cdr (assoc "kind" item)) '("defun" "cl-defun" "defmacro"))
                  (puthash (cdr (assoc "symbol" item)) t custom-names))) visible))
      (mapc (lambda (item)
              (let ((name (intern (cdr (assoc "symbol" item))))
                    (kind (cdr (assoc "kind" item))))
                ;; Any authored callable overrides the native fallback, even
                ;; when duplicate authored declarations make it ambiguous.
                (unless (and (equal kind "builtin") (gethash (symbol-name name) custom-names))
                  (when (member kind '("defun" "cl-defun" "defmacro" "builtin"))
                    (puthash name (1+ (gethash name counts 0)) counts))
                  (cond ((equal kind "defmacro") (push name macros))
                        ((member kind '("defun" "cl-defun" "builtin")) (push name functions))))))
            visible))
    (list :text text :records records :external external :definitions definitions
          :functions (cl-remove-if-not (lambda (name) (= (gethash name counts 0) 1)) functions)
          :macros macros)))

;;;###autoload
(defun nelisp-dev-source-lookup (text offset &optional parsed unresolved external)
  "Resolve a declaration, variable reference, or known-context call at OFFSET.
Only complete forms before the first reader error are considered. Return nil
for strings, comments, quoted data, local function scopes, or unknown contexts.
Simple parameters and let/let* bindings take precedence over global variable
declarations. Unknown binding syntax blocks fallback through that scope.
Results are source declarations, never a claim about loaded runtime bindings.
PARSED optionally supplies the unchanged TEXT's internal reader snapshot.
UNRESOLVED retains a known-context token even without a local declaration, for
workspace candidate search. EXTERNAL supplies callable source declarations;
only unique non-macro functions allow argument traversal. Local declarations
take precedence. This does not establish module or runtime visibility."
  (unless (and (integerp offset) (<= 0 offset) (<= offset (length text)))
    (error "Source offset must be an integer inside the text"))
  (let* ((records (plist-get (or parsed (nelisp-dev-source--parse text)) :forms))
         (context (if (and nelisp-dev-source--lookup-context
                           (eq text (plist-get nelisp-dev-source--lookup-context :text))
                           (eq records (plist-get nelisp-dev-source--lookup-context :records))
                           (eq external (plist-get nelisp-dev-source--lookup-context :external)))
                      nelisp-dev-source--lookup-context
                    (nelisp-dev-source--prepare-lookup-context records text external)))
         (definitions (plist-get context :definitions))
         (functions (plist-get context :functions)) (macros (plist-get context :macros)) found matches)
    (with-temp-buffer
      (insert text)
      (while (and records (not found))
        (setq found (nelisp-dev-source--lookup-form (pop records) offset functions macros t))))
    (when found
      (if (plist-get found :binding)
          (let ((binding (plist-get found :binding)))
            (push (list (cons "symbol" (plist-get found :symbol))
                        (cons "kind" (plist-get binding :binding-kind))
                        (cons "signature" "") (cons "documentation" "")
                        (cons "start" (nelisp-dev-source--position text (plist-get binding :start)))
                        (cons "end" (nelisp-dev-source--position text (plist-get binding :end)))) matches))
        (mapc (lambda (item)
              (when (and (equal (cdr (assoc "symbol" item)) (plist-get found :symbol))
                         (eq (not (member (cdr (assoc "kind" item)) '("defvar" "defconst" "defcustom")))
                             (equal (plist-get found :namespace) "function")))
                (push item matches)))
            definitions))
      (when (or matches unresolved)
        (list (cons "symbol" (plist-get found :symbol))
              (cons "namespace" (plist-get found :namespace))
              (cons "declaration" (if (plist-get found :declaration) t :false))
              (cons "offsets" (list (cons "start" (plist-get found :start))
                                    (cons "end" (plist-get found :end))))
              (cons "start" (nelisp-dev-source--position text (plist-get found :start)))
              (cons "end" (nelisp-dev-source--position text (plist-get found :end)))
              (cons "definitions" (vconcat (nreverse matches))))))))

;;;###autoload
(defun nelisp-dev-source-local-completions (text offset &optional external)
  "Return visible simple source bindings at OFFSET, without executing TEXT.
Repair only the cursor prefix in a private snapshot. Reuse lookup's evaluation
contexts, shadowing and unknown-binding barriers; opaque contexts yield none.
EXTERNAL supplies callable source declarations as in `nelisp-dev-source-lookup'."
  (unless (and (integerp offset) (<= 0 offset (length text)))
    (error "Source offset must be inside the text"))
  (nelisp-dev-source--ensure-lisp-mode)
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert (substring text 0 offset))
    (let ((state (condition-case err
                     (parse-partial-sexp (point-min) (point-max))
                   (error (signal (car err) (cdr err))))))
      (when (and state (>= (car state) 0) (not (nth 3 state)) (not (nth 4 state)) (not (nth 5 state)))
        (goto-char (point-max))
        (unless (and (char-before) (memq (char-syntax (char-before)) '(?w ?_)))
          (insert "__nelisp_completion_cursor__"))
        (insert "\n" (apply #'string (mapcar (lambda (start) (if (eq (char-after start) ?\[) ?\] ?\)))
                                           (reverse (nth 9 state)))))
        (let* ((snapshot (buffer-string))
               (parsed (nelisp-dev-source--parse snapshot))
               scope
               (nelisp-dev-source--scope-observer (lambda (variables) (setq scope variables)))
               (lookup (unless (plist-get parsed :problem)
                         (nelisp-dev-source-lookup snapshot offset parsed t external)))
               seen result)
          (unless (eq (cdr (assoc "declaration" lookup)) t)
            (while (and scope (not (eq (caar scope) :unknown-bindings)))
              (let* ((entry (pop scope)) (symbol (car entry)))
                (unless (memq symbol seen)
                  (push symbol seen)
                  (push (list (cons "symbol" (symbol-name symbol))
                              (cons "kind" (plist-get (cdr entry) :binding-kind))
                              (cons "signature" "") (cons "documentation" "")
                              (cons "insert_text" (prin1-to-string symbol))) result)))))
          (vconcat (nreverse result)))))))

;;;###autoload
(defun nelisp-dev-source-signature (text offset &optional external)
  "Return a plain source call signature and active argument at OFFSET.
For unfinished input, close the prefix's open delimiters in a private reader
snapshot. Never edit or execute TEXT. Strings, comments, quoted/opaque contexts
and unsupported lambda lists return nil. Complete input retains forward defs.
EXTERNAL contains source declaration candidates used only for unresolved calls."
  (unless (and (integerp offset) (<= 0 offset (length text)))
    (error "Source offset must be inside the text"))
  (nelisp-dev-source--ensure-lisp-mode)
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert text)
    (let* ((state (condition-case err
                      (parse-partial-sexp (point-min) (1+ offset))
                    (error (signal (car err) (cdr err)))))
           (open (nth 1 state)))
      (when (and open (> (car state) 0) (not (nth 3 state)) (not (nth 4 state)) (not (nth 5 state)))
        (let* ((parsed (nelisp-dev-source--parse text))
               (snapshot (if (plist-get parsed :problem)
                             (concat (substring text 0 offset) "\n"
                                     (apply #'string (mapcar (lambda (start) (if (eq (char-after start) ?\[) ?\] ?\)))
                                                            (reverse (nth 9 state)))))
                           text)))
          (unless (equal snapshot text) (setq parsed (nelisp-dev-source--parse snapshot)))
          (unless (plist-get parsed :problem)
            (erase-buffer) (insert snapshot)
            (let ((pending (copy-sequence (plist-get parsed :forms))) call)
              (while (and pending (not call))
                (let ((record (pop pending)))
                  (if (= (plist-get record :start) (1- open))
                      (setq call record)
                    (when (<= (plist-get record :start) (1- open) (plist-get record :end))
                      (setq pending (append (nelisp-dev-source--children record) pending))))))
              (let* ((children (and call (nelisp-dev-source--children call)))
                     (head (car children))
                     (lookup (and head (>= offset (plist-get head :end))
                                  (nelisp-dev-source-lookup snapshot (plist-get head :start) parsed t external)))
                     (definitions (cdr (assoc "definitions" lookup))))
                (when (and lookup (= (length definitions) 0)
                           (equal (cdr (assoc "namespace" lookup)) "function"))
                  (setq definitions
                        (vconcat (cl-remove-if-not
                                  (lambda (definition)
                                    (and (equal (cdr (assoc "symbol" definition))
                                                (cdr (assoc "symbol" lookup)))
                                         (member (cdr (assoc "kind" definition))
                                                 '("defun" "cl-defun" "defmacro"))))
                                  external))))
                (when (and (= (length definitions) 1)
                           (member (cdr (assoc "kind" (aref definitions 0))) '("defun" "cl-defun" "defmacro")))
                  (let* ((definition (aref definitions 0))
                         (signature (cdr (assoc "signature" definition)))
                         (args (car (read-from-string signature)))
                         (arity (nelisp-dev-source--plain-arity args))
                         (prefix (concat (cdr (assoc "symbol" definition)) " "))
                         (active (cl-count-if (lambda (child) (< (plist-get child :end) offset)) (cdr children)))
                         parameters)
                    (when arity
                      (with-temp-buffer
                        (insert signature)
                        (dolist (argument (nelisp-dev-source--children (list :form args :start 0 :end (length signature))))
                          (unless (memq (plist-get argument :form) '(&optional &rest))
                            (push (list (cons "label" (prin1-to-string (plist-get argument :form)))
                                        (cons "start" (+ (length prefix) (plist-get argument :start)))
                                        (cons "end" (+ (length prefix) (plist-get argument :end)))) parameters))))
                      (setq parameters (nreverse parameters))
                      (list (cons "label" (concat prefix signature))
                            (cons "documentation" (cdr (assoc "documentation" definition)))
                            (cons "parameters" (vconcat parameters))
                            (cons "active_parameter" (if parameters (min active (1- (length parameters))) :null))))))))))))))

(defun nelisp-dev-source--symbol-offsets (text parsed symbol)
  "Collect at most 4096 reader token offsets for SYMBOL in PARSED TEXT."
  (with-temp-buffer
    (insert text)
    (let ((pending (copy-sequence (plist-get parsed :forms))) (count 0) offsets)
      (while pending
        (let* ((record (pop pending)) (form (plist-get record :form)))
          (if (symbolp form)
              (when (equal (symbol-name form) symbol)
                (when (> (setq count (1+ count)) 4096)
                  (error "Source reference candidate limit exceeded"))
                (push (plist-get record :start) offsets))
            ;; Only the reader prefix needs synthetic children here. An
            ;; ordinary (function target) list could also be a parameter list.
            (let ((argument (and (eq (char-after (1+ (plist-get record :start))) ?#)
                                 (nelisp-dev-source--function-argument record))))
              (setq pending (append (if argument (list argument) (nelisp-dev-source--children record)) pending))))))
      (nreverse offsets))))

;;;###autoload
(defun nelisp-dev-source-occurrences (text name namespace &optional external)
  "Return supported global NAME occurrences in TEXT for NAMESPACE.
Do not treat local bindings, strings, comments or quoted/opaque data as global
references. Unresolved global tokens remain source candidates, not load proof."
  (unless (and (stringp name) (member namespace '("function" "variable")))
    (error "Global occurrences require a symbol name and namespace"))
  (let* ((parsed (nelisp-dev-source--parse text))
         (offsets (nelisp-dev-source--symbol-offsets text parsed name))
         (nelisp-dev-source--lookup-context
          (and offsets (nelisp-dev-source--prepare-lookup-context (plist-get parsed :forms) text external))) results)
    (dolist (offset offsets)
      (let* ((lookup (nelisp-dev-source-lookup text offset parsed t external))
             (definitions (cdr (assoc "definitions" lookup))))
        (when (and (equal (cdr (assoc "namespace" lookup)) namespace)
                   (not (cl-some (lambda (item) (member (cdr (assoc "kind" item)) '("parameter" "binding"))) definitions)))
          (push (list (cons "start" (cdr (assoc "start" lookup)))
                      (cons "end" (cdr (assoc "end" lookup)))
                      (cons "declaration" (cdr (assoc "declaration" lookup)))) results))))
    (vconcat (nreverse results))))

;;;###autoload
(defun nelisp-dev-source-references (text offset &optional parsed external)
  "Return same-document references to the unique source binding at OFFSET.
Read TEXT once; do not execute source or treat quoted data as references.
Use the same conservative evaluation contexts as `nelisp-dev-source-lookup'.
Ambiguous declarations yield no locations. Reject more than 4096 candidates.
PARSED optionally shares the unchanged TEXT's internal reader snapshot."
  (let* ((parsed (or parsed (nelisp-dev-source--parse text)))
         (nelisp-dev-source--lookup-context
          (nelisp-dev-source--prepare-lookup-context (plist-get parsed :forms) text external))
         (target (nelisp-dev-source-lookup text offset parsed nil external))
         (definitions (cdr (assoc "definitions" target)))
         (symbol (cdr (assoc "symbol" target)))
         results)
    (when (= (length definitions) 1)
      (dolist (candidate (nelisp-dev-source--symbol-offsets text parsed symbol))
        (let ((lookup (nelisp-dev-source-lookup text candidate parsed nil external)))
          (when (equal (cdr (assoc "definitions" lookup)) definitions)
            (push lookup results)))))
    (vconcat (nreverse results))))

(defun nelisp-dev-source--rename-safe-form-p (form functions macros)
  "Whether FORM has fully understood binding syntax for a local source edit.
FUNCTIONS contains unique plain source functions.
MACROS makes their calls opaque."
  (or (atom form)
      (and (proper-list-p form) (not (memq (car form) macros))
           (let ((head (car form)) (args (cdr form)))
             (cond
              ((eq head 'quote) (= (length args) 1))
              ((eq head 'lambda)
               (and (nelisp-dev-source--plain-arity (car args))
                    (cl-every (lambda (child) (nelisp-dev-source--rename-safe-form-p child functions macros)) (cdr args))))
              ((memq head '(let let*))
               (and (proper-list-p (car args))
                    (cl-every (lambda (binding)
                                (or (and (symbolp binding) (not (memq binding '(nil t))))
                                    (and (proper-list-p binding) (<= 1 (length binding) 2)
                                         (symbolp (car binding)) (not (memq (car binding) '(nil t)))
                                         (nelisp-dev-source--rename-safe-form-p (cadr binding) functions macros))))
                              (car args))
                    (cl-every (lambda (child) (nelisp-dev-source--rename-safe-form-p child functions macros)) (cdr args))))
              ((eq head 'setq)
               (and (= (% (length args) 2) 0)
                    (let ((rest args) (valid t))
                      (while rest
                        (unless (and (symbolp (pop rest))
                                     (nelisp-dev-source--rename-safe-form-p (pop rest) functions macros))
                          (setq valid nil rest nil)))
                      valid)))
              ((or (memq head '(if and or progn prog1 prog2 when unless while)) (memq head functions))
               (cl-every (lambda (child) (nelisp-dev-source--rename-safe-form-p child functions macros)) args)))))))

;;;###autoload
(defun nelisp-dev-source-rename (text offset &optional new-name)
  "Plan a local lexical source rename at OFFSET without executing TEXT.
Omit NEW-NAME to validate and prepare. Reject opaque owner syntax, incomplete
input, declared special variables and any existing destination symbol. This
does not resolve bindings introduced by external code or macro expansion."
  (unless (string-match-p "\\`[ \t]*;+.*-\\*-[ \t]*lexical-binding:[ \t]*t;[ \t]*-\\*-[ \t]*\\'"
                          (car (split-string text "\n")))
    (error "Local rename requires an explicit lexical-binding: t header"))
  (let* ((parsed (nelisp-dev-source--parse text))
         (records (plist-get parsed :forms))
         (target (nelisp-dev-source-lookup text offset parsed))
         (definitions (cdr (assoc "definitions" target)))
         (old-name (cdr (assoc "symbol" target)))
         (declarations (nelisp-dev-source--declarations records text))
         (owner (cl-find-if (lambda (record) (<= (plist-get record :start) offset (plist-get record :end))) records))
         (counts (make-hash-table :test 'eq)) functions macros edits)
    (when (plist-get parsed :problem) (error "Rename requires syntactically complete source"))
    (unless (and (= (length definitions) 1)
                 (not (string-match-p "\\`[&:]" old-name))
                 (member (cdr (assoc "kind" (aref definitions 0))) '("parameter" "binding")))
      (error "Rename currently requires a local parameter or let binding"))
    (mapc (lambda (item)
            (let ((name (intern (cdr (assoc "symbol" item)))) (kind (cdr (assoc "kind" item))))
              (when (and (equal (symbol-name name) old-name) (member kind '("defvar" "defconst" "defcustom")))
                (error "Cannot rename a declared special variable"))
              (when (member kind '("defun" "cl-defun" "defmacro"))
                (puthash name (1+ (gethash name counts 0)) counts))
              (when (equal kind "defmacro") (push name macros))
              (when (member kind '("defun" "cl-defun")) (push name functions)))) declarations)
    (setq functions (cl-remove-if-not (lambda (name) (= (gethash name counts 0) 1)) functions))
    (let* ((form (plist-get owner :form))
           (body (if (memq (car-safe form) '(defun cl-defun))
                     (progn (unless (nelisp-dev-source--plain-arity (nth 2 form)) (error "Unsupported parameter syntax"))
                            (nthcdr 3 form))
                   (list form))))
      (unless (cl-every (lambda (child) (nelisp-dev-source--rename-safe-form-p child functions macros)) body)
        (error "Rename cannot prove references through opaque syntax in the enclosing form")))
    (when new-name
      (unless (and (stringp new-name) (> (length new-name) 0) (<= (length new-name) 256)
                   (not (member new-name '("nil" "t"))) (not (string-match-p "\\`[&:]\\|[\n\r\0]" new-name)))
        (error "Invalid local variable name"))
      (unless (equal new-name old-name)
        (let ((pending (mapcar (lambda (record) (plist-get record :form)) records)))
          (while pending
            (let ((form (pop pending)))
              (cond ((and (symbolp form) (equal (symbol-name form) new-name))
                     (error "Destination symbol already occurs in this document"))
                    ((consp form) (push (cdr form) pending) (push (car form) pending))
                    ((vectorp form) (setq pending (append form pending)))))))
        (mapc (lambda (item)
                (push (list (cons "start" (cdr (assoc "start" item)))
                            (cons "end" (cdr (assoc "end" item)))
                            (cons "newText" (prin1-to-string (intern new-name)))) edits))
              (nelisp-dev-source-references text offset parsed))))
    (list (cons "target" target) (cons "edits" (vconcat (nreverse edits))))))

(defun nelisp-dev-source--plain-arity (lambda-list)
  "Return (MIN . MAX) for a simple plain defun LAMBDA-LIST.
MAX is nil for an `&rest' function.  Return nil for unsupported shapes."
  (catch 'invalid
    (unless (proper-list-p lambda-list) (throw 'invalid nil))
    (let ((state 'required) (required 0) (optional 0) rest names)
      (while lambda-list
        (let ((arg (pop lambda-list)))
          (cond
           ((eq arg '&optional)
            (unless (eq state 'required) (throw 'invalid nil))
            (setq state 'optional))
           ((eq arg '&rest)
            (when (or rest (not (= (length lambda-list) 1))
                      (not (symbolp (car lambda-list)))
                      (memq (car lambda-list) (append '(nil t) names))
                      (string-match-p "\\`[&:]" (symbol-name (car lambda-list))))
              (throw 'invalid nil))
            (pop lambda-list) (setq rest t state 'rest))
           ((memq arg '(&key &allow-other-keys &aux &body))
            (throw 'invalid nil))
           ((and (symbolp arg) (not (memq arg (append '(nil t) names)))
                 (not (string-match-p "\\`[&:]" (symbol-name arg))))
            (push arg names)
            (pcase state
              ('required (cl-incf required))
              ('optional (cl-incf optional))
              ('rest (throw 'invalid nil))))
           (t (throw 'invalid nil)))))
      (cons required (unless rest (+ required optional))))))

(defun nelisp-dev-source--arity-diagnostic (source record symbol arity actual form)
  (let ((diagnostic (nelisp-dev-source--diagnostic
   "NELISP-CHECK-ARITY" source (plist-get record :start) (plist-get record :end)
   (format "Call to %s has %d argument%s; expected %s"
           symbol actual (if (= actual 1) "" "s")
           (if (cdr arity)
               (format "%d..%d" (car arity) (cdr arity))
             (format "at least %d" (car arity))))
   (list symbol actual form (cl-incf nelisp-dev-source--diagnostic-sequence)))))
    (append diagnostic
            (list (cons "precision" "definition")
                  (cons "expected"
                        (list (cons "minimum_arguments" (car arity))
                              (cons "maximum_arguments" (or (cdr arity) :null))))
                  (cons "actual" (list (cons "arguments" actual)))))))

(defun nelisp-dev-source--known-call-forms (form defs macros diagnostics source)
  "Collect only arity violations provable from a reader form.
Unknown forms are deliberately opaque: they may be macros."
  (if (not (and (consp form) (proper-list-p form)))
      diagnostics
    (let ((head (car form)))
      (cond
       ((memq head macros) diagnostics)
       ((memq head '(quote function flet labels cl-flet cl-labels)) diagnostics)
       ((and (memq head '(if and or progn prog1 prog2 when unless while))
             (not (memq head macros)))
       (dolist (child (cdr form))
          (setq diagnostics
                (nelisp-dev-source--known-call-forms
                 child defs macros diagnostics source)))
        diagnostics)
       ((memq head '(let let*))
        (dolist (binding (and (proper-list-p (cadr form)) (cadr form)))
          (when (consp binding)
            (setq diagnostics
                  (nelisp-dev-source--known-call-forms
                   (cadr binding) defs macros diagnostics source))))
        (dolist (child (cddr form))
          (setq diagnostics
                (nelisp-dev-source--known-call-forms
                 child defs macros diagnostics source)))
        diagnostics)
       ((eq head 'setq)
        (let ((rest (cdr form)))
          (while (consp rest)
            (setq diagnostics
                  (nelisp-dev-source--known-call-forms
                   (cadr rest) defs macros diagnostics source)
                  rest (cddr rest)))
          diagnostics))
       ((and (symbolp head) (assoc head defs))
        (let* ((arity (cdr (assq head defs)))
               (actual (1- (length form))))
          (when (or (< actual (car arity))
                    (and (cdr arity) (> actual (cdr arity))))
            (push (nelisp-dev-source--arity-diagnostic
                   source (list :start (plist-get source :scan-start)
                                :end (plist-get source :scan-end))
                   (symbol-name head) arity actual form)
                  diagnostics))
          (dolist (child (cdr form))
            (setq diagnostics
                  (nelisp-dev-source--known-call-forms
                   child defs macros diagnostics source)))
          diagnostics))
       ((and (symbolp head) (memq head macros)) diagnostics)
       ;; An unrecognised head may be an unexpanded macro.  Do not inspect
       ;; its arguments, since that could turn a possible form into a false
       ;; arity error.
       (t diagnostics)))))

(defun nelisp-dev-source--calls (form)
  "Return syntactic call-head candidates, excluding quoted data.
Unknown macros and local function shadowing prevent these candidates from
establishing a complete runtime dependency graph."
  (when (and (consp form) (proper-list-p form))
    (let ((head (car form)))
      (cond
       ((memq head '(quote function)) nil)
       ((memq head '(let let*))
        (append
         (cl-mapcan (lambda (binding)
                      (when (consp binding) (nelisp-dev-source--calls (cadr binding))))
                    (and (proper-list-p (cadr form)) (cadr form)))
         (cl-mapcan #'nelisp-dev-source--calls (cddr form))))
       ((memq head '(setq setq-default))
        (let ((rest (cdr form)) calls)
          (while (consp rest)
            (setq calls (append calls (nelisp-dev-source--calls (cadr rest)))
                  rest (cddr rest))) calls))
       (t (append (and (symbolp head) (list head))
                  (cl-mapcan #'nelisp-dev-source--calls (cdr form))))))))

(defun nelisp-dev-source--diagnostic (code source start end message subject)
  (let* ((span (list (cons "start" (nelisp-dev-source--position (plist-get source :text) start))
                     (cons "end" (nelisp-dev-source--position (plist-get source :text) end))))
         (id (secure-hash 'sha256
                          (prin1-to-string (list code (plist-get source :hash)
                                                (plist-get source :path) span subject)))))
    (list (cons "id" id) (cons "code" code) (cons "phase" "source")
          (cons "path" (plist-get source :path)) (cons "span" span)
          (cons "message" message))))

(defun nelisp-dev-source-dispatch (request context)
  "Describe, parse-check or inspect call candidates in explicitly named files.
This is a bounded in-memory index fallback, not a replacement repository index.
It does not run application forms, macros, source loaders, or dependency builds."
  (let* ((op (cdr (assoc "operation" request)))
         (root (file-name-as-directory (expand-file-name
                                       (or (plist-get context :root) default-directory))))
         (paths (or (nelisp-dev-source--arg request "files")
                    (vector (nelisp-dev-source--arg request "path"))))
         (symbol (nelisp-dev-source--arg request "symbol"))
         sources diagnostics definitions edges errors)
    (setq nelisp-dev-source--diagnostic-sequence 0)
    (condition-case err
        (progn
          (unless (and (vectorp paths) (< 0 (length paths))
                       (<= (length paths) nelisp-dev-source--max-files))
            (error "Explicit path or files array of 1..32 paths is required"))
          (unless (or (equal op "check") (and (stringp symbol) (> (length symbol) 0)))
            (error "arguments.symbol is required"))
          (dolist (path (delete-dups (sort (append paths nil) #'string<)))
            (let* ((source (nelisp-dev-source--read root path))
                   (problem (plist-get source :problem)))
              (push source sources)
              (when problem
                (setq errors (1+ (or errors 0)))
                (push (nelisp-dev-source--diagnostic
                       "NELISP-CHECK-SYNTAX" source (plist-get problem :start)
                       (plist-get problem :end) (plist-get problem :message) "reader")
                      diagnostics))
              nil)))
      (error
       (setq errors (1+ (or errors 0)))
       (push (list (cons "code" "NELISP-SOURCE-INPUT")
                   (cons "message" (error-message-string err))) diagnostics)))
    (setq sources (nreverse sources)
          edges (nreverse edges) diagnostics (nreverse diagnostics))
    ;; Build the index only from uniquely declared, simple plain defuns.
    (let (plain macros all-names)
      (dolist (source sources)
        (dolist (record (plist-get source :forms))
          (let* ((form (plist-get record :form))
                 (definition (nelisp-dev-source--definition record
                                                            (plist-get source :text))))
            (when definition
              (push (cdr (assoc "symbol" definition)) all-names)
              (when (equal symbol (cdr (assoc "symbol" definition)))
                (push (append (list (cons "path" (plist-get source :path))) definition)
                      definitions))
              (when (and (equal op "impact")
                         (cl-some (lambda (head) (equal (symbol-name head) symbol))
                                  (cl-mapcan #'nelisp-dev-source--calls (cdddr form))))
                (push (list (cons "caller" (cdr (assoc "symbol" definition)))
                            (cons "path" (plist-get source :path))
                            (cons "kind" "syntactic-call-candidate")
                            (cons "provenance" "inferred: unexpanded reader form"))
                      edges))
              (cond
               ((equal "defmacro" (cdr (assoc "kind" definition)))
                (push (intern (cdr (assoc "symbol" definition))) macros))
               ((and (equal "defun" (cdr (assoc "kind" definition)))
                     (nelisp-dev-source--plain-arity (nth 2 form)))
                (let ((name (cadr form)))
                  (push (cons name
                              (cons (nelisp-dev-source--plain-arity (nth 2 form))
                                    (cdr (assq name plain))))
                        plain)))))))
      ;; Any duplicate declaration makes the identity unknown and therefore
      ;; removes the name from the provable index.
      (let (unique)
        (dolist (entry plain)
          (when (and (cdr entry) (null (cddr entry))
                     (= 1 (cl-count (symbol-name (car entry)) all-names :test #'equal)))
            (push (cons (car entry) (cadr entry)) unique)))
        (setq plain unique))
      (dolist (source sources)
        (dolist (record (plist-get source :forms))
          (let ((form (plist-get record :form)))
            (when (and (equal op "check") (consp form) (eq (car form) 'defun)
                       (symbolp (cadr form))
                       (assq (cadr form) plain))
              (let ((source-copy (copy-sequence source)))
                (plist-put source-copy :scan-start (plist-get record :start))
                (plist-put source-copy :scan-end (plist-get record :end))
                (dolist (body (cdddr form))
                  (setq diagnostics
                        (nelisp-dev-source--known-call-forms
                         body plain macros diagnostics source-copy))))))))))
    (setq definitions (nreverse definitions)
          diagnostics (nreverse diagnostics))
    (when (equal op "check")
      (let ((arity-errors
             (cl-count-if (lambda (diagnostic)
                            (equal "NELISP-CHECK-ARITY"
                                   (cdr (assoc "code" diagnostic))))
                          diagnostics)))
        (when (> arity-errors 0)
          (setq errors (+ (or errors 0) arity-errors)))))
    (let ((identity
           (list (cons "target" (or (plist-get context :target) "host-emacs"))
                 (cons "source_revision" :null)
                 (cons "source_content_hash"
                       (cond ((null sources) :null)
                             ((null (cdr sources)) (concat "sha256:" (plist-get (car sources) :hash)))
                             (t (concat "sha256:"
                                        (secure-hash 'sha256
                                         (prin1-to-string
                                          (mapcar (lambda (s) (cons (plist-get s :path)
                                                                    (plist-get s :hash))) sources)))))))
                 (cons "runtime_artifact_hash" :null)
                 (cons "session_id" (or (plist-get context :session-id) :null))
                 (cons "generation" :null))))
      (nelisp-dev-protocol-envelope
       op (cdr (assoc "request_id" request))
       (cond (errors "failed")
             ((and (equal op "describe") (= (length definitions) 1)) "ok")
             (t "inconclusive"))
       identity
       (list (cons "errors" (or errors 0)) (cons "files" (length sources))
             (cons "matches" (length definitions)) (cons "callers" (length edges)))
       (vconcat diagnostics)
       (list (cons "definitions" (vconcat definitions)) (cons "callers" (vconcat edges))
             (cons "coverage" "Explicit files only; syntax and parsed definitions"))
       ["No application code or macro was executed."
        "Arity coverage is limited to uniquely declared plain defuns in the explicit input set."
        "Dynamic calls, macro expansion and complete dependency closure are not proven."
        "Loaded artifact identity, runtime contracts and effects remain unknown."
        "Multiple source definitions do not identify which definition is currently loaded."]))))

(provide 'nelisp-dev-source)
;;; nelisp-dev-source.el ends here
