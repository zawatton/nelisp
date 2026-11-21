;;; compall-gen.el --- generate the native row table from a dynamic profile  -*- lexical-binding: t; -*-
;; usage (repository root): COMPALL_OUT=lisp/nelisp-fxir-native-data.el BINARY --load tools/ai/compall-gen.el
;; Optional: COMPALL_PIN (default tools/ai/compall2-pin.txt), COMPALL_SELECT (default
;; tools/ai/compall2-select.txt), COMPALL_MAX_ROWS (default `compall-gen-max-rows'),
;; COMPALL_MAX_IR (default `compall-gen-max-ir'), COMPALL_EXCLUDE=FILE (default
;; tools/ai/compall2-exclude.txt, one "NAME REASON"
;; per line: functions left out, e.g. the native-row census's STALE names).
;;
;; BINARY must be built from the same sources (the fx compiler and the prelude macros
;; decide the fxir; the host GNU Emacs does not reproduce them).  The candidates are
;; the names of COMPALL_PIN (the rows earlier lanes harvested, kept) and then those of
;; COMPALL_SELECT in its order: the prelude functions ranked by a dynamic profile of
;; the survey rows (tools/ai/compall2-prof.el, tools/ai/compall2-rank.py: executed
;; calls x per-call cost).  A candidate is compiled again (`nelisp-fxir-recompile', the
;; compile the install pass repeats at boot) and gets a row
;; (NAME ID NLOCALS CALLEES IR (MIN MAX REST)) when it is eligible:
;;   - its fx record is an ordinary one (not a string intrinsic or literal record);
;;   - the recompile gives the live record's code (else: recompile-differs);
;;   - its printed fxir is at most COMPALL_MAX_IR characters (else: ir-size N);
;;   - it is not part of the machinery that converts and checks rows: the fx compiler
;;     (nelisp--fx-*) and the byte-code JIT's dispatcher (lisp/nelisp-bytecode-jit.el),
;;     which the install pass and tools/ai/native-row-census.sh run while they convert
;;     or redefine a row's function;
;;   - its current source definition (the census's: lisp/*.el, scripts/*.el, the defuns
;;     scripts/nelisp-standalone-build.el embeds) compiles to the same code and fxir
;;     (else: source-differs; a function with no source definition uses the image's);
;;   - it is not listed in COMPALL_EXCLUDE;
;;   - it fits the metadata budget COMPALL_META_BUDGET (candidates in order, pinned
;;     ones free, each costing `compall-gen--meta-cost'): the rows' unit
;;     (fxir-native.o) adds symbols and relocations to the build's active-unit metadata,
;;     bounded at 4 MiB for all units (tools/ai/compall2-unit.el checks the bound
;;     offline before a build).
;; A byte-code function of the image (adopted by the build's byte-code pass) is compiled
;; from its source definition when it has exactly one (defined under a probe name); its
;; row makes the byte-code pass keep it as source (scripts/nelisp-prelude-bytecode.el),
;; as GNU loads a function's native code instead of its byte code.
;; Ids are `compall-gen-id-base' plus the rank of the name among the selected names in
;; `string<' order: no id is assigned by hand, and regenerating after a source change
;; renumbers deterministically.  Prints "GEN NAME ok ID", "GEN NAME skip REASON" and a
;; final "compall-gen: rows=N skipped=M".
(defvar compall-gen--boot-names
  (let ((names nil))
    (mapatoms (lambda (s) (when (and (fboundp s)
                                     (or (eq (car-safe (symbol-function s)) 'closure)
                                         (byte-code-function-p (symbol-function s))))
                            (push s names))))
    names)
  "The image's closures and byte-code functions before this tool loads anything (the tool's own are not rows).")
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)

(defconst compall-gen-id-base 4096
  "First generated row id: above the fx corpus rows (1000-2135,
lisp/nelisp-fxir-native-corpus-data.el) and the ids lanes harvested by hand.")
(defconst compall-gen-max-ir 8000
  "Largest printed fxir (characters) given a row; see the commentary.")
(defconst compall-gen-max-rows 2000
  "Most rows generated.")
(defconst compall-gen-meta-budget 350000
  "Estimated active-unit metadata bytes the selected (not pinned) rows may add.
v35 with raw-address reads: fxir-native.o's entry 577,589 bytes, room up to
1,213,221 (tools/ai/compall2-unit.el); the estimate is checked by that tool
before a build.")
(defun compall-gen--meta-cost (row)
  "Estimated metadata bytes of ROW: a symbol and its fixed relocations (about 600
bytes) plus about 100 per call node (the call-out's relocations).  Fitted on a
207-row trial: 414 KB for 2,901 call nodes."
  (+ 600 (* 100 (length (nth 3 row)))))

(defun compall-gen--name-list (file)
  "Symbols named one per line in FILE (the first word of each line); nil if FILE is unreadable."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((out nil))
        (dolist (l (split-string (buffer-string) "\n" t))
          (let ((w (car (split-string l " " t))))
            (when (and w (not (string-prefix-p "#" w))) (push (intern w) out))))
        (nreverse out)))))

;; Source definitions, found as tools/ai/native-row-census.el finds them: a defun at
;; top level of lisp/*.el or scripts/*.el (or under unless/when/progn/eval-and-compile/
;; if), or embedded in scripts/nelisp-standalone-build.el as string literals.
(defun compall-gen--read-forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms nil) (form t))
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (push form forms))
      (nreverse forms))))
(defun compall-gen--defuns (form tbl)
  (cond ((not (consp form)) nil)
        ((and (eq (car form) 'defun) (symbolp (nth 1 form)))
         (puthash (nth 1 form) (cons form (gethash (nth 1 form) tbl)) tbl))
        ((memq (car form) '(unless when progn eval-and-compile if))
         (dolist (f (cdr form)) (compall-gen--defuns f tbl)))))
(defun compall-gen--embedded (form tbl)
  (when (consp form)
    (let ((l form) (run nil))
      (while (consp l)
        (let ((x (car l)))
          (cond ((stringp x) (push x run))
                (t (compall-gen--embedded-flush run tbl) (setq run nil)
                   (when (consp x) (compall-gen--embedded x tbl)))))
        (setq l (cdr l)))
      (compall-gen--embedded-flush run tbl))))
(defun compall-gen--embedded-flush (run tbl)
  (when run
    (let ((text (apply #'concat (reverse run))) (pos 0))
      (while (string-match "(defun \\([^ \n()]+\\) " text pos)
        (let ((start (match-beginning 0)))
          (setq pos (match-end 0))
          (let ((form (condition-case nil (car (read-from-string text start)) (error nil))))
            (when (and (eq (car-safe form) 'defun) (symbolp (nth 1 form)))
              (puthash (nth 1 form) (cons form (gethash (nth 1 form) tbl)) tbl))))))))
(defun compall-gen--sources ()
  "NAME -> its source defuns, the last in file-name order first."
  (let ((tbl (make-hash-table :test 'eq)))
    (dolist (file (append (directory-files "lisp" t "\\.el\\'") (directory-files "scripts" t "\\.el\\'")))
      (when (and (not (string-match-p "native-data\\|native-corpus" file))
                 (with-temp-buffer (insert-file-contents file) (search-forward "(defun " nil t)))
        (dolist (form (condition-case nil (compall-gen--read-forms file) (error nil)))
          (compall-gen--defuns form tbl)
          (when (string-match-p "standalone-build" file) (compall-gen--embedded form tbl)))))
    tbl))
(defun compall-gen--closure-of (form &optional probe)
  "The closure source defun FORM defines, as `defun' installs it, under a probe
name PROBE (the real name keeps its image definition); nil if not a closure."
  (let ((probe (or probe (make-symbol "compall-gen-probe"))))
    (condition-case nil
        (progn (eval (cons 'defun (cons probe (nthcdr 2 form))) t)
               (let ((fn (symbol-function probe)))
                 (and (eq (car-safe fn) 'closure) fn)))
      (error nil))))
(defun compall-gen--source-same-p (forms ir code args)
  "Non-nil when one of FORMS compiles to IR and CODE with arglist ARGS."
  (let ((ok nil))
    (dolist (f forms)
      (unless ok
        (let* ((fn (compall-gen--closure-of f))
               (c (and fn (equal (nth 2 fn) args) (ignore-errors (nelisp-fxir-recompile fn)))))
          (when (and c (equal (nth 2 c) ir) (nelisp-fxir-code-same-p (car c) code))
            (setq ok t)))))
    ok))

(defun compall-gen--embedded-bytecode-providers ()
  "Names installed as byte code by actual pre-generated boot provider blocks.
The source-only boot does not reconstruct these definitions or their omitted
macros. A source defun found elsewhere therefore cannot make them eligible."
  (let ((names nil))
    (with-temp-buffer
      (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
      (goto-char (point-min))
      (while (re-search-forward "^;;; BEGIN GENERATED \\([^\n]+\\)$" nil t)
        (let* ((block (match-string 1)) (begin (point))
               (end (and (re-search-forward
                          (concat "^;;; END GENERATED " (regexp-quote block) "$") nil t)
                         (match-beginning 0))))
          (unless end (error "Generated provider block has no end: %s" block))
          (let ((text (buffer-substring-no-properties begin end)) (pos 0))
            (cl-labels
                ((walk (form)
                   (when (consp form)
                     (cond
                      ((and (eq (car form) 'fset)
                            (eq (car-safe (nth 1 form)) 'quote)
                            (symbolp (cadr (nth 1 form)))
                            (byte-code-function-p (nth 2 form)))
                       (cl-pushnew (cadr (nth 1 form)) names))
                      ((memq (car form) '(unless when if progn eval-and-compile))
                       (dolist (part (cdr form)) (walk part)))))))
              (condition-case nil
                  (while t
                    (let ((read-result (read-from-string text pos)))
                      (setq pos (cdr read-result))
                      (walk (car read-result))))
                (end-of-file nil)))))))
    (sort names (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defvar compall-gen--pinned nil "Names of COMPALL_PIN.")
(defvar compall-gen--bytecode t
  "Non-nil gives byte-code functions rows (compiled from their source definition).")

(let* ((out (or (getenv "COMPALL_OUT") "lisp/nelisp-fxir-native-data.el"))
       (max-ir (if (getenv "COMPALL_MAX_IR") (string-to-number (getenv "COMPALL_MAX_IR"))
                 compall-gen-max-ir))
       (exclude (make-hash-table :test 'eq))
       (exclude-file (or (getenv "COMPALL_EXCLUDE") "tools/ai/compall2-exclude.txt"))
       (names nil) (rows nil) (skipped 0) (src (compall-gen--sources))
       ;; The byte-code JIT's dispatcher, which every byte-code call goes through.
       (jit (let ((tbl (make-hash-table :test 'eq)))
              (dolist (f (compall-gen--read-forms "lisp/nelisp-bytecode-jit.el"))
                (compall-gen--defuns f tbl))
              tbl)))
  (when (file-readable-p exclude-file)
    (with-temp-buffer
      (insert-file-contents exclude-file)
      (dolist (l (split-string (buffer-string) "\n" t))
        (let ((w (split-string l " " t)))
          (puthash (intern (car w)) (or (cadr w) "excluded") exclude)))))
  ;; These exact byte-code adoption contracts are authoritative. Derive their
  ;; names instead of discovering one rejected provider after each full build.
  (let ((contracts '(("Native variable-alias bytecode selection drift" . "required-variable-alias-bytecode")
                     ("Native buffer-local bytecode selection drift" . "required-buffer-local-bytecode")))
        (found nil) (required nil))
    (cl-labels
        ((names (node reason)
           (when (consp node)
             (if (and (eq (car node) 'quote) (consp (cadr node))
                      (cl-every #'stringp (cadr node)))
                 (dolist (name (cadr node))
                   (cl-pushnew (intern name) required)
                   (puthash (intern name) reason exclude))
               (names (car node) reason)
               (names (cdr node) reason))))
         (walk (node)
           (when (consp node)
             (when (eq (car node) 'unless)
               (dolist (body (cddr node))
                 (let ((contract (and (consp body) (eq (car body) 'error)
                                      (assoc (cadr body) contracts))))
                   (when contract
                     (cl-pushnew (car contract) found :test #'equal)
                     (names (cadr node) (cdr contract))))))
             (walk (car node))
             (walk (cdr node)))))
      (dolist (form (compall-gen--read-forms "scripts/nelisp-standalone-build.el"))
        (walk form)))
    (unless (= (length found) (length contracts))
      (error "Required bytecode provider contract discovery drift: %S" found))
    (princ (format "compall-gen: authoritative bytecode providers %d names %S\n"
                   (length required) (sort required (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))
  ;; Pre-generated bytecode fsets bypass the defun keep-as-source transform.
  ;; Discover all such providers; do not silently harvest their unrelated source
  ;; definitions (which can depend on macros omitted by the block generator).
  (let* ((providers (compall-gen--embedded-bytecode-providers))
         (pin (compall-gen--name-list (or (getenv "COMPALL_PIN") "tools/ai/compall2-pin.txt"))))
    (dolist (name providers)
      (when (and (fboundp name) (byte-code-function-p (symbol-function name)))
        (when (memq name pin)
          (error "Pinned row unavailable: embedded bytecode provider %S" name))
        (puthash name "embedded-bytecode-provider-no-runtime-source" exclude)))
    (princ (format "compall-gen: embedded bytecode providers %d names %S\n"
                   (length providers) providers)))
  ;; Candidates: the pinned names, then the profile's selection in rank order.
  (let ((pin (compall-gen--name-list (or (getenv "COMPALL_PIN") "tools/ai/compall2-pin.txt")))
        (sel (compall-gen--name-list (or (getenv "COMPALL_SELECT") "tools/ai/compall2-select.txt")))
        (seen (make-hash-table :test 'eq)))
    (dolist (n (append pin sel))
      (unless (gethash n seen)
        (puthash n t seen)
        (if (memq n compall-gen--boot-names) (push n names)
          (setq skipped (1+ skipped))
          (princ (format "GEN %s skip not-a-boot-function\n" n)))))
    (setq names (nreverse names))
    (setq compall-gen--pinned pin))
  (dolist (name names)
    (let* ((bc (byte-code-function-p (symbol-function name)))
           (forms (gethash name src))
           ;; A byte-code function (adopted by the build's byte-code pass) is compiled
           ;; from its one source definition: with a row the pass keeps it as source.
           (probe (and bc compall-gen--bytecode (= (length forms) 1) (make-symbol "compall-gen-probe")))
           (fn (if bc (and probe (compall-gen--closure-of (car forms) probe))
                 (symbol-function name)))
           (rec (and fn (progn (if probe (ignore-errors (nelisp--fx-register-fn probe fn))
                                 (ignore-errors (nelisp--fx-register name)))
                               (gethash (nthcdr 3 fn) nelisp--fx-table))))
           (c (and (vectorp rec) (not (memq (aref rec 1) '(-1 -2)))
                   (ignore-errors (nelisp-fxir-recompile fn))))
           (irlen (and c (nth 2 c) (length (prin1-to-string (nth 2 c)))))
           (reason
            (cond ((gethash name exclude) (gethash name exclude))
                  ((and bc (not compall-gen--bytecode)) "bytecode-adopted")
                  ((and bc (null forms)) "bytecode-no-source")
                  ((and bc (null probe)) "bytecode-several-sources")
                  ((and bc (null fn)) "bytecode-source-not-closure")
                  ((not (vectorp rec)) (if bc "bytecode-fx-declined" "no-fx-record"))
                  ((memq (aref rec 1) '(-1 -2)) "intrinsic-record")
                  ((or (null c) (null (nth 2 c)) (not (nelisp-fxir-code-same-p (car c) (aref rec 0))))
                   "recompile-differs")
                  ((> irlen max-ir) (format "ir-size %d" irlen))
                  ;; The fx compiler and its registration: the install pass and
                  ;; the census run them while converting or checking rows.
                  ((string-prefix-p "nelisp--fx-" (symbol-name name)) "fx-compiler")
                  ((gethash name jit) "bytecode-jit")
                  ((and (gethash name src) (not bc)
                        (not (compall-gen--source-same-p (gethash name src) (nth 2 c) (car c) (nth 2 fn))))
                   "source-differs"))))
      (if reason
          (progn (setq skipped (1+ skipped)) (princ (format "GEN %s skip %s\n" name reason)))
        (push (list name nil (max 0 (nth 1 c)) (nelisp-fxir-callees (nth 2 c) (car c)) (nth 2 c)
                    (list (aref rec 2) (aref rec 5) (aref rec 6)))
              rows))))
  (setq rows (nreverse rows))
  ;; Budget: the first COMPALL_MAX_ROWS eligible candidates, pinned ones first.
  (let ((max-rows (if (getenv "COMPALL_MAX_ROWS") (string-to-number (getenv "COMPALL_MAX_ROWS"))
                    compall-gen-max-rows))
        (meta (if (getenv "COMPALL_META_BUDGET") (string-to-number (getenv "COMPALL_META_BUDGET"))
                compall-gen-meta-budget))
        (kept nil) (n 0) (used 0))
    (dolist (r rows)
      (let ((cost (compall-gen--meta-cost r)))
        (if (or (memq (car r) compall-gen--pinned)
                (and (< n max-rows) (<= (+ used cost) meta)))
            (progn (setq n (1+ n)) (push r kept)
                   (unless (memq (car r) compall-gen--pinned) (setq used (+ used cost))))
          (setq skipped (1+ skipped))
          (princ (format "GEN %s skip budget\n" (car r))))))
    (princ (format "compall-gen: metadata estimate of the selected rows %d of %d\n" used meta))
    (setq rows (sort kept (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
    (princ (format "compall-gen: max-rows %d\n" max-rows)))
  (let ((id compall-gen-id-base))
    (dolist (r rows)
      (setcar (cdr r) id)
      (princ (format "GEN %s ok %d\n" (car r) id))
      (setq id (1+ id))))
  (with-temp-file out
    (insert ";;; nelisp-fxir-native-data.el --- generated fxir rows (nativecomp)  -*- lexical-binding: t -*-\n\n"
            ";; GENERATED by tools/ai/compall-gen.el -- do not edit rows by hand.\n"
            ";; Rows (NAME ID NLOCALS CALLEES IR (MIN MAX REST)): the functions of a NeLisp\n"
            ";; image built from these sources named in tools/ai/compall2-pin.txt (rows earlier\n"
            ";; lanes harvested) and tools/ai/compall2-select.txt (ranked by a dynamic profile of\n"
            ";; the survey rows, tools/ai/compall2-rank.py) whose definition is a closure with an\n"
            ";; ordinary fx record, or byte code with one source definition, compiled as the\n"
            ";; install pass does, in rank order within the metadata budget.\n"
            (format ";; Ids are %d plus the rank of the name\n" compall-gen-id-base)
            ";; in `string<' order.  A stale row is harmless: the install pass compares fxir\n"
            ";; and callees and leaves a changed function on the word VM;\n"
            ";; tools/ai/native-row-census.sh finds stale rows.  Regenerate after a source\n"
            ";; change: COMPALL_OUT=lisp/nelisp-fxir-native-data.el BINARY --load tools/ai/compall-gen.el\n\n"
            "(defconst nelisp-fxir-native-data\n  '(")
    (let ((print-length nil) (print-level nil))
      (dolist (r rows) (insert (prin1-to-string r) "\n    ")))
    (insert "))\n\n(provide 'nelisp-fxir-native-data)\n;;; nelisp-fxir-native-data.el ends here\n"))
  (princ (format "compall-gen: rows=%d skipped=%d\n" (length rows) skipped)))
(kill-emacs 0)
