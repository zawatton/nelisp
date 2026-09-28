;;; nelisp-prelude-bytecode.el --- Safe prelude byte-code selection -*- lexical-binding: nil; -*-

;; GNU Emacs 31.1 is the build-time compiler.  Keep selection driven by the
;; actual byte-code and the standalone VM's reviewed opcode set, not a list of
;; function names.  Unsupported candidates stay source and are reported.

(require 'cl-lib)

(defconst nelisp-prelude-bytecode--opcodes
  '(0 8 16 24 32 40 48 49 50 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 75 79 80 81 83 84 85 86 87 88 89 90 92 93 94 95 129 130 131 132 133 134 135 136 137 142 152 154 155 157 158 159 160 161 162 166 167 168 175 178 182 183 192)
  "GNU byte-code opcodes admitted by the focused standalone prelude VM.
Opcode 32 is CALL (raw 32-39, arity 0-7 plus the explicit 1-/2-byte operand
widths): verified for both core-mode and the general prelude -- see the
`fixture' cond clauses below for what the general prelude still requires.
Opcodes 48/50 are POP-HANDLER/PUSH-CATCH, 56/69/70/88/89/90/92/93/94/166/175
are the NTH/LIST3/LIST4/LEQ/GEQ/DIFF/PLUS/MAX/MIN/REM/LISTN arithmetic and
list primitives, 0/49/57/59/60/68/129/157/182 are STACK-REF/PUSHCONDITIONCASE/
SYMBOLP/STRINGP/LISTP/LIST2/CONSTANT2/MEMBER/DISCARDN -- all implemented
natively by the standalone VM (see wf_bytecode_call_primitive and its
siblings in scripts/nelisp-standalone-build.el) without going through a
symbol's (possibly `fset'-rebound) function cell.

Opcode 95 (Bmult, \"*\") is fully implemented and parity-verified (see the
mult-* cases in tools/nelisp-bytecode-opcode-parity.el), unconditionally,
in both core-mode and the general prelude. It used to carry a core-mode-
only block, the same shape opcode 32 briefly had: allowlisting 95 for
core-mode mass-adopted ~17 unrelated core defuns that happen to multiply
(nelisp-eln-system-loader--file-u and others), and back then one of them
triggered a `(ash 1)' wrong-number-of-arguments regression in
lisp/nelisp-elf-write.el's relocation writer, breaking
test/nelisp-eln-same-artifact-smoke.sh's default lane. Re-investigated
(bisection + an isolated host-vs-standalone loop-shape parity case, see
mult-write-loop-index-matches-interpreted below): opcode 95's own VM
semantics were never at fault -- the real cause was core-mode forcing
`lexical-binding' to nil while compiling source that declares `t' (fixed
elsewhere in this file), which miscompiled several of those same ~17
defuns independently of multiplication. With that fixed, file-u and the
rest adopt and run correctly (verified directly, byte-code-function-p
true, values equal to the interpreted source for representative inputs),
and default/decrement/zerop all still pass with 95 fully allowlisted. The
block is removed; nothing here still depends on the general prelude's
per-function fixture gate to stay safe.

Opcode 183 (Bswitch) is admitted.  The VM's jump-table lookup
(scripts/nelisp-standalone-build.el's wf_bytecode_switch) was always
correct; what aborted the adopted `nelisp-eln-leaf-code-valid-p' was the
reader.  `read-from-string' -- which core byte-code installation uses --
tries the native single-form parser with a fixed 2048-slot pool, and
parser slots are consumed per node, so a form holding a ~520-element
`(unibyte-string ...)' literal declined to the Elisp fallback reader.  That
reader has no `#s(...)' syntax and read the jump table as the symbol `#s'
followed by a list, so the switch jumped through a non-table and the
process aborted.  `load' sizes its pool from the whole file, which is why a
plain `load' of the same bytes worked.  The single-form parser now retries
with a larger pool before declining (bf_read_one_from_string_native), and
tools/nelisp-bytecode-opcode-parity.el's switch-reader-* cases pin both
the reader-literal path and eq/equal jump tables.")

(defun nelisp-prelude-bytecode--decode-opcodes (code)
  "Return CODE's instruction bases, or nil when its encoding is malformed."
  (let ((pc 0) (length (length code)) (opcodes nil) (valid t))
    (while (and valid (< pc length))
      (let* ((raw (aref code pc))
             (base raw)
             (operand (if (< raw 48) (logand raw 7) 0))
             (width 0))
        (setq pc (1+ pc))
        (when (< raw 48)
          (setq base (logand raw 248)
                width (cond ((= operand 6) 1)
                            ((= operand 7) 2)
                            (t 0))))
        ;; Bpushconditioncase (49) and Bpushcatch (50) also carry an
        ;; explicit 2-byte jump-target operand -- omitting them here (this
        ;; decoder disagreed with the VM interpreter's own width table,
        ;; scripts/nelisp-standalone-build.el's operand-decode preamble,
        ;; which already has them) made a following operand byte pair get
        ;; read as if it were up to two MORE opcodes: e.g. a
        ;; `condition-case'-using function's real Bpushconditioncase jump
        ;; target of 126 was reported as opcode 126 (Bwiden) itself, a
        ;; phantom "unsupported opcode" a function never actually used.
        (when (or (and (>= raw 129) (<= raw 134))
                  (memq base '(49 50 175 178 182)))
          (setq width (if (memq base '(175 178 182)) 1 2)))
        (when (>= raw 192) (setq base 192))
        (push base opcodes)
        (setq pc (+ pc width))
        (when (> pc length) (setq valid nil))))
    (and valid (nreverse opcodes))))

(defun nelisp-prelude-bytecode--nested-unsupported (function)
  "Return unadmitted opcodes of FUNCTION's nested byte-code constants.
A lexical closure compiles to a `make-closure' call on a prototype stored
in the constants vector; the VM runs that prototype's code just as it
runs the outer function's, so every prototype must pass the same opcode
allowlist.  A malformed nested encoding is reported as :malformed.
FUNCTION's own code is not included (the caller decodes it)."
  (let ((pending (append (aref function 2) nil)) (seen nil) (bad nil))
    (while pending
      (let ((constant (pop pending)))
        (when (and (byte-code-function-p constant)
                   (not (memq constant seen)))
          (push constant seen)
          (let* ((code (aref constant 1))
                 (opcodes (and (stringp code)
                               (nelisp-prelude-bytecode--decode-opcodes code))))
            (if (null opcodes)
                (push :malformed bad)
              (dolist (opcode opcodes)
                (unless (memq opcode nelisp-prelude-bytecode--opcodes)
                  (push opcode bad))))
            (setq pending (append (aref constant 2) pending))))))
    (delete-dups (nreverse bad))))

(defun nelisp-prelude-bytecode--metadata (function)
  "Return readable byte-code metadata for FUNCTION."
  (format "args=%S;constants=%d;stack=%S;interactive=%S"
          (aref function 0)
          (length (aref function 2))
          (aref function 3)
          (and (> (length function) 5) (aref function 5))))

(defun nelisp-prelude-bytecode--skip-trivia (source position)
  "Return POSITION advanced over reader whitespace and semicolon comments."
  (let ((length (length source)) (again t))
    (while (and again (< position length))
      (let ((char (aref source position)))
        (cond
         ((memq char '(9 10 12 13 32)) (setq position (1+ position)))
         ((= char ?\;)
          (while (and (< position length) (/= (aref source position) 10))
            (setq position (1+ position))))
         (t (setq again nil)))))
    position))

(defun nelisp-prelude-bytecode--list-children (source start)
  "Return (HEAD-END . SPANS) for a proper list at START in SOURCE.
SPANS contains (START . END) pairs for each element after the list head."
  (let* ((open (nelisp-prelude-bytecode--skip-trivia source start))
         (head-result (read-from-string source (1+ open)))
         (position (cdr head-result))
         (spans nil)
         (done nil))
    (while (not done)
      (setq position (nelisp-prelude-bytecode--skip-trivia source position))
      (cond
       ((>= position (length source)) (setq done t))
       ((= (aref source position) ?\)) (setq done t))
       (t (let* ((child-start position)
                 (child-result (read-from-string source position)))
            (push (cons child-start (cdr child-result)) spans)
            (setq position (cdr child-result))))))
    (cons (cdr head-result) (nreverse spans))))

(defun nelisp-prelude-bytecode-source-defuns (source)
  "Return SOURCE defun forms, including those inside supported guards."
  (let ((position 0) (forms nil))
    (cl-labels
        ((walk (start)
           (let* ((read-result (read-from-string source start))
                  (form (car read-result)))
             (cond
              ;; `defsubst' (vendor/staged-emacs-lisp/files.el's
              ;; file-attribute-* accessors, etc.) has the exact same
              ;; NAME ARGS [DOC] BODY... shape as `defun' at this raw,
              ;; pre-macro-expansion source level; when it is not
              ;; actually inlined at a call site (the common case once
              ;; the caller is itself compiled/adopted separately) it is
              ;; an ordinary function, so it is just as adoptable.
              ((memq (car-safe form) '(defun defsubst)) (push form forms))
              ((and (consp form)
                    (memq (car form) '(progn when unless if)))
               (let* ((children (cdr (nelisp-prelude-bytecode--list-children
                                      source start)))
                      (body-children
                       (if (memq (car form) '(when unless if))
                           (cdr children)
                         children)))
                 (mapc (lambda (span) (walk (car span))) body-children)))))))
      (while (< position (length source))
        (setq position (nelisp-prelude-bytecode--skip-trivia source position))
        (when (< position (length source))
          (walk position)
          (setq position (cdr (read-from-string source position)))))
      (nreverse forms))))

(defun nelisp-prelude-bytecode-read-parity-fixtures (path)
  "Read the verified/rejected candidate matrix from PATH."
  (let (rows)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (fields (split-string line "\t")))
          (unless (or (string= line "") (string-prefix-p "#" line))
            (unless (= (length fields) 8)
              (error "Malformed prelude parity fixture row: %s" line))
            (push (list (intern (nth 0 fields)) (nth 1 fields)
                        (nth 2 fields) (nth 3 fields) (nth 4 fields)
                        (nth 5 fields) (nth 6 fields) (nth 7 fields)) rows))
          (forward-line 1)))
    ;; These two source forms were independently byte-compiled with GNU
    ;; Emacs 31.1 and their emitted bytecode is covered by the focused
    ;; standalone parity test.  Keep the digest gate here so source edits
    ;; invalidate adoption until that evidence is refreshed.
    (dolist (fixture
             '((nelisp--path-split "" "" "pass"
                "standalone-byte-opcode-parity" "bytecode"
                "fddde430251ce99f8d1e417a93331f1302553d966f9a64849da5ddf5d160013b" "-")
               (expand-file-name "" "" "pass"
                "standalone-byte-opcode-parity" "bytecode"
                "3a49025c2fe1f13b65706071c38277eb13cefbbca5c8bcf1eea4f8299b2a0afb" "32")
               (make-temp-name "" "" "pass"
                "standalone-byte-opcode-parity" "bytecode"
                "a9f396ac0db78a1201097aaca65d76bd04e89e250bc8a7d2b4246f24c66e30ef" "32")))
      (unless (assq (car fixture) rows)
        (push fixture rows)))
    (nreverse rows))))

;; Core-mode compilation context.  GNU `byte-compile-file' compiles a module
;; after evaluating its own top-level `defvar's and loading every `require'd
;; feature, so a `let' of a declared special variable becomes a dynamic
;; binding (varbind/unbind) and a macro call is expanded.  Core-mode compiles
;; each defun on its own in a host where none of these modules is loaded, so
;; without this context a `let' of e.g. `nl-ffi-loader--file-mappings' became
;; a LEXICAL stack slot: byte-code that looks valid, passes the opcode
;; allowlist, and silently hides the binding from every callee that reads the
;; variable.  The declarations below reproduce the compile-time environment
;; `byte-compile-file' would have had: the module's own top-level specials and
;; macros plus those of every module it (transitively) `require's from this
;; tree.

(defvar nelisp-prelude-bytecode-repo-root nil
  "Repository root for resolving a core module's `require'd sources.
nil means `nelisp-standalone--repo-root' when bound, else `default-directory'.")

(defconst nelisp-prelude-bytecode--source-dirs
  '("lisp" "src" "packages/nl-ffi/src" "scripts")
  "Repository directories searched for a `require'd feature's source.")

(defvar nelisp-prelude-bytecode--declarations-cache nil
  "Alist of (PATH SHA256 SPECIALS MACROS REQUIRES) read from module sources.")

(defun nelisp-prelude-bytecode--repo-root ()
  "Return the repository root used to resolve `require'd module sources."
  (file-name-as-directory
   (or nelisp-prelude-bytecode-repo-root
       (and (boundp 'nelisp-standalone--repo-root)
            (symbol-value 'nelisp-standalone--repo-root))
       default-directory)))

(defun nelisp-prelude-bytecode--source-lexical-p (source)
  "Return non-nil unless SOURCE's first line declares `lexical-binding: nil'."
  (not (string-match-p "\\`[^\n]*-\\*-[^\n]*lexical-binding: *nil" source)))

(defun nelisp-prelude-bytecode--top-level-declarations (source)
  "Return (SPECIALS MACRO-FORMS REQUIRES) declared at top level in SOURCE.
Top level includes the bodies of `progn', `eval-and-compile',
`eval-when-compile', `when', `unless' and `if', as `byte-compile-file' sees
them."
  (let ((position 0) (length (length source))
        (specials nil) (macros nil) (requires nil) (pending nil))
    (while (< position length)
      (setq position (nelisp-prelude-bytecode--skip-trivia source position))
      (if (>= position length)
          (setq position length)
        (let ((read-result (read-from-string source position)))
          (progn
            (setq position (cdr read-result)
                  pending (list (car read-result)))
            (while pending
              (let ((form (car pending)))
                (setq pending (cdr pending))
                (cond
                 ((memq (car-safe form)
                        '(progn eval-and-compile eval-when-compile
                                when unless if))
                  (setq pending (append (cl-remove-if-not #'consp (cdr form))
                                        pending)))
                 ((and (memq (car-safe form)
                             '(defvar defconst defcustom defvar-local))
                       (symbolp (nth 1 form)))
                  (push (nth 1 form) specials))
                 ((and (eq (car-safe form) 'defmacro) (symbolp (nth 1 form)))
                  (push form macros))
                 ((and (eq (car-safe form) 'require)
                       (eq (car-safe (nth 1 form)) 'quote)
                       (symbolp (cadr (nth 1 form))))
                  (push (cadr (nth 1 form)) requires)))))))))
    (list specials macros requires)))

(defun nelisp-prelude-bytecode--feature-source (feature)
  "Return the tree source file providing FEATURE, or nil."
  (let ((root (nelisp-prelude-bytecode--repo-root)) (found nil))
    (dolist (dir nelisp-prelude-bytecode--source-dirs)
      (let ((path (expand-file-name (format "%s/%s.el" dir feature) root)))
        (when (and (not found) (file-readable-p path))
          (setq found path))))
    found))

(defun nelisp-prelude-bytecode--file-declarations (path)
  "Return PATH's (SPECIALS MACRO-FORMS REQUIRES), cached by content hash."
  (let* ((source (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string)))
         (hash (secure-hash 'sha256 source))
         (cached (assoc path nelisp-prelude-bytecode--declarations-cache)))
    (if (and cached (equal (nth 1 cached) hash))
        (nthcdr 2 cached)
      (let ((declarations (nelisp-prelude-bytecode--top-level-declarations
                           source)))
        (setq nelisp-prelude-bytecode--declarations-cache
              (cons (cons path (cons hash declarations))
                    (cl-remove path nelisp-prelude-bytecode--declarations-cache
                               :key #'car :test #'equal)))
        declarations))))

(defun nelisp-prelude-bytecode--compile-context (source)
  "Return (SPECIALS MACRO-NAMES . MACRO-ENVIRONMENT) for compiling SOURCE.
SPECIALS and MACRO-NAMES are SOURCE's own top-level declarations plus those
of every module it transitively `require's from this tree.
MACRO-ENVIRONMENT holds a compile-time definition for each such macro the
host does not already define, as `byte-compile-file' would have after
evaluating the `defmacro' (or loading the required file).  Names the host
already defines keep the host's definition: the host is the GNU Emacs whose
libraries the vendored copies were taken from."
  (let* ((own (nelisp-prelude-bytecode--top-level-declarations source))
         (specials (copy-sequence (nth 0 own)))
         (macro-forms (copy-sequence (nth 1 own)))
         (queue (copy-sequence (nth 2 own)))
         (seen nil)
         (environment nil))
    (while queue
      (let ((feature (car queue)))
        (setq queue (cdr queue))
        (unless (memq feature seen)
          (push feature seen)
          (let ((path (nelisp-prelude-bytecode--feature-source feature)))
            (when path
              (let ((declarations (nelisp-prelude-bytecode--file-declarations
                                   path)))
                (setq specials (append (nth 0 declarations) specials)
                      macro-forms (append (nth 1 declarations) macro-forms)
                      queue (append queue (nth 2 declarations)))))))))
    (dolist (form macro-forms)
      (let ((name (nth 1 form)))
        ;; Evaluating a `function' form only builds the closure; the
        ;; macro body itself runs when a caller is compiled, where a
        ;; failure surfaces as that caller's compile-error rejection.
        (unless (or (fboundp name) (assq name environment))
          (push (cons name
                      (eval (list 'function (cons 'lambda (nthcdr 2 form)))
                            t))
                environment))))
    (cons (delete-dups specials)
          (cons (delete-dups (mapcar (lambda (form) (nth 1 form))
                                     macro-forms))
                environment))))

(defun nelisp-prelude-bytecode--declare-specials (body specials)
  "Return BODY with a local `(defvar SYM)' for each of SPECIALS.
The declarations follow BODY's leading docstring, `declare' and
`interactive' forms, which must stay first."
  (if (null specials)
      body
    (let ((head nil) (rest body))
      (while (and rest
                  (cdr rest)
                  (or (stringp (car rest))
                      (memq (car-safe (car rest)) '(declare interactive))))
        (push (car rest) head)
        (setq rest (cdr rest)))
      (append (nreverse head)
              (mapcar (lambda (symbol) (list 'defvar symbol)) specials)
              rest))))

(defun nelisp-prelude-bytecode-transform (source provenance &optional parity-fixtures core-mode)
  "Compile SOURCE defuns whose GNU byte-code uses admitted opcodes.
PROVENANCE is a repository-relative source filename.  Return (TEXT COUNT
REPORT), where REPORT has one row for each source defun and names its decision,
reason, source line, source-form digest, opcode sequence, and retained metadata."
  (let ((position 0)
        (source-length (length source))
        (count 0)
        (report nil)
        (replacements nil)
        (compile-lexical (and core-mode
                              (nelisp-prelude-bytecode--source-lexical-p
                               source)))
        (compile-context (and core-mode
                              (nelisp-prelude-bytecode--compile-context
                               source))))
    (cl-labels
        ((inspect (start)
           (let* ((read-result (read-from-string source start))
                  (form (car read-result))
                  (end (cdr read-result)))
           (cond
            ((and (eq (car-safe form) 'quote) (consp (cdr form))) form)
            ((and (memq (car-safe form) '(defun defsubst))
                  (symbolp (nth 1 form)))
             (let* ((name (nth 1 form))
                    (args (nth 2 form))
                    (body (cdddr form))
                    (doc (and (stringp (nth 3 form))
                              (cddddr form) (nth 3 form)))
                    (lambda-form
                     (append (list 'lambda args)
                             (if compile-lexical
                                 ;; Every visible special, not just the
                                 ;; ones BODY names: a macro expansion can
                                 ;; bind one BODY never mentions (cl-seq's
                                 ;; `cl--parsing-keywords' binds `cl-test').
                                 (nelisp-prelude-bytecode--declare-specials
                                  body (car compile-context))
                               body)))
                    (compiled
                     (condition-case err
                         (with-temp-buffer
                           ;; The general prelude (scripts/nelisp-stdlib-
                           ;; prelude.el, vendor/staged-emacs-lisp) is
                           ;; uniformly `-*- lexical-binding: nil; -*-';
                           ;; every core module (lisp/nelisp-eln-*.el,
                           ;; nelisp-native-load.el) is uniformly `t'.
                           ;; Compiling core source under a forced nil
                           ;; produces byte-code with the WRONG closure
                           ;; semantics for anything that captures an
                           ;; enclosing lexical binding (a free variable
                           ;; under dynamic scope instead of a closed-over
                           ;; value) -- it byte-compiles fine and passes
                           ;; every opcode-allowlist check, since nothing
                           ;; about that is opcode-visible, but signals
                           ;; void-variable the first time such a function
                           ;; actually runs as byte-code. Match core-mode's
                           ;; own source's declared binding instead of
                           ;; always forcing the prelude's.
                           ;; A core module that declares `nil' (the
                           ;; regexp matcher) is compiled dynamically too.
                           (setq-local lexical-binding compile-lexical)
                           (let ((byte-compile-warnings nil)
                                 (byte-compile-verbose nil)
                                 (byte-compile-initial-macro-environment
                                  (append (cddr compile-context)
                                          byte-compile-initial-macro-environment)))
                             (byte-compile lambda-form)))
                       (error (list :compile-error err))))
                    (code (and (byte-code-function-p compiled)
                               (aref compiled 1)))
                    (opcodes (and (stringp code)
                                  (nelisp-prelude-bytecode--decode-opcodes code)))
                    (unsupported (and opcodes
                                      (cl-remove-if
                                       (lambda (opcode)
                                         (memq opcode nelisp-prelude-bytecode--opcodes))
                                       opcodes)))
                    (nested-unsupported
                     (and (byte-code-function-p compiled)
                          (> (length compiled) 2)
                          (vectorp (aref compiled 2))
                          (nelisp-prelude-bytecode--nested-unsupported
                           compiled)))
                    ;; The standalone reader has no bignum type: its
                    ;; fixnums cap at the SAME 61-bit boundary host Emacs
                    ;; uses (most-positive-fixnum/most-negative-fixnum are
                    ;; identical constants on both), but host's byte
                    ;; optimizer can constant-fold a literal expression
                    ;; like `(ash 1 61)' at compile time into an actual
                    ;; bignum embedded straight in the constants vector.
                    ;; That prints and reads back on the standalone as a
                    ;; silently wrapped/negated fixnum (a known standing
                    ;; limitation, not a bug this lane can fix), so a
                    ;; range check the source expected to be exact becomes
                    ;; exact for the wrong value. Detect any out-of-fixnum-
                    ;; range integer constant and refuse to adopt.
                    (has-bignum-constant
                     (and (byte-code-function-p compiled)
                          (> (length compiled) 2)
                          (let ((constants (aref compiled 2)))
                            (and (vectorp constants)
                                 (cl-some (lambda (c)
                                            (and (integerp c)
                                                 (or (> c most-positive-fixnum)
                                                     (< c most-negative-fixnum))))
                                          constants)))))
                    ;; A tree macro the host compiler never saw (see the
                    ;; compile context above) was compiled as a plain
                    ;; function call to the macro's name; GNU
                    ;; `byte-compile-file' would have expanded it.
                    (unexpanded-macros
                     (and compile-context
                          (byte-code-function-p compiled)
                          (> (length compiled) 2)
                          (vectorp (aref compiled 2))
                          (cl-remove-if-not
                           (lambda (c)
                             (and (symbolp c)
                                  (memq c (cadr compile-context))
                                  (not (fboundp c))
                                  (not (assq c (cddr compile-context)))))
                           (append (aref compiled 2) nil))))
                    (replacement form)
                    (status "reject")
                    (reason "no-byte-code-or-malformed")
                    (metadata "")
                    (source-digest (secure-hash 'sha256
                                                (substring source start end)))
                    (fixture (assq name parity-fixtures)))
               (cond
                ((and (consp compiled) (eq (car compiled) :compile-error))
                 (setq reason (format "compile-error:%s"
                                      (error-message-string (cadr compiled)))))
                ((not opcodes))
                (unsupported
                 (setq reason (format "unsupported-opcodes:%S"
                                      (delete-dups unsupported))))
                ;; Found 2026-09-28: `nelisp-eln-native-subr-multi-import-
                ;; analysis' was adopted although a nested closure used
                ;; opcode 156 (Belt), and the standalone VM aborted
                ;; ("form aborted without signal") the first time it ran.
                (nested-unsupported
                 (setq reason (format "unsupported-nested-opcodes:%S"
                                      nested-unsupported)))
                (has-bignum-constant
                 (setq reason "constant-folded-bignum-outside-fixnum-range"))
                (unexpanded-macros
                 (setq reason (format "unexpanded-tree-macros:%S"
                                      unexpanded-macros)))
                ((and (null fixture) (not core-mode))
                 (setq reason "no-parity-fixture"))
                ;; REVERT NOTE (this session): a broader relaxation was
                ;; tried here -- dropping "no-parity-fixture" entirely for
                ;; the general prelude, matching core-mode's discipline of
                ;; "allowlisted opcodes are the only gate" -- and it mass-
                ;; adopted ~500 more prelude functions in one rebuild. That
                ;; surfaced a REAL, deeper regression the opcode-level
                ;; parity suite cannot see: a function whose source calls
                ;; `setf' on a cl-defstruct slot (e.g. `write-region' via
                ;; (setf (nelisp-buffer-modified b) t)) compiles, under host
                ;; cl-lib, to EITHER a call to a function literally named
                ;; (setf ACCESSOR) (if the struct's home file was not
                ;; `require'd into the same host Emacs process before
                ;; compiling -- the standalone runtime never defines that
                ;; callable) OR, if it is required first so cl-lib inlines
                ;; the slot-set, a reference to a `cl-struct-TYPE-tags'
                ;; variable that plain host cl-defstruct generates for its
                ;; runtime type-check -- but NeLisp's OWN cl-defstruct
                ;; (lisp/nelisp-cl-macros.el) represents structs as plain
                ;; records via `nelisp--record-ref' and never defines any
                ;; such tags variable, so that reference is void too. Either
                ;; way, "opcode allowlisted" is true (varref/call/aset are
                ;; all ordinary allowlisted opcodes) while the function is
                ;; still broken at run time -- broke the default
                ;; nelisp-eln-same-artifact-smoke.sh lane via `write-region'
                ;; inside nelisp-eln-emitter-write-ir. Fixing this for real
                ;; needs NeLisp's struct runtime to provide an equivalent
                ;; (out of this lane's scope: lisp/nelisp-cl-macros.el, not
                ;; scripts/nelisp-standalone-build.el or this file), not an
                ;; opcode-allowlist change, so the fixture requirement stays
                ;; for the general prelude: a passing fixture is still each
                ;; function's own end-to-end proof that its specific
                ;; compiled form actually runs, which the opcode-only gate
                ;; cannot provide. core-mode is unaffected by this note --
                ;; its 229/354 adoption predates and does not depend on the
                ;; reverted change, and the default lane passes with it.
                ;;
                ;; Bcall (opcode 32, raw 32-39) previously had its own extra
                ;; gate on top of this ("opcode-32-not-explicitly-verified")
                ;; because a call's safety also depends on what it calls.
                ;; It now has verified VM semantics for arity 0-7, the
                ;; explicit 1-/2-byte operand widths, callee errors, and
                ;; callee-cell variety (builtin/lambda/byte-code/void/
                ;; autoload) -- see the call-* cases in
                ;; tools/nelisp-bytecode-opcode-parity.el -- so it is just
                ;; another allowlisted opcode now, in both modes. One
                ;; confirmed gap remains tracked, not fixed, by that parity
                ;; suite: a THROW from a Bcall-invoked callee that needs to
                ;; escape the top-level `byte-code' call itself (no matching
                ;; pushcatch/pophandler within the same byte-code region)
                ;; loses its tag/value pair in the native "byte-code" entry
                ;; point's unhandled-exit path -- see
                ;; call-throw-escapes-bytecode-known-gap in the parity file.
                ;; That is a property of the native "byte-code" entry point
                ;; used by every already-baked function today, not
                ;; something adopting more of them via this opcode
                ;; introduces or worsens.
                ((and fixture (not core-mode)
                      (not (equal (nth 6 fixture) source-digest)))
                 (setq reason "source-form-digest-mismatch"))
                ((and fixture (not core-mode)
                      (not (equal (nth 3 fixture) "pass")))
                 (setq reason (format "parity-rejected:%s" (nth 4 fixture))))
                ((and fixture (not core-mode)
                      (not (equal (nth 5 fixture) "bytecode")))
                 (setq reason "verified-parity-cell-is-not-byte-code"))
                (t
                 (setq status "adopt"
                       reason (cond
                               (core-mode
                                "core-module-source-hash-and-opcode-allowlist")
                               (fixture
                                "opcode-allowlisted-and-host-parity-fixture-passed")
                               (t "opcode-allowlisted"))
                       count (1+ count)
                       metadata (nelisp-prelude-bytecode--metadata compiled)
                       replacement
                       (list 'prog1 (list 'quote name)
                             (list 'fset (list 'quote name)
                                   (append
                                    (list 'make-byte-code
                                          ;; `(aref compiled 0)', not the
                                          ;; source ARGS list: under
                                          ;; lexical-binding t (every core
                                          ;; module) that arg-spec is an
                                          ;; integer bitfield, not a plain
                                          ;; symbol list, and it must match
                                          ;; the calling convention CODE
                                          ;; below actually implements.
                                          ;; Under dynamic binding (the
                                          ;; general prelude) byte-compile's
                                          ;; own arg-spec already equals the
                                          ;; source list, so this is
                                          ;; equivalent there and simply
                                          ;; more direct.
                                          (list 'quote (aref compiled 0))
                                          (cons 'unibyte-string
                                                (string-to-list code))
                                          (list 'quote (aref compiled 2))
                                          (aref compiled 3)
                                          doc)
                                    (when (> (length compiled) 5)
                                      (list (aref compiled 5)))))))))
               (push (list provenance
                           (1+ (cl-count ?\n source :end start))
                           name status reason source-digest
                           (format "%S" opcodes) metadata
                           (if fixture (nth 3 fixture)
                             (if core-mode "core-opcode-review" "missing"))
                           (if fixture (nth 2 fixture) "")
                           (if fixture (nth 5 fixture) ""))
                     report)
               (when (equal status "adopt")
                 (push (list start end replacement) replacements))
               replacement))
            ((and (consp form)
                  (memq (car form) '(progn when unless if)))
             (let* ((children (cdr (nelisp-prelude-bytecode--list-children
                                    source start)))
                    (body-children
                     (if (memq (car form) '(when unless if))
                         (cdr children)
                       children)))
               (mapc (lambda (span) (inspect (car span))) body-children)
               form))
            (t form)))))
      (while (< position source-length)
        (setq position (nelisp-prelude-bytecode--skip-trivia source position))
        (if (>= position source-length)
            (setq position source-length)
          (inspect position)
          (setq position (cdr (read-from-string source position)))))
    (let ((output source)
          ;; A REPLACEMENT's constants vector (`(aref compiled 2)') can
          ;; hold a nested byte-code-function object of its own, for a
          ;; closure literal in the source (see the `nested-unsupported'
          ;; opcode check above, which walks the very same vector).  That
          ;; nested object's OWN code string is an arbitrary byte string,
          ;; not the outer function's -- it never goes through the
          ;; `(cons 'unibyte-string (string-to-list code))' rendering
          ;; this file uses for the outer CODE, so `prin1-to-string' here
          ;; prints it with Emacs's own `#[...]' byte-code reader syntax,
          ;; octal-escaping each byte of that string.  `prin1' only does
          ;; that escaping for control characters (NUL among them) when
          ;; `print-escape-control-characters' is non-nil; it is nil by
          ;; default, so a control byte in a nested closure's code string
          ;; -- not a rare byte, since op codes below 32 are common --
          ;; was written into this generator's output as a literal
          ;; control byte, NUL included.  Text after a raw NUL is exactly
          ;; the kind of thing an ordinary C-string-oriented reader can
          ;; mis-scan, and the byte itself is never valid inside a Lisp
          ;; string constant; either way, the entry for whatever module
          ;; came after it read back with the wrong content.  Binding
          ;; this repairs it at the only place a nested byte-code object
          ;; ever gets printed this way.
          (print-escape-control-characters t))
      (dolist (patch (sort replacements (lambda (a b) (> (car a) (car b)))))
        (setq output (concat (substring output 0 (nth 0 patch))
                             (prin1-to-string (nth 2 patch))
                             (substring output (nth 1 patch)))))
    (list output
          count
          (nreverse report))))))

(defun nelisp-prelude-bytecode-write-report (path reports)
  "Write REPORTS to PATH as a TSV adoption/rejection manifest.

PATH is a fixed, repo-relative diagnostic byproduct (not a file anyone
edits interactively), and every concurrent caller regenerates the exact
same content from the same source tree, so a `create-lockfiles' advisory
lock buys nothing here and only introduces a race: `write-region' takes
that lock unconditionally around any write to a real filename, and when
two lanes are writing PATH at once (e.g. two concurrent runs of the same
smoke test against a shared checkout, or ordinary system load spreading
out their start times enough to overlap), the second one's lock attempt
finds the first one's lock still held and calls `ask-user-about-lock',
which -- since this always runs `--batch' -- has no terminal to prompt
and signals `file-locked' with \"Cannot resolve lock conflict in batch
mode\", aborting the whole caller with a plain exit 1.  Binding
`create-lockfiles' to nil for this write removes that lock attempt
entirely without changing what gets written."
  (let ((create-lockfiles nil))
    (with-temp-file path
      (insert (format "# compiler\t%s\n" emacs-version))
      (insert "# selection\tGNU Emacs 31.1 prelude defuns with VM-allowlisted opcodes\n")
      (insert (format "# candidates\t%d\n" (length reports)))
      (insert (format "# adopted\t%d\n"
                      (cl-count "adopt" reports :key (lambda (row) (nth 3 row))
                                :test #'equal)))
      (insert (format "# rejected\t%d\n"
                      (cl-count "reject" reports :key (lambda (row) (nth 3 row))
                                :test #'equal)))
      (insert "source\tline\tname\tdecision\treason\tform-sha256\topcodes\tmetadata\tparity-status\thost-result\texpected-cell-kind\n")
      (dolist (row reports)
        (insert (mapconcat (lambda (field)
                             (replace-regexp-in-string
                              "[\t\n]" " " (format "%s" field)))
                           row "\t")
                "\n")))))

(provide 'nelisp-prelude-bytecode)
;;; nelisp-prelude-bytecode.el ends here
