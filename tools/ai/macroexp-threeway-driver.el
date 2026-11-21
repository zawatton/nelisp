;;; macroexp-threeway-driver.el --- 3-way macro-expansion measurement driver -*- lexical-binding: t; -*-

;; Measurement scaffolding only (no runtime change).  Compare macroexpand-all
;; under: GNU native, GNU forced-bytecode, NeLisp forced-bytecode.  GNU
;; compiles THIS file and macroexp.elc; all engines run the same bytecode.
;;
;; Environment:
;;   MEXP_MODE      native | bytecode
;;   MEXP_PROVIDER  full path of macroexp.elc (bytecode mode; loaded nosuffix)
;;   MEXP_CASE      plain | macros | state | conditional
;;   BENCH_N        positive integer (iterations / passes)
;;   MEXP_WIDTH     optional; default plain=1024, macros=256
;;   NDTW_ROOT      game repository root (state / conditional)
;;   MEXP_BAD_KIND=1     require the OPPOSITE execution kind (must exit 1)
;;   BENCH_BAD_RESULT=1  perturb the checked result (runner must reject)
;;   MEXP_META_DIR  existing directory for per-process metadata logs
;;   MEXP_SAMPLE_ID supervisor-provided identity (never trust emacs-pid here)
;;   MEXP_TRACE_DIR optional last-phase checkpoints; diagnostic runs only
;;   MEXP_EXTRA_PROVIDER  optional full path of an extra .elc (compiled
;;                  when/push/unless/dolist/dotimes/pop macros), loaded AFTER
;;                  the conditional prefix and before the bench helper
;;   MEXP_EXTRA_EXPECT_BYTECODE=1  require those six expanders to be byte-code
;;                  (without MEXP_EXTRA_PROVIDER: negative control, must fail
;;                  on source expanders).  Both unset: hook is a no-op.
;;
;; stdout: exactly one line  BENCH-CHECK n=N result=TOKEN elapsed=SECONDS
;; Metadata files: kinds and per-pass/per-form observations.  The standalone
;; runtime has no external-debugging-output stream, so do not use stderr APIs.
;; elapsed = SUM of macroexpand-all-only seconds.  Per-pass "total" in the
;; corpus cases is an instrumented read/expand/eval/check loop, NOT exact
;; source-load latency; first-pass vs repeated-pass numbers are not cold-load
;; timing.  Digests are hashes of printed expansions, not proof of semantics.
;; Fixture choice: gensym-counter is reset to 0 at the start of each pass
;; (after any prefix load).  Truncated trailing source would end reading
;; silently (only end-of-file around `read' ends a pass).

(defvar mexp--counter 0 "Macro invocation counter for synthetic fixtures.")
(defvar mexp--eof (make-symbol "mexp-eof") "Unique end-of-source marker.")
(defvar mexp--metadata nil "Reverse-ordered metadata lines.")

(defmacro mexp-test-macro (i)
  "Count the call, then expand to an if/let using lexical variable `mexp-x'."
  (setq mexp--counter (1+ mexp--counter))
  (list 'if '(> mexp-x 0)
        (list 'let (list (list 'mexp-y i)) '(+ mexp-x mexp-y))
        0))

(defun mexp--log (fmt &rest args)
  "Retain a formatted metadata line outside the timed expansion."
  (push (apply #'format fmt args) mexp--metadata))

(defun mexp--trace (phase index)
  "Retain only the last diagnostic PHASE and form INDEX, outside timing."
  (let ((dir (getenv "MEXP_TRACE_DIR")))
    (when dir
      (write-region (format "phase=%s index=%d\n" phase index) nil
                    (expand-file-name
                     (concat (or (getenv "MEXP_SAMPLE_ID") "setup") ".log") dir)
                    nil 'silent))))

(defun mexp--flush ()
  "Write observations using the supervisor's sample identity."
  (let* ((dir (getenv "MEXP_META_DIR"))
         (name (format "%s-%s-%s.log" (getenv "MEXP_MODE")
                       (getenv "MEXP_CASE")
                       (or (getenv "MEXP_SAMPLE_ID") "setup"))))
    (unless (and dir (file-directory-p dir))
      (error "MEXP: MEXP_META_DIR must be an existing directory"))
    (write-region (mapconcat #'identity (reverse mexp--metadata) "")
                  nil (expand-file-name name dir) nil 'silent)))

(defun mexp--flag (name)
  "Non-nil when env NAME is set to something other than empty or 0."
  (let ((v (getenv name)))
    (and v (not (string= v "")) (not (string= v "0")))))

(defun mexp--pos-int (name default)
  "Positive integer from env NAME; DEFAULT if unset; error if malformed."
  (let ((v (getenv name)))
    (cond ((or (null v) (string= v ""))
           (or default (error "MEXP: %s is required" name)))
          ((string-match "\\`[1-9][0-9]*\\'" v) (string-to-number v))
          (t (error "MEXP: %s must be a positive integer, got %S" name v)))))

(defun mexp--kind (sym)
  "Execution kind of the function bound to SYM."
  (if (not (fboundp sym))
      "unbound"
    (let ((f (indirect-function sym)))
      (cond ((consp f) "cons")
            ((and (fboundp 'native-comp-function-p)
                  (funcall 'native-comp-function-p f))
             "native")
            ((byte-code-function-p f) "bytecode")
            ((and (fboundp 'subrp) (subrp f)) "subr")
            (t "other")))))

(defun mexp--check-kinds (expected)
  "Log kinds and signal unless the five core functions are all EXPECTED."
  (let ((bad nil))
    (dolist (s '(internal-macroexpand-for-load macroexpand-all
                 macroexp--expand-all macroexp--all-forms
                 macroexp-macroexpand))
      (let ((k (mexp--kind s)))
        (mexp--log "MEXP-KIND name=%s kind=%s expected=%s\n" s k expected)
        (unless (string= k expected) (push s bad))))
    (dolist (s '(macroexpand macroexpand-1 macrop function-get))
      (mexp--log "MEXP-KIND name=%s kind=%s\n" s (mexp--kind s)))
    (when bad
      (error "MEXP: kind mismatch (expected %s): %S" expected (nreverse bad)))))

(defun mexp--extra-provider (expected)
  "Optionally load MEXP_EXTRA_PROVIDER and check the six core macro expanders.
No-op unless MEXP_EXTRA_PROVIDER is non-empty or MEXP_EXTRA_EXPECT_BYTECODE is
set.  With EXTRA_PROVIDER, load it loudly.  Then require each of `when' `push'
`unless' `dolist' `dotimes' `pop' to be a macro cell whose expander is
byte-code, and re-check the five core kinds against EXPECTED.  Setting only
MEXP_EXTRA_EXPECT_BYTECODE (no provider) is a negative control: source
expanders must make this fail."
  (let* ((path (getenv "MEXP_EXTRA_PROVIDER"))
         (have-path (and path (not (string= path ""))))
         (expect (mexp--flag "MEXP_EXTRA_EXPECT_BYTECODE")))
    (when (or have-path expect)
      (when have-path
        (unless (file-readable-p path)
          (error "MEXP: MEXP_EXTRA_PROVIDER unreadable: %S" path))
        (load path nil t t))
      (let ((bad nil))
        (dolist (s '(when push unless dolist dotimes pop))
          (let* ((cell (and (fboundp s) (symbol-function s)))
                 (macro (eq (car-safe cell) 'macro))
                 (bc (and macro (byte-code-function-p (cdr cell)))))
            (mexp--log "MEXP-EXTRA name=%s macro=%s expander-bytecode=%s provider=%s\n"
                       s (and macro t) (and bc t) (if have-path path "none"))
            (unless bc (push s bad))))
        (when bad
          (error "MEXP: extra provider expanders not bytecode macros: %S"
                 (nreverse bad))))
      (mexp--check-kinds expected))))

;;; Synthetic fixtures (plain / macros)

(defun mexp--make-synth (kind width)
  "Build a fresh input tree of WIDTH forms for KIND (plain or macros)."
  (let ((forms nil) (i 0))
    (while (< i width)
      (push (if (eq kind 'plain) (list '+ 1 2) (list 'mexp-test-macro i))
            forms)
      (setq i (1+ i)))
    (setq forms (nreverse forms))
    (if (eq kind 'plain)
        (cons 'progn forms)
      (list 'let '((mexp-x 5)) (cons '+ forms)))))

(defun mexp-bench-synth (kind width n)
  "Run N timed macroexpand-all iterations of synthetic KIND.
Returns (ELAPSED RESULT-STRING).  Input construction, counter reset,
evaluation and checks are all outside the timer."
  (let* ((expected-result (if (eq kind 'plain)
                              3
                            (+ (* 5 width) (/ (* width (1- width)) 2))))
         (expected-count (if (eq kind 'plain) 0 width))
         (warm 5) (i 0) (elapsed 0.0) (first-dt nil) (rsum 0) (csum 0))
    (while (< i (+ warm n))
      (let ((tree (mexp--make-synth kind width))
            (exp nil) (t0 nil) (dt nil) (count nil) (result nil))
        (setq mexp--counter 0)
        (setq t0 (float-time))
        (setq exp (macroexpand-all tree))
        (setq dt (- (float-time) t0))
        (setq count mexp--counter)
        (unless (= count expected-count)
          (error "MEXP: macro count %S, expected %S" count expected-count))
        (setq result (eval exp t))
        (unless (equal result expected-result)
          (error "MEXP: eval result %S, expected %S" result expected-result))
        (when (>= i warm)
          (unless first-dt (setq first-dt dt))
          (setq elapsed (+ elapsed dt)
                rsum (+ rsum result)
                csum (+ csum count))))
      (setq i (1+ i)))
    (mexp--log "MEXP-SYNTH case=%s width=%d n=%d warm=%d first=%.6f sum=%.6f\n"
               kind width n warm first-dt elapsed)
    (list elapsed
          (format "case=%s n=%d width=%d eval-sum=%d counter-sum=%d"
                  kind n width rsum csum))))

;;; Real corpora (state / conditional)

(defun mexp--lexical-cookie (head)
  "Non-nil when HEAD carries a lexical-binding cookie other than nil."
  (and (string-match "lexical-binding:[ \t]*\\([^ \t\n;*-]+\\)" head)
       (not (string= (match-string 1 head) "nil"))))

(defun mexp-bench-corpus (file n)
  "Run N passes over FILE: incremental read -> expand (timed) -> hash -> eval.
Returns (ELAPSED RESULT-STRING).  Any error, including end-of-file raised
during expansion or evaluation, propagates."
  (let ((dir (file-name-directory file))
        (src (generate-new-buffer " *mexp-src*"))
        (lexical nil) (pass 0) (elapsed 0.0)
        (first-digest nil) (first-forms nil))
    (unwind-protect
        (while (< pass n)
          (let ((load-file-name file) (load-true-file-name file)
                (default-directory dir)
                (print-length nil) (print-level nil)
                (coding-system-for-read 'utf-8)
                (pass-t0 (float-time))
                (pexp 0.0) (pread 0.0) (peval 0.0)
                (digests nil) (index 0) (done nil))
            (with-current-buffer src
              (erase-buffer)
              (insert-file-contents file)
              (setq lexical
                    (mexp--lexical-cookie
                     (buffer-substring-no-properties
                      (point-min) (min (point-max) 401))))
              (goto-char (point-min)))
            (setq gensym-counter 0)
            (let ((lexical-binding lexical))
              (while (not done)
                (mexp--trace "before-read" index)
                (let* ((r0 (float-time))
                       (form (condition-case nil
                                 (read src)
                               (end-of-file mexp--eof))))
                  (setq pread (+ pread (- (float-time) r0)))
                  (mexp--trace "after-read" index)
                  (if (eq form mexp--eof)
                      (setq done t)
                    (mexp--trace "before-expand" index)
                    (let* ((x0 (float-time))
                           (exp (macroexpand-all form))
                           (dt (- (float-time) x0))
                           (_trace (mexp--trace "before-hash" index))
                           (digest (secure-hash 'sha256 (prin1-to-string exp)))
                           (e0 nil))
                      (setq pexp (+ pexp dt))
                      (push digest digests)
                      (if (mexp--flag "MEXP_FORM_TIMES")
                          (mexp--log "MEXP-FORM pass=%d index=%d digest=%s expand=%.6f name=%s\n"
                                     pass index digest dt
                                     (and (consp form)
                                          (memq (car form) '(defun defmacro defsubst))
                                          (car (cdr form))))
                        (mexp--log "MEXP-FORM pass=%d index=%d digest=%s\n"
                                   pass index digest))
                      (setq e0 (float-time))
                      (mexp--trace "before-eval" index)
                      (eval exp lexical)
                      (mexp--trace "after-eval" index)
                      (setq peval (+ peval (- (float-time) e0)))
                      (setq index (1+ index)))))))
            (let ((pass-digest (secure-hash
                                'sha256
                                (mapconcat #'identity (nreverse digests) ""))))
              (mexp--log (concat "MEXP-PASS pass=%d forms=%d expand=%.6f "
                                 "read=%.6f eval=%.6f total=%.6f digest=%s\n")
                         pass index pexp pread peval
                         (- (float-time) pass-t0) pass-digest)
              (if (= pass 0)
                  (setq first-digest pass-digest first-forms index)
                (unless (and (= index first-forms)
                             (string= pass-digest first-digest))
                  (error "MEXP: pass %d forms/digest differ from pass 0 (%d/%s vs %d/%s)"
                         pass index pass-digest first-forms first-digest)))
              (setq elapsed (+ elapsed pexp))
              (setq pass (1+ pass)))))
      (kill-buffer src))
    (list elapsed
          (format "file=%s n=%d forms=%d digest=%s"
                  (file-name-nondirectory file) n first-forms first-digest))))

;;; Entry

(defun mexp--run ()
  "Validate environment, check kinds, run the case, print the one line."
  (let* ((mode (getenv "MEXP_MODE"))
         (kind (getenv "MEXP_CASE"))
         (n (mexp--pos-int "BENCH_N" nil))
         (bad-kind (mexp--flag "MEXP_BAD_KIND"))
         (bad-result (mexp--flag "BENCH_BAD_RESULT"))
         (expected nil)
         (out nil))
    (unless (member mode '("native" "bytecode"))
      (error "MEXP: MEXP_MODE must be native or bytecode, got %S" mode))
    (unless (member kind '("plain" "macros" "state" "conditional"))
      (error "MEXP: bad MEXP_CASE %S" kind))
    (when (string= mode "bytecode")
      (let ((provider (getenv "MEXP_PROVIDER")))
        (when (boundp 'native-comp-jit-compilation)
          (set 'native-comp-jit-compilation nil))
        (unless (and provider (file-readable-p provider))
          (error "MEXP: MEXP_PROVIDER unreadable: %S" provider))
        (load provider nil t t)))
    (mexp--log "MEXP-ENV mode=%s case=%s n=%d bad-kind=%s bad-result=%s\n"
               mode kind n bad-kind bad-result)
    (setq expected (if (eq (string= mode "native") (not bad-kind))
                       "native"
                     "bytecode"))
    (mexp--check-kinds expected)
    (if (member kind '("plain" "macros"))
        (setq out (progn
                   (mexp--extra-provider expected)
                   (mexp-bench-synth
                   (intern kind)
                   (mexp--pos-int "MEXP_WIDTH"
                                  (if (string= kind "plain") 1024 256))
                   n)))
      (let* ((root (getenv "NDTW_ROOT"))
             (sub (and root (expand-file-name "nelisp_runtime" root)))
             (file (and sub (expand-file-name
                             (concat "gamedata-" kind ".el") sub)))
             (runner (and sub (expand-file-name "game-runner.el" sub))))
        (unless (and file (file-readable-p file))
          (error "MEXP: corpus unreadable (NDTW_ROOT=%S): %S" root file))
        (when (string= kind "conditional")
          (unless (file-readable-p runner)
            (error "MEXP: prefix unreadable: %S" runner))
          (mexp--trace "before-prefix" -1)
          (load runner nil t t)
          (mexp--trace "after-prefix" -1))
        (mexp--extra-provider expected)
        (setq out (mexp-bench-corpus file n))))
    (let* ((result (car (cdr out)))
           (token (secure-hash 'sha256
                               (if bad-result (concat result "|BAD") result))))
      (mexp--log "MEXP-DONE case=%s mode=%s n=%d result=%s\n"
                 kind mode n result)
      (mexp--trace "before-flush" -1)
      (mexp--flush)
      (mexp--trace "after-flush" -1)
      (princ (format "BENCH-CHECK n=%d result=%s elapsed=%.6f\n"
                     n token (car out))))))

(defun mexp-main ()
  "Run the driver; any error prints a bounded message and exits 1."
  (condition-case err
      (progn (mexp--run) (kill-emacs 0))
    (error
     (let ((msg (prin1-to-string err)))
       (mexp--log "MEXP-ERROR %s\n"
                  (if (> (length msg) 600) (substring msg 0 600) msg)))
     (mexp--flush)
     (kill-emacs 1))))

(unless (getenv "MEXP_LIBRARY_ONLY") (mexp-main))

;;; macroexp-threeway-driver.el ends here
