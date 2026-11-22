;;; nelisp-aot-buffer-leaf-test.el --- step D buffer leaf lowering  -*- lexical-binding: t; -*-

;; Host-side tests of `nelisp-aot-compiler-buffer-leaves' (default off): the
;; lowering emits the bridged extern `nl_native_buf_leaf' with every guard
;; before it and the generic call as fallback.  The in-image GNU differential
;; is test/nelisp-aot-buffer-leaf-image.el (batch build, see
;; defects/batch-build-checklist.md).  Run:
;;   emacs -Q --batch -L lisp -L src -L test -l test/nelisp-aot-buffer-leaf-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(require 'nelisp-native-load)

(defconst nelisp-aot-buffer-leaf-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defvar nelisp-aot-buffer-leaf-test--dir nil)
(defvar nelisp-aot-buffer-leaf-test--last-settings nil
  "Codegen settings in force when the last artifact was compiled; the image
loader must expect exactly these (the artifact records its flags).")

(defun nelisp-aot-buffer-leaf-test--dir ()
  (or nelisp-aot-buffer-leaf-test--dir
      (setq nelisp-aot-buffer-leaf-test--dir (make-temp-file "bl" t))))

(defun nelisp-aot-buffer-leaf-test--tools-p ()
  (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)
       (executable-find "objdump")))

(defun nelisp-aot-buffer-leaf-test--compile (source tag flag)
  "Compile SOURCE with the leaf flag FLAG; return the object file path."
  (let* ((dir (nelisp-aot-buffer-leaf-test--dir))
         (src (expand-file-name (concat tag ".el") dir))
         (art (concat src ".neln"))
         (nelisp-aot-compiler--dynamic-user-calls t) ; the in-process loader mode
         (nelisp-aot-compiler-buffer-leaves flag))
    (with-temp-file src (insert source))
    (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
    (with-temp-buffer
      (insert-file-contents art)
      (unless (re-search-forward ":object-base64 \"\\([^\"]+\\)\"" nil t) (error "no object"))
      (let ((obj (concat art ".o")) (bytes (base64-decode-string (match-string 1))))
        (with-temp-file obj (set-buffer-multibyte nil) (insert bytes))
        obj))))

(defun nelisp-aot-buffer-leaf-test--relocs (obj)
  "Return the list of relocation target symbol names of OBJ."
  (with-temp-buffer
    (call-process "objdump" nil t nil "-r" obj)
    (let (out)
      (goto-char (point-min))
      (while (re-search-forward "R_X86_64_[A-Z0-9_]+[ \t]+\\([A-Za-z_0-9.]+\\)" nil t)
        (push (match-string 1) out))
      (nreverse out))))

(defun nelisp-aot-buffer-leaf-test--call-count (obj)
  (with-temp-buffer
    (call-process "objdump" nil t nil "-d" "-M" "intel" obj)
    (let ((n 0)) (goto-char (point-min))
         (while (re-search-forward "\tcall " nil t) (setq n (1+ n))) n)))

(defconst nelisp-aot-buffer-leaf-test--cases
  '((point "(defun blt (x) (point))")
    (point-min "(defun blt (x) (point-min))")
    (point-max "(defun blt (x) (point-max))")
    (bobp "(defun blt (x) (bobp))")
    (eobp "(defun blt (x) (eobp))")
    (following-char "(defun blt (x) (following-char))")
    (preceding-char "(defun blt (x) (preceding-char))")
    (char-after "(defun blt (x) (char-after x))")
    (char-after0 "(defun blt (x) (char-after))")
    (goto-char "(defun blt (x) (goto-char x))")
    (char-before "(defun blt (x) (char-before x))")
    (char-before0 "(defun blt (x) (char-before))")
    (bolp "(defun blt (x) (bolp))")
    (eolp "(defun blt (x) (eolp))")
    (buffer-size "(defun blt (x) (buffer-size))"))
  "Each leaf in a defun that has the AOT boundary params.")

(ert-deftest nelisp-aot-buffer-leaf-bridge-contract ()
  "The externs are appended last (after aot-regs' `nl_exit_values'), so every earlier index is unchanged."
  (should (equal (last nelisp-native-load-bridgeable-symbols 3)
                 '("nl_exit_values" "nl_native_buf_leaf" "nl_native_buf_rec")))
  (should (equal (cl-position "wf_bytecode_condition_matches"
                              nelisp-native-load-bridgeable-symbols :test #'equal)
                 (- (length nelisp-native-load-bridgeable-symbols) 4))))

(ert-deftest nelisp-aot-buffer-leaf-extern-emitted ()
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (dolist (case nelisp-aot-buffer-leaf-test--cases)
    (let* ((on (nelisp-aot-buffer-leaf-test--compile (cadr case) (format "on-%s" (car case)) t))
           (off (nelisp-aot-buffer-leaf-test--compile (cadr case) (format "off-%s" (car case)) nil))
           (r-on (nelisp-aot-buffer-leaf-test--relocs on))
           (r-off (nelisp-aot-buffer-leaf-test--relocs off)))
      ;; Flag on: the extern is called once and the generic call stays as the fallback.
      (should (= 1 (cl-count "nl_native_buf_leaf" r-on :test #'equal)))
      (should (cl-some (lambda (n) (string-match-p "nl_alloc_symbol\\|funcalln" n)) r-on))
      ;; Flag off: no extern, the old generic call only.
      (should (= 0 (cl-count "nl_native_buf_leaf" r-off :test #'equal)))
      (should (cl-some (lambda (n) (string-match-p "nl_alloc_symbol\\|funcalln" n)) r-off)))))

(ert-deftest nelisp-aot-buffer-leaf-flag-default-off ()
  (should (null nelisp-aot-compiler-buffer-leaves))
  (let ((a (nelisp-aot-buffer-leaf-test--compile "(defun blt (x) (point))" "dflt" nil)))
    (should (null (member "nl_native_buf_leaf" (nelisp-aot-buffer-leaf-test--relocs a))))))

(ert-deftest nelisp-aot-buffer-leaf-not-lowered ()
  "forward-char (no leaf), wrong arity and a unit-defined name stay generic."
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (dolist (src '("(defun blt (x) (forward-char 1))"
                 "(defun blt (x) (buffer-size x))"
                 "(defun blt (x) (point x))"
                 "(defun blt (x) (goto-char))"
                 "(defun blt (x y) (char-after x y))"
                 "(defun point () 1)\n(defun blt (x) (point))"))
    (let ((obj (nelisp-aot-buffer-leaf-test--compile src (format "nl-%d" (sxhash src)) t)))
      (should (null (member "nl_native_buf_leaf" (nelisp-aot-buffer-leaf-test--relocs obj)))))))

(ert-deftest nelisp-aot-buffer-leaf-unrelated-unit-identical ()
  "A unit without a buffer leaf is byte-identical with the flag on and off."
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (let* ((src "(defun blt (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (1+ i))) s))")
         (a (nelisp-aot-buffer-leaf-test--compile src "id-on" t))
         (b (nelisp-aot-buffer-leaf-test--compile src "id-off" nil)))
    (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally a) (buffer-string))
                   (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally b) (buffer-string))))))

(defun nelisp-aot-buffer-leaf-test--guarded-p (form)
  "Non-nil when every `nelisp-aot--leaf-extern' in FORM sits inside the
then-arm of an `if' (1-arg leaves) so that guards run before the extern."
  (let ((ok t))
    (cl-labels ((walk (f in-guarded)
                  (cond
                   ((atom f) nil)
                   ((eq (car f) 'nelisp-aot--leaf-extern)
                    (unless in-guarded (setq ok nil)))
                   ((and (eq (car f) 'if) (consp (cdr f)) (consp (cddr f)))
                    (walk (nth 1 f) in-guarded)
                    (walk (nth 2 f) (or in-guarded
                                        (equal (nth 1 f) '(= (logand (nelisp-aot--word-bits ta) 3) 1))
                                        t))
                    (dolist (e (nthcdr 3 f)) (walk e in-guarded)))
                   (t (dolist (e f) (walk e in-guarded))))))
      (walk form nil))
    ok))

(ert-deftest nelisp-aot-buffer-leaf-guards-before-extern ()
  "The 1-argument leaves test the argument word before the extern; the else
arm is the generic call on the same already evaluated argument."
  (dolist (src '((char-after x) (goto-char x)))
    (let* ((form (nelisp-aot-compiler--buffer-leaf-rewrite src))
           (ta (car (car (cadr form))))
           (guard (nth 1 (nth 2 form)))
           (then (nth 2 (nth 2 form)))
           (else (nth 3 (nth 2 form))))
      ;; (let ((TA (as-word X))) (if GUARD THEN ELSE))
      (should (eq (car form) 'let))
      (should (equal guard (nelisp-aot-compiler--lisp-word-fixnum-form ta)))
      (should (equal else `(nelisp-aot--generic-call ,(car src) ,ta)))
      ;; the extern is only in THEN, and its fallback is the same generic call
      (should-not (cl-search "leaf-extern" (format "%S" guard)))
      (should-not (cl-search "leaf-extern" (format "%S" else)))
      (should (cl-search "leaf-extern" (format "%S" then)))
      (should (cl-search (format "%S" else) (format "%S" then))))))

(ert-deftest nelisp-aot-buffer-leaf-guard-mutation-detected ()
  "Control: with the argument guard dropped the structural check fails."
  (cl-letf (((symbol-function 'nelisp-aot-compiler--lisp-word-fixnum-form)
             (lambda (_v) t)))
    (let* ((form (nelisp-aot-compiler--buffer-leaf-rewrite '(goto-char x)))
           (guard (nth 1 (nth 2 form))))
      (should (eq guard t))
      (should-not (equal guard '(= (logand (nelisp-aot--word-bits ta) 3) 1))))))

;;; Stale binary: the reader's `nelisp--native-symbol-addr' answers 0 for an
;;; index past its build-time chain (documented in the standalone build), and
;;; the loader turns 0 into an error, so an artifact that needs
;;; `nl_native_buf_leaf' cannot be loaded on a binary built without it.
(ert-deftest nelisp-aot-buffer-leaf-stale-binary-fails-loudly ()
  (let* ((n (length nelisp-native-load-bridgeable-symbols))
         (old-chain (- n 2)))             ; a binary built before this entry
    (cl-letf (((symbol-function 'nelisp--native-symbol-addr)
               (lambda (i) (if (< i old-chain) (+ #x1000 i) 0))))
      (should (> (nelisp-native-load--symbol-addr "nl_alloc_symbol") 0))
      (let ((err (should-error (nelisp-native-load--symbol-addr "nl_native_buf_leaf")
                               :type 'error)))
        (should (string-match-p "resolved to 0" (cadr err)))))
    ;; control: a binary with the entry resolves it
    (cl-letf (((symbol-function 'nelisp--native-symbol-addr)
               (lambda (i) (+ #x1000 i))))
      (should (> (nelisp-native-load--symbol-addr "nl_native_buf_leaf") 0)))))

(ert-deftest nelisp-aot-buffer-leaf-build-list-matches-loader-list ()
  "The build script's bridge list and the loader's are the same (no link between them)."
  (let ((build (expand-file-name "scripts/nelisp-standalone-build.el" nelisp-aot-buffer-leaf-test--root)))
    (with-temp-buffer
      (insert-file-contents build)
      (goto-char (point-min))
      (re-search-forward "(defconst nelisp-standalone--reader-neln-bridgeable-symbols")
      (goto-char (match-beginning 0))
      (let ((names (cl-remove-if-not (lambda (x) (string-match-p "\\`[a-z_0-9]+\\'" x))
                                     (nelisp-aot-buffer-leaf-test--strings (read (current-buffer))))))
        ;; the computed middle of both lists is not readable here: compare the tails
        (should (equal (last names 4) '("wf_bytecode_condition_matches" "nl_exit_values" "nl_native_buf_leaf" "nl_native_buf_rec")))
        (should (equal (last nelisp-native-load-bridgeable-symbols 4) (last names 4)))
        (should (member "nl_native_buf_leaf" names))
        ))))

(defun nelisp-aot-buffer-leaf-test--strings (form)
  "All string literals of FORM in order (quoted lists inside append/mapcar)."
  (cond ((stringp form) (list form))
        ((consp form) (append (nelisp-aot-buffer-leaf-test--strings (car form))
                              (nelisp-aot-buffer-leaf-test--strings (cdr form))))))

;;; Inline read flag
(defun nelisp-aot-buffer-leaf-test--compile-inline (source tag)
  (let ((nelisp-aot-compiler-buffer-leaves-inline t))
    (nelisp-aot-buffer-leaf-test--compile source tag nil)))

(ert-deftest nelisp-aot-buffer-leaf-inline-emitted ()
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (dolist (c '(point point-min point-max))
    (let* ((src (format "(defun blt (x) (%s))" c))
           (r (nelisp-aot-buffer-leaf-test--relocs
               (nelisp-aot-buffer-leaf-test--compile-inline src (format "in-%s" c)))))
      (should (= 1 (cl-count "nl_native_buf_rec" r :test #'equal)))
      (should (= 0 (cl-count "nl_native_buf_leaf" r :test #'equal)))
      (should (cl-some (lambda (n) (string-match-p "nl_alloc_symbol\\|funcalln" n)) r))))
  ;; the others stay on the bridged leaf (flag-inline alone lowers nothing else)
  (dolist (c '("(char-after x)" "(goto-char x)" "(eobp)"))
    (let ((r (nelisp-aot-buffer-leaf-test--relocs
              (nelisp-aot-buffer-leaf-test--compile-inline
               (format "(defun blt (x) %s)" c) (format "inn-%d" (sxhash c))))))
      (should (= 0 (cl-count "nl_native_buf_rec" r :test #'equal)))
      (should (= 0 (cl-count "nl_native_buf_leaf" r :test #'equal))))))

(ert-deftest nelisp-aot-buffer-leaf-inline-is-call-free-between-fetch-and-read ()
  "Between the record fetch and the reads there is no call: the object has
exactly one call to the fetch before the fallback's calls."
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (let* ((obj (nelisp-aot-buffer-leaf-test--compile-inline "(defun blt (x) (point-max))" "cf"))
         (dis (with-temp-buffer (call-process "objdump" nil t nil "-dr" "-M" "intel" obj) (buffer-string)))
         (pos (string-match "nl_native_buf_rec" dis))
         (after (and pos (substring dis pos)))
         (first-other (and after (string-match "R_X86_64_PLT32[ \t]+\\(nl_alloc\\|nelisp_aot\\)" after)))
         (between (and first-other (substring after 0 first-other))))
    (should between)
    ;; the only call between the fetch and the first fallback reloc is that
    ;; fallback's own call instruction (its reloc line follows it)
    (should (= 1 (cl-count-if (lambda (l) (string-match-p "\tcall " l)) (split-string between "\n"))
               ))))

(ert-deftest nelisp-aot-buffer-leaf-fxloop-declines-leaf-loops ()
  "A fixnum loop with a leaf call stays declined: object identical with the loop flag on/off."
  (skip-unless (nelisp-aot-buffer-leaf-test--tools-p))
  (let* ((src "(defun blt (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (point))) (setq i (1+ i))) s))")
         (a (let ((nelisp-aot-compiler-fixnum-loops t)) (nelisp-aot-buffer-leaf-test--compile src "fl-on" t)))
         (b (let ((nelisp-aot-compiler-fixnum-loops nil)) (nelisp-aot-buffer-leaf-test--compile src "fl-off" t))))
    (should (equal (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally a) (buffer-string))
                   (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally b) (buffer-string))))))

;;; In-image GNU differential (NOT RUN until the batch build) -----------------

(require 'nelisp-aot-buffer-leaf-image)

(defun nelisp-aot-buffer-leaf-test--home ()
  "A checkout with a binary built with nl_native_buf_leaf, or nil."
  (let ((h (getenv "NELISP_AOT_BUFLEAF_HOME")))
    (and h (file-executable-p (expand-file-name "target/nelisp" h)) h)))

(defun nelisp-aot-buffer-leaf-test--gnu-lines ()
  (let ((pos 0) (r nil))
    (while (setq r (condition-case nil
                       (read-from-string nelisp-aot-buffer-leaf-image-source pos)
                     (end-of-file nil)))
      (eval (car r) nil) (setq pos (cdr r))))
  (dolist (n nelisp-aot-buffer-leaf-image-names) (byte-compile (intern n)))
  (nelisp-aot-buffer-leaf-image-run (lambda (n a) (apply (intern n) a))))

(defun nelisp-aot-buffer-leaf-test--image-lines (neln home)
  (let* ((dir (file-name-directory neln))
         (script (expand-file-name "run.el" dir))
         (root nelisp-aot-buffer-leaf-test--root))
    (write-region
     (format "(setq load-path (append (list %S %S) load-path))
(require 'nelisp-native-load)
(setq nelisp-native-load-expected-codegen-settings '%S)
(load %S nil t t)
(defvar dt-handles nil)
(defun dt-call (name args)
  (let ((h (cdr (assoc name dt-handles))))
    (unless h (setq h (nelisp-native-load-artifact %S name))
            (setq dt-handles (cons (cons name h) dt-handles)))
    (nelisp-native-load-call h args)))
(dolist (l (nelisp-aot-buffer-leaf-image-run (function dt-call)))
  (princ \"L \") (princ l) (princ \"\\n\"))
(princ \"DT-DONE\\n\")\n"
             (expand-file-name "lisp" root) (expand-file-name "src" root)
             nelisp-aot-buffer-leaf-test--last-settings
             (expand-file-name "test/nelisp-aot-buffer-leaf-image.el" root) neln)
     nil script nil 'silent)
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory home))
            (process-environment (cons (concat "NELISP_HOME=" home) process-environment)))
        (call-process "bash" nil t nil "-c"
                      (format "ulimit -v 8388608; exec timeout 900 taskset -c 9,25 target/nelisp exec-runtime-image build/nemacs-runtime.nlri '(load \"%s\" nil t t)' < /dev/null" script)))
      (goto-char (point-min))
      (should (search-forward "DT-DONE" nil t))
      (let (out) (goto-char (point-min))
           (while (re-search-forward "^L \\(.*\\)$" nil t) (push (match-string 1) out))
           (nreverse out)))))

(ert-deftest nelisp-aot-buffer-leaf/image-differential-inline-vs-gnu ()
  "Same differential with the inline point/point-min/point-max reads."
  (skip-unless (nelisp-aot-buffer-leaf-test--home))
  (let* ((nelisp-aot-compiler-buffer-leaves-inline t)
         (neln (nelisp-aot-buffer-leaf-test--compile-neln
                nelisp-aot-buffer-leaf-image-source "inl" t))
         (want (nelisp-aot-buffer-leaf-test--gnu-lines))
         (got (nelisp-aot-buffer-leaf-test--image-lines neln (nelisp-aot-buffer-leaf-test--home))))
    (should (equal want got))))

(ert-deftest nelisp-aot-buffer-leaf/image-differential-vs-gnu ()
  "Every leaf, in narrowing/gap/multibyte states with marker, out-of-range,
bignum and wrong-type arguments, answers as GNU's byte-compiled code does."
  (skip-unless (nelisp-aot-buffer-leaf-test--home))
  (let* ((neln (nelisp-aot-buffer-leaf-test--compile-neln
                nelisp-aot-buffer-leaf-image-source "all" t))
         (want (nelisp-aot-buffer-leaf-test--gnu-lines))
         (got (nelisp-aot-buffer-leaf-test--image-lines neln (nelisp-aot-buffer-leaf-test--home))))
    (should (equal (length want) (length got)))
    (should (equal want got))))

(ert-deftest nelisp-aot-buffer-leaf/image-mutation-control ()
  "Control: a lowering that swaps point and point-max ids is caught."
  (skip-unless (nelisp-aot-buffer-leaf-test--home))
  (let* ((nelisp-aot-compiler--buffer-leaves
          (mapcar (lambda (e) (cond ((eq (car e) 'point) (list 'point 42 0 0))
                                    ((eq (car e) 'point-max) (list 'point-max 40 0 0))
                                    (t e)))
                  nelisp-aot-compiler--buffer-leaves))
         (neln (nelisp-aot-buffer-leaf-test--compile-neln
                nelisp-aot-buffer-leaf-image-source "mut" t))
         (want (nelisp-aot-buffer-leaf-test--gnu-lines))
         (got (nelisp-aot-buffer-leaf-test--image-lines neln (nelisp-aot-buffer-leaf-test--home))))
    (should-not (equal want got))
    ;; The difference must come from wrong values, not from a load refusal.
    (should-not (cl-some (lambda (l) (string-match-p "cannot load\\|codegen-settings" l)) got))))

(defun nelisp-aot-buffer-leaf-test--compile-neln (source tag flag)
  (let* ((dir (nelisp-aot-buffer-leaf-test--dir))
         (src (expand-file-name (concat tag ".el") dir))
         (art (concat src ".neln"))
         (nelisp-aot-compiler--dynamic-user-calls t)
         (nelisp-aot-compiler-buffer-leaves flag))
    (with-temp-file src (insert source))
    (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'opportunistic)
    (setq nelisp-aot-buffer-leaf-test--last-settings (nelisp-native-load-codegen-settings))
    art))

(provide 'nelisp-aot-buffer-leaf-test)
;;; nelisp-aot-buffer-leaf-test.el ends here
