;;; runtime-stdlib-extra-test.el --- init-parity stdlib checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for the init-parity helpers baked into the bridge runtime
;; image (nemacs-runtime-stdlib-extra.el): the ubiquitous config helpers a
;; standalone runtime lacked -- when-let / if-let (+ `*' forms), mapcan,
;; assoc-default, cl-reduce.  Two layers, mirroring skk-okuri-conversion-test:
;;
;;   1. Host ERT (always): pins the gated definitions in the source.
;;   2. Standalone gate (opt-in: NEMACS_RUN_STDLIB_EXTRA=1 + a built reader):
;;      builds a prelude+stdlib-extra image and asserts the behaviours on the
;;      actual standalone runtime (these macros are expanded by the runtime
;;      reader, so host Emacs can't exercise them meaningfully).

;;; Code:

(require 'ert)

(defconst runtime-stdlib-extra-test--repo-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun runtime-stdlib-extra-test--path (rel)
  (expand-file-name rel runtime-stdlib-extra-test--repo-root))

(defconst runtime-stdlib-extra-test--source
  (runtime-stdlib-extra-test--path "src/nemacs-runtime-stdlib-extra.el"))

(defun runtime-stdlib-extra-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;; --- host source-shape (always runs) -------------------------------------

(ert-deftest runtime-stdlib-extra-test/source-shape ()
  "stdlib-extra defines the init-parity helpers (gated) and balances parens."
  (should (file-readable-p runtime-stdlib-extra-test--source))
  (with-temp-buffer
    (insert-file-contents runtime-stdlib-extra-test--source)
    (goto-char (point-min))
    (check-parens))
  (let ((source (runtime-stdlib-extra-test--slurp
                 runtime-stdlib-extra-test--source)))
    (dolist (needle '("(unless (fboundp 'mapcan)"
                      "(unless (fboundp 'assoc-default)"
                      "(unless (fboundp 'cl-reduce)"
                      "(unless (fboundp 'if-let*)"
                      "(defmacro when-let* "
                      "(defmacro if-let* "
                      "(defmacro when-let "
                      "(defmacro if-let "
                      "(unless (fboundp 'add-to-list)"
                      "(unless (fboundp 'ignore)"
                      "(unless (fboundp 'string-blank-p)"
                      "(unless (fboundp 'string-remove-prefix)"
                      "(unless (fboundp 'string-remove-suffix)"
                      "(unless (fboundp 'cl-find)"
                      "(unless (fboundp 'cl-remove-duplicates)"
                      "(unless (fboundp 'add-hook)"
                      "(unless (fboundp 'remove-hook)"
                      "(unless (fboundp 'run-hooks)"
                      "(unless (fboundp 'eval-after-load)"
                      "(unless (fboundp 'with-eval-after-load)"
                      "(unless (fboundp 'setq-local)"
                      "(unless (fboundp 'cl-pushnew)"
                      "(unless (fboundp 'cl-sort)"))
      (should (string-match-p (regexp-quote needle) source)))))

;;; --- standalone gate (opt-in) --------------------------------------------

(defun runtime-stdlib-extra-test--reader ()
  "Return an absolute, executable standalone NeLisp reader path, or nil."
  (catch 'found
    (dolist (candidate
             (list (getenv "NEMACS_GUI_BRIDGE_NELISP")
                   (getenv "NELISP")
                   (runtime-stdlib-extra-test--path "../nelisp/target/nelisp")
                   "/tmp/nelisp-snap/nelisp"))
      (when candidate
        (let ((abs (expand-file-name candidate)))
          (when (file-executable-p abs) (throw 'found abs)))))
    nil))

(defmacro runtime-stdlib-extra-test--skip-unless-standalone (&rest body)
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_STDLIB_EXTRA"))
     (ert-skip "set NEMACS_RUN_STDLIB_EXTRA=1 to run standalone stdlib checks"))
    ((not (runtime-stdlib-extra-test--reader))
     (ert-skip "no standalone reader; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    (t ,@body)))

(defun runtime-stdlib-extra-test--build-image ()
  "Write a source-v1 image of prelude + regexp + stdlib-extra (UTF-8)."
  (let ((image (make-temp-file "stdlib-extra-image-" nil ".nlri"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (files (list
                (runtime-stdlib-extra-test--path
                 "../nelisp/scripts/nelisp-stdlib-prelude.el")
                (runtime-stdlib-extra-test--path
                 "../nelisp/lisp/nelisp-stdlib-regexp.el")
                runtime-stdlib-extra-test--source)))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (dolist (f files)
        (when (file-readable-p f)
          (insert-file-contents f) (goto-char (point-max))))
      (insert "\n)\n"))
    image))

(defun runtime-stdlib-extra-test--run (reader image form)
  "Run READER exec-runtime-image IMAGE FORM; return captured output (buffer).
No transport isolation needed here: each call-process is a fresh runtime with
fresh globals and these forms write no /tmp/nemacs-* state (unlike the SKK
suite, whose persistent IME learning file leaks on disk)."
  (with-temp-buffer
    (let ((status (call-process reader nil (current-buffer) nil
                                "exec-runtime-image" image form)))
      (unless (equal 0 status)
        (ert-fail (format "exec-runtime-image failed: status=%S\noutput:\n%s"
                          status (buffer-string))))
      (buffer-string))))

(ert-deftest runtime-stdlib-extra-test/standalone-behaviour ()
  "The init-parity helpers behave correctly on the standalone runtime."
  (runtime-stdlib-extra-test--skip-unless-standalone
    (let ((reader (runtime-stdlib-extra-test--reader))
          (image (runtime-stdlib-extra-test--build-image)))
      (unwind-protect
          (let ((out (runtime-stdlib-extra-test--run
                      reader image
                      "(progn (princ (concat
  \"mapcan=\" (format \"%S\" (mapcan (lambda (x) (list x x)) '(1 2 3))) \"\\n\"
  \"assoc=\" (format \"%S\" (assoc-default 'b '((a . 1) (b . 2)))) \"\\n\"
  \"reduce=\" (format \"%S\" (cl-reduce '+ '(1 2 3 4))) \"\\n\"
  \"reduce-init=\" (format \"%S\" (cl-reduce '+ '(1 2 3) :initial-value 10)) \"\\n\"
  \"wl-hit=\" (format \"%S\" (when-let ((a 5) (b 7)) (+ a b))) \"\\n\"
  \"wl-miss=\" (format \"%S\" (when-let ((a 5) (b nil)) (+ a 1))) \"\\n\"
  \"il-else=\" (format \"%S\" (if-let ((a nil)) 'then 'else)) \"\\n\"
  \"wl-single=\" (format \"%S\" (when-let (x 42) (* x 2))) \"\\n\")))")))
            (should (string-match-p "mapcan=(1 1 2 2 3 3)" out))
            (should (string-match-p "assoc=2" out))
            (should (string-match-p "reduce=10" out))
            (should (string-match-p "reduce-init=16" out))
            (should (string-match-p "wl-hit=12" out))
            (should (string-match-p "wl-miss=nil" out))
            (should (string-match-p "il-else=else" out))
            (should (string-match-p "wl-single=84" out)))
        (delete-file image)))))

(ert-deftest runtime-stdlib-extra-test/standalone-init-helpers ()
  "add-to-list + string/cl helpers behave correctly on the standalone runtime."
  (runtime-stdlib-extra-test--skip-unless-standalone
    (let ((reader (runtime-stdlib-extra-test--reader))
          (image (runtime-stdlib-extra-test--build-image)))
      (unwind-protect
          (let ((out (runtime-stdlib-extra-test--run
                      reader image
                      "(progn
  (set 'lp '(\"/b\")) (add-to-list 'lp \"/a\") (add-to-list 'lp \"/b\") (add-to-list 'lp \"/c\" t)
  (princ (concat
    \"atl=\" (format \"%S\" (symbol-value 'lp)) \"\\n\"
    \"atl-fresh=\" (format \"%S\" (add-to-list 'fresh-var 'x)) \"\\n\"
    \"ignore=\" (format \"%S\" (ignore 1 2)) \" always=\" (format \"%S\" (always 9)) \"\\n\"
    \"blank-y=\" (format \"%S\" (string-blank-p \"   \")) \" blank-n=\" (format \"%S\" (string-blank-p \" x \")) \"\\n\"
    \"rmpre=\" (string-remove-prefix \"foo-\" \"foo-bar\") \" rmsuf=\" (string-remove-suffix \".el\" \"init.el\") \"\\n\"
    \"clfind=\" (format \"%S\" (cl-find 3 '(1 2 3 4))) \"\\n\"
    \"clfind-key=\" (format \"%S\" (cl-find 2 '((a 1) (b 2)) :key 'cadr)) \"\\n\"
    \"clrmdup=\" (format \"%S\" (cl-remove-duplicates '(1 2 1 3 2))) \"\\n\")))")))
            (should (string-match-p "atl=(\"/a\" \"/b\" \"/c\")" out))
            (should (string-match-p "atl-fresh=(x)" out))
            (should (string-match-p "ignore=nil always=t" out))
            (should (string-match-p "blank-y=0 blank-n=nil" out))
            (should (string-match-p "rmpre=bar rmsuf=init" out))
            (should (string-match-p "clfind=3" out))
            (should (string-match-p "clfind-key=(b 2)" out))
            (should (string-match-p "clrmdup=(1 3 2)" out)))
        (delete-file image)))))

(ert-deftest runtime-stdlib-extra-test/standalone-hooks-and-load ()
  "add-hook / run-hooks / with-eval-after-load / cl-sort on the standalone runtime."
  (runtime-stdlib-extra-test--skip-unless-standalone
    (let ((reader (runtime-stdlib-extra-test--reader))
          (image (runtime-stdlib-extra-test--build-image)))
      (unwind-protect
          (let ((out (runtime-stdlib-extra-test--run
                      reader image
                      "(progn
  (set 'my-hook nil) (defun fa () 'a) (defun fb () 'b)
  (add-hook 'my-hook 'fa) (add-hook 'my-hook 'fb) (add-hook 'my-hook 'fa)
  (set 'ran nil) (defun rec () (set 'ran (cons 'x (symbol-value 'ran))))
  (set 'h2 nil) (add-hook 'h2 'rec) (run-hooks 'h2) (run-hooks 'h2)
  (set 'eal nil) (defun hookfn () nil)
  (with-eval-after-load 'anypkg (set 'eal 'applied) (add-hook 'h2 'hookfn))
  (set 'cont 'continued)
  (with-eval-after-load 'pkg2 (+ 1 (car 5)))   ; catchable error -> swallowed
  (princ (concat
    \"hook=\" (format \"%S\" (symbol-value 'my-hook)) \"\\n\"
    \"removed=\" (progn (remove-hook 'my-hook 'fa) (format \"%S\" (symbol-value 'my-hook))) \"\\n\"
    \"ran=\" (format \"%S\" (symbol-value 'ran)) \"\\n\"
    \"eal=\" (format \"%S\" (symbol-value 'eal)) \"\\n\"
    \"cont=\" (format \"%S\" (symbol-value 'cont)) \"\\n\"
    \"clsort=\" (format \"%S\" (cl-sort '(3 1 2) '<)) \"\\n\"
    \"clsort-key=\" (format \"%S\" (cl-sort '((1 \"c\") (2 \"a\")) 'string< :key 'cadr)) \"\\n\")))")))
            (should (string-match-p "hook=(fb fa)" out))
            (should (string-match-p "removed=(fb)" out))
            (should (string-match-p "ran=(x x)" out))
            (should (string-match-p "eal=applied" out))
            (should (string-match-p "cont=continued" out))
            (should (string-match-p "clsort=(1 2 3)" out))
            (should (string-match-p "clsort-key=((2 \"a\") (1 \"c\"))" out)))
        (delete-file image)))))

(ert-deftest runtime-stdlib-extra-test/standalone-realistic-config ()
  "Capstone: a realistic init.el-style config loads + applies on the runtime.
Exercises the whole init-parity surface together (require, add-to-list,
setq/setq-local, cl-pushnew, add-hook + run-hooks, with-eval-after-load,
when-let, assoc-default, cl-remove-duplicates, cl-sort) -- the concrete goal
of the init-parity substrate (task #26)."
  (runtime-stdlib-extra-test--skip-unless-standalone
    (let ((reader (runtime-stdlib-extra-test--reader))
          (image (runtime-stdlib-extra-test--build-image)))
      (unwind-protect
          (let ((out (runtime-stdlib-extra-test--run
                      reader image
                      "(progn
  (require 'cl-lib) (require 'subr-x)
  (defvar my-load-path nil)
  (add-to-list 'my-load-path \"/opt/lisp\")
  (add-to-list 'my-load-path \"/usr/lisp\")
  (add-to-list 'my-load-path \"/opt/lisp\")
  (setq my-tab-width 4)
  (setq-local my-indent 2)
  (defvar my-modes nil)
  (cl-pushnew 'prog-mode my-modes) (cl-pushnew 'text-mode my-modes) (cl-pushnew 'prog-mode my-modes)
  (defun my-prog-setup () (setq my-prog-configured t))
  (add-hook 'prog-mode-hook 'my-prog-setup)
  (with-eval-after-load 'cl-lib (setq my-deferred 'ran))
  (setq my-found (when-let ((v (assoc-default 'b '((a . 1) (b . 2))))) v))
  (setq my-uniq (cl-remove-duplicates '(1 2 1 3 2)))
  (setq my-sorted (cl-sort '(3 1 2) '<))
  (run-hooks 'prog-mode-hook)
  (princ (concat
    \"load-path=\" (format \"%S\" my-load-path) \"\\n\"
    \"tab=\" (format \"%S\" my-tab-width) \" indent=\" (format \"%S\" my-indent) \"\\n\"
    \"modes=\" (format \"%S\" my-modes) \"\\n\"
    \"prog-configured=\" (format \"%S\" my-prog-configured) \"\\n\"
    \"deferred=\" (format \"%S\" my-deferred) \"\\n\"
    \"found=\" (format \"%S\" my-found) \" uniq=\" (format \"%S\" my-uniq) \" sorted=\" (format \"%S\" my-sorted) \"\\n\")))")))
            (should (string-match-p "load-path=(\"/usr/lisp\" \"/opt/lisp\")" out))
            (should (string-match-p "tab=4 indent=2" out))
            (should (string-match-p "modes=(text-mode prog-mode)" out))
            (should (string-match-p "prog-configured=t" out))
            (should (string-match-p "deferred=ran" out))
            (should (string-match-p "found=2 uniq=(1 3 2) sorted=(1 2 3)" out)))
        (delete-file image)))))

(provide 'runtime-stdlib-extra-test)

;;; runtime-stdlib-extra-test.el ends here
