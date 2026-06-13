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
                      "(defmacro if-let "))
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
  "Run READER exec-runtime-image IMAGE FORM; return captured output (buffer)."
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

(provide 'runtime-stdlib-extra-test)

;;; runtime-stdlib-extra-test.el ends here
