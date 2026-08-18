;;; nelisp-standalone-calln-test.el --- standalone builtin calln checks  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused regression tests for the standalone `nelisp_aot_builtin_calln`
;; bridge.  These guard the source shape, the boxed-arg ordering path, and
;; the runtime symbol resolver wiring without touching the unrelated target
;; test file.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-standalone-build)

(defun nelisp-standalone-calln-test--repo-root ()
  "Return the repository root for this test file."
  (let ((this (or load-file-name buffer-file-name)))
    (if this
        (file-name-directory
         (directory-file-name (file-name-directory this)))
      default-directory)))

(defun nelisp-standalone-calln-test--build-script-source ()
  "Return the standalone build script as a plain source string."
  (let ((path (expand-file-name "scripts/nelisp-standalone-build.el"
                                (nelisp-standalone-calln-test--repo-root))))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun nelisp-standalone-calln-test--calln-form ()
  "Return the quoted source form for `nelisp_aot_builtin_calln'."
  (cl-find-if
   (lambda (form)
     (and (consp form)
          (eq (car form) 'defun)
          (eq (cadr form) 'nelisp_aot_builtin_calln)))
   nelisp-standalone--applyfn-bf-helpers))

(defun nelisp-standalone-calln-test--tree-count (needle tree)
  "Count NEEDLE in TREE using `equal' over the full nested structure."
  (cond
   ((equal needle tree) 1)
   ((consp tree)
    (+ (nelisp-standalone-calln-test--tree-count needle (car tree))
       (nelisp-standalone-calln-test--tree-count needle (cdr tree))))
   (t 0)))

(defun nelisp-standalone-calln-test--run-reader-src (source)
  "Run the standalone reader with SOURCE passed through `--eval'."
  (let ((stdout-buf (generate-new-buffer " *nelisp-reader-stdout*"))
        (stderr-file (make-temp-file "nelisp-reader-stderr"))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (call-process (nelisp-standalone--output-path t)
                              nil (list stdout-buf stderr-file) nil
                              "--eval" source))
          (let ((stdout-text (with-current-buffer stdout-buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                (stderr-text (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (list :exit exit-code
                  :stdout stdout-text
                  :stderr stderr-text)))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nelisp-standalone-calln-test--run-reader-value (source)
  "Run SOURCE in the standalone reader and return the printed value."
  (let* ((result (nelisp-standalone-calln-test--run-reader-src source))
         (exit (plist-get result :exit))
         (stdout (plist-get result :stdout))
         (stderr (plist-get result :stderr)))
    (should (= exit 0))
    (should (string-match-p "\\`[[:space:]\n]*\\'" stderr))
    (car (read-from-string stdout))))

(defun nelisp-standalone-calln-test--probe (argc args)
  "Execute the bridge with ARGC and opaque boxed ARGS.
Return a plist with the allocation count and the observed dispatcher
call.  The probe stubs `nelisp_cons_construct' so the source-level list
construction can be asserted without building a native artifact."
  (unless (fboundp 'nelisp_aot_builtin_calln)
    (eval (nelisp-standalone-calln-test--calln-form)))
  (let ((slots (make-hash-table :test 'eq))
        (alloc-count 0)
        (seen nil)
        (next-slot 1000))
    (cl-labels ((resolve
                 (slot)
                 (cond
                  ((and (numberp slot) (= slot 1000)) nil)
                  ((and (numberp slot) (gethash slot slots slot)))
                  (t slot))))
      (cl-letf (((symbol-function 'alloc-bytes)
                 (lambda (&rest _)
                   (cl-incf alloc-count)
                   (prog1 next-slot
                     (setq next-slot (1+ next-slot)))))
                ((symbol-function 'ptr-write-u64)
                 (lambda (&rest _) 0))
                ((symbol-function 'seq)
                 (lambda (&rest forms)
                   (car (last forms))))
                ((symbol-function 'nl_alloc_symbol)
                 (lambda (_bytes _len slot)
                   (puthash slot 'builtin slots)
                   slot))
                ((symbol-function 'nelisp_cons_construct)
                 (lambda (car cdr slot)
                   (puthash slot (cons (resolve car) (resolve cdr)) slots)
                   slot))
                ((symbol-function 'nelisp_apply_function)
                 (lambda (func arg-list mirror out)
                   (setq seen (list :func (resolve func)
                                    :args (resolve arg-list)
                                    :mirror mirror
                                    :out out))
                   0)))
        (apply #'nelisp_aot_builtin_calln
               'mirror 'frames 'list argc 'out 'scratch
               '(a0 a1 a2 a3 a4 a5 a6 a7))
        (list :alloc-count alloc-count :seen seen)))))

(ert-deftest nelisp-standalone-calln/source-signature-is-fixed ()
  "The bridge keeps the exact ABI shape the compiler expects."
  (let* ((form (nelisp-standalone-calln-test--calln-form))
         (args (nth 2 form))
         (source (prin1-to-string form)))
    (should form)
    (should (equal args
                   '(mirror frames name argc out scratch a0 a1 a2 a3 a4 a5 a6 a7)))
    (should-not (memq '&rest args))
    (should (string-match-p
             "(error \"nelisp_aot_builtin_calln: argc out of range: %S\" argc)"
             source))
    (should (string-match-p "nelisp_apply_function func args0 mirror out"
                            source))
    (should (= (cl-count-if
                (lambda (form)
                  (and (consp form)
                       (eq (car form) 'defun)
                       (eq (cadr form) 'nelisp_aot_builtin_calln)))
                nelisp-standalone--applyfn-bf-helpers)
               1))))

(ert-deftest nelisp-standalone-calln/boxed-args-preserve-order-and-mirror ()
  "The bridge forwards boxed args in order and keeps MIRROR/OUT intact."
  (let ((zero (nelisp-standalone-calln-test--probe 0 nil))
        (two (nelisp-standalone-calln-test--probe 2 '(a0 a1)))
        (eight (nelisp-standalone-calln-test--probe 8
                                                   '(a0 a1 a2 a3 a4 a5 a6 a7))))
    (should (= (plist-get zero :alloc-count) 5))
    (should (= (plist-get two :alloc-count) 7))
    (should (= (plist-get eight :alloc-count) 13))
    (should (equal (plist-get (plist-get zero :seen) :args) nil))
    (should (equal (plist-get (plist-get two :seen) :args) '(a0 a1)))
    (should (equal (plist-get (plist-get eight :seen) :args)
                   '(a0 a1 a2 a3 a4 a5 a6 a7)))
    (should (equal (plist-get (plist-get zero :seen) :func)
                   '(builtin list)))
    (should (equal (plist-get (plist-get two :seen) :func)
                   '(builtin list)))
    (should (equal (plist-get (plist-get eight :seen) :func)
                   '(builtin list)))
    (dolist (probe (list zero two eight))
      (should (eq (plist-get (plist-get probe :seen) :mirror) 'mirror))
      (should (eq (plist-get (plist-get probe :seen) :out) 'out)))))

(ert-deftest nelisp-standalone-calln/rejects-argc-outside-0..8-before-args ()
  "Bad argc values must fail before any boxed argument is touched."
  (let ((alloc-count 0))
    (cl-letf (((symbol-function 'alloc-bytes)
               (lambda (&rest _)
                 (cl-incf alloc-count)
                 (error "alloc-bytes should not be reached"))))
      (should-error
       (nelisp_aot_builtin_calln 'mirror 'frames 'list -1 'out 'scratch
                                 'a0 'a1 'a2 'a3 'a4 'a5 'a6 'a7)
       :type 'error)
      (should-error
       (nelisp_aot_builtin_calln 'mirror 'frames 'list 9 'out 'scratch
                                 'a0 'a1 'a2 'a3 'a4 'a5 'a6 'a7)
       :type 'error)
      (should (= alloc-count 0)))))

(ert-deftest nelisp-standalone-calln/runtime-symbol-resolver-returns-positive-address ()
  "The standalone reader resolver must expose a live address for calln."
  (skip-unless (and (executable-find "nm")
                    (file-exists-p (nelisp-standalone--output-path t))))
  (let ((addr (nelisp-standalone-calln-test--run-reader-value
               "(nelisp--runtime-symbol-address \"nelisp_aot_builtin_calln\")")))
    (should (integerp addr))
    (should (> addr 0))))

(ert-deftest nelisp-standalone-calln/linked-symbol-is-unique-in-standalone-reader ()
  "The linked standalone reader binary should export the calln shim once."
  (skip-unless (executable-find "nm"))
  (let* ((repo-root (nelisp-standalone-calln-test--repo-root))
         (default-directory repo-root)
         (binary (nelisp-standalone--output-path t)))
    (call-process "make" nil nil nil "standalone-reader")
    (should (file-exists-p binary))
    (let ((lines nil))
      (with-temp-buffer
        (should (= (call-process "nm" nil t nil binary) 0))
        (setq lines (split-string (buffer-string) "\n" t)))
      (should (= (cl-count-if
                  (lambda (line)
                    (string-match-p "\\<nelisp_aot_builtin_calln\\>" line))
                  lines)
                 1)))))

(provide 'nelisp-standalone-calln-test)

;;; nelisp-standalone-calln-test.el ends here
