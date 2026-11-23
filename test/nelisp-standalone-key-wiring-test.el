;;; nelisp-standalone-key-wiring-test.el --- unit keys follow the closure digest -*- lexical-binding: t; -*-

;; Loads the real build script and computes `nelisp-standalone--unit-cache-key'
;; with `--this-file' pointing at an edited temp copy.  No unit is compiled.
;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-key-wiring-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'nelisp-standalone-build)

(defconst nl-kw--src nelisp-standalone--this-file)

(defun nl-kw--key (edits &optional source)
  "Unit key of SOURCE with the script text edited by EDITS ((FROM . TO), once each)."
  (let ((tmp (make-temp-file "nl-kw" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert-file-contents nl-kw--src)
            (dolist (e edits)
              (goto-char (point-min))
              (should (search-forward (car e) nil t))
              (should-not (search-forward (car e) nil t))
              (replace-match (cdr e) t t nil)))
          ;; The temp copy must sit next to the libraries for its `require' scan.
          (let ((nelisp-standalone--this-file tmp)
                (nelisp-standalone--toolchain-digest nil)
                (nelisp-standalone--key-closure nil))
            (nelisp-standalone--unit-cache-key (or source '(seq (defun probe_a (x) x))))))
      (delete-file tmp))))

(ert-deftest nl-kw-comment-edit-keeps-every-key ()
  (let ((base (nl-kw--key nil)))
    (should (equal base (nl-kw--key '(("(defun nelisp-standalone--unit-cache-key (source)"
                                       . ";; a comment\n\n(defun nelisp-standalone--unit-cache-key   (source)")))))
    ;; Negative controls: a post-key compiler def edit changes the key; so does
    ;; FULL_KEY=1 for the same comment-only edit (global key hashes the file).
    (should-not (equal base (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                                           . "(defun nelisp-standalone--xinline-enabled-p () (ignore 1)")))))
    (let ((process-environment (cons "NELISP_STANDALONE_FULL_KEY=1" process-environment)))
      (should-not (equal (nl-kw--key nil)
                         (nl-kw--key '((";;; nelisp-standalone-build.el ends here" . ";; c\n;;; nelisp-standalone-build.el ends here"))))))))

(ert-deftest nl-kw-producer-edit-keeps-keys ()
  (let ((base (nl-kw--key nil)))
    (should (equal base (nl-kw--key '(("(defun nelisp-standalone--patch-binding-kinds (source)"
                                       . "(defun nelisp-standalone--patch-binding-kinds (source) (ignore 42)")))))))

(ert-deftest nl-kw-opaque-form-takes-global-key ()
  (let ((base (nl-kw--key nil))
        (msgs nil))
    (cl-letf (((symbol-function 'message) (lambda (f &rest a) (push (apply #'format f a) msgs))))
      ;; An unlisted intern in a closure def: fallback fires, so ANY edit rekeys.
      (let ((k1 (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                               . "(defun nelisp-standalone--xinline-enabled-p () (intern \"x\")"))))
            (k2 (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                               . "(defun nelisp-standalone--xinline-enabled-p () (intern \"x\")")
                              ("(defun nelisp-standalone--patch-binding-kinds (source)"
                               . "(defun nelisp-standalone--patch-binding-kinds (source) (ignore 7)")))))
        (should-not (equal k1 base))
        (should-not (equal k1 k2))))
    (should (seq-some (lambda (m) (string-match-p "global key" m)) msgs))))

(ert-deftest nl-kw-dynamic-binding-values-enter-the-key ()
  (let ((src '(seq (defun probe_a (x) x)))
        (nelisp-standalone--toolchain-digest "fixed"))
    (let ((a (let ((nelisp-aot-compiler--allow-external-user-calls nil)) (nelisp-standalone--unit-cache-key src)))
          (b (let ((nelisp-aot-compiler--allow-external-user-calls t)) (nelisp-standalone--unit-cache-key src)))
          (c (let ((nelisp-aot-compiler--x86-private-continuations (not nelisp-aot-compiler--x86-private-continuations)))
               (nelisp-standalone--unit-cache-key src))))
      (should-not (equal a b))
      (should-not (equal a c))
      ;; Negative control: an unrelated binding does not move the key.
      (should (equal a (let ((nelisp-aot-compiler--allow-external-user-calls nil)
                             (nelisp-standalone--recompiled '(x)))
                         (nelisp-standalone--unit-cache-key src)))))))

(provide 'nelisp-standalone-key-wiring-test)
