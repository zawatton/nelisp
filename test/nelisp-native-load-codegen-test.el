;;; nelisp-native-load-codegen-test.el --- artifacts bind to codegen settings  -*- lexical-binding: t; -*-

;; An artifact records every switch that changes generated code plus the
;; compiler source hash; the loader refuses one built under other settings.
;;   emacs -Q --batch -L lisp -L src -L test -l test/nelisp-native-load-codegen-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(require 'nelisp-native-load)

(defvar nelisp-native-load-codegen-test--dir nil)

(defun nelisp-native-load-codegen-test--compile (tag)
  "Compile a one-defun unit under the current settings; return its manifest."
  (let* ((dir (or nelisp-native-load-codegen-test--dir
                  (setq nelisp-native-load-codegen-test--dir (make-temp-file "cgs" t))))
         (src (expand-file-name (concat tag ".el") dir))
         (art (concat src ".neln")))
    (with-temp-file src
      (insert "(defun cgs-sum (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (1+ i))) s))\n"))
    (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
    (nelisp-native-load-manifest art)))

(defun nelisp-native-load-codegen-test--codegen (problems)
  (cl-remove-if-not (lambda (p) (memq (car-safe p) '(:codegen-settings-mismatch
                                                      :codegen-settings-missing)))
                    problems))

(ert-deftest nelisp-native-load-codegen/flag-list-is-complete ()
  (let (missing)
    (mapatoms (lambda (s)
                (let ((n (symbol-name s)))
                  (when (and (string-prefix-p "nelisp-aot-compiler-" n)
                             (not (string-prefix-p "nelisp-aot-compiler--" n))
                             (boundp s)
                             (not (memq s nelisp-native-load-codegen-flag-symbols)))
                    (push s missing)))))
    (should-not missing)))

(ert-deftest nelisp-native-load-codegen/mismatch-is-refused ()
  (skip-unless (and (eq system-type 'gnu/linux) (executable-find "cc")))
  (dolist (flag '(nelisp-aot-compiler-fixnum-loops
                  nelisp-aot-compiler-lisp-inline-incdec
                  nelisp-aot-compiler-lisp-word-fixnum-test))
    (let* ((a (symbol-value flag))
           (manifest (progn (set flag a)
                            (nelisp-native-load-codegen-test--compile "a"))))
      (unwind-protect
          (progn
            ;; Same settings: nothing codegen-related is reported.
            (should-not (nelisp-native-load-codegen-test--codegen
                         (nelisp-native-load-check manifest "cgs-sum")))
            ;; Switched: refused, naming the flag.
            (set flag (not a))
            (let ((p (nelisp-native-load-codegen-test--codegen
                      (nelisp-native-load-check manifest "cgs-sum"))))
              (should p)
              (should (assq flag (cdr (car p)))))
            ;; Negative control: with the comparison removed the stale
            ;; artifact passes the pre-flight, so the refusal is this check.
            (cl-letf (((symbol-function 'nelisp-native-load--codegen-problems)
                       (lambda (_r) nil)))
              (should-not (nelisp-native-load-codegen-test--codegen
                           (nelisp-native-load-check manifest "cgs-sum")))))
        (set flag a)))))

(ert-deftest nelisp-native-load-codegen/record-and-hash-required ()
  (skip-unless (and (eq system-type 'gnu/linux) (executable-find "cc")))
  (let* ((manifest (nelisp-native-load-codegen-test--compile "r"))
         (native (copy-sequence (plist-get manifest :native))))
    (should (plist-get native :codegen-settings))
    (should (stringp (plist-get (plist-get native :codegen-settings) :compiler-sha256)))
    ;; No record: refused.
    (let ((m (plist-put (copy-sequence manifest) :native
                        (let ((n (copy-sequence native))) (plist-put n :codegen-settings nil)))))
      (should (assq :codegen-settings-missing
                    (mapcar (lambda (p) (cons (car-safe p) p))
                            (nelisp-native-load-check m "cgs-sum")))))
    ;; Different compiler source hash: refused.
    (let* ((rec (copy-sequence (plist-get native :codegen-settings)))
           (m (plist-put (copy-sequence manifest) :native
                         (plist-put (copy-sequence native) :codegen-settings
                                    (plist-put rec :compiler-sha256 (make-string 64 ?0))))))
      (should (nelisp-native-load-codegen-test--codegen
              (nelisp-native-load-check m "cgs-sum"))))))

;; In-image: an artifact built with the fixnum-loop pass ON is refused by a
;; loader expecting the default (OFF) settings; with the comparison removed the
;; same stale artifact runs.  Needs NELISP_AOT_DIFF_HOME (a checkout with
;; target/nelisp).
(ert-deftest nelisp-native-load-codegen/image-refuses-stale-artifact ()
  (let ((home (getenv "NELISP_AOT_DIFF_HOME")))
    (skip-unless (and home (not (string-empty-p home))))
    ;; Set but unusable: fail loudly, never skip.
    (unless (file-executable-p (expand-file-name "target/nelisp" home))
      (error "NELISP_AOT_DIFF_HOME=%s has no executable target/nelisp" home))
    (let* ((dir (make-temp-file "cgi" t))
           (src (expand-file-name "m.el" dir))
           (art (expand-file-name "on.neln" dir))
           (probe (expand-file-name "probe.el" dir)))
      (with-temp-file src
        (insert "(defun cgs-sum (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (1+ i))) s))\n"))
      (let ((nelisp-aot-compiler-fixnum-loops t))
        (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required))
      (with-temp-file (expand-file-name "default.el" dir)
        (prin1 (nelisp-native-load-codegen-settings) (current-buffer)))
      (with-temp-file probe
        (insert (format "(load %S nil t)\n" (locate-library "nelisp-native-load.el")))
        (insert (format "(defun cg-try (tag) (princ (format \"%%s %%S\\n\" tag (condition-case e (nelisp-native-load-call (nelisp-native-load-artifact %S \"cgs-sum\") '(100)) (error 'REFUSED)))))\n" art))
        (insert (format "(setq nelisp-native-load-expected-codegen-settings (car (read-from-string (nelisp--syscall-read-file %S))))\n" (expand-file-name "default.el" dir)))
        (insert "(cg-try \"B\")\n(defun nelisp-native-load--codegen-problems (r) nil)\n(cg-try \"C\")\n"))
      (let ((out (with-temp-buffer
                   (let ((default-directory (file-name-as-directory home)))
                     (call-process "bash" nil t nil "-c"
                                   (format "ulimit -v 8388608; exec timeout 300 target/nelisp --load %s -- < /dev/null"
                                           (shell-quote-argument probe))))
                   (buffer-string))))
        (should (string-match-p "^B REFUSED$" out))
        (should (string-match-p "^C 4950$" out)))
      (delete-directory dir t))))

(provide 'nelisp-native-load-codegen-test)
