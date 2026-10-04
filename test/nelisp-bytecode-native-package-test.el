;;; nelisp-bytecode-native-package-test.el --- package reader tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-native-rooted-stack)

(ert-deftest nelisp-bytecode-native-package-rejects-fingerprint-before-effects ()
  "A dialect/ABI mismatch is rejected before reading artifacts or evaluating .elc."
  (let* ((root (make-temp-file "nelisp-package-fingerprint-" t))
         (elc (expand-file-name "module.elc" root))
         (manifest-path (expand-file-name "package.npkg" root))
         (side-effect 'nelisp-package-fingerprint-side-effect)
         (hash-function (symbol-function
                         'nelisp-bytecode-native-package--sha256-file))
         (hash-count 0)
         (failure nil))
    (unwind-protect
        (progn
          (when (boundp side-effect) (makunbound side-effect))
          (with-temp-file elc
            (insert ";ELC\n(defvar nelisp-package-fingerprint-side-effect nil)\n"
                    "(setq nelisp-package-fingerprint-side-effect t)\n"
                    "(provide 'nelisp-package-fingerprint-fixture)\n"))
          (with-temp-file manifest-path
            (prin1 (list :format nelisp-bytecode-native-package--marker
                         :abi-fingerprint "tampered-fingerprint"
                         :feature 'nelisp-package-fingerprint-fixture
                         :elc "module.elc"
                         :elc-sha256 "not-read"
                         :entries '((:name missing
                                           :artifact missing.neln
                                           :entry missing
                                           :sha256 missing
                                           :minimum 0 :maximum 0)))
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'nelisp-bytecode-native-package--sha256-file)
                     (lambda (path)
                       (setq hash-count (1+ hash-count))
                       (funcall hash-function path))))
            (setq failure
                  (condition-case error-data
                      (progn
                        (nelisp-bytecode-native-package-open manifest-path)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal failure
                         "bytecode-native-package: ABI fingerprint mismatch"))
          (should (= hash-count 0))
          (should-not (boundp side-effect)))
      (when (file-directory-p root) (delete-directory root t))
      (when (boundp side-effect) (makunbound side-effect)))))

(ert-deftest nelisp-bytecode-native-package-reads-without-evaluation ()
  (let ((path (make-temp-file "nelisp-package-reader-" nil ".elc"))
        (variable 'nelisp-package-reader-side-effect))
    (unwind-protect
        (progn
          (makunbound variable)
          (with-temp-file path
            (insert ";ELC\n(setq nelisp-package-reader-side-effect t)\n"
                    "(provide 'nelisp-package-reader-fixture)\n"))
          (should (= (length
                      (nelisp-bytecode-native-package--read-elc-forms path))
                     2))
          (should-not (boundp variable)))
      (when (file-exists-p path) (delete-file path))
      (when (boundp variable) (makunbound variable)))))

(ert-deftest nelisp-bytecode-native-package-rejects-truncated-elc ()
  (let ((path (make-temp-file "nelisp-package-truncated-" nil ".elc"))
        (variable 'nelisp-package-truncated-side-effect))
    (unwind-protect
        (progn
          (makunbound variable)
          (with-temp-file path
            (insert ";ELC\n(setq nelisp-package-truncated-side-effect t)\n"
                    "(provide"))
          (should-error (nelisp-bytecode-native-package--read-elc-forms path))
          (should-not (boundp variable)))
      (when (file-exists-p path) (delete-file path))
      (when (boundp variable) (makunbound variable)))))

(ert-deftest nelisp-bytecode-native-package-skips-exact-ascii-doc-count ()
  (let ((path (make-temp-file "nelisp-package-doc-count-" nil ".elc")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            ;; The count includes the separator: space + "ab". There is no
            ;; newline between the final doc byte and the next form.
            (insert ";ELC\n#@3 ab(provide 'after-doc)\n")
            (write-region (point-min) (point-max) path nil 'silent))
          (should (equal
                   (nelisp-bytecode-native-package--read-elc-forms path)
                   '((provide 'after-doc))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert ";ELC\n#@2 a)(provide 'after-doc)\n")
            (write-region (point-min) (point-max) path nil 'silent))
          (should-error
           (nelisp-bytecode-native-package--read-elc-forms path))
          (dolist (count '("0" "00"))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert ";ELC\n#@" count " (provide 'after-doc)\n")
              (write-region (point-min) (point-max) path nil 'silent))
            (should-error
             (nelisp-bytecode-native-package--read-elc-forms path)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert ";ELC\n#@99 short")
            (write-region (point-min) (point-max) path nil 'silent))
          (should-error
           (nelisp-bytecode-native-package--read-elc-forms path)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-bytecode-native-package-skips-multibyte-doc-byte-count ()
  (let ((path (make-temp-file "nelisp-package-doc-multibyte-" nil ".elc")))
    (unwind-protect
        (progn
          ;; GNU 31.1 counts the separator, nine UTF-8 bytes, and DEL.
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert ";ELC\n#@12 "
                    (unibyte-string #xe6 #x97 #xa5 #xe6 #x9c #xac
                                    #xe8 #xaa #x9e #x1f #x29)
                    "(provide 'after-doc)\n")
            (write-region (point-min) (point-max) path nil 'silent))
          (should (equal
                   (nelisp-bytecode-native-package--read-elc-forms path)
                   '((provide 'after-doc))))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert ";ELC\n#@11 "
                    (unibyte-string #xe6 #x97 #xa5 #xe6 #x9c #xac
                                    #xe8 #xaa #x9e #x1f #x29)
                    "(provide 'after-doc)\n")
            (write-region (point-min) (point-max) path nil 'silent))
          (should-error
           (nelisp-bytecode-native-package--read-elc-forms path)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-bytecode-native-package-entry-encoding-is-injective ()
  (should-not
   (equal (nelisp-bytecode-native-package--native-entry-name 'a-b)
          (nelisp-bytecode-native-package--native-entry-name 'a_b)))
  (should (string-match-p
           "\\`[A-Za-z_][A-Za-z0-9_]*\\'"
           (nelisp-bytecode-native-package--native-entry-name 'a-b))))

(ert-deftest nelisp-bytecode-native-package-sealed-preflight-refuses-mutation ()
  "Package tokens reject function and compiler-result mutation before backend work."
  (let ((api nelisp-bytecode-native-compiler-package-preflight-api))
    (dolist (mutation '(code constants descriptor input-code input-constants
                        ir cyclic-ir frame dialect helper helper-input-accessor
                        helper-ir-accessor helper-frame-accessor))
      (let* ((function (byte-compile '(lambda () 'sealed-value)))
             (sealed (funcall (plist-get api :preflight) function))
             (input (plist-get sealed :input))
             (token (plist-get sealed :token))
             (backend-calls 0)
             (failure nil))
        (unwind-protect
            (progn
              (cond
               ((eq mutation 'code)
                (aset (aref function 1) 0
                      (logxor 1 (aref (aref function 1) 0))))
               ((eq mutation 'constants)
                (aset (aref function 2) 0 'changed-constant))
               ((eq mutation 'descriptor)
                (plist-put input :argument-descriptor 999))
               ((eq mutation 'input-code)
                (plist-put input :code (copy-sequence (plist-get input :code))))
               ((eq mutation 'input-constants)
                (plist-put input :constants (copy-sequence (plist-get input :constants))))
               ((eq mutation 'ir)
                (plist-put input :ir-result 'changed-ir))
               ((eq mutation 'cyclic-ir)
                (let ((cycle (list 'changed-ir)))
                  (setcdr cycle cycle)
                  (plist-put input :ir-result cycle)))
               ((eq mutation 'frame)
                (plist-put input :frame-result 'changed-frame))
               ((eq mutation 'dialect)
                (plist-put input :dialect-evidence 'changed-dialect)))
              (setq failure
                    (if (memq mutation '(helper helper-input-accessor
                                         helper-ir-accessor helper-frame-accessor))
                        (cl-letf (((symbol-function 'nelisp-bytecode-frame-ir-build)
                                   (lambda (&rest _) 'changed-helper))
                                  ((symbol-function 'nelisp-bytecode-compiler-input-native-package-dependencies)
                                   (if (eq mutation 'helper-input-accessor)
                                       (lambda (&rest _) '(nelisp-bytecode-compiler-input-build))
                                     (symbol-function 'nelisp-bytecode-compiler-input-native-package-dependencies)))
                                  ((symbol-function 'nelisp-bytecode-ir-native-package-dependencies)
                                   (if (eq mutation 'helper-ir-accessor)
                                       (lambda (&rest _) '(nelisp-bytecode-ir-validate))
                                     (symbol-function 'nelisp-bytecode-ir-native-package-dependencies)))
                                  ((symbol-function 'nelisp-bytecode-frame-ir-native-package-dependencies)
                                   (if (eq mutation 'helper-frame-accessor)
                                       (lambda (&rest _) '(nelisp-bytecode-frame-ir-build))
                                     (symbol-function 'nelisp-bytecode-frame-ir-native-package-dependencies)))
                                  ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                                   (lambda (&rest _)
                                     (setq backend-calls (1+ backend-calls))
                                     'forged-backend-reached)))
                          (condition-case error-data
                              (progn (funcall (plist-get api :build) token
                                              "unused.neln" "unused")
                                     nil)
                            (error (error-message-string error-data))))
                      (cl-letf (((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                                 (lambda (&rest _)
                                   (setq backend-calls (1+ backend-calls))
                                   'forged-backend-reached)))
                        (condition-case error-data
                            (progn (funcall (plist-get api :build) token
                                            "unused.neln" "unused")
                                   nil)
                          (error (error-message-string error-data))))))
              (should (stringp failure))
              (should (= backend-calls 0)))
          (funcall (plist-get api :discard) token))))))

(ert-deftest nelisp-bytecode-native-package-sealed-preflight-valid-token-builds ()
  "A genuine unchanged sealed token reaches the backend once and is consumed."
  (let* ((api nelisp-bytecode-native-compiler-package-preflight-api)
         (function (byte-compile '(lambda () 'sealed-value)))
         (sealed (funcall (plist-get api :preflight) function))
         (token (plist-get sealed :token))
         (calls 0)
         result)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (actual path entry input)
                       (setq calls (1+ calls))
                       (should (eq actual function))
                       (should (eq input (plist-get sealed :input)))
                       (list :status 'complete :artifact path :entry entry))))
            (setq result (funcall (plist-get api :build) token
                                  "unit.neln" "unit_entry")))
          (should (eq (plist-get result :status) 'complete))
          (should (= calls 1))
          (should-error (funcall (plist-get api :build) token
                                 "unit.neln" "unit_entry")))
      (funcall (plist-get api :discard) token))))

(ert-deftest nelisp-bytecode-native-package-sealed-preflight-large-input-is-uncacheable ()
  "Oversized genuine analysis uses the ordinary compiler path instead of refusing."
  (let* ((api nelisp-bytecode-native-compiler-package-preflight-api)
         (function (byte-compile '(lambda () 'sealed-value)))
         (input-builder (symbol-function 'nelisp-bytecode-compiler-input-build))
         (sealed nil)
         (deep nil)
         (api nelisp-bytecode-native-compiler-package-preflight-api))
    (dotimes (_ 130) (setq deep (list deep)))
    (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
               (lambda (actual)
                 (let ((input (funcall input-builder actual)))
                   (plist-put input :ir-result deep)
                   input))))
      (setq sealed (funcall (plist-get api :preflight) function)))
    (should (eq function (plist-get (plist-get sealed :input) :function)))
    (should (plist-get sealed :uncacheable))
    (should-not (plist-get sealed :token))
    (should (funcall (plist-get api :source-current-p) function
                     (plist-get sealed :source-witness)))
    (aset (aref function 1) 0 (logxor 1 (aref (aref function 1) 0)))
    (should-not (funcall (plist-get api :source-current-p) function
                         (plist-get sealed :source-witness)))))

(ert-deftest nelisp-bytecode-native-package-rejects-unsafe-file-symbol ()
  (should-error
   (nelisp-bytecode-native-package--safe-entry-name 'unsafe/name)))

(ert-deftest nelisp-bytecode-native-package-refuses-raw-v2-cons-before-publication ()
  (let* ((root (make-temp-file "nelisp-package-raw-cons-" t))
         (elc (expand-file-name "module.elc" root))
         (output (expand-file-name "package" root))
         (function (make-byte-code 514 (unibyte-string 1 1 66 135) [] 4))
         (make-directory-function (symbol-function 'make-directory))
         (directory-calls 0)
         (backend-calls 0)
         (failure nil))
    (unwind-protect
        (progn
          (with-temp-file elc
            (insert ";ELC\n")
            (prin1 (list 'defalias (list 'quote 'nelisp-package-cons)
                         function)
                   (current-buffer))
            (insert "\n")
            (prin1 '(provide 'nelisp-package-raw-cons) (current-buffer)))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest arguments)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function arguments)))
                    ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (&rest _arguments)
                       (setq backend-calls (1+ backend-calls))
                       (error "raw-v2 refusal test unexpectedly reached backend"))))
            (setq failure
                  (condition-case error-data
                      (progn
                        (nelisp-bytecode-native-package-compile-elc
                         elc 'nelisp-package-raw-cons
                         '(nelisp-package-cons) output)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal
                   failure
                   "bytecode-native-package: nelisp-package-cons lowers to raw-runtime-v2 and cannot enter a boxed .neln package"))
          (should (= directory-calls 0))
          (should (= backend-calls 0))
          (should-not (file-exists-p output))
          (message "raw-v2 package refusal: make-directory=%d backend=%d"
                   directory-calls backend-calls)
          ;; A forced-off template guard must be observed attempting both
          ;; filesystem mutation and backend work by the negative control.
          (setq directory-calls 0 backend-calls 0 failure nil)
          (cl-letf (((symbol-function
                      'nelisp-bytecode-native-compiler--cons-template-p)
                     (lambda (_input) nil))
                    ((symbol-function 'make-directory)
                     (lambda (&rest arguments)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function arguments)))
                    ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (&rest _arguments)
                       (setq backend-calls (1+ backend-calls))
                       (error "guard-removal negative control reached backend"))))
            (setq failure
                  (condition-case error-data
                      (progn
                        (nelisp-bytecode-native-package-compile-elc
                         elc 'nelisp-package-raw-cons
                         '(nelisp-package-cons) output)
                        nil)
                    (error (error-message-string error-data)))))
          (should (equal failure "guard-removal negative control reached backend"))
          (should (> directory-calls 0))
          (should (= backend-calls 1))
          (should-not (file-exists-p output))
          (message "guard-removal negative control: make-directory=%d backend=%d"
                   directory-calls backend-calls)
          (should-error
           (unless (and (= directory-calls 0) (= backend-calls 0))
             (ert-fail "guard-removal negative control detected package side effects"))
           :type 'ert-test-failed))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-package-refuses-rooted-stack-before-publication ()
  (let* ((root (make-temp-file "nelisp-package-rooted-stack-" t))
         (source (expand-file-name "module.el" root))
         (elc (concat source "c"))
         (output (expand-file-name "package" root))
         (directory-calls 0) (backend-calls 0)
         (make-directory-function (symbol-function 'make-directory)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(defun nelisp-package-rooted (x) (cons nil (car (cdr x))))\n")
            (insert "(provide 'nelisp-package-rooted-feature)\n"))
          (unless (byte-compile-file source) (error "GNU byte compiler failed"))
          (load elc nil t t)
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest args)
                       (setq directory-calls (1+ directory-calls))
                       (apply make-directory-function args)))
                    ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (&rest _) (setq backend-calls (1+ backend-calls))
                       (error "rooted package refusal reached backend")))
                    ((symbol-function 'nelisp-bytecode-native-compiler-rooted-stack-input-p)
                     (lambda (_) t)))
            (should-error
             (nelisp-bytecode-native-package-compile-elc
              elc 'nelisp-package-rooted-feature '(nelisp-package-rooted) output)
             :type 'error))
          (should (= directory-calls 0))
          (should (= backend-calls 0))
          (should-not (file-exists-p output)))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-package-rejects-missing-entry-before-analysis ()
  "A missing last entry is refused before any compiler input is built."
  (let* ((root (make-temp-file "nelisp-package-missing-precheck-" t))
         (source (expand-file-name "module.el" root))
         (elc (concat source "c"))
         (output (expand-file-name "package" root))
         (build-function (symbol-function 'nelisp-bytecode-compiler-input-build))
         (build-count 0))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defun nelisp-package-present () 17)\n"
                    "(provide 'nelisp-package-missing-precheck)\n"))
          (unless (byte-compile-file source)
            (error "GNU byte compiler failed"))
          (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input-build)
                     (lambda (function)
                       (setq build-count (1+ build-count))
                       (funcall build-function function))))
            (should-error
             (nelisp-bytecode-native-package-compile-elc
              elc 'nelisp-package-missing-precheck
              '(nelisp-package-present nelisp-package-missing) output)
             :type 'error))
          (should (= build-count 0))
          (should-not (file-exists-p output)))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-package-cleans-failed-publication ()
  (let* ((root (make-temp-file "nelisp-package-publish-" t))
         (source (expand-file-name "fixture.el" root))
         (elc (concat source "c"))
         (output (expand-file-name "package" root))
         (variable 'nelisp-package-publish-side-effect))
    (unwind-protect
        (progn
          (makunbound variable)
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defvar nelisp-package-publish-side-effect nil)\n"
                    "(setq nelisp-package-publish-side-effect t)\n"
                    "(defun nelisp-package-publish-function (value) value)\n"
                    "(provide 'nelisp-package-publish-fixture)\n"))
          (should (byte-compile-file source))
          (cl-letf (((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (_function artifact entry)
                       (write-region "artifact" nil artifact nil 'silent)
                       (list :status 'complete :input
                             (list :argument-min 1 :argument-max 1))))
                    ((symbol-function 'rename-file)
                     (lambda (&rest _args) (error "simulated publish failure"))))
            (should-error
             (nelisp-bytecode-native-package-compile-elc
              elc 'nelisp-package-publish-fixture
              '(nelisp-package-publish-function) output)))
          (should-not (file-exists-p output))
          (should-not (boundp variable)))
      (when (file-exists-p root) (delete-directory root t))
      (when (boundp variable) (makunbound variable)))))

(ert-deftest nelisp-bytecode-native-package-preserves-racing-publisher-directory ()
  "A loser must not claim or delete a directory published after its precheck."
  (let* ((root (make-temp-file "nelisp-package-race-" t))
         (source (expand-file-name "fixture.el" root))
         (elc (concat source "c"))
         (output (expand-file-name "package" root))
         (winner-manifest (expand-file-name "package.npkg" output))
         (winner-artifact (expand-file-name "winner.neln" output))
         (file-exists-p-function (symbol-function 'file-exists-p))
         (make-directory-function (symbol-function 'make-directory))
         (injected nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defun nelisp-package-race-function (value) value)\n"
                    "(provide 'nelisp-package-race-fixture)\n"))
          (should (byte-compile-file source))
          (cl-letf (((symbol-function 'file-exists-p)
                     (lambda (path)
                       (let ((exists (funcall file-exists-p-function path)))
                         (when (and (not injected) (not exists)
                                    (equal (expand-file-name path) output))
                           (setq injected t)
                           (funcall make-directory-function output)
                           (write-region "winner manifest" nil winner-manifest
                                         nil 'silent)
                           (write-region "winner artifact" nil winner-artifact
                                         nil 'silent))
                         exists)))
                    ((symbol-function 'nelisp-bytecode-native-compiler--build-from-input)
                     (lambda (&rest _args)
                       (error "injected losing compiler failure"))))
          (should-error
           (nelisp-bytecode-native-package-compile-elc
            elc 'nelisp-package-race-fixture
            '(nelisp-package-race-function) output)))
          (should injected)
          (should (file-exists-p winner-manifest))
          (should (file-exists-p winner-artifact))
          (should (equal (with-temp-buffer
                           (insert-file-contents winner-manifest)
                           (buffer-string))
                         "winner manifest"))
          (should (equal (with-temp-buffer
                           (insert-file-contents winner-artifact)
                           (buffer-string))
                         "winner artifact")))
      (when (file-exists-p root) (delete-directory root t)))))

(ert-deftest nelisp-bytecode-native-package-source-free-33-entry-unit ()
  (let* ((root (make-temp-file "nelisp-package-many-host-" t))
         (source (expand-file-name "many.el" root))
         (elc (concat source "c"))
         (directory (expand-file-name "unit" root))
         (duplicate (expand-file-name "duplicate" root))
         (missing (expand-file-name "missing" root))
         (names (mapcar (lambda (index)
                          (intern (format "nelisp-package-test-many-%02d" index)))
                        (number-sequence 0 32)))
         (feature 'nelisp-package-test-many-unit)
         (package nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (dolist (name names)
              (prin1 `(defun ,name (value &optional supplied) (or supplied value))
                     (current-buffer))
              (insert "\n"))
            (prin1 `(provide ',feature) (current-buffer)))
          (byte-compile-file source)
          (delete-file source)
          (should-not (file-exists-p source))
          (let* ((result (nelisp-bytecode-native-package-compile-elc
                          elc feature names directory))
                 (entries (plist-get result :entries)))
            (should (eq (plist-get result :status) 'complete))
            (should (= (length entries) 33))
            (should (eq (plist-get (car (last entries)) :name) (nth 32 names)))
            (should-error (nelisp-bytecode-native-package-compile-elc
                           elc feature (cons (car names) names) duplicate))
            (should-not (file-exists-p duplicate))
            (should-error (nelisp-bytecode-native-package-compile-elc
                           elc feature (append names '(nelisp-package-test-absent)) missing))
            (should-not (file-exists-p missing))
            (setq package (nelisp-bytecode-native-package-open (plist-get result :manifest)))
            (should (= (hash-table-count (aref package 3)) 33))
            (should (= (hash-table-count (aref package 6)) 0))
            (should (featurep feature))
            (nelisp-bytecode-native-package-close package)
            (should-error (nelisp-bytecode-native-package--handle package))
            (setq package nil)))
      (when package (nelisp-bytecode-native-package-close package))
      (dolist (name names) (when (fboundp name) (fmakunbound name)))
      (setq features (delq feature features))
      (delete-directory root t))))

(ert-deftest nelisp-bytecode-native-package-stale-artifact-before-cold-effects ()
  (let* ((root (make-temp-file "nelisp-package-stale-host-" t))
         (source (expand-file-name "stale.el" root))
         (elc (concat source "c"))
         (directory (expand-file-name "unit" root))
         (name 'nelisp-package-test-stale-identity)
         (feature 'nelisp-package-test-stale-unit))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(setq nelisp-package-test-stale-effect t)\n"
                    "(defun nelisp-package-test-stale-identity (value &optional supplied) (or supplied value))\n"
                    "(provide 'nelisp-package-test-stale-unit)\n"))
          (byte-compile-file source)
          (delete-file source)
          (let* ((result (nelisp-bytecode-native-package-compile-elc
                          elc feature (list name) directory))
                 (entry (car (plist-get result :entries)))
                 (artifact (expand-file-name (plist-get entry :artifact) directory)))
            (should (eq (plist-get result :status) 'complete))
            (should-not (boundp 'nelisp-package-test-stale-effect))
            (should-not (fboundp name))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally artifact)
              (goto-char (point-max)) (insert "stale")
              (write-region (point-min) (point-max) artifact nil 'silent))
            (let ((failure
                   (condition-case error-data
                       (progn (nelisp-bytecode-native-package-open
                               (plist-get result :manifest)) nil)
                     (error (error-message-string error-data)))))
              (should (stringp failure))
              (should (string-match-p "native artifact identity mismatch" failure)))
            (should-not (boundp 'nelisp-package-test-stale-effect))
            (should-not (fboundp name))
            (should-not (featurep feature))))
      (when (fboundp name) (fmakunbound name))
      (when (boundp 'nelisp-package-test-stale-effect)
        (makunbound 'nelisp-package-test-stale-effect))
      (setq features (delq feature features))
      (delete-directory root t))))

(provide 'nelisp-bytecode-native-package-test)
;;; nelisp-bytecode-native-package-test.el ends here
