;;; nelisp-native-load-rooted-stack-contract-test.el --- rooted-stack manifest gate -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-bytecode-native-rooted-stack)
(require 'nelisp-bytecode-compiler-input)
(require 'bytecomp)

(defun nelisp-native-load-rooted-stack-contract-test--manifest ()
  (let* ((imports '("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_cons_v2"))
         (manifest (list :native-rooted-stack-contract-version
                         nelisp-native-load-rooted-stack-contract-version
                         :native-rooted-stack-entry "nl_native_stack_probe_v1"
                         :native-rooted-stack-gateway-imports imports
                         :native-rooted-stack-status-base 256
                         :native-rooted-stack-contract-hash
                         (nelisp-native-load--rooted-stack-contract-hash imports)
                         :native (list :imports (mapcar (lambda (name)
                                                         (list :name name :kind 'func :abi
                                                               nelisp-native-load-raw-runtime-abi-v2
                                                               :index 0 :address-mode 'resolver))
                                                       imports)
                                     :exports (list (list :name "nl_native_stack_probe_v1"
                                                          :type 'func :abi nelisp-native-load-raw-runtime-abi-v2
                                                          :arity 4 :params '(u64 u64 u64 u64)
                                                          :return 'u64))))))
    manifest))

(defun nelisp-native-load-rooted-stack-contract-test--preserved-manifest ()
  (let ((path (getenv "NELISP_ROOTED_MANIFEST_FIXTURE")))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (read (current-buffer))))))

(ert-deftest nelisp-native-load-rooted-stack-accepts-and-rejects-preserved-actual-manifest ()
  (skip-unless (nelisp-native-load-rooted-stack-contract-test--preserved-manifest))
  (let* ((manifest (nelisp-native-load-rooted-stack-contract-test--preserved-manifest))
         (reasons (mapcar #'car (nelisp-native-load-raw-v2-check
                                 manifest "nl_native_stack_probe_v1"))))
    (should-not (memq :raw-native-rooted-stack-contract-invalid reasons))
    (should-not (memq :raw-native-rooted-stack-selected-entry reasons))
    (let ((mutated (copy-tree manifest)))
      (setf (plist-get mutated :native-rooted-stack-entry) "nl_native_other_v1")
      (should (nelisp-native-load-rooted-stack-contract-test--invalid-p mutated)))))

(defun nelisp-native-load-rooted-stack-contract-test--invalid-p (manifest &optional entry)
  (let ((reasons (mapcar #'car (nelisp-native-load-raw-v2-check manifest entry))))
    (or (member :raw-native-rooted-stack-contract-invalid reasons)
        (member :raw-native-rooted-stack-selected-entry reasons))))

(ert-deftest nelisp-native-load-rooted-stack-contract-rejects-version-and-hash-mutation ()
  (let ((manifest (nelisp-native-load-rooted-stack-contract-test--manifest)))
    (setf (plist-get manifest :native-rooted-stack-contract-version) "unknown")
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))
    (setq manifest (nelisp-native-load-rooted-stack-contract-test--manifest))
    (setf (plist-get manifest :native-rooted-stack-contract-hash) "forged")
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))))

(ert-deftest nelisp-native-load-rooted-stack-contract-rejects-entry-and-type-mutation ()
  (let ((manifest (nelisp-native-load-rooted-stack-contract-test--manifest)))
    (setf (plist-get manifest :native-rooted-stack-entry) "other_entry")
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))
    (setq manifest (nelisp-native-load-rooted-stack-contract-test--manifest))
    (setf (plist-get (car (plist-get (plist-get manifest :native) :exports)) :params)
          '(u64 u64))
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))
    (setq manifest (nelisp-native-load-rooted-stack-contract-test--manifest))
    (setf (plist-get (car (plist-get (plist-get manifest :native) :exports)) :type) 'object)
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p
             (nelisp-native-load-rooted-stack-contract-test--manifest)
             "wrong-selected-entry"))))

(ert-deftest nelisp-native-load-rooted-stack-contract-rejects-unknown-import-and-legacy-triple ()
  (let ((manifest (nelisp-native-load-rooted-stack-contract-test--manifest)))
    (setf (plist-get manifest :native-rooted-stack-gateway-imports)
          '("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_unknown_v2"))
    (should (nelisp-native-load-rooted-stack-contract-test--invalid-p manifest))
    (setq manifest (nelisp-native-load-rooted-stack-contract-test--manifest))
    (dolist (key '(:native-rooted-stack-contract-version :native-rooted-stack-entry
                   :native-rooted-stack-gateway-imports :native-rooted-stack-status-base
                   :native-rooted-stack-contract-hash))
      (setq manifest (plist-put manifest key nil)))
    (should (member :raw-native-object-op-contract
                    (mapcar #'car (nelisp-native-load-raw-v2-check manifest))))))

(ert-deftest nelisp-native-load-rooted-stack-source-body-mutation-stops-before-backend ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((dir (make-temp-file "rooted-stack-source-" t))
         (compile-path (expand-file-name "fixture.el" dir))
         (elc-path (concat compile-path "c"))
         (raw-path (expand-file-name "mutated.el" dir))
         input plan forms (backend-calls 0))
    (unwind-protect
        (progn
          (with-temp-file compile-path
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(defun nelisp-bytecode-native-rooted-stack-fixture (x) (car x))\n"))
          (unless (byte-compile-file compile-path)
            (error "GNU byte compiler did not make control ELC"))
          (load elc-path nil t t)
          (let ((compiled (symbol-function 'nelisp-bytecode-native-rooted-stack-fixture)))
            (setq input (nelisp-bytecode-compiler-input-build
                         (make-byte-code (aref compiled 0) (aref compiled 1)
                                         (aref compiled 2) (aref compiled 3)))))
          (setq plan (nelisp-bytecode-native-rooted-stack-plan input))
          (should (eq (plist-get plan :status) 'complete))
          (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
                     (lambda (&rest _) (setq backend-calls (1+ backend-calls)))))
            (dolist (mutator
                     (list (lambda (forms)
                             (setf (nth 3 (car (last forms))) 1))
                           (lambda (forms)
                             (setf (car (nth 3 (car (last forms))))
                                   (make-symbol "let")))))
              (setq forms nil)
              (dolist (entry nelisp-runtime-reload-gc-contract)
                (push (list 'defun (intern (car entry))
                            (cl-loop for i below (cdr entry) collect
                                     (intern (format "arg%d" i))) 0) forms))
              (push (list 'defun 'nl_native_stack_probe_v1
                          '(env ticket argument-count root-count)
                          (nelisp-bytecode-native-rooted-stack-body
                           (plist-get plan :operations))) forms)
              (setq forms (nreverse forms))
              (funcall mutator forms)
              (with-temp-file raw-path
                (let ((print-gensym t))
                  (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
              (should-error
               (nelisp-native-load-raw-v2-compile-file
                raw-path (concat raw-path ".nelr") nil (make-string 64 ?0) nil
                (list :input input :plan plan)))
              (should (= backend-calls 0)))))
      (delete-directory dir t))))

(ert-deftest nelisp-native-load-rooted-stack-gc-stub-mutations-stop-before-backend ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((dir (make-temp-file "rooted-stack-gc-source-" t))
         (compile-path (expand-file-name "fixture.el" dir))
         (elc-path (concat compile-path "c"))
         (raw-path (expand-file-name "mutated.el" dir))
         input plan (backend-calls 0))
    (unwind-protect
        (progn
          (with-temp-file compile-path
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(defun nelisp-bytecode-native-rooted-stack-gc-fixture (x) (car x))\n"))
          (unless (byte-compile-file compile-path)
            (error "GNU byte compiler did not make control ELC"))
          (load elc-path nil t t)
          (let ((compiled (symbol-function 'nelisp-bytecode-native-rooted-stack-gc-fixture)))
            (setq input (nelisp-bytecode-compiler-input-build
                         (make-byte-code (aref compiled 0) (aref compiled 1)
                                         (aref compiled 2) (aref compiled 3)))))
          (setq plan (nelisp-bytecode-native-rooted-stack-plan input))
          (should (eq (plist-get plan :status) 'complete))
          (dolist (mutator
                   (list (lambda (forms) (setf (nth 3 (car forms)) 1))
                         (lambda (forms) (setf (cadr (cadr forms))
                                               (cadr (car forms))))
                         (lambda (forms) (setf (car forms) nil))))
            (let (forms)
              (dolist (entry nelisp-runtime-reload-gc-contract)
                (push (list 'defun (intern (car entry))
                            (cl-loop for i below (cdr entry) collect
                                     (intern (format "arg%d" i))) 0) forms))
              (push (list 'defun 'nl_native_stack_probe_v1
                          '(env ticket argument-count root-count)
                          (nelisp-bytecode-native-rooted-stack-body
                           (plist-get plan :operations))) forms)
              (setq forms (nreverse forms))
              (funcall mutator forms)
              (setq forms (delq nil forms))
              (with-temp-file raw-path
                (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
              (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
                         (lambda (&rest _) (setq backend-calls (1+ backend-calls)))))
                (should-error
                 (nelisp-native-load-raw-v2-compile-file
                  raw-path (concat raw-path ".nelr") nil (make-string 64 ?0) nil
                  (list :input input :plan plan)))
                (should (= backend-calls 0))))))
      (delete-directory dir t))))

(defun nelisp-native-load-rooted-stack-contract-test--find-import-mapcars (node)
  (if (consp node)
      (append (if (and (eq (car node) 'mapcar)
                       (consp (cadr node))
                       (eq (caadr node) 'lambda)
                       (equal (cadadr node) '(o)))
                  (list node))
              (nelisp-native-load-rooted-stack-contract-test--find-import-mapcars
               (car node))
              (nelisp-native-load-rooted-stack-contract-test--find-import-mapcars
               (cdr node)))
    nil))

(defun nelisp-native-load-rooted-stack-contract-test--find-import-planned (node)
  (if (consp node)
      (if (and (eq (car node) 'planned) (= (length node) 2))
          (list node)
        (append
         (nelisp-native-load-rooted-stack-contract-test--find-import-planned (car node))
         (nelisp-native-load-rooted-stack-contract-test--find-import-planned (cdr node))))
    nil))

(defun nelisp-native-load-rooted-stack-contract-test--find-import-when (node)
  (if (consp node)
      (if (and (eq (car node) 'when)
               (eq (cadr node) 'rooted-stack-spec)
               (eq (car-safe (nth 2 node)) 'let)
               (nelisp-native-load-rooted-stack-contract-test--find-import-planned
                (nth 2 node)))
          (list node)
        (append
         (nelisp-native-load-rooted-stack-contract-test--find-import-when (car node))
         (nelisp-native-load-rooted-stack-contract-test--find-import-when (cdr node))))
    nil))

(defun nelisp-native-load-rooted-stack-contract-test--find-index-lets (node)
  (if (consp node)
      (if (and (eq (car node) 'let)
               (equal (cadr node) '((index 0))))
          (list node)
        (append
         (nelisp-native-load-rooted-stack-contract-test--find-index-lets (car node))
         (nelisp-native-load-rooted-stack-contract-test--find-index-lets (cdr node))))
    nil))

(defun nelisp-native-load-rooted-stack-contract-test--find-rooted-preflight (node)
  (if (consp node)
      (if (and (eq (car node) 'when)
               (eq (cadr node) 'rooted-stack-spec)
               (equal (nth 2 node) '(require 'nelisp-bytecode-native-rooted-stack)))
          (list node)
        (append
         (nelisp-native-load-rooted-stack-contract-test--find-rooted-preflight (car node))
         (nelisp-native-load-rooted-stack-contract-test--find-rooted-preflight (cdr node))))
    nil))

(defun nelisp-native-load-rooted-stack-contract-test--contains-call (tree function)
  (and (consp tree)
       (or (eq (car tree) function)
           (nelisp-native-load-rooted-stack-contract-test--contains-call (car tree) function)
           (nelisp-native-load-rooted-stack-contract-test--contains-call (cdr tree) function))))

(defun nelisp-native-load-rooted-stack-contract-test--genuine-import-plan ()
  (let* ((dir (make-temp-file "rooted-import-plan-" t))
         (file (expand-file-name "fixture.el" dir))
         (elc (concat file "c")) input)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (insert "(defun nelisp-native-load-rooted-stack-imports-fixture (x)\n")
            (insert "  (cons (car x) (cons (cdr x) (car x))))\n"))
          (unless (byte-compile-file file)
            (error "GNU byte compiler did not produce import fixture"))
          (load elc nil t t)
          (let ((compiled (symbol-function
                           'nelisp-native-load-rooted-stack-imports-fixture)))
            (setq input
                  (nelisp-bytecode-compiler-input-build
                   (make-byte-code (aref compiled 0) (aref compiled 1)
                                   (aref compiled 2) (aref compiled 3)))))
          input)
      (when (fboundp 'nelisp-native-load-rooted-stack-imports-fixture)
        (fmakunbound 'nelisp-native-load-rooted-stack-imports-fixture))
      (delete-directory dir t))))

(ert-deftest nelisp-native-load-rooted-stack-import-mapcar-has-two-arguments ()
  (let* ((loaded (or (symbol-file 'nelisp-native-load-raw-v2-compile-file 'defun)
                     (locate-library "nelisp-native-load")))
         (source (if (and loaded (string-suffix-p ".elc" loaded))
                     (concat (file-name-sans-extension loaded) ".el")
                   loaded))
         (function-form nil)
         (mapcar-calls nil)
         (planned-bindings nil)
         (when-forms nil)
         (index-lets nil)
         (preflights nil))
    (should (and source (file-readable-p source)))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (cl-labels ((find-definition (node)
                    (when (consp node)
                      (when (and (eq (car node) 'defun)
                                 (eq (cadr node) 'nelisp-native-load-raw-v2-compile-file))
                        (setq function-form node))
                      (find-definition (car node)) (find-definition (cdr node)))))
        (condition-case nil
            (while (not function-form) (find-definition (read (current-buffer))))
          (end-of-file nil))))
    (should function-form)
    (setq mapcar-calls
          (nelisp-native-load-rooted-stack-contract-test--find-import-mapcars
           function-form))
    (setq planned-bindings
          (nelisp-native-load-rooted-stack-contract-test--find-import-planned
           function-form))
    (setq when-forms
          (nelisp-native-load-rooted-stack-contract-test--find-import-when
           function-form))
    (setq index-lets
          (nelisp-native-load-rooted-stack-contract-test--find-index-lets function-form))
    (setq preflights
          (nelisp-native-load-rooted-stack-contract-test--find-rooted-preflight function-form))
    (should (= (length mapcar-calls) 1))
    (should (= (length planned-bindings) 1))
    (should (= (length when-forms) 1))
    (should (= (length index-lets) 1))
    (should (= (length preflights) 1))
    (let* ((call (car mapcar-calls))
           (mapper (cadr call))
           (planned (car planned-bindings))
           (sort-call (cadr planned))
           (dedupe-call (cadr sort-call))
           (when-form (car when-forms))
           (let-form (nth 2 when-form))
           (bindings (cadr let-form))
           (comparison (nth 2 let-form))
           (input (nelisp-native-load-rooted-stack-contract-test--genuine-import-plan))
           (plan (nelisp-bytecode-native-rooted-stack-plan input)))
      (should (= (length call) 3))
      (should (= (length mapper) 3))
      (should (= (length planned) 2))
      (should (eq (car sort-call) 'sort))
      (should (= (length sort-call) 3))
      (should (eq (car dedupe-call) 'delete-dups))
      (should (= (length dedupe-call) 2))
      (should (eq (car let-form) 'let))
      (should (= (length when-form) 3))
      (should (= (length let-form) 3))
      (should-not (nelisp-native-load-rooted-stack-contract-test--find-index-lets
                   when-form))
      (let* ((preflight (car preflights))
             (preflight-let (nth 3 preflight)))
        (should (= (length preflight) 4))
        (should (eq (car preflight-let) 'let*))
        (should (= (length preflight-let) 3))
        (should-not (nelisp-native-load-rooted-stack-contract-test--contains-call
                     preflight 'nelisp-aot-compile-to-link-unit))
        (should (nelisp-native-load-rooted-stack-contract-test--contains-call
                 function-form 'nelisp-aot-compile-to-link-unit)))
      (should (= (length bindings) 2))
      (should (eq (car comparison) 'unless))
      (should (equal (cadr comparison) '(equal actual planned)))
      (should (equal (nth 2 call)
                     '(plist-get (plist-get rooted-stack-spec :plan) :operations)))
      (should (equal (nth 2 mapper)
                     '(format "nl_native_%s_v2" (plist-get o :operation))))
      (should (eq (plist-get plan :status) 'complete))
      (let* ((expected '("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_cons_v2"))
             (spec (list :plan plan))
             (equal-env `((imports . ,expected) (rooted-stack-spec . ,spec)))
             (wrong-env `((imports . ,(cons "bogus" expected))
                          (rooted-stack-spec . ,spec))))
        (should (null (eval when-form equal-env)))
        (should-error (eval when-form wrong-env)
                      :type 'error)))))

(provide 'nelisp-native-load-rooted-stack-contract-test)
;;; nelisp-native-load-rooted-stack-contract-test.el ends here
