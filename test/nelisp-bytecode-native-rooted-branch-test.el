;;; nelisp-bytecode-native-rooted-branch-test.el --- exact branch admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-branch)
(require 'nelisp-native-load)

(defconst nelisp-bytecode-native-rooted-branch-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-rooted-branch/admit-only-genuine-gnu31-car-cdr-diamond ()
  (unless (equal emacs-version "31.1")
    (ert-skip "Requires pinned GNU Emacs 31.1"))
  (let* ((dir (make-temp-file "gnu-rooted-branch-" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c")) function input plan)
    (unwind-protect
        (progn
          (copy-file (expand-file-name
                      "test/fixtures/native-bytecode/gnu-31.1-rooted-branch.el"
                      nelisp-bytecode-native-rooted-branch-test--root) source t)
          (should (byte-compile-file source))
          (delete-file source)
          (load elc nil t t)
          (setq function (symbol-function 'gnu-rooted-branch)
                input (nelisp-bytecode-compiler-input-build function)
                plan (nelisp-bytecode-native-rooted-branch-plan input))
          (should (eq (plist-get input :status) 'complete))
          (should (equal (plist-get input :code)
                         (unibyte-string 2 131 7 0 1 64 135 65 135)))
          (should (eq (plist-get plan :status) 'complete))
          (should (= (plist-get plan :required-root-count) 5))
          (should (eq (funcall function nil '(head . tail) '(left . right)) 'right))
          (should (eq (funcall function t '(head . tail) '(left . right)) 'head))
          (let ((bad (copy-sequence input)))
            (plist-put bad :code (unibyte-string 2 131 7 0 2 64 135 65 135))
            (should (eq (plist-get (nelisp-bytecode-native-rooted-branch-plan bad)
                                   :status) 'unsupported)))
          (fmakunbound 'gnu-rooted-branch))
      (when (fboundp 'gnu-rooted-branch) (fmakunbound 'gnu-rooted-branch))
      (delete-directory dir t))))

(ert-deftest nelisp-rooted-branch/body-has-explicit-three-import-status-paths ()
  (let ((body (prin1-to-string
               (nelisp-bytecode-native-rooted-branch--body))))
    (dolist (name '("nl_root_pin_slot_v2" "nl_native_car_v2" "nl_native_cdr_v2"))
      (should (string-match-p name body)))
    (should (string-match-p "258" body))
    (should (string-match-p "259" body))
    (should (string-match-p "gateway-status" body))))

(ert-deftest nelisp-rooted-branch/loader-authenticates-exact-typed-import-set ()
  (let* ((imports nil)
         (names '("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2"))
         (exports (list (list :name "nl_native_rooted_branch_probe_v1" :value 16 :size 48
                              :type 'func :abi nelisp-native-load-raw-runtime-abi-v2
                              :arity 4 :params '(u64 u64 u64 u64) :return 'u64)))
         (manifest nil))
    (dolist (name names)
      (let* ((root-slot (equal name "nl_root_pin_slot_v2"))
             (index (if root-slot
                        (nelisp-native-load--raw-v2-conditional-import-index name)
                      (cl-position name nelisp-native-load-bridgeable-symbols :test #'equal))))
        (push (append (list :name name :kind 'func
                            :abi nelisp-native-load-raw-runtime-abi-v2 :index index
                            :address-mode (if root-slot 'conditional-root-slot-v1
                                            'native-bridgeable-v1))
                      '(:arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64))
              imports)))
    (setq manifest
          (list :native-rooted-branch-contract-version
                nelisp-native-load-raw-v2-rooted-branch-contract-version
                :native-rooted-branch-entry "nl_native_rooted_branch_probe_v1"
                :native-rooted-branch-imports names
                :native-rooted-branch-status-base 256
                :native-rooted-branch-contract-hash
                (nelisp-native-load--rooted-branch-contract-hash)
                :native (list :exports exports :imports imports)))
    (should (nelisp-native-load--raw-v2-rooted-branch-contract-valid-p manifest))
    (plist-put (car imports) :address-mode 'resolver)
    (should-not (nelisp-native-load--raw-v2-rooted-branch-contract-valid-p manifest))))

(ert-deftest nelisp-rooted-branch-call/zero-ticket-stops-before-map-and-reserve ()
  (require 'nelisp-bytecode-native-rooted-branch-call)
  (let ((map-calls 0) (reserve-calls 0) (end-calls 0))
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-branch-authenticated-result-p)
               (lambda (_) t))
              ((symbol-function 'nelisp-native-load-running-binary-sha256)
               (lambda () "binary"))
              ((symbol-function 'nelisp-native-load-root-v2-addresses)
               (lambda () '(:environment 1 :begin 10 :reserve 11 :end 12 :slot 13)))
              ((symbol-function 'ptr-call)
               (lambda (address &rest _)
                 (cond ((= address 10) 0)
                       ((= address 11) (setq reserve-calls (1+ reserve-calls)) 1)
                       ((= address 12) (setq end-calls (1+ end-calls)) 1))))
              ((symbol-function 'nelisp-native-load-raw-v2-artifact)
               (lambda (&rest _) (setq map-calls (1+ map-calls))))
              ((symbol-function 'nelisp-native-load-raw-export-address)
               (lambda (&rest _) (ert-fail "entry export reached on failed frame begin"))))
      (should-error
       (nelisp-bytecode-native-rooted-branch-call
        '(:status complete :artifact-path "x" :runtime-binary-sha256 "binary"
          :gateway-imports ("nl_native_car_v2" "nl_native_cdr_v2" "nl_root_pin_slot_v2"))
        nil nil nil)))
    (should (= map-calls 0))
    (should (= reserve-calls 0))
    (should (= end-calls 0))))

(ert-deftest nelisp-rooted-branch/source-body-mutation-stops-before-backend ()
  (require 'nelisp-runtime-reload-abi)
  (let* ((dir (make-temp-file "rooted-branch-source-" t))
         (source (expand-file-name "mutated.el" dir))
         (input (nelisp-bytecode-compiler-input-build
                 (make-byte-code 771 (unibyte-string 2 131 7 0 1 64 135 65 135) [] 4)))
         (plan (nelisp-bytecode-native-rooted-branch-plan input))
         (forms nil) (backend-calls 0) failure)
    (unwind-protect
        (progn
          (should (eq (plist-get plan :status) 'complete))
          (dolist (contract nelisp-runtime-reload-gc-contract)
            (let ((fn (intern (car contract))) args)
              (dotimes (i (cdr contract))
                (setq args (append args (list (intern (format "arg%d" i))))))
              (push (list 'defun fn args 0) forms)))
          ;; The expected AST is a quoted literal; mutate a private copy so
          ;; the negative fixture cannot corrupt the trusted expectation.
          (push (copy-tree (nelisp-bytecode-native-rooted-branch--body)) forms)
          (setq forms (nreverse forms))
          (setf (nth 3 (car (last forms))) 0)
          (with-temp-file source
            (let ((print-gensym t))
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
          (let* ((text (with-temp-buffer
                         (insert-file-contents-literally source)
                         (buffer-string)))
                 (parsed (nelisp-native-load--raw-source-forms source text)))
            (should (equal (nth 3 (car (last parsed))) 0))
            (should-not
             (equal (nelisp-native-load--rooted-stack-normalize-ast
                     (car (last parsed)))
                    (nelisp-native-load--rooted-stack-normalize-ast
                     (nelisp-bytecode-native-rooted-branch--body)))))
          (cl-letf (((symbol-function 'nelisp-aot-compile-to-link-unit)
                     (lambda (&rest _) (setq backend-calls (1+ backend-calls)))))
            (setq failure
                  (condition-case err
                      (progn
                        (nelisp-native-load-raw-v2-compile-file
                         source (concat source ".nelr") nil (make-string 64 ?0) nil nil nil
                         (list :input input :plan plan
                               :entry-ast (nelisp-bytecode-native-rooted-branch--body)))
                        nil)
                    (error (error-message-string err))))
            (should (string-match-p "rooted branch AST/plan mismatch" failure))
            (should (= backend-calls 0))))
      (delete-directory dir t))))

(provide 'nelisp-bytecode-native-rooted-branch-test)
;;; nelisp-bytecode-native-rooted-branch-test.el ends here
