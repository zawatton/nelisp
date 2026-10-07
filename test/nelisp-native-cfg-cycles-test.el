;;; nelisp-native-cfg-cycles-test.el --- Cyclic lowering qualification -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-bytecode-native-rooted-cfg-constructor-contract)
(require 'nelisp-aot-compiler)
(load (expand-file-name "support/native-cfg-cycles-fixtures.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest native-cfg-cycles/genuine-fixtures ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (fixture (native-cfg-cycles-fixtures))
    (let* ((fn (nth 1 fixture)) (input (nelisp-bytecode-compiler-input-build fn))
           (plan (nelisp-bytecode-native-rooted-cfg-plan input))
           (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "cycle_probe")))
      (should (eq (plist-get plan :status) 'complete))
      (when (eq (car fixture) 'irreducible)
        (let ((topology (nelisp-bytecode-native-rooted-cfg-topology-check
                         (plist-get input :frame-result))))
          (should (member '(4 8 12) (plist-get topology :sccs)))
          ;; Entry reaches two distinct members before entering the SCC.
          (should (equal (mapcar (lambda (edge) (plist-get edge :target))
                                (append (plist-get (aref (plist-get (plist-get input :frame-result)
                                                                      :blocks) 0) :successors) nil))
                         '(4 8)))))
      (should (eq (plist-get emitted :status) 'complete))
      (should (eq (car (nth 2 (nth 3 (plist-get emitted :form)))) 'cfg))
      (let* ((canonical (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                         plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
             (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan canonical)))
        (should (nelisp-bytecode-native-rooted-cfg-contract-valid-p contract))
        (should (if (plist-get plan :funcall-version)
                    (nelisp-bytecode-native-rooted-cfg-contract-f1-p contract)
                  (nelisp-bytecode-native-rooted-cfg-contract-constructor-p contract))))
      (unless (memq (car fixture) '(closed swap))
        (dolist (args (nth 2 fixture)) (should (or (apply fn args) t))))
      (should (plist-get (nelisp-aot-compile-to-link-unit (plist-get emitted :form)) :text)))))

(ert-deftest native-cfg-cycles/closed-has-no-join ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (nth 1 (assq 'closed (native-cfg-cycles-fixtures)))))
         (analysis (nelisp-bytecode-native-rooted-cfg-postdom-analyze input)))
    (should (eq (plist-get analysis :status) 'complete))
    (should (equal (plist-get analysis :postdominators) '((0 0))))
    (should-not (plist-get analysis :nearest-joins))))

(ert-deftest native-cfg-cycles/budget-and-malformed-edge ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (nth 1 (assq 'entry (native-cfg-cycles-fixtures)))))
         (frame (plist-get input :frame-result)))
    (let ((nelisp-bytecode-native-rooted-cfg-max-analysis-steps 0))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check frame) :status) 'unsupported)))
    (setq frame (copy-tree frame t))
    (plist-put (aref (plist-get (aref (plist-get frame :blocks) 0) :successors) 0) :target 999)
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check frame) :status) 'unsupported))))
(defun native-cfg-cycles--copy-result (parallel)
  (let* ((slots (vector nil [1 2 3 4] [5 6 7 8] [0 0 0 0] [0 0 0 0]))
         (body (if parallel
                   (nelisp-native-funcall-v2-copy-form
                    '(1 2) '(3 4) (nelisp-native-funcall-v2-copy-form '(3 4) '(2 1) 0))
                 (nelisp-native-funcall-v2-copy-form '(1 2) '(2 1) 0))))
    (cl-letf (((symbol-function 'extern-call) (lambda (_name _env _ticket root &rest _) root))
              ((symbol-function 'ptr-read-u64) (lambda (root offset) (aref (aref slots root) (/ offset 8))))
              ((symbol-function 'ptr-write-u64)
               (lambda (root offset value) (aset (aref slots root) (/ offset 8) value))))
      (eval `(let ((env 0) (ticket 1) (nl_root_pin_slot_v2 nil)) ,body) t))
    (list (aref slots 1) (aref slots 2))))

(ert-deftest native-cfg-cycles/parallel-copies-survive-swaps ()
  (let ((expected '([5 6 7 8] [1 2 3 4])))
    (should (equal (native-cfg-cycles--copy-result t) expected))
    ;; Same slot-copy ABI with sequential assignments is the defect control.
    (should-not (equal (native-cfg-cycles--copy-result nil) expected))))

(ert-deftest native-cfg-cycles/coincident-branch-edges ()
  (skip-unless (equal emacs-version "31.1"))
  ;; Both branch arms enter the same loop body; one edge block must suffice.
  (let* ((fn (make-byte-code 257 (unibyte-string 137 131 4 0 137 131 12 0 65 130 0 0 135) [] 2))
         (plan (nelisp-bytecode-native-rooted-cfg-plan (nelisp-bytecode-compiler-input-build fn)))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "coincident")))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'complete))
    (should (plist-get (nelisp-aot-compile-to-link-unit (plist-get emitted :form)) :text))))

(provide 'nelisp-native-cfg-cycles-test)
