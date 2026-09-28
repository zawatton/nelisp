;;; nelisp-eln-registration-objects-test.el --- metadata type-word view tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration-objects)

(defun nelisp-eln-registration-objects-test--call
    (activation type token &optional stale)
  "Call the subr-view constructor with stubbed native and memory services."
  (let ((allocations 0) (constructors 0) (ordinary-encodes 0)
        (metadata-requires 0) (real-require (symbol-function 'require))
        (written-type nil) (next-address 1000) result)
    (cl-letf (((symbol-function 'nelisp-eln-registration-objects--live-activation)
               #'identity)
              ((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (&rest _) '(nil nil nil 123 nil nil 4)))
              ((symbol-function 'nelisp-eln-native-subr-create)
               (lambda (&rest _) (setq constructors (1+ constructors))
                 (lambda () nil)))
              ((symbol-function 'func-arity) (lambda (&rest _) '(0 . 0)))
              ((symbol-function 'nl-ffi-memory-cstring) (lambda (&rest _) 'cstring))
              ((symbol-function 'nl-ffi-memory-allocate)
               (lambda (&rest _) (setq allocations (1+ allocations)
                                       next-address (+ next-address 100))
                 next-address))
              ((symbol-function 'nl-ffi-memory-address) #'identity)
              ((symbol-function 'nelisp-eln-registration-objects--pointer-word)
               (lambda (address) (+ address 5)))
              ((symbol-function 'nelisp-eln-registration-objects-unit-word)
               (lambda (&rest _) 99))
              ((symbol-function 'ptr-write-u8) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-eln-abi-write-word)
               (lambda (_address offset value)
                 (when (= offset 80) (setq written-type value))))
              ((symbol-function 'nelisp-eln-objects-encode)
               (lambda (_objects object)
                 (setq ordinary-encodes (1+ ordinary-encodes))
                 (if (eq object type) 42 43)))
              ((symbol-function 'nelisp-eln-registration-metadata-type-word)
               (lambda (candidate &optional _index)
                 (if (and (eq candidate token) (not stale)) #x7fff
                   (signal 'nelisp-eln-registration-objects-error
                           '(released-metadata-token)))))
              ((symbol-function 'nelisp-eln-registration-metadata-decode)
               (lambda (candidate word)
                 (if (and (eq candidate token) (= word #x7fff))
                     (aref candidate 0)
                   nil)))
              ((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (eq feature 'nelisp-eln-registration-metadata)
                     (setq metadata-requires (1+ metadata-requires))
                   (apply real-require feature args)))))
      (condition-case err
          (setq result
                (nelisp-eln-registration-objects-subr-view
                 activation "name" "c-name" nil nil 0 type 0 token))
        (error (setq result err)))
      (list result written-type constructors allocations ordinary-encodes
            metadata-requires))))

(defun nelisp-eln-registration-objects-test--activation ()
  (vector nil (vector nil 'handle 'objects) 'open nil nil))

(ert-deftest nelisp-eln-subr-view-writes-authenticated-metadata-type-word ()
  (let* ((source (vector 'stateful-type))
         (token (vector source))
         (result (nelisp-eln-registration-objects-test--call
                  (nelisp-eln-registration-objects-test--activation)
                  source token)))
    (should (= (nth 1 result) #x7fff))
    (should (= (nth 2 result) 1))
    (should (= (nth 3 result) 1))
    (should (= (nth 4 result) 2))
    (should (= (nth 5 result) 1))))

(ert-deftest nelisp-eln-subr-view-rejects-wrong-source-before-allocation ()
  (let* ((source (vector 'type))
         (wrong (vector 'type))
         (token (vector source))
         (result (nelisp-eln-registration-objects-test--call
                  (nelisp-eln-registration-objects-test--activation)
                  wrong token)))
    (should (eq (car (car result)) 'nelisp-eln-registration-objects-error))
    (should (= (nth 2 result) 0))
    (should (= (nth 3 result) 0))
    (should (= (nth 5 result) 1))))

(ert-deftest nelisp-eln-subr-view-rejects-released-token-before-allocation ()
  (let* ((source (vector 'type))
         (token (vector source))
         (result (nelisp-eln-registration-objects-test--call
                  (nelisp-eln-registration-objects-test--activation)
                  source token t)))
    (should (eq (car (car result)) 'nelisp-eln-registration-objects-error))
    (should (= (nth 2 result) 0))
    (should (= (nth 3 result) 0))
    (should (= (nth 5 result) 1))))

(ert-deftest nelisp-eln-subr-view-keeps-ordinary-nil-type-codec-path ()
  (let* ((result (nelisp-eln-registration-objects-test--call
                  (nelisp-eln-registration-objects-test--activation) nil nil)))
    (should (= (nth 1 result) 42))
    (should (= (nth 4 result) 3))
    (should (= (nth 5 result) 0))))

(ert-deftest nelisp-eln-subr-view-cache-separates-metadata-token-identity ()
  (let* ((activation (nelisp-eln-registration-objects-test--activation))
         (source (vector 'type))
         (token-a (vector source))
         (token-b (vector source))
         (first (nelisp-eln-registration-objects-test--call activation source token-a)))
    (should (= (nth 2 first) 1))
    (let ((same (nelisp-eln-registration-objects-test--call activation source token-a)))
      (should (= (nth 2 same) 0))
      (should (= (nth 3 same) 0)))
    (let ((wrong (nelisp-eln-registration-objects-test--call activation (vector 'other) token-a)))
      (should (eq (car (car wrong)) 'nelisp-eln-registration-objects-error))
      (should (= (nth 2 wrong) 0))
      (should (= (nth 3 wrong) 0)))
    ;; Different token identities must not alias merely because vectors are equal.
    (let ((different (nelisp-eln-registration-objects-test--call activation source token-b)))
      (should (= (nth 2 different) 1))
      (should (= (nth 3 different) 1)))))

(provide 'nelisp-eln-registration-objects-test)

;;; nelisp-eln-registration-objects-test.el ends here
