;;; nelisp-eln-callable-import-test.el --- unary import bridge tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-callable-import)

(defvar nelisp-eln-callable-import-test--descriptor-words nil)
(defvar nelisp-eln-callable-import-test--invoke-callback nil)
(defvar nelisp-eln-callable-import-test--pop-fails nil)
(defvar nelisp-eln-callable-import-test--argument-token nil)
(defvar nelisp-eln-callable-import-test--result-token nil)
(defvar nelisp-eln-callable-import-test--release-count 0)
(defvar nelisp-eln-callable-import-test--last-release-count nil)
(defvar nelisp-eln-callable-import-test--last-status nil)

(defun nelisp-eln-callable-import-test--run
    (invoke extra &optional fail pop-fail old-status)
  (let ((nelisp-eln-callable-import-test--descriptor-words
         (cons 17 (append extra (make-list (- 6 (length extra)) 0))))
        (nelisp-eln-callable-import-test--invoke-callback invoke)
        (nelisp-eln-callable-import-test--pop-fails pop-fail)
        (nelisp-eln-callable-import-test--release-count 0)
        (nelisp-eln-callable-import-test--argument-token nil)
        (nelisp-eln-callable-import-test--result-token nil)
        (status-value (or old-status 0))
        (nelisp-eln-callable-import--frames nil)
        (units 0) (activations 0) (calls 0) (releases 0)
        (callback-result nil))
    (cl-letf
        (((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
          (lambda (cap) cap))
         ((symbol-function 'nelisp-eln-objects-create)
          (lambda () (setq units (1+ units)) (vector units)))
         ((symbol-function 'nelisp-eln-objects-encode)
          (lambda (_unit value) (if (= value 5) 17 77)))
         ((symbol-function 'nelisp-eln-objects-activation-acquire)
          (lambda (_unit)
            (setq activations (1+ activations))
            (let ((token (vector activations)))
              (if (= activations 1)
                  (setq nelisp-eln-callable-import-test--argument-token token)
                (setq nelisp-eln-callable-import-test--result-token token))
              token)))
         ((symbol-function 'nelisp-eln-objects-activation-decode)
          (lambda (token word)
            (cond ((eq token nelisp-eln-callable-import-test--argument-token)
                   (if (= word 17) 5 :bad-argument))
                  ((eq token nelisp-eln-callable-import-test--result-token)
                   (if (= word 77) 9 :bad-result))
                  (t :bad-token))))
         ((symbol-function 'nelisp-eln-raw-call-context-create) (lambda () 'ctx))
         ((symbol-function 'nelisp-eln-raw-call-context-release)
          (lambda (_context) t))
         ((symbol-function 'nelisp-native-load--symbol-addr)
          (lambda (name)
            (cond ((equal name "nelisp_eln_callback_context_pop") 9001)
                  ((equal name "nl_eln_callback7_context") 9002)
                  ((equal name "wf_bytecode_call_gateway") 9003)
                  (t 9000))))
         ((symbol-function 'nelisp--native-symbol-addr)
          (lambda (_index) 9000))
         ((symbol-function 'nelisp--native-env) (lambda () 'env))
         ((symbol-function 'nelisp-native-load--pin-begin)
          (lambda (_env) 'pin))
         ((symbol-function 'nelisp-native-load--pin-reserve)
          (lambda (_env _marker) 9100))
         ((symbol-function 'nelisp-native-load-box)
          (lambda (_slot _function) t))
         ((symbol-function 'nelisp-native-load--pin-end)
          (lambda (_env _marker) t))
         ((symbol-function 'ptr-read-u64) (lambda (_address _offset) status-value))
         ((symbol-function 'ptr-write-u64)
          (lambda (_address _offset value) (setq status-value value) t))
         ((symbol-function 'ptr-call)
          (lambda (address &rest _args)
            (setq calls (1+ calls))
            (if (= address 9001)
                (if nelisp-eln-callable-import-test--pop-fails 0 1)
              42)))
         ((symbol-function 'nelisp-eln-objects-release)
          (lambda (_unit)
            (setq releases (1+ releases)
                  nelisp-eln-callable-import-test--release-count
                  (1+ nelisp-eln-callable-import-test--release-count)) t))
         ((symbol-function 'nelisp-eln-objects-activation-release)
          (lambda (_token)
            (setq releases (1+ releases)
                  nelisp-eln-callable-import-test--release-count
                  (1+ nelisp-eln-callable-import-test--release-count)) t))
         ((symbol-function 'nelisp-eln-abi-read-word)
          (lambda (_address offset)
            (nth (/ offset 8) nelisp-eln-callable-import-test--descriptor-words)))
         ((symbol-function 'nelisp-eln-raw-call-word)
          (lambda (_context _address _args)
            (if nelisp-eln-callable-import-test--invoke-callback
                (progn
                  (setq callback-result
                        (nelisp-eln-callable-import--dispatch 8192))
                  (+ (car callback-result) (ash (cdr callback-result) 32)))
              17))))
      (let (answer)
        (unwind-protect
            (setq answer
                  (nelisp-eln-callable-import--call-unary
                   '(capability)
                   (cond ((eq fail 'quit)
                          (lambda (_x) (signal 'quit nil)))
                         (fail (lambda (_x) (car 1)))
                         (t (lambda (x) (+ x 4))))
                   5))
          (setq nelisp-eln-callable-import-test--last-status status-value
                nelisp-eln-callable-import-test--last-release-count releases))
        answer))))

(ert-deftest nelisp-eln-callable-import-fast-path-decodes-argument-lease ()
  (should (= (nelisp-eln-callable-import-test--run nil nil) 5)))

(ert-deftest nelisp-eln-callable-import-restores-idle-and-nested-status ()
  (dolist (old-status '(1 -1))
    (should (= (nelisp-eln-callable-import-test--run
                nil nil nil nil old-status) 5))
    (should (= nelisp-eln-callable-import-test--last-status old-status))))

(ert-deftest nelisp-eln-callable-import-dispatches-one-argument-and-result-lease ()
  (should (= (nelisp-eln-callable-import-test--run t nil) 9)))

(ert-deftest nelisp-eln-callable-import-rejects-unspecified-arguments ()
  (should-error (nelisp-eln-callable-import-test--run t '(1))
                :type 'nelisp-eln-callable-import-error))

(ert-deftest nelisp-eln-callable-import-re-signals-callback-condition ()
  ;; The callback's condition must survive the callback ABI's zero pair.
  (should-error (nelisp-eln-callable-import-test--run t nil t)
                :type 'wrong-type-argument))

(ert-deftest nelisp-eln-callable-import-re-signals-callback-quit ()
  (should (equal '(quit)
              (condition-case condition
                  (nelisp-eln-callable-import-test--run t nil 'quit)
                (quit condition)))))

(ert-deftest nelisp-eln-callable-import-rejects-symbol-as-captured-function ()
  (should-error
   (nelisp-eln-callable-import--call-unary '(capability) '1+ 5)
   :type 'nelisp-eln-callable-import-error))

(ert-deftest nelisp-eln-callable-import-retains-resources-on-pop-failure ()
  (setq nelisp-eln-callable-import-test--release-count 0)
  (setq nelisp-eln-callable-import-test--last-release-count nil)
  (should-error (nelisp-eln-callable-import-test--run nil nil nil t)
                :type 'nelisp-eln-callable-import-error)
  (should nelisp-eln-callable-import--pending-cleanups)
  (should (= nelisp-eln-callable-import-test--last-release-count 0))
  (should-error
   (nelisp-eln-callable-import--call-unary '(capability) (lambda (x) x) 5)
   :type 'nelisp-eln-callable-import-error)
  (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
             (lambda (_name) 9001))
            ((symbol-function 'ptr-call) (lambda (&rest _args) 1))
            ((symbol-function 'nelisp-native-load--pin-end)
             (lambda (&rest _args) t))
            ((symbol-function 'ptr-write-u64) (lambda (&rest _args) t))
            ((symbol-function 'nelisp-eln-objects-activation-release)
             (lambda (_token) t))
            ((symbol-function 'nelisp-eln-objects-release)
             (lambda (_unit) t))
            ((symbol-function 'nelisp-eln-raw-call-context-release)
             (lambda (_context) t)))
    (should (nelisp-eln-callable-import-retry-cleanup)))
  (should-not nelisp-eln-callable-import--pending-cleanups))

;; --- MANY (argc, argv) convention: descriptor-gated N-ary decode path ---
;;
;; The unary harness above pins the callback ABI's word[0] to a single
;; Lisp argument.  This harness instead places (ARGC ARGV-ADDRESS 0 0 0 0
;; 0) in the same seven-word callback slot and answers a *second*
;; `nelisp-eln-abi-read-word' address (ARGV-ADDRESS) with the stack-built
;; argument array, exactly as `nelisp-eln-callable-import--args' decodes a
;; genuine GNU MANY call such as vendor `zerop's call into `Feqlsign'.

(defvar nelisp-eln-callable-import-test--argv-address 20000)
(defvar nelisp-eln-callable-import-test--argv-words nil)
(defvar nelisp-eln-callable-import-test--argv-reads 0)

(defun nelisp-eln-callable-import-test--run-many
    (implementation argc word0 word1 &optional arity)
  "Drive `--call-unary' with the MANY convention and a 2-slot ARGV array.
ARGC is the callback's native argument count word; WORD0/WORD1 are the raw
words the native array would hold.  ARITY (default 2) is the descriptor
arity passed to `--call-unary'.  Never reads ARGV past ARGC words."
  (setq nelisp-eln-callable-import-test--argv-reads 0)
  (let* ((arity (or arity 2))
         (argv-address nelisp-eln-callable-import-test--argv-address)
         (nelisp-eln-callable-import-test--descriptor-words
          (list argc argv-address 0 0 0 0 0))
         (nelisp-eln-callable-import-test--argv-words (list word0 word1))
         (nelisp-eln-callable-import--frames nil)
         (units 0) (activations 0) (releases 0)
         (argument-token nil) (result-token nil))
    (cl-letf
        (((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
          (lambda (cap) cap))
         ((symbol-function 'nelisp-eln-objects-create)
          (lambda () (setq units (1+ units)) (vector units)))
         ((symbol-function 'nelisp-eln-objects-encode)
          (lambda (_unit value) (if (eq value :outer-argument) 5000
                                  (if (eq value t) 6000 6001))))
         ((symbol-function 'nelisp-eln-objects-activation-acquire)
          (lambda (_unit)
            (setq activations (1+ activations))
            (let ((token (vector activations)))
              (if (= activations 1) (setq argument-token token)
                (setq result-token token))
              token)))
         ((symbol-function 'nelisp-eln-objects-activation-decode)
          (lambda (token word)
            (cond ((eq token argument-token)
                   (cond ((= word 5000) :outer-argument)
                         ((= word 101) 0) ((= word 202) 0) ((= word 303) 7)
                         (t (error "unexpected argument word %s" word))))
                  ((eq token result-token) (if (= word 6000) t nil))
                  (t :bad-token))))
         ((symbol-function 'nelisp-eln-raw-call-context-create) (lambda () 'ctx))
         ((symbol-function 'nelisp-eln-raw-call-context-release) (lambda (_c) t))
         ((symbol-function 'nelisp-native-load--symbol-addr)
          (lambda (name)
            (cond ((equal name "nelisp_eln_callback_context_pop") 9001)
                  ((equal name "nl_eln_callback7_context") 9002)
                  ((equal name "wf_bytecode_call_gateway") 9003)
                  (t 9000))))
         ((symbol-function 'nelisp--native-env) (lambda () 'env))
         ((symbol-function 'nelisp-native-load--pin-begin) (lambda (_e) 'pin))
         ((symbol-function 'nelisp-native-load--pin-reserve) (lambda (_e _m) 9100))
         ((symbol-function 'nelisp-native-load-box) (lambda (_s _f) t))
         ((symbol-function 'nelisp-native-load--pin-end) (lambda (_e _m) t))
         ((symbol-function 'ptr-read-u64) (lambda (_a _o) 0))
         ((symbol-function 'ptr-write-u64) (lambda (_a _o _v) t))
         ((symbol-function 'ptr-call)
          (lambda (address &rest _args) (if (= address 9001) 1 42)))
         ((symbol-function 'nelisp-eln-objects-release)
          (lambda (_unit) (setq releases (1+ releases)) t))
         ((symbol-function 'nelisp-eln-objects-activation-release)
          (lambda (_token) (setq releases (1+ releases)) t))
         ((symbol-function 'nelisp-eln-abi-read-word)
          (lambda (address offset)
            (if (eq address argv-address)
                (progn
                  (setq nelisp-eln-callable-import-test--argv-reads
                        (1+ nelisp-eln-callable-import-test--argv-reads))
                  (nth (/ offset 8) nelisp-eln-callable-import-test--argv-words))
              (nth (/ offset 8)
                   nelisp-eln-callable-import-test--descriptor-words))))
         ((symbol-function 'nelisp-eln-raw-call-word)
          (lambda (_context _address _args)
            (let ((callback-result (nelisp-eln-callable-import--dispatch 8192)))
              (+ (car callback-result) (ash (cdr callback-result) 32))))))
      (prog1
          (nelisp-eln-callable-import--call-unary
           '(capability) implementation :outer-argument 'many arity)
        (setq nelisp-eln-callable-import-test--release-count releases)))))

(ert-deftest nelisp-eln-callable-import-many-decodes-two-word-array ()
  "argc 2, two array words, exact `=' result; ARGV read exactly ARITY times."
  (should (eq (nelisp-eln-callable-import-test--run-many (lambda (a b) (= a b)) 2 101 202) t))
  (should (= nelisp-eln-callable-import-test--argv-reads 2))
  (should (eq (nelisp-eln-callable-import-test--run-many (lambda (a b) (= a b)) 2 101 303) nil)))

(ert-deftest nelisp-eln-callable-import-many-signals-exact-error ()
  "A real `wrong-type-argument' from the applied builtin survives intact."
  (should-error (nelisp-eln-callable-import-test--run-many
                 (lambda (a _b) (car a)) 2 101 202)
                :type 'wrong-type-argument))

(ert-deftest nelisp-eln-callable-import-many-signals-quit ()
  (should (equal '(quit)
              (condition-case condition
                  (nelisp-eln-callable-import-test--run-many
                   (lambda (_a _b) (signal 'quit nil)) 2 101 202)
                (quit condition)))))

(ert-deftest nelisp-eln-callable-import-many-rejects-argc-above-arity ()
  "ARGC greater than ARITY is rejected before any ARGV word is read."
  (should-error (nelisp-eln-callable-import-test--run-many (lambda (a b) (= a b)) 3 101 202)
                :type 'nelisp-eln-callable-import-error)
  (should (= nelisp-eln-callable-import-test--argv-reads 0)))

(ert-deftest nelisp-eln-callable-import-many-rejects-argc-below-arity ()
  "ARGC less than ARITY is also rejected; the array is never over-read."
  (should-error (nelisp-eln-callable-import-test--run-many (lambda (a b) (= a b)) 1 101 202)
                :type 'nelisp-eln-callable-import-error)
  (should (= nelisp-eln-callable-import-test--argv-reads 0)))

(ert-deftest nelisp-eln-callable-import-many-releases-owners-exactly-once ()
  (should (eq (nelisp-eln-callable-import-test--run-many (lambda (a b) (= a b)) 2 101 202) t))
  (should-not nelisp-eln-callable-import--pending-cleanups)
  (let ((releases-after-success
         nelisp-eln-callable-import-test--release-count))
    (should (> releases-after-success 0))
    ;; Nothing is pending, so a retry must not touch the owner again.
    (should (nelisp-eln-callable-import-retry-cleanup))
    (should (= nelisp-eln-callable-import-test--release-count
               releases-after-success))))

(provide 'nelisp-eln-callable-import-test)

;;; nelisp-eln-callable-import-test.el ends here
