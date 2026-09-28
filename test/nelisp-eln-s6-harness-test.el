;;; nelisp-eln-s6-harness-test.el --- S6 isolated registration, ports, codec -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for the S6 harness redesign pieces:
;;
;;   - isolated registration namespaces: publication, lookup and retraction
;;     stay inside the namespace and never touch the global function cell
;;     or plist, and an invalid namespace is refused;
;;   - slot-identifying port dispatch: a callback whose descriptor word 6
;;     names no installed port is rejected, a known port decodes `raw' /
;;     `lisp' arguments by its own spec and answers a C bool;
;;   - the exact multi-import shapes: a template match proves the shape, one
;;     changed fixed byte does not;
;;   - the codec's artifact-symbol scope: an interned symbol with entirely
;;     empty global state is admitted only inside the scope, a bound or
;;     fbound interned symbol never is, and an authenticated constant symbol
;;     needs no view at all.
;;
;; Native memory (`ptr-read-*', nl-ffi) is unavailable on host Emacs, so
;; descriptor reads are served from a vector through a mocked
;; `nelisp-eln-abi-read-word'; the end-to-end native paths are covered by
;; test/nelisp-eln-s6-measure.sh (S6.17-S6.21).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
;; This file calls `nelisp-eln-callable-import-port-tag' and
;; `nelisp-eln-callable-import--dispatch-port' directly (unmocked), so it
;; must require that module itself: `nelisp-eln-registration' no longer
;; loads it eagerly (S7.7.4 corpus-gate laziness), only on first genuine
;; use inside its own admission/activation path.
(require 'nelisp-eln-callable-import)

;;; Isolated registration namespace

(ert-deftest nelisp-eln-s6-isolated-namespace-publishes-privately ()
  (let* ((name (intern "nelisp-eln-s6-test-isolated-name"))
         (namespace (nelisp-eln-registration-make-isolated-namespace))
         (callable (lambda (x) x))
         (nelisp-eln-registration--load-namespace namespace))
    (fset name #'car)
    (put name 'nelisp-eln-s6-test-prop 'global)
    (unwind-protect
        (progn
          ;; The runtime's own binding does not count as bound here.
          (should-not (nelisp-eln-registration--target-fboundp name))
          (should (nelisp-eln-registration--target-plist-empty-p name))
          (nelisp-eln-registration--publish name callable)
          (nelisp-eln-registration--target-put name 'compiler-macro 'cm)
          (should (eq (nelisp-eln-registration--target-function name) callable))
          (should (eq (nelisp-eln-registration-isolated-function namespace name)
                      callable))
          (should (eq (nelisp-eln-registration-isolated-get
                       namespace name 'compiler-macro)
                      'cm))
          (should-not (nelisp-eln-registration--target-plist-empty-p name))
          ;; Global cells untouched.
          (should (eq (symbol-function name) #'car))
          (should (eq (get name 'nelisp-eln-s6-test-prop) 'global))
          (should-not (get name 'compiler-macro))
          ;; Retraction only for the exact callable, and only in the namespace.
          (nelisp-eln-registration--unpublish name (lambda (x) x))
          (should (eq (nelisp-eln-registration--target-function name) callable))
          (nelisp-eln-registration--unpublish name callable)
          (should-not (nelisp-eln-registration--target-function name))
          (should (eq (symbol-function name) #'car)))
      (fmakunbound name)
      (setplist name nil))))

(ert-deftest nelisp-eln-s6-global-target-without-namespace ()
  "With no namespace, publication is the ordinary global `fset'."
  (let ((name (make-symbol "nelisp-eln-s6-test-global"))
        (callable (lambda (x) x))
        (nelisp-eln-registration--load-namespace nil))
    (should-not (nelisp-eln-registration--target-fboundp name))
    (nelisp-eln-registration--publish name callable)
    (should (eq (symbol-function name) callable))
    (nelisp-eln-registration--unpublish name callable)
    (should-not (symbol-function name))))

(ert-deftest nelisp-eln-s6-invalid-namespace-is-refused ()
  (should-error (nelisp-eln-registration--check-namespace [not a namespace])
                :type 'nelisp-eln-registration-error)
  (should-not (nelisp-eln-registration--check-namespace nil))
  (let ((namespace (nelisp-eln-registration-make-isolated-namespace)))
    (should (eq (nelisp-eln-registration--check-namespace namespace)
                namespace)))
  (should-error (nelisp-eln-registration-isolated-function 'x 'car)
                :type 'wrong-type-argument))

;;; Port dispatch

(defmacro nelisp-eln-s6-test--with-descriptor (words &rest body)
  "Run BODY with `nelisp-eln-abi-read-word' serving WORDS at address 8192.
WORDS is a vector of the seven callback descriptor words."
  (declare (indent 1))
  `(let ((words ,words))
     (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
                (lambda (address offset)
                  (unless (= address 8192)
                    (error "unexpected descriptor address %S" address))
                  (aref words (/ offset 8)))))
       ,@body)))

(ert-deftest nelisp-eln-s6-port-dispatch-rejects-unknown-port ()
  "A callback through a port no import was installed on fails closed."
  (let* ((seen nil)
         (spec (list :slot 1 :convention 'fixed :arity 2
                     :implementation (lambda (&rest a) (setq seen a) t)
                     :arguments '(raw raw) :return 'bool))
         (frame (list :ports
                      (list (cons (nelisp-eln-callable-import-port-tag 0)
                                  spec)))))
    (nelisp-eln-s6-test--with-descriptor
     (vector 7 2 0 0 0 0 (nelisp-eln-callable-import-port-tag 1))
     (should (equal (should-error
                     (nelisp-eln-callable-import--dispatch-port 8192 frame)
                     :type 'nelisp-eln-callable-import-error)
                    (list 'nelisp-eln-callable-import-error 'unknown-port
                          (nelisp-eln-callable-import-port-tag 1)))))
    ;; A stray caller-register word 6 (not a port tag) is equally unknown.
    (nelisp-eln-s6-test--with-descriptor (vector 7 2 0 0 0 0 0)
                                         (should-error (nelisp-eln-callable-import--dispatch-port 8192 frame)
                                                       :type 'nelisp-eln-callable-import-error))
    (should-not seen)))

(ert-deftest nelisp-eln-s6-port-dispatch-uses-the-port-spec ()
  "A known port takes exactly its arity, passes `raw' words through
unchanged, ignores unspecified registers, and answers a C bool."
  (let* ((seen nil)
         (spec (list :slot 1 :convention 'fixed :arity 2
                     :implementation (lambda (&rest a) (setq seen a) t)
                     :arguments '(raw raw) :return 'bool))
         (frame (list :ports
                      (list (cons (nelisp-eln-callable-import-port-tag 0)
                                  spec)))))
    (nelisp-eln-s6-test--with-descriptor
     (vector 7 2 12345 999 0 0 (nelisp-eln-callable-import-port-tag 0))
     (should (equal (nelisp-eln-callable-import--dispatch-port 8192 frame)
                    '(1 . 0))))
    (should (equal seen '(7 2)))))

(ert-deftest nelisp-eln-s6-port-tag-range-is-checked ()
  (should (= (nelisp-eln-callable-import-port-tag 0)
             nelisp-eln-callable-import--port-tag-base))
  (should-error (nelisp-eln-callable-import-port-tag
                 nelisp-eln-callable-import--port-count)
                :type 'nelisp-eln-callable-import-error)
  (should-error (nelisp-eln-callable-import-port-tag -1)
                :type 'nelisp-eln-callable-import-error))

;;; Exact multi-import shapes

(defun nelisp-eln-s6-test--template-bytes (shape)
  "Return SHAPE's template as a unibyte string, holes filled with zero."
  (let* ((template (plist-get (cdr (assq shape
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (bytes (make-string (length template) 0)))
    (dotimes (i (length template))
      (aset bytes i (or (aref template i) 0)))
    (string-to-unibyte bytes)))

(ert-deftest nelisp-eln-s6-multi-import-shape-needs-exact-bytes ()
  (dolist (case '((fixnum-range (1317 1) (7))
                  (bignum (945 1 7) (1 4))
                  (car-eq-constant (7) (1 4))))
    (let* ((bytes (nelisp-eln-s6-test--template-bytes (car case)))
           (analysis (nelisp-eln-tail-code-analyze-multi-import-call
                      bytes #x100000)))
      (should (eq (plist-get analysis :shape) (car case)))
      (should (eq (plist-get analysis :proof) :multi-import-call))
      (should (equal (mapcar (lambda (i) (plist-get i :slot))
                             (plist-get analysis :imports))
                     (nth 1 case)))
      (should (equal (mapcar (lambda (d) (plist-get d :slot))
                             (plist-get analysis :data-relocations))
                     (nth 2 case)))
      ;; Flip one fixed (non-hole) byte: no shape at all.
      (let ((template (plist-get (cdr (assq (car case)
                                            nelisp-eln-tail-code--multi-import-shapes))
                                 :template))
            (tampered (copy-sequence bytes))
            (index 0))
        (while (null (aref template index)) (setq index (1+ index)))
        (aset tampered index (logxor (aref tampered index) 1))
        (should-not (nelisp-eln-tail-code-analyze-multi-import-call
                     tampered #x100000))))))

(ert-deftest nelisp-eln-s6-multi-import-specs-match-the-shapes ()
  "Every shape has a spec whose port slots and constants mirror it."
  (dolist (shape nelisp-eln-tail-code--multi-import-shapes)
    (let ((spec (cdr (assq (car shape)
                           nelisp-eln-native-subr--multi-import-specs))))
      (should spec)
      (should (equal (mapcar #'car (plist-get spec :ports))
                     (plist-get (cdr shape) :imports)))
      (should (equal (mapcar #'car (plist-get spec :constants))
                     (plist-get (cdr shape) :data))))))

;;; Codec: artifact-symbol scope

(defmacro nelisp-eln-s6-test--with-mirror (&rest body)
  "Run BODY with a host stand-in for the runtime's mirror-cell query.
The standalone runtime keeps a name-keyed mirror entry for every symbol
with global state; model that as \"bound or fbound\"."
  `(cl-letf (((symbol-function 'nelisp--symbol-global-cell-p)
              (lambda (symbol) (or (boundp symbol) (fboundp symbol)))))
     ,@body))

(ert-deftest nelisp-eln-s6-empty-interned-symbol-only-inside-scope ()
  (nelisp-eln-s6-test--with-mirror
   (let ((empty (intern "nelisp-eln-s6-test-empty-interned-symbol")))
     (should-not (nelisp-eln-objects--empty-interned-symbol-p empty))
     (should-error (nelisp-eln-objects--preflight (list empty))
                   :type 'nelisp-eln-objects-unsupported)
     (nelisp-eln-objects-call-with-artifact-symbols
      nil
      (lambda ()
        (should (nelisp-eln-objects--empty-interned-symbol-p empty))
        (should (memq empty (aref (nelisp-eln-objects--preflight
                                   (list empty))
                                  3)))))
     ;; The scope is restored on exit.
     (should-not nelisp-eln-objects--admit-empty-interned-symbols)
     (should-not (nelisp-eln-objects--empty-interned-symbol-p empty)))))

(ert-deftest nelisp-eln-s6-bound-interned-symbols-still-rejected ()
  (nelisp-eln-s6-test--with-mirror
   (let ((fbound 'car)
         (bound (intern "nelisp-eln-s6-test-bound-symbol")))
     (set bound 1)
     (unwind-protect
         (nelisp-eln-objects-call-with-artifact-symbols
          nil
          (lambda ()
            (dolist (symbol (list fbound bound))
              (should-not (nelisp-eln-objects--empty-interned-symbol-p symbol))
              (should (equal (car (should-error
                                   (nelisp-eln-objects--preflight
                                    (list symbol))
                                   :type 'nelisp-eln-objects-unsupported))
                             'nelisp-eln-objects-unsupported)))))
       (makunbound bound)))))

(ert-deftest nelisp-eln-s6-constant-symbol-encodes-to-its-word ()
  "An authenticated artifact constant needs no view and encodes to its word."
  (nelisp-eln-objects-call-with-artifact-symbols
   '((car . 123456))
   (lambda ()
     (should-not (aref (nelisp-eln-objects--preflight (list 'car)) 3))
     (should (= (nelisp-eln-objects--encode-word nil 'car) 123456))))
  (should-not nelisp-eln-objects--constant-symbol-words)
  ;; Only real symbol constants are accepted as scope input.
  (should-error (nelisp-eln-objects-call-with-artifact-symbols
                 '((t . 1)) #'ignore)
                :type 'nelisp-eln-objects-unsupported)
  (should-error (nelisp-eln-objects-call-with-artifact-symbols
                 '((car . "x")) #'ignore)
                :type 'nelisp-eln-objects-unsupported))

(provide 'nelisp-eln-s6-harness-test)

;;; nelisp-eln-s6-harness-test.el ends here
