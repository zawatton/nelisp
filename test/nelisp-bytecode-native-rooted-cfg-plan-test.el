;;; nelisp-bytecode-native-rooted-cfg-plan-test.el --- bounded rooted CFG planner -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)

(defvar nelisp-rooted-cfg-unbound-variable nil)

(defun nelisp-bytecode-native-rooted-cfg-test--two-diamonds ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (condition-a left-a right-a condition-b left-b right-b)
      (cons (car (if condition-a left-a right-a))
            (cdr (if condition-b left-b right-b)))))))

(defun nelisp-bytecode-native-rooted-cfg-test--two-phis-at-one-join ()
  ;; A genuine GNU 31.1 byte-code function: both arms push two constants,
  ;; branch to one shared CONS, and return its result.
  (nelisp-bytecode-compiler-input-build
   (make-byte-code 1542 (unibyte-string 1 131 9 0 192 193 130 11 0
                                         194 195 66 135)
                   [11 12 21 22] 8)))

(defun nelisp-bytecode-native-rooted-cfg-test--replace-edge
    (input block-index edge-index key value)
  (let* ((copy (copy-tree input t))
         (frame (plist-get copy :frame-result))
         (blocks (plist-get frame :blocks))
         (block (aref blocks block-index))
         (edges (plist-get block :successors))
         (edge (aref edges edge-index)))
    (plist-put edge key value)
    copy))

(ert-deftest nelisp-bytecode-native-rooted-cfg/admit-genuine-two-diamond-gnu31-frame ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-test--two-diamonds))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (blocks (plist-get plan :blocks))
         (operations (cl-loop for block in blocks append
                              (plist-get block :operations)))
         (branches (cl-remove-if-not
                    (lambda (operation)
                      (eq (plist-get operation :opcode) 'conditional-branch))
                    operations))
         (phis (plist-get plan :phis)))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get (plist-get input :frame-result) :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (should (= (length blocks) 7))
    (should (= (length branches) 2))
    (should (cl-every (lambda (branch)
                        (and (= (plist-get branch :bytecode-opcode) 131)
                             (equal (plist-get branch :condition-test)
                                    '(nil-tag-p condition-root))
                             (eq (plist-get branch :taken-if) 'nil)))
                      branches))
    (should (= (length phis) 2))
    (should (cl-every (lambda (phi)
                        (and (integerp (plist-get phi :id))
                             (= (length (plist-get phi :incoming)) 2)
                             (not (equal (cdar (plist-get phi :incoming))
                                         (cdadr (plist-get phi :incoming))))))
                      phis))
    (should (equal (plist-get plan :initial-roots)
                   '(frame-root 1 2 3 4 5 6)))
    (should (equal (plist-get plan :gateway-imports)
                   '("nl_native_car_v2" "nl_native_cdr_v2" "nl_native_cons_v2")))
    (should (equal (mapcar (lambda (op) (plist-get op :opcode))
                           (cl-remove-if-not
                            (lambda (op) (memq (plist-get op :opcode) '(car cdr cons)))
                            operations))
                   '(car cdr cons)))
    (should (equal (plist-get plan :return-selectors) '((19 . 9))))
    (should (< (plist-get plan :required-root-count) 256))
    (should (eq (plist-get (plist-get plan :entry-ast) :infrastructure-status)
                'propagate-unchanged))))

(ert-deftest nelisp-bytecode-native-rooted-cfg/plans-constants-stack-aliases-dup-discard ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  ;; These are genuine GNU 31.1 byte-code instructions materialized as a
  ;; byte-code function and then admitted through the public input verifier.
  (let* ((function (make-byte-code 0 (unibyte-string 192 193 1 137 136 135)
                                   [1 2] 4))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (operations (cl-loop for block in (plist-get plan :blocks) append
                              (plist-get block :operations))))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (should (equal (plist-get plan :constant-roots) '((0 . 1) (1 . 2))))
    (should (memq 'const (mapcar (lambda (op) (plist-get op :opcode)) operations)))
    (should (memq 'stack-ref (mapcar (lambda (op) (plist-get op :opcode)) operations)))
    (should (memq 'dup (mapcar (lambda (op) (plist-get op :opcode)) operations)))
    (should (memq 'discard (mapcar (lambda (op) (plist-get op :opcode)) operations)))
    (should (= (plist-get plan :required-root-count) 3))))

(ert-deftest nelisp-bytecode-native-rooted-cfg/preserves-all-phis-at-one-join ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-test--two-phis-at-one-join))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (join (assq 11 (mapcar (lambda (block)
                                 (cons (plist-get block :start) block))
                               (plist-get plan :blocks))))
         (phis (plist-get (cdr join) :phis)))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (should (= (length phis) 2))
    (should (= (length (plist-get plan :phis)) 2))
    (should (equal (mapcar (lambda (phi) (plist-get phi :slot)) phis) '(6 7)))
    (should (equal (mapcar (lambda (phi) (plist-get phi :incoming)) phis)
                   '(((4 . 7) (9 . 9)) ((4 . 8) (9 . 10)))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg/records-constant-root-values ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (value '(nil t 37))
    (let* ((function (byte-compile (eval `(lambda () ',value))))
           (input (nelisp-bytecode-compiler-input-build function))
           (plan (nelisp-bytecode-native-rooted-cfg-plan input))
           (initializer (car (plist-get plan :constant-initializers))))
      (should (eq (plist-get input :status) 'complete))
      (should (eq (plist-get plan :status) 'complete))
      (should (equal (plist-get initializer :value) value))
      (should (integerp (plist-get initializer :root))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg/rejects-cycle-bad-edge-and-stack-shape ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-test--two-diamonds))
         (cycle (nelisp-bytecode-native-rooted-cfg-test--replace-edge
                 input 1 0 :target 0))
         (missing-target-slot
          (nelisp-bytecode-native-rooted-cfg-test--replace-edge
           input 0 0 :target-slots []))
         (bad-target (nelisp-bytecode-native-rooted-cfg-test--replace-edge
                      input 0 0 :target 999)))
    (dolist (invalid (list cycle missing-target-slot bad-target))
      (let ((plan (nelisp-bytecode-native-rooted-cfg-plan invalid)))
        (should (eq (plist-get plan :status) 'unsupported))
        (should-not (plist-get plan :entry-ast))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg/admits-optionals-and-variable-access-but-refuses-captures ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((optional (nelisp-bytecode-compiler-input-build
                    (byte-compile (lambda (&optional value) value))))
         (dynamic (nelisp-bytecode-compiler-input-build
                   (byte-compile (lambda () nelisp-rooted-cfg-unbound-variable))))
         (fixed (nelisp-bytecode-native-rooted-cfg-test--two-diamonds))
         (capture (copy-tree fixed t)))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan optional) :status)
                'complete))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan dynamic) :status)
                'complete))
    (should (plist-get (nelisp-bytecode-native-rooted-cfg-plan dynamic) :frame-descriptor))
    (plist-put capture :capture-values-available t)
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan capture) :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            (plist-put (copy-tree fixed t) :argument-min 5)) :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            (plist-put (copy-tree fixed t) :argument-count -1)) :status)
                'unsupported))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                            (plist-put (copy-tree fixed t) :constants nil)) :status)
                'unsupported))
    (let ((too-many (copy-tree fixed t)))
      (plist-put too-many :constants (make-vector 256 nil))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan too-many) :status)
                  'unsupported)))))

(provide 'nelisp-bytecode-native-rooted-cfg-plan-test)
;;; nelisp-bytecode-native-rooted-cfg-plan-test.el ends here
