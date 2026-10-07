;;; nelisp-bytecode-native-rooted-cfg-emit-test.el --- rooted CFG AST emitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-aot-compiler)

(defun nelisp-bytecode-native-rooted-cfg-emit-test--two-diamonds ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (condition-a left-a right-a condition-b left-b right-b)
      (cons (car (if condition-a left-a right-a))
            (cdr (if condition-b left-b right-b)))))))

(defun nelisp-bytecode-native-rooted-cfg-emit-test--two-phis-at-one-join ()
  (nelisp-bytecode-compiler-input-build
   (make-byte-code 1542 (unibyte-string 1 131 9 0 192 193 130 11 0
                                         194 195 66 135)
                   [11 12 21 22] 8)))

(defun nelisp-bytecode-native-rooted-cfg-emit-test--carried-phi ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (a x y b z)
      (cons (if a x y) (if b z (if a x y)))))))

(defun nelisp-bytecode-native-rooted-cfg-emit-test--walk (form predicate)
  (let (matches)
    (cl-labels ((visit (node)
                  (when (consp node)
                    (when (funcall predicate node) (push node matches))
                    (visit (car node))
                    (visit (cdr node)))))
      (visit form))
    (nreverse matches)))

(defun nelisp-bytecode-native-rooted-cfg-emit-test--contains-p (tree value)
  (if (consp tree)
      (or (equal tree value)
          (nelisp-bytecode-native-rooted-cfg-emit-test--contains-p (car tree) value)
          (nelisp-bytecode-native-rooted-cfg-emit-test--contains-p (cdr tree) value))
    (equal tree value)))

(ert-deftest nelisp-bytecode-native-rooted-cfg-emit/expands-genuine-diamonds-and-compiles-aot ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-emit-test--two-diamonds))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_test_v1"))
         (form (plist-get emitted :form))
         (unit (nelisp-aot-compile-to-link-unit form))
         (calls (nelisp-bytecode-native-rooted-cfg-emit-test--walk
                 form (lambda (node) (eq (car node) 'extern-call))))
         (status-encodings
          (nelisp-bytecode-native-rooted-cfg-emit-test--walk
           form (lambda (node)
                  (and (eq (car node) '+) (memq 256 (cdr node)))))))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'complete))
    (should (equal (plist-get emitted :gateway-imports)
                   '("nl_native_car_v2" "nl_native_cdr_v2"
                     "nl_native_cons_v2" "nl_root_pin_slot_v2")))
    (should (> (plist-get emitted :expansion-count)
               (length (plist-get plan :blocks))))
    (should (equal (plist-get emitted :initial-roots)
                   '(frame-root 1 2 3 4 5 6)))
    (should (cl-every (lambda (name)
                        (member name (plist-get unit :extern-symbols)))
                      (plist-get emitted :gateway-imports)))
    (should (equal (plist-get (car (plist-get unit :defuns)) :arity) 4))
    (should (member "nl_native_rooted_cfg_test_v1"
                    (mapcar (lambda (defun) (plist-get defun :name))
                            (plist-get unit :defuns))))
    (should (cl-some (lambda (call) (eq (cadr call) 'nl_root_pin_slot_v2)) calls))
    (should (cl-some (lambda (call) (eq (cadr call) 'nl_native_car_v2)) calls))
    (should (cl-some (lambda (call) (eq (cadr call) 'nl_native_cdr_v2)) calls))
    (should (cl-some (lambda (call) (eq (cadr call) 'nl_native_cons_v2)) calls))
    (should (cl-every (lambda (call) (= (length call) 8)) calls))
    (should status-encodings)
    (should (nelisp-bytecode-native-rooted-cfg-emit-test--contains-p form 521))
    (dolist (root '(2 3 5 6))
      (should (nelisp-bytecode-native-rooted-cfg-emit-test--contains-p
               form `(+ 256 ,root))))
    (should (equal (plist-get emitted :required-root-count) 10))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-emit/resolves-multiple-phi-roots-per-predecessor ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-emit-test--two-phis-at-one-join))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_phi_test_v1"))
         (form (plist-get emitted :form))
         (unit (nelisp-aot-compile-to-link-unit form))
         (calls (nelisp-bytecode-native-rooted-cfg-emit-test--walk
                 form (lambda (node) (eq (car node) 'extern-call))))
         (cons-calls (cl-remove-if-not
                      (lambda (call) (eq (cadr call) 'nl_native_cons_v2)) calls)))
    (should (eq (plist-get plan :status) 'complete))
    (should (= (length (plist-get plan :phis)) 2))
    (should (eq (plist-get emitted :status) 'complete))
    (should (= (length cons-calls) 2))
    (should (cl-every (lambda (call)
                        (and (integerp (nth 4 call))
                             (integerp (nth 5 call))
                             (integerp (nth 6 call))))
                      cons-calls))
    (should (member "nl_native_cons_v2" (plist-get unit :extern-symbols)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-emit/resolves-carried-phi-through-later-join ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-emit-test--carried-phi))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_carried_phi_v1"))
         (unit (and (eq (plist-get emitted :status) 'complete)
                    (nelisp-aot-compile-to-link-unit (plist-get emitted :form))))
         (cons-calls (and (eq (plist-get emitted :status) 'complete)
                          (nelisp-bytecode-native-rooted-cfg-emit-test--walk
                           (plist-get emitted :form)
                           (lambda (node) (eq (cadr node) 'nl_native_cons_v2)))))
         (phis (plist-get plan :phis))
         (earlier (cl-find 9 phis :key (lambda (phi) (plist-get phi :block))))
         (later (cl-find 26 phis :key (lambda (phi) (plist-get phi :block)))))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get plan :status) 'complete))
    (should (>= (length phis) 2))
    (should earlier)
    (should later)
    (should (= (cdr (assq 13 (plist-get later :incoming)))
               (plist-get earlier :slot)))
    (should (eq (plist-get emitted :status) 'complete))
    (should (plist-get unit :defuns))
    (should (member "nl_native_cons_v2" (plist-get unit :extern-symbols)))
    ;; Argument z is root 5. Keep it distinct from the earlier joined x/y
    ;; value (roots 2/3) in both arms of the later conditional.
    (should (member '(extern-call nl_native_cons_v2 env ticket 3 5 6 0)
                    cons-calls))
    (should (member '(extern-call nl_native_cons_v2 env ticket 2 5 6 0)
                    cons-calls))
    (should (member '(extern-call nl_native_cons_v2 env ticket 3 2 6 0)
                    cons-calls))
    (should (member '(extern-call nl_native_cons_v2 env ticket 2 3 6 0)
                    cons-calls))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-emit/refuses-mutated-root-phi-and-limit-before-output ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-emit-test--two-phis-at-one-join))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (bad-root (copy-tree plan t))
         (bad-phi (copy-tree plan t))
         (too-many (copy-tree plan t)))
    (setf (plist-get (car (plist-get (car (plist-get bad-root :blocks)) :operations))
                     :output-root)
          999)
    (setf (plist-get (car (plist-get bad-phi :phis)) :incoming) '((999 . 7) (998 . 9)))
    (setf (plist-get too-many :required-root-count) 256)
    (dolist (invalid (list bad-root bad-phi too-many))
      (let ((result (nelisp-bytecode-native-rooted-cfg-emit
                     invalid "nl_native_rooted_cfg_invalid_v1")))
        (should (eq (plist-get result :status) 'unsupported))
        (should-not (plist-get result :form))
        (should-not (plist-get result :gateway-imports))))))

(provide 'nelisp-bytecode-native-rooted-cfg-emit-test)
;;; nelisp-bytecode-native-rooted-cfg-emit-test.el ends here
