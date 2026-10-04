;;; nelisp-bytecode-native-guarded-lowering.el --- Source-owned guard selection -*- lexical-binding: t; -*-
(require 'nelisp-bytecode-native-arithmetic-lowering)
(require 'nelisp-native-optimization-guard-v1)

(let ((owners nil) (owner-checker nil)
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error))
      (guard-checker (symbol-function 'nelisp-native-optimization-guard-v1-owner-valid-p))
      (lowering-checker (symbol-function 'nelisp-bytecode-native-arithmetic-lowering-owner-valid-p)))
(defun nelisp-bytecode-native-guarded-lowering-owner-valid-p ()
  "Check original lowering/guard owners before any context copying occurs.
Composes source-owned public gates without enumerating foreign private helpers."
  (let ((remaining owners))
    (while remaining
      (let ((entry (funcall head remaining)))
        (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
            nil (funcall reject "guarded-lowering: source owner changed")))
      (setq remaining (funcall tail remaining)))
    (funcall guard-checker)
    (funcall lowering-checker)))

(defun nelisp-bytecode-native-guarded-lowering-build (operation count environment ticket mode)
  "Select MODE for an already authenticated arithmetic planner operation.
This public result is structural evidence, not a native capability. The
compiler must seal MODE with plan options, helper identities and root origins;
an artifact loader must verify the exact source, descriptor and import records.
There is no caller override of a sealed plan's mode."
  (funcall owner-checker)
  (if (not (memq mode '(on off))) (list :status 'refused)
    (let ((lowered (nelisp-bytecode-native-arithmetic-lowering-build
                    operation count environment ticket)))
      (if (not (eq (plist-get lowered :status) 'complete)) lowered
        (let* ((descriptor (nelisp-native-optimization-guard-v1-descriptor mode))
               (call (plist-get lowered :call))
               (name (if (eq mode 'on) 'nl_native_add_guard_v1 'nl_native_add_v2)))
          (list :status 'complete :arithmetic-guard-mode mode
                :call (cons name (cdr call))
                :gateway descriptor
                :additional-source (nelisp-native-optimization-guard-v1-source mode)
                :runtime-imports (nelisp-native-arithmetic-v2-runtime-imports)))))))

(defun nelisp-bytecode-native-guarded-lowering--bounded-p (value active budget depth)
  "Bound untrusted plan traversal before comparing canonical owner data."
  (setcar budget (1- (car budget)))
  (and (>= (car budget) 0) (<= depth 64)
       (cond
        ((consp value)
         (and (not (memq value active))
              (let ((next (cons value active)))
                (and (nelisp-bytecode-native-guarded-lowering--bounded-p
                      (car value) next budget (1+ depth))
                     (nelisp-bytecode-native-guarded-lowering--bounded-p
                      (cdr value) next budget depth)))))
        ;; Byte-code functions expose bounded code/constants via sequence
        ;; access. Native subrs below are immutable identity-only owners.
        ((or (vectorp value) (byte-code-function-p value))
         (and (<= (length value) 1024) (not (memq value active))
              (let ((index 0) (valid t) (next (cons value active)))
                (while (and valid (< index (length value)))
                  (setq valid (nelisp-bytecode-native-guarded-lowering--bounded-p
                               (aref value index) next budget (1+ depth))
                        index (1+ index)))
                valid)))
        ((stringp value) (and (<= (length value) 4096)
                             (not (text-properties-at 0 value))
                             (let ((change (next-property-change 0 value (length value))))
                               (or (null change) (= change (length value))))))
        ((or (symbolp value) (integerp value) (floatp value) (subrp value)) t)
        (t nil))))

(defun nelisp-bytecode-native-guarded-lowering-select (plan)
  "Select only a mode reproduced by the genuine public canonical planner.
The producer must seal the public planner owner before calling this wrapper;
this result alone is not an authentication certificate. Missing
normalized mode, conflicting plan data, unsupported planner versions and
unbounded/cyclic data refuse. The producer still owns root-origin and artifact
admission proofs. No caller-supplied predicate or option overrides the plan."
  (funcall owner-checker)
  (if (not (nelisp-bytecode-native-guarded-lowering--bounded-p plan nil (list 16384) 0))
      (list :status 'refused)
    (let ((mode (plist-get plan :arithmetic-guard-mode)))
      (if (not (and (eq (plist-get plan :status) 'complete) (memq mode '(on off))))
          (list :status 'refused)
        (require 'nelisp-bytecode-native-rooted-cfg-plan)
        (let ((canonical
               (condition-case nil
                   (nelisp-bytecode-native-rooted-cfg-plan
                    (plist-get plan :input) (plist-get plan :lowering-mode) mode)
                 (error nil))))
          (if (not (and canonical
                        (eq (plist-get canonical :arithmetic-guard-mode) mode)
                        (equal canonical plan)))
              (list :status 'refused)
            (list :status 'complete :requires-planner-owner-seal t :arithmetic-guard-mode mode
                  :gateway (nelisp-native-optimization-guard-v1-descriptor mode)
                  :additional-source (nelisp-native-optimization-guard-v1-source mode)
                  :runtime-imports (nelisp-native-arithmetic-v2-runtime-imports))))))))

(defun nelisp-bytecode-native-guarded-lowering-dependency-context ()
  "Return complete public lowering/guard dependencies for compiler sealing."
  (funcall owner-checker)
  (vector (symbol-function 'nelisp-bytecode-native-guarded-lowering-build)
          (symbol-function 'nelisp-bytecode-native-guarded-lowering-owner-valid-p)
          (symbol-function 'nelisp-bytecode-native-guarded-lowering-select)
          (symbol-function 'nelisp-bytecode-native-guarded-lowering--bounded-p)
          (and (fboundp 'nelisp-bytecode-native-rooted-cfg-plan)
               (symbol-function 'nelisp-bytecode-native-rooted-cfg-plan))
          (symbol-function 'nelisp-bytecode-native-guarded-lowering-dependency-context)
          (nelisp-bytecode-native-arithmetic-lowering-dependency-context)
          (nelisp-native-optimization-guard-v1-dependency-context)
          (mapcar #'symbol-function '(memq not eq plist-get cons car cdr list vector mapcar
                                     symbol-function and cond >= <= + setcar 1- 1+
                                     vectorp byte-code-function-p symbolp integerp floatp subrp
                                     stringp length < aref text-properties-at
                                     next-property-change null or = equal require fboundp))))

(setq owner-checker (funcall lookup 'nelisp-bytecode-native-guarded-lowering-owner-valid-p)
      owners
      (mapcar (lambda (name) (cons name (funcall lookup name)))
              '(nelisp-bytecode-native-guarded-lowering-owner-valid-p
                nelisp-bytecode-native-guarded-lowering-build
                nelisp-bytecode-native-guarded-lowering-select
                nelisp-bytecode-native-guarded-lowering--bounded-p
                nelisp-bytecode-native-guarded-lowering-dependency-context
                nelisp-native-optimization-guard-v1-owner-valid-p
                nelisp-bytecode-native-arithmetic-lowering-build
                nelisp-bytecode-native-arithmetic-lowering-owner-valid-p
                nelisp-bytecode-native-arithmetic-lowering-dependency-context
                nelisp-native-arithmetic-v2-runtime-imports
                symbol-function eq car cdr memq not plist-get cons list vector
                mapcar and or cond unless >= <= + < > setcar 1- 1+ vectorp
                byte-code-function-p symbolp integerp floatp subrp stringp length
                aref text-properties-at next-property-change null = equal require
                fboundp error)))
)

(provide 'nelisp-bytecode-native-guarded-lowering)
