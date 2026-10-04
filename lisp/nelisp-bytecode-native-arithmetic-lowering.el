;;; nelisp-bytecode-native-arithmetic-lowering.el --- Rooted opcode 92 lowering -*- lexical-binding: t; -*-
(require 'nelisp-native-arithmetic-v2)

(let ((owners nil) (owner-checker nil)
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error))
      (provider-checker (symbol-function 'nelisp-native-arithmetic-v2-owner-valid-p)))
(defun nelisp-bytecode-native-arithmetic-lowering-owner-valid-p ()
  "Check original lowering and public provider owners before helper execution."
  (let ((remaining owners))
    (while remaining
      (let ((entry (funcall head remaining)))
        (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
            nil (funcall reject "arithmetic-lowering: source owner changed")))
      (setq remaining (funcall tail remaining)))
    (funcall provider-checker)))

(defun nelisp-bytecode-native-arithmetic-lowering--variable-p (value)
  "Accept a real emitter variable rather than a constant expression."
  (and (symbolp value) value (not (eq value t)) (not (keywordp value))))

(defun nelisp-bytecode-native-arithmetic-lowering--operation-p (operation count)
  "Check the bounded gateway record; the planner authenticates root origins."
  (let ((remaining operation) (seen nil) (pairs 0) (valid t))
    (while (and valid remaining (< pairs 5))
      (if (and (consp remaining) (consp (cdr remaining))
               (memq (car remaining)
                     '(:opcode :bytecode-opcode :input-roots :output-root :exit-root-base))
               (not (memq (car remaining) seen)))
          (progn
            (setq seen (cons (car remaining) seen)
                  remaining (cdr (cdr remaining)) pairs (1+ pairs)))
        (setq valid nil)))
    (and valid (null remaining) (= pairs 5)
         (integerp count) (> count 3) (<= count 16384)
         (eq (plist-get operation :opcode) 'add)
         (eq (plist-get operation :bytecode-opcode) 92)
         (let* ((inputs (plist-get operation :input-roots))
                (output (plist-get operation :output-root))
                (exit (plist-get operation :exit-root-base)))
           (and (consp inputs) (consp (cdr inputs)) (null (cdr (cdr inputs)))
                (integerp (car inputs)) (integerp (car (cdr inputs)))
                (integerp output) (integerp exit)
                (> (car inputs) 0) (< (car inputs) count)
                (> (car (cdr inputs)) 0) (< (car (cdr inputs)) count)
                (> output 0) (< output count) (> exit 0) (< (+ exit 2) count)
                (not (and (>= (car inputs) exit) (< (car inputs) (+ exit 3))))
                (not (and (>= (car (cdr inputs)) exit) (< (car (cdr inputs)) (+ exit 3))))
                (not (and (>= output exit) (< output (+ exit 3)))))))))

(defun nelisp-bytecode-native-arithmetic-lowering-build (operation root-count environment ticket)
  "Lower an authenticated planner OPERATION to the public numeric gateway.
This checks structure and ABI bounds. The caller must authenticate the plan
and root origins before accepting this result as executable evidence. Status
0 writes output, 1 transfers exact exit roots, and 2 refuses the request."
  (funcall owner-checker)
  (if (not (and (nelisp-bytecode-native-arithmetic-lowering--operation-p operation root-count)
                (nelisp-bytecode-native-arithmetic-lowering--variable-p environment)
                (nelisp-bytecode-native-arithmetic-lowering--variable-p ticket)
                (not (eq environment ticket))))
      (list :status 'refused)
    (list :status 'complete
          :call (list 'nl_native_add_v2 environment ticket
                      (car (plist-get operation :input-roots))
                      (car (cdr (plist-get operation :input-roots)))
                      (plist-get operation :output-root)
                      (plist-get operation :exit-root-base))
          :import (nelisp-native-arithmetic-v2-descriptor)
          :source (nelisp-native-arithmetic-v2-source))))

(defun nelisp-bytecode-native-arithmetic-lowering-dependency-context ()
  "Return actual lowering and provider owners for authenticated emitter seals."
  (funcall owner-checker)
  (vector (symbol-function 'nelisp-bytecode-native-arithmetic-lowering-build)
          (symbol-function 'nelisp-bytecode-native-arithmetic-lowering--operation-p)
          (symbol-function 'nelisp-bytecode-native-arithmetic-lowering--variable-p)
          (symbol-function 'nelisp-bytecode-native-arithmetic-lowering-dependency-context)
          (nelisp-native-arithmetic-v2-dependency-context)
          (mapcar #'symbol-function
                  '(symbolp keywordp eq not consp car cdr memq cons null integerp
                    = < > <= >= + 1+ plist-get list vector mapcar symbol-function))))
(setq owner-checker (funcall lookup 'nelisp-bytecode-native-arithmetic-lowering-owner-valid-p)
      owners (mapcar (lambda (name) (cons name (funcall lookup name)))
                    '(nelisp-bytecode-native-arithmetic-lowering-owner-valid-p
                      nelisp-bytecode-native-arithmetic-lowering-build
                      nelisp-bytecode-native-arithmetic-lowering--operation-p
                      nelisp-bytecode-native-arithmetic-lowering--variable-p
                      nelisp-bytecode-native-arithmetic-lowering-dependency-context
                      nelisp-native-arithmetic-v2-owner-valid-p
                      symbolp keywordp eq not consp car cdr memq cons null integerp
                      = < > <= >= + 1+ plist-get list vector mapcar symbol-function
                      error and or)))
)
(provide 'nelisp-bytecode-native-arithmetic-lowering)
