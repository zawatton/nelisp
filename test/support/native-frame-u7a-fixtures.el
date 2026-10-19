;;; native-frame-u7a-fixtures.el --- Dynamic frame parity corpus -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar u7a-special 'outside)
(defvar u7a-mode nil)
(defvar u7a-effects nil)
(defvar u7a-inner nil)
(defvar u7a-other-buffer nil)
(defvar u7a-bare-target 'target-outside)
(defvaralias 'u7a-bare-alias 'u7a-bare-target)
(defun u7a-callback ()
  (push (symbol-value 'u7a-special) u7a-effects)
  (garbage-collect)
  (when (eq u7a-mode 'switch-read)
    (with-current-buffer u7a-other-buffer
      (push (symbol-value 'u7a-special) u7a-effects)
      (set 'u7a-special 'other-changed)
      (push (symbol-value 'u7a-special) u7a-effects)))
  (cond ((eq u7a-mode 'throw) (throw 'u7a-exit (symbol-value 'u7a-special)))
        ((eq u7a-mode 'signal) (signal 'error (list "u7a" (symbol-value 'u7a-special))))
        ((eq u7a-mode 'quit) (signal 'quit (list (symbol-value 'u7a-special))))
        ((eq u7a-mode 'reenter)
         (let ((u7a-mode nil)) (funcall u7a-inner 'inner))
         (push (symbol-value 'u7a-special) u7a-effects)))
  (symbol-value 'u7a-special))
(defun native-frame-u7a-function (name &optional symbol)
  "Use the same GNU bytecode body for the interpreter and both native backends."
  (cond
   ((eq name 'constant)
    (make-byte-code 257 (unibyte-string 24 193 32 41 135) [t u7a-callback] 1))
   ((eq name 'alias)
    (make-byte-code 257 (unibyte-string 24 193 32 41 135) [u7a-alias u7a-alias-callback] 1))
   ((eq name 'nested) (make-byte-code 514 (unibyte-string 24 24 8 41 8 41 66 135)
                                    (vector (or symbol 'u7a-special)) 2))
   ((eq name 'implicit) (make-byte-code 257 (unibyte-string 24 8 135)
                                      (vector (or symbol 'u7a-special)) 1))
   ((eq name 'set) (make-byte-code 257 (unibyte-string 24 193 16 8 41 135)
                                 (vector (or symbol 'u7a-special) 'changed) 1))
   ((eq name 'zero) (make-byte-code 0 (unibyte-string 40 192 135) [zero] 1))
   ((integerp name)
    (make-byte-code 0 (apply #'unibyte-string
                            (append (apply #'append (make-list name '(193 24)))
                                    (list (+ 40 name) 194 135)))
                    (vector (or symbol 'u7a-special) 'inside 'result) 1))
   (t (make-byte-code 257 (unibyte-string 24 193 32 41 135)
                      (vector (or symbol 'u7a-special) 'u7a-callback) 1))))
(defun native-frame-u7a-bare-function (kind)
  "Return a variable-only bytecode fixture without binding/frame opcodes."
  (pcase kind
    ('get (make-byte-code 0 (unibyte-string 8 135) [u7a-special] 1))
    ('set (make-byte-code 257 (unibyte-string 137 16 135) [u7a-special] 2))
    ('constant-set (make-byte-code 257 (unibyte-string 137 16 135) [t] 2))
    ('rich
     (make-byte-code
      514
      (apply #'unibyte-string
             '(1 194 61 131 8 0 8 135 1 195 61 131 18 0 137 137 16
               135 1 196 61 131 26 0 9 135 1 197 61 131 36 0 137 137
               17 135 1 198 61 131 61 0 137 131 50 0 8 130 54 0 199
               67 137 16 200 32 136 137 8 66 135 1 201 61 131 72 0
               201 202 8 34 135 203 204 205 8 68 34 135))
      (vector 'u7a-special 'u7a-bare-alias 'get 'set 'alias-get
              'alias-set 'gc-diamond 'changed 'garbage-collect 'throw
              'u7a-exit 'signal 'error 'u7a-bare)
      6))
    (_ (error "Unknown U7a bare fixture %S" kind))))
(defun native-frame-u7a-observe (function args)
  "Observe result, exact signal/throw, effects and the binding after exit."
  (setq u7a-effects nil)
  (let ((result (condition-case err
                    (catch 'u7a-exit (list 'returned (apply function args)))
                  (error (list 'signal err))
                  (quit (list 'quit err)))))
    (list result (reverse u7a-effects)
          (if (boundp 'u7a-special) (symbol-value 'u7a-special) 'void))))
(provide 'native-frame-u7a-fixtures)

(defvar u7a-target 'outside)
(defun u7a-alias-callback ()
  (garbage-collect)
  (cons (symbol-value 'u7a-alias) (symbol-value 'u7a-target)))
