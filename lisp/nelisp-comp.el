;;; nelisp-comp.el --- Runtime bridge for GNU comp.el -*- lexical-binding: t; -*-

(require 'emacs-cc-comp-1)
(require 'emacs-cc-comp-2)
(require 'emacs-cc-command-modes-1)
(require 'emacs-cc-census-other-w301)
(require 'emacs-cc-fns-2)
(unless (fboundp 'map-keymap) (require 'emacs-keymap-builtins))
(unless (fboundp 'command-remapping) (require 'emacs-cc-census-buffer-w201))

(defvar comp-subr-list nil
  "NeLisp primitive objects available to the front end.
The emitter independently uses the ordered imports of its GNU ABI profile.")

(defvar comp-subr-arities-h
  (let ((arities (make-hash-table :test 'equal)))
    ;; Like GNU loadup.el, snapshot the arities rather than inspecting
    ;; the current function definition each time comp.el emits a call.
    (mapatoms (lambda (name)
                (let ((function (symbol-function name)))
                  (when (subr-primitive-p function)
                    (push function comp-subr-list)
                    (puthash name (subr-arity function) arities)))))
    arities)
  "Primitive name to original arity, for the native compiler.")

(defvar comp-abi-hash "ba35c031")
(defvar comp-native-version-dir "31.1-ba35c031")
(defvar native-comp-eln-load-path '("../native-lisp/"))
(defvar comp-deferred-pending-h (make-hash-table :test 'eq))
(defvar comp-eln-to-el-h (make-hash-table :test 'equal))
(defvar comp-installed-trampolines-h (make-hash-table))
(defvar comp-no-native-file-h (make-hash-table :test 'equal))
(defvar comp--no-native-compile (make-hash-table :test 'equal))
(defvar native-comp-enable-subr-trampolines nil)
(defvar comp-loaded-comp-units-h (make-hash-table :test 'equal :weakness 'value))
(defvar comp-file-preloaded-p nil)
(defvar comp-sanitizer-active nil)
(defvar comp--\#$ "#$")

(require 'comp)

;; Worker startup and deferred load are separate compiler stages.
(when (and (eq system-type 'gnu/linux)
           (string-prefix-p "x86_64" system-configuration)
           (fboundp 'nelisp--native-subr-create)
           (fboundp 'nl-ffi-call)
           (executable-find "ld")
           (condition-case nil
               (let ((handle (nl-ffi-call "dlopen" 0 2)))
                 (and (integerp handle) (not (zerop handle))
                      (eql (nl-ffi-call "dlclose" handle) 0)))
             (nelisp-unsupported-primitive nil)))
  (provide 'native-compile))

(provide 'nelisp-comp)
