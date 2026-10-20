;;; nelisp-buffer-local.el --- Mirror-owned buffer-local cells -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Explicit local cells use the same mirror and dynamic frames as ordinary
;; variables. This does not qualify automatic-local creation or watcher hooks.
(defvar nelisp-buffer-local--mirror nil)
(defvar nelisp-buffer-local--serial 0)
(defun nelisp-buffer-local--mirror ()
  "Borrow the evaluator's mirror through an authenticated existing root API."
  (or nelisp-buffer-local--mirror
      (progn
        (require 'nelisp-native-load)
        (require 'nelisp-env)
        (let* ((env (nelisp--native-env))
               (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
               (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
               (slot-api (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2"))
               (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
               (ticket (ptr-call begin env 0 0 0 0 0)))
          (unwind-protect
              (let* ((slot (ptr-call reserve env ticket 0 0 0 0))
                     (marker (ptr-call slot-api env ticket 0 0 0 0)))
                (unless (> slot 0) (error "Buffer-local mirror root unavailable"))
                (dotimes (i 4) (ptr-write-u64 slot (* i 8) (ptr-read-u64 env (* i 8))))
                (setq nelisp-buffer-local--mirror
                      (nelisp--native-unbox-reference slot env marker)))
            (unless (= (ptr-call end env ticket 0 0 0 0) 1)
              (error "Buffer-local mirror root ownership lost")))))))
(defun nelisp-buffer-local--legacy-entry (variable &optional create)
  "Return VARIABLE's canonical mirror entry; CREATE adds redirect metadata."
  (unless (symbolp variable) (signal 'wrong-type-argument (list 'symbolp variable)))
  (let* ((mirror (nelisp-buffer-local--mirror))
         (table (nelisp--record-ref mirror 0))
         (name (if (and (boundp 'nelisp--native-variable-aliases)
                        nelisp--native-variable-aliases)
                   (indirect-variable variable) variable))
         (seen nil)
         (uninterned (not (eq (intern-soft (symbol-name name)) name)))
         (key (if uninterned name (symbol-name name)))
         (entry (nelisp--fast-hash-get table key nil uninterned)))
    (while (and entry (> (length entry) 5) (nelisp--record-ref entry 4))
      (when (memq name seen) (signal 'cyclic-variable-indirection (list variable)))
      (push name seen)
      (setq name (nelisp--record-ref entry 4)
            uninterned (not (eq (intern-soft (symbol-name name)) name))
            key (if uninterned name (symbol-name name))
            entry (nelisp--fast-hash-get table key nil uninterned)))
    (when create
      (when (or (memq name '(nil t)) (keywordp name)
                (and entry (nelisp--record-ref entry 3)))
        (signal 'setting-constant (list variable)))
      (unless entry
        (setq entry (nelisp-env--make-symbol-entry))
        (nelisp--fast-hash-put table key entry uninterned))
      (when (<= (length entry) 6)
        (setq entry (nelisp--make-record 'symbol-entry
                     (nelisp--record-ref entry 0) (nelisp--record-ref entry 1)
                     (nelisp--record-ref entry 2) (nelisp--record-ref entry 3)
                     (and (> (length entry) 5) (nelisp--record-ref entry 4)) nil))
        (nelisp--fast-hash-put table key entry uninterned))
      (unless (nelisp--record-ref entry 5)
        (nelisp--record-set entry 5 (vector 'nelisp--current-buffer nil))))
    entry))
(defun nelisp-buffer-local--entry (variable &optional create)
  "Return VARIABLE's entry without bootstrapping a native compiler."
  (if (not (and (boundp 'nelisp--native-variable-aliases) nelisp--native-variable-aliases))
      (nelisp-buffer-local--legacy-entry variable create)
    (unless (symbolp variable) (signal 'wrong-type-argument (list 'symbolp variable)))
    (let ((name (indirect-variable variable)))
      (when (and create (or (memq name '(nil t)) (keywordp name)))
        (signal 'setting-constant (list variable)))
      (let ((entry (nelisp--env-globals-op 'variable-entry name nil)))
        (when create
          (when (and entry (nelisp--record-ref entry 3))
            (signal 'setting-constant (list variable)))
          (setq entry (nelisp--env-globals-op 'variable-entry name t))
          (unless (nelisp--record-ref entry 5)
            (nelisp--record-set entry 5 (vector 'nelisp--current-buffer nil))))
        entry))))
(defun nelisp-buffer-local--redirect (variable)
  (let ((entry (nelisp-buffer-local--entry variable)))
    (and entry (> (length entry) 6) (nelisp--record-ref entry 5))))
(defun nelisp-buffer-local-make (variable)
  "Create VARIABLE's explicit local cell in the current buffer, preserving void."
  (let* ((entry (nelisp-buffer-local--entry variable t))
         (redirect (nelisp--record-ref entry 5))
         (buffer (current-buffer)))
    (unless (assq buffer (aref redirect 1))
      (let ((cell (intern (format "nelisp-buffer-local--cell-%d"
                                 (setq nelisp-buffer-local--serial
                                       (1+ nelisp-buffer-local--serial))))))
        (eval (list 'defvar cell))
        (put cell 'nelisp--buffer-local-variable (indirect-variable variable))
        (when (boundp variable) (set cell (symbol-value variable)))
        (aset redirect 1 (cons (cons buffer cell) (aref redirect 1)))))
    variable))
(defun nelisp-buffer-local-p (variable &optional buffer)
  "Return non-nil for an explicit local cell in BUFFER, including void cells."
  (or (eq (indirect-variable variable) 'enable-multibyte-characters)
      (let ((redirect (nelisp-buffer-local--redirect variable)))
        (and redirect (and (assq (or buffer (current-buffer)) (aref redirect 1)) t)))))
(defun nelisp-buffer-local-default (operation variable &optional value)
  "Perform OPERATION on the default binding without selecting a local cell."
  (unless (symbolp variable) (signal 'wrong-type-argument (list 'symbolp variable)))
  (if (and (boundp 'nelisp--native-variable-aliases) nelisp--native-variable-aliases)
      (if (memq variable '(nil t))
          (if (eq operation 'set) (signal 'setting-constant (list variable))
            (if (eq operation 'boundp) t variable))
        (if (keywordp variable)
            (if (eq operation 'set)
                (if (eq variable value) value (signal 'setting-constant (list variable)))
              (if (eq operation 'boundp) t variable))
        (nelisp--env-globals-op
         (cond ((eq operation 'set) 'variable-set-default)
               ((eq operation 'boundp) 'variable-default-boundp)
               (t 'variable-default-value)) variable value)))
  (let* ((redirect (nelisp-buffer-local--redirect variable))
         (locals (and redirect (aref redirect 1))))
    (unwind-protect
        (progn (when redirect (aset redirect 1 nil))
               (if (eq operation 'set) (set variable value)
                 (funcall operation variable)))
      (when redirect (aset redirect 1 locals))))))
(defun nelisp-buffer-local-kill (variable)
  "Remove VARIABLE's current local redirect, revealing its default binding."
  ;; GNU keeps this always-local built-in, including when named by an alias.
  (unless (eq (indirect-variable variable) 'enable-multibyte-characters)
    (let ((redirect (nelisp-buffer-local--redirect variable)))
      (when redirect
        ;; CREATE authenticates the mutation guard even when no resize is needed.
        (nelisp-buffer-local--entry variable t)
        (aset redirect 1 (assq-delete-all (current-buffer) (aref redirect 1))))))
  variable)
(when (fboundp 'nelisp--native-env)
  ;; The provider reports this built-in as local even in a fresh buffer.
  ;; Its initial default must exist before insert consults that local flag.
  (defvar enable-multibyte-characters t)
  (fset 'make-local-variable #'nelisp-buffer-local-make)
  (fset 'local-variable-p #'nelisp-buffer-local-p)
  (fset 'default-value (lambda (variable) (nelisp-buffer-local-default 'symbol-value variable)))
  (fset 'default-boundp (lambda (variable) (nelisp-buffer-local-default 'boundp variable)))
  (fset 'set-default (lambda (variable value) (nelisp-buffer-local-default 'set variable value)))
  (fset 'kill-local-variable #'nelisp-buffer-local-kill))
(provide 'nelisp-buffer-local)
