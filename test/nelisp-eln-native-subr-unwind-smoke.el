;;; nelisp-eln-native-subr-unwind-smoke.el --- unary bridge cleanup probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nl-ffi)
(require 'nl-ffi-memory)
(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-native-subr)

(let* ((word-base (symbol-function 'nelisp-eln-raw-call-word))
       (activation-base
        (symbol-function 'nelisp-eln-objects-activation-release))
       (objects-base (symbol-function 'nelisp-eln-objects-release))
       (context-base (symbol-function 'nelisp-eln-raw-call-context-release))
       (memory-base (symbol-function 'nl-ffi-memory-release))
       (mode nil) (released nil) (mappings 0)
       (saved-activation nil) (saved-objects nil) (saved-context nil)
       (baseline nil))
  (unwind-protect
      (progn
        ;; Warm the process-lifetime trampoline before taking the registry baseline.
        (let ((context (nelisp-eln-raw-call-context-create)))
          (nelisp-eln-raw-call-context-release context))
        (setq baseline
              (list (length nelisp-eln-objects--live-units)
                    (length nelisp-eln-objects--activations)
                    (length nelisp-eln-objects--identity-records)
                    (length nelisp-eln-objects--pending-cleanups)
                    (length nelisp-eln-raw-call--pending-cleanup)))
        (fset 'nelisp-eln-raw-call-word
              (lambda (&rest _args)
                (pcase mode
                  ('throw (throw 'unwind-smoke-tag 'thrown))
                  ('quit (signal 'quit nil))
                  (_ (signal 'nelisp-eln-native-subr-error '(injected))))))
        (fset 'nelisp-eln-objects-activation-release
              (lambda (owner)
                (setq saved-activation owner)
                (push 'activation released)
                (funcall activation-base owner)))
        (fset 'nelisp-eln-objects-release
              (lambda (owner)
                (setq saved-objects owner)
                (push 'objects released)
                (funcall objects-base owner)))
        (fset 'nelisp-eln-raw-call-context-release
              (lambda (owner)
                (setq saved-context owner)
                (push 'context released)
                (funcall context-base owner)))
        (fset 'nl-ffi-memory-release
              (lambda (owner)
                (setq mappings (1+ mappings))
                (funcall memory-base owner)))
        (dolist (case '((error . (error (nelisp-eln-native-subr-error injected)))
                        (throw . thrown)
                        (quit . (quit (quit)))))
          (setq mode (car case) released nil mappings 0
                saved-activation nil saved-objects nil saved-context nil)
          (let* ((result
                  (catch 'unwind-smoke-tag
                    (condition-case err
                        (progn
                          (nelisp-eln-native-subr--unary-bridge
                           0 (list (concat "unwind-" (symbol-name mode))
                                   (cons 2 3)))
                          'returned)
                      (error (list 'error err))
                      (quit (list 'quit err)))))
                 (state
                  (list (length nelisp-eln-objects--live-units)
                        (length nelisp-eln-objects--activations)
                        (length nelisp-eln-objects--identity-records)
                        (length nelisp-eln-objects--pending-cleanups)
                        (length nelisp-eln-raw-call--pending-cleanup))))
            (unless (eq (car case) 'throw)
              (unless (equal result (cdr case))
                (error "wrong primary outcome for %S: %S" mode result)))
            (when (eq (car case) 'throw)
              (unless (eq result 'thrown)
                (error "throw outcome changed: %S" result)))
            (unless (equal (nreverse released) '(activation objects context))
              (error "owners not all released for %S: %S" mode released))
            (unless (and (> mappings 0)
                         (eq (aref saved-activation 1) 'closed)
                         (eq (aref saved-objects 1) 'closed)
                         (eq (aref saved-context 4) t)
                         (equal state baseline))
              (error "owner state leaked for %S: mappings=%d state=%S baseline=%S"
                     mode mappings state baseline))))
        (princ "NELISP_UNARY_BRIDGE_ERROR_THROW_QUIT_CLEANUP=PASS\n"))
    (fset 'nelisp-eln-raw-call-word word-base)
    (fset 'nelisp-eln-objects-activation-release activation-base)
    (fset 'nelisp-eln-objects-release objects-base)
    (fset 'nelisp-eln-raw-call-context-release context-base)
    (fset 'nl-ffi-memory-release memory-base)))

;;; nelisp-eln-native-subr-unwind-smoke.el ends here
