;;; nelisp-eln-s610-scenarios.el --- Doc 210 S10.3 shared byte-compile-form scenarios -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Scenarios that drive the genuine `byte-compile-form' native body
;; (gnu-byte-compile-form.eln) through its guarded `condition-case' regions
;; and through non-local exits, and print a transcript.  The same file is
;; loaded by host GNU Emacs 31.1 (test/nelisp-eln-s610-host.el, where the
;; artifact is native code loaded by GNU's own loader) and by the NeLisp
;; driver (test/nelisp-eln-s610-driver.el, where the same native code runs
;; against the Doc 210 shadow handler blocks).  Every transcript line begins
;; with `T ' and the two transcripts must be byte-identical.
;;
;; The platform defines, before calling `s10-run-all':
;;   (s10-prepare)       one-time setup (NeLisp flags `most-positive-fixnum'
;;                       constant like GNU does)
;;   (s10-after NAME)    per-scenario platform checks (not part of the T lines)
;; and loads test/fixtures/s6-corpus/byte-compile-form.wrapper.el, which
;; provides `s6-corpus--byte-compile-env'.

;;; Code:

(defvar s10-marker nil)

(defun s10-run (name thunk)
  "Run THUNK as scenario NAME and print its transcript."
  (setq s10-marker nil)
  (let ((outcome
         (condition-case e
             (list 'value (catch 's10-tag (funcall thunk)))
           (error (list 'error e))
           (quit (list 'quit)))))
    (princ (format "T scenario %s\n" name))
    (princ (format "T   outcome: %S\n" outcome))
    (princ (format "T   marker: %S\n" s10-marker))
    (s10-after name)))

(defun s10-with-function (symbol function thunk)
  "Call THUNK with SYMBOL's function cell temporarily set to FUNCTION."
  (let ((saved (symbol-function symbol)))
    (unwind-protect
        (progn (fset symbol function) (funcall thunk))
      (fset symbol saved))))

(defun s10-form (form &optional for-effect)
  "Compile FORM with `byte-compile-form' in the corpus environment."
  (let ((byte-compile-warnings nil))
    (s6-corpus--byte-compile-form form for-effect)))

(defun s10-restore-check (form)
  "Run `byte-compile-form' on FORM under distinctive outer bindings of the
two specials it binds, catch whatever leaves it (error, quit or throw) and
report the result and the bindings afterwards."
  (s6-corpus--byte-compile-env
   (let ((byte-compile--for-effect 'outer-effect)
         (byte-compile-form-stack '(outer-stack))
         (byte-compile-warnings nil)
         (result nil))
     (setq result
           (catch 's10-tag
             (condition-case e
                 (list 'returned (byte-compile-form form nil))
               (error (list 'error e))
               (quit (list 'quit)))))
     (setq s10-marker
           (list result byte-compile--for-effect byte-compile-form-stack))
     'done)))

(defun s10-raise (&rest _)
  (signal 'error '("s10 boom")))

(defun s10-quit (&rest _)
  (signal 'quit nil))

(defun s10-throw (&rest _)
  (throw 's10-tag 'thrown-through-native))

(defun s10-run-all ()
  (s10-prepare)
  ;; 1. no handler is entered for a plain constant.
  (s10-run "constant" (lambda () (s10-form 42)))
  ;; 2. a bound non-constant variable in head position enters the guarded
  ;;    region: push_handler, Fsymbol_value, Fset, the normal-path pop.
  (s10-run "bound-symbol-head" (lambda () (s10-form '(load-path 1))))
  ;; 3. a constant symbol in head position: Fset signals `setting-constant',
  ;;    the native handler catches it, the landing pad pops the handler and
  ;;    the body carries on with the handler value `t'.
  (s10-run "constant-symbol-head"
           (lambda () (s10-form '(most-positive-fixnum 1))))
  ;; 4. the same twice in a row: nothing is left behind by the first.
  (s10-run "constant-symbol-head-twice"
           (lambda ()
             (list (s10-form '(most-positive-fixnum 1))
                   (s10-form '(most-positive-fixnum 2)))))
  ;; 5. an error raised by a callee leaves the native frame; both specbinds
  ;;    of the body are undone.
  (s10-run "error-restores-specbinds"
           (lambda ()
             (s10-with-function 'byte-compile-constant #'s10-raise
                                (lambda () (s10-restore-check 42)))))
  ;; 6. a throw through the native frame.
  (s10-run "throw-restores-specbinds"
           (lambda ()
             (s10-with-function 'byte-compile-constant #'s10-throw
                                (lambda () (s10-restore-check 42)))))
  ;; 7. a quit through the native frame.
  (s10-run "quit-restores-specbinds"
           (lambda ()
             (s10-with-function 'byte-compile-constant #'s10-quit
                                (lambda () (s10-restore-check 42)))))
  ;; 8. a caught error first (the handler lands and pops), then a callee
  ;;    error leaves the frame: the specbinds are restored after a caught
  ;;    error and the handler chain holds nothing.
  (s10-run "caught-then-error-restores"
           (lambda ()
             (s10-with-function 'byte-compile-normal-call #'s10-raise
                                (lambda ()
                                  (s10-restore-check '(most-positive-fixnum 1))))))
  ;; 9. a spread of ordinary forms: for-effect, strings, vectors, calls,
  ;;    special forms.
  (s10-run "mixed-forms"
           (lambda ()
             (mapcar (lambda (entry) (apply #'s10-form entry))
                     '((42 t) (some-var t) ("str" nil) ([1 2] nil) (nil t)
                       ((car x) nil) ((car x) t) ((if a b c) nil)
                       ((setq y 3) nil) ((quote (1 2)) nil)
                       ((function car) nil)))))
  ;; 10. and once more a plain compile after all of the above.
  (s10-run "constant-after" (lambda () (s10-form 42)))
  (princ "T end\n"))

(provide 'nelisp-eln-s610-scenarios)

;;; nelisp-eln-s610-scenarios.el ends here
