;;; nelisp-runtime-reload-telemetry-test.el --- source transformer tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../scripts/nelisp-runtime-reload-telemetry.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))) nil t)

(ert-deftest nelisp-runtime-reload-telemetry/replaces-only-executable-clears ()
  (let* ((source
          '(seq
            (defun nl_gc_conserv_begin ()
              (seq (nl_gc_conserv_state_clear) 1))
            (defun nl_gc_conserv_state_clear ()
              (seq (nl_gc_conserv_state_clear) 2))
            (defun worker ()
              (seq (nl_gc_conserv_state_clear)
                   (quote (nl_gc_conserv_state_clear))
                   (function nl_gc_conserv_state_clear)
                   (function (lambda () (nl_gc_conserv_state_clear)))
                   (nl_gc_conserv_state_clear 1)
                   0))))
         (actual (nelisp-runtime-reload-instrument-gc source))
         (begin (nth 1 actual))
         (clear (nth 2 actual))
         (worker (nth 3 actual))
         (finish (car (last actual))))
    (should (equal begin (nth 1 source)))
    (should (equal clear (nth 2 source)))
    (should (equal (nth 1 (nth 3 worker))
                   '(nl_runtime_reload_gc_finish)))
    (should (equal (nth 2 (nth 3 worker))
                   '(quote (nl_gc_conserv_state_clear))))
    (should (equal (nth 3 (nth 3 worker))
                   '(function nl_gc_conserv_state_clear)))
    (should (equal (nth 4 (nth 3 worker))
                   '(function (lambda () (nl_runtime_reload_gc_finish)))))
    (should (equal (nth 5 (nth 3 worker))
                   '(nl_gc_conserv_state_clear 1)))
    (should (equal (car finish) 'defun))
    (should (eq (cadr finish) 'nl_runtime_reload_gc_finish))
    (should (= (cl-count 'nl_runtime_reload_gc_finish actual
                         :key (lambda (form) (and (consp form) (cadr form))))
               1))))

(ert-deftest nelisp-runtime-reload-telemetry/is-idempotent-and-pure ()
  (let* ((source '(seq (defun worker () (nl_gc_conserv_state_clear))))
         (once (nelisp-runtime-reload-instrument-gc source))
         (twice (nelisp-runtime-reload-instrument-gc once)))
    (should (equal source
                   '(seq (defun worker () (nl_gc_conserv_state_clear)))))
    (should (equal once twice))))

;;; nelisp-runtime-reload-telemetry-test.el ends here
