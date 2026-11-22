;;; nelisp-native-load-cc-test.el --- native condition-case, known defect  -*- lexical-binding: t; -*-

;; A native `condition-case' does not catch a signal raised by a builtin it
;; calls: since the signal-stash flip (6fa5b5116) the pending exit lives in
;; the BSS link symbol nl_exit_values, but the native handler still reads the
;; stale arena stash.  The loader (nelisp-native-load-call) was fixed in
;; F112; the handler needs the bridged symbol, i.e. a rebuild.  This test is
;; marked :expected-result :failed so it documents the defect and turns into
;; an unexpected pass (remove the marker) once the batch build fixes it.
;; Needs NELISP_AOT_DIFF_HOME.  Item: $S/defects/batch-build-checklist.md.

(require 'ert)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(ert-deftest nelisp-native-load-cc/condition-case-catches-builtin-signal ()
  :expected-result :failed
  (let ((home (getenv "NELISP_AOT_DIFF_HOME")))
    (unless (and home (not (string-empty-p home)))
      (ert-skip "NELISP_AOT_DIFF_HOME unset"))
    (unless (file-executable-p (expand-file-name "target/nelisp" home))
      (error "NELISP_AOT_DIFF_HOME=%s has no executable target/nelisp" home))
    (let* ((dir (make-temp-file "cc" t))
           (src (expand-file-name "cc.el" dir))
           (art (expand-file-name "cc.neln" dir))
           (probe (expand-file-name "probe.el" dir))
           (lisp (file-name-directory (locate-library "nelisp-native-load.el"))))
      (unwind-protect
          (progn
            (with-temp-file src
              (insert "(defun cc-try (a) (condition-case e (+ a 1) (error (list 'caught (car e)))))\n"))
            (nelisp-artifact-compile-file src art nil nil nil nil nil 'neln 'required)
            (with-temp-file probe
              (insert (format "(load %S nil t)\n" (expand-file-name "nelisp-native-load.el" lisp)))
              (insert (format "(princ (format \"R %%S\\n\" (condition-case e (nelisp-native-load-call (nelisp-native-load-artifact %S \"cc-try\") (list 'x)) (error (list 'ESCAPED (car e))))))\n" art)))
            (let ((out (with-temp-buffer
                         (let ((default-directory (file-name-as-directory home)))
                           (call-process "bash" nil t nil "-c"
                                         (format "ulimit -v 8388608; exec timeout 300 target/nelisp --load %s -- < /dev/null"
                                                 (shell-quote-argument probe))))
                         (buffer-string))))
              ;; GNU answer: (caught wrong-type-argument).  Today: ESCAPED.
              (should (string-match-p "^R (caught wrong-type-argument)$" out))))
        (delete-directory dir t)))))

(provide 'nelisp-native-load-cc-test)
