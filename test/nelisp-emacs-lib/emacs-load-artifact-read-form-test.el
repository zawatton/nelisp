;;; emacs-load-artifact-read-form-test.el --- Focused ERT for artifact list reads  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-artifact-read-form-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-load-artifact-read-form-test--source-file
  (expand-file-name "src/emacs-load.el"
                    emacs-load-artifact-read-form-test--root))

;; `src/emacs-load.el' only installs its standalone `load'/`load-file'
;; override while its own top-level gate reads standalone (see its
;; `(when (or ... (not (stringp emacs-version))) ...)'); the `let' below
;; fakes that for this one nested `load-file' call so the internal
;; artifact-cache helpers below become available.  `defun' mutates the
;; global function cell, so it is not undone merely by `emacs-version'
;; unwinding — without explicitly restoring `load'/`load-file' to the
;; host originals captured here, they stay overridden with the
;; standalone implementation for the rest of this Emacs process, which
;; then crashes any later `require'/`load' of a feature not yet
;; provided (its `emacs-load--resolve-file' calls
;; `emacs-fns--load-candidate-suffixes', a helper `src/emacs-fns.el'
;; only installs under the same standalone gate, so it is void here).
(let ((host-load (symbol-function 'load))
      (host-load-file (symbol-function 'load-file)))
  (let ((emacs-version 1)
        (native-comp-enable-subr-trampolines nil))
    (funcall host-load-file emacs-load-artifact-read-form-test--source-file))
  (fset 'load host-load)
  (fset 'load-file host-load-file))

(defun emacs-load-artifact-read-form-test--standalone-active-p ()
  "Non-nil when the standalone emacs-load override is active."
  (or (fboundp 'rdf)
      (fboundp 'nelisp--eval-source-string)
      (fboundp 'emacs-load--artifact-load-or-compile)))

(defun emacs-load-artifact-read-form-test--collect-item-ranges (source start end)
  "Return top-level item ranges inside list SOURCE from START to END."
  (let ((pos (emacs-load--artifact-source-skip-ws-comments source start))
        (ranges nil))
    (should (< pos end))
    (should (= (emacs-load--artifact-byte-at source pos) ?\())
    (setq pos (1+ pos))
    (while (progn
             (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
             (< pos end))
      (when (= (emacs-load--artifact-byte-at source pos) ?\))
        (setq pos end))
      (when (< pos end)
        (let ((item-start pos)
              (item-end (emacs-load--artifact-source-form-end source pos)))
          (push (cons item-start item-end) ranges)
          (setq pos item-end))))
    (nreverse ranges)))

(ert-deftest emacs-load-artifact-read-form-test/large-list-fallback-uses-sliced-single-form-helper-per-item ()
  (skip-unless (emacs-load-artifact-read-form-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (emacs-load-artifact-native-sections-native-reader-threshold 8)
        (path "/tmp/emacs-load-artifact-read-form-test.neln")
        (source
         "(
  ; head comment
  (:alpha 1)
  ; middle comment
  (:beta 2 :nested (3 4))
  ; tail comment
  (:gamma 3)
)")
        (read-calls nil)
        (full-read-calls 0)
        (single-form-calls 0)
        (native-calls 0)
        (batch-fn nil))
    (when (fboundp 'nelisp--read-batch-from-string-native)
      (setq batch-fn (symbol-function 'nelisp--read-batch-from-string-native))
      (fmakunbound 'nelisp--read-batch-from-string-native))
    (unwind-protect
        (let ((expected-ranges
               (emacs-load-artifact-read-form-test--collect-item-ranges
                source 0 (length source)))
              (orig-read-from-string (symbol-function 'read-from-string)))
          (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                     (lambda (&rest _args)
                       (setq native-calls (1+ native-calls))
                       (error "native whole-list path must not be called")))
                    ((symbol-function 'emacs-load--artifact-source-read-form)
                     (lambda (&rest _args)
                       (setq full-read-calls (1+ full-read-calls))
                       (error "full-source read helper must not be called")))
                    ((symbol-function 'emacs-load--artifact-source-read-single-form-value)
                     (lambda (&rest _args)
                       (setq single-form-calls (1+ single-form-calls))
                       (error "single-form helper must not be called")))
                    ((symbol-function 'read-from-string)
                     (lambda (string &optional start end)
                       (push (list string start end) read-calls)
                       (funcall orig-read-from-string string start end))))
            (should (equal (emacs-load--artifact-source-read-form-nil-or-list-value
                            source (cons 0 (length source)) path :module-init)
                           '((:alpha 1)
                             (:beta 2 :nested (3 4))
                             (:gamma 3))))
            (should (= native-calls 0))
            (should (= full-read-calls 0))
            (should (= single-form-calls 0))
            (let ((calls (nreverse read-calls)))
              (should (= (length calls) (length expected-ranges)))
              (should (equal (mapcar #'car calls)
                             (mapcar (lambda (range)
                                       (substring source (car range) (cdr range)))
                                     expected-ranges)))
              (should (equal (mapcar #'cadr calls)
                             (make-list (length expected-ranges) 0)))
              (should (equal (mapcar #'cl-caddr calls)
                             (mapcar (lambda (range)
                                       (- (cdr range) (car range)))
                                     expected-ranges))))))
      (when batch-fn
        (fset 'nelisp--read-batch-from-string-native batch-fn)))))

(provide 'emacs-load-artifact-read-form-test)

;;; emacs-load-artifact-read-form-test.el ends here
