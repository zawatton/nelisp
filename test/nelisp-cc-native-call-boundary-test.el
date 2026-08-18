;;; nelisp-cc-native-call-boundary-test.el --- ERT for native call boundary source  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(setq load-prefer-newer t)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc-native-call-boundary)

(defconst nelisp-cc-native-call-boundary-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir
         (file-name-directory
          (directory-file-name test-dir))))
  "Absolute path to the repo root.")

(load-file
 (expand-file-name "scripts/nelisp-standalone-build.el"
                   nelisp-cc-native-call-boundary-test--repo-root))

(defun nelisp-cc-native-call-boundary-test--walk (form pred)
  "Return the first FORM in the tree for which PRED returns non-nil."
  (if (funcall pred form)
      form
    (when (consp form)
      (catch 'nelisp-cc-native-call-boundary-test--found
        (dolist (elt form)
          (let ((hit (nelisp-cc-native-call-boundary-test--walk elt pred)))
            (when hit
              (throw 'nelisp-cc-native-call-boundary-test--found hit))))
        nil))))

(defun nelisp-cc-native-call-boundary-test--defun (name)
  "Return the quoted DEFUN form for NAME from the source datum."
  (nelisp-cc-native-call-boundary-test--walk
   nelisp-cc-native-call-boundary--source
   (lambda (form)
     (and (consp form)
          (eq (car form) 'defun)
          (eq (cadr form) name)))))

(defun nelisp-cc-native-call-boundary-test--ensure-frame-helpers ()
  "Load the pure helper defuns from the quoted source datum."
  (unless (fboundp 'bf_native_call_boundary--frame-bytes)
    (eval (nelisp-cc-native-call-boundary-test--defun
           'bf_native_call_boundary--even-round))
    (eval (nelisp-cc-native-call-boundary-test--defun
           'bf_native_call_boundary--frame-bytes))))

(defun nelisp-cc-native-call-boundary-test--build-script-source ()
  "Return the standalone build script as a literal source string."
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "scripts/nelisp-standalone-build.el"
                       nelisp-cc-native-call-boundary-test--repo-root))
    (buffer-string)))

(ert-deftest nelisp-cc-native-call-boundary/source-is-seq-and-defun-present ()
  "The source datum is a `seq' and includes `bf_native_call_boundary'."
  (let* ((source nelisp-cc-native-call-boundary--source)
         (text (prin1-to-string source)))
    (should (consp source))
    (should (eq (car source) 'seq))
    (should (nelisp-cc-native-call-boundary-test--defun
             'bf_native_call_boundary))
    (should (string-match-p "\\<bf_native_call_boundary\\>" text))
    (should-not (string-match-p "\\<list\\>" text))
    (should-not (string-match-p "\\<ptr-call\\>" text))
    (should-not (string-match-p "&rest" text))))

(ert-deftest nelisp-cc-native-call-boundary/reader-integration-is-structural ()
  "The standalone reader wires the native boundary source only in reader paths."
  (let ((source (nelisp-cc-native-call-boundary-test--build-script-source)))
    (should (string-match-p
             (regexp-quote "(require 'nelisp-cc-native-call-boundary)")
             source))
    (should (string-match-p
             (regexp-quote "(cdr nelisp-cc-native-call-boundary--source)")
             source))
    (should (= (cl-count "nelisp--native-call-boundary"
                         nelisp-standalone--reader-builtins
                         :test #'equal)
               1))
    (should (= (cl-count '((:lit "nelisp--native-call-boundary")
                           . (bf_native_call_boundary args env out))
                         nelisp-standalone--applyfn-bf-arms
                         :test #'equal)
               1))
    (should-not (string-match-p
                 (regexp-quote "nelisp--native-call-boundary")
                 (prin1-to-string nelisp-standalone--applyfn-baked-source)))))

(ert-deftest nelisp-cc-native-call-boundary/hidden-slots-use-arity-plus-index ()
  "Hidden slots are emitted from `arity + index' with no odd-padding start."
  (let ((text (prin1-to-string
               (nelisp-cc-native-call-boundary-test--defun
                'bf_native_call_boundary))))
    (should (string-match-p
             (regexp-quote "(bf_native_call_boundary--slot-disp (+ arity 0))")
             text))
    (should (string-match-p
             (regexp-quote "(bf_native_call_boundary--slot-disp (+ arity 16))")
             text))
    (should-not (string-match-p
                 (regexp-quote "bf_native_call_boundary--emit-hidden-slots")
                 text))
    (should-not (string-match-p
                 (regexp-quote "if (= (logand arity 1) 1) 1 0")
                 text))))

(ert-deftest nelisp-cc-native-call-boundary/success-path-boxes-result-before-zero ()
  "Success path calls native code, unmaps, boxes its result, then returns 0."
  (let* ((text (prin1-to-string
                (nelisp-cc-native-call-boundary-test--defun
                 'bf_native_call_boundary)))
         (call-pos (string-match (regexp-quote "(call-ptr page") text))
         (sys-pos (and call-pos
                       (string-match
                        (regexp-quote
                         "(syscall-direct 11 page 4096 0 0 0 0)")
                        text
                        call-pos)))
         (box-pos (and sys-pos
                       (string-match
                        (regexp-quote "(wf_write_int out native-result)")
                        text
                        sys-pos))))
    (should call-pos)
    (should sys-pos)
    (should box-pos)
    (should (< call-pos sys-pos))
    (should (< sys-pos box-pos))
    (should (string-match-p (regexp-quote "(wf_argval args 3)") text))))

(ert-deftest nelisp-cc-native-call-boundary/frame-bytes-authoritative ()
  "Frame helper matches the expected values for arity 0, 1, and 6 with rt 18."
  (nelisp-cc-native-call-boundary-test--ensure-frame-helpers)
  (should (= (bf_native_call_boundary--frame-bytes 0 18) 144))
  (should (= (bf_native_call_boundary--frame-bytes 1 18) 160))
  (should (= (bf_native_call_boundary--frame-bytes 6 18) 192)))

(ert-deftest nelisp-cc-native-call-boundary/standalone-reader-fboundp-is-true ()
  "The built standalone reader reports the native boundary as a builtin."
  (let* ((bin nelisp-standalone--reader-out)
         (stdout-buf (generate-new-buffer " *nelisp-native-call-boundary-stdout*"))
         (stderr-file (make-temp-file "nelisp-native-call-boundary-stderr"))
         (exit-code nil))
    (unwind-protect
        (progn
          (should (file-exists-p bin))
          (setq exit-code
                (call-process bin nil (list stdout-buf stderr-file) nil
                              "--eval" "(fboundp 'nelisp--native-call-boundary)"))
          (should (= exit-code 0))
          (should (string-match-p
                   "\\`[[:space:]\n]*\\'"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (buffer-substring-no-properties (point-min) (point-max)))))
          (should (eq (car (read-from-string
                            (with-current-buffer stdout-buf
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
                      t)))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

;;; nelisp-cc-native-call-boundary-test.el ends here
