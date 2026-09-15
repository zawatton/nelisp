;;; nelisp-native-frame-kind-build.el --- Private native frame fixture -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-standalone-build)
(require 'nelisp-cc-frame-stack-find)

(defun nelisp-native-frame-kind-test-build ()
  "Build a private reader fixture without adding product builtin names.
NELISP_STANDALONE_READER_OUTPUT must select a separate test executable.
NELISP_FRAME_BASELINE_SOURCE optionally loads an earlier canonical unit for
against-the-bug capture checks; missing kind-search support remains missing."
  (unless (getenv "NELISP_STANDALONE_READER_OUTPUT")
    (error "Select an isolated NELISP_STANDALONE_READER_OUTPUT"))
  (when (equal (file-truename (nelisp-standalone--output-path t))
               (file-truename (expand-file-name "target/nelisp" nelisp-standalone--repo-root)))
    (error "The frame fixture must not replace target/nelisp"))
  (when-let* ((baseline (getenv "NELISP_FRAME_BASELINE_SOURCE")))
    (load (expand-file-name baseline) nil t))
  (let* ((kind-search
          (cl-find-if (lambda (form)
                        (eq (cadr form) 'nelisp_frame_stack_find_kind))
                      (cdr nelisp-cc-frame-stack-find--source)))
         (arms
          (append
           '(((:lit "frame-test-cell") . (seq (cell-make (wf_arg_ptr args 0) out) 0))
             ((:lit "frame-test-value") . (nl_cell_get_value (wf_arg_ptr args 0) out))
             ((:lit "frame-test-capture") . (nl_capture_descend_native (wf_arg_ptr args 0) out)))
           (when (cl-find-if (lambda (form) (eq (cadr form) 'nelisp_frame_local_declare))
                             (cdr nelisp-cc-frame-stack-find--source))
             '(((:lit "frame-test-declare") .
                (wf_write_int out (nelisp_frame_local_declare
                                   (wf_arg_ptr args 0) (wf_arg_ptr args 1))))
               ((:lit "frame-test-local-special") .
                (if (= (nelisp_frame_local_special_p
                        (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                    (wf_write_t out) (wf_write_nil out)))))
           (when kind-search
             '(((:lit "frame-test-find") .
                (let* ((cell (nelisp_frame_stack_find_kind
                              (wf_arg_ptr args 0) (wf_arg_ptr args 1)
                              (wf_argval args 2) 0)))
                  (if (= cell 0) (wf_write_nil out)
                    (nl_cell_get_value cell out))))))))
         (nelisp-standalone--applyfn-bf-arms
          (append arms nelisp-standalone--applyfn-bf-arms))
         (nelisp-standalone--reader-builtins
          (append (mapcar (lambda (arm) (cadar arm)) arms)
                  nelisp-standalone--reader-builtins)))
    (nelisp-standalone-build-reader)))

;;; nelisp-native-frame-kind-build.el ends here
