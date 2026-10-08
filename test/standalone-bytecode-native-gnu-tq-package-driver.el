;;; standalone-bytecode-native-gnu-tq-package-driver.el --- GNU tq package proof -*- lexical-binding: t; -*-
(require 'nelisp-bytecode-native-raw-package)
(let* ((phase (getenv "NELISP_TQ_PHASE"))
       (elc (getenv "NELISP_TQ_ELC"))
       (source (getenv "NELISP_TQ_SOURCE"))
       (package (getenv "NELISP_TQ_PACKAGE"))
       (manifest (expand-file-name "package.nraw" package))
       (expected (with-temp-buffer (insert (getenv "NELISP_TQ_ORACLE"))
                   (goto-char (point-min)) (read (current-buffer))))
       (input (cons (list 'queue) (cons 'process 'buffer))))
  (unless (and (not (file-exists-p source)) (file-readable-p elc)
               (not (featurep 'tq)) (not (fboundp 'tq-queue)))
    (error "GNU tq package phase is not source-free and cold"))
  (cond
   ((equal phase "writer")
    (let ((build (nelisp-bytecode-native-raw-package-compile-elc
                  elc 'tq 'tq-queue package)))
      (unless (eq (plist-get build :status) 'complete)
        (error "GNU tq raw package compile failed: %S" build))
      (condition-case nil
          (nelisp-bytecode-native-raw-package-compile-elc
           elc 'tq 'tq-enqueue (concat package "-unsupported"))
        (error nil))
      (when (file-exists-p (concat package "-unsupported"))
        (error "Unsupported tq function published output"))
      ;; Negative control proves the admission check protects backend access.
      (let ((backend-calls 0) (bypass (concat package "-guard-bypass")))
        (cl-letf (((symbol-function 'nelisp-bytecode-native-compiler-unary-template-operation)
                   (lambda (_input) 'car))
                  ((symbol-function 'nelisp-bytecode-native-compiler-build)
                   (lambda (&rest _args)
                     (setq backend-calls (1+ backend-calls))
                     (list :status 'unsupported))))
          (condition-case nil
              (nelisp-bytecode-native-raw-package-compile-elc
               elc 'tq 'tq-buffer bypass)
            (error nil)))
        (unless (and (= backend-calls 1) (not (file-exists-p bypass)))
          (error "Admission negative control failed: backend=%d" backend-calls)))
      (princ "GNU-TQ-WRITER: PASS (ELC data only; unsupported rejected)\n")))
   ((equal phase "reader")
    (unless (file-readable-p manifest) (error "Writer package is missing"))
    ;; Recompute the outer digest after adding an unknown raw import to the
    ;; inner artifact. Refusal must precede module feature/function effects.
    (let* ((outer (with-temp-buffer (insert-file-contents-literally manifest)
                    (goto-char (point-min)) (read (current-buffer))))
           (artifact (expand-file-name (plist-get outer :artifact) package))
           (artifact-bytes (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally artifact)
                             (buffer-string)))
           (outer-bytes (with-temp-buffer
                          (insert-file-contents-literally manifest)
                          (buffer-string)))
           (raw (nelisp-native-load-manifest artifact))
           (native (plist-get raw :native))
           (newline (string-match "\\n" artifact-bytes)))
      (unwind-protect
          (progn
            (unless (and native newline) (error "Malformed raw artifact fixture"))
            (setq raw (plist-put raw :native
                                 (plist-put native :extern-symbols
                                            '("nl_call1_smoke_unknown_import"))))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert (substring artifact-bytes 0 (1+ newline)))
              (prin1 raw (current-buffer))
              (write-region (point-min) (point-max) artifact nil 'silent))
            (setq outer (plist-put outer :artifact-sha256
                                   (nelisp-bytecode-native-package-raw-file-sha256
                                    artifact)))
            (with-temp-file manifest (prin1 outer (current-buffer)))
            (unless (condition-case nil
                        (progn (nelisp-bytecode-native-raw-package-open manifest) nil)
                      (error t))
              (error "Unknown inner import was admitted despite outer digest update"))
            (unless (and (not (featurep 'tq)) (not (fboundp 'tq-queue)))
              (error "Corrupt artifact was refused after module effects")))
        (with-temp-file artifact (insert artifact-bytes))
        (with-temp-file manifest (insert outer-bytes))))
    (let* ((first (nelisp-bytecode-native-raw-package-open manifest))
           (second (nelisp-bytecode-native-raw-package-open manifest)))
      (unless (and (featurep 'tq) (fboundp 'tq-queue)
                   (eq (nelisp-bytecode-native-raw-package-native-call-count first) 0))
        (error "Reader open was not cold/lazy"))
      (unless (and (equal (nelisp-bytecode-native-raw-package-call first input) expected)
                   (= (nelisp-bytecode-native-raw-package-native-call-count first) 1))
        (error "First native dispatch mismatched GNU oracle"))
      (let ((nelisp-bytecode-native-package-native-enabled nil))
        (nelisp-bytecode-native-raw-package-call first input)
        (unless (= (nelisp-bytecode-native-raw-package-native-call-count first) 1)
          (error "Disabled native path incremented dispatch count")))
      (let ((old (symbol-function 'tq-queue)))
        (unwind-protect
            (progn
              (fset 'tq-queue (lambda (x) (car (cdr x))))
              (unless (eq (nelisp-bytecode-native-raw-package-call first input) 'process)
                (error "Redefined function cell did not use VM"))
              (unless (= (nelisp-bytecode-native-raw-package-native-call-count first) 1)
                (error "VM call incremented native count")))
          (fset 'tq-queue old)))
      (nelisp-bytecode-native-raw-package-close first)
      (unless (condition-case nil
                  (progn (nelisp-bytecode-native-raw-package-call first input) nil)
                (error t))
        (error "Closed package handle remained callable"))
      (princ "GNU-TQ-READER: PASS (cold open, lazy native, VM controls, close)\n")))
   (t (error "Unknown GNU tq smoke phase: %S" phase))))
;;; standalone-bytecode-native-gnu-tq-package-driver.el ends here
