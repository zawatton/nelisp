;;; nelisp-argframe-generation-test.el --- Stable IDs and cache provenance -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun nelisp-argframe-generation--old-symbols ()
  "Compute the unchanged ordered input of the original static-symbol map."
  (delete-dups
   (cl-remove-if
    (lambda (name) (or (member name '("nil" "t"))
                       (> (length (encode-coding-string name 'utf-8 t)) 120)))
    (append nelisp-standalone--static-symbol-intercepts
            nelisp-standalone--static-symbol-specials
            (mapcar (lambda (entry) (cadar entry)) nelisp-standalone--applyfn-dispatch-table)
            (copy-sequence nelisp-standalone--static-symbol-extras)))))

(ert-deftest nelisp-argframe-static-map-preserves-every-old-id ()
  (let ((old (nelisp-argframe-generation--old-symbols))
        (all (nelisp-standalone--static-symbols)))
    (should (equal old (cl-subseq all 0 (length old))))
    (should (equal (nthcdr (length old) all) nelisp-argframe-static-symbol-suffix))
    (should (= (length nelisp-argframe-static-symbol-suffix)
               (length (delete-dups (copy-sequence nelisp-argframe-static-symbol-suffix)))))))

(ert-deftest nelisp-argframe-frozen-suffix-matches-canonical-admission ()
  (let ((nelisp-standalone--target 'linux-x86_64)
        (process-environment (copy-sequence process-environment)))
    (setenv "NELISP_READER_DYNAMIC" nil)
    (let* ((names (nelisp-argframe-admitted-names (nelisp-standalone--applyfn-guarded-reader-table)))
           (missing (cl-set-difference names (nelisp-argframe-generation--old-symbols) :test #'equal)))
      (should (equal (sort missing #'string<)
                     (sort (copy-sequence nelisp-argframe-static-symbol-suffix) #'string<))))))

(ert-deftest nelisp-argframe-every-target-admission-has-early-id ()
  (let ((old (nelisp-argframe-generation--old-symbols))
        (process-environment (copy-sequence process-environment)))
    (dolist (nelisp-standalone--target '(linux-x86_64 windows-x86_64 linux-aarch64 macos-aarch64))
      (dolist (dynamic '(nil "1"))
        (setenv "NELISP_READER_DYNAMIC" dynamic)
        (let* ((table (nelisp-standalone--applyfn-guarded-reader-table))
               (names (nelisp-argframe-admitted-names table))
               (all (nelisp-standalone--static-symbols)))
          (should (equal old (cl-subseq all 0 (length old))))
          (let ((mode-table (nelisp-argframe-mode-table table #'nelisp-standalone--sid)))
            (dolist (entry table)
              (unless (nelisp-standalone--sid (cadar entry))
                (should (equal (cdr (assoc (car entry) mode-table))
                               (cons :word `(if (= _pad 0) ,(cddr entry) 2)))))))
          (when (eq nelisp-standalone--target 'linux-x86_64)
            (should (cl-every #'nelisp-standalone--sid names)))
          (should (nelisp-standalone--argframe-admission-form 'probe_sid)))))))

(ert-deftest nelisp-argframe-module-is-a-toolchain-input ()
  (let* ((original (locate-library "nelisp-standalone-argframe.el"))
         (temp (make-temp-file "argframe-toolchain-" nil ".el"))
         (locator (symbol-function 'locate-library)))
    (unwind-protect
        (progn
          (copy-file original temp t)
          (should (member original (nelisp-standalone--dep-files)))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (name &rest rest)
                       (if (equal name "nelisp-standalone-argframe.el") temp
                         (apply locator name rest)))))
            (let* ((nelisp-standalone--toolchain-digest nil)
                   (before (nelisp-standalone--unit-cache-key '(defun probe (x) x))))
              (with-temp-buffer (insert-file-contents temp)
                                (goto-char (point-max)) (insert "\n;; provenance mutation\n")
                                (write-region nil nil temp nil 'silent))
              (setq nelisp-standalone--toolchain-digest nil)
              (should-not (equal before (nelisp-standalone--unit-cache-key '(defun probe (x) x)))))))
      (delete-file temp))))

(provide 'nelisp-argframe-generation-test)
;;; nelisp-argframe-generation-test.el ends here
