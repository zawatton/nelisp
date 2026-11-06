;;; nelisp-native-application-selection-test.el --- selected preflight -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-native-application-producer)
(require 'nelisp-native-application-build)

(defvar nnap-selection-sentinel nil)

(defun nnap-selection-test--digest-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nnap-selection-test--fixture ()
  (let* ((directory (make-temp-file "nnap-selection-" t))
         (module (expand-file-name "module.el" directory))
         (manifest-file (expand-file-name "manifest.el" directory))
         (names-file (expand-file-name "names.txt" directory))
         (selected (intern (make-temp-name "nnap-selected-")))
         (excluded (intern (make-temp-name "nnap-excluded-")))
         (forms (list `(defun ,selected (value)
                        (setq nnap-selection-sentinel :selected) value)
                      `(defun ,excluded (value)
                        (setq nnap-selection-sentinel :excluded) value)
                      '(setq nnap-selection-sentinel :executed)
                      '(defvar nnap-selection-sentinel :initialized)
                      '(defmacro nnap-selection-macro ()
                         (setq nnap-selection-sentinel :macro))
                      '(require 'nnap-selection-missing-feature)
                      '(main)))
         pin manifest)
    (with-temp-file module
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (dolist (form forms) (prin1 form (current-buffer)) (terpri)))
    (setq pin (list :file module :sha256 (nnap-selection-test--digest-file module)
                    :limit nelisp-native-application-build-max-file-bytes)
          manifest
          (list :schema 1 :kind :application-intake
                :baseline-sha256
                (nelisp-cc-fxir-admission--digest
                 (nelisp-cc-fxir-native-all-rows))
                :modules (list pin) :names (list selected)
                :definitions
                (list (list :name selected :source pin
                            :form-sha256
                            (nelisp-cc-fxir-admission--digest (car forms))))
                :rejections
                (list (list :name excluded :reason 'excluded-by-request
                            :source pin :form-sha256
                            (nelisp-cc-fxir-admission--digest (cadr forms))))))
    (list :directory directory :module module :manifest-file manifest-file
          :names-file names-file :selected selected :excluded excluded
          :forms forms :manifest manifest :pin pin)))

(defun nnap-selection-test--write (fixture manifest)
  (with-temp-file (plist-get fixture :manifest-file)
    (prin1 manifest (current-buffer)))
  (with-temp-file (plist-get fixture :names-file)
    (insert (symbol-name (plist-get fixture :selected)) "\n")))

(defun nnap-selection-test--preflight (fixture &optional mode)
  (let ((manifest-file (plist-get fixture :manifest-file))
        (names-file (plist-get fixture :names-file)))
    (nelisp-native-application-producer-preflight
     manifest-file names-file (nnap-selection-test--digest-file manifest-file)
     (nnap-selection-test--digest-file names-file) mode)))

(ert-deftest nnap-selection-admits-only-authenticated-defuns-and-describes-skips ()
  (let* ((fixture (nnap-selection-test--fixture))
         (selected (plist-get fixture :selected))
         (excluded (plist-get fixture :excluded)))
    (unwind-protect
        (progn
          (nnap-selection-test--write fixture (plist-get fixture :manifest))
          (should-error (nnap-selection-test--preflight fixture))
          (let ((plan (nnap-selection-test--preflight
                       fixture :selected-definitions)))
            (should (eq (plist-get plan :mode) :selected-definitions))
            (should (eq (plist-get plan :context-policy)
                        :pinned-producer-baseline))
            (should (equal (plist-get plan :names) (list selected)))
            (should (equal (cdar (plist-get plan :forms)) (car (plist-get fixture :forms))))
            (should (equal (plist-get plan :omitted-definition-names) (list excluded)))
            (should (= (plist-get plan :source-form-count) 2))
            (should (= (plist-get plan :selected-definition-count) 1))
            (should (= (plist-get plan :excluded-definition-count) 1))
            (should (= (plist-get plan :skipped-topform-count) 5))
            (should (= (plist-get (car (plist-get plan :skipped-topforms)) :ordinal) 3))
            (cl-loop for descriptor in (plist-get plan :skipped-topforms)
                     for form in (cddr (plist-get fixture :forms))
                     for ordinal from 3 do
                     (should (equal (plist-get descriptor :source)
                                    (plist-get fixture :pin)))
                     (should (= (plist-get descriptor :ordinal) ordinal))
                     (should (equal (plist-get descriptor :form-sha256)
                                    (nelisp-cc-fxir-admission--digest form)))
                     (should (equal (plist-get descriptor :head)
                                    (symbol-name (car form)))))
            (should-not (plist-get plan :module-initialization-executed))
            (should-not (plist-get plan :whole-module-initialization-equivalence))
            (should-not (bound-and-true-p nnap-selection-sentinel))
            (should-not (fboundp 'nnap-selection-macro))
            (should-not (fboundp selected))
            (should-not (fboundp excluded)))
          (should-error (nnap-selection-test--preflight fixture :unknown)))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-rejects-overlap-wrong-reason-and-stale-records ()
  (let* ((fixture (nnap-selection-test--fixture))
         (manifest (plist-get fixture :manifest))
         (selected (plist-get fixture :selected))
         (bad-reason (copy-tree manifest))
         (overlap (copy-tree manifest))
         (stale-hash (copy-tree manifest))
         (wrong-source (copy-tree manifest)))
    (unwind-protect
        (progn
          (setf (plist-get (car (plist-get bad-reason :rejections)) :reason)
                :unsupported)
          (setf (plist-get (car (plist-get overlap :rejections)) :name) selected)
          (setf (plist-get (car (plist-get stale-hash :rejections)) :form-sha256)
                (make-string 64 ?0))
          (setf (plist-get (car (plist-get wrong-source :rejections)) :source)
                (list :file (plist-get fixture :module) :sha256 (make-string 64 ?a)
                      :limit nelisp-native-application-build-max-file-bytes))
          (dolist (bad (list bad-reason overlap stale-hash wrong-source))
            (nnap-selection-test--write fixture bad)
            (should-error (nnap-selection-test--preflight
                           fixture :selected-definitions))))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-rejects-missing-duplicate-and-swapped-records ()
  (let* ((fixture (nnap-selection-test--fixture))
         (original (plist-get fixture :manifest)))
    (unwind-protect
        (dolist (mutation '(:missing :duplicate-selected :duplicate-excluded
                           :stale-selected :swapped-hash :extra-excluded
                           :baseline :names :module))
          (let ((bad (copy-tree original)))
            (pcase mutation
              (:missing (setf (plist-get bad :rejections) nil))
              (:duplicate-selected
               (setf (plist-get bad :definitions)
                     (append (plist-get bad :definitions)
                             (copy-tree (plist-get bad :definitions)))))
              (:duplicate-excluded
               (setf (plist-get bad :rejections)
                     (append (plist-get bad :rejections)
                             (copy-tree (plist-get bad :rejections)))))
              (:stale-selected
               (setf (plist-get (car (plist-get bad :definitions)) :form-sha256)
                     (make-string 64 ?0)))
              (:swapped-hash
               (cl-rotatef
                (plist-get (car (plist-get bad :definitions)) :form-sha256)
                (plist-get (car (plist-get bad :rejections)) :form-sha256)))
              (:extra-excluded
               (let ((extra (copy-tree (car (plist-get bad :rejections)))))
                 (setf (plist-get extra :name) 'nnap-not-in-source)
                 (push extra (plist-get bad :rejections))))
              (:baseline (setf (plist-get bad :baseline-sha256) (make-string 64 ?0)))
              (:names (setf (plist-get bad :names) '(nnap-wrong-name)))
              (:module (setf (plist-get (car (plist-get bad :modules)) :sha256)
                             (make-string 64 ?0))))
            (nnap-selection-test--write fixture bad)
            (should-error (nnap-selection-test--preflight fixture :selected-definitions))))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-rejects-unaccounted-and-duplicate-source-defuns ()
  (dolist (duplicate '(nil t))
    (let* ((fixture (nnap-selection-test--fixture))
           (manifest (plist-get fixture :manifest))
           (module (plist-get fixture :module)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert-file-contents module)
              (goto-char (point-max))
              (prin1 (if duplicate (car (plist-get fixture :forms))
                       '(defun nnap-unaccounted () :never-run)) (current-buffer))
              (write-region (point-min) (point-max) module nil 'silent))
            ;; Reauthenticate module bytes to reach source accounting itself.
            (setf (plist-get (plist-get fixture :pin) :sha256)
                  (nnap-selection-test--digest-file module))
            (nnap-selection-test--write fixture manifest)
            (should-error (nnap-selection-test--preflight fixture :selected-definitions)))
        (delete-directory (plist-get fixture :directory) t)))))

(ert-deftest nnap-selection-never-evaluates-or-installs-application-forms ()
  (let* ((fixture (nnap-selection-test--fixture))
         (original-fset (symbol-function 'fset))
         ;; The pinned repository baseline reader evaluates its own quoted data.
         ;; Freeze that exact baseline before instrumenting application intake.
         (baseline (nelisp-cc-fxir-native-all-rows))
         (writes 0)
         plan observed-writes
         (nnap-selection-sentinel nil))
    (unwind-protect
        (progn
          (nnap-selection-test--write fixture (plist-get fixture :manifest))
          (cl-letf (((symbol-function 'nelisp-cc-fxir-native-all-rows)
                     (lambda () baseline))
                    ((symbol-function 'eval)
                     (lambda (&rest _) (error "Unexpected application evaluation")))
                    ((symbol-function 'fset)
                     (lambda (name definition)
                       (setq writes (1+ writes))
                       (funcall original-fset name definition))))
            (setq writes 0)
            (setq plan (nnap-selection-test--preflight fixture :selected-definitions)
                  observed-writes writes))
          (should (eq (plist-get plan :status) :validated-inputs))
          (should (= observed-writes 0))
          (should-not nnap-selection-sentinel)
          (should-not (fboundp (plist-get fixture :selected)))
          (should-not (fboundp (plist-get fixture :excluded))))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-rejects-swapped-authentic-module-descriptor ()
  (let* ((fixture (nnap-selection-test--fixture))
         (manifest (plist-get fixture :manifest))
         (other (expand-file-name "other.el" (plist-get fixture :directory))))
    (unwind-protect
        (progn
          (with-temp-file other (insert ";;; -*- lexical-binding: t; -*-\n"))
          (let ((pin (list :file other :sha256 (nnap-selection-test--digest-file other)
                           :limit nelisp-native-application-build-max-file-bytes)))
            (push pin (plist-get manifest :modules))
            (setf (plist-get (car (plist-get manifest :rejections)) :source) pin))
          (nnap-selection-test--write fixture manifest)
          (should-error (nnap-selection-test--preflight fixture :selected-definitions)))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-rejects-discovery-aliases-and-explicit-pin-drift ()
  (let ((fixture (nnap-selection-test--fixture)))
    (unwind-protect
        (progn
          (nnap-selection-test--write fixture (plist-get fixture :manifest))
          (let* ((manifest (plist-get fixture :manifest-file))
                 (names (plist-get fixture :names-file))
                 (manifest-hash (nnap-selection-test--digest-file manifest))
                 (names-hash (nnap-selection-test--digest-file names)))
            (should-error (nelisp-native-application-producer-preflight
                           manifest names (make-string 64 ?0) names-hash :selected-definitions))
            (should-error (nelisp-native-application-producer-preflight
                           manifest names manifest-hash (make-string 64 ?0) :selected-definitions))
            (should-error (nelisp-native-application-producer-preflight
                           manifest manifest manifest-hash manifest-hash :selected-definitions))
            (with-temp-file names (insert "nnap-tampered-names\n"))
            (should-error (nnap-selection-test--preflight fixture :selected-definitions))))
      (delete-directory (plist-get fixture :directory) t))))

(ert-deftest nnap-selection-preserves-request-order-in-renamed-reordered-source ()
  (let* ((fixture (nnap-selection-test--fixture))
         (manifest (plist-get fixture :manifest))
         (pin (plist-get fixture :pin))
         (second (intern (make-temp-name "nnap-renamed-later-")))
         (second-form `(defun ,second (argument) (list argument argument)))
         (original-selected (car (plist-get fixture :forms)))
         (source-forms (append (list (cadr (plist-get fixture :forms)) second-form
                                    original-selected)
                               (cddr (plist-get fixture :forms)))))
    (unwind-protect
        (progn
          (with-temp-file (plist-get fixture :module)
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (dolist (form source-forms) (prin1 form (current-buffer)) (terpri)))
          (setf (plist-get pin :sha256)
                (nnap-selection-test--digest-file (plist-get fixture :module)))
          (setf (plist-get manifest :definitions)
                (append (plist-get manifest :definitions)
                        (list (list :name second :source pin :form-sha256
                                    (nelisp-cc-fxir-admission--digest second-form)))))
          (setf (plist-get manifest :names)
                (list (plist-get fixture :selected) second))
          (nnap-selection-test--write fixture manifest)
          (with-temp-file (plist-get fixture :names-file)
            (dolist (name (plist-get manifest :names))
              (insert (symbol-name name) "\n")))
          (let ((plan (nnap-selection-test--preflight fixture :selected-definitions)))
            (should (equal (mapcar #'car (plist-get plan :forms))
                           (plist-get manifest :names)))
            (should (equal (mapcar #'cdr (plist-get plan :forms))
                           (list original-selected second-form)))
            (should-not (fboundp second))))
      (delete-directory (plist-get fixture :directory) t))))

(provide 'nelisp-native-application-selection-test)
;;; nelisp-native-application-selection-test.el ends here
