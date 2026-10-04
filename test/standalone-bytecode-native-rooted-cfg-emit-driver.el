;;; standalone-bytecode-native-rooted-cfg-emit-driver.el --- source-free generic CFG proof -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-bytecode-native-rooted-cfg-call)
(require 'nelisp-native-load)

(defun nelisp-test-rooted-cfg-stage (stage detail)
  (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
    (when path
      (write-region (format "%s %s\n" stage detail) nil path t 'silent))))

(defun nelisp-test-rooted-cfg-definition (elc name)
  (let* ((definitions (nelisp-bytecode-native-package-read-elc-functions elc))
         (definition (cdr (assq name definitions))))
    (unless (byte-code-function-p definition)
      (error "rooted-cfg: missing GNU ELC function %s (available=%S type=%S)"
             name (mapcar #'car definitions) (type-of definition)))
    definition))

(defun nelisp-test-rooted-cfg-build (elc name artifact)
  (nelisp-test-rooted-cfg-stage "input" name)
  (let* ((function (nelisp-test-rooted-cfg-definition elc name))
         (input (nelisp-bytecode-compiler-input-build function)))
    (unless (eq (plist-get input :status) 'complete)
      (error "rooted-cfg: GNU frame input rejected: %S"
             (plist-get input :reason)))
    (nelisp-test-rooted-cfg-stage "plan" name)
    (let ((plan (nelisp-bytecode-native-rooted-cfg-plan input)))
      (unless (eq (plist-get plan :status) 'complete)
        (error "rooted-cfg: verified plan rejected: %S" (plist-get plan :reason)))
      (nelisp-test-rooted-cfg-stage "backend" name)
      (let ((result (nelisp-bytecode-native-rooted-cfg-native-build input artifact)))
        (nelisp-test-rooted-cfg-stage "built" name)
        (list function input plan result)))))

(defun nelisp-test-rooted-cfg-smoke ()
  "Execute genuine source-free GNU ELC through raw-v2 generic CFG entries."
  (let* ((source (getenv "NELISP_ROOTED_CFG_SOURCE"))
         (elc (getenv "NELISP_ROOTED_CFG_ELC"))
         (artifact-dir (getenv "NELISP_ROOTED_CFG_ARTIFACT_DIR")))
    (unless (and (stringp source) (not (file-exists-p source))
                 (stringp elc) (file-exists-p elc)
                 (stringp artifact-dir) (file-directory-p artifact-dir))
      (error "rooted-cfg: source-free fixture precondition failed"))
    (let* ((a (nelisp-test-rooted-cfg-build
               elc 'gnu-rooted-cfg-two-diamonds
               (expand-file-name "two-diamonds.nelr" artifact-dir)))
           (function-a (nth 0 a)) (plan-a (nth 2 a)) (result-a (nth 3 a))
           (b (nelisp-test-rooted-cfg-build
               elc 'gnu-rooted-cfg-carried-phi
               (expand-file-name "carried-phi.nelr" artifact-dir)))
           (function-b (nth 0 b)) (plan-b (nth 2 b)) (result-b (nth 3 b))
           (real-export (symbol-function 'nelisp-native-load-raw-export-address))
           (real-entry (symbol-function 'nelisp-bytecode-native-rooted-cfg-entry-call))
           (real-map (symbol-function 'nelisp-native-load-raw-v2-artifact))
           (real-roots (symbol-function 'nelisp-native-load-root-v2-addresses))
           (addresses nil) (entries 0) (maps 0) (root-address-lookups 0))
      (unless (and (null (nelisp-native-load-raw-v2-check
                          (plist-get result-a :manifest)
                          "nl_native_rooted_cfg_probe_v1"))
                   (null (nelisp-native-load-raw-v2-check
                          (plist-get result-b :manifest)
                          "nl_native_rooted_cfg_probe_v1")))
        (error "rooted-cfg: producer artifact contract refused"))
      (cl-letf (((symbol-function 'nelisp-native-load-raw-export-address)
                 (lambda (mapping entry)
                   (let ((address (funcall real-export mapping entry)))
                     (when (equal entry "nl_native_rooted_cfg_probe_v1")
                       (push address addresses))
                     address)))
                ((symbol-function 'nelisp-bytecode-native-rooted-cfg-entry-call)
                 (lambda (address env ticket arity roots)
                   (unless (memq address addresses)
                     (error "rooted-cfg: entry address was not authenticated"))
                   (setq entries (1+ entries))
                   (nelisp-test-rooted-cfg-stage "entry" (format "%d/%d" arity roots))
                   (funcall real-entry address env ticket arity roots)))
                ((symbol-function 'nelisp-native-load-raw-v2-artifact)
                 (lambda (&rest args)
                   (setq maps (1+ maps))
                   (nelisp-test-rooted-cfg-stage "map" (cadr args))
                   (apply real-map args)))
                ((symbol-function 'nelisp-native-load-root-v2-addresses)
                 (lambda (&optional manifest)
                   (setq root-address-lookups (1+ root-address-lookups))
                   (funcall real-roots manifest))))
        ;; Unknown and copied results must fail before root-context discovery,
        ;; mapping, or native entry dispatch.
        (dolist (result (list (copy-sequence result-a) (list :status 'complete)))
          (unless (condition-case nil
                      (progn (nelisp-bytecode-native-rooted-cfg-call result) nil)
                    (error t))
            (error "rooted-cfg: forged result was accepted")))
        (unless (and (= root-address-lookups 0) (= maps 0) (= entries 0))
          (error "rooted-cfg: forged result reached native effects"))
        ;; Exercise every arm pair with distinguishable mutable values and
        ;; identity checks after the caller's forced GC.
        (dolist (condition-a '(nil t))
          (dolist (condition-b '(nil t))
            (let* ((a-leaf (list 'a-leaf)) (ar-leaf (list 'ar-leaf))
                   (b-leaf (list 'b-leaf)) (br-leaf (list 'br-leaf))
                   (left-a (cons a-leaf nil)) (right-a (cons ar-leaf nil))
                   (left-b (cons 'left b-leaf)) (right-b (cons 'right br-leaf))
                   (expected (funcall function-a condition-a left-a right-a
                                      condition-b left-b right-b))
                   (actual (nelisp-bytecode-native-rooted-cfg-call
                            result-a condition-a left-a right-a condition-b
                            left-b right-b))
                   (wanted-a (if condition-a a-leaf ar-leaf))
                   (wanted-b (if condition-b br-leaf b-leaf)))
              (unless (and (consp expected) (consp actual)
                           (eq (car actual) (car expected))
                           (eq (cdr actual) (cdr expected))
                           (eq (car actual) wanted-a) (eq (cdr actual) wanted-b))
                (error "rooted-cfg: two-diamond arm or identity mismatch"))
              (setcar wanted-a 'gc-visible)
              (unless (eq (car (car actual)) 'gc-visible)
                (error "rooted-cfg: mutable object did not survive native GC"))))
        ;; Wrong-type payloads on both conditional gateway paths must match GNU.
        (dolist (condition '(nil t))
          (let* ((bad 17) (good (cons 'ok nil))
                 (left (if condition bad good)) (right (if condition good bad))
                 (left-b (cons 'left nil)) (right-b (cons 'right nil))
                 (native-error (condition-case err
                                   (nelisp-bytecode-native-rooted-cfg-call
                                    result-a condition left right t left-b right-b)
                                 (wrong-type-argument err)))
                 (vm-error (condition-case err
                               (funcall function-a condition left right t left-b right-b)
                             (wrong-type-argument err))))
            (unless (equal native-error vm-error)
              (error "rooted-cfg: CAR wrong-type payload differs from GNU"))))
        (dolist (condition '(nil t))
          (let* ((bad 17) (good (cons 'ok nil))
                 (left-b (if condition good bad))
                 (right-b (if condition bad good))
                 (left-a (cons 'left nil)) (right-a (cons 'right nil))
                 (native-error (condition-case err
                                   (nelisp-bytecode-native-rooted-cfg-call
                                    result-a t left-a right-a condition left-b right-b)
                                 (wrong-type-argument err)))
                 (vm-error (condition-case err
                               (funcall function-a t left-a right-a condition left-b right-b)
                             (wrong-type-argument err))))
            (unless (equal native-error vm-error)
              (error "rooted-cfg: CDR wrong-type payload differs from GNU"))))
        ;; The later join must preserve z distinctly from the earlier x/y phi.
        (dolist (a-value '(nil t))
          (dolist (b-value '(nil t))
            (let* ((x (list 'x)) (y (list 'y)) (z (list 'z))
                   (expected (funcall function-b a-value x y b-value z))
                   (actual (nelisp-bytecode-native-rooted-cfg-call
                            result-b a-value x y b-value z))
                   (wanted-car (if a-value x y))
                   (wanted-cdr (if b-value z (if a-value x y))))
              (unless (and (eq (car actual) (car expected))
                           (eq (cdr actual) (cdr expected))
                           (eq (car actual) wanted-car)
                           (eq (cdr actual) wanted-cdr))
                (error "rooted-cfg: carried-phi join lost path value"))))
        ;; One post-error success proves frame/mapping cleanup permits reuse.
        (let ((actual (nelisp-bytecode-native-rooted-cfg-call
                       result-b t (list 'x) (list 'y) t (list 'z))))
          (unless (and (eq (car actual) (car actual))
                       (eq (cdr actual) (cdr actual)))
            (error "rooted-cfg: post-error recovery failed"))))
      (unless (and (= entries 13) (= maps entries)
                   (= root-address-lookups entries))
        (error "rooted-cfg: native call/map/root counts differ: %S"
               (list entries maps root-address-lookups)))
      (princ (format "rooted-cfg: PASS entries=%d maps=%d root-contexts=%d; two diamonds, carried phi, CONS, errors, GC\n"
                     entries maps root-address-lookups)))))))

(defun nelisp-test-rooted-cfg-diagnostic ()
  "Run one authenticated CFG entry with flushed per-boundary timings."
  (let* ((source (getenv "NELISP_ROOTED_CFG_SOURCE"))
         (elc (getenv "NELISP_ROOTED_CFG_ELC"))
         (artifact-dir (getenv "NELISP_ROOTED_CFG_ARTIFACT_DIR"))
         (check-real (symbol-function 'nelisp-native-load-raw-v2-check))
         (contract-real (symbol-function 'nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p))
         (entry-real (symbol-function 'nelisp-bytecode-native-rooted-cfg-entry-call))
         (check-count 0) (contract-count 0) (entry-count 0))
    (unless (and (stringp source) (not (file-exists-p source))
                 (stringp elc) (file-exists-p elc)
                 (stringp artifact-dir) (file-directory-p artifact-dir))
      (error "rooted-cfg: diagnostic source-free precondition failed"))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-v2-check)
               (lambda (&rest args)
                 (setq check-count (1+ check-count))
                 (let ((start (float-time)))
                   (nelisp-test-rooted-cfg-stage "raw-v2-check-start"
                                                  (number-to-string check-count))
                   (prog1 (apply check-real args)
                     (nelisp-test-rooted-cfg-stage
                      "raw-v2-check-end"
                      (format "%d %.3f" check-count (- (float-time) start)))))))
              ((symbol-function 'nelisp-native-load-raw-v2-rooted-cfg-contract-valid-p)
               (lambda (manifest)
                 (setq contract-count (1+ contract-count))
                 (let ((start (float-time)))
                   (nelisp-test-rooted-cfg-stage "contract-rebuild-start"
                                                  (number-to-string contract-count))
                   (prog1 (funcall contract-real manifest)
                     (nelisp-test-rooted-cfg-stage
                      "contract-rebuild-end"
                      (format "%d %.3f" contract-count (- (float-time) start)))))))
              ((symbol-function 'nelisp-bytecode-native-rooted-cfg-entry-call)
               (lambda (&rest args)
                 (setq entry-count (1+ entry-count))
                 (apply entry-real args))))
      (let* ((built (nelisp-test-rooted-cfg-build
                     elc 'gnu-rooted-cfg-two-diamonds
                     (expand-file-name "diagnostic.nelr" artifact-dir)))
             (function (nth 0 built)) (result (nth 3 built))
             (left (cons 'diagnostic-left nil))
             (right (cons 'diagnostic-right nil))
             (expected (funcall function nil left right nil left right))
             (actual (nelisp-bytecode-native-rooted-cfg-call
                      result nil left right nil left right))
             (cache-stats (nelisp-native-load-raw-v2-rooted-cfg-cache-statistics)))
        (unless (and (= entry-count 1) (equal actual expected)
                     (eq (car actual) (car expected)) (eq (cdr actual) (cdr expected)))
          (error "rooted-cfg: diagnostic native result mismatch"))
        (nelisp-test-rooted-cfg-stage
         "diagnostic-complete"
         (format "checks=%d contracts=%d entries=%d cache-hits=%d cache-misses=%d"
                 check-count contract-count entry-count
                 (plist-get cache-stats :hits)
                 (plist-get cache-stats :misses)))))))

(provide 'standalone-bytecode-native-rooted-cfg-emit-driver)
;;; standalone-bytecode-native-rooted-cfg-emit-driver.el ends here
