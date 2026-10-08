;;; nelisp-native-rooted-startup-evidence-test.el --- Generic rendering controls -*- lexical-binding: t; -*-
;; Generation fixtures do not grant native capability or verify a new image.
(require 'ert)
(require 'nelisp-native-rooted-startup-evidence)

(ert-deftest nelisp-root-startup-pins-current-inputs-and-refuses-mutation ()
  (let* ((root (make-temp-file "root-startup-render-" t))
         (metadata (expand-file-name "metadata.json" root))
         (data (expand-file-name "data.json" root))
         (manifest (expand-file-name "manifest.json" root))
         (closure (expand-file-name "closure.json" root))
         (template (getenv "NELISP_ROOT_PROOF_SOURCE"))
         (layout-source (getenv "NELISP_ROOT_PROTOCOL_EVIDENCE"))
         (source-closure (getenv "NELISP_ROOT_DIRECT_PROTOCOL"))
         (source-metadata (getenv "NELISP_ROOT_PROOF_METADATA"))
         (source-data (getenv "NELISP_ROOT_BSS_OWNER"))
         (builder-hash (secure-hash 'sha256 "new active generation source"))
         (capture (list :manifest manifest :metadata metadata :generated-data data)))
    (unwind-protect
        (progn
          (load (expand-file-name layout-source) nil t)
          (copy-file source-metadata metadata)
          (let ((object (car (nelisp-native-rooted-startup-evidence--json source-data 1048576))))
            (setcdr (assq 'owner-source-sha256 object) builder-hash)
            (with-temp-file data (insert (json-encode object))))
          (with-temp-file manifest
            (insert (json-encode
                     (list :builder-sha256 builder-hash
                           :metadata-sha256 (nelisp-native-rooted-build-evidence-source-hash metadata 4194304)
                           :generated-data-sha256 (nelisp-native-rooted-build-evidence-source-hash data 1048576)))))
          (let ((object (car (nelisp-native-rooted-startup-evidence--json source-closure 4194304))))
            (push (cons 'active_manifest_sha256
                        (nelisp-native-rooted-build-evidence-source-hash manifest 65536)) object)
            (with-temp-file closure (insert (json-encode object))))
          (let* ((digest (nelisp-native-rooted-build-evidence-source-hash closure 4194304))
                 (layout (plist-get (nelisp-native-rooted-abi-evidence) :layout))
                 (first (nelisp-native-rooted-startup-evidence-render capture closure digest template layout))
                 (second (nelisp-native-rooted-startup-evidence-render capture closure digest template layout)))
            (should (equal first second))
            (should (string-match-p (regexp-quote builder-hash) (plist-get first :startup-source)))
            (should (string-match-p (regexp-quote (plist-get first :evidence-sha256))
                                    (plist-get first :startup-source)))
            (with-temp-buffer (insert (plist-get first :startup-source)) (check-parens))
            (with-temp-file closure (insert "{}"))
            (should-error (nelisp-native-rooted-startup-evidence-render capture closure digest template layout))))
      (delete-directory root t))))

(ert-deftest nelisp-root-startup-refuses-same-body-owner-and-forged-checker ()
  (let* ((name 'nelisp-native-load-rooted-production-contract)
         (original (symbol-function name))
         (checker (symbol-function 'nelisp-native-rooted-startup-evidence--owners-valid-p))
         (clone (with-temp-buffer
                  (insert-file-contents (symbol-file name 'defun))
                  (goto-char (point-min))
                  (let (form)
                    (while (not (and (eq (car-safe form) 'defun) (eq (cadr form) name)))
                      (setq form (read (current-buffer))))
                    (eval (cons 'lambda (cddr form)) t)))))
    (unwind-protect
        (progn
          (should (equal original clone))
          (should-not (eq original clone))
          (fset name clone)
          (fset 'nelisp-native-rooted-startup-evidence--owners-valid-p (lambda () t))
          (should-error (nelisp-native-rooted-startup-evidence-build nil nil nil nil))
          (should-error (nelisp-native-rooted-startup-evidence-render nil nil nil nil nil)))
      (fset name original)
      (fset 'nelisp-native-rooted-startup-evidence--owners-valid-p checker))))

(ert-deftest nelisp-root-startup-context-uses-opaque-eq-and-json-snapshot ()
  (let* ((function (eval '(lambda (x) x) t)) (clone (eval '(lambda (x) x) t))
         (path (make-temp-file "root-json-snapshot-")))
    (unwind-protect
        (progn
          (should (nelisp-native-rooted-startup-evidence--context-equal-p
                   (list (cons 'owner function) "data")
                   (list (cons 'owner function) (copy-sequence "data"))))
          (should-not (nelisp-native-rooted-startup-evidence--context-equal-p
                       (list (cons 'owner function)) (list (cons 'owner clone))))
          (with-temp-file path (insert "{\"value\":42}"))
          (let ((snapshot (nelisp-native-rooted-startup-evidence--json path 100)))
            (should (= (alist-get 'value (car snapshot)) 42))
            (should (equal (cdr snapshot) (secure-hash 'sha256 "{\"value\":42}"))))
          (should-error (nelisp-native-rooted-startup-evidence--json path 2)))
      (delete-file path))))

(ert-deftest nelisp-root-startup-embedding-refuses-mutate-restore-read ()
  (let ((path (make-temp-file "root-embed-snapshot-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert ";;; Genuine bytes\n"))
          (let ((digest (secure-hash 'sha256 ";;; Genuine bytes\n")))
            (should (equal (nelisp-native-rooted-startup-evidence--source path digest)
                           ";;; Genuine bytes\n"))
            ;; The underlying file remains authentic throughout. An altered
            ;; read cannot be hidden by restoring it before a final rehash.
            (cl-letf (((symbol-function 'insert-file-contents-literally)
                       (lambda (&rest _) (insert ";;; Forged bytes\n"))))
              (should-error (nelisp-native-rooted-startup-evidence--source path digest)))
            (should (equal digest (nelisp-native-rooted-build-evidence-source-hash path 100)))))
      (delete-file path))))

(ert-deftest nelisp-root-startup-refuses-replaced-error-before-render ()
  (let ((original (symbol-function 'error)) (calls 0))
    (unwind-protect
        (progn
          (fset 'error (lambda (&rest _) (setq calls (1+ calls))))
          (should-error (nelisp-native-rooted-startup-evidence-render nil nil nil nil nil))
          (should (= calls 0)))
      (fset 'error original))))
