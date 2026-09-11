;;; nelisp-dev-session-test.el --- JSON development session adapter tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" here)))
(require 'nelisp-dev-session)

(defun nelisp-dev-session-test--json (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (let ((json-object-type 'alist)
          (json-array-type 'vector)
          (json-key-type 'string)
          (json-false nil))
      (json-read-from-string (buffer-string)))))

(defun nelisp-dev-session-test--kind-p (result kind)
  (cl-some (lambda (entry)
             (equal (cdr (assoc "code" entry))
                    (symbol-name kind)))
           (cdr (assoc "diagnostics" result))))

(defun nelisp-dev-session-test--status (result)
  (cdr (assoc "status" result)))

(defun nelisp-dev-session-test--context (manifest &optional policy)
  (append (list (cons "manifest" manifest))
          (and policy (list (cons "effects-policy" policy)))))

(defmacro nelisp-dev-session-test--with-files (&rest body)
  `(let* ((directory (make-temp-file "nelisp-dev-session-" t))
          (source (expand-file-name "fixture.el" directory))
          (recipe (expand-file-name "session.el" directory))
          (manifest (expand-file-name "session.manifest.json" directory)))
     (unwind-protect
         (progn
           (with-temp-file source
             (insert "(setq nelisp-dev-session-test-side-effect 99)\n"))
           ,@body)
       (delete-directory directory t))))

(ert-deftest nelisp-dev-session/export-uses-live-registry-and-json-schema ()
  "Export takes records from the REPL registry and emits JSON v1."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil)
         (nelisp-repl-session--next-id 1))
     (nelisp-repl-session-record '(setq nelisp-dev-session-test-value 7))
     (nelisp-repl-session-record-load source)
     ;; String-key contexts are the public boundary; no :session records are
     ;; supplied by the caller.
    (let ((result (nelisp-dev-session-dispatch
                    (list (cons "operation" "session.export")
                          (cons "request_id" "export-1")
                          (cons "arguments"
                                (list (cons "recipe" recipe)
                                      (cons "manifest" manifest))))
                    (list (cons "session-id" "session-1")))))
       (should (equal (nelisp-dev-session-test--status result) "ok"))
       (should (= (cdr (assoc "record-count" (cdr (assoc "data" result)))) 2))
       (let ((data (nelisp-dev-session-test--json manifest)))
         (should (equal (cdr (assoc "schema_version" data)) "1"))
         (should (equal (cdr (assoc "session_id" data)) "session-1"))
         (should (stringp (cdr (assoc "recipe" data))))
         (should (not (file-name-absolute-p (cdr (assoc "recipe" data)))))
         (should (vectorp (cdr (assoc "sources" data))))
         (should (= (length (cdr (assoc "sources" data))) 1))
       (should (vectorp (cdr (assoc "limitations" data))))))
     (should (stringp (nelisp-dev-protocol-json
                       (nelisp-dev-session-dispatch
                        "session.validate"
                        (nelisp-dev-session-test--context manifest)))))
     (let ((value (nelisp-dev-session-dispatch
                   "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (equal (nelisp-dev-session-test--status value) "ok"))))))

(ert-deftest nelisp-dev-session/validate-tampering-reports-all-errors ()
  "Source and recipe changes, unknown versions and bad paths are visible."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record-load source)
     (should (equal
              (nelisp-dev-session-test--status
               (nelisp-dev-session-dispatch "session.export"
                                            (list (cons "session-id" "s")
                                                  (cons "recipe" recipe)
                                                  (cons "manifest" manifest))))
              "ok"))
     (with-temp-file source (insert "tampered\n"))
     (let ((result (nelisp-dev-session-dispatch
                    "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (equal (nelisp-dev-session-test--status result) "failed"))
       (should (nelisp-dev-session-test--kind-p result :sha-mismatch)))
     ;; Re-export restores the source and recipe hashes, then tamper recipe.
     (with-temp-file source
       (insert "(setq nelisp-dev-session-test-side-effect 99)\n"))
     (nelisp-dev-session-dispatch "session.export"
                                  (list (cons "session-id" "s")
                                        (cons "recipe" recipe)
                                        (cons "manifest" manifest)))
     (with-temp-file recipe (insert "tampered recipe\n"))
     (let ((result (nelisp-dev-session-dispatch
                    "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (equal (nelisp-dev-session-test--status result) "failed"))
       (should (nelisp-dev-session-test--kind-p result :sha-mismatch)))
     ;; Missing files are accumulated in one validation result.
     (delete-file source)
     (delete-file recipe)
     (let ((result (nelisp-dev-session-dispatch
                    "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (>= (length (cdr (assoc "diagnostics" result))) 2))
       (should (nelisp-dev-session-test--kind-p result :missing)))
     (with-temp-file source
       (insert "(setq nelisp-dev-session-test-side-effect 99)\n"))
     (nelisp-dev-session-dispatch "session.export"
                                  (list (cons "session-id" "s")
                                        (cons "recipe" recipe)
                                        (cons "manifest" manifest)))
     ;; Unknown version and absolute paths are rejected as manifest data.
     (let ((data (nelisp-dev-session-test--json manifest)))
       (setf (cdr (assoc "schema_version" data)) "999")
       (with-temp-file manifest
         (insert (json-encode data) "\n")))
     (let ((result (nelisp-dev-session-dispatch
                    "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (nelisp-dev-session-test--kind-p result :unknown-version)))
     (let ((bad `(("schema_version" . "1") ("session_id" . "s")
                  ("recipe" . "/absolute/session.el")
                  ("recipe_sha256" . ,(make-string 64 ?0))
                  ("recipe_bytes" . 1)
                  ("sources" . []) ("limitations" . []))))
       (with-temp-file manifest
         (insert (json-encode bad) "\n")))
     (let ((result (nelisp-dev-session-dispatch
                    "session.validate" (nelisp-dev-session-test--context manifest))))
       (should (equal (nelisp-dev-session-test--status result) "failed"))
       (should (nelisp-dev-session-test--kind-p result :path-schema))))))

(ert-deftest nelisp-dev-session/validate-does-not-load-or-eval ()
  "Validation hashes bytes only, including a recipe containing side effects."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil)
         (nelisp-dev-session-test-side-effect 0))
     (nelisp-repl-session-record '(setq nelisp-dev-session-test-side-effect 99))
     (let ((result (nelisp-dev-session-dispatch
                    "session.export"
                    (list (cons "session-id" "safe") (cons "recipe" recipe)
                          (cons "manifest" manifest)))))
       (should (equal (nelisp-dev-session-test--status result) "ok")))
     (should (equal
              (nelisp-dev-session-test--status
               (nelisp-dev-session-dispatch
                "session.validate" (nelisp-dev-session-test--context manifest)))
              "ok"))
     (should (= nelisp-dev-session-test-side-effect 0)))))

(ert-deftest nelisp-dev-session/export-rejects-output-collisions ()
  "Recipe and manifest outputs never overwrite registered source files."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record-load source)
     (dolist (paths (list (list recipe recipe) (list source manifest)))
       (let ((result (nelisp-dev-session-dispatch
                      "session.export"
                      (list (cons "session-id" "collision")
                            (cons "recipe" (car paths))
                            (cons "manifest" (cadr paths))))))
         (should (equal (nelisp-dev-session-test--status result) "failed"))
         (should (nelisp-dev-session-test--kind-p result :path-collision)))))))

(ert-deftest nelisp-dev-session/rejects-nil-session-and-missing-manifest ()
  "A live export needs an identity; malformed and missing JSON is invalid."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (let ((result (nelisp-dev-session-dispatch
                    "session.export"
                    (list (cons "session-id" nil) (cons "recipe" recipe)
                          (cons "manifest" manifest)))))
       (should (equal (nelisp-dev-session-test--status result) "failed"))
       (should (nelisp-dev-session-test--kind-p result :nil-session)))
     (with-temp-file manifest (insert "42\n"))
     (should (nelisp-dev-session-test--kind-p
              (nelisp-dev-session-dispatch
               "session.validate" (nelisp-dev-session-test--context manifest))
              :manifest-schema))
     (delete-file manifest)
     (should (nelisp-dev-session-test--kind-p
              (nelisp-dev-session-dispatch
               "session.validate" (nelisp-dev-session-test--context manifest))
              :manifest-missing)))))

(ert-deftest nelisp-dev-session/replay-never-implicitly-executes ()
  "Replay requires policy and validation, then remains explicitly unsupported."
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record '(setq nelisp-dev-session-test-side-effect 13))
     (nelisp-dev-session-dispatch
      "session.export"
      (list (cons "session-id" "replay") (cons "recipe" recipe)
            (cons "manifest" manifest)))
     (let ((result (nelisp-dev-session-dispatch
                    "session.replay" (nelisp-dev-session-test--context manifest))))
       (should (equal (nelisp-dev-session-test--status result) "unsupported"))
     (let ((result (nelisp-dev-session-dispatch
                    "session.replay"
                    (nelisp-dev-session-test--context manifest "explicit-only"))))
       (should (equal (nelisp-dev-session-test--status result) "unsupported")))))))

(provide 'nelisp-dev-session-test)

(ert-deftest nelisp-dev-session/required-recipe-and-count-cannot-be-null ()
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record-load source)
     (nelisp-dev-session-dispatch "session.export"
                                  (list :session-id "strict" :recipe recipe :manifest manifest))
     (let ((original (nelisp-dev-session-test--json manifest)))
       (dolist (field '("recipe" "recipe_sha256" "recipe_bytes" "record_count" "session_id"))
         (let ((mutated (copy-tree original)))
           (setf (alist-get field mutated nil nil #'equal) nil)
           (with-temp-file manifest (insert (json-encode mutated)))
           (should (equal "failed"
                          (nelisp-dev-session-test--status
                           (nelisp-dev-session-dispatch
                            "session.validate" (list :manifest manifest)))))))))))

(ert-deftest nelisp-dev-session/oversized-files-are-not-hashed ()
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record-load source)
     (nelisp-dev-session-dispatch "session.export"
                                  (list :session-id "bounded" :recipe recipe :manifest manifest))
     (let ((nelisp-dev-session-max-file-bytes 1))
       (cl-letf (((symbol-function 'nelisp-dev-session--sha256)
                  (lambda (_) (ert-fail "Oversized file was read for hashing"))))
         (should (equal "failed"
                        (nelisp-dev-session-test--status
                         (nelisp-dev-session-dispatch
                          "session.validate" (list :manifest manifest))))))))))

(ert-deftest nelisp-dev-session/root-relative-unicode-manifest ()
  (nelisp-dev-session-test--with-files
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record-load source)
     (nelisp-dev-session-dispatch
      '(("operation" . "session.export") ("request_id" . "export")
        ("arguments" ("recipe" . "再現.el") ("manifest" . "再現.json")))
      (list :root directory :session-id "unicode"))
     (should (equal "ok"
                    (nelisp-dev-session-test--status
                     (nelisp-dev-session-dispatch
                      '(("operation" . "session.validate") ("request_id" . "validate")
                        ("arguments" ("manifest" . "再現.json")))
                      (list :root directory))))))))
;;; nelisp-dev-session-test.el ends here
