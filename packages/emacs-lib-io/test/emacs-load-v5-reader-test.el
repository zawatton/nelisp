;;; emacs-load-v5-reader-test.el --- Focused ERT for v5 native artifact reader  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-v5-reader-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-load-v5-reader-test--source-file
  (expand-file-name "src/emacs-load.el" emacs-load-v5-reader-test--root))

(let ((emacs-version 1))
  (load-file emacs-load-v5-reader-test--source-file))

(defun emacs-load-v5-reader-test--v5-native-section
    (text-base64 extern-symbols reloc-data defuns heavy-char)
  "Return a raw v5 native section fixture."
  (let ((symbols (mapcar (lambda (entry) (plist-get entry :name)) defuns)))
    (list :native-section-version 5
          :serialized-char-size 0
          :runtime-prefix
          (vector 2
                  0
                  "x86_64"
                  symbols
                  text-base64
                  'indexed-plt32-v1
                  (/ (length reloc-data) 3)
                  reloc-data
                  extern-symbols
                  defuns)
          :object-format 'nelisp-aot-elf-v1
          :object-base64 (make-string 20000 heavy-char)
          :compile-report
          (list (list :name (or (car symbols) "probe") :native t)))))

(ert-deftest emacs-load-v5-reader-test/artifact-load-or-compile-uses-default-v5-wire ()
  (let* ((temp-dir (make-temp-file "emacs-load-v5-wire-" t))
         (resolved (expand-file-name "sample.el" temp-dir))
         (artifact (expand-file-name "sample.neln" temp-dir))
         (sidecar (concat artifact ".source-sha256"))
         (temp-input (expand-file-name "compile-input.el" temp-dir))
         (source "(defun emacs-load-v5-wire-test () 1)\n")
         (compiler-path "/tmp/fake-nelisp")
         (call-process-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-load--artifact-compiler)
                   (lambda () compiler-path))
                  ((symbol-function 'emacs-load--artifact-cache-paths)
                   (lambda (_resolved)
                     (list artifact sidecar)))
                  ((symbol-function 'emacs-load--artifact-load-path-cli-args)
                   (lambda () nil))
                  ((symbol-function 'make-temp-file)
                   (lambda (&rest _args) temp-input))
                  ((symbol-function 'call-process)
                   (lambda (&rest args)
                     (setq call-process-args args)
                     (with-temp-file artifact
                       (insert ";;; nelisp-private-nelc-v2\n")
                       (prin1 '(:module-init nil) (current-buffer)))
                     0))
                  ((symbol-function 'emacs-load--artifact-replay-file)
                   (lambda (_path)
                     t)))
          (let ((emacs-load-auto-native-compile t))
            (should (eq (emacs-load--artifact-load-or-compile resolved source) t)))
          (should (equal call-process-args
                         (list compiler-path nil nil nil
                               "compile-elisp-artifact"
                               "--kind" "neln"
                               "--input" temp-input
                               "--output" artifact
                               "--native-policy" "opportunistic")))
          (should-not (member "--native-wire" call-process-args)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-v5-reader-test/artifact-source-read-form-nil-or-list-value-keeps-native-error-context ()
  (let ((path "/tmp/emacs-load-v5-native-error.neln")
        (source "xx((:alpha 1))yy")
        (err nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (_string)
                 (error "real native parse error"))))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--artifact-source-read-form-nil-or-list-value
                   source (cons 2 (- (length source) 2)) path :module-init)
                  nil)
              (error caught))))
    (should err)
    (should (string-match-p
             (regexp-quote "invalid :module-init in /tmp/emacs-load-v5-native-error.neln")
             (error-message-string err)))
    (should (string-match-p "real native parse error"
                            (error-message-string err)))))

(ert-deftest emacs-load-v5-reader-test/native-sections-from-source-canonicalizes-v5-runtime-prefix-and-compact-relocs ()
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
         (path "/tmp/emacs-load-v5-native-sections.neln")
         (externs '("nl_alloc_symbol" "nelisp_aot_builtin_call1"))
         (defuns-a
          '((:name "emacs-load-v5-reader-test--a"
             :offset 0
             :body-offset 1
             :arity 0
             :rt-slot-count 1)))
         (defuns-b
          '((:name "emacs-load-v5-reader-test--b"
             :offset 4
             :body-offset 2
             :arity 1
             :rt-slot-count 2)))
         (section-a
          (emacs-load-v5-reader-test--v5-native-section
           "QUJD" externs '(176 0 -4 263 1 -4) defuns-a ?o))
         (section-b
          (emacs-load-v5-reader-test--v5-native-section
           "REVG" nil nil defuns-b ?p))
         (content (concat "("
                          (prin1-to-string section-a)
                          "\n ; keep comment between sections\n"
                          (prin1-to-string section-b)
                          ")"))
         (heavy-read-calls 0)
         (orig-read (symbol-function 'read-from-string)))
    (cl-letf (((symbol-function 'read-from-string)
               (lambda (string &optional start end)
                 (let* ((from (or start 0))
                        (to (or end (length string)))
                        (snippet (substring string from to)))
                   (when (or (string-match-p ":object-base64" snippet)
                             (string-match-p ":compile-report" snippet))
                     (setq heavy-read-calls (1+ heavy-read-calls))
                     (error "heavy native field must not be materialized"))
                   (if end
                       (funcall orig-read string start end)
                     (funcall orig-read string start))))))
      (let ((sections (emacs-load--artifact-native-sections-from-source
                       content 0 (length content) path)))
        (should (= heavy-read-calls 0))
        (should (equal sections
                       (list
                        `(:arch "x86_64"
                          :text-base64 "QUJD"
                          :relocs ((:offset 176
                                    :type plt32
                                    :symbol "nl_alloc_symbol"
                                    :addend -4)
                                   (:offset 263
                                    :type plt32
                                    :symbol "nelisp_aot_builtin_call1"
                                    :addend -4))
                          :extern-symbols ("nl_alloc_symbol"
                                           "nelisp_aot_builtin_call1")
                          :defuns ,defuns-a)
                        `(:arch "x86_64"
                          :text-base64 "REVG"
                          :relocs nil
                          :extern-symbols nil
                          :defuns ,defuns-b))))
        (dolist (section sections)
          (should-not (plist-member section :native-section-version))
          (should-not (plist-member section :runtime-prefix))
          (should-not (plist-member section :object-base64))
          (should-not (plist-member section :compile-report)))))))

(ert-deftest emacs-load-v5-reader-test/artifact-replay-payload-streaming-round-trips-v5-native-sections ()
  (let* ((path "/tmp/emacs-load-v5-streaming.neln")
         (externs '("nl_alloc_symbol" "nelisp_aot_builtin_call1"))
         (defuns-a
          '((:name "emacs-load-v5-reader-test--stream-a"
             :offset 0
             :body-offset 1
             :arity 0
             :rt-slot-count 1)))
         (defuns-b
          '((:name "emacs-load-v5-reader-test--stream-b"
             :offset 4
             :body-offset 2
             :arity 1
             :rt-slot-count 2)))
         (section-a
          (emacs-load-v5-reader-test--v5-native-section
           "QUJD" externs '(176 0 -4 263 1 -4) defuns-a ?x))
         (section-b
          (emacs-load-v5-reader-test--v5-native-section
           "REVG" nil nil defuns-b ?y))
         (payload
          `(:native-sections (,section-a ,section-b)
            :module-init ((:fn emacs-load-v5-reader-test--stream-a)
                          (:fn emacs-load-v5-reader-test--stream-b))))
         (content (concat ";;; nelisp-private-nelc-v2\n"
                          (prin1-to-string payload)))
         (replayed nil)
         (bases-seen nil)
         (index-seen nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-native-map-section)
               (lambda (section _path)
                 (intern (format "base-%s"
                                 (plist-get section :text-base64)))))
              ((symbol-function 'emacs-load--artifact-native-eligible-p)
               (lambda (_section) t))
              ((symbol-function 'emacs-load--artifact-replay-item-with-native-sections)
               (lambda (item _path section-bases defuns-index)
                 (setq bases-seen section-bases
                       index-seen defuns-index)
                 (push item replayed)
                 :ok)))
      (let ((summary (emacs-load--artifact-replay-payload-streaming
                      content path (length ";;; nelisp-private-nelc-v2\n"))))
        (should (plist-get summary :streaming))
        (should (eq (plist-get summary :native-mode) :native-sections))
        (should (= (plist-get summary :module-init-count) 2))
        (should (equal (plist-get summary :fields)
                       '(:native-sections :module-init)))
        (should (equal (nreverse replayed)
                       '((:fn emacs-load-v5-reader-test--stream-a)
                         (:fn emacs-load-v5-reader-test--stream-b))))
        (should (= (length bases-seen) 2))
        (dolist (entry bases-seen)
          (let ((section (car entry)))
            (should (equal (plist-get section :arch) "x86_64"))
            (should (stringp (plist-get section :text-hash)))
            (should-not (plist-member section :runtime-prefix))
            (should-not (plist-member section :object-base64))
            (should-not (plist-member section :compile-report))))
        (should index-seen)
        (should (gethash "emacs-load-v5-reader-test--stream-a" index-seen))
        (should (gethash "emacs-load-v5-reader-test--stream-b" index-seen))))))

(provide 'emacs-load-v5-reader-test)

;;; emacs-load-v5-reader-test.el ends here
