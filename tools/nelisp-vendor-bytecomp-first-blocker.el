;;; nelisp-vendor-bytecomp-first-blocker.el --- Bounded bytecomp load probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Reuse the pinned triparity byte-compile-lambda fixture.  Insert progress
;; markers into disposable copies of the vendor sources so a load error can
;; be attributed to the exact original top-level form.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'nelisp-vendor-bytecode-triparity)

(defconst nelisp-vendor-bytecomp-first-blocker--sources
  '("macroexp.el" "cconv.el" "bytecomp.el"))

(defun nelisp-vendor-bytecomp-first-blocker--source-forms (file)
  "Return top-level form spans from FILE using the Host Emacs reader."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (rows done)
      (while (not done)
        (forward-comment (point-max))
        (if (eobp)
            (setq done t)
          (let* ((start (point))
                 (line (line-number-at-pos start))
                 (form (condition-case nil
                           (read (current-buffer))
                         (end-of-file (setq done t) nil))))
            (unless done
              (let ((end-line (line-number-at-pos (point)))
                    (printed (prin1-to-string form)))
                (push (list :index (1+ (length rows)) :start start
                            :line line :end_line end-line
                            :form printed :form_sha256 (secure-hash 'sha256 printed))
                      rows))))))
      (nreverse rows))))

(defun nelisp-vendor-bytecomp-first-blocker--instrument (file destination)
  "Copy FILE to DESTINATION with a marker before each original form.
Return the form rows, in source order."
  (let ((rows (nelisp-vendor-bytecomp-first-blocker--source-forms file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (row (reverse rows))
        (goto-char (plist-get row :start))
        (insert (format "\n(princ %S)\n"
                        (format "NELISP_BC_FIRST:%s:%d\n"
                                (file-name-nondirectory file)
                                (plist-get row :index)))))
      (write-region (point-min) (point-max) destination nil 'silent))
    rows))

(defun nelisp-vendor-bytecomp-first-blocker--last-marker (output)
  "Return the last source marker in OUTPUT, or nil."
  (let ((start 0) result)
    (while (string-match "NELISP_BC_FIRST:\\([^:\n]+\\):\\([0-9]+\\)" output start)
      (setq result (cons (match-string 1 output)
                         (string-to-number (match-string 2 output)))
            start (match-end 0)))
    result))

(defun nelisp-vendor-bytecomp-first-blocker--missing (stderr)
  "Extract the first missing symbol or opcode from STDERR."
  (cond
   ((string-match "\\(void-function\\|void-variable\\): (\\([^() \n]+\\))" stderr)
    (list :kind (match-string 1 stderr) :symbol (match-string 2 stderr)))
   ((string-match "\\(?:unknown\\|unsupported\\|invalid\\)[^\n]*opcode[^0-9]*\\([0-9]+\\)" stderr)
    (list :kind "opcode" :opcode (string-to-number (match-string 1 stderr))))))

(defun nelisp-vendor-bytecomp-first-blocker--run-lane
    (program object arguments lane sources rows timeout-seconds)
  "Run one triparity LANE against instrumented SOURCES and ROWS."
  (let* ((out (make-temp-file "nelisp-bytecomp-out-"))
         (err (make-temp-file "nelisp-bytecomp-err-"))
         (form (nelisp-vendor-bytecode-triparity--child-form
                object arguments lane sources))
         (started (float-time)))
    (unwind-protect
        (let* ((exit-code (process-file
                           "timeout" nil (list (list :file out) err) nil
                           (number-to-string timeout-seconds)
                           program "-Q" "--batch" "--eval" form))
               (stdout (with-temp-buffer (insert-file-contents out) (buffer-string)))
               (stderr (with-temp-buffer (insert-file-contents err) (buffer-string)))
               (marker (nelisp-vendor-bytecomp-first-blocker--last-marker stdout))
               (source-rows (cdr (assoc (car marker) rows)))
               (source-form (and marker (nth (1- (cdr marker)) source-rows)))
               (missing (nelisp-vendor-bytecomp-first-blocker--missing stderr)))
          (list :status (cond ((equal exit-code 0) "ok")
                              ((equal exit-code 124) "timeout")
                              (t "error"))
                :exit_code exit-code
                :elapsed_ms (* 1000.0 (- (float-time) started))
                :stage (if source-form (car marker) "before-source-form")
                :first_failing_form
                (when source-form
                  (list :source (car marker)
                        :index (plist-get source-form :index)
                        :line (plist-get source-form :line)
                        :end_line (plist-get source-form :end_line)
                        :form (plist-get source-form :form)
                        :form_sha256 (plist-get source-form :form_sha256)))
                :missing_symbol (plist-get missing :symbol)
                :missing_opcode (plist-get missing :opcode)
                :error_kind (plist-get missing :kind)
                :error_excerpt (substring stderr 0 (min 400 (length stderr)))))
      (delete-file out)
      (delete-file err))))

(defun nelisp-vendor-bytecomp-first-blocker--negative-control
    (binary directory object arguments &optional expected-sha)
  "Run a deliberately broken load fixture through the same detector."
  (let* ((file (expand-file-name "deliberately-broken.el" directory))
         (copy (expand-file-name "deliberately-broken-instrumented.el" directory))
         (symbol "nelisp-bytecomp-deliberately-missing"))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n"
              "(defvar nelisp-bytecomp-control 1)\n"
              "(nelisp-bytecomp-deliberately-missing)\n"))
    (let* ((rows (nelisp-vendor-bytecomp-first-blocker--instrument file copy))
           (expected-sha (or expected-sha
                             (nelisp-vendor-bytecode-jit-coverage--source-fingerprint
                              binary)))
           (result (nelisp-vendor-bytecomp-first-blocker--run-pinned-lane
                    binary expected-sha object arguments 'vm (list copy)
                    (list (cons (file-name-nondirectory file) rows)) 15))
           (form (plist-get result :first_failing_form)))
      (list :detected (and (equal (plist-get result :status) "error")
                           (equal (plist-get result :missing_symbol) symbol)
                           (= (or (plist-get form :line) 0) 3))
            :result result))))

(defun nelisp-vendor-bytecomp-first-blocker--run-pinned-lane
    (binary expected-sha object arguments lane sources rows timeout-seconds)
  "Run one LANE only while BINARY still matches EXPECTED-SHA.
Return before/after identity evidence so the caller can persist a report
before failing loudly if the binary changed."
  (let ((before (nelisp-vendor-bytecode-jit-coverage--source-fingerprint binary)))
    (if (not (equal before expected-sha))
        (list :status "binary-mismatch"
              :binary_sha256_before before
              :binary_sha256_after before
              :binary_identity_ok nil
              :identity_error "binary changed before lane")
      (let* ((result (nelisp-vendor-bytecomp-first-blocker--run-lane
                      binary object arguments lane sources rows timeout-seconds))
             (after (nelisp-vendor-bytecode-jit-coverage--source-fingerprint binary))
             (identity-ok (equal after expected-sha)))
        (append (list :status (if identity-ok (plist-get result :status)
                                "binary-mismatch")
                      :lane_status (plist-get result :status)
                      :binary_sha256_before before
                      :binary_sha256_after after
                      :binary_identity_ok identity-ok)
                (cl-loop for (key value) on result by #'cddr
                         unless (eq key :status) append (list key value)))))))

(defun nelisp-vendor-bytecomp-first-blocker-run ()
  "Write a small JSON report for the pinned bytecomp first blocker."
  (interactive)
  (let* ((root (nelisp-vendor-bytecode-triparity--root))
         (vendor-root (nelisp-vendor-bytecode-jit-coverage--source-root))
         (configured-binary (or (getenv "NELISP_BIN")
                                (expand-file-name "target/nelisp" root)))
         (binary (and (file-executable-p configured-binary)
                      (file-truename configured-binary)))
         (host-binary (or (getenv "NELISP_EMACS") "emacs"))
         (output (or (getenv "NELISP_BYTECOMP_BLOCKER_OUTPUT")
                     (expand-file-name "target/bytecomp-first-blocker/report.json" root)))
         (directory (file-name-directory output))
         (started (float-time)))
    (unless binary
      (error "Standalone binary missing or not executable: %s" configured-binary))
    (let ((binary-sha256
           (nelisp-vendor-bytecode-jit-coverage--source-fingerprint binary)))
    (make-directory directory t)
    (let* ((pins (nelisp-vendor-bytecode-jit-coverage--verify-sources))
           (source-check-ms (* 1000.0 (- (float-time) started)))
           (_loaded (nelisp-vendor-bytecode-jit-coverage--load-sources))
           (object (nelisp-vendor-bytecode-triparity--object 'byte-compile-lambda))
           (arguments (cdr (assq 'byte-compile-lambda
                                 nelisp-vendor-bytecode-triparity--fixtures)))
           (fingerprint (nelisp-vendor-bytecode-triparity--bytecode-hash object))
           (host (nelisp-vendor-bytecode-triparity--lane
                  host-binary object arguments 'host
                  nelisp-vendor-bytecomp-first-blocker--sources 15))
           (rows nil)
           (copies nil))
      (dolist (name nelisp-vendor-bytecomp-first-blocker--sources)
        (let* ((original (expand-file-name name vendor-root))
               (copy (expand-file-name (concat "instrumented-" name) directory))
               (forms (nelisp-vendor-bytecomp-first-blocker--instrument original copy)))
          (push (cons name forms) rows)
          (push copy copies)))
      (setq rows (nreverse rows) copies (nreverse copies))
      (let* ((vm (nelisp-vendor-bytecomp-first-blocker--run-pinned-lane
                  binary binary-sha256 object arguments 'vm copies rows 15))
             (jit (nelisp-vendor-bytecomp-first-blocker--run-pinned-lane
                   binary binary-sha256 object arguments 'jit copies rows 15))
             (control (nelisp-vendor-bytecomp-first-blocker--negative-control
                       binary directory object arguments binary-sha256))
             (report
              (list :schema 1
                    :binary_path binary
                    :binary_sha256 binary-sha256
                    :binary_identity_ok
                    (and (plist-get vm :binary_identity_ok)
                         (plist-get jit :binary_identity_ok)
                         (plist-get (plist-get control :result)
                                    :binary_identity_ok))
                    :source_sha256 (mapcar (lambda (pin)
                                             (cons (symbol-name (car pin)) (cdr pin)))
                                           pins)
                    :fixture "byte-compile-lambda"
                    :bytecode_sha256 fingerprint
                    :elapsed_stage_ms
                    (list :source_validation source-check-ms
                          :host (plist-get host :process_elapsed_ms)
                          :vm (plist-get vm :elapsed_ms)
                          :jit (plist-get jit :elapsed_ms))
                    :host (list :status (plist-get host :status)
                                :reason (plist-get host :reason))
                    :vm vm :jit jit
                    :negative_control
                    control)))
        (with-temp-file output (insert (json-encode report) "\n"))
        (princ (format "bytecomp first blocker: %s\n" output))
        (unless (plist-get report :binary_identity_ok)
          (error "Standalone binary changed during first-blocker lanes; see %s" output))
        (unless (and (equal (plist-get host :status) "ok")
                     (plist-get control :detected))
          (error "bytecomp first blocker precondition or negative control failed"))
        report)))))

(provide 'nelisp-vendor-bytecomp-first-blocker)
;;; nelisp-vendor-bytecomp-first-blocker.el ends here
