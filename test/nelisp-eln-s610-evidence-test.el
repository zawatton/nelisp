;;; nelisp-eln-s610-evidence-test.el --- S10.3 evidence validator tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The validator (tools/nelisp-eln-s610-evidence.el) is exercised on synthetic
;; evidence built from the real ledger, sources and pinned artifact, with a
;; throwaway "binary" file; the live S6.22 validation is stubbed except in the
;; propagation test.  No NeLisp binary is run.

;;; Code:

(require 'ert)
(require 'cl-lib)
(defconst s10e-test--dir (file-name-directory (or load-file-name buffer-file-name)))
(load (expand-file-name "../tools/nelisp-eln-s610-evidence.el" s10e-test--dir) nil t)

(defconst s10e-test--root nelisp-eln-s610-root)
(defconst s10e-test--ledger
  (expand-file-name "tools/ai/eln-progress.org" s10e-test--root))

(defconst s10e-test--lines
  '(("s610-measure"
     "S6_MEASURE_RESULT function=byte-compile-form status=PASS native_raw_calls=2 native_dispatch_calls=1 eln_sha256=x")
    ("scenarios" "NELISP-ELN-S610-SCENARIOS-PASS pushes=0 landings=0"
     "transcript lines: 31 identical")
    ("forced" "NELISP-ELN-S610-FORCED-PASS pushes=5 landings=4"
     "transcript lines: 31 identical" "S10_RED_CONTROL_DIFFERS=PASS")
    ("tamper" "NELISP-ELN-S610-TAMPER-PASS")
    ("mutation" "NELISP-ELN-S610-MUTATION-PASS mutants=39")))

(defun s10e-test--eln-ok-p ()
  (let ((eln (nelisp-eln-s610--eln-file)))
    (and (file-readable-p eln)
         (equal (nelisp-eln-s6-corpus--file-sha256 eln) nelisp-eln-s610-eln-sha256))))

(defun s10e-test--write-evidence (dir binary)
  (let ((evidence (expand-file-name "evidence.json" dir)))
    (with-temp-file evidence
      (insert
       (json-serialize
        `((schema . ,nelisp-eln-s610-schema)
          (binary_path . ,binary)
          (binary_sha256 . ,(nelisp-eln-s6-corpus--file-sha256 binary))
          (eln_sha256 . ,nelisp-eln-s610-eln-sha256)
          (source_sha256 . ,(nelisp-eln-s610--source-digest s10e-test--root))
          (stages
           . ,(vconcat
               (mapcar
                (lambda (stage)
                  (let ((cmd (nelisp-eln-s610--stage-cmd stage s10e-test--ledger)))
                    `((stage . ,(car stage))
                      (cmd . ,cmd)
                      (cmd_digest . ,(nelisp-eln-s610--cmd-digest cmd s10e-test--root))
                      (exit_code . 0)
                      (status . "PASS")
                      (reason . :null)
                      (result_lines
                       . ,(vconcat (cdr (assoc (car stage) s10e-test--lines)))))))
                nelisp-eln-s610-stages))))
        :null-object :null :false-object :json-false)))
    evidence))

(defmacro s10e-test--with (vars &rest body)
  "Bind (BINARY EVIDENCE) VARS to a fresh fake binary and matching evidence."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "s10e-" t))
          (,(nth 0 vars) (expand-file-name "fake-bin" dir))
          (_ (progn (with-temp-file ,(nth 0 vars) (insert "#!/bin/sh\n"))
                    (set-file-modes ,(nth 0 vars) #o755)))
          (,(nth 1 vars) (s10e-test--write-evidence dir ,(nth 0 vars))))
     (ignore _)
     (unwind-protect
         (cl-letf (((symbol-function 'nelisp-eln-s6-corpus-validate)
                    (lambda (&rest _) nil)))
           ,@body)
       (delete-directory dir t))))

(defun s10e-test--validate (binary evidence)
  (nelisp-eln-s610-validate evidence binary s10e-test--ledger s10e-test--root))

(defun s10e-test--mutate (evidence fn)
  (let ((data (nelisp-eln-s6-corpus-read-evidence evidence)))
    (setq data (funcall fn data))
    ;; The parser yields lists; JSON arrays must be vectors again.
    (setf (alist-get 'stages data)
          (vconcat (mapcar (lambda (r)
                             (let ((lines (alist-get 'result_lines r)))
                               (setf (alist-get 'result_lines r) (vconcat lines))
                               r))
                           (alist-get 'stages data))))
    (with-temp-file evidence
      (insert (json-serialize data :null-object :null :false-object :json-false)))))

(defun s10e-test--map-stage (data name fn)
  (setf (alist-get 'stages data)
        (vconcat (mapcar (lambda (r)
                           (if (equal (alist-get 'stage r) name) (funcall fn r) r))
                         (alist-get 'stages data))))
  data)

(defun s10e-test--has (regexp problems)
  (cl-some (lambda (p) (string-match-p regexp p)) problems))

(ert-deftest nelisp-eln-s610-evidence/fresh-all-pass-validates ()
  (skip-unless (s10e-test--eln-ok-p))
  (s10e-test--with (binary evidence)
    (should-not (s10e-test--validate binary evidence))))

(ert-deftest nelisp-eln-s610-evidence/stale-binary-fails ()
  (skip-unless (s10e-test--eln-ok-p))
  (s10e-test--with (binary evidence)
    (with-temp-file binary (insert "#!/bin/sh\n# rebuilt\n"))
    (should (s10e-test--has "stale: binary" (s10e-test--validate binary evidence)))))

(ert-deftest nelisp-eln-s610-evidence/missing-evidence-fails ()
  (s10e-test--with (binary evidence)
    (delete-file evidence)
    (should (s10e-test--has "evidence file missing"
                            (s10e-test--validate binary evidence)))))

(ert-deftest nelisp-eln-s610-evidence/missing-stage-fails ()
  (skip-unless (s10e-test--eln-ok-p))
  (s10e-test--with (binary evidence)
    (s10e-test--mutate
     evidence
     (lambda (d)
       (setf (alist-get 'stages d)
             (vconcat (cl-remove-if (lambda (r) (equal (alist-get 'stage r) "tamper"))
                                    (alist-get 'stages d))))
       d))
    (should (member "tamper: no evidence row" (s10e-test--validate binary evidence)))))

(ert-deftest nelisp-eln-s610-evidence/fail-stage-fails ()
  (skip-unless (s10e-test--eln-ok-p))
  (s10e-test--with (binary evidence)
    (s10e-test--mutate
     evidence
     (lambda (d) (s10e-test--map-stage
                  d "mutation" (lambda (r) (setf (alist-get 'status r) "FAIL") r))))
    (should (s10e-test--has "\\`mutation: status FAIL" (s10e-test--validate binary evidence)))
    (s10e-test--mutate
     evidence
     (lambda (d) (s10e-test--map-stage
                  d "mutation" (lambda (r) (setf (alist-get 'status r) "PASS"
                                                 (alist-get 'exit_code r) 1) r))))
    (should (s10e-test--has "mutation: exit code 1" (s10e-test--validate binary evidence)))))

(ert-deftest nelisp-eln-s610-evidence/tampered-evidence-fails ()
  (skip-unless (s10e-test--eln-ok-p))
  ;; A PASS row whose result lines lack the required markers.
  (s10e-test--with (binary evidence)
    (s10e-test--mutate
     evidence
     (lambda (d) (s10e-test--map-stage
                  d "forced" (lambda (r) (setf (alist-get 'result_lines r)
                                               (vector "NELISP-ELN-S610-FORCED-PASS pushes=0 landings=0")) r))))
    (let ((problems (s10e-test--validate binary evidence)))
      (should (s10e-test--has "forced: no result line matches" problems))))
  ;; Zero native calls in the S6.10 line.
  (s10e-test--with (binary evidence)
    (s10e-test--mutate
     evidence
     (lambda (d) (s10e-test--map-stage
                  d "s610-measure"
                  (lambda (r) (setf (alist-get 'result_lines r)
                                    (vector "S6_MEASURE_RESULT function=byte-compile-form status=PASS native_raw_calls=0 native_dispatch_calls=0 eln_sha256=x")) r))))
    (should (s10e-test--has "not both > 0" (s10e-test--validate binary evidence))))
  ;; Changed sources / commands / artifact sha.
  (s10e-test--with (binary evidence)
    (s10e-test--mutate evidence (lambda (d) (setf (alist-get 'source_sha256 d) (make-string 64 ?0)) d))
    (should (s10e-test--has "harness or lisp/ sources changed" (s10e-test--validate binary evidence))))
  (s10e-test--with (binary evidence)
    (s10e-test--mutate
     evidence
     (lambda (d) (s10e-test--map-stage
                  d "tamper" (lambda (r) (setf (alist-get 'cmd_digest r) (make-string 64 ?0)) r))))
    (should (s10e-test--has "tamper: stale: command" (s10e-test--validate binary evidence))))
  (s10e-test--with (binary evidence)
    (s10e-test--mutate evidence (lambda (d) (setf (alist-get 'eln_sha256 d) (make-string 64 ?0)) d))
    (should (s10e-test--has "pinned .eln" (s10e-test--validate binary evidence)))))

(ert-deftest nelisp-eln-s610-evidence/corpus-validator-failure-propagates ()
  (skip-unless (s10e-test--eln-ok-p))
  (s10e-test--with (binary evidence)
    (cl-letf (((symbol-function 'nelisp-eln-s6-corpus-validate)
               (lambda (&rest _) (list "byte-compile-form: no evidence row"))))
      (should (member "S6.22: byte-compile-form: no evidence row"
                      (s10e-test--validate binary evidence))))))

(provide 'nelisp-eln-s610-evidence-test)
;;; nelisp-eln-s610-evidence-test.el ends here
