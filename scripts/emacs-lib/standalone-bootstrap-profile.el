;;; standalone-bootstrap-profile.el --- profile bootstrap bundle via standalone reader  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defvar standalone-bootstrap-profile-reader nil
  "Path to target/nelisp-standalone-reader.")

(defvar standalone-bootstrap-profile-bundle nil
  "Path to the generated nemacs bootstrap bundle.")

(defvar standalone-bootstrap-profile-limit nil
  "Optional maximum number of bundle sections to profile.")

(defvar standalone-bootstrap-profile-timeout nil
  "Advisory timeout string printed in diagnostics.
The host runner uses `call-process' directly; Makefile owns any outer timeout.")

(defun standalone-bootstrap-profile--number-or-nil (value)
  "Return numeric VALUE, or nil for nil/empty/non-numeric values."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (not (string= value ""))
         (not (string= value "nil")))
    (string-to-number value))
   (t nil)))

(defun standalone-bootstrap-profile--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun standalone-bootstrap-profile--sections (source)
  "Split bootstrap SOURCE into dependency-order sections.
Each returned entry is (NAME . TEXT)."
  (let ((pos 0)
        sections)
    (while (string-match "^;;; >>> \\(.+\\)$" source pos)
      (let* ((name (match-string 1 source))
             (start (match-beginning 0))
             (content-start (match-end 0))
             (next (and (string-match "^;;; >>> \\(.+\\)$" source content-start)
                        (match-beginning 0))))
        (push (cons name (substring source start (or next (length source))))
              sections)
        (setq pos (or next (length source)))))
    (nreverse sections)))

(defun standalone-bootstrap-profile--write-program (sections upto output)
  "Write a standalone-reader program containing SECTIONS through UPTO."
  (with-temp-file output
    (insert ";;; standalone bootstrap profile probe -*- lexical-binding: t; -*-\n")
    (cl-loop for idx from 0 below upto
             for section = (nth idx sections)
             do (insert (cdr section))
             do (insert "\n"))
    ;; The standalone reader currently reports Lisp errors as exit 0.
    ;; A successful probe must therefore reach this explicit sentinel.
    (insert "\n42\n")))

(defun standalone-bootstrap-profile--run-one (reader sections index)
  "Run READER with bundle SECTIONS through one-based INDEX."
  (let ((tmp (make-temp-file "nemacs-standalone-bootstrap-" nil ".el"))
        (start (float-time))
        exit elapsed)
    (unwind-protect
        (progn
          (standalone-bootstrap-profile--write-program sections index tmp)
          (setq exit (call-process reader nil nil nil tmp))
          (setq elapsed (- (float-time) start))
          (list exit elapsed))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun standalone-bootstrap-profile-batch ()
  "Profile generated bootstrap bundle sections through standalone-reader."
  (unless (and standalone-bootstrap-profile-reader
               (file-executable-p standalone-bootstrap-profile-reader))
    (error "standalone-bootstrap-profile-reader is not executable: %S"
           standalone-bootstrap-profile-reader))
  (unless (and standalone-bootstrap-profile-bundle
               (file-readable-p standalone-bootstrap-profile-bundle))
    (error "standalone-bootstrap-profile-bundle is not readable: %S"
           standalone-bootstrap-profile-bundle))
  (let* ((sections (standalone-bootstrap-profile--sections
                    (standalone-bootstrap-profile--read-file
                     standalone-bootstrap-profile-bundle)))
         (limit (or (standalone-bootstrap-profile--number-or-nil
                     standalone-bootstrap-profile-limit)
                    (length sections)))
         (count (min limit (length sections)))
         (failed nil))
    (princ (format "standalone-bootstrap-profile bundle=%S sections=%d limit=%d timeout=%S\n"
                   standalone-bootstrap-profile-bundle
                   (length sections)
                   count
                   standalone-bootstrap-profile-timeout))
    (cl-loop for index from 1 to count
             for section = (nth (1- index) sections)
             until failed
             do (princ (format "standalone-bootstrap-step index=%d file=%S status=start\n"
                               index (car section)))
             do (pcase-let ((`(,exit ,elapsed)
                              (standalone-bootstrap-profile--run-one
                               standalone-bootstrap-profile-reader
                               sections index)))
                  (if (and (numberp exit) (= exit 42))
                      (princ (format "standalone-bootstrap-step index=%d file=%S status=done elapsed=%S exit=%d\n"
                                     index (car section) elapsed exit))
                    (setq failed t)
                    (princ (format "standalone-bootstrap-step index=%d file=%S status=fail elapsed=%S exit=%S expected=42\n"
                                   index (car section) elapsed exit)))))
    (if failed
        (progn
          (princ "standalone-bootstrap-summary status=fail\n")
          (kill-emacs 1))
      (princ "standalone-bootstrap-summary status=done\n"))))

(provide 'standalone-bootstrap-profile)

;;; standalone-bootstrap-profile.el ends here
