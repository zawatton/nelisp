;;; vendor-form-walk.el --- eval vendor files form by form  -*- lexical-binding: t; -*-

;; Diagnostic helper for standalone NeLisp vendor onboarding.  This is
;; intentionally separate from the smoke gates: it prints progress
;; before and after each top-level form so timeout runs reveal the last
;; form that started.

;;; Code:

(defvar vendor-form-walk-file nil
  "Vendor file path to evaluate form by form.")

(defvar vendor-form-walk-start-index 1
  "One-based top-level form index to start evaluating.")

(defvar vendor-form-walk-start-pos nil
  "Optional zero-based file offset that corresponds to START-INDEX.")

(defvar vendor-form-walk-limit 0
  "Maximum number of forms to evaluate.  Zero means no limit.")

(defvar vendor-form-walk-print-every 1
  "Print progress for every Nth form.  START lines always print for N=1.")

(defvar vendor-form-walk-print-skips nil
  "When non-nil, print progress for forms skipped before START-INDEX.")

(defvar vendor-form-walk-print-read nil
  "When non-nil, print boundary/read progress before evaluating each form.")

(defvar vendor-form-walk-print-timing t
  "Non-nil means include elapsed time in done/error output when available.")

(defun vendor-form-walk--now ()
  "Return a timestamp suitable for elapsed reporting, or nil."
  (and (fboundp 'float-time)
       (ignore-errors (float-time))))

(defun vendor-form-walk--elapsed-field (start end)
  "Return a printable elapsed field for START and END."
  (let ((elapsed (and vendor-form-walk-print-timing
                      (numberp start)
                      (numberp end)
                      (- end start))))
    (if elapsed
        (format " elapsed=%S" elapsed)
      "")))

(defun vendor-form-walk--read-file (file)
  "Return FILE contents as a string."
  (cond
   ((fboundp 'nelisp--syscall-read-file)
    (or (nelisp--syscall-read-file file) ""))
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))))

(defun vendor-form-walk--head (form)
  "Return a short printable head for FORM."
  (cond
   ((consp form) (car form))
   ((symbolp form) form)
   (t (type-of form))))

(defun vendor-form-walk--raw-head (text pos end)
  "Return a cheap printable head for one form in TEXT from POS to END."
  (let ((i pos)
        (limit end))
    (when (and (< i limit) (= (aref text i) ?\())
      (setq i (1+ i)))
    (while (and (< i limit)
                (vendor-form-walk--space-char-p (aref text i)))
      (setq i (1+ i)))
    (let ((start i))
      (while (and (< i limit)
                  (not (vendor-form-walk--atom-terminator-p
                        (aref text i))))
        (setq i (1+ i)))
      (if (< start i)
          (substring text start i)
        "<form>"))))

(defun vendor-form-walk--space-char-p (char)
  "Return non-nil when CHAR is whitespace."
  (or (= char 32) ; space
      (= char 9)  ; tab
      (= char 10) ; newline
      (= char 13) ; carriage return
      (= char 12))) ; form feed

(defun vendor-form-walk--skip-space-comments (text pos)
  "Return first non-space/comment position in TEXT at or after POS."
  (let ((len (length text))
        (done nil))
    (while (and (< pos len) (not done))
      (let ((ch (aref text pos)))
        (cond
         ((vendor-form-walk--space-char-p ch)
          (setq pos (1+ pos)))
         ((= ch ?\;)
          (while (and (< pos len)
                      (not (= (aref text pos) ?\n)))
            (setq pos (1+ pos))))
         (t
          (setq done t)))))
    pos))

(defun vendor-form-walk--atom-terminator-p (char)
  "Return non-nil when CHAR terminates a top-level atom."
  (or (vendor-form-walk--space-char-p char)
      (= char ?\;)
      (= char ?\()
      (= char ?\))
      (= char ?\")
      (= char ?\')))

(defun vendor-form-walk--form-end (text pos)
  "Return a conservative end offset for one top-level form at POS."
  (let ((len (length text))
        (i pos)
        (depth 0)
        (in-string nil)
        (escaped nil)
        (done nil))
    (while (and (< i len) (not done))
      (let ((ch (aref text i)))
        (cond
         (in-string
          (cond
           (escaped (setq escaped nil))
           ((= ch ?\\) (setq escaped t))
           ((= ch ?\") (setq in-string nil)))
          (setq i (1+ i)))
         ((= ch ?\;)
          (while (and (< i len)
                      (not (= (aref text i) ?\n)))
            (setq i (1+ i))))
         ((= ch ?\")
          (setq in-string t)
          (setq i (1+ i)))
         ((= ch ?\?)
          ;; Character literals such as `?\)' contain delimiter
          ;; characters that must not affect list depth.
          (setq i (1+ i))
          (when (and (< i len) (= (aref text i) ?\\))
            (setq i (1+ i)))
          (when (< i len)
            (setq i (1+ i))))
         ((= ch ?\()
          (setq depth (1+ depth))
          (setq i (1+ i)))
         ((= ch ?\))
          (setq depth (1- depth))
          (setq i (1+ i))
          (when (<= depth 0)
            (setq done t)))
         ((= depth 0)
          (while (and (< i len)
                      (not (vendor-form-walk--atom-terminator-p
                            (aref text i))))
            (setq i (1+ i)))
          (setq done t))
         (t
          (setq i (1+ i))))))
    i))

(defun vendor-form-walk--print-p (index)
  "Return non-nil when progress for INDEX should be printed."
  (or (<= vendor-form-walk-print-every 1)
      (= 0 (% index vendor-form-walk-print-every))))

(defun vendor-form-walk-run (&optional file)
  "Evaluate FILE top-level forms one by one.
Returns the number of forms evaluated."
  (let* ((path (or file vendor-form-walk-file))
         (pos (or vendor-form-walk-start-pos 0))
         (index (if vendor-form-walk-start-pos
                    (1- vendor-form-walk-start-index)
                  0))
         (evaluated 0)
         (done nil)
         (current-eval-start nil)
         (load-file-name path)
         (buffer-file-name path)
         text)
    (unless path
      (error "vendor-form-walk-file is nil"))
    (princ (format "vendor-form-read file=%S status=start\n" path))
    (setq text (vendor-form-walk--read-file path))
    (princ (format "vendor-form-read file=%S status=done len=%d\n"
                   path (length text)))
    (while (not done)
      (condition-case err
          (let* ((form-pos (vendor-form-walk--skip-space-comments text pos))
                 (form-end (and (< form-pos (length text))
                                (vendor-form-walk--form-end text form-pos)))
                 (next-index (1+ index)))
            (when (>= form-pos (length text))
              (signal 'end-of-file nil))
            (when (and vendor-form-walk-print-read
                       (>= next-index vendor-form-walk-start-index))
              (princ (format "vendor-form index=%d status=boundary pos=%d end=%d span=%d head=%S\n"
                             next-index form-pos form-end
                             (- form-end form-pos)
                             (vendor-form-walk--raw-head
                              text form-pos form-end))))
            (cond
             ((< next-index vendor-form-walk-start-index)
              (setq pos form-end
                    index next-index)
              (when (and vendor-form-walk-print-skips
                         (vendor-form-walk--print-p index))
                (princ (format "vendor-form index=%d status=skip pos=%d head=%S\n"
                               index pos
                               (vendor-form-walk--raw-head
                                text form-pos form-end)))))
             (t
              (when vendor-form-walk-print-read
                (princ (format "vendor-form index=%d status=read-start pos=%d end=%d\n"
                               next-index form-pos form-end)))
              (let* ((form-text (substring text form-pos form-end))
                     (read-result (read-from-string form-text))
                     (form (car read-result))
                     (next-pos (+ form-pos (cdr read-result)))
                     (eval-start nil))
                (when vendor-form-walk-print-read
                  (princ (format "vendor-form index=%d status=read-done next=%d head=%S\n"
                                 next-index next-pos
                                 (vendor-form-walk--head form))))
                (when (<= next-pos form-pos)
                  (error "reader did not advance at pos %S form %S"
                         form-pos form))
                (setq pos next-pos
                      index next-index)
                (when (and (> vendor-form-walk-limit 0)
                           (>= evaluated vendor-form-walk-limit))
                  (setq done t))
                (unless done
                  (when (vendor-form-walk--print-p index)
                    (princ (format "vendor-form index=%d status=start pos=%d head=%S\n"
                                   index pos (vendor-form-walk--head form))))
                  (setq eval-start (vendor-form-walk--now)
                        current-eval-start eval-start)
                  (eval form t)
                  (setq current-eval-start nil)
                  (setq evaluated (1+ evaluated))
                  (when (vendor-form-walk--print-p index)
                    (princ (format "vendor-form index=%d status=done pos=%d head=%S%s\n"
                                   index pos (vendor-form-walk--head form)
                                   (vendor-form-walk--elapsed-field
                                    eval-start
                                    (vendor-form-walk--now))))))))))
        (end-of-file
         (setq done t))
        (error
         (princ (format "vendor-form index=%d status=error pos=%d err=%S%s\n"
                        (1+ index) pos err
                        (vendor-form-walk--elapsed-field
                         current-eval-start
                         (vendor-form-walk--now))))
         (signal (car err) (cdr err)))))
    (princ (format "vendor-form-summary file=%S forms=%d evaluated=%d feature-simple=%S\n"
                   path index evaluated (featurep 'simple)))
    evaluated))

(provide 'vendor-form-walk)

;;; vendor-form-walk.el ends here
