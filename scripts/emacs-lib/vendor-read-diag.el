;;; vendor-read-diag.el --- standalone vendor reader diagnostic -*- lexical-binding: t; -*-

;;; Code:

(defvar vendor-read-diag-file nil
  "Vendor file path to inspect.")

(defvar vendor-read-diag-limit 5
  "Maximum number of top-level forms to read.")

(defun vendor-read-diag--read-file (file)
  "Return FILE contents as a string."
  (cond
   ((fboundp 'nelisp--syscall-read-file)
    (or (nelisp--syscall-read-file file) ""))
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))))

(defun vendor-read-diag--space-char-p (char)
  "Return non-nil when CHAR is whitespace."
  (or (= char 32) ; space
      (= char 9)  ; tab
      (= char 10) ; newline
      (= char 13) ; carriage return
      (= char 12))) ; form feed

(defun vendor-read-diag--skip-space-comments (text pos)
  "Return first non-space/comment position in TEXT at or after POS."
  (let ((len (length text))
        done)
    (while (and (< pos len) (not done))
      (let ((ch (aref text pos)))
        (cond
         ((vendor-read-diag--space-char-p ch)
          (setq pos (1+ pos)))
         ((= ch ?\;)
          (while (and (< pos len)
                      (not (= (aref text pos) ?\n)))
            (setq pos (1+ pos))))
         (t
          (setq done t)))))
    pos))

(defun vendor-read-diag--atom-terminator-p (char)
  "Return non-nil when CHAR terminates a top-level atom."
  (or (vendor-read-diag--space-char-p char)
      (= char ?\;)
      (= char ?\()
      (= char ?\))
      (= char ?\")
      (= char ?\')))

(defun vendor-read-diag--form-end (text pos)
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
                      (not (vendor-read-diag--atom-terminator-p
                            (aref text i))))
            (setq i (1+ i)))
          (setq done t))
         (t
          (setq i (1+ i))))))
    i))

(defun vendor-read-diag--head (form)
  "Return a short printable head for FORM."
  (cond
   ((consp form) (car form))
   ((symbolp form) form)
   (t (type-of form))))

(defun vendor-read-diag-run ()
  "Read top-level forms from `vendor-read-diag-file' without evaluating."
  (condition-case err
      (let* ((path vendor-read-diag-file)
             (text (vendor-read-diag--read-file path))
             (len (length text))
             (pos 0)
             (index 0))
        (princ (format "vendor-read path=%S len=%S\n" path len))
        (while (< index vendor-read-diag-limit)
          (let ((form-pos (vendor-read-diag--skip-space-comments text pos)))
            (if (>= form-pos len)
                (setq index vendor-read-diag-limit)
              (let* ((form-end (vendor-read-diag--form-end text form-pos)))
                (princ (format "vendor-read index=%S read-start=%S read-end=%S span=%S\n"
                               (1+ index) form-pos form-end
                               (- form-end form-pos)))
                (let* ((read-result (read-from-string text form-pos form-end))
                     (form (car read-result))
                     (next-pos (cdr read-result)))
                  (setq index (1+ index)
                        pos next-pos)
                  (princ (format "vendor-read index=%S read-done next=%S head=%S\n"
                                 index next-pos (vendor-read-diag--head form))))))))
        index)
    (error
     (princ (format "vendor-read error=%S\n" err))
     (signal (car err) (cdr err)))))

(provide 'vendor-read-diag)

;;; vendor-read-diag.el ends here
