;;; emacs-cc-census-other-w301.el --- Array and documentation primitives  -*- lexical-binding: t; -*-

(defun emacs-cc-census-other-w301--doc-value (value)
  "Resolve a documentation VALUE without substituting command keys."
  (cond
   ((stringp value) value)
   ;; The standalone has no dumped DOC file.  GNU also returns nil for
   ;; integer references when no DOC file is available.
   ((integerp value)
    (when (fboundp 'get-doc-string) (get-doc-string value)))
   ((and (consp value) (stringp (car value)) (integerp (cdr value)))
    (if (fboundp 'get-doc-string)
        (get-doc-string value)
      (if (file-exists-p (car value))
          (error "External documentation requires get-doc-string")
        (format "Cannot open doc string file \"%s\"\n" (car value)))))
   (t (eval value))))

(defun emacs-cc-census-other-w301--doc-result (value raw)
  "Apply command key substitution to VALUE unless RAW is non-nil."
  (if (and (stringp value) (not raw)
           (fboundp 'substitute-command-keys))
      (substitute-command-keys value)
    value))

(unless (fboundp 'documentation-property)
  (defun documentation-property (symbol property &optional raw)
    "Return SYMBOL's PROPERTY documentation, evaluating non-string values.
Unless RAW is non-nil, substitute command keys in the resulting string."
    (unless (symbolp symbol)
      (signal 'wrong-type-argument (list 'symbolp symbol)))
    (emacs-cc-census-other-w301--doc-result
     (emacs-cc-census-other-w301--doc-value (get symbol property)) raw)))

(unless (fboundp 'documentation)
  (defun documentation (function &optional raw)
    "Return FUNCTION's documentation string.
Unless RAW is non-nil, substitute command keys in the resulting string."
    (let ((original function)
          (property (and (symbolp function)
                         (get function 'function-documentation)))
          seen doc)
      (if property
          (setq doc (emacs-cc-census-other-w301--doc-value property))
        (while (symbolp function)
          (when (memq function seen)
            (signal 'cyclic-function-indirection (list original)))
          (setq seen (cons function seen))
          (unless (fboundp function)
            (signal 'void-function (list function)))
          (setq function (symbol-function function)))
        (while (eq (car-safe function) 'macro)
          (setq function (cdr function)))
        (cond
         ((eq (type-of function) 'interpreted-function)
          (when (> (length function) 4)
            (setq doc (aref function 4))))
         ((byte-code-function-p function)
          (when (> (length function) 4)
            (setq doc (aref function 4))))
         ((and (consp function)
               (memq (car function) '(lambda autoload))
               (consp (cdr function)))
          (setq doc (nth 2 function)))
         ((subrp function)
          ;; Native documentation is only available when the runtime
          ;; exposes a string, rather than GNU's internal sentinel t.
          (when (fboundp 'internal-subr-documentation)
            (let ((value (internal-subr-documentation function)))
              (when (stringp value) (setq doc value)))))
         (t (signal 'invalid-function (list function))))
        (setq doc (if (or (stringp doc) (integerp doc)
                          (and (consp doc) (stringp (car doc))
                               (integerp (cdr doc))))
                      (emacs-cc-census-other-w301--doc-value doc)
                    nil)))
      (emacs-cc-census-other-w301--doc-result doc raw))))

(unless (fboundp 'fillarray)
  (defun fillarray (array item)
    "Store ITEM in every element of ARRAY and return ARRAY.
String replacements must preserve the string's byte length."
    (unless (arrayp array)
      (signal 'wrong-type-argument (list 'arrayp array)))
    (cond
     ((char-table-p array)
      ;; GNU fills root slots, preserving allocated subtables.  The
      ;; standalone range API does not expose that allocation topology.
      (error "Char-table root slot access is unavailable"))
     ((stringp array)
      (unless (characterp item)
        (signal 'wrong-type-argument (list 'characterp item)))
      (if (multibyte-string-p array)
          (unless (= (string-bytes array)
                     (* (length array) (string-bytes (string item))))
            (error "Attempt to change byte length of a string"))
        (setq item (logand item 255))))
     ((bool-vector-p array) (setq item (and item t))))
    (let ((index 0) (size (length array)))
      (while (< index size)
        (aset array index item)
        (setq index (1+ index))))
    array))

(defun emacs-cc-census-other-w301--index-description (index)
  "Describe INDEX as a character in GNU's vector description format."
  (cond
   ((= index 9) "TAB")
   ((= index 13) "RET")
   ((= index 27) "ESC")
   ((= index 32) "SPC")
   ((= index 127) "DEL")
   ((< index 32)
    (concat "C-" (char-to-string
                  (if (and (> index 0) (< index 27))
                      (+ index 96)
                    (+ index 64)))))
   (t (char-to-string index))))

(defun emacs-cc-census-other-w301--describe-range (start end value describer)
  "Insert one non-nil VALUE for START through END using DESCRIBER."
  (when value
    (let ((label (emacs-cc-census-other-w301--index-description start)))
      (unless (= start end)
        (setq label
              (concat label " .. "
                      (emacs-cc-census-other-w301--index-description end))))
      (princ label)
      (princ (if (< (length label) 8) "\t\t" "\t"))
      (funcall (or describer #'princ) value)
      (terpri))))

(unless (fboundp 'describe-vector)
  (defun describe-vector (vector &optional describer)
    "Insert a description of VECTOR's non-nil contents in the current buffer.
Group consecutive equal values.  DESCRIBER defaults to `princ'."
    (unless (or (vectorp vector) (char-table-p vector))
      (signal 'wrong-type-argument (list 'vector-or-char-table-p vector)))
    (let ((standard-output (current-buffer)) (started nil))
      (if (char-table-p vector)
          (map-char-table
           (lambda (range value)
             (when value
               (unless started (terpri) (setq started t))
               (emacs-cc-census-other-w301--describe-range
                (if (consp range) (car range) range)
                (if (consp range) (cdr range) range) value describer)))
           vector)
        (let ((index 0) (size (length vector)))
          (while (< index size)
            (let ((start index) (value (aref vector index)))
              (setq index (1+ index))
              (while (and (< index size) (equal value (aref vector index)))
                (setq index (1+ index)))
              (when value
                (unless started (terpri) (setq started t))
                (emacs-cc-census-other-w301--describe-range
                 start (1- index) value describer)))))))
    nil))

(unless (fboundp 'funcall-with-delayed-message)
  (defun funcall-with-delayed-message (timeout message function)
    "Call FUNCTION, displaying MESSAGE if it takes TIMEOUT seconds.
TIMEOUT must be a number and MESSAGE must be a string."
    (unless (numberp timeout)
      (signal 'wrong-type-argument (list 'numberp timeout)))
    (unless (stringp message)
      (signal 'wrong-type-argument (list 'stringp message)))
    (let ((timer (run-at-time (max 0 timeout) nil #'message "%s" message)))
      (unwind-protect
          (funcall function)
        (cancel-timer timer)))))

(provide 'emacs-cc-census-other-w301)
;;; emacs-cc-census-other-w301.el ends here
