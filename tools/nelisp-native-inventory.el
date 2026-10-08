;;; nelisp-native-inventory.el --- Check/generate the native inventory -*- lexical-binding:t -*-

(require 'cl-lib)

(defconst nn-source (or (getenv "NN_SOURCE") "scripts/nelisp-standalone-build.el"))
(defconst nn-inventory (or (getenv "NN_INVENTORY") "tools/nelisp-native-inventory.txt"))
(defconst nn-classifications
  (or (getenv "NN_CLASSIFICATIONS")
      "/home/madblack-21/.cache/tmp/nelisp-doc211/inventory/a2-classified.tsv"))

(defun nn-table ()
  (with-temp-buffer
    (insert-file-contents nn-source)
    (goto-char (point-min))
    (let (found form bridge)
      (while (and (not found) (condition-case nil (progn (setq form (read (current-buffer))) t)
                                (end-of-file nil)))
        ;; The generic evaluator entry is a root ABI export, not a Lisp
        ;; reader builtin. Audit its actual six-word source definition too.
        (cl-labels ((walk (node)
                      (when (consp node)
                        (if (and (eq (car node) 'defun) (eq (cadr node) 'nl_native_funcall_v2))
                            (progn
                              (when bridge (error "Duplicate F1 evaluator entry"))
                              (unless (= (length (nth 2 node)) 6) (error "F1 evaluator arity drift"))
                              (setq bridge "nl_native_funcall_v2"))
                          (walk (car node)) (walk (cdr node))))))
          (walk form))
        (when (and (consp form) (eq (car form) 'defconst)
                   (eq (cadr form) 'nelisp-standalone--reader-builtins))
          (setq found (cl-letf (((symbol-function 'nelisp-standalone--runtime-reload-enabled-p)
                                 (lambda () nil)))
                        (eval (nth 2 form) t)))))
      (unless found (error "reader-builtins defconst not found in %s" nn-source))
      (append found (and bridge (list bridge))))))

(defun nn-classifications ()
  (let ((result (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents nn-classifications)
      (let* ((rows (split-string (buffer-string) "\n" t))
             (header (split-string (pop rows) "\t"))
             (state-col (cl-position "state" header :test #'equal))
             (name-col 0)
             (verdict-col (cl-position "verdict" header :test #'equal))
             (reason-col (cl-position "rule/reason/replacement sketch" header :test #'equal)))
        (dolist (row rows)
          (let ((fields (split-string row "\t")))
            (when (and (equal (nth state-col fields) "native")
                       (< (max name-col verdict-col reason-col) (length fields)))
              (puthash (nth name-col fields)
                       (cons (nth verdict-col fields) (nth reason-col fields)) result))))))
    result))

(defun nn-lines (&optional classes)
  (mapcar (lambda (name)
            (let* ((entry (if (equal name "nl_native_funcall_v2")
                              '("native-must-stay" . "Evaluator entry (clause 2), authenticated rooted funcall and exit publication; Lisp reference nelisp-native-funcall-v2-reference; measurements in F1.1/F1.2.")
                            (and classes (gethash name classes))))
                   (verdict (or (car-safe entry) "native-review"))
                   (reason (or (cdr-safe entry) "No classification row; review against Doc 211 §6.")))
              (format "%s\t%s\t%s" name verdict
                      (replace-regexp-in-string "[\t\r\n]+" " " reason))))
          (nn-table)))

(defun nn-check ()
  (let* ((expected (nn-lines))
         (actual (with-temp-buffer
                   (insert-file-contents nn-inventory)
                   (cdr (split-string (buffer-string) "\n" t))))
         (expected-names (mapcar (lambda (line) (car (split-string line "\t"))) expected))
         (actual-names (mapcar (lambda (line) (car (split-string line "\t"))) actual)))
    (unless (equal expected-names actual-names)
      (error "Native inventory/table mismatch: missing=%S stale=%S"
             (cl-set-difference expected-names actual-names :test #'equal)
             (cl-set-difference actual-names expected-names :test #'equal)))
    (dolist (line actual)
      (unless (= (length (split-string line "\t")) 3)
        (error "Malformed inventory line: %s" line)))
    (princ (format "GATE-COUNT checked=%d findings=0\n" (length expected)))))

(if (getenv "NN_GENERATE")
    (let ((classes (nn-classifications)))
      (with-temp-file nn-inventory
        (insert "# name\tverdict\treason\n")
        (dolist (line (nn-lines classes)) (insert line "\n")))
      (princ (format "generated %d native entries\n" (length (nn-table)))))
  (nn-check))

;;; nelisp-native-inventory.el ends here
