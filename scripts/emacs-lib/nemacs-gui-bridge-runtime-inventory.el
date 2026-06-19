;;; nemacs-gui-bridge-runtime-inventory.el --- inventory GUI bridge runtime symbols -*- lexical-binding: t; -*-

;;; Commentary:

;; Static inventory helper for docs/design/09-runtime-bridge-integration.org.
;; It classifies top-level `setq' variables and `fset' functions in
;; `src/nemacs-gui-file-bridge-runtime.el' without loading the runtime.

;;; Code:

(require 'cl-lib)

(defvar nemacs-gui-bridge-runtime-inventory-source
  (expand-file-name
   "../src/nemacs-gui-file-bridge-runtime.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "GUI bridge runtime source file.")

(defvar nemacs-gui-bridge-runtime-inventory-output
  (expand-file-name
   "../build/gui-bridge-runtime-inventory.tsv"
   (file-name-directory (or load-file-name buffer-file-name)))
  "TSV output path for the runtime inventory.")

(defun nemacs-gui-bridge-runtime-inventory--slurp (file)
  "Return FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-bridge-runtime-inventory--target (symbol-name kind)
  "Return target module for SYMBOL-NAME of KIND."
  (cond
   ((string-match-p
     "\\(transport\\|bridge\\|session\\|snapshot\\|request\\|response\\|toolbar\\)"
     symbol-name)
    "src/nemacs-gui-file-bridge-runtime.el")
   ((string-match-p "\\(minibuffer\\|read-string\\|completing-read\\)" symbol-name)
    "src/emacs-minibuffer.el")
   ((string-match-p
     "\\(command-execute\\|call-interactively\\|keymap\\|lookup-key\\|prefix-arg\\|keyboard\\)"
     symbol-name)
    "src/emacs-command-loop.el")
   ((string-match-p
     "\\(file\\|buffer\\|save\\|write\\|read-file\\|find-file\\|revert\\|directory\\)"
     symbol-name)
    "src/emacs-fileio.el")
   ((string-match-p
     "\\(window\\|frame\\|tab\\|modeline\\|cursor\\|redisplay\\|view\\)"
     symbol-name)
    "src/emacs-window.el")
   ((string-match-p "\\(help\\|describe\\|apropos\\)" symbol-name)
    "src/emacs-help.el")
   ((string-match-p "\\(info\\)" symbol-name)
    "src/emacs-info.el")
   ((string-match-p "\\(dired\\)" symbol-name)
    "src/emacs-dired.el")
   ((string-match-p "\\(project\\)" symbol-name)
    "src/emacs-project.el")
   ((string-match-p "\\(vc\\|magit\\)" symbol-name)
    "src/emacs-vc.el")
   ((string-match-p "\\(process\\|shell\\|compile\\|grep\\)" symbol-name)
    "src/emacs-process.el")
   ((string-match-p "\\(package\\|custom\\)" symbol-name)
    "src/emacs-package.el")
   ((string-match-p
     "\\(kill\\|yank\\|undo\\|mark\\|point\\|insert\\|delete\\|forward\\|backward\\|line\\|word\\|sentence\\|paragraph\\|rectangle\\|abbrev\\|fill\\|indent\\|search\\|replace\\)"
     symbol-name)
    "src/simple.el")
   ((equal kind "variable") "src/emacs-vars.el")
   (t "src/emacs-command-loop.el")))

(defun nemacs-gui-bridge-runtime-inventory--class (symbol-name kind target)
  "Return coarse class for SYMBOL-NAME of KIND moving to TARGET."
  (cond
   ((equal target "src/nemacs-gui-file-bridge-runtime.el")
    "adapter")
   ((equal kind "variable")
    "state")
   ((string-match-p
     "\\(command-execute\\|call-interactively\\|interactive\\|key\\|keyboard\\)"
     symbol-name)
    "command-loop")
   ((string-match-p "\\(write\\|read\\|serialize\\|snapshot\\|status\\)" symbol-name)
    "serialization")
   (t "runtime")))

(defun nemacs-gui-bridge-runtime-inventory--top-level-forms (text)
  "Return readable top-level forms from TEXT."
  (let ((forms nil)
        (pos 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file
           (goto-char (point-max)))
          (invalid-read-syntax
           (setq pos (1+ pos))
           (goto-char pos)))))
    (nreverse forms)))

(defun nemacs-gui-bridge-runtime-inventory--entries (forms)
  "Return symbol inventory entries from FORMS."
  (let (entries)
    (dolist (form forms)
      (cond
       ((and (consp form) (eq (car form) 'setq))
        (let ((pairs (cdr form)))
          (while pairs
            (let* ((symbol (pop pairs))
                   (_value (pop pairs))
                   (name (and (symbolp symbol) (symbol-name symbol))))
              (when name
                (let* ((target
                        (nemacs-gui-bridge-runtime-inventory--target
                         name "variable"))
                       (class
                        (nemacs-gui-bridge-runtime-inventory--class
                         name "variable" target)))
                  (push (list name "variable" class target) entries)))))))
       ((and (consp form) (eq (car form) 'fset))
        (let* ((quoted (cadr form))
               (symbol (if (and (consp quoted) (eq (car quoted) 'quote))
                           (cadr quoted)
                         quoted))
               (name (and (symbolp symbol) (symbol-name symbol))))
          (when name
            (let* ((target
                    (nemacs-gui-bridge-runtime-inventory--target
                     name "function"))
                   (class
                    (nemacs-gui-bridge-runtime-inventory--class
                     name "function" target)))
              (push (list name "function" class target) entries)))))))
    (sort (nreverse entries)
          (lambda (a b)
            (string< (car a) (car b))))))

(defun nemacs-gui-bridge-runtime-inventory-batch ()
  "Write GUI bridge runtime symbol inventory TSV."
  (let* ((forms
          (nemacs-gui-bridge-runtime-inventory--top-level-forms
           (nemacs-gui-bridge-runtime-inventory--slurp
            nemacs-gui-bridge-runtime-inventory-source)))
         (entries
          (nemacs-gui-bridge-runtime-inventory--entries forms))
         (counts (make-hash-table :test 'equal)))
    (make-directory (file-name-directory
                     nemacs-gui-bridge-runtime-inventory-output)
                    t)
    (with-temp-file nemacs-gui-bridge-runtime-inventory-output
      (insert "symbol\tkind\tclass\ttarget-module\n")
      (dolist (entry entries)
        (cl-incf (gethash (nth 2 entry) counts 0))
        (insert (mapconcat #'identity entry "\t") "\n")))
    (princ
     (format
      "nemacs-gui-bridge-runtime-inventory: symbols=%d adapter=%d state=%d runtime=%d command-loop=%d serialization=%d output=%s\n"
      (length entries)
      (gethash "adapter" counts 0)
      (gethash "state" counts 0)
      (gethash "runtime" counts 0)
      (gethash "command-loop" counts 0)
      (gethash "serialization" counts 0)
      nemacs-gui-bridge-runtime-inventory-output))))

(provide 'nemacs-gui-bridge-runtime-inventory)

;;; nemacs-gui-bridge-runtime-inventory.el ends here
