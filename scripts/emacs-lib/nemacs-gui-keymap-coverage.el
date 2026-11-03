;;; nemacs-gui-keymap-coverage.el --- compare host Emacs keys with GUI bridge runtime  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a planning aid for the nelisp-emacs / nelisp-gui boundary.
;; It reads GNU Emacs' current global keymap from the host process, reads the
;; GUI bridge keymap literals from `nemacs-gui-file-bridge-runtime.el', and
;; prints a TSV coverage table.  The GUI side should not interpret this table;
;; it remains responsible only for transporting raw key sequences.

;;; Code:

(require 'cl-lib)

(defvar nemacs-gui-keymap-coverage-runtime
  (expand-file-name
   "../src/nemacs-gui-file-bridge-runtime.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Runtime source file that owns GUI bridge keymap semantics.")

(defun nemacs-gui-keymap-coverage--slurp (file)
  "Return FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-keymap-coverage--string-concat-form (form)
  "Return the string represented by FORM when it is a string/concat form."
  (cond
   ((stringp form) form)
   ((and (consp form)
         (eq (car form) 'concat))
    (mapconcat #'nemacs-gui-keymap-coverage--string-concat-form
               (cdr form)
               ""))
   (t "")))

(defun nemacs-gui-keymap-coverage--add-runtime-binding-lines (bindings text)
  "Parse keymap TEXT and add its entries to BINDINGS."
  (dolist (line (split-string text "\n" t))
    (let ((fields (split-string line "\t")))
      (when (>= (length fields) 2)
        (puthash (string-trim (nth 0 fields))
                 (string-trim (nth 1 fields))
                 bindings)))))

(defun nemacs-gui-keymap-coverage--runtime-bindings (source)
  "Return a hash mapping runtime keys to commands parsed from SOURCE."
  (let ((bindings (make-hash-table :test #'equal))
        (pos 0))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (condition-case nil
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (eq (car form) 'setq))
                (let ((pairs (cdr form)))
                  (while pairs
                    (let ((variable (pop pairs))
                          (value (pop pairs)))
                      (when (memq variable
                                  '(files--keymap-source
                                    files--minibuffer-keymap-source))
                        (nemacs-gui-keymap-coverage--add-runtime-binding-lines
                         bindings
                         (nemacs-gui-keymap-coverage--string-concat-form
                          value))))))))
          (end-of-file
           (goto-char (point-max)))
          (invalid-read-syntax
           (setq pos (1+ pos))
           (goto-char pos)))))
    bindings))

(defun nemacs-gui-keymap-coverage--runtime-functions (source)
  "Return a hash of command names implemented with fset in SOURCE."
  (let ((functions (make-hash-table :test #'equal))
        (pos 0))
    (while (string-match "(fset '\\([^][() \n\t]+\\)" source pos)
      (puthash (match-string 1 source) t functions)
      (setq pos (match-end 0)))
    functions))

(defun nemacs-gui-keymap-coverage--keyboard-key-p (description)
  "Return non-nil if DESCRIPTION is a keyboard key worth tracking for GUI."
  (and (not (string-match-p
             "\\(<[^>]+>\\|menu-bar\\|tool-bar\\|mouse\\|wheel\\|drag\\|down-mouse\\|double-mouse\\|triple-mouse\\|remap\\)"
             description))
       (not (string-match-p "\\.\\." description))
       (not (string-match-p "\\`ESC " description))))

(defun nemacs-gui-keymap-coverage--host-bindings ()
  "Return a hash mapping host GNU Emacs global keyboard keys to commands."
  (let ((bindings (make-hash-table :test #'equal)))
    (dolist (entry (accessible-keymaps (current-global-map)))
      (let ((prefix (car entry))
            (keymap (cdr entry)))
        (map-keymap
         (lambda (event binding)
           (when (and (symbolp binding)
                      (commandp binding))
             (let ((key (key-description (vconcat prefix (vector event)))))
               (when (or (equal key "ESC 0..9")
                         (nemacs-gui-keymap-coverage--keyboard-key-p key))
                 (puthash key (symbol-name binding) bindings)))))
         keymap)))
    (when (gethash "ESC 0..9" bindings)
      (let ((command (gethash "ESC 0..9" bindings))
            (digit 0))
        (while (< digit 10)
          (puthash (concat "M-" (number-to-string digit)) command bindings)
          (setq digit (1+ digit)))))
    (remhash "ESC 0..9" bindings)
    bindings))

(defun nemacs-gui-keymap-coverage--hash-keys (hash)
  "Return HASH keys."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) hash)
    keys))

(defun nemacs-gui-keymap-coverage--field (value)
  "Return VALUE as a TSV field."
  (or value ""))

(defun nemacs-gui-keymap-coverage-run ()
  "Print GNU Emacs to GUI bridge keymap coverage as TSV."
  (let* ((source (nemacs-gui-keymap-coverage--slurp
                  nemacs-gui-keymap-coverage-runtime))
         (runtime-bindings
          (nemacs-gui-keymap-coverage--runtime-bindings source))
         (runtime-functions
          (nemacs-gui-keymap-coverage--runtime-functions source))
         (host-bindings (nemacs-gui-keymap-coverage--host-bindings))
         (all-keys (sort (delete-dups
                          (append (nemacs-gui-keymap-coverage--hash-keys host-bindings)
                                  (nemacs-gui-keymap-coverage--hash-keys runtime-bindings)))
                         #'string<))
         (implemented 0)
         (different 0)
         (missing 0)
         (command-missing 0)
         (runtime-only 0))
    (princ "status\tkey\temacs-command\tnemacs-command\tnemacs-command-implemented\n")
    (dolist (key all-keys)
      (let* ((host-command (gethash key host-bindings))
             (runtime-command (gethash key runtime-bindings))
             (runtime-command-implemented
              (and runtime-command
                   (gethash runtime-command runtime-functions)))
             (status
              (cond
               ((and host-command runtime-command
                     (not (equal host-command runtime-command)))
                (setq different (1+ different))
                "different")
               ((and host-command runtime-command
                     runtime-command-implemented)
                (setq implemented (1+ implemented))
                "implemented")
               ((and host-command runtime-command)
                (setq command-missing (1+ command-missing))
                "command-missing")
               (host-command
                (setq missing (1+ missing))
                "missing")
               (t
                (setq runtime-only (1+ runtime-only))
                "runtime-only"))))
        (princ (mapconcat
                #'identity
                (list status
                      key
                      (nemacs-gui-keymap-coverage--field host-command)
                      (nemacs-gui-keymap-coverage--field runtime-command)
                      (if runtime-command-implemented "yes" "no"))
                "\t"))
        (princ "\n")))
    (message
     "implemented=%d different=%d command-missing=%d missing=%d runtime-only=%d"
     implemented different command-missing missing runtime-only)))

(when noninteractive
  (nemacs-gui-keymap-coverage-run))

(provide 'nemacs-gui-keymap-coverage)

;;; nemacs-gui-keymap-coverage.el ends here
