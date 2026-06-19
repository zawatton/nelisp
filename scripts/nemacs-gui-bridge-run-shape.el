;;; nemacs-gui-bridge-run-shape.el --- summarize GUI bridge run body shape -*- lexical-binding: t; -*-

;;; Commentary:

;; Static shape inventory for `nemacs-gui-file-bridge-run'.  The runtime is a
;; large generated-style fset form, so this script gives the next refactor a
;; stable before/after artifact without loading the runtime.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar nemacs-gui-bridge-run-shape-source
  (expand-file-name
   "../src/nemacs-gui-file-bridge-runtime.el"
   (file-name-directory (or load-file-name buffer-file-name)))
  "GUI bridge runtime source file.")

(defvar nemacs-gui-bridge-run-shape-output
  (expand-file-name
   "../build/nemacs-gui-bridge-run-shape.org"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Org summary output path.")

(defun nemacs-gui-bridge-run-shape--slurp (file)
  "Return FILE contents."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nemacs-gui-bridge-run-shape--top-level-forms (text)
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

(defun nemacs-gui-bridge-run-shape--fset-name (form)
  "Return fset target symbol name from FORM, or nil."
  (when (and (consp form) (eq (car form) 'fset))
    (let ((target (cadr form)))
      (when (and (consp target) (eq (car target) 'quote)
                 (symbolp (cadr target)))
        (symbol-name (cadr target))))))

(defun nemacs-gui-bridge-run-shape--find-fset (forms name)
  "Find fset NAME in FORMS."
  (cl-find-if
   (lambda (form)
     (equal (nemacs-gui-bridge-run-shape--fset-name form) name))
   forms))

(defun nemacs-gui-bridge-run-shape--lambda-body (fset-form)
  "Return lambda body from FSET-FORM."
  (let ((value (caddr fset-form)))
    (unless (and (consp value) (eq (car value) 'lambda))
      (error "Expected lambda fset for nemacs-gui-file-bridge-run"))
    (cddr value)))

(defun nemacs-gui-bridge-run-shape--linear-run-forms (lambda-body)
  "Return a linear sequence from LAMBDA-BODY for phase splitting.
The bridge runner stores nearly all work in one top-level `let'.  Treat
its binding initializers and body forms as a linear stream so the
command-run boundary can split setup from writeback without counting
binding variable names as calls."
  (let (forms)
    (dolist (form lambda-body)
      (if (and (consp form) (eq (car form) 'let))
          (progn
            (dolist (binding (cadr form))
              (push (if (consp binding) (cadr binding) nil) forms))
            (dolist (body-form (cddr form))
              (push body-form forms)))
        (push form forms)))
    (nreverse forms)))

(defun nemacs-gui-bridge-run-shape--walk (form fn)
  "Walk FORM recursively and call FN for each cons."
  (when (consp form)
    (funcall fn form)
    (let ((rest form))
      (while (consp rest)
        (nemacs-gui-bridge-run-shape--walk (car rest) fn)
        (setq rest (cdr rest)))
      (when rest
        (nemacs-gui-bridge-run-shape--walk rest fn)))))

(defun nemacs-gui-bridge-run-shape--contains-call-p (form symbol)
  "Return non-nil when FORM contains a call to SYMBOL."
  (let ((found nil))
    (nemacs-gui-bridge-run-shape--walk
     form
     (lambda (node)
       (when (and (consp node) (eq (car node) symbol))
         (setq found t))))
    found))

(defun nemacs-gui-bridge-run-shape--split-at-command-run (body)
  "Split BODY before and after `files--command-loop-run-request-current-context'."
  (let ((before nil)
        (after nil)
        (seen nil))
    (dolist (form body)
      (cond
       ((not seen)
        (push form before)
        (when (nemacs-gui-bridge-run-shape--contains-call-p
               form 'files--command-loop-run-request-current-context)
          (setq seen t)))
       (t
        (push form after))))
    (list (nreverse before) (nreverse after) seen)))

(defun nemacs-gui-bridge-run-shape--inc (table key)
  "Increment KEY in TABLE."
  (puthash key (1+ (gethash key table 0)) table))

(defun nemacs-gui-bridge-run-shape--walk-calls (form fn)
  "Walk evaluable call positions in FORM and call FN for each call cons."
  (when (consp form)
    (let ((head (car form)))
      (cond
       ((memq head '(quote function))
        nil)
       ((memq head '(let let*))
        (funcall fn form)
        (dolist (binding (cadr form))
          (when (consp binding)
            (nemacs-gui-bridge-run-shape--walk-calls (cadr binding) fn)))
        (dolist (body-form (cddr form))
          (nemacs-gui-bridge-run-shape--walk-calls body-form fn)))
       ((eq head 'setq)
        (funcall fn form)
        (let ((pairs (cdr form)))
          (while (consp (cdr pairs))
            (nemacs-gui-bridge-run-shape--walk-calls (cadr pairs) fn)
            (setq pairs (cddr pairs)))))
       ((eq head 'lambda)
        (funcall fn form)
        (dolist (body-form (cddr form))
          (nemacs-gui-bridge-run-shape--walk-calls body-form fn)))
       ((symbolp head)
        (funcall fn form)
        (dolist (arg (cdr form))
          (nemacs-gui-bridge-run-shape--walk-calls arg fn)))
       (t
        (dolist (child form)
          (nemacs-gui-bridge-run-shape--walk-calls child fn)))))))

(defun nemacs-gui-bridge-run-shape--call-counts (forms)
  "Return call-count hash table for FORMS."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (form forms)
      (nemacs-gui-bridge-run-shape--walk-calls
       form
       (lambda (node)
         (when (and (consp node) (symbolp (car node)))
           (nemacs-gui-bridge-run-shape--inc
            counts (symbol-name (car node)))))))
    counts))

(defun nemacs-gui-bridge-run-shape--hash-keys (table)
  "Return keys from hash TABLE."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) table)
    keys))

(defun nemacs-gui-bridge-run-shape--prefix-total (counts prefix)
  "Return total call count in COUNTS whose key starts with PREFIX."
  (let ((total 0))
    (maphash
     (lambda (key value)
       (when (string-prefix-p prefix key)
         (setq total (+ total value))))
     counts)
    total))

(defun nemacs-gui-bridge-run-shape--symbol-total (counts symbols)
  "Return total call count in COUNTS for SYMBOLS."
  (let ((total 0))
    (dolist (symbol symbols)
      (setq total (+ total (gethash (symbol-name symbol) counts 0))))
    total))

(defun nemacs-gui-bridge-run-shape--top-prefix (counts prefix)
  "Return sorted entries in COUNTS whose key starts with PREFIX."
  (let (entries)
    (maphash
     (lambda (key value)
       (when (string-prefix-p prefix key)
         (push (cons key value) entries)))
     counts)
    (sort entries
          (lambda (a b)
            (or (> (cdr a) (cdr b))
                (and (= (cdr a) (cdr b))
                     (string< (car a) (car b))))))))

(defun nemacs-gui-bridge-run-shape--transport-name-in-node (node)
  "Return transport name set in NODE, or nil."
  (let ((name nil))
    (nemacs-gui-bridge-run-shape--walk
     node
     (lambda (inner)
       (when (and (not name)
                  (consp inner)
                  (eq (car inner) 'setq)
                  (eq (cadr inner) 'files--transport-name)
                  (stringp (cl-caddr inner)))
         (setq name (cl-caddr inner)))))
    name))

(defun nemacs-gui-bridge-run-shape--transport-names (forms call-symbols)
  "Return transport names around CALL-SYMBOLS calls in FORMS."
  (let (names)
    (dolist (form forms)
      (nemacs-gui-bridge-run-shape--walk
       form
       (lambda (node)
         (when (and (consp node)
                    (cl-some
                     (lambda (call-symbol)
                       (nemacs-gui-bridge-run-shape--contains-call-p
                        node call-symbol))
                     call-symbols))
           (let ((name (nemacs-gui-bridge-run-shape--transport-name-in-node
                        node)))
             (when name
               (push name names)))))))
    (sort (delete-dups (nreverse names)) #'string<)))

(defun nemacs-gui-bridge-run-shape--cmd-tests (forms)
  "Return command names compared with cmd in FORMS."
  (let (names)
    (dolist (form forms)
      (nemacs-gui-bridge-run-shape--walk
       form
       (lambda (node)
         (when (and (consp node)
                    (eq (car node) 'equal)
                    (eq (cadr node) 'cmd)
                    (stringp (cl-caddr node)))
           (push (cl-caddr node) names)))))
    (sort (delete-dups (nreverse names)) #'string<)))

(defun nemacs-gui-bridge-run-shape--line-number (text pattern)
  "Return 1-based line number of PATTERN in TEXT, or nil."
  (let ((pos (string-match-p pattern text)))
    (when pos
      (1+ (cl-count ?\n text :end pos)))))

(defun nemacs-gui-bridge-run-shape--insert-table (title entries)
  "Insert TITLE and count ENTRIES."
  (insert (format "\n** %s\n\n" title))
  (insert "| name | count |\n")
  (insert "|-+-------|\n")
  (dolist (entry entries)
    (insert (format "| %s | %d |\n" (car entry) (cdr entry)))))

(defun nemacs-gui-bridge-run-shape--insert-list (title values)
  "Insert TITLE and VALUES."
  (insert (format "\n** %s\n\n" title))
  (dolist (value values)
    (insert (format "- =%s=\n" value))))

(defun nemacs-gui-bridge-run-shape--write-summary (source output)
  "Write shape summary for SOURCE to OUTPUT."
  (let* ((text (nemacs-gui-bridge-run-shape--slurp source))
         (forms (nemacs-gui-bridge-run-shape--top-level-forms text))
         (run-form (nemacs-gui-bridge-run-shape--find-fset
                    forms "nemacs-gui-file-bridge-run"))
         (body (nemacs-gui-bridge-run-shape--lambda-body run-form))
         (linear-body (nemacs-gui-bridge-run-shape--linear-run-forms body))
         (split (nemacs-gui-bridge-run-shape--split-at-command-run
                 linear-body))
         (before (nth 0 split))
         (after (nth 1 split))
         (found-run (nth 2 split))
         (before-counts (nemacs-gui-bridge-run-shape--call-counts before))
         (after-counts (nemacs-gui-bridge-run-shape--call-counts after))
         (all-counts (nemacs-gui-bridge-run-shape--call-counts linear-body))
         (direct-read-symbols '(rdf files--transport-read-current))
         (cmd-tests (nemacs-gui-bridge-run-shape--cmd-tests after))
         (read-names
          (nemacs-gui-bridge-run-shape--transport-names
           before direct-read-symbols))
         (write-names
          (nemacs-gui-bridge-run-shape--transport-names
           after '(nl-write-file))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: Nemacs GUI Bridge Run Shape\n\n")
      (insert (format "- source: =%s=\n" source))
      (insert (format "- =nemacs-gui-file-bridge-run= starts near line: %s\n"
                      (or (nemacs-gui-bridge-run-shape--line-number
                           text "(fset 'nemacs-gui-file-bridge-run")
                          "unknown")))
      (insert (format "- command-run boundary found: %s\n" found-run))
      (insert (format "- linear body forms before boundary: %d\n"
                      (length before)))
      (insert (format "- linear body forms after boundary: %d\n"
                      (length after)))
      (insert (format "- direct transport reads before boundary: %d\n"
                      (nemacs-gui-bridge-run-shape--symbol-total
                       before-counts direct-read-symbols)))
      (insert (format "- direct transport writes after boundary: %d\n"
                      (gethash "nl-write-file" after-counts 0)))
      (insert (format "- =files--read*= calls before boundary: %d\n"
                      (nemacs-gui-bridge-run-shape--prefix-total
                       before-counts "files--read")))
      (insert (format "- =files--write*= calls after boundary: %d\n"
                      (nemacs-gui-bridge-run-shape--prefix-total
                       after-counts "files--write")))
      (insert (format "- command-specific =equal cmd= tests after boundary: %d\n"
                      (length cmd-tests)))
      (nemacs-gui-bridge-run-shape--insert-table
       "Top Calls Before Boundary"
       (cl-subseq
        (sort (mapcar (lambda (key) (cons key (gethash key before-counts)))
                      (nemacs-gui-bridge-run-shape--hash-keys before-counts))
              (lambda (a b) (> (cdr a) (cdr b))))
        0 (min 30 (hash-table-count before-counts))))
      (nemacs-gui-bridge-run-shape--insert-table
       "Top Calls After Boundary"
       (cl-subseq
        (sort (mapcar (lambda (key) (cons key (gethash key after-counts)))
                      (nemacs-gui-bridge-run-shape--hash-keys after-counts))
              (lambda (a b) (> (cdr a) (cdr b))))
        0 (min 30 (hash-table-count after-counts))))
      (nemacs-gui-bridge-run-shape--insert-table
       "Read Helpers Before Boundary"
       (nemacs-gui-bridge-run-shape--top-prefix before-counts "files--read"))
      (nemacs-gui-bridge-run-shape--insert-table
       "Write Helpers After Boundary"
       (nemacs-gui-bridge-run-shape--top-prefix after-counts "files--write"))
      (nemacs-gui-bridge-run-shape--insert-list
       "Direct Transport Reads Before Boundary" read-names)
      (nemacs-gui-bridge-run-shape--insert-list
       "Direct Transport Writes After Boundary" write-names)
      (insert "\n** Machine Notes\n\n")
      (insert "- The initial helper extraction target is the pre-boundary setup, not an extra wrapper.\n")
      (insert "- A valid no-op refactor must keep these counts stable before optimization.\n")
      (insert (format "- Total distinct calls in body: %d\n"
                      (hash-table-count all-counts))))))

(defun nemacs-gui-bridge-run-shape-batch ()
  "Generate GUI bridge run shape artifact."
  (nemacs-gui-bridge-run-shape--write-summary
   nemacs-gui-bridge-run-shape-source
   nemacs-gui-bridge-run-shape-output)
  (princ
   (format "nemacs-gui-bridge-run-shape: output=%s\n"
           nemacs-gui-bridge-run-shape-output)))

(provide 'nemacs-gui-bridge-run-shape)

;;; nemacs-gui-bridge-run-shape.el ends here
