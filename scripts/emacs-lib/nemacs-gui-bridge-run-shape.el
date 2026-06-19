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

(defun nemacs-gui-bridge-run-shape--linear-run-entries (lambda-body)
  "Return linear entries from LAMBDA-BODY with source metadata."
  (let (entries)
    (dolist (form lambda-body)
      (if (and (consp form) (eq (car form) 'let))
          (progn
            (dolist (binding (cadr form))
              (push (list :source 'binding
                          :name (if (consp binding) (car binding) binding)
                          :form (if (consp binding) (cadr binding) nil))
                    entries))
            (dolist (body-form (cddr form))
              (push (list :source 'body :form body-form) entries)))
        (push (list :source 'top :form form) entries)))
    (nreverse entries)))

(defun nemacs-gui-bridge-run-shape--linear-run-forms (lambda-body)
  "Return a linear sequence from LAMBDA-BODY for phase splitting.
The bridge runner stores nearly all work in one top-level `let'.  Treat
its binding initializers and body forms as a linear stream so the
command-run boundary can split setup from writeback without counting
binding variable names as calls."
  (mapcar (lambda (entry) (plist-get entry :form))
          (nemacs-gui-bridge-run-shape--linear-run-entries lambda-body)))

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

(defun nemacs-gui-bridge-run-shape--split-entries-at-command-run (entries)
  "Split ENTRIES before and after `files--command-loop-run-request-current-context'."
  (let ((before nil)
        (after nil)
        (seen nil))
    (dolist (entry entries)
      (let ((form (plist-get entry :form)))
        (cond
         ((not seen)
          (push entry before)
          (when (nemacs-gui-bridge-run-shape--contains-call-p
                 form 'files--command-loop-run-request-current-context)
            (setq seen t)))
         (t
          (push entry after)))))
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

(defun nemacs-gui-bridge-run-shape--contains-call-prefix-p (form prefix)
  "Return non-nil when FORM contains a call whose name starts with PREFIX."
  (let ((found nil))
    (nemacs-gui-bridge-run-shape--walk-calls
     form
     (lambda (node)
       (when (and (consp node)
                  (symbolp (car node))
                  (string-prefix-p prefix (symbol-name (car node))))
         (setq found t))))
    found))

(defun nemacs-gui-bridge-run-shape--contains-symbol-p (form symbol)
  "Return non-nil when FORM contains SYMBOL anywhere."
  (cond
   ((eq form symbol) t)
   ((consp form)
    (or (nemacs-gui-bridge-run-shape--contains-symbol-p (car form) symbol)
        (nemacs-gui-bridge-run-shape--contains-symbol-p (cdr form) symbol)))
   (t nil)))

(defun nemacs-gui-bridge-run-shape--contains-setq-p (form symbol)
  "Return non-nil when FORM contains a setq to SYMBOL."
  (let ((found nil))
    (nemacs-gui-bridge-run-shape--walk
     form
     (lambda (node)
       (when (and (consp node)
                  (eq (car node) 'setq)
                  (eq (cadr node) symbol))
         (setq found t))))
    found))

(defconst nemacs-gui-bridge-run-shape--phase-order
  '("setup"
    "transport-input-bindings"
    "local-scratch-bindings"
    "bridge-context"
    "toolbar"
    "broad-state-read"
    "request-ingest"
    "buffer-init"
    "window-point-init"
    "command-run-prep"
    "command-run"
    "other")
  "Preferred phase order for bridge-run pre-boundary summaries.")

(defun nemacs-gui-bridge-run-shape--entry-phase (entry)
  "Return phase name for linear ENTRY."
  (let ((source (plist-get entry :source))
        (name (plist-get entry :name))
        (form (plist-get entry :form)))
    (cond
     ((or (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--refresh-transport-derived-paths)
          (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--ensure-standard-special-buffers))
      "setup")
     ((and (eq source 'binding)
           (nemacs-gui-bridge-run-shape--contains-call-p
            form 'files--transport-read-current))
      "transport-input-bindings")
     ((eq source 'binding)
      "local-scratch-bindings")
     ((nemacs-gui-bridge-run-shape--contains-call-p
       form 'files--command-loop-run-request-current-context)
      "command-run")
     ((or (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--command-loop-save-undo-if-needed-current-context)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--clamp-point)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--clamp-mark)
          (nemacs-gui-bridge-run-shape--contains-setq-p
           form 'files--bridge-session-initialized))
      "command-run-prep")
     ((nemacs-gui-bridge-run-shape--contains-call-p form 'files--handle-toolbar-click)
      "toolbar")
     ((or (nemacs-gui-bridge-run-shape--contains-call-p
           form 'emacs-command-loop-gui-ingest-request-context)
          (nemacs-gui-bridge-run-shape--transport-name-in-node form))
      "request-ingest")
     ((or (memq name '(transport-point transport-point-index
                       transport-mark transport-mark-index
                       kill-ring-index-scan
                       transport-window-start transport-window-start-index))
          (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--bridge-initialize-buffer-state)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--read-current-narrow-state)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--read-minibuffer-state)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--kill-ring-push)
          (and (consp form)
               (eq (car form) 'if)
               (or (nemacs-gui-bridge-run-shape--contains-call-p form 'rdf)
                   (nemacs-gui-bridge-run-shape--contains-symbol-p
                    form 'files--buffer-name))))
      "buffer-init")
     ((or (nemacs-gui-bridge-run-shape--contains-call-prefix-p
           form "files--read")
          (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--bridge-read-broad-state)
          (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--load-user-init)
          (and (consp form)
               (eq (car form) 'setq)
               (memq (cadr form) '(files--window-hscroll
                                    files--window-split-delta))))
      "broad-state-read")
     ((or (nemacs-gui-bridge-run-shape--contains-call-p form 'aref)
          (nemacs-gui-bridge-run-shape--contains-call-p
           form 'files--bridge-initialize-window-point-state)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--clamp-point)
          (nemacs-gui-bridge-run-shape--contains-call-p form 'files--clamp-mark))
      "window-point-init")
     ((and (consp form)
           (eq (car form) 'setq)
           (symbolp (cadr form))
           (or (string-prefix-p "files--bridge" (symbol-name (cadr form)))
               (eq (cadr form) 'files--prefix-arg)))
      "bridge-context")
     ((and (consp form)
           (eq (car form) 'if)
           (or (nemacs-gui-bridge-run-shape--contains-call-p form 'intern)
               (nemacs-gui-bridge-run-shape--contains-call-p form 'plist-member)
               (nemacs-gui-bridge-run-shape--contains-call-p form 'plist-get)))
      "bridge-context")
     (t
      "other"))))

(defun nemacs-gui-bridge-run-shape--phase-index (phase)
  "Return sorting index for PHASE."
  (or (cl-position phase nemacs-gui-bridge-run-shape--phase-order
                   :test #'equal)
      (length nemacs-gui-bridge-run-shape--phase-order)))

(defun nemacs-gui-bridge-run-shape--phase-summaries (entries direct-read-symbols)
  "Return phase summaries for ENTRIES using DIRECT-READ-SYMBOLS."
  (let ((phase-forms (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((phase (nemacs-gui-bridge-run-shape--entry-phase entry)))
        (puthash phase
                 (cons (plist-get entry :form)
                       (gethash phase phase-forms nil))
                 phase-forms)))
    (let (summaries)
      (maphash
       (lambda (phase forms)
         (let* ((forms (nreverse forms))
                (counts (nemacs-gui-bridge-run-shape--call-counts forms)))
           (push (list :phase phase
                       :forms (length forms)
                       :direct-read
                       (nemacs-gui-bridge-run-shape--symbol-total
                        counts direct-read-symbols)
                       :direct-write (gethash "nl-write-file" counts 0)
                       :read-helper
                       (nemacs-gui-bridge-run-shape--prefix-total
                        counts "files--read")
                       :write-helper
                       (nemacs-gui-bridge-run-shape--prefix-total
                        counts "files--write"))
                 summaries)))
       phase-forms)
      (sort summaries
            (lambda (a b)
              (< (nemacs-gui-bridge-run-shape--phase-index
                  (plist-get a :phase))
                 (nemacs-gui-bridge-run-shape--phase-index
                  (plist-get b :phase))))))))

(defun nemacs-gui-bridge-run-shape--insert-phase-table (title summaries)
  "Insert TITLE and phase SUMMARIES."
  (insert (format "\n** %s\n\n" title))
  (insert "| phase | forms | direct-read | direct-write | read-helper | write-helper |\n")
  (insert "|-+-------+-------------+--------------+-------------+--------------|\n")
  (dolist (summary summaries)
    (insert (format "| %s | %d | %d | %d | %d | %d |\n"
                    (plist-get summary :phase)
                    (plist-get summary :forms)
                    (plist-get summary :direct-read)
                    (plist-get summary :direct-write)
                    (plist-get summary :read-helper)
                    (plist-get summary :write-helper)))))

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
         (linear-entries
          (nemacs-gui-bridge-run-shape--linear-run-entries body))
         (linear-body (mapcar (lambda (entry) (plist-get entry :form))
                              linear-entries))
         (entry-split
          (nemacs-gui-bridge-run-shape--split-entries-at-command-run
           linear-entries))
         (before-entries (nth 0 entry-split))
         (after-entries (nth 1 entry-split))
         (found-run (nth 2 entry-split))
         (before (mapcar (lambda (entry) (plist-get entry :form))
                         before-entries))
         (after (mapcar (lambda (entry) (plist-get entry :form))
                        after-entries))
         (before-counts (nemacs-gui-bridge-run-shape--call-counts before))
         (after-counts (nemacs-gui-bridge-run-shape--call-counts after))
         (all-counts (nemacs-gui-bridge-run-shape--call-counts linear-body))
         (direct-read-symbols '(rdf files--transport-read-current))
         (phase-summaries
          (nemacs-gui-bridge-run-shape--phase-summaries
           before-entries direct-read-symbols))
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
      (nemacs-gui-bridge-run-shape--insert-phase-table
       "Pre-Boundary Phases" phase-summaries)
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
      (insert "- The pre-boundary phase table is the next guard for phase helper extraction.\n")
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
