;;; nelisp-repl-reload.el --- publish the defuns of an ordinary module -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-artifact-reload-source-file' publishes a file whose top-level forms
;; are ALL `defun'.  That is the right contract for it: staging must not
;; execute a module's load-time effects, so a file that also contains
;; `require', `defvar' or a bare call is refused whole.  Measured 2026-09-12
;; against a consumer module (nelisp-emacs-lib src/emacs-mode.el):
;;
;;     :status rejected :phase preflight :reason (:unsupported-top-level require)
;;
;; Every real module opens with `require' and `defvar', so the repair loop the
;; REPL exists for -- edit one function, publish it, keep the session and its
;; state -- could not be pointed at one.  The workaround was to copy the
;; edited defun into a scratch file by hand, which is both a transcription
;; step and a second copy of the source that then drifts.
;;
;; This module closes the gap without weakening that contract.  It selects the
;; module's `defun' forms, copies their EXACT source bytes into a staging
;; file, and hands that file to the strict API.  The other top-level forms are
;; NAMED IN THE RESULT rather than run: a live `defvar' keeps the value the
;; session has given it and a `require' is not re-executed.  For a hot patch
;; that is the wanted behaviour, and for anything else the caller can see in
;; `:skipped' exactly what was not applied.
;;
;; What this does not do: it does not remove a definition deleted from the
;; file, it does not re-run load-time effects, and it does not make the
;; module's `:source' provenance point at the module -- `nelisp-repl-code-info'
;; will report the staging file, whose `:source-sha256' covers the selected
;; subset.  `:module' and `:staged-source' in the result say which is which.

;;; Code:

(require 'nelisp-artifact)

(defun nelisp-repl-reload--file-string (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defconst nelisp-repl-reload--whitespace '(?\s ?\t ?\n ?\f ?\r)
  "Characters that separate top-level forms.")

(defun nelisp-repl-reload--content-start (source index)
  "Return where the next form starts at or after INDEX in SOURCE.

Returns nil when only whitespace and comments remain.  This decides
whether there is anything left to read; it never decides where a form
ends -- the reader does that."
  (let ((len (length source))
        (i index)
        (start nil)
        (done nil))
    (while (not done)
      (cond
       ((>= i len)
        (setq done t))
       ((memq (aref source i) nelisp-repl-reload--whitespace)
        (setq i (1+ i)))
       ((eq (aref source i) ?\;)
        (while (and (< i len) (not (eq (aref source i) ?\n)))
          (setq i (1+ i))))
       (t
        (setq start i)
        (setq done t))))
    start))

(defun nelisp-repl-reload--spans (source)
  "Return (FORM START END) for each top-level form of SOURCE, in order.

START is the end of the previous form, so the slice START..END carries any
comment written between the two.

A truncated last form is an error naming its position, never a silently
shorter list: dropping it would publish a module minus whatever the
unclosed form was going to define."
  (let ((index 0)
        (spans nil)
        (done nil))
    (while (not done)
      (let ((content (nelisp-repl-reload--content-start source index)))
        (if (null content)
            (setq done t)
          (let ((read-result
                 (condition-case err
                     (read-from-string source content)
                   (end-of-file
                    ;; Carry the reader's own condition: the position says
                    ;; which form, and the condition says what the reader
                    ;; was still waiting for.
                    (error "nelisp-repl-reload: source ends inside the form at position %d: %S"
                           content err)))))
            (setq spans (cons (list (car read-result) index (cdr read-result))
                              spans))
            (setq index (cdr read-result))))))
    (nreverse spans)))

(defun nelisp-repl-reload--defun-name (form)
  "Return the name defined by FORM when it is a plain `defun', else nil."
  (and (consp form)
       (eq (car form) 'defun)
       (symbolp (nth 1 form))
       (nth 1 form)))

(defun nelisp-repl-reload--lexical-cookie (source)
  "Return SOURCE's own file-local variables line when it sets lexical binding.

Copied verbatim so the staged subset compiles under the binding rule the
module declares.  A module without the cookie stages without one."
  (let* ((newline (string-match-p "\n" source))
         (first (if newline (substring source 0 newline) source)))
    (and (string-match-p "-\\*-" first)
         (string-match-p "lexical-binding:" first)
         (concat first "\n"))))

(defconst nelisp-repl-reload--variable-heads '(defvar defconst defcustom)
  "Top-level heads whose NAME must still be declared special when skipped.")

(defun nelisp-repl-reload--declaration (form)
  "Return the variable FORM declares, when FORM declares one."
  (and (consp form)
       (memq (car form) nelisp-repl-reload--variable-heads)
       (symbolp (nth 1 form))
       (nth 1 form)))

(defun nelisp-repl-reload-select (module-path &optional names)
  "Return the staging plan for the `defun' forms of MODULE-PATH.

NAMES limits the selection to those function names (a symbol or a list);
nil selects every `defun'.  The plan is a plist with `:source' (the staged
text), `:selected' (names, in file order), `:declared' (the module's own
variables, whose initializers were NOT re-run), `:skipped' ((HEAD
. POSITION) for every top-level form left out) and `:missing' (requested
names the file does not define).

`:declared' is reporting, not staging: the staged text contains the
selected defuns only.  The module's variables keep the values the session
holds, and the republished functions read them through the standalone's
own namespace (see `nelisp-bc--host-globals-p' -- a VM that consulted the
wrong namespace was what made this report `nelisp-unbound-variable' for a
variable the session had a value for)."
  (let* ((wanted (cond ((null names) nil)
                       ((symbolp names) (list names))
                       (t names)))
         (source (nelisp-repl-reload--file-string module-path))
         (spans (nelisp-repl-reload--spans source))
         (cookie (nelisp-repl-reload--lexical-cookie source))
         (selected nil)
         (declared nil)
         (skipped nil)
         (pieces nil))
    (dolist (span spans)
      (let* ((form (nth 0 span))
             (start (nth 1 span))
             (end (nth 2 span))
             (name (nelisp-repl-reload--defun-name form)))
        (if (and name (or (null wanted) (memq name wanted)))
            (progn
              (setq selected (cons name selected))
              (setq pieces (cons (substring source start end) pieces)))
          (let ((variable (nelisp-repl-reload--declaration form)))
            (when variable
              (setq declared (cons variable declared))))
          (setq skipped (cons (cons (if (consp form) (car form) form) start)
                              skipped)))))
    (setq selected (nreverse selected))
    (setq declared (nreverse declared))
    (list :source (concat (or cookie "")
                          (apply #'concat (nreverse pieces))
                          "\n")
          :selected selected
          :declared declared
          :skipped (nreverse skipped)
          :missing (let ((missing nil))
                     (dolist (name wanted)
                       (unless (memq name selected)
                         (setq missing (cons name missing))))
                     (nreverse missing)))))

(defun nelisp-repl-reload-defuns (module-path &optional names build-id)
  "Publish the `defun' forms of MODULE-PATH into the running session.

NAMES limits the publication to those functions; nil publishes every
`defun' in the file.  BUILD-ID is passed through to
`nelisp-artifact-reload-source-file'.

The return value is that function's own result plist plus `:module' (the
file the definitions were read from), `:staged-source' (the file that was
actually compiled, kept so the provenance the strict API recorded can
still be resolved), `:selected', `:declared' (the module variables whose
initializers were not re-run), `:skipped'
and `:missing'.  A request
that selects nothing, or that names a function the file does not define,
is refused here with `:format' `nelisp-repl-reload-v1' and `:status'
`rejected' before anything is staged."
  (let* ((module (expand-file-name module-path))
         (plan (nelisp-repl-reload-select module names))
         (selected (plist-get plan :selected))
         (missing (plist-get plan :missing)))
    (cond
     (missing
      (list :format 'nelisp-repl-reload-v1 :status 'rejected :phase 'select
            :module module :selected selected
            :declared (plist-get plan :declared)
            :skipped (plist-get plan :skipped)
            :missing missing
            :reason (list :not-defined-in-module missing)))
     ((null selected)
      (list :format 'nelisp-repl-reload-v1 :status 'rejected :phase 'select
            :module module :selected nil
            :declared (plist-get plan :declared)
            :skipped (plist-get plan :skipped)
            :missing nil
            :reason (list :no-defun-selected module)))
     (t
      ;; A constant prefix: `file-name-base' is not defined in the standalone
      ;; runtime this runs in (measured 2026-09-12 against target/nelisp built
      ;; from 2fb0fd8f3), and the module is identified by `:module' in the
      ;; result rather than by the staging file's name.
      (let ((staged (make-temp-file "nelisp-repl-reload-" nil ".el")))
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file staged
            (insert (plist-get plan :source))))
        (append (nelisp-artifact-reload-source-file staged build-id)
                (list :module module
                      :staged-source staged
                      :selected selected
                      :declared (plist-get plan :declared)
                      :skipped (plist-get plan :skipped)
                      :missing nil)))))))

(provide 'nelisp-repl-reload)

;;; nelisp-repl-reload.el ends here
