;;; nelisp-nadvice-substrate.el --- GNU oclosure.el + nadvice.el on NeLisp  -*- lexical-binding: t; -*-

;; This file is the autoload target of the nadvice entry points registered
;; by the stdlib prelude (`advice-add', `add-function', `advice--cd*r', ...).
;; GNU Emacs 31 preloads nadvice.el, whose advice objects are OClosures
;; (oclosure.el), whose places are handled by gv.el.  Here all three are the
;; genuine vendored GNU sources, loaded on first use; this file only supplies
;; the pieces GNU gets from its dump, without editing any vendored file:
;;
;; - The class objects `oclosure.el' builds on: `cl-slot-descriptor',
;;   `built-in-class' and every built-in type class in file order of the
;;   built-in type DAG and derived type definitions, taken as the exact
;;   top-level forms of GNU's cl-preloaded.el (vendor/staged-emacs-lisp),
;;   plus `cl--arglist-args' and
;;   `cl--lambda-list-keywords' from cl-macs.el.  The prelude's own
;;   cl-lib subset stands in for the rest of cl-macs, so `cl-macs' is
;;   provided (loading GNU's would replace that subset).
;; - `gv.el', loaded as it is.  It defines `setf', which would shadow the
;;   prelude's (that one knows this runtime's struct accessors); the prelude
;;   `setf' is put back and defers to `gv-get' for every place it does not
;;   know, which is what `add-function' and `advice-add' need.
;;
;; An OClosure here is an interpreted closure whose slot 4 holds the type
;; (see `make-interpreted-closure' in the prelude and the native slot view).
;; Mutable OClosure slots are not supported: the slot-2 view is a copy, so
;; `oclosure--set' cannot write through to the closure.

;;; Code:

(require 'cl-lib)

(defun nelisp-nadvice-substrate--stage-forms (library predicate expected)
  "Evaluate selected top-level forms of LIBRARY's vendored GNU source.
The forms are read one after another exactly as written, and those for which
PREDICATE is true are collected; reading stops as soon as EXPECTED forms have
been collected and they are then evaluated in file order.  Reaching the end of
the source first signals `end-of-file' from the reader, so a missing form is
never silent.  The source is read from a buffer with `read' alone: every
`length', `string-match', `read-from-string' offset or `forward-line' on a
large multibyte source costs time proportional to its whole length here, and
skipping trivia and reading from a string offset made this loader quadratic
(25 seconds for cl-macs.el)."
  (let ((file (locate-library library nil nil))
        (forms nil)
        (count 0))
    (unless file
      (signal 'file-missing (list "Cannot locate staged GNU source" library)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (< count expected)
        (let ((form (read (current-buffer))))
          (when (funcall predicate form)
            (push form forms)
            (setq count (1+ count))))))
    (dolist (form (nreverse forms))
      (eval form t))))

(defun nelisp-nadvice-substrate--defstruct-named-p (form names)
  "Non-nil if FORM is a `cl-defstruct' whose type name is in NAMES."
  (and (eq (car-safe form) 'cl-defstruct)
       (memq (if (consp (nth 1 form)) (car (nth 1 form)) (nth 1 form))
             names)))

(defun nelisp-nadvice-substrate--stage-classes ()
  "Define the class objects oclosure.el needs, from GNU's cl-preloaded.el."
  (unless (get 'interpreted-function 'cl--class)
    (nelisp-nadvice-substrate--stage-forms
     "cl-preloaded"
     (lambda (form)
       (or (nelisp-nadvice-substrate--defstruct-named-p
            form '(cl-slot-descriptor built-in-class))
           (and (memq (car-safe form) '(defun defmacro))
                (memq (nth 1 form) '(cl--copy-slot-descriptor
                                     cl--class-allparents
                                     cl--define-built-in-type)))
           ;; GNU 31.1 derived types (for example `natnum') may be parents
           ;; of later vendor definitions. Stage all type declarations in
           ;; source order so parents are registered before their children.
           (memq (car-safe form) '(cl--define-built-in-type cl-deftype))))
     61)
    ;; Guard the staging contract against GNU source changes: every type
    ;; declared by cl-preloaded must now be visible to cl's class lookup.
    (nelisp-nadvice-substrate--check-preloaded-types)))

(defun nelisp-nadvice-substrate--check-preloaded-types ()
  "Signal if any type declared in GNU cl-preloaded.el was not registered."
  (let ((file (locate-library "cl-preloaded" nil nil)) names)
    (unless file
      (signal 'file-missing (list "Cannot locate staged GNU source" "cl-preloaded")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (memq (car-safe form) '(cl--define-built-in-type cl-deftype))
                (push (nth 1 form) names))))
        (end-of-file nil)))
    (dolist (name (nreverse names))
      (unless (cl--find-class name)
        (error "GNU cl-preloaded type was not registered: %S" name)))))

(defun nelisp-nadvice-substrate--stage-cl-macs ()
  "Define the two cl-macs.el helpers oclosure.el uses, and provide `cl-macs'."
  (unless (fboundp 'cl--arglist-args)
    (nelisp-nadvice-substrate--stage-forms
     "cl-macs"
     (lambda (form)
       (or (and (eq (car-safe form) 'defconst)
                (eq (nth 1 form) 'cl--lambda-list-keywords))
           (and (eq (car-safe form) 'defun)
                (eq (nth 1 form) 'cl--arglist-args))))
     2))
  (unless (featurep 'cl-macs)
    (provide 'cl-macs)))

(defun nelisp-nadvice-substrate--load-gv ()
  "Load GNU gv.el, keeping the prelude's `setf' (which defers to gv)."
  (unless (featurep 'gv)
    (let ((prelude-setf (symbol-function 'setf)))
      (require 'gv)
      (fset 'setf prelude-setf))))

(nelisp-nadvice-substrate--stage-classes)
(nelisp-nadvice-substrate--stage-cl-macs)
(nelisp-nadvice-substrate--load-gv)
(require 'oclosure)
(require 'nadvice)

(provide 'nelisp-nadvice-substrate)

;;; nelisp-nadvice-substrate.el ends here
