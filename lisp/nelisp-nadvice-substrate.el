;;; nelisp-nadvice-substrate.el --- GNU oclosure.el + nadvice.el on NeLisp  -*- lexical-binding: t; -*-

;; This file is the autoload target of the nadvice entry points registered
;; by the stdlib prelude (`advice-add', `add-function', `advice--cd*r', ...).
;; GNU Emacs 31 preloads nadvice.el, whose advice objects are OClosures
;; (oclosure.el), whose places are handled by gv.el.  Here all three are the
;; genuine vendored GNU sources, loaded on first use; this file only supplies
;; the pieces GNU gets from its dump, without editing any vendored file:
;;
;; - The class objects `oclosure.el' builds on: `cl-slot-descriptor',
;;   `built-in-class' and the `t' .. `interpreted-function' branch of the
;;   built-in type DAG, taken as the exact top-level forms of GNU's
;;   cl-preloaded.el (vendor/staged-emacs-lisp), plus `cl--arglist-args' and
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

(defun nelisp-nadvice-substrate--skip-trivia (source position)
  "Return the first position at or after POSITION in SOURCE that starts a form.
Whitespace and `;' comment lines between top-level forms are skipped."
  (while (and (< position (length source))
              (eq (string-match "[ \t\n\r\f]+\\|;[^\n]*" source position)
                  position))
    (setq position (match-end 0)))
  position)

(defun nelisp-nadvice-substrate--stage-forms (library predicate)
  "Evaluate the top-level forms of LIBRARY's source for which PREDICATE is true.
The forms are read from the vendored GNU source exactly as written."
  (let ((file (locate-library library nil nil)))
    (unless file
      (signal 'file-missing (list "Cannot locate staged GNU source" library)))
    (let ((source (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
          (position 0))
      (while (< (setq position
                      (nelisp-nadvice-substrate--skip-trivia source position))
                (length source))
        (let ((read-result (read-from-string source position)))
          (setq position (cdr read-result))
          (when (funcall predicate (car read-result))
            (eval (car read-result) t)))))))

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
           (and (eq (car-safe form) 'cl--define-built-in-type)
                (memq (nth 1 form) '(t atom function compiled-function closure
                                       byte-code-function
                                       interpreted-function))))))))

(defun nelisp-nadvice-substrate--stage-cl-macs ()
  "Define the two cl-macs.el helpers oclosure.el uses, and provide `cl-macs'."
  (unless (fboundp 'cl--arglist-args)
    (nelisp-nadvice-substrate--stage-forms
     "cl-macs"
     (lambda (form)
       (or (and (eq (car-safe form) 'defconst)
                (eq (nth 1 form) 'cl--lambda-list-keywords))
           (and (eq (car-safe form) 'defun)
                (eq (nth 1 form) 'cl--arglist-args))))))
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
