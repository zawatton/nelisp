;;; nelisp-runtime-image.el --- Runtime image commands in Elisp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runtime image CLI support intentionally lives in Elisp.  The image is
;; a replayable source bundle: loading an image evaluates the stored
;; forms in a fresh NeLisp environment.  Extending an image appends a new
;; source bundle after first validating that base + extension evaluate.

;;; Code:

(defconst nelisp-runtime-image--magic ";;; nelisp-runtime-image source-v1\n")

(defconst nelisp-runtime-image--usage
  "usage: nelisp dump-runtime-image IMAGE [FORM...]
       nelisp extend-runtime-image BASE-IMAGE OUT-IMAGE FORM...
       nelisp eval-runtime-image IMAGE FORM...
       nelisp exec-runtime-image IMAGE FORM...")

(defun nelisp-runtime-image--print-error (msg)
  (nelisp--write-stderr-line (concat "nelisp: " msg)))

(defun nelisp-runtime-image--join-forms (forms)
  "Join CLI FORM strings into one source string."
  (let ((out "")
        (cur forms))
    (while cur
      (setq out (if (= (length out) 0)
                    (car cur)
                  (concat out " " (car cur))))
      (setq cur (cdr cur)))
    out))

(defun nelisp-runtime-image--ensure-final-newline (source)
  "Return SOURCE with a trailing newline."
  (if (or (= (length source) 0)
          (not (= (aref source (- (length source) 1)) ?\n)))
      (concat source "\n")
    source))

(defun nelisp-runtime-image--wrap-source (source)
  "Wrap SOURCE as a runtime-image source bundle."
  (concat nelisp-runtime-image--magic
          "(progn\n"
          (nelisp-runtime-image--ensure-final-newline source)
          ")\n"))

(defun nelisp-runtime-image--read-file (path)
  "Read PATH as a string or signal an error."
  (let ((source (nelisp--syscall-read-file path)))
    (unless (stringp source)
      (error "cannot read runtime image: %s" path))
    source))

(defun nelisp-runtime-image--write-file (path source)
  "Write SOURCE to PATH or signal an error."
  (unless (eq (nl-write-file path source) t)
    (error "cannot write runtime image: %s" path))
  t)

(defun nelisp-runtime-image--eval-source (source)
  "Evaluate all top-level forms in SOURCE and return the last value."
  (let ((forms (nelisp--read-all-from-string-impl source))
        (last nil))
    (while forms
      (setq last (eval (car forms)))
      (setq forms (cdr forms)))
    last))

(defun nelisp-runtime-image-dump-cli (args)
  "Implement `nelisp dump-runtime-image' for ARGS."
  (let ((path (nth 1 args)))
    (if (null path)
        (progn
          (nelisp-runtime-image--print-error nelisp-runtime-image--usage)
          2)
      (condition-case err
          (let ((source (nelisp-runtime-image--join-forms (cdr (cdr args)))))
            (nelisp-runtime-image--eval-source source)
            (nelisp-runtime-image--write-file
             path (nelisp-runtime-image--wrap-source source))
            0)
        (error
         (nelisp-runtime-image--print-error
          (format "dump-runtime-image: %s" (error-message-string err)))
         1)))))

(defun nelisp-runtime-image-extend-cli (args)
  "Implement `nelisp extend-runtime-image' for ARGS."
  (let ((base-path (nth 1 args))
        (out-path (nth 2 args))
        (forms (cdr (cdr (cdr args)))))
    (if (or (null base-path) (null out-path) (null forms))
        (progn
          (nelisp-runtime-image--print-error nelisp-runtime-image--usage)
          2)
      (condition-case err
          (let* ((base-source (nelisp-runtime-image--read-file base-path))
                 (extension-source (nelisp-runtime-image--join-forms forms))
                 (extension-image
                  (nelisp-runtime-image--wrap-source extension-source)))
            (nelisp-runtime-image--eval-source base-source)
            (nelisp-runtime-image--eval-source extension-source)
            (nelisp-runtime-image--write-file
             out-path
             (concat (nelisp-runtime-image--ensure-final-newline base-source)
                     extension-image))
            0)
        (error
         (nelisp-runtime-image--print-error
          (format "extend-runtime-image: %s" (error-message-string err)))
         1)))))

(defun nelisp-runtime-image-eval-cli (args print-value)
  "Implement runtime image eval/exec for ARGS.
When PRINT-VALUE is non-nil, print the final form value."
  (let ((path (nth 1 args))
        (forms (cdr (cdr args))))
    (if (or (null path) (null forms))
        (progn
          (nelisp-runtime-image--print-error nelisp-runtime-image--usage)
          2)
      (condition-case err
          (let ((last nil))
            (nelisp-runtime-image--eval-source
             (nelisp-runtime-image--read-file path))
            (setq last
                  (nelisp-runtime-image--eval-source
                   (nelisp-runtime-image--join-forms forms)))
            (when print-value
              (nelisp--write-stdout-bytes (prin1-to-string last))
              (nelisp--write-stdout-bytes "\n"))
            0)
        (error
         (nelisp-runtime-image--print-error
          (format "eval-runtime-image: %s" (error-message-string err)))
         1)))))

(provide 'nelisp-runtime-image)

;;; nelisp-runtime-image.el ends here
