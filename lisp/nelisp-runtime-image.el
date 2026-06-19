;;; nelisp-runtime-image.el --- Runtime image commands in Elisp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runtime image CLI support intentionally lives in Elisp.  The image is
;; a replayable source bundle: loading an image evaluates the stored
;; forms in a fresh NeLisp environment.  Extending an image appends a new
;; source bundle.  The command CLI validates base + extension before
;; writing; the standalone-reader command path writes source-v1 directly
;; and leaves full-image validation to normal file-mode replay.

;;; Code:

(defconst nelisp-runtime-image--magic ";;; nelisp-runtime-image source-v1\n")

(defconst nelisp-runtime-image--usage
  "usage: nelisp dump-runtime-image IMAGE [--load FILE]... [FORM...]
       nelisp extend-runtime-image BASE-IMAGE OUT-IMAGE [--load FILE]... FORM...
       nelisp eval-runtime-image IMAGE [--cache-kind nelc|neln|auto] FORM...
       nelisp exec-runtime-image IMAGE [--cache-kind nelc|neln|auto] FORM...")

(declare-function nelisp-artifact-compile-runtime-image-file
                  "nelisp-artifact"
                  (image-path artifact-path &optional manifest-path target
                              load-paths preloads requested-feature kind
                              native-policy))
(declare-function nelisp-artifact-load-file "nelisp-artifact" (artifact-path))
(declare-function nelisp-artifact--eval-forms "nelisp-artifact"
                  (forms &optional kind))
(declare-function nelisp-artifact--read-all-from-string
                  "nelisp-artifact" (source))
(declare-function nelisp-eval "nelisp-eval" (form))

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

(defun nelisp-runtime-image--append-chunk (chunks chunk)
  "Return CHUNKS with CHUNK appended when CHUNK is non-empty."
  (if (and (stringp chunk) (> (length chunk) 0))
      (cons chunk chunks)
    chunks))

(defun nelisp-runtime-image--concat-chunks (chunks)
  "Return CHUNKS, accumulated in reverse order, as one source string."
  (let ((out "")
        (cur (nreverse chunks)))
    (while cur
      (setq out
            (concat out
                    (if (= (length out) 0) "" "\n")
                    (car cur)))
      (setq cur (cdr cur)))
    out))

(defun nelisp-runtime-image--source-from-args (args)
  "Return runtime-image source described by ARGS.
ARGS accepts ordinary FORM strings and `--load FILE' pairs.  Loaded
files and inline forms are concatenated in argument order, which lets a
caller pre-bake runtime sources once and then evaluate shorter forms
against that image."
  (let ((cur args)
        (chunks nil))
    (while cur
      (let ((arg (car cur)))
        (cond
         ((equal arg "--")
          (setq chunks
                (nelisp-runtime-image--append-chunk
                 chunks (nelisp-runtime-image--join-forms (cdr cur))))
          (setq cur nil))
         ((equal arg "--load")
          (setq cur (cdr cur))
          (unless cur
            (error "--load requires FILE"))
          (setq chunks
                (nelisp-runtime-image--append-chunk
                 chunks (nelisp-runtime-image--read-file (car cur))))
          (setq cur (cdr cur)))
         (t
          (setq chunks
                (nelisp-runtime-image--append-chunk chunks arg))
          (setq cur (cdr cur))))))
    (nelisp-runtime-image--concat-chunks chunks)))

(defun nelisp-runtime-image--ensure-final-newline (source)
  "Return SOURCE with a trailing newline."
  (if (fboundp 'nelisp--eval-source-string)
      (concat source "\n")
    (if (or (= (length source) 0)
          (not (= (aref source (- (length source) 1)) ?\n)))
        (concat source "\n")
      source)))

(defun nelisp-runtime-image--wrap-source (source)
  "Wrap SOURCE as a runtime-image source bundle."
  (concat nelisp-runtime-image--magic
          "(progn\n"
          (nelisp-runtime-image--ensure-final-newline source)
          ")\n"))

(defun nelisp-runtime-image--read-file (path)
  "Read PATH as a string or signal an error."
  (let ((source (if (fboundp 'nelisp--eval-source-string)
                    (rdf path)
                  (nelisp--syscall-read-file path))))
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
  (if (fboundp 'nelisp--eval-source-string)
      (nelisp--eval-source-string source)
    (let ((forms (nelisp--read-all-from-string-impl source))
          (last nil))
      (while forms
        (setq last (eval (car forms)))
        (setq forms (cdr forms)))
      last)))

(defun nelisp-runtime-image--parse-cache-kind (kind)
  "Return normalized runtime-image cache KIND."
  (cond
   ((or (eq kind 'nelc) (equal kind "nelc")) 'nelc)
   ((or (eq kind 'neln) (equal kind "neln")) 'neln)
   ((or (eq kind 'auto) (equal kind "auto")) 'auto)
   (t (error "unsupported runtime image cache kind: %S" kind))))

(defun nelisp-runtime-image--resolve-cache-kind (image-path kind)
  "Resolve KIND for IMAGE-PATH into `nelc' or `neln'."
  (let ((kind (nelisp-runtime-image--parse-cache-kind kind)))
    (if (not (eq kind 'auto))
        kind
      (cond
       ((file-exists-p (concat (expand-file-name image-path) ".neln"))
        'neln)
       ((file-exists-p (concat (expand-file-name image-path) ".nelc"))
        'nelc)
       (t 'neln)))))

(defun nelisp-runtime-image--cache-artifact-path (image-path kind)
  "Return artifact path for IMAGE-PATH and cache KIND."
  (let ((resolved (nelisp-runtime-image--resolve-cache-kind image-path kind)))
    (concat (expand-file-name image-path) "." (symbol-name resolved))))

(defun nelisp-runtime-image--ensure-artifact-support ()
  "Ensure runtime-image artifact cache functions are available."
  (unless (and (fboundp 'nelisp-artifact-compile-runtime-image-file)
               (fboundp 'nelisp-artifact-load-file))
    (condition-case nil
        (require 'nelisp-artifact)
      (error nil)))
  (unless (and (fboundp 'nelisp-artifact-compile-runtime-image-file)
               (fboundp 'nelisp-artifact-load-file))
    (error "runtime image artifact cache is unavailable in this command path"))
  t)

(defun nelisp-runtime-image--load-artifact-cache (image-path kind)
  "Load IMAGE-PATH through an artifact cache of KIND.
Missing, stale, or invalid cache artifacts are regenerated before load."
  (nelisp-runtime-image--ensure-artifact-support)
  (let* ((resolved-kind (nelisp-runtime-image--resolve-cache-kind image-path kind))
         (artifact-path (nelisp-runtime-image--cache-artifact-path
                         image-path resolved-kind)))
    (condition-case nil
        (nelisp-artifact-load-file artifact-path)
      ((file-error nelisp-artifact-invalid nelisp-artifact-stale)
       (nelisp-artifact-compile-runtime-image-file
        image-path artifact-path nil nil nil nil nil resolved-kind nil)
       (nelisp-artifact-load-file artifact-path)))
    artifact-path))

(defun nelisp-runtime-image--eval-form-source (source)
  "Evaluate SOURCE after a runtime image cache has been loaded."
  (if (and (fboundp 'nelisp-eval)
           (fboundp 'nelisp-artifact--read-all-from-string))
      (let ((forms (nelisp-artifact--read-all-from-string source))
            (last nil))
        (while forms
          (setq last (nelisp-eval (car forms)))
          (setq forms (cdr forms)))
        last)
    (nelisp-runtime-image--eval-source source)))

(defun nelisp-runtime-image--parse-eval-args (args)
  "Parse runtime image eval/exec ARGS into a plist."
  (let ((path (nth 1 args))
        (rest (cdr (cdr args)))
        (cache-kind nil))
    (when (and rest (equal (car rest) "--cache-kind"))
      (setq rest (cdr rest))
      (unless rest
        (error "--cache-kind requires nelc, neln, or auto"))
      (setq cache-kind (nelisp-runtime-image--parse-cache-kind (car rest)))
      (setq rest (cdr rest)))
    (list :path path :forms rest :cache-kind cache-kind)))

(defun nelisp-runtime-image-dump-cli (args)
  "Implement `nelisp dump-runtime-image' for ARGS."
  (let ((path (nth 1 args)))
    (if (null path)
        (progn
          (nelisp-runtime-image--print-error nelisp-runtime-image--usage)
          2)
      (condition-case err
          (let ((source (nelisp-runtime-image--source-from-args
                         (cdr (cdr args)))))
            (if (fboundp 'nelisp--eval-source-string)
                (progn
                  (nelisp-runtime-image--write-file
                   path (nelisp-runtime-image--wrap-source source)))
              (nelisp-runtime-image--eval-source source)
              (nelisp-runtime-image--write-file
               path (nelisp-runtime-image--wrap-source source)))
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
                 (extension-source (nelisp-runtime-image--source-from-args forms))
                 (extension-image
                  (nelisp-runtime-image--wrap-source extension-source)))
            (unless (fboundp 'nelisp--eval-source-string)
              (nelisp-runtime-image--eval-source base-source)
              (nelisp-runtime-image--eval-source extension-source))
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
  (let* ((opts (condition-case err
                   (nelisp-runtime-image--parse-eval-args args)
                 (error
                  (nelisp-runtime-image--print-error
                   (format "eval-runtime-image: %s" (error-message-string err)))
                  nil)))
         (path (plist-get opts :path))
         (forms (plist-get opts :forms))
         (cache-kind (plist-get opts :cache-kind)))
    (if (or (null path) (null forms))
        (progn
          (nelisp-runtime-image--print-error nelisp-runtime-image--usage)
          1)
      (condition-case err
          (let ((last nil))
            (if cache-kind
                (nelisp-runtime-image--load-artifact-cache path cache-kind)
              (nelisp-runtime-image--eval-source
               (nelisp-runtime-image--read-file path)))
            (setq last
                  (if (and cache-kind
                           (fboundp 'nelisp-artifact--eval-forms))
                      (nelisp-artifact--eval-forms
                       forms
                       (nelisp-runtime-image--resolve-cache-kind path cache-kind))
                    (nelisp-runtime-image--eval-form-source
                     (nelisp-runtime-image--join-forms forms))))
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
