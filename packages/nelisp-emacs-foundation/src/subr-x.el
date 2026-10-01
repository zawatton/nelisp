;;; subr-x.el --- lightweight standard subr-x facade for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Keep common vendor `(require 'subr-x)' paths on the small Layer-2
;; surface.  Most primitives here are normally preloaded from subr.el or
;; provided by subr-x.el in GNU Emacs; standalone NeLisp reaches them while
;; loading vendor files before a full dump/loaddefs image exists.

;;; Code:

(defconst subr-x--load-directory
  ;; `load-file-name' is nil inside the concatenated bootstrap bundle, and the
  ;; bare `default-directory' fallback then resolved siblings to <repo>/X.el
  ;; instead of <repo>/src/X.el.  Same shape as `cl-lib--load-directory'.
  (let ((source-file
         (or (and (boundp 'load-file-name) load-file-name)
             (and (boundp 'buffer-file-name) buffer-file-name))))
    (cond
     (source-file
      (file-name-directory source-file))
     ((catch 'source-dir
        (dolist (entry load-path)
          (when (and (stringp entry)
                     (file-readable-p
                      (expand-file-name "emacs-subr-extras.el" entry)))
            (throw 'source-dir (file-name-as-directory entry))))))
     ((and (boundp 'default-directory)
           (stringp default-directory))
      (let ((src (expand-file-name "src/" default-directory)))
        (if (and (fboundp 'file-directory-p)
                 (file-directory-p src))
            src
          default-directory)))
     (t nil)))
  "Directory that contains the subr-x facade and its sibling features.")

(defun subr-x--load-feature (feature)
  "Load FEATURE from the subr-x facade directory, unless already loaded.
See the identical `featurep' rationale on `emacs-foundation--load-feature':
inside the pre-concatenated bootstrap bundle several of the features
listed below (emacs-eval, emacs-string, emacs-hash, cl-lib) are already
provided by the time this file runs, so an unconditional `load' here
re-read and re-evaluated each one from disk a second time.  A feature
not yet provided (e.g. emacs-subr-extras, which the bundle currently
provides later) is still loaded exactly as before."
  (unless (featurep feature)
    (load (expand-file-name (concat (symbol-name feature) ".el")
                            subr-x--load-directory)
          nil t)))

(subr-x--load-feature 'emacs-eval)
(subr-x--load-feature 'emacs-subr-extras)
(subr-x--load-feature 'emacs-string)
(subr-x--load-feature 'emacs-hash)
(subr-x--load-feature 'cl-lib)

(defun subr-x--define-p (symbol)
  "Return non-nil when SYMBOL should be supplied by this facade."
  (or (not (fboundp symbol))
      (and (fboundp 'autoloadp)
           (autoloadp (symbol-function symbol)))))

(when (subr-x--define-p 'internal--thread-argument)
  (defmacro internal--thread-argument (first &rest forms)
    "Internal implementation for `thread-first' and `thread-last'."
    (let ((value (car forms))
          (tail (cdr forms)))
      (while tail
        (let ((form (car tail)))
          (setq value
                (cond
                 ((consp form)
                  (if first
                      (cons (car form) (cons value (cdr form)))
                    (append form (list value))))
                 (first (list form value))
                 (t (list form value)))))
        (setq tail (cdr tail)))
      value)))

(when (subr-x--define-p 'thread-first)
  (defmacro thread-first (&rest forms)
    "Thread FORMS as the first argument through each successive form."
    (declare (indent 0))
    (cons 'internal--thread-argument (cons t forms))))

(when (subr-x--define-p 'thread-last)
  (defmacro thread-last (&rest forms)
    "Thread FORMS as the last argument through each successive form."
    (declare (indent 0))
    (cons 'internal--thread-argument (cons nil forms))))

(when (subr-x--define-p 'named-let)
  (defmacro named-let (name bindings &rest body)
    "Looping let form named NAME with BINDINGS and BODY."
    (declare (indent 2))
    (let ((vars nil)
          (vals nil))
      (dolist (binding bindings)
        (push (car binding) vars)
        (push (cadr binding) vals))
      (setq vars (nreverse vars)
            vals (nreverse vals))
      `(cl-labels ((,name ,vars ,@body))
         (,name ,@vals)))))

(defun hash-table-empty-p (hash-table)
  "Return non-nil when HASH-TABLE has no entries."
  (= (hash-table-count hash-table) 0))

;; Kept unguarded (audit 2026-09-29): on host Emacs `emacs-hash' may claim these
;; names first with an alist-backed polyfill that breaks real hash tables; the
;; maphash versions below are equivalent to the native ones on NeLisp.
(defun hash-table-keys (hash-table)
  "Return a list of HASH-TABLE keys."
  (let (keys)
    (maphash (lambda (key _value) (push key keys)) hash-table)
    keys))

(defun hash-table-values (hash-table)
  "Return a list of HASH-TABLE values."
  (let (values)
    (maphash (lambda (_key value) (push value values)) hash-table)
    values))

(when (subr-x--define-p 'string-remove-prefix)
  (defun string-remove-prefix (prefix string)
    "Remove PREFIX from STRING when present."
    (if (string-prefix-p prefix string)
        (substring string (length prefix))
      string)))

(when (subr-x--define-p 'string-remove-suffix)
  (defun string-remove-suffix (suffix string)
    "Remove SUFFIX from STRING when present."
    (if (string-suffix-p suffix string)
        (substring string 0 (- (length string) (length suffix)))
      string)))

(when (subr-x--define-p 'string-replace)
  (defun string-replace (from-string to-string in-string)
    "Replace all non-overlapping FROM-STRING matches with TO-STRING."
    (if (= (length from-string) 0)
        in-string
      (let ((start 0)
            (pieces nil)
            pos)
        (while (setq pos (string-search from-string in-string start))
          (push (substring in-string start pos) pieces)
          (push to-string pieces)
          (setq start (+ pos (length from-string))))
        (push (substring in-string start) pieces)
        (apply #'concat (nreverse pieces))))))

(when (subr-x--define-p 'string-truncate-left)
  (defun string-truncate-left (string length)
    "If STRING is longer than LENGTH, truncate it from the left."
    (if (<= (length string) length)
        string
      (let ((keep (max 0 (- length 3))))
        (concat "..." (substring string (- (length string) keep)))))))

(when (subr-x--define-p 'string-limit)
  (defun string-limit (string length &optional end _coding-system)
    "Return up to LENGTH characters from STRING.
When END is non-nil, keep the last LENGTH characters."
    (unless (and (integerp length) (>= length 0))
      (signal 'wrong-type-argument (list 'natnump length)))
    (cond
     ((<= (length string) length) string)
     (end (substring string (- (length string) length)))
     (t (substring string 0 length)))))

(when (subr-x--define-p 'string-pad)
  (defun string-pad (string length &optional padding start)
    "Pad STRING to LENGTH using PADDING.
When START is non-nil, pad on the left."
    (unless (and (integerp length) (>= length 0))
      (signal 'wrong-type-argument (list 'natnump length)))
    (let ((pad-length (- length (length string))))
      (if (<= pad-length 0)
          string
        (let ((pad (make-string pad-length (or padding ?\s))))
          (if start (concat pad string) (concat string pad)))))))

(when (subr-x--define-p 'string-chop-newline)
  (defun string-chop-newline (string)
    "Remove STRING's final newline, when present."
    (string-remove-suffix "\n" string)))

(when (subr-x--define-p 'proper-list-p)
  (defun proper-list-p (object)
    "Return OBJECT's list length when it is a proper list, else nil."
    (let ((slow object)
          (fast object)
          (len 0)
          (done nil)
          result)
      (while (not done)
        (cond
         ((null fast)
          (setq result len
                done t))
         ((not (consp fast))
          (setq done t))
         ((null (cdr fast))
          (setq result (1+ len)
                done t))
         ((not (consp (cdr fast)))
          (setq done t))
         (t
          (setq slow (cdr slow)
                fast (cdr (cdr fast))
                len (+ len 2))
          (when (eq slow fast)
            (setq done t)))))
      result)))

(when (subr-x--define-p 'mapcan)
  (defun mapcan (function sequence &rest more-sequences)
    "Apply FUNCTION across SEQUENCE and concatenate the list results."
    (let ((results (apply #'mapcar function sequence more-sequences)))
      (apply #'nconc results))))

;; Ported verbatim from GNU Emacs 31.1 lisp/emacs-lisp/subr-x.el.  Only the
;; pure text-property and buffer-text helpers are ported here; the
;; pixel-width helpers (`string-pixel-width', `truncate-string-pixelwise',
;; `work-buffer--prepare-pixelwise') need `buffer-text-pixel-size', a
;; display/font-metrics primitive the standalone does not implement, and
;; `read-process-name' needs a live process list, so both stay out of this
;; facade.

(when (subr-x--define-p 'string-fill)
  (defun string-fill (string width)
    "Try to word-wrap STRING so that it displays with lines no wider than WIDTH.
STRING is wrapped where there is whitespace in it.  If there are
individual words in STRING that are wider than WIDTH, the result
will have lines that are wider than WIDTH."
    (declare (important-return-value t))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((fill-column width)
            (adaptive-fill-mode nil))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(when (subr-x--define-p 'add-remove--display-text-property)
  (defun add-remove--display-text-property (start end spec value
                                                  &optional object remove)
    (let ((sub-start start)
          (sub-end 0)
          (limit (if (stringp object)
                     (min (length object) end)
                   (min end (point-max))))
          disp)
      (while (< sub-end end)
        (setq sub-end (next-single-property-change sub-start 'display object
                                                    limit))
        (if (not (setq disp (get-text-property sub-start 'display object)))
            ;; No old properties in this range.
            (unless remove
              (put-text-property sub-start sub-end 'display (list spec value)
                                 object))
          ;; We have old properties.
          (let ((changed nil)
                type)
            ;; Make disp into a list.
            (setq disp
                  (cond
                   ((vectorp disp)
                    (setq type 'vector)
                    (seq-into disp 'list))
                   ((or (not (consp (car-safe disp)))
                        ;; If disp looks like ((margin ...) ...), that's
                        ;; still a single display specification.
                        (eq (caar disp) 'margin))
                    (setq type 'scalar)
                    (list disp))
                   (t
                    (setq type 'list)
                    disp)))
            ;; Remove any old instances.
            (when-let* ((old (assoc spec disp)))
              ;; If the property value was a list, don't modify the
              ;; original value in place; it could be used by other
              ;; regions of text.
              (setq disp (if (eq type 'list)
                             (remove old disp)
                           (delete old disp))
                    changed t))
            (unless remove
              (setq disp (cons (list spec value) disp)
                    changed t))
            (when changed
              (if (not disp)
                  (remove-text-properties sub-start sub-end '(display nil) object)
                (when (eq type 'vector)
                  (setq disp (seq-into disp 'vector)))
                ;; Finally update the range.
                (put-text-property sub-start sub-end 'display disp object)))))
        (setq sub-start sub-end)))))

(when (subr-x--define-p 'add-display-text-property)
  (defun add-display-text-property (start end spec value &optional object)
    "Add the display specification (SPEC VALUE) to the text from START to END.
If any text in the region has a non-nil `display' property, the existing
display specifications are retained.

OBJECT is either a string or a buffer to add the specification to.
If omitted, OBJECT defaults to the current buffer."
    (add-remove--display-text-property start end spec value object)))

(when (subr-x--define-p 'remove-display-text-property)
  (defun remove-display-text-property (start end spec &optional object)
    "Remove the display specification SPEC from the text from START to END.
SPEC is the car of the display specification to remove, e.g. `height'.
If any text in the region has other display specifications, those specs
are retained.

OBJECT is either a string or a buffer to remove the specification from.
If omitted, OBJECT defaults to the current buffer."
    (add-remove--display-text-property start end spec nil object 'remove)))

(when (subr-x--define-p 'emacs-etc--hide-local-variables)
  (defun emacs-etc--hide-local-variables ()
    "Hide local variables.
Used by `emacs-authors-mode' and `emacs-news-mode'."
    (narrow-to-region (point-min)
                      (save-excursion
                        (goto-char (point-max))
                        ;; Obfuscate to avoid this being interpreted
                        ;; as a local variable section itself.
                        (if (re-search-backward "^Local\sVariables:$" nil t)
                            (progn (forward-line -1) (point))
                          (point-max))))))

(provide 'subr-x)

;;; subr-x.el ends here
