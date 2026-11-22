;;; nl-dev-session.el --- resident request loop for a NeLisp runtime image  -*- lexical-binding: t; -*-
;; Loaded by tools/dev/nl-dev-session.sh start as
;;   exec-runtime-image IMAGE '(progn (load "nl-dev-session.el" nil t t) (nl-dev-session-run DIR))'
;; The loop polls DIR/req.el; each request is READ and EVAL'd form by form from
;; this loop, never inside an outer (load ...) (the in-load trap, state-perf F95:
;; loops called under an active load stay interpreted).  Output goes to stdout.

(defvar nl-dev-session-last-value nil)

(defun nl-dev-time-call (fn)
  "Call FN with no args, return (ELAPSED-SECONDS . VALUE).  Refuses to time under an active load."
  (when (and (boundp 'load-in-progress) load-in-progress)
    (error "nl-dev-time-call: refusing to time inside (load ...): loops stay interpreted"))
  (let* ((t0 (float-time)) (v (funcall fn)))
    (cons (- (float-time) t0) v)))

(defvar nl-dev--orig-load nil "The image's own `load', saved by `nl-dev-fast-load-enable'.")
(defvar nl-dev-fast-load-count 0)
(defvar nl-dev--force-fast nil "Bound non-nil by `nl-dev-load-el' so an explicitly named file is always fast-loaded.")

(defun nl-dev--find-el (file)
  "Return an existing .el path for FILE (absolute, or via `load-path'), or nil."
  (let ((cands (if (and (> (length file) 0) (= (aref file 0) 47)) (list "") load-path))
        (found nil))
    (while (and cands (not found))
      (let* ((base (if (equal (car cands) "") file (concat (file-name-as-directory (car cands)) file)))
             (try (if (string-suffix-p ".el" base) base (concat base ".el"))))
        (when (file-exists-p try) (setq found try)))
      (setq cands (cdr cands)))
    found))

(defun nl-dev-fast-load (file &optional noerror nomessage nosuffix must-suffix)
  "Fast stand-in for `load' on .el sources: READ + EVAL each form, no load-time rewrites.
Measured ~300x faster than the image's `load' on lisp/nelisp-native-load.el (0.08 s vs 27 s).
Differences from the real load: no reader-literal rewrites, no eager macroexpansion,
no periodic GC, `load-in-progress' stays nil.  Non-.el or unresolved files go to the real load."
  (let ((path (and (stringp file)
                   (if nosuffix
                       (and (string-suffix-p ".el" file) (file-exists-p file) file)
                     (nl-dev--find-el file)))))
    (if (or (not path) (and (not nl-dev--force-fast) (not (nl-dev--under-roots-p path))))
        (funcall nl-dev--orig-load file noerror nomessage nosuffix must-suffix)
      (let ((forms (nl-dev-session--read-forms path)) (old load-file-name))
        (setq nl-dev-fast-load-count (1+ nl-dev-fast-load-count))
        (unwind-protect
            (progn (setq load-file-name path)
                   (while forms (eval (car forms) t) (setq forms (cdr forms))))
          (setq load-file-name old))
        t))))

(defvar nl-dev--orig-require nil)
(defvar nl-dev-fast-roots nil
  "Directories (with trailing /) whose .el files `load'/`require' read+eval fast when fast mode is on.
Files elsewhere (vendor, baked packages) keep the image's real loader, which applies reader rewrites.")

(defun nl-dev--under-roots-p (path)
  (let ((r nl-dev-fast-roots) (hit nil))
    (while (and r (not hit))
      (when (string-prefix-p (car r) path) (setq hit t))
      (setq r (cdr r)))
    hit))

(defun nl-dev-fast-require (feature &optional filename noerror)
  (if (featurep feature)
      feature
    (let ((path (nl-dev--find-el (or filename (symbol-name feature)))))
      (if (and path (nl-dev--under-roots-p path))
          (progn (nl-dev-fast-load path)
                 (if (featurep feature) feature
                   (funcall nl-dev--orig-require feature filename noerror)))
        (funcall nl-dev--orig-require feature filename noerror)))))

(defun nl-dev-fast-load-enable ()
  (unless nl-dev--orig-load (setq nl-dev--orig-load (symbol-function 'load)))
  (unless nl-dev--orig-require (setq nl-dev--orig-require (symbol-function 'require)))
  (fset 'load 'nl-dev-fast-load)
  (fset 'require 'nl-dev-fast-require) 'fast-load-on)

(defun nl-dev-fast-load-disable ()
  (when nl-dev--orig-load (fset 'load nl-dev--orig-load))
  (when nl-dev--orig-require (fset 'require nl-dev--orig-require)) 'fast-load-off)

(defun nl-dev-load-el (file &optional real)
  "Load source FILE and return after it finished.  REAL non-nil uses the image's real `load'."
  (if real
      (funcall (or nl-dev--orig-load 'load) file nil t t)
    (let ((was (eq (symbol-function 'load) 'nl-dev-fast-load)))
      (nl-dev-fast-load-enable)
      (setq nl-dev--force-fast t)
      (unwind-protect (load file nil t t)
        (setq nl-dev--force-fast nil)
        (unless was (nl-dev-fast-load-disable))))))

(defun nl-dev-load-neln (file &rest symbols)
  "Load FILE (.neln) symbols via nelisp-native-load; returns an alist (SYM . HANDLE)."
  (unless (featurep 'nelisp-native-load) (require 'nelisp-native-load))
  (mapcar (lambda (s) (cons s (nelisp-native-load-artifact file s))) symbols))

(defvar nl-dev-session--warned nil)
(defun nl-dev-session--warn-tier ()
  "Say once that fast-loaded code is not the real loader's code."
  (unless nl-dev-session--warned
    (setq nl-dev-session--warned t)
    (princ "NLDS-WARN fast-loaded code may run on a different tier than under the real loader (state-perf F95: code run under an active `load' stays interpreted, top-level calls reach the word VM) and skips eager macroexpansion, load-history and reader rewrites: no timing claim, no tier or semantic equivalence claim; time in cold runs only.\n")))

(defun nl-dev-session--read-forms (file)
  "Read every form of FILE; signal when the final form is truncated.
`read-from-string' signals end-of-file both at a clean end and inside an
unfinished form, so after it the rest must be only whitespace and comments."
  (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
        (pos 0) (forms nil) (go t))
    (nl-dev-session--warn-tier)
    (while go
      (let ((r (condition-case nil (read-from-string text pos) (end-of-file nil))))
        (if r
            (progn (setq forms (cons (car r) forms)) (setq pos (cdr r)))
          (unless (string-match-p "\\`\\(?:[ \t\n\r\f]\\|;[^\n]*\\)*\\'" (substring text pos))
            (error "nl-dev-session: truncated or unbalanced final form in %s after offset %d" file pos))
          (setq go nil))))
    (nreverse forms)))

(defun nl-dev-session--serve (file)
  (condition-case e
      (let ((forms (nl-dev-session--read-forms file)) (v nil))
        (while forms
          (setq v (eval (car forms) t))
          (setq forms (cdr forms)))
        (setq nl-dev-session-last-value v)
        (princ (format "NLDS=> %S\n" v)))
    (error (princ (format "NLDS-ERR %S\n" e)))))

(defun nl-dev-session-run (dir)
  (let ((req (concat dir "/req.el")) (cur (concat dir "/cur.el"))
        (done (concat dir "/done")) (stop (concat dir "/stop")) (n 0))
    (princ "NLDS-READY\n")
    (while (not (file-exists-p stop))
      (if (file-exists-p req)
          (progn
            (rename-file req cur t)
            (setq n (1+ n))
            (nl-dev-session--serve cur)
            (princ "NLDS-END\n")
            (write-region (format "%d\n" n) nil done nil 'silent))
        (sit-for 0.005)))
    (princ "NLDS-STOPPED\n")))
