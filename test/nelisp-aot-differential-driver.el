;;; nelisp-aot-differential-driver.el --- AOT against the interpreter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs each generated program twice inside the reader -- once through
;; the interpreter, once as loaded native code -- and compares.
;;
;; The expected answer is computed, not written down.  That is the whole
;; difference from `nelisp-native-load-driver.el': a fixture with a
;; hand-written answer can only catch a shape someone anticipated, and
;; every representation defect found so far was found by anticipating one
;; more shape by hand.  Here the interpreter is the oracle, so a wrong
;; native answer is detected whether or not anyone expected it.
;;
;; `nelisp-aot-differential-manifest' and the artifact directory come
;; from a generated prelude, matching how the loader driver is run: the
;; reader has no `getenv'.

;;; Code:

(defvar nelisp-aot-differential-dir nil
  "Directory holding the compiled cases; set by the generated prelude.")

(defvar nelisp-aot-differential-chunk 0
  "Which slice of the manifest this process runs; set by the prelude.

The arena does not reclaim, so a single process walking the whole
population is killed partway through (exit 137) whether or not each
artifact is unmapped after use.  One reader process per slice keeps each
run inside the arena rather than making the population smaller.")

(defvar nelisp-aot-differential-chunk-size 10)

(defvar nelisp-aot-differential--dir nelisp-aot-differential-dir)
(defvar nelisp-aot-differential--wrong 0)
(defvar nelisp-aot-differential--checked 0)
(defvar nelisp-aot-differential--skipped 0)

(defun nelisp-aot-differential--interpreted (source name arg)
  "Define SOURCE in the interpreter and call NAME with ARG."
  (eval (car (read-from-string source)) t)
  (funcall (intern name) arg))

(defun nelisp-aot-differential--native (name args)
  "Load NAME's artifact once, call it with each of ARGS, unmap it.

`nelisp-native-load-exec' maps three regions per call and leaves them
mapped, which is what a cache wants and not what a run over a hundred
artifacts wants: the first version of this driver was OOM-killed partway
through (exit 137).  Load once, answer both arguments, release."
  (let ((handle (nelisp-native-load-artifact
                 (concat nelisp-aot-differential--dir "/" name ".neln")
                 name))
        (out nil))
    (dolist (arg args)
      (push (condition-case e (nelisp-native-load-call handle (list arg))
              (error (list 'native-error e)))
            out))
    (nelisp-native-load-unload handle)
    (nreverse out)))

(defun nelisp-aot-differential--case (entry)
  "Run one manifest ENTRY through both paths and compare."
  (let ((name (nth 0 entry))
        (source (nth 1 entry))
        (args (nth 2 entry))
        (native (nth 3 entry)))
    ;; Announced before it runs.  A wrong answer is reported by this
    ;; process; a native fault kills it, and then the last `RUN' line is
    ;; the only thing that says which program did it.
    (princ (format "RUN %s\n" name))
    (if (not native)
        ;; No native defun means the compiler refused the program and the
        ;; caller falls back to byte code.  Slower, not wrong.
        (setq nelisp-aot-differential--skipped
              (1+ nelisp-aot-differential--skipped))
      (let ((gots (condition-case e
                      (nelisp-aot-differential--native name args)
                    (error (mapcar (lambda (_) (list 'native-error e)) args))))
            (rest args))
        (while rest
          (let ((arg (car rest))
                (got (car gots))
                (want nil))
            (setq want (condition-case e
                           (nelisp-aot-differential--interpreted
                            source name arg)
                         (error (list 'interpreter-error e))))
            (setq nelisp-aot-differential--checked
                  (1+ nelisp-aot-differential--checked))
            (unless (equal want got)
              (setq nelisp-aot-differential--wrong
                    (1+ nelisp-aot-differential--wrong))
              (princ (format "WRONG %s(%S): interpreter %S, native %S\n"
                             name arg want got)))
            (setq rest (cdr rest))
            (setq gots (cdr gots))))))))

(defun nelisp-aot-differential--slice ()
  "Return this process's slice of the manifest."
  (let ((rest nelisp-aot-differential-manifest)
        (skip (* nelisp-aot-differential-chunk
                 nelisp-aot-differential-chunk-size))
        (take nelisp-aot-differential-chunk-size)
        (out nil))
    (while (and rest (> skip 0))
      (setq rest (cdr rest))
      (setq skip (1- skip)))
    (while (and rest (> take 0))
      (push (car rest) out)
      (setq rest (cdr rest))
      (setq take (1- take)))
    (nreverse out)))

(defun nelisp-aot-differential-run ()
  "Run this chunk's cases and report."
  (dolist (entry (nelisp-aot-differential--slice))
    (nelisp-aot-differential--case entry))
  (princ (format "differential chunk %d: %d comparisons, %d wrong, %d not native\n"
                 nelisp-aot-differential-chunk
                 nelisp-aot-differential--checked
                 nelisp-aot-differential--wrong
                 nelisp-aot-differential--skipped))
  (when (> nelisp-aot-differential--wrong 0)
    (kill-emacs 1)))

(nelisp-aot-differential-run)

;;; nelisp-aot-differential-driver.el ends here
