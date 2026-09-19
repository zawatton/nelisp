;;; nelisp-ns-check.el --- namespace boundaries as a gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs the `nl-ns' checker over a small set of files and reports the
;; result through the gate contract, so that "my package names are
;; clean" becomes something a machine asserts rather than something an
;; author believes.
;;
;; Elisp has one obarray, so a second definition of a name silently
;; wins.  `nl-ns' reads files and reports where boundaries that the
;; language cannot enforce have been crossed; this wrapper picks which
;; of those findings should stop a build.
;;
;; Two classes are reported but do not fail:
;;
;;   ns-shadows-host          shim modules do this deliberately
;;   ns-undeclared-dependency one finding per edge of the real
;;                            dependency graph — worth reading once,
;;                            useless as a threshold
;;
;; For the whole tree use `make ns-inventory', which ratchets against a
;; recorded baseline instead.  This wrapper is for a handful of files
;; that should simply be clean, such as a recipe skeleton.
;;
;; Usage:
;;   emacs --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
;;     -l tools/ai/nelisp-ns-check.el -f nelisp-ns-check-run FILE...

;;; Code:

(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name
                                        buffer-file-name
                                        default-directory))))
(require 'nelisp-gate-lib)
(require 'nl-ns)

(defconst nelisp-ns-check-failing-kinds
  '(ns-prefix-violation
    ns-private-escape
    ns-quoted-member
    ns-collision-divergent
    ns-partial-override
    ns-unsafe-shim-guard
    ns-file-shadows-library
    ns-unreadable)
  "Finding kinds that fail the gate.
`ns-unreadable' is in the list on purpose: a file that could not be
read produced no findings, and a check that examined nothing must not
report success.")

(defun nelisp-ns-check--baseline ()
  "Return the host baseline path, or nil when it is not available.
Without a baseline the host-shadow findings are suppressed rather than
guessed at, which is the right default but worth knowing about."
  (let ((path "packages/nl-ns/baseline/emacs-30.2.el"))
    (and (file-readable-p path) path)))

(defun nelisp-ns-check-run ()
  "Check the files named on the command line and emit a gate report."
  (let* ((files command-line-args-left)
         (name (or (getenv "NELISP_GATE_NAME") "ns"))
         (baseline (nelisp-ns-check--baseline))
         (started (float-time)))
    (setq command-line-args-left nil)
    (when (null files)
      (nelisp-gate-emit :name name :kind "ns" :ran 0 :failed 1
                        :reason "no file given; nothing was checked")
      (message "nelisp-ns-check: no file given")
      (kill-emacs 1))
    (let* ((findings (nl-ns-check-files files nil baseline))
           (failing (seq-filter
                     (lambda (finding)
                       (memq (plist-get finding :kind)
                             nelisp-ns-check-failing-kinds))
                     findings))
           (duration (round (* 1000 (- (float-time) started)))))
      (princ (nl-ns-report findings baseline))
      (princ "\n")
      (nelisp-gate-emit :name name
                        :kind "ns"
                        ;; Files examined: the count that makes a check
                        ;; over an empty or unreadable set detectable.
                        :ran (length files)
                        :passed (- (length files) (length failing))
                        :failed (length failing)
                        :reason (if failing
                                    (format "%d namespace finding(s) in %s"
                                            (length failing)
                                            (mapconcat
                                             (lambda (f)
                                               (format "%s" (plist-get f :kind)))
                                             failing ", "))
                                  "")
                        :status (if failing "fail" "pass")
                        :command (format "nelisp-ns-check %d file(s)"
                                         (length files))
                        :duration-ms duration)
      (kill-emacs (if failing 1 0)))))

(provide 'nelisp-ns-check)

;;; nelisp-ns-check.el ends here
