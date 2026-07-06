;;; nemacs-init-transport.el --- shared wrapped-init transport consumer  -*- lexical-binding: nil; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Loader reconcile Phase 2 (nemacs init 読み込み 2 系統 reconcile plan,
;; approved 2026-07-06).  Promotes the M15 GUI-bridge wrapped-init
;; consumer (`files--load-user-init',
;; src/nemacs-gui-file-bridge-runtime.el :7698-7841 pre-Phase-2) to a
;; session/library-tier helper so `nemacs-loadup.el' (Lane A, Doc 35)
;; can reuse the identical per-form marker isolation when it detects
;; the macro-less standalone runtime, instead of only ever doing a raw
;; `load' of early-init.el/init.el.
;;
;; `nemacs-wrap-init' (scripts/nemacs-wrap-init.el, run ahead of time
;; under a full host Emacs) hardcodes the marker call names below
;; (`nemacs-init--begin' / `nemacs-init--ok' / `nemacs-init--note-file'
;; / `nemacs-init--file-loaded-p') into every wrapped-transport file it
;; emits.  Whichever consumer `load's that transport must provide these
;; exact global bindings first -- that contract is what makes this
;; module reusable across the GUI bridge and the session loader without
;; either one depending on the other's transport-path conventions (the
;; GUI keeps its own `/tmp' transport-dir naming via
;; `files--init-wrapper-path'/`files--init-report-path'; the session
;; loader resolves its own path from `nemacs-user-emacs-directory').
;;
;; This source is intentionally written with `setq' and `fset' instead
;; of `defvar' / `defun', mirroring `nemacs-gui-file-bridge-runtime.el'
;; (see its top-of-file comment): the current source-v1 runtime-image
;; replay path in the standalone reader reliably preserves those
;; primitive forms, so this file stays safe to fold into the GUI
;; bridge's `nelisp exec-runtime-image' bake alongside the bridge
;; source, and keeps the M18 shallow-nesting discipline the bridge
;; reader depends on.  `provide'/`require' are plain functions on every
;; target (host Emacs, and the standalone `emacs-fns.el' polyfill), so
;; ordinary `(require (quote nemacs-init-transport))' from
;; `nemacs-loadup.el' works without any macro layer.

;;;; --- per-form marker state (contract fixed by `nemacs-wrap-init') ---

(setq nemacs-init--pending "")
(setq nemacs-init--applied 0)
(setq nemacs-init--seen 0)
(setq nemacs-init--failed "")
(setq nemacs-init--files nil)
(setq nemacs-init--last-load-path-dir nil)
(setq nemacs-init--loaded-mtime "")

(fset 'nemacs-init--note-file
      (lambda (f)
        (setq nemacs-init--files (cons f nemacs-init--files))
        f))

(fset 'nemacs-init--file-loaded-p
      (lambda (f)
        (let ((xs nemacs-init--files)
              (hit nil))
          (while xs
            (if (equal (car xs) f)
                (progn (setq hit t) (setq xs nil))
              (setq xs (cdr xs))))
          hit)))

(fset 'nemacs-init--begin
      (lambda (n hint)
        (if (equal nemacs-init--pending "")
            nil
          (setq nemacs-init--failed
                (concat nemacs-init--failed "failed\t" nemacs-init--pending "\n")))
        (setq nemacs-init--pending hint)
        (setq nemacs-init--seen n)
        n))

(fset 'nemacs-init--ok
      (lambda (n)
        (setq nemacs-init--applied (+ nemacs-init--applied 1))
        (setq nemacs-init--pending "")
        n))

;;;; --- consume orchestration -------------------------------------------

;; Self-contained existence check (does not depend on the GUI bridge's
;; `files--file-exists-p' / `files--access-path' globals): both known
;; consumers only reach `nemacs-init-transport-consume' after the
;; standalone `nelisp--syscall-stat-field' primitive is confirmed
;; `fboundp', and `nelisp--syscall-path-int' ships alongside it in the
;; same substrate tier (see `src/emacs-fileio-builtins.el',
;; `src/files-standalone-buffer.el').
(fset 'nemacs-init-transport--file-exists-p
      (lambda (path)
        (= 0 (nelisp--syscall-path-int 21 path 0))))

(fset 'nemacs-init-transport-consume
      (lambda (wrapper report)
        ;; WRAPPER is the `nemacs-wrap-init' OUT path (no suffix); its
        ;; `-packages' and `-pkgs-lowered' companions are read relative
        ;; to it.  REPORT is where the mtime/total/applied/skipped +
        ;; failed-forms summary is written; pass nil to skip writing one.
        ;; Returns non-nil when a transport was found and applied (fresh
        ;; load or already-applied-this-mtime); nil when there is no
        ;; wrapper at WRAPPER, or the runtime lacks the stat primitive --
        ;; either way the caller should fall back to a raw `load'.
        (if (fboundp 'nelisp--syscall-stat-field)
            (if (nemacs-init-transport--file-exists-p wrapper)
                (let ((mstr (number-to-string
                             (nelisp--syscall-stat-field wrapper 88)))
                      (skipped 0))
                  (if (equal nemacs-init--loaded-mtime mstr)
                      ;; the guard is process-local: every fresh one-shot
                      ;; bridge process re-applies the init (its globals
                      ;; start from defaults), a session process applies
                      ;; once per wrapper mtime
                      t
                    (progn
                      (setq nemacs-init--pending "")
                      (setq nemacs-init--applied 0)
                      (setq nemacs-init--seen 0)
                      (setq nemacs-init--failed "")
                      (setq nemacs-init--loaded-mtime mstr)
                      ;; pre-note every resolved package file from the
                      ;; companion list: the IMAGE evaluator drops their
                      ;; defuns anyway, and a big nested package load
                      ;; inside the image replay crashes the reader --
                      ;; the registry guards then skip those forms
                      ;; (package use in the editor = M19-2 transpile)
                      (let ((plist2 (rdf (concat wrapper "-packages")))
                            (pi2 0)
                            (pn2 0)
                            (pls2 0))
                        (setq pn2 (length plist2))
                        (while (< pi2 pn2)
                          (setq pls2 pi2)
                          (while (if (< pi2 pn2)
                                     (if (= (aref plist2 pi2) 10) nil t)
                                   nil)
                            (setq pi2 (+ pi2 1)))
                          (if (> pi2 pls2)
                              (nemacs-init--note-file
                               (substring plist2 pls2 pi2))
                            nil)
                          (setq pi2 (+ pi2 1))))
                      (load wrapper nil t)
                      ;; M19-2: the lowered package transpile gives the
                      ;; image evaluator callable package functions (raw
                      ;; files drop their defuns there); forms it cannot
                      ;; evaluate abort alone per top-level unit
                      (if (nemacs-init-transport--file-exists-p
                           (concat wrapper "-pkgs-lowered"))
                          (load (concat wrapper "-pkgs-lowered") nil t)
                        nil)
                      (if (equal nemacs-init--pending "")
                          nil
                        (progn
                          (setq nemacs-init--failed
                                (concat nemacs-init--failed "failed\t"
                                        nemacs-init--pending "\n"))
                          (setq nemacs-init--pending "")))
                      (setq skipped (- nemacs-init--seen nemacs-init--applied))
                      (if report
                          (nl-write-file report
                                         (concat "mtime\t" mstr "\n"
                                                 "total\t" (number-to-string
                                                            nemacs-init--seen) "\n"
                                                 "applied\t" (number-to-string
                                                              nemacs-init--applied) "\n"
                                                 "skipped\t" (number-to-string skipped) "\n"
                                                 nemacs-init--failed))
                        nil)
                      t)))
              nil)
          nil)))

(provide 'nemacs-init-transport)

;;; nemacs-init-transport.el ends here
