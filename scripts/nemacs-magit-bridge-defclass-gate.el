;;; nemacs-magit-bridge-defclass-gate.el --- empty-class regression gate  -*- lexical-binding: t; -*-

;; Reproduces the "registered but empty EIEIO class" defect WITHOUT magit or
;; transient: run the bridge preconditions, load bundle part 1, then evaluate a
;; plain two-slot `defclass' and report the resulting class metadata.
;;
;; The defect this guards against: the alist `cl-defstruct' shim in
;; `src/emacs-cl-macros.el' records `:include' parent slot lists only at
;; macro-expansion time and emits `cl-struct-setter' as top-level `put' forms.
;; A baked runtime image keeps definitions but does not replay top-level side
;; effects, so `(cl-defstruct (eieio--class ... (:include cl--class) ...))'
;; evaluated at runtime silently drops cl--class's five slots.  Every slot write
;; in `eieio-defclass-internal' then resolves to `nelisp--record-set' against a
;; cons -- a SILENT no-op -- while the class registration itself (a
;; `cl-simple-setter' place, and evaluated before the slot loop) succeeds.  The
;; result is a registered, nameless, zero-slot class, and `defclass' returns
;; without signalling anything at all.
;;
;; PASS: GATE name=zz-gate-cls nslots=2 nparents=1
;;       GATE-INST ok=t aa=1
;; FAIL: GATE name=nil nslots=0 nparents=0
;;
;; `GATE-PRE-OK', `GATE-PART1-OK' and `GATE-DONE' must all appear; losing a
;; pre-existing marker is a regression and outranks any newly gained one.
;;
;; Usage (roughly five minutes; run it in the background):
;;
;;   timeout 1800 env NELISP_HOME=$PWD/vendor/nelisp vendor/nelisp/target/nelisp \
;;     exec-runtime-image build/nemacs-runtime.nlri \
;;     "$(cat scripts/nemacs-magit-bridge-defclass-gate.el)" > /tmp/gate.log 2>&1
;;   grep -aE '^GATE' /tmp/gate.log
;;
;; Anchor the grep at line start: the error path dumps the failing source, which
;; contains the whole bundle text, and an unanchored pattern matches inside it.
;;
;; The repository root is read from NEMACS_LIB_REPO, defaulting to this file's
;; grandparent directory.

(progn
  (setq zz-gate-root
        (or (getenv "NEMACS_LIB_REPO")
            "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el" zz-gate-root))
  (setq nelisp-emacs-magit-bridge-repo-root zz-gate-root)
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "GATE-PRE-OK\n")

  (condition-case e
      (load (expand-file-name
             "build/nelisp-emacs-magit-bridge-bundle-part1.el" zz-gate-root)
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "GATE-PART1-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "GATE-PART1-OK\n")

  (condition-case e
      (eval '(defclass zz-gate-cls nil
               ((aa :initarg :aa :initform nil)
                (bb :initarg :bb :initform nil)))
            t)
    (error (nelisp--write-stdout-bytes (format "GATE-DEFCLASS-ERR %S\n" e))))

  (condition-case e
      (let ((c (ignore-errors (find-class 'zz-gate-cls nil))))
        (nelisp--write-stdout-bytes
         (format "GATE name=%S nslots=%S nparents=%S\n"
                 (and c (ignore-errors (eieio--class-name c)))
                 (and c (ignore-errors
                          (length (append (eieio--class-slots c) nil))))
                 (and c (ignore-errors
                          (length (append (eieio--class-parents c) nil)))))))
    (error (nelisp--write-stdout-bytes (format "GATE ERR %S\n" e))))

  (condition-case e
      (let ((o (ignore-errors (make-instance 'zz-gate-cls :aa 1))))
        (nelisp--write-stdout-bytes
         (format "GATE-INST ok=%S aa=%S\n"
                 (and o t)
                 (and o (ignore-errors (slot-value o 'aa))))))
    (error (nelisp--write-stdout-bytes (format "GATE-INST ERR %S\n" e))))

  (nelisp--write-stdout-bytes "GATE-DONE\n")
  0)

;;; nemacs-magit-bridge-defclass-gate.el ends here
