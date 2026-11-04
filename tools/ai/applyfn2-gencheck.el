;;; applyfn2-gencheck.el --- applyfn2 S2: split applyfn units, word entries for the list-walk builtins, Fapply spread, offline  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/applyfn2-gencheck.el
;; Prints one CHECK line per property and GENCHECK bad=N; exit 0 iff bad = 0.
;; - apply: fx `apply' of a symbol whose cell holds a Subr object with a word entry takes
;;   GNU Fapply's shape (`m5_fx_apply_subr': one list measure, one spread, the Bcall
;;   route `m5_fx_call_subr_any'), not the declining callpath2 spread; `funcall' of such
;;   a symbol runs `m5_fx_call_subr_any' first; a Subr object as the definition is run.
;; - admit: every walk-eligible handler is admitted except the ones listed in
;;   `applyfn2-excluded' (each with its reason), and every admitted name has a static id.
;; - hot: those handlers lower, and omit the literal (nl_cons_cdr_word args).
;;   This syntactic guard does not prove absence of alias-derived transport.
;; - split: the reader applyfn splits into the main unit and the argv-entry units; every
;;   form of the logical source is emitted exactly once; the main unit holds no argv
;;   entry; the routers reach every leaf; every argv-entry defun passes the AOT parse.
;; - size: each unit's source is within its gate on both targets (calibrated against the
;;   offline compile, tools/ai/applyfn2-unitsize.el).
;; - wiring: the reader build compiles and links the argv-entry units, prewarmed.
;; - listside: the list-route leaf bodies are those of HEAD (28c60a8c1).
;; This generator check uses depth 20000.  Default-depth claims come only from
;; the separate real frame compiles and explicit depth-1600 donor controls.
(setq max-lisp-eval-depth 20000)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(defvar af2-bad 0)
(defun af2-check (name ok &optional detail)
  (unless ok (setq af2-bad (1+ af2-bad)))
  (princ (format "CHECK %s %s%s\n" name (if ok "ok" "BAD") (if detail (format " %s" detail) ""))))
(defun af2-has (x form)
  (cond ((equal x form) t)
        ((consp form) (or (af2-has x (car form)) (af2-has x (cdr form))))))
(defun af2-count (x form)
  (cond ((eq x form) 1) ((consp form) (+ (af2-count x (car form)) (af2-count x (cdr form)))) (t 0)))
(defun af2-find (name forms)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name))) forms))
(defun af2-parse (d)
  (condition-case nil
      (let ((nelisp-aot-compiler--allow-external-user-calls t))
        (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse d)) t)
    (error nil)))
(defun af2-chars (forms)
  (let ((print-length nil) (print-level nil))
    (apply #'+ (mapcar (lambda (f) (length (prin1-to-string f))) forms))))

(defconst applyfn2-excluded nil
  "Walk-eligible handlers deliberately left on the list route, with the reason.
Each entry is (NAME . REASON).")

;; Per-unit source gates, chars of the printed forms (calibration in the ledger NOTE
;; of S2.1: tools/ai/applyfn2-unitsize.el on both targets).
(defconst applyfn2-main-max 1300000)
(defconst applyfn2-frame-max 260000)
;; Sorted list-route leaf body hashes of HEAD 28c60a8c1 (target/applyfn2/head-export).
(defconst applyfn2-listside-head "2dc550ed7ebf7941c79c8637813a7af20ef0325dd80e51c52225fe246dc69625")

;; apply / funcall
(let* ((helpers (nelisp-standalone--argframe-m5-helpers))
       (ap (af2-find 'm5_fx_apply_run helpers))
       (as (af2-find 'm5_fx_apply_subr helpers))
       (ws (af2-find 'm5_fx_apply_word_subr helpers))
       (ll (af2-find 'm5_fx_apply_list_len helpers))
       (site (af2-find 'm5_fx_funcall_site helpers))
       (fv (af2-find 'm5_fx_funcall_value helpers)))
  (af2-check "apply"
             (and ap as ws ll site fv
                  (af2-has '(m5_fx_apply_subr stk sp argc resw env) ap)
                  (af2-has '(m5_fx_apply_word_subr fw) ap)
                  (af2-has 'm5_fx_call_subr_any as) (af2-has 'm5_fx_apply_list_len as)
                  (af2-has 'nl_root_release as)
                  (not (af2-has 'm5_fx_funcall_site as)) (not (af2-has 'nl_eic_apply_list_ok as))
                  (af2-has '(m5_fx_call_subr_any env fn stk sp argc resw) site)
                  (af2-has '(m5_fx_subr_argv env opr def stk sp argc resw) fv)
                  (cl-every #'af2-parse (list ap as ws ll site fv)))
             (format "parsed=%s" (and ap as ws ll site fv t))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (table (nelisp-standalone--applyfn-guarded-reader-table))
       (adm (nelisp-argframe-admitted-names table))
       (eligible (let ((nelisp-argframe-walk-names nil)) (nelisp-argframe-admitted-names table)))
       (left (cl-remove-if (lambda (n) (member n adm)) eligible)))
  (af2-check "admit" (and (equal (sort (copy-sequence left) #'string<)
                                 (sort (mapcar #'car applyfn2-excluded) #'string<))
                          (cl-every (lambda (n) (let ((sid (nelisp-standalone--sid n))) (and sid (< sid 4095)))) adm))
             (format "admitted=%d eligible=%d excluded=%d nosid=%S" (length adm) (length eligible) (length left)
                     (cl-remove-if #'nelisp-standalone--sid adm)))
  (let ((counts nil) (ok t) (controls t) (data-walk nil)
        (transport-free (lambda (low)
                          (and low (not (af2-has '(nl_cons_cdr_word args) low))))))
    (dolist (name '("concat" "mapcar" "assoc" "alist-get" "remhash"))
      (let* ((e (cl-find-if (lambda (e) (equal (cadar e) name)) table))
             (low (and (member name adm)
                       (condition-case nil
                           (let ((nelisp-argframe-generated (cons nil (make-hash-table :test #'equal))))
                             (nelisp-argframe-lower (cddr e) 'args 'argc))
                         (nelisp-argframe-ineligible nil)))))
        (push (format "%s=%s" name (and low (af2-count 'nl_argframe_fresh_list low))) counts)
        ;; Data arguments can themselves be lists (assoc/alist-get); walking
        ;; those is required.  Only the argument-list transport must disappear.
        (unless (funcall transport-free low) (setq ok nil))
        ;; Calibrate the actual predicate on each real lowered handler:
        ;; inserting argument-list transport must be rejected.
        (when (funcall transport-free `(seq ,low (nl_cons_cdr_word args)))
          (setq controls nil))
        ;; Assoc walks the alist in wf_assoc_walk_w rather than in this
        ;; lowered entry.  Include that legitimate data operation explicitly.
        (when (and (equal name "assoc") low
                   (af2-has 'wf_assoc_walk_w low)
                   (funcall transport-free `(seq ,low (nl_cons_cdr_word alist))))
          (setq data-walk t))))
    (af2-check "hot" ok (mapconcat #'identity (nreverse counts) " "))
    (af2-check "hot-control" (and controls data-walk)
               (format "injected-direct-cdr-rejected=%s assoc-data-walk-allowed=%s" controls data-walk))))

(dolist (tgt '(linux-x86_64 windows-x86_64))
  (condition-case err
  (let* ((nelisp-standalone--target tgt)
         (full (condition-case err (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source))
                 (error (princ (format "GEN-ERROR %S\n" err)) nil)))
         (split (and full (fboundp 'nelisp-standalone--applyfn-split-units)
                     (condition-case err (nelisp-standalone--applyfn-split-units full)
                       (error (princ (format "SPLIT-ERROR %S\n" err)) nil))))
         (main (car split)) (frames (cdr split))
         (names (lambda (u) (mapcar #'cadr (cl-remove-if-not (lambda (f) (eq (car-safe f) 'defun)) (cdr u)))))
         (all-full (funcall names full))
         (all-split (apply #'append (mapcar names split)))
         (subrouters (cl-remove-if-not (lambda (n) (string-match-p "\\`nl_apply_builtin_frame_part_[0-9]+_w\\'" (symbol-name n)))
                                       all-split))
         (leaves (cl-remove-if-not #'nelisp-standalone--applyfn-frame-leaf-key all-full))
         (frame-forms (apply #'append (mapcar #'cdr frames))))
    (when (eq tgt 'linux-x86_64)
      (af2-check "split"
                 (and split (= (length frames) nelisp-standalone--applyfn-frame-unit-count)
                      ;; every logical form once, plus one subrouter per part after the first
                      (equal (sort (mapcar #'symbol-name all-split) #'string<)
                             (sort (mapcar #'symbol-name (append all-full subrouters)) #'string<))
                      (= (length subrouters) (1- (length frames)))
                      (cl-notany (lambda (n) (or (nelisp-standalone--applyfn-frame-leaf-key n)
                                                 (nelisp-standalone--applyfn-frame-spec-p n)
                                                 (memq n nelisp-standalone--applyfn-frame-routers)))
                                 (funcall names main))
                      (cl-every (lambda (r) (memq r (funcall names (car frames))))
                                nelisp-standalone--applyfn-frame-routers)
                      ;; each leaf is called from exactly one place in each router path
                      (let ((sid-router (af2-find 'nl_apply_builtin_frame_sid_w frame-forms))
                            (subs (mapcar (lambda (n) (af2-find n frame-forms)) subrouters)))
                        (cl-every (lambda (l) (= 1 (+ (af2-count l sid-router)
                                                      (apply #'+ (mapcar (lambda (s) (af2-count l s)) subs)))))
                                  leaves))
                      (cl-every #'af2-parse (cl-remove-if-not (lambda (f) (eq (car-safe f) 'defun)) frame-forms)))
                 (format "units=%d leaves=%d specialized=%d parts=%S"
                         (length split) (length leaves)
                         (cl-count-if #'nelisp-standalone--applyfn-frame-spec-p all-full)
                         (mapcar (lambda (u) (length (cdr u))) frames))))
    (let ((mc (and main (af2-chars (cdr main))))
          (fc (mapcar (lambda (u) (af2-chars (cdr u))) frames)))
      (af2-check (format "size-%s" tgt)
                 (and mc fc (<= mc applyfn2-main-max) (cl-every (lambda (c) (<= c applyfn2-frame-max)) fc))
                 (format "main=%s frames=%S" mc fc)))
    (when (eq tgt 'linux-x86_64)
      (let* ((list-leaves (cl-remove-if-not (lambda (f) (and (eq (car-safe f) 'defun)
                                                             (string-match-p "^nl_apply_builtin_leaf_" (symbol-name (cadr f)))))
                                            (cdr full)))
             (print-length nil) (print-level nil)
             (h (secure-hash 'sha256 (mapconcat #'identity
                                                (sort (mapcar (lambda (f) (secure-hash 'sha256 (prin1-to-string (cddr f)))) list-leaves)
                                                      #'string<) ","))))
        (princ (format "LISTSIDE %s\n" h))
        (af2-check "listside" (equal h applyfn2-listside-head) (format "leaves=%d" (length list-leaves))))))
    (error (af2-check (format "split-%s" tgt) nil (format "%S" err)))))

(af2-check "wiring"
           (and (fboundp 'nelisp-standalone--applyfn-frame-units)
                (member "applyfn-frame-0.o" nelisp-standalone--prewarm-units)
                (member (format "applyfn-frame-%d.o" (1- (or (bound-and-true-p nelisp-standalone--applyfn-frame-unit-count) 1)))
                        nelisp-standalone--prewarm-units)
                (let ((def (symbol-function 'nelisp-standalone--reader-units)))
                  (or (af2-has 'applyfn-frames def)
                      ;; byte-compiled or not, the reader link list names the frame units
                      (string-match-p "applyfn-frames" (prin1-to-string def))))))

(princ (format "GENCHECK bad=%d\n" af2-bad))
(kill-emacs (if (= af2-bad 0) 0 1))
