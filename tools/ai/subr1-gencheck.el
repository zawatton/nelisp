;;; subr1-gencheck.el --- subr1 S2: Subr objects and argv entries, offline  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/subr1-gencheck.el
;; Prints one CHECK line per property and GENCHECK bad=N; exit 0 iff bad = 0.
;; - desc: every installed reader builtin gets a Subr object whose header
;;   carries min/max (GNU struct Lisp_Subr), not only the directcall ones.
;; - lower: `wf_arg_ptr' arms are admitted to argv entries (rx-exec among them)
;;   and every argv read is a raw load.
;; - shape: the static-id shape tree gives rx-exec (7 . 7) and nothing to the
;;   evaluator entries (ids 1-18).
;; - calllist: the list call-out finds the argv entry by the static id
;;   (`nl_apply_builtin_frame_sid_w'), with no `nl_subr_desc' name dispatch and
;;   no `nl_apply_name_classify'.
;; - sidw: `nl_apply_builtin_frame_sid_w' is generated and resolves no name.
;; - native: an id-less Subr skips `nl_directcall_invoke' and its legacy list
;;   skips the "builtin" name test.
;; - parse: the changed defuns pass the AOT parser.
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(defvar subr1-bad 0)
(defun subr1-check (name ok &optional detail)
  (unless ok (setq subr1-bad (1+ subr1-bad)))
  (princ (format "CHECK %s %s%s\n" name (if ok "ok" "BAD") (if detail (format " %s" detail) ""))))
(defun subr1-has (x form)
  (let ((found nil))
    (while (and (not found) form)
      (if (equal x form) (setq found t form nil)
        (if (consp form)
            (progn (when (subr1-has x (car form)) (setq found t))
                   (setq form (cdr form)))
          (setq form nil))))
    found))
(defun subr1-find-defun (name forms)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name))) forms))
(defun subr1-parse (defun-form)
  (let ((nelisp-aot-compiler--allow-external-user-calls t))
    (condition-case err
        (progn (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse defun-form)) t)
      (error (princ (format "PARSE-ERROR %S\n" err)) nil))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (forms (nelisp-standalone--reader-install-builtins-forms))
       (subr (cl-count-if (lambda (f) (subr1-has 'nl_install_subr f)) forms))
       (rx (nelisp-standalone--subr-desc "nelisp--rx-exec")))
  (subr1-check "desc" (and (= subr (length forms)) rx
                           (= (logand (ash rx -8) 255) 7) (= (logand (ash rx -16) 255) 7))
               (format "subr=%d forms=%d" subr (length forms))))

(let* ((table (nelisp-standalone--applyfn-guarded-reader-table))
       (adm (nelisp-argframe-admitted-names table))
       (rx (assoc '(:lit "nelisp--rx-exec") table))
       (low (and rx (condition-case nil (nelisp-argframe-lower (cddr rx) 'argv 'argc)
                      (nelisp-argframe-ineligible nil)))))
  (subr1-check "lower" (and (member "nelisp--rx-exec" adm) (>= (length adm) 141) low
                            (subr1-has 'm5_fx_word_ptr low) (not (subr1-has 'heap-read-u64 low))
                            (not (subr1-has 'args low)))
               (format "admitted=%d" (length adm))))

(let* ((sid (nelisp-standalone--sid "nelisp--rx-exec"))
       (form (nelisp-standalone--argframe-shape-form 'argframe_sid))
       (shape (eval `(let ((argframe_sid ,sid)) ,form) t))
       (evaluator (cl-loop for s from 1 to 18 sum (eval `(let ((argframe_sid ,s)) ,form) t))))
  (subr1-check "shape" (and (= shape (logior 1 (ash 7 8) (ash 7 16))) (= evaluator 0))
               (format "rx=%S evaluator=%S" shape evaluator)))

(let* ((helpers (nelisp-standalone--argframe-m5-helpers))
       (cl (subr1-find-defun 'm5_fx_call_list helpers)))
  (subr1-check "calllist" (and cl (subr1-has 'nl_apply_builtin_frame_sid_w cl)
                               (not (subr1-has 'nl_subr_desc cl))
                               (not (subr1-has 'nl_apply_name_classify cl))
                               (not (subr1-has 'nl_apply_builtin_frame_w cl))))
  (subr1-check "parse-calllist" (and cl (subr1-parse cl)))
  ;; builtinname: the head test is the static id compare; the name compare
  ;; only for an id-less head.
  (let ((bn (subr1-find-defun 'm5_fx_builtin_name helpers)))
    (subr1-check "builtinname" (and bn (subr1-has 'nl_sym_sid bn)
                                    (subr1-has (nelisp-standalone--sid "builtin") bn)
                                    (subr1-parse bn)))))

(let* ((src (nelisp-standalone--applyfn-source))
       (defs (cdr src))
       (sidw (subr1-find-defun 'nl_apply_builtin_frame_sid_w defs))
       (leaves (cl-remove-if-not
                (lambda (f) (and (eq (car-safe f) 'defun)
                                 (string-prefix-p "nl_apply_builtin_frame_leaf_" (symbol-name (cadr f)))))
                defs)))
  (subr1-check "sidw" (and sidw (not (subr1-has 'nl_apply_builtin_name_key sidw))
                           (not (subr1-has 'nl_ds sidw))
                           (subr1-has 'nl_dk sidw))
               (format "frame-leaves=%d" (length leaves)))
  (subr1-check "leaves-raw" (and (>= (length leaves) 100)
                                 (cl-notany (lambda (f) (or (subr1-has '(wf_arg_ptr args) f)
                                                            (string-match-p "(heap-read-u64 args "
                                                                            (prin1-to-string f))))
                                            leaves)))
  (subr1-check "parse-sidw" (and sidw (subr1-parse sidw))))

(let* ((src (prin1-to-string (nelisp-cc-fxir-native-source))))
  (subr1-check "native" (and (string-match-p "(if (= (logand (ptr-read-u64 (- fn 6) 0) 65280) 0) 2 (nl_directcall_invoke" src)
                             (string-match-p "(nc_builtin_call env site sym legacy stk argc resw oslot 1)" src)
                             (string-match-p "(if (if (= known 1) 0 (= (m5_fx_builtin_name fn 0) 0)) -5" src))))

(princ (format "GENCHECK bad=%d\n" subr1-bad))
(kill-emacs (if (= subr1-bad 0) 0 1))
