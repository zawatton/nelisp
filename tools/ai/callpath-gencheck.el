;;; callpath-gencheck.el --- callpath S2: direct Subr calls, offline  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/callpath-gencheck.el
;; Prints one CHECK line per property and GENCHECK bad=N; exit 0 iff bad = 0.
;; - admit: a handler that hands `args' whole to a helper is admitted by the
;;   helper's body (memq, aref, recordp, fboundp, string-search-chars).
;; - lower: aref's argv body reads the array raw and declines (2) on the
;;   char-table bridge, which calls Lisp with the list.
;; - subrword: Subr objects carry the argv entry word (static id, min, max);
;;   funcall (an evaluator entry) has none.
;; - fx/native: the VM's CALL and `nc_call' reach the argv entry through
;;   `m5_fx_call_subr_any' before the call-out; a non-(-3) status is the call's.
;; - argv: the reader's `m5_fx_subr_argv' calls `nl_apply_builtin_frame_sid_w'
;;   with no name lookup, autoload test or list (except the 2 fallback).
;; - parse: the changed defuns pass the AOT parser.
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(defvar cp-bad 0)
(defun cp-check (name ok &optional detail)
  (unless ok (setq cp-bad (1+ cp-bad)))
  (princ (format "CHECK %s %s%s\n" name (if ok "ok" "BAD") (if detail (format " %s" detail) ""))))
(defun cp-has (x form)
  (cond ((equal x form) t)
        ((consp form) (or (cp-has x (car form)) (cp-has x (cdr form))))))
(defun cp-find (name forms)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) name))) forms))
(defun cp-parse (defun-form)
  (let ((nelisp-aot-compiler--allow-external-user-calls t))
    (condition-case err
        (progn (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse defun-form)) t)
      (error (princ (format "PARSE-ERROR %S\n" err)) nil))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (table (nelisp-standalone--applyfn-guarded-reader-table))
       (adm (nelisp-argframe-admitted-names table))
       (want '("memq" "aref" "recordp" "fboundp" "nelisp--string-search-chars" "length")))
  (cp-check "admit" (and (>= (length adm) 193) (cl-every (lambda (n) (member n adm)) want)
                         (cl-every #'nelisp-standalone--sid adm))
            (format "admitted=%d" (length adm)))
  (let* ((aref (assoc '(:lit "aref") table))
         (low (and aref (condition-case nil (nelisp-argframe-lower (cddr aref) 'argv 'argc)
                          (nelisp-argframe-ineligible nil)))))
    (cp-check "lower" (and low (not (cp-has 'args low)) (cp-has 2 low)
                           (not (cp-has 'bf_aref_nemacs_bridge_w low))
                           (not (cp-has 'nl_argframe_list_only low))))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (memq (nelisp-standalone--subr-argv-word "memq"))
       (aref (nelisp-standalone--subr-argv-word "aref"))
       (forms (nelisp-standalone--reader-install-builtins-forms)))
  (cp-check "subrword" (and (= (logand memq 65535) (nelisp-standalone--sid "memq"))
                            (= (logand (ash memq -16) 255) 2) (= (logand (ash memq -24) 255) 2)
                            (= (logand aref 65535) (nelisp-standalone--sid "aref"))
                            (= (nelisp-standalone--subr-argv-word "funcall") 0)
                            (cl-some (lambda (f) (cp-has memq f)) forms))
            (format "memq=%d aref=%d" memq aref)))

(let* ((helpers (nelisp-standalone--argframe-m5-helpers))
       (sa (cp-find 'm5_fx_subr_argv helpers))
       (any (cp-find 'm5_fx_call_subr_any helpers)))
  (cp-check "argv" (and sa (cp-has 'nl_apply_builtin_frame_sid_w sa)
                        (not (cp-has 'nl_autoload_cell_p sa)) (not (cp-has 'nl_sym_sid sa))
                        (not (cp-has 'nl_eic_builtin_name sa)) (cp-has 'nl_n4_push_words sa)))
  (cp-check "parse-argv" (and sa any (cp-parse sa) (cp-parse any))))

(let* ((src (prin1-to-string (nelisp-cc-fxir-native-source)))
       (fx (prin1-to-string nelisp-standalone--applyfn-m5-helpers)))
  (cp-check "native" (string-match-p "(m5_fx_call_subr_any env site stk argc argc resw)" src))
  (ignore fx)
  (let ((rb (cp-find 'm5_fx_run_body nelisp-standalone--applyfn-m5-helpers)))
    (cp-check "fx" (and rb (cp-has 'm5_fx_call_subr_any rb) (cp-parse rb)))))

;; leaves: the full reader applyfn source has argv frame leaves for memq (sid 73)
;; and aref (226), none reads `args' as a list, and every frame leaf parses.
(let* ((nelisp-standalone--target 'linux-x86_64)
       (src (nelisp-standalone--applyfn-source))
       (defs (cdr src))
       (leaves (cl-remove-if-not
                (lambda (f) (and (eq (car-safe f) 'defun)
                                 (string-prefix-p "nl_apply_builtin_frame_leaf_" (symbol-name (cadr f)))))
                defs))
       (names (mapcar (lambda (f) (symbol-name (cadr f))) leaves))
       (bad (cl-remove-if-not
             (lambda (f) (or (cp-has '(wf_arg_ptr args) f) (cp-has '(wf_arg_word args) f)
                             (string-match-p "(heap-read-u64 args " (prin1-to-string f))))
             leaves))
       (unparsed (cl-remove-if #'cp-parse leaves)))
  (cp-check "leaves" (and (>= (length leaves) 185)
                          (member (format "nl_apply_builtin_frame_leaf_%d_w" (nelisp-standalone--sid "memq")) names)
                          (member (format "nl_apply_builtin_frame_leaf_%d_w" (nelisp-standalone--sid "aref")) names)
                          (null bad) (null unparsed))
            (format "frame-leaves=%d listreads=%d unparsed=%d" (length leaves) (length bad) (length unparsed))))

(princ (format "GENCHECK bad=%d\n" cp-bad))
(kill-emacs (if (= cp-bad 0) 0 1))
