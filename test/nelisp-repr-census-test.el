;;; nelisp-repr-census-test.el --- Doc 238 A0 representation census (structural)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks of the debug-only census compiled in by
;; NELISP_REPR_CENSUS=1.  The build script is loaded in a child Emacs with the
;; switch off and on; nothing here runs a NeLisp image.
;; NELISP_CENSUS_BUILD overrides the build script path;
;; NELISP_CENSUS_SEED names a pristine build script for the negative control.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-repr-census-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defconst nelisp-repr-census-test--root
  (expand-file-name ".." nelisp-repr-census-test--dir))

(defun nelisp-repr-census-test--file ()
  (or (getenv "NELISP_CENSUS_BUILD")
      (expand-file-name "scripts/nelisp-standalone-build.el"
                        nelisp-repr-census-test--root)))

(defun nelisp-repr-census-test--seed ()
  (let ((f (or (getenv "NELISP_CENSUS_SEED")
               (expand-file-name ".seed-build.el" nelisp-repr-census-test--root))))
    (and (file-exists-p f) f)))

(defconst nelisp-repr-census-test--probe
  '(progn
     (princ (format "%S\n"
                    (list (and (fboundp 'nelisp-standalone--census-p)
                               (nelisp-standalone--census-p))
                          (nelisp-standalone--driver-bss-size)
                          (and (member "nelisp--repr-census" nelisp-standalone--reader-builtins) t)
                          (and (assoc '(:lit "nelisp--repr-census") nelisp-standalone--applyfn-bf-arms) t)
                          (and (memq 'nelisp--repr-census
                                     (cdr (assoc '(1 . 1) nelisp-standalone--builtin-arity-ranges)))
                               t)
                          (and (fboundp 'nelisp-standalone--census-instrument)
                               (let ((f '(seq (defun nl_word_data (w) (foo w)))))
                                 (eq f (nelisp-standalone--census-instrument f)))))))))

(defun nelisp-repr-census-test--run (file switch)
  "Load FILE in a child Emacs with NELISP_REPR_CENSUS=SWITCH; return the probe list."
  (with-temp-buffer
    (let* ((process-environment
            (cons (concat "NELISP_REPR_CENSUS=" switch) process-environment))
           (default-directory nelisp-repr-census-test--root)
           (status (call-process
                    "sh" nil t nil "-c"
                    (format "ulimit -v 8388608; ulimit -c 0; exec emacs --batch -L lisp -L src -L scripts -l %s --eval %s 2>/dev/null"
                            (shell-quote-argument file)
                            (shell-quote-argument (format "%S" nelisp-repr-census-test--probe))))))
      (should (eq status 0))
      (goto-char (point-max))
      (forward-line -1)
      (read (current-buffer)))))

(defun nelisp-repr-census-test--text (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest nelisp-repr-census-off-is-seed-identical ()
  "Switch off: BSS size and registrations equal the pristine script's."
  (skip-unless (nelisp-repr-census-test--seed))
  (let ((off (nelisp-repr-census-test--run (nelisp-repr-census-test--file) "0"))
        (seed (nelisp-repr-census-test--run (nelisp-repr-census-test--seed) "1")))
    (should (equal (nth 1 off) (nth 1 seed)))
    (should (equal (cddr off) '(nil nil nil t)))
    (should-not (car off))
    ;; Even NELISP_REPR_CENSUS=1 is ignored by the pristine script.
    (should (equal (cddr seed) '(nil nil nil nil)))))

(ert-deftest nelisp-repr-census-on-adds-table-and-builtin ()
  "Switch on: 512 BSS bytes, and the builtin is registered in all three tables."
  (let ((off (nelisp-repr-census-test--run (nelisp-repr-census-test--file) "0"))
        (on (nelisp-repr-census-test--run (nelisp-repr-census-test--file) "1")))
    (should (car on))
    (should (= (- (nth 1 on) (nth 1 off)) 512))
    (should (equal (cddr on) '(t t t nil)))))

(ert-deftest nelisp-repr-census-bss-offset-consistent ()
  "nl_repr_census sits right after nl_ctl_slots (+64) and is gated by the switch."
  (let* ((text (nelisp-repr-census-test--text (nelisp-repr-census-test--file)))
         (ctl "(+ (nelisp-standalone--catch-head-offset) 8 128 32 56 96 512 16 128 64 2048 24 32 272 2072")
         (pos (string-match "(when (nelisp-standalone--census-p)\n      (list (nelisp-link-symbol \"nl_repr_census\"" text)))
    (should pos)
    (should (string-match-p
             (concat (regexp-quote ctl) " 64)\n *:section 'bss")
             (substring text pos (+ pos 400))))
    (should (string-match-p (regexp-quote "(if (nelisp-standalone--census-p) 512 0)") text))
    ;; The 512-byte reservation is declared right after nl_ctl_slots' 64 bytes.
    (should (string-match-p "     64\n     ;; `nl_repr_census'[^\n]*\n[^\n]*\n     (if (nelisp-standalone--census-p) 512 0)" text))))

(ert-deftest nelisp-repr-census-instrumentation-shape ()
  "With the switch on, accessors and producers call the bump; unrelated defuns do not."
  (let ((out (nelisp-repr-census-test--run-form
              '(let ((r (nelisp-standalone--census-instrument
                         '(seq (defun nl_word_data (w) 1) (defun nl_word_length (w) 2)
                               (defun nl_str_target (sx) 3) (defun nl_bi_strptr (sx0) 4)
                               (defun nl_sexp_clone_into (src d) 5) (defun nl_word_clone (w) 6)
                               (defun m5_fx_word_ptr (word) 7) (defun other (w) 8)
                               (defun nl_region_stats_w (p) 9)))))
                 (list (cl-count 'nl_census_bump (flatten-tree r))
                       (cl-count 'nl_repr_census_w (flatten-tree r))
                       (and (member '(defun other (w) 8) r) t))))))
    ;; 7 call sites plus the injected bump defun's own name.
    (should (equal out '(8 1 t)))))

(ert-deftest nelisp-repr-census-image-resident-shape ()
  "Each counted site classifies image-resident legacy views as shape 5 (Doc 238 A1.1)."
  (let ((out (nelisp-repr-census-test--run-form
              '(let ((r (nelisp-standalone--census-instrument
                         '(seq (defun nl_word_data (w) 1) (defun nl_word_length (w) 2)
                               (defun nl_str_target (sx) 3) (defun nl_bi_strptr (sx0) 4)
                               (defun nl_sexp_clone_into (src d) 5) (defun nl_word_clone (w) 6)
                               (defun m5_fx_word_ptr (word) 7)
                               (defun nl_region_stats_w (p) 9)))))
                 (let ((flat (flatten-tree r)))
                   (list (cl-count 'nl_gc_is_boot flat)
                         (cl-count 'nl_gc_stats flat)))))))
    (should (equal out '(7 7)))))

;; Regression: build 21c failed with `:duplicate-defun nl_census_bump' because the
;; cached-unit path and `nelisp-standalone--compile-to-unit' both instrument.
(ert-deftest nelisp-repr-census-instrument-is-idempotent ()
  "Instrumenting twice equals instrumenting once (no duplicate bump defun)."
  (let ((out (nelisp-repr-census-test--run-form
              '(let* ((src '(seq (defun nl_word_data (w) 1) (defun nl_region_stats_w (p) 9)))
                      (once (nelisp-standalone--census-instrument src))
                      (twice (nelisp-standalone--census-instrument once)))
                 (list (equal once twice)
                       (cl-count 'nl_census_bump (flatten-tree twice)))))))
    ;; One site call plus the injected defun's own name.
    (should (equal out '(t 2)))))

(defun nelisp-repr-census-test--run-form (form)
  (with-temp-buffer
    (let* ((process-environment (cons "NELISP_REPR_CENSUS=1" process-environment))
           (default-directory nelisp-repr-census-test--root)
           (status (call-process
                    "sh" nil t nil "-c"
                    (format "ulimit -v 8388608; ulimit -c 0; exec emacs --batch -L lisp -L src -L scripts -l %s --eval %s 2>/dev/null"
                            (shell-quote-argument (nelisp-repr-census-test--file))
                            (shell-quote-argument (format "(princ (format \"%%S\\n\" %S))" form))))))
      (should (eq status 0))
      (goto-char (point-max))
      (forward-line -1)
      (read (current-buffer)))))

(ert-deftest nelisp-repr-census-negative-control-seed-lacks-census ()
  "The structural predicates reject the pristine script (the checks can fail)."
  (skip-unless (nelisp-repr-census-test--seed))
  (let ((text (nelisp-repr-census-test--text (nelisp-repr-census-test--seed))))
    (should-not (string-match-p "nl_repr_census" text))
    (should-not (string-match-p "NELISP_REPR_CENSUS" text))))

(provide 'nelisp-repr-census-test)
;;; nelisp-repr-census-test.el ends here
