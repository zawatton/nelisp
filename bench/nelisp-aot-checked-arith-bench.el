;;; nelisp-aot-checked-arith-bench.el --- Checked AOT arithmetic gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Continuously gate the cost of Doc 187's opt-in AOT arithmetic checks.
;; The same tight `+' loop is compiled twice, with
;; `nelisp-aot-compiler--checked-arith' nil and non-nil.  The native-object
;; hashes must differ before timing begins; this is the structural guard
;; against the flag silently becoming a no-op.
;;
;; Timing follows tools/ai/bench-compare.sh's three-guard method:
;; subtract an N=0 run from each N=input run (slope), run the baseline at N
;; twice (drift), and refuse a ratio when too few rounds have usable signal.
;; Five independent rounds make the verdict their median rather than one
;; lucky sample.  This is a Linux x86_64 native-artifact gate; other hosts
;; report GATE-SKIP rather than measuring a different execution engine.

;;; Code:

(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-artifact)

(defgroup nelisp-aot-checked-arith-bench nil
  "Doc 187 checked AOT arithmetic benchmark."
  :group 'nelisp)

(defcustom nelisp-aot-checked-arith-bench-input 20000000
  "Iteration count for the compiled tight `+' loop."
  :type 'integer
  :group 'nelisp-aot-checked-arith-bench)

(defcustom nelisp-aot-checked-arith-bench-rounds 5
  "Number of independent slope/drift measurement rounds."
  :type 'integer
  :group 'nelisp-aot-checked-arith-bench)

(defcustom nelisp-aot-checked-arith-bench-drift-limit 0.15
  "Largest baseline noise/signal ratio accepted for one round."
  :type 'number
  :group 'nelisp-aot-checked-arith-bench)

(defcustom nelisp-aot-checked-arith-bench-ceiling 1.15
  "Maximum checked/unchecked runtime ratio accepted by the gate.
Measured 2026-08-26 over 11 independent full gate runs: the median of
their medians was 1.013x, with a 0.986x..1.046x range (0.060x spread).
The 1.15x ceiling leaves about ten percentage points above the worst
observed full-run median without preserving Doc 187's historical 1.27x
as an indefinitely permissive baseline."
  :type 'number
  :group 'nelisp-aot-checked-arith-bench)

(defconst nelisp-aot-checked-arith-bench--symbol
  "doc187-bench-add-loop"
  "Native symbol measured by the gate.")

(defun nelisp-aot-checked-arith-bench-supported-p ()
  "Return non-nil when this host can run the native proof lane."
  (and (eq system-type 'gnu/linux)
       (string-match-p "x86_64" (or system-configuration ""))
       (or (executable-find "cc") (executable-find "gcc"))
       (executable-find "objcopy")))

(defun nelisp-aot-checked-arith-bench--source ()
  "Return the one-add-per-iteration benchmark module source."
  (mapconcat
   #'identity
   '("(defun doc187-bench-add-loop (n i)"
     "  (if (= i n)"
     "      i"
     "    (doc187-bench-add-loop n (+ i 1))))"
     "(provide 'doc187-bench-checked-arith)")
   "\n"))

(defun nelisp-aot-checked-arith-bench--compile
    (source artifact manifest checked)
  "Compile SOURCE to ARTIFACT/MANIFEST with CHECKED arithmetic."
  (let ((nelisp-aot-compiler--checked-arith checked)
        (process-environment (cons "NELISP_TCO=1" process-environment)))
    (nelisp-artifact-compile-file
     source artifact manifest nil nil nil 'doc187-bench-checked-arith
     'neln 'required)))

(defun nelisp-aot-checked-arith-bench--object-sha256 (artifact)
  "Return ARTIFACT's embedded native-object SHA-256."
  (let* ((manifest (nelisp-artifact-read-manifest artifact))
         (native (plist-get manifest :native))
         (hash (and native (plist-get native :object-sha256))))
    (unless (and (stringp hash) (> (length hash) 0))
      (error "%s has no native object hash" artifact))
    hash))

(defun nelisp-aot-checked-arith-bench--runner (artifact)
  "Build and return the cached native runner for ARTIFACT."
  (let ((cc (or (executable-find "cc") (executable-find "gcc")))
        (objcopy (executable-find "objcopy")))
    (nelisp-artifact--native-exec-general-exe
     artifact nelisp-aot-checked-arith-bench--symbol '(0 0) cc objcopy)))

(defun nelisp-aot-checked-arith-bench--run (runner n)
  "Run RUNNER at N and return `(ELAPSED-SECONDS . VALUE)'."
  (let ((start (current-time))
        (status nil)
        (output nil))
    (with-temp-buffer
      (setq status
            (call-process runner nil t nil (number-to-string n) "0"))
      (setq output (string-trim (buffer-string))))
    (unless (eq status 0)
      (error "%s exited %s at n=%d" runner status n))
    (unless (string-match-p "\\`[0-9]+\\'" output)
      (error "%s returned non-integer output %S" runner output))
    (cons (float-time (time-since start)) (string-to-number output))))

(defun nelisp-aot-checked-arith-bench--time (runner n)
  "Return RUNNER's elapsed seconds at N, checking its result."
  (let ((sample (nelisp-aot-checked-arith-bench--run runner n)))
    (unless (= (cdr sample) n)
      (error "%s returned %d at n=%d" runner (cdr sample) n))
    (car sample)))

(defun nelisp-aot-checked-arith-bench--round (base candidate input)
  "Measure one slope/drift round for BASE and CANDIDATE at INPUT."
  (let* ((base0 (nelisp-aot-checked-arith-bench--time base 0))
         (base-n (nelisp-aot-checked-arith-bench--time base input))
         (candidate0 (nelisp-aot-checked-arith-bench--time candidate 0))
         (candidate-n
          (nelisp-aot-checked-arith-bench--time candidate input))
         (base-n2 (nelisp-aot-checked-arith-bench--time base input))
         (base-cost (- base-n base0))
         (candidate-cost (- candidate-n candidate0))
         (noise (abs (- base-n base-n2)))
         (noise-signal (if (> base-cost 0.0)
                           (/ noise base-cost)
                         most-positive-fixnum))
         (valid (and (> base-cost 0.0)
                     (> candidate-cost 0.0)
                     (<= noise-signal
                         nelisp-aot-checked-arith-bench-drift-limit))))
    (list :base-cost base-cost
          :candidate-cost candidate-cost
          :noise-signal noise-signal
          :valid valid
          :ratio (and valid (/ candidate-cost base-cost)))))

(defun nelisp-aot-checked-arith-bench--median (values)
  "Return the median of non-empty VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (count (length sorted)))
    (if (cl-oddp count)
        (nth (/ count 2) sorted)
      (/ (+ (nth (1- (/ count 2)) sorted)
            (nth (/ count 2) sorted))
         2.0))))

(defun nelisp-aot-checked-arith-bench--format-values (values format-string)
  "Format VALUES with FORMAT-STRING, separated by spaces."
  (mapconcat (lambda (value) (format format-string value)) values " "))

(defun nelisp-aot-checked-arith-bench--report-path ()
  "Return the persistent report path for this benchmark."
  (expand-file-name "target/ai/aot-checked-arith-add-bench.txt"
                    default-directory))

(defun nelisp-aot-checked-arith-bench--write-report (lines)
  "Write LINES to the persistent report and print them."
  (let ((path (nelisp-aot-checked-arith-bench--report-path))
        (text (concat (mapconcat #'identity lines "\n") "\n")))
    (make-directory (file-name-directory path) t)
    (write-region text nil path nil 'silent)
    (princ text)))

(defun nelisp-aot-checked-arith-bench-run ()
  "Compile both arms, measure them, write a report, and return a result plist."
  (let* ((report-dir (expand-file-name "target/ai" default-directory))
         (_ (make-directory report-dir t))
         (temp-dir
          (make-temp-file
           (expand-file-name "nelisp-aot-checked-arith-bench-" report-dir) t))
         ;; Sandboxed and CI homes are not necessarily writable.  Keep the
         ;; native-driver cache with the other throwaway benchmark files.
         (process-environment
          (cons (concat "XDG_CACHE_HOME=" temp-dir) process-environment))
         (temporary-file-directory (file-name-as-directory temp-dir))
         (source (expand-file-name "doc187-bench.el" temp-dir))
         (base-artifact (expand-file-name "unchecked.neln" temp-dir))
         (base-manifest (concat base-artifact ".manifest.el"))
         (candidate-artifact (expand-file-name "checked.neln" temp-dir))
         (candidate-manifest (concat candidate-artifact ".manifest.el")))
    (unwind-protect
        (progn
          (write-region (nelisp-aot-checked-arith-bench--source)
                        nil source nil 'silent)
          (nelisp-aot-checked-arith-bench--compile
           source base-artifact base-manifest nil)
          (nelisp-aot-checked-arith-bench--compile
           source candidate-artifact candidate-manifest t)
          (let ((base-hash
                 (nelisp-aot-checked-arith-bench--object-sha256
                  base-artifact))
                (candidate-hash
                 (nelisp-aot-checked-arith-bench--object-sha256
                  candidate-artifact)))
            (if (equal base-hash candidate-hash)
                (let ((lines
                       (list
                        "aot-checked-arith-add"
                        (format "input:        %d"
                                nelisp-aot-checked-arith-bench-input)
                        (format "base object:  %s" base-hash)
                        (format "checked obj:  %s" candidate-hash)
                        "result:       FAIL (checked and unchecked native objects are identical; --checked-arith is a no-op)")))
                  (nelisp-aot-checked-arith-bench--write-report lines)
                  (list :state 'fail :reason "identical native objects"))
              (let* ((base-runner
                      (nelisp-aot-checked-arith-bench--runner base-artifact))
                     (candidate-runner
                      (nelisp-aot-checked-arith-bench--runner
                       candidate-artifact))
                     (warmup (max 1
                                  (/ nelisp-aot-checked-arith-bench-input 10)))
                     (rounds nil))
                ;; Warm both linked drivers and the CPU before the measured
                ;; rounds.  Correct results are checked on every invocation.
                (nelisp-aot-checked-arith-bench--time base-runner warmup)
                (nelisp-aot-checked-arith-bench--time candidate-runner warmup)
                (dotimes (_ nelisp-aot-checked-arith-bench-rounds)
                  (push (nelisp-aot-checked-arith-bench--round
                         base-runner candidate-runner
                         nelisp-aot-checked-arith-bench-input)
                        rounds))
                (setq rounds (nreverse rounds))
                (let* ((valid-rounds
                        (cl-remove-if-not
                         (lambda (round) (plist-get round :valid)) rounds))
                       (ratios (mapcar (lambda (round)
                                         (plist-get round :ratio))
                                       valid-rounds))
                       (required (1+ (/ nelisp-aot-checked-arith-bench-rounds
                                        2)))
                       (ratio (and (>= (length ratios) required)
                                   (nelisp-aot-checked-arith-bench--median
                                    ratios)))
                       (minimum (and ratios (apply #'min ratios)))
                       (maximum (and ratios (apply #'max ratios)))
                       (base-costs (mapcar (lambda (round)
                                             (plist-get round :base-cost))
                                           rounds))
                       (candidate-costs
                        (mapcar (lambda (round)
                                  (plist-get round :candidate-cost))
                                rounds))
                       (noise-values
                        (mapcar (lambda (round)
                                  (plist-get round :noise-signal))
                                rounds))
                       (state (cond
                               ((not ratio) 'skip)
                               ((<= ratio
                                    nelisp-aot-checked-arith-bench-ceiling)
                                'pass)
                               (t 'fail)))
                       (lines
                        (list
                         "aot-checked-arith-add"
                         (format "input:        %d"
                                 nelisp-aot-checked-arith-bench-input)
                         (format "rounds:       %d (%d valid, %d required)"
                                 nelisp-aot-checked-arith-bench-rounds
                                 (length valid-rounds) required)
                         (format "base object:  %s" base-hash)
                         (format "checked obj:  %s" candidate-hash)
                         (format "base slopes:  [%s] ms"
                                 (nelisp-aot-checked-arith-bench--format-values
                                  (mapcar (lambda (value) (* value 1000.0))
                                          base-costs)
                                  "%.2f"))
                         (format "check slopes: [%s] ms"
                                 (nelisp-aot-checked-arith-bench--format-values
                                  (mapcar (lambda (value) (* value 1000.0))
                                          candidate-costs)
                                  "%.2f"))
                         (format "noise/signal: [%s] (limit %.2f)"
                                 (nelisp-aot-checked-arith-bench--format-values
                                  noise-values "%.3f")
                                 nelisp-aot-checked-arith-bench-drift-limit)
                         (if ratios
                             (format "valid ratios: [%s]"
                                     (nelisp-aot-checked-arith-bench--format-values
                                      ratios "%.3f"))
                           "valid ratios: []")
                         (if ratio
                             (format "ratio:        %.3fx (spread %.3f..%.3f, range %.3f; ceiling %.2f)"
                                     ratio minimum maximum (- maximum minimum)
                                     nelisp-aot-checked-arith-bench-ceiling)
                           (format "ratio:        DISCARDED (fewer than %d valid rounds)"
                                   required))
                         (format "result:       %s"
                                 (pcase state
                                   ('pass "PASS")
                                   ('fail "FAIL (checked arithmetic exceeded the cost ceiling)")
                                   (_ "SKIP (measurement invalidated by drift)"))))))
                  (nelisp-aot-checked-arith-bench--write-report lines)
                  (list :state state :ratio ratio :ratios ratios
                        :minimum minimum :maximum maximum
                        :valid (length valid-rounds)
                        :required required))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(defun nelisp-aot-checked-arith-bench-batch ()
  "Batch entry point for the checked-arithmetic performance gate."
  (if (not (nelisp-aot-checked-arith-bench-supported-p))
      (progn
        (princ "GATE-SKIP bench-aot-checked-arith requires Linux x86_64 with cc + objcopy\n")
        (when noninteractive (kill-emacs 0)))
    (condition-case err
        (let* ((result (nelisp-aot-checked-arith-bench-run))
               (state (plist-get result :state)))
          (pcase state
            ('pass
             (princ "GATE-COUNT checked=1 findings=0\n")
             (when noninteractive (kill-emacs 0)))
            ('fail
             (princ "GATE-COUNT checked=1 findings=1\n")
             (when noninteractive (kill-emacs 1)))
            (_
             (princ
              "GATE-SKIP bench-aot-checked-arith measurement discarded by drift guard; see target/ai/aot-checked-arith-add-bench.txt\n")
             (when noninteractive (kill-emacs 0)))))
      (error
       (princ (format "bench-aot-checked-arith: ERROR: %s\n"
                      (error-message-string err)))
       (princ "GATE-COUNT checked=0 findings=1\n")
       (when noninteractive (kill-emacs 1))))))

(provide 'nelisp-aot-checked-arith-bench)

;;; nelisp-aot-checked-arith-bench.el ends here
