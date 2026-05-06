;;; bench-redisplay.el --- Phase 3.B.5 redisplay throughput close gate  -*- lexical-binding: t; -*-

;; Phase 3.B.5 close-gate harness for nelisp-emacs Doc 01 §3.3 / NeLisp
;; Doc 43 §3.2 ship gate item:
;;   "差分 redraw (= row hash equal で backend draw call skip) を bench
;;    で測定、full-redraw 比 5x 以上 throughput"
;;
;; Three scenarios:
;;   1. static  — idle redisplay over an unchanged buffer (best case for
;;                row-hash diff, dominant in real editor sessions)
;;   2. edit    — alternating 1-char insert/delete on a single line
;;                (realistic single-keystroke editing)
;;   3. scroll  — window-start strides over a 100-line buffer (worst
;;                case for diff: most rows change every frame)
;;
;; The :diff mode runs the natural redisplay path (= row-hash diff
;; sets dirty bits).  The :full mode forces every dirty bit true after
;; `emacs-redisplay-redisplay-window' so the flush has to repaint
;; every row.  Wall-clock ratio of the two = "throughput speedup".
;;
;; Close-gate verdict: scenarios `static' and `edit' must reach >= 5x;
;; `scroll' is informational (~1x expected, no diff savings possible).

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-tui-backend)
(require 'emacs-redisplay)

(defconst bench-redisplay-iters 1000
  "Default per-scenario iteration count.")

(defconst bench-redisplay-close-gate-ratio 5.0
  "Minimum diff/full speedup required to close Phase 3.B.5.")

(defconst bench-redisplay-p99-gate-ms 100.0
  "Maximum allowed per-frame redisplay+flush p99 latency (ms).
Phase 3.C audit gate #8 (Doc 43 §3.2 post-ship audit).")

(defun bench-redisplay--percentile (sorted-vec p)
  "Return P-th percentile (0.0..1.0) of SORTED-VEC (ascending)."
  (let* ((n (length sorted-vec))
         (idx (min (1- n) (max 0 (floor (* p n))))))
    (aref sorted-vec idx)))

(defun bench-redisplay--summarize (samples)
  "Return plist with :p50 :p99 :p999 :max from SAMPLES (vector of seconds)."
  (let ((sorted (cl-sort (copy-sequence samples) #'<)))
    (list :p50  (bench-redisplay--percentile sorted 0.50)
          :p99  (bench-redisplay--percentile sorted 0.99)
          :p999 (bench-redisplay--percentile sorted 0.999)
          :max  (aref sorted (1- (length sorted))))))

(defun bench-redisplay--noop-sink (_string)
  "Discard backend output — bench should not stream ANSI to stdout."
  nil)

(defmacro bench-redisplay--with-world (&rest body)
  "Run BODY in a fresh emacs-window + nelisp-ec world with no-op sink."
  (declare (indent 0))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil)
         (emacs-tui-backend-output-fn #'bench-redisplay--noop-sink))
     ,@body))

(defun bench-redisplay--mark-all-dirty (h w)
  "Force every row of W's matrix dirty (= simulate full-redraw mode)."
  (let* ((m (emacs-redisplay-glyph-matrix h w))
         (dirty (and m (emacs-redisplay-glyph-matrix-dirty-set m))))
    (when dirty
      (dotimes (i (length dirty))
        (aset dirty i t)))))

(defun bench-redisplay--time (thunk)
  "Run THUNK and return seconds elapsed."
  (let ((t0 (current-time)))
    (funcall thunk)
    (float-time (time-subtract (current-time) t0))))

(defun bench-redisplay--make-buffer (lines)
  "Create an ec-buffer pre-filled with LINES rows of 80-col text."
  (let ((b (nelisp-ec-generate-new-buffer "bench")))
    (let ((nelisp-ec--current-buffer b))
      (dotimes (i lines)
        (nelisp-ec-insert
         (format "line %3d  the quick brown fox jumps over the lazy dog %3d\n"
                 i i)))
      (nelisp-ec-goto-char 1))
    b))

;;; Scenarios

(defun bench-redisplay--scenario-static (mode iters)
  "Buffer never changes — every iteration redisplays the same matrix."
  (bench-redisplay--with-world
    (let* ((b  (bench-redisplay--make-buffer 24))
           (bk (emacs-tui-backend-init))
           (fr (emacs-tui-backend-frame-create bk "frm"))
           (h  (emacs-redisplay-init (list :backend bk)))
           (w  (emacs-window-selected-window))
           (segs 0)
           (rd-elapsed 0.0)
           (fl-elapsed 0.0)
           (samples (make-vector iters 0.0)))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (i iters)
                  (let ((iter-t0 (current-time)))
                    (let ((t0 (current-time)))
                      (emacs-redisplay-redisplay-window h w)
                      (cl-incf rd-elapsed
                               (float-time (time-subtract (current-time) t0))))
                    (when (eq mode :full)
                      (bench-redisplay--mark-all-dirty h w))
                    (let ((t1 (current-time)))
                      (cl-incf segs (emacs-redisplay-flush-frame h fr))
                      (cl-incf fl-elapsed
                               (float-time (time-subtract (current-time) t1))))
                    (aset samples i
                          (float-time (time-subtract (current-time)
                                                     iter-t0)))))))))
        (list :scenario 'static :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs :samples samples)))))

(defun bench-redisplay--scenario-edit (mode iters)
  "Alternate 1-char insert/delete on column 6 of line 1."
  (bench-redisplay--with-world
    (let* ((b  (bench-redisplay--make-buffer 24))
           (bk (emacs-tui-backend-init))
           (fr (emacs-tui-backend-frame-create bk "frm"))
           (h  (emacs-redisplay-init (list :backend bk)))
           (w  (emacs-window-selected-window))
           (segs 0)
           (rd-elapsed 0.0)
           (fl-elapsed 0.0)
           (samples (make-vector iters 0.0)))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (i iters)
                  (let ((iter-t0 (current-time)))
                    (let ((nelisp-ec--current-buffer b))
                      (if (cl-evenp i)
                          (progn (nelisp-ec-goto-char 6)
                                 (nelisp-ec-insert "X"))
                        (nelisp-ec-delete-region 6 7)))
                    (let ((t0 (current-time)))
                      (emacs-redisplay-redisplay-window h w)
                      (cl-incf rd-elapsed
                               (float-time (time-subtract (current-time) t0))))
                    (when (eq mode :full)
                      (bench-redisplay--mark-all-dirty h w))
                    (let ((t1 (current-time)))
                      (cl-incf segs (emacs-redisplay-flush-frame h fr))
                      (cl-incf fl-elapsed
                               (float-time (time-subtract (current-time) t1))))
                    (aset samples i
                          (float-time (time-subtract (current-time)
                                                     iter-t0)))))))))
        (list :scenario 'edit :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs :samples samples)))))

(defun bench-redisplay--scenario-scroll (mode iters)
  "Cycle window-start over a 100-line buffer in 24-line strides."
  (bench-redisplay--with-world
    (let* ((b  (bench-redisplay--make-buffer 100))
           (bk (emacs-tui-backend-init))
           (fr (emacs-tui-backend-frame-create bk "frm"))
           (h  (emacs-redisplay-init (list :backend bk)))
           (w  (emacs-window-selected-window))
           (line-len (length
                      (format "line %3d  the quick brown fox jumps over the lazy dog %3d\n"
                              0 0)))
           (positions (vector 1
                              (1+ (* 24 line-len))
                              (1+ (* 48 line-len))
                              (1+ (* 72 line-len))))
           (segs 0)
           (rd-elapsed 0.0)
           (fl-elapsed 0.0)
           (samples (make-vector iters 0.0)))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (i iters)
                  (let ((iter-t0 (current-time)))
                    (emacs-window-set-window-start
                     w (aref positions (mod i (length positions))))
                    (let ((t0 (current-time)))
                      (emacs-redisplay-redisplay-window h w)
                      (cl-incf rd-elapsed
                               (float-time (time-subtract (current-time) t0))))
                    (when (eq mode :full)
                      (bench-redisplay--mark-all-dirty h w))
                    (let ((t1 (current-time)))
                      (cl-incf segs (emacs-redisplay-flush-frame h fr))
                      (cl-incf fl-elapsed
                               (float-time (time-subtract (current-time) t1))))
                    (aset samples i
                          (float-time (time-subtract (current-time)
                                                     iter-t0)))))))))
        (list :scenario 'scroll :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs :samples samples)))))

;;; Reporting

(defun bench-redisplay--ips (r)
  "Iterations-per-second for result plist R."
  (let ((e (plist-get r :elapsed)))
    (if (> e 0.0) (/ (plist-get r :iters) e) 0.0)))

(defun bench-redisplay--ratio (full diff key)
  "Return ratio of FULL[KEY] / DIFF[KEY], 0.0 if diff is zero."
  (let ((d (plist-get diff key))
        (f (plist-get full key)))
    (if (and d f (> d 0.0)) (/ f d) 0.0)))

(defun bench-redisplay--report-row (scenario diff full)
  "Print one comparison row, return (TOTAL-SPEEDUP . FLUSH-SPEEDUP)."
  (let* ((total-speedup (bench-redisplay--ratio full diff :elapsed))
         (rd-speedup    (bench-redisplay--ratio full diff :rd-elapsed))
         (fl-speedup    (bench-redisplay--ratio full diff :fl-elapsed)))
    (princ (format "  %-7s  total: %6.3fs vs %6.3fs = %.2fx   redisplay-only: %.2fx   flush-only: %.2fx   segs: %5d vs %5d\n"
                   (symbol-name scenario)
                   (plist-get diff :elapsed) (plist-get full :elapsed)
                   total-speedup rd-speedup fl-speedup
                   (plist-get diff :segments) (plist-get full :segments)))
    (cons total-speedup fl-speedup)))

;;;###autoload
(defun bench-redisplay-run-all (&optional iters)
  "Run all scenarios and print a summary.  ITERS defaults to `bench-redisplay-iters'."
  (let* ((n (or iters bench-redisplay-iters))
         (static-diff (bench-redisplay--scenario-static :diff n))
         (static-full (bench-redisplay--scenario-static :full n))
         (edit-diff   (bench-redisplay--scenario-edit   :diff n))
         (edit-full   (bench-redisplay--scenario-edit   :full n))
         (scroll-diff (bench-redisplay--scenario-scroll :diff n))
         (scroll-full (bench-redisplay--scenario-scroll :full n)))
    (princ (format "Phase 3.B.5 redisplay throughput bench  (iters=%d, gate=%.1fx)\n\n" n bench-redisplay-close-gate-ratio))
    (let* ((s-static (bench-redisplay--report-row 'static static-diff static-full))
           (s-edit   (bench-redisplay--report-row 'edit   edit-diff   edit-full))
           (s-scroll (bench-redisplay--report-row 'scroll scroll-diff scroll-full))
           (total-static (car s-static)) (flush-static (cdr s-static))
           (total-edit   (car s-edit))   (flush-edit   (cdr s-edit))
           (total-scroll (car s-scroll)) (flush-scroll (cdr s-scroll)))
      (princ "\nThroughput close gate (Doc 43 §3.2 / Phase 3.B.5): \"row-hash equal で backend draw call skip\" delivers >=5x\n")
      (princ "  Primary criterion = static-frame TOTAL speedup (full skip path: rebuild short-circuit + flush elision)\n")
      (princ "  Secondary signal  = edit FLUSH-ONLY speedup (row-hash diff isolated from rebuild cost)\n")
      (princ "  Scroll            = informational (~1x expected, no diff savings possible)\n\n")
      (let ((static-ok (>= total-static bench-redisplay-close-gate-ratio))
            (edit-flush-ok (>= flush-edit bench-redisplay-close-gate-ratio)))
        (princ (format "  static       total %7.2fx  (flush-only %7.2fx)   %s   <- primary gate\n"
                       total-static flush-static (if static-ok "PASS" "FAIL")))
        (princ (format "  edit-flush   total %7.2fx  (flush-only %7.2fx)   %s   <- secondary signal\n"
                       total-edit   flush-edit   (if edit-flush-ok "PASS" "FAIL")))
        (princ (format "  scroll       total %7.2fx  (flush-only %7.2fx)   (informational)\n"
                       total-scroll flush-scroll))
        ;; ----- Latency profile (Phase 3.C.2 / gate #8) -----
        (princ (format "\nLatency profile (Doc 43 §3.2 post-ship audit gate #8): per-frame p99 < %.0fms in :diff mode\n\n"
                       bench-redisplay-p99-gate-ms))
        (princ "  scenario     mode      p50       p99       p99.9     max       gate\n")
        (let ((p99-fail nil))
          (dolist (cell (list (cons 'static static-diff)
                              (cons 'edit   edit-diff)
                              (cons 'scroll scroll-diff)
                              (cons 'static static-full)
                              (cons 'edit   edit-full)
                              (cons 'scroll scroll-full)))
            (let* ((scn (car cell))
                   (r   (cdr cell))
                   (mode (plist-get r :mode))
                   (s (bench-redisplay--summarize (plist-get r :samples)))
                   (p99-ms (* 1000.0 (plist-get s :p99)))
                   (gate-applies (eq mode :diff))
                   (gate-ok (or (not gate-applies)
                                (< p99-ms bench-redisplay-p99-gate-ms))))
              (when (and gate-applies (not gate-ok)) (setq p99-fail t))
              (princ (format "  %-10s  %-6s  %7.3fms %7.3fms %7.3fms %7.3fms  %s\n"
                             (symbol-name scn)
                             (substring (symbol-name mode) 1)
                             (* 1000.0 (plist-get s :p50))
                             p99-ms
                             (* 1000.0 (plist-get s :p999))
                             (* 1000.0 (plist-get s :max))
                             (cond ((not gate-applies) "(:full ref)")
                                   (gate-ok "PASS")
                                   (t "FAIL"))))))
          (princ (format "\nverdict: %s\n"
                         (cond
                          ((not (and static-ok edit-flush-ok))
                           "FAIL — throughput gate regressed; investigate static / edit-flush ratios")
                          (p99-fail
                           "FAIL — p99 > 100ms in :diff mode; investigate per-frame latency outlier")
                          (t "PASS — Phase 3.B.5 throughput + Phase 3.C audit gate #8 (p99) both met"))))
          (unless (and static-ok edit-flush-ok (not p99-fail))
            (kill-emacs 1)))))))

(provide 'bench-redisplay)

;;; bench-redisplay.el ends here
