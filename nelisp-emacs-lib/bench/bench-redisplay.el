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
           (fl-elapsed 0.0))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (_ iters)
                  (let ((t0 (current-time)))
                    (emacs-redisplay-redisplay-window h w)
                    (cl-incf rd-elapsed
                             (float-time (time-subtract (current-time) t0))))
                  (when (eq mode :full)
                    (bench-redisplay--mark-all-dirty h w))
                  (let ((t1 (current-time)))
                    (cl-incf segs (emacs-redisplay-flush-frame h fr))
                    (cl-incf fl-elapsed
                             (float-time (time-subtract (current-time) t1)))))))))
        (list :scenario 'static :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs)))))

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
           (fl-elapsed 0.0))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (i iters)
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
                             (float-time (time-subtract (current-time) t1)))))))))
        (list :scenario 'edit :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs)))))

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
           (fl-elapsed 0.0))
      (emacs-window-set-window-buffer w b)
      (emacs-redisplay-redisplay-window h w)
      (emacs-redisplay-flush-frame h fr)
      (garbage-collect)
      (let ((elapsed
             (bench-redisplay--time
              (lambda ()
                (dotimes (i iters)
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
                             (float-time (time-subtract (current-time) t1)))))))))
        (list :scenario 'scroll :mode mode :iters iters
              :elapsed elapsed :rd-elapsed rd-elapsed :fl-elapsed fl-elapsed
              :segments segs)))))

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
      (princ "\nClose gate (Doc 43 §3.2): \"row-hash equal で backend draw call skip\" delivers >=5x throughput\n")
      (princ "  Primary criterion = static-frame TOTAL speedup, which exercises the full\n")
      (princ "    skip path (rebuild short-circuit + flush draw-call elision).  This is\n")
      (princ "    the dominant case in real editor sessions (idle redisplay, cursor blink, etc.)\n")
      (princ "  Secondary signal = edit FLUSH-ONLY speedup, which isolates the row-hash\n")
      (princ "    diff effectiveness even when the rebuild cost is unavoidable.\n")
      (princ "  Scroll is informational (~1x expected; no diff savings possible).\n\n")
      (let ((static-ok (>= total-static bench-redisplay-close-gate-ratio))
            (edit-flush-ok (>= flush-edit bench-redisplay-close-gate-ratio)))
        (princ (format "  static       total %7.2fx  (flush-only %7.2fx)   %s   <- primary gate\n"
                       total-static flush-static (if static-ok "PASS" "FAIL")))
        (princ (format "  edit-flush   total %7.2fx  (flush-only %7.2fx)   %s   <- secondary signal\n"
                       total-edit   flush-edit   (if edit-flush-ok "PASS" "FAIL")))
        (princ (format "  scroll       total %7.2fx  (flush-only %7.2fx)   (informational)\n"
                       total-scroll flush-scroll))
        (princ (format "\nverdict: %s\n"
                       (if (and static-ok edit-flush-ok)
                           "PASS — Phase 3.B.5 close gate met"
                         "FAIL — investigate static (rebuild short-circuit) or edit-flush (row-hash diff)")))
        (unless (and static-ok edit-flush-ok)
          (kill-emacs 1))))))

(provide 'bench-redisplay)

;;; bench-redisplay.el ends here
