;;; autocad-bench.el --- per-operation benches for nelisp-autocad -*- lexical-binding: t -*-

;; Usage: BENCH=NAME [AUTOCAD_FIXTURE=svg] nelisp --load test/perf/autocad-bench.el
;;
;; One bench per run, named by $BENCH (the runtime does not pass script
;; arguments after `--' to `command-line-args-left' yet).  Each prints
;; "AUTOCAD-BENCH NAME value unit (limit L) PASS|FAIL" and exits non-zero
;; when the value is over its limit, so tools/ai/autocad-perf-progress.org
;; can use it as a `cmd:' check.  Per-operation figures subtract an empty
;; loop of the same length, so they measure the operation, not `while'.
;; Limits come from handoff/nelisp-autocad-perf_dev-nelisp_2026-10-17.md
;; in Notes.  AUTOCAD_FIXTURE is an SVG from test/perf/gen-inkscape-svg.py.

(defmacro autocad-bench-loop (n &rest body)
  "Seconds to run BODY N times."
  `(let ((t0 (float-time)) (i 0))
     (while (< i ,n) ,@body (setq i (1+ i)))
     (- (float-time) t0)))

(defmacro autocad-bench-per-op (n &rest body)
  "Microseconds per evaluation of BODY, less an empty loop's."
  `(let ((base (autocad-bench-loop ,n nil))
         (run (autocad-bench-loop ,n ,@body)))
     (max 0.0 (/ (* 1e6 (- run base)) ,n))))

(defun autocad-bench-fixture (&optional var)
  (let ((f (getenv (or var "AUTOCAD_FIXTURE"))))
    (unless (and f (file-exists-p f))
      (error "AUTOCAD_FIXTURE must name an SVG from gen-inkscape-svg.py"))
    (with-temp-buffer (insert-file-contents f) (buffer-string))))

(defconst autocad-bench-line
  "M 1090.09365241565,234.5 L 12.5,6.25 C 1,2 3,4 5,6\tstroke-width:0.26458332;fill:none")

(defun autocad-bench-1mb-buffer ()
  "Insert about 1 MB of 70-char lines into the current buffer."
  (insert (mapconcat #'identity (make-list 15000 (substring autocad-bench-line 0 69)) "\n")))

(defun autocad-bench-tokenize (text)
  "Count the d=\"...\" numbers and <text> elements of SVG TEXT.
Returns (PATHS TEXTS NUMBERS)."
  (let ((paths 0) (texts 0) (numbers 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward " d=\"" nil t)
        (let* ((start (point))
               (end (progn (search-forward "\"") (1- (point))))
               (d (buffer-substring start end))
               (pos 0))
          (setq paths (1+ paths))
          (while (string-match "-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e-?[0-9]+\\)?" d pos)
            (string-to-number (match-string 0 d))
            (setq numbers (1+ numbers) pos (match-end 0)))))
      (goto-char (point-min))
      (while (re-search-forward "<text\\b" nil t)
        (setq texts (1+ texts))
        (dolist (attr '("x" "y"))
          (save-excursion
            (when (re-search-forward (concat " " attr "=\"\\([^\"]*\\)\"") nil t)
              (string-to-number (match-string 1))
              (setq numbers (1+ numbers)))))))
    (list paths texts numbers)))

(defconst autocad-bench-cases
  ;; NAME LIMIT UNIT THUNK
  `((stn-float 10 "us/op"
     ,(lambda () (autocad-bench-per-op 200 (string-to-number "1090.09365241565"))))
    (stn-exp 10 "us/op"
     ,(lambda () (autocad-bench-per-op 200 (string-to-number "-1.5e-3"))))
    (format-4f 10 "us/op"
     ,(lambda () (autocad-bench-per-op 200 (format "%.4f" 1090.09365241565))))
    (string-search-70 5 "us/op"
     ,(lambda () (autocad-bench-per-op 500 (string-search "\t" autocad-bench-line))))
    (string-search-miss-3k 20 "us/op"
     ,(lambda () (let ((big (make-string 3000 ?a)))
                   (autocad-bench-per-op 200 (string-search "zz" big)))))
    (string-equal-8 5 "us/op"
     ,(lambda () (autocad-bench-per-op 500 (string= "abcdefgh" "abcdefgh"))))
    (substring-8 5 "us/op"
     ,(lambda () (autocad-bench-per-op 500 (substring autocad-bench-line 2 10))))
    (numbers-fixture 1.0 "s"
     ,(lambda () (let ((text (autocad-bench-fixture)) (count 0))
                   ;; Every d="..." coordinate, found with string-search and
                   ;; read with string-to-number: the regex-free path.
                   (prog1
                       (autocad-bench-loop 1
                         (let ((pos 0))
                           (while (setq pos (string-search " d=\"" text pos))
                             (let* ((start (+ pos 4)) (end (string-search "\"" text start))
                                    (p start))
                               (while (< p end)
                                 (let ((q p))
                                   (while (and (< q end) (not (memq (aref text q) '(?\s ?,))))
                                     (setq q (1+ q)))
                                   (when (> q p)
                                     (let ((tok (substring text p q)))
                                       (unless (member tok '("M" "L"))
                                         (string-to-number tok)
                                         (setq count (1+ count)))))
                                   (setq p (1+ q))))
                               (setq pos end)))))
                     (unless (= count 16038) (error "Counted %d numbers" count))))))
    (point-1mb 5 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer) (goto-char 500000)
                                   (autocad-bench-per-op 500 (point)))))
    (goto-char-1mb 5 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer)
                   (autocad-bench-per-op 500 (goto-char (if (= 0 (% i 2)) 1000 900000))))))
    (char-after-1mb 5 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer) (goto-char 500000)
                                   (autocad-bench-per-op 500 (char-after)))))
    (buffer-substring-60 10 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer)
                   (autocad-bench-per-op 500 (buffer-substring 500000 500060)))))
    (insert-70-1mb 20 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer)
                   (autocad-bench-per-op 500 (insert autocad-bench-line)))))
    (forward-line-fixture 1.0 "s"
     ,(lambda () (let ((text (autocad-bench-fixture)))
                   (with-temp-buffer (insert text) (goto-char (point-min))
                     (autocad-bench-loop 1 (while (= 0 (forward-line 1))))))))
    (search-forward-fixture 0.5 "s"
     ,(lambda () (let ((text (autocad-bench-fixture)))
                   (with-temp-buffer (insert text) (goto-char (point-min))
                     (autocad-bench-loop 1 (while (search-forward " d=\"" nil t)))))))
    (string-match-digits 20 "us/op"
     ,(lambda () (autocad-bench-per-op 200 (string-match "[0-9]+" autocad-bench-line 8))))
    (split-string-tab 20 "us/op"
     ,(lambda () (autocad-bench-per-op 200 (split-string autocad-bench-line "\t"))))
    (re-search-forward-digits 50 "us/op"
     ,(lambda () (with-temp-buffer (autocad-bench-1mb-buffer) (goto-char (point-min))
                   (autocad-bench-per-op 200 (re-search-forward "[0-9]+" nil t)))))
    (split-lines-fixture 1.0 "s"
     ,(lambda () (let ((text (autocad-bench-fixture)))
                   (autocad-bench-loop 1 (split-string text "\n")))))
    (aref-lines-fixture 2.0 "s"
     ,(lambda () (let ((lines (split-string (autocad-bench-fixture) "\n")))
                   (autocad-bench-loop 1
                     (dolist (l lines) (let ((k 0) (n (length l)))
                                         (while (< k n) (aref l k) (setq k (1+ k)))))))))
    (tokenize-fixture 5.0 "s"
     ,(lambda () (let* ((text (autocad-bench-fixture)) (counts nil)
                        (secs (autocad-bench-loop 1 (setq counts (autocad-bench-tokenize text)))))
                   ;; The default fixture: 2000 paths, 550 texts, 17138 numbers.
                   (if (equal counts '(2000 550 17138)) secs
                     (error "Tokenizer counted %S" counts)))))
    (tokenize-fixture-4x 20.0 "s"
     ,(lambda () (let* ((text (autocad-bench-fixture "AUTOCAD_FIXTURE_4X")) (counts nil)
                        (secs (autocad-bench-loop 1 (setq counts (autocad-bench-tokenize text)))))
                   ;; gen-inkscape-svg.py OUT 8000 2200: 2.8 MB, the size of
                   ;; the larger real drawings.
                   (if (equal counts '(8000 2200 68516)) secs
                     (error "Tokenizer counted %S" counts)))))))

(let* ((name (intern (or (getenv "BENCH") "")))
       (case (assq name autocad-bench-cases))
       (ok nil))
  (if (not case)
      (princ (format "AUTOCAD-BENCH unknown BENCH=%s; one of %S\n" name
                     (mapcar #'car autocad-bench-cases)))
    (condition-case err
        (let ((value (funcall (nth 3 case))))
          (setq ok (<= value (nth 1 case)))
          (princ (format "AUTOCAD-BENCH %s %.3f %s (limit %s) %s\n" name value (nth 2 case)
                         (nth 1 case) (if ok "PASS" "FAIL"))))
      (error (princ (format "AUTOCAD-BENCH %s ERROR %S\n" name err)))))
  (kill-emacs (if ok 0 1)))

;;; autocad-bench.el ends here
