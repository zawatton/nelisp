;;; emacs-cc-indent-1.el --- indent.c primitives -*- lexical-binding: t; -*-

(defun emacs-cc-indent-1--text ()
  "Return current buffer text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun emacs-cc-indent-1--line-col (text pos)
  "Return (LINE . COLUMN) for POS in TEXT."
  (let ((i 0) (line 0) (col 0) (tab (max 1 (or tab-width 8))))
    (while (< i (min (max 0 pos) (length text)))
      (let ((ch (aref text i)))
        (if (= ch ?\n) (setq line (1+ line) col 0)
          (setq col (if (= ch ?\t) (* tab (1+ (/ col tab)))
                      (+ col (if (and (fboundp 'char-width)
                                      (numberp (char-width ch)))
                                 (max 0 (char-width ch)) 1)))))
        (setq i (1+ i))))
    (cons line col)))

(unless (fboundp 'compute-motion)
  (defun compute-motion (from frompos to topos width offsets window)
    "Scan current buffer from FROM and return ending position and screen location.
See GNU Emacs `compute-motion' for the argument semantics."
    (ignore window)
    (unless (or (integerp from) (markerp from))
      (signal 'wrong-type-argument (list 'integer-or-marker-p from)))
    (unless (and (consp frompos) (integerp (car frompos)) (integerp (cdr frompos)))
      (signal 'wrong-type-argument (list 'consp frompos)))
    (unless (or (null to) (integerp to)) (signal 'wrong-type-argument (list 'integerp to)))
    (unless (or (null topos) (consp topos)) (signal 'wrong-type-argument (list 'consp topos)))
    (let* ((text (emacs-cc-indent-1--text))
           (start (max (point-min) (min from (point-max))))
           (limit (max start (min (or to (point-max)) (point-max))))
           (h (car frompos)) (v (cdr frompos))
           (target-v (and topos (cdr topos))) (target-h (and topos (car topos)))
           ;; In batch mode a nil WINDOW has no display continuation state;
           ;; GNU's primitive counts buffer newlines and does not wrap by WIDTH.
           (max-width (if (and window (window-live-p window))
                          (or width (window-body-width window)) 0))
           (scroll (or (car-safe offsets) 0)) (taboff (or (cdr-safe offsets) 0))
           (pos start) (prev h) (contin nil))
      (while (and (< pos limit)
                  (not (and target-v (or (> v target-v)
                                         (and (= v target-v) (>= h target-h))))))
        (let* ((p (1- pos)) (ch (aref text p)) (w (if (= ch ?\t)
              (- (* (max 1 tab-width) (1+ (/ (+ h taboff) (max 1 tab-width)))) (+ h taboff))
              (if (= ch ?\n) 0 (max 1 (or (and (fboundp 'char-width) (char-width ch)) 1))))))
          (setq prev h)
          (if (= ch ?\n) (setq pos (1+ pos) v (1+ v) h 0 contin nil)
            (if (and (> max-width 0) (> (+ h w) max-width) (> h 0))
                (setq v (1+ v) h 0 contin t prev 0)
              (setq h (+ h w) pos (1+ pos) contin nil)
              (when (= v 0) (setq prev h))))))
      (list pos h v prev contin))))

(unless (fboundp 'vertical-motion)
  (defun vertical-motion (&rest args)
    "Move point by LINES screen lines and return the number traversed."
    (unless (and (<= 1 (length args)) (<= (length args) 3))
      (signal 'wrong-number-of-arguments (list 'vertical-motion (length args))))
    (let* ((lines (car args))
           (cur-col (nth 2 args))
           (cols (and (consp lines) (car lines)))
           (n (if (consp lines) (cdr lines) lines))
           (text (emacs-cc-indent-1--text))
           (origin (point))
           (lc (emacs-cc-indent-1--line-col text (- origin (point-min))))
           (target-line (+ (car lc) (if (integerp n) n (signal 'wrong-type-argument (list 'fixnump n)))))
           (line 0) (idx 0) (start 0) (end 0) (col 0)
           (found nil))
      ;; This fallback maps each buffer line to one screen line.  CUR-COL is a
      ;; source-position hint and cannot select a different destination column
      ;; in this no-wrap model; integer LINES always targets the line start.
      (ignore cur-col)
      (while (and (<= idx (length text)) (not found))
        (setq start idx)
        (while (and (< idx (length text)) (/= (aref text idx) ?\n)) (setq idx (1+ idx)))
        (setq end idx)
        (when (= line target-line) (setq found t))
        (if (< idx (length text)) (setq idx (1+ idx) line (1+ line))
          (setq idx (1+ idx))))
      (unless found (setq start (if (< target-line 0) 0 (length text)) end start))
      ;; GNU's noninteractive implementation delegates to `vmotion' with only
      ;; N LINES, so the optional COLS value is used only by its display path.
      (when (and cols (not noninteractive)) (setq col cols))
      (let ((p start) (c 0))
        (while (and (< p end) (< c col))
          (setq c (+ c (max 1 (or (and (fboundp 'char-width) (char-width (aref text p))) 1))) p (1+ p)))
        (goto-char (+ (point-min) p)))
      (- (car (emacs-cc-indent-1--line-col text (- (point) (point-min))))
         (car lc)))))

(provide 'emacs-cc-indent-1)
