;;; emacs-redisplay-core.el --- fast first-frame redisplay core  -*- lexical-binding: t; -*-

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Minimal first-frame redisplay for the standalone NeLisp path.
;; `emacs-redisplay.el' remains the full implementation.  This core
;; defines the same public entry points needed by `nemacs-main' without
;; loading the full face / overlay / glyph-matrix engine, so TUI startup
;; can paint a usable buffer quickly and full redisplay can still be
;; required later.

;;; Code:

(require 'emacs-window)
(require 'emacs-buffer)
(require 'emacs-cc-xdisp-1)
(require 'emacs-tui-backend)

(defvar emacs-redisplay-core--handle-counter 0
  "Monotonic counter for lightweight redisplay handles.")

(defvar emacs-redisplay--current-handle nil
  "Current redisplay handle used by trigger helpers.")

(defvar emacs-redisplay-paint-mode-line-p t
  "Non-nil means reserve the last row of each window for a mode line.")

(defun emacs-redisplay-core--cell (object key)
  "Return OBJECT's alist cell for KEY."
  (assoc key (cdr object)))

(defun emacs-redisplay-core--get (object key)
  "Return OBJECT's value for KEY."
  (cdr (emacs-redisplay-core--cell object key)))

(defun emacs-redisplay-core--set (object key value)
  "Set OBJECT's KEY to VALUE and return VALUE."
  (let ((cell (emacs-redisplay-core--cell object key)))
    (if cell
        (setcdr cell value)
      (setcdr object (cons (cons key value) (cdr object)))))
  value)

(defun emacs-redisplay-handlep (object)
  "Return non-nil when OBJECT is a lightweight redisplay handle."
  (and (consp object) (eq (car object) 'emacs-redisplay-handle)))

(defun emacs-redisplay-handle-id (handle)
  "Return HANDLE's id."
  (emacs-redisplay-core--get handle :id))

(defun emacs-redisplay-handle-alive-p (handle)
  "Return non-nil if HANDLE is live."
  (emacs-redisplay-core--get handle :alive-p))

(defun emacs-redisplay-handle-backend (handle)
  "Return HANDLE's backend."
  (emacs-redisplay-core--get handle :backend))

(defun emacs-redisplay-handle-window-cache (handle)
  "Return HANDLE's per-window render cache."
  (emacs-redisplay-core--get handle :window-cache))

(defun emacs-redisplay-core--set-window-cache (handle value)
  "Set HANDLE's render cache to VALUE."
  (emacs-redisplay-core--set handle :window-cache value))

(defun emacs-redisplay-core--set-backend (handle value)
  "Set HANDLE's backend to VALUE."
  (emacs-redisplay-core--set handle :backend value))

(defun emacs-redisplay-core--set-alive-p (handle value)
  "Set HANDLE's alive flag to VALUE."
  (emacs-redisplay-core--set handle :alive-p value))

(defun emacs-redisplay-core--make-handle (id backend)
  "Return a lightweight redisplay handle."
  (list 'emacs-redisplay-handle
        (cons :id id)
        (cons :alive-p t)
        (cons :backend backend)
        (cons :window-cache nil)))

(defun emacs-redisplay-core--make-matrix (window width height rows cursor
                                                 dirty-rows state)
  "Return a lightweight matrix cache record."
  (list 'emacs-redisplay-glyph-matrix
        (cons :window window)
        (cons :width width)
        (cons :height height)
        (cons :rows rows)
        (cons :cursor cursor)
        (cons :dirty-rows dirty-rows)
        (cons :state state)))

(defun emacs-redisplay-glyph-matrix-window (matrix)
  "Return MATRIX's window."
  (emacs-redisplay-core--get matrix :window))

(defun emacs-redisplay-glyph-matrix-width (matrix)
  "Return MATRIX's width."
  (emacs-redisplay-core--get matrix :width))

(defun emacs-redisplay-glyph-matrix-height (matrix)
  "Return MATRIX's height."
  (emacs-redisplay-core--get matrix :height))

(defun emacs-redisplay-glyph-matrix-rows (matrix)
  "Return MATRIX's vector of rendered row strings."
  (emacs-redisplay-core--get matrix :rows))

(defun emacs-redisplay-glyph-matrix-cursor (matrix)
  "Return MATRIX's cursor cell."
  (emacs-redisplay-core--get matrix :cursor))

(defun emacs-redisplay-glyph-matrix-dirty-rows (matrix)
  "Return MATRIX's dirty row bitvector."
  (emacs-redisplay-core--get matrix :dirty-rows))

(defun emacs-redisplay-core--matrix-state (matrix)
  "Return MATRIX's lightweight render state."
  (emacs-redisplay-core--get matrix :state))

(defun emacs-redisplay-core--same-row-p (old-rows row text)
  "Return non-nil when OLD-ROWS has TEXT at ROW."
  (and old-rows
       (< row (length old-rows))
       (string= (aref old-rows row) text)))

(defun emacs-redisplay-core--dirty-rows (old-matrix rows height)
  "Return a dirty bitvector comparing OLD-MATRIX against ROWS."
  (let ((dirty (make-bool-vector height nil))
        (old-rows (and old-matrix
                       (= (emacs-redisplay-glyph-matrix-height old-matrix)
                          height)
                       (emacs-redisplay-glyph-matrix-rows old-matrix)))
        (r 0))
    (while (< r height)
      (let ((text (aref rows r)))
        (when (if old-rows
                  (not (emacs-redisplay-core--same-row-p old-rows r text))
                t)
          (aset dirty r t)))
      (setq r (1+ r)))
    dirty))

(defun emacs-redisplay-core--pad-row (text width)
  "Return TEXT clipped or padded to WIDTH terminal columns."
  (if (emacs-redisplay-core--printable-ascii-p text)
      (let ((size (length text)))
        (if (> size width) (substring text 0 width)
          (concat text (make-string (- width size) ?\s))))
    (let* ((clipped (truncate-string-to-width text width))
           (padding (max 0 (- width (string-width clipped)))))
      (concat clipped (make-string padding ?\s)))))

(defun emacs-redisplay-core--check-handle (handle)
  "Signal unless HANDLE is a live redisplay handle."
  (unless (emacs-redisplay-handlep handle)
    (signal 'wrong-type-argument (list 'emacs-redisplay-handlep handle)))
  (unless (emacs-redisplay-handle-alive-p handle)
    (signal 'error (list "redisplay handle is shut down" handle))))

(defun emacs-redisplay-core--cache-key (window)
  "Return a stable cache key for WINDOW."
  (if (and (fboundp 'emacs-window-id) (emacs-window-p window))
      (emacs-window-id window)
    window))

(defun emacs-redisplay-core--get-matrix (handle window)
  "Return HANDLE's cached matrix for WINDOW."
  (cdr (assoc (emacs-redisplay-core--cache-key window)
              (emacs-redisplay-handle-window-cache handle))))

(defun emacs-redisplay-core--put-matrix (handle window matrix)
  "Store MATRIX for WINDOW in HANDLE."
  (let* ((key (emacs-redisplay-core--cache-key window))
         (cache (emacs-redisplay-handle-window-cache handle))
         (cell (assoc key cache)))
    (if cell
        (setcdr cell matrix)
      (emacs-redisplay-core--set-window-cache
       handle (cons (cons key matrix) cache))))
  matrix)

;;;###autoload
(defun emacs-redisplay-init (&optional args)
  "Initialize a lightweight redisplay driver and return its handle."
  (let* ((counter (1+ emacs-redisplay-core--handle-counter))
         (id (intern (format "rdc-%d" counter)))
         (backend (plist-get args :backend)))
    (setq emacs-redisplay-core--handle-counter counter)
    (emacs-redisplay-core--make-handle id backend)))

;;;###autoload
(defun emacs-redisplay-shutdown (handle)
  "Shut down HANDLE."
  (emacs-redisplay-core--check-handle handle)
  (emacs-redisplay-core--set-alive-p handle nil)
  (emacs-redisplay-core--set-window-cache handle nil)
  (emacs-redisplay-core--set-backend handle nil)
  t)

(defun emacs-redisplay-set-current-handle (handle)
  "Set the active redisplay HANDLE."
  (when (and handle (not (emacs-redisplay-handlep handle)))
    (signal 'wrong-type-argument (list 'emacs-redisplay-handlep handle)))
  (setq emacs-redisplay--current-handle handle))

(defun emacs-redisplay-current-handle ()
  "Return the active redisplay handle, or nil."
  emacs-redisplay--current-handle)

(defun emacs-redisplay-core--buffer-string (buffer)
  "Return BUFFER's accessible text without properties."
  (if (bufferp buffer)
      (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))
    (nelisp-ec-with-current-buffer buffer (nelisp-ec-buffer-string))))

(defun emacs-redisplay-core--buffer-name (buffer)
  "Return BUFFER's display name."
  (if (bufferp buffer) (buffer-name buffer) (nelisp-ec-buffer-name buffer)))

(defun emacs-redisplay-core--buffer-size (buffer)
  "Return BUFFER's accessible character count."
  (if (bufferp buffer) (with-current-buffer buffer (buffer-size))
    (nelisp-ec-buffer-size buffer)))

(defun emacs-redisplay-core--buffer-text-tick (buffer)
  "Return BUFFER's character modification tick."
  (if (bufferp buffer) (with-current-buffer buffer (buffer-chars-modified-tick))
    (nelisp-ec-buffer-text-tick buffer)))

(defun emacs-redisplay-core--render-state (buffer width height start point)
  "Return the cheap state key for BUFFER in a lightweight WINDOW."
  (list buffer width height start point
        (emacs-redisplay-core--buffer-text-tick buffer)
        (emacs-redisplay-core--buffer-size buffer)
        (emacs-redisplay-core--buffer-name buffer)))

(defun emacs-redisplay-core--render-state-cacheable-p (state)
  "Return non-nil when STATE can prove text identity cheaply."
  (or (nth 5 state) (nth 6 state)))

(defun emacs-redisplay-core--same-render-state-p (a b)
  "Return non-nil when render states A and B describe the same rows.
Compare buffers by identity; using `equal' here recursively walks the
entire buffer object under standalone NeLisp."
  (and a b
       (eq (nth 0 a) (nth 0 b))
       (equal (nth 1 a) (nth 1 b))
       (equal (nth 2 a) (nth 2 b))
       (equal (nth 3 a) (nth 3 b))
       (equal (nth 4 a) (nth 4 b))
       (equal (nth 5 a) (nth 5 b))
       (equal (nth 6 a) (nth 6 b))
       (equal (nth 7 a) (nth 7 b))))

(defun emacs-redisplay-core--line-end (text start)
  "Return the index of the next newline in TEXT at or after START."
  (let ((i start)
        (n (length text))
        (found nil))
    (while (and (< i n) (not found))
      (if (= (aref text i) ?\n)
          (setq found i)
        (setq i (1+ i))))
    (or found n)))

(defun emacs-redisplay-core--printable-ascii-p (text)
  "Return non-nil when rendered TEXT consists of single-column ASCII."
  (let ((i 0) (n (length text)) (printable t))
    (while (and printable (< i n))
      (let ((char (aref text i)))
        (unless (and (<= 32 char) (<= char 126))
          (setq printable nil)))
      (setq i (1+ i)))
    printable))

(defun emacs-redisplay-core--row-width (text)
  "Measure rendered TEXT without table lookup for each printable ASCII cell."
  (cond
   ((emacs-redisplay-core--printable-ascii-p text) (length text))
   ((or (and (boundp 'buffer-display-table) buffer-display-table)
          (and (boundp 'standard-display-table) standard-display-table))
    (string-width text))
   (t
    (let ((i 0) (width 0) (n (length text)))
      (while (< i n)
        (let ((char (aref text i)))
          (setq width (+ width (if (and (<= 32 char) (<= char 126))
                                  1 (string-width (string char))))))
        (setq i (1+ i)))
      width))))

(defun emacs-redisplay-core--fit (text width)
  "Return rendered TEXT clipped to WIDTH terminal columns."
  (if (emacs-redisplay-core--printable-ascii-p text)
      (if (> (length text) width) (substring text 0 width) text)
    ;; A single curly quote must not send every ASCII cell in an otherwise
    ;; fitting row through the slower Unicode truncation path.
    (if (<= (emacs-redisplay-core--row-width text) width) text
      (truncate-string-to-width text width))))

(defun emacs-redisplay-core--blank-row-p (text)
  "Return non-nil when TEXT is all spaces."
  (let ((i 0)
        (n (length text))
        (blank t))
    (while (and (< i n) blank)
      (unless (= (aref text i) ?\s)
        (setq blank nil))
      (setq i (1+ i)))
    blank))

(defun emacs-redisplay-core--mode-line (buffer width)
  "Return a simple mode line for BUFFER."
  (emacs-redisplay-core--fit
   (concat " " (emacs-redisplay-core--buffer-name buffer) " ")
   width))

(defun emacs-redisplay-core--cursor-for (text start point width height)
  "Return an approximate cursor cons for POINT in TEXT."
  (let ((idx (max 0 (1- (or point 1))))
        (limit (length text))
        (pos (max 0 (1- (or start 1))))
        (row 0)
        (col 0)
        (body-height (max 1 height)))
    (while (and (< pos limit) (< pos idx) (< row body-height))
      (if (= (aref text pos) ?\n)
          (setq row (1+ row) col 0)
        (let ((char (aref text pos)))
          (setq col (+ col (if (and (<= 32 char) (<= char 126))
                              1 (char-width char)))))
        (when (>= col width)
          (setq row (1+ row) col 0)))
      (setq pos (1+ pos)))
    (cons (min row (1- body-height)) (min col (max 0 (1- width))))))

(defun emacs-redisplay-core--row-at-point (text start point width height)
  "Return (ROW COL ROW-START ROW-END) for POINT in visible TEXT.
START and POINT are one-based buffer positions.  WIDTH and HEIGHT are
the visible body dimensions."
  (let ((idx (max 0 (1- (or point 1))))
        (limit (length text))
        (pos (max 0 (1- (or start 1))))
        (row 0)
        (col 0)
        (row-start (max 0 (1- (or start 1)))))
    (while (and (< pos limit) (< pos idx) (< row height))
      (if (= (aref text pos) ?\n)
          (setq row (1+ row)
                col 0
                row-start (1+ pos))
        (setq col (+ col (char-width (aref text pos))))
        (when (>= col width)
          (setq row (1+ row)
                col 0
                row-start (1+ pos))))
      (setq pos (1+ pos)))
    (when (< row height)
      (let ((end (min (emacs-redisplay-core--line-end text row-start)
                      (+ row-start width))))
        (list row (min col (max 0 (1- width))) row-start end)))))

(defun emacs-redisplay-core--direct-draw-row (backend frame row col text)
  "Draw TEXT at ROW/COL using the cheapest available TUI path."
  (cond
   ((and (fboundp 'emacs-tui-backend--emit)
         (fboundp 'emacs-tui-backend--cup))
    (emacs-tui-backend--emit
     (concat (emacs-tui-backend--cup row col) text))
    t)
   ((and backend frame (fboundp 'emacs-tui-backend-canvas-draw-text))
    (emacs-tui-backend-canvas-draw-text backend frame row col text nil)
    (when (fboundp 'emacs-tui-backend-canvas-flush)
      (emacs-tui-backend-canvas-flush backend frame))
    t)
   (t nil)))

(defun emacs-redisplay-core--direct-cursor-if-changed (frame row col)
  "Move FRAME's cursor with a direct CUP write when ROW/COL changed."
  (let ((same (and (fboundp 'emacs-tui-backend-framep)
                   (emacs-tui-backend-framep frame)
                   (fboundp 'emacs-tui-backend-frame-cursor-row)
                   (fboundp 'emacs-tui-backend-frame-cursor-col)
                   (equal (emacs-tui-backend-frame-cursor-row frame) row)
                   (equal (emacs-tui-backend-frame-cursor-col frame) col))))
    (unless same
      (when (and (fboundp 'emacs-tui-backend-framep)
                 (emacs-tui-backend-framep frame))
        (setf (emacs-tui-backend-frame-cursor-row frame) row
              (emacs-tui-backend-frame-cursor-col frame) col))
      (when (and (fboundp 'emacs-tui-backend--emit)
                 (fboundp 'emacs-tui-backend--cup))
        (emacs-tui-backend--emit (emacs-tui-backend--cup row col))))
    (cons row col)))

(defun emacs-redisplay-core--direct-draw-row-and-cursor
    (backend frame row col text cursor-row cursor-col)
  "Draw TEXT and park cursor using one direct emit when possible."
  (cond
   ((and (fboundp 'emacs-tui-backend--emit)
         (fboundp 'emacs-tui-backend--cup))
    (emacs-tui-backend--emit
     (concat (emacs-tui-backend--cup row col)
             text
             (emacs-tui-backend--cup cursor-row cursor-col)))
    (when (and (fboundp 'emacs-tui-backend-framep)
               (emacs-tui-backend-framep frame))
      (setf (emacs-tui-backend-frame-cursor-row frame) cursor-row
            (emacs-tui-backend-frame-cursor-col frame) cursor-col))
    (cons cursor-row cursor-col))
   (t
    (emacs-redisplay-core--direct-draw-row backend frame row col text)
    (emacs-redisplay-core--direct-cursor-if-changed
     frame cursor-row cursor-col))))

(defun emacs-redisplay-core--ensure-matrix (handle window width height)
  "Return WINDOW's matrix cache, creating an empty one when absent."
  (or (emacs-redisplay-core--get-matrix handle window)
      (let ((rows (make-vector height "")))
        (emacs-redisplay-core--put-matrix
         handle window
         (emacs-redisplay-core--make-matrix
          window width height rows (cons 0 0)
          (make-bool-vector height nil) nil)))))

(defun emacs-redisplay-core--insert-hint-p (hint)
  "Return non-nil when HINT describes a printable insert."
  (or (and (vectorp hint)
           (= (length hint) 4)
           (memq (aref hint 0) '(insert-char insert-text)))
      (and (consp hint)
           (memq (plist-get hint :kind) '(insert-char insert-text)))))

(defun emacs-redisplay-core--insert-hint-text (hint)
  "Return the inserted text from HINT, or nil."
  (cond
   ((and (vectorp hint) (= (length hint) 4)
         (eq (aref hint 0) 'insert-char)
         (integerp (aref hint 1)))
    (string (aref hint 1)))
   ((and (vectorp hint) (= (length hint) 4)
         (eq (aref hint 0) 'insert-text)
         (stringp (aref hint 1)))
    (aref hint 1))
   ((and (consp hint) (eq (plist-get hint :kind) 'insert-char)
         (integerp (plist-get hint :char)))
    (string (plist-get hint :char)))
   ((and (consp hint) (eq (plist-get hint :kind) 'insert-text)
         (stringp (plist-get hint :text)))
    (plist-get hint :text))
   (t nil)))

(defun emacs-redisplay-core--apply-insert-hint
    (handle frame window backend edges width _height body-height hint)
  "Apply printable insert HINT to WINDOW's cached current row.
Return non-nil when the hint was applied without reading buffer text."
  (let* ((matrix (emacs-redisplay-core--get-matrix handle window))
         (cursor (and matrix (emacs-redisplay-glyph-matrix-cursor matrix)))
         (text (emacs-redisplay-core--insert-hint-text hint))
         (row (and cursor (car cursor)))
         (col (and cursor (cdr cursor))))
    (when (and matrix cursor (stringp text)
               (> (length text) 0)
               (integerp row) (integerp col)
               (< row body-height)
               (< col width))
      (let* ((rows (emacs-redisplay-glyph-matrix-rows matrix))
             (old-line (if (and rows (< row (length rows)))
                           (aref rows row)
                         ""))
             (line-len (length old-line)))
        (when (<= col line-len)
          (let* ((prefix (substring old-line 0 col))
                 (suffix (substring old-line col))
                 (line (emacs-redisplay-core--fit
                        (concat prefix text suffix)
                        width))
                 (abs-row (+ (nth 1 edges) row))
                 (abs-col (nth 0 edges))
                 (new-col (min (+ col (length text)) (1- width))))
            (when (and rows (< row (length rows)))
              (aset rows row line))
            (emacs-redisplay-core--set matrix :cursor (cons row new-col))
            (emacs-redisplay-core--direct-draw-row-and-cursor
             backend frame abs-row abs-col
             (emacs-redisplay-core--pad-row line width)
             abs-row (+ abs-col new-col))
            t))))))

;;;###autoload
(defun emacs-redisplay-redisplay-window (handle window)
  "Render WINDOW's body and shared mode line into the row cache."
  (emacs-redisplay-core--check-handle handle)
  (let* ((w (or window (emacs-window-selected-window)))
         (buffer (emacs-window-window-buffer w))
         (width (max 1 (emacs-window-window-width w)))
         (height (max 1 (emacs-window-window-height w)))
         (body-height (if (and emacs-redisplay-paint-mode-line-p (> height 1)) (1- height) height))
         (text (emacs-redisplay-core--buffer-string buffer))
         (point (or (emacs-window-window-point w) 1))
         (window-start (or (emacs-window-window-start w) 1))
         (starts (list 0)) (i 0) (line 0) (start-line 0) (point-line 0)
         (rows (make-vector height ""))
         (old (emacs-redisplay-core--get-matrix handle w)))
    (while (< i (length text))
      (when (= (aref text i) ?\n) (push (1+ i) starts))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    (dolist (pos starts)
      (when (< pos window-start) (setq start-line line))
      (when (< pos point) (setq point-line line))
      (setq line (1+ line)))
    (unless (and (<= start-line point-line) (< point-line (+ start-line body-height)))
      (setq start-line (max 0 (- point-line (/ body-height 2)))
            window-start (1+ (nth start-line starts)))
      (emacs-window-set-window-start w window-start))
    (setq i 0)
    (while (< i body-height)
      (let ((beg (nth (+ start-line i) starts))
            (end (nth (+ start-line i 1) starts)))
        (aset rows i (if beg (emacs-redisplay-core--fit
                             (substring text beg (if end (1- end) (length text))) width) "")))
      (setq i (1+ i)))
    (let* ((end (or (nth (+ start-line body-height) starts) (length text)))
           (spans (and (< body-height height)
                       (emacs-redisplay-mode-line-spans w width (1+ end))))
           (mode (mapconcat #'car spans ""))
           (state (list spans (emacs-window-window-edges w)))
           (matrix (emacs-redisplay-core--make-matrix
                    w width height rows
                    (emacs-redisplay-core--cursor-for text window-start point width body-height)
                    (emacs-redisplay-core--dirty-rows old rows height) state)))
      (when spans
        (aset rows body-height (emacs-redisplay-core--pad-row mode width))
        (aset (emacs-redisplay-glyph-matrix-dirty-rows matrix) body-height
              (not (and old
                        (equal spans (car (emacs-redisplay-core--matrix-state old)))
                        (emacs-redisplay-core--same-row-p
                         (emacs-redisplay-glyph-matrix-rows old) body-height
                         (aref rows body-height))))))
      (when (and old (not (equal (nth 1 (emacs-redisplay-core--matrix-state old))
                                 (nth 1 state))))
        (dotimes (r height) (aset (emacs-redisplay-glyph-matrix-dirty-rows matrix) r t)))
      (emacs-redisplay-core--put-matrix handle w matrix))))

(defun emacs-redisplay-redisplay (handle &optional _frame)
  "Render all live leaf windows into HANDLE."
  (emacs-redisplay-core--check-handle handle)
  (dolist (w (emacs-window-window-list))
    (when (and (emacs-window-p w) (emacs-window-leaf-p w))
      (emacs-redisplay-redisplay-window handle w)))
  handle)

(defun emacs-redisplay-glyph-matrix (handle window)
  "Return HANDLE's cached matrix for WINDOW."
  (emacs-redisplay-core--check-handle handle)
  (emacs-redisplay-core--get-matrix handle window))

;;;###autoload
(defun emacs-redisplay-core--paint-text (backend frame row col text face)
  "Paint TEXT at its terminal column with already realized FACE."
  (if (fboundp 'emacs-tui-backend--emit)
      (emacs-tui-backend--emit
       (concat (emacs-tui-backend--cup row col) "\e[0m"
               (emacs-tui-backend--sgr-from-face face) text "\e[0m"))
    (emacs-tui-backend-canvas-draw-text backend frame row col text face)))

(defun emacs-redisplay-flush-frame (handle frame)
  "Flush live window rows, styled mode lines and the echo area."
  (let ((backend (emacs-redisplay-handle-backend handle)) (count 0))
    (when backend
      (dolist (w (emacs-window-window-list))
        (let ((matrix (emacs-redisplay-core--get-matrix handle w)))
          (when matrix
            (let* ((edges (emacs-window-window-edges w)) (left (nth 0 edges)) (top (nth 1 edges))
                   (rows (emacs-redisplay-glyph-matrix-rows matrix))
                   (dirty (emacs-redisplay-glyph-matrix-dirty-rows matrix))
                   (width (emacs-redisplay-glyph-matrix-width matrix))
                   (height (emacs-redisplay-glyph-matrix-height matrix))
                   (spans (car (emacs-redisplay-core--matrix-state matrix))))
              (dotimes (r height)
                (when (aref dirty r)
                  (if (and spans (= r (1- height)))
                      (let ((col 0))
                        (dolist (span (append spans (list (cons (make-string width ?\s) '((:reverse . t))))))
                          (when (< col width)
                            (let ((text (emacs-redisplay-core--fit (car span) (- width col))))
                              (emacs-redisplay-core--paint-text backend frame (+ top r) (+ left col) text (cdr span))
                              (setq col (+ col (emacs-redisplay-core--row-width text)))))))
                    (emacs-redisplay-core--paint-text backend frame (+ top r) left
                     (emacs-redisplay-core--pad-row (aref rows r) width) nil))
                  (aset dirty r nil) (setq count (1+ count))))))))
      (when (boundp 'emacs-special-buffers-echo-message)
        (emacs-redisplay-core--paint-text backend frame
         (1- (emacs-tui-backend-frame-height frame)) 0
         (emacs-redisplay-core--pad-row (or emacs-special-buffers-echo-message "")
                                      (emacs-tui-backend-frame-width frame)) nil))
      (unless (fboundp 'emacs-tui-backend--emit)
        (emacs-tui-backend-canvas-flush backend frame)))
    count))

(defun emacs-redisplay-set-cursor (handle frame &optional window)
  "Show the cursor for WINDOW on FRAME."
  (emacs-redisplay-core--check-handle handle)
  (let* ((backend (emacs-redisplay-handle-backend handle))
         (w (or window (emacs-window-selected-window)))
         (matrix (and w (emacs-redisplay-core--get-matrix handle w)))
         (cursor (and matrix (emacs-redisplay-glyph-matrix-cursor matrix)))
         (edges (and w (emacs-window-window-edges w))))
    (when (and backend edges)
      (emacs-tui-backend-cursor-show
       backend frame
       (+ (nth 1 edges) (or (car cursor) 0))
       (+ (nth 0 edges) (or (cdr cursor) 0))))))

(defun emacs-redisplay-core--set-cursor-if-changed (handle frame window)
  "Park cursor for WINDOW on FRAME, avoiding redundant TUI writes."
  (let* ((backend (emacs-redisplay-handle-backend handle))
         (matrix (and window (emacs-redisplay-core--get-matrix handle window)))
         (cursor (and matrix (emacs-redisplay-glyph-matrix-cursor matrix)))
         (edges (and window (emacs-window-window-edges window))))
    (when (and backend edges)
      (let ((row (+ (nth 1 edges) (or (car cursor) 0)))
            (col (+ (nth 0 edges) (or (cdr cursor) 0))))
        (if (fboundp 'emacs-tui-backend-cursor-show-if-changed)
            (emacs-tui-backend-cursor-show-if-changed backend frame row col)
          (emacs-tui-backend-cursor-show backend frame row col))))))

(defun emacs-redisplay-mark-window-dirty (_handle _window)
  "Compatibility no-op; lightweight redisplay rebuilds rows on demand."
  t)

(defun emacs-redisplay-mark-frame-dirty (_handle &optional _frame)
  "Compatibility no-op; lightweight redisplay rebuilds rows on demand."
  t)

(defun emacs-redisplay-core-initial-paint (handle frame)
  "Paint the first visible TUI line for HANDLE on FRAME.
This intentionally bypasses the full row-cache path: the first screen
only needs an observable selected-buffer mode line, and direct output is
much cheaper than building and flushing a full matrix under standalone
NeLisp."
  (emacs-redisplay-core--check-handle handle)
  (let* ((backend (emacs-redisplay-handle-backend handle))
         (window (and (fboundp 'emacs-window-selected-window)
                      (emacs-window-selected-window)))
         (buffer (and window (emacs-window-window-buffer window)))
         (edges (and window (emacs-window-window-edges window)))
         (left (or (nth 0 edges) 0))
         (top (or (nth 1 edges) 0))
         (height (if window (max 1 (emacs-window-window-height window)) 1))
         (row (+ top (1- height)))
         (width (if window (emacs-window-window-width window) 80))
         (text (emacs-redisplay-core--mode-line buffer
                                                width)))
    (let ((painted
           (cond
            ((and (fboundp 'emacs-tui-backend--emit)
                  (fboundp 'emacs-tui-backend--cup))
             (emacs-tui-backend--emit
              (concat (emacs-tui-backend--cup row left) text))
             t)
            ((and backend frame (fboundp 'emacs-tui-backend-canvas-draw-text))
             (emacs-tui-backend-canvas-draw-text backend frame row left text nil)
             (when (fboundp 'emacs-tui-backend-canvas-flush)
               (emacs-tui-backend-canvas-flush backend frame))
             t)
            (t nil))))
      ;; Alternate-screen entry cleared the body.  Cache that painted blank
      ;; state even for nonempty buffers, so the first input need only paint
      ;; their text rows.  Newly split windows still clear every covered row.
      (when window
        (let* ((body-height (if (and emacs-redisplay-paint-mode-line-p
                                     (> height 1))
                                (1- height)
                              height))
               (rows (make-vector height ""))
               (dirty (make-bool-vector height nil))
               (window-start (or (emacs-window-window-start window) 1))
               (point (emacs-window-window-point window)))
          (when (< body-height height)
            (aset rows body-height
                  (emacs-redisplay-core--mode-line buffer width)))
          (emacs-redisplay-core--put-matrix
           handle window
           (emacs-redisplay-core--make-matrix
            window width height rows
            (cons 0 0) dirty
            (list nil (emacs-window-window-edges window))))))
      painted)))

(defun emacs-redisplay-core-repaint (handle frame)
  "Repaint all leaf windows and then restore the selected cursor."
  (emacs-redisplay-redisplay handle)
  (emacs-redisplay-flush-frame handle frame)
  (emacs-redisplay-set-cursor handle frame (emacs-window-selected-window))
  t)

(defun emacs-redisplay-core-repaint-current-line (handle frame &optional _hint)
  "Repaint body, mode lines and echo area after an insertion."
  (emacs-redisplay-core-repaint handle frame))

(defun emacs-redisplay-redraw-display (handle &optional frame)
  "Render and optionally flush HANDLE."
  (emacs-redisplay-redisplay handle frame)
  (when frame
    (emacs-redisplay-flush-frame handle frame)))

(defun emacs-redisplay-force-mode-line-update (&rest _args)
  "Compatibility no-op for the lightweight redisplay core."
  t)

(unless (fboundp 'md5)
  (defun md5 (object &optional start end coding-system noerror)
    "Return the MD5 digest of OBJECT, a string or buffer.
START and END delimit characters.  CODING-SYSTEM specifies the encoding;
NOERROR permits falling back to raw text for an invalid coding system."
    (unless (or (stringp object) (bufferp object))
      (signal 'error (list "Invalid object argument" object)))
    (let* ((buffer (bufferp object))
           (text
            (if buffer
                (with-current-buffer object
                  (let ((from (or start (point-min)))
                        (to (or end (point-max))))
                    (dolist (position (list from to))
                      (unless (or (integerp position) (markerp position))
                        (signal 'wrong-type-argument
                                (list 'integer-or-marker-p position))))
                    (when (markerp from) (setq from (marker-position from)))
                    (when (markerp to) (setq to (marker-position to)))
                    (unless (and from to)
                      (signal 'error '("Marker does not point anywhere")))
                    (unless (and (<= (point-min) from (point-max))
                                 (<= (point-min) to (point-max)))
                      (signal 'args-out-of-range (list from to)))
                    (when (> from to)
                      (let ((swap from)) (setq from to to swap)))
                    (buffer-substring-no-properties from to)))
              (substring object (or start 0) end)))
           (coding
            (or coding-system
                (and buffer
                     (with-current-buffer object
                       (or (and (boundp 'coding-system-for-write)
                                coding-system-for-write)
                           (and (boundp 'buffer-file-coding-system)
                                buffer-file-coding-system))))
                (if (multibyte-string-p text)
                    (coding-system-priority-list t)
                  'raw-text))))
      (unless (coding-system-p coding)
        (if noerror
            (setq coding 'raw-text)
          (signal 'coding-system-error (list coding))))
      ;; GNU validates the coding system but hashes unibyte text unchanged.
      ;; Raw text preserves the internal byte representation of multibyte text.
      (secure-hash 'md5
                   (cond
                    ((not (multibyte-string-p text)) text)
                    ((eq coding 'raw-text) (string-as-unibyte text))
                    (t (encode-coding-string text coding)))))))

(defun emacs-redisplay-core--character-width (char)
  "Return CHAR's width under the current buffer's control display policy."
  (cond
   ((= char ?\t)
    (if (and (boundp 'tab-width) (integerp tab-width)
             (> tab-width 0) (<= tab-width 1000))
        tab-width
      8))
   ((or (< char 32) (= char 127))
    (if (= char ?\n) 0
      (if (and (boundp 'ctl-arrow) (not ctl-arrow)) 4 2)))
   ((or (and (<= 128 char) (< char 160))
        (and (<= #x3fff80 char) (<= char #x3fffff)))
    4)
   (t (char-width char))))

(when (or (not (fboundp 'string-width))
          (fboundp 'nelisp--repr))
  (defun string-width (&rest args)
    "Return STRING's display width in the current buffer.
FROM and TO delimit a substring, including negative substring indices.
Each tab occupies `tab-width' columns, independently of its position.

(fn STRING &optional FROM TO)"
    (unless (<= 1 (length args) 3)
      (signal 'wrong-number-of-arguments (list 'string-width (length args))))
    (let ((string (car args))
          (from (cadr args))
          (to (nth 2 args)))
      (unless (stringp string)
        (signal 'wrong-type-argument (list 'stringp string)))
      (let* ((text (substring string (or from 0) to))
             (table (or (and (boundp 'buffer-display-table)
                             buffer-display-table)
                        (and (boundp 'standard-display-table)
                             standard-display-table)))
             (width 0)
             (i 0))
        (while (< i (length text))
          (let* ((char (aref text i))
                 (glyphs (and (char-table-p table) (aref table char))))
            (if (vectorp glyphs)
                (let ((j 0))
                  (while (< j (length glyphs))
                    (setq width
                          (+ width (emacs-redisplay-core--character-width
                                    (logand (aref glyphs j) #x3fffff)))
                          j (1+ j))))
              (setq width (+ width
                             (emacs-redisplay-core--character-width char)))))
          (setq i (1+ i)))
        width))))

(defun emacs-redisplay-core--uid (uid)
  "Convert UID to an unsigned 32-bit user ID, or signal GNU's error."
  (let ((value uid))
    (when (consp value)
      (let ((high (car value))
            (low (if (consp (cdr value)) (cadr value) (cdr value)))
            (tail (and (consp (cdr value)) (cddr value))))
        ;; The obsolete (HIGH MIDDLE . LOW) representation appends a
        ;; 16-bit LOW component.  A valid LOW selects that format; otherwise GNU
        ;; treats the first two components as a pair of 16-bit integers.
        (setq value
              (and (integerp high) (integerp low)
                   (<= 0 high 65535) (<= 0 low 65535)
                   (if (and (integerp tail) (<= 0 tail 65535))
                       ;; A nonzero HIGH cannot fit an unsigned 32-bit UID.
                       (and (= high 0) (+ (* low 65536) tail))
                     (+ (* high 65536) low))))))
    (unless (and (numberp value) (<= 0 value #xffffffff)
                 (or (integerp value) (= value (truncate value))))
      (signal 'error
              '("Not an in-range integer, integral float, or cons of integers")))
    (truncate value)))

(defun emacs-redisplay-core--passwd (key)
  "Look up KEY in the operating system's user database.
Return the colon-separated account fields, or nil for an unknown account."
  (with-temp-buffer
    (when (eq (call-process "getent" nil t nil "passwd"
                            (if (stringp key) key (number-to-string key)))
              0)
      (let ((fields (split-string (buffer-string) ":")))
        ;; getent also accepts numeric keys; a string key names a login.
        (when (and (>= (length fields) 7)
                   (or (not (stringp key)) (equal key (car fields))))
          fields)))))

(unless (fboundp 'user-login-name)
  (defun user-login-name (&optional uid)
    "Return the current login name, or the login name belonging to UID.
An unknown numeric user ID returns nil."
    (if (null uid)
        (if (boundp 'user-login-name)
            (symbol-value 'user-login-name)
          (or (getenv "LOGNAME") (getenv "USER")
              (car (emacs-redisplay-core--passwd (user-uid)))))
      (car (emacs-redisplay-core--passwd
            (emacs-redisplay-core--uid uid))))))

(unless (fboundp 'user-full-name)
  (defun user-full-name (&optional uid)
    "Return the current full name, or the full name of UID.
UID may be a numeric user ID or a login name.  Unknown users return nil."
    (if (and (null uid) (boundp 'user-full-name))
        (symbol-value 'user-full-name)
      (unless (or (null uid) (stringp uid) (numberp uid))
        (signal 'error '("Invalid UID specification")))
      (let* ((key (if (stringp uid) uid
                    (emacs-redisplay-core--uid (or uid (user-uid)))))
             (fields (emacs-redisplay-core--passwd key)))
        (if fields
            (let* ((name (car (split-string (nth 4 fields) ",")))
                   (login (car fields))
                   (capitalized (if (= (length login) 0) login
                                  (concat (upcase (substring login 0 1))
                                          (substring login 1)))))
              (replace-regexp-in-string "&" capitalized (or name "") t t))
          (and (null uid) "unknown"))))))

(provide 'emacs-redisplay-core)

;;; emacs-redisplay-core.el ends here
