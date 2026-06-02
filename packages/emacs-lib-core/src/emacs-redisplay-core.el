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
                (not (emacs-redisplay-core--blank-row-p text)))
          (aset dirty r t)))
      (setq r (1+ r)))
    dirty))

(defun emacs-redisplay-core--pad-row (text width)
  "Return TEXT clipped or right-padded to WIDTH."
  (let ((n (length text)))
    (cond
     ((> n width) (substring text 0 width))
     ((< n width) (concat text (make-string (- width n) ?\s)))
     (t text))))

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
  "Return BUFFER's text as a plain string."
  (cond
   ((and buffer (fboundp 'nelisp-ec-with-current-buffer)
         (fboundp 'nelisp-ec-buffer-string))
    (nelisp-ec-with-current-buffer buffer
      (nelisp-ec-buffer-string)))
   ((and (fboundp 'buffer-string)
         (or (null buffer) (eq buffer (current-buffer))))
    (buffer-string))
   (t "")))

(defun emacs-redisplay-core--buffer-name (buffer)
  "Return BUFFER's display name."
  (cond
   ((and buffer (fboundp 'nelisp-ec-buffer-name))
    (nelisp-ec-buffer-name buffer))
   ((fboundp 'buffer-name)
    (or (buffer-name buffer) ""))
   (t "")))

(defun emacs-redisplay-core--buffer-size (buffer)
  "Return BUFFER's character count without copying its full text."
  (cond
   ((and buffer (fboundp 'nelisp-ec-buffer-size))
    (condition-case _ (nelisp-ec-buffer-size buffer) (error nil)))
   ((and (fboundp 'buffer-size)
         (or (null buffer) (eq buffer (current-buffer))))
    (buffer-size))
   (t nil)))

(defun emacs-redisplay-core--buffer-text-tick (buffer)
  "Return BUFFER's text-content tick when available."
  (cond
   ((and buffer (fboundp 'nelisp-ec-buffer-text-tick))
    (condition-case _ (nelisp-ec-buffer-text-tick buffer) (error nil)))
   ((fboundp 'emacs-buffer-buffer-text-tick)
    (condition-case _ (emacs-buffer-buffer-text-tick buffer) (error nil)))
   (t nil)))

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

(defun emacs-redisplay-core--fit (text width)
  "Return TEXT clipped to WIDTH.
The lightweight core intentionally does not right-pad rows: the TUI
backend already starts with a blank canvas, and sending only visible
text avoids an expensive full-width first paint under standalone
NeLisp."
  (let ((n (length text)))
    (if (> n width) (substring text 0 width) text)))

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
        (setq col (1+ col))
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
        (setq col (1+ col))
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
  "Render WINDOW into HANDLE's lightweight row cache."
  (emacs-redisplay-core--check-handle handle)
  (let* ((w (or window (emacs-window-selected-window)))
         (buffer (and w (emacs-window-window-buffer w)))
         (width (max 1 (emacs-window-window-width w)))
         (height (max 1 (emacs-window-window-height w)))
         (body-height (if (and emacs-redisplay-paint-mode-line-p (> height 1))
                          (1- height)
                        height))
         (window-start (or (emacs-window-window-start w) 1))
         (start (max 0 (1- window-start)))
         (point (and w (emacs-window-window-point w)))
         (state (emacs-redisplay-core--render-state
                 buffer width height window-start point))
         (old (emacs-redisplay-core--get-matrix handle w))
         (text nil)
         (rows (make-vector height ""))
         (pos start)
         (r 0))
    (if (and old
             (emacs-redisplay-core--render-state-cacheable-p state)
             (emacs-redisplay-core--same-render-state-p
              (emacs-redisplay-core--matrix-state old) state))
        old
      (setq text (emacs-redisplay-core--buffer-string buffer))
    (unless (= (length text) 0)
      (while (< r body-height)
        (let* ((end (emacs-redisplay-core--line-end text pos))
               (line (if (<= pos (length text))
                         (substring text pos end)
                       "")))
          (aset rows r (emacs-redisplay-core--fit line width))
          (setq pos (if (< end (length text)) (1+ end) end))
          (setq r (1+ r)))))
    (when (< r height)
      (aset rows r (emacs-redisplay-core--mode-line buffer width))
      (setq r (1+ r)))
    (while (< r height)
      (aset rows r (make-string width ?\s))
      (setq r (1+ r)))
      (emacs-redisplay-core--put-matrix
       handle w
       (emacs-redisplay-core--make-matrix
        w width height rows
        (emacs-redisplay-core--cursor-for text window-start
                                          point width body-height)
        (emacs-redisplay-core--dirty-rows old rows height)
        state)))))

;;;###autoload
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
(defun emacs-redisplay-flush-frame (handle frame)
  "Flush HANDLE's cached rows to FRAME through the TUI backend."
  (emacs-redisplay-core--check-handle handle)
  (let ((backend (emacs-redisplay-handle-backend handle))
        (count 0))
    (when backend
      (dolist (entry (emacs-redisplay-handle-window-cache handle))
        (let* ((matrix (cdr entry))
               (window (emacs-redisplay-glyph-matrix-window matrix))
               (edges (emacs-window-window-edges window))
               (left (nth 0 edges))
               (top (nth 1 edges))
               (rows (emacs-redisplay-glyph-matrix-rows matrix))
               (dirty (emacs-redisplay-glyph-matrix-dirty-rows matrix))
               (width (emacs-redisplay-glyph-matrix-width matrix))
               (height (emacs-redisplay-glyph-matrix-height matrix))
               (r 0))
          (while (< r height)
            (when (and dirty (aref dirty r))
              (let ((text (emacs-redisplay-core--pad-row
                           (aref rows r) width)))
                (emacs-tui-backend-canvas-draw-text
                 backend frame (+ top r) left text nil)
                (aset dirty r nil)
                (setq count (1+ count))))
            (setq r (1+ r)))))
      (emacs-tui-backend-canvas-flush backend frame))
    count))

;;;###autoload
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
      (when (and window (= (or (emacs-redisplay-core--buffer-size buffer) -1) 0))
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
            (emacs-redisplay-core--render-state
             buffer width height window-start point)))))
      painted)))

(defun emacs-redisplay-core-repaint (handle frame)
  "Fast repaint for the selected TUI window under the lightweight core.
This path is used by the standalone NeLisp event loop after key input.
It updates only the selected window's lightweight matrix and flushes
dirty rows, keeping the full redisplay engine lazy while avoiding a
whole-frame rebuild on every input event."
  (emacs-redisplay-core--check-handle handle)
  (let ((window (and (fboundp 'emacs-window-selected-window)
                     (emacs-window-selected-window))))
    (when window
      (let* ((old (emacs-redisplay-core--get-matrix handle window))
             (matrix (emacs-redisplay-redisplay-window handle window)))
        (unless (eq matrix old)
          (emacs-redisplay-flush-frame handle frame)))
      (emacs-redisplay-core--set-cursor-if-changed handle frame window)
      t)))

(defun emacs-redisplay-core-repaint-current-line (handle frame &optional hint)
  "Repaint only the selected window's current display row.
This is the event-loop path for simple printable self-insert commands.
It avoids rebuilding the whole selected-window matrix and emits a single
cursor-addressed row write when the current row changed."
  (emacs-redisplay-core--check-handle handle)
  (let* ((window (and (fboundp 'emacs-window-selected-window)
                      (emacs-window-selected-window)))
         (backend (emacs-redisplay-handle-backend handle))
         (buffer (and window (emacs-window-window-buffer window)))
         (edges (and window (emacs-window-window-edges window))))
    (when (and window edges)
      (let* ((width (max 1 (emacs-window-window-width window)))
             (height (max 1 (emacs-window-window-height window)))
             (body-height (if (and emacs-redisplay-paint-mode-line-p
                                   (> height 1))
                              (1- height)
                            height))
             (window-start (or (emacs-window-window-start window) 1))
             (point (or (emacs-window-window-point window) 1)))
        (if (and (emacs-redisplay-core--insert-hint-p hint)
                 (emacs-redisplay-core--apply-insert-hint
                  handle frame window backend edges width height body-height
                  hint))
            t
          (let* ((text (emacs-redisplay-core--buffer-string buffer))
                 (row-info (emacs-redisplay-core--row-at-point
                            text window-start point width body-height)))
            (if (not row-info)
                (emacs-redisplay-core-repaint handle frame)
              (let* ((row (nth 0 row-info))
                     (col (nth 1 row-info))
                     (line (substring text (nth 2 row-info) (nth 3 row-info)))
                     (padded (emacs-redisplay-core--pad-row line width))
                     (matrix (emacs-redisplay-core--ensure-matrix
                              handle window width height))
                     (rows (emacs-redisplay-glyph-matrix-rows matrix))
                     (abs-row (+ (nth 1 edges) row))
                     (abs-col (nth 0 edges))
                     (old-cursor (emacs-redisplay-glyph-matrix-cursor matrix))
                     (changed (not (and rows
                                        (< row (length rows))
                                        (string= (aref rows row) line)))))
                (when changed
                  (emacs-redisplay-core--direct-draw-row-and-cursor
                   backend frame abs-row abs-col padded abs-row (+ abs-col col))
                  (when (and rows (< row (length rows)))
                    (aset rows row line)))
                (emacs-redisplay-core--set matrix :cursor (cons row col))
                (when (and (not changed)
                           (not (equal old-cursor (cons row col))))
                  (emacs-redisplay-core--direct-cursor-if-changed
                   frame abs-row (+ abs-col col)))
                t))))))))

(defun emacs-redisplay-redraw-display (handle &optional frame)
  "Render and optionally flush HANDLE."
  (emacs-redisplay-redisplay handle frame)
  (when frame
    (emacs-redisplay-flush-frame handle frame)))

(defun emacs-redisplay-force-mode-line-update (&rest _args)
  "Compatibility no-op for the lightweight redisplay core."
  t)

(provide 'emacs-redisplay-core)

;;; emacs-redisplay-core.el ends here
