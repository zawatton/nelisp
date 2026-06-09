;;; emacs-window.el --- Emacs C window.c port on top of nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 1 module 2/6 per nelisp-emacs Doc 01 (LOCKED v2).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-window-' so loading inside a host Emacs does NOT
;; shadow `selected-window', `split-window', `window-list', etc.
;;
;; Foundation contract:
;;   - `nelisp-emacs-compat' (T39 SHIPPED) provides the buffer struct
;;     (`nelisp-ec-buffer'), point/marker primitives.  Window tracks a
;;     `nelisp-ec-buffer' as its currently-displayed buffer.
;;   - `emacs-buffer' (Phase 1 module 1/6) is NOT required by this
;;     module; window-buffer references the underlying `nelisp-ec-buffer'
;;     directly.  This keeps emacs-window orthogonal to buffer-local
;;     extensions.
;;
;; Architecture:
;;   - A *window* is a node in the window tree.  Each node is either:
;;       - a *leaf* window (displays a buffer), or
;;       - a *split* window (an internal node with N child windows
;;         arranged horizontally [side-by-side] or vertically [stacked]).
;;   - The root of the tree is `emacs-window--root'.  Phase 1 has a
;;     single implicit "frame".  Phase 2 (`emacs-frame.el') will
;;     promote root to per-frame.
;;   - `emacs-window--selected' is the currently selected leaf window.
;;
;;   Side-table state lives entirely in the window struct itself; we do
;;   NOT extend `nelisp-ec-buffer' (= consistent with emacs-buffer.el).
;;
;; API surface (~28 public APIs across 5 categories):
;;
;;   A. window query  (10 APIs)
;;      selected-window / get-window / window-buffer / window-frame
;;      window-list / window-list-1 / next-window / previous-window
;;      get-buffer-window / get-buffer-window-list
;;      windowp
;;
;;   B. window split / delete  (8 APIs)
;;      split-window / split-window-vertically / split-window-horizontally
;;      delete-window / delete-other-windows / delete-windows-on
;;      one-window-p / balance-windows
;;
;;   C. window size / position  (6 APIs)
;;      window-width / window-height
;;      window-pixel-width / window-pixel-height
;;      window-start / window-end / window-point
;;      window-edges / window-resizable
;;
;;   D. window-local config  (6 APIs)
;;      set-window-buffer / set-window-point / set-window-start
;;      window-parameter / set-window-parameter
;;      window-configuration-p / current-window-configuration
;;      set-window-configuration
;;
;;   E. window selection  (3 APIs)
;;      select-window / save-selected-window (macro)
;;      with-selected-window (macro)
;;
;;   F. newer window helpers  (8 APIs, Phase 1 §4.3)
;;      split-window-below / split-window-right (Emacs 24+ aliases)
;;      other-window (cycle by N steps)
;;      window-live-p / window-valid-p (predicates)
;;      enlarge-window / shrink-window (resize)
;;      frame-selected-window (Phase 1 = selected-window)
;;
;; Non-goals (deferred per task spec):
;;   - frame integration               (= emacs-frame.el, Phase 1 mod 3/6)
;;   - minibuffer windows               (= emacs-minibuffer.el, mod 4/6)
;;   - redisplay / window-text-pixel-size accuracy (= Phase 11)
;;   - decorations: scroll-bar, fringe, header-line (= Phase 11)
;;   - dedicated windows + display-buffer/pop-to-buffer hooks (Phase 1.5)

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)

;;; Errors

(define-error 'emacs-window-error  "emacs-window error")
(define-error 'emacs-window-deleted "Window is deleted" 'emacs-window-error)
(define-error 'emacs-window-only    "Cannot delete sole window of frame"
  'emacs-window-error)
(define-error 'emacs-window-too-small
  "Window would be too small after split/resize" 'emacs-window-error)

;;; Tunables

(defconst emacs-window--default-cols  80
  "Default total-cols for the implicit root window.")
(defconst emacs-window--default-lines 24
  "Default total-lines for the implicit root window.")
(defconst emacs-window--min-cols  2
  "Minimum allowed window total-cols after split/resize.")
(defconst emacs-window--min-lines 1
  "Minimum allowed window total-lines after split/resize.")
(defconst emacs-window--pixel-col-px  8
  "Pseudo pixel-per-col multiplier (`window-pixel-width').")
(defconst emacs-window--pixel-line-px 16
  "Pseudo pixel-per-line multiplier (`window-pixel-height').")

;;; Window struct

(cl-defstruct (emacs-window
               (:constructor emacs-window--make)
               (:copier      emacs-window--copy-shallow)
               (:predicate   emacs-window-p))
  "A node in the window tree.

Slots:
- ID            : monotonically increasing integer; useful for debugging.
- LEAF-P        : t = displays a buffer; nil = internal split node.
- BUFFER        : `nelisp-ec-buffer' shown in this leaf, or nil if split.
- POINT         : position cached for this leaf (an integer).
- START         : window-start position cached for this leaf.
- TOTAL-COLS    : total width in columns of this window.
- TOTAL-LINES   : total height in lines of this window.
- PARENT        : parent split node, or nil if root.
- DIRECTION     : for split nodes, the symbol `vertical' (children stacked
                  top-to-bottom) or `horizontal' (children side-by-side).
                  nil for leaves.
- CHILDREN      : for split nodes, an ordered list of child windows.  nil
                  for leaves.
- PARAMETERS    : alist of (KEY . VALUE) for `window-parameter'.
- DELETED-P     : t once window has been removed from its tree."
  (id           0)
  (leaf-p       t)
  (buffer       nil)
  (point        1)
  (start        1)
  (total-cols   emacs-window--default-cols)
  (total-lines  emacs-window--default-lines)
  (parent       nil)
  (direction    nil)
  (children     nil)
  (parameters   nil)
  (deleted-p    nil))

;;; Module state

(defvar emacs-window--id-counter 0
  "Monotonically increasing window-id counter.")

(defvar emacs-window--root nil
  "Root of the window tree for the implicit Phase 1 frame.")

(defvar emacs-window--selected nil
  "Currently selected window (a leaf).")

(defvar emacs-window--frame 'emacs-window--default-frame
  "Pseudo frame value returned by `window-frame' (Phase 1 placeholder).")

;;; Init / fresh-world helpers

(defun emacs-window--next-id ()
  (cl-incf emacs-window--id-counter))

(defun emacs-window--ensure-root ()
  "Ensure the implicit root window exists and is selected.

Returns the (single) leaf window of a freshly created tree, or the
existing selected window if a tree is already in place."
  (unless (and emacs-window--root
               (emacs-window-p emacs-window--root)
               (not (emacs-window-deleted-p emacs-window--root)))
    (let ((w (emacs-window--make
              :id          (emacs-window--next-id)
              :leaf-p      t
              :buffer      nil
              :total-cols  emacs-window--default-cols
              :total-lines emacs-window--default-lines)))
      (setq emacs-window--root     w
            emacs-window--selected w)))
  emacs-window--selected)

(defun emacs-window-reset ()
  "Tear down the tree so the next API call starts fresh.

Test-only convenience; not part of the public Emacs API surface."
  (setq emacs-window--id-counter 0
        emacs-window--root       nil
        emacs-window--selected   nil)
  nil)

;;; Internal sanity helpers

(defsubst emacs-window--check-live (win)
  (unless (emacs-window-p win)
    (signal 'wrong-type-argument (list 'emacs-window-p win)))
  (when (emacs-window-deleted-p win)
    (signal 'emacs-window-deleted (list win)))
  win)

(defsubst emacs-window--check-leaf (win)
  (emacs-window--check-live win)
  (unless (emacs-window-leaf-p win)
    (signal 'emacs-window-error
            (list "Operation requires a leaf window" win)))
  win)

(defun emacs-window--leaves-of (win)
  "Return the list of leaf descendants of WIN, in tree order."
  (if (emacs-window-leaf-p win)
      (list win)
    (apply #'append
           (mapcar #'emacs-window--leaves-of
                   (emacs-window-children win)))))

(defun emacs-window--all-leaves ()
  (when emacs-window--root
    (emacs-window--leaves-of emacs-window--root)))

;;; A. window query

(defun emacs-window-windowp (object)
  "Return t if OBJECT is a (live) window, nil otherwise.
Mirrors Emacs `windowp'."
  (and (emacs-window-p object)
       (not (emacs-window-deleted-p object))))

(defun emacs-window-selected-window ()
  "Return the currently selected window."
  (emacs-window--ensure-root)
  emacs-window--selected)

(defun emacs-window-get-window (&optional window)
  "Return WINDOW or the selected window if nil."
  (or window (emacs-window-selected-window)))

(defun emacs-window-window-buffer (&optional window)
  "Return the buffer displayed by WINDOW (selected window if nil)."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-leaf w)
    (emacs-window-buffer w)))

(defun emacs-window-window-frame (&optional window)
  "Return the frame containing WINDOW.

Phase 1 returns the single implicit frame value (a constant symbol).
Phase 2 (`emacs-frame.el') will promote this to a real frame object."
  (emacs-window--check-live (emacs-window-get-window window))
  emacs-window--frame)

(defun emacs-window-window-list (&optional frame _minibuf window)
  "Return the live windows on FRAME starting from WINDOW.

In this Phase-1 implementation FRAME is ignored (single implicit frame),
MINIBUF is ignored (no minibuffer yet), and the result starts at WINDOW
if non-nil and rotates to keep tree order."
  (ignore frame)
  (emacs-window--ensure-root)
  (let* ((leaves (emacs-window--all-leaves))
         (start  (or window emacs-window--selected)))
    (if (or (null start) (not (memq start leaves)))
        leaves
      (let ((tail (memq start leaves)))
        (append tail (cl-subseq leaves 0 (- (length leaves) (length tail))))))))

(defun emacs-window-window-list-1 (&optional window _minibuf _all-frames)
  "Like `emacs-window-window-list' but always starts at WINDOW or selected.
MINIBUF and ALL-FRAMES are accepted for signature compatibility and
otherwise ignored in Phase 1."
  (emacs-window-window-list nil nil window))

(defun emacs-window-next-window (&optional window _minibuf _all-frames)
  "Return the next window after WINDOW in tree order.
Wraps around at the end."
  (emacs-window--ensure-root)
  (let* ((leaves (emacs-window--all-leaves))
         (cur    (or window emacs-window--selected))
         (tail   (memq cur leaves)))
    (cond
     ((null leaves) nil)
     ((null tail)   (car leaves))
     ((cdr tail)    (cadr tail))
     (t             (car leaves)))))

(defun emacs-window-previous-window (&optional window _minibuf _all-frames)
  "Return the previous window before WINDOW in tree order.
Wraps around at the beginning."
  (emacs-window--ensure-root)
  (let* ((leaves (emacs-window--all-leaves))
         (cur    (or window emacs-window--selected))
         (idx    (cl-position cur leaves)))
    (cond
     ((null leaves) nil)
     ((null idx)    (car (last leaves)))
     ((zerop idx)   (car (last leaves)))
     (t             (nth (1- idx) leaves)))))

(defun emacs-window-get-buffer-window (buffer-or-name &optional _all-frames)
  "Return the first window currently displaying BUFFER-OR-NAME, or nil."
  (let ((bufs (cond
               ((nelisp-ec-buffer-p buffer-or-name)
                (list buffer-or-name))
               ((stringp buffer-or-name)
                (let ((b (cdr (assoc buffer-or-name nelisp-ec--buffers))))
                  (and b (list b))))
               (t nil))))
    (cl-loop for w in (emacs-window--all-leaves)
             when (memq (emacs-window-buffer w) bufs)
             return w)))

(defun emacs-window-get-buffer-window-list (buffer-or-name
                                            &optional _minibuf _all-frames)
  "Return the list of all windows displaying BUFFER-OR-NAME."
  (let ((bufs (if (nelisp-ec-buffer-p buffer-or-name)
                  (list buffer-or-name)
                (cl-remove-if-not
                 (lambda (b)
                   (and (nelisp-ec-buffer-p b)
                        (equal buffer-or-name
                               (nelisp-ec-buffer-name b))))
                 nelisp-ec--buffers))))
    (cl-loop for w in (emacs-window--all-leaves)
             when (memq (emacs-window-buffer w) bufs)
             collect w)))

;;; B. split / delete

(defun emacs-window--make-leaf (buffer total-cols total-lines)
  (emacs-window--make
   :id          (emacs-window--next-id)
   :leaf-p      t
   :buffer      buffer
   :total-cols  total-cols
   :total-lines total-lines))

(defun emacs-window--split-sizes (total size new-side)
  "Compute (NEW-SIZE . OLD-SIZE) for splitting a window of TOTAL into two.

If SIZE is non-nil it specifies the size of the new window (positive =
new window above/left of old, when NEW-SIDE is `above'; negative is not
yet supported and treated as |size|).

If SIZE is nil, splits in half (floor for new, ceil for old).

Errors with `emacs-window-too-small' if either side would fall under the
configured minimum."
  (let* ((min (if (eq new-side 'above)
                  emacs-window--min-lines
                emacs-window--min-cols))
         (new-sz (if size (abs size) (/ total 2)))
         (old-sz (- total new-sz)))
    (when (or (< new-sz min) (< old-sz min))
      (signal 'emacs-window-too-small
              (list :total total :new new-sz :old old-sz)))
    (cons new-sz old-sz)))

(defun emacs-window-split-window (&optional window size side)
  "Split WINDOW into two and return the new window.

SIDE is one of `below' (default), `above', `right', `left'.  For the
top/bottom pair the split is *vertical* (stacked); for left/right it is
*horizontal* (side-by-side).  This matches Emacs.

WINDOW defaults to the selected window.  SIZE, if non-nil, specifies the
size of the *new* window."
  (let* ((win  (or window (emacs-window-selected-window)))
         (side (or side 'below)))
    (emacs-window--check-leaf win)
    (let* ((vertical (memq side '(below above)))
           (total    (if vertical
                         (emacs-window-total-lines win)
                       (emacs-window-total-cols win)))
           (sizes    (emacs-window--split-sizes total size
                                                (if vertical 'above 'left)))
           (new-sz   (car sizes))
           (old-sz   (cdr sizes))
           (new-leaf (emacs-window--make-leaf
                      (emacs-window-buffer win)
                      (if vertical (emacs-window-total-cols win) new-sz)
                      (if vertical new-sz (emacs-window-total-lines win)))))
      ;; resize the original
      (if vertical
          (setf (emacs-window-total-lines win) old-sz)
        (setf (emacs-window-total-cols win) old-sz))
      ;; If WIN already lives in a split with the same direction, just
      ;; insert NEW-LEAF as a sibling.  Otherwise wrap WIN inside a new
      ;; split node.
      (let ((parent (emacs-window-parent win))
            (dir    (if vertical 'vertical 'horizontal)))
        (if (and parent (eq (emacs-window-direction parent) dir))
            (emacs-window--insert-sibling parent win new-leaf side)
          (emacs-window--wrap-in-split win new-leaf dir side)))
      ;; Selected window stays on WIN unless caller selects new-leaf.
      new-leaf)))

(defun emacs-window--insert-sibling (parent win new-leaf side)
  (let* ((children (emacs-window-children parent))
         (idx      (cl-position win children)))
    (unless idx
      (signal 'emacs-window-error (list "win not a child of parent" win)))
    (let ((new-children
           (if (memq side '(above left))
               (append (cl-subseq children 0 idx)
                       (list new-leaf)
                       (cl-subseq children idx))
             (append (cl-subseq children 0 (1+ idx))
                     (list new-leaf)
                     (cl-subseq children (1+ idx))))))
      (setf (emacs-window-children parent) new-children
            (emacs-window-parent new-leaf) parent))))

(defun emacs-window--wrap-in-split (win new-leaf dir side)
  (let* ((old-parent (emacs-window-parent win))
         (split (emacs-window--make
                 :id          (emacs-window--next-id)
                 :leaf-p      nil
                 :buffer      nil
                 :total-cols  (if (eq dir 'horizontal)
                                  (+ (emacs-window-total-cols win)
                                     (emacs-window-total-cols new-leaf))
                                (emacs-window-total-cols win))
                 :total-lines (if (eq dir 'vertical)
                                  (+ (emacs-window-total-lines win)
                                     (emacs-window-total-lines new-leaf))
                                (emacs-window-total-lines win))
                 :parent      old-parent
                 :direction   dir
                 :children    (if (memq side '(above left))
                                  (list new-leaf win)
                                (list win new-leaf)))))
    ;; reparent
    (setf (emacs-window-parent win)      split
          (emacs-window-parent new-leaf) split)
    ;; substitute in old-parent or replace root
    (if old-parent
        (setf (emacs-window-children old-parent)
              (mapcar (lambda (c) (if (eq c win) split c))
                      (emacs-window-children old-parent)))
      (setq emacs-window--root split))))

(defun emacs-window-split-window-vertically (&optional size)
  "Split the selected window into two stacked windows."
  (emacs-window-split-window nil size 'below))

(defun emacs-window-split-window-horizontally (&optional size)
  "Split the selected window into two side-by-side windows."
  (emacs-window-split-window nil size 'right))

(defun emacs-window-one-window-p (&optional _no-mini _all-frames)
  "Return t if there is exactly one (live) window in the tree."
  (= 1 (length (emacs-window--all-leaves))))

(defun emacs-window-delete-window (&optional window)
  "Delete WINDOW (default = selected window).

Errors with `emacs-window-only' if WINDOW is the sole window."
  (let ((win (or window (emacs-window-selected-window))))
    (emacs-window--check-leaf win)
    (when (emacs-window-one-window-p)
      (signal 'emacs-window-only (list win)))
    (let* ((parent   (emacs-window-parent win))
           (siblings (cl-remove win (emacs-window-children parent))))
      ;; Give WIN's space back to the first surviving sibling.
      (let ((heir (car siblings)))
        (cond
         ((eq (emacs-window-direction parent) 'vertical)
          (setf (emacs-window-total-lines heir)
                (+ (emacs-window-total-lines heir)
                   (emacs-window-total-lines win))))
         (t
          (setf (emacs-window-total-cols heir)
                (+ (emacs-window-total-cols heir)
                   (emacs-window-total-cols win))))))
      (setf (emacs-window-children parent) siblings)
      (setf (emacs-window-deleted-p win) t)
      ;; If parent now has only one child, splice the parent out.
      (when (= 1 (length siblings))
        (emacs-window--splice-singleton parent))
      ;; If the deleted window was the selected one, pick a new selection.
      (when (eq win emacs-window--selected)
        (setq emacs-window--selected (car (emacs-window--all-leaves))))
      nil)))

(defun emacs-window--splice-singleton (split)
  "If SPLIT has only one child, replace SPLIT in its parent with that child."
  (let ((only (car (emacs-window-children split)))
        (gp   (emacs-window-parent split)))
    (setf (emacs-window-parent only) gp)
    ;; carry SPLIT's outer dimensions into the child
    (setf (emacs-window-total-cols  only) (emacs-window-total-cols  split)
          (emacs-window-total-lines only) (emacs-window-total-lines split))
    (if gp
        (setf (emacs-window-children gp)
              (mapcar (lambda (c) (if (eq c split) only c))
                      (emacs-window-children gp)))
      (setq emacs-window--root only))
    (setf (emacs-window-deleted-p split) t)))

(defun emacs-window-delete-other-windows (&optional window)
  "Delete every window in the tree except WINDOW (default = selected).

The surviving window inherits the full root dimensions."
  (let* ((keep (or window (emacs-window-selected-window))))
    (emacs-window--check-leaf keep)
    (let ((root-cols  (emacs-window-total-cols  emacs-window--root))
          (root-lines (emacs-window-total-lines emacs-window--root)))
      (dolist (w (emacs-window--all-leaves))
        (unless (eq w keep)
          (setf (emacs-window-deleted-p w) t)))
      (setf (emacs-window-parent      keep) nil
            (emacs-window-total-cols  keep) root-cols
            (emacs-window-total-lines keep) root-lines)
      (setq emacs-window--root     keep
            emacs-window--selected keep)
      nil)))

(defun emacs-window-delete-windows-on (buffer-or-name &optional _frame)
  "Delete all windows currently displaying BUFFER-OR-NAME.

If deleting a window would leave only one in the tree, the buffer is
cleared from that window instead (= matches Emacs semantics)."
  (let ((wins (emacs-window-get-buffer-window-list buffer-or-name)))
    (dolist (w wins)
      (cond
       ((emacs-window-deleted-p w))                ; already gone
       ((emacs-window-one-window-p)
        (setf (emacs-window-buffer w) nil))
       (t (emacs-window-delete-window w))))
    nil))

(defun emacs-window-balance-windows (&optional window)
  "Equalize the sizes of sibling windows under each split node.

Walks the tree top-down from the root (or, if WINDOW is non-nil, from
the smallest enclosing split node containing WINDOW), redistributing
each split's total dimension equally among its children."
  (emacs-window--ensure-root)
  (let ((root (cond
               ((null window) emacs-window--root)
               ((emacs-window-leaf-p window)
                (or (emacs-window-parent window) emacs-window--root))
               (t window))))
    (emacs-window--balance-node root)))

(defun emacs-window--balance-node (node)
  (when (and node (not (emacs-window-leaf-p node)))
    (let* ((children (emacs-window-children node))
           (n        (length children))
           (vertical (eq (emacs-window-direction node) 'vertical))
           (total    (if vertical
                         (emacs-window-total-lines node)
                       (emacs-window-total-cols node)))
           (each     (/ total n))
           (rest     (- total (* each n))))
      (cl-loop for c in children
               for i from 0 do
               (let ((sz (+ each (if (= i 0) rest 0))))
                 (if vertical
                     (setf (emacs-window-total-lines c) sz)
                   (setf (emacs-window-total-cols c) sz))
                 (emacs-window--balance-node c))))))

;;; C. size / position

(defun emacs-window-window-width (&optional window)
  "Return the total width in columns of WINDOW (selected if nil)."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-live w)
    (emacs-window-total-cols w)))

(defun emacs-window-window-height (&optional window)
  "Return the total height in lines of WINDOW (selected if nil)."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-live w)
    (emacs-window-total-lines w)))

(defun emacs-window-window-pixel-width (&optional window)
  "Return a pseudo pixel width derived from `window-width'."
  (* (emacs-window-window-width window) emacs-window--pixel-col-px))

(defun emacs-window-window-pixel-height (&optional window)
  "Return a pseudo pixel height derived from `window-height'."
  (* (emacs-window-window-height window) emacs-window--pixel-line-px))

(defun emacs-window-window-start (&optional window)
  "Return cached window-start for WINDOW (selected if nil)."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-leaf w)
    (emacs-window-start w)))

(defun emacs-window-window-end (&optional window _update)
  "Return a coarse window-end approximation for WINDOW.

Phase 1: returns (start + width*height), clamped to buffer-size when the
buffer is non-nil.  Real geometry-aware end requires Phase 11 redisplay."
  (let* ((w     (emacs-window-get-window window))
         (start (emacs-window-window-start w))
         (cols  (emacs-window-window-width  w))
         (lines (emacs-window-window-height w))
         (cap   (* cols lines))
         (end   (+ start cap))
         (buf   (emacs-window-buffer w)))
    (if (and buf (nelisp-ec-buffer-p buf))
        (min end (1+ (nelisp-ec-buffer-size buf)))
      end)))

(defun emacs-window-window-point (&optional window)
  "Return cached window-point for WINDOW (selected if nil)."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-leaf w)
    (emacs-window-point w)))

(defun emacs-window-window-edges (&optional window
                                            _body
                                            _absolute
                                            _pixelwise)
  "Return (LEFT TOP RIGHT BOTTOM) edges of WINDOW in column units.

Edges are computed by walking the tree from the root, accumulating
sibling sizes.  BODY/ABSOLUTE/PIXELWISE are accepted for signature
compatibility and ignored in Phase 1."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-live w)
    (emacs-window--edges-of w)))

(defun emacs-window--edges-of (win)
  (let ((left 0) (top 0)
        (cur win))
    (while (emacs-window-parent cur)
      (let* ((parent   (emacs-window-parent cur))
             (vertical (eq (emacs-window-direction parent) 'vertical))
             (done     nil))
        (dolist (sib (emacs-window-children parent))
          (unless done
            (cond
             ((eq sib cur) (setq done t))
             (vertical (cl-incf top  (emacs-window-total-lines sib)))
             (t        (cl-incf left (emacs-window-total-cols  sib))))))
        (setq cur parent)))
    (list left top
          (+ left (emacs-window-total-cols  win))
          (+ top  (emacs-window-total-lines win)))))

(defun emacs-window-window-resizable (window delta &optional horizontal
                                              _ignore _pixelwise)
  "Return DELTA (clamped) by which WINDOW can be resized along axis.

If HORIZONTAL is non-nil, axis = columns (width); else = lines (height).
Phase 1 implementation: simply checks the requested delta does not push
WINDOW (or its first sibling-donor) below its configured minimum."
  (let* ((w   (emacs-window-get-window window))
         (cur (if horizontal
                  (emacs-window-total-cols  w)
                (emacs-window-total-lines w)))
         (min (if horizontal
                  emacs-window--min-cols
                emacs-window--min-lines)))
    (cond
     ((>= delta 0) delta)
     ((>= (+ cur delta) min) delta)
     (t (- min cur)))))

;;; D. window-local config

(defun emacs-window-set-window-buffer (window buffer-or-name &optional
                                              _keep-margins)
  "Set WINDOW to display BUFFER-OR-NAME.

WINDOW may be nil = selected window.  BUFFER-OR-NAME must be a
`nelisp-ec-buffer' for now (string lookup is best-effort by name)."
  (let ((w (emacs-window-get-window window))
        (b (cond
            ((nelisp-ec-buffer-p buffer-or-name) buffer-or-name)
            ((stringp buffer-or-name)
             (or (cdr (assoc buffer-or-name nelisp-ec--buffers))
                 (signal 'emacs-window-error
                         (list "no such buffer" buffer-or-name))))
            (t (signal 'wrong-type-argument
                       (list 'nelisp-ec-buffer-p buffer-or-name))))))
    (emacs-window--check-leaf w)
    (nelisp-ec--check-live b)
    (setf (emacs-window-buffer w) b
          (emacs-window-point  w) (nelisp-ec-buffer-point b)
          (emacs-window-start  w) 1)
    nil))

(defun emacs-window-set-window-point (window pos)
  "Set the cached point of WINDOW to POS."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-leaf w)
    (setf (emacs-window-point w) pos)))

(defun emacs-window-set-window-start (window pos &optional _noforce)
  "Set the cached window-start of WINDOW to POS."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-leaf w)
    (setf (emacs-window-start w) pos)))

(defun emacs-window-window-parameter (window parameter)
  "Return the value of PARAMETER for WINDOW, or nil."
  (let ((w (emacs-window-get-window window)))
    (emacs-window--check-live w)
    (alist-get parameter (emacs-window-parameters w))))

(defun emacs-window-set-window-parameter (window parameter value)
  "Set PARAMETER to VALUE for WINDOW.  Returns VALUE."
  (let* ((w     (emacs-window-get-window window))
         (alist (emacs-window-parameters w)))
    (emacs-window--check-live w)
    (setf (alist-get parameter alist) value)
    (setf (emacs-window-parameters w) alist)
    value))

;;; D'. window-configuration

(cl-defstruct (emacs-window-configuration
               (:constructor emacs-window-configuration--make)
               (:copier      nil)
               (:predicate   emacs-window-configuration-p))
  "Snapshot of the window tree.

Slots:
- ROOT      : a fully detached deep copy of the window tree at snapshot.
- SELECTED  : the leaf-id within that copy that was selected."
  root selected)

(defun emacs-window--copy-tree (node parent)
  "Deep copy NODE; its parent in the copy will be PARENT."
  (let ((copy (emacs-window--copy-shallow node)))
    (setf (emacs-window-parent copy) parent)
    (when (not (emacs-window-leaf-p copy))
      (setf (emacs-window-children copy)
            (mapcar (lambda (c) (emacs-window--copy-tree c copy))
                    (emacs-window-children node))))
    copy))

(defun emacs-window--reparent (node parent)
  (setf (emacs-window-parent node) parent)
  (unless (emacs-window-leaf-p node)
    (dolist (c (emacs-window-children node))
      (emacs-window--reparent c node))))

(defun emacs-window--find-by-id (root id)
  (cond
   ((null root) nil)
   ((eq (emacs-window-id root) id) root)
   ((emacs-window-leaf-p root) nil)
   (t (cl-some (lambda (c) (emacs-window--find-by-id c id))
               (emacs-window-children root)))))

(defun emacs-window-current-window-configuration (&optional _frame)
  "Return a deep copy snapshot of the current window tree."
  (emacs-window--ensure-root)
  (emacs-window-configuration--make
   :root     (emacs-window--copy-tree emacs-window--root nil)
   :selected (emacs-window-id emacs-window--selected)))

(defun emacs-window-set-window-configuration (config)
  "Restore the window tree from CONFIG.
CONFIG must be a value returned by
`emacs-window-current-window-configuration'."
  (unless (emacs-window-configuration-p config)
    (signal 'wrong-type-argument
            (list 'emacs-window-configuration-p config)))
  (let ((root-copy (emacs-window--copy-tree
                    (emacs-window-configuration-root config) nil)))
    (emacs-window--reparent root-copy nil)
    (setq emacs-window--root root-copy)
    (let ((sel (emacs-window--find-by-id
                root-copy
                (emacs-window-configuration-selected config))))
      (setq emacs-window--selected
            (or sel (car (emacs-window--all-leaves)))))
    nil))

;;; User-facing window commands

(defun emacs-window--normalize-prefix-number (value)
  "Return VALUE as a plain integer or nil when VALUE is nil."
  (when value
    (if (integerp value)
        value
      (prefix-numeric-value value))))

;;;###autoload
(defun split-window-below (&optional size)
  "Split the selected window into two stacked windows and return the new window.

SIZE, when non-nil, is the size of the new window."
  (interactive
   (list (emacs-window--normalize-prefix-number current-prefix-arg)))
  (emacs-window-split-window-vertically size))

;;;###autoload
(defun split-window-right (&optional size)
  "Split the selected window side-by-side and return the new window.

SIZE, when non-nil, is the size of the new window."
  (interactive
   (list (emacs-window--normalize-prefix-number current-prefix-arg)))
  (emacs-window-split-window-horizontally size))

(defun emacs-window-other-window-impl (&optional count all-frames)
  "Select the COUNTth next window and return it.

COUNT defaults to 1.  Negative COUNT cycles backward.  ALL-FRAMES is
accepted for API compatibility and ignored in Phase 1."
  (let* ((n (or count 1))
         (target (emacs-window-selected-window)))
    (dotimes (_ (abs n))
      (setq target
            (if (< n 0)
                (emacs-window-previous-window target nil all-frames)
              (emacs-window-next-window target nil all-frames))))
    (when target
      (emacs-window-select-window target))
    target))

;;;###autoload
(defun other-window (&optional n all-frames)
  "Select the Nth next window.

N defaults to 1.  Negative N cycles backward.  ALL-FRAMES is accepted
for API compatibility and ignored in Phase 1."
  (interactive "p")
  (emacs-window-other-window-impl n all-frames))

;;;###autoload
(defun delete-window (&optional window)
  "Delete WINDOW, or the selected window if WINDOW is nil."
  (interactive)
  (emacs-window-delete-window window))

;;;###autoload
(defun delete-other-windows (&optional window)
  "Delete every window except WINDOW, or the selected window if WINDOW is nil."
  (interactive)
  (emacs-window-delete-other-windows window))

;;; E. selection

(defun emacs-window-select-window (window &optional _norecord)
  "Select WINDOW, returning it.  Errors if WINDOW is not a live leaf."
  (emacs-window--check-leaf window)
  (setq emacs-window--selected window)
  window)

(defmacro emacs-window-save-selected-window (&rest body)
  "Run BODY without permanently changing the selected window."
  (declare (indent 0) (debug (body)))
  (let ((saved (gensym "saved-")))
    `(let ((,saved (emacs-window-selected-window)))
       (unwind-protect
           (progn ,@body)
         (when (and (emacs-window-p ,saved)
                    (not (emacs-window-deleted-p ,saved)))
           (setq emacs-window--selected ,saved))))))

(defmacro emacs-window-with-selected-window (window &rest body)
  "Select WINDOW, run BODY, restore previous selection."
  (declare (indent 1) (debug (form body)))
  (let ((win (gensym "win-")))
    `(let ((,win ,window))
       (emacs-window-save-selected-window
         (emacs-window-select-window ,win)
         ,@body))))

;;; F. newer window helpers (Phase 1 §4.3, 8 APIs)
;;
;; Thin layer of widely-used helpers expected by host Emacs 24+ code.
;; The split-window-{below,right} pair matches the Emacs 24 rename of
;; -vertically / -horizontally and keeps the same argument shape.
;; other-window mirrors C-x o semantics: cycle the leaf list by N and
;; select the result.  enlarge/shrink-window are intentionally MVP: they
;; redistribute size with the *immediate* next sibling under a
;; same-direction split parent; cross-tree redistribution lands in
;; Phase 11 once the redisplay engine drives resize.

;;;###autoload
(defun emacs-window-split-window-below (&optional size)
  "Split the selected window into two stacked windows.
Emacs 24+ alias of `emacs-window-split-window-vertically'."
  (emacs-window-split-window nil size 'below))

;;;###autoload
(defun emacs-window-split-window-right (&optional size)
  "Split the selected window into two side-by-side windows.
Emacs 24+ alias of `emacs-window-split-window-horizontally'."
  (emacs-window-split-window nil size 'right))

;;;###autoload
(defun emacs-window-window-live-p (window)
  "Return non-nil iff WINDOW is a live leaf window.
Internal split nodes return nil — only leaf windows that display a
buffer are \"live\" in the Emacs sense."
  (and (emacs-window-p window)
       (emacs-window-leaf-p window)
       (not (emacs-window-deleted-p window))))

;;;###autoload
(defun emacs-window-window-valid-p (window)
  "Return non-nil iff WINDOW is a valid (= non-deleted) window record.
Both leaves and internal split nodes return t until they are deleted."
  (and (emacs-window-p window)
       (not (emacs-window-deleted-p window))))

;;;###autoload
(defun emacs-window-frame-selected-window (&optional _frame)
  "Return the selected window of FRAME.
Phase 1 has a single implicit frame so this delegates to
`emacs-window-selected-window' regardless of FRAME."
  (emacs-window-selected-window))

;;;###autoload
(defun emacs-window-other-window (n &optional _all-frames)
  "Select the window N leaves forward (or backward when N is negative).
Wraps around the live-leaf list.  Returns the newly selected window.
ALL-FRAMES is accepted for ABI compatibility but ignored in Phase 1."
  (emacs-window--ensure-root)
  (let* ((leaves (emacs-window--all-leaves))
         (len    (length leaves)))
    (when (zerop len)
      (signal 'emacs-window-error '("No live windows")))
    (let* ((cur     (or emacs-window--selected (car leaves)))
           (idx     (or (cl-position cur leaves) 0))
           (new-idx (mod (+ idx n) len)))
      (emacs-window-select-window (nth new-idx leaves)))))

(defun emacs-window--resize-sibling-pair (win delta horizontal)
  "Add DELTA to WIN's size on the HORIZONTAL axis, taking from a sibling.
HORIZONTAL non-nil = column adjustment; nil = line adjustment.  WIN's
parent must split in the same direction (= `horizontal' for cols,
`vertical' for lines).  Picks the next sibling if any, else the
previous one."
  (let* ((parent (emacs-window-parent win))
         (want   (if horizontal 'horizontal 'vertical)))
    (unless parent
      (signal 'emacs-window-too-small (list win)))
    (unless (eq (emacs-window-direction parent) want)
      (signal 'emacs-window-too-small (list win)))
    (let* ((sibs (emacs-window-children parent))
           (idx  (cl-position win sibs))
           (next (or (nth (1+ idx) sibs)
                     (and (> idx 0) (nth (1- idx) sibs)))))
      (unless next
        (signal 'emacs-window-too-small (list win)))
      (let* ((win-size  (if horizontal
                            (emacs-window-total-cols win)
                          (emacs-window-total-lines win)))
             (next-size (if horizontal
                            (emacs-window-total-cols next)
                          (emacs-window-total-lines next)))
             (min-size  (if horizontal
                            emacs-window--min-cols
                          emacs-window--min-lines))
             (new-win   (+ win-size delta))
             (new-next  (- next-size delta)))
        (when (or (< new-win min-size) (< new-next min-size))
          (signal 'emacs-window-too-small (list win delta)))
        (if horizontal
            (setf (emacs-window-total-cols win)  new-win
                  (emacs-window-total-cols next) new-next)
          (setf (emacs-window-total-lines win)  new-win
                (emacs-window-total-lines next) new-next))
        delta))))

;;;###autoload
(defun emacs-window-enlarge-window (size &optional horizontal)
  "Enlarge the selected window by SIZE lines (or cols if HORIZONTAL).
Borrows the space from the immediate next sibling in the same-direction
split parent.  Signals `emacs-window-too-small' when the resize would
push either window below the minimum size, or when the parent split
direction doesn't match the requested axis."
  (let ((win (emacs-window-selected-window)))
    (emacs-window--check-leaf win)
    (emacs-window--resize-sibling-pair win size horizontal)
    nil))

;;;###autoload
(defun emacs-window-shrink-window (size &optional horizontal)
  "Shrink the selected window by SIZE lines (or cols if HORIZONTAL).
Inverse of `emacs-window-enlarge-window'.  Same constraints apply."
  (emacs-window-enlarge-window (- size) horizontal))

(defun emacs-window-display-buffer (buffer-or-name &optional _action _frame)
  "Display BUFFER-OR-NAME in a window and return that window.

Minimal `display-buffer' policy for the standalone editor:
- if some live window already shows the buffer, reuse it;
- else if there is more than one live window, reuse a non-selected one;
- else split the selected window below and use the new window.

ACTION and FRAME are accepted for call-compatibility and otherwise ignored.
BUFFER-OR-NAME must resolve to an existing `nelisp-ec' buffer (object, or a
name present in `nelisp-ec--buffers')."
  (ignore _action _frame)
  (emacs-window--ensure-root)
  (let* ((buffer (cond
                  ((nelisp-ec-buffer-p buffer-or-name) buffer-or-name)
                  ((stringp buffer-or-name)
                   (or (cdr (assoc buffer-or-name nelisp-ec--buffers))
                       (signal 'emacs-window-error
                               (list "no such buffer" buffer-or-name))))
                  (t (signal 'wrong-type-argument
                             (list 'nelisp-ec-buffer-p buffer-or-name)))))
         (existing (emacs-window-get-buffer-window buffer)))
    (or existing
        (let* ((selected (emacs-window-selected-window))
               (others (delq selected (emacs-window-window-list)))
               (target (or (car others)
                           (emacs-window-split-window-below))))
          (emacs-window-set-window-buffer target buffer)
          target))))

(defun emacs-window-pop-to-buffer (buffer-or-name &optional action _norecord)
  "Display BUFFER-OR-NAME via `emacs-window-display-buffer' and select its window.
ACTION is forwarded to `emacs-window-display-buffer'; NORECORD is ignored.
Returns the displayed buffer."
  (let ((window (emacs-window-display-buffer buffer-or-name action)))
    (emacs-window-select-window window)
    (emacs-window-buffer window)))

(defun emacs-window-quit-window (&optional kill window)
  "Quit WINDOW (default the selected window): stop displaying its buffer.

If more than one live window exists, delete WINDOW — this closes a popup
such as a `*Help*' window and returns to the editing layout.  Otherwise
switch WINDOW to another live buffer, leaving the quit buffer buried.  With
KILL non-nil, kill the quit buffer afterward.  Returns nil.

This is the `q' complement to `emacs-window-display-buffer' for the
help/completion/popup workflow."
  (emacs-window--ensure-root)
  (let* ((w (or window (emacs-window-selected-window)))
         (buf (emacs-window-buffer w)))
    (if (> (length (emacs-window-window-list)) 1)
        (emacs-window-delete-window w)
      (let ((other (cl-loop for (_name . b) in nelisp-ec--buffers
                            when (not (eq b buf)) return b)))
        (when other
          (emacs-window-set-window-buffer w other)
          (nelisp-ec-set-buffer other))))
    (when (and kill (nelisp-ec-buffer-p buf)
               ;; only kill once it is no longer displayed anywhere
               (not (emacs-window-get-buffer-window buf)))
      (nelisp-ec-kill-buffer buf))
    nil))

(provide 'emacs-window)

;;; emacs-window.el ends here
