;;; emacs-cc-xfaces-1.el --- xfaces C primitive replacements -*- lexical-binding: t; -*-

(defvar emacs-cc-xfaces-1--alternative-font-family-alist nil)
(defvar emacs-cc-xfaces-1--alternative-font-registry-alist nil)
(defvar emacs-cc-xfaces-1--font-selection-order '(:width :height :weight :slant))
(defvar emacs-cc-xfaces-1--suppress-bold-inverse-default-colors nil)
(defvar emacs-cc-xfaces-1--face-table (make-hash-table :test 'eq))

(defun emacs-cc-xfaces-1--color (spec)
  (unless (stringp spec) (signal 'wrong-type-argument (list 'stringp spec)))
  (let ((parsed (color-values-from-color-spec spec))
        (known '(("black" 0 0 0) ("white" 65535 65535 65535)
                 ("red" 65535 0 0) ("green" 0 32896 0)
                 ("blue" 0 0 65535) ("yellow" 65535 65535 0)
                 ("cyan" 0 65535 65535) ("magenta" 65535 0 65535)
                 ("gray" 32896 32896 32896) ("grey" 32896 32896 32896))))
    (or parsed (cdr (assoc-string (downcase spec) known t))
        (and (string-match "\\`gray\\([0-9]+\\)\\'" (downcase spec))
             (let ((n (string-to-number (match-string 1 spec))))
               (when (<= 0 n 100) (let ((v (round (* 65535 (/ n 100.0))))) (list v v v)))))
        (signal 'error (if (stringp spec) (list "Invalid color" spec) (list "Invalid color"))))))

(unless (fboundp 'clear-face-cache)
  (defun clear-face-cache (&optional thoroughly)
    "Clear face caches on all frames. Optional THOROUGHLY non-nil means try to free unused fonts, too."
    (ignore thoroughly) nil))
(unless (fboundp 'color-distance)
  (defun color-distance (color1 color2 &optional frame metric)
    "Return an integer distance between COLOR1 and COLOR2 on FRAME."
    (ignore frame)
    (let* ((rgb1 (if (stringp color1) (emacs-cc-xfaces-1--color color1) color1))
           (rgb2 (if (stringp color2) (emacs-cc-xfaces-1--color color2) color2)))
      (unless (and (listp rgb1) (= (length rgb1) 3)
                   (listp rgb2) (= (length rgb2) 3)
                   (let ((valid t))
                     (dolist (component (append rgb1 rgb2))
                       (unless (and (integerp component)
                                    (<= 0 component 65535))
                         (setq valid nil)))
                     valid))
        (signal 'error (list "Invalid color")))
      (if metric (funcall metric rgb1 rgb2)
        (let* ((r (- (nth 0 rgb1) (nth 0 rgb2)))
               (g (- (nth 1 rgb1) (nth 1 rgb2)))
               (b (- (nth 2 rgb1) (nth 2 rgb2)))
               (r-mean (/ (+ (nth 0 rgb1) (nth 0 rgb2)) 2)))
          (ash (+ (ash (* (+ (* 2 65536) r-mean) r r) -16)
                  (* 4 g g)
                  (ash (* (+ (* 2 65536) 65535 (- r-mean)) b b) -16))
               -16))))))
(unless (fboundp 'color-gray-p)
  (defun color-gray-p (color &optional frame)
    "Return non-nil if COLOR is a shade of gray (or white or black)."
    (ignore frame) (let ((rgb (emacs-cc-xfaces-1--color color))) (= (car rgb) (cadr rgb) (caddr rgb)))))
(unless (fboundp 'color-supported-p)
  (defun color-supported-p (color &optional frame background-p)
    "Return non-nil if COLOR can be displayed on FRAME."
    (ignore frame background-p)
    (unless (stringp color) (signal 'wrong-type-argument (list 'stringp color)))
    (condition-case nil (progn (emacs-cc-xfaces-1--color color) t) (error nil))))
(unless (fboundp 'color-values-from-color-spec)
  (defun color-values-from-color-spec (spec)
    "Parse color SPEC as a numeric color and return (RED GREEN BLUE)."
    (unless (stringp spec) (signal 'wrong-type-argument (list 'stringp spec)))
    (let ((case-fold-search t))
      (cond
       ((string-match "\\`#\\([[:xdigit:]]\\{1,4\\}\\)\\([[:xdigit:]]\\{1,4\\}\\)\\([[:xdigit:]]\\{1,4\\}\\)\\'" spec)
        (let* ((parts (list (match-string 1 spec) (match-string 2 spec) (match-string 3 spec)))
               (width (length (car parts))))
          (when (and (= width (length (cadr parts))) (= width (length (caddr parts))))
            (mapcar (lambda (s) (/ (* (string-to-number s 16) 65535) (1- (expt 16 width)))) parts))))
       ((string-match "\\`rgb:\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)/\\([[:xdigit:]]+\\)\\'" spec)
        (let ((parts (list (match-string 1 spec) (match-string 2 spec) (match-string 3 spec))))
          (when (and (<= (length (car parts)) 4) (<= (length (cadr parts)) 4) (<= (length (caddr parts)) 4))
            (mapcar (lambda (s) (/ (* (string-to-number s 16) 65535) (1- (expt 16 (length s))))) parts))))
       ((string-match "\\`rgbi:\\([0-9.]+\\)/\\([0-9.]+\\)/\\([0-9.]+\\)\\'" spec)
        (let ((parts (mapcar #'string-to-number (list (match-string 1 spec) (match-string 2 spec) (match-string 3 spec)))))
          (when (and (<= 0 (car parts) 1) (<= 0 (cadr parts) 1) (<= 0 (caddr parts) 1))
            (mapcar (lambda (n) (round (* n 65535))) parts))))))))
(unless (fboundp 'face-attributes-as-vector)
  (defun face-attributes-as-vector (plist)
    "Return a vector of face attributes corresponding to PLIST."
    (let ((v (make-vector 20 'unspecified)))
      (dolist (pair '((:family . 0) (:foundry . 1) (:width . 2) (:height . 3) (:weight . 5) (:slant . 6)
                      (:underline . 7) (:overline . 8) (:foreground . 9) (:background . 10) (:inverse-video . 11)
                      (:stipple . 12) (:strike-through . 13) (:box . 14) (:inherit . 16) (:extend . 19)))
        (let ((value (plist-get plist (car pair)))) (when value (aset v (cdr pair) value)))) v)))
(unless (fboundp 'frame--face-hash-table)
  (defun frame--face-hash-table (&optional frame)
    "Return a hash table of frame-local faces defined on FRAME."
    (when (and frame (not (frame-live-p frame))) (signal 'wrong-type-argument (list 'frame-live-p frame)))
    emacs-cc-xfaces-1--face-table))
(unless (fboundp 'internal-set-alternative-font-family-alist)
  (defun internal-set-alternative-font-family-alist (alist)
    "Define alternative font families to try in face font selection."
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (setq emacs-cc-xfaces-1--alternative-font-family-alist alist)
    (mapcar (lambda (entry) (cons (intern (car entry)) (mapcar #'intern (cdr entry)))) alist)))
(unless (fboundp 'internal-set-alternative-font-registry-alist)
  (defun internal-set-alternative-font-registry-alist (alist)
    "Define alternative font registries to try in face font selection."
    (unless (listp alist) (signal 'wrong-type-argument (list 'listp alist)))
    (setq emacs-cc-xfaces-1--alternative-font-registry-alist alist)
    (mapcar (lambda (entry) (cons (car entry) (cdr entry))) alist)))
(unless (fboundp 'internal-set-font-selection-order)
  (defun internal-set-font-selection-order (order)
    "Set font selection order for face font selection to ORDER."
    (unless (and (listp order) (= (length order) 4)
                 (equal (sort (copy-sequence order) (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                        '(:height :slant :weight :width)))
      (signal 'error (list "Invalid font sort order")))
    (setq emacs-cc-xfaces-1--font-selection-order order)
    nil))
(unless (fboundp 'internal-set-lisp-face-attribute-from-resource)
  (defun internal-set-lisp-face-attribute-from-resource (face attr value &optional frame)
    "Set FACE attribute ATTR to VALUE from a resource."
    (unless (symbolp face) (signal 'wrong-type-argument (list 'symbolp face)))
    (unless (symbolp attr) (signal 'wrong-type-argument (list 'symbolp attr)))
    (unless (stringp value) (signal 'wrong-type-argument (list 'stringp value)))
    (ignore frame) nil))
(unless (fboundp 'tty-suppress-bold-inverse-default-colors)
  (defun tty-suppress-bold-inverse-default-colors (suppress)
    "Suppress/allow boldness of faces with inverse default colors."
    (setq emacs-cc-xfaces-1--suppress-bold-inverse-default-colors suppress)))

;; The standalone prelude predates live frames and uses the new-frame
;; defaults table for every face.  Keep its attribute validator, but give
;; each live frame independent vectors.  Host C primitives stay untouched.
(when (fboundp 'nelisp--lface-from-face-name)
  (defvar emacs-cc-xfaces-1--frame-stores (make-hash-table :test 'eq)
    "Frame to (ID/attribute table . public attribute table).")
  (defvar emacs-cc-xfaces-1--set-attribute
    (symbol-function 'internal-set-lisp-face-attribute)
    "Prelude setter retained for its GNU attribute validation.")
  (defvar emacs-cc-xfaces-1--stores-separated nil)

  (defun emacs-cc-xfaces-1--empty-vector ()
    "Return an unspecified Lisp face vector."
    (let ((attributes (make-vector 20 'unspecified)))
      (aset attributes 0 'face)
      attributes))

  (defun emacs-cc-xfaces-1--frame-store (frame)
    "Return FRAME's face tables, initializing from new-frame defaults."
    (setq frame (or frame (selected-frame)))
    (nelisp--check-live-frame frame)
    (or (gethash frame emacs-cc-xfaces-1--frame-stores)
        (let ((store (make-hash-table :test 'eq))
              (public (make-hash-table :test 'eq)))
          (maphash
           (lambda (name entry)
             (let ((attributes (copy-sequence (cdr entry))))
               (puthash name (cons (car entry) attributes) store)
               (puthash name attributes public)))
           face--new-frame-defaults)
          (let ((tables (cons store public)))
            ;; Publish before applying specs, whose setters reenter here.
            (puthash frame tables emacs-cc-xfaces-1--frame-stores)
            (when (fboundp 'face-set-after-frame-default)
              (face-set-after-frame-default frame))
            tables))))

  (defun emacs-cc-xfaces-1--store (frame)
    "Return the defaults table for t, or FRAME's private face table."
    (if (eq frame t) face--new-frame-defaults
      (car (emacs-cc-xfaces-1--frame-store frame))))

  (defun emacs-cc-xfaces-1--lface (face frame signal-p)
    "Resolve FACE and look up its vector in FRAME's store."
    (let* ((store (emacs-cc-xfaces-1--store frame))
           (name (nelisp--resolve-face-name face signal-p))
           (entry (gethash name store)))
      (if entry (cdr entry)
        (when signal-p (signal 'error (list "Invalid face" name))))))

  ;; All attributes present at this point came from startup face specs on
  ;; the selected frame.  GNU keeps those specs out of new-frame defaults;
  ;; later explicit FRAME t writes populate that separate table.
  (unless emacs-cc-xfaces-1--stores-separated
    (let ((store (make-hash-table :test 'eq))
          (public (make-hash-table :test 'eq)))
      (maphash
       (lambda (name entry)
         (let ((attributes (cdr entry)))
           (puthash name (cons (car entry) attributes) store)
           (puthash name attributes public)
           (setcdr entry (emacs-cc-xfaces-1--empty-vector))))
       face--new-frame-defaults)
      (puthash (selected-frame) (cons store public)
               emacs-cc-xfaces-1--frame-stores))
    (setq emacs-cc-xfaces-1--stores-separated t))

  (defun frame--face-hash-table (&optional frame)
    "Return the table of face vectors belonging to FRAME."
    (cdr (emacs-cc-xfaces-1--frame-store frame)))

  (defun internal-lisp-face-p (face &optional frame)
    "Return FACE's global vector, or its frame-local vector on FRAME."
    (setq face (nelisp--resolve-face-name face t))
    (when frame (nelisp--check-live-frame frame))
    (emacs-cc-xfaces-1--lface face (or frame t) nil))

  (defun internal-make-lisp-face (face &optional frame)
    "Create or reset FACE globally for nil FRAME, or locally on FRAME."
    (unless (symbolp face)
      (signal 'wrong-type-argument (list 'symbolp face)))
    (when frame (nelisp--check-live-frame frame))
    (let* ((name (nelisp--resolve-face-name face nil))
           (global (gethash name face--new-frame-defaults)))
      (unless global
        (setq global (cons nelisp--face-next-id
                           (emacs-cc-xfaces-1--empty-vector)))
        (setq nelisp--face-next-id (1+ nelisp--face-next-id))
        (puthash face global face--new-frame-defaults)
        (put face 'face (car global))
        (setq name face))
      (let* ((tables (and frame (emacs-cc-xfaces-1--frame-store frame)))
             (store (if frame (car tables) face--new-frame-defaults))
             (entry (gethash name store))
             (attributes (if entry (cdr entry)
                           (emacs-cc-xfaces-1--empty-vector)))
             (i 1))
        (while (< i (length attributes))
          (aset attributes i 'unspecified)
          (setq i (1+ i)))
        (unless entry
          (puthash face (cons (car global) attributes) store)
          (when frame (puthash face attributes (cdr tables))))
        attributes)))

  (defun internal-get-lisp-face-attribute (symbol keyword &optional frame)
    "Return SYMBOL's KEYWORD attribute on FRAME, or its defaults for t."
    (unless (symbolp symbol)
      (signal 'wrong-type-argument (list 'symbolp symbol)))
    (unless (symbolp keyword)
      (signal 'wrong-type-argument (list 'symbolp keyword)))
    (let* ((attributes (emacs-cc-xfaces-1--lface symbol frame t))
           (index (cdr (assq keyword nelisp--lface-attr-index-alist))))
      (unless index
        (signal 'error (list "Invalid face attribute name" keyword)))
      (let ((value (aref attributes index)))
        (if (eq value :ignore-defface) 'unspecified value))))

  (defun internal-set-lisp-face-attribute (face attr value &optional frame)
    "Set FACE's ATTR on FRAME; t sets defaults, 0 sets defaults and frames."
    (unless (symbolp face)
      (signal 'wrong-type-argument (list 'symbolp face)))
    (unless (symbolp attr)
      (signal 'wrong-type-argument (list 'symbolp attr)))
    (setq face (nelisp--resolve-face-name face t))
    (if (eq frame 0)
        (progn
          (internal-set-lisp-face-attribute face attr value t)
          (dolist (target (frame-list))
            (internal-set-lisp-face-attribute face attr value target))
          face)
      (unless (eq frame t)
        (setq frame (or frame (selected-frame)))
        (nelisp--check-live-frame frame)
        (unless (emacs-cc-xfaces-1--lface face frame nil)
          (internal-make-lisp-face face frame)))
      ;; The prelude's lookup and inheritance validator both consult this
      ;; dynamically bound table.  A local face already exists, so its
      ;; global-only creation fallback cannot run.
      (let ((face--new-frame-defaults (emacs-cc-xfaces-1--store frame)))
        (funcall emacs-cc-xfaces-1--set-attribute face attr value frame))))

  (defun internal-copy-lisp-face (from to frame new-frame)
    "Copy FROM on FRAME into TO on NEW-FRAME, preserving independent stores."
    (unless (symbolp from)
      (signal 'wrong-type-argument (list 'symbolp from)))
    (unless (symbolp to)
      (signal 'wrong-type-argument (list 'symbolp to)))
    (unless (eq frame t) (nelisp--check-live-frame frame))
    ;; GNU ignores NEW-FRAME when copying global definitions (FRAME t).
    (when (and (not (eq frame t)) new-frame)
      (nelisp--check-live-frame new-frame))
    ;; GNU creates/resets the destination after obtaining the source
    ;; vector.  A same-face copy in the same store therefore resets it.
    (let* ((source (emacs-cc-xfaces-1--lface from frame t))
           (target (if (eq frame t) t (or new-frame frame)))
           (destination (internal-make-lisp-face to
                         (unless (eq target t) target)))
           (i 1))
      (while (< i (length source))
        (aset destination i (aref source i))
        (setq i (1+ i)))
      to))

  (defun internal-lisp-face-equal-p (face1 face2 &optional frame)
    "Compare FACE1 and FACE2's attributes in FRAME's store."
    (let ((left (emacs-cc-xfaces-1--lface face1 frame t))
          (right (emacs-cc-xfaces-1--lface face2 frame t))
          (i 1) (same t))
      (while (and same (< i (length left)))
        (setq same (equal (aref left i) (aref right i)) i (1+ i)))
      same))

  (defun internal-lisp-face-empty-p (face &optional frame)
    "Return t if FACE has only unspecified attributes in FRAME's store."
    (let ((attributes (emacs-cc-xfaces-1--lface face frame t))
          (i 1) (empty t))
      (while (and empty (< i (length attributes)))
        (unless (eq (aref attributes i) 'unspecified) (setq empty nil))
        (setq i (1+ i)))
      empty)))

(provide 'emacs-cc-xfaces-1)
