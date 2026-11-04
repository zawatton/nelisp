;;; emacs-cc-census-buffer-w202.el --- Buffer evaluation and insertion primitives  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-buffer-w202--position (position)
  "Validate POSITION and return its integer position in the current buffer."
  (unless (or (integerp position) (markerp position))
    (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
  (let ((value (if (markerp position) (marker-position position) position)))
    (when (null value)
      (signal 'error '("Marker does not point anywhere")))
    (unless (and value (<= (point-min) value) (<= value (point-max)))
      (signal 'args-out-of-range (list position)))
    value))

(defun emacs-cc-census-buffer-w202--buffer (buffer)
  "Resolve BUFFER, signaling the error used by buffer evaluation and copying."
  (or (get-buffer buffer) (signal 'error '("No such buffer"))))

(unless (fboundp 'eval-buffer)
  (defun eval-buffer (&optional buffer printflag filename unibyte do-allow-print)
    "Evaluate accessible Lisp in BUFFER, preserving point and returning nil.
PRINTFLAG selects the output stream.  FILENAME identifies the loaded source.
UNIBYTE is obsolete.  DO-ALLOW-PRINT permits output without PRINTFLAG."
    (ignore unibyte)
    (with-current-buffer (if buffer
                            (emacs-cc-census-buffer-w202--buffer buffer)
                          (current-buffer))
      (when (and filename (not (stringp filename)))
        (signal 'wrong-type-argument (list 'stringp filename)))
      (save-excursion
        (let* ((load-file-name filename)
               (standard-output (or printflag
                                    (and do-allow-print standard-output)
                                    (lambda (_character) nil)))
               (lexical-binding
                (if (local-variable-p 'lexical-binding)
                    lexical-binding
                  (if (fboundp 'default-toplevel-value)
                      (default-toplevel-value 'lexical-binding)
                    nil))))
          ;; The file's first-line cookie overrides its ordinary local value.
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (looking-at "#!") (forward-line 1))
            (let ((line (buffer-substring-no-properties
                         (point) (line-end-position))))
              (when (string-match "-\\*-.*lexical-binding:[ \t]*\\([^; \t]+\\)" line)
                (setq lexical-binding
                      (not (string= (match-string 1 line) "nil"))))))
          (eval-region (point-min) (point-max) printflag))))
    nil))

(defun emacs-cc-census-buffer-w202--field (position escape limit endp)
  "Find the field boundary at POSITION, respecting ESCAPE, LIMIT and ENDP."
  (let* ((pos (emacs-cc-census-buffer-w202--position (or position (point))))
         (bound (and limit (emacs-cc-census-buffer-w202--position limit)))
         (lo (point-min)) (hi (point-max))
         (cursor (if endp
                     (if (and escape (< pos hi)) pos (max lo (1- pos)))
                   (max lo (1- pos))))
         (value (get-char-property cursor 'field)))
    ;; A boundary field separates the two adjoining fields; escaping it
    ;; includes the field on the far side rather than stopping inside it.
    (when (and escape (eq value 'boundary))
      (if endp
          (progn
            (while (and (< cursor hi)
                        (eq (get-char-property cursor 'field) 'boundary))
              (setq cursor (1+ cursor)))
            (setq value (get-char-property cursor 'field)))
        (while (and (> cursor lo)
                    (eq (get-char-property cursor 'field) 'boundary))
          (setq cursor (1- cursor)))
        (setq value (get-char-property cursor 'field))))
    (if endp
        (progn
          (while (and (< cursor hi)
                      (eq (get-char-property cursor 'field) value))
            (setq cursor (1+ cursor)))
          (if bound (min cursor bound) cursor))
      (while (and (> cursor lo)
                  (eq (get-char-property (1- cursor) 'field) value))
        (setq cursor (1- cursor)))
      (if bound (max cursor bound) cursor))))

(unless (fboundp 'field-beginning)
  (defun field-beginning (&optional pos escape-from-edge limit)
    "Return the beginning of the field at POS, optionally bounded by LIMIT."
    (emacs-cc-census-buffer-w202--field pos escape-from-edge limit nil)))

(unless (fboundp 'field-end)
  (defun field-end (&optional pos escape-from-edge limit)
    "Return the end of the field at POS, optionally bounded by LIMIT."
    (emacs-cc-census-buffer-w202--field pos escape-from-edge limit t)))

(defvar unread-command-events nil)
(defvar unread-post-input-method-events nil)
(defvar unread-input-method-events nil)

(unless (fboundp 'input-pending-p)
  (defun input-pending-p (&optional check-timers)
    "Return non-nil when queued command or input-method events are available.
CHECK-TIMERS is accepted; this implementation examines the Lisp input queues."
    ;; Timer dispatch and terminal polling require an input-loop provider.
    (ignore check-timers)
    (and (or unread-command-events unread-post-input-method-events
             unread-input-method-events) t)))

(defun emacs-cc-census-buffer-w202--sticky-p (property specification)
  "Return non-nil if SPECIFICATION includes PROPERTY."
  (or (eq specification t)
      (and (listp specification) (memq property specification))))

(defun emacs-cc-census-buffer-w202--rear-sticky-p (property properties)
  "Return non-nil when PROPERTY in PROPERTIES can be inherited from the left."
  (let ((default (and (boundp 'text-property-default-nonsticky)
                      (assq property text-property-default-nonsticky))))
    (not (or (emacs-cc-census-buffer-w202--sticky-p
              property (plist-get properties 'rear-nonsticky))
             (and default (cdr default))))))

(defun emacs-cc-census-buffer-w202--inherited-properties ()
  "Return properties inherited from the characters adjoining point."
  (let* ((left (and (> (point) (point-min))
                    (text-properties-at (1- (point)))))
         (right (and (< (point) (point-max)) (text-properties-at (point))))
         (front (plist-get right 'front-sticky))
         (left-front (plist-get left 'front-sticky))
         (tail left) properties front-properties)
    (while tail
      (let ((key (car tail)) (value (cadr tail)))
        (unless (or (eq key 'rear-nonsticky)
                    (eq key 'front-sticky)
                    (not (emacs-cc-census-buffer-w202--rear-sticky-p key left)))
          (setq properties (plist-put properties key value))))
      (setq tail (cddr tail)))
    (setq tail right)
    (while tail
      (let ((key (car tail)))
        (when (and (not (memq key '(front-sticky rear-nonsticky)))
                   (emacs-cc-census-buffer-w202--sticky-p key front))
          (setq properties (plist-put properties key (cadr tail)))))
      (setq tail (cddr tail)))
    (setq tail (append right left))
    (while tail
      (let ((key (car tail)))
        (when (and (not (memq key '(front-sticky rear-nonsticky)))
                   (not (memq key front-properties))
                   (if (and (plist-member left key)
                            (emacs-cc-census-buffer-w202--rear-sticky-p key left)
                            (not (and (plist-member right key)
                                      (emacs-cc-census-buffer-w202--sticky-p
                                       key front))))
                       (emacs-cc-census-buffer-w202--sticky-p key left-front)
                     (emacs-cc-census-buffer-w202--sticky-p key front)))
          (push key front-properties)))
      (setq tail (cddr tail)))
    (when front-properties
      (setq properties (plist-put properties 'front-sticky
                                  (nreverse front-properties))))
    properties))

(unless (fboundp 'insert-and-inherit)
  (defun insert-and-inherit (&rest args)
    "Insert strings and characters in ARGS, inheriting adjoining properties."
    (dolist (arg args)
      (let ((properties (emacs-cc-census-buffer-w202--inherited-properties))
            (start (point)))
        (insert arg)
        (let ((position start))
          (while (< position (point))
            (let ((existing (text-properties-at position)) (tail properties))
              (while tail
                (unless (plist-member existing (car tail))
                  (put-text-property position (1+ position) (car tail) (cadr tail)))
                (setq tail (cddr tail))))
            (setq position (1+ position))))))
    nil))

(unless (fboundp 'insert-buffer-substring)
  (defun insert-buffer-substring (buffer &optional start end)
    "Insert the accessible substring of BUFFER between START and END."
    (unless (or (bufferp buffer) (stringp buffer))
      (signal 'wrong-type-argument (list 'stringp buffer)))
    (unless (get-buffer buffer)
      (signal 'error (list (format "No buffer named %s" buffer))))
    (unless (or (null start) (integerp start) (markerp start))
      (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
    (when (and (markerp start) (null (marker-position start)))
      (signal 'error '("Marker does not point anywhere")))
    (unless (or (null end) (integerp end) (markerp end))
      (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    (when (and (markerp end) (null (marker-position end)))
      (signal 'error '("Marker does not point anywhere")))
    (let ((text
           (with-current-buffer (emacs-cc-census-buffer-w202--buffer buffer)
             (buffer-substring
              (if (markerp start) (marker-position start) (or start (point-min)))
              (if (markerp end) (marker-position end) (or end (point-max)))))))
      (insert text))))

(defconst emacs-cc-census-buffer-w202--modifiers
  '(("M-" meta 134217728) ("C-" control 67108864)
    ("S-" shift 33554432) ("H-" hyper 16777216)
    ("s-" super 8388608) ("A-" alt 4194304)
    ("triple-" triple 32) ("double-" double 16)
    ("drag-" drag 4) ("down-" down 2) ("up-" up 1))
  "Recognized event prefixes in canonical modifier order.")

(unless (fboundp 'internal-event-symbol-parse-modifiers)
  (defun internal-event-symbol-parse-modifiers (symbol)
    "Parse SYMBOL into its base event and canonical modifiers, caching both."
    (unless (symbolp symbol)
      (signal 'wrong-type-argument (list 'symbolp symbol)))
    (if (get symbol 'event-symbol-element-mask)
        (get symbol 'event-symbol-elements)
        (let ((name (symbol-name symbol)) (mask 0) (more t) modifiers)
          (while more
            (setq more nil)
            (dolist (entry emacs-cc-census-buffer-w202--modifiers)
              (let ((prefix (car entry)))
                (when (and (not more) (>= (length name) (length prefix))
                           (string= prefix (substring name 0 (length prefix))))
                  (setq name (substring name (length prefix))
                        mask (logior mask (nth 2 entry))
                        more t)))))
          (dolist (entry emacs-cc-census-buffer-w202--modifiers)
            (unless (zerop (logand mask (nth 2 entry)))
              (push (nth 1 entry) modifiers)))
          (let* ((base (intern name)) (elements (cons base (nreverse modifiers))))
            (put symbol 'event-symbol-element-mask (list base mask))
            (put symbol 'event-symbol-elements elements)
            elements)))))

(unless (fboundp 'local-variable-if-set-p)
  (defun local-variable-if-set-p (variable &optional buffer)
    "Return non-nil if VARIABLE is local or automatically local in BUFFER."
    (unless (symbolp variable)
      (signal 'wrong-type-argument (list 'symbolp variable)))
    (setq variable (indirect-variable variable))
    (or (local-variable-p variable buffer)
        (and (boundp 'emacs-buffer--variable-buffer-local)
             (memq variable emacs-buffer--variable-buffer-local) t))))

;; Labeled restrictions need the narrowing engine to enforce their bounds
;; during ordinary `widen', `narrow-to-region' and `save-restriction'.
;; Those owners are outside this unit, so their existing entry points remain.

(provide 'emacs-cc-census-buffer-w202)
;;; emacs-cc-census-buffer-w202.el ends here
