;;; emacs-buffer-builtins.el --- Unprefixed Emacs C-core buffer builtins  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 9 — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* buffer builtins (= the names
;; that vanilla Elisp code expects: `generate-new-buffer',
;; `with-current-buffer', `point-min', `buffer-substring-no-properties',
;; ...) to NeLisp's `nelisp-emacs-compat' (= `nelisp-ec-*') primitives.
;;
;; Phase 8 shipped a pragmatic accumulator-string approximation for
;; `with-temp-buffer' / `insert' / `buffer-string' inside `emacs-stub.el'.
;; That sufficed to unblock anvil-memory tokenizer + worklog write paths
;; but failed once a caller wanted to manipulate two buffers at once
;; (the accumulator was a single global string), or wanted the natural
;; `(buffer-substring-no-properties (point-min) (point-max))' pattern.
;;
;; Phase 9 replaces the accumulator with the real `nelisp-ec-*' buffer
;; substrate (T39, ~31 APIs), which already implements multi-buffer
;; current-buffer dispatch, narrow/widen, markers, and search.  This
;; file is primarily a *naming bridge* — every definition is gated so
;; loading inside a host Emacs is a cheap no-op and the host's own C
;; builtins win.
;;
;; What this module unblocks (= deferred from Phase 8 commit):
;;
;;   - `anvil-worklog-export-org' (= multi-buffer; uses
;;     `generate-new-buffer' + `with-current-buffer' + `kill-buffer'
;;     in `unwind-protect' shape).
;;   - any future MCP tool that wants `buffer-substring-no-properties'
;;     of a non-temp buffer.
;;
;; Non-goals (= still deferred):
;;
;;   - `make-network-process' / `memory-serve-start' (Phase 10
;;     candidate, requires socket primitive separate from buffer).
;;   - file-coding handling beyond UTF-8 default
;;     (= `coding-system-for-write' is read but not enforced).
;;   - hooks like `before-change-functions' / `after-change-functions'
;;     (= callers in the 22/27 working set don't depend on them).

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- buffer/point/insert/print-stream primitives must operate on the ec-buffer layer.
;;; Code:

(require 'nelisp-emacs-compat)

(unless (boundp 'text-property-default-nonsticky)
  (defvar text-property-default-nonsticky nil
    "Default non-sticky text properties for inserted text."))

(defun emacs-buffer-builtins--standalone-p ()
  "Non-nil on a standalone NeLisp reader (nemacs).
nemacs binds the variable `emacs-version' for vendor compatibility, so a
classic boundp-of-`emacs-version' standalone test fails there: the global
buffer ops and the `with-temp-buffer' / `with-current-buffer' macros are
left as the broken `emacs-stub' no-ops while the working `nelisp-ec-*'
implementations sit unused, and a temp-buffer insert reads back as nil.
Key off the reader-only primitive `nelisp--write-stdout-bytes' (absent
under host Emacs) so this bridge's standalone install path -- which
replaces the whole buffer-op chain with `nelisp-ec-*' -- fires on nemacs."
  (or (not (boundp 'emacs-version))
      (fboundp 'nelisp--write-stdout-bytes)))

(defun emacs-buffer-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  ;; A standalone image can already provide a complete native implementation
  ;; from its prelude.  Preserve it exactly as host Emacs preserves a C subr;
  ;; only marked bulk stubs and genuinely absent names need this bridge.
  (or (get symbol 'emacs-stub-bulk)
      (not (fboundp symbol))))

(defun emacs-buffer-builtins--replace-buffer-family-p ()
  "Return non-nil when standalone needs the complete compatibility family.
Keep an already installed `current-buffer' as the owner of a coherent native
buffer API.  When it is absent, the compatibility aliases must still be
installed as one family rather than mixed with unrelated partial fallbacks."
  (and (emacs-buffer-builtins--standalone-p)
       (not (fboundp 'current-buffer))))

(defun emacs-buffer-builtins--call-emacs-buffer (function args)
  "Lazy-load `emacs-buffer' and call FUNCTION with ARGS."
  (unless (fboundp function)
    (require 'emacs-buffer))
  (apply function args))

(defun emacs-buffer-builtins--sxhash-string (string)
  "Return a deterministic integer hash for STRING."
  (let ((hash 5381)
        (i 0)
        (len (length string)))
    (while (< i len)
      (setq hash (logand #x7FFFFFFF
                         (+ (* hash 33) (aref string i))))
      (setq i (1+ i)))
    hash))

(defun emacs-buffer-builtins--sxhash-object (object)
  "Return a deterministic session-stable integer hash for OBJECT."
  (emacs-buffer-builtins--sxhash-string (prin1-to-string object)))

(when (emacs-buffer-builtins--install-function-p 'sxhash)
  (defun sxhash (object)
    "Return a deterministic integer hash for OBJECT."
    (emacs-buffer-builtins--sxhash-object object)))

(when (emacs-buffer-builtins--install-function-p 'sxhash-equal)
  (defun sxhash-equal (object)
    "Return a deterministic integer hash for OBJECT using equal semantics."
    (emacs-buffer-builtins--sxhash-object object)))

(when (emacs-buffer-builtins--install-function-p 'sxhash-eq)
  (defun sxhash-eq (object)
    "Return a deterministic integer hash for OBJECT using eq-style identity."
    (emacs-buffer-builtins--sxhash-object object)))

(defun emacs-buffer-builtins--sticky-member-p (property specification)
  "Return whether PROPERTY is selected by sticky SPECIFICATION."
  (and specification
       (or (not (listp specification)) (memq property specification))))

(defun emacs-buffer-builtins--inherited-properties (position)
  "Return sticky properties adjoining POSITION in the current buffer."
  (let (left right)
    (save-restriction
      (widen)
      (setq left (and (> position 1) (text-properties-at (1- position)))
            right (and (< position (point-max))
                       (text-properties-at position))))
    (let* ((rear (plist-get left 'rear-nonsticky))
           (left-front (plist-get left 'front-sticky))
           (right-front (plist-get right 'front-sticky))
           (right-rear (plist-get right 'rear-nonsticky))
           (rest left) fronts rears result)
      (while rest
        (let ((property (car rest)) (value (cadr rest)))
          (when (and (not (memq property '(front-sticky rear-nonsticky)))
                     (not (emacs-buffer-builtins--sticky-member-p property rear))
                     (not (cdr (assq property text-property-default-nonsticky))))
            (setq result (cons property (cons value result)))
            (when (emacs-buffer-builtins--sticky-member-p property left-front)
              (push property fronts))))
        (setq rest (cddr rest)))
      (setq rest right)
      (while rest
        (let ((property (car rest)) (value (cadr rest)))
          ;; A rear-sticky value on the left wins over a front-sticky value.
          (when (and (not (memq property '(front-sticky rear-nonsticky)))
                     (not (plist-get result property))
                     (emacs-buffer-builtins--sticky-member-p property right-front))
            (setq result (plist-put result property value))
            (push property fronts)
            (when (emacs-buffer-builtins--sticky-member-p property right-rear)
              (push property rears))))
        (setq rest (cddr rest)))
      (when rears (setq result (cons 'rear-nonsticky (cons rears result))))
      (if fronts (cons 'front-sticky (cons fronts result)) result))))

(defun emacs-buffer-builtins--string-property-runs (string)
  "Return STRING's property intervals without scanning individual characters."
  (or (emacs-buffer-builtins--call-string-property 'runs string)
      (let ((position 0) (limit (length string)) result)
        (while (< position limit)
          (let ((properties (text-properties-at position string))
                (end (or (next-property-change position string) limit)))
            (when properties (push (list position end properties) result))
            (setq position end)))
        (nreverse result))))

(defun emacs-buffer-builtins--insert-and-inherit (&rest args)
  "Insert ARGS at point, inheriting sticky properties from adjoining text."
  (dolist (argument args)
    (let* ((start (point))
           (properties (emacs-buffer-builtins--inherited-properties start))
           (runs (and (stringp argument)
                      (emacs-buffer-builtins--string-property-runs argument))))
      (insert argument)
      (let ((end (point)) (inhibit-read-only t))
        (when (< start end)
          (set-text-properties start end nil)
          (dolist (run runs)
            (add-text-properties (+ start (nth 0 run))
                                 (+ start (nth 1 run)) (nth 2 run)))
          ;; GNU's inherited values override the inserted string's values.
          (when properties (add-text-properties start end properties))))))
  nil)

;;;; --- batched trivial defaliases (Doc 51 Phase 5 boot perf) -----------
;;
;; Pattern source: commit d3c17fa (emacs-stub-bulk Phase 11.D batch).  The
;; nelisp standalone interpreter charges ~47ms per top-level form for the
;; original `(unless (fboundp X) (defalias X #'nelisp-ec-Y))' idiom — 23
;; clauses below + 14 in `emacs-fileio-builtins.el' add ~1.8s on every
;; bootstrap.  Collapsing through one dolist body keeps the gate semantics
;; identical (= each entry still does exactly one fboundp test) while
;; paying the per-form interpreter overhead only once.  Under host Emacs
;; the C subr wins fboundp so this is a no-op either way.

(let ((--aliases--
       '((generate-new-buffer        . nelisp-ec-generate-new-buffer)
         (kill-buffer                . nelisp-ec-kill-buffer)
         (bufferp                    . nelisp-ec-buffer-p)
         (current-buffer             . nelisp-ec-current-buffer)
         (set-buffer                 . nelisp-ec-set-buffer)
         (point                      . nelisp-ec-point)
         (point-min                  . nelisp-ec-point-min)
         (point-max                  . nelisp-ec-point-max)
         (goto-char                  . nelisp-ec-goto-char)
         (buffer-size                . nelisp-ec-buffer-size)
         (insert                     . nelisp-ec-insert)
         (insert-and-inherit         . emacs-buffer-builtins--insert-and-inherit)
         (erase-buffer               . nelisp-ec-erase-buffer)
         (delete-region              . nelisp-ec-delete-region)
         (buffer-string              . nelisp-ec-buffer-string)
         (buffer-substring           . nelisp-ec-buffer-substring)
         ;; Phase 9 MVP: text properties are not yet stored on
         ;; `nelisp-ec-buffer'; the substring already carries no
         ;; properties so `-no-properties' is a plain alias.
         (buffer-substring-no-properties . nelisp-ec-buffer-substring)
         (narrow-to-region           . nelisp-ec-narrow-to-region)
         (widen                      . nelisp-ec-widen)
         (make-marker                . nelisp-ec-make-marker)
         (markerp                    . nelisp-ec-marker-p)
         (set-marker                 . nelisp-ec-set-marker)
         (move-marker                . nelisp-ec-set-marker)
         (marker-position            . nelisp-ec-marker-position)
         (marker-buffer              . nelisp-ec-marker-buffer)
         (marker-insertion-type      . nelisp-ec-marker-insertion-type)
         (set-marker-insertion-type  . nelisp-ec-set-marker-insertion-type)
         (point-marker               . nelisp-ec-point-marker)
         (insert-before-markers      . nelisp-ec-insert-before-markers))))
  (if (emacs-buffer-builtins--replace-buffer-family-p)
      (dolist (--cell-- --aliases--)
        (fset (car --cell--) (cdr --cell--)))
    (dolist (--cell-- --aliases--)
      (let ((--name-- (car --cell--)) (--target-- (cdr --cell--)))
        (unless (fboundp --name--)
          (defalias --name-- --target--))))))

(require 'emacs-buffer)

(defvar emacs-buffer-builtins--indirect-families (make-hash-table :test 'eq)
  "Base buffer to buffers sharing its text, including the base itself.")

(defun emacs-buffer-builtins--indirect-family (buffer)
  "Return BUFFER's shared-text family, or nil for an ordinary buffer."
  (let* ((ext (gethash buffer emacs-buffer--state))
         (base (or (and ext (emacs-buffer--ext-base-buffer ext)) buffer)))
    (gethash base emacs-buffer-builtins--indirect-families)))

(defun emacs-buffer-builtins--share-text (source)
  "Publish SOURCE's text and properties to its live indirect siblings.
Each sibling retains its own point and restriction.  Low-level edit advice
adjusts those positions before calling this function."
  (let ((family (emacs-buffer-builtins--indirect-family source))
        (source-ext (gethash source emacs-buffer--state)))
    (dolist (buffer family)
      (when (and (not (eq buffer source)) (buffer-live-p buffer))
        (let ((position (nelisp-point buffer))
              (ext (emacs-buffer--ensure-text-property-ext buffer)))
          (setf (nelisp-buffer-before-gap buffer)
                (nelisp-buffer-before-gap source))
          (setf (nelisp-buffer-after-gap buffer)
                (nelisp-buffer-after-gap source))
          (setf (nelisp-buffer-text-properties buffer)
                (nelisp-buffer-text-properties source))
          (setf (nelisp-buffer-modified buffer) (nelisp-buffer-modified source))
          (when source-ext
            (setf (emacs-buffer--ext-text-props ext)
                  (emacs-buffer--ext-text-props source-ext)))
          (puthash buffer (gethash source nelisp-buffer--tick 0)
                   nelisp-buffer--tick)
          (remhash buffer nelisp-buffer--text-cache)
          (remhash buffer nelisp-buffer--blen-cache)
          (remhash buffer nelisp-buffer--size-cache)
          (remhash buffer nelisp-buffer--pending-point)
          (nelisp-goto-char position buffer))))))

(defun emacs-buffer-builtins--indirect-edit (operation orig &rest args)
  "Run text OPERATION through ORIG and adjust indirect sibling positions."
  (let* ((buffer (nelisp-buffer--ambient
                  (nth (cond ((eq operation 'delete) 2)
                             ((eq operation 'erase) 0) (t 1)) args)))
         (family (emacs-buffer-builtins--indirect-family buffer)))
    (if (not family)
        (apply orig args)
      (let* ((insert-p (memq operation '(insert insert-before-markers)))
             (start (cond (insert-p (nelisp-point buffer))
                          ((eq operation 'erase) 1)
                          (t (min (car args) (cadr args)))))
             (end (cond (insert-p start)
                        ((eq operation 'erase) (1+ (nelisp-buffer-size buffer)))
                        (t (max (car args) (cadr args)))))
             (size (if insert-p (length (car args)) (- start end)))
             (positions nil))
        (dolist (peer family)
          (when (and (not (eq peer buffer)) (buffer-live-p peer))
            (push (cons peer (nelisp-point peer)) positions)))
        (prog1 (apply orig args)
          (dolist (entry positions)
            (let* ((peer (car entry))
                   (map-position
                    (lambda (pos)
                      (if insert-p
                          (if (> pos start) (+ pos size) pos)
                        (cond ((<= pos start) pos)
                              ((>= pos end) (+ pos size))
                              (t start)))))
                   (position (funcall map-position (cdr entry))))
              (when (nelisp-buffer-narrow-start peer)
                (setf (nelisp-buffer-narrow-start peer)
                      (funcall map-position (nelisp-buffer-narrow-start peer))))
              (when (nelisp-buffer-narrow-end peer)
                (setf (nelisp-buffer-narrow-end peer)
                      (if (and insert-p (>= (nelisp-buffer-narrow-end peer) start))
                          (+ (nelisp-buffer-narrow-end peer) size)
                        (funcall map-position (nelisp-buffer-narrow-end peer)))))
              (if insert-p
                  (progn
                    (if (eq operation 'insert-before-markers)
                        (nelisp-buffer--shift-markers-on-insert-before-markers
                         peer start size)
                      (nelisp-buffer--shift-markers-on-insert peer start size))
                    (nelisp-buffer--shift-overlays-on-insert peer start size))
                (nelisp-buffer--shift-markers-on-delete peer start end)
                (nelisp-buffer--shift-overlays-on-delete peer start end))
              ;; Record the mapped point without clamping to the old text size.
              (puthash peer position nelisp-buffer--pending-point)))
          (emacs-buffer-builtins--share-text buffer))))))

(defvar combine-after-change-calls nil
  "Non-nil means defer after-change notifications when possible.")
(defvar inhibit-modification-hooks nil
  "Non-nil means suppress buffer modification hooks.")

(defvar emacs-buffer-builtins--after-change-buffer nil
  "Target of GNU's single deferred notification queue.
The aggregate itself is stored in that buffer's event side table.")

(defun emacs-buffer-builtins--change-edit (operation original args)
  "Run OPERATION through ORIGINAL, recording deferred change notifications."
  (if (or inhibit-modification-hooks
          (and (null before-change-functions) (null after-change-functions)))
      (apply #'emacs-buffer-builtins--indirect-edit operation original args)
    (let* ((buffer (nelisp-buffer--ambient
                    (nth (cond ((eq operation 'delete) 2)
                               ((eq operation 'erase) 0) (t 1)) args)))
           (size (nelisp-buffer-size buffer))
           (insert-p (memq operation '(insert insert-before-markers)))
           (beg (cond (insert-p (nelisp-point buffer))
                      ((eq operation 'erase) 1)
                      (t (min (car args) (cadr args)))))
           (end (cond (insert-p beg) ((eq operation 'erase) (1+ size))
                      (t (max (car args) (cadr args)))))
           (new-length (if insert-p (length (car args)) 0))
           (old-length (- end beg)))
      (if (and (= old-length 0) (= new-length 0))
          (apply #'emacs-buffer-builtins--indirect-edit operation original args)
        (unless (and combine-after-change-calls (null before-change-functions))
          (emacs-buffer-builtins--flush-after-change
           emacs-buffer-builtins--after-change-buffer))
        (let ((inhibit-modification-hooks t))
          (when before-change-functions
            (run-hook-with-args 'before-change-functions beg end)))
        (prog1 (apply #'emacs-buffer-builtins--indirect-edit operation original args)
          (if (and combine-after-change-calls (null before-change-functions))
              (progn
                (when (and emacs-buffer-builtins--after-change-buffer
                           (not (eq buffer emacs-buffer-builtins--after-change-buffer)))
                  (let ((previous emacs-buffer-builtins--after-change-buffer))
                    (emacs-buffer-builtins--flush-after-change previous)
                    ;; GNU re-records an inner flush under the outer combining
                    ;; scope, then assigns the queue to the edited buffer.
                    (emacs-buffer--set-pending-after-change buffer
                          (emacs-buffer--pending-after-change previous))
                    (emacs-buffer--set-pending-after-change previous nil)))
                (setq emacs-buffer-builtins--after-change-buffer buffer)
                (let* ((ext (emacs-buffer--ensure-ext buffer))
                       (pending (emacs-buffer--pending-after-change buffer))
                       ;; GNU records unchanged suffix space relative to BEG
                       ;; and the net size change, rather than the replaced end.
                       (tail (- size (1- beg)))
                       (delta (- new-length old-length)))
                  (if pending
                      (progn (aset pending 0 (min beg (aref pending 0)))
                             (aset pending 1 (min tail (aref pending 1)))
                             (aset pending 2 (+ delta (aref pending 2))))
                    (emacs-buffer--set-pending-after-change buffer
                          (vector beg tail delta)))))
            (let ((inhibit-modification-hooks t))
              (run-hook-with-args 'after-change-functions
                                  beg (+ beg new-length) old-length))))))))

(defun emacs-buffer-builtins--flush-after-change (buffer)
  "Deliver and clear BUFFER's accumulated after-change notification."
  (let* ((ext (gethash buffer emacs-buffer--state))
         (pending (and ext (emacs-buffer--pending-after-change buffer))))
    (when (eq buffer emacs-buffer-builtins--after-change-buffer)
      (setq emacs-buffer-builtins--after-change-buffer nil))
    (when (and pending (buffer-live-p buffer))
      (emacs-buffer--set-pending-after-change buffer nil)
      (with-current-buffer buffer
        (let* ((size (nelisp-buffer-size buffer))
               (beg (min (1+ size) (aref pending 0)))
               (end (- (1+ size) (min size (aref pending 1))))
               (delta (aref pending 2))
               (old-length (- end beg delta)))
          (if (and combine-after-change-calls (null before-change-functions))
              ;; An inner flush records the aggregate again while the outer
              ;; combining scope remains active, matching GNU's internal API.
              (progn
                (setq emacs-buffer-builtins--after-change-buffer buffer)
                (emacs-buffer--set-pending-after-change buffer
                      (vector beg (- (1+ size) beg delta) delta)))
            (let ((inhibit-modification-hooks t))
              (run-hook-with-args 'after-change-functions beg end old-length)))))))
  nil)

(defun emacs-buffer-builtins--indirect-insert (orig &rest args)
  "Share ordinary native insertion across indirect buffers."
  (emacs-buffer-builtins--change-edit 'insert orig args))

(defun emacs-buffer-builtins--indirect-insert-before-markers (orig &rest args)
  "Share marker-advancing native insertion across indirect buffers."
  (emacs-buffer-builtins--change-edit 'insert-before-markers orig args))

(defun emacs-buffer-builtins--indirect-delete (orig &rest args)
  "Share native region deletion across indirect buffers."
  (emacs-buffer-builtins--change-edit 'delete orig args))

(defun emacs-buffer-builtins--indirect-erase (orig &rest args)
  "Share native erasure across indirect buffers."
  (emacs-buffer-builtins--change-edit 'erase orig args))

(defun emacs-buffer-builtins--share-current-text (&rest ignored)
  "Publish current-buffer sidecar properties after a standard edit."
  (ignore ignored)
  (when (and (current-buffer) (nelisp-buffer-p (current-buffer)))
    (emacs-buffer-builtins--share-text (current-buffer))))

(defun emacs-buffer-builtins--indirect-kill (orig buffer)
  "Kill BUFFER and its indirect children when BUFFER is a base buffer."
  (let* ((ext (gethash buffer emacs-buffer--state))
         (base (and ext (emacs-buffer--ext-base-buffer ext)))
         (family (gethash buffer emacs-buffer-builtins--indirect-families)))
    (when family
      (remhash buffer emacs-buffer-builtins--indirect-families)
      (dolist (peer family)
        (when (and (not (eq peer buffer)) (buffer-live-p peer))
          (dolist (marker (nelisp-buffer-markers peer))
            (setf (nelisp-marker-buffer marker) nil))
          (setf (nelisp-buffer-markers peer) nil)
          (funcall orig peer)
          (remhash peer emacs-buffer--state))))
    (when base
      (let ((remaining (delq buffer
                             (gethash base emacs-buffer-builtins--indirect-families))))
        (if (cdr remaining)
            (puthash base remaining emacs-buffer-builtins--indirect-families)
          (remhash base emacs-buffer-builtins--indirect-families))))
    (prog1 (funcall orig buffer)
      (when (or family base) (remhash buffer emacs-buffer--state)))))

(defun emacs-buffer-builtins--indirect-set-modified (flag &optional buffer)
  "Publish BUFFER's modified FLAG to the rest of its indirect family."
  (setq buffer (nelisp-buffer--ambient buffer))
  (dolist (peer (emacs-buffer-builtins--indirect-family buffer))
    (when (buffer-live-p peer)
      (setf (nelisp-buffer-modified peer) (and flag t)))))

(when (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'nelisp-insert))
  (dolist (entry '((nelisp-insert . emacs-buffer-builtins--indirect-insert)
                   (nelisp-insert-before-markers . emacs-buffer-builtins--indirect-insert-before-markers)
                   (nelisp-delete-region . emacs-buffer-builtins--indirect-delete)
                   (nelisp-erase-buffer . emacs-buffer-builtins--indirect-erase)
                   (nelisp-kill-buffer . emacs-buffer-builtins--indirect-kill)))
    (advice-add (car entry) :around (cdr entry)))
  (advice-add 'nelisp-buffer-set-modified :after
              #'emacs-buffer-builtins--indirect-set-modified)
  (dolist (function '(insert insert-before-markers delete-region erase-buffer))
    (advice-add function :after #'emacs-buffer-builtins--share-current-text)))

(when (emacs-buffer-builtins--standalone-p)
  (defun buffer-base-buffer (&optional buffer)
    "Return the base buffer of BUFFER when it is an indirect clone."
    (setq buffer (or buffer (current-buffer)))
    (unless (bufferp buffer)
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    (let ((ext (gethash buffer emacs-buffer--state)))
      (or (and ext (emacs-buffer--ext-base-buffer ext))
          (gethash (buffer-name buffer)
                   emacs-buffer--indirect-base-by-name))))

  (defun make-indirect-buffer (base-buffer name &optional clone inhibit-buffer-hooks)
    "Create an indirect buffer named NAME sharing BASE-BUFFER's text.
CLONE copies buffer-local bindings.  INHIBIT-BUFFER-HOOKS suppresses
buffer creation and killing hooks in the new buffer."
    (let ((base (get-buffer base-buffer)))
      (unless base (error "No such buffer: ‘%s’" base-buffer))
      (unless (stringp name)
        (signal 'wrong-type-argument (list 'stringp name)))
      (unless (buffer-live-p base) (error "Base buffer has been killed"))
      (when (get-buffer name) (error "Buffer name ‘%s’ is in use" name))
      (if (not (nelisp-buffer-p base))
          (emacs-buffer-clone-indirect-buffer name base)
        (let* ((root (or (buffer-base-buffer base) base))
               (buffer (generate-new-buffer name inhibit-buffer-hooks))
               (ext (emacs-buffer--ensure-text-property-ext buffer))
               (base-ext (emacs-buffer--ensure-text-property-ext base)))
          (setf (emacs-buffer--ext-base-buffer ext) root)
          (setf (nelisp-buffer-before-gap buffer) (nelisp-buffer-before-gap base))
          (setf (nelisp-buffer-after-gap buffer) (nelisp-buffer-after-gap base))
          (setf (nelisp-buffer-narrow-start buffer) (nelisp-buffer-narrow-start base))
          (setf (nelisp-buffer-narrow-end buffer) (nelisp-buffer-narrow-end base))
          (setf (nelisp-buffer-text-properties buffer) (nelisp-buffer-text-properties base))
          (setf (nelisp-buffer-modified buffer) (nelisp-buffer-modified base))
          (setf (emacs-buffer--ext-text-props ext) (emacs-buffer--ext-text-props base-ext))
          (puthash buffer (gethash base nelisp-buffer--tick 0) nelisp-buffer--tick)
          (nelisp-goto-char (nelisp-point base) buffer)
          (when clone
            (when (eq base (current-buffer))
              (emacs-buffer--swap-out
               base (emacs-buffer--swap-active-symbols base base)))
            (let ((locals nil))
              (dolist (cell (buffer-local-variables base))
                (push (if (consp cell)
                          (cons (car cell) (buffer-local-value (car cell) base))
                        cell) locals))
              (setf (emacs-buffer--ext-locals ext) (nreverse locals))))
          (when inhibit-buffer-hooks
            (emacs-buffer-set-buffer-local-value 'kill-buffer-hook buffer nil)
            (emacs-buffer-set-buffer-local-value 'kill-buffer-query-functions buffer nil)
            (emacs-buffer-set-buffer-local-value 'buffer-list-update-hook buffer nil))
          (puthash root
                   (cons buffer (or (gethash root emacs-buffer-builtins--indirect-families)
                                    (list root)))
                   emacs-buffer-builtins--indirect-families)
          buffer)))))

(defun emacs-buffer-builtins--buffer-local-variables (&optional buffer &rest extra)
  "Return fresh bindings for BUFFER, using symbols for locally void values."
  (when extra
    (signal 'wrong-number-of-arguments
            (list 'buffer-local-variables (1+ (length extra)))))
  (setq buffer (or buffer (current-buffer)))
  (unless (bufferp buffer)
    (signal 'wrong-type-argument (list 'bufferp buffer)))
  (let* ((ext (gethash buffer emacs-buffer--state))
         (current (eq buffer (current-buffer))) result)
    (when ext
      (dolist (cell (emacs-buffer--ext-locals ext))
        (let ((symbol (car cell)))
          (push (if current
                    (if (boundp symbol) (cons symbol (symbol-value symbol))
                      symbol)
                  (cons symbol (cdr cell))) result))))
    (let ((cell (assq 'buffer-file-name result))
          (value (emacs-buffer-buffer-local-value 'buffer-file-name buffer)))
      (if cell
          (setcdr cell value)
        (push (cons 'buffer-file-name value) result)))
    (nreverse result)))

(defun emacs-buffer-builtins--kill-all-local-variables (&optional kill-permanent &rest extra)
  "Switch to Fundamental mode and remove current buffer local bindings.
Run `change-major-mode-hook' first.  Preserve `permanent-local' bindings
unless KILL-PERMANENT is non-nil; partially permanent hooks retain only
functions marked with `permanent-local-hook' and the global-hook marker.
Reset the local keymap, syntax and case tables, and mode-line display.

(fn &optional KILL-PERMANENT)"
  (when extra
    (signal 'wrong-number-of-arguments
            (list 'kill-all-local-variables (1+ (length extra)))))
  (unless (fboundp 'emacs-mode-builtins--reset-local-variable)
    (require 'emacs-mode-builtins))
  (run-hooks 'change-major-mode-hook)
  (emacs-mode-builtins--set-local-value 'major-mode 'fundamental-mode)
  (emacs-mode-builtins--set-local-value 'mode-name "Fundamental")
  (use-local-map nil)
  (dolist (binding (buffer-local-variables))
    (let* ((symbol (if (consp binding) (car binding) binding))
           (permanent (get symbol 'permanent-local)))
      (cond
       ;; These GNU buffer slots are always local and are not reset by
       ;; a major-mode change, even with KILL-PERMANENT.  Mode state and
       ;; invisibility are also always local but are reset explicitly.
       ((memq symbol '(major-mode mode-name buffer-invisibility-spec
                       buffer-file-name default-directory buffer-backed-up
                       buffer-saved-size buffer-auto-save-file-name
                       buffer-read-only buffer-undo-list local-minor-modes
                       mark-active point-before-scroll buffer-file-truename
                       buffer-file-format buffer-auto-save-file-format
                       buffer-display-count buffer-display-time
                       enable-multibyte-characters)))
       ((and (not kill-permanent)
             (memq symbol '(truncate-lines buffer-file-coding-system))))
       ;; GNU's resettable builtin buffer slots use native permanence
       ;; flags rather than the symbol's permanent-local property.
       ((memq symbol '(mode-line-format abbrev-mode overwrite-mode
                       auto-fill-function selective-display
                       selective-display-ellipses tab-width truncate-lines
                       word-wrap ctl-arrow fill-column left-margin
                       local-abbrev-table buffer-display-table
                       cache-long-scans bidi-display-reordering
                       bidi-paragraph-direction bidi-paragraph-separate-re
                       bidi-paragraph-start-re buffer-file-coding-system
                       left-margin-width right-margin-width
                       left-fringe-width right-fringe-width
                       fringes-outside-margins scroll-bar-width
                       scroll-bar-height vertical-scroll-bar
                       horizontal-scroll-bar indicate-empty-lines
                       indicate-buffer-boundaries fringe-indicator-alist
                       fringe-cursor-alist scroll-up-aggressively
                       scroll-down-aggressively header-line-format
                       tab-line-format cursor-type line-spacing
                       text-conversion-style cursor-in-non-selected-windows))
        (emacs-mode-builtins--reset-local-variable symbol))
       ((and (not kill-permanent) permanent)
        (when (and (eq permanent 'permanent-local-hook) (boundp symbol))
          (emacs-mode-builtins--set-local-value
           symbol (emacs-mode-builtins--permanent-hook-value
                   (symbol-value symbol)))))
       (t (emacs-mode-builtins--reset-local-variable symbol)))))
  (when (boundp 'buffer-invisibility-spec)
    (emacs-mode-builtins--set-local-value 'buffer-invisibility-spec t))
  (set-syntax-table (standard-syntax-table))
  (when (and (fboundp 'standard-case-table) (fboundp 'set-case-table))
    (set-case-table (standard-case-table)))
  (when (and (fboundp 'standard-category-table) (fboundp 'set-category-table))
    (set-category-table (standard-category-table)))
  (when (boundp 'local-abbrev-table)
    (setq local-abbrev-table (default-value 'local-abbrev-table)))
  (force-mode-line-update)
  nil)

(let ((--local-aliases--
       '((make-local-variable       . emacs-buffer-make-local-variable)
         (make-variable-buffer-local . emacs-buffer-make-variable-buffer-local)
         (buffer-local-variables    . emacs-buffer-builtins--buffer-local-variables)
         (buffer-local-value        . emacs-buffer-buffer-local-value)
         (local-variable-p          . emacs-buffer-local-variable-p)
         (default-value             . emacs-buffer-default-value)
         (default-boundp            . emacs-buffer-default-boundp)
         ;; Public set-default must update the live cell as setq-default does.
         ;; A table-only write leaves Custom :set callbacks' variables unbound.
         (set-default               . emacs-buffer-setq-default-1)
         (kill-local-variable       . emacs-buffer-kill-local-variable)
         (kill-all-local-variables  . emacs-buffer-builtins--kill-all-local-variables))))
  (if (emacs-buffer-builtins--standalone-p)
      (dolist (--cell-- --local-aliases--)
        (fset (car --cell--) (cdr --cell--)))
    (dolist (--cell-- --local-aliases--)
      (let ((--name-- (car --cell--))
            (--target-- (cdr --cell--)))
        (when (emacs-buffer-builtins--install-function-p --name--)
          (defalias --name-- --target--))))))

(defun emacs-buffer-builtins-buffer-narrowed-p ()
  "Return non-nil if the current `nelisp-ec' buffer is narrowed."
  (let ((buf (nelisp-ec-current-buffer)))
    (and buf
         (or (nelisp-ec-buffer-narrow-start buf)
             (nelisp-ec-buffer-narrow-end buf))
         t)))

(when (emacs-buffer-builtins--install-function-p 'buffer-narrowed-p)
  (defalias 'buffer-narrowed-p
    #'emacs-buffer-builtins-buffer-narrowed-p))

(defun emacs-buffer-builtins-copy-marker (&optional marker-or-integer type)
  "Return a fresh marker copied from MARKER-OR-INTEGER.
nil MARKER-OR-INTEGER returns a detached marker, matching Emacs."
  (let ((marker (nelisp-ec-make-marker)))
    (cond
     ((null marker-or-integer) nil)
     ((nelisp-ec-marker-p marker-or-integer)
      (nelisp-ec-set-marker marker
                            (nelisp-ec-marker-position marker-or-integer)
                            (nelisp-ec-marker-buffer marker-or-integer)))
     ((integerp marker-or-integer)
      (nelisp-ec-set-marker marker marker-or-integer))
     (t
      (signal 'wrong-type-argument
              (list '(or marker integer null) marker-or-integer))))
    (when type
      (nelisp-ec-set-marker-insertion-type marker type))
    marker))

(when (emacs-buffer-builtins--install-function-p 'copy-marker)
  (defalias 'copy-marker #'emacs-buffer-builtins-copy-marker))

(when (emacs-buffer-builtins--install-function-p 'point-min-marker)
  (defun point-min-marker ()
    "Return a marker at `point-min' in the current buffer."
    (copy-marker (point-min))))

(when (emacs-buffer-builtins--install-function-p 'point-max-marker)
  (defun point-max-marker ()
    "Return a marker at `point-max' in the current buffer."
    (copy-marker (point-max))))

(defun emacs-buffer-builtins--text-property-object (object)
  "Return a standalone buffer object, or :string-or-unsupported.

When OBJECT is nil and the standalone runtime already has a current buffer,
resolve that buffer eagerly instead of letting the lower `emacs-buffer' owner
re-discover it indirectly.  The implicit current-buffer path has proven brittle
under the live Magit bridge even when the actual current buffer is valid."
  (cond
   ((null object)
    (let ((buf (and (fboundp 'current-buffer)
                    (ignore-errors (current-buffer)))))
      (if (and buf
               (or (and (fboundp 'nelisp-ec-buffer-p)
                        (nelisp-ec-buffer-p buf))
                   (and (fboundp 'buffer-live-p)
                        (buffer-live-p buf))))
          buf
        nil)))
   ((or (and (fboundp 'nelisp-ec-buffer-p) (nelisp-ec-buffer-p object))
        (and (fboundp 'buffer-live-p) (buffer-live-p object)))
    object)
   ((stringp object) object)
   (t :string-or-unsupported)))

(defun emacs-buffer-builtins--property-target (object)
  "Resolve OBJECT and validate it as a buffer or string."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (when (eq target :string-or-unsupported)
      (signal 'wrong-type-argument (list 'buffer-or-string-p object)))
    target))

(defun emacs-buffer-builtins--position (position)
  "Return POSITION as an integer, accepting markers."
  (cond ((integerp position) position)
        ((markerp position) (or (marker-position position) 0))
        (t (signal 'wrong-type-argument
                   (list 'integer-or-marker-p position)))))

(defun emacs-buffer-builtins--property-bounds (object)
  "Return the accessible bounds of buffer or string OBJECT."
  (if (stringp object)
      (list 0 (length object))
    (with-current-buffer object (list (point-min) (point-max)))))

(defun emacs-buffer-builtins--property-range (start end object)
  "Validate and normalize a text-property range in OBJECT."
  (setq start (emacs-buffer-builtins--position start)
        end (emacs-buffer-builtins--position end))
  (when (> start end)
    (let ((saved start)) (setq start end end saved)))
  ;; GNU accepts empty ranges even when they lie outside the object.
  (unless (= start end)
    (let ((bounds (emacs-buffer-builtins--property-bounds object)))
      (when (or (< start (car bounds)) (> end (cadr bounds)))
        (signal 'args-out-of-range (list start end)))))
  (list start end))

(defun emacs-buffer-builtins--property-read-position (position object)
  "Validate POSITION for a text-property read in OBJECT."
  (setq position (emacs-buffer-builtins--position position))
  (let ((bounds (emacs-buffer-builtins--property-bounds object)))
    (when (or (< position (car bounds)) (> position (cadr bounds)))
      (signal 'args-out-of-range (list position position))))
  position)

(defun emacs-buffer-builtins--text-properties-at-arguments (position object)
  "Return validated POSITION and target OBJECT for `text-properties-at'."
  ;; GNU accepts killed buffers, whose only valid position is 1.
  (let ((target (if (and object (bufferp object))
                    object
                  (emacs-buffer-builtins--property-target object))))
    (setq position (emacs-buffer-builtins--checked-position position))
    (if (and (bufferp target) (not (buffer-live-p target)))
        (unless (= position 1)
          (signal 'args-out-of-range (list position position)))
      (setq position
            (emacs-buffer-builtins--property-read-position position target)))
    (cons position target)))

(defun emacs-buffer-builtins--call-string-property (operation string &rest args)
  "Call the shared string-property provider for STRING and ARGS."
  (emacs-buffer-builtins--call-emacs-buffer
   'emacs-buffer-string-text-property (cons operation (cons string args))))

(defvar buffer-invisibility-spec nil
  "Standalone bridge for Emacs's per-buffer invisibility spec.")

(when (fboundp 'make-variable-buffer-local)
  (make-variable-buffer-local 'buffer-invisibility-spec))

;; Doc 33 §8 item 242 (buffer-local swap engine, M2 completion
;; blocker): register Emacs's own `DEFVAR_PER_BUFFER' variables so
;; every buffer switch swaps them regardless of whether a given buffer
;; ever called `make-local-variable' on them.  `buffer-read-only' is
;; the M2 minimal-repro symbol itself (Doc 33 §8: a brand-new buffer
;; must never read back a DIFFERENT buffer's `buffer-read-only' value);
;; `major-mode' and `default-directory' are the other two builtins the
;; M2 magit-status smoke path depends on.  Gated to standalone only —
;; under host Emacs these are genuine C-level per-buffer variables
;; already, so declaring them here too would fight the host's own
;; machinery instead of bridging a gap.
;;
;; `default-directory' is registered conditionally on `boundp': this
;; file (`emacs-buffer-builtins.el') loads BEFORE `emacs-fileio.el''s
;; own `(defvar default-directory "/" ...)' (`emacs-fileio.el' itself
;; `require's this file), so at this point in the load order the name
;; is not bound yet.  Passing an explicit `nil' default here regardless
;; would freeze `nil' into the registered default forever
;; (`emacs-buffer-declare-per-buffer' tracks "a default WAS given" via
;; its own `&rest' arity, so even an explicit `nil' would count) and
;; permanently shadow the real `"/"' default `emacs-fileio.el' goes on
;; to establish moments later — so the unbound case omits the DEFAULT
;; argument entirely, leaving `emacs-buffer-default-value''s ordinary
;; `boundp' fallback to pick up whatever value eventually gets
;; `defvar'd.
(when (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'emacs-buffer-declare-per-buffer))
  ;; An initialized defvar marks this intrinsic special without resetting an
  ;; existing bound value, so dynamic lets remain visible across caller files.
  (defvar buffer-read-only nil)
  (emacs-buffer-declare-per-buffer 'buffer-read-only nil)
  (emacs-buffer-declare-per-buffer 'major-mode 'fundamental-mode)
  ;; Like major-mode, the display name starts fresh in every new buffer.
  ;; Scratch's Lisp Interaction name must not become the global default.
  (emacs-buffer-declare-per-buffer 'mode-name "Fundamental")
  ;; Preserve any live value/default established before this reload.  The
  ;; core intrinsic-initial-value table supplies nil per buffer independently
  ;; of the variable's mutable default.
  (unless (boundp 'buffer-file-name)
    (setq buffer-file-name nil))
  (emacs-buffer-declare-per-buffer 'buffer-file-name nil)
  ;; GNU files.el reads this intrinsic slot before assigning a visited name.
  (defvar buffer-auto-save-file-name nil
    "Name of the current buffer's auto-save file, or nil.")
  (emacs-buffer-declare-per-buffer 'buffer-auto-save-file-name nil)
  (if (boundp 'default-directory)
      (emacs-buffer-declare-per-buffer 'default-directory default-directory)
    (emacs-buffer-declare-per-buffer 'default-directory)))

(defun emacs-buffer-builtins--buffer-invisibility-spec ()
  "Return the current buffer's `buffer-invisibility-spec' value."
  (if (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'nelisp-ec-current-buffer)
           (nelisp-ec-current-buffer)
           (fboundp 'emacs-buffer-local-variable-p)
           (emacs-buffer-local-variable-p 'buffer-invisibility-spec
                                          (nelisp-ec-current-buffer)))
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-buffer-local-value
       (list 'buffer-invisibility-spec (nelisp-ec-current-buffer)))
    buffer-invisibility-spec))

(defun emacs-buffer-builtins--set-buffer-invisibility-spec (value)
  "Set the current buffer's `buffer-invisibility-spec' to VALUE."
  (if (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'nelisp-ec-current-buffer)
           (nelisp-ec-current-buffer))
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-set-buffer-local-value
       (list 'buffer-invisibility-spec (nelisp-ec-current-buffer) value))
    (setq buffer-invisibility-spec value)))

(defun emacs-buffer-builtins-add-to-invisibility-spec (element)
  "Add ELEMENT to `buffer-invisibility-spec'."
  (let ((spec (emacs-buffer-builtins--buffer-invisibility-spec)))
    (when (eq spec t)
      (setq spec (list t)))
    (emacs-buffer-builtins--set-buffer-invisibility-spec
     (cons element spec))))

(defun emacs-buffer-builtins-remove-from-invisibility-spec (element)
  "Remove ELEMENT from `buffer-invisibility-spec'."
  (let ((spec (emacs-buffer-builtins--buffer-invisibility-spec)))
    (emacs-buffer-builtins--set-buffer-invisibility-spec
     (if (consp spec)
         (delete element spec)
       (list t)))))

(defun emacs-buffer-builtins-invisible-p (prop)
  "Return t or 2 when PROP is invisible in the current buffer.
An integer or marker denotes a position whose `invisible' character
property is used.  A matching cons with non-nil cdr requests ellipses."
  (when (or (integerp prop) (markerp prop))
    (setq prop (get-char-property
                (emacs-buffer-builtins--position prop) 'invisible)))
  (let ((spec (and (boundp 'buffer-invisibility-spec)
                   (emacs-buffer-builtins--buffer-invisibility-spec)))
        (result nil))
    (cond
     ((null prop) nil)
     ((eq spec t) t)
     (t
      (while (and (consp spec) (not (eq result 2)))
        (let* ((entry (car spec))
               (key (if (consp entry) (car entry) entry)))
          (when (or (eq prop key) (and (consp prop) (memq key prop)))
            (setq result (if (and (consp entry) (cdr entry)) 2 t))))
        (setq spec (cdr spec)))
      result))))

(when (emacs-buffer-builtins--install-function-p 'put-text-property)
  (defun put-text-property (&rest args)
    "Set text property PROP to VALUE on buffer or string OBJECT."
    (let ((count (length args)))
      (when (or (< count 4) (> count 5))
        (signal 'wrong-number-of-arguments (list 'put-text-property count))))
    (let* ((start (nth 0 args)) (end (nth 1 args))
           (prop (nth 2 args)) (value (nth 3 args)) (object (nth 4 args))
           (target (emacs-buffer-builtins--property-target object))
           (range (emacs-buffer-builtins--property-range
                   (emacs-buffer-builtins--checked-position start)
                   (emacs-buffer-builtins--checked-position end) target)))
      (setq start (car range) end (cadr range))
      (unless (= start end)
        (if (stringp target)
            (emacs-buffer-builtins--call-string-property
             'put target start end prop value)
          (emacs-buffer-builtins--call-emacs-buffer
           'emacs-buffer-put-text-property
           (list start end prop value target))))
      nil)))

(when (emacs-buffer-builtins--install-function-p 'get-text-property)
  (defun get-text-property (pos prop &optional object)
    "Return text property PROP at POS on buffer or string OBJECT."
    (let ((target (emacs-buffer-builtins--property-target object)))
      (setq pos (emacs-buffer-builtins--property-read-position pos target))
      (if (stringp target)
          (emacs-buffer-builtins--call-string-property 'get target pos prop)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-get-text-property (list pos prop target))))))

(defun emacs-buffer-builtins--get-text-property-around (orig pos prop &optional object)
  "Read sidecar properties while preserving native `get-text-property'."
  (let* ((native-value (funcall orig pos prop object))
         (pos (emacs-buffer-builtins--position pos))
         (target (emacs-buffer-builtins--text-property-object object))
         (managed
          (cond
           ((stringp target)
           (gethash target emacs-buffer--string-state))
           ((and target (not (eq target :string-or-unsupported)))
            (gethash target emacs-buffer--state))))
         (sidecar-value
          (cond
           ((stringp target)
            (emacs-buffer-builtins--call-string-property
             'get target pos prop))
           ((not (eq target :string-or-unsupported))
            (emacs-buffer-builtins--call-emacs-buffer
             'emacs-buffer-get-text-property
             (list pos prop target))))))
    (if managed sidecar-value native-value)))

(defun emacs-buffer-builtins--whole-buffer-size (&optional buffer)
  "Return BUFFER's total character count, independent of narrowing."
  (setq buffer (or buffer (current-buffer)))
  (unless (bufferp buffer)
    (signal 'wrong-type-argument (list 'bufferp buffer)))
  (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer))
      (nelisp-buffer-size buffer)
    (nelisp-ec-buffer-size buffer)))

(when (fboundp 'nelisp--repr)
  (defalias 'buffer-size #'emacs-buffer-builtins--whole-buffer-size))

(when (and (fboundp 'nelisp--buffer-multibyte-p)
           (fboundp 'get-text-property))
  (advice-add 'get-text-property :around
              #'emacs-buffer-builtins--get-text-property-around))

(defun emacs-buffer-builtins--text-properties-at-around (orig pos &optional object)
  "Read complete managed sidecar plists before native text properties."
  (let* ((arguments
          (emacs-buffer-builtins--text-properties-at-arguments pos object))
         (pos (car arguments))
         (target (cdr arguments)))
    ;; Validate before ORIG: the native reader has different range errors.
    (unless (and (bufferp target) (not (buffer-live-p target)))
      (let* ((native-value (funcall orig pos object))
             (managed
              (if (stringp target)
                  (gethash target emacs-buffer--string-state)
                (gethash target emacs-buffer--state)))
             (sidecar-value
              (if (stringp target)
                  (emacs-buffer-builtins--call-string-property 'at target pos)
                (emacs-buffer-builtins--call-emacs-buffer
                 'emacs-buffer-text-property-at (list pos target)))))
        (if managed sidecar-value native-value)))))

(when (and (fboundp 'nelisp--buffer-multibyte-p)
           (fboundp 'text-properties-at))
  (advice-add 'text-properties-at :around
              #'emacs-buffer-builtins--text-properties-at-around))

(defun emacs-buffer-builtins--text-property-mutation-around
    (operation orig start end &rest args)
  "Mirror native text-property mutation OPERATION into the sidecar."
  (let* ((result (apply orig start end args))
         (put-p (eq operation 'put))
         (payload (car args))
         (value (cadr args))
         (object (if put-p (nth 2 args) (nth 1 args)))
         (target (emacs-buffer-builtins--text-property-object object)))
    (when (and (not (eq target :string-or-unsupported))
               (not (and (eq operation 'add) (null payload))))
      (when (memq operation '(put add))
        (let ((range (emacs-buffer-builtins--property-range start end target)))
          (setq start (car range) end (cadr range))))
      (unless (= start end)
        (if (stringp target)
          (emacs-buffer-builtins--call-string-property
           operation target start end payload value)
        (if (and (fboundp 'nelisp-ec-buffer-p)
                 (nelisp-ec-buffer-p target))
            (emacs-buffer-builtins--call-emacs-buffer
             (pcase operation
               ('put 'emacs-buffer-put-text-property)
               ('add 'emacs-buffer-add-text-properties)
               ('remove 'emacs-buffer-remove-text-properties)
               ('set 'emacs-buffer-set-text-properties))
             (pcase operation
               ('put (list start end payload value target))
               (_ (list start end payload target))))
          (emacs-buffer-builtins--call-emacs-buffer
           'emacs-buffer--native-buffer-text-property-mutation
           (append (list operation target start end)
                   (if put-p (list payload value) (list payload))))))))
    (when (and (not (stringp target))
               (not (eq target :string-or-unsupported))
               (nelisp-buffer-p target))
      (emacs-buffer-builtins--share-text target))
    result))

(defun emacs-buffer-builtins--put-text-property-around
    (orig &rest args)
  "Standalone sidecar advice for native `put-text-property'."
  (let ((count (length args)))
    (when (or (< count 4) (> count 5))
      (signal 'wrong-number-of-arguments (list 'put-text-property count))))
  (let ((start (nth 0 args)) (end (nth 1 args))
        (prop (nth 2 args)) (value (nth 3 args)) (object (nth 4 args)))
    (emacs-buffer-builtins--property-target object)
    (setq start (emacs-buffer-builtins--checked-position start)
          end (emacs-buffer-builtins--checked-position end))
    (emacs-buffer-builtins--text-property-mutation-around
     'put orig start end prop value object)))

(defun emacs-buffer-builtins--add-text-properties-around
    (orig start end properties &optional object)
  "Standalone sidecar advice for native `add-text-properties'."
  (emacs-buffer-builtins--text-property-mutation-around
   'add orig start end properties object))

(defun emacs-buffer-builtins--remove-text-properties-around
    (orig start end properties &optional object)
  "Standalone sidecar advice for native `remove-text-properties'."
  (emacs-buffer-builtins--text-property-mutation-around
   'remove orig start end properties object))

(defun emacs-buffer-builtins--set-text-properties-around
    (orig start end properties &optional object)
  "Standalone sidecar advice for native `set-text-properties'."
  (emacs-buffer-builtins--text-property-mutation-around
   'set orig start end properties object))

(when (fboundp 'nelisp--buffer-multibyte-p)
  (dolist (entry '((put-text-property . emacs-buffer-builtins--put-text-property-around)
                   (add-text-properties . emacs-buffer-builtins--add-text-properties-around)
                   (remove-text-properties . emacs-buffer-builtins--remove-text-properties-around)
                   (set-text-properties . emacs-buffer-builtins--set-text-properties-around)))
    (when (fboundp (car entry))
      (advice-add (car entry) :around (cdr entry)))))

(defun emacs-buffer-builtins-text-properties-at (pos &optional object)
  "Return all text properties at POS in buffer or string OBJECT."
  (let* ((arguments
          (emacs-buffer-builtins--text-properties-at-arguments pos object))
         (pos (car arguments))
         (target (cdr arguments)))
    (unless (and (bufferp target) (not (buffer-live-p target)))
      (if (stringp target)
          (emacs-buffer-builtins--call-string-property 'at target pos)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-text-property-at (list pos target))))))

(when (emacs-buffer-builtins--install-function-p 'text-properties-at)
  (defalias 'text-properties-at
    #'emacs-buffer-builtins-text-properties-at))

(when (emacs-buffer-builtins--install-function-p 'get-char-property)
  (defun get-char-property (pos prop &optional object)
    "Return char property PROP at POS on buffer OBJECT."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (eq target :string-or-unsupported)
        (if (stringp target)
            (emacs-buffer-builtins--call-string-property 'get target pos prop)
          (emacs-buffer-builtins--call-emacs-buffer
           'emacs-buffer-get-char-property
           (list pos prop target)))))))

(when (emacs-buffer-builtins--install-function-p 'invisible-p)
  (defalias 'invisible-p #'emacs-buffer-builtins-invisible-p))

(when (emacs-buffer-builtins--install-function-p 'add-to-invisibility-spec)
  (defalias 'add-to-invisibility-spec
    #'emacs-buffer-builtins-add-to-invisibility-spec))

(when (emacs-buffer-builtins--install-function-p 'remove-from-invisibility-spec)
  (defalias 'remove-from-invisibility-spec
    #'emacs-buffer-builtins-remove-from-invisibility-spec))

(defun emacs-buffer-builtins-next-property-change (pos &optional object limit)
  "Return next property change after POS in OBJECT.
String property scans are not yet represented in the standalone
substrate, so unsupported objects return LIMIT or nil."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-next-property-change
       (list pos target limit)))))

(defun emacs-buffer-builtins-previous-property-change (pos &optional object limit)
  "Return previous property change before POS in OBJECT.
String property scans are not yet represented in the standalone
substrate, so unsupported objects return LIMIT or nil."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-previous-property-change
       (list pos target limit)))))

(defun emacs-buffer-builtins-next-single-property-change
    (pos prop &optional object limit)
  "Return next change after POS for text property PROP in OBJECT."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-next-single-property-change
       (list pos prop target limit)))))

(defun emacs-buffer-builtins-previous-single-property-change
    (pos prop &optional object limit)
  "Return previous change before POS for text property PROP in OBJECT."
  (let ((target (emacs-buffer-builtins--text-property-object object)))
    (if (eq target :string-or-unsupported)
        limit
      (emacs-buffer-builtins--call-emacs-buffer
       'emacs-buffer-previous-single-property-change
       (list pos prop target limit)))))

(when (emacs-buffer-builtins--install-function-p 'next-property-change)
  (defalias 'next-property-change
    #'emacs-buffer-builtins-next-property-change))

(when (emacs-buffer-builtins--install-function-p 'previous-property-change)
  (defalias 'previous-property-change
    #'emacs-buffer-builtins-previous-property-change))

(when (emacs-buffer-builtins--install-function-p 'next-single-property-change)
  (defalias 'next-single-property-change
    #'emacs-buffer-builtins-next-single-property-change))

(when (emacs-buffer-builtins--install-function-p 'previous-single-property-change)
  (defalias 'previous-single-property-change
    #'emacs-buffer-builtins-previous-single-property-change))

(defun emacs-buffer-builtins--single-char-property-change
    (pos prop object limit backward)
  "Scan PROP in OBJECT from POS toward LIMIT, including overlays.
BACKWARD selects the previous boundary.  Compare values using `eq'."
  (let* ((target (emacs-buffer-builtins--property-target object))
         (bounds (emacs-buffer-builtins--property-bounds target))
         (lo (car bounds)) (hi (cadr bounds))
         (string (stringp target)))
    (setq pos (emacs-buffer-builtins--position pos))
    (when (or (< pos lo) (> pos hi))
      (signal 'args-out-of-range
              (if string (list pos pos) (list pos))))
    (when limit (setq limit (emacs-buffer-builtins--position limit)))
    (let* ((stop (or limit (if backward lo hi)))
           (edge (if backward (max lo stop) (min hi stop)))
           (scan (if backward (1- pos) pos))
           (read-property
            (lambda (at)
              (if string (get-text-property at prop target)
                (with-current-buffer target (get-char-property at prop)))))
           (value (and (>= scan lo) (< scan hi)
                       (funcall read-property scan)))
           (found nil))
      (if backward
          (while (and (> scan edge) (not found))
            (if (eq value (funcall read-property (1- scan)))
                (setq scan (1- scan))
              (setq found scan)))
        (setq scan (1+ scan))
        (while (and (< scan edge) (not found))
          (if (eq value (funcall read-property scan))
              (setq scan (1+ scan))
            (setq found scan))))
      (or found stop))))

(when (emacs-buffer-builtins--install-function-p 'next-single-char-property-change)
  (defun next-single-char-property-change (pos prop &optional object limit)
    "Return the next change of PROP, considering overlays and text properties."
    (emacs-buffer-builtins--single-char-property-change
     pos prop object limit nil)))

(when (emacs-buffer-builtins--install-function-p 'previous-single-char-property-change)
  (defun previous-single-char-property-change (pos prop &optional object limit)
    "Return the previous change of PROP, considering overlays and text properties."
    (emacs-buffer-builtins--single-char-property-change
     pos prop object limit t)))

(when (emacs-buffer-builtins--install-function-p 'add-text-properties)
  (defun add-text-properties (start end props &optional object)
    "Add text PROPS on buffer or string OBJECT; return t if changed."
    (let ((tail (if (and props (not (consp props))) (list props nil) props))
          (plist nil) (changed nil))
      ;; GNU checks the property list first, even on an empty range.
      (while (consp tail)
        (unless (consp (cdr tail))
          (error "Odd length text property list"))
        (setq plist (append plist (list (car tail) (cadr tail)))
              tail (cddr tail)))
      (when plist
        (let* ((target (emacs-buffer-builtins--property-target object))
               (range (emacs-buffer-builtins--property-range start end target)))
          (setq start (car range) end (cadr range))
          (let ((pos start))
            (while (and (< pos end) (not changed))
              (let ((existing (text-properties-at pos target)) (rest plist))
                (while (and rest (not changed))
                  (unless (and (plist-member existing (car rest))
                               (eq (plist-get existing (car rest)) (cadr rest)))
                    (setq changed t))
                  (setq rest (cddr rest))))
              (setq pos (1+ pos))))
          (when changed
            (if (stringp target)
                (emacs-buffer-builtins--call-string-property
                 'add target start end plist)
              (emacs-buffer-builtins--call-emacs-buffer
               'emacs-buffer-add-text-properties (list start end plist target))))))
      changed)))

(when (emacs-buffer-builtins--install-function-p 'remove-text-properties)
  (defun remove-text-properties (start end props &optional object)
    "Remove text PROPS on buffer or string OBJECT."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported) (>= start end))
        (if (stringp target)
            (emacs-buffer-builtins--call-string-property
             'remove target start end props)
          (emacs-buffer-builtins--call-emacs-buffer
           'emacs-buffer-remove-text-properties
           (list start end props target)))))))

(when (emacs-buffer-builtins--install-function-p 'set-text-properties)
  (defun set-text-properties (start end props &optional object)
    "Set text PROPS on buffer or string OBJECT."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (unless (or (eq target :string-or-unsupported) (>= start end))
        (if (stringp target)
            (emacs-buffer-builtins--call-string-property
             'set target start end props)
          (emacs-buffer-builtins--call-emacs-buffer
           'emacs-buffer-set-text-properties
           (list start end props target)))))))

(when (emacs-buffer-builtins--install-function-p 'text-property-not-all)
  (defun text-property-not-all (start end prop value &optional object)
    "Return the position in [START, END) of OBJECT where PROP first
differs (via `eq') from VALUE, or nil if it never does.  OBJECT may be
a buffer, nil for the current buffer, or a string."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (if (stringp target)
          (emacs-buffer-builtins--call-string-property
           'not-all target start end prop value)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-text-property-not-all
         (list start end prop value target))))))

(when (emacs-buffer-builtins--install-function-p 'text-property-any)
  (defun text-property-any (start end prop value &optional object)
    "Return the position in [START, END) of OBJECT where PROP first
matches VALUE via `eq', or nil if it never does.  OBJECT may be a
buffer, nil for the current buffer, or a string."
    (let ((target (emacs-buffer-builtins--text-property-object object)))
      (if (stringp target)
          (emacs-buffer-builtins--call-string-property
           'any target start end prop value)
        (emacs-buffer-builtins--call-emacs-buffer
         'emacs-buffer-text-property-any
         (list start end prop value target))))))

(defun emacs-buffer-builtins-ensure-initial-buffer (&optional name)
  "Ensure standalone NeLisp has a selected initial buffer.
NAME defaults to \"*scratch*\".  If a current buffer already exists,
return it.  Otherwise reuse an existing buffer named NAME or create it,
select it, and return it."
  (let* ((buffer-name (or name "*scratch*"))
         (buf (or (nelisp-ec-current-buffer)
                  (cdr (assoc buffer-name nelisp-ec--buffers))
                  (nelisp-ec-generate-new-buffer buffer-name))))
    (unless (eq (nelisp-ec-current-buffer) buf)
      (nelisp-ec-set-buffer buf))
    buf))

(when (and (emacs-buffer-builtins--standalone-p)
           (not (nelisp-ec-current-buffer)))
  (emacs-buffer-builtins-ensure-initial-buffer))

;;;; --- overlays ---------------------------------------------------------

;; Overlay support lives in `emacs-buffer.el', which is large because it
;; also carries text-properties, buffer-local variables, modified ticks,
;; and undo metadata.  Standalone bootstrap does not need that whole layer
;; until an overlay API is actually called, so these unprefixed wrappers
;; lazy-load it on demand.

(when (emacs-buffer-builtins--install-function-p 'overlayp)
  (defun overlayp (object)
    "Return non-nil if OBJECT is an overlay."
    (and (fboundp 'emacs-buffer-overlayp)
         (emacs-buffer-overlayp object))))

(defun emacs-buffer-builtins--call-overlay (function args)
  "Call overlay FUNCTION with the coherent native buffer owner in ARGS."
  (let* ((buffer (current-buffer))
         (slot (cdr (assq function
                          '((emacs-buffer-make-overlay . 3)
                            (emacs-buffer-move-overlay . 4)
                            (emacs-buffer-remove-overlays . 5)
                            (emacs-buffer-overlays-at . 2)
                            (emacs-buffer-overlays-in . 3)
                            (emacs-buffer-next-overlay-change . 2)
                            (emacs-buffer-previous-overlay-change . 2)
                            (emacs-buffer-overlay-lists . 1))))))
    (when (and slot (not (nelisp-ec-buffer-p buffer)))
      (setq args (copy-sequence args))
      (while (< (length args) slot)
        (setq args (append args (list nil))))
      (when (or (null (nth (1- slot) args))
                (eq function 'emacs-buffer-overlays-at))
        (setcar (nthcdr (1- slot) args) buffer)))
    (emacs-buffer-builtins--call-emacs-buffer function args)))

(defun emacs-buffer-builtins--checked-position (position)
  "Return POSITION as an integer, rejecting detached markers."
  (when (and (markerp position) (not (marker-position position)))
    (signal 'error '("Marker does not point anywhere")))
  (emacs-buffer-builtins--position position))

(defun emacs-buffer-builtins--overlay-operation (name function args)
  "Apply overlay NAME with GNU argument checks and detached overlay semantics."
  (let* ((arity (cdr (assq name
                         '((overlay-start 1 1) (overlay-end 1 1)
                           (overlay-buffer 1 1) (overlay-properties 1 1)
                           (overlay-put 3 3) (overlay-get 2 2)
                           (move-overlay 3 4) (delete-overlay 1 1)
                           (overlays-at 1 2) (overlays-in 2 2)
                           (previous-overlay-change 1 1)
                           (overlay-lists 0 0)))))
         (count (length args)))
    (when (or (< count (car arity)) (> count (cadr arity)))
      (signal 'wrong-number-of-arguments (list name count))))
  (let ((overlay (car args)))
    (when (memq name '(overlay-start overlay-end overlay-buffer
                      overlay-properties overlay-put overlay-get
                      move-overlay delete-overlay))
      (unless (emacs-buffer-overlayp overlay)
        (signal 'wrong-type-argument (list 'overlayp overlay)))
      ;; Buffer deletion detaches GNU overlays.  Observe that lazily so no
      ;; buffer-wide sweep or kill hook is required by the bridge.
      (let ((buffer (emacs-buffer--overlay-rec-buffer overlay)))
        (when (and buffer (not (buffer-live-p buffer)))
          (setf (emacs-buffer--overlay-rec-buffer overlay) nil))))
    (cond
     ((eq name 'overlay-properties)
      (copy-sequence (emacs-buffer--overlay-rec-properties overlay)))
     ((eq name 'overlay-put)
      (setf (emacs-buffer--overlay-rec-properties overlay)
            (plist-put (emacs-buffer--overlay-rec-properties overlay)
                       (cadr args) (nth 2 args)))
      (nth 2 args))
     ((eq name 'overlay-get)
      (let* ((property (cadr args))
             (properties (emacs-buffer--overlay-rec-properties overlay))
             (value (plist-get properties property))
             (category (plist-get properties 'category)))
        (if (plist-member properties property) value
          (and (symbolp category) category (get category property)))))
     ((eq name 'move-overlay)
      (let* ((old (emacs-buffer--overlay-rec-buffer overlay))
             (buffer (or (nth 3 args) old (current-buffer))))
        (unless (bufferp buffer)
          (signal 'wrong-type-argument (list 'bufferp buffer)))
        (unless (buffer-live-p buffer)
          (signal 'error '("Attempt to move overlay to a dead buffer")))
        (dolist (position (list (cadr args) (nth 2 args)))
          (when (and (markerp position) (not (eq (marker-buffer position) buffer)))
            (signal 'error (list "Marker points into wrong buffer" position))))
        (let* ((start (emacs-buffer-builtins--checked-position (cadr args)))
               (end (emacs-buffer-builtins--checked-position (nth 2 args)))
               (limit (1+ (buffer-size buffer)))
               (low (max 1 (min limit (min start end))))
               (high (max 1 (min limit (max start end)))))
          (when old (emacs-buffer--overlay-remove-rec old overlay))
          (setf (emacs-buffer--overlay-rec-start overlay) low
                (emacs-buffer--overlay-rec-end overlay) high
                (emacs-buffer--overlay-rec-buffer overlay) buffer)
          (emacs-buffer--overlay-insert-sorted buffer overlay)
          overlay)))
     ((eq name 'overlay-lists)
      (let ((ext (gethash (current-buffer) emacs-buffer--state)))
        (list (and ext (copy-sequence (emacs-buffer--ext-overlays ext))))))
     ((eq name 'overlays-at)
      (let* ((position (emacs-buffer-builtins--checked-position (car args)))
             (overlays (emacs-buffer-overlays-at position (current-buffer))))
        (if (not (cadr args)) overlays
          (sort overlays #'emacs-buffer-builtins--overlay-priority-less-p))))
     ((eq name 'overlays-in)
      (let* ((start (emacs-buffer-builtins--checked-position (car args)))
             (end (emacs-buffer-builtins--checked-position (cadr args)))
             (ext (gethash (current-buffer) emacs-buffer--state)) result)
        (when (and ext (<= start end))
          (dolist (ov (emacs-buffer--ext-overlays ext))
            (let ((low (emacs-buffer--overlay-rec-start ov))
                  (high (emacs-buffer--overlay-rec-end ov)))
              (when (or (and (< low end) (> high start))
                        (and (= low high) (<= start low)
                             (or (< low end) (= start end low))))
                (push ov result)))))
        (nreverse result)))
     ((eq name 'previous-overlay-change)
      (let* ((position (emacs-buffer-builtins--checked-position (car args)))
             (previous (point-min))
             (ext (gethash (current-buffer) emacs-buffer--state)))
        (when ext
          (dolist (ov (emacs-buffer--ext-overlays ext))
            (let ((low (emacs-buffer--overlay-rec-start ov))
                  (high (emacs-buffer--overlay-rec-end ov)))
              (when (and (< low position) (> low previous))
                (setq previous low))
              (when (and (< high position) (> high previous))
                (setq previous high)))))
        previous))
     (t (emacs-buffer-builtins--call-overlay function args)))))

(defun emacs-buffer-builtins--overlay-priority-less-p (left right)
  "Return non-nil if LEFT has greater overlay priority than RIGHT."
  (let* ((left-value (overlay-get left 'priority))
         (right-value (overlay-get right 'priority))
         (left-primary (if (consp left-value) (car left-value) left-value))
         (right-primary (if (consp right-value) (car right-value) right-value))
         (left-secondary (and (consp left-value) (cdr left-value)))
         (right-secondary (and (consp right-value) (cdr right-value)))
         (left-start (overlay-start left))
         (left-end (overlay-end left))
         (right-start (overlay-start right))
         (right-end (overlay-end right)))
    (setq left-primary (if (integerp left-primary) left-primary 0)
          right-primary (if (integerp right-primary) right-primary 0))
    (cond
     ((/= left-primary right-primary) (> left-primary right-primary))
     ((and (>= left-start right-start) (<= left-end right-end)
           (or (> left-start right-start) (< left-end right-end))) t)
     ((and (>= right-start left-start) (<= right-end left-end)
           (or (> right-start left-start) (< right-end left-end))) nil)
     (t (> (if (integerp left-secondary) left-secondary 0)
           (if (integerp right-secondary) right-secondary 0))))))

(dolist (--cell--
         '((make-overlay       . emacs-buffer-make-overlay)
           (overlay-start      . emacs-buffer-overlay-start)
           (overlay-end        . emacs-buffer-overlay-end)
           (overlay-buffer     . emacs-buffer-overlay-buffer)
           (overlay-properties . emacs-buffer-overlay-properties)
           (overlay-put        . emacs-buffer-overlay-put)
           (overlay-get        . emacs-buffer-overlay-get)
           (move-overlay       . emacs-buffer-move-overlay)
           (delete-overlay     . emacs-buffer-delete-overlay)
           (remove-overlays    . emacs-buffer-remove-overlays)
           (overlays-at        . emacs-buffer-overlays-at)
           (overlays-in        . emacs-buffer-overlays-in)
           (next-overlay-change . emacs-buffer-next-overlay-change)
           (previous-overlay-change . emacs-buffer-previous-overlay-change)
           (overlay-lists      . emacs-buffer-overlay-lists)
           (copy-overlay       . emacs-buffer-copy-overlay)))
  (let ((--name-- (car --cell--))
        (--target-- (cdr --cell--)))
    (when (emacs-buffer-builtins--install-function-p --name--)
      (fset --name--
            (list 'lambda '(&rest args)
                  (if (memq --name-- '(overlay-start overlay-end overlay-buffer
                                      overlay-properties overlay-put overlay-get
                                      move-overlay delete-overlay overlays-at
                                      overlays-in previous-overlay-change overlay-lists))
                      (list 'emacs-buffer-builtins--overlay-operation
                            (list 'quote --name--)
                            (list 'quote --target--) 'args)
                    (list 'emacs-buffer-builtins--call-overlay
                          (list 'quote --target--) 'args)))))))

(when (emacs-buffer-builtins--install-function-p 'overlay-recenter)
  (defun overlay-recenter (pos)
    "Set the overlay search center to POS in the current buffer.
The sidecar stores overlays in a flat list, so no repartition is needed."
    (emacs-buffer-builtins--position pos)
    nil))

;;;; --- creation / liveness -----------------------------------------------

(when (emacs-buffer-builtins--install-function-p 'buffer-live-p)
  (defun buffer-live-p (object)
    "Return non-nil when OBJECT is a live (non-killed) buffer."
    (and (nelisp-ec-buffer-p object)
         (not (nelisp-ec-buffer-killed-p object)))))

(when (emacs-buffer-builtins--install-function-p 'buffer-name)
  (defun buffer-name (&optional buffer)
    "Return the name of BUFFER (default = current buffer)."
    (cond
     ((null buffer)
      (let ((b (nelisp-ec-current-buffer)))
        (and b (nelisp-ec-buffer-name b))))
     ((nelisp-ec-buffer-p buffer)
      (nelisp-ec-buffer-name buffer))
     (t nil))))

;; Doc 200 adds a distinct unibyte representation for strings, but this
;; compatibility bridge does not implement Emacs's in-place buffer storage
;; conversion.  Measured on NeLisp v1.1.0+1 after loading this bridge, a fresh
;; `nelisp-ec' buffer produced `(t nil t t t)' for (MODE-BEFORE RETURN-NIL
;; MODE-AFTER-NIL RETURN-T MODE-AFTER-T).  Preserve that buffer-layer behavior:
;; return FLAG without changing the underlying `nelisp-text-buffer' mode.
(when (emacs-buffer-builtins--install-function-p 'set-buffer-multibyte)
  (defun set-buffer-multibyte (flag)
    flag))

;; NeLisp v1.1.0 (Doc 200) gave unibyte strings their own representation,
;; so the runtime's own `multibyte-string-p' answers as Emacs does: nil for
;; a pure-ASCII string and nil for a unibyte one.  Installing the bridge
;; stub over it would report every string as multibyte again.  Ask the
;; runtime about a pure-ASCII string and keep the stub only for a reader
;; that predates Doc 200 (or has no `multibyte-string-p' at all).
(when (and (emacs-buffer-builtins--install-function-p 'multibyte-string-p)
           (condition-case nil (multibyte-string-p "a") (error t)))
  (defun multibyte-string-p (object)
    (stringp object)))

;;;; --- registry lookup (Phase L1, 2026-05-03) --------------------------

;; S2 coverage batch 5 (2026-09-28): on the standalone, a NATIVE `get-buffer'
;; primitive is already `fboundp' at this point (bound to the reader's own
;; buffer family: `nelisp-buffer-p'/`nelisp-get-buffer', operating on the
;; *scratch* buffer the runtime creates at startup), so
;; the buffer-builtins install predicate used to skip installing this
;; polyfill entirely.  That native `get-buffer' has no notion of this
;; bridge's own `nelisp-ec-buffer' struct (the type `buffer-list' below
;; returns), so `(get-buffer SOME-NELISP-EC-BUFFER)' fell through its `cond'
;; to a `(signal (list \\='stringp buffer-or-name))' clause -- observed via
;; `(with-current-buffer (car (buffer-list)) ...)', which expands (using the
;; native `with-current-buffer') to exactly that call.  Real Emacs's own
;; `get-buffer' accepts either a buffer or a name, so this is a genuine gap,
;; not a deliberate native/bridge split.  Fixed by always installing this
;; polyfill on the standalone (`emacs-buffer-builtins--standalone-p'), and
;; keeping the pre-existing implementation (native primitive or an earlier
;; stub) as an explicit fallback for anything this bridge's own registry
;; does not know about -- e.g. `(get-buffer "*Messages*")' when the native
;; `message' primitive created that buffer natively before this bridge's
;; own `nelisp-ec-generate-new-buffer' ever ran (the same native-buffer case
;; `emacs-special-buffers--ensure-core-buffer' already documents above).
;; Host Emacs is unaffected: `emacs-buffer-builtins--standalone-p' is nil
;; there, so the original `install-function-p' gate alone still applies and
;; the genuine C `get-buffer' is left untouched.
(defvar emacs-buffer-builtins--native-get-buffer
  (and (fboundp 'get-buffer) (symbol-function 'get-buffer))
  "The `get-buffer' implementation in place before this file's own
polyfill replaces it (a native primitive on the standalone, or an earlier
stub).  `get-buffer' below falls back to this for anything it does not
recognize as one of its own `nelisp-ec-buffer' structs or registry
entries, so buffers created outside this bridge keep resolving exactly
as before.")

(when (or (emacs-buffer-builtins--install-function-p 'get-buffer)
          (emacs-buffer-builtins--standalone-p))
  (defun get-buffer (buffer-or-name)
    "Phase L1 polyfill: look BUFFER-OR-NAME up in the `nelisp-ec' registry.
When BUFFER-OR-NAME is a buffer object, return it if live else nil.
When it is a string, return the matching buffer record if this bridge
has one; otherwise fall back to `emacs-buffer-builtins--native-get-buffer'
(covers native-only buffers such as *Messages*, and host Emacs's own
buffers when this polyfill is active there)."
    (cond
     ((null buffer-or-name) nil)
     ((nelisp-ec-buffer-p buffer-or-name)
      (if (nelisp-ec-buffer-killed-p buffer-or-name)
          nil
        buffer-or-name))
     ((and (stringp buffer-or-name) (assoc buffer-or-name nelisp-ec--buffers))
      (cdr (assoc buffer-or-name nelisp-ec--buffers)))
     (emacs-buffer-builtins--native-get-buffer
      (funcall emacs-buffer-builtins--native-get-buffer buffer-or-name))
     (t nil))))

(when (emacs-buffer-builtins--install-function-p 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    "Phase L1 polyfill: get an existing buffer or create a fresh one.
INHIBIT-BUFFER-HOOKS is accepted for API parity but no buffer-hook
subsystem exists yet to honor it.

Creates through the plain `generate-new-buffer' symbol, not the
`nelisp-ec'-prefixed constructor, so the object returned is always the
same buffer representation the currently active `get-buffer' recognizes
-- mirroring GNU's own `Fget_buffer_create' (buffer.c), which builds
through the same primitives `Fget_buffer' uses and never a foreign
buffer representation.  On a standalone image whose prelude already
ships a complete native buffer family (`get-buffer'/`bufferp'/
`current-buffer'/`generate-new-buffer' already `fboundp', so
the buffer-builtins install predicate leaves that family installed
as-is per its C-subr-preservation policy), `generate-new-buffer'
resolves to the SAME native constructor `get-buffer' already
understands.  Calling `nelisp-ec-generate-new-buffer' unconditionally
here used to hand back this repo's own `nelisp-ec-buffer' struct even on
such an image, which the native `get-buffer'/`with-current-buffer' only
recognize via their own `nelisp-buffer-p' record -- the very next lookup
of that buffer then hit their `(t (signal \\='wrong-type-argument
(list \\='stringp buffer-or-name)))' fallback (see
`test/nemacs-process-sync-smoke.el' and
`emacs-buffer-builtins-test/get-buffer-create-buffer-is-recognized-by-get-buffer')."
    (ignore inhibit-buffer-hooks)
    (or (get-buffer buffer-or-name)
        (generate-new-buffer
         (cond
          ((stringp buffer-or-name) buffer-or-name)
          ((nelisp-ec-buffer-p buffer-or-name)
           (nelisp-ec-buffer-name buffer-or-name))
          (t " *unnamed*"))))))

;; The native constructor owns buffer identity, while this shared boundary
;; supplies GNU's caller-directory inheritance for fresh buffers only.
(when (and (fboundp 'nelisp--write-stdout-bytes)
           (fboundp 'nelisp-get-buffer-create))
  (defun emacs-buffer-builtins--inherit-native-buffer-directory
      (original buffer-or-name &rest args)
    "Copy the caller's directory when ORIGINAL creates a new buffer."
    (let ((existing (get-buffer buffer-or-name))
          (directory (and (boundp 'default-directory) default-directory)))
      (let ((buffer (apply original buffer-or-name args)))
        (when (and (not existing) (stringp directory))
          (emacs-buffer-builtins--call-emacs-buffer
           #'emacs-buffer-set-buffer-local-value
           (list 'default-directory buffer directory)))
        buffer)))
  (advice-add 'nelisp-get-buffer-create :around
              #'emacs-buffer-builtins--inherit-native-buffer-directory))

(when (emacs-buffer-builtins--install-function-p 'buffer-list)
  (defun buffer-list (&optional frame)
    "Phase L1 polyfill: return a list of every live buffer in the registry.
FRAME is accepted for API parity (host filters by frame) but the
prefixed substrate has no per-frame buffer affinity, so all live
buffers are returned regardless."
    (ignore frame)
    (let ((acc nil))
      (dolist (cell nelisp-ec--buffers)
        (let ((buf (cdr cell)))
          (when (and buf (not (nelisp-ec-buffer-killed-p buf)))
            (setq acc (cons buf acc)))))
      ;; Reverse for registry-insertion order (= push above prepended).
      (let ((rev nil))
        (while acc
          (setq rev (cons (car acc) rev))
          (setq acc (cdr acc)))
        ;; A native runtime owns one coherent public buffer family.  Legacy
        ;; compatibility objects cannot be passed to its current-buffer,
        ;; bufferp or indirect-buffer APIs.  Their private registry remains
        ;; available to explicit nelisp-ec consumers, not GNU buffer-list.
        (if (fboundp 'nelisp-buffer-list)
            (nelisp-buffer-list)
          rev)))))

;;;; --- current buffer ---------------------------------------------------

;; current-buffer / set-buffer batched into the dolist near the top.

(when (emacs-buffer-builtins--install-function-p 'with-current-buffer)
  (defmacro with-current-buffer (buf &rest body)
    "Phase 9 polyfill: forward to `nelisp-ec-with-current-buffer'."
    (declare (indent 1) (debug (form body)))
    (cons 'nelisp-ec-with-current-buffer (cons buf body))))

(when (emacs-buffer-builtins--install-function-p 'default-value)
  (defalias 'default-value #'emacs-buffer-default-value))

(when (emacs-buffer-builtins--install-function-p 'default-boundp)
  (defalias 'default-boundp #'emacs-buffer-default-boundp))

(when (emacs-buffer-builtins--install-function-p 'set-default)
  (defalias 'set-default #'emacs-buffer-setq-default-1))

(defun emacs-buffer-builtins-buffer-modified-tick (&optional buffer)
  "Return BUFFER's standalone modified tick."
  (emacs-buffer-builtins--call-emacs-buffer
   'emacs-buffer-buffer-chars-modified-tick
   (list buffer)))

(when (emacs-buffer-builtins--install-function-p 'buffer-modified-tick)
  (defalias 'buffer-modified-tick
    #'emacs-buffer-builtins-buffer-modified-tick))

(when (emacs-buffer-builtins--install-function-p 'buffer-chars-modified-tick)
  (defun buffer-chars-modified-tick (&optional buffer)
    "Return BUFFER's character modification counter, ignoring property edits."
    (setq buffer (or buffer (current-buffer)))
    (unless (bufferp buffer)
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer))
        (1+ (gethash buffer nelisp-buffer--tick 0))
      (1+ (nelisp-ec-buffer-text-tick buffer)))))

;;;; --- positions ---------------------------------------------------------

;; point / point-min / point-max / goto-char batched into the dolist near
;; the top.

(when (emacs-buffer-builtins--install-function-p 'forward-char)
  (defun forward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters forward.
Bound to C-f / <right>.

Matches the real Emacs C `forward-char' end-of-buffer semantics: when
the target lies past the accessible end, point is clamped to point-max
and `end-of-buffer' is signaled (not `nelisp-ec-args-out-of-range' from
the underlying primitive).  The command loop catches the signal as a
soft non-fatal end-of-buffer message; non-loop callers can wrap in
`condition-case' against `end-of-buffer'."
    (interactive "p")
    (let* ((n (or n 1))
           (p (nelisp-ec-point))
           (lo (nelisp-ec-point-min))
           (hi (nelisp-ec-point-max))
           (target (+ p n)))
      (cond
       ((< target lo)
        (nelisp-ec-goto-char lo)
        (signal 'beginning-of-buffer nil))
       ((> target hi)
        (nelisp-ec-goto-char hi)
        (signal 'end-of-buffer nil))
       (t
        (nelisp-ec-goto-char target)
        t)))))

(when (emacs-buffer-builtins--install-function-p 'backward-char)
  (defun backward-char (&optional n)
    "Phase 9 polyfill: move point N (default 1) characters backward.
Bound to C-b / <left>.

Symmetric to `forward-char' for `beginning-of-buffer' / `end-of-buffer'
clamp + signal semantics."
    (interactive "p")
    (setq n (or n 1))
    (unless (integerp n)
      (signal 'wrong-type-argument (list 'fixnump n)))
    (let ((target (- (point) n)) (lo (point-min)) (hi (point-max)))
      (cond ((< target lo) (goto-char lo) (signal 'beginning-of-buffer nil))
            ((> target hi) (goto-char hi) (signal 'end-of-buffer nil))
            (t (goto-char target) nil)))))

;; buffer-size batched into the dolist near the top.

;;;; --- text mutation + accessors ----------------------------------------

;; insert / erase-buffer / delete-region batched into the dolist near the
;; top.

(defun emacs-buffer-builtins-char-after (&optional pos)
  "Return character at POS, or nil at end of accessible buffer."
  (let ((p (or pos (nelisp-ec-point))))
    (if (and (integerp p)
             (>= p (nelisp-ec-point-min))
             (< p (nelisp-ec-point-max)))
        (aref (nelisp-ec-buffer-substring p (1+ p)) 0)
      nil)))

(defun emacs-buffer-builtins-char-before (&optional pos)
  "Return character before POS, or nil at beginning of accessible buffer."
  (let ((p (or pos (nelisp-ec-point))))
    (if (and (integerp p)
             (> p (nelisp-ec-point-min))
             (<= p (nelisp-ec-point-max)))
        (aref (nelisp-ec-buffer-substring (1- p) p) 0)
      nil)))

(defun emacs-buffer-builtins-following-char ()
  "Return character at point, or 0 at end of accessible buffer."
  (or (emacs-buffer-builtins-char-after) 0))

(defun emacs-buffer-builtins-preceding-char ()
  "Return character before point, or 0 at beginning of accessible buffer."
  (or (emacs-buffer-builtins-char-before) 0))

(dolist (--cell--
         '((char-after     . emacs-buffer-builtins-char-after)
           (char-before    . emacs-buffer-builtins-char-before)
           (following-char . emacs-buffer-builtins-following-char)
           (preceding-char . emacs-buffer-builtins-preceding-char)))
  (let ((--name-- (car --cell--))
        (--target-- (cdr --cell--)))
    (when (emacs-buffer-builtins--install-function-p --name--)
      (defalias --name-- --target--))))

(defun emacs-buffer-builtins-subst-char-in-region
    (start end fromchar tochar &optional noundo)
  "Replace FROMCHAR with TOCHAR between START and END.
Preserve point, markers and text properties.  NOUNDO suppresses
recording replacement undo."
  (setq start (emacs-buffer-builtins--position start)
        end (emacs-buffer-builtins--position end))
  (unless (characterp fromchar)
    (signal 'wrong-type-argument (list 'characterp fromchar)))
  (unless (characterp tochar)
    (signal 'wrong-type-argument (list 'characterp tochar)))
  (when (> start end)
    (let ((saved start)) (setq start end end saved)))
  (when (or (< start (point-min)) (> end (point-max)))
    (signal 'args-out-of-range (list (current-buffer) start end)))
  (when (and (if (and (fboundp 'nelisp--buffer-multibyte-p)
                       (nelisp-buffer-p (current-buffer)))
                  (nelisp--buffer-multibyte-p (current-buffer))
                (and (boundp 'enable-multibyte-characters)
                     enable-multibyte-characters))
             (/= (string-bytes (string fromchar))
                 (string-bytes (string tochar))))
    (error "Characters in ‘subst-char-in-region’ have different byte-lengths"))
  (let ((text (buffer-substring start end)) (i 0) (changed nil))
    (while (< i (length text))
      (when (= (aref text i) fromchar)
        (unless changed
          (when (and buffer-read-only (not inhibit-read-only))
            (signal 'buffer-read-only (list (current-buffer))))
          ;; GNU checks the entire requested range when a match is found,
          ;; including read-only characters which are not being replaced.
          (let ((check start))
            (while (< check end)
              (let ((read-only (get-text-property check 'read-only)))
                (when (and read-only
                           (not (if (listp inhibit-read-only)
                                    (memq read-only inhibit-read-only)
                                  inhibit-read-only)))
                  (signal 'text-read-only nil)))
              (setq check (1+ check)))))
        (aset text i tochar)
        (setq changed t))
      (setq i (1+ i)))
    (when changed
      (let ((buffer (current-buffer)))
        (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer))
            ;; Replacing characters in place must not detach or move markers,
            ;; or shift property intervals as delete-and-insert would do.
            (let* ((full (nelisp-buffer-string buffer))
                   (saved-point (nelisp-point buffer))
                   (replacement (concat (substring full 0 (1- start))
                                        text (substring full (1- end))))
                   (ext (gethash buffer emacs-buffer--state)))
              (when (and (not noundo) ext
                         (not (eq (emacs-buffer--ext-undo-list ext) t)))
                (setf (emacs-buffer--ext-undo-list ext)
                      (cons (cons (substring full (1- start) (1- end)) start)
                            (emacs-buffer--ext-undo-list ext))))
              (setf (nelisp-buffer-before-gap buffer)
                    (substring replacement 0 (1- saved-point)))
              (setf (nelisp-buffer-after-gap buffer)
                    (substring replacement (1- saved-point)))
              (remhash buffer nelisp-buffer--pending-point)
              (nelisp-buffer--bump-tick buffer)
              (setf (nelisp-buffer-modified buffer) t)
              (emacs-buffer-builtins--share-text buffer)
              ;; The file layer can own a separate saved-modification flag.
              (when (fboundp 'set-buffer-modified-p)
                (set-buffer-modified-p t)))
          (let ((saved-point (point)))
            (unwind-protect
                (progn (goto-char start) (delete-region start end) (insert text))
              (goto-char saved-point)))))))
  nil)

(when (emacs-buffer-builtins--install-function-p 'subst-char-in-region)
  (defalias 'subst-char-in-region
    #'emacs-buffer-builtins-subst-char-in-region))

(when (emacs-buffer-builtins--install-function-p 'delete-char)
  (defun delete-char (n &optional killflag)
    "Phase 9 polyfill: delete N characters forward (negative = backward).
KILLFLAG accepted for host API parity but ignored in MVP.
Forwards to `nelisp-ec-delete-char'.  Bound to C-d.

The `(interactive \"p\")' form supplies N from the prefix-arg, so a
keymap dispatch with no prefix passes N=1.  Without this form,
`call-interactively' would build an empty arg list and crash on the
required N parameter (= the same lambda-arity-mismatch that bit
`delete-backward-char' before its 2026-05-04 fix)."
    (interactive "p")
    (ignore killflag)
    (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p (current-buffer)))
        ;; The standalone core's buffers (`with-temp-buffer',
        ;; `find-file-noselect', ...) are native; the ec layer below never sees
        ;; them.  Same contract as Fdelete_char.
        (let ((pos (+ (point) n)))
          (cond ((< pos (point-min)) (signal 'beginning-of-buffer nil))
                ((> pos (point-max)) (signal 'end-of-buffer nil))
                (t (delete-region (min pos (point)) (max pos (point)))))
          nil)
      (nelisp-ec-delete-char n))))

;; buffer-string / buffer-substring / buffer-substring-no-properties
;; batched into the dolist near the top.

;;;; --- save-* family ----------------------------------------------------

(when (emacs-buffer-builtins--install-function-p 'save-excursion)
  (defmacro save-excursion (&rest body)
    "Phase 9 polyfill: expand to `nelisp-ec-save-excursion' semantics."
    (declare (indent 0) (debug (body)))
    (nelisp-ec--save-excursion-form body)))

(when (emacs-buffer-builtins--install-function-p 'save-restriction)
  (defmacro save-restriction (&rest body)
    "Phase 9 polyfill: expand to `nelisp-ec-save-restriction' semantics."
    (declare (indent 0) (debug (body)))
    (nelisp-ec--save-restriction-form body)))

(when (emacs-buffer-builtins--install-function-p 'save-current-buffer)
  (defmacro save-current-buffer (&rest body)
    "Phase 9 polyfill: expand to `nelisp-ec-save-current-buffer' semantics."
    (declare (indent 0) (debug (body)))
    (nelisp-ec--save-current-buffer-form body)))

;; Doc 33 §8 item 242: the vendor NeLisp stdlib's own `setq-default'
;; (`vendor/nelisp/lisp/nelisp-stdlib-eval-special.el') is a plain
;; `setq' alias ("NeLisp has no buffer-local"), so without this
;; polyfill every buffer would share one poisoned default the moment
;; ANY buffer ran `setq-default' on a per-buffer symbol — e.g.
;; the magit bridge buffer-defaults helper's own
;; `(setq-default buffer-read-only nil)' would otherwise never persist
;; once `magit-section-mode' next sets the GLOBAL `buffer-read-only' to
;; `t' via ordinary `setq'.  Each SYM/VALUE pair routes through
;; `emacs-buffer-setq-default-1' (function, not macro, since macro
;; expansion here just needs to call something once per pair).
(when (emacs-buffer-builtins--install-function-p 'setq-default)
  (defmacro setq-default (&rest pairs)
    "Phase 9/Doc 33 item 242 polyfill: route through the swap engine.
Supersedes the vendor NeLisp stdlib `setq-default' (plain `setq' alias)
— see the commentary immediately above this form."
    (let (forms (rest pairs))
      (while rest
        (push (list 'emacs-buffer-setq-default-1
                     (list 'quote (car rest)) (cadr rest))
              forms)
        (setq rest (cddr rest)))
      (cons 'progn (nreverse forms)))))

(defun emacs-buffer-builtins--restriction-limits (buffer)
  "Return BUFFER's innermost labeled restriction, if it has a non-nil label."
  (let* ((ext (gethash buffer emacs-buffer--state))
         (entry (and ext (car (emacs-buffer--labeled-restrictions buffer)))))
    (and entry (car entry) entry)))

(defun emacs-buffer-builtins--restricted-narrow (original start end &optional buffer)
  "Call ORIGINAL narrowing to START and END within BUFFER's label limits."
  (setq buffer (or buffer (current-buffer)))
  (let ((entry (emacs-buffer-builtins--restriction-limits buffer)))
    (if entry
        (let* ((lo (marker-position (nth 1 entry)))
               (hi (marker-position (nth 2 entry)))
               (s (max lo (min hi (min start end))))
               (e (max lo (min hi (max start end)))))
          (funcall original s e buffer))
      (funcall original start end buffer))))

(defun emacs-buffer-builtins--restricted-widen (original &optional buffer)
  "Widen BUFFER up to its current labeled restriction."
  (setq buffer (or buffer (current-buffer)))
  (let ((entry (emacs-buffer-builtins--restriction-limits buffer)))
    (if entry
        (nelisp-narrow-to-region (marker-position (nth 1 entry))
                                (marker-position (nth 2 entry)) buffer)
      (funcall original buffer))))

(defun emacs-buffer-builtins--save-labeled-restriction (original &rest body)
  "Save labels and moving bounds around BODY, releasing all saved markers."
  (ignore original)
  (let ((buffer (make-symbol "restriction-buffer"))
        (stack (make-symbol "restriction-stack"))
        (start (make-symbol "restriction-start"))
        (end (make-symbol "restriction-end")))
    (list 'let* (list (list buffer '(current-buffer))
                     (list stack (list 'emacs-buffer-builtins--label-stack buffer))
                     (list start '(copy-marker (point-min) nil))
                     (list end '(copy-marker (point-max) t)))
          (list 'unwind-protect (cons 'progn body)
                (list 'unwind-protect
                      (list 'progn
                            (list 'emacs-buffer-builtins--restore-label-stack buffer stack)
                            (list 'when (list 'buffer-live-p buffer)
                                  (list 'with-current-buffer buffer '(widen)
                                        (list 'narrow-to-region
                                              (list 'marker-position start)
                                              (list 'marker-position end)))))
                      (list 'set-marker start nil)
                      (list 'set-marker end nil))))))

(defun emacs-buffer-builtins--label-stack (buffer)
  "Retain the label stack for BUFFER without allocating state."
  (let* ((ext (gethash buffer emacs-buffer--state))
         (stack (and ext (emacs-buffer--labeled-restrictions buffer))))
    (emacs-buffer-builtins--retain-label-stack stack)
    stack))

(defun emacs-buffer-builtins--retain-label-stack (stack)
  "Retain STACK's head; each head in turn owns its tail."
  (when stack
    (let ((count (nthcdr 3 (car stack))))
      (setcar count (1+ (car count)))))
  stack)

(defun emacs-buffer-builtins--release-label-stack (stack)
  "Release STACK and detach markers belonging to newly unreachable entries."
  (while stack
    (let ((count (nthcdr 3 (car stack))))
      (setcar count (1- (car count)))
      (if (> (car count) 0)
          (setq stack nil)
        (set-marker (nth 1 (car stack)) nil)
        (set-marker (nth 2 (car stack)) nil)
        (setq stack (cdr stack))))))

(defun emacs-buffer-builtins--restore-label-stack (buffer stack)
  "Restore BUFFER's STACK before restoring its saved accessible range."
  (if (buffer-live-p buffer)
      (let ((ext (gethash buffer emacs-buffer--state)))
        (when ext
          (emacs-buffer-builtins--release-label-stack
           (emacs-buffer--labeled-restrictions buffer))
          (emacs-buffer--set-labeled-restrictions buffer stack)))
    (emacs-buffer-builtins--release-label-stack stack)))

(defvar emacs-buffer-builtins--restriction-bridge-installed nil
  "Non-nil after labeled narrowing has connected to native narrowing.")

(defun emacs-buffer-builtins--install-restriction-bridge ()
  "Connect native narrowing and saved restrictions to the label stack."
  (unless emacs-buffer-builtins--restriction-bridge-installed
    (setq emacs-buffer-builtins--restriction-bridge-installed t)
    (advice-add 'nelisp-narrow-to-region :around #'emacs-buffer-builtins--restricted-narrow)
    (advice-add 'nelisp-widen :around #'emacs-buffer-builtins--restricted-widen)
    (if (fboundp 'nelisp--set-special-form-implementation)
        (nelisp--set-special-form-implementation
         'save-restriction
         (cons 'macro
               (lambda (&rest body)
                 (apply #'emacs-buffer-builtins--save-labeled-restriction nil body))))
      (advice-add 'save-restriction :around #'emacs-buffer-builtins--save-labeled-restriction))))

;;;; --- narrow / widen ---------------------------------------------------

;; narrow-to-region / widen batched into the dolist near the top.

;;;; --- markers ----------------------------------------------------------

;; make-marker / set-marker / marker-position / marker-buffer /
;; point-marker batched into the dolist near the top.

;;;; --- with-temp-buffer / with-temp-file (Phase 9 rewrite) -------------

;; Phase 8 used a global string accumulator (`emacs-stub--current-temp-buffer')
;; which collapsed under multi-buffer scenarios.  Phase 9 replaces the body
;; with a real `nelisp-ec' buffer that participates in the current-buffer
;; dispatch and respects narrow / point.

(when (emacs-buffer-builtins--install-function-p 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-buffer'.
A fresh `nelisp-ec' buffer named ` *temp*' is created, made current
for BODY, then killed unconditionally on exit (= via `unwind-protect')."
    (declare (indent 0) (debug (body)))
    (let ((buf (make-symbol "buf")))
      (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp*")))
            (list 'unwind-protect
                  (cons 'nelisp-ec-with-current-buffer (cons buf body))
                  (list 'nelisp-ec-kill-buffer buf))))))

(when (emacs-buffer-builtins--install-function-p 'with-temp-file)
  (defmacro with-temp-file (path &rest body)
    "Phase 9 polyfill: real-buffer rewrite of `with-temp-file'.
BODY runs inside a fresh `nelisp-ec' buffer; on normal exit the buffer
contents are written to PATH via `nl-write-file' (when available),
falling back to `write-region' under host Emacs."
    (declare (indent 1) (debug (form body)))
    (let ((buf (make-symbol "buf"))
          (p (make-symbol "p"))
          (s (make-symbol "s")))
      (list 'let (list (list p path)
                       (list buf (list 'nelisp-ec-generate-new-buffer
                                       " *temp-file*")))
            (list 'unwind-protect
                  (list 'progn
                        (cons 'nelisp-ec-with-current-buffer (cons buf body))
                        (list 'let (list (list s
                                               (list
                                                'nelisp-ec-with-current-buffer
                                                buf
                                                '(nelisp-ec-buffer-string))))
                              (list 'cond
                                    (list (list 'fboundp (list 'quote
                                                               'nl-write-file))
                                          (list 'nl-write-file p s))
                                    (list (list 'fboundp (list 'quote
                                                               'write-region))
                                          (list 'write-region s nil p)))))
                  (list 'nelisp-ec-kill-buffer buf))))))

;; ---- buffer-hash (C builtin) ----
;; A non-cryptographic content hash used to detect buffer changes.  Callers
;; only compare two hashes for equality.  The runtime's sxhash / md5 /
;; secure-hash are stubbed (return nil) here, so use a deterministic djb2
;; digest over the buffer text -- content-sensitive and dependency-free.

(unless (fboundp 'buffer-hash)
  (defun buffer-hash (&optional buffer-or-name)
    "Return a hash string of the entire contents of BUFFER-OR-NAME.
Ignores narrowing (hashes the whole buffer)."
    (let ((buf (if (stringp buffer-or-name)
                   (get-buffer buffer-or-name)
                 (or buffer-or-name (current-buffer))))
          (s nil))
      ;; Capture the text via `setq' rather than relying on the return value
      ;; of `with-current-buffer'/`save-restriction', which this runtime does
      ;; not propagate (they return the buffer, not the body value).
      (save-current-buffer
        (set-buffer buf)
        (save-restriction
          (widen)
          (setq s (buffer-substring-no-properties (point-min) (point-max)))))
      (let ((h 5381) (i 0) (n (length s)))
        (while (< i n)
          (setq h (logand (+ (* h 33) (aref s i)) 1099511627775)) ; mod 2^40
          (setq i (1+ i)))
        (number-to-string h)))))

;; Consumer `nelisp-ec' buffers and markers are also valid print/read
;; streams.  The standalone reader's native stream helpers only know its
;; own buffer representation, so bridge these objects at the helper level.
(defvar emacs-buffer-builtins--native-valid-print-stream-p nil)
(defvar emacs-buffer-builtins--native-emit-to-stream nil)
(defvar emacs-buffer-builtins--native-read-dispatch nil)
(defvar emacs-buffer-builtins--native-prn-to-string nil)

(defun emacs-buffer-builtins-valid-print-stream-p (stream)
  "Return non-nil when STREAM is a supported print stream."
  (or (nelisp-ec-buffer-p stream)
      (nelisp-ec-marker-p stream)
      (and emacs-buffer-builtins--native-valid-print-stream-p
           (funcall emacs-buffer-builtins--native-valid-print-stream-p stream))))

(defun emacs-buffer-builtins--emit-to-ec-marker (str marker)
  "Emit STR at MARKER while preserving the marker's buffer and point."
  (let ((mbuf (nelisp-ec-marker-buffer marker)))
    (unless mbuf
      (signal 'error (list "Marker does not point anywhere")))
    (let ((saved-buffer nelisp-ec--current-buffer))
      (unwind-protect
          (let ((nelisp-ec--current-buffer mbuf))
             (let* ((orig-point (nelisp-ec-point))
                   (ins-pos (nelisp-ec-marker-position marker))
                   (n (length str))
                   (completed nil))
              (when (or (< ins-pos (nelisp-ec-point-min))
                        (> ins-pos (nelisp-ec-point-max)))
                (signal 'error
                        (list "Marker is outside the accessible part of the buffer"
                              marker)))
              (unwind-protect
                  (progn
                    (nelisp-ec-goto-char ins-pos)
                    (nelisp-ec-insert str)
                    (nelisp-ec-goto-char (if (>= orig-point ins-pos)
                                              (+ orig-point n) orig-point))
                    (nelisp-ec-set-marker marker (+ ins-pos n) mbuf)
                    (setq completed t))
                (when (and (not completed)
                           (nelisp-ec-buffer-p mbuf)
                           (not (nelisp-ec-buffer-killed-p mbuf)))
                  (nelisp-ec-goto-char orig-point)))))
        (setq nelisp-ec--current-buffer saved-buffer)))))

(defun emacs-buffer-builtins-emit-to-stream (str stream)
  "Emit STR to an EC buffer/marker or delegate to the native helper."
  (cond
   ((nelisp-ec-buffer-p stream)
    (let ((saved nelisp-ec--current-buffer))
      (unwind-protect
          (let ((nelisp-ec--current-buffer stream)) (nelisp-ec-insert str))
        (setq nelisp-ec--current-buffer saved))))
   ((nelisp-ec-marker-p stream)
    (emacs-buffer-builtins--emit-to-ec-marker str stream))
   (emacs-buffer-builtins--native-emit-to-stream
    (funcall emacs-buffer-builtins--native-emit-to-stream str stream))
   (t (princ str))))

(defun emacs-buffer-builtins-read-dispatch (stream)
  "Read one form from an EC buffer/marker or delegate natively."
  (cond
   ((nelisp-ec-buffer-p stream)
    (let ((saved nelisp-ec--current-buffer))
      (unwind-protect
          (let ((nelisp-ec--current-buffer stream))
            (let* ((base (1- (nelisp-ec-point-min)))
                   (start (- (1- (nelisp-ec-point)) base))
                   (full (nelisp-ec-buffer-string))
                   (r (condition-case nil
                          (read-from-string full start)
                        (end-of-file (signal 'end-of-file (list stream))))))
              (nelisp-ec-goto-char (+ (nelisp-ec-point-min) (cdr r)))
              (car r)))
        (setq nelisp-ec--current-buffer saved))))
   ((nelisp-ec-marker-p stream)
    (let ((mbuf (nelisp-ec-marker-buffer stream)))
      (unless mbuf (signal 'error (list "Marker does not point anywhere")))
      (let ((saved nelisp-ec--current-buffer))
        (unwind-protect
            (let ((nelisp-ec--current-buffer mbuf))
              ;; Marker streams ignore narrowing on GNU Emacs.  Read the
              ;; complete buffer, then restore both restriction and point.
              (let* ((orig-point (nelisp-ec-point))
                     (saved-lo (nelisp-ec-buffer-narrow-start mbuf))
                     (saved-hi (nelisp-ec-buffer-narrow-end mbuf)))
                (unwind-protect
                    (progn
                      (nelisp-ec-widen)
                      (let* ((full (nelisp-ec-buffer-string))
                             (start (1- (nelisp-ec-marker-position stream)))
                             (r (read-from-string full start)))
                        (nelisp-ec-set-marker stream (1+ (cdr r)) mbuf)
                        (car r)))
                  (nelisp-ec--set-buffer-narrow-start mbuf saved-lo)
                  (nelisp-ec--set-buffer-narrow-end mbuf saved-hi)
                  (nelisp-ec-goto-char orig-point))))
          (setq nelisp-ec--current-buffer saved)))))
   (emacs-buffer-builtins--native-read-dispatch
    (funcall emacs-buffer-builtins--native-read-dispatch stream))
   (t (signal (if (symbolp stream) 'void-function 'invalid-function)
              (list stream)))))


(defun emacs-buffer-builtins-prn-to-string (obj escape &optional depth)
  "Print EC buffers/markers in Emacs's opaque object notation."
  (cond
   ((nelisp-ec-buffer-p obj)
    (if (nelisp-ec-buffer-killed-p obj) "#<killed buffer>"
      (format "#<buffer %s>" (nelisp-ec-buffer-name obj))))
   ((nelisp-ec-marker-p obj)
    (let ((buf (nelisp-ec-marker-buffer obj)))
      (if buf (format "#<marker at %d in %s>"
                      (nelisp-ec-marker-position obj)
                      (nelisp-ec-buffer-name buf))
        "#<marker in no buffer>")))
   (emacs-buffer-builtins--native-prn-to-string
    (funcall emacs-buffer-builtins--native-prn-to-string obj escape depth))
   (t (format "#<unprintable %S>" obj))))

(when (emacs-buffer-builtins--standalone-p)
  (when (and (fboundp 'nelisp--valid-print-stream-p)
             (not emacs-buffer-builtins--native-valid-print-stream-p))
    (setq emacs-buffer-builtins--native-valid-print-stream-p
          (symbol-function 'nelisp--valid-print-stream-p))
    (fset 'nelisp--valid-print-stream-p #'emacs-buffer-builtins-valid-print-stream-p))
  (when (and (fboundp 'nelisp--emit-to-stream)
             (not emacs-buffer-builtins--native-emit-to-stream))
    (setq emacs-buffer-builtins--native-emit-to-stream
          (symbol-function 'nelisp--emit-to-stream))
    (fset 'nelisp--emit-to-stream #'emacs-buffer-builtins-emit-to-stream))
  (when (and (fboundp 'nelisp--read-dispatch)
             (not emacs-buffer-builtins--native-read-dispatch))
    (setq emacs-buffer-builtins--native-read-dispatch
          (symbol-function 'nelisp--read-dispatch))
    (fset 'nelisp--read-dispatch #'emacs-buffer-builtins-read-dispatch))
  (when (and (fboundp 'nelisp--prn-to-string)
             (not emacs-buffer-builtins--native-prn-to-string))
    (setq emacs-buffer-builtins--native-prn-to-string
          (symbol-function 'nelisp--prn-to-string))
    (fset 'nelisp--prn-to-string #'emacs-buffer-builtins-prn-to-string)))

(when (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'nelisp-narrow-to-region))
  (emacs-buffer-builtins--install-restriction-bridge))


;;;; --- native marker position arguments --------------------------------

(defvar emacs-buffer-builtins--native-substring-no-properties nil)
(defun emacs-buffer-builtins--marker-position-argument (position)
  "Resolve marker POSITION and retain the native validation of other types."
  (if (markerp position)
      (or (marker-position position) (error "Marker does not point anywhere"))
    position))
(defun emacs-buffer-builtins-buffer-substring-no-properties (start end)
  "Return native text between integer or marker bounds START and END."
  (funcall emacs-buffer-builtins--native-substring-no-properties
           (emacs-buffer-builtins--marker-position-argument start)
           (emacs-buffer-builtins--marker-position-argument end)))
(when (and (emacs-buffer-builtins--standalone-p)
           (fboundp 'buffer-substring-no-properties))
  (unless emacs-buffer-builtins--native-substring-no-properties
    (setq emacs-buffer-builtins--native-substring-no-properties
          (symbol-function 'buffer-substring-no-properties)))
  (defalias 'buffer-substring-no-properties
    #'emacs-buffer-builtins-buffer-substring-no-properties))

(defvar emacs-buffer-builtins--native-goto-char nil)
(defun emacs-buffer-builtins-goto-char (position)
  "Move native point to integer or marker POSITION, returning POSITION."
  (funcall emacs-buffer-builtins--native-goto-char
           (emacs-buffer-builtins--checked-position position))
  position)
(defvar emacs-buffer-builtins--native-char-after nil)
(defvar emacs-buffer-builtins--native-char-before nil)
(defun emacs-buffer-builtins-char-after (&optional position)
  "Return the native character at optional integer or marker POSITION."
  (funcall emacs-buffer-builtins--native-char-after
           (and position (emacs-buffer-builtins--checked-position position))))
(defun emacs-buffer-builtins-char-before (&optional position)
  "Return the native character preceding integer or marker POSITION."
  (funcall emacs-buffer-builtins--native-char-before
           (and position (emacs-buffer-builtins--checked-position position))))
(defun emacs-buffer-builtins--install-native-marker-position-bridges ()
  "Keep marker validation at the shared buffer boundary after IO loads."
  ;; Capture once: the IO adapter may itself capture this bridge, so capturing
  ;; its replacement later would form a cycle through that adapter.
  (unless emacs-buffer-builtins--native-goto-char
    (setq emacs-buffer-builtins--native-goto-char (symbol-function 'goto-char)))
  (unless emacs-buffer-builtins--native-char-after
    (setq emacs-buffer-builtins--native-char-after (symbol-function 'char-after)))
  (unless emacs-buffer-builtins--native-char-before
    (setq emacs-buffer-builtins--native-char-before (symbol-function 'char-before)))
  (defalias 'goto-char #'emacs-buffer-builtins-goto-char)
  (defalias 'char-after #'emacs-buffer-builtins-char-after)
  (defalias 'char-before #'emacs-buffer-builtins-char-before))
(when (emacs-buffer-builtins--standalone-p)
  (emacs-buffer-builtins--install-native-marker-position-bridges)
  ;; Standalone provide executes named feature callbacks. This also handles
  ;; concatenated bootstrap source, where the IO adapter is evaluated later.
  (let ((entry (assq 'files-standalone-buffer after-load-alist)))
    (if entry
        (unless (memq 'emacs-buffer-builtins--install-native-marker-position-bridges (cdr entry))
          (setcdr entry
                  (cons 'emacs-buffer-builtins--install-native-marker-position-bridges (cdr entry))))
      (setq after-load-alist
            (cons (list 'files-standalone-buffer
                        'emacs-buffer-builtins--install-native-marker-position-bridges)
                  after-load-alist)))))

(provide 'emacs-buffer-builtins)

;;; emacs-buffer-builtins.el ends here
