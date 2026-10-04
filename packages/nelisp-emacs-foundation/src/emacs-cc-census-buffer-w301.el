;;; emacs-cc-census-buffer-w301.el --- Batch input and text operations  -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-census-buffer-w301--interrupt-input t
  "Whether batch input uses interrupts, as reported by GNU Emacs.")

(defun emacs-cc-census-buffer-w301--check-arity (name arguments min max)
  "Validate the number of ARGUMENTS for NAME against MIN and MAX."
  (let ((count (length arguments)))
    (unless (and (>= count min) (<= count max))
      (signal 'wrong-number-of-arguments (list name count)))))

(defun emacs-cc-census-buffer-w301--position (position)
  "Validate POSITION and return its integer or marker position."
  (cond
   ((integerp position) position)
   ((markerp position)
    (or (marker-position position)
        (error "Marker does not point anywhere")))
   (t (signal 'wrong-type-argument
              (list 'integer-or-marker-p position)))))

(defun emacs-cc-census-buffer-w301--removal-p (start end properties object)
  "Return whether PROPERTIES occur explicitly between START and END."
  (let ((position start) found)
    (while (and (< position end) (not found))
      (let ((plist (text-properties-at position object))
            (rest properties))
        (while (and (consp rest) (not found))
          (when (plist-member plist (car rest)) (setq found t))
          (setq rest (cdr rest))))
      (unless found
        (setq position (or (next-property-change position object end) end))))
    found))

(defun emacs-cc-census-buffer-w301--check-read-only (start end buffer)
  "Check read-only restrictions on BUFFER between START and END."
  (with-current-buffer buffer
    (when (and buffer-read-only (not inhibit-read-only))
      (signal 'buffer-read-only (list buffer)))
    (let ((position start))
      (while (< position end)
        (let ((value (get-text-property position 'read-only)))
          (when (and value
                     (not (if (listp inhibit-read-only)
                              (memq value inhibit-read-only)
                            inhibit-read-only)))
            (signal 'text-read-only nil)))
        (setq position (or (next-property-change position nil end) end))))))

(unless (fboundp 'current-input-mode)
  (defun current-input-mode (&rest arguments)
    "Return the batch terminal's (INTERRUPT FLOW META QUIT) input modes."
    (emacs-cc-census-buffer-w301--check-arity
     'current-input-mode arguments 0 0)
    (list emacs-cc-census-buffer-w301--interrupt-input nil t 7)))

(unless (fboundp 'set-input-mode)
  (defun set-input-mode (&rest arguments)
    "Set input INTERRUPT, FLOW, META and optional QUIT modes.
On the batch terminal only INTERRUPT changes.  GNU ignores the tty-only
FLOW, META and QUIT arguments, including their types, in this setting."
    (emacs-cc-census-buffer-w301--check-arity
     'set-input-mode arguments 3 4)
    (setq emacs-cc-census-buffer-w301--interrupt-input
          (and (car arguments) t))
    nil))

(unless (fboundp 'remove-list-of-text-properties)
  (defun remove-list-of-text-properties (&rest arguments)
    "Remove property names from START through END of optional OBJECT.
OBJECT defaults to the current buffer.  Return t if a property was
removed.  String positions are zero-based; buffer positions accept markers."
    (emacs-cc-census-buffer-w301--check-arity
     'remove-list-of-text-properties arguments 3 4)
    (let ((start (nth 0 arguments))
          (end (nth 1 arguments))
          (properties (nth 2 arguments))
          (object (nth 3 arguments)))
      ;; GNU validates the object before either position, and accepts an
      ;; empty range even when the positions lie outside the object.
      (unless (or (null object) (bufferp object) (stringp object))
        (signal 'wrong-type-argument (list 'buffer-or-string-p object)))
      (setq start (emacs-cc-census-buffer-w301--position start)
            end (emacs-cc-census-buffer-w301--position end))
      (unless (= start end)
        (let* ((lo (min start end))
               (hi (max start end))
               (buffer (unless (stringp object) (or object (current-buffer))))
               (first (if buffer (with-current-buffer buffer (point-min)) 0))
               (last (if buffer (with-current-buffer buffer (point-max))
                       (length object)))
               plist)
          (unless (and (>= lo first) (<= hi last))
            (signal 'args-out-of-range (list start end)))
          ;; GNU traverses cons cells only, ignoring a non-list argument
          ;; or the final non-cons tail of an improper list.
          (while (consp properties)
            (setq plist (cons (car properties) (cons nil plist))
                  properties (cdr properties)))
          (when (and plist
                     (emacs-cc-census-buffer-w301--removal-p
                      lo hi (nth 2 arguments) object))
            (when buffer
              (emacs-cc-census-buffer-w301--check-read-only lo hi buffer))
            (and (remove-text-properties lo hi plist object) t)))))))

(unless (fboundp 'search-backward-regexp)
  (defun search-backward-regexp (&rest arguments)
    "Search backward for REGEXP with optional BOUND, NOERROR and COUNT.
Return the resulting point.  Negative COUNT searches forward."
    (emacs-cc-census-buffer-w301--check-arity
     'search-backward-regexp arguments 1 4)
    (let ((regexp (car arguments))
          (bound (nth 1 arguments))
          (count (or (nth 3 arguments) 1)))
      ;; GNU checks COUNT, REGEXP and BOUND before compiling the pattern,
      ;; and still checks the bound when COUNT is zero.
      (unless (integerp count)
        (signal 'wrong-type-argument (list 'fixnump count)))
      (unless (stringp regexp)
        (signal 'wrong-type-argument (list 'stringp regexp)))
      (when bound
        (setq bound (emacs-cc-census-buffer-w301--position bound))
        (when (if (< count 0) (< bound (point)) (> bound (point)))
          (error "Invalid search bound (wrong side of point)")))
      (if (= count 0)
          (point)
        ;; The matcher does not reject every malformed pattern by itself.
        ;; Use the prelude's structural validator before searching, even
        ;; when there is no accessible text to search.
        (nelisp--w202-regexp-validate regexp)
        (re-search-backward regexp bound (nth 2 arguments) count)))))

;; Deferred change notifications require the edit operations to collect a
;; pending change record.  The bundle's edit operations do not do so.
;; Leave `combine-after-change-execute' unchanged.
;;
;; Labeled restrictions also constrain ordinary `widen' and
;; `narrow-to-region', and unwind with `save-restriction'.  Those owners do
;; not support a restriction stack.  Do not implement an isolated label
;; table which would silently allow widening past a protected restriction.
;; Leave both internal labeled restriction primitives unchanged.
;;
;; The buffer-local bridge swaps values through ordinary symbol cells.
;; It cannot distinguish or preserve an active dynamic binding while
;; changing its underlying buffer-local top-level cell.  Leave
;; `set-buffer-local-toplevel-value' unchanged.
;;
;; `suspend-emacs' requires terminal/process suspension and resumption.
;; Leave its existing definition unchanged rather than emulate suspension.

(provide 'emacs-cc-census-buffer-w301)
;;; emacs-cc-census-buffer-w301.el ends here
