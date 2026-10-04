;;; emacs-cc-census-buffer-w201.el --- Buffer and command primitives  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-buffer-w201--arity (name args minimum maximum)
  "Check ARGS against NAME's primitive arity."
  (let ((count (length args)))
    (unless (and (>= count minimum) (<= count maximum))
      (signal 'wrong-number-of-arguments (list name count)))))

(unless (fboundp 'command-error-default-function)
  (defun command-error-default-function (&rest args)
    "Display an unhandled error DATA with CONTEXT and SIGNAL.

(fn DATA CONTEXT SIGNAL)"
    (emacs-cc-census-buffer-w201--arity
     'command-error-default-function args 3 3)
    (let ((data (car args)) (context (cadr args)))
      (message "%s%s" (or context "") (error-message-string data))
      (when noninteractive (kill-emacs -1)))
    nil))

(unless (fboundp 'command-remapping)
  (defun command-remapping (&rest args)
    "Return COMMAND's first remapping in KEYMAPS or the active maps.
POSITION selects the active maps at that position.

(fn COMMAND &optional POSITION KEYMAPS)"
    (emacs-cc-census-buffer-w201--arity 'command-remapping args 1 3)
    (let ((command (car args)) (position (cadr args)) (maps (nth 2 args)))
      (when (symbolp command)
        (cond
         ((keymapp maps) (setq maps (list maps)))
         ((null maps) (setq maps (current-active-maps t position)))
         ((not (listp maps))
          (signal 'wrong-type-argument (list 'keymapp maps))))
        (catch 'remapping
          (dolist (map maps)
            ;; GNU ignores non-keymaps inside a list of maps.
            (when (keymapp map)
              (let ((binding (lookup-key map (vector 'remap command))))
                (when (and binding (not (numberp binding)))
                  (throw 'remapping binding)))))
          nil)))))

(unless (fboundp 'defining-kbd-macro)
  (defun defining-kbd-macro (&rest args)
    "Record keyboard input, appending when APPEND is non-nil.
Non-nil NO-EXEC suppresses replay of the previous macro.

(fn APPEND &optional NO-EXEC)"
    (emacs-cc-census-buffer-w201--arity 'defining-kbd-macro args 1 2)
    (when defining-kbd-macro (error "Already defining kbd macro"))
    (let ((append (car args)) (no-exec (cadr args)))
      (when append
        (unless (or (stringp last-kbd-macro) (vectorp last-kbd-macro))
          (signal 'wrong-type-argument (list 'arrayp last-kbd-macro)))
        (unless no-exec (execute-kbd-macro last-kbd-macro)))
      (setq emacs-cc-macros-1--recording t
            defining-kbd-macro t
            emacs-cc-macros-1--events
            (if append (append last-kbd-macro nil) nil))
      (unless (and (boundp 'inhibit-message) inhibit-message)
        (message "%s" (if append "Appending to kbd macro..."
                        "Defining kbd macro..."))))
    nil))

(unless (fboundp 'delete-overlay)
  (defun delete-overlay (&rest args)
    "Detach OVERLAY from its buffer, retaining its properties.
Deleting an already detached overlay is harmless.

(fn OVERLAY)"
    (emacs-cc-census-buffer-w201--arity 'delete-overlay args 1 1)
    (let ((overlay (car args)))
      (unless (overlayp overlay)
        (signal 'wrong-type-argument (list 'overlayp overlay)))
      (emacs-buffer-delete-overlay overlay))))

(defun emacs-cc-census-buffer-w201--key-description (keys)
  "Describe KEYS, including all integer event modifier bits."
  (mapconcat
   (lambda (event)
     (if (not (integerp event))
         (if (symbolp event) (symbol-name event) (prin1-to-string event))
       (let ((base (logand event 4194303)) (prefix ""))
         (dolist (modifier '((4194304 . "A-") (67108864 . "C-")
                             (16777216 . "H-") (134217728 . "M-")
                             (33554432 . "S-") (8388608 . "s-")))
           (unless (= (logand event (car modifier)) 0)
             (setq prefix (concat prefix (cdr modifier)))))
         (concat prefix
                 (cond ((= base 32) "SPC") ((= base 9) "TAB")
                       ((= base 10) "LFD") ((= base 13) "RET")
                       ((= base 27) "ESC") ((= base 127) "DEL")
                       ((< base 32) (concat "C-" (string (downcase (+ base 64)))))
                       (t (string base)))))))
   (append keys nil) " "))

(defun emacs-cc-census-buffer-w201--describe-map
    (map prefix filter menus ancestors)
  "Insert MAP's bindings below PREFIX, matching FILTER and MENUS.
ANCESTORS prevents recursion through cyclic prefix maps."
  (unless (memq map ancestors)
    (map-keymap
     (lambda (event binding)
       (let ((keys (vconcat prefix (vector event))))
         (when (and (or menus (not (memq event '(menu-bar tool-bar))))
                    (or (null filter)
                        (let ((i 0) (match t))
                          (while (and match (< i (min (length keys)
                                                     (length filter))))
                            (unless (equal (aref keys i) (elt filter i))
                              (setq match nil))
                            (setq i (1+ i)))
                          match)))
           (if (keymapp binding)
               (emacs-cc-census-buffer-w201--describe-map
                binding keys filter menus (cons map ancestors))
             (when (and binding
                        (or (null filter) (>= (length keys) (length filter))))
               (insert (emacs-cc-census-buffer-w201--key-description keys) "\t\t"
                       (if (symbolp binding) (symbol-name binding)
                         (prin1-to-string binding))
                       "\n"))))))
     map)))

(unless (fboundp 'describe-buffer-bindings)
  (defun describe-buffer-bindings (&rest args)
    "Insert bindings from BUFFER, optionally limited by PREFIX and MENUS.

(fn BUFFER &optional PREFIX MENUS)"
    (emacs-cc-census-buffer-w201--arity 'describe-buffer-bindings args 1 3)
    (let ((source (car args)) (prefix (cadr args)) (menus (nth 2 args))
          maps)
      (unless (bufferp source)
        (signal 'wrong-type-argument (list 'bufferp source)))
      (unless (or (null prefix) (stringp prefix) (vectorp prefix)
                  (listp prefix))
        (signal 'wrong-type-argument (list 'sequencep prefix)))
      (when (and prefix (listp prefix))
        (signal 'wrong-type-argument (list 'arrayp prefix)))
      (setq maps (with-current-buffer source (current-active-maps)))
      (when (and (boundp 'key-translation-map) (keymapp key-translation-map))
        (insert "Key translations:\n\nKey             Binding\n"
                "-------------------------------------------------------------------------------\n")
        (emacs-cc-census-buffer-w201--describe-map
         key-translation-map [] prefix menus nil)
        (insert "\n"))
      (dolist (map maps)
        (insert (if (eq map (current-global-map))
                    "Global Bindings:\n" "Local Bindings:\n")
                "\nKey             Binding\n"
                "-------------------------------------------------------------------------------\n")
        (emacs-cc-census-buffer-w201--describe-map map [] prefix menus nil)
        (insert "\n")))
    nil))

(unless (fboundp 'discard-input)
  (defun discard-input (&rest args)
    "Discard pending command events and stop defining a keyboard macro.

(fn)"
    (emacs-cc-census-buffer-w201--arity 'discard-input args 0 0)
    (when defining-kbd-macro
      (setq last-kbd-macro (vconcat emacs-cc-macros-1--events)))
    (setq unread-command-events nil
          emacs-keymap--input-queue nil
          emacs-command-loop--unread-events nil
          defining-kbd-macro nil
          emacs-cc-macros-1--recording nil
          emacs-cc-macros-1--events nil)
    nil))

(defun emacs-cc-census-buffer-w201--position (position)
  "Return POSITION as an integer, accepting markers."
  (cond ((integerp position) position)
        ((markerp position) (or (marker-position position) 0))
        (t (signal 'wrong-type-argument
                   (list 'integer-or-marker-p position)))))

(defun emacs-cc-census-buffer-w201--downcase (beg end)
  "Downcase [BEG, END), retaining character positions and metadata."
  (setq beg (emacs-cc-census-buffer-w201--position beg)
        end (emacs-cc-census-buffer-w201--position end))
  (when (> beg end)
    (let ((saved beg)) (setq beg end end saved)))
  (when (or (< beg (point-min)) (> end (point-max)))
    (signal 'args-out-of-range (list (current-buffer) beg end)))
  (when (< beg end)
    (when (and buffer-read-only (not inhibit-read-only))
      (signal 'buffer-read-only (list (current-buffer))))
    (let ((pos beg))
      (while (< pos end)
        (let ((read-only (get-text-property pos 'read-only)))
          (when (and read-only
                     (not (if (listp inhibit-read-only)
                              (memq read-only inhibit-read-only)
                            inhibit-read-only)))
            (signal 'text-read-only nil)))
        (setq pos (1+ pos))))
    (let* ((text (buffer-substring beg end))
           (replacement (downcase text)))
      (unless (equal text replacement)
        (let ((buffer (current-buffer)))
          (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer))
              ;; Use the same storage interface as subst-char-in-region.
              ;; Delete/insert would collapse markers and overlays within
              ;; a region whose character positions have not changed.
              (let* ((full (nelisp-buffer-string buffer))
                     (saved-point (point))
                     (result (concat (substring full 0 (1- beg)) replacement
                                     (substring full (1- end))))
                     (ext (gethash buffer emacs-buffer--state)))
                (when (and ext (not (eq (emacs-buffer--ext-undo-list ext) t)))
                  (setf (emacs-buffer--ext-undo-list ext)
                        (cons (cons beg end)
                              (cons (cons text beg)
                                    (emacs-buffer--ext-undo-list ext)))))
                (setf (nelisp-buffer-before-gap buffer)
                      (substring result 0 (1- saved-point)))
                (setf (nelisp-buffer-after-gap buffer)
                      (substring result (1- saved-point)))
                (remhash buffer nelisp-buffer--pending-point)
                (nelisp-buffer--bump-tick buffer)
                (setf (nelisp-buffer-modified buffer) t)
                (when (fboundp 'emacs-buffer-builtins--share-text)
                  (emacs-buffer-builtins--share-text buffer))
                (set-buffer-modified-p t))
            (let ((saved-point (point)))
              (unwind-protect
                  (progn (goto-char beg) (delete-region beg end)
                         (insert replacement))
                (goto-char saved-point))))))))
  nil)

(unless (fboundp 'downcase-region)
  (defun downcase-region (&rest args)
    "Convert BEG through END to lower case, leaving point unchanged.
For REGION-NONCONTIGUOUS-P, use the current region's bounds.

(fn BEG END &optional REGION-NONCONTIGUOUS-P)"
    (interactive "r")
    (emacs-cc-census-buffer-w201--arity 'downcase-region args 2 3)
    (if (nth 2 args)
        (dolist (bounds (region-bounds))
          (emacs-cc-census-buffer-w201--downcase (car bounds) (cdr bounds)))
      (emacs-cc-census-buffer-w201--downcase (car args) (cadr args)))
    nil))

(defun emacs-cc-census-buffer-w201--word-char-p (position)
  "Return whether POSITION is a word constituent in the current syntax."
  (let ((syntax (char-syntax (char-after position))))
    (or (eq syntax ?w)
        (and (boundp 'words-include-escapes) words-include-escapes
             (memq syntax '(?\\ ?/))))))

(defun emacs-cc-census-buffer-w201--word-end (position count)
  "Return the endpoint after scanning COUNT words from POSITION."
  (let ((forward (> count 0)) (remaining (abs count))
        (low (point-min)) (high (point-max)))
    (while (and (> remaining 0)
                (if forward (< position high) (> position low)))
      (while (and (if forward (< position high) (> position low))
                  (not (emacs-cc-census-buffer-w201--word-char-p
                        (if forward position (1- position)))))
        (setq position (+ position (if forward 1 -1))))
      (while (and (if forward (< position high) (> position low))
                  (emacs-cc-census-buffer-w201--word-char-p
                   (if forward position (1- position))))
        (setq position (+ position (if forward 1 -1))))
      (setq remaining (1- remaining)))
    position))

(unless (fboundp 'downcase-word)
  (defun downcase-word (&rest args)
    "Downcase ARG words from point; negative ARG leaves point unchanged.

(fn ARG)"
    (interactive "p")
    (emacs-cc-census-buffer-w201--arity 'downcase-word args 1 1)
    (let ((arg (car args)) (start (point)) end)
      (unless (fixnump arg)
        (signal 'wrong-type-argument (list 'fixnump arg)))
      ;; The bundle's older forward-word operates on a different buffer
      ;; owner.  Scan the current buffer using its public character API.
      (setq end (emacs-cc-census-buffer-w201--word-end start arg))
      (emacs-cc-census-buffer-w201--downcase start end)
      (when (> arg 0) (goto-char end)))
    nil))

(provide 'emacs-cc-census-buffer-w201)
;;; emacs-cc-census-buffer-w201.el ends here
