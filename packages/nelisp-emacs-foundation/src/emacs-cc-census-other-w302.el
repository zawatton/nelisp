;;; emacs-cc-census-other-w302.el --- Help, hook, and invocation primitives  -*- lexical-binding: t; -*-

;;; Code:

;; Positioned symbols require a native object type.  Likewise, backtrace
;; enumeration, obarray bucket layout, and the focus event queue are not
;; exposed by this runtime.  Do not replace them with fabricated Lisp data.

(defun emacs-cc-census-other-w302--arity (name args minimum maximum)
  "Check the number of ARGS accepted by NAME."
  (let ((count (length args)))
    (when (or (< count minimum) (and maximum (> count maximum)))
      (signal 'wrong-number-of-arguments (list (symbol-function name) count)))))

(defun emacs-cc-census-other-w302--keyelt (object)
  "Remove menu labels from OBJECT without loading or filtering commands."
  (let ((done nil))
    (while (and (consp object) (not done))
      (cond
       ((eq (car object) 'menu-item)
        (if (consp (cdr object))
            (progn
              (setq object (cddr object))
              (when (consp object) (setq object (car object))))
          (setq done t)))
       ((stringp (car object)) (setq object (cdr object)))
       (t (setq done t))))
    object))

(defun emacs-cc-census-other-w302--event-description (event)
  "Describe EVENT using GNU's character and modifier conventions."
  (when (and (consp event) (integerp (car event)) (integerp (cdr event)))
    (setq event (concat (emacs-cc-census-other-w302--event-description (car event))
                        ".."
                        (emacs-cc-census-other-w302--event-description (cdr event)))))
  (when (consp event)
    (let ((tail event)
          (lucid (not (memq (car event)
                            '(help-echo vertical-line mode-line tab-line header-line)))))
      (while (and lucid (consp tail))
        (unless (or (integerp (car tail)) (symbolp (car tail))) (setq lucid nil))
        (setq tail (cdr tail)))
      (setq event (if (and lucid (null tail)) (event-convert-list event) (car event)))))
  (cond
   ((integerp event)
    (let* ((code (logand event 268435455))
           (base (logand code (lognot 264241152)))
           (meta-tab (and (= base 9) (/= 0 (logand code 134217728))))
           (prefix ""))
      (if (> base 4194303)
          (format "[%d]" code)
        (dolist (modifier '((4194304 . "A-") (67108864 . "C-")
                            (16777216 . "H-") (134217728 . "M-")
                            (33554432 . "S-") (8388608 . "s-")))
          (when (or (/= 0 (logand code (car modifier)))
                    (and (= (car modifier) 67108864)
                         (or meta-tab
                             (and (< base 32) (not (memq base '(9 13 27)))))))
            (setq prefix (concat prefix (cdr modifier)))))
        (concat prefix
                (cond ((= base 27) "ESC")
                      (meta-tab "i")
                      ((= base 9) "TAB")
                      ((= base 13) "RET")
                      ((< base 32)
                       (char-to-string (+ base (if (and (> base 0) (<= base 26))
                                                  96 64))))
                      ((= base 127) "DEL")
                      ((= base 32) "SPC")
                      (t (char-to-string base)))))))
   ((symbolp event)
    (let ((name (symbol-name event)) (index 0))
      (while (and (< index (- (length name) 3))
                  (= (aref name (1+ index)) ?-)
                  (memq (aref name index) '(?C ?M ?S ?s ?H ?A)))
        (setq index (+ index 2)))
      (concat (substring name 0 index) "<" (substring name index) ">")))
   ((stringp event) (copy-sequence event))
   (t (error "KEY must be an integer, cons, symbol, or string"))))

(defun emacs-cc-census-other-w302--key-description (key prefix)
  "Describe KEY after PREFIX, including escape and unibyte meta keys."
  (let ((events nil) (parts nil) (pending-meta nil)
        (escape (if (boundp 'meta-prefix-char) meta-prefix-char 27)))
    ;; GNU checks the prefix length before validating its sequence type.
    (length prefix)
    (unless (or (null prefix) (stringp prefix) (vectorp prefix) (consp prefix))
      (signal 'wrong-type-argument (list 'arrayp prefix)))
    (setq events (append prefix nil))
    (when (and (stringp prefix) (not (multibyte-string-p prefix)))
      (setq events (mapcar (lambda (character)
                            (if (>= character 128)
                                (logxor character 134217856)
                              character))
                          events)))
    (setq events (append events (list key)))
    (while events
      (let ((event (car events)) (emit t))
        (cond
         (pending-meta
          (if (or (not (integerp event)) (eq event escape)
                  (/= 0 (logand event 134217728)))
              (progn
                (push (emacs-cc-census-other-w302--event-description escape) parts)
                (if (eq event escape)
                    (setq emit nil)
                  (setq pending-meta nil)))
            (setq event (logior event 134217728) pending-meta nil)))
         ((eq event escape) (setq pending-meta t emit nil)))
        (when emit
          (push (emacs-cc-census-other-w302--event-description event) parts)))
      (setq events (cdr events)))
    (when pending-meta
      (push (emacs-cc-census-other-w302--event-description escape) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun emacs-cc-census-other-w302--insert-key (key prefix)
  "Insert a fontified description of KEY following PREFIX."
  (let ((description (emacs-cc-census-other-w302--key-description key prefix)))
    (add-text-properties 0 (length description)
                         '(font-lock-face help-key-binding) description)
    (insert description)))

(defun emacs-cc-census-other-w302--shadow (maps key)
  "Look up KEY in MAPS, ignoring an overlong key sequence result."
  (let ((binding (lookup-key maps (vector key) t)))
    (unless (and (integerp binding) (>= binding 0)) binding)))

(defun emacs-cc-census-other-w302--shadow-boundary (maps start end binding)
  "Find the first shadow change after START, without scanning characters."
  (let ((limit end))
    (dolist (map (if (keymapp maps) (list maps) maps))
      (map-keymap
       (lambda (key _definition)
         (let ((low (if (consp key) (car key) key))
               (high (if (consp key) (cdr key) key)))
           (when (and (integerp low) (integerp high))
             (dolist (boundary (list low (1+ high)))
               (when (and (> boundary start) (<= boundary limit)
                          (not (equal (emacs-cc-census-other-w302--shadow maps boundary)
                                      binding)))
                 (setq limit (1- boundary)))))))
       map))
    limit))

(unless (fboundp 'help--describe-vector)
  (defun help--describe-vector (&rest arguments)
    "Insert key descriptions from VECTOR using DESCRIBER.
The arguments are VECTOR PREFIX DESCRIBER PARTIAL SHADOW ENTIRE-MAP
and MENTION-SHADOW.  Suppress hidden commands and group equal bindings."
    (emacs-cc-census-other-w302--arity 'help--describe-vector arguments 7 7)
    (let* ((vector (nth 0 arguments)) (prefix (nth 1 arguments))
           (describer (nth 2 arguments)) (partial (nth 3 arguments))
           (shadow (nth 4 arguments)) (entire-map (nth 5 arguments))
           (mention-shadow (nth 6 arguments)) (first t)
           (standard-output (current-buffer)))
      (unless (or (vectorp vector) (char-table-p vector))
        (signal 'wrong-type-argument (list 'vector-or-char-table-p vector)))
      (let ((emit
             (lambda (start end definition shadowed-by)
               (when first (insert "\n") (setq first nil))
               (emacs-cc-census-other-w302--insert-key start prefix)
               (when (/= start end)
                 (insert " .. ")
                 (emacs-cc-census-other-w302--insert-key end prefix))
               (funcall describer definition)
               (when (and shadowed-by (not (eq shadowed-by definition)))
                 (backward-char 1)
                 (insert (if (symbolp shadowed-by)
                             (format-message "  (currently shadowed by `%s')"
                                             (symbol-name shadowed-by))
                           "  (currently shadowed)"))
                 (forward-char 1)))))
        (if (char-table-p vector)
            (progn
              (map-char-table
               (lambda (range value)
                 (let ((start (if (consp range) (car range) range))
                       (end (if (consp range) (cdr range) range))
                       (definition (emacs-cc-census-other-w302--keyelt value)))
                   (while (<= start end)
                     (let ((stop (if (< start 4194176) (min end 4194175) end))
                           (shadowed-by nil))
                       (when (and definition
                                  (not (and partial (symbolp definition)
                                            (get definition 'suppress-keymap)))
                                  (progn
                                    (setq shadowed-by
                                          (and shadow
                                               (emacs-cc-census-other-w302--shadow shadow start)))
                                    t)
                                  (or (not shadowed-by) (eq shadowed-by definition)
                                      mention-shadow)
                                  (or (not entire-map)
                                      (eq (lookup-key entire-map (vector start) t) definition)))
                         (when (and shadow
                                    (boundp 'describe-bindings-check-shadowing-in-ranges)
                                    describe-bindings-check-shadowing-in-ranges
                                    (not (and (eq describe-bindings-check-shadowing-in-ranges
                                                  'ignore-self-insert)
                                              (eq definition 'self-insert-command))))
                           (setq stop (emacs-cc-census-other-w302--shadow-boundary
                                       shadow start stop shadowed-by)))
                         (funcall emit start stop definition shadowed-by))
                       (setq start (1+ stop))))))
               vector)
              (let ((default (char-table-range vector nil)))
                (when default (insert "default") (funcall describer default))))
          (let ((index 0) (size (length vector)))
            (while (< index size)
              (let* ((start index)
                     (definition (emacs-cc-census-other-w302--keyelt (aref vector index)))
                     (shadowed-by nil))
                (when (and definition
                           (not (and partial (symbolp definition)
                                     (get definition 'suppress-keymap)))
                           (progn
                             (setq shadowed-by
                                   (and shadow
                                        (emacs-cc-census-other-w302--shadow shadow index)))
                             t)
                           (or (not shadowed-by) (eq shadowed-by definition) mention-shadow)
                           (or (not entire-map)
                               (eq (lookup-key entire-map (vector index) t) definition)))
                  (while (and (< (1+ index) size)
                              (equal definition
                                     (emacs-cc-census-other-w302--keyelt
                                      (aref vector (1+ index)))))
                    (setq index (1+ index)))
                  (funcall emit start index definition shadowed-by)))
              (setq index (1+ index))))))
      nil)))

(unless (fboundp 'invocation-directory)
  (defun invocation-directory (&rest arguments)
    "Return a copy of the directory containing the executable."
    (emacs-cc-census-other-w302--arity 'invocation-directory arguments 0 0)
    (copy-sequence invocation-directory)))

(unless (fboundp 'invocation-name)
  (defun invocation-name (&rest arguments)
    "Return a copy of the program name used to start the executable."
    (emacs-cc-census-other-w302--arity 'invocation-name arguments 0 0)
    (copy-sequence invocation-name)))

(unless (fboundp 'remove-pos-from-symbol)
  (defun remove-pos-from-symbol (&rest arguments)
    "Return a positioned symbol's bare symbol, or ARG unchanged."
    (emacs-cc-census-other-w302--arity 'remove-pos-from-symbol arguments 1 1)
    (let ((arg (car arguments)))
      (if (symbol-with-pos-p arg) (bare-symbol arg) arg))))

(defun emacs-cc-census-other-w302--global-hook (value wrapper args)
  "Call WRAPPER for functions in the default hook VALUE with ARGS."
  (if (or (not (consp value)) (eq (car value) 'lambda))
      (and value (apply wrapper value args))
    (let ((result nil))
      (while (and (consp value) (not result))
        (unless (eq (car value) t)
          (setq result (apply wrapper (car value) args)))
        (setq value (cdr value)))
      result)))

(unless (fboundp 'run-hook-wrapped)
  (defun run-hook-wrapped (&rest arguments)
    "Run HOOK through WRAP-FUNCTION with ARGS until it returns non-nil.
Local hook entries of t run the default hook at that position."
    (emacs-cc-census-other-w302--arity 'run-hook-wrapped arguments 2 nil)
    (let ((hook (car arguments)) (wrapper (cadr arguments))
          (args (cddr arguments)) (result nil))
      (unless (symbolp hook)
        (signal 'wrong-type-argument (list 'symbolp hook)))
      (when (boundp hook)
        (let ((value (symbol-value hook)))
          (if (or (not (consp value)) (functionp value))
              (when value (setq result (apply wrapper value args)))
            (while (and (consp value) (not result))
              (setq result
                    (if (eq (car value) t)
                        (emacs-cc-census-other-w302--global-hook
                         (default-value hook) wrapper args)
                      (apply wrapper (car value) args)))
              (setq value (cdr value))))))
      result)))

(provide 'emacs-cc-census-other-w302)
;;; emacs-cc-census-other-w302.el ends here
