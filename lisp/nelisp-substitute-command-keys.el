;;; nelisp-substitute-command-keys.el --- scoped doc key expansion -*- lexical-binding: t; -*-

(require 'cl-lib)

;; This provider covers the explicit-map command escapes used by the genuine
;; calendar source.  Implicit active-map discovery and keymap summaries need
;; frame/buffer APIs that this headless runtime does not expose.

(defun nl-substitute-command-keys--walk (map command prefix seen state)
  (unless (memq map seen)
    (map-keymap
     (lambda (event binding)
       (cond
        ((keymapp binding)
         (nl-substitute-command-keys--walk
          binding command
          (vconcat prefix (if (vectorp event) event (vector event)))
          (cons map seen) state))
        ((eq binding command)
         (if (or (integerp event) (vectorp event) (symbolp event))
             (aset state 0
                   (cons (vconcat prefix (if (vectorp event) event (vector event)))
                         (aref state 0)))
           (aset state 1 t)))))
     map)))

(defun nl-substitute-command-keys--find (command map)
  (let ((state (vector nil nil)))
    (when map (nl-substitute-command-keys--walk map command [] nil state))
    (unless (aref state 0)
      (when (and (boundp 'global-map) (keymapp global-map)
                 (not (eq map global-map)))
        (nl-substitute-command-keys--walk global-map command [] nil state)))
    (when (and (not (aref state 0)) (aref state 1))
      (error "Matching binding uses an unsupported key event"))
    (car (sort (aref state 0)
               (lambda (a b)
                 (if (= (length a) (length b))
                     (string< (key-description a) (key-description b))
                   (< (length a) (length b))))))))

(defun nl-substitute-command-keys--delimited (string start close)
  (let ((end start))
    (while (and (< end (length string)) (/= (aref string end) close))
      (setq end (1+ end)))
    (unless (< end (length string))
      (error "Unterminated substitute-command-keys escape"))
    (cons (substring string start end) (1+ end))))

(defun nl-substitute-command-keys--escape (string i map explicit-map no-face)
  "Expand the escape at I; return next index, text, map, and explicit-map flag."
  (let ((kind (aref string (1+ i))))
    (cl-case kind
      (61 (list (min (length string) (+ i 3))
                (if (< (+ i 2) (length string))
                    (substring string (+ i 2) (+ i 3)) "") map explicit-map))
      (60 (let* ((field (nl-substitute-command-keys--delimited
                         string (+ i 2) 62))
                 (name (intern (car field))))
            (unless (and (boundp name) (keymapp (symbol-value name)))
              (error "Keymap %s is not currently defined" name))
            (list (cdr field) "" (symbol-value name) t)))
      (91 (unless explicit-map
            (error "Implicit active-map lookup is unsupported"))
          (let* ((field (nl-substitute-command-keys--delimited
                         string (+ i 2) 93))
                 (command (intern (car field)))
                 (key (nl-substitute-command-keys--find command map))
                 (text (if key (key-description key)
                         (concat "M-x " (symbol-name command)))))
            (list (cdr field)
                  (if no-face text
                    (propertize text 'face 'help-key-binding
                                'font-lock-face 'help-key-binding))
                  map explicit-map)))
      (otherwise (error "Unsupported substitute-command-keys escape: \\%c" kind)))))

(defun nl-substitute-command-keys (string &optional no-face _include-menus)
  "Expand explicit-map `\\<MAPVAR>' and `\\[COMMAND]' escapes in STRING.
Unqualified command escapes and unsupported documentation forms signal an
error because active local-map lookup and map summaries are unavailable."
  (if (null string)
      nil
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (let ((i 0) (length (length string)) (map nil) (explicit-map nil) (out ""))
      (while (< i length)
        (if (/= (aref string i) ?\\)
            (setq out (concat out (substring string i (1+ i)))
                  i (1+ i))
          (when (= (1+ i) length)
            (error "Trailing backslash in substitute-command-keys input"))
          (let ((result (nl-substitute-command-keys--escape
                         string i map explicit-map no-face)))
            (setq i (nth 0 result)
                  out (concat out (nth 1 result))
                  map (nth 2 result)
                  explicit-map (nth 3 result)))))
      (if (string= out string) string out))))

(unless (fboundp 'substitute-command-keys)
  (defalias 'substitute-command-keys #'nl-substitute-command-keys))

(provide 'nelisp-substitute-command-keys)
;;; nelisp-substitute-command-keys.el ends here
