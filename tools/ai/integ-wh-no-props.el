;;; integ-wh-no-props.el --- the -no-properties producers return strings without intervals  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same bytes.  GNU's Fbuffer_substring_no_properties
;; (editfns.c) makes a fresh string with no intervals; Fbuffer_substring copies the buffer's intervals.  This runtime keeps a
;; buffer's text in gap strings and its properties in the buffer, and the native concat and substring copy a source string's
;; intervals (GNU's concat_to_string and copy_text_properties), so a gap string that was built from propertized text carries
;; those properties and every result sliced from it does too.  Each case prints the printed representation of the result
;; (#("abc" 0 3 (p 7)) when it has intervals), its text-properties-at for each position, and its interval list.

(defun iwh-np (tag s)
  (princ (format "NP %s ## %S | %S | %S\n" tag s
                 (and (stringp s) (let ((i 0) r) (while (< i (length s)) (push (text-properties-at i s) r) (setq i (1+ i))) (nreverse r)))
                 ;; the first position where the properties change, nil for a string without intervals
                 (and (stringp s) (> (length s) 0) (next-property-change 0 s)))))
(defmacro iwh-nps (tag form)
  `(condition-case e (iwh-np ,tag ,form) (error (princ (format "NP %s ## SIGNAL %S\n" ,tag (car e))))))
(defun iwh-np-buf (tag setup &rest calls)
  "Run SETUP in a fresh buffer, then print each (LABEL . FUNCTION) of CALLS."
  (with-temp-buffer
    (funcall setup)
    (dolist (c calls) (iwh-nps (concat tag " " (car c)) (funcall (cdr c))))))

(defun iwh-setup-plain () (insert "xx") (insert (propertize "abc" 'probe 7)) (insert "yy"))
(defun iwh-setup-only () (insert (propertize "abc" 'probe 7)))
(defun iwh-setup-multi ()
  (insert (propertize "ab" 'p 1)) (insert "-") (insert (propertize "cd" 'p 3)) (insert (propertize "e" 'face 'bold)))
(defun iwh-setup-then-removed ()
  (insert (propertize "abcd" 'probe 7)) (remove-text-properties 1 5 '(probe nil)))
(defun iwh-setup-then-set ()
  (insert (propertize "abcd" 'probe 7)) (set-text-properties 1 3 nil))
(defun iwh-setup-then-put ()
  (insert "abcd") (put-text-property 2 4 'probe 9))
(defun iwh-setup-after-delete ()
  (insert "ab") (insert (propertize "cd" 'probe 7)) (insert "ef") (delete-region 3 5))
(defun iwh-setup-after-erase ()
  (insert (propertize "abcd" 'probe 7)) (erase-buffer) (insert "wxyz"))
(defun iwh-setup-mb ()
  (insert (propertize "日本" 'probe 7)) (insert "é"))

(let ((calls (list (cons "bsnp 1 4" (lambda () (buffer-substring-no-properties 1 4)))
                   (cons "bsnp 1 3" (lambda () (buffer-substring-no-properties 1 3)))
                   (cons "bsnp 2 4" (lambda () (buffer-substring-no-properties 2 4)))
                   (cons "bsnp 2 3" (lambda () (buffer-substring-no-properties 2 3)))
                   (cons "bsnp 2 2" (lambda () (buffer-substring-no-properties 2 2)))
                   (cons "bsnp 4 1 reversed" (lambda () (buffer-substring-no-properties 4 1)))
                   (cons "bsnp point-min max" (lambda () (buffer-substring-no-properties (point-min) (point-max))))
                   (cons "bs 1 4" (lambda () (buffer-substring 1 4)))
                   (cons "bs 2 3" (lambda () (buffer-substring 2 3)))
                   (cons "bs point-min max" (lambda () (buffer-substring (point-min) (point-max))))
                   (cons "buffer-string" (lambda () (buffer-string)))
                   (cons "bsnp then substring" (lambda () (substring (buffer-substring-no-properties (point-min) (point-max)) 0 1)))
                   (cons "bsnp then concat" (lambda () (concat (buffer-substring-no-properties (point-min) (point-max)) "z")))
                   (cons "bsnp twice" (lambda () (buffer-substring-no-properties (point-min) (point-max)) (buffer-substring-no-properties (point-min) (point-max))))
                   (cons "bsnp eq not twice" (lambda () (eq (buffer-substring-no-properties (point-min) (point-max)) (buffer-substring-no-properties (point-min) (point-max))))))))
  (dolist (s '(("plain+propertized+plain" . iwh-setup-plain) ("propertized only" . iwh-setup-only)
               ("multi" . iwh-setup-multi) ("after remove-text-properties" . iwh-setup-then-removed)
               ("after set-text-properties" . iwh-setup-then-set) ("put-text-property" . iwh-setup-then-put)
               ("after delete-region" . iwh-setup-after-delete) ("after erase-buffer" . iwh-setup-after-erase)
               ("multibyte" . iwh-setup-mb)))
    (apply #'iwh-np-buf (car s) (cdr s) calls)))

;; the other -no-properties producers
(let ((ps (propertize "abcde" 'probe 7)))
  (iwh-nps "substring-no-properties all" (substring-no-properties ps))
  (iwh-nps "substring-no-properties 1 3" (substring-no-properties ps 1 3))
  (iwh-nps "substring-no-properties 2" (substring-no-properties ps 2))
  (iwh-nps "substring-no-properties neg" (substring-no-properties ps -3 -1))
  (iwh-nps "substring control" (substring ps 1 3))
  (iwh-nps "copy-sequence control" (copy-sequence ps))
  (iwh-nps "concat control" (concat ps "x"))
  (iwh-nps "string-trim control" (string-trim (concat " " ps " ")))
  (iwh-nps "match-string-np string" (progn (string-match "b\\(cd\\)" ps) (match-string-no-properties 1 ps)))
  (iwh-nps "match-string string" (progn (string-match "b\\(cd\\)" ps) (match-string 1 ps)))
  (iwh-nps "match-string-np buffer"
           (with-temp-buffer (insert ps) (goto-char (point-min)) (re-search-forward "b\\(cd\\)") (match-string-no-properties 1)))
  (iwh-nps "match-string buffer"
           (with-temp-buffer (insert ps) (goto-char (point-min)) (re-search-forward "b\\(cd\\)") (match-string 1)))
  (iwh-nps "insert-and-inherit control"
           (with-temp-buffer (insert (propertize "hello" 'face 'bold)) (buffer-string)))
  (iwh-nps "propertize roundtrip np" (substring-no-properties (propertize "q" 'a 1)))
  (iwh-nps "format control" (format "<%s>" ps))
  (iwh-nps "buffer-substring errors range" (with-temp-buffer (insert "abc") (condition-case e (buffer-substring-no-properties 1 99) (error (car e)))))
  (iwh-nps "buffer-substring errors type" (with-temp-buffer (insert "abc") (condition-case e (buffer-substring-no-properties 'a 2) (error (car e))))))
;; Not covered here, each a separate difference that predates the batch: a plain `insert' inside a propertized run extends the run
;; over the new text (GNU gives it no properties unless it is `insert-and-inherit'); two adjacent intervals with equal
;; plists print as one (GNU prints two); `add-text-properties' puts the new property after the old ones (GNU before them).
;; The properties of an inserted string belong to the buffer: queried through the buffer, sliced, and kept apart from the gap strings.
(defun iwh-np-bufq (tag setup)
  (with-temp-buffer
    (funcall setup)
    (let ((n (1- (point-max))) (i 1) props)
      (while (<= i n) (push (get-text-property i 'p) props) (setq i (1+ i)))
      (iwh-nps (concat tag " buffer-string") (buffer-string))
      (iwh-nps (concat tag " get-text-property") (nreverse props))
      (iwh-nps (concat tag " text-properties-at") (let ((i 1) r) (while (<= i n) (push (text-properties-at i) r) (setq i (1+ i))) (nreverse r)))
      (iwh-nps (concat tag " next-single-property-change") (list (next-single-property-change 1 'p) (and (> n 2) (next-single-property-change 3 'p))))
      (iwh-nps (concat tag " bs 2 4") (buffer-substring 2 (min 4 (1+ n))))
      (iwh-nps (concat tag " bsnp 2 4") (buffer-substring-no-properties 2 (min 4 (1+ n)))))))
(iwh-np-bufq "ins start" (lambda () (insert (propertize "bb" 'p 1)) (insert "cc")))
(iwh-np-bufq "ins middle" (lambda () (insert "aa") (insert (propertize "bb" 'p 1)) (insert "cc")))
(iwh-np-bufq "ins end" (lambda () (insert "aa") (insert (propertize "bb" 'p 1))))
(iwh-np-bufq "ins propertized inside a run" (lambda () (insert (propertize "abcd" 'p 1)) (goto-char 3) (insert (propertize "XY" 'p 2))))
(iwh-np-bufq "ins inside plain" (lambda () (insert "abcd") (goto-char 3) (insert (propertize "XY" 'p 2))))
(iwh-np-bufq "ins before a run" (lambda () (insert (propertize "cd" 'p 1)) (goto-char 1) (insert (propertize "ab" 'p 2))))
(iwh-np-bufq "ins two values" (lambda () (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2))))
(iwh-np-bufq "ins string with gaps" (lambda () (insert "aa") (insert (concat (propertize "ab" 'p 1) "-" (propertize "cd" 'p 3))) (insert "zz")))
(iwh-np-bufq "ins before markers" (lambda () (insert "aa") (insert-before-markers (propertize "bb" 'p 1)) (insert "cc")))
(iwh-np-bufq "ins then delete part" (lambda () (insert (propertize "abcdef" 'p 1)) (delete-region 2 4)))
(iwh-np-bufq "ins then delete all" (lambda () (insert (propertize "abcdef" 'p 1)) (delete-region 1 7) (insert "xy")))
(iwh-np-bufq "ins then erase" (lambda () (insert (propertize "abcdef" 'p 1)) (erase-buffer) (insert "xy")))
(iwh-np-bufq "ins then put" (lambda () (insert (propertize "abcdef" 'p 1)) (put-text-property 2 4 'p 5)))
(iwh-np-bufq "ins then remove" (lambda () (insert (propertize "abcdef" 'p 1)) (remove-text-properties 3 5 '(p nil))))
(iwh-np-bufq "ins multibyte" (lambda () (insert "é") (insert (propertize "日本" 'p 1)) (insert "z")))
(iwh-np-bufq "ins plain after props" (lambda () (insert (propertize "ab" 'p 1)) (insert "cd") (insert "ef")))
(iwh-np-bufq "many inserts" (lambda () (let ((i 0)) (while (< i 30) (insert (propertize "x" 'p i)) (insert "-") (setq i (1+ i))))))
(princ "INTEG-WH-NO-PROPS DONE\n")
