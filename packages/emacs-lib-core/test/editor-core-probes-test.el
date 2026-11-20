;;; editor-core-probes-test.el --- editor core behavior probes  -*- lexical-binding: t; -*-

;; Each check states GNU Emacs 31.1 behavior, so it passes on host Emacs (the
;; control) and must pass on NeLisp.  ERT is not usable inside the runtime
;; image, so checks print one PASS/FAIL row each and the runner
;; tools/emacs-lib/editor-core-probes-test.sh fails on any FAIL row.

(defvar editor-core-probes--failures 0)
(defmacro editor-core-probes-check (name &rest body)
  "Run BODY; the check passes iff it returns exactly t."
  `(let ((r (condition-case e (progn ,@body) (error (list 'ERR e)))))
     (unless (eq r t) (setq editor-core-probes--failures (1+ editor-core-probes--failures)))
     (princ (format "%s\t%s\t%S\n" (if (eq r t) "PASS" "FAIL") ,name (if (eq r t) nil r)))))

(defun editor-core-probes--count-edits (edit)
  "Run EDIT in a temp buffer with local change hooks; return (BEFORE . AFTER)."
  (let ((before nil) (after nil))
    (with-temp-buffer
      (insert "hello")
      (add-hook 'before-change-functions
                (lambda (&rest a) (push a before)) nil t)
      (add-hook 'after-change-functions
                (lambda (&rest a) (push a after)) nil t)
      (funcall edit))
    (cons (nreverse before) (nreverse after))))

(editor-core-probes-check "after-change-local-insert"
  (let ((x 0))
    (with-temp-buffer
      (add-hook 'after-change-functions (lambda (&rest _) (setq x (1+ x))) nil t)
      (insert "a"))
    (= x 1)))

(editor-core-probes-check "change-hooks-args"
  (and
   (equal (editor-core-probes--count-edits (lambda () (goto-char 3) (insert "XY")))
          '(((3 3)) . ((3 5 0))))
   (equal (editor-core-probes--count-edits (lambda () (delete-region 2 4)))
          '(((2 4)) . ((2 2 2))))
   (equal (editor-core-probes--count-edits (lambda () (goto-char 2) (delete-char 1)))
          '(((2 3)) . ((2 2 1))))
   (equal (editor-core-probes--count-edits (lambda () (goto-char 1) (insert-char ?z 3)))
          '(((1 1)) . ((1 4 0))))
   (equal (editor-core-probes--count-edits #'erase-buffer)
          '(((1 6)) . ((1 1 5))))
   (equal (editor-core-probes--count-edits
           (lambda () (goto-char 1) (insert-before-markers "q")))
          '(((1 1)) . ((1 2 0))))))

(editor-core-probes-check "change-hooks-inhibited"
  (let ((n 0))
    (with-temp-buffer
      (add-hook 'after-change-functions (lambda (&rest _) (setq n (1+ n))) nil t)
      (let ((inhibit-modification-hooks t)) (insert "abc")))
    (= n 0)))

(editor-core-probes-check "change-hooks-global-and-local"
  (let ((log nil))
    (let ((after-change-functions (list (lambda (&rest _) (push 'g log)))))
      (with-temp-buffer
        (add-hook 'after-change-functions (lambda (&rest _) (push 'l log)) nil t)
        (insert "a")))
    (equal (sort (mapcar #'symbol-name log) #'string<) '("g" "l"))))

(editor-core-probes-check "run-with-timer fires during sit-for"
  (let ((x nil))
    (run-with-timer 0.05 nil (lambda () (setq x t)))
    (dotimes (_ 30) (unless x (sit-for 0.05)))
    x))

(editor-core-probes-check "repeating timer fires repeatedly and cancels"
  (let ((n 0) (tm nil))
    (setq tm (run-with-timer 0.02 0.02 (lambda () (setq n (1+ n)))))
    (dotimes (_ 40) (when (< n 3) (sit-for 0.02)))
    (cancel-timer tm)
    (let ((seen n))
      (sit-for 0.1)
      (and (>= seen 3) (= n seen)))))

(editor-core-probes-check "call-interactively forward-word"
  (with-temp-buffer
    (insert "foo bar") (goto-char 1)
    (call-interactively 'forward-word)
    (= (point) 4)))

(editor-core-probes-check "C-level edit and motion commands are commands"
  (let ((bad nil))
    (dolist (f '(forward-word forward-char backward-char forward-line delete-char
                 delete-region erase-buffer goto-char insert-char narrow-to-region
                 widen upcase-word upcase-region search-forward re-search-forward
                 move-to-column indent-to kill-local-variable make-local-variable
                 write-region rename-file copy-file eval-buffer eval-region))
      (unless (commandp f) (push f bad)))
    (or (null bad) bad)))

(editor-core-probes-check "interactive spec of C commands matches GNU"
  (and (equal (interactive-form 'forward-word) '(interactive "^p"))
       (equal (interactive-form 'delete-region) '(interactive "r"))
       (equal (interactive-form 'forward-line) '(interactive "^p"))))

(editor-core-probes-check "call-interactively motion/edit with prefix"
  (with-temp-buffer
    (insert "one two three four") (goto-char 1)
    (let ((current-prefix-arg 2)) (call-interactively 'forward-word))
    (and (= (point) 8)
         (progn (call-interactively 'backward-char) (= (point) 7))
         (progn (let ((current-prefix-arg 2)) (call-interactively 'delete-char))
                (equal (buffer-string) "one twthree four")))))

(editor-core-probes-check "shift_jis decode"
  (equal (decode-coding-string "\x93\xfa" 'shift_jis) "日"))

(editor-core-probes-check "japanese coding systems round trip"
  (let ((bad nil) (text "日本語Aｱabc"))
    (dolist (c '(shift_jis sjis japanese-shift-jis cp932 euc-jp japanese-iso-8bit
                 iso-2022-jp))
      (let ((enc (encode-coding-string text c)))
        (unless (and (not (multibyte-string-p enc))
                     (equal (decode-coding-string enc c)
                            (if (eq c 'iso-2022-jp) "日本語A abc" text)))
          (push c bad))))
    (or (null bad) bad)))

(editor-core-probes-check "japanese encodings match GNU bytes"
  (and (equal (append (encode-coding-string "日" 'shift_jis) nil) '(147 250))
       (equal (append (encode-coding-string "日" 'euc-jp) nil) '(198 252))
       (equal (append (encode-coding-string "ｱ" 'euc-jp) nil) '(142 177))
       (equal (append (encode-coding-string "日" 'iso-2022-jp) nil)
              '(27 36 66 70 124 27 40 66))
       (equal (append (encode-coding-string "A\n" 'shift_jis-dos) nil) '(65 13 10))
       (equal (decode-coding-string "\306\374" 'euc-jp) "日")
       (equal (decode-coding-string "\e$BF|\e(B" 'iso-2022-jp) "日")))

(princ "PROBE-DONE\n")

;;; editor-core-probes-test.el ends here
