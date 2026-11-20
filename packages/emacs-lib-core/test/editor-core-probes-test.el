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

(princ "PROBE-DONE\n")

;;; editor-core-probes-test.el ends here
