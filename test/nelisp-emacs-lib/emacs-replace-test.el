;;; emacs-replace-test.el --- ERT for emacs-replace  -*- lexical-binding: t; -*-

;;; Commentary:

;; occur / replace / line-filter tests.  All operations are pure buffer units
;; driven by `string-match' (no `re-search-forward'), so they validate the
;; Layer 2 logic independently of the reader.

;;; Code:

(require 'ert)
(require 'emacs-replace)

;;;; --- occur --------------------------------------------------------

(ert-deftest emacs-replace-test/occur-matches-line-and-pos ()
  (with-temp-buffer
    (insert "alpha 1\nbeta 2\ngamma 3\nalpha 4\n")
    (let ((m (emacs-occur-matches "alpha")))
      (should (= 2 (length m)))
      (should (= 1 (plist-get (nth 0 m) :line)))
      (should (= 1 (plist-get (nth 0 m) :pos)))
      (should (equal "alpha 1" (plist-get (nth 0 m) :text)))
      (should (= 4 (plist-get (nth 1 m) :line)))
      (should (= 24 (plist-get (nth 1 m) :pos))))))

(ert-deftest emacs-replace-test/occur-builds-buffer-and-goto ()
  (with-temp-buffer
    (insert "alpha 1\nbeta 2\ngamma 3\nalpha 4\n")
    (let ((count (emacs-occur "alpha")))
      (unwind-protect
          (progn
            (should (= 2 count))
            (with-current-buffer emacs-occur-buffer-name
              (should (string-match-p "2 matches for" (buffer-string)))
              (should (string-match-p "1:alpha 1" (buffer-string)))
              (should (string-match-p "4:alpha 4" (buffer-string))))
            ;; goto jumps to the 2nd match's source position (line 4)
            (let ((p (emacs-occur-goto 2)))
              (should (= 24 p))
              (should (= (point) p))))
        (when (get-buffer emacs-occur-buffer-name)
          (kill-buffer emacs-occur-buffer-name))))))

;;;; --- replace ------------------------------------------------------

(ert-deftest emacs-replace-test/replace-regexp-counts-and-rewrites ()
  (with-temp-buffer
    (insert "foo1 foo2 foo3")
    (let ((n (emacs-replace-regexp "foo[0-9]" "BAR")))
      (should (= 3 n))
      (should (equal "BAR BAR BAR" (buffer-string))))))

(ert-deftest emacs-replace-test/replace-regexp-no-match-leaves-buffer ()
  (with-temp-buffer
    (insert "nothing here")
    (should (= 0 (emacs-replace-regexp "zzz+" "X")))
    (should (equal "nothing here" (buffer-string)))))

(ert-deftest emacs-replace-test/replace-string-is-literal ()
  (with-temp-buffer
    (insert "a.b a.b axb")
    (let ((n (emacs-replace-string "a.b" "Z")))
      (should (= 2 n))               ; literal "a.b" only; "axb" untouched
      (should (equal "Z Z axb" (buffer-string))))))

(ert-deftest emacs-replace-test/how-many ()
  (with-temp-buffer
    (insert "x x y x")
    (should (= 3 (emacs-replace-how-many "x")))
    (should (= 0 (emacs-replace-how-many "q")))))

;;;; --- line filters -------------------------------------------------

(ert-deftest emacs-replace-test/flush-lines-drops-matching ()
  (with-temp-buffer
    (insert "keep1\ndrop me\nkeep2\ndrop me too\n")
    (let ((removed (emacs-replace-flush-lines "drop")))
      (should (= 2 removed))
      (should (equal "keep1\nkeep2\n" (buffer-string))))))

(ert-deftest emacs-replace-test/keep-lines-keeps-matching ()
  (with-temp-buffer
    (insert "yes a\nno b\nyes c\n")
    (let ((removed (emacs-replace-keep-lines "yes")))
      (should (= 1 removed))
      (should (equal "yes a\nyes c\n" (buffer-string))))))

;;;; --- query-replace ------------------------------------------------

(ert-deftest emacs-replace-test/query-replace-honours-decisions ()
  (with-temp-buffer
    (insert "x A x B x C")
    (goto-char (point-min))
    (let* ((decisions (list 'act 'skip 'act))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "x" "Z" decide)))
      (should (= 2 n))
      (should (equal "Z A x B Z C" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-act-all ()
  (with-temp-buffer
    (insert "a a a a")
    (goto-char (point-min))
    (let* ((decisions (list 'skip 'act-all))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "a" "Z" decide)))
      (should (= 3 n))               ; first skipped, the remaining three replaced
      (should (equal "a Z Z Z" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-quit-stops ()
  (with-temp-buffer
    (insert "a a a")
    (goto-char (point-min))
    (let* ((decisions (list 'act 'quit))
           (decide (lambda (_m _b _e) (pop decisions)))
           (n (emacs-query-replace "a" "Z" decide)))
      (should (= 1 n))
      (should (equal "Z a a" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-regexp-backref ()
  (with-temp-buffer
    (insert "f(1) f(2)")
    (goto-char (point-min))
    (let ((n (emacs-query-replace-regexp "f(\\([0-9]\\))" "g[\\1]"
                                         (lambda (_m _b _e) 'act))))
      (should (= 2 n))
      (should (equal "g[1] g[2]" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-region-is-direct-engine ()
  (with-temp-buffer
    (insert "x1 x2 x3")
    (goto-char (point-min))
    (let ((seen nil)
          (decisions (list 'act 'skip 'act-all)))
      (should
       (= 2
          (emacs-query-replace-region
           "x[0-9]" "Z"
           (lambda (matched beg end)
             (push (list matched beg end) seen)
             (pop decisions)))))
      (should (equal '(("x3" 6 8) ("x2" 3 5) ("x1" 1 3)) seen))
      (should (equal "Z x2 Z" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-session-steps ()
  (with-temp-buffer
    (insert "x A x B x C")
    (goto-char (point-min))
    (let ((session (emacs-query-replace-session-start "x" "Z")))
      (should (emacs-query-replace-session-active-p session))
      (should (equal "Replace x with Z? (y/n/!/q)"
                     (emacs-query-replace-session-message session)))
      (setq session (emacs-query-replace-session-handle-key session ?y))
      (setq session (emacs-query-replace-session-handle-key session ?n))
      (setq session (emacs-query-replace-session-handle-key session ?y))
      (should-not (emacs-query-replace-session-active-p session))
      (should (= 2 (emacs-query-replace-session-count session)))
      (should (equal "Z A x B Z C" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-session-prompt-and-decision ()
  (with-temp-buffer
    (insert "x")
    (goto-char (point-min))
    (let ((session (emacs-query-replace-session-start "x" "Z")))
      (should (equal "Replace x with Z? (y/n/!/q)"
                     (emacs-query-replace-session-prompt session)))
      (should (eq 'act (emacs-query-replace-session-decision ?y)))
      (should (eq 'act (emacs-query-replace-session-decision "SPC")))
      (should (eq 'skip (emacs-query-replace-session-decision "DEL")))
      (should (eq 'act-all (emacs-query-replace-session-decision ?!)))
      (should (eq 'quit (emacs-query-replace-session-decision "C-g")))
      (should (eq 'reask (emacs-query-replace-session-decision "other"))))))

(ert-deftest emacs-replace-test/query-replace-session-act-all ()
  (with-temp-buffer
    (insert "a a a a")
    (goto-char (point-min))
    (let ((session (emacs-query-replace-session-start "a" "Z")))
      (setq session (emacs-query-replace-session-handle-key session "n"))
      (setq session (emacs-query-replace-session-handle-key session "!"))
      (should-not (emacs-query-replace-session-active-p session))
      (should (= 3 (emacs-query-replace-session-count session)))
      (should (equal "Replaced 3 (! all)"
                     (emacs-query-replace-session-message session)))
      (should (equal "a Z Z Z" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-session-regexp-backref ()
  (with-temp-buffer
    (insert "f(1) f(2)")
    (goto-char (point-min))
    (let ((session
           (emacs-query-replace-session-start
            "f(\\([0-9]\\))" "g[\\1]" t)))
      (setq session
            (emacs-query-replace-session-handle-decision session 'act-all))
      (should (= 2 (emacs-query-replace-session-count session)))
      (should (equal "g[1] g[2]" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-run-command-uses-frontend-hooks ()
  (with-temp-buffer
    (insert "alpha beta alpha")
    (goto-char (point-min))
    (let ((answers '("alpha" "OMEGA"))
          (prompts nil)
          (confirmed nil)
          (after-count nil)
          (buffer (current-buffer)))
      (should
       (= 2
          (emacs-query-replace-run-command
           :read-string (lambda (prompt)
                          (push prompt prompts)
                          (pop answers))
           :read-confirmation (lambda (timeout)
                                (setq confirmed timeout))
           :current-buffer (lambda () buffer)
           :start-function #'point
           :after-success
           (lambda (session)
             (setq after-count
                   (emacs-query-replace-session-count session))))))
      (should (= 2 after-count))
      (should (= 1000 confirmed))
      (should (equal "OMEGA beta OMEGA" (buffer-string)))
      (should (equal '("Query replace alpha with: " "Query replace: ")
                     prompts)))))

(ert-deftest emacs-replace-test/query-replace-run-command-supports-callback-prompt ()
  (with-temp-buffer
    (insert "alpha beta alpha")
    (goto-char (point-min))
    (let ((buffer (current-buffer))
          prompts
          callbacks
          state
          pending
          status)
      (emacs-query-replace-run-command
       :begin-prompt (lambda (prompt callback)
                       (push prompt prompts)
                       (push callback callbacks))
       :current-buffer (lambda () buffer)
       :start-function (lambda ()
                         (with-current-buffer buffer
                           (point)))
       :state-function (lambda (session)
                         (setq state session))
       :pending-function (lambda (active)
                           (setq pending active))
       :status-function (lambda (message)
                          (setq status message)))
      (should (equal '("Query replace: ") prompts))
      (funcall (pop callbacks) "alpha")
      (should (equal '("Query replace alpha with: " "Query replace: ")
                     prompts))
      (funcall (pop callbacks) "OMEGA")
      (should (emacs-query-replace-session-active-p state))
      (should pending)
      (should (equal "Replace alpha with OMEGA? (y/n/!/q)" status))
      (setq state (emacs-query-replace-session-handle-key state "!"))
      (should (= 2 (emacs-query-replace-session-count state)))
      (should (equal "OMEGA beta OMEGA" (buffer-string))))))

(ert-deftest emacs-replace-test/query-replace-run-command-callback-reports-empty-from ()
  (let (prompts callbacks status)
    (emacs-query-replace-run-command
     :begin-prompt (lambda (prompt callback)
                     (push prompt prompts)
                     (push callback callbacks))
     :status-function (lambda (message)
                        (setq status message)))
    (funcall (pop callbacks) "")
    (should (equal '("Query replace: ") prompts))
    (should (equal "query-replace: empty FROM" status))
    (should-not callbacks)))

(ert-deftest emacs-replace-test/replace-install-binds-standard-names ()
  (let ((old-occur (and (fboundp 'occur) (symbol-function 'occur)))
        (old-how-many (and (fboundp 'how-many) (symbol-function 'how-many)))
        (old-replace-regexp
         (and (fboundp 'replace-regexp) (symbol-function 'replace-regexp)))
        (old-replace-string
         (and (fboundp 'replace-string) (symbol-function 'replace-string)))
        (old-flush-lines
         (and (fboundp 'flush-lines) (symbol-function 'flush-lines)))
        (old-keep-lines
         (and (fboundp 'keep-lines) (symbol-function 'keep-lines)))
        (old-query-replace
         (and (fboundp 'query-replace) (symbol-function 'query-replace)))
        (old-query-replace-regexp
         (and (fboundp 'query-replace-regexp)
              (symbol-function 'query-replace-regexp))))
    (unwind-protect
        (progn
          (emacs-replace-install)
          (should (eq (symbol-function 'occur) #'emacs-occur))
          (should (eq (symbol-function 'how-many) #'emacs-replace-how-many))
          (should (eq (symbol-function 'replace-regexp)
                      #'emacs-replace-regexp))
          (should (eq (symbol-function 'replace-string)
                      #'emacs-replace-string))
          (should (eq (symbol-function 'flush-lines)
                      #'emacs-replace-flush-lines))
          (should (eq (symbol-function 'keep-lines)
                      #'emacs-replace-keep-lines))
          (should (eq (symbol-function 'query-replace)
                      #'emacs-query-replace))
          (should (eq (symbol-function 'query-replace-regexp)
                      #'emacs-query-replace-regexp)))
      (dolist (entry `((occur . ,old-occur)
                       (how-many . ,old-how-many)
                       (replace-regexp . ,old-replace-regexp)
                       (replace-string . ,old-replace-string)
                       (flush-lines . ,old-flush-lines)
                       (keep-lines . ,old-keep-lines)
                       (query-replace . ,old-query-replace)
                       (query-replace-regexp . ,old-query-replace-regexp)))
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(provide 'emacs-replace-test)

;;; emacs-replace-test.el ends here
