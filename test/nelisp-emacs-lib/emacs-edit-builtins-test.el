;;; emacs-edit-builtins-test.el --- ERT for emacs-edit-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 editing-command MVP.  Under host Emacs the
;; `unless (fboundp ...)' gates skip our defaliases (= host's C
;; builtins win), so behavioural assertions exercise the substrate
;; via `nelisp-ec-*' calls + polyfill-body lambdas.  Featurep /
;; fboundp parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-edit-builtins)
(require 'cl-lib)

(defmacro emacs-edit-builtins-test--with-fresh-buffer (text &rest body)
  "Run BODY against a fresh nelisp-ec buffer pre-filled with TEXT.
Also resets kill-ring + kill-ring-yank-pointer."
  (declare (indent 1) (debug (form body)))
  (let ((buf (make-symbol "buf")))
    `(let ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (nelisp-ec--match-data nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil))
       (let ((,buf (nelisp-ec-generate-new-buffer "edit")))
         (unwind-protect
             (nelisp-ec-with-current-buffer ,buf
               (nelisp-ec-insert ,text)
               (nelisp-ec-goto-char (nelisp-ec-point-min))
               ,@body)
           (nelisp-ec-kill-buffer ,buf))))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-edit-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-edit-builtins))
  (dolist (sym '(self-insert-command newline delete-backward-char
                 ensure-empty-lines
                 kill-new copy-region-as-kill kill-region kill-line
                 yank forward-word backward-word))
    (should (fboundp sym)))
  (should (boundp 'kill-ring))
  (should (boundp 'kill-ring-max))
  (should (boundp 'kill-ring-yank-pointer))
  (should (boundp 'last-command-event)))

;;;; B. self-insert-command body — inserts char N times

(defun emacs-edit-builtins-test--self-insert (char &optional n)
  (let* ((count (or n 1))
         (s (cond ((stringp char) char)
                  ((integerp char) (string char))
                  (t (error "bad")))))
    (let ((i 0))
      (while (< i count)
        (nelisp-ec-insert s)
        (setq i (+ i 1))))))

(ert-deftest emacs-edit-builtins-test/self-insert-body-N-times ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (emacs-edit-builtins-test--self-insert ?a 3)
    (should (equal "aaa" (nelisp-ec-buffer-string)))
    (emacs-edit-builtins-test--self-insert "bc")
    (should (equal "aaabc" (nelisp-ec-buffer-string)))))

;;;; C. newline body — inserts \n N times

(ert-deftest emacs-edit-builtins-test/newline-body-N-times ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((newline-impl (lambda (n)
                          (let ((c (or n 1)) (i 0))
                            (while (< i c)
                              (nelisp-ec-insert "\n")
                              (setq i (+ i 1)))))))
      (funcall newline-impl 2)
      (should (equal "abc\n\n" (nelisp-ec-buffer-string))))))

;;;; D. delete-backward-char body

(ert-deftest emacs-edit-builtins-test/delete-backward-char-body ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (nelisp-ec-delete-char -2)
    (should (equal "hel" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-edit-builtins-test/delete-backward-char-polyfill-shape ()
  "Track X (2026-05-04) regression for the user-reported
`wrong-number-of-arguments: lambda (expected 1, got 0)' on
Backspace: the nelisp-driver polyfill must (a) make N optional
with a default of 1 so `call-interactively' supplies 0 args
without erroring, and (b) carry `(interactive \"p\")' so a
prefix arg actually flows through.  Read the source form
directly instead of `fboundp'-trampolining since under the
host driver the upstream `delete-backward-char' wins via the
install-function guard."
  (let* ((file (locate-library "emacs-edit-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (re-search-forward
               "(when (emacs-edit-builtins--install-function-p 'delete-backward-char)"
               nil t))
      (let* ((form-start (match-beginning 0))
             (form-end (save-excursion
                         (goto-char form-start)
                         (forward-sexp)
                         (point)))
             (source (buffer-substring form-start form-end)))
        (should (string-match-p
                 "delete-backward-char (&optional n killflag)"
                 source))
        (should (string-match-p
                 "(interactive \"p\")" source))))))

(defun emacs-edit-builtins-test--read-defun (file marker)
  "Read FILE and return the source of the form starting at MARKER (a regexp)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward marker nil t)
      (let* ((form-start (match-beginning 0))
             (form-end (save-excursion
                         (goto-char form-start)
                         (forward-sexp)
                         (point))))
        (buffer-substring form-start form-end)))))

(ert-deftest emacs-edit-builtins-test/keymap-bound-cmd-shape-audit ()
  "Doc 51 Track X (2026-05-04) audit regression: every command bound
in `nemacs-main-keymap' must carry an `(interactive ...)' form so
`call-interactively' supplies a non-empty arg list, and must accept
0 required args so the lambda dispatch never raises
`wrong-number-of-arguments' under no-prefix-arg keymap routing.

This audit covers the polyfills shipped from `emacs-edit-builtins.el'
(self-insert-command / newline / kill-line; delete-backward-char is
checked by its dedicated shape test above)."
  (let* ((file (locate-library "emacs-edit-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (let ((s (emacs-edit-builtins-test--read-defun
              file "(when (emacs-edit-builtins--install-function-p 'self-insert-command)")))
      (should s)
      (should (string-match-p "self-insert-command (&optional n char)" s))
      (should (string-match-p "(interactive \"p\")" s)))
    (let ((s (emacs-edit-builtins-test--read-defun
              file "(when (emacs-edit-builtins--install-function-p 'newline)")))
      (should s)
      (should (string-match-p "newline (&optional n interactive)" s))
      (should (string-match-p "(interactive \"p\")" s)))
    (let ((s (emacs-edit-builtins-test--read-defun
              file "(when (emacs-edit-builtins--install-function-p 'kill-line)")))
      (should s)
      (should (string-match-p "kill-line (&optional arg)" s))
      (should (string-match-p "(interactive \"P\")" s)))))

;;;; E. kill-new pushes onto kill-ring

(ert-deftest emacs-edit-builtins-test/kill-new-pushes-and-trims ()
  (let ((kill-ring nil)
        (kill-ring-max 3))
    ;; Replicate the polyfill body literally.
    (let ((kill-new-impl
           (lambda (string)
             (when (and (stringp string) (> (length string) 0))
               (setq kill-ring (cons string kill-ring))
               (let ((c kill-ring) (i 1))
                 (while (and c (< i kill-ring-max))
                   (setq c (cdr c))
                   (setq i (+ i 1)))
                 (when c (setcdr c nil)))))))
      (funcall kill-new-impl "a")
      (funcall kill-new-impl "b")
      (funcall kill-new-impl "c")
      (should (equal '("c" "b" "a") kill-ring))
      (funcall kill-new-impl "d")
      (should (equal '("d" "c" "b") kill-ring)))))

;;;; F. copy-region-as-kill / kill-region

(ert-deftest emacs-edit-builtins-test/copy-region-as-kill-substrate ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello world"
    ;; Copy "world".
    (kill-new (nelisp-ec-buffer-substring 7 12))
    (should (equal "world" (car kill-ring)))
    ;; Buffer untouched.
    (should (equal "hello world" (nelisp-ec-buffer-string)))))

(ert-deftest emacs-edit-builtins-test/kill-region-substrate ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello world"
    (let ((s 7) (e 12))
      (kill-new (nelisp-ec-buffer-substring s e))
      (nelisp-ec-delete-region s e))
    (should (equal "world" (car kill-ring)))
    (should (equal "hello " (nelisp-ec-buffer-string)))))

;;;; G. kill-line at EOL kills the trailing \n

(ert-deftest emacs-edit-builtins-test/kill-line-at-eol-eats-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 3) ; at \n
    (let* ((start (nelisp-ec-point))
           (em (nelisp-ec-point-max))
           (eol (emacs-line--eol-pos))
           (kill-ring-local nil))
      ;; mirror kill-line polyfill
      (cond
       ((= start eol)
        (when (< eol em)
          (let ((seg (nelisp-ec-buffer-substring start (+ eol 1))))
            (push seg kill-ring-local)
            (nelisp-ec-delete-region start (+ eol 1)))))
       (t
        (let ((seg (nelisp-ec-buffer-substring start eol)))
          (push seg kill-ring-local)
          (nelisp-ec-delete-region start eol))))
      (should (equal '("\n") kill-ring-local))
      (should (equal "abcd" (nelisp-ec-buffer-string))))))

;;;; H. yank inserts head of kill-ring

(ert-deftest emacs-edit-builtins-test/yank-inserts-head ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((kill-ring '("XYZ" "older")))
      (nelisp-ec-goto-char (nelisp-ec-point-max))
      (nelisp-ec-insert (car kill-ring))
      (should (equal "ABXYZ" (nelisp-ec-buffer-string))))))

;; Phase 2.AI — overwrite-mode toggles self-insert from insert→replace.
(ert-deftest emacs-edit-builtins-test/overwrite-mode-replaces ()
  (emacs-edit-builtins-test--with-fresh-buffer "ABCDE"
    (let ((overwrite-mode t)
          (last-command-event ?X))
      (nelisp-ec-goto-char 2)            ; before B
      (emacs-edit--self-insert-command 1 nil)
      ;; B is replaced by X, point lands after X
      (should (equal "AXCDE" (nelisp-ec-buffer-string)))
      (should (= 3 (nelisp-ec-point))))))

;; Phase 2.AI — overwrite-mode does NOT eat the line terminator.
(ert-deftest emacs-edit-builtins-test/overwrite-mode-skips-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB\nCD"
    (let ((overwrite-mode t)
          (last-command-event ?X))
      (nelisp-ec-goto-char 3)            ; on the \n
      (emacs-edit--self-insert-command 1 nil)
      ;; \n preserved; X inserted before it
      (should (equal "ABX\nCD" (nelisp-ec-buffer-string))))))

;; Phase 2.AA — yank-pop replaces last yank with older entry.
;; Only assertable against our polyfill (host's C yank/yank-pop drive the
;; host buffer, not the substrate), so this calls the pure-Elisp bodies
;; directly.
(ert-deftest emacs-edit-builtins-test/yank-pop-cycles-kill-ring ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((kill-ring '("alpha" "beta" "gamma"))
          (kill-ring-yank-pointer nil)
          (emacs-edit--last-yank-bounds nil)
          (interprogram-paste-function nil))
      (emacs-edit--yank nil)
      (should (equal "alpha" (nelisp-ec-buffer-string)))
      (emacs-edit--yank-pop 1)
      (should (equal "beta" (nelisp-ec-buffer-string)))
      (emacs-edit--yank-pop 1)
      (should (equal "gamma" (nelisp-ec-buffer-string)))
      ;; wraps around modulo length
      (emacs-edit--yank-pop 1)
      (should (equal "alpha" (nelisp-ec-buffer-string))))))

;; Phase 2.AA — yank-pop signals when last command wasn't a yank.
(ert-deftest emacs-edit-builtins-test/yank-pop-rejects-without-yank ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((kill-ring '("X"))
          (emacs-edit--last-yank-bounds nil))
      (should-error (emacs-edit--yank-pop 1) :type 'error))))

;;;; I. forward-word polyfill body (= ASCII alnum boundary on substrate)
;; Under host Emacs the public `forward-word' uses host's syntax-table-
;; based C builtin and walks the host buffer rather than nelisp-ec
;; substrate, so we exercise the polyfill body via a lambda.

(defvar emacs-edit-builtins-test--forward-word
  (lambda (arg)
    (let* ((count (or arg 1))
           (sign (if (>= count 0) 1 -1))
           (n (abs count)))
      (cond
       ((= sign 1)
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (em (nelisp-ec-point-max)))
            (while (and (< p em)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at p))))
              (setq p (+ p 1)))
            (while (and (< p em)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at p)))
              (setq p (+ p 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1)))))
       (t
        (while (> n 0)
          (let ((p (nelisp-ec-point))
                (bm (nelisp-ec-point-min)))
            (while (and (> p bm)
                        (not (emacs-edit--word-char-p
                              (emacs-edit--char-at (- p 1)))))
              (setq p (- p 1)))
            (while (and (> p bm)
                        (emacs-edit--word-char-p
                         (emacs-edit--char-at (- p 1))))
              (setq p (- p 1)))
            (nelisp-ec-goto-char p)
            (setq n (- n 1)))))))))

(ert-deftest emacs-edit-builtins-test/forward-word-polyfill-body-ascii ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello, world! foo"
    (nelisp-ec-goto-char 1)
    (funcall emacs-edit-builtins-test--forward-word 1)
    (should (= 6 (nelisp-ec-point))) ; just after "hello"
    (funcall emacs-edit-builtins-test--forward-word 1)
    (should (= 13 (nelisp-ec-point))) ; just after "world"
    (funcall emacs-edit-builtins-test--forward-word 1)
    (should (= 18 (nelisp-ec-point))))) ; just after "foo" (= EOB)

;;;; J. backward-word polyfill body

(ert-deftest emacs-edit-builtins-test/backward-word-polyfill-body-ascii ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello, world! foo"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (funcall emacs-edit-builtins-test--forward-word -1)
    (should (= 15 (nelisp-ec-point))) ; start of "foo"
    (funcall emacs-edit-builtins-test--forward-word -1)
    (should (= 8 (nelisp-ec-point))) ; start of "world"
    (funcall emacs-edit-builtins-test--forward-word -1)
    (should (= 1 (nelisp-ec-point))))) ; start of "hello"

;;;; K. emacs-edit--word-char-p

(ert-deftest emacs-edit-builtins-test/word-char-p ()
  (should (emacs-edit--word-char-p ?a))
  (should (emacs-edit--word-char-p ?Z))
  (should (emacs-edit--word-char-p ?5))
  (should (emacs-edit--word-char-p ?_))
  (should-not (emacs-edit--word-char-p ?\s))
  (should-not (emacs-edit--word-char-p ?,))
  (should-not (emacs-edit--word-char-p ?\n))
  (should-not (emacs-edit--word-char-p nil)))

;;;; K2. interprogram-cut-function: kill-new mirrors to external clipboard
;;
;; Use module-level dynamic vars (= dynamic, not lexical) so the
;; lambdas we install on `interprogram-cut-function' / `-paste-function'
;; can mutate them via plain `setq' from inside `kill-new' / `yank'.
;; A let-bound lexical binds outside the lambda's closure capture under
;; host Emacs's compiled C path, so we can't use that here.

(defvar emacs-edit-builtins-test--cut-captured nil)
(defvar emacs-edit-builtins-test--cut-call-count 0)

(defun emacs-edit-builtins-test--cut-fn (s)
  (setq emacs-edit-builtins-test--cut-captured
        (cons s emacs-edit-builtins-test--cut-captured))
  (setq emacs-edit-builtins-test--cut-call-count
        (1+ emacs-edit-builtins-test--cut-call-count)))

(ert-deftest emacs-edit-builtins-test/kill-new-fires-interprogram-cut-function ()
  "When `interprogram-cut-function' is set, `kill-new' must call it
with the killed string so GUI display backends can mirror onto the
system clipboard."
  (setq emacs-edit-builtins-test--cut-captured nil)
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function #'emacs-edit-builtins-test--cut-fn))
    (emacs-edit--kill-new "hello")
    (emacs-edit--kill-new "world")
    (should (equal '("world" "hello") emacs-edit-builtins-test--cut-captured))
    (should (equal '("world" "hello") kill-ring))))

(ert-deftest emacs-edit-builtins-test/kill-new-skips-cut-fn-on-empty ()
  "Polyfill-only: empty strings should not trigger the cut hook
(= avoid clobbering the system clipboard with stray empty kills).
This verifies the pure-Elisp body directly because host Emacs's C
`kill-new' has different empty-string clipboard semantics."
  (setq emacs-edit-builtins-test--cut-call-count 0)
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function #'emacs-edit-builtins-test--cut-fn))
    (emacs-edit--kill-new "")
    (should (= 0 emacs-edit-builtins-test--cut-call-count))
    (emacs-edit--kill-new "x")
    (should (= 1 emacs-edit-builtins-test--cut-call-count))))

;;;; K3. interprogram-paste-function: yank pulls clipboard

(defvar emacs-edit-builtins-test--paste-return nil)
(defvar emacs-edit-builtins-test--paste-call-count 0)

(defun emacs-edit-builtins-test--paste-fn ()
  (setq emacs-edit-builtins-test--paste-call-count
        (1+ emacs-edit-builtins-test--paste-call-count))
  emacs-edit-builtins-test--paste-return)

(ert-deftest emacs-edit-builtins-test/yank-prepends-interprogram-paste ()
  "When `interprogram-paste-function' returns a string different from
the head of `kill-ring', `yank' must push it onto kill-ring before
inserting (= GUI clipboard wins, matching Emacs `current-kill').

Only assertable against our polyfill (= host Emacs's C `yank' has
its own clipboard plumbing), so this calls the pure-Elisp body
directly."
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (setq emacs-edit-builtins-test--paste-return "from-clipboard")
    (let ((kill-ring '("local-head"))
          (kill-ring-yank-pointer '("local-head"))
          (interprogram-paste-function #'emacs-edit-builtins-test--paste-fn))
      (emacs-edit--yank nil)
      (should (equal "from-clipboard" (car kill-ring)))
      (should (equal "from-clipboard" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-edit-builtins-test/yank-skips-duplicate-clipboard ()
  "When `interprogram-paste-function' returns a string equal to the
head of `kill-ring', the duplicate must be dropped (= avoid pushing
our own just-cut text twice)."
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (setq emacs-edit-builtins-test--paste-return "same")
    (let ((kill-ring '("same"))
          (kill-ring-yank-pointer '("same"))
          (interprogram-paste-function #'emacs-edit-builtins-test--paste-fn))
      (emacs-edit--yank nil)
      (should (= 1 (length kill-ring)))
      (should (equal "same" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-edit-builtins-test/yank-ignores-paste-fn-with-arg ()
  "Non-default `yank' ARG (= older entry) must NOT consult the
clipboard — `arg' explicitly chose a kill-ring entry."
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (setq emacs-edit-builtins-test--paste-call-count 0)
    (setq emacs-edit-builtins-test--paste-return "should-not-appear")
    (let ((kill-ring '("head" "older"))
          (kill-ring-yank-pointer '("head" "older"))
          (interprogram-paste-function #'emacs-edit-builtins-test--paste-fn))
      (emacs-edit--yank 2)
      (should (= 0 emacs-edit-builtins-test--paste-call-count))
      (should (equal "older" (nelisp-ec-buffer-string))))))

;;;; L. Idempotence

(ert-deftest emacs-edit-builtins-test/require-is-idempotent ()
  (let ((before-self  (symbol-function 'self-insert-command))
        (before-yank  (symbol-function 'yank))
        (before-kill  (symbol-function 'kill-new)))
    (require 'emacs-edit-builtins)
    (should (eq before-self (symbol-function 'self-insert-command)))
    (should (eq before-yank (symbol-function 'yank)))
    (should (eq before-kill (symbol-function 'kill-new)))))

(provide 'emacs-edit-builtins-test)

;;; emacs-edit-builtins-test.el ends here
