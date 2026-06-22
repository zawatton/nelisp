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
                 kill-word backward-kill-word kill-sentence
                 backward-kill-sentence kill-sexp yank forward-word
                 backward-word forward-paragraph backward-paragraph
                 forward-sentence backward-sentence
                 forward-sexp backward-sexp
                 delete-indentation zap-to-char sort-lines
                 tab-to-tab-stop
                 delete-blank-lines delete-trailing-whitespace
                 fill-paragraph))
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

(ert-deftest emacs-edit-builtins-test/delete-backward-direct-reports-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-delete-backward-direct 2)))
      (should (equal "hel" (nelisp-ec-buffer-string)))
      (should (equal 4 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal "lo" (plist-get edit :text)))
      (should (equal 2 (plist-get edit :delete-len)))
      (should (equal "lo" (plist-get edit :delete-text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/delete-backward-direct-reports-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-delete-backward-direct 1)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal "\n" (plist-get edit :text)))
      (should (equal 1 (plist-get edit :delete-len)))
      (should (equal "\n" (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline)))))

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

(ert-deftest emacs-edit-builtins-test/copy-region-direct-reports-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello\nworld"
    (let ((edit (emacs-edit-copy-region-direct 1 6)))
      (should (equal "hello\nworld" (nelisp-ec-buffer-string)))
      (should (equal "hello" (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal "hello" (plist-get edit :text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/kill-region-direct-reports-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello\nworld"
    (let ((edit (emacs-edit-kill-region-direct 6 7)))
      (should (equal "helloworld" (nelisp-ec-buffer-string)))
      (should (equal "\n" (car kill-ring)))
      (should (equal 6 (plist-get edit :beg)))
      (should (equal 7 (plist-get edit :end)))
      (should (equal "\n" (plist-get edit :text)))
      (should (equal 1 (plist-get edit :delete-len)))
      (should (equal "\n" (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/delete-region-direct-does-not-touch-kill-ring ()
  (emacs-edit-builtins-test--with-fresh-buffer "hello\nworld"
    (let ((kill-ring '("kept")))
      (let ((edit (emacs-edit-delete-region-direct 6 7 'deleted)))
        (should (equal "helloworld" (nelisp-ec-buffer-string)))
        (should (equal '("kept") kill-ring))
        (should (equal 6 (plist-get edit :beg)))
        (should (equal 6 (plist-get edit :end)))
        (should (equal "\n" (plist-get edit :text)))
        (should (equal 1 (plist-get edit :delete-len)))
        (should (equal "\n" (plist-get edit :delete-text)))
        (should (plist-get edit :deleted-newline))
        (should (eq 'deleted (plist-get edit :status)))))))

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

(ert-deftest emacs-edit-builtins-test/kill-line-direct-reports-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 2)
    (let ((edit (emacs-edit-kill-line-direct)))
      (should (equal "a\ncd" (nelisp-ec-buffer-string)))
      (should (equal "b" (car kill-ring)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal "b" (plist-get edit :text)))
      (should (equal 1 (plist-get edit :delete-len)))
      (should (equal "b" (plist-get edit :delete-text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/kill-line-direct-reports-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-kill-line-direct)))
      (should (equal "abcd" (nelisp-ec-buffer-string)))
      (should (equal "\n" (car kill-ring)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "\n" (plist-get edit :text)))
      (should (equal 1 (plist-get edit :delete-len)))
      (should (equal "\n" (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/transform-region-direct-replaces ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (let ((edit (emacs-edit-transform-region-direct 1 3 #'upcase)))
      (should (equal "AB cd" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal "AB" (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 2 (plist-get edit :delete-len)))
      (should (equal "ab" (plist-get edit :delete-text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/run-transform-region-command-no-mark ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (let ((result
           (emacs-edit-run-transform-region-command
            nil "edit" "edit" #'upcase "upcase-region")))
      (should (eq 'no-mark (plist-get result :status)))
      (should (equal "no mark set" (plist-get result :message)))
      (should-not (plist-get result :edit)))))

(ert-deftest emacs-edit-builtins-test/run-transform-region-command-transforms ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (nelisp-ec-goto-char 3)
    (let ((result
           (emacs-edit-run-transform-region-command
            1 "edit" "edit" #'upcase "upcase-region")))
      (should (equal "AB cd" (nelisp-ec-buffer-string)))
      (should (eq 'transformed (plist-get result :status)))
      (should (equal "upcase-region: 2 chars"
                     (plist-get result :message)))
      (should (equal "AB" (plist-get (plist-get result :edit) :text))))))

(ert-deftest emacs-edit-builtins-test/toggle-line-comment-direct-comments ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((edit (emacs-edit-toggle-line-comment-direct 1 4)))
      (should (equal ";; abc" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal ";; " (plist-get edit :text)))
      (should-not (plist-get edit :delete-len))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/toggle-line-comment-direct-uncomments ()
  (emacs-edit-builtins-test--with-fresh-buffer ";; abc"
    (let ((edit (emacs-edit-toggle-line-comment-direct 1 7)))
      (should (equal "abc" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal ";; " (plist-get edit :text)))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (equal ";; " (plist-get edit :delete-text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/toggle-line-comment-direct-uncomments-indented ()
  (emacs-edit-builtins-test--with-fresh-buffer "  ;; abc"
    (let ((edit (emacs-edit-toggle-line-comment-direct 1 9)))
      (should (equal "  abc" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal ";; " (plist-get edit :text)))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (equal ";; " (plist-get edit :delete-text)))
      (should-not (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/line-bols-in-range-excludes-end-bol ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb\nc"
    (should (equal '(1 3) (emacs-edit-line-bols-in-range 1 5)))))

(ert-deftest emacs-edit-builtins-test/comment-dwim-direct-comments-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((edit (emacs-edit-comment-dwim-direct)))
      (should (equal ";; abc" (nelisp-ec-buffer-string)))
      (should (eq 'line (plist-get edit :status)))
      (should (equal 1 (plist-get edit :line-count)))
      (should (equal 1 (length (plist-get edit :edits)))))))

(ert-deftest emacs-edit-builtins-test/comment-dwim-direct-comments-region ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb\nc"
    (let ((edit (emacs-edit-comment-dwim-direct '(1 . 5))))
      (should (equal ";; a\n;; b\nc" (nelisp-ec-buffer-string)))
      (should (eq 'region (plist-get edit :status)))
      (should (equal 2 (plist-get edit :line-count)))
      (should (equal 2 (length (plist-get edit :edits)))))))

(ert-deftest emacs-edit-builtins-test/transpose-chars-direct-middle ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-transpose-chars-direct)))
      (should (equal "acb" (nelisp-ec-buffer-string)))
      (should (equal 4 (nelisp-ec-point)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "cb" (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 2 (plist-get edit :delete-len)))
      (should (equal "bc" (plist-get edit :delete-text))))))

(ert-deftest emacs-edit-builtins-test/transpose-chars-direct-at-eob ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-transpose-chars-direct)))
      (should (equal "acb" (nelisp-ec-buffer-string)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal "bc" (plist-get edit :delete-text))))))

(ert-deftest emacs-edit-builtins-test/transpose-chars-direct-before-second-char-is-noop ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 2)
    (let ((edit (emacs-edit-transpose-chars-direct)))
      (should (equal "abc" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should-not (plist-get edit :text)))))

(ert-deftest emacs-edit-builtins-test/horizontal-whitespace-bounds-around ()
  (emacs-edit-builtins-test--with-fresh-buffer "a \t  b"
    (should (equal '(2 . 6)
                   (emacs-edit-horizontal-whitespace-bounds-around 4)))
    (should-not (emacs-edit-horizontal-whitespace-bounds-around 1))))

(ert-deftest emacs-edit-builtins-test/just-one-space-direct-collapses-run ()
  (emacs-edit-builtins-test--with-fresh-buffer "a \t  b"
    (nelisp-ec-goto-char 4)
    (let ((edit (emacs-edit-just-one-space-direct)))
      (should (equal "a b" (nelisp-ec-buffer-string)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal " " (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 4 (plist-get edit :delete-len)))
      (should (equal " \t  " (plist-get edit :delete-text))))))

(ert-deftest emacs-edit-builtins-test/delete-horizontal-space-direct-deletes-run ()
  (emacs-edit-builtins-test--with-fresh-buffer "a \t  b"
    (nelisp-ec-goto-char 4)
    (let ((edit (emacs-edit-delete-horizontal-space-direct)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should (equal 2 (plist-get edit :beg)))
      (should (equal 2 (plist-get edit :end)))
      (should (equal " \t  " (plist-get edit :text)))
      (should-not (plist-get edit :replacement))
      (should (equal 4 (plist-get edit :delete-len)))
      (should (equal " \t  " (plist-get edit :delete-text))))))

(ert-deftest emacs-edit-builtins-test/delete-horizontal-space-direct-noop ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char 2)
    (let ((edit (emacs-edit-delete-horizontal-space-direct)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should-not (plist-get edit :text)))))

(ert-deftest emacs-edit-builtins-test/delete-indentation-direct-joins-with-space ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\n  cd"
    (nelisp-ec-goto-char 6)
    (let ((edit (emacs-edit-delete-indentation-direct)))
      (should (equal "ab cd" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal " " (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (equal "\n  " (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline))
      (should (eq 'joined (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/delete-indentation-direct-skips-extra-space ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab \n  cd"
    (nelisp-ec-goto-char 7)
    (let ((edit (emacs-edit-delete-indentation-direct)))
      (should (equal "ab cd" (nelisp-ec-buffer-string)))
      (should (equal 4 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "" (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (equal "\n  " (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/delete-indentation-direct-at-bob ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (let ((edit (emacs-edit-delete-indentation-direct)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'bob (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/scan-forward-to-char-finds-without-moving ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc def"
    (should (equal 6 (emacs-edit-scan-forward-to-char ?d)))
    (should (equal 1 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-forward-to-char-not-found ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (should-not (emacs-edit-scan-forward-to-char ?z))
    (should (equal 1 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/zap-to-char-direct-kills-through-char ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc def"
    (let ((edit (emacs-edit-zap-to-char-direct ?c)))
      (should (equal " def" (nelisp-ec-buffer-string)))
      (should (equal "abc" (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "abc" (plist-get edit :text)))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (eq ?c (plist-get edit :target)))
      (should (eq 'killed (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/zap-to-char-direct-not-found ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((edit (emacs-edit-zap-to-char-direct ?z)))
      (should (equal "abc" (nelisp-ec-buffer-string)))
      (should-not kill-ring)
      (should-not (plist-get edit :beg))
      (should (eq ?z (plist-get edit :target)))
      (should (eq 'not-found (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/sort-lines-direct-sorts-region ()
  (emacs-edit-builtins-test--with-fresh-buffer "b\na\nc\n"
    (let ((edit (emacs-edit-sort-lines-direct 1 (nelisp-ec-point-max))))
      (should (equal "a\nb\nc\n" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal (nelisp-ec-point-max) (plist-get edit :end)))
      (should (equal "a\nb\nc\n" (plist-get edit :text)))
      (should (plist-get edit :replacement))
      (should (equal 6 (plist-get edit :delete-len)))
      (should (equal "b\na\nc\n" (plist-get edit :delete-text)))
      (should (plist-get edit :deleted-newline))
      (should (equal 3 (plist-get edit :line-count)))
      (should (eq 'sorted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/sort-lines-direct-preserves-no-trailing-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "b\na"
    (let ((edit (emacs-edit-sort-lines-direct 1 (nelisp-ec-point-max))))
      (should (equal "a\nb" (nelisp-ec-buffer-string)))
      (should (equal "a\nb" (plist-get edit :text)))
      (should (equal 2 (plist-get edit :line-count)))
      (should (plist-get edit :deleted-newline)))))

(ert-deftest emacs-edit-builtins-test/sort-lines-direct-reverse ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nc\nb"
    (let ((edit (emacs-edit-sort-lines-direct 1 (nelisp-ec-point-max) t)))
      (should (equal "c\nb\na" (nelisp-ec-buffer-string)))
      (should (equal "c\nb\na" (plist-get edit :text)))
      (should (equal 3 (plist-get edit :line-count))))))

(ert-deftest emacs-edit-builtins-test/blank-line-at-p-detects-whitespace-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n  \n\t\nb"
    (should-not (emacs-edit-blank-line-at-p 1))
    (should (emacs-edit-blank-line-at-p 3))
    (should (emacs-edit-blank-line-at-p 6))
    (should-not (emacs-edit-blank-line-at-p 8))))

(ert-deftest emacs-edit-builtins-test/delete-blank-lines-direct-collapses-run-on-blank ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\n\nb"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-delete-blank-lines-direct)))
      (should (equal "a\n\nb" (nelisp-ec-buffer-string)))
      (should (equal 3 (nelisp-ec-point)))
      (should (equal 4 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "\n" (plist-get edit :text)))
      (should (equal 1 (plist-get edit :delete-len)))
      (should (plist-get edit :deleted-newline))
      (should (eq 'deleted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/delete-blank-lines-direct-keeps-single-blank-on-blank ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-delete-blank-lines-direct)))
      (should (equal "a\n\nb" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'nothing-to-remove (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/delete-blank-lines-direct-deletes-following-run ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\n\nb"
    (let ((edit (emacs-edit-delete-blank-lines-direct)))
      (should (equal "a\nb" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal "\n\n" (plist-get edit :text)))
      (should (equal 2 (plist-get edit :delete-len)))
      (should (plist-get edit :deleted-newline))
      (should (eq 'deleted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/delete-blank-lines-direct-none-after-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb"
    (let ((edit (emacs-edit-delete-blank-lines-direct)))
      (should (equal "a\nb" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'none-to-delete (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/trailing-whitespace-ranges-descending ()
  (emacs-edit-builtins-test--with-fresh-buffer "a  \nb\t \n c  "
    (should (equal '((11 . 13) (6 . 8) (2 . 4))
                   (emacs-edit-trailing-whitespace-ranges)))))

(ert-deftest emacs-edit-builtins-test/delete-trailing-whitespace-direct-deletes-all-ranges ()
  (emacs-edit-builtins-test--with-fresh-buffer "a  \nb\t \n c  "
    (let ((edit (emacs-edit-delete-trailing-whitespace-direct)))
      (should (equal "a\nb\n c" (nelisp-ec-buffer-string)))
      (should (equal '((11 . 13) (6 . 8) (2 . 4))
                     (plist-get edit :delete-ranges)))
      (should (equal 6 (plist-get edit :char-count)))
      (should (equal 3 (plist-get edit :line-count)))
      (should (equal 3 (length (plist-get edit :edits))))
      (should-not (plist-get edit :deleted-newline))
      (should (eq 'deleted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/delete-trailing-whitespace-direct-none ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n b"
    (let ((edit (emacs-edit-delete-trailing-whitespace-direct)))
      (should (equal "a\n b" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :delete-ranges))
      (should (equal 0 (plist-get edit :char-count)))
      (should (equal 0 (plist-get edit :line-count)))
      (should (eq 'none (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/fill-paragraph-bounds-current-paragraph ()
  (emacs-edit-builtins-test--with-fresh-buffer "before\n\none two\nthree\n\nnext"
    (nelisp-ec-goto-char 10)
    (should (equal '(9 . 23) (emacs-edit-fill-paragraph-bounds)))))

(ert-deftest emacs-edit-builtins-test/fill-paragraph-direct-wraps-greedily ()
  (emacs-edit-builtins-test--with-fresh-buffer "one two three\nfour five"
    (let ((edit (emacs-edit-fill-paragraph-direct 10)))
      (should (equal "one two\nthree four\nfive" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal (nelisp-ec-point-max) (plist-get edit :end)))
      (should (plist-get edit :replacement))
      (should (equal 23 (plist-get edit :old-length)))
      (should (equal 23 (plist-get edit :new-length)))
      (should (eq 'filled (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/fill-paragraph-direct-stops-at-empty-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "before\n\none two three four\n\nnext"
    (nelisp-ec-goto-char 10)
    (let ((edit (emacs-edit-fill-paragraph-direct 10)))
      (should (equal "before\n\none two\nthree four\nnext"
                     (nelisp-ec-buffer-string)))
      (should (equal 9 (plist-get edit :beg)))
      (should (equal 27 (plist-get edit :end)))
      (should (eq 'filled (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/fill-paragraph-direct-empty-line-fills-next-paragraph ()
  (emacs-edit-builtins-test--with-fresh-buffer "before\n\none two three"
    (nelisp-ec-goto-char 8)
    (let ((edit (emacs-edit-fill-paragraph-direct 7)))
      (should (equal "before\n\none two\nthree" (nelisp-ec-buffer-string)))
      (should (equal 9 (plist-get edit :beg)))
      (should (eq 'filled (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/fill-paragraph-direct-empty-at-eob ()
  (emacs-edit-builtins-test--with-fresh-buffer "before\n\n"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-fill-paragraph-direct 10)))
      (should (equal "before\n\n" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'empty (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/run-fill-paragraph-command-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "one two three\nfour five"
    (let ((result (emacs-edit-run-fill-paragraph-command 10)))
      (should (eq 'filled (plist-get result :status)))
      (should (equal "fill-paragraph: 23→23 chars"
                     (plist-get result :message)))
      (should (eq 'filled
                  (plist-get (plist-get result :edit) :status))))))

(ert-deftest emacs-edit-builtins-test/run-fill-paragraph-command-empty-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "before\n\n"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((result (emacs-edit-run-fill-paragraph-command 10)))
      (should (eq 'empty (plist-get result :status)))
      (should (equal "fill-paragraph: empty"
                     (plist-get result :message)))
      (should (eq 'empty
                  (plist-get (plist-get result :edit) :status))))))

(ert-deftest emacs-edit-builtins-test/forward-paragraph-position-skips-empty-lines ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 3)
    (should (equal 8 (emacs-edit-forward-paragraph-position)))))

(ert-deftest emacs-edit-builtins-test/backward-paragraph-position-lands-before-current-paragraph ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 9)
    (should (equal 3 (emacs-edit-backward-paragraph-position)))))

(ert-deftest emacs-edit-builtins-test/forward-paragraph-direct-moves-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 4)
    (let ((move (emacs-edit-forward-paragraph-direct)))
      (should (equal 8 (nelisp-ec-point)))
      (should (equal 4 (plist-get move :old-point)))
      (should (equal 8 (plist-get move :point)))
      (should (eq 'moved (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/backward-paragraph-direct-moves-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 9)
    (let ((move (emacs-edit-backward-paragraph-direct)))
      (should (equal 3 (nelisp-ec-point)))
      (should (equal 9 (plist-get move :old-point)))
      (should (equal 3 (plist-get move :point)))
      (should (eq 'moved (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/mark-paragraph-direct-selects-current-paragraph ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 5)
    (let ((mark (emacs-edit-mark-paragraph-direct)))
      (should (equal 8 (nelisp-ec-point)))
      (should (equal 4 (plist-get mark :beg)))
      (should (equal 8 (plist-get mark :end)))
      (should (equal 8 (plist-get mark :point)))
      (should (eq 'marked (plist-get mark :status))))))

(ert-deftest emacs-edit-builtins-test/mark-paragraph-direct-on-empty-line-selects-previous ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 8)
    (let ((mark (emacs-edit-mark-paragraph-direct)))
      (should (equal 8 (nelisp-ec-point)))
      (should (equal 4 (plist-get mark :beg)))
      (should (equal 8 (plist-get mark :end))))))

(ert-deftest emacs-edit-builtins-test/run-mark-paragraph-command-state ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n\nb\nc\n\nz"
    (nelisp-ec-goto-char 5)
    (let ((result (emacs-edit-run-mark-paragraph-command "buf")))
      (should (eq 'marked (plist-get result :status)))
      (should (equal 4 (plist-get result :mark)))
      (should (equal 8 (plist-get result :point)))
      (should (equal "buf" (plist-get result :buffer)))
      (should-not (plist-get result :shift-region))
      (should (equal "Mark paragraph" (plist-get result :message))))))

(ert-deftest emacs-edit-builtins-test/goto-buffer-boundary-direct-moves ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 2)
    (let ((beg (emacs-edit-goto-buffer-boundary-direct 'beginning)))
      (should (eq 'moved (plist-get beg :status)))
      (should (equal 'beginning (plist-get beg :boundary)))
      (should (equal 1 (plist-get beg :point)))
      (should (equal 1 (nelisp-ec-point))))
    (let ((end (emacs-edit-goto-buffer-boundary-direct 'end)))
      (should (eq 'moved (plist-get end :status)))
      (should (equal 'end (plist-get end :boundary)))
      (should (equal (nelisp-ec-point-max) (plist-get end :point)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/mark-whole-buffer-direct-state ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((result (emacs-edit-mark-whole-buffer-direct "buf")))
      (should (eq 'marked (plist-get result :status)))
      (should (equal 1 (plist-get result :mark)))
      (should (equal (nelisp-ec-point-max) (plist-get result :point)))
      (should (equal "buf" (plist-get result :buffer)))
      (should-not (plist-get result :shift-region))
      (should (equal "Selected whole buffer (3 chars)"
                     (plist-get result :message))))))

(ert-deftest emacs-edit-builtins-test/exchange-point-and-mark-direct-state ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 3)
    (let ((result
           (emacs-edit-exchange-point-and-mark-direct 1 "buf" "buf")))
      (should (eq 'exchanged (plist-get result :status)))
      (should (equal 1 (nelisp-ec-point)))
      (should (equal 3 (plist-get result :mark)))
      (should (equal "buf" (plist-get result :buffer)))
      (should-not (plist-get result :shift-region))
      (should (equal "Exchange point and mark"
                     (plist-get result :message)))))
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((result
           (emacs-edit-exchange-point-and-mark-direct nil nil "buf")))
      (should (eq 'no-mark (plist-get result :status)))
      (should (equal "exchange-point-and-mark: no mark"
                     (plist-get result :message))))))

(ert-deftest emacs-edit-builtins-test/beginning-of-defun-position-finds-top-level ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a\n b)\n\nx"
    (nelisp-ec-goto-char 5)
    (should (equal 1 (emacs-edit-beginning-of-defun-position)))))

(ert-deftest emacs-edit-builtins-test/beginning-of-defun-position-none ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\n b"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should-not (emacs-edit-beginning-of-defun-position))))

(ert-deftest emacs-edit-builtins-test/defun-bounds-restores-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a\n b)\n\nx"
    (nelisp-ec-goto-char 5)
    (let ((bounds (emacs-edit-defun-bounds)))
      (should (equal 5 (nelisp-ec-point)))
      (should (equal 1 (plist-get bounds :beg)))
      (should (equal 7 (plist-get bounds :end)))
      (should (eq 'ok (plist-get bounds :status))))))

(ert-deftest emacs-edit-builtins-test/defun-bounds-scan-error ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((bounds (emacs-edit-defun-bounds)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (equal 1 (plist-get bounds :beg)))
      (should-not (plist-get bounds :end))
      (should (eq 'scan-error (plist-get bounds :status))))))

(ert-deftest emacs-edit-builtins-test/mark-defun-direct-marks-bounds ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a\n b)\n\nx"
    (nelisp-ec-goto-char 5)
    (let ((mark (emacs-edit-mark-defun-direct)))
      (should (equal 1 (nelisp-ec-point)))
      (should (equal 1 (plist-get mark :beg)))
      (should (equal 7 (plist-get mark :end)))
      (should (equal 1 (plist-get mark :point)))
      (should (eq 'marked (plist-get mark :status))))))

(ert-deftest emacs-edit-builtins-test/narrow-to-defun-direct-narrows-bounds ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a\n b)\n\nx"
    (nelisp-ec-goto-char 5)
    (let ((narrow (emacs-edit-narrow-to-defun-direct)))
      (should (equal 1 (nelisp-ec-point-min)))
      (should (equal 7 (nelisp-ec-point-max)))
      (should (equal 7 (nelisp-ec-point)))
      (should (equal 1 (plist-get narrow :beg)))
      (should (equal 7 (plist-get narrow :end)))
      (should (eq 'narrowed (plist-get narrow :status))))))

(ert-deftest emacs-edit-builtins-test/set-mark-direct-reports-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 2)
    (let ((mark (emacs-edit-set-mark-direct "buf")))
      (should (eq 'marked (plist-get mark :status)))
      (should (equal 2 (plist-get mark :mark)))
      (should (equal 2 (plist-get mark :point)))
      (should (equal "buf" (plist-get mark :buffer)))
      (should-not (plist-get mark :shift-region))
      (should (equal "Mark set @ 2" (plist-get mark :message))))))

(ert-deftest emacs-edit-builtins-test/region-bounds-direct-orders-endpoints ()
  (emacs-edit-builtins-test--with-fresh-buffer "abcdef"
    (nelisp-ec-goto-char 5)
    (should (equal '(2 . 5)
                   (emacs-edit-region-bounds-direct 2 "buf" "buf")))
    (nelisp-ec-goto-char 1)
    (should (equal '(1 . 5)
                   (emacs-edit-region-bounds-direct 5 "buf" "buf")))
    (should-not (emacs-edit-region-bounds-direct 1 "other" "buf"))
    (should-not (emacs-edit-region-bounds-direct 1 "buf" "buf"))))

(ert-deftest emacs-edit-builtins-test/shift-selection-plan-actions ()
  (let ((motions '(left right)))
    (let ((plan (emacs-edit-shift-selection-plan
                 'right 1
                 :shift-mask 1
                 :motion-events motions
                 :point 7
                 :mark-pos nil
                 :mark-buffer nil
                 :active-buffer-name "buf"
                 :shift-region nil)))
      (should (eq 'activate (plist-get plan :action)))
      (should (equal 7 (plist-get plan :mark)))
      (should (equal "buf" (plist-get plan :buffer)))
      (should (plist-get plan :shift-region))
      (should (equal "Mark activated" (plist-get plan :message))))
    (should (eq 'deactivate
                (plist-get
                 (emacs-edit-shift-selection-plan
                  'right 0
                  :shift-mask 1
                  :motion-events motions
                  :point 7
                  :mark-pos 3
                  :mark-buffer "buf"
                  :active-buffer-name "buf"
                  :shift-region t)
                 :action)))
    (should (eq 'none
                (plist-get
                 (emacs-edit-shift-selection-plan
                  'a 1
                  :shift-mask 1
                  :motion-events motions
                  :point 7)
                 :action)))))

(ert-deftest emacs-edit-builtins-test/mouse-drag-region-plan-anchors-on-first-drag ()
  (let ((plan (emacs-edit-mouse-drag-region-plan
               3 9 nil nil "buf")))
    (should (eq 'anchored (plist-get plan :status)))
    (should (equal 3 (plist-get plan :mark)))
    (should (equal "buf" (plist-get plan :buffer)))
    (should-not (plist-get plan :shift-region))
    (should (equal 9 (plist-get plan :point)))
    (should (equal "drag → 3..9" (plist-get plan :message)))))

(ert-deftest emacs-edit-builtins-test/mouse-drag-region-plan-keeps-active-mark ()
  (let ((plan (emacs-edit-mouse-drag-region-plan
               3 9 5 "buf" "buf")))
    (should (eq 'extended (plist-get plan :status)))
    (should (equal 5 (plist-get plan :mark)))
    (should (equal "drag → 5..9" (plist-get plan :message)))))

(ert-deftest emacs-edit-builtins-test/page-scroll-direct-moves-by-viewport ()
  (emacs-edit-builtins-test--with-fresh-buffer
      "1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    (nelisp-ec-goto-char (nelisp-ec-point-min))
    (let ((down (emacs-edit-page-scroll-direct 'down 5)))
      (should (eq 'moved (plist-get down :status)))
      (should (equal 'down (plist-get down :direction)))
      (should (equal 3 (plist-get down :delta)))
      (should (> (nelisp-ec-point) (plist-get down :old-point))))
    (let ((up (emacs-edit-page-scroll-direct 'up 5)))
      (should (equal 'up (plist-get up :direction)))
      (should (equal -3 (plist-get up :delta))))))

(ert-deftest emacs-edit-builtins-test/count-lines-in-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb\nc"
    (should (equal 0 (emacs-edit-count-lines-in-range 1 1)))
    (should (equal 2 (emacs-edit-count-lines-in-range 1 5)))
    (should (equal 3 (emacs-edit-count-lines-in-range 1 (nelisp-ec-point-max))))))

(ert-deftest emacs-edit-builtins-test/count-words-in-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "one, two\n3_four"
    (should (equal 3 (emacs-edit-count-words-in-range
                      1 (nelisp-ec-point-max))))))

(ert-deftest emacs-edit-builtins-test/count-range-reports-lines-words-chars ()
  (emacs-edit-builtins-test--with-fresh-buffer "one two\nthree"
    (let ((counts (emacs-edit-count-range 1 (nelisp-ec-point-max))))
      (should (equal 1 (plist-get counts :beg)))
      (should (equal (nelisp-ec-point-max) (plist-get counts :end)))
      (should (equal 2 (plist-get counts :lines)))
      (should (equal 3 (plist-get counts :words)))
      (should (equal 13 (plist-get counts :chars)))
      (should (eq 'counted (plist-get counts :status))))))

(ert-deftest emacs-edit-builtins-test/dabbrev-word-at-point-prefix ()
  (emacs-edit-builtins-test--with-fresh-buffer "foo ba"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (equal '(5 . "ba")
                   (emacs-edit-dabbrev-word-at-point-prefix)))))

(ert-deftest emacs-edit-builtins-test/dabbrev-find-completion-skips-cycled ()
  (emacs-edit-builtins-test--with-fresh-buffer "bar baz bag ba"
    (let ((hit (emacs-edit-dabbrev-find-completion "ba" 13 '("ba"))))
      (should (equal '(9 12 "bag" 9) hit)))
    (let ((hit (emacs-edit-dabbrev-find-completion "ba" 13 '("ba" "bag"))))
      (should (equal '(5 8 "baz" 5) hit)))))

(ert-deftest emacs-edit-builtins-test/dabbrev-expand-direct-replaces-fragment ()
  (emacs-edit-builtins-test--with-fresh-buffer "bar ba"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-dabbrev-expand-direct 5 (nelisp-ec-point) "bar")))
      (should (equal "bar bar" (nelisp-ec-buffer-string)))
      (should-not kill-ring)
      (should (equal 5 (plist-get edit :beg)))
      (should (equal (nelisp-ec-point-max) (plist-get edit :end)))
      (should (equal "bar" (plist-get edit :text)))
      (should (equal 2 (plist-get edit :delete-len)))
      (should (equal "ba" (plist-get edit :delete-text)))
      (should (equal "bar" (plist-get edit :word)))
      (should (eq 'expanded (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/current-column-in-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncde"
    (nelisp-ec-goto-char 6)
    (should (equal 2 (emacs-edit-current-column-in-line)))))

(ert-deftest emacs-edit-builtins-test/tab-to-tab-stop-direct-inserts-to-next-stop ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-tab-to-tab-stop-direct 4)))
      (should (equal "ab  " (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 5 (plist-get edit :end)))
      (should (equal "  " (plist-get edit :text)))
      (should (equal 2 (plist-get edit :columns-added)))
      (should (equal 2 (plist-get edit :old-column)))
      (should (equal 4 (plist-get edit :new-column)))
      (should (eq 'inserted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/tab-to-tab-stop-direct-inserts-full-stop-at-stop ()
  (emacs-edit-builtins-test--with-fresh-buffer "abcd"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-tab-to-tab-stop-direct 4)))
      (should (equal "abcd    " (nelisp-ec-buffer-string)))
      (should (equal 4 (plist-get edit :columns-added)))
      (should (equal 4 (plist-get edit :old-column)))
      (should (equal 8 (plist-get edit :new-column))))))

(ert-deftest emacs-edit-builtins-test/electric-pair-direct-pairs-opener ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-electric-pair-direct ?\()))
      (should (equal "ab()" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 5 (plist-get edit :end)))
      (should (equal "()" (plist-get edit :text)))
      (should (equal 4 (plist-get edit :point)))
      (should (equal ?\( (plist-get edit :char)))
      (should (equal ?\) (plist-get edit :close)))
      (should (eq 'paired (plist-get edit :status)))
      (should (equal 4 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/electric-pair-direct-skips-matching-closer ()
  (emacs-edit-builtins-test--with-fresh-buffer "()"
    (nelisp-ec-goto-char 2)
    (let ((edit (emacs-edit-electric-pair-direct ?\))))
      (should (equal "()" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (equal 2 (plist-get edit :old-point)))
      (should (equal 3 (plist-get edit :point)))
      (should (equal ?\) (plist-get edit :char)))
      (should (eq 'skipped (plist-get edit :status)))
      (should (equal 3 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/electric-pair-direct-inserts-unmatched-closer ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-electric-pair-direct ?\))))
      (should (equal "ab)" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal ")" (plist-get edit :text)))
      (should (equal 4 (plist-get edit :point)))
      (should (equal ?\) (plist-get edit :char)))
      (should (eq 'inserted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/run-electric-pair-command-paired-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((result (emacs-edit-run-electric-pair-command ?\()))
      (should (eq 'paired (plist-get result :status)))
      (should (equal "electric-pair: ()" (plist-get result :message)))
      (should (equal "()" (plist-get (plist-get result :edit) :text))))))

(ert-deftest emacs-edit-builtins-test/run-electric-pair-command-skip-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "()"
    (nelisp-ec-goto-char 2)
    (let ((result (emacs-edit-run-electric-pair-command ?\))))
      (should (eq 'skipped (plist-get result :status)))
      (should (equal "electric-pair: skip )"
                     (plist-get result :message)))
      (should (equal 3
                     (plist-get (plist-get result :edit) :point))))))

(ert-deftest emacs-edit-builtins-test/run-electric-pair-command-insert-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((result (emacs-edit-run-electric-pair-command ?\))))
      (should (eq 'inserted (plist-get result :status)))
      (should (equal "electric-pair: ) (no match)"
                     (plist-get result :message)))
      (should (equal ")" (plist-get (plist-get result :edit) :text))))))

(ert-deftest emacs-edit-builtins-test/copy-to-register-direct-stores-region ()
  (emacs-edit-builtins-test--with-fresh-buffer "alpha beta"
    (let* ((edit (emacs-edit-copy-to-register-direct nil ?a 1 6))
           (registers (plist-get edit :registers)))
      (should (equal "alpha" (emacs-edit-register-value registers ?a)))
      (should (equal ?a (plist-get edit :char)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal "alpha" (plist-get edit :text)))
      (should (eq 'stored (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/insert-register-direct-inserts-string ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-insert-register-direct
                 (list (cons ?a "XYZ")) ?a)))
      (should (equal "abXYZ" (nelisp-ec-buffer-string)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal "XYZ" (plist-get edit :text)))
      (should (equal "XYZ" (plist-get edit :value)))
      (should (eq 'inserted (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/insert-register-direct-rejects-position ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (let ((edit (emacs-edit-insert-register-direct
                 (list (cons ?p '(:point "buf" 3))) ?p)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'position (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/point-to-register-direct-stores-target ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (nelisp-ec-goto-char 3)
    (let* ((edit (emacs-edit-point-to-register-direct nil ?p "buf"))
           (registers (plist-get edit :registers)))
      (should (equal '(:point "buf" 3)
                     (emacs-edit-register-value registers ?p)))
      (should (equal "buf" (plist-get edit :buffer)))
      (should (equal 3 (plist-get edit :point)))
      (should (eq 'stored (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/jump-to-register-target-reports-kind ()
  (let ((registers (list (cons ?p '(:point "buf" 7))
                         (cons ?s "text"))))
    (let ((target (emacs-edit-jump-to-register-target registers ?p)))
      (should (equal "buf" (plist-get target :buffer)))
      (should (equal 7 (plist-get target :point)))
      (should (eq 'point (plist-get target :status))))
    (should (eq 'string
                (plist-get (emacs-edit-jump-to-register-target registers ?s)
                           :status)))
    (should (eq 'empty
                (plist-get (emacs-edit-jump-to-register-target registers ?z)
                           :status)))))

(ert-deftest emacs-edit-builtins-test/goto-position-direct-clamps ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((edit (emacs-edit-goto-position-direct 99)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (equal 99 (plist-get edit :requested-point)))
      (should (equal (nelisp-ec-point-max) (plist-get edit :point)))
      (should (eq 'moved (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/goto-register-position-direct-wraps-goto-position ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (let ((edit (emacs-edit-goto-register-position-direct 99)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (equal 99 (plist-get edit :requested-point)))
      (should (equal (nelisp-ec-point-max) (plist-get edit :point)))
      (should (eq 'moved (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/kill-line-direct-at-eob-is-noop ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-kill-line-direct)))
      (should (equal "ab" (nelisp-ec-buffer-string)))
      (should (null kill-ring))
      (should-not (plist-get edit :beg))
      (should-not (plist-get edit :text)))))

(ert-deftest emacs-edit-builtins-test/kill-whole-line-direct-kills-with-newline ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 2)
    (let ((edit (emacs-edit-kill-whole-line-direct)))
      (should (equal "cd" (nelisp-ec-buffer-string)))
      (should (equal "ab\n" (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "ab\n" (plist-get edit :text)))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (plist-get edit :deleted-newline))
      (should (eq 'whole-line (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/kill-whole-line-direct-kills-last-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 4)
    (let ((edit (emacs-edit-kill-whole-line-direct)))
      (should (equal "ab\n" (nelisp-ec-buffer-string)))
      (should (equal "cd" (car kill-ring)))
      (should (equal 4 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should-not (plist-get edit :deleted-newline))
      (should (eq 'last-line (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/kill-whole-line-direct-empty-buffer-is-noop ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((edit (emacs-edit-kill-whole-line-direct)))
      (should (equal "" (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'empty-buffer (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/forward-word-position-forward ()
  (emacs-edit-builtins-test--with-fresh-buffer "  ab cd"
    (should (equal 5 (emacs-edit-forward-word-position 1 1)))
    (should (equal 8 (emacs-edit-forward-word-position 1 2)))))

(ert-deftest emacs-edit-builtins-test/forward-word-position-backward ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd  "
    (should (equal 4 (emacs-edit-forward-word-position 8 -1)))
    (should (equal 1 (emacs-edit-forward-word-position 8 -2)))))

(ert-deftest emacs-edit-builtins-test/kill-word-direct-kills-forward-word ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (let ((edit (emacs-edit-kill-word-direct 1)))
      (should (equal " cd" (nelisp-ec-buffer-string)))
      (should (equal "ab" (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 3 (plist-get edit :end)))
      (should (equal "ab" (plist-get edit :text)))
      (should (equal 2 (plist-get edit :delete-len))))))

(ert-deftest emacs-edit-builtins-test/kill-word-direct-skips-space-forward ()
  (emacs-edit-builtins-test--with-fresh-buffer "  ab cd"
    (let ((edit (emacs-edit-kill-word-direct 1)))
      (should (equal " cd" (nelisp-ec-buffer-string)))
      (should (equal "  ab" (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 5 (plist-get edit :end)))
      (should (equal 4 (plist-get edit :delete-len))))))

(ert-deftest emacs-edit-builtins-test/kill-word-direct-kills-backward-word ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-kill-word-direct -1)))
      (should (equal "ab " (nelisp-ec-buffer-string)))
      (should (equal "cd" (car kill-ring)))
      (should (equal 4 (plist-get edit :beg)))
      (should (equal 6 (plist-get edit :end)))
      (should (equal 2 (plist-get edit :delete-len))))))

(ert-deftest emacs-edit-builtins-test/sexp-symbol-char-p ()
  (should (emacs-edit-sexp-symbol-char-p ?a))
  (should (emacs-edit-sexp-symbol-char-p ?-))
  (should-not (emacs-edit-sexp-symbol-char-p ?\s))
  (should-not (emacs-edit-sexp-symbol-char-p nil)))

(ert-deftest emacs-edit-builtins-test/sexp-skip-forward-ws-skips-comments ()
  (emacs-edit-builtins-test--with-fresh-buffer "  ; comment\nabc"
    (should (equal 13 (emacs-edit-sexp-skip-forward-ws
                       (nelisp-ec-point-max))))
    (should (equal 13 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-sexp-forward-symbol ()
  (emacs-edit-builtins-test--with-fresh-buffer "abc def"
    (should (equal 4 (emacs-edit-scan-sexp-forward
                      (nelisp-ec-point-max))))
    (should (equal 4 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-sexp-forward-balanced-list ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a (b)) c"
    (should (equal 8 (emacs-edit-scan-sexp-forward
                      (nelisp-ec-point-max))))
    (should (equal 8 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-sexp-forward-string-with-escape ()
  (emacs-edit-builtins-test--with-fresh-buffer "\"a\\\"b\" c"
    (should (equal 7 (emacs-edit-scan-sexp-forward
                      (nelisp-ec-point-max))))
    (should (equal 7 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-sexp-forward-unbalanced-keeps-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a"
    (should-not (emacs-edit-scan-sexp-forward (nelisp-ec-point-max)))
    (should (equal 1 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/scan-sexp-backward-balanced-list ()
  (emacs-edit-builtins-test--with-fresh-buffer "x (a b)"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should (equal 3 (emacs-edit-scan-sexp-backward
                      (nelisp-ec-point-min))))
    (should (equal 3 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/matching-paren-position-direct-preserves-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a [b] c)"
    (should (equal 9 (emacs-edit-matching-paren-position-direct)))
    (should (equal 1 (nelisp-ec-point)))
    (nelisp-ec-goto-char 7)
    (should (equal 4 (emacs-edit-matching-paren-position-direct)))
    (should (equal 7 (nelisp-ec-point)))
    (nelisp-ec-goto-char 3)
    (should-not (emacs-edit-matching-paren-position-direct))
    (should (equal 3 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/matching-paren-position-direct-unbalanced-preserves-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a"
    (should-not (emacs-edit-matching-paren-position-direct))
    (should (equal 1 (nelisp-ec-point)))))

(ert-deftest emacs-edit-builtins-test/kill-sexp-direct-kills-after-whitespace ()
  (emacs-edit-builtins-test--with-fresh-buffer "  (a b) c"
    (let ((edit (emacs-edit-kill-sexp-direct)))
      (should (equal "   c" (nelisp-ec-buffer-string)))
      (should (equal "(a b)" (car kill-ring)))
      (should (equal 3 (plist-get edit :beg)))
      (should (equal 8 (plist-get edit :end)))
      (should (equal "(a b)" (plist-get edit :text)))
      (should (equal 5 (plist-get edit :delete-len)))
      (should (eq 'killed (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/kill-sexp-direct-scan-error-restores-point ()
  (emacs-edit-builtins-test--with-fresh-buffer "  (a"
    (let ((edit (emacs-edit-kill-sexp-direct)))
      (should (equal "  (a" (nelisp-ec-buffer-string)))
      (should (equal 1 (nelisp-ec-point)))
      (should-not (plist-get edit :beg))
      (should (eq 'scan-error (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/forward-sexp-direct-moves ()
  (emacs-edit-builtins-test--with-fresh-buffer "  (a b) c"
    (let ((edit (emacs-edit-forward-sexp-direct)))
      (should (eq 'moved (plist-get edit :status)))
      (should (equal 8 (plist-get edit :point)))
      (should (equal 8 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/forward-sexp-direct-moves-multiple ()
  (emacs-edit-builtins-test--with-fresh-buffer "(a) (b)"
    (let ((edit (emacs-edit-forward-sexp-direct 2)))
      (should (eq 'moved (plist-get edit :status)))
      (should (equal 8 (plist-get edit :point)))
      (should (equal 8 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/forward-sexp-direct-scan-error ()
  (emacs-edit-builtins-test--with-fresh-buffer "  (a"
    (let ((edit (emacs-edit-forward-sexp-direct)))
      (should (eq 'scan-error (plist-get edit :status)))
      (should (equal 3 (plist-get edit :point)))
      (should (equal 3 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/backward-sexp-direct-moves ()
  (emacs-edit-builtins-test--with-fresh-buffer "x (a b)  "
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-backward-sexp-direct)))
      (should (eq 'moved (plist-get edit :status)))
      (should (equal 3 (plist-get edit :point)))
      (should (equal 3 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/backward-sexp-direct-scan-error ()
  (emacs-edit-builtins-test--with-fresh-buffer "]"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-backward-sexp-direct)))
      (should (eq 'scan-error (plist-get edit :status)))
      (should (equal 2 (plist-get edit :point)))
      (should (equal 2 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/sentence-end-char-p ()
  (should (emacs-edit-sentence-end-char-p ?.))
  (should (emacs-edit-sentence-end-char-p ?!))
  (should (emacs-edit-sentence-end-char-p ??))
  (should-not (emacs-edit-sentence-end-char-p ?,)))

(ert-deftest emacs-edit-builtins-test/forward-sentence-position-finds-terminator ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (should (equal 4 (emacs-edit-forward-sentence-position 1)))))

(ert-deftest emacs-edit-builtins-test/forward-sentence-motion-position-skips-space ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (should (equal 5 (emacs-edit-forward-sentence-motion-position 1)))))

(ert-deftest emacs-edit-builtins-test/forward-sentence-position-uses-eob ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi"
    (should (equal (nelisp-ec-point-max)
                   (emacs-edit-forward-sentence-position 1)))))

(ert-deftest emacs-edit-builtins-test/backward-sentence-position-finds-boundary ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (should (equal 5 (emacs-edit-backward-sentence-position
                      (nelisp-ec-point-max))))))

(ert-deftest emacs-edit-builtins-test/forward-sentence-direct-moves-to-next-start ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (let ((move (emacs-edit-forward-sentence-direct)))
      (should (equal 5 (nelisp-ec-point)))
      (should (equal 1 (plist-get move :old-point)))
      (should (equal 5 (plist-get move :point)))
      (should (eq 'moved (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/forward-sentence-direct-reports-eob ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi"
    (let ((move (emacs-edit-forward-sentence-direct)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (eq 'eob (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/forward-sentence-direct-eob-terminator-is-moved ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi."
    (let ((move (emacs-edit-forward-sentence-direct)))
      (should (equal (nelisp-ec-point-max) (nelisp-ec-point)))
      (should (eq 'moved (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/backward-sentence-direct-moves ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((move (emacs-edit-backward-sentence-direct)))
      (should (equal 5 (nelisp-ec-point)))
      (should (equal 5 (plist-get move :point)))
      (should (eq 'moved (plist-get move :status))))))

(ert-deftest emacs-edit-builtins-test/kill-sentence-direct-kills-forward ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (let ((edit (emacs-edit-kill-sentence-direct)))
      (should (equal " There" (nelisp-ec-buffer-string)))
      (should (equal "Hi." (car kill-ring)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 4 (plist-get edit :end)))
      (should (equal "Hi." (plist-get edit :text)))
      (should (equal 3 (plist-get edit :delete-len)))
      (should (eq 'killed (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/kill-sentence-direct-empty-at-eob ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi."
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-kill-sentence-direct)))
      (should (equal "Hi." (nelisp-ec-buffer-string)))
      (should-not (plist-get edit :beg))
      (should (eq 'empty (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/backward-kill-sentence-direct-kills-backward ()
  (emacs-edit-builtins-test--with-fresh-buffer "Hi. There"
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (let ((edit (emacs-edit-backward-kill-sentence-direct)))
      (should (equal "Hi. " (nelisp-ec-buffer-string)))
      (should (equal "There" (car kill-ring)))
      (should (equal 5 (plist-get edit :beg)))
      (should (equal 10 (plist-get edit :end)))
      (should (equal "There" (plist-get edit :text)))
      (should (equal 5 (plist-get edit :delete-len)))
      (should (eq 'killed (plist-get edit :status))))))

;;;; H. yank inserts head of kill-ring

(ert-deftest emacs-edit-builtins-test/yank-inserts-head ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((kill-ring '("XYZ" "older")))
      (nelisp-ec-goto-char (nelisp-ec-point-max))
      (nelisp-ec-insert (car kill-ring))
      (should (equal "ABXYZ" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-edit-builtins-test/yank-direct-reports-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((kill-ring '("XYZ" "older"))
          (interprogram-paste-function nil))
      (nelisp-ec-goto-char (nelisp-ec-point-max))
      (let ((edit (emacs-edit-yank-direct)))
        (should (equal "ABXYZ" (nelisp-ec-buffer-string)))
        (should (equal 3 (plist-get edit :beg)))
        (should (equal 6 (plist-get edit :end)))
        (should (equal "XYZ" (plist-get edit :text)))
        (should-not (plist-get edit :deleted-newline))))))

(ert-deftest emacs-edit-builtins-test/mouse-yank-primary-direct-moves-and-yanks ()
  (emacs-edit-builtins-test--with-fresh-buffer "ABCD"
    (let ((kill-ring '("X"))
          (interprogram-paste-function nil))
      (nelisp-ec-goto-char (nelisp-ec-point-min))
      (let ((edit (emacs-edit-mouse-yank-primary-direct 3)))
        (should (equal "ABXCD" (nelisp-ec-buffer-string)))
        (should (equal 3 (plist-get edit :point)))
        (should (equal 3 (plist-get edit :beg)))
        (should (equal 4 (plist-get edit :end)))
        (should (equal "X" (plist-get edit :text)))))))

(ert-deftest emacs-edit-builtins-test/run-mouse-yank-primary-command-uses-hooks ()
  (emacs-edit-builtins-test--with-fresh-buffer "ABCD"
    (let ((kill-ring '("X"))
          (interprogram-paste-function nil)
          applied
          status
          point-call)
      (nelisp-ec-goto-char (nelisp-ec-point-min))
      (let ((buffer (current-buffer)))
        (let ((result
               (emacs-edit-run-mouse-yank-primary-command
                :event '(button 2 4 5)
                :point-function (lambda (row col)
                                  (setq point-call (list row col))
                                  3)
                :current-buffer (lambda () buffer)
                :apply-function (lambda (edit)
                                  (setq applied edit))
                :status-function (lambda (message)
                                   (setq status message)))))
          (should (equal '(4 5) point-call))
          (should (eq result applied))
          (should (equal "mouse-2 yank @ point 3" status))
          (should (equal "ABXCD" (nelisp-ec-buffer-string)))
          (should (equal 3 (plist-get result :point))))))))

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

(ert-deftest emacs-edit-builtins-test/self-insert-direct-reports-range ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((edit (emacs-edit-self-insert-direct ?x)))
      (should (equal "xAB" (nelisp-ec-buffer-string)))
      (should (equal 1 (plist-get edit :beg)))
      (should (equal 2 (plist-get edit :end)))
      (should-not (plist-get edit :overwrote)))))

(ert-deftest emacs-edit-builtins-test/self-insert-direct-overwrites ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((overwrite-mode t))
      (nelisp-ec-goto-char 1)
      (let ((edit (emacs-edit-self-insert-direct ?x)))
        (should (equal "xB" (nelisp-ec-buffer-string)))
        (should (equal 1 (plist-get edit :beg)))
        (should (equal 2 (plist-get edit :end)))
        (should (plist-get edit :overwrote))))))

(ert-deftest emacs-edit-builtins-test/run-quoted-insert-command-inserts-literal ()
  (emacs-edit-builtins-test--with-fresh-buffer "AB"
    (let ((result (emacs-edit-run-quoted-insert-command ?x)))
      (should (eq 'inserted (plist-get result :status)))
      (should (equal "quoted-insert: x (#120)"
                     (plist-get result :message)))
      (should (equal "xAB" (nelisp-ec-buffer-string)))
      (should (equal "x" (plist-get (plist-get result :edit) :text))))))

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

(ert-deftest emacs-edit-builtins-test/yank-pop-direct-reports-replacement ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((kill-ring '("alpha" "beta" "gamma"))
          (kill-ring-yank-pointer nil)
          (emacs-edit--last-yank-bounds nil)
          (interprogram-paste-function nil))
      (emacs-edit-yank-direct)
      (let ((edit (emacs-edit-yank-pop-direct 1)))
        (should (equal "beta" (nelisp-ec-buffer-string)))
        (should (equal 1 (plist-get edit :beg)))
        (should (equal 5 (plist-get edit :end)))
        (should (equal "beta" (plist-get edit :text)))
        (should (plist-get edit :replacement))
        (should (equal 5 (plist-get edit :delete-len)))
        (should (equal "alpha" (plist-get edit :delete-text)))
        (should-not (plist-get edit :deleted-newline))))))

(ert-deftest emacs-edit-builtins-test/yank-pop-result-direct-reports-success ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((kill-ring '("alpha" "beta"))
          (kill-ring-yank-pointer nil)
          (emacs-edit--last-yank-bounds nil)
          (interprogram-paste-function nil))
      (emacs-edit-yank-direct)
      (let ((result (emacs-edit-yank-pop-result-direct 1)))
        (should (eq 'ok (plist-get result :status)))
        (should (equal "yank-pop" (plist-get result :message)))
        (should (equal "beta" (plist-get result :text)))
        (should (plist-get result :replacement))
        (should (equal "beta" (nelisp-ec-buffer-string)))))))

(ert-deftest emacs-edit-builtins-test/yank-pop-result-direct-reports-error ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((kill-ring '("alpha"))
          (emacs-edit--last-yank-bounds nil))
      (let ((result (emacs-edit-yank-pop-result-direct 1)))
        (should (eq 'error (plist-get result :status)))
        (should (eq 'error (plist-get result :condition)))
        (should (equal "yank-pop: Previous command was not a yank"
                       (plist-get result :message)))))))

(ert-deftest emacs-edit-builtins-test/run-yank-pop-command-uses-hooks ()
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (let ((kill-ring '("alpha" "beta"))
          (kill-ring-yank-pointer nil)
          (emacs-edit--last-yank-bounds nil)
          (interprogram-paste-function nil)
          applied
          status)
      (emacs-edit-yank-direct)
      (let ((buffer (current-buffer)))
        (let ((result
               (emacs-edit-run-yank-pop-command
                :current-buffer (lambda () buffer)
                :apply-function (lambda (edit)
                                  (setq applied edit))
                :status-function (lambda (message)
                                   (setq status message)))))
          (should (eq 'ok (plist-get result :status)))
          (should (eq result applied))
          (should (equal "yank-pop" status))
          (should (equal "beta" (plist-get result :text)))
          (should (equal "beta" (nelisp-ec-buffer-string))))))))

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

(ert-deftest emacs-edit-builtins-test/public-char-helpers ()
  "Public wrappers expose character inspection without private helper deps."
  (should (emacs-edit-word-char-p ?a))
  (should-not (emacs-edit-word-char-p ?\s))
  (emacs-edit-builtins-test--with-fresh-buffer "abc"
    (should (equal ?a (emacs-edit-char-at (nelisp-ec-point-min))))
    (should (null (emacs-edit-char-at (nelisp-ec-point-max))))))

(ert-deftest emacs-edit-builtins-test/buffer-line-count ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb\n"
    (should (equal 3 (emacs-edit-buffer-line-count))))
  (emacs-edit-builtins-test--with-fresh-buffer ""
    (should (equal 1 (emacs-edit-buffer-line-count)))))

(ert-deftest emacs-edit-builtins-test/goto-line-direct-clamps ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nbb\nccc"
    (let ((edit (emacs-edit-goto-line-direct 2)))
      (should (equal 2 (plist-get edit :line)))
      (should (equal 3 (plist-get edit :total-lines)))
      (should (equal 3 (plist-get edit :point)))
      (should (equal 3 (nelisp-ec-point)))
      (should (eq 'moved (plist-get edit :status))))
    (let ((edit (emacs-edit-goto-line-direct 99)))
      (should (equal 3 (plist-get edit :line)))
      (should (equal 6 (plist-get edit :point)))
      (should (equal 6 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/goto-line-direct-rejects-non-positive ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nb"
    (nelisp-ec-goto-char 3)
    (let ((edit (emacs-edit-goto-line-direct 0)))
      (should (equal 0 (plist-get edit :line)))
      (should (equal 3 (plist-get edit :point)))
      (should (eq 'bad-number (plist-get edit :status)))
      (should (equal 3 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/buffer-row-helpers ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nbb\nccc"
    (should (equal 0 (emacs-edit-buffer-row-at-point 1)))
    (should (equal 1 (emacs-edit-buffer-row-at-point 3)))
    (should (equal 2 (emacs-edit-buffer-row-at-point 6)))
    (should (equal 3 (emacs-edit-buffer-row-start-position 1)))
    (should (equal (nelisp-ec-point-max)
                   (emacs-edit-buffer-row-start-position 99)))))

(ert-deftest emacs-edit-builtins-test/move-to-buffer-row-direct ()
  (emacs-edit-builtins-test--with-fresh-buffer "a\nbb\nccc"
    (let ((edit (emacs-edit-move-to-buffer-row-direct 2)))
      (should (equal 2 (plist-get edit :row)))
      (should (equal 6 (plist-get edit :point)))
      (should (equal 6 (nelisp-ec-point)))
      (should (eq 'moved (plist-get edit :status))))))

(ert-deftest emacs-edit-builtins-test/window-line-target-row ()
  (let ((top (emacs-edit-window-line-target-row 10 5 0))
        (middle (emacs-edit-window-line-target-row 10 5 1))
        (bottom (emacs-edit-window-line-target-row 10 5 2)))
    (should (equal 10 (plist-get top :row)))
    (should (equal "top" (plist-get top :label)))
    (should (equal 1 (plist-get top :next-state)))
    (should (equal 12 (plist-get middle :row)))
    (should (equal "middle" (plist-get middle :label)))
    (should (equal 2 (plist-get middle :next-state)))
    (should (equal 14 (plist-get bottom :row)))
    (should (equal "bottom" (plist-get bottom :label)))
    (should (equal 0 (plist-get bottom :next-state)))))

(ert-deftest emacs-edit-builtins-test/recenter-scroll-offset ()
  (should (equal 8 (emacs-edit-recenter-scroll-offset 10 4)))
  (should (equal 0 (emacs-edit-recenter-scroll-offset 1 10))))

(ert-deftest emacs-edit-builtins-test/word-bounds-at ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd_2!"
    (should (equal '(4 . 8) (emacs-edit-word-bounds-at 5)))
    (should-not (emacs-edit-word-bounds-at 3))))

(ert-deftest emacs-edit-builtins-test/select-word-at-direct-selects-word ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (let ((mark (emacs-edit-select-word-at-direct 4)))
      (should (equal 4 (plist-get mark :beg)))
      (should (equal 6 (plist-get mark :end)))
      (should (equal 6 (plist-get mark :point)))
      (should (eq 'selected (plist-get mark :status)))
      (should (equal 6 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/select-word-at-direct-ignores-non-word ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (nelisp-ec-goto-char 2)
    (let ((mark (emacs-edit-select-word-at-direct 3)))
      (should-not (plist-get mark :beg))
      (should (equal 2 (plist-get mark :point)))
      (should (eq 'not-word (plist-get mark :status)))
      (should (equal 2 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/run-select-word-at-command-state ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (let ((mark (emacs-edit-run-select-word-at-command 4 "buf")))
      (should (eq 'selected (plist-get mark :status)))
      (should (equal 4 (plist-get mark :mark)))
      (should (equal "buf" (plist-get mark :buffer)))
      (should-not (plist-get mark :shift-region))
      (should (equal "Selected word (2 chars)"
                     (plist-get mark :message))))))

(ert-deftest emacs-edit-builtins-test/run-select-word-at-command-no-word-message ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab cd"
    (let ((mark (emacs-edit-run-select-word-at-command 3 "buf")))
      (should (eq 'not-word (plist-get mark :status)))
      (should (equal "double-click: no word at point"
                     (plist-get mark :message))))))

(ert-deftest emacs-edit-builtins-test/select-line-at-direct-selects-line ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncde\nz"
    (let ((mark (emacs-edit-select-line-at-direct 5)))
      (should (equal 4 (plist-get mark :beg)))
      (should (equal 7 (plist-get mark :end)))
      (should (equal 7 (plist-get mark :point)))
      (should (eq 'selected (plist-get mark :status)))
      (should (equal 7 (nelisp-ec-point))))))

(ert-deftest emacs-edit-builtins-test/run-select-line-at-command-state ()
  (emacs-edit-builtins-test--with-fresh-buffer "ab\ncde\nz"
    (let ((mark (emacs-edit-run-select-line-at-command 5 "buf")))
      (should (eq 'selected (plist-get mark :status)))
      (should (equal 4 (plist-get mark :mark)))
      (should (equal "buf" (plist-get mark :buffer)))
      (should-not (plist-get mark :shift-region))
      (should (equal "Selected line (3 chars)"
                     (plist-get mark :message))))))

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
