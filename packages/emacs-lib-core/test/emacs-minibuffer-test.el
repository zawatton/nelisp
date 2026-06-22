;;; emacs-minibuffer-test.el --- ERT tests for emacs-minibuffer.el  -*- lexical-binding: t; -*-

;; Phase 1 module 5/6 tests per nelisp-emacs Doc 01.
;; Covers all 5 categories of `emacs-minibuffer-*' API plus the
;; ERT plug-in pattern (`emacs-minibuffer--read-fn' /
;; `emacs-minibuffer--y-or-n-fn' / `emacs-minibuffer--key-fn').
;;
;; Categories:
;;   A. core readers                              (5 tests)
;;   B. typed readers                             (6 tests)
;;   C. confirmation                              (3 tests)
;;   D. completion                                (15 tests)
;;   E. minibuffer state / control                (6 tests)
;;   F. plug-in / control-flow / sentinel         (4 tests)
;;   G. GUI backend adapter                       (16 tests)
;; Total: 72 tests (>= task spec 15+)

(require 'ert)
(require 'emacs-minibuffer)
(require 'nelisp-emacs-compat)

;;; Fresh-world fixture

(defmacro emacs-minibuffer-test--with-fresh-world (&rest body)
  "Run BODY with a clean minibuffer module + a fresh ec-buffer registry."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil))
     (unwind-protect
         (progn
           (emacs-minibuffer-reset)
           ,@body)
       (emacs-minibuffer-reset))))

;;;; A. core readers (5 tests)

(ert-deftest emacs-minibuffer-read-from-minibuffer-basic ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "hello")
    (should (string-equal "hello"
                          (emacs-minibuffer-read-from-minibuffer "Prompt: ")))))

(ert-deftest emacs-minibuffer-read-from-minibuffer-default-on-empty ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "")
    (should (string-equal "fallback"
                          (emacs-minibuffer-read-from-minibuffer
                           "P: " nil nil nil nil "fallback")))))

(ert-deftest emacs-minibuffer-read-from-minibuffer-read-back ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "(1 2 3)")
    (should (equal '(1 2 3)
                   (emacs-minibuffer-read-from-minibuffer
                    "Lisp: " nil nil t)))))

(ert-deftest emacs-minibuffer-read-string-no-readback ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "(1 2 3)")
    ;; read-string never `read's
    (should (string-equal "(1 2 3)"
                          (emacs-minibuffer-read-string "Str: ")))))

(ert-deftest emacs-minibuffer-read-no-blanks-rejects-whitespace ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "has space")
    (should-error (emacs-minibuffer-read-no-blanks-input "P: ")
                  :type 'emacs-minibuffer-error)))

(ert-deftest emacs-minibuffer-read-key-int-and-symbol ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input ?a 'f1 "bc")
    (should (eq ?a (emacs-minibuffer-read-key)))
    (should (eq 'f1 (emacs-minibuffer-read-key)))
    ;; "bc" -> ?b consumed, ?c left as a 1-char string
    (should (eq ?b (emacs-minibuffer-read-key)))
    (should (eq ?c (emacs-minibuffer-read-key)))))

;;;; B. typed readers (6 tests)

(ert-deftest emacs-minibuffer-read-buffer-string-default ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "")
    (should (string-equal "scratch"
                          (emacs-minibuffer-read-buffer "Buf: " "scratch")))))

(ert-deftest emacs-minibuffer-read-buffer-from-typed-name ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "foo")))
      (ignore b)
      (emacs-minibuffer-feed-input "foo")
      (should (string-equal "foo"
                            (emacs-minibuffer-read-buffer "Buf: "))))))

(ert-deftest emacs-minibuffer-read-file-name-relative-prepends-dir ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "foo.txt")
    (should (string-equal "/tmp/foo.txt"
                          (emacs-minibuffer-read-file-name "F: " "/tmp")))))

(ert-deftest emacs-minibuffer-read-directory-name-trailing-slash ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "subdir")
    (should (string-equal "/tmp/subdir/"
                          (emacs-minibuffer-read-directory-name
                           "D: " "/tmp")))))

(ert-deftest emacs-minibuffer-read-passwd-confirm-mismatch ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "first" "second")
    (should-error (emacs-minibuffer-read-passwd "P: " t)
                  :type 'emacs-minibuffer-error)))

(ert-deftest emacs-minibuffer-read-passwd-confirm-match ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "same" "same")
    (should (string-equal "same"
                          (emacs-minibuffer-read-passwd "P: " t)))))

(ert-deftest emacs-minibuffer-read-number-default-on-empty ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "")
    (should (= 42 (emacs-minibuffer-read-number "N: " 42)))))

(ert-deftest emacs-minibuffer-read-number-parses-integer ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "-7")
    (should (= -7 (emacs-minibuffer-read-number "N: ")))))

;;;; C. confirmation (3 tests)

(ert-deftest emacs-minibuffer-y-or-n-p-yes ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "y")
    (should (emacs-minibuffer-y-or-n-p "Sure? "))))

(ert-deftest emacs-minibuffer-y-or-n-p-no ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "n")
    (should-not (emacs-minibuffer-y-or-n-p "Sure? "))))

(ert-deftest emacs-minibuffer-yes-or-no-p-strict ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "yes")
    (should (emacs-minibuffer-yes-or-no-p "Sure? "))
    (emacs-minibuffer-feed-input "no")
    (should-not (emacs-minibuffer-yes-or-no-p "Sure? "))
    ;; "y" alone is rejected by yes-or-no-p
    (emacs-minibuffer-feed-input "y")
    (should-error (emacs-minibuffer-yes-or-no-p "Sure? ")
                  :type 'emacs-minibuffer-error)))

;;;; D. completion (4 tests)

(ert-deftest emacs-minibuffer-completing-read-list ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "apple")
    (should (string-equal "apple"
                          (emacs-minibuffer-completing-read
                           "P: " '("apple" "banana" "cherry") nil t)))))

(ert-deftest emacs-minibuffer-completing-read-require-match-rejects ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "durian")
    (should-error (emacs-minibuffer-completing-read
                   "P: " '("apple" "banana") nil t)
                  :type 'emacs-minibuffer-error)))

(ert-deftest emacs-minibuffer-completing-read-predicate ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "banana")
    (should (string-equal
             "banana"
             (emacs-minibuffer-completing-read
              "P: " '("apple" "banana" "avocado")
              (lambda (s) (string-prefix-p "b" s)) t)))
    ;; "apple" filtered out
    (emacs-minibuffer-feed-input "apple")
    (should-error (emacs-minibuffer-completing-read
                   "P: " '("apple" "banana" "avocado")
                   (lambda (s) (string-prefix-p "b" s)) t)
                  :type 'emacs-minibuffer-error)))

(ert-deftest emacs-minibuffer-try-completion-prefix ()
  ;; pure helper, no minibuffer needed
  (should (string-equal "ap"
                        (emacs-minibuffer--try-completion
                         "a" '("apple" "apricot"))))
  (should (eq t (emacs-minibuffer--try-completion
                 "apple" '("apple"))))
  (should (null (emacs-minibuffer--try-completion
                 "x" '("apple" "banana")))))

(ert-deftest emacs-minibuffer-try-completion-public-list ()
  ;; Public wrapper: handles raw COLLECTION via --collection->list.
  (should (string-equal "ap"
                        (emacs-minibuffer-try-completion
                         "a" '("apple" "apricot"))))
  (should (eq t (emacs-minibuffer-try-completion "apple" '("apple"))))
  (should (null (emacs-minibuffer-try-completion
                 "x" '("apple" "banana")))))

(ert-deftest emacs-minibuffer-try-completion-public-with-predicate ()
  (should (string-equal "b"
                        (emacs-minibuffer-try-completion
                         "" '("apple" "banana" "berry")
                         (lambda (s) (string-prefix-p "b" s)))))
  ;; PREDICATE eliminates everything -> nil.
  (should (null (emacs-minibuffer-try-completion
                 "a" '("apple" "apricot")
                 (lambda (_) nil)))))

(ert-deftest emacs-minibuffer-try-completion-alist ()
  ;; (STRING . ANY) alist collection is normalised to its cars.
  (should (string-equal "ap"
                        (emacs-minibuffer-try-completion
                         "a" '(("apple" . 1) ("apricot" . 2)))))
  (should (eq t (emacs-minibuffer-try-completion
                 "apple" '(("apple" . 1))))))

(ert-deftest emacs-minibuffer-try-completion-function-collection ()
  ;; FUNCTION collection: called with ("" nil t) for enumeration.
  (let ((coll (lambda (_s _p flag)
                (when flag '("apple" "apricot" "avocado")))))
    (should (string-equal "a"
                          (emacs-minibuffer-try-completion "" coll)))
    (should (string-equal "ap"
                          (emacs-minibuffer-try-completion "ap" coll)))))

(ert-deftest emacs-minibuffer-try-completion-ignore-case ()
  (let ((emacs-minibuffer-completion-ignore-case t))
    ;; Common prefix of "Apple"+"Application" is "Appl" — case taken from
    ;; the first candidate (post-filter).
    (should (string-equal "Appl"
                          (emacs-minibuffer-try-completion
                           "app" '("Apple" "Application"))))
    ;; STRING differs in case but matches uniquely + length-equal -> t.
    (should (eq t (emacs-minibuffer-try-completion
                   "APPLE" '("apple")))))
  ;; Without ignore-case the match fails on capitalisation.
  (let ((emacs-minibuffer-completion-ignore-case nil))
    (should (null (emacs-minibuffer-try-completion
                   "app" '("Apple"))))))

(ert-deftest emacs-minibuffer-all-completions-basic ()
  (should (equal '("apple" "apricot")
                 (emacs-minibuffer-all-completions
                  "a" '("apple" "apricot" "banana"))))
  ;; Empty STRING returns the whole (filtered) collection.
  (should (equal '("apple" "apricot" "banana")
                 (emacs-minibuffer-all-completions
                  "" '("apple" "apricot" "banana"))))
  ;; No match -> empty list.
  (should (equal nil (emacs-minibuffer-all-completions
                      "z" '("apple" "banana")))))

(ert-deftest emacs-minibuffer-all-completions-predicate ()
  (should (equal '("banana" "berry")
                 (emacs-minibuffer-all-completions
                  "" '("apple" "banana" "berry")
                  (lambda (s) (string-prefix-p "b" s))))))

(ert-deftest emacs-minibuffer-all-completions-function-collection ()
  (let ((coll (lambda (_s _p flag)
                (when flag '("apple" "apricot" "banana")))))
    (should (equal '("apple" "apricot")
                   (emacs-minibuffer-all-completions "a" coll)))))

(ert-deftest emacs-minibuffer-all-completions-ignore-case ()
  (let ((emacs-minibuffer-completion-ignore-case t))
    (should (equal '("Apple" "Application")
                   (emacs-minibuffer-all-completions
                    "app" '("Apple" "Application" "Banana"))))))

(ert-deftest emacs-minibuffer-test-completion-exact-match ()
  (should (eq t (emacs-minibuffer-test-completion
                 "apple" '("apple" "banana"))))
  (should (null (emacs-minibuffer-test-completion
                 "applesauce" '("apple" "banana"))))
  ;; Prefix is not exact -> nil.
  (should (null (emacs-minibuffer-test-completion
                 "app" '("apple")))))

(ert-deftest emacs-minibuffer-test-completion-predicate ()
  ;; Without predicate STRING is in table.
  (should (eq t (emacs-minibuffer-test-completion
                 "apple" '("apple" "banana"))))
  ;; Predicate rejects "apple" -> nil.
  (should (null (emacs-minibuffer-test-completion
                 "apple" '("apple" "banana")
                 (lambda (s) (string-prefix-p "b" s))))))

(ert-deftest emacs-minibuffer-test-completion-ignore-case ()
  (let ((emacs-minibuffer-completion-ignore-case t))
    (should (eq t (emacs-minibuffer-test-completion
                   "APPLE" '("apple" "banana")))))
  (let ((emacs-minibuffer-completion-ignore-case nil))
    (should (null (emacs-minibuffer-test-completion
                   "APPLE" '("apple" "banana"))))))

(ert-deftest emacs-minibuffer-completing-read-default-aliases ()
  ;; Both names must resolve to the same underlying function object.
  (should (eq (indirect-function 'emacs-minibuffer-completing-read-default)
              (indirect-function 'emacs-minibuffer-completing-read)))
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-feed-input "banana")
    (should (string-equal "banana"
                          (emacs-minibuffer-completing-read-default
                           "P: " '("apple" "banana") nil t)))))

(ert-deftest emacs-minibuffer-completing-read-ignore-case ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((emacs-minibuffer-completion-ignore-case t))
      (emacs-minibuffer-feed-input "APPLE")
      (should (string-equal "APPLE"
                            (emacs-minibuffer-completing-read
                             "P: " '("apple" "banana") nil t))))))

;;;; E. minibuffer state / control (6 tests)

(ert-deftest emacs-minibuffer-minibufferp-during-read ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((seen-flag nil))
      (setq emacs-minibuffer--read-fn
            (lambda (_p _i _d _h _k _r)
              (setq seen-flag (emacs-minibuffer-minibufferp
                               (emacs-minibuffer--current-buffer)))
              "x"))
      (emacs-minibuffer-read-from-minibuffer "P: ")
      (should seen-flag))))

(ert-deftest emacs-minibuffer-active-window-during-read ()
  (emacs-minibuffer-test--with-fresh-world
    (let (mid-active outer-active)
      (setq emacs-minibuffer--read-fn
            (lambda (_p _i _d _h _k _r)
              (setq mid-active (emacs-minibuffer-active-minibuffer-window))
              "x"))
      (setq outer-active (emacs-minibuffer-active-minibuffer-window))
      (should-not outer-active)
      (emacs-minibuffer-read-from-minibuffer "P: ")
      ;; mid-active was non-nil during the read
      (should mid-active)
      ;; after-read cleared
      (should-not (emacs-minibuffer-active-minibuffer-window)))))

(ert-deftest emacs-minibuffer-prompt-and-contents-during-read ()
  (emacs-minibuffer-test--with-fresh-world
    (let (mid-prompt mid-contents)
      (setq emacs-minibuffer--read-fn
            (lambda (_p _i _d _h _k _r)
              (setq mid-prompt   (emacs-minibuffer-minibuffer-prompt)
                    mid-contents (emacs-minibuffer-minibuffer-contents))
              "ignored"))
      (emacs-minibuffer-read-from-minibuffer "Hello: " "init")
      ;; prompt must be the literal we passed
      (should (string-equal "Hello: " mid-prompt))
      ;; contents must be the inserted INITIAL (before the reader returned)
      (should (string-equal "init" mid-contents)))))

(ert-deftest emacs-minibuffer-prompt-end-and-width ()
  (emacs-minibuffer-test--with-fresh-world
    (let (mid-end mid-width)
      (setq emacs-minibuffer--read-fn
            (lambda (_p _i _d _h _k _r)
              (setq mid-end   (emacs-minibuffer-minibuffer-prompt-end)
                    mid-width (emacs-minibuffer-minibuffer-prompt-width))
              "x"))
      (emacs-minibuffer-read-from-minibuffer "Hi: ")
      ;; Hi:  is 4 chars + initial empty, so prompt-end = 5 (1-based, after prompt)
      (should (= 5 mid-end))
      (should (= 4 mid-width)))))

(ert-deftest emacs-minibuffer-message-returns-nil ()
  (emacs-minibuffer-test--with-fresh-world
    (should-not (emacs-minibuffer-minibuffer-message "msg %s" "ok"))))

(ert-deftest emacs-minibuffer-stack-pop-on-error ()
  ;; Verify the unwind-protect pops a frame even when the reader signals.
  (emacs-minibuffer-test--with-fresh-world
    (setq emacs-minibuffer--read-fn
          (lambda (_p _i _d _h _k _r)
            (signal 'emacs-minibuffer-error '("forced"))))
    (should (= 0 emacs-minibuffer--depth))
    (should-error (emacs-minibuffer-read-from-minibuffer "P: ")
                  :type 'emacs-minibuffer-error)
    (should (= 0 emacs-minibuffer--depth))
    (should (null emacs-minibuffer--buffers))))

;;;; F. plug-in / control-flow / sentinel (4 tests)

(ert-deftest emacs-minibuffer-exit-and-abort-via-sentinel ()
  (emacs-minibuffer-test--with-fresh-world
    ;; :exit -> empty string (then default substituted)
    (emacs-minibuffer-feed-input :exit)
    (should (string-equal "DEF"
                          (emacs-minibuffer-read-from-minibuffer
                           "P: " nil nil nil nil "DEF")))
    ;; :abort -> signal 'quit (caught via condition-case because ERT's
    ;; should-error does not intercept the bare 'quit signal)
    (emacs-minibuffer-feed-input :abort)
    (let ((caught nil))
      (let ((inhibit-quit nil))
        (condition-case _err
            (emacs-minibuffer-read-from-minibuffer "P: ")
          (quit (setq caught t))))
      (should caught))
    ;; depth must be back to 0 after the abort unwind.
    (should (= 0 emacs-minibuffer--depth))))

(ert-deftest emacs-minibuffer-no-input-error ()
  (emacs-minibuffer-test--with-fresh-world
    ;; queue empty -> should signal
    (should-error (emacs-minibuffer-read-from-minibuffer "P: ")
                  :type 'emacs-minibuffer-no-input)))

(ert-deftest emacs-minibuffer-plugin-read-fn-overrides-queue ()
  (emacs-minibuffer-test--with-fresh-world
    (setq emacs-minibuffer--read-fn
          (lambda (prompt _i _d _h _k _r)
            (concat "got:" prompt)))
    (should (string-equal "got:P: "
                          (emacs-minibuffer-read-from-minibuffer "P: ")))
    ;; queue untouched
    (should (null emacs-minibuffer--input-queue))))

(ert-deftest emacs-minibuffer-history-pushed-on-read ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((my-hist nil))
      (defvar emacs-minibuffer-test--hist nil)
      (setq emacs-minibuffer-test--hist nil)
      (emacs-minibuffer-feed-input "alpha" "beta" "")
      (emacs-minibuffer-read-from-minibuffer "P: " nil nil nil
                                             'emacs-minibuffer-test--hist)
      (emacs-minibuffer-read-from-minibuffer "P: " nil nil nil
                                             'emacs-minibuffer-test--hist)
      ;; empty input is NOT added to history
      (emacs-minibuffer-read-from-minibuffer "P: " nil nil nil
                                             'emacs-minibuffer-test--hist)
      (ignore my-hist)
      (should (equal '("beta" "alpha") emacs-minibuffer-test--hist)))))

;;;; G. GUI backend adapter (14 tests)

(ert-deftest emacs-minibuffer-gui-history-symbol-for-purpose ()
  (emacs-minibuffer-test--with-fresh-world
    (should (string-equal
             "extended-command-history"
             (emacs-minibuffer-gui-history-symbol-for-purpose
              "execute-extended-command")))
    (should (string-equal
             "file-name-history"
             (emacs-minibuffer-gui-history-symbol-for-purpose "dired")))
    (should (string-equal
             "buffer-name-history"
             (emacs-minibuffer-gui-history-symbol-for-purpose
              "switch-to-buffer")))
    (should (string-equal
             "shell-command-history"
             (emacs-minibuffer-gui-history-symbol-for-purpose
              "project-async-shell-command")))
    (should (string-equal
             "minibuffer-history"
             (emacs-minibuffer-gui-history-symbol-for-purpose
              "unknown-command")))))

(ert-deftest emacs-minibuffer-gui-purpose-uses-read-p ()
  (emacs-minibuffer-test--with-fresh-world
    (should (emacs-minibuffer-gui-purpose-uses-read-p "find-file"))
    (should (emacs-minibuffer-gui-purpose-uses-read-p "goto-char"))
    (should (emacs-minibuffer-gui-purpose-uses-read-p "project-query-replace-regexp-to"))
    (should-not (emacs-minibuffer-gui-purpose-uses-read-p
                 "execute-extended-command"))
    (setq emacs-minibuffer-gui-purpose "rename-buffer")
    (should (emacs-minibuffer-gui-purpose-uses-read-p))))

(ert-deftest emacs-minibuffer-gui-keymap-entry ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((source (concat "C-x C-f\tfind-file\tFind file: \n"
                          "C-x b\tswitch-to-buffer\tSwitch to buffer: \n"
                          "bad\tmissing-prompt\n")))
      (should (equal '("find-file" . "Find file: ")
                     (emacs-minibuffer-gui-keymap-entry
                      source "C-x C-f")))
      (should (equal '("switch-to-buffer" . "Switch to buffer: ")
                     (emacs-minibuffer-gui-keymap-entry
                      source "C-x b")))
      (should-not (emacs-minibuffer-gui-keymap-entry
                   source "C-x C-s"))
      (should-not (emacs-minibuffer-gui-keymap-entry
                   source "bad")))))

(ert-deftest emacs-minibuffer-gui-extended-command-followup ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '("goto-char" . "Goto char: ")
                   (emacs-minibuffer-gui-extended-command-followup
                    "goto-char")))
    (should (equal '("project-query-replace-regexp"
                     . "Project query replace regexp: ")
                   (emacs-minibuffer-gui-extended-command-followup
                    "project-query-replace-regexp")))
    (should-not (emacs-minibuffer-gui-extended-command-followup
                 "forward-char"))))

(ert-deftest emacs-minibuffer-gui-extended-command-commit-spec ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '("execute-extended-command"
                     . ("execute-extended-command" . "forward-char"))
                   (emacs-minibuffer-gui-extended-command-commit-spec
                    "forward-char")))
    (should-not (emacs-minibuffer-gui-extended-command-commit-spec
                 "goto-char"))))

(ert-deftest emacs-minibuffer-gui-replace-state-policy ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '("replace-string-to" . "Replace string alpha with: ")
                   (emacs-minibuffer-gui-replace-followup
                    "replace-string" "alpha")))
    (should (equal '("project-query-replace-regexp-to"
                     . "Project query replace regexp beta with: ")
                   (emacs-minibuffer-gui-replace-followup
                    "project-query-replace-regexp" "beta")))
    (should-not (emacs-minibuffer-gui-replace-followup
                 "forward-char" "x"))
    (should (equal "query-replace-regexp"
                   (emacs-minibuffer-gui-replace-commit-command
                    "query-replace-regexp-to")))
    (should-not (emacs-minibuffer-gui-replace-commit-command
                 "replace-string"))
    (should (equal '(:replace-from "old" :changed t)
                   (emacs-minibuffer-gui-replace-from-store-state
                    "old")))
    (should (equal '(:replace-from "" :changed t)
                   (emacs-minibuffer-gui-replace-from-clear-state)))))

(ert-deftest emacs-minibuffer-gui-command-commit-spec ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '("find-file" . ("find-file" . "/tmp/a.txt"))
                   (emacs-minibuffer-gui-command-commit-spec
                    "find-file" "/tmp/a.txt")))
    (should (equal '("rename-buffer" . ("rename-buffer" . "notes"))
                   (emacs-minibuffer-gui-command-commit-spec
                    "rename-buffer" "notes")))))

(ert-deftest emacs-minibuffer-gui-collection-lines ()
  (emacs-minibuffer-test--with-fresh-world
    (should (string-equal
             "alpha\nbeta\ngamma\n"
             (emacs-minibuffer-gui-collection-lines
              '("alpha" ("beta" . 1) gamma 42))))
    (should (string-equal
             "raw\nlines\n"
             (emacs-minibuffer-gui-collection-lines "raw\nlines\n")))
    (should (string-equal
             "compat\n"
             (emacs-minibuffer-gui--collection-lines '("compat"))))))

(ert-deftest emacs-minibuffer-gui-candidate-source-kind ()
  (emacs-minibuffer-test--with-fresh-world
    (should (eq 'buffer-list
                (emacs-minibuffer-gui-candidate-source-kind
                 "switch-to-buffer")))
    (should (eq 'project-buffer-list
                (emacs-minibuffer-gui-candidate-source-kind
                 "project-switch-to-buffer")))
    (should (eq 'emoji
                (emacs-minibuffer-gui-candidate-source-kind "emoji-insert")))
    (should (eq 'extended-command
                (emacs-minibuffer-gui-candidate-source-kind
                 "execute-extended-command")))
    (should (eq 'key
                (emacs-minibuffer-gui-candidate-source-kind "describe-key")))
    (setq emacs-minibuffer-gui-completion-table "explicit\n")
    (should (eq 'explicit
                (emacs-minibuffer-gui-candidate-source-kind
                 "switch-to-buffer")))))

(ert-deftest emacs-minibuffer-gui-candidates-for-purpose ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-gui-register-backend
     :buffer-candidates (lambda () "main\n*scratch*\n")
     :project-buffer-candidates (lambda () "project-a\n")
     :emoji-candidates (lambda () "smile\tSMILING FACE\n")
     :extended-command-candidates (lambda () "forward-char\nkill-line\n")
     :key-candidates (lambda () "C-x C-s\tsave-buffer\n"))
    (should (string-equal
             "main\n*scratch*\n"
             (emacs-minibuffer-gui-candidates-for-purpose
              "switch-to-buffer")))
    (should (string-equal
             "project-a\n"
             (emacs-minibuffer-gui-candidates-for-purpose
              "project-switch-to-buffer")))
    (should (string-equal
             "smile\tSMILING FACE\n"
             (emacs-minibuffer-gui-candidates-for-purpose "emoji-insert")))
    (should (string-equal
             "forward-char\nkill-line\n"
             (emacs-minibuffer-gui-candidates-for-purpose
              "execute-extended-command")))
    (should (string-equal
             "C-x C-s\tsave-buffer\n"
             (emacs-minibuffer-gui-candidates-for-purpose "describe-key")))
    (should (string-match-p
             (regexp-quote "forward-char\n")
             (emacs-minibuffer-gui-candidates-for-purpose
              "describe-function")))))

(ert-deftest emacs-minibuffer-gui-filter-candidate-lines ()
  (emacs-minibuffer-test--with-fresh-world
    (should (string-equal
             "alpha\nalphabet\n"
             (emacs-minibuffer-gui-filter-candidate-lines
              "alpha\nbeta\nalphabet\n\n" "alp")))
    (should (string-equal
             "alpha\nbeta\n"
             (emacs-minibuffer-gui-filter-candidate-lines
              "alpha\nbeta\n" "")))))

(ert-deftest emacs-minibuffer-gui-filtered-candidates-for-purpose ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-gui-register-backend
     :buffer-candidates (lambda () "main\nmail\n*scratch*\n"))
    (should (string-equal
             "main\nmail\n"
             (emacs-minibuffer-gui-filtered-candidates-for-purpose
              "switch-to-buffer" "ma")))
    (should (equal '(:purpose "switch-to-buffer"
                     :candidates "main\nmail\n")
                   (emacs-minibuffer-gui-candidate-refresh-state
                    "switch-to-buffer" "ma")))))

(ert-deftest emacs-minibuffer-gui-completion-candidates ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '("alpha" "alpine")
                   (emacs-minibuffer-gui-completion-candidates
                    (lambda (input)
                      (should (equal "al" input))
                      '("alpha" "alpine"))
                    "al")))
    (should-not
     (emacs-minibuffer-gui-completion-candidates nil "al"))
    (should-not
     (emacs-minibuffer-gui-completion-candidates
      (lambda (_input) (error "boom"))
      "al"))))

(ert-deftest emacs-minibuffer-gui-longest-common-prefix ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal "" (emacs-minibuffer-gui-longest-common-prefix nil)))
    (should (equal "alpha"
                   (emacs-minibuffer-gui-longest-common-prefix
                    '("alpha"))))
    (should (equal "alp"
                   (emacs-minibuffer-gui-longest-common-prefix
                    '("alpha" "alpine" "alpaca"))))))

(ert-deftest emacs-minibuffer-gui-candidate-suffix ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal ""
                   (emacs-minibuffer-gui-candidate-suffix nil
                                                          '("alpha"))))
    (should (equal "  {no match}"
                   (emacs-minibuffer-gui-candidate-suffix t nil)))
    (should (equal "  {alpha}"
                   (emacs-minibuffer-gui-candidate-suffix
                    t '("alpha"))))
    (should (equal "  {alpha beta}"
                   (emacs-minibuffer-gui-candidate-suffix
                    t '("alpha" "beta"))))))

(ert-deftest emacs-minibuffer-gui-tab-completion-plan ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal (list :input "al"
                         :candidates nil
                         :message "No match")
                   (emacs-minibuffer-gui-tab-completion-plan
                    "al" nil #'ignore)))
    (let ((plan (emacs-minibuffer-gui-tab-completion-plan
                 "al"
                 '("alpha")
                 (lambda (input)
                   (should (equal "alpha" input))
                   '("alpha")))))
      (should (equal "alpha" (plist-get plan :input)))
      (should (equal '("alpha") (plist-get plan :candidates)))
      (should-not (plist-member plan :message)))
    (let ((plan (emacs-minibuffer-gui-tab-completion-plan
                 "a"
                 '("alpha" "alpine")
                 (lambda (input)
                   (should (equal "alp" input))
                   '("alpha" "alpine")))))
      (should (equal "alp" (plist-get plan :input)))
      (should (equal '("alpha" "alpine")
                     (plist-get plan :candidates))))
    (should (equal (list :input "alp"
                         :candidates '("alpha" "alpine")
                         :message "2 candidates")
                   (emacs-minibuffer-gui-tab-completion-plan
                    "alp" '("alpha" "alpine") #'ignore)))))

(ert-deftest emacs-minibuffer-gui-key-plan ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '(:action confirm :input "done")
                   (emacs-minibuffer-gui-key-plan
                    'return "done" nil nil)))
    (should (equal '(:action cancel :message "Quit")
                   (emacs-minibuffer-gui-key-plan
                    7 "abc" '("abc") #'ignore)))
    (let ((plan (emacs-minibuffer-gui-key-plan
                 'backspace "ab" nil
                 (lambda (input)
                   (should (equal "a" input))
                   '("alpha")))))
      (should (eq 'update (plist-get plan :action)))
      (should (equal "a" (plist-get plan :input)))
      (should (equal '("alpha") (plist-get plan :candidates))))
    (let ((plan (emacs-minibuffer-gui-key-plan
                 ?c "ab" nil
                 (lambda (input)
                   (should (equal "abc" input))
                   '("abc")))))
      (should (eq 'update (plist-get plan :action)))
      (should (equal "abc" (plist-get plan :input)))
      (should (equal '("abc") (plist-get plan :candidates))))
    (should (equal '(:action ignore :input "ab" :candidates ("abc"))
                   (emacs-minibuffer-gui-key-plan
                    'left "ab" '("abc") #'ignore)))))

(ert-deftest emacs-minibuffer-gui-text-state-helpers ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '(:text "ac" :cursor 1 :changed t)
                   (emacs-minibuffer-gui-text-delete-backward-state
                    "abc" 2)))
    (should (equal '(:text "abc" :cursor 0 :changed nil)
                   (emacs-minibuffer-gui-text-delete-backward-state
                    "abc" 0)))
    (should (equal '(:text "abXYc" :cursor 4 :changed t)
                   (emacs-minibuffer-gui-text-insert-state
                    "abc" 2 "XY")))
    (should (equal '(:text "alpha" :cursor 5 :changed t)
                   (emacs-minibuffer-gui-complete-first-line-state
                    "alpha\nalpine\n")))
    (should-not
     (emacs-minibuffer-gui-complete-first-line-state ""))))

(ert-deftest emacs-minibuffer-gui-session-state-helpers ()
  (emacs-minibuffer-test--with-fresh-world
    (should (equal '(:purpose "execute-extended-command"
                     :prompt "M-x "
                     :active t
                     :text ""
                     :cursor 0
                     :candidates ""
                     :effective-command "minibuffer"
                     :status "minibuffer")
                   (emacs-minibuffer-gui-session-begin-state
                    "execute-extended-command" "M-x ")))
    (should (equal '(:text "forward-char" :cursor 12 :changed t)
                   (emacs-minibuffer-gui-session-initial-input-state
                    "forward-char")))
    (should-not
     (emacs-minibuffer-gui-session-initial-input-state ""))
    (should (equal '(:committed-text "forward-char"
                     :history
                     "old\nexecute-extended-command\tforward-char\nextended-command-history\tforward-char\n"
                     :active nil
                     :prompt ""
                     :text ""
                     :cursor 0
                     :candidates ""
                     :require-match nil)
                   (emacs-minibuffer-gui-session-commit-state
                    "execute-extended-command"
                    "forward-char"
                    "old\n"
                    "extended-command-history")))
    (should (equal '(:committed-text ""
                     :history "old\n"
                     :active nil
                     :prompt ""
                     :text ""
                     :cursor 0
                     :candidates ""
                     :require-match nil)
                   (emacs-minibuffer-gui-session-commit-state
                    "execute-extended-command"
                    ""
                    "old\n"
                    "extended-command-history")))))

(ert-deftest emacs-minibuffer-gui-enter-and-exit-state ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((callback #'ignore)
          (completion-fn (lambda (input)
                           (should (equal "" input))
                           '("alpha"))))
      (let ((state (emacs-minibuffer-gui-enter-state
                    "Prompt: " callback completion-fn)))
        (should (plist-get state :active))
        (should (equal "Prompt: " (plist-get state :prompt)))
        (should (equal "" (plist-get state :input)))
        (should (eq callback (plist-get state :on-confirm)))
        (should (eq completion-fn (plist-get state :completion-fn)))
        (should (equal '("alpha") (plist-get state :candidates)))))
    (should (equal '(:active nil
                     :prompt ""
                     :input ""
                     :on-confirm nil
                     :completion-fn nil
                     :candidates nil)
                   (emacs-minibuffer-gui-exit-state)))))

(ert-deftest emacs-minibuffer-gui-standard-backend-normalizes-callbacks ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((backend
           (emacs-minibuffer-gui-standard-backend
            :begin-read #'ignore
            :set-initial-input nil
            :insert-text #'identity)))
      (should (eq #'ignore (plist-get backend :begin-read)))
      (should-not (memq :set-initial-input backend))
      (should (eq #'identity (plist-get backend :insert-text))))
    (should-error
     (emacs-minibuffer-gui-standard-backend :unknown #'ignore)
     :type 'wrong-type-argument)))

(ert-deftest emacs-minibuffer-gui-register-standard-backend-installs-plist ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-gui-register-standard-backend
     :begin-read #'ignore
     :set-status #'identity)
    (should (eq #'ignore
                (plist-get emacs-minibuffer-gui-backend :begin-read)))
    (should (eq #'identity
                (plist-get emacs-minibuffer-gui-backend :set-status)))))

(ert-deftest emacs-minibuffer-gui-backend-call-dispatches-registered-callback ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-standard-backend
       :set-status
       (lambda (status)
         (push status calls)
         :ok))
      (should (eq :ok
                  (emacs-minibuffer-gui-backend-call :set-status "ready")))
      (should (equal '("ready") calls))
      (should-not (emacs-minibuffer-gui-backend-call :insert-text "x")))))

(ert-deftest emacs-minibuffer-gui-read-from-minibuffer-backend ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-prompt
                     emacs-minibuffer-gui-initial-input
                     emacs-minibuffer-gui-require-match)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input) calls)))
      (should (eq :started
                  (emacs-minibuffer-read-from-minibuffer
                   "Prompt: " "seed" nil nil nil "fallback")))
      (should (equal '((:begin "Prompt: " "seed" nil)
                       (:initial "seed"))
                     (reverse calls)))
      (should (string-equal "" emacs-minibuffer-gui-completion-table))
      (should-not emacs-minibuffer-gui-collection))))

(ert-deftest emacs-minibuffer-gui-completing-read-backend ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-prompt
                     emacs-minibuffer-gui-completion-table
                     emacs-minibuffer-gui-require-match)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input) calls)))
      (should (eq :started
                  (emacs-minibuffer-completing-read
                   "Pick: " '("alpha" ("beta" . 1) gamma) nil t "al")))
      (should (equal '((:begin "Pick: " "alpha\nbeta\ngamma\n" t)
                       (:initial "al"))
                     (reverse calls)))
      (should (equal '("alpha" ("beta" . 1) gamma)
                     emacs-minibuffer-gui-collection)))))

(ert-deftest emacs-minibuffer-gui-start-purpose-read ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt
                     emacs-minibuffer-gui-completion-table
                     emacs-minibuffer-gui-require-match)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input) calls))
       :extended-command-candidates
       (lambda () "find-file\nsave-buffer\n"))
      (should (eq :started
                  (emacs-minibuffer-gui-start-purpose-read
                   "find-file" "Find file: ")))
      (should (equal '((:begin "find-file" "Find file: " "" nil)
                       (:initial ""))
                     (reverse calls)))
      (setq calls nil)
      (should (eq :started
                  (emacs-minibuffer-gui-start-purpose-read
                   "execute-extended-command" "M-x ")))
      (should (equal '((:begin "execute-extended-command" "M-x " "" t)
                       (:initial ""))
                     (reverse calls)))
      (should (string-equal
               "find-file\n"
               (emacs-minibuffer-gui-filtered-candidates-for-purpose
                "execute-extended-command" "find"))))))

(ert-deftest emacs-minibuffer-gui-start-from-keymap ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((source (concat "C-x C-f\tfind-file\tFind file: \n"
                          "C-x b\tswitch-to-buffer\tSwitch to buffer: \n"))
          calls)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt
                     emacs-minibuffer-gui-require-match)
               calls)
         :started)
       :set-initial-input (lambda () nil)
       :buffer-candidates (lambda () "main\nmail\n"))
      (should (emacs-minibuffer-gui-start-from-keymap source "C-x C-f"))
      (should (equal '(("find-file" "Find file: " nil)) calls))
      (setq calls nil)
      (should (emacs-minibuffer-gui-start-from-keymap source "C-x b"))
      (should (equal '(("switch-to-buffer" "Switch to buffer: " t)) calls))
      (setq calls nil)
      (should-not (emacs-minibuffer-gui-start-from-keymap source "C-x C-s"))
      (should-not calls))))

(ert-deftest emacs-minibuffer-gui-start-spec-from-keymaps ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((mode-source "C-c x\tmode-command\tMode prompt: \n")
          (global-source (concat "C-c x\tglobal-command\tGlobal prompt: \n"
                                 "C-c y\tglobal-y\tGlobal y: \n")))
      (should (equal '(:purpose "mode-command"
                       :prompt "Mode prompt: "
                       :key "C-c x"
                       :initial-input "seed"
                       :source mode)
                     (emacs-minibuffer-gui-start-spec-from-keymaps
                      mode-source global-source "C-c x" "seed")))
      (should (equal '(:purpose "global-y"
                       :prompt "Global y: "
                       :key "C-c y"
                       :initial-input ""
                       :source global)
                     (emacs-minibuffer-gui-start-spec-from-keymaps
                      mode-source global-source "C-c y")))
      (should-not
       (emacs-minibuffer-gui-start-spec-from-keymaps
        mode-source global-source "C-c z")))))

(ert-deftest emacs-minibuffer-gui-start-spec ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls text cursor finished)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt)
               calls)
         :started)
       :set-initial-input (lambda () nil)
       :set-text (lambda (value) (setq text value))
       :set-cursor (lambda (value) (setq cursor value))
       :finish-read (lambda () (setq finished t)))
      (should
       (emacs-minibuffer-gui-start-spec
        '(:purpose "find-file"
          :prompt "Find file: "
          :key "C-x C-f"
          :initial-input "/tmp/a.txt"
          :source global)))
      (should (equal '((:begin "find-file" "Find file: "))
                     calls))
      (should (string-equal "/tmp/a.txt" text))
      (should (= 10 cursor))
      (should finished)
      (setq calls nil
            text nil
            cursor nil
            finished nil)
      (should-not (emacs-minibuffer-gui-start-spec nil))
      (should-not calls)
      (should-not text)
      (should-not cursor)
      (should-not finished))))

(ert-deftest emacs-minibuffer-gui-maybe-start-from-keymap-prefill ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls text cursor finished)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input)
               calls))
       :set-text (lambda (value) (setq text value))
       :set-cursor (lambda (value) (setq cursor value))
       :finish-read (lambda () (setq finished t)))
      (should
       (emacs-minibuffer-gui-maybe-start-from-keymap
        "C-x C-f\tfind-file\tFind file: \n"
        "C-x C-f"
        "/tmp/a.txt"))
      (should (equal '((:initial "")
                       (:begin "find-file" "Find file: "))
                     calls))
      (should (string-equal "/tmp/a.txt" text))
      (should (= 10 cursor))
      (should finished))))

(ert-deftest emacs-minibuffer-gui-maybe-start-from-keymaps-mode-first ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((mode-source "C-c x\tmode-command\tMode prompt: \n")
          (global-source "C-c x\tglobal-command\tGlobal prompt: \n")
          calls text cursor finished)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt)
               calls)
         :started)
       :set-initial-input (lambda () nil)
       :set-text (lambda (value) (setq text value))
       :set-cursor (lambda (value) (setq cursor value))
       :finish-read (lambda () (setq finished t)))
      (should
       (emacs-minibuffer-gui-maybe-start-from-keymaps
        mode-source global-source "C-c x" "seed"))
      (should (equal '((:begin "mode-command" "Mode prompt: "))
                     calls))
      (should (string-equal "seed" text))
      (should (= 4 cursor))
      (should finished))))

(ert-deftest emacs-minibuffer-gui-start-current-context ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () "find-file")
       :prompt (lambda () "Find file: ")
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input)
               calls)))
      (should (eq :started
                  (emacs-minibuffer-gui-start-current-context)))
      (should (equal '((:initial "")
                       (:begin "find-file" "Find file: "))
                     calls)))))

(ert-deftest emacs-minibuffer-gui-maybe-start-current-context ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls text cursor finished)
      (emacs-minibuffer-gui-register-backend
       :mode-keymap-source (lambda () "C-c x\tmode-command\tMode: \n")
       :keymap-source (lambda () "C-c x\tglobal-command\tGlobal: \n")
       :key (lambda () "C-c x")
       :initial-input (lambda () "seed")
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-purpose
                     emacs-minibuffer-gui-prompt)
               calls)
         :started)
       :set-initial-input (lambda () nil)
       :set-text (lambda (value) (setq text value))
       :set-cursor (lambda (value) (setq cursor value))
       :finish-read (lambda () (setq finished t)))
      (should (emacs-minibuffer-gui-maybe-start-current-context))
      (should (equal '((:begin "mode-command" "Mode: ")) calls))
      (should (string-equal "seed" text))
      (should (= 4 cursor))
      (should finished))))

(ert-deftest emacs-minibuffer-gui-handle-key-edits-state ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((text "ab")
          (cursor 2)
          refreshed
          statuses
          effective)
      (emacs-minibuffer-gui-register-backend
       :key (lambda () "c")
       :purpose (lambda () "find-file")
       :insert-text
       (lambda (value)
         (setq text
               (concat (substring text 0 cursor)
                       value
                       (substring text cursor)))
         (setq cursor (+ cursor (length value))))
       :delete-backward-char
       (lambda ()
         (setq text
               (concat (substring text 0 (1- cursor))
                       (substring text cursor)))
         (setq cursor (1- cursor)))
       :refresh-candidates (lambda () (setq refreshed (1+ (or refreshed 0))))
       :set-effective-command (lambda (value) (push value effective))
       :set-status (lambda (value) (push value statuses)))
      (emacs-minibuffer-gui-handle-key)
      (should (string-equal "abc" text))
      (should (= 3 cursor))
      (emacs-minibuffer-gui-handle-key "DEL" "find-file")
      (should (string-equal "ab" text))
      (should (= 2 cursor))
      (should (= 2 refreshed))
      (should (equal '("minibuffer" "minibuffer") statuses))
      (should (equal '("minibuffer" "minibuffer") effective)))))

(ert-deftest emacs-minibuffer-gui-handle-key-current-context ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((text "ab")
          (cursor 2)
          refreshed)
      (emacs-minibuffer-gui-register-backend
       :key (lambda () "c")
       :purpose (lambda () "find-file")
       :insert-text
       (lambda (value)
         (setq text
               (concat (substring text 0 cursor)
                       value
                       (substring text cursor)))
         (setq cursor (+ cursor (length value))))
       :refresh-candidates (lambda () (setq refreshed t))
       :set-effective-command (lambda (_value) nil)
       :set-status (lambda (_value) nil))
      (emacs-minibuffer-gui-handle-key-current-context)
      (should (string-equal "abc" text))
      (should (= 3 cursor))
      (should refreshed))))

(ert-deftest emacs-minibuffer-gui-handle-key-control-paths ()
  (emacs-minibuffer-test--with-fresh-world
    (let (finished completed cleared query statuses)
      (emacs-minibuffer-gui-register-backend
       :finish-read (lambda () (setq finished t))
       :complete (lambda () (setq completed t))
       :clear-quit-state (lambda () (setq cleared t))
       :handle-query-replace-key (lambda () (setq query t))
       :set-effective-command (lambda (_value) nil)
       :set-status (lambda (value) (push value statuses)))
      (emacs-minibuffer-gui-handle-key "RET" "find-file")
      (should finished)
      (emacs-minibuffer-gui-handle-key "TAB" "find-file")
      (should completed)
      (emacs-minibuffer-gui-handle-key "C-g" "find-file")
      (should cleared)
      (should (equal '("minibuffer") statuses))
      (emacs-minibuffer-gui-handle-key "y" "query-replace-confirm")
      (should query))))

(ert-deftest emacs-minibuffer-gui-finish-read-executes-m-x-command ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((purpose "execute-extended-command")
          (calls nil))
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () purpose)
       :commit-read (lambda () "forward-char")
       :execute-command-spec
       (lambda (command effective arg)
         (push (list :execute command effective arg) calls)))
      (emacs-minibuffer-gui-finish-read)
      (should (equal '((:execute "execute-extended-command"
                                 "execute-extended-command"
                                 "forward-char"))
                     calls)))))

(ert-deftest emacs-minibuffer-gui-finish-read-starts-followup ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((purpose "execute-extended-command")
          (calls nil))
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () purpose)
       :commit-read (lambda () "goto-line")
       :start-followup
       (lambda (next-purpose prompt)
         (push (list :followup next-purpose prompt) calls))
       :followup-prefill-text (lambda () ""))
      (emacs-minibuffer-gui-finish-read)
      (should (equal '((:followup "goto-line" "Goto line: "))
                     calls)))))

(ert-deftest emacs-minibuffer-gui-finish-read-replace-two-stage ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((purpose "replace-string")
          (text-queue '("old" "new"))
          (replace-from "")
          (calls nil))
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () purpose)
       :commit-read (lambda () (pop text-queue))
       :start-followup
       (lambda (next-purpose prompt)
         (setq purpose next-purpose)
         (push (list :followup next-purpose prompt) calls))
       :followup-prefill-text
       (lambda () "")
       :set-replace-from
       (lambda (text)
         (setq replace-from text)
         (push (list :from text) calls))
       :replace-from (lambda () replace-from)
       :clear-replace-from
       (lambda ()
         (setq replace-from "")
         (push :clear-from calls))
       :execute-replace-command
       (lambda (command from to)
         (push (list :replace command from to) calls)))
      (emacs-minibuffer-gui-finish-read)
      (emacs-minibuffer-gui-finish-read)
      (should (equal '(:clear-from
                       (:replace "replace-string" "old" "new")
                       (:followup "replace-string-to"
                                  "Replace string old with: ")
                       (:from "old"))
                     calls)))))

(ert-deftest emacs-minibuffer-gui-finish-read-generic-command-saves-undo ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((purpose "switch-to-buffer")
          (calls nil))
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () purpose)
       :commit-read (lambda () "notes")
       :save-undo-if-needed (lambda () (push :undo calls))
       :execute-command-spec
       (lambda (command effective arg)
         (push (list :execute command effective arg) calls)))
      (emacs-minibuffer-gui-finish-read)
      (should (equal '((:execute "switch-to-buffer"
                                 "switch-to-buffer"
                                 "notes")
                      :undo)
                     calls)))))

(ert-deftest emacs-minibuffer-gui-finish-read-zap-to-char-saves-undo ()
  (emacs-minibuffer-test--with-fresh-world
    (let ((purpose "zap-to-char")
          (calls nil))
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () purpose)
       :commit-read (lambda () "t")
       :save-undo-if-needed (lambda () (push :undo calls))
       :execute-command-spec
       (lambda (command effective arg)
         (push (list :execute command effective arg) calls)))
      (emacs-minibuffer-gui-finish-read)
      (should (equal '((:execute "zap-to-char"
                                 "zap-to-char"
                                 "t")
                       :undo)
                     calls)))))

(ert-deftest emacs-minibuffer-public-default-variables ()
  (emacs-minibuffer-test--with-fresh-world
    (should (null emacs-minibuffer-prompt-properties))
    (should (eq 'emacs-minibuffer-history
                emacs-minibuffer-default-history-symbol))
    (should (= 2 emacs-minibuffer-message-timeout))
    (should (string-equal "" emacs-minibuffer-gui-history-symbol))
    (dolist (key '(:begin-read :set-initial-input :commit-read :complete
                   :handle-query-replace-key))
      (should (memq key emacs-minibuffer-gui-standard-backend-keys)))
    (dolist (purpose '("find-file" "query-replace-regexp-to" "goto-char"))
      (should (member purpose emacs-minibuffer-gui-read-purpose-names)))
    (should (assoc "goto-line"
                   emacs-minibuffer-gui-extended-command-followup-alist))
    (should (assoc "replace-string"
                   emacs-minibuffer-gui-replace-followup-alist))
    (should (assoc "query-replace-regexp-to"
                   emacs-minibuffer-gui-replace-commit-command-alist))
    (should (member "C-g" emacs-minibuffer-gui-abort-key-names))))

(ert-deftest emacs-minibuffer-gui-refresh-context-from-backend ()
  (emacs-minibuffer-test--with-fresh-world
    (emacs-minibuffer-gui-register-backend
     :purpose (lambda () "find-file")
     :prompt (lambda () "Find file: ")
     :initial-input (lambda () "/tmp/a.txt"))
    (should (emacs-minibuffer-gui-refresh-context-from-backend))
    (should (string-equal "find-file" emacs-minibuffer-gui-purpose))
    (should (string-equal "Find file: " emacs-minibuffer-gui-prompt))
    (should (string-equal "/tmp/a.txt"
                          emacs-minibuffer-gui-initial-input))))

(ert-deftest emacs-minibuffer-gui-direct-read-forwarders ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :begin-read
       (lambda ()
         (push (list :begin
                     emacs-minibuffer-gui-prompt
                     emacs-minibuffer-gui-initial-input
                     emacs-minibuffer-gui-completion-table
                     emacs-minibuffer-gui-require-match)
               calls)
         :started)
       :set-initial-input
       (lambda ()
         (push (list :initial emacs-minibuffer-gui-initial-input) calls))
       :commit-read
       (lambda ()
         (push :commit calls)
         "committed")
       :complete
       (lambda ()
         (push :complete calls)
         :completed))
      (should (eq :started
                  (emacs-minibuffer-gui-read-from-minibuffer
                   "Prompt: " "seed" nil nil nil "fallback")))
      (should (equal '((:begin "Prompt: " "seed" "" nil)
                       (:initial "seed"))
                     (reverse calls)))
      (setq calls nil)
      (should (eq :started
                  (emacs-minibuffer-gui-completing-read
                   "Pick: " '("alpha" "beta") nil t "al")))
      (should (equal '((:begin "Pick: " "al" "alpha\nbeta\n" t)
                       (:initial "al"))
                     (reverse calls)))
      (should (string-equal "committed"
                            (emacs-minibuffer-gui-commit-read)))
      (should (eq :completed (emacs-minibuffer-gui-complete)))
      (setq calls nil)
      (should (emacs-minibuffer-gui-set-initial-input))
      (should (equal '(("al" :initial)) (list (reverse (car calls))))))))

(ert-deftest emacs-minibuffer-gui-abort-key-p ()
  (emacs-minibuffer-test--with-fresh-world
    (should (emacs-minibuffer-gui-abort-key-p "C-g"))
    (should (emacs-minibuffer-gui-abort-key-p "M-ESC ESC"))
    (should-not (emacs-minibuffer-gui-abort-key-p "RET"))
    (should-not (emacs-minibuffer-gui-abort-key-p nil))))

(ert-deftest emacs-minibuffer-gui-finish-followup-direct ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :purpose (lambda () "find-file")
       :start-followup
       (lambda (purpose prompt)
         (push (list :followup purpose prompt) calls))
       :followup-prefill-text (lambda () "seed")
       :set-text (lambda (text) (push (list :text text) calls))
       :set-cursor (lambda (cursor) (push (list :cursor cursor) calls))
       :commit-read (lambda () "seed")
       :execute-command-spec
       (lambda (command effective arg)
         (push (list :execute command effective arg) calls)))
      (should (emacs-minibuffer-gui-finish-followup
               "find-file" "Find file: "))
      (should (equal '((:execute "find-file" "find-file" "seed")
                       (:cursor 4)
                       (:text "seed")
                       (:followup "find-file" "Find file: "))
                     calls)))))

(ert-deftest emacs-minibuffer-gui-execute-command-spec-direct ()
  (emacs-minibuffer-test--with-fresh-world
    (let (calls)
      (emacs-minibuffer-gui-register-backend
       :save-undo-if-needed (lambda () (push :undo calls))
       :execute-command-spec
       (lambda (command effective arg)
         (push (list :execute command effective arg) calls)))
      (should (emacs-minibuffer-gui-execute-command-spec
               '("find-file" . ("find-file" . "/tmp/a.txt"))
               t))
      (should (equal '((:execute "find-file" "find-file" "/tmp/a.txt")
                       :undo)
                     calls))
      (setq calls nil)
      (should-not (emacs-minibuffer-gui-execute-command-spec nil))
      (should-not calls))))

(ert-deftest emacs-minibuffer-window-and-abort-recursive-edit ()
  (emacs-minibuffer-test--with-fresh-world
    (should-not (emacs-minibuffer-minibuffer-window))
    (emacs-minibuffer-feed-input "value")
    (emacs-minibuffer-read-string "P: ")
    (should (emacs-minibuffer-minibuffer-window))
    (should-not (emacs-minibuffer-abort-recursive-edit))
    (let ((caught nil))
      (condition-case _err
          (emacs-minibuffer-read-string "P: ")
        (quit (setq caught t)))
      (should caught))
    (setq emacs-minibuffer--read-fn (lambda (&rest _args) "unused"))
    (let ((caught nil))
      (condition-case _err
          (emacs-minibuffer-abort-recursive-edit)
        (quit (setq caught t)))
      (should caught))))

(provide 'emacs-minibuffer-test)
;;; emacs-minibuffer-test.el ends here
