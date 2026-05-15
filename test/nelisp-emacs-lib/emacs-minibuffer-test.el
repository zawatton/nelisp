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
;; Total: 39 tests (>= task spec 15+)

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

(provide 'emacs-minibuffer-test)
;;; emacs-minibuffer-test.el ends here
