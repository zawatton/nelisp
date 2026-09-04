;;; emacs-subr-extras-test.el --- ERT for Phase B2 subr.el shims  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;; Tests cover the four subr.el primitives + 15 four-level cdr/car
;; aliases ported in `emacs-subr-extras.el'.  Behaviour mirrors host
;; Emacs so these run identically under the test harness and on
;; standalone NeLisp.

(require 'ert)
(require 'emacs-subr-extras)

;;;; number-sequence

(ert-deftest emacs-subr-extras-number-sequence-basic ()
  (should (equal (number-sequence 1 5) '(1 2 3 4 5))))

(ert-deftest emacs-subr-extras-number-sequence-step-2 ()
  (should (equal (number-sequence 0 10 2) '(0 2 4 6 8 10))))

(ert-deftest emacs-subr-extras-number-sequence-descending ()
  (should (equal (number-sequence 5 1 -1) '(5 4 3 2 1))))

(ert-deftest emacs-subr-extras-number-sequence-single-arg ()
  (should (equal (number-sequence 7) '(7))))

(ert-deftest emacs-subr-extras-number-sequence-empty-asc-when-from-gt-to ()
  (should (equal (number-sequence 5 3) nil)))

;;;; assoc-default

(ert-deftest emacs-subr-extras-assoc-default-found ()
  (should (equal (assoc-default 'b '((a . 1) (b . 2) (c . 3))) 2)))

(ert-deftest emacs-subr-extras-assoc-default-missing-returns-nil ()
  ;; Real `assoc-default' returns nil when no element matches;
  ;; DEFAULT is only used for matched-but-not-cons entries.
  (should (null (assoc-default 'z '((a . 1)) nil 'sentinel))))

(ert-deftest emacs-subr-extras-assoc-default-bare-key-uses-default ()
  ;; Bare-key match (= element is symbol, not cons) returns DEFAULT.
  (should (eq (assoc-default 'b '(a b c) nil 'sentinel) 'sentinel)))

(ert-deftest emacs-subr-extras-assoc-default-with-test-fn ()
  (should (equal (assoc-default "B" '(("a" . 1) ("b" . 2))
                                (lambda (k v) (string= (downcase k) (downcase v))))
                 2)))

(ert-deftest emacs-subr-extras-assoc-default-skips-nil-entries ()
  (should (equal (assoc-default 'a '(nil (a . 1) nil)) 1)))

;;;; string-join

(ert-deftest emacs-subr-extras-string-join-default-sep ()
  (should (string= (string-join '("foo" "bar" "baz")) "foobarbaz")))

(ert-deftest emacs-subr-extras-string-join-with-sep ()
  (should (string= (string-join '("a" "b" "c") "-") "a-b-c")))

(ert-deftest emacs-subr-extras-string-join-empty-list ()
  (should (string= (string-join nil) "")))

;;;; member-ignore-case

(ert-deftest emacs-subr-extras-member-ignore-case-found ()
  (let ((tail (member-ignore-case "BAR" '("foo" "bar" "baz"))))
    (should (equal tail '("bar" "baz")))))

(ert-deftest emacs-subr-extras-member-ignore-case-missing ()
  (should (null (member-ignore-case "qux" '("foo" "bar")))))

(ert-deftest emacs-subr-extras-member-ignore-case-skips-non-strings ()
  (let ((tail (member-ignore-case "bar" '(foo ("nope") "BAR" baz))))
    (should (equal tail '("BAR" baz)))))

;;;; md5

(ert-deftest emacs-subr-extras-md5-fallback-shape ()
  (let ((a (md5 "abc"))
        (b (md5 "abd")))
    (should (stringp a))
    (should (= (length a) 32))
    (should (string-match-p "\\`[0-9a-f]+\\'" a))
    (should (equal a (md5 "abc")))
    (should-not (equal a b))))

;;;; user identity

(ert-deftest emacs-subr-extras-user-identity-shape ()
  (should (integerp (user-uid)))
  (should (integerp (user-real-uid)))
  (should (integerp (group-gid)))
  (should (stringp (user-login-name)))
  (should (stringp (user-real-login-name)))
  (should (stringp (user-full-name))))

;;;; four-level cdr/car (spot-check 4 of 15 — full coverage in cl-lib's defaliases)

(ert-deftest emacs-subr-extras-caaaar ()
  (should (= (caaaar '((((1 . a)))) ) 1)))

(ert-deftest emacs-subr-extras-cdddar ()
  ;; cdddar = cdr(cdr(cdr(car x)))
  (should (equal (cdddar '((1 2 3 4 5) ignored)) '(4 5))))

(ert-deftest emacs-subr-extras-cddddr ()
  ;; cddddr = cdr(cdr(cdr(cdr x))) → 5th element onward
  (should (equal (cddddr '(1 2 3 4 5 6 7)) '(5 6 7))))

(ert-deftest emacs-subr-extras-cadadr ()
  ;; cadadr = car(cdr(car(cdr x)))
  ;; Walk through:
  ;;   x       = ((10) (20 99) 30)
  ;;   cdr x   = ((20 99) 30)
  ;;   car cdr = (20 99)
  ;;   cdr car cdr = (99)
  ;;   car cdr car cdr = 99
  (should (= (cadadr '((10) (20 99) 30)) 99)))

;;;; vendored cl-lib.el load smoke (= the actual symptom we are fixing)

(ert-deftest emacs-subr-extras-fboundp-after-require ()
  (should (fboundp 'number-sequence))
  (should (fboundp 'assoc-default))
  (should (fboundp 'string-join))
  (should (fboundp 'member-ignore-case))
  (dolist (s '(caaaar caaadr caadar caaddr cadaar cadadr caddar
               cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
    (should (fboundp s))))

;;;; if-let* / when-let* / if-let / when-let

(ert-deftest emacs-subr-extras-if-let-star-all-non-nil ()
  (should (= (if-let* ((a 1) (b (+ a 2))) (* a b) 999) 3)))

(ert-deftest emacs-subr-extras-if-let-star-first-nil ()
  (should (= (if-let* ((a nil) (b 99)) 1 2) 2)))

(ert-deftest emacs-subr-extras-if-let-star-second-nil ()
  (should (eq (if-let* ((a 1) (b nil)) 'then 'else) 'else)))

(ert-deftest emacs-subr-extras-if-let-star-no-else ()
  (should (null (if-let* ((a nil)) 'then))))

(ert-deftest emacs-subr-extras-when-let-star-positive ()
  (should (= (when-let* ((a 5) (b (* a 2))) b) 10)))

(ert-deftest emacs-subr-extras-when-let-star-negative ()
  (should (null (when-let* ((a nil)) 'never))))

(ert-deftest emacs-subr-extras-if-let-alias ()
  (should (= (if-let ((a 7)) a 0) 7)))

(ert-deftest emacs-subr-extras-when-let-alias ()
  (should (= (when-let ((a 8)) a) 8)))

;;;; derived-mode-add-parents

(ert-deftest emacs-subr-extras-derived-mode-add-parents-stores-extra-parents ()
  (let* ((mode (make-symbol "emacs-subr-extras-test-derived-mode"))
         (parents '(foo-mode bar-mode)))
    (derived-mode-add-parents mode parents)
    (should (eq (derived-mode-add-parents mode parents) nil))
    (should (eq (get mode 'derived-mode-extra-parents) parents))))

(ert-deftest emacs-subr-extras-derived-mode-add-parents-recursive-flush ()
  (let* ((root (make-symbol "emacs-subr-extras-test-root-mode"))
         (child-a (make-symbol "emacs-subr-extras-test-child-a-mode"))
         (child-b (make-symbol "emacs-subr-extras-test-child-b-mode"))
         (grandchild (make-symbol "emacs-subr-extras-test-grandchild-mode")))
    (put root 'derived-mode--followers (list child-a child-b))
    (put child-a 'derived-mode--followers (list grandchild))
    (put root 'derived-mode--all-parents 'stale-root)
    (put child-a 'derived-mode--all-parents 'stale-child-a)
    (put child-b 'derived-mode--all-parents 'stale-child-b)
    (put grandchild 'derived-mode--all-parents 'stale-grandchild)
    (derived-mode-add-parents root '(base-root-mode))
    (should-not (get root 'derived-mode--all-parents))
    (should-not (get child-a 'derived-mode--all-parents))
    (should-not (get child-b 'derived-mode--all-parents))
    (should-not (get grandchild 'derived-mode--all-parents))
    (should-not (get root 'derived-mode--followers))
    (should-not (get child-a 'derived-mode--followers))
    (should-not (get child-b 'derived-mode--followers))))

(ert-deftest emacs-subr-extras-derived-mode-add-parents-with-no-followers ()
  (let ((mode (make-symbol "emacs-subr-extras-test-no-followers-mode")))
    (put mode 'derived-mode--followers nil)
    (put mode 'derived-mode--all-parents 'stale)
    (derived-mode-add-parents mode '(base-mode))
    (should-not (get mode 'derived-mode--followers))
    (should (equal (get mode 'derived-mode-extra-parents) '(base-mode)))
    (should-not (get mode 'derived-mode--all-parents))))

(provide 'emacs-subr-extras-test)

(ert-deftest emacs-subr-extras-treesit-error-hierarchy ()
  (should (equal (get 'treesit-error 'error-conditions)
                 '(treesit-error error)))
  (should (equal (get 'treesit-font-lock-error 'error-conditions)
                 '(treesit-font-lock-error treesit-error error))))

;;;; treesit-font-lock-rules

(defmacro emacs-subr-extras-test--with-temporary-function (name fn &rest body)
  `(let ((old (and (fboundp ',name) (symbol-function ',name))))
     (unwind-protect
       (progn
         (fset ',name ,fn)
         ,@body)
       (if old
           (fset ',name old)
         (fmakunbound ',name)))))

(ert-deftest emacs-subr-extras-treesit-font-lock-rules-unavailable ()
  (emacs-subr-extras-test--with-temporary-function
   treesit-available-p (lambda () nil)
   (emacs-subr-extras-test--with-temporary-function
    treesit-query-p (lambda (_token) (error "should not be called"))
    (emacs-subr-extras-test--with-temporary-function
     treesit-compiled-query-p (lambda (_token) (error "should not be called"))
     (emacs-subr-extras-test--with-temporary-function
      treesit-query-compile (lambda (_language _token) (error "should not be called"))
      (should-not
       (treesit-font-lock-rules
        :language 'foo
        :feature 'bar
        "query")))))))

(ert-deftest emacs-subr-extras-treesit-font-lock-rules-compile-shape ()
  (let ((seen nil))
    (emacs-subr-extras-test--with-temporary-function
     treesit-available-p (lambda () t)
     (emacs-subr-extras-test--with-temporary-function
      treesit-query-p (lambda (token) (stringp token))
      (emacs-subr-extras-test--with-temporary-function
       treesit-compiled-query-p (lambda (_token) nil)
       (emacs-subr-extras-test--with-temporary-function
        treesit-query-compile
        (lambda (lang token)
          (push (cons lang token) seen)
          (cond
           ((equal token "one") 'compiled-one)
           ((equal token "two") 'compiled-two)
           ((equal token "three") 'compiled-three)
           (t (make-symbol (concat "compiled-" token)))))
        (let ((default-language 'default-language)
              (primary-language 'primary-language)
              (primary-feature 'primary-feature)
              (secondary-feature 'secondary-feature)
              (tertiary-language 'tertiary-language)
              (tertiary-feature 'tertiary-feature))
          (should
           (equal
            (treesit-font-lock-rules
             :default-language default-language
             :language primary-language
             :feature primary-feature
             :override 'append
             "one"
             :feature secondary-feature
             "two"
             :language tertiary-language
             :feature tertiary-feature
             "three")
            '((compiled-one t primary-feature append)
              (compiled-two t secondary-feature nil)
              (compiled-three t tertiary-feature nil))))
          (should (equal (nreverse seen)
                         (list (cons default-language "one")
                               (cons default-language "two")
                               (cons default-language "three")))))))))))

(ert-deftest emacs-subr-extras-treesit-font-lock-rules-compiled-query-shape ()
  (emacs-subr-extras-test--with-temporary-function
   treesit-available-p (lambda () t)
   (emacs-subr-extras-test--with-temporary-function
    treesit-query-p (lambda (token) (eq token 'compiled-token))
    (emacs-subr-extras-test--with-temporary-function
     treesit-compiled-query-p (lambda (token) (eq token 'compiled-token))
     (emacs-subr-extras-test--with-temporary-function
      treesit-query-compile
      (lambda (_language _token) (error "should not be called"))
      (should
       (equal (treesit-font-lock-rules
               :default-language 'default-language
               :feature 'compiled-feature
               'compiled-token)
              (list (list 'default-language 'token)))))))))

(ert-deftest emacs-subr-extras-treesit-font-lock-rules-validation ()
  (emacs-subr-extras-test--with-temporary-function
   treesit-available-p (lambda () t)
   (emacs-subr-extras-test--with-temporary-function
    treesit-query-p (lambda (token) (stringp token))
     (emacs-subr-extras-test--with-temporary-function
     treesit-compiled-query-p (lambda (_token) nil)
     (emacs-subr-extras-test--with-temporary-function
      treesit-query-compile (lambda (_language _token) 'compiled-fallback)
      (should-error (treesit-font-lock-rules :feature 'feat "query")
                    :type 'treesit-font-lock-error)
      (should-error (treesit-font-lock-rules :language 'lang "query")
                    :type 'treesit-font-lock-error)
      (should-error (treesit-font-lock-rules :default-language 'lang :feature 'feat
                                            "query" :bogus 1)
                    :type 'treesit-font-lock-error)
      (should-error (treesit-font-lock-rules :language 'lang :feature 'feat
                                            :override 'invalid "query")
                    :type 'treesit-font-lock-error)
      (should-error (treesit-font-lock-rules :language 'lang :feature t "query")
                    :type 'treesit-font-lock-error)
      (should-error (treesit-font-lock-rules :language 'lang :default-language nil
                                            :feature 'feat "query")
                    :type 'treesit-font-lock-error))))))

;;; emacs-subr-extras-test.el ends here

(ert-deftest emacs-subr-extras-test/gc-stats-api-shape ()
  "garbage-collect / memory-use-counts return host-shaped structures (Doc 06 A2)."
  (let ((stats (garbage-collect)))
    (should (consp stats))
    (should (listp (assq 'conses stats))))
  (should (= 7 (length (memory-use-counts))))
  (should (integerp (nth 6 (memory-use-counts)))))

(ert-deftest emacs-subr-extras-test/event-functions-match-host ()
  "event-modifiers / event-basic-type / event-convert-list match host (B3).
Modifier lists are compared as sets (order is allowed to differ)."
  (let ((symsort (lambda (l) (sort (copy-sequence l)
                                   (lambda (x y) (string< (symbol-name x)
                                                          (symbol-name y)))))))
    (dolist (e (list ?a ?A ?\C-a ?\M-a ?\M-\C-a ?5
                     'left (intern "C-left") (intern "C-M-right") (intern "S-f5")))
      (should (equal (funcall symsort (emacs-subr-extras-event-modifiers e))
                     (funcall symsort (event-modifiers e))))
      (should (equal (emacs-subr-extras-event-basic-type e)
                     (event-basic-type e))))
    (dolist (l (list (list 'control ?a) (list 'meta ?a) (list 'control 'left)
                     (list 'meta 'shift 'f1) (list 'control 'meta 'right)
                     (list 'control 'meta ?a) (list ?a) (list ?A)))
      (should (equal (emacs-subr-extras-event-convert-list l)
                     (event-convert-list l))))))
