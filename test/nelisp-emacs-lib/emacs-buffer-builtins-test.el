;;; emacs-buffer-builtins-test.el --- ERT tests for emacs-buffer-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs buffer builtin bridge.  Under batch
;; host Emacs the host C builtins remain active, so these tests lean on
;; the `nelisp-ec-*' substrate directly to verify the contract that the
;; polyfill body is supposed to bridge to.

;;; Code:

(require 'ert)
(require 'emacs-buffer-builtins)
(require 'emacs-buffer)
(require 'cl-lib)

(defmacro emacs-buffer-builtins-test--with-fresh-world (&rest body)
  "Run BODY with a clean NeLisp buffer registry/current-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil))
     ,@body))

(defmacro emacs-buffer-builtins-test--with-temp-buffer-polyfill (&rest body)
  "Mirror the Phase 9 `with-temp-buffer' rewrite for macroexpand checks."
  (declare (indent 0) (debug (body)))
  (let ((buf (make-symbol "buf")))
    (list 'let (list (list buf (list 'nelisp-ec-generate-new-buffer
                                     " *temp*")))
          (list 'unwind-protect
                (cons 'nelisp-ec-with-current-buffer (cons buf body))
                (list 'nelisp-ec-kill-buffer buf)))))

;;;; A. Load cleanly

(ert-deftest emacs-buffer-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-buffer-builtins))
  (should (fboundp 'buffer-string))
  (should (fboundp 'with-current-buffer))
  (dolist (sym '(default-value default-boundp set-default get-char-property
                 invisible-p next-property-change previous-property-change
                 next-single-property-change previous-single-property-change
                 next-single-char-property-change
                 previous-single-char-property-change
                 insert-and-inherit char-before char-after following-char
                 preceding-char subst-char-in-region
                 buffer-modified-tick buffer-chars-modified-tick))
    (should (fboundp sym))))

(ert-deftest emacs-buffer-builtins-test/default-and-char-property-bridges-in-source ()
  (let ((file (locate-library "emacs-buffer-builtins")))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("(defalias 'default-value #'emacs-buffer-default-value"
                        "(defalias 'default-boundp #'emacs-buffer-default-boundp"
                        "(defalias 'set-default #'emacs-buffer-set-default"
                        "(defun get-char-property"
                        "(defalias 'invisible-p #'emacs-buffer-builtins-invisible-p"
                        "(defalias 'next-single-char-property-change"
                        "(defalias 'previous-single-char-property-change"
                        "(insert-and-inherit         . nelisp-ec-insert)"
                        "(defalias 'subst-char-in-region"
                        "(defalias 'buffer-modified-tick"
                        "(defalias 'buffer-chars-modified-tick"))
        (goto-char (point-min))
        (should (search-forward needle nil t))))))

(ert-deftest emacs-buffer-builtins-test/invisible-p-helper-matches-core-shapes ()
  (let ((buffer-invisibility-spec t))
    (should (eq t (emacs-buffer-builtins-invisible-p 'foo)))
    (should (null (emacs-buffer-builtins-invisible-p nil))))
  (let ((buffer-invisibility-spec nil))
    (should (null (emacs-buffer-builtins-invisible-p 'foo))))
  (let ((buffer-invisibility-spec '(foo (bar . t) baz)))
    (should (eq t (emacs-buffer-builtins-invisible-p 'foo)))
    (should (= 2 (emacs-buffer-builtins-invisible-p 'bar)))
    (should (eq t (emacs-buffer-builtins-invisible-p 'baz)))
    (should (null (emacs-buffer-builtins-invisible-p 'qux)))
    (should (eq t (emacs-buffer-builtins-invisible-p '(foo qux))))))

(ert-deftest emacs-buffer-builtins-test/property-change-bridges-use-substrate ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "props")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (emacs-buffer-put-text-property 3 5 'face 'bold buf)
        (should (= 3 (emacs-buffer-builtins-next-property-change 1 buf nil)))
        (should (= 5 (emacs-buffer-builtins-next-property-change 3 buf nil)))
        (should (= 5 (emacs-buffer-builtins-next-single-property-change
                      3 'face buf nil)))
        (should (= 5 (emacs-buffer-builtins-previous-property-change
                      6 buf nil)))
        (should (= 5 (emacs-buffer-builtins-previous-single-property-change
                      5 'face buf nil)))
        (should (= 9 (emacs-buffer-builtins-next-single-property-change
                      1 'face "string-object" 9)))))))

(ert-deftest emacs-buffer-builtins-test/ensure-initial-buffer-creates-current ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (emacs-buffer-builtins-ensure-initial-buffer)))
      (should (nelisp-ec-buffer-p buf))
      (should (eq buf (nelisp-ec-current-buffer)))
      (should (equal "*scratch*" (nelisp-ec-buffer-name buf)))
      (should (eq buf (cdr (assoc "*scratch*" nelisp-ec--buffers)))))))

(ert-deftest emacs-buffer-builtins-test/ensure-initial-buffer-reuses-existing ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((existing (nelisp-ec-generate-new-buffer "*scratch*")))
      (should (eq existing (emacs-buffer-builtins-ensure-initial-buffer)))
      (should (eq existing (nelisp-ec-current-buffer)))
      (should-not (cdr (assoc "*scratch*<2>" nelisp-ec--buffers))))))

;;;; B. Temp-buffer style roundtrip via nelisp-ec-*

(ert-deftest emacs-buffer-builtins-test/temp-buffer-roundtrip-via-nelisp-ec ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf1 (nelisp-ec-generate-new-buffer " *temp*"))
          (buf2 (nelisp-ec-generate-new-buffer " *temp*")))
      (should (equal "" (nelisp-ec-with-current-buffer buf1
                          (nelisp-ec-buffer-string))))
      (should (equal "alpha" (nelisp-ec-with-current-buffer buf1
                               (nelisp-ec-insert "alpha")
                               (nelisp-ec-buffer-string))))
      (should (equal 6 (nelisp-ec-with-current-buffer buf2
                         (nelisp-ec-insert "alpha")
                         (nelisp-ec-point))))
      (should (equal "alpha" (nelisp-ec-with-current-buffer buf2
                               (nelisp-ec-buffer-string)))))))

(ert-deftest emacs-buffer-builtins-test/text-buffer-insert-char-code-direct ()
  "Layer 0 single-character insertion should work at edge and middle points."
  (let ((tb (make-text-buffer "ac")))
    (text-buffer-set-cursor tb 1)
    (should (eq tb (text-buffer-insert-char-code tb ?b)))
    (should (= 3 (text-buffer-length tb)))
    (should (= 2 (text-buffer-cursor tb)))
    (should (equal "abc" (text-buffer-substring tb 0 3)))
    (text-buffer-set-cursor tb 0)
    (text-buffer-insert-char-code tb ?_)
    (should (equal "_abc" (text-buffer-substring tb 0 4)))
    (text-buffer-set-cursor tb (text-buffer-length tb))
    (text-buffer-insert-char-code tb ?!)
    (should (equal "_abc!" (text-buffer-substring
                            tb 0 (text-buffer-length tb))))))

(ert-deftest emacs-buffer-builtins-test/text-tick-tracks-text-edits ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "tick")))
      (nelisp-ec-with-current-buffer buf
        (should (= 0 (nelisp-ec-buffer-text-tick buf)))
        (nelisp-ec-insert "abc")
        (should (= 1 (nelisp-ec-buffer-text-tick buf)))
        (nelisp-ec-delete-region 2 3)
        (should (= 2 (nelisp-ec-buffer-text-tick buf)))
        (nelisp-ec-delete-region 2 2)
        (should (= 2 (nelisp-ec-buffer-text-tick buf)))))))

(ert-deftest emacs-buffer-builtins-test/insert-skips-redundant-cursor-sync ()
  "Repeated insertion at current point should not re-sync the text cursor."
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "sync"))
          (set-cursor-calls 0)
          (orig (symbol-function 'text-buffer-set-cursor)))
      (cl-letf (((symbol-function 'text-buffer-set-cursor)
                 (lambda (&rest args)
                   (setq set-cursor-calls (1+ set-cursor-calls))
                   (apply orig args))))
        (nelisp-ec-with-current-buffer buf
          (nelisp-ec-insert "a")
          (nelisp-ec-insert "b")
          (should (= 0 set-cursor-calls))
          (should (= 3 (nelisp-ec-point)))
          (should (equal "ab" (nelisp-ec-buffer-string))))))))

(ert-deftest emacs-buffer-builtins-test/insert-char-code-fast-updates-buffer ()
  "The single-char insert helper should preserve normal insert bookkeeping."
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "char-fast")))
      (nelisp-ec-with-current-buffer buf
        (should (= 2 (nelisp-ec-insert-char-code-fast ?a)))
        (should (= 2 (nelisp-ec-point)))
        (should (= 1 (nelisp-ec-buffer-text-tick buf)))
        (should (nelisp-ec-buffer-modified-p buf))
        (should (equal "a" (nelisp-ec-buffer-string)))))))

(ert-deftest emacs-buffer-builtins-test/insert-accepts-character-codes ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "insert-char")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "a" ?b "c")
        (should (equal "abc" (nelisp-ec-buffer-string)))
        (should (= 4 (nelisp-ec-point)))
        (should (= 3 (nelisp-ec-buffer-text-tick buf)))))))

(ert-deftest emacs-buffer-builtins-test/char-accessors-and-subst-use-ec-buffer ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "chars")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abacad")
        (nelisp-ec-goto-char 3)
        (should (= ?b (emacs-buffer-builtins-char-before)))
        (should (= ?a (emacs-buffer-builtins-char-after)))
        (should (= ?b (emacs-buffer-builtins-preceding-char)))
        (should (= ?a (emacs-buffer-builtins-following-char)))
        (should (null (emacs-buffer-builtins-char-before
                       (nelisp-ec-point-min))))
        (should (null (emacs-buffer-builtins-char-after
                       (nelisp-ec-point-max))))
        (emacs-buffer-builtins-subst-char-in-region
         1 (nelisp-ec-point-max) ?a ?x)
        (should (equal "xbxcxd" (nelisp-ec-buffer-string)))
        (should (= 3 (nelisp-ec-point)))))))

;;;; C. with-current-buffer restores selection

(ert-deftest emacs-buffer-builtins-test/with-current-buffer-restores-current-buffer ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (nelisp-ec-set-buffer a)
      (should (eq a (nelisp-ec-current-buffer)))
      (nelisp-ec-with-current-buffer b
        (should (eq b (nelisp-ec-current-buffer)))
        (should (equal "b" (nelisp-ec-buffer-name (nelisp-ec-current-buffer)))))
      (should (eq a (nelisp-ec-current-buffer))))))

;;;; D. generate-new-buffer / kill-buffer

(ert-deftest emacs-buffer-builtins-test/generate-and-kill-buffer-roundtrip ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "scratch")))
      (should (nelisp-ec-buffer-p buf))
      (should (equal "scratch" (nelisp-ec-buffer-name buf)))
      (should (eq buf (cdr (assoc "scratch" nelisp-ec--buffers))))
      (should (equal t (nelisp-ec-kill-buffer buf)))
      (should (nelisp-ec-buffer-killed-p buf))
      (should-not (assoc "scratch" nelisp-ec--buffers))
      (should-error (nelisp-ec-set-buffer buf)
                    :type 'nelisp-ec-buffer-killed))))

;;;; E. point-min / point-max

(ert-deftest emacs-buffer-builtins-test/point-min-max-on-populated-buffer ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "points")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcd")
        (should (= 1 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (should (= 5 (nelisp-ec-point)))
        (should (= 4 (nelisp-ec-buffer-size)))))))

;;;; F. buffer-substring

(ert-deftest emacs-buffer-builtins-test/buffer-substring-range-correct ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "substr")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (should (equal "bcd" (nelisp-ec-buffer-substring 2 5)))
        (should (equal "bcd" (nelisp-ec-buffer-substring 5 2)))
        (should (= 7 (nelisp-ec-point-max)))
        (should (= 1 (nelisp-ec-point-min)))))))

;;;; G. save-excursion

(ert-deftest emacs-buffer-builtins-test/save-excursion-restores-point ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "excursion")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-goto-char 3)
        (should (= 3 (nelisp-ec-point)))
        (nelisp-ec-save-excursion
          (nelisp-ec-goto-char 6)
          (should (= 6 (nelisp-ec-point))))
        (should (= 3 (nelisp-ec-point)))
        (should (eq buf (nelisp-ec-current-buffer)))))))

;;;; H. save-restriction

(ert-deftest emacs-buffer-builtins-test/save-restriction-restores-bounds ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "restrict")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-narrow-to-region 2 5)
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (nelisp-ec-save-restriction
          (nelisp-ec-widen)
          (should (= 1 (nelisp-ec-point-min)))
          (should (= 7 (nelisp-ec-point-max)))
          (nelisp-ec-narrow-to-region 3 4)
          (should (= 3 (nelisp-ec-point-min)))
          (should (= 4 (nelisp-ec-point-max))))
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))))))

;;;; I. narrow-to-region

(ert-deftest emacs-buffer-builtins-test/narrow-to-region-clips-point-min-max ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "narrow")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abcdef")
        (nelisp-ec-goto-char 6)
        (should (= 6 (nelisp-ec-point)))
        (nelisp-ec-narrow-to-region 2 5)
        (should (= 2 (nelisp-ec-point-min)))
        (should (= 5 (nelisp-ec-point-max)))
        (should (= 5 (nelisp-ec-point)))
        (nelisp-ec-widen)
        (should (= 1 (nelisp-ec-point-min)))
        (should (= 7 (nelisp-ec-point-max)))))))

;;;; J. markers

(ert-deftest emacs-buffer-builtins-test/make-marker-set-marker-roundtrip ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "marker")))
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-insert "abc")
        (let ((m (nelisp-ec-make-marker)))
          (should (null (nelisp-ec-marker-position m)))
          (should (null (nelisp-ec-marker-buffer m)))
          (should (null (nelisp-ec-marker-insertion-type m)))
          (should (eq t (nelisp-ec-set-marker-insertion-type m t)))
          (should (eq t (nelisp-ec-marker-insertion-type m)))
          (should (null (nelisp-ec-set-marker-insertion-type m nil)))
          (should (null (nelisp-ec-marker-insertion-type m)))
          (should (eq m (nelisp-ec-set-marker m 2 buf)))
          (should (= 2 (nelisp-ec-marker-position m)))
          (should (eq buf (nelisp-ec-marker-buffer m)))
          (should (eq m (nelisp-ec-set-marker m nil)))
          (should (null (nelisp-ec-marker-position m)))
          (should (null (nelisp-ec-marker-buffer m))))))))

;;;; K. macroexpand shape of with-temp-buffer rewrite

(ert-deftest emacs-buffer-builtins-test/with-temp-buffer-macroexpand-uses-ec-substrate ()
  (cl-letf (((symbol-function 'with-temp-buffer)
             (symbol-function
              'emacs-buffer-builtins-test--with-temp-buffer-polyfill)))
    (let* ((expanded (macroexpand '(with-temp-buffer
                                     (insert "x")
                                     (buffer-string))))
           (flat (flatten-tree expanded)))
      (should (eq 'let (car expanded)))
      (should (memq 'nelisp-ec-generate-new-buffer flat))
      (should (memq 'nelisp-ec-with-current-buffer flat))
      (should (memq 'nelisp-ec-kill-buffer flat)))))

;;;; L. Nested temp-buffer style buffers stay independent

(ert-deftest emacs-buffer-builtins-test/nested-buffers-preserve-separate-content ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((outer (nelisp-ec-generate-new-buffer "outer"))
          (inner (nelisp-ec-generate-new-buffer "inner")))
      (should (not (eq outer inner)))
      (nelisp-ec-with-current-buffer outer
        (nelisp-ec-insert "outer")
        (should (equal "outer" (nelisp-ec-buffer-string)))
        (nelisp-ec-with-current-buffer inner
          (nelisp-ec-insert "inner")
          (should (equal "inner" (nelisp-ec-buffer-string))))
        (should (equal "outer" (nelisp-ec-buffer-string)))
        (should (eq outer (nelisp-ec-current-buffer)))))))

;;;; M. Phase L1 — get-buffer / get-buffer-create / buffer-list

(defun emacs-buffer-builtins-test--get-buffer (buffer-or-name)
  "Polyfill body of `get-buffer' (= verbatim from emacs-buffer-builtins)."
  (cond
   ((null buffer-or-name) nil)
   ((nelisp-ec-buffer-p buffer-or-name)
    (if (nelisp-ec-buffer-killed-p buffer-or-name)
        nil
      buffer-or-name))
   ((stringp buffer-or-name)
    (cdr (assoc buffer-or-name nelisp-ec--buffers)))
   (t nil)))

(defun emacs-buffer-builtins-test--get-buffer-create (buffer-or-name)
  (or (emacs-buffer-builtins-test--get-buffer buffer-or-name)
      (nelisp-ec-generate-new-buffer
       (cond
        ((stringp buffer-or-name) buffer-or-name)
        ((nelisp-ec-buffer-p buffer-or-name)
         (nelisp-ec-buffer-name buffer-or-name))
        (t " *unnamed*")))))

(defun emacs-buffer-builtins-test--buffer-list ()
  (let ((acc nil))
    (dolist (cell nelisp-ec--buffers)
      (let ((buf (cdr cell)))
        (when (and buf (not (nelisp-ec-buffer-killed-p buf)))
          (setq acc (cons buf acc)))))
    (let ((rev nil))
      (while acc (setq rev (cons (car acc) rev)) (setq acc (cdr acc)))
      rev)))

(ert-deftest emacs-buffer-builtins-test/L1-get-buffer-by-string ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "alpha")))
      (should (eq b (emacs-buffer-builtins-test--get-buffer "alpha")))
      (should (null (emacs-buffer-builtins-test--get-buffer "beta"))))))

(ert-deftest emacs-buffer-builtins-test/L1-get-buffer-by-buffer-passes-live-rejects-killed ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "live")))
      (should (eq b (emacs-buffer-builtins-test--get-buffer b)))
      (nelisp-ec-kill-buffer b)
      (should (null (emacs-buffer-builtins-test--get-buffer b))))))

(ert-deftest emacs-buffer-builtins-test/L1-get-buffer-create-returns-existing-or-creates ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((b1 (emacs-buffer-builtins-test--get-buffer-create "x")))
      (should (nelisp-ec-buffer-p b1))
      ;; Second call returns the same existing buffer.
      (should (eq b1 (emacs-buffer-builtins-test--get-buffer-create "x")))
      ;; Different name creates a fresh buffer.
      (let ((b2 (emacs-buffer-builtins-test--get-buffer-create "y")))
        (should (not (eq b1 b2)))
        (should (nelisp-ec-buffer-p b2))))))

(ert-deftest emacs-buffer-builtins-test/L1-buffer-list-returns-live-only ()
  (emacs-buffer-builtins-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b"))
          (c (nelisp-ec-generate-new-buffer "c")))
      (should (equal 3 (length (emacs-buffer-builtins-test--buffer-list))))
      (nelisp-ec-kill-buffer b)
      (let ((live (emacs-buffer-builtins-test--buffer-list)))
        (should (equal 2 (length live)))
        (should (memq a live))
        (should (memq c live))
        (should-not (memq b live))))))

(ert-deftest emacs-buffer-builtins-test/L1-fboundp-parity ()
  (should (fboundp 'get-buffer))
  (should (fboundp 'get-buffer-create))
  (should (fboundp 'buffer-list)))

;;;; M. Doc 51 Track X audit — keymap-bound polyfills carry interactive form

(defun emacs-buffer-builtins-test--read-defun (file marker)
  "Return the source of the form starting at MARKER (a regexp) in FILE."
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

(ert-deftest emacs-buffer-builtins-test/keymap-bound-cmd-shape-audit ()
  "Doc 51 Track X (2026-05-04) regression: `forward-char' /
`backward-char' / `delete-char' must be wrapper polyfills (= not
plain `defalias' to `nelisp-ec-*') with `(interactive \"p\")', because
the inner `nelisp-ec-delete-char' has a REQUIRED N parameter that
would crash on a no-prefix-arg keymap dispatch (= same lambda-arity
mismatch that bit `delete-backward-char' before its 2026-05-04 fix).

`nelisp-ec-forward-char' / `nelisp-ec-backward-char' have all-optional
arglists, so a plain alias would work today — but the prefix-arg path
needs the wrapper's `(interactive \"p\")' so `C-u 4 C-f' actually moves
4 chars instead of dropping the prefix."
  (let* ((file (locate-library "emacs-buffer-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (let ((s (emacs-buffer-builtins-test--read-defun
              file "(when (emacs-buffer-builtins--install-function-p 'forward-char)")))
      (should s)
      (should (string-match-p "forward-char (&optional n)" s))
      (should (string-match-p "(interactive \"p\")" s))
      ;; Track X EOB-handling: clamps + signals end-of-buffer /
      ;; beginning-of-buffer rather than leaking the underlying
      ;; primitive's `nelisp-ec-args-out-of-range'.
      (should (string-match-p "end-of-buffer" s))
      (should (string-match-p "beginning-of-buffer" s)))
    (let ((s (emacs-buffer-builtins-test--read-defun
              file "(when (emacs-buffer-builtins--install-function-p 'backward-char)")))
      (should s)
      (should (string-match-p "backward-char (&optional n)" s))
      (should (string-match-p "(interactive \"p\")" s))
      ;; Forwarder to forward-char with negated count.
      (should (string-match-p "forward-char" s)))
    (let ((s (emacs-buffer-builtins-test--read-defun
              file "(when (emacs-buffer-builtins--install-function-p 'delete-char)")))
      (should s)
      (should (string-match-p "delete-char (n &optional killflag)" s))
      (should (string-match-p "(interactive \"p\")" s))
      (should (string-match-p "nelisp-ec-delete-char" s)))))

;;;; N. Doc 51 Track X — forward-char / backward-char EOB / BOB semantics

(ert-deftest emacs-buffer-builtins-test/forward-char-source-handles-eob ()
  "Track X (2026-05-04) regression for the user-reported visible
\"nelisp: eval error: args-out-of-range (29 1 28)\" when pressing
<right> at end-of-buffer.

Real Emacs's C `forward-char' clamps to ZV and signals `end-of-buffer'
when target > ZV.  Our polyfill must match — leaking the underlying
`nelisp-ec-args-out-of-range' would bubble into the Layer-1 nelisp
eval-error printer and surface as a noisy console line.

Source-shape test (rather than fboundp dispatch) so the polyfill body
is verified even under host driver where the upstream C `forward-char'
shadows our defun."
  (let* ((file (locate-library "emacs-buffer-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file))
         (s (emacs-buffer-builtins-test--read-defun
             file "(when (emacs-buffer-builtins--install-function-p 'forward-char)"))
         (bs (emacs-buffer-builtins-test--read-defun
              file "(when (emacs-buffer-builtins--install-function-p 'backward-char)"))
         (cf (locate-library "emacs-command-loop"))
         (cf (if (and cf (string-match-p "\\.elc\\'" cf))
                 (concat (substring cf 0 (- (length cf) 1)))
               cf))
         (cl (emacs-buffer-builtins-test--read-defun
              cf "(defun emacs-command-loop-1")))
    (should s) (should bs) (should cl)
    ;; forward-char clamps + signals.
    (should (string-match-p "(signal 'end-of-buffer" s))
    (should (string-match-p "(signal 'beginning-of-buffer" s))
    (should (string-match-p "(nelisp-ec-goto-char hi)" s))
    (should (string-match-p "(nelisp-ec-goto-char lo)" s))
    ;; backward-char delegates to forward-char with negated arg.
    (should (string-match-p "(forward-char (- (or n 1)))" bs))
    ;; command-loop-1 catches both signals.
    (should (string-match-p "(end-of-buffer" cl))
    (should (string-match-p "(beginning-of-buffer" cl))
    (should (string-match-p "\"End of buffer\"" cl))
    (should (string-match-p "\"Beginning of buffer\"" cl))))

(provide 'emacs-buffer-builtins-test)

;;; emacs-buffer-builtins-test.el ends here
