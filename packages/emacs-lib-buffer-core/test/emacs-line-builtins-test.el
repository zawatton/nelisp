;;; emacs-line-builtins-test.el --- ERT for emacs-line-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 line / column derivation module.  Under host
;; Emacs the `unless (fboundp ...)' gates skip the polyfills, so the
;; public unprefixed symbols stay bound to host C builtins that have
;; no awareness of the `nelisp-ec' substrate.  Therefore behavioural
;; tests run against:
;;
;;   (a) The two ungated helpers — `emacs-line--bol-pos' /
;;       `emacs-line--eol-pos' — which always exercise the L2 logic.
;;   (b) Synthetic copies of the polyfill bodies invoked as lambdas
;;       referring to the substrate, so we pin the polyfill's
;;       semantics regardless of host vs. standalone NeLisp.
;;
;; Featurep / fboundp parity is checked separately.

;;; Code:

(require 'ert)
(require 'emacs-line-builtins)
(require 'cl-lib)

(defmacro emacs-line-builtins-test--with-fresh-buffer (text &rest body)
  "Run BODY against a fresh nelisp-ec buffer pre-filled with TEXT."
  (declare (indent 1) (debug (form body)))
  (let ((buf (make-symbol "buf")))
    `(let ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (nelisp-ec--match-data nil))
       (let ((,buf (nelisp-ec-generate-new-buffer "line")))
         (unwind-protect
             (nelisp-ec-with-current-buffer ,buf
               (nelisp-ec-insert ,text)
               (nelisp-ec-goto-char (nelisp-ec-point-min))
               ,@body)
           (nelisp-ec-kill-buffer ,buf))))))

;;;; Polyfill body lambdas (= literal copies of `emacs-line-builtins.el'
;;;; gated forms, so we can exercise the substrate path even when the
;;;; host's C builtin shadows the unprefixed name).

(defvar emacs-line-builtins-test--bobp
  (lambda () (= (nelisp-ec-point) (nelisp-ec-point-min))))
(defvar emacs-line-builtins-test--eobp
  (lambda () (= (nelisp-ec-point) (nelisp-ec-point-max))))
(defvar emacs-line-builtins-test--bolp
  (lambda ()
    (or (funcall emacs-line-builtins-test--bobp)
        (let* ((pt (nelisp-ec-point))
               (s (nelisp-ec-buffer-substring (- pt 1) pt)))
          (and (> (length s) 0) (eq (aref s 0) ?\n))))))
(defvar emacs-line-builtins-test--eolp
  (lambda ()
    (or (funcall emacs-line-builtins-test--eobp)
        (let* ((pt (nelisp-ec-point))
               (s (nelisp-ec-buffer-substring pt (+ pt 1))))
          (and (> (length s) 0) (eq (aref s 0) ?\n))))))

;;;; A. Load cleanly + fboundp parity

(ert-deftest emacs-line-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-line-builtins))
  (dolist (sym '(bobp eobp bolp eolp
                 line-beginning-position line-end-position
                 beginning-of-line end-of-line
                 forward-line line-number-at-pos))
    (should (fboundp sym)))
  (should (fboundp 'emacs-line--bol-pos))
  (should (fboundp 'emacs-line--eol-pos)))

;;;; B. emacs-line--bol-pos / --eol-pos helper (= ungated, always runs L2 body)

(ert-deftest emacs-line-builtins-test/bol-eol-helpers-find-line-bounds ()
  (emacs-line-builtins-test--with-fresh-buffer "alpha\nbeta\ngamma"
    ;; "alpha\nbeta\ngamma" — 1-indexed positions:
    ;; a=1 l=2 p=3 h=4 a=5 \n=6 b=7 e=8 t=9 a=10 \n=11 g=12 a=13 m=14 m=15 a=16
    (should (= 1 (emacs-line--bol-pos 1)))
    (should (= 1 (emacs-line--bol-pos 3)))
    (should (= 6 (emacs-line--eol-pos 3)))
    (should (= 7 (emacs-line--bol-pos 8)))
    (should (= 11 (emacs-line--eol-pos 8)))
    (should (= 12 (emacs-line--bol-pos 14)))
    (should (= 17 (emacs-line--eol-pos 14)))))

;;;; C. Polyfill body bobp / eobp via lambda (= bypasses host shadow)

(ert-deftest emacs-line-builtins-test/polyfill-bobp-eobp-on-substrate ()
  (emacs-line-builtins-test--with-fresh-buffer "alpha\nbeta"
    (nelisp-ec-goto-char (nelisp-ec-point-min))
    (should (funcall emacs-line-builtins-test--bobp))
    (should-not (funcall emacs-line-builtins-test--eobp))
    (nelisp-ec-goto-char (nelisp-ec-point-max))
    (should-not (funcall emacs-line-builtins-test--bobp))
    (should (funcall emacs-line-builtins-test--eobp))
    (nelisp-ec-goto-char 5)
    (should-not (funcall emacs-line-builtins-test--bobp))
    (should-not (funcall emacs-line-builtins-test--eobp))))

;;;; D. Polyfill body bolp / eolp via lambda

(ert-deftest emacs-line-builtins-test/polyfill-bolp-eolp-on-substrate ()
  (emacs-line-builtins-test--with-fresh-buffer "ab\ncd"
    (nelisp-ec-goto-char 1) ; before 'a'
    (should (funcall emacs-line-builtins-test--bolp))
    (should-not (funcall emacs-line-builtins-test--eolp))
    (nelisp-ec-goto-char 3) ; before '\n'
    (should-not (funcall emacs-line-builtins-test--bolp))
    (should (funcall emacs-line-builtins-test--eolp))
    (nelisp-ec-goto-char 4) ; before 'c' (= after \n)
    (should (funcall emacs-line-builtins-test--bolp))
    (should-not (funcall emacs-line-builtins-test--eolp))
    (nelisp-ec-goto-char 6) ; eob
    (should-not (funcall emacs-line-builtins-test--bolp))
    (should (funcall emacs-line-builtins-test--eolp))))

;;;; E. line-beginning-position / line-end-position via emacs-line-- helpers

(ert-deftest emacs-line-builtins-test/line-pos-via-helpers-no-N ()
  (emacs-line-builtins-test--with-fresh-buffer "alpha\nbeta\ngamma"
    (nelisp-ec-goto-char 8) ; in "beta"
    (should (= 7 (emacs-line--bol-pos)))
    (should (= 11 (emacs-line--eol-pos)))
    ;; Helpers don't move point.
    (should (= 8 (nelisp-ec-point)))))

;;;; F. Synthetic forward-line behaviour

(ert-deftest emacs-line-builtins-test/synthetic-forward-line-positive ()
  (emacs-line-builtins-test--with-fresh-buffer "a\nb\nc\nd"
    (nelisp-ec-goto-char (nelisp-ec-point-min))
    (let ((fwd-impl
           (lambda (n)
             (let ((c n) (done nil))
               (while (and (> c 0) (not done))
                 (let ((em (nelisp-ec-point-max)))
                   (if (= (nelisp-ec-point) em)
                       (setq done t)
                     (let ((eol (emacs-line--eol-pos)))
                       (cond
                        ((< eol em)
                         (nelisp-ec-goto-char (+ eol 1))
                         (setq c (- c 1)))
                        (t
                         (nelisp-ec-goto-char em)
                         (setq c (- c 1))))))))
               c))))
      (should (= 0 (funcall fwd-impl 1)))
      (should (= 3 (nelisp-ec-point))) ; start of "b"
      (should (= 0 (funcall fwd-impl 2)))
      (should (= 7 (nelisp-ec-point))) ; start of "d"
      ;; From "d" (line 4) requesting 5 forward: only 1 advance possible
      ;; (= move to EOB), so 4 remain.
      (should (= 4 (funcall fwd-impl 5)))
      (should (= 8 (nelisp-ec-point))))))

(ert-deftest emacs-line-builtins-test/synthetic-line-number-at-pos ()
  (emacs-line-builtins-test--with-fresh-buffer "alpha\nbeta\ngamma\ndelta"
    (let ((line-num
           (lambda (p)
             (let* ((bm (nelisp-ec-point-min))
                    (s (nelisp-ec-buffer-substring bm p))
                    (n (length s))
                    (i 0)
                    (count 1))
               (while (< i n)
                 (when (eq (aref s i) ?\n)
                   (setq count (+ count 1)))
                 (setq i (+ i 1)))
               count))))
      (should (= 1 (funcall line-num 1)))
      (should (= 1 (funcall line-num 5)))
      (should (= 2 (funcall line-num 7)))
      (should (= 3 (funcall line-num 13)))
      (should (= 4 (funcall line-num 18))))))

;;;; G. Helper out-of-range behaviour

(ert-deftest emacs-line-builtins-test/helpers-clamp-at-buffer-bounds ()
  (emacs-line-builtins-test--with-fresh-buffer "x"
    (should (= 1 (emacs-line--bol-pos 1)))
    (should (= 2 (emacs-line--eol-pos 1)))
    (should (= 1 (emacs-line--bol-pos 0)))   ; below point-min clamps
    (should (= 2 (emacs-line--eol-pos 5))))) ; beyond point-max clamps

;;;; H. Empty buffer

(ert-deftest emacs-line-builtins-test/helpers-empty-buffer ()
  (emacs-line-builtins-test--with-fresh-buffer ""
    (should (= 1 (emacs-line--bol-pos)))
    (should (= 1 (emacs-line--eol-pos)))))

;;;; I. Idempotence

(ert-deftest emacs-line-builtins-test/require-is-idempotent ()
  (let ((before-bol (symbol-function 'emacs-line--bol-pos))
        (before-eol (symbol-function 'emacs-line--eol-pos)))
    (require 'emacs-line-builtins)
    (should (eq before-bol (symbol-function 'emacs-line--bol-pos)))
    (should (eq before-eol (symbol-function 'emacs-line--eol-pos)))))

;;;; J. Doc 51 Track X audit — every keymap-bound cmd has interactive form

(defun emacs-line-builtins-test--read-defun (file marker)
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

(ert-deftest emacs-line-builtins-test/keymap-bound-cmd-shape-audit ()
  "Doc 51 Track X (2026-05-04) audit: motion commands bound in
`nemacs-main-keymap' (= beginning-of-line / end-of-line / next-line /
previous-line) must carry `(interactive \"p\")' so prefix-arg flows
through and `call-interactively' produces a well-formed arg list.

Without the form, dispatch builds 0 args; the polyfills already accept
all-optional arglists so a no-arg call works, but the prefix-arg path
silently drops arguments, breaking `C-u 4 C-n' style motion."
  (let* ((file (locate-library "emacs-line-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (dolist (spec '(("(unless (fboundp 'beginning-of-line)"
                     "beginning-of-line (&optional n)" "p")
                    ("(unless (fboundp 'end-of-line)"
                     "end-of-line (&optional n)" "p")
                    ("(unless (fboundp 'next-line)"
                     "next-line (&optional n _try-vscroll)" "p")
                    ("(unless (fboundp 'previous-line)"
                     "previous-line (&optional n _try-vscroll)" "p")))
      (let ((s (emacs-line-builtins-test--read-defun file (nth 0 spec))))
        (should s)
        (should (string-match-p (regexp-quote (nth 1 spec)) s))
        (should (string-match-p
                 (concat "(interactive \"" (nth 2 spec) "\")") s))))))

(provide 'emacs-line-builtins-test)

;;; emacs-line-builtins-test.el ends here
