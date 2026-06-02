;;; emacs-search-builtins.el --- Unprefixed regex / search builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.B' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* search/match builtins (=
;; `re-search-forward', `looking-at', `match-data', `match-string',
;; ...) to NeLisp's `nelisp-emacs-compat' (= `nelisp-ec-*') primitives,
;; mirroring the Phase 9 `emacs-buffer-builtins.el' pattern.
;;
;; Why this exists (= Phase 11.A' diagnosis): under standalone NeLisp,
;; the previous nil-stub layer in `emacs-stub.el' would intercept calls
;; to `re-search-forward' etc. before any real impl could be reached.
;; The `nelisp-ec-*' substrate already implements a working buffer-side
;; search, so we just wire the unprefixed names to it.
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with the real search/match substrate.
;;
;; Bridgeable today (substrate present in `nelisp-emacs-compat.el'):
;;
;;   - `re-search-forward' / `re-search-backward' (3-arg substrate;
;;     4th `count' arg is accepted for API parity but ignored — the
;;     callers we care about pass it as nil).
;;   - `search-forward' / `search-backward' (same shape).
;;   - `looking-at' / `looking-at-p'.
;;   - `match-data' / `match-beginning' / `match-end' (= read the most
;;     recent match data set by the `nelisp-ec' search side).
;;   - `match-string' / `match-string-no-properties' (= derived from
;;     `match-beginning' + `match-end' + `buffer-substring' under
;;     `nelisp-ec', or directly from STRING when the optional STRING
;;     argument is supplied — matching Emacs' contract).
;;
;; Phase 4 B (2026-05-06) — un-stub `string-match' /
;; `string-match-p' / `replace-regexp-in-string' so MELPA real
;; packages (= s.el's s-trim, s-replace, etc.) work end-to-end
;; under the nelisp driver.  Strategy: route into `nelisp-rx-*'
;; (= the real regex engine in `nelisp-regex.el') and bridge the
;; plist match-data shape to the integer-list shape Emacs expects
;; via `nelisp-ec--rx-match-data-to-ec' (= Phase 9 helper, base = 0
;; for string matches).  See `string-match' below.
;;
;; Deferred (= still keep the `emacs-stub.el' nil-stubs for now):
;;
;;   - `replace-match': depends on a buffer-modifying replace
;;     primitive that hasn't been ported (= the string variant is
;;     covered by `replace-regexp-in-string').
;;   - `looking-back': no `nelisp-ec-*' impl (= would need bounded
;;     reverse scan).
;;   - `set-match-data' (public form): the L1.5 helper
;;     `nelisp-ec--set-match-data' is internal.
;;
;; Phase 11.B' also deletes the duplicate stubs that this file
;; supersedes from `emacs-stub.el' (= same load-order shadowing risk
;; that Phase 11.A' fixed for the buffer side).

;;; Code:

(require 'nelisp-emacs-compat)
(require 'nelisp-regex)

(defun emacs-search-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

;;;; --- string-match family (Phase 4 B, 2026-05-06) ---------------------

;; Bridge `nelisp-rx-string-match' (= plist return) to the standard
;; `string-match' contract (= integer return + global match-data side
;; effect).  Without this bridge, `s-trim' / `s-replace' / countless
;; MELPA packages silently no-op under the nelisp driver because the
;; pre-existing `emacs-stub.el' polyfill returns nil unconditionally.

(when (emacs-search-builtins--install-function-p 'string-match)
  (defun string-match (regexp string &optional start inhibit-modify)
    "Phase 4 B polyfill: substrate-backed `string-match'.
Calls `nelisp-rx-string-match' for the actual scan.  On match,
returns the integer match-start AND populates the global
match-data registry via `nelisp-ec--rx-match-data-to-ec' so that
subsequent `match-beginning' / `match-end' / `match-string'
calls observe the right boundaries.  When INHIBIT-MODIFY is
non-nil the global match-data is left untouched (Emacs 27+
contract that `string-match-p' relies on)."
    (let* ((s (or start 0))
           (m (nelisp-rx-string-match regexp string s)))
      (cond
       ((null m) nil)
       (t
        (unless inhibit-modify
          (nelisp-ec--rx-match-data-to-ec 0 m))
        (plist-get m :start))))))

(when (emacs-search-builtins--install-function-p 'string-match-p)
  (defun string-match-p (regexp string &optional start)
    "Phase 4 B polyfill: predicate variant of `string-match'.
Returns t / nil and does NOT bump the global match-data registry."
    (and (string-match regexp string start t) t)))

(when (emacs-search-builtins--install-function-p 'replace-match)
  (defun replace-match (newtext &optional fixedcase literal string subexp)
    "Phase 4 B polyfill: substrate-backed `replace-match'.
After a successful `string-match', returns STRING with the matched
range substituted by NEWTEXT.  Without STRING, mutates the current
buffer (deferred — substrate buffer mutation through match-data
isn't wired yet, so we signal in that case).

FIXEDCASE / LITERAL / SUBEXP are accepted for API parity:
  - LITERAL  → backref substitution (`\\1' / `\\\\&') is suppressed
              when non-nil (default in this polyfill is to treat
              NEWTEXT literally, matching MVP).
  - SUBEXP   → integer index of which group's range to replace
              (default = 0, the whole match).
  - FIXEDCASE → ignored (= no case-fold layer)."
    (ignore fixedcase literal)
    (cond
     ((stringp string)
      (let* ((idx (or subexp 0))
             (b (match-beginning idx))
             (e (match-end idx)))
        (cond
         ((and (integerp b) (integerp e))
          (concat (substring string 0 b) newtext (substring string e)))
         (t string))))
     (t
      (signal 'error
              (list "replace-match without STRING needs a buffer-side"
                    "mutator that the substrate has not wired yet"))))))

(when (emacs-search-builtins--install-function-p 'replace-regexp-in-string)
  (defun replace-regexp-in-string
      (regexp rep string &optional fixedcase literal subexp start)
    "Phase 4 B polyfill: substrate-backed `replace-regexp-in-string'.
REGEXP is matched repeatedly in STRING from START (default = 0).
REP can be a string (used as the replacement directly) or a
function (called once per match with the matched substring;
its return value becomes the replacement).  FIXEDCASE / LITERAL /
SUBEXP are accepted for API parity but applied minimally:

  - LITERAL non-nil  → backref substitution is skipped (default).
  - SUBEXP non-nil   → only the SUBEXP-th group is replaced.
  - FIXEDCASE        → ignored (= no case-fold matching layer yet).

Backref expansion (`\\1' etc) is NOT performed in MVP — that
lands with Phase 9c backref groups."
    (ignore fixedcase)
    (let* ((from (or start 0))
           (head (substring string 0 from))
           (tail (substring string from)))
      (cond
       ;; SUBEXP form is rare; punt to the literal-only path for MVP
       ;; and signal cleanly when caller actually exercises it.
       (subexp
        (signal 'error
                (list "replace-regexp-in-string SUBEXP form not yet supported"
                      regexp subexp)))
       ((functionp rep)
        ;; Iteratively scan + replace one match at a time so REP sees
        ;; the matched substring.  Each iteration advances past the
        ;; replaced region (or by 1 char if zero-length match).
        (let ((acc head)
              (cursor 0))
          (while
              (let ((m (nelisp-rx-string-match regexp tail cursor)))
                (cond
                 ((null m)
                  (setq acc (concat acc (substring tail cursor)))
                  nil)
                 (t
                  (let* ((s (plist-get m :start))
                         (e (plist-get m :end))
                         (matched (substring tail s e)))
                    (setq acc (concat acc
                                      (substring tail cursor s)
                                      (funcall rep matched)))
                    (setq cursor (if (= s e) (1+ e) e))
                    t)))))
          acc))
       (t
        ;; Literal replacement.  When LITERAL is nil we still treat REP
        ;; literally in MVP (= backref substitution deferred to 9c).
        (concat head
                (nelisp-rx-replace-all regexp tail
                                       (if literal rep rep))))))))

;;;; --- regex / string-side search ---------------------------------------

(when (emacs-search-builtins--install-function-p 're-search-forward)
  (defun re-search-forward (regexp &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-re-search-forward'.
COUNT (= repeat the search COUNT times) is accepted for API parity
with the host builtin but applied via a simple loop here."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-re-search-forward regexp bound noerror)))
        (setq c (1- c)))
      last)))

(when (emacs-search-builtins--install-function-p 're-search-backward)
  (defun re-search-backward (regexp &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-re-search-backward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-re-search-backward regexp bound noerror)))
        (setq c (1- c)))
      last)))

(when (emacs-search-builtins--install-function-p 'search-forward)
  (defun search-forward (string &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-search-forward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-search-forward string bound noerror)))
        (setq c (1- c)))
      last)))

(when (emacs-search-builtins--install-function-p 'search-backward)
  (defun search-backward (string &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-search-backward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-search-backward string bound noerror)))
        (setq c (1- c)))
      last)))

;;;; --- looking-at family ------------------------------------------------

(when (emacs-search-builtins--install-function-p 'looking-at)
  (defalias 'looking-at #'nelisp-ec-looking-at))

(when (emacs-search-builtins--install-function-p 'looking-at-p)
  (defalias 'looking-at-p #'nelisp-ec-looking-at-p))

;;;; --- match-data accessors --------------------------------------------

(when (emacs-search-builtins--install-function-p 'match-data)
  (defun match-data (&optional integers reuse reseat)
    "Phase 11.B' polyfill: forward to `nelisp-ec-match-data'.
INTEGERS / REUSE / RESEAT are accepted for API parity but ignored —
the L1.5 substrate already returns a plain integer list."
    (ignore integers reuse reseat)
    (nelisp-ec-match-data)))

(when (emacs-search-builtins--install-function-p 'match-beginning)
  (defalias 'match-beginning #'nelisp-ec-match-beginning))

(when (emacs-search-builtins--install-function-p 'match-end)
  (defalias 'match-end #'nelisp-ec-match-end))

(when (emacs-search-builtins--install-function-p 'match-string)
  (defun match-string (num &optional string)
    "Phase 11.B' polyfill for `match-string'.
When STRING is non-nil, slice the matched range out of STRING.
Otherwise read the matched range from the current `nelisp-ec' buffer
via `buffer-substring' (= bridged in Phase 9 to `nelisp-ec-buffer-substring')."
    (let ((b (match-beginning num))
          (e (match-end num)))
      (when (and (integerp b) (integerp e))
        (cond
         ((stringp string)
          (substring string b e))
         (t
          (buffer-substring b e)))))))

(when (emacs-search-builtins--install-function-p 'match-string-no-properties)
  ;; Phase 11.B' MVP: substrate stores no text properties on matches,
  ;; so the no-properties variant is the same body.
  (defalias 'match-string-no-properties #'match-string))

(provide 'emacs-search-builtins)

;;; emacs-search-builtins.el ends here
