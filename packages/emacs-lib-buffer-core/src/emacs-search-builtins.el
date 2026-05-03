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
;; Each definition is gated on `unless (fboundp ...)' so loading inside
;; a host Emacs is a cheap no-op (= host's C builtins win).
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
;; Deferred (= keep the `emacs-stub.el' nil-stubs for now):
;;
;;   - `string-match' / `string-match-p': substrate is
;;     `nelisp-rx-string-match' but it returns a plist instead of the
;;     integer `match-start' Emacs callers expect, AND does not bump
;;     the global match-data registry the way the unprefixed builtin
;;     does.  Bridging cleanly needs an adapter layer.
;;   - `replace-match' / `replace-regexp-in-string': no `nelisp-ec-*'
;;     impl yet; depend on a buffer-modifying replace primitive that
;;     hasn't been ported.
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

;;;; --- regex / string-side search ---------------------------------------

(unless (fboundp 're-search-forward)
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

(unless (fboundp 're-search-backward)
  (defun re-search-backward (regexp &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-re-search-backward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-re-search-backward regexp bound noerror)))
        (setq c (1- c)))
      last)))

(unless (fboundp 'search-forward)
  (defun search-forward (string &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-search-forward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-search-forward string bound noerror)))
        (setq c (1- c)))
      last)))

(unless (fboundp 'search-backward)
  (defun search-backward (string &optional bound noerror count)
    "Phase 11.B' polyfill: forward to `nelisp-ec-search-backward'."
    (let ((c (or count 1))
          (last nil))
      (while (and (> c 0)
                  (setq last (nelisp-ec-search-backward string bound noerror)))
        (setq c (1- c)))
      last)))

;;;; --- looking-at family ------------------------------------------------

(unless (fboundp 'looking-at)
  (defalias 'looking-at #'nelisp-ec-looking-at))

(unless (fboundp 'looking-at-p)
  (defalias 'looking-at-p #'nelisp-ec-looking-at-p))

;;;; --- match-data accessors --------------------------------------------

(unless (fboundp 'match-data)
  (defun match-data (&optional integers reuse reseat)
    "Phase 11.B' polyfill: forward to `nelisp-ec-match-data'.
INTEGERS / REUSE / RESEAT are accepted for API parity but ignored —
the L1.5 substrate already returns a plain integer list."
    (ignore integers reuse reseat)
    (nelisp-ec-match-data)))

(unless (fboundp 'match-beginning)
  (defalias 'match-beginning #'nelisp-ec-match-beginning))

(unless (fboundp 'match-end)
  (defalias 'match-end #'nelisp-ec-match-end))

(unless (fboundp 'match-string)
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

(unless (fboundp 'match-string-no-properties)
  ;; Phase 11.B' MVP: substrate stores no text properties on matches,
  ;; so the no-properties variant is the same body.
  (defalias 'match-string-no-properties #'match-string))

(provide 'emacs-search-builtins)

;;; emacs-search-builtins.el ends here
