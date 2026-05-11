;;; nelisp-cc-spike-noop.el --- Doc 99 §99.B C-callable spike  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 99 §99.B spike — minimum elisp source that compiles to a
;; C-callable function via the Phase 47 chain.  The function returns
;; the integer 42 unconditionally so the Rust probe (= `cargo test')
;; can prove the end-to-end build pipeline:
;;
;;   1. `scripts/compile-elisp-objects.el' invokes
;;      `nelisp-phase47-compile-to-object' on this source → ET_REL .o.
;;   2. `build-tool/build.rs' bundles the .o into a static archive
;;      and tells cargo to link the final `nelisp' binary against it.
;;   3. Rust declares `extern "C" fn nelisp_spike_noop() -> i64;'
;;      and a probe test asserts the return value is 42.
;;
;; The Phase 47 v1 grammar does not accept docstrings or multi-form
;; bodies (= Doc 97 §1), so the source below is a single Elisp-style
;; `(defun NAME () BODY)' form.  The symbol name uses underscores so
;; the resulting STT_FUNC entry is byte-compatible with the C name
;; `nelisp_spike_noop' that the Rust extern declaration mangles to.

;;; Code:

(defconst nelisp-cc-spike-noop--source
  '(defun nelisp_spike_noop () 42)
  "The §99.B spike source — the one form `compile-to-object' consumes.
Kept as a defconst so the build orchestrator can `require' this file
and read the canonical source without re-parsing a comment.")

(provide 'nelisp-cc-spike-noop)

;;; nelisp-cc-spike-noop.el ends here
