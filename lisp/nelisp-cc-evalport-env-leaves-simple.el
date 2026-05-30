;;; nelisp-cc-evalport-env-leaves-simple.el --- Phase 47 env-leaf simple ctx-accessors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.C — SIMPLE env-leaf ctx-accessors.
;;
;; Lowered from packages/nelisp-sys/eval-port/env-leaves-simple.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;;
;; Exports 2 C-ABI symbols deleted by commit fa8932eb:
;;   nl_env_lookup_val(name_ptr, env, out) -> i64
;;   nl_env_pop_frame(env, _pad) -> i64
;;
;; NOTE: nl_env_set_value is provided by evalport-env-leaves-bind.o
;; (the 3-arg FIXED version) to avoid duplicate symbol collision.
;;
;; Linux-x86_64 only — same `:requires-arch x86_64' gate as sibling
;; eval-port entries that use `alloc-bytes' / extern-call ABI.

;;; Code:

(defconst nelisp-cc-evalport-env-leaves-simple--source
  '(seq
    (defun nl_env_lookup_val (name_ptr env out)
      (let ((mirror_ptr (+ env 0))
            (frames_ptr (+ env 32)))
        (nelisp_env_lookup_value mirror_ptr frames_ptr name_ptr out)))
    (defun nl_env_pop_frame (env _pad)
      (let ((frames_ptr (+ env 32))
            (scratch_slot (alloc-bytes 32 8)))
        (nelisp_frame_pop frames_ptr scratch_slot))))
  "Doc 135 Stage 135.C Phase 47 source for simple env-leaf ctx-accessors.

Two-entry `(seq DEFUN ...)' manifest.

Lowered from packages/nelisp-sys/eval-port/env-leaves-simple.nl.
nl_env_set_value omitted (provided by evalport-env-leaves-bind.o).

Exports: nl_env_lookup_val / nl_env_pop_frame.
Net Rust delta: zero.  Resolves 2 undefined symbols.")

(provide 'nelisp-cc-evalport-env-leaves-simple)

;;; nelisp-cc-evalport-env-leaves-simple.el ends here
