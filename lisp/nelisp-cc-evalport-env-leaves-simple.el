;;; nelisp-cc-evalport-env-leaves-simple.el --- AOT env-leaf simple ctx-accessors  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 135 Stage 135.C — SIMPLE env-leaf ctx-accessors.
;;
;; Originally lowered from packages/nelisp-sys/eval-port/env-leaves-simple.nl via
;; `nelisp-sys-backend-lower-module' targeting x86_64-unknown-linux-gnu.
;; The executable lookup below additionally checks the context's unbound marker.
;;
;; Exports 1 C-ABI symbol deleted by commit fa8932eb:
;;   nl_env_lookup_val(name_ptr, env, out) -> i64
;;
;; NOTE: nl_env_set_value is provided by evalport-env-leaves-bind.o
;; (the 3-arg FIXED version) to avoid duplicate symbol collision.
;;
;; NOTE: nl_env_pop_frame moved OUT to `nelisp-standalone--shim-source' in
;; scripts/nelisp-standalone-build.el (standalone-eval-test link fix,
;; 2026-09-17): its only caller anywhere in the tree,
;; `nelisp_eval_call_root_done', lives in that shared shim source, which the
;; smaller `standalone-eval-test' manifest links but this unit's own
;; "env-leaves-simple.o" entry (reader-only, via
;; `nelisp-standalone--reader-real-sf-manifest') does not reach.  Moving
;; rather than duplicating keeps the reader link (which already resolved
;; `nl_env_pop_frame' from here) at exactly one definition of the symbol,
;; now supplied by shim.o for both builds instead of by this unit for the
;; reader alone.  See that build script's own comment at the new definition
;; for the full reasoning.
;;
;; Linux-x86_64 only — same `:requires-arch x86_64' gate as sibling
;; eval-port entries that use `alloc-bytes' / extern-call ABI.

;;; Code:

(defconst nelisp-cc-evalport-env-leaves-simple--source
  '(seq
    (defun nl_env_lookup_val (name_ptr env out)
      (nl_abi_lookup_to_rc (nl_env_lookup_val_w name_ptr env 0) out))
    ;; Doc 215 word ABI: the value WORD; 2 when NAME is unbound (not found,
    ;; or an entry holding only the env+64 unbound marker); 0 on a signal.
    (defun nl_env_lookup_val_w (name_ptr env _pad)
      ;; GNU eval_sub of a symbol: Fassq over the lexical environment, then the
      ;; symbol's value.  R is the lexical value, 0 for no lexical binding, -1 for
      ;; a shape the loop leaves to the general code.
      (let* ((first -1))
        (seq
         ;; Probe only the newest lexical frame. A miss or any declaration,
         ;; legacy shape, or pair-backed Cell retains the full lookup below.
         (if (= (logand name_ptr 7) 6)
             (let* ((rec (ptr-read-u64 env 40))
                    (depth (sar (ptr-read-u64 rec 24) 2))
                    (backing (ptr-read-u64 rec 16)))
               (if (if (> depth 0) (= (logand backing 7) 6) 0)
                   (let* ((frame (ptr-read-u64 (+ backing 2) (* (- depth 1) 8))))
                     (if (= (logand frame 7) 6)
                         (let* ((hdr (ptr-read-u64 (- frame 6) 0)))
                           (if (if (= (logand hdr 63) 12)
                                   (if (>= hdr 512) (= (logand (ptr-read-u64 frame 18) 1) 1) 1)
                                 0)
                               (let* ((cur (ptr-read-u64 frame 10)))
                                 (while (= (logand cur 7) 2)
                                   (let* ((pair (ptr-read-u64 (- cur 2) 0)))
                                     (seq
                                      (setq cur (ptr-read-u64 cur 6))
                                      (if (= (logand pair 7) 2)
                                          (let* ((key (ptr-read-u64 (- pair 2) 0)))
                                            (if (= key name_ptr)
                                                (let* ((v (ptr-read-u64 pair 6)))
                                                  (seq
                                                   (setq cur 3)
                                                   (setq first
                                                         (if (= (logand v 1) 1) v
                                                           (if (= (logand v 7) 2) v
                                                             (if (= (logand v 7) 6)
                                                                 (let* ((vh (ptr-read-u64 (- v 6) 0)))
                                                                   (if (= (logand vh 63) 11)
                                                                       (if (= (logand vh 256) 0)
                                                                           (ptr-read-u64 v 2) -1)
                                                                     v))
                                                               -1))))))
                                              (if (= (logand key 7) 6) 0 (setq cur 3))))
                                        0)))))
                             0))
                       0))
                 0))
           0)
         (let* ((r (if (= first -1)
                       (extern-call nl_lex_walk (+ env 32) name_ptr 2)
                     first)))
        (if (= r -1)
            (nl_env_lookup_val_done_w
             (nelisp_env_lookup_value_w (+ env 0) (+ env 32) name_ptr 0)
             env 0 0)
          (if (= r 0)
              (let* ((g (extern-call nl_gv_fast env name_ptr)))
                (if (= g -1)
                    (nl_env_lookup_val_done_w
                     (extern-call nl_sym_find_value_w env name_ptr 0)
                     env 0 0)
                  g))
            r))))))
    ;; W is the looked-up value WORD (or 0 / 2 from the lookup); answers 2
    ;; when it is the env+64 unbound marker.
    (defun nl_env_lookup_val_done_w (w env _pad _pad1)
      (if (= (logand w 1) 1) w
        (if (< w 8) w
          (if (= (extern-call nl_uninterned_symbol_equal w (+ env 64)) 1) 2 w))))
    ;; An entry can exist solely for its function cell.  Its value is still
    ;; unbound; never expose that internal marker as a successful lookup.
    ;; Consume the lookup result as a helper argument so it cannot be lost
    ;; across a native call.  Preserve all other lookup failures unchanged.
    ;; Legacy (rc, OUT already holding the value) form of the check above.
    (defun nl_env_lookup_val_done (rc env out _pad)
      ;; OUT already contains the physical value. Check the unbound marker
      ;; in that view and leave all 32 bytes intact; there is no WORD result.
      (if (= rc 0)
          (if (= (extern-call nl_uninterned_symbol_equal out (+ env 64)) 1) 1 0)
        1)))
  "Doc 135 Stage 135.C AOT source for simple env-leaf ctx-accessors.

One public entry and a lookup-result helper in a `(seq DEFUN ...)' manifest.

Lowered from packages/nelisp-sys/eval-port/env-leaves-simple.nl.
nl_env_set_value omitted (provided by evalport-env-leaves-bind.o).
nl_env_pop_frame moved to `nelisp-standalone--shim-source' (see NOTE above).

Exports: nl_env_lookup_val.
Net Rust delta: zero.  Resolves 1 undefined symbol.")

(provide 'nelisp-cc-evalport-env-leaves-simple)

;;; nelisp-cc-evalport-env-leaves-simple.el ends here
