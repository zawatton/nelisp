;;; nelisp-cc-sf-unwind-protect.el --- AOT nl_sf_unwind_protect swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The protected result stays rooted while the cleanup forms run. An exit
;; from the body stays rooted separately while cleanups run with an empty
;; exit stash. A cleanup exit replaces it, as in GNU's unbind_to.

;;; Code:

(defconst nelisp-cc-sf-unwind-protect--source
  '(seq

    (defun nl_sf_uw_cleanup_w (cleanup env cursor pending)
      ;; Root the remaining list: a cleanup may mutate the source form and
      ;; collect. Discard its result on the word ABI without a scratch view.
      (let* ((w 3))
        (seq
         (while (if (= w 0) 0 (= (sexp-tag cleanup) 7))
           (let* ((form (nl_cons_car_word cleanup)))
             (seq
              (setq cleanup (nl_cons_cdr_word cleanup))
              (ptr-write-u64 cursor 0 cleanup)
              ;; A normally returning internal operation can leave a stale
              ;; flag. Each cleanup of a pending exit starts with it clear.
              (if (= pending 0) 0 (ptr-write-u64 268435472 0 0))
              (setq w (nelisp_eval_call_w form env 0))
              (setq cleanup (ptr-read-u64 cursor 0)))))
         w)))

    (defun nl_sf_uw_cleanup_pending_w (cleanup env cursor)
      (let* ((flag (heap-read-u64 268435472 0))
             (mark (nl_root_mark env))
             (saved (nl_exit_save env)))
        (if (= saved 0)
            (seq (nl_root_release env mark) (wf_bytecode_root_exhausted))
          (seq
           (if (= (nl_sf_uw_cleanup_w cleanup env cursor 1) 0) 0
             (seq (nl_exit_restore saved)
                  (ptr-write-u64 268435472 0 flag)))
           (nl_root_release env mark)
           1))))

    (defun nl_sf_uw_body_w (args env vslot _pad)
      (let* ((w (nelisp_eval_call_w (nl_cons_car_word args) env 0))
             (cleanup (nl_cons_cdr_word args)))
        (if (= (sexp-tag cleanup) 0) w
          (if (= w 0)
              (seq (nl_sf_uw_cleanup_pending_w cleanup env (+ vslot 8)) 0)
            (seq
             (ptr-write-u64 vslot 0 w)
             (if (= (nl_sf_uw_cleanup_w cleanup env (+ vslot 8) 0) 0) 0
               (ptr-read-u64 vslot 0)))))))

    (defun nl_sf_unwind_protect_w (args env _pad _pad1)
      (if (= (sexp-tag args) 0)
          (nl_abi_fail 1 0)
        (let* ((mark (nl_root_mark env)) (value (nl_root_frame env 2)))
          (if (= value 0)
              (seq (nl_root_release env mark) (nl_abi_fail 1 0))
            (let* ((w (nl_sf_uw_body_w args env value 0)))
              (seq (nl_root_release env mark) w))))))

    ;; (rc, OUT) adapter.
    (defun nl_sf_unwind_protect (args env out _pad)
      (nl_abi_word_to_rc (nl_sf_unwind_protect_w args env 0 0) out)))

  "Word evaluator entries for the unwind-protect special form.")

(provide 'nelisp-cc-sf-unwind-protect)

;;; nelisp-cc-sf-unwind-protect.el ends here
