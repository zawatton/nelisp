;;; nc5-gencheck.el --- nc5 S2: the generated nc_ID / nc_d_ID shape  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/nc5-gencheck.el
;; Every row's activation enters through `nc_enter' (no `nl_root_reserve'),
;; takes the root mark and release inline on the main thread (the helper
;; calls only in the registry branch), and every direct-call wrapper has the
;; guard, the record push and the depth limit inline (no `nc_site_rec',
;; `nl_n4_push_words' or unconditional `nl_eval_depth_limit' call).
(require 'nelisp-cc-fxir-native)
(let ((rows (nelisp-cc-fxir-native--rows)) (bad 0) (n 0) (w 0))
  (dolist (r rows)
    (let* ((d (prin1-to-string (nelisp-cc-fxir-native--defun r)))
           (x (prin1-to-string (nelisp-cc-fxir-native--wrapper r))))
      (setq n (1+ n))
      (unless (and (not (string-match-p "(nl_root_reserve env)" d))
                   (string-match-p "(nl_root_mark env)))" d)
                   (string-match-p "(nl_root_release env mark))" d)
                   (not (string-match-p "(mark (nl_root_mark env))" d))
                   (or (not (string-match-p "(nc_enter env" d))
                       (string-match-p "(setq mem (nc_enter env [0-9]+)) (if (= mem 0) (setq st -4) (setq oslot (\\+ mem [0-9]+)))" d)))
        (setq bad (1+ bad)) (princ (format "BAD-DEFUN %s\n" (car r))))
      (setq w (1+ w))
      ;; Build 1's lesson: the String-aware `heap-read-u64' at offsets 0-24 on
      ;; a plain variable is an `nl_string_read64' call; the inline forms use
      ;; raw loads.
      (unless (and (not (string-match-p "(heap-read-u64 [a-z]+ \\(0\\|8\\|16\\|24\\))" x))
                   (not (string-match-p "nc_site_rec\\|(nl_n4_push_words env" x))
                   (string-match-p "(nl_n4_push_words_cold env bfn stk argc)" x)
                   (string-match-p "(if (< (\\+ depth 2) 100) 100 (nl_eval_depth_limit env (\\+ depth 2)))" x))
        (setq bad (1+ bad)) (princ (format "BAD-WRAPPER %s\n" (car r))))))
  (princ (format "GENCHECK rows=%d wrappers=%d bad=%d\n" n w bad))
  (kill-emacs (if (and (> n 100) (= bad 0)) 0 1)))
