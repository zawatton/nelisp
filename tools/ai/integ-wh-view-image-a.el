;;; integ-wh-view-image-a.el --- string views in containers, dump side  -*- lexical-binding: t -*-

;; Run: VIEW_IMAGE_OUT=PATH BIN --load tools/ai/integ-wh-view-image-a.el
;; then:                   BIN --cold-load-from PATH --load tools/ai/integ-wh-view-image-b.el
;;
;; (nelisp--debug-switch 80 STR) answers a cons whose car is the VIEW box of STR (the 32-byte transport form
;; with the canonical header in its capacity word), not the String WORD.  The heap holds such boxes in cons
;; cells when a view is stored without nl_val_clone_into; the image dump used to leave the dumping process's
;; header and sdata addresses in them, and the first collection after a restore read through those addresses
;; (SIGSEGV in nl_gc_mark_legacy_slot) -- the failure of the library's images (case tables, SKK, ccore).
;; The case table at the end is the library's failing shape: a copied standard table with a modified pair.

(defvar viewimg-one (nelisp--debug-switch 80 (concat "view-" "canonical")))
(defvar viewimg-chain
  (let ((a (nelisp--debug-switch 80 (make-string 3 ?q)))
        (b (nelisp--debug-switch 80 (string 960 961)))
        (c (nelisp--debug-switch 80 (concat "sh" "ared"))))
    (setcdr a b) (setcdr b c) a))
(defvar viewimg-pair
  (let ((s (concat "al" "ias")))
    (cons (nelisp--debug-switch 80 s) (nelisp--debug-switch 80 s))))
(defvar viewimg-ctab (copy-case-table (standard-case-table)))
(set-case-syntax-pair ?Q ?z viewimg-ctab)
(set-case-table viewimg-ctab)
(garbage-collect)
(princ (format "VIEW-IMAGE-A codes %S %S %S\n"
               (nelisp--debug-switch 81 viewimg-one) (nelisp--debug-switch 81 viewimg-chain)
               (nelisp--debug-switch 81 (car viewimg-pair))))
(let ((n (nelisp--arena-dump-image-stream (getenv "VIEW_IMAGE_OUT"))))
  (princ (format "VIEW-IMAGE-A dump %S\n" n)))
