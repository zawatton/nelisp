;;; nemacs-runtime-skk.el --- SKK kana-kanji conversion layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; The conversion layer for SKK-style Japanese input on the bridge GUI
;; runtime: a yomi (kana reading) -> a clean list of kanji candidates,
;; looked up in the local SKK CDB dictionary (built host-side once via
;; scripts/skk-jisyo-to-cdb.py).  Sits on top of the buffer-free cdb reader
;; (nemacs-runtime-cdb.el); no network.
;;
;; This is the conversion half of an input method.  The editor side -- the
;; keystroke loop (accumulate a yomi, trigger conversion on a key, show /
;; cycle candidates, insert the choice) -- wires into the bridge's keypress
;; handler and calls `skk-convert' / `skk-convert-first'.
;;
;; Validated against the user's real SKK-JISYO.L (175,774 entries):
;;   (skk-convert "みらい")  => ("未来" "味蕾")
;;   (skk-convert "かんじ")  => ("漢字" "幹事" "監事" "感じ" ...)
;;   (skk-convert-first "にほん") => "日本"

;;; Code:

(defvar skk-cdb-dict-path "/tmp/skk.cdb"
  "Path to the SKK dictionary CDB (build with scripts/skk-jisyo-to-cdb.py).
Point this at the deployed SKK-JISYO.L.cdb.")

(defun skk-convert (yomi)
  "Return the kanji candidate list for YOMI from the SKK CDB, or nil.
Each candidate has its trailing `;annotation' stripped.  Returns nil on a
miss or if the dictionary is unavailable (so callers can fall back)."
  (condition-case nil
      (progn
        (cdb-init skk-cdb-dict-path)
        (let ((raw (cdb-get skk-cdb-dict-path yomi)))
          (when raw
            ;; raw is "/cand1/cand2;annot/.../"; split on `/', drop empties,
            ;; strip the `;annotation' tail each candidate may carry.
            (mapcar (lambda (p) (car (split-string p ";")))
                    (split-string raw "/" t)))))
    (error nil)))

(defun skk-convert-first (yomi)
  "Return the first (most-likely) kanji candidate for YOMI, or nil."
  (car (skk-convert yomi)))

(provide 'nemacs-runtime-skk)

;;; nemacs-runtime-skk.el ends here
