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

(defun skk-convert-string (yomi)
  "Newline-joined candidate string for YOMI, or nil on a miss.
This is the shape the bridge's `files--ime-fetch' wants (one candidate per
line), so it can use the local SKK CDB before any network fallback."
  (let ((c (skk-convert yomi)))
    (when c
      (let ((out "") (l c))
        (while l
          (setq out (concat out (car l) "\n"))
          (setq l (cdr l)))
        out))))

;;; --- okuri-ari (送り仮名): verb / adjective conjugation -------------------
;;
;; The okuri-nasi path above handles nouns (みらい -> 未来).  Verbs and
;; adjectives need okuri-ari conversion: the dictionary key is the kana stem
;; plus ONE latin letter = the gojuon COLUMN of the okurigana's first kana
;; (か行 -> k, ら行 -> r, ま行 -> m, が行 -> g ...; a vowel-initial okurigana,
;; as in i-adjectives 高い and う-verbs 買う, uses the vowel itself).  The
;; dictionary value is the bare kanji STEM (かk -> /書/...), so the editor
;; appends the okurigana back: 書 + く = 書く.  Verified against the real
;; SKK-JISYO.L key forms かk/はしr/よm/およg/たかi/かu/たb (godan, ichidan,
;; adjectives all confirmed).
;;
;; Strings here are raw UTF-8 byte vectors (the standalone reader's model), so
;; `substring' is byte-indexed; every hiragana is 3 bytes -> a single kana is a
;; 3-byte slice.  The lookup uses only `equal' (no `assoc' dependency).

(defconst skk-okuri--column
  '(("か" . "k") ("き" . "k") ("く" . "k") ("け" . "k") ("こ" . "k")
    ("が" . "g") ("ぎ" . "g") ("ぐ" . "g") ("げ" . "g") ("ご" . "g")
    ("さ" . "s") ("し" . "s") ("す" . "s") ("せ" . "s") ("そ" . "s")
    ("ざ" . "z") ("じ" . "z") ("ず" . "z") ("ぜ" . "z") ("ぞ" . "z")
    ("た" . "t") ("ち" . "t") ("つ" . "t") ("て" . "t") ("と" . "t")
    ("だ" . "d") ("ぢ" . "d") ("づ" . "d") ("で" . "d") ("ど" . "d")
    ("な" . "n") ("に" . "n") ("ぬ" . "n") ("ね" . "n") ("の" . "n")
    ("は" . "h") ("ひ" . "h") ("ふ" . "h") ("へ" . "h") ("ほ" . "h")
    ("ば" . "b") ("び" . "b") ("ぶ" . "b") ("べ" . "b") ("ぼ" . "b")
    ("ぱ" . "p") ("ぴ" . "p") ("ぷ" . "p") ("ぺ" . "p") ("ぽ" . "p")
    ("ま" . "m") ("み" . "m") ("む" . "m") ("め" . "m") ("も" . "m")
    ("や" . "y") ("ゆ" . "y") ("よ" . "y")
    ("ら" . "r") ("り" . "r") ("る" . "r") ("れ" . "r") ("ろ" . "r")
    ("わ" . "w") ("を" . "w") ("ん" . "n")
    ("あ" . "a") ("い" . "i") ("う" . "u") ("え" . "e") ("お" . "o"))
  "First-okurigana-kana -> SKK okuri key letter (gojuon column; vowels self).")

(defun skk-okuri--lookup (kana)
  "Okuri key letter for the single KANA string, or nil.  Uses only `equal'."
  (let ((l skk-okuri--column) (r nil))
    (while (and l (not r))
      (when (equal (car (car l)) kana) (setq r (cdr (car l))))
      (setq l (cdr l)))
    r))

(defun skk-okuri-letter (okurigana)
  "The SKK okuri key letter for OKURIGANA (a kana string), or nil.
Keys on the first kana = the first 3 UTF-8 bytes (byte-string model)."
  (when (and okurigana (> (length okurigana) 2))
    (skk-okuri--lookup (substring okurigana 0 3))))

(defun skk-convert-okuri (yomi okurigana)
  "Kanji candidates for verb/adjective YOMI conjugated with OKURIGANA.
E.g. (skk-convert-okuri \"か\" \"く\") => (\"書く\" \"描く\" ...);
(skk-convert-okuri \"はし\" \"る\") => (\"走る\" ...).  Looks up the okuri-ari
key (YOMI + the okurigana's column letter) and appends OKURIGANA to each
kanji stem.  nil on a miss / no dictionary, so callers can fall back."
  (let ((letter (skk-okuri-letter okurigana)))
    (when letter
      (let ((stems (skk-convert (concat yomi letter))))
        (when stems
          (mapcar (lambda (st) (concat st okurigana)) stems))))))

(defun skk-convert-okuri-first (yomi okurigana)
  "First (most-likely) conjugated candidate for YOMI + OKURIGANA, or nil."
  (car (skk-convert-okuri yomi okurigana)))

(defun skk-convert-okuri-string (yomi okurigana)
  "Newline-joined okuri-ari candidates, the shape `files--ime-fetch' wants."
  (let ((c (skk-convert-okuri yomi okurigana)))
    (when c
      (let ((out "") (l c))
        (while l (setq out (concat out (car l) "\n")) (setq l (cdr l)))
        out))))

(defun skk-convert-auto (reading)
  "Convert READING with no SKK shift-key marking, newline-joined candidates.
Merges two candidate sources so the editor's SPC-cycle can reach either:
  1. the whole-word okuri-nasi (noun) reading (`skk-convert-string'), and
  2. auto okuri-ari -- the LAST kana of READING taken as okurigana and the
     rest as the verb / adjective stem (`skk-convert-okuri-string').
A bare conjugated reading is inherently ambiguous without the shift marker
\(かく = 核 the noun OR 書く the verb; たかい = 他界 OR 高い), so both are
offered, okuri-nasi first.  はしる -> 走る (no noun reading); かく -> 核 then
書く 描く ...; たかい -> 他界 then 高い.  Returns nil if nothing matches."
  (let ((nasi (skk-convert-string reading))
        (ari (when (> (length reading) 5)   ; need stem (>=3B) + okuri (>=3B)
               (let* ((n (length reading))
                      (oku (substring reading (- n 3) n))
                      (stem (substring reading 0 (- n 3))))
                 (skk-convert-okuri-string stem oku)))))
    (cond ((and nasi ari) (concat nasi ari))
          (nasi nasi)
          (ari ari)
          (t nil))))

(provide 'nemacs-runtime-skk)

;;; nemacs-runtime-skk.el ends here
