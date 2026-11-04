;;; emoji-labels.el --- lightweight emoji label data  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Small pure-Elisp substitute for Emacs' generated emoji-labels file.
;; The full vendor file contains thousands of glyph/name entries; daily
;; startup only needs a valid data shape for `emoji.el' and a small useful
;; representative set until the generated table can be baked into an image.

;;; Code:

(defun emoji-labels--char (codepoint)
  "Return a one-character string for CODEPOINT."
  (string codepoint))

(defun emoji-labels--hash (&rest pairs)
  "Return an equal-test hash table populated from PAIRS."
  (let ((table (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defconst emoji-labels--grinning (emoji-labels--char #x1f600))
(defconst emoji-labels--smiley (emoji-labels--char #x1f603))
(defconst emoji-labels--joy (emoji-labels--char #x1f602))
(defconst emoji-labels--slight-smile (emoji-labels--char #x1f642))
(defconst emoji-labels--heart (emoji-labels--char #x2764))
(defconst emoji-labels--thumbs-up (emoji-labels--char #x1f44d))
(defconst emoji-labels--party (emoji-labels--char #x1f389))
(defconst emoji-labels--check (emoji-labels--char #x2705))
(defconst emoji-labels--warning (emoji-labels--char #x26a0))
(defconst emoji-labels--rocket (emoji-labels--char #x1f680))

(defconst emoji--labels
  `(("Smileys"
     ("smiling" ,emoji-labels--grinning ,emoji-labels--smiley
      ,emoji-labels--slight-smile)
     ("amused" ,emoji-labels--joy))
    ("Emotion" ,emoji-labels--heart)
    ("Body" ("hand" ,emoji-labels--thumbs-up))
    ("Activities" ("event" ,emoji-labels--party))
    ("Symbols"
     ("warning" ,emoji-labels--warning)
     ("other-symbol" ,emoji-labels--check))
    ("Travel & Places" ("transport-air" ,emoji-labels--rocket)))
  "Lightweight emoji label hierarchy.")

(defconst emoji--derived
  (emoji-labels--hash emoji-labels--thumbs-up
                      (list (concat emoji-labels--thumbs-up
                                    (emoji-labels--char #x1f3fb))
                            (concat emoji-labels--thumbs-up
                                    (emoji-labels--char #x1f3fd))
                            (concat emoji-labels--thumbs-up
                                    (emoji-labels--char #x1f3ff))))
  "Lightweight mapping from base emoji glyphs to derived variants.")

(defconst emoji--names
  (emoji-labels--hash
   emoji-labels--grinning "grinning face"
   emoji-labels--smiley "grinning face with big eyes"
   emoji-labels--joy "face with tears of joy"
   emoji-labels--slight-smile "slightly smiling face"
   emoji-labels--heart "red heart"
   emoji-labels--thumbs-up "thumbs up"
   emoji-labels--party "party popper"
   emoji-labels--check "check mark button"
   emoji-labels--warning "warning"
   emoji-labels--rocket "rocket")
  "Lightweight mapping from emoji glyphs to Unicode names.")

(provide 'emoji-labels)

;;; emoji-labels.el ends here
