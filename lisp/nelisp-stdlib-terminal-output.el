;;; nelisp-stdlib-terminal-output.el --- Standalone UTF-8 output device -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; The standalone has one implicit non-graphical frame and a byte-output
;; device, not a font backend or a terminal glyph-code protocol.  Its native
;; character constructor encodes UTF-8 (nl_u8_encode), and princ passes those
;; bytes to the native stdout writer.  Installation authenticates that runtime
;; and checks its constructor representation.  No terminal is inferred from
;; locale variables, public version metadata, or a package feature.

(declare-function nelisp-bytecode-compiler-input-dialect
                  "nelisp-bytecode-compiler-input")
(defvar nelisp--terminal-output-device nil)

(defun nelisp--terminal-output-live-state ()
  "Return the authenticated output state, or fail if its owner changed."
  (let ((state nelisp--terminal-output-device))
    (unless (and state
                 (eq (plist-get state :coding) 'utf-8-unix)
                 (null (plist-get state :glyph-protocol))
                 (eq (plist-get state :constructor) (symbol-function 'char-to-string))
                 (eq (plist-get state :writer) (symbol-function 'nelisp--write-stdout-bytes))
                 (eq (plist-get state :printer) (symbol-function 'nelisp--native-princ))
                 (null (selected-frame))
                 (not (display-graphic-p)))
      (error "Standalone output device is unavailable"))
    state))

(defun nelisp--terminal-output-character (character)
  "Validate an Emacs character, including its extended internal range."
  (unless (and (integerp character) (<= 0 character) (<= character #x3fffff))
    (signal 'wrong-type-argument (list 'characterp character))))

(defun nelisp--terminal-output-font (position &optional character)
  "Implement the non-graphical, no-glyph-protocol branch of internal-char-font."
  (if (null position)
      (nelisp--terminal-output-character character)
    (unless (or (integerp position) (markerp position))
      (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
    (let ((pos (if (markerp position) (marker-position position) position)))
      (unless pos (error "Marker does not point anywhere"))
      (unless (and (<= (point-min) pos) (< pos (point-max)))
        (signal 'args-out-of-range (list position (point-min) (point-max)))))
    (when character
      (unless (and (integerp character) (>= character 0))
        (signal 'wrong-type-argument (list 'wholenump character)))))
  (nelisp--terminal-output-live-state)
  ;; The actual device reports bytes; it has no glyph-code map or font.
  nil)

(defun nelisp--terminal-output-coding (&optional terminal)
  "Read coding from the existing implicit frame's output device."
  (when terminal
    (signal 'wrong-type-argument (list 'terminal-live-p terminal)))
  (plist-get (nelisp--terminal-output-live-state) :coding))

(defun nelisp--terminal-output-coding-property (coding property)
  "Read the supported output codec's actual character repertoire."
  (unless (symbolp coding)
    (signal 'wrong-type-argument (list 'symbolp coding)))
  (unless (memq coding '(nil utf-8 utf-8-unix))
    (signal 'coding-system-error (list coding)))
  (nelisp--terminal-output-live-state)
  (cond ((null coding) nil)
        ((eq property :charset-list) '(unicode))
        ((eq property :name) 'utf-8)))

(defun nelisp--terminal-output-encode-character (character charset)
  "Map a character into the output device's Unicode repertoire."
  (nelisp--terminal-output-character character)
  (unless (eq charset 'unicode)
    (signal 'wrong-type-argument (list 'charsetp charset)))
  (nelisp--terminal-output-live-state)
  (and (<= character #x10ffff) character))

;;;###autoload
(defun nelisp-stdlib-terminal-output-install ()
  "Install the authenticated standalone output owner, preserving GNU hosts.
Only the existing UTF-8 output device is modeled.  Other coding systems and
charsets require their genuine providers; this does not create a GUI or
attest package acceptance."
  (if (and (fboundp 'internal-char-font) (fboundp 'terminal-coding-system))
      'preserved
    (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
      (unless (and (eq (plist-get evidence :status) 'pinned)
                   (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified)
                   (fboundp 'nelisp--write-stdout-bytes)
                   (fboundp 'nelisp--native-princ)
                   (subrp (symbol-function 'char-to-string))
                   (subrp (symbol-function 'nelisp--write-stdout-bytes))
                   (null (selected-frame)) (not (display-graphic-p))
                   (equal (string-as-unibyte
                           (concat (char-to-string 8593) (char-to-string 955)
                                   (char-to-string 128512)))
                          (unibyte-string 226 134 145 206 187 240 159 152 128)))
        (error "Standalone UTF-8 output evidence is unavailable"))
      (setq nelisp--terminal-output-device
            (list :coding 'utf-8-unix :glyph-protocol nil
                  :constructor (symbol-function 'char-to-string)
                  :writer (symbol-function 'nelisp--write-stdout-bytes)
                  :printer (symbol-function 'nelisp--native-princ)))
      (dolist (entry '((internal-char-font . nelisp--terminal-output-font)
                       (terminal-coding-system . nelisp--terminal-output-coding)
                       (coding-system-get . nelisp--terminal-output-coding-property)
                       (encode-char . nelisp--terminal-output-encode-character)))
        (unless (fboundp (car entry)) (fset (car entry) (symbol-function (cdr entry)))))
      'installed)))

(provide 'nelisp-stdlib-terminal-output)
;;; nelisp-stdlib-terminal-output.el ends here
