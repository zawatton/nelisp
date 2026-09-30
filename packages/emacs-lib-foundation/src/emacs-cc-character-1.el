;;; emacs-cc-character-1.el --- character.c primitives -*- lexical-binding: t; -*-

(unless (fboundp 'char-resolve-modifiers)
  (defun char-resolve-modifiers (char)
    "Resolve modifiers in the character CHAR.
The value is a character with modifiers resolved into the character
code.  Unresolved modifiers are kept in the value."
    (unless (integerp char)
      (signal 'wrong-type-argument (list 'fixnump char)))
    (let* ((control #x0400000)
           (shift #x2000000)
           (meta #x0800000)
           (bits (logand char (logior control shift meta)))
           (base (logand char (lognot (logior control shift)))))
      (when (/= 0 (logand bits shift))
        (setq base (upcase base)))
      (when (/= 0 (logand bits control))
        (setq base (cond ((= base ?\s) 0) ((= base ??) 127)
                         ((and (>= base ?@) (<= base ?_))
                          (logand base 31))
                         ((and (>= base ?a) (<= base ?z))
                          (logand base 31))
                         (t base))))
      (logior base (logand bits meta)))))

(unless (fboundp 'get-byte)
  (defun get-byte (&optional position string)
    "Return a byte value of a character at point.
Optional 1st arg POSITION, if non-nil, is a position of a character to get
 a byte value.
Optional 2nd arg STRING, if non-nil, is a string of which first
character is a target to get a byte value.  In this case, POSITION, if
non-nil, is an index of a target character in the string.

If the current buffer (or STRING) is multibyte, and the target
character is not ASCII nor 8-bit character, an error is signaled."
    (if string
        (let* ((index (or position 0))
               (char (aref string index)))
          (if (multibyte-string-p string)
              (emacs-cc-character-1--byte-from-char char)
            char))
      (let* ((pos (or position (point)))
             (char (char-after pos)))
        (if enable-multibyte-characters
            (emacs-cc-character-1--byte-from-char char)
          char)))))

(defun emacs-cc-character-1--byte-from-char (char)
  "Return CHAR as a byte, signaling when it has no byte representation."
  (let ((byte (multibyte-char-to-unibyte char)))
    (if (< byte 0)
        (error "Not an ASCII nor an 8-bit character: %d" char)
      byte)))

(provide 'emacs-cc-character-1)
