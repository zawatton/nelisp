;;; emacs-cc-character-1.el --- character.c primitives -*- lexical-binding: t; -*-

(unless (fboundp 'char-resolve-modifiers)
  (defun char-resolve-modifiers (char)
    "Resolve modifiers in the character CHAR.
The value is a character with modifiers resolved into the character
code.  Unresolved modifiers are kept in the value."
    (unless (fixnump char)
      (signal 'wrong-type-argument (list 'fixnump char)))
    (let ((control #x4000000) (shift #x2000000) (modifiers #xfc00000))
      ;; Reflect Shift/Control only into ASCII.  Preserve all other modifier
      ;; bits and unresolved modifiers, matching character.c.
      (when (<= (logand char (lognot modifiers)) 127)
        (when (/= 0 (logand char shift))
          (let ((byte (logand char 255)))
            (cond
             ((and (>= byte ?A) (<= byte ?Z))
              (setq char (logand char (lognot shift))))
             ((and (>= byte ?a) (<= byte ?z))
              (setq char (- (logand char (lognot shift)) 32)))
             ((<= (logand char (lognot modifiers)) 32)
              (setq char (logand char (lognot shift)))))))
        (when (/= 0 (logand char control))
          (let ((byte (logand char 255)) (base (logand char 127)))
            (cond
             ((= byte 32) (setq char (logand char (lognot (logior 127 control)))))
             ((= byte 63)
              (setq char (logior 127 (logand char (lognot (logior 127 control))))))
             ((or (and (>= (logand char 95) ?A) (<= (logand char 95) ?Z))
                  (and (>= base ?@) (<= base ?_)))
              (setq char (logand char (logior 31 (lognot (logior 127 control))))))))))
      char)))

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
