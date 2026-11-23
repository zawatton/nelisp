F1 subr car                        t
F2 macro when                      nil
F3 special form if                 nil
F4 defmacro                        nil
F5 defun                           t
F6 alias to defun                  t
F7 alias to macro                  nil
F8 symbol with (macro . f)         nil
F9 unbound symbol                  nil
F10 nil                            nil
F11 t                              nil
F12 keyword                        nil
F13 subr object car                t
F14 subr object if                 nil
F15 macro object when              nil
F16 (lambda (x) x) list            t
F17 (closure ...) list             nil
F18 (lambda) list                  t
F19 (macro . car) cons             nil
F20 other lists                    (nil nil nil)
F21 non-functions                  (nil nil nil nil nil)
F22 lambda closure object          t
F23 lexical closure object         t
F24 #'fp-f                         t
F25 byte-code object               t
F26 symbol bound to a value        nil
F27 defun macro special            (nil nil nil t)
F28 symbol -> (lambda)             t
F29 symbol -> (closure) list       nil
F30 symbol -> special subr         nil
F31 autoload type nil              t
F32 autoload type macro/t/keymap   (nil nil nil)
F33 closure through a mapcar       (t t nil nil nil)
