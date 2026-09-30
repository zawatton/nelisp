;;; emacs-cc-marker-1.el --- marker.c primitives -*- lexical-binding: t; -*-

(unless (fboundp 'marker-last-position)
  (defun marker-last-position (marker)
    "Return last position of MARKER in its buffer.
This is like `marker-position' with one exception: If the buffer of
MARKER is dead, it returns the last position of MARKER in that buffer
before it was killed."
    (cond
     ((and (fboundp 'markerp) (markerp marker))
      (or (marker-position marker) 0))
     ((and (fboundp 'nelisp-ec-marker-p) (nelisp-ec-marker-p marker))
      (or (nelisp-ec-marker-position marker) 0))
     (t (signal 'wrong-type-argument (list 'markerp marker))))))

(provide 'emacs-cc-marker-1)
