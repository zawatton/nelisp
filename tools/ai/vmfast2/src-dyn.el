;;; src-dyn.el --- vmfast2 parity corpus, dynamic-binding part (compiled by mkbc.el with GNU 31.1)
(defvar v2-dyn 'outer)
(defun v2d-cb (x) (list x v2-dyn))
(defun v2d-bind (v2-dyn) (v2d-cb 0))
