;;; windows-native-hot-loop-fixture.el --- W1.4 hot loop  -*- lexical-binding: t; -*-
;; The same lexical numeric loop as P3.1; the value is N*(N-1).
(defun p3-loop (n)
  (let ((s 0) (i 0))
    (while (< i n)
      (setq s (+ s (* i 2)) i (1+ i)))
    s))
