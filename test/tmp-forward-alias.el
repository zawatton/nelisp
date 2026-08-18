(defalias 'forward-alias-probe 'forward-alias-target)
(defun forward-alias-target () 42)
(defun native-inc-probe (x) (+ x 1))
;; Recompile probe after byte-accurate assembler position fix.
