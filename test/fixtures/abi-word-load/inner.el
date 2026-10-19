(setq abi-load-inner-name load-file-name)
(setq abi-load-inner-special (bound-and-true-p abi-load-special))
(garbage-collect)
(provide 'abi-load-inner)
-7
