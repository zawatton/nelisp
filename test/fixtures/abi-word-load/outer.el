(setq abi-load-outer-before load-file-name)
(load "test/fixtures/abi-word-load/inner.el" nil t)
(setq abi-load-outer-after load-file-name)
(garbage-collect)
(provide 'abi-load-outer)
17
