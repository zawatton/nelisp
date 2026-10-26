;;; gnutests-drv  -*- lexical-binding: t -*-
(load (getenv "SHIM_SRC") nil t)
(shim-load (getenv "SHIM_FILE"))
(shim-run)
