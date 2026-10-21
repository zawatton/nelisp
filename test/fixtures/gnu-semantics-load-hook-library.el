;;; gnu-semantics-load-hook-library.el --- Per-file expansion selection -*- lexical-binding: t; -*-
(if gs-load-hook-replacement
    (fset 'internal-macroexpand-for-load gs-load-hook-replacement)
  (fmakunbound 'internal-macroexpand-for-load))
(setq gs-load-hook-after t)
