;;; regex-syncache-identity.el --- cache identity control -*- lexical-binding: nil; -*-
(let ((case-fold-search nil)
      (search-spaces-regexp nil)
      (old-table (syntax-table))
      (one (copy-syntax-table))
      (two (copy-syntax-table))
      (pattern "\\`\\sw\\'"))
  (unwind-protect
      (progn
        (nlre--compiled-cache-clear)
        (set-syntax-table one)
        (let ((first (nlre--compiled-pattern pattern)))
          (unless (eq first (nlre--compiled-pattern pattern))
            (error "SYNCACHE-IDENTITY: a same-table hit recompiled"))
          (set-syntax-table two)
          (when (eq first (nlre--compiled-pattern pattern))
            (error "SYNCACHE-IDENTITY: equal-content tables shared an entry"))
          (set-syntax-table one)
          (unless (eq first (nlre--compiled-pattern pattern))
            (error "SYNCACHE-IDENTITY: switching back lost the original entry"))))
    (set-syntax-table old-table)))
(princ "REGEXP-SYNCACHE identity PASS\n")
nil
