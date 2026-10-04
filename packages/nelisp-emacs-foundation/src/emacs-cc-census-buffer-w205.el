;;; emacs-cc-census-buffer-w205.el --- Decompression availability  -*- lexical-binding: t; -*-

(unless (fboundp 'zlib-available-p)
  (defun zlib-available-p ()
    "Return t if zlib decompression is available in this runtime."
    ;; The standalone decompressor delegates to the gzip executable.
    (and (fboundp 'zlib-decompress-region)
         (fboundp 'call-process-region)
         (fboundp 'executable-find)
         (executable-find "gzip")
         t)))

(provide 'emacs-cc-census-buffer-w205)
