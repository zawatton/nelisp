(zlib-decompress-region
  (let ((bytes (apply #'unibyte-string '(31 139 8 0 0 0 0 0 2 255 203 72 205 201 201 7 0 134 166 16 54 5 0 0 0))))
    (with-temp-buffer (set-buffer-multibyte nil) (insert bytes)
      (let ((result (zlib-decompress-region (point-min) (point-max))))
        (list result (buffer-string)))))
  (let ((bytes (apply #'unibyte-string '(31 139 8 0 0 0 0 0 2 255 203 72 205 201 201 7 0 134 166 16 54 5 0 0 0))))
    (with-temp-buffer (set-buffer-multibyte nil) (insert "prefix" bytes "suffix")
      (let ((result (zlib-decompress-region 7 32)))
        (list result (buffer-string)))))
  (with-temp-buffer (set-buffer-multibyte nil) (insert "bad")
    (let ((result (zlib-decompress-region 1 4))) (list result (buffer-string)))))
;; (zlib-decompress-region nil 2) is not probed: it crashed host GNU 31.1 (SIGSEGV) in the combined run.
