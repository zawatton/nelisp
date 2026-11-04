;;; emacs-cc-decompress-1.el --- decompression primitive -*- lexical-binding: t; -*-

(unless (fboundp 'zlib-decompress-region)
  (defun zlib-decompress-region (start end &optional allow-partial)
    "Decompress a gzip- or zlib-compressed region.
Replace the text in the region by the decompressed data.

If optional parameter ALLOW-PARTIAL is nil or omitted, then on
failure, return nil and leave the data in place.  Otherwise, return
the number of bytes that were not decompressed and replace the region
text by whatever data was successfully decompressed (similar to gzip).
If decompression is completely successful return t.

This function can be called only in unibyte buffers."
    (unless (or (integerp start) (markerp start))
      (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
    (unless (or (integerp end) (markerp end))
      (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    (unless (and (<= (point-min) start) (<= start end) (<= end (point-max)))
      (signal 'args-out-of-range (list start end)))
    (let ((input (buffer-substring-no-properties start end))
          (out (generate-new-buffer " *zlib-decompress*"))
          status output)
      (unwind-protect
          (progn
            (with-current-buffer out
              (set-buffer-multibyte nil)
              (insert input)
              (setq status (call-process-region (point-min) (point-max)
                                                "gzip" t t nil "-dc"))
              (setq output (buffer-substring-no-properties (point-min) (point-max))))
            (if (and (integerp status) (= status 0))
                (progn (delete-region start end) (goto-char start) (insert output) t)
              (if (not allow-partial) nil
                (let ((remaining (max 0 (- (length input) (length output)))))
                  (delete-region start end) (goto-char start) (insert output)
                  remaining))))
        (kill-buffer out)))))

(provide 'emacs-cc-decompress-1)
