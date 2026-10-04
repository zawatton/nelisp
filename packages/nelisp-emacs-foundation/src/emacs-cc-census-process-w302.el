;;; emacs-cc-census-process-w302.el --- Thread join argument validation  -*- lexical-binding: t; -*-

(unless (fboundp 'thread-join)
  (defun thread-join (thread)
    "Wait for THREAD to exit and return its result.
The standalone runtime has no thread objects, so every available value
fails GNU Emacs's thread type check.  Joining an actual thread requires
native thread support."
    (signal 'wrong-type-argument (list 'threadp thread))))

(provide 'emacs-cc-census-process-w302)
;;; emacs-cc-census-process-w302.el ends here
