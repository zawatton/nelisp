;;; emacs-parity-subdirs.el --- (intentionally no-op) subdir load-path walk -*- lexical-binding: t; -*-

;; HISTORY / WHY THIS IS A NO-OP
;;
;; This file used to redefine `normal-top-level-add-subdirs-to-load-path'
;; (startup.el) with a hash-set dedup, on the theory that its O(n) `member'
;; against `normal-top-level-add-subdirs-inode-list' made the real-init
;; load-path walk O(n^2) and was a dominant reason the full-org audit did not
;; finish init.
;;
;; Direct measurement on the standalone binary refuted that:
;;
;;   * The baked `normal-top-level-add-subdirs-to-load-path' walks the whole
;;     external-packages tree CORRECTLY and reasonably fast:
;;       len=788  themes=t  org-download=t  time=25.87s
;;     (the `member' dedup is only ~5s of that; the rest is directory-files /
;;     file-directory-p filesystem I/O that any implementation pays.)
;;
;;   * Every redefine attempt here was strictly worse:
;;       - `file-attribute-file-identifier' -> void-function (added nothing);
;;       - `normal-top-level-add-to-load-path' -> void-function (added nothing);
;;       - relative `file-directory-p' -> nil on this substrate (added nothing);
;;       - `file-truename' dedup key -> correct but >120s (stats every path
;;         component per dir);
;;       - `file-attributes' inode key -> nil inode here, so all dirs collide
;;         to one bucket (added only the root's 177 children, dropped every
;;         nested dir, e.g. themes/themes -- which lost the doom-dracula theme);
;;       - absolute-path key -> correct (len=787 themes=t org-download=t) but
;;         95.88s, i.e. 3.7x SLOWER than the baked original.
;;
;; The earlier "load-path form 32s -> <3s" speedup was an ILLUSION: the redefine
;; was erroring out immediately and adding nothing, so it merely SKIPPED the
;; subdir scan.  That skip is exactly what dropped external-packages'
;; themes/themes and org-download from `load-path' and broke theme / org-download
;; loading in the audit.
;;
;; Conclusion: the baked function is already correct and faster than anything we
;; can cheaply do in pure Elisp on this substrate, so we intentionally do NOT
;; override it.  Any real load-path-walk speedup must come from making the
;; substrate's `directory-files' / `file-directory-p' cheaper, not from redoing
;; the dedup in Elisp.  Keep this file as a documented no-op so the loader's
;; `require'/load of `emacs-parity-subdirs' still succeeds.

(provide 'emacs-parity-subdirs)
;;; emacs-parity-subdirs.el ends here
