;;; emacs-load-unibyte-source.el --- UTF-8 loader fixture  -*- lexical-binding: t; -*-

;; 日本語のコメントも byte-indexed scanner を通過する。

(defun emacs-load-unibyte-fixture-text ()
  "日本語のドキュメント。"
  "日本語の文字列。")

(defconst emacs-load-unibyte-fixture-escaped "\N{U+3042}\x42")

(provide 'emacs-load-unibyte-source)

;;; emacs-load-unibyte-source.el ends here
