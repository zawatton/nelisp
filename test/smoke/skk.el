;;; skk.el --- DDSKK kana and dictionary-backed kanji input -*- lexical-binding: t; -*-

(defun smoke-skk ()
  (let ((dictionary (expand-file-name "SKK-JISYO.fixture"))
        (personal (expand-file-name "SKK-JISYO.personal")))
    (when (smoke-step "skk-load"
            (require 'skk))
      (let ((skk-jisyo personal)
            (skk-init-file (expand-file-name "absent-init.el"))
            (skk-jisyo-code 'utf-8-unix)
            (skk-large-jisyo (cons dictionary 'utf-8-unix))
            (skk-server-host nil)
            (skk-servers-list nil)
            (skk-kakutei-jisyo nil)
            (skk-search-prog-list '((skk-search-jisyo-file skk-large-jisyo 0)))
            (skk-show-tooltip nil))
        (switch-to-buffer (generate-new-buffer " *smoke-skk*"))
        (smoke-step "skk-kana"
          (skk-mode 1)
          (execute-kbd-macro "kana")
          (list (key-binding "k") (smoke-state)))
        (smoke-step "skk-kanji"
          (execute-kbd-macro (kbd "K a n a SPC C-j"))
          (unless (equal (buffer-string) "かな仮名")
            (error "SKK did not commit the dictionary candidate: %s" (buffer-string)))
          (smoke-state))))))
