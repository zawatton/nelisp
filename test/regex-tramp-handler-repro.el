;;; tramp-regex-slow.el --- b93d504eb regex regression repro  -*- lexical-binding: t -*-
;; Run: nelisp --load tramp-regex-slow.el ; prints ms per match for each tramp handler regexp.
;; fb54c401f: ~0.5 ms each.  b93d504eb: 136-176 ms each.  Result must be nil for both (GNU: nil).
(defvar trs-file "/home/user/.emacs.d/external-packages/all-the-icons.el/data/data-faicons.el")
(defvar trs-regexps '(
  "\\(?:\\`/\\)\\(?:\\(?:\\(-\\|[[:alnum:]]\\{2,\\}\\)\\(?::\\)\\(?:\\([^\"$'-*/:-<>?[-^`{-}[:blank:]]+\\|\\$[_[:alnum:]]+\\)\\(?:@\\)\\)?\\(\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)?\\(?:]\\)\\)\\(?:\\(?:#\\)\\(?:[[:digit:]]+\\)\\)?\\)?\\)\\(?:|\\)\\)*\\(?:\\(?:-\\|[[:alnum:]]+\\)\\(?:\\(?::\\)\\(?:\\(?:[^\"$'-*/:-<>?[-^`{-}[:blank:]]+\\|\\$[_[:alnum:]]+\\)\\(?:@\\)\\)?\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)\\(?:]\\)?\\)?\\)?\\)?\\)?\\'"
  "\\(?:\\`/\\)\\(\\(?:\\(?:\\(-\\|[[:alnum:]]\\{2,\\}\\)\\(?::\\)\\(?:\\([^\"$'-*/:-<>?[-^`{-}[:blank:]]+\\|\\$[_[:alnum:]]+\\)\\(?:@\\)\\)?\\(\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)?\\(?:]\\)\\)\\(?:\\(?:#\\)\\(?:[[:digit:]]+\\)\\)?\\)?\\)\\(?:|\\)\\)+\\)?\\(?:\\(-\\|[[:alnum:]]\\{2,\\}\\)\\(?::\\)\\(?:\\([^\"$'-*/:-<>?[-^`{-}[:blank:]]+\\|\\$[_[:alnum:]]+\\)\\(?:@\\)\\)?\\(\\(?:[%._[:alnum:]-]+\\|\\(?:\\[\\)\\(?:\\(?:[[:alnum:]]*:\\)+[.[:alnum:]]*\\)?\\(?:]\\)\\)\\(?:\\(?:#\\)\\(?:[[:digit:]]+\\)\\)?\\)?\\)\\(?::\\)\\([^\n]*\\'\\)"))
(dolist (re trs-regexps)
  (let ((t0 (float-time)) r)
    (dotimes (_ 5) (setq r (string-match re trs-file)))
    (princ (format "%8.2f ms/match result=%S\n" (* 200 (- (float-time) t0)) r))))
