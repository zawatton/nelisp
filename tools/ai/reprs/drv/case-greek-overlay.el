;;; -*- lexical-binding: t; -*-
;; The survey4 ct-upcase-greek-str row with the case-fast-path overlay loaded first (SURVEY4 = survey4 directory).
(load (expand-file-name "../case-overlay.el" (file-name-directory load-file-name)) nil t)
(load (expand-file-name "drv/ct-upcase-greek-str.el" (getenv "SURVEY4")) nil t)
