;;; batch.el --- Execute one named smoke scenario -*- lexical-binding: t; -*-

(load (expand-file-name "driver.el" (getenv "NELISP_SMOKE_SUITE")) nil t t)
(let ((errors (smoke-run-case (getenv "NELISP_SMOKE_CASE"))))
  (kill-emacs (if (= errors 0) 0 1)))
