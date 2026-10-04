;;; nelisp-load-gnu-elc-public-api-test.el --- public GNU ELC predicate -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-load)

(ert-deftest nelisp-load-gnu-elc-public-api/recognizes-only-marker-prefix ()
  (should (nelisp-load-gnu-elc-p (concat ";ELC" (string 31) "payload")))
  (dolist (contents '(nil 7 "" ";ELC" ";ELC\0" "ELC\37"))
    (should-not (nelisp-load-gnu-elc-p contents))))

(provide 'nelisp-load-gnu-elc-public-api-test)
;;; nelisp-load-gnu-elc-public-api-test.el ends here
