;;; emacs-cc-x-geometry-1-test.el --- x-parse-geometry contract -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name
       "../../packages/nelisp-emacs-foundation/src/emacs-cc-x-geometry-1.el"
       (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest emacs-cc-x-geometry-1/x-parse-geometry-valid-matrix ()
  (dolist (case
           '(("80" . ((width . 80)))
             ("x24" . ((height . 24)))
             ("x+10+20" . ((height . 10) (left . 20)))
             ("80x24" . ((height . 24) (width . 80)))
             ("80x24+10+20" . ((height . 24) (width . 80)
                                (top . 20) (left . 10)))
             ("80x24+1" . ((height . 24) (width . 80) (left . 1)))
             ("80x24+1-2" . ((height . 24) (width . 80)
                              (top . -2) (left . 1)))
             ("80x24-1" . ((height . 24) (width . 80) (left . -1)))
             ("80x24-10-20" . ((height . 24) (width . 80)
                                (top . -20) (left . -10)))
             ("+10+20" . ((top . 20) (left . 10)))
             ("+10" . ((left . 10)))
             ("-10" . ((left . -10)))
             ("-10-20" . ((top . -20) (left . -10)))
             ("80x24+0+0" . ((height . 24) (width . 80)
                             (top . 0) (left . 0)))
             ("80x24-0-0" . ((height . 24) (width . 80)
                             (top - 0) (left - 0)))
             ("80x24-0+0" . ((height . 24) (width . 80)
                             (top . 0) (left - 0)))
             ("80X24" . ((height . 24) (width . 80)))
             ("80X24+1+2" . ((height . 24) (width . 80)
                             (top . 2) (left . 1)))
             ("=80x24+1+2" . ((height . 24) (width . 80)
                              (top . 2) (left . 1)))
             ("= 80x24+1+2" . ((height . 24) (width . 80)
                               (top . 2) (left . 1)))
             (" 80x24" . ((height . 24) (width . 80)))))
    (should (equal (x-parse-geometry (car case)) (cdr case)))))

(ert-deftest emacs-cc-x-geometry-1/x-parse-geometry-malformed-matrix ()
  (dolist (string '("80x24+" "80x24+1-" "80x24+1-2junk"
                    "80x24+1+2junk" "80x24junk" "" "80x"
                    "80x24 "))
    (should-not (x-parse-geometry string))))

(ert-deftest emacs-cc-x-geometry-1/x-parse-geometry-type-and-arity-errors ()
  (dolist (value '(nil 7 foo '(80 24) [80 24]))
    (should-error (x-parse-geometry value) :type 'wrong-type-argument))
  (should-error (x-parse-geometry) :type 'wrong-number-of-arguments)
  (should-error (x-parse-geometry "80x24" "1x1")
                :type 'wrong-number-of-arguments))

(provide 'emacs-cc-x-geometry-1-test)
;;; emacs-cc-x-geometry-1-test.el ends here
