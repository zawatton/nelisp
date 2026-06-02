;;; iso-transl-test.el --- tests for lightweight ISO translations  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'iso-transl)

(ert-deftest iso-transl-test/feature-loads ()
  (should (featurep 'iso-transl))
  (should (keymapp iso-transl-ctl-x-8-map))
  (should (keymapp key-translation-map)))

(ert-deftest iso-transl-test/common-latin-translations ()
  (should (equal (lookup-key iso-transl-ctl-x-8-map "\"a")
                 [#x00e4]))
  (should (equal (lookup-key iso-transl-ctl-x-8-map "'e")
                 [#x00e9]))
  (should (equal (lookup-key iso-transl-ctl-x-8-map "~n")
                 [#x00f1])))

(ert-deftest iso-transl-test/symbol-translations ()
  (should (equal (lookup-key iso-transl-ctl-x-8-map "*E")
                 [#x20ac]))
  (should (equal (lookup-key iso-transl-ctl-x-8-map "a>")
                 [#x2192]))
  (should (equal (lookup-key iso-transl-ctl-x-8-map "/=")
                 [#x2260])))

(ert-deftest iso-transl-test/language-overlays-short-bindings ()
  (iso-transl-set-language "Spanish")
  (should (equal (lookup-key iso-transl-ctl-x-8-map "N")
                 [#x00d1]))
  (should (equal (lookup-key iso-transl-ctl-x-8-map "?")
                 [#x00bf])))

(ert-deftest iso-transl-test/key-translation-prefix-installed ()
  (should (eq (lookup-key key-translation-map "\C-x8")
              iso-transl-ctl-x-8-map)))

(provide 'iso-transl-test)

;;; iso-transl-test.el ends here
