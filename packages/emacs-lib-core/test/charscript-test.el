;;; charscript-test.el --- tests for GNU character scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "../../../test/emacs-lib/nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)

(load (nemacs-test-paths-source "charscript.el")
      nil t)

(ert-deftest charscript-test/feature-loads ()
  (should (featurep 'charscript))
  (should (char-table-p char-script-table))
  (should (eq (char-table-subtype char-script-table) 'char-script-table)))

(ert-deftest charscript-test/script-data-and-category ()
  (should (eq (aref char-script-table ?A) 'latin))
  (should (eq (aref char-script-table #x3042) 'kana))
  (should (eq (aref char-script-table #x1f642) 'emoji))
  (should (memq 'kana (char-table-extra-slot char-script-table 0)))
  (should (aref (char-category-set #x2042) ?5)))

(provide 'charscript-test)

;;; charscript-test.el ends here
