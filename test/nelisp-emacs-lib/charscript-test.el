;;; charscript-test.el --- tests for lightweight charscript facade  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)

(load (nemacs-test-paths-source "charscript.el")
      nil t)

(ert-deftest charscript-test/feature-loads ()
  (should (featurep 'charscript))
  (should (boundp 'char-script-table))
  (should (fboundp 'charscript--char-script)))

(ert-deftest charscript-test/lightweight-table-covers-latin ()
  (let ((table (charscript--make-table)))
    (should (char-table-p table))
    (should (eq (aref table ?A) 'latin))
    (should (eq (aref table #xff) 'latin))
    (should (memq 'latin (char-table-extra-slot table 0)))))

(ert-deftest charscript-test/char-script-helper-is-bounded ()
  (should (eq (charscript--char-script ?z) 'latin))
  (should-not (charscript--char-script #x3042)))

(ert-deftest charscript-test/ensure-table-installs-extra-slot ()
  (let ((char-script-table (make-char-table 'char-script-table nil)))
    (charscript--ensure-table)
    (should (memq 'latin (char-table-extra-slot char-script-table 0)))))

(provide 'charscript-test)

;;; charscript-test.el ends here
