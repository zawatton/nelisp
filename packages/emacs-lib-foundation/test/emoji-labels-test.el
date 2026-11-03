;;; emoji-labels-test.el --- tests for lightweight emoji labels  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Code:

(require 'ert)
(require 'emoji-labels)

(ert-deftest emoji-labels-test/feature-loads ()
  (should (featurep 'emoji-labels))
  (should (consp emoji--labels))
  (should (hash-table-p emoji--derived))
  (should (hash-table-p emoji--names)))

(ert-deftest emoji-labels-test/names-map-representative-glyphs ()
  (should (equal (gethash (emoji-labels--char #x1f600) emoji--names)
                 "grinning face"))
  (should (equal (gethash (emoji-labels--char #x1f680) emoji--names)
                 "rocket")))

(ert-deftest emoji-labels-test/labels-have-emoji-compatible-shape ()
  (let ((smileys (assoc "Smileys" emoji--labels)))
    (should smileys)
    (should (assoc "smiling" (cdr smileys)))
    (should (member (emoji-labels--char #x1f600)
                    (cdr (assoc "smiling" (cdr smileys)))))))

(ert-deftest emoji-labels-test/derived-variants-are-available ()
  (let* ((base (emoji-labels--char #x1f44d))
         (derived (gethash base emoji--derived)))
    (should (consp derived))
    (should (member (concat base (emoji-labels--char #x1f3fd)) derived))))

(provide 'emoji-labels-test)

;;; emoji-labels-test.el ends here
