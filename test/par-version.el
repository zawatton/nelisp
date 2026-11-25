;;; par-version.el --- GNU version comparison and parsing -*- lexical-binding: t; -*-

(prin1 (list 'versions
             (version-to-list ".5")
             (version-to-list "22.8 Beta3")
             (version-to-list "1.0-git")
             (version< "1.0rc2" "1.0")
             (version<= "1" "1.0.0")
             (version= "1.0.0" "1")))
(terpri)

(dolist (compare '(version< version<= version=))
  (prin1 (list compare
               (condition-case err
                   (funcall compare 1 "2")
                 (error (error-message-string err)))))
  (terpri))
