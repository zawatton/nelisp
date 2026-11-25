;;; par-single-owner-round5.el --- GNU definitions and their substrates -*- lexical-binding: t; -*-

(defun round5-row (name result)
  (prin1 (cons name result))
  (terpri))

(round5-row
 'replace-buffer-contents
 (with-temp-buffer
   (insert "keep")
   (let ((missing (condition-case err
                      (replace-buffer-contents " *round5-missing*")
                    (error err)))
         (nil-source (condition-case err
                         (replace-region-contents (point-min) (point-max) nil)
                       (error err))))
     (list missing nil-source (buffer-string)))))

(round5-row
 'subregexp-context-p
 (mapcar (lambda (regexp)
           (list regexp
                 (condition-case err (string-match regexp "") (error err))
                 (subregexp-context-p regexp (length regexp))))
         '("[abc" "[a-" "a\\" "a\\{" "a\\{2," "a\\{2\\" "a\\{x" "\\(" "a\\{2\\}")))

(round5-row
 'calendar
 (condition-case err
     (let ((autoload (autoloadp (symbol-function 'calendar)))
           (library (locate-library "calendar")))
       (if autoload
           (autoload-do-load (symbol-function 'calendar) 'calendar)
         (require 'calendar))
       (list autoload
             (equal (file-name-nondirectory
                     (directory-file-name (file-name-directory library)))
                    "calendar")
             (featurep 'cal-menu)
             (calendar-gregorian-from-absolute
              (calendar-absolute-from-gregorian '(2 29 2000)))))
   (error (list (car err)))))

(round5-row
 'cvs-dired-noselect
 (let ((dir (make-temp-file "round5-" t))
       (current-prefix-arg nil)
       (cvs-dired-action 'cvs-quickdir)
       (cvs-dired-use-hook 'always))
   (unwind-protect
       (let ((cvs (expand-file-name "CVS" dir)))
         (make-directory cvs)
         (write-region "" nil (expand-file-name "Entries" cvs) nil 'silent)
         (list (equal (file-name-base (symbol-file 'cvs-dired-noselect 'defun))
                      "loaddefs")
               (cvs-dired-noselect dir)
               (autoloadp (symbol-function 'cvs-quickdir))
               (condition-case err
                   (let ((buffer (cvs-dired-noselect cvs)))
                     (and (bufferp buffer)
                          (with-current-buffer buffer major-mode)))
                 (error (car err)))))
     (delete-directory dir t))))

(require 'cl-lib)
(defvar treesit-language-source-alist)

(round5-row
 'treesit-install-language-grammar
 (condition-case err
     (progn
       (autoload-do-load (symbol-function 'treesit-install-language-grammar)
                         'treesit-install-language-grammar)
       (let ((treesit-language-source-alist '((round5 "recipe" "revision"))))
         (catch 'round5-install
           (cl-letf (((symbol-function 'treesit--install-language-grammar-1)
                      (lambda (&rest args) (throw 'round5-install args))))
             (treesit-install-language-grammar 'round5 "/round5")))))
   (error (list (car err)))))

(round5-row
 'treesit-font-lock-rules
 (condition-case err
     (progn
       (require 'treesit)
       (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
                 ((symbol-function 'treesit-query-p) #'stringp)
                 ((symbol-function 'treesit-compiled-query-p) #'ignore)
                 ((symbol-function 'treesit-query-compile) #'list))
         (treesit-font-lock-rules
          :language 'elisp :feature 'sample :reversed t "(node) @sample")))
   (error (list (car err)))))

(round5-row
 'message
 (condition-case err
     (let ((original (symbol-function 'message))
           (inhibit-message t))
       (require 'ob-scheme)
       (list (subrp original)
             (eval '(org-babel-scheme-capture-current-message
                     (message "Scheme %s" "output")) t)
             (eq original (symbol-function 'message))))
   (error (list (car err)))))
