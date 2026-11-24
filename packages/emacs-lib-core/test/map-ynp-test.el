;;; map-ynp-test.el --- ERT for GNU map-ynp  -*- lexical-binding: t; -*-

;;; Code:

(require 'nemacs-test-paths
         (expand-file-name "../../../test/emacs-lib/nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'cl-lib)

(load (nemacs-test-paths-vendor "emacs-lisp/emacs-lisp/map-ynp.el") nil t)

(defmacro map-ynp-test--with-keys (keys &rest body)
  "Run BODY while `read-key-sequence-vector' returns KEYS in order."
  (declare (indent 1))
  `(let ((events ,keys)
         (use-dialog-box nil)
         (y-or-n-p-use-read-key t))
     (cl-letf (((symbol-function 'read-key-sequence-vector)
                (lambda (&rest _)
                  (unless events
                    (error "Unexpected key read"))
                  (vector (pop events)))))
       ,@body)))

(ert-deftest map-ynp-test/source-loads-standard-commands ()
  (should (fboundp 'map-y-or-n-p))
  (should (fboundp 'read-answer))
  (should (boundp 'read-answer-short))
  (should (boundp 'read-answer-map--memoize)))

(ert-deftest map-ynp-test/map-y-or-n-p-acts-on-yes-only ()
  (let (acted)
    (map-ynp-test--with-keys '(?y ?n ?\s)
      (should (= (map-y-or-n-p "Act on %s? "
                               (lambda (object) (push object acted))
                               '(a b c))
                 2)))
    (should (equal (nreverse acted) '(a c)))))

(ert-deftest map-ynp-test/map-y-or-n-p-bang-acts-on-rest ()
  (let (acted)
    (map-ynp-test--with-keys '(?!)
      (should (= (map-y-or-n-p "Act on %s? "
                               (lambda (object) (push object acted))
                               '(a b c))
                 3)))
    (should (equal (nreverse acted) '(a b c)))))

(ert-deftest map-ynp-test/map-y-or-n-p-dot-acts-once-and-exits ()
  (let (acted)
    (map-ynp-test--with-keys '(?.)
      (should (= (map-y-or-n-p "Act on %s? "
                               (lambda (object) (push object acted))
                               '(a b c))
                 1)))
    (should (equal acted '(a)))))

(ert-deftest map-ynp-test/map-y-or-n-p-custom-action ()
  (let (acted inspected)
    (map-ynp-test--with-keys '(?i ?y ?n)
      (should (= (map-y-or-n-p
                  "Act on %s? "
                  (lambda (object) (push object acted))
                  '(a b)
                  nil
                  `((?i ,(lambda (object)
                           (push object inspected)
                           nil)
                        "inspect")))
                 1)))
    (should (equal (nreverse inspected) '(a)))
    (should (equal acted '(a)))))

(ert-deftest map-ynp-test/prompter-nil-skips-and-truthy-acts ()
  (let (acted)
    (map-ynp-test--with-keys '(?n)
      (should (= (map-y-or-n-p (lambda (object)
                                 (cond
                                  ((eq object 'skip) nil)
                                  ((eq object 'auto) t)
                                  (t "Ask? ")))
                               (lambda (object) (push object acted))
                               '(skip auto ask))
                 1)))
    (should (equal acted '(auto)))))

(ert-deftest map-ynp-test/read-answer-short-and-long ()
  (let ((answers '(("yes" ?y "accept")
                   ("no" ?n "skip")))
        (use-dialog-box nil))
    (let ((read-answer-short t))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (_prompt _initial keymap &rest _)
                   (should (keymapp keymap))
                   "yes")))
        (should (equal (read-answer "Proceed? " answers) "yes"))))
    (let ((read-answer-short nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "no")))
        (should (equal (read-answer "Proceed? " answers) "no"))))))

(provide 'map-ynp-test)

;;; map-ynp-test.el ends here
