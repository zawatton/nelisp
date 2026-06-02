;;; map-ynp-test.el --- ERT for lightweight map-ynp facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name
       "../src/map-ynp.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro map-ynp-test--with-keys (keys &rest body)
  "Run BODY while `read-key' returns KEYS in order."
  (declare (indent 1))
  `(let ((events ,keys))
     (cl-letf (((symbol-function 'read-key)
                (lambda (&optional _prompt)
                  (let ((event (car events)))
                    (setq events (cdr events))
                    event))))
       ,@body)))

(ert-deftest map-ynp-test/require-loads-standard-feature ()
  (should (featurep 'map-ynp))
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
                   ("no" ?n "skip"))))
    (let ((read-answer-short t))
      (map-ynp-test--with-keys '(?y)
        (should (equal (read-answer "Proceed? " answers) "yes"))))
    (let ((read-answer-short nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "no")))
        (should (equal (read-answer "Proceed? " answers) "no"))))))

(provide 'map-ynp-test)

;;; map-ynp-test.el ends here
