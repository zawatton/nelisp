;;; emacs-bookmark-ui-test.el --- ERT tests for bookmark UI helpers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-bookmark-ui)

(ert-deftest emacs-bookmark-ui-completion-candidates-sort-and-filter ()
  (let ((bookmarks '(("zeta" . (:buffer "b" :pos 3))
                     ("alpha" . (:buffer "a" :pos 1))
                     ("alpine" . (:buffer "c" :pos 2)))))
    (should (equal '("alpha" "alpine" "zeta")
                   (mapcar #'car (emacs-bookmark-ui-sorted bookmarks))))
    (should (equal '("alpha" "alpine")
                   (emacs-bookmark-ui-completion-candidates
                    bookmarks "al")))
    (should (equal '("alpha" "alpine" "zeta")
                   (emacs-bookmark-ui-completion-candidates
                    bookmarks "")))))

(ert-deftest emacs-bookmark-ui-listing-renders-empty-and-sorted ()
  (should (equal "No bookmarks set.\n"
                 (plist-get (emacs-bookmark-ui-listing nil) :text)))
  (let* ((bookmarks '(("zeta" . (:buffer "zbuf" :pos 30))
                      ("alpha" . (:buffer "abuf" :pos 10))))
         (listing (emacs-bookmark-ui-listing bookmarks)))
    (should (= 2 (plist-get listing :count)))
    (should (equal '("alpha" "zeta")
                   (mapcar #'car (plist-get listing :entries))))
    (should (equal (concat
                    "Bookmarks:\n\n"
                    "  alpha                          -> abuf:10\n"
                    "  zeta                           -> zbuf:30\n")
                   (plist-get listing :text)))))

(ert-deftest emacs-bookmark-ui-jump-plan-reports-states ()
  (should (equal
           '(no-bookmarks "bookmark-jump: no bookmarks")
           (let ((plan (emacs-bookmark-ui-jump-plan nil nil)))
             (list (plist-get plan :status)
                   (plist-get plan :message)))))
  (let ((bookmarks '(("alpha" . (:buffer "abuf" :pos 10)))))
    (should (equal
             '(missing "bookmark-jump: beta not found")
             (let ((plan (emacs-bookmark-ui-jump-plan
                          bookmarks "beta" (lambda (_name) t))))
               (list (plist-get plan :status)
                     (plist-get plan :message)))))
    (should (equal
             '(buffer-missing "abuf" 10 "bookmark-jump: buffer abuf gone")
             (let ((plan (emacs-bookmark-ui-jump-plan
                          bookmarks "alpha" (lambda (_name) nil))))
               (list (plist-get plan :status)
                     (plist-get plan :buffer-name)
                     (plist-get plan :point)
                     (plist-get plan :message)))))
    (should (equal
             '(ok "alpha" "abuf" 10 "bookmark-jump: alpha -> abuf:10")
             (let ((plan (emacs-bookmark-ui-jump-plan
                          bookmarks "alpha" (lambda (_name) t))))
               (list (plist-get plan :status)
                     (plist-get plan :bookmark)
                     (plist-get plan :buffer-name)
                     (plist-get plan :point)
                     (plist-get plan :message)))))))

(provide 'emacs-bookmark-ui-test)

;;; emacs-bookmark-ui-test.el ends here
