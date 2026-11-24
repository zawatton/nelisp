;;; -*- lexical-binding: t; -*-
;; init.el (require 'consult) -> "Not a choice type: nil" from bookmark.el's top-level
;; (custom-add-choice 'tab-bar-new-tab-choice ...).  GNU preloads tab-bar.el, so the option already has a
;; `choice' custom-type; NeLisp's minimal tab-bar surface never declared it.
(defmacro row (n f) `(princ (format "%-34s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(row "T1 custom-type is choice" (car (get 'tab-bar-new-tab-choice 'custom-type)))
(row "T2 default value" (default-value 'tab-bar-new-tab-choice))
(row "T3 require bookmark" (require 'bookmark))
(row "T4 bookmark choice added" (and (member '(const :tag "Bookmark List" bookmark-bmenu-get-buffer)
                                              (cdr (get 'tab-bar-new-tab-choice 'custom-type))) t))
