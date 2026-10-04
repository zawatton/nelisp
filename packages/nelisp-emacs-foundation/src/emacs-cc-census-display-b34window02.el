;;; emacs-cc-census-display-b34window02.el --- Per-window display state  -*- lexical-binding: t; -*-

(defun emacs-cc-census-display-b34window02--get (window key default)
  "Read KEY from live WINDOW, returning DEFAULT when unset."
  (let ((target (or window (selected-window))))
    (unless (window-live-p target)
      (signal 'wrong-type-argument (list 'window-live-p target)))
    (let ((value (window-parameter target key)))
      (if (null value) default value))))

(defun emacs-cc-census-display-b34window02--set (window key value)
  "Store VALUE under KEY on live WINDOW and return VALUE."
  (let ((target (or window (selected-window))))
    (unless (window-live-p target)
      (signal 'wrong-type-argument (list 'window-live-p target)))
    (set-window-parameter target key value)))

(unless (fboundp 'window-dedicated-p)
  (defun window-dedicated-p (&optional window)
    "Return WINDOW's dedication state, or nil if it is not dedicated."
    (emacs-cc-census-display-b34window02--get window 'ccore-dedicated nil)))

(unless (fboundp 'set-window-dedicated-p)
  (defun set-window-dedicated-p (window flag)
    "Set WINDOW's dedication state to FLAG and return FLAG."
    (emacs-cc-census-display-b34window02--set
     window 'ccore-dedicated flag)))

(unless (fboundp 'window-display-table)
  (defun window-display-table (&optional window)
    "Return WINDOW's display table, or nil when it has none."
    (emacs-cc-census-display-b34window02--get window 'ccore-display-table nil)))

(unless (fboundp 'set-window-display-table)
  (defun set-window-display-table (window table)
    "Set WINDOW's display TABLE and return TABLE."
    (unless (or (null table) (char-table-p table))
      (signal 'wrong-type-argument (list 'char-table-p table)))
    (emacs-cc-census-display-b34window02--set
     window 'ccore-display-table table)
    table))

(unless (fboundp 'window-hscroll)
  (defun window-hscroll (&optional window)
    "Return WINDOW's horizontal scroll offset."
    (emacs-cc-census-display-b34window02--get window 'ccore-hscroll 0)))

(unless (fboundp 'set-window-hscroll)
  (defun set-window-hscroll (window columns)
    "Set WINDOW's horizontal scroll offset to COLUMNS."
    (unless (integerp columns)
      (signal 'wrong-type-argument (list 'integerp columns)))
    (emacs-cc-census-display-b34window02--set
     window 'ccore-hscroll (max 0 columns)))
  )

(unless (fboundp 'window-combination-limit)
  (defun window-combination-limit (window)
    "Return WINDOW's combination limit, or nil when no limit is set."
    (let ((target window))
      (unless (window-valid-p target)
        (signal 'wrong-type-argument (list 'window-valid-p target)))
      (when (window-live-p target)
        (error "Combination limit is meaningful for internal windows only"))
      (window-parameter target 'ccore-combination-limit))))

(unless (fboundp 'scroll-left)
  (defun scroll-left (&optional columns _wholetail)
    "Scroll the selected window left by COLUMNS and return its offset."
    (let* ((window (selected-window))
           (amount (if columns columns (max 0 (- (window-width window) 2)))) )
      (unless (integerp amount)
        (signal 'wrong-type-argument (list 'integerp amount)))
      (set-window-hscroll window (+ (window-hscroll window) amount)))))

(unless (fboundp 'scroll-right)
  (defun scroll-right (&optional columns _wholetail)
    "Scroll the selected window right by COLUMNS and return its offset."
    (let* ((window (selected-window))
           (amount (if columns columns (max 0 (- (window-width window) 2)))))
      (unless (integerp amount)
        (signal 'wrong-type-argument (list 'integerp amount)))
      (set-window-hscroll window (max 0 (- (window-hscroll window) amount))))))

(provide 'emacs-cc-census-display-b34window02)
