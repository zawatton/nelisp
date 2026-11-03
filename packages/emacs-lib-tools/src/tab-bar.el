;;; tab-bar.el --- Minimal tab-bar subset for nelisp-emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defvar tab-bar-mode nil
  "Non-nil when the minimal tab bar is enabled.")

(defvar tab-bar--tabs nil
  "List of minimal tab records.")

(defvar tab-bar--selected-index 0
  "Zero-based selected tab index.")

(defun tab-bar--ensure-tabs ()
  "Ensure a live tab list exists."
  (unless tab-bar--tabs
    (setq tab-bar--tabs (list '((name . "1") (explicit-name . nil))))
    (setq tab-bar--selected-index 0))
  tab-bar--tabs)

(defun tab-bar-tabs (&optional _frame)
  "Return the current minimal tab list."
  (tab-bar--ensure-tabs))

(defun tab-bar-current-tab (&optional _frame)
  "Return the selected minimal tab."
  (nth tab-bar--selected-index (tab-bar--ensure-tabs)))

(defun tab-bar-current-tab-index (&optional _frame)
  "Return the zero-based selected tab index."
  (tab-bar--ensure-tabs)
  tab-bar--selected-index)

(defun tab-bar--tab-name (index)
  "Return default tab name for zero-based INDEX."
  (number-to-string (1+ index)))

;;; Per-tab window configuration ------------------------------------------
;;
;; Each tab carries its own window layout so switching tabs swaps the whole
;; window configuration (the "real tab-bar" behaviour).  Capture/restore go
;; through `emacs-window' when available and degrade to name-only tabs
;; otherwise (guarded by `fboundp', so there is no hard load-order coupling).

(defun tab-bar--capture-wc ()
  "Return the current window configuration, or nil when unsupported."
  (and (fboundp 'emacs-window-current-window-configuration)
       (emacs-window-current-window-configuration)))

(defun tab-bar--save-current-wc ()
  "Store the live window configuration into the selected tab."
  (let ((tab (nth tab-bar--selected-index tab-bar--tabs))
        (wc (tab-bar--capture-wc)))
    (when (and tab wc)
      (let ((cell (assq 'wc tab)))
        (if cell
            (setcdr cell wc)
          (nconc tab (list (cons 'wc wc))))))))

(defun tab-bar--restore-selected-wc ()
  "Restore the selected tab's window configuration, if it has one."
  (let* ((tab (nth tab-bar--selected-index tab-bar--tabs))
         (wc (cdr (assq 'wc tab))))
    (when (and wc (fboundp 'emacs-window-set-window-configuration))
      (emacs-window-set-window-configuration wc))))

(defun tab-bar-new-tab (&optional _arg)
  "Create and select a new minimal tab."
  (interactive "P")
  (tab-bar--ensure-tabs)
  (tab-bar--save-current-wc)
  (let ((tab `((name . ,(tab-bar--tab-name (length tab-bar--tabs)))
               (explicit-name . nil)
               (wc . ,(tab-bar--capture-wc)))))
    (setq tab-bar--tabs (append tab-bar--tabs (list tab)))
    (setq tab-bar--selected-index (1- (length tab-bar--tabs)))
    tab))

(defun tab-bar-select-tab (tab-number)
  "Select one-based TAB-NUMBER and return the selected tab."
  (interactive "nSelect tab: ")
  (tab-bar--ensure-tabs)
  (let ((index (1- tab-number)))
    (unless (and (integerp index)
                 (>= index 0)
                 (< index (length tab-bar--tabs)))
      (user-error "No such tab: %s" tab-number))
    (tab-bar--save-current-wc)
    (setq tab-bar--selected-index index)
    (tab-bar--restore-selected-wc)
    (tab-bar-current-tab)))

(defun tab-bar-switch-to-next-tab (&optional arg)
  "Select the next tab, wrapping around."
  (interactive "p")
  (tab-bar--ensure-tabs)
  (let* ((count (max 1 (length tab-bar--tabs)))
         (step (or arg 1)))
    (tab-bar--save-current-wc)
    (setq tab-bar--selected-index
          (mod (+ tab-bar--selected-index step) count))
    (tab-bar--restore-selected-wc)
    (tab-bar-current-tab)))

(defun tab-bar-switch-to-prev-tab (&optional arg)
  "Select the previous tab, wrapping around."
  (interactive "p")
  (tab-bar-switch-to-next-tab (- 0 (or arg 1))))

(defun tab-bar-close-tab (&optional tab-number)
  "Close one-based TAB-NUMBER, or the selected tab.
The last remaining tab is kept, matching the daily-driver guardrail."
  (interactive)
  (tab-bar--ensure-tabs)
  (let ((count (length tab-bar--tabs)))
    (if (<= count 1)
        (tab-bar-current-tab)
      (let ((index (if tab-number (1- tab-number) tab-bar--selected-index)))
        (unless (and (integerp index) (>= index 0) (< index count))
          (user-error "No such tab: %s" tab-number))
        (setq tab-bar--tabs
              (append (cl-subseq tab-bar--tabs 0 index)
                      (nthcdr (1+ index) tab-bar--tabs)))
        (setq tab-bar--selected-index
              (min tab-bar--selected-index (1- (length tab-bar--tabs))))
        (tab-bar--restore-selected-wc)
        (tab-bar-current-tab)))))

(defun tab-bar-rename-tab (name &optional tab-number)
  "Rename one-based TAB-NUMBER, or the selected tab, to NAME."
  (interactive "sTab name: ")
  (tab-bar--ensure-tabs)
  (let* ((index (if tab-number (1- tab-number) tab-bar--selected-index))
         (tab (nth index tab-bar--tabs)))
    (unless tab
      (user-error "No such tab: %s" tab-number))
    (setcdr (assq 'name tab) name)
    (setcdr (assq 'explicit-name tab) t)
    tab))

(defun tab-bar-mode (&optional arg)
  "Toggle the minimal tab bar mode."
  (interactive "P")
  (setq tab-bar-mode
        (if (null arg)
            (not tab-bar-mode)
          (> (prefix-numeric-value arg) 0)))
  (tab-bar--ensure-tabs)
  tab-bar-mode)

(unless (fboundp 'tab-bar-height)
  (defun tab-bar-height (&optional _frame)
    "Return the minimal tab bar height in text lines."
    (if tab-bar-mode 1 0)))

(defalias 'tab-new #'tab-bar-new-tab)
(defalias 'tab-close #'tab-bar-close-tab)
(defalias 'tab-next #'tab-bar-switch-to-next-tab)
(defalias 'tab-previous #'tab-bar-switch-to-prev-tab)
(defalias 'tab-select #'tab-bar-select-tab)
(defalias 'tab-rename #'tab-bar-rename-tab)

(provide 'tab-bar)

;;; tab-bar.el ends here
