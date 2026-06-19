;;; emacs-toolbar.el --- GUI toolbar runtime helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Runtime-owned toolbar/dropdown semantics for the GUI bridge.  The
;; bridge still owns transport files; this module owns the default
;; toolbar spec, menu contents, hit testing, and click resolution.

;;; Code:

(defconst emacs-toolbar-gui-default-spec
  (concat "New\tC-x C-f\n"
          "Open\tC-x C-f\n"
          "Save\tC-x C-s\n"
          "Undo\tC-/\n"
          "Cut\tC-w\n"
          "Copy\tM-w\n"
          "Paste\tC-y\n"
          "Search\tC-s\n")
  "Default GUI toolbar spec as LABEL<TAB>KEYS lines.")

(defconst emacs-toolbar-gui-default-menus
  '(("New" . "Find File\tC-x C-f\nSwitch Buffer\tC-x b\n")
    ("Open" . "Open File\tC-x C-f\nOpen Read Only\tC-x C-r\nInsert File\tC-x i\n")
    ("Save" . "Save\tC-x C-s\nWrite File\tC-x C-w\nSave Some\tC-x s\n")
    ("Undo" . "Undo\tC-/\nRedo\tC-?\n")
    ("Cut" . "Cut\tC-w\nKill Line\tC-k\nDelete Region\tC-w\n")
    ("Copy" . "Copy\tM-w\nSelect All\tC-x h\n")
    ("Paste" . "Paste\tC-y\nYank Pop\tM-y\n")
    ("Search" . "Search Forward\tC-s\nSearch Backward\tC-r\nQuery Replace\tM-%\n"))
  "Default toolbar dropdown menus.")

(defvar emacs-toolbar-gui-backend nil
  "PLIST of GUI bridge toolbar backend callbacks.")

(defvar emacs-toolbar-gui-spec emacs-toolbar-gui-default-spec
  "Current GUI toolbar spec as LABEL<TAB>KEYS lines.")

(defvar emacs-toolbar-gui-cell-width-default 9
  "Fallback toolbar cell width in pixels.")

(defvar emacs-toolbar-gui-button-left-padding 6
  "Left pixel offset of the first toolbar button.")

(defvar emacs-toolbar-gui-button-extra-width 14
  "Non-text pixel width included in every toolbar button.")

(defvar emacs-toolbar-gui-menu-top 18
  "Y pixel coordinate where toolbar dropdown rows begin.")

(defvar emacs-toolbar-gui-menu-row-height 16
  "Dropdown row height in pixels.")

;;;###autoload
(defun emacs-toolbar-gui-register-backend (&rest backend)
  "Register BACKEND plist for GUI toolbar transport callbacks."
  (setq emacs-toolbar-gui-backend backend))

(defun emacs-toolbar-gui--backend-call (key &rest args)
  "Call toolbar backend function KEY with ARGS when registered."
  (let ((fn (and emacs-toolbar-gui-backend
                 (plist-get emacs-toolbar-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-toolbar-gui--digits-number (text)
  "Parse decimal digits from TEXT, ignoring non-digits."
  (let ((i 0)
        (n 0)
        (text (or text "")))
    (while (< i (length text))
      (let ((ch (aref text i)))
        (when (and (>= ch ?0) (<= ch ?9))
          (setq n (+ (* n 10) (- ch ?0)))))
      (setq i (1+ i)))
    n))

;;;###autoload
(defun emacs-toolbar-gui-cell-width ()
  "Return the GUI toolbar cell width in pixels."
  (let ((value (emacs-toolbar-gui--backend-call :cell-width)))
    (cond
     ((and (integerp value) (> value 0) (< value 256)) value)
     ((stringp value)
      (let ((n (emacs-toolbar-gui--digits-number value)))
        (if (and (> n 0) (< n 256))
            n
          emacs-toolbar-gui-cell-width-default)))
     (t emacs-toolbar-gui-cell-width-default))))

;;;###autoload
(defun emacs-toolbar-gui-write-state ()
  "Write the current toolbar state through the registered backend."
  (emacs-toolbar-gui--backend-call :write-state emacs-toolbar-gui-spec))

;;;###autoload
(defun emacs-toolbar-gui-write-menu (menu)
  "Write MENU through the registered toolbar backend."
  (emacs-toolbar-gui--backend-call :write-menu (or menu "")))

;;;###autoload
(defun emacs-toolbar-gui-current-menu ()
  "Read the currently open toolbar menu through the backend."
  (or (emacs-toolbar-gui--backend-call :read-menu) ""))

;;;###autoload
(defun emacs-toolbar-gui-menu-for-label (label)
  "Return dropdown menu text for toolbar LABEL."
  (or (cdr (assoc (or label "") emacs-toolbar-gui-default-menus))
      ""))

(defun emacs-toolbar-gui--entry-at-x (clickx)
  "Return toolbar entry plist at CLICKX, or nil."
  (let ((spec (or emacs-toolbar-gui-spec ""))
        (i 0)
        (tx emacs-toolbar-gui-button-left-padding)
        (found nil)
        (cw (emacs-toolbar-gui-cell-width)))
    (while (and (< i (length spec)) (not found))
      (let ((lstart i)
            (keys ""))
        (while (and (< i (length spec))
                    (/= (aref spec i) ?\t)
                    (/= (aref spec i) ?\n))
          (setq i (1+ i)))
        (let ((label (substring spec lstart i)))
          (when (and (< i (length spec)) (= (aref spec i) ?\t))
            (setq i (1+ i))
            (let ((ks i))
              (while (and (< i (length spec)) (/= (aref spec i) ?\n))
                (setq i (1+ i)))
              (setq keys (substring spec ks i))))
          (when (and (< i (length spec)) (= (aref spec i) ?\n))
            (setq i (1+ i)))
          (let ((bw (+ emacs-toolbar-gui-button-extra-width
                       (* (length label) cw))))
            (when (and (>= clickx tx) (< clickx (+ tx bw)))
              (setq found (list :label label :keys keys)))
            (setq tx (+ tx bw))))))
    found))

;;;###autoload
(defun emacs-toolbar-gui-keys-at-x (clickx)
  "Return the key sequence for the toolbar button at CLICKX."
  (or (plist-get (emacs-toolbar-gui--entry-at-x clickx) :keys) ""))

;;;###autoload
(defun emacs-toolbar-gui-label-at-x (clickx)
  "Return the label for the toolbar button at CLICKX."
  (or (plist-get (emacs-toolbar-gui--entry-at-x clickx) :label) ""))

;;;###autoload
(defun emacs-toolbar-gui-menu-keys-at-row (menu row)
  "Return key sequence from MENU at zero-based ROW."
  (let ((lines (split-string (or menu "") "\n" t))
        (line nil))
    (setq line (nth row lines))
    (if (and line (string-match "\t\\([^\t\n]+\\)\\'" line))
        (match-string 1 line)
      "")))

;;;###autoload
(defun emacs-toolbar-gui-parse-click (raw)
  "Parse RAW toolbar click text into (X . Y).
Old GUI builds sent only X; that form is treated as Y=0."
  (let ((i 0)
        (x 0)
        (y 0)
        (seen-comma nil)
        (raw (or raw "")))
    (while (< i (length raw))
      (let ((ch (aref raw i)))
        (cond
         ((= ch ?,)
          (setq seen-comma t))
         ((and (>= ch ?0) (<= ch ?9))
          (if seen-comma
              (setq y (+ (* y 10) (- ch ?0)))
            (setq x (+ (* x 10) (- ch ?0)))))))
      (setq i (1+ i)))
    (unless seen-comma
      (setq y 0))
    (cons x y)))

;;;###autoload
(defun emacs-toolbar-gui-handle-click (raw)
  "Resolve RAW toolbar click text and update dropdown backend state.
Return a plist with `:keys', `:command', `:effective-command', and
`:menu'.  Opening or cancelling a dropdown returns command `ignore'."
  (let* ((xy (emacs-toolbar-gui-parse-click raw))
         (cx (car xy))
         (cy (cdr xy)))
    (if (< cy emacs-toolbar-gui-menu-top)
        (let* ((label (emacs-toolbar-gui-label-at-x cx))
               (menu (emacs-toolbar-gui-menu-for-label label)))
          (if (equal menu "")
              (progn
                (emacs-toolbar-gui-write-menu "")
                (list :keys (emacs-toolbar-gui-keys-at-x cx)
                      :command nil
                      :effective-command ""
                      :menu ""))
            (emacs-toolbar-gui-write-menu menu)
            (list :keys ""
                  :command 'ignore
                  :effective-command "ignore"
                  :menu menu)))
      (let* ((menu (emacs-toolbar-gui-current-menu))
             (row (/ (- cy emacs-toolbar-gui-menu-top)
                     emacs-toolbar-gui-menu-row-height))
             (keys (emacs-toolbar-gui-menu-keys-at-row menu row)))
        (emacs-toolbar-gui-write-menu "")
        (if (equal keys "")
            (list :keys ""
                  :command 'ignore
                  :effective-command "ignore"
                  :menu "")
          (list :keys keys
                :command nil
                :effective-command ""
                :menu ""))))))

(provide 'emacs-toolbar)

;;; emacs-toolbar.el ends here
