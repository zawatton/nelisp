;;; emacs-isearch.el --- Incremental search UI on top of nelisp-emacs search primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; v0.1 daily-driver gate, M2.1 isearch UI per
;; `docs/design/02-v01-daily-driver.org' §3.2.1.
;;
;; This module provides a small interactive incremental-search layer:
;; `isearch-forward' / `isearch-backward', echo-area prompt updates,
;; current-match highlighting through a temporary `face' text property,
;; repeat with `C-s' / `C-r', `C-g' abort, `RET' commit, and `DEL'
;; query shrink.
;;
;; Search itself is delegated to the existing substrate primitives.
;; Under standalone NeLisp the unprefixed bridge resolves to the Layer 2
;; search builtins; under host-ERT runs we dispatch straight to the
;; `nelisp-ec-*' search functions so a `nelisp-ec-buffer' can still be
;; searched without touching the host Emacs buffer state.

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)
(require 'emacs-buffer)
(require 'emacs-faces)
(require 'emacs-keymap)
(require 'emacs-minibuffer)
(require 'emacs-search-builtins)

(define-error 'emacs-isearch-error "emacs-isearch error")

(defvar emacs-isearch--active nil
  "Non-nil while `isearch-forward' / `isearch-backward' is reading input.")

(defvar emacs-isearch--buffer nil
  "The `nelisp-ec-buffer' searched by the active session.")

(defvar emacs-isearch--direction 'forward
  "Direction of the active session: `forward' or `backward'.")

(defvar emacs-isearch--query ""
  "Current incremental search query.")

(defvar emacs-isearch--start-point nil
  "Original point saved when the active session started.")

(defvar emacs-isearch--failing nil
  "Non-nil when the current query has no match.")

(defvar emacs-isearch--match-beg nil
  "Inclusive beginning of the current highlighted match.")

(defvar emacs-isearch--match-end nil
  "Exclusive end of the current highlighted match.")

(defvar emacs-isearch--saved-face-runs nil
  "Saved `(BEG END FACE)' runs underneath the temporary isearch highlight.")

(defvar emacs-isearch--last-prompt ""
  "Most recent echo-area prompt rendered by `emacs-isearch'.")

(defconst emacs-isearch-highlight-face 'isearch
  "Face symbol applied to the current match during isearch.")

(defun emacs-isearch--ensure-face ()
  "Ensure the standard `isearch' face exists in the local registry."
  (unless (emacs-faces-facep emacs-isearch-highlight-face)
    (emacs-faces-make-face emacs-isearch-highlight-face)
    (emacs-faces-set-attribute emacs-isearch-highlight-face nil
                               :foreground "black"
                               :background "yellow")))

(defun emacs-isearch--ensure-global-bindings ()
  "Install `C-s' / `C-r' on the active global keymap."
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (emacs-keymap-current-global-map)
                 (emacs-keymap-make-sparse-keymap))))
    (when (fboundp 'use-global-map)
      (use-global-map map))
    (unless (eq map (emacs-keymap-current-global-map))
      (emacs-keymap-use-global-map map))
    (if (fboundp 'define-key)
        (progn
          (define-key map (kbd "C-s") #'isearch-forward)
          (define-key map (kbd "C-r") #'isearch-backward))
      (emacs-keymap-define-key map (kbd "C-s") #'isearch-forward)
      (emacs-keymap-define-key map (kbd "C-r") #'isearch-backward))))

(defun emacs-isearch--current-buffer ()
  "Return the current `nelisp-ec-buffer' or signal `user-error'."
  (or (nelisp-ec-current-buffer)
      (signal 'user-error '("isearch requires an active nelisp buffer"))))

(defun emacs-isearch--with-buffer (buffer fn)
  "Call FN with BUFFER current in the nelisp substrate."
  (nelisp-ec-with-current-buffer buffer
    (funcall fn)))

(defun emacs-isearch--prompt-string ()
  "Return the current isearch prompt string."
  (concat
   (cond
    (emacs-isearch--failing
     (if (eq emacs-isearch--direction 'backward)
         "Failing I-search backward: "
       "Failing I-search: "))
    ((eq emacs-isearch--direction 'backward)
     "I-search backward: ")
    (t "I-search: "))
   emacs-isearch--query))

(defun emacs-isearch--display-prompt ()
  "Render the current prompt in the minibuffer/echo area."
  (setq emacs-isearch--last-prompt (emacs-isearch--prompt-string))
  (emacs-minibuffer-minibuffer-message "%s" emacs-isearch--last-prompt))

(defun emacs-isearch--snapshot-face-runs (beg end buffer)
  "Return saved face runs for [BEG, END) in BUFFER."
  (let ((runs nil)
        (pos beg)
        (run-beg beg)
        (run-face nil))
    (while (< pos end)
      (let ((face (emacs-buffer-get-text-property pos 'face buffer)))
        (if (= pos beg)
            (setq run-face face)
          (unless (equal face run-face)
            (push (list run-beg pos run-face) runs)
            (setq run-beg pos
                  run-face face))))
      (setq pos (1+ pos)))
    (when (< run-beg end)
      (push (list run-beg end run-face) runs))
    (nreverse runs)))

(defun emacs-isearch--restore-face-runs (buffer runs)
  "Restore BUFFER text properties from saved face RUNS."
  (dolist (run runs)
    (pcase-let ((`(,beg ,end ,face) run))
      (if face
          (emacs-buffer-put-text-property beg end 'face face buffer)
        (emacs-buffer-remove-text-properties beg end '(face) buffer)))))

(defun emacs-isearch--clear-highlight ()
  "Remove the active isearch highlight and restore prior face state."
  (when (and emacs-isearch--buffer
             emacs-isearch--match-beg
             emacs-isearch--match-end
             (> emacs-isearch--match-end emacs-isearch--match-beg))
    (emacs-isearch--restore-face-runs emacs-isearch--buffer
                                      emacs-isearch--saved-face-runs))
  (setq emacs-isearch--match-beg nil
        emacs-isearch--match-end nil
        emacs-isearch--saved-face-runs nil))

(defun emacs-isearch--highlight-match (beg end)
  "Highlight [BEG, END) as the current isearch match."
  (emacs-isearch--clear-highlight)
  (setq emacs-isearch--saved-face-runs
        (emacs-isearch--snapshot-face-runs beg end emacs-isearch--buffer))
  (emacs-buffer-put-text-property beg end 'face
                                  emacs-isearch-highlight-face
                                  emacs-isearch--buffer)
  (setq emacs-isearch--match-beg beg
        emacs-isearch--match-end end))

(defun emacs-isearch--search-forward (query)
  "Run a forward literal search for QUERY in the current nelisp buffer."
  (if (nelisp-ec-current-buffer)
      (nelisp-ec-search-forward query nil t)
    (search-forward query nil t)))

(defun emacs-isearch--search-backward (query)
  "Run a backward literal search for QUERY in the current nelisp buffer."
  (if (nelisp-ec-current-buffer)
      (nelisp-ec-search-backward query nil t)
    (search-backward query nil t)))

(defun emacs-isearch--search-current (query direction)
  "Search QUERY in DIRECTION from point in the active buffer.
Return non-nil on success and update the temporary highlight."
  (let ((len (length query)))
    (when (> len 0)
      (let* ((found (pcase direction
                      ('backward (emacs-isearch--search-backward query))
                      (_ (emacs-isearch--search-forward query))))
             (beg (and found
                       (if (eq direction 'backward)
                           found
                         (- found len))))
             (end (and found (+ beg len))))
        (when found
          (emacs-isearch--highlight-match beg end))
        found))))

(defun emacs-isearch-restore-start-direct (start-point)
  "Restore point to START-POINT and return a movement plist."
  (nelisp-ec-goto-char start-point)
  (list :status 'restored
        :point start-point
        :failing nil))

(defun emacs-isearch-search-from-start-direct (query direction start-point)
  "Search QUERY in DIRECTION after resetting point to START-POINT.
DIRECTION is `forward' or `backward'.  Empty QUERY only restores point.
The returned plist contains `:status', `:found', `:failing', and
`:point'.  Failed searches restore point to START-POINT."
  (cond
   ((or (null query) (= (length query) 0))
    (emacs-isearch-restore-start-direct start-point)
    (list :status 'empty
          :query query
          :direction direction
          :start-point start-point
          :found nil
          :failing nil
          :point (nelisp-ec-point)))
   (t
    (emacs-isearch-restore-start-direct start-point)
    (let ((found
           (condition-case nil
               (pcase direction
                 ('backward (emacs-isearch--search-backward query))
                 (_ (emacs-isearch--search-forward query)))
             (error nil))))
      (cond
       (found
        (list :status 'found
              :query query
              :direction direction
              :start-point start-point
              :found found
              :failing nil
              :point (nelisp-ec-point)))
       (t
        (emacs-isearch-restore-start-direct start-point)
        (list :status 'failing
              :query query
              :direction direction
              :start-point start-point
              :found nil
              :failing t
              :point (nelisp-ec-point))))))))

(defun emacs-isearch-repeat-direct (query direction)
  "Repeat QUERY search from current point in DIRECTION.
Return a plist with `:status', `:found', `:failing', and `:point'.  When
QUERY is empty or no match is found, point is left at the original
position."
  (let ((origin (nelisp-ec-point)))
    (cond
     ((or (null query) (= (length query) 0))
      (list :status 'empty
            :query query
            :direction direction
            :found nil
            :failing nil
            :point origin))
     (t
      (let ((found
             (condition-case nil
                 (pcase direction
                   ('backward (emacs-isearch--search-backward query))
                   (_ (emacs-isearch--search-forward query)))
               (error nil))))
        (cond
         (found
          (list :status 'found
                :query query
                :direction direction
                :found found
                :failing nil
                :point (nelisp-ec-point)))
         (t
          (nelisp-ec-goto-char origin)
          (list :status 'failing
                :query query
                :direction direction
                :found nil
                :failing t
                :point origin))))))))

(defun emacs-isearch--search-from-start ()
  "Restart the current query from `emacs-isearch--start-point'."
  (emacs-isearch--with-buffer
   emacs-isearch--buffer
   (lambda ()
     (if (= (length emacs-isearch--query) 0)
         (progn
           (nelisp-ec-goto-char emacs-isearch--start-point)
           (setq emacs-isearch--failing nil)
           (emacs-isearch--clear-highlight)
           t)
       (let ((origin emacs-isearch--start-point))
         (nelisp-ec-goto-char origin)
         (let ((found (emacs-isearch--search-current
                       emacs-isearch--query
                       emacs-isearch--direction)))
           (setq emacs-isearch--failing (null found))
           (unless found
             (nelisp-ec-goto-char origin)
             (emacs-isearch--clear-highlight))
           found))))))

(defun emacs-isearch--repeat-search (direction)
  "Repeat the active search in DIRECTION."
  (setq emacs-isearch--direction direction)
  (if (= (length emacs-isearch--query) 0)
      (setq emacs-isearch--failing nil)
    (emacs-isearch--with-buffer
     emacs-isearch--buffer
     (lambda ()
       (let ((result (emacs-isearch-repeat-direct
                      emacs-isearch--query
                      emacs-isearch--direction)))
         (setq emacs-isearch--failing
               (plist-get result :failing)))))))

(defun emacs-isearch--append-char (event)
  "Append EVENT as a character to the query and restart the search."
  (setq emacs-isearch--query
        (concat emacs-isearch--query (char-to-string event)))
  (emacs-isearch--search-from-start))

(defun emacs-isearch--delete-char ()
  "Delete the last character from the query and restart."
  (when (> (length emacs-isearch--query) 0)
    (setq emacs-isearch--query
          (substring emacs-isearch--query 0 (1- (length emacs-isearch--query)))))
  (emacs-isearch--search-from-start))

(defun emacs-isearch--dispatch-event (event)
  "Handle one isearch EVENT.
Return `commit', `abort', or `continue'."
  (cond
   ((or (eq event 'return) (eq event 'enter) (eq event ?\r))
    'commit)
   ((or (eq event ?\C-g) (eq event 'escape))
    'abort)
   ((or (eq event ?\C-s) (eq event 'isearch-forward))
    (emacs-isearch--repeat-search 'forward)
    'continue)
   ((or (eq event ?\C-r) (eq event 'isearch-backward))
    (emacs-isearch--repeat-search 'backward)
    'continue)
   ((or (eq event ?\d) (eq event 127) (eq event 'backspace) (eq event 'delete))
    (emacs-isearch--delete-char)
    'continue)
   ((and (integerp event)
         (>= event 32)
         (< event 127))
    (emacs-isearch--append-char event)
    'continue)
   (t 'continue)))

(defun emacs-isearch--finish (result)
  "Tear down the active session and return RESULT."
  (let ((final-query emacs-isearch--query))
    (when (eq result 'abort)
      (emacs-isearch--with-buffer
       emacs-isearch--buffer
       (lambda ()
         (nelisp-ec-goto-char emacs-isearch--start-point))))
    (emacs-isearch--clear-highlight)
    (setq emacs-isearch--active nil
          emacs-isearch--buffer nil
          emacs-isearch--direction 'forward
          emacs-isearch--query ""
          emacs-isearch--start-point nil
          emacs-isearch--failing nil)
    (emacs-minibuffer-minibuffer-message
     "%s"
     (if (eq result 'abort)
         "I-search aborted"
       (format "I-search: %s" final-query)))
    (if (eq result 'abort) nil final-query)))

(defun emacs-isearch--run (direction)
  "Run an incremental search session in DIRECTION."
  (setq emacs-isearch--active t
        emacs-isearch--buffer (emacs-isearch--current-buffer)
        emacs-isearch--direction direction
        emacs-isearch--query ""
        emacs-isearch--start-point
        (emacs-isearch--with-buffer emacs-isearch--buffer #'nelisp-ec-point)
        emacs-isearch--failing nil)
  (emacs-isearch--clear-highlight)
  (catch 'done
    (while emacs-isearch--active
      (emacs-isearch--display-prompt)
      (let ((event (condition-case nil
                       (emacs-minibuffer-read-key emacs-isearch--last-prompt)
                     (quit ?\C-g))))
        (pcase (emacs-isearch--dispatch-event event)
          ('commit (throw 'done (emacs-isearch--finish 'commit)))
          ('abort (throw 'done (emacs-isearch--finish 'abort))))))
    nil))

(defun emacs-isearch-reset ()
  "Reset all isearch module state.
Test-only helper."
  (emacs-isearch--clear-highlight)
  (setq emacs-isearch--active nil
        emacs-isearch--buffer nil
        emacs-isearch--direction 'forward
        emacs-isearch--query ""
        emacs-isearch--start-point nil
        emacs-isearch--failing nil
        emacs-isearch--last-prompt ""))

;;;###autoload
(defun isearch-forward (&optional regexp-p no-recursive-edit)
  "Incrementally search forward from point.
REGEXP-P and NO-RECURSIVE-EDIT are accepted for API compatibility and
ignored by this literal-search MVP."
  (interactive)
  (ignore regexp-p no-recursive-edit)
  (emacs-isearch--run 'forward))

;;;###autoload
(defun isearch-backward (&optional regexp-p no-recursive-edit)
  "Incrementally search backward from point.
REGEXP-P and NO-RECURSIVE-EDIT are accepted for API compatibility and
ignored by this literal-search MVP."
  (interactive)
  (ignore regexp-p no-recursive-edit)
  (emacs-isearch--run 'backward))

(emacs-isearch--ensure-face)
(emacs-isearch--ensure-global-bindings)

(provide 'emacs-isearch)

;;; emacs-isearch.el ends here
