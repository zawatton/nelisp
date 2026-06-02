;;; map-ynp.el --- lightweight boolean question helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Emacs uses `map-y-or-n-p' for save/recover/delete confirmation loops
;; and `read-answer' for small answer sets.  The upstream map-ynp.el
;; also handles dialog boxes, query-replace keymaps, and help windows.
;; This facade keeps the daily-driver semantics that matter on the
;; pure-Elisp NeLisp path: deterministic y/n prompting, answer-all,
;; act-once-and-exit, custom action keys, and long/short answer reading.

;;; Code:

(defvar read-answer-short 'auto
  "Non-nil means `read-answer' accepts short answers.")

(defvar read-answer-map--memoize nil
  "Compatibility variable for callers that expect map-ynp.el to bind it.")

(defun map-y-or-n-p--next (state)
  "Return next object from STATE, or the sentinel `:map-y-or-n-p-end'."
  (let ((source (car state)))
    (cond
     ((functionp source)
      (let ((value (funcall source)))
        (if value value :map-y-or-n-p-end)))
     ((consp source)
      (let ((value (car source)))
        (setcar state (cdr source))
        value))
     (t :map-y-or-n-p-end))))

(defun map-y-or-n-p--prompt (prompter object)
  "Return PROMPTER result for OBJECT."
  (if (stringp prompter)
      (format prompter object)
    (funcall prompter object)))

(defun map-y-or-n-p--read-event (prompt action-alist)
  "Read one answer event for PROMPT and ACTION-ALIST."
  (let ((suffix (if action-alist
                    (concat
                     (mapconcat (lambda (entry)
                                  (char-to-string (car entry)))
                                action-alist
                                ", ")
                     ", ")
                  "")))
    (cond
     ((fboundp 'read-key)
      (read-key (format "%s(y, n, !, ., q, %s?) " prompt suffix)))
     ((fboundp 'read-char)
      (read-char (format "%s(y, n, !, ., q, %s?) " prompt suffix)))
     (t
      (let ((answer (read-from-minibuffer
                     (format "%s(y, n, !, ., q, %s?) " prompt suffix))))
        (if (> (length answer) 0) (aref answer 0) ?n))))))

(defun map-y-or-n-p (prompter actor list &optional _help action-alist
                              _no-cursor-in-echo-area)
  "Ask a boolean question for each object in LIST and call ACTOR.
PROMPTER is a format string or a function of one object.  ACTOR is
called for every object accepted by the user.  Return the number of
actions taken."
  (let ((state (list list))
        (actions 0)
        automatic
        done)
    (while (not done)
      (let ((object (map-y-or-n-p--next state)))
        (if (eq object :map-y-or-n-p-end)
            (setq done t)
          (let ((prompt (map-y-or-n-p--prompt prompter object)))
            (cond
             ((null prompt))
             ((not (stringp prompt))
              (funcall actor object)
              (setq actions (1+ actions)))
             (automatic
              (funcall actor object)
              (setq actions (1+ actions)))
             (t
              (let ((answered nil))
                (while (not answered)
                  (let* ((event (map-y-or-n-p--read-event prompt action-alist))
                         (custom (assq event action-alist)))
                    (cond
                     ((memq event '(?y ?Y ?\s))
                      (funcall actor object)
                      (setq actions (1+ actions)
                            answered t))
                     ((memq event '(?n ?N ?\177))
                      (setq answered t))
                     ((eq event ?!)
                      (funcall actor object)
                      (setq actions (1+ actions)
                            automatic t
                            answered t))
                     ((eq event ?.)
                      (funcall actor object)
                      (setq actions (1+ actions)
                            answered t
                            done t))
                     ((memq event '(?q ?Q ?\e ?\r ?\n))
                      (setq answered t
                            done t))
                     (custom
                      (when (funcall (cadr custom) object)
                        (setq actions (1+ actions)
                              answered t)))
                     ((eq event ?\C-g)
                      (signal 'quit nil))
                     (t
                      (message "Please answer y, n, !, ., or q."))))))))))))
    actions))

(defun read-answer--short-p ()
  "Return non-nil when `read-answer' should accept short answers."
  (if (eq read-answer-short 'auto)
      (or (and (boundp 'use-short-answers) use-short-answers)
          (and (fboundp 'yes-or-no-p)
               (eq (symbol-function 'yes-or-no-p) 'y-or-n-p)))
    read-answer-short))

(defun read-answer--short-char (answer)
  "Return ANSWER's short character when it has one."
  (let ((short (cadr answer)))
    (cond
     ((characterp short) short)
     ((and (vectorp short) (= (length short) 1)) (aref short 0))
     (t nil))))

(defun read-answer--match (input answers short)
  "Return long answer matching INPUT in ANSWERS."
  (let ((down (and (stringp input) (downcase input)))
        found)
    (while (and answers (not found))
      (let* ((entry (car answers))
             (long (car entry))
             (char (read-answer--short-char entry)))
        (when (or (and down (string= down (downcase long)))
                  (and short down char (= (length down) 1)
                       (= (aref down 0) char)))
          (setq found long)))
      (setq answers (cdr answers)))
    found))

(defun read-answer (question answers)
  "Ask QUESTION and return one long answer from ANSWERS.
ANSWERS is an alist of (LONG-ANSWER SHORT-ANSWER HELP-MESSAGE)."
  (let* ((short (read-answer--short-p))
         (prompt (format "%s(%s) "
                         question
                         (mapconcat
                          (lambda (answer)
                            (if short
                                (let ((char (read-answer--short-char answer)))
                                  (if char (char-to-string char) (car answer)))
                              (car answer)))
                          answers
                          ", ")))
         result)
    (while (not result)
      (let ((input (if short
                       (let ((event (if (fboundp 'read-key)
                                        (read-key prompt)
                                      (read-char prompt))))
                         (char-to-string event))
                     (read-from-minibuffer prompt))))
        (setq result (read-answer--match input answers short))
        (unless result
          (message "Please answer one of: %s"
                   (mapconcat #'car answers ", ")))))
    result))

(provide 'map-ynp)

;;; map-ynp.el ends here
