;;; lisp.el --- lightweight Lisp editing commands for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; GNU Emacs's emacs-lisp/lisp.el does not provide the feature `lisp',
;; but the vendor Class-A smoke lane intentionally requires that feature
;; as a stable NeLisp compatibility surface.  This facade exposes the
;; common Lisp editing commands without pulling in the full vendor file.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-line-builtins)
(require 'emacs-edit-builtins)

(defun lisp--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this facade."
  (if (not (boundp 'emacs-version))
      t
    (not (fboundp symbol))))

(unless (boundp 'defun-prompt-regexp)
  (defvar defun-prompt-regexp nil
    "Regexp to ignore before a defun opener."))

(unless (boundp 'parens-require-spaces)
  (defvar parens-require-spaces t
    "Non-nil means pair insertion adds spacing when needed."))

(unless (boundp 'forward-sexp-function)
  (defvar forward-sexp-function nil
    "Function used by `forward-sexp' when non-nil."))

(unless (boundp 'beginning-of-defun-function)
  (defvar beginning-of-defun-function nil
    "Function used by `beginning-of-defun' when non-nil."))

(unless (boundp 'end-of-defun-function)
  (defvar end-of-defun-function nil
    "Function used by `end-of-defun' when non-nil."))

(unless (boundp 'end-of-defun-moves-to-eol)
  (defvar end-of-defun-moves-to-eol t
    "Non-nil means `end-of-defun' moves to end of line after a form."))

(unless (boundp 'narrow-to-defun-include-comments)
  (defvar narrow-to-defun-include-comments nil
    "Compatibility variable accepted by `narrow-to-defun'."))

(unless (boundp 'insert-pair-alist)
  (defvar insert-pair-alist
    '((?\( ?\( . ?\)) (?\[ ?\[ . ?\]) (?\{ ?\{ . ?\})
      (?\" ?\" . ?\") (?\' ?\' . ?\'))
    "Pairs used by `insert-pair' when CLOSE is omitted."))

(unless (boundp 'delete-pair-blink-delay)
  (defvar delete-pair-blink-delay nil
    "Compatibility variable for `delete-pair'."))

(unless (boundp 'lisp--mark)
  (defvar lisp--mark nil
    "Fallback mark position for standalone NeLisp."))

(unless (boundp 'mark-active)
  (defvar mark-active nil
    "Fallback active-mark flag for standalone NeLisp."))

(defun lisp--char-at (pos)
  "Return character at POS, or nil when POS is outside the buffer."
  (when (and (>= pos (point-min)) (< pos (point-max)))
    (let ((s (buffer-substring-no-properties pos (1+ pos))))
      (and (> (length s) 0) (aref s 0)))))

(defun lisp--space-char-p (char)
  "Return non-nil when CHAR is simple whitespace."
  (memq char '(?\s ?\t ?\n ?\r)))

(defun lisp--open-char-p (char)
  "Return non-nil when CHAR opens a list."
  (memq char '(?\( ?\[ ?\{)))

(defun lisp--close-char-p (char)
  "Return non-nil when CHAR closes a list."
  (memq char '(?\) ?\] ?\})))

(defun lisp--matching-close (open)
  "Return the close delimiter matching OPEN."
  (cdr (assq open '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))))

(defun lisp--matching-open (close)
  "Return the open delimiter matching CLOSE."
  (cdr (assq close '((?\) . ?\() (?\] . ?\[) (?\} . ?\{)))))

(defun lisp--symbol-char-p (char)
  "Return non-nil when CHAR can be part of a lightweight Lisp atom."
  (and char
       (not (lisp--space-char-p char))
       (not (memq char '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\" ?\;)))))

(defun lisp--prefix-char-p (char)
  "Return non-nil when CHAR prefixes the next sexp."
  (memq char '(?\' ?` ?,)))

(defun lisp--skip-forward-trivia (&optional limit)
  "Move point over whitespace and line comments up to LIMIT."
  (let ((p (point))
        (end (or limit (point-max))))
    (catch 'done
      (while (< p end)
        (let ((ch (lisp--char-at p)))
          (cond
           ((lisp--space-char-p ch)
            (setq p (1+ p)))
           ((eq ch ?\;)
            (while (and (< p end)
                        (not (eq (lisp--char-at p) ?\n)))
              (setq p (1+ p))))
           (t
            (throw 'done nil))))))
    (goto-char (min p end))))

(defun lisp--skip-backward-trivia (&optional limit)
  "Move point backward over whitespace down to LIMIT."
  (let ((p (point))
        (start (or limit (point-min))))
    (while (and (> p start)
                (lisp--space-char-p (lisp--char-at (1- p))))
      (setq p (1- p)))
    (goto-char (max p start))))

(defun lisp--scan-string-forward (pos limit)
  "Return position after string at POS, or nil before LIMIT."
  (let ((p (1+ pos))
        found)
    (catch 'done
      (while (< p limit)
        (let ((ch (lisp--char-at p)))
          (cond
           ((eq ch ?\\)
            (setq p (+ p 2)))
           ((eq ch ?\")
            (setq found (1+ p))
            (throw 'done nil))
           (t
            (setq p (1+ p)))))))
    found))

(defun lisp--scan-atom-forward (pos limit)
  "Return position after atom at POS before LIMIT."
  (let ((p pos))
    (while (and (< p limit)
                (lisp--symbol-char-p (lisp--char-at p)))
      (setq p (1+ p)))
    p))

(defun lisp--scan-one-forward-at (pos limit)
  "Return end position of one sexp starting at POS, or nil."
  (let ((ch (lisp--char-at pos)))
    (cond
     ((null ch) nil)
     ((lisp--space-char-p ch)
      (save-excursion
        (goto-char pos)
        (lisp--skip-forward-trivia limit)
        (lisp--scan-one-forward-at (point) limit)))
     ((eq ch ?\;)
      (save-excursion
        (goto-char pos)
        (lisp--skip-forward-trivia limit)
        (lisp--scan-one-forward-at (point) limit)))
     ((lisp--prefix-char-p ch)
      (lisp--scan-one-forward-at (1+ pos) limit))
     ((and (eq ch ?#)
           (< (1+ pos) limit)
           (lisp--prefix-char-p (lisp--char-at (1+ pos))))
      (lisp--scan-one-forward-at (+ pos 2) limit))
     ((eq ch ?\")
      (lisp--scan-string-forward pos limit))
     ((lisp--open-char-p ch)
      (let ((close (lisp--matching-close ch))
            (p (1+ pos))
            done)
        (catch 'done
          (while (< p limit)
            (let ((c (lisp--char-at p)))
              (cond
               ((eq c close)
                (setq done (1+ p))
                (throw 'done nil))
               ((lisp--close-char-p c)
                (throw 'done nil))
               ((or (lisp--open-char-p c)
                    (eq c ?\")
                    (lisp--prefix-char-p c)
                    (and (eq c ?#)
                         (< (1+ p) limit)
                         (lisp--prefix-char-p (lisp--char-at (1+ p)))))
                (let ((next (lisp--scan-one-forward-at p limit)))
                  (unless next
                    (throw 'done nil))
                  (setq p next)))
               ((eq c ?\;)
                (while (and (< p limit)
                            (not (eq (lisp--char-at p) ?\n)))
                  (setq p (1+ p))))
               (t
                (setq p (1+ p)))))))
        done))
     ((lisp--close-char-p ch)
      nil)
     (t
      (lisp--scan-atom-forward pos limit)))))

(defun lisp--scan-sexp-forward (&optional limit)
  "Move over one sexp and return point, or nil on failure."
  (let ((start (point))
        (end (or limit (point-max))))
    (lisp--skip-forward-trivia end)
    (let ((next (lisp--scan-one-forward-at (point) end)))
      (if next
          (progn (goto-char next) next)
        (goto-char start)
        nil))))

(defun lisp--scan-string-backward (pos limit)
  "Return start of string ending before POS, or nil."
  (let ((p (- pos 2))
        found)
    (catch 'done
      (while (>= p limit)
        (let ((ch (lisp--char-at p)))
          (cond
           ((eq ch ?\")
            (setq found p)
            (throw 'done nil))
           (t
            (setq p (1- p)))))))
    found))

(defun lisp--scan-list-backward (pos limit)
  "Return start of list ending before POS, or nil."
  (let* ((close (lisp--char-at (1- pos)))
         (open (lisp--matching-open close))
         (depth 1)
         (p (1- pos))
         found)
    (catch 'done
      (while (> p limit)
        (setq p (1- p))
        (let ((ch (lisp--char-at p)))
          (cond
           ((eq ch close)
            (setq depth (1+ depth)))
           ((eq ch open)
            (setq depth (1- depth))
            (when (= depth 0)
              (setq found p)
              (throw 'done nil)))))))
    found))

(defun lisp--scan-atom-backward (pos limit)
  "Return start of atom ending at POS."
  (let ((p (1- pos)))
    (while (and (> p limit)
                (lisp--symbol-char-p (lisp--char-at (1- p))))
      (setq p (1- p)))
    (while (and (> p limit)
                (lisp--prefix-char-p (lisp--char-at (1- p))))
      (setq p (1- p)))
    (when (and (> p limit)
               (eq (lisp--char-at (1- p)) ?#))
      (setq p (1- p)))
    p))

(defun lisp--scan-sexp-backward (&optional limit)
  "Move backward over one sexp and return point, or nil on failure."
  (let ((start (point))
        (minpos (or limit (point-min))))
    (lisp--skip-backward-trivia minpos)
    (let* ((end (point))
           (ch (and (> end minpos) (lisp--char-at (1- end))))
           (prev
            (cond
             ((null ch) nil)
             ((lisp--close-char-p ch)
              (lisp--scan-list-backward end minpos))
             ((eq ch ?\")
              (lisp--scan-string-backward end minpos))
             ((lisp--symbol-char-p ch)
              (lisp--scan-atom-backward end minpos))
             (t
              (1- end)))))
      (if prev
          (progn (goto-char prev) prev)
        (goto-char start)
        nil))))

(when (lisp--install-function-p 'buffer-end)
  (defun buffer-end (arg)
    "Return `point-max' when ARG is positive, otherwise `point-min'."
    (if (> (or arg 1) 0) (point-max) (point-min))))

(when (lisp--install-function-p 'forward-sexp-default-function)
  (defun forward-sexp-default-function (&optional arg)
    "Default implementation for `forward-sexp-function'."
    (forward-sexp arg)))

(when (lisp--install-function-p 'forward-sexp)
  (defun forward-sexp (&optional arg interactive)
    "Move forward across ARG balanced expressions."
    (interactive "p")
    (let ((count (or arg 1)))
      (cond
       ((and forward-sexp-function (not interactive))
        (funcall forward-sexp-function count))
       ((= count 0)
        nil)
       ((> count 0)
        (while (> count 0)
          (unless (lisp--scan-sexp-forward)
            (signal 'scan-error (list "No next sexp" (point) (point-max))))
          (setq count (1- count))))
       (t
        (while (< count 0)
          (unless (lisp--scan-sexp-backward)
            (signal 'scan-error (list "No previous sexp" (point-min) (point))))
          (setq count (1+ count))))))))

(when (lisp--install-function-p 'backward-sexp)
  (defun backward-sexp (&optional arg interactive)
    "Move backward across ARG balanced expressions."
    (interactive "p")
    (forward-sexp (- (or arg 1)) interactive)))

(when (lisp--install-function-p 'forward-list)
  (defun forward-list (&optional arg interactive)
    "Move forward across ARG parenthesized groups."
    (interactive "p")
    (ignore interactive)
    (forward-sexp (or arg 1))))

(when (lisp--install-function-p 'backward-list)
  (defun backward-list (&optional arg interactive)
    "Move backward across ARG parenthesized groups."
    (interactive "p")
    (ignore interactive)
    (forward-sexp (- (or arg 1)))))

(when (lisp--install-function-p 'down-list)
  (defun down-list (&optional arg interactive)
    "Move forward into ARG nested lists."
    (interactive "p")
    (ignore interactive)
    (let ((count (or arg 1)))
      (while (> count 0)
        (let ((p (point))
              (end (point-max))
              found)
          (catch 'done
            (while (< p end)
              (when (lisp--open-char-p (lisp--char-at p))
                (setq found (1+ p))
                (throw 'done nil))
              (setq p (1+ p))))
          (unless found
            (signal 'scan-error (list "No containing list" (point) end)))
          (goto-char found))
        (setq count (1- count))))))

(when (lisp--install-function-p 'up-list)
  (defun up-list (&optional arg escape-strings no-syntax-crossing)
    "Move forward out of ARG containing lists."
    (interactive "p")
    (ignore escape-strings no-syntax-crossing)
    (let ((count (or arg 1)))
      (while (> count 0)
        (let ((p (point))
              (end (point-max))
              found)
          (catch 'done
            (while (< p end)
              (let ((ch (lisp--char-at p)))
                (cond
                 ((eq ch ?\")
                  (let ((next (lisp--scan-string-forward p end)))
                    (setq p (or next end))))
                 ((lisp--close-char-p ch)
                  (setq found (1+ p))
                  (throw 'done nil))
                 (t
                  (setq p (1+ p)))))))
          (unless found
            (signal 'scan-error (list "No containing list" (point) end)))
          (goto-char found))
        (setq count (1- count))))))

(when (lisp--install-function-p 'backward-up-list)
  (defun backward-up-list (&optional arg escape-strings no-syntax-crossing)
    "Move backward out of ARG containing lists."
    (interactive "p")
    (ignore escape-strings no-syntax-crossing)
    (let ((count (or arg 1)))
      (while (> count 0)
        (let ((p (point))
              (minpos (point-min))
              found)
          (catch 'done
            (while (> p minpos)
              (setq p (1- p))
              (when (lisp--open-char-p (lisp--char-at p))
                (setq found p)
                (throw 'done nil))))
          (unless found
            (signal 'scan-error (list "No containing list" minpos (point))))
          (goto-char found))
        (setq count (1- count))))))

(when (lisp--install-function-p 'kill-sexp)
  (defun kill-sexp (&optional arg interactive)
    "Kill ARG sexps after point."
    (interactive "p")
    (ignore interactive)
    (let ((start (point)))
      (forward-sexp (or arg 1))
      (kill-region start (point)))))

(when (lisp--install-function-p 'backward-kill-sexp)
  (defun backward-kill-sexp (&optional arg interactive)
    "Kill ARG sexps before point."
    (interactive "p")
    (ignore interactive)
    (kill-sexp (- (or arg 1)))))

(when (lisp--install-function-p 'kill-backward-up-list)
  (defun kill-backward-up-list (&optional arg)
    "Kill text backward to the start of an enclosing list."
    (interactive "p")
    (let ((end (point)))
      (backward-up-list (or arg 1))
      (kill-region (point) end))))

(defun lisp--beginning-of-defun-once ()
  "Move to the previous top-level form opener."
  (let ((p (point))
        found)
    (catch 'done
      (while (> p (point-min))
        (setq p (1- p))
        (when (and (eq (lisp--char-at p) ?\()
                   (or (= p (point-min))
                       (eq (lisp--char-at (1- p)) ?\n)))
          (setq found p)
          (throw 'done nil))))
    (goto-char (or found (point-min)))))

(when (lisp--install-function-p 'beginning-of-defun)
  (defun beginning-of-defun (&optional arg)
    "Move to the beginning of ARG top-level forms."
    (interactive "p")
    (let ((count (or arg 1)))
      (cond
       (beginning-of-defun-function
        (funcall beginning-of-defun-function count))
       ((>= count 0)
        (while (> count 0)
          (lisp--beginning-of-defun-once)
          (setq count (1- count))))
       (t
        (end-of-defun (- count)))))))

(when (lisp--install-function-p 'beginning-of-defun-raw)
  (defun beginning-of-defun-raw (&optional arg)
    "Raw alias for `beginning-of-defun'."
    (beginning-of-defun arg)))

(when (lisp--install-function-p 'beginning-of-defun-comments)
  (defun beginning-of-defun-comments (&optional arg)
    "Compatibility alias for `beginning-of-defun'."
    (beginning-of-defun arg)))

(when (lisp--install-function-p 'end-of-defun)
  (defun end-of-defun (&optional arg interactive)
    "Move to the end of ARG top-level forms."
    (interactive "p")
    (ignore interactive)
    (let ((count (or arg 1)))
      (cond
       (end-of-defun-function
        (funcall end-of-defun-function count))
       ((>= count 0)
        (while (> count 0)
          (beginning-of-defun 1)
          (forward-sexp 1)
          (when end-of-defun-moves-to-eol
            (end-of-line))
          (setq count (1- count))))
       (t
        (beginning-of-defun (- count)))))))

(when (lisp--install-function-p 'mark)
  (defun mark (&optional force)
    "Return fallback mark position."
    (if (or lisp--mark force)
        lisp--mark
      (signal 'mark-inactive nil))))

(when (lisp--install-function-p 'set-mark)
  (defun set-mark (pos)
    "Set fallback mark to POS."
    (setq lisp--mark pos
          mark-active t)
    nil))

(when (lisp--install-function-p 'push-mark)
  (defun push-mark (&optional location nomsg activate)
    "Set fallback mark to LOCATION or point."
    (ignore nomsg)
    (set-mark (or location (point)))
    (setq mark-active (or activate mark-active))
    nil))

;; Doc 33 item 244: `region-active-p'/`use-region-p' (simple.el, preloaded
;; in real Emacs so absent from vendor bundles) are read by Magit's
;; section-highlight machinery on every status refresh
;; (`magit-section--refresh-region' path) — void without this.  Built on
;; the same fallback mark this file already owns; there is no
;; `transient-mark-mode' toggle in the standalone substrate, so an active
;; fallback mark is the whole truth.

(when (lisp--install-function-p 'region-active-p)
  (defun region-active-p ()
    "Return non-nil when the fallback mark is active."
    (and mark-active lisp--mark t)))

(when (lisp--install-function-p 'use-region-p)
  (defun use-region-p ()
    "Return non-nil when the active fallback region should be used."
    (region-active-p)))

(when (lisp--install-function-p 'region-beginning)
  (defun region-beginning ()
    "Return the smaller of point and the fallback mark."
    (min (point) (or lisp--mark (point)))))

(when (lisp--install-function-p 'region-end)
  (defun region-end ()
    "Return the larger of point and the fallback mark."
    (max (point) (or lisp--mark (point)))))

(when (lisp--install-function-p 'deactivate-mark)
  (defun deactivate-mark (&optional _force)
    "Deactivate the fallback mark."
    (setq mark-active nil)
    nil))

(when (lisp--install-function-p 'mark-sexp)
  (defun mark-sexp (&optional arg allow-extend)
    "Set mark ARG sexps from point."
    (interactive "p")
    (ignore allow-extend)
    (push-mark
     (save-excursion
       (forward-sexp (or arg 1))
       (point))
     nil t)))

(when (lisp--install-function-p 'mark-defun)
  (defun mark-defun (&optional arg interactive)
    "Set mark around ARG defuns."
    (interactive "p")
    (ignore interactive)
    (let ((start (save-excursion
                   (beginning-of-defun 1)
                   (point)))
          (end (save-excursion
                 (end-of-defun (or arg 1))
                 (point))))
      (goto-char start)
      (push-mark end nil t))))

(when (lisp--install-function-p 'narrow-to-defun)
  (defun narrow-to-defun (&optional include-comments)
    "Narrow buffer to the current defun."
    (interactive "P")
    (ignore include-comments narrow-to-defun-include-comments)
    (let ((start (save-excursion
                   (beginning-of-defun 1)
                   (point)))
          (end (save-excursion
                 (end-of-defun 1)
                 (point))))
      (narrow-to-region start end))))

(when (lisp--install-function-p 'insert-pair)
  (defun insert-pair (&optional arg open close)
    "Insert OPEN and CLOSE around point or ARG following sexps."
    (interactive "P")
    (let* ((open-char (or open ?\())
           (pair (or close (cdr (assq open-char insert-pair-alist))))
           (close-char (cond
                        ((consp pair) (cdr pair))
                        ((integerp pair) pair)
                        (t ?\)))))
      (insert (string open-char))
      (let ((mid (point)))
        (when arg
          (forward-sexp (prefix-numeric-value arg)))
        (insert (string close-char))
        (unless arg
          (goto-char mid))))))

(when (lisp--install-function-p 'insert-parentheses)
  (defun insert-parentheses (&optional arg)
    "Insert parentheses around point or ARG following sexps."
    (interactive "P")
    (insert-pair arg ?\( ?\))))

(when (lisp--install-function-p 'delete-pair)
  (defun delete-pair (&optional arg)
    "Delete ARG pairs around point."
    (interactive "p")
    (let ((count (or arg 1)))
      (while (> count 0)
        (let* ((open-pos (point))
               (open (lisp--char-at open-pos))
               (end (and (lisp--open-char-p open)
                         (lisp--scan-one-forward-at open-pos (point-max)))))
          (unless end
            (signal 'scan-error (list "No pair at point" open-pos (point-max))))
          (delete-region (1- end) end)
          (delete-region open-pos (1+ open-pos)))
        (setq count (1- count))))))

(when (lisp--install-function-p 'raise-sexp)
  (defun raise-sexp (&optional n)
    "Replace the containing list with the next N sexps."
    (interactive "p")
    (let ((start (save-excursion
                   (backward-up-list 1)
                   (point)))
          (end (save-excursion
                 (up-list 1)
                 (point)))
          (sexp-start (point))
          sexp-end text)
      (forward-sexp (or n 1))
      (setq sexp-end (point)
            text (buffer-substring-no-properties sexp-start sexp-end))
      (delete-region start end)
      (insert text))))

(when (lisp--install-function-p 'move-past-close-and-reindent)
  (defun move-past-close-and-reindent ()
    "Move past a closing delimiter."
    (interactive)
    (when (lisp--close-char-p (lisp--char-at (point)))
      (forward-char 1))
    nil))

(defun lisp--check-parens-range (start end)
  "Return nil when START..END has balanced delimiters, else error."
  (let ((p start)
        stack)
    (while (< p end)
      (let ((ch (lisp--char-at p)))
        (cond
         ((eq ch ?\")
          (let ((next (lisp--scan-string-forward p end)))
            (unless next
              (signal 'scan-error (list "Unmatched string quote" p end)))
            (setq p (1- next))))
         ((eq ch ?\;)
          (while (and (< p end)
                      (not (eq (lisp--char-at p) ?\n)))
            (setq p (1+ p))))
         ((lisp--open-char-p ch)
          (setq stack (cons ch stack)))
         ((lisp--close-char-p ch)
          (let ((open (lisp--matching-open ch)))
            (unless (and stack (eq (car stack) open))
              (signal 'scan-error (list "Unmatched closing delimiter" p end)))
            (setq stack (cdr stack))))))
      (setq p (1+ p)))
    (when stack
      (signal 'scan-error (list "Unmatched opening delimiter" start end))))
  nil)

(when (lisp--install-function-p 'check-parens)
  (defun check-parens ()
    "Signal `scan-error' when the current buffer has unmatched delimiters."
    (interactive)
    (lisp--check-parens-range (point-min) (point-max))))

(when (lisp--install-function-p 'field-complete)
  (defun field-complete (table &optional predicate)
    "Compatibility wrapper around `completion-in-region' when available."
    (ignore table predicate)
    (when (fboundp 'completion-at-point)
      (completion-at-point))))

(when (lisp--install-function-p 'lisp-complete-symbol)
  (defun lisp-complete-symbol (&optional predicate)
    "Complete the Lisp symbol at point when completion support exists."
    (interactive)
    (ignore predicate)
    (when (fboundp 'completion-at-point)
      (completion-at-point))))

(provide 'lisp)

;;; lisp.el ends here
