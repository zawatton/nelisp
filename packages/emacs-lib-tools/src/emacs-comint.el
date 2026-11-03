;;; emacs-comint.el --- minimal command-interpreter (comint) machinery  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) comint semantics: the process-buffer machinery a
;; shell / REPL needs -- an output mark where subprocess output accumulates,
;; an input ring (history) with previous/next navigation, and `comint-send-input'
;; that lifts the pending input, records it, and sends it to the process.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the output-insertion mark, the input ring, the
;;     send-input lift/record/send semantics, and the buffer construction.
;;   - nelisp-gui OWNS: rendering and key transport.
;;   - nelisp (L1) OWNS: the subprocess substrate (`make-process' pipes).
;;
;; The machinery is deliberately process-optional: `emacs-comint-output-filter'
;; and the input ring operate on a buffer-local mark, so they run on the
;; standalone reader (whose `make-process' cannot yet hold an interactive
;; subprocess open).  A live subprocess round-trip works on host Emacs; on the
;; reader it is gated by that L1 substrate gap.
;;
;; Per-buffer state lives in a buffer-keyed hash table (mirrors `emacs-ielm');
;; `defvar-local' is avoided for standalone-reader portability.

;;; Code:

(defvar emacs-comint--state (make-hash-table :test 'equal)
  "Per-buffer comint state, keyed by buffer NAME.
Each value is a plist with keys:
- `:mark'       integer position where output is inserted / input begins
- `:ring'       newest-first list of submitted input strings
- `:ring-index' nil, or a zero-based index into `:ring' for prev/next
- `:process'    the associated process object, or nil

A buffer-name string key is used rather than the buffer object or a
`make-local-variable': on the standalone reader buffer objects are not
usable as `eq' hash keys, and buffer-local variables are not isolated
per buffer.  `comint-mode' resets the entry, so the killed-and-reused
temp-buffer name (e.g. \" *temp*\") never leaks state between sessions.")

(defun emacs-comint--key (buffer)
  "Return the state-hash key for BUFFER (default current)."
  (buffer-name (or buffer (current-buffer))))

(defun emacs-comint--state (buffer)
  "Return BUFFER's comint state, creating a default record when needed."
  (let ((k (emacs-comint--key buffer)))
    (or (gethash k emacs-comint--state)
        (puthash k
                 (list :mark 1 :ring nil :ring-index nil :process nil)
                 emacs-comint--state))))

(defun emacs-comint--set (buffer key value)
  "Store VALUE under KEY in BUFFER's comint state and return VALUE."
  (let ((state (copy-sequence (emacs-comint--state buffer))))
    (puthash (emacs-comint--key buffer)
             (plist-put state key value) emacs-comint--state)
    value))

(defun emacs-comint--get (key &optional buffer)
  "Return BUFFER's comint state value for KEY."
  (plist-get (emacs-comint--state (or buffer (current-buffer))) key))

;;;; --- output mark --------------------------------------------------

(defun emacs-comint--mark (&optional buffer)
  "Return the output/input boundary position for BUFFER.
When a live process is attached the process mark wins; otherwise the
buffer-local integer mark is used (defaulting to `point-max')."
  (with-current-buffer (or buffer (current-buffer))
    (let ((proc (emacs-comint--get :process)))
      (if (and proc (fboundp 'process-mark) (markerp (process-mark proc)))
          (marker-position (process-mark proc))
        (let ((m (emacs-comint--get :mark)))
          (if (integerp m) m (point-max)))))))

(defun emacs-comint--set-mark (pos &optional buffer)
  "Record POS as BUFFER's output/input boundary (and the process mark)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((proc (emacs-comint--get :process)))
      (when (and proc (fboundp 'process-mark) (markerp (process-mark proc)))
        (set-marker (process-mark proc) pos)))
    (emacs-comint--set (current-buffer) :mark pos)))

;;;; --- output -------------------------------------------------------

(defun emacs-comint-output-filter (process string)
  "Insert STRING at the comint mark of PROCESS's buffer; advance the mark.
PROCESS may be nil, in which case the current buffer and its buffer-local
mark are used -- this lets the output path run without a live subprocess."
  (let ((buffer (if (and process (fboundp 'process-buffer))
                    (process-buffer process)
                  (current-buffer))))
    (when (and buffer (buffer-live-p buffer) (stringp string))
      (with-current-buffer buffer
        (when (and process (not (eq (emacs-comint--get :process) process)))
          (emacs-comint--set buffer :process process))
        (save-excursion
          (let ((pos (emacs-comint--mark buffer)))
            (goto-char pos)
            (insert string)
            (emacs-comint--set-mark (point) buffer)))))))

;;;; --- input ring (history) -----------------------------------------

(defun emacs-comint--blank-p (string)
  "Non-nil when STRING is empty or only whitespace."
  (or (null string)
      (string-match-p "\\`[ \t\n\r]*\\'" string)))

(defun emacs-comint-add-to-input-history (cmd)
  "Push CMD onto the current buffer's input ring unless blank.
Resets the navigation index."
  (unless (emacs-comint--blank-p cmd)
    (emacs-comint--set (current-buffer) :ring
                       (cons cmd (emacs-comint--get :ring))))
  (emacs-comint--set (current-buffer) :ring-index nil)
  cmd)

(defun emacs-comint-input-ring ()
  "Return the current buffer's input ring (newest first)."
  (emacs-comint--get :ring))

(defun emacs-comint--navigate (n)
  "Move N steps through the input ring and replace the pending input.
Positive N walks toward older entries (previous-input)."
  (let* ((ring (emacs-comint--get :ring))
         (len (length ring)))
    (when (> len 0)
      (let ((idx (max 0 (min (1- len)
                             (+ (or (emacs-comint--get :ring-index) -1) n)))))
        (emacs-comint--set (current-buffer) :ring-index idx)
        (let ((mark (emacs-comint--mark))
              (entry (nth idx ring)))
          (delete-region mark (point-max))
          (goto-char (point-max))
          (insert entry)
          entry)))))

(defun emacs-comint-previous-input (n)
  "Replace the pending input with the Nth previous history entry."
  (interactive "p")
  (emacs-comint--navigate (or n 1)))

(defun emacs-comint-next-input (n)
  "Replace the pending input with the Nth next history entry."
  (interactive "p")
  (emacs-comint--navigate (- (or n 1))))

;;;; --- send ---------------------------------------------------------

(defun emacs-comint-send-string (process string)
  "Send STRING to PROCESS (a thin wrapper over `process-send-string')."
  (when (and process (fboundp 'process-send-string))
    (process-send-string process string)))

(defun emacs-comint-send-input ()
  "Lift the pending input (mark .. point-max), record it, and send it.
Returns the input string.  When a live process is attached, the input
plus a newline is written to it; otherwise only the buffer/ring state
advances (the reader's process substrate gap)."
  (interactive)
  (let* ((mark (emacs-comint--mark))
         (input (buffer-substring-no-properties mark (point-max)))
         (proc (emacs-comint--get :process)))
    (emacs-comint--set (current-buffer) :ring-index nil)
    (emacs-comint-add-to-input-history input)
    (goto-char (point-max))
    (insert "\n")
    (emacs-comint--set-mark (point-max))
    (when (and proc (fboundp 'process-send-string))
      (process-send-string proc (concat input "\n")))
    input))

;;;; --- mode + buffer construction -----------------------------------

(defun emacs-comint-mode ()
  "Put the current buffer into a minimal comint mode (reset state)."
  (interactive)
  (setq major-mode 'comint-mode
        mode-name "Comint")
  (puthash (emacs-comint--key (current-buffer))
           (list :mark (point-max) :ring nil :ring-index nil :process nil)
           emacs-comint--state)
  nil)

(defun emacs-comint-make (name buffer program &rest program-args)
  "Start PROGRAM (with PROGRAM-ARGS) in BUFFER named NAME under comint.
BUFFER may be a buffer or name; nil means `*NAME*'.  When PROGRAM is nil,
a comint buffer with no process is created.  Returns the buffer."
  (let ((buf (get-buffer-create
              (or buffer (concat "*" name "*")))))
    (with-current-buffer buf
      (emacs-comint-mode)
      (when (and program (fboundp 'make-process))
        (let ((proc (make-process
                     :name name :buffer buf
                     :command (cons program program-args)
                     :filter #'emacs-comint-output-filter)))
          (when (and (fboundp 'process-mark) (markerp (process-mark proc)))
            (set-marker (process-mark proc) (point-max)))
          (emacs-comint--set buf :process proc))))
    buf))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-comint-install ()
  "Bind the standard comint command names to the `emacs-comint-*'
implementations.  Not run on `require' (keeps a bare load from touching
shared command symbols)."
  (defalias 'comint-output-filter #'emacs-comint-output-filter)
  (defalias 'comint-send-input #'emacs-comint-send-input)
  (defalias 'comint-send-string #'emacs-comint-send-string)
  (defalias 'comint-add-to-input-history #'emacs-comint-add-to-input-history)
  (defalias 'comint-previous-input #'emacs-comint-previous-input)
  (defalias 'comint-next-input #'emacs-comint-next-input)
  (defalias 'comint-mode #'emacs-comint-mode)
  (defalias 'make-comint-in-buffer #'emacs-comint-make))

(provide 'emacs-comint)

;;; emacs-comint.el ends here
