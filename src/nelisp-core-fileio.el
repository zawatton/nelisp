;;; nelisp-core-fileio.el --- Minimal core file I/O for NeLisp loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Doc 141 Stage 2: extract the loader's minimal file/path surface into
;; a core-only layer.  This file owns deterministic path surgery,
;; existence/readability probes, raw byte reads, and UTF-8 source-file
;; decoding.  Editor/buffer compatibility APIs remain outside core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-coding)

(declare-function nl-syscall-access "nelisp-runtime")
(declare-function nl-syscall-stat-ex "nelisp-runtime")
(declare-function nl-syscall-read-file "nelisp-runtime")

(defun nelisp-core--syscall-available-p (sym)
  "Return non-nil if the runtime syscall SYM is wired."
  (fboundp sym))

;;;###autoload
(defun nelisp-core-file-name-absolute-p (name)
  "Return non-nil if NAME starts with `/' (POSIX absolute path).
Tilde-expansion (~user/) is treated as absolute as in Emacs.  This
helper does NOT touch the filesystem."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (and (> (length name) 0)
       (or (eq (aref name 0) ?/)
           (eq (aref name 0) ?~))))

;;;###autoload
(defun nelisp-core-file-name-directory (name)
  "Return the directory part of NAME, or nil if NAME has no slash.
The trailing slash is preserved (= directory part is itself a
directory name)."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let ((idx (cl-position ?/ name :from-end t)))
    (and idx (substring name 0 (1+ idx)))))

;;;###autoload
(defun nelisp-core-file-name-as-directory (name)
  "Return NAME with a trailing `/' appended (idempotent)."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (if (and (> (length name) 0)
           (eq (aref name (1- (length name))) ?/))
      name
    (concat name "/")))

(defun nelisp-core--collapse-segments (segments)
  "Collapse `.' / `..' / empty SEGMENTS in a POSIX-style path list.
Returns the simplified list (does NOT touch leading `/')."
  (let ((acc nil))
    (dolist (seg segments)
      (cond
       ((or (string-empty-p seg) (string-equal seg ".")) nil)
       ((string-equal seg "..")
        (when acc (pop acc)))
       (t (push seg acc))))
    (nreverse acc)))

;;;###autoload
(defun nelisp-core-expand-file-name (name &optional default-dir)
  "Convert NAME to an absolute POSIX path.
If NAME is already absolute, only `.' / `..' / `//' collapsing is
performed.  Otherwise NAME is appended to DEFAULT-DIR (which is
itself made absolute against the host CWD when relative).  When
DEFAULT-DIR is omitted the value of the host `default-directory' is
used.

This helper is pure NeLisp string surgery.  The only host input is
`default-directory' when DEFAULT-DIR is omitted."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((dd (or default-dir
                 (and (boundp 'default-directory) default-directory)
                 "/"))
         (dd (if (eq (aref dd 0) ?/) dd (concat "/" dd)))
         (seed (cond
                ((nelisp-core-file-name-absolute-p name) name)
                (t (concat (nelisp-core-file-name-as-directory dd) name))))
         (seed (cond
                ((and (> (length seed) 0) (eq (aref seed 0) ?~))
                 (let ((home (or (getenv "HOME") "/")))
                   (cond
                    ((or (= (length seed) 1) (eq (aref seed 1) ?/))
                     (concat home (substring seed 1)))
                    (t seed))))
                (t seed)))
         (segments (split-string seed "/" t))
         (collapsed (nelisp-core--collapse-segments segments))
         (joined (mapconcat #'identity collapsed "/")))
    (concat "/" joined)))

;;;###autoload
(defun nelisp-core-file-exists-p (file)
  "Return non-nil if FILE exists.  Wraps stat(2)."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-core--syscall-available-p 'nl-syscall-stat-ex)
    (let ((rc (nl-syscall-stat-ex file)))
      (and rc (>= (or (plist-get rc :rc) 0) 0))))
   (t (file-exists-p file))))

;;;###autoload
(defun nelisp-core-file-readable-p (file)
  "Return non-nil if FILE exists and is readable.  Wraps access(F_OK | R_OK)."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (cond
   ((nelisp-core--syscall-available-p 'nl-syscall-access)
    (zerop (nl-syscall-access file 4)))
   (t (file-readable-p file))))

(defun nelisp-core--read-raw-bytes (file &optional beg end)
  "Read FILE between byte offsets BEG (inclusive) and END (exclusive).
Returns a unibyte string of raw bytes."
  (cond
   ((nelisp-core--syscall-available-p 'nl-syscall-read-file)
    (nl-syscall-read-file file (or beg 0) end))
   (t
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil beg end)
      (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun nelisp-core-read-file-as-string (file)
  "Read FILE as UTF-8 source text and return the decoded string.
Signals `file-error' with data `(Cannot read NeLisp file FILE)' when
FILE is unreadable."
  (unless (stringp file)
    (signal 'wrong-type-argument (list 'stringp file)))
  (unless (nelisp-core-file-readable-p file)
    (signal 'file-error (list "Cannot read NeLisp file" file)))
  (let* ((raw (nelisp-core--read-raw-bytes file))
         (plist (nelisp-coding-utf8-decode raw)))
    (plist-get plist :string)))

(provide 'nelisp-core-fileio)
;;; nelisp-core-fileio.el ends here
