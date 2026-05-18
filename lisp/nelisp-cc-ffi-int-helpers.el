;;; nelisp-cc-ffi-int-helpers.el --- Width-dispatched FFI int read/write shims  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Thin elisp shims around the unified `nl-ffi-read-int' / `nl-ffi-write-int'
;; Rust primitives.  Each shim names a (signedness, byte-width) pair and
;; forwards to the unified primitive.  Loaded before `nelisp-stdlib-os.el',
;; which uses these wrappers extensively to lay out and parse `struct
;; sockaddr_in' / `struct stat' / `inotify_event' / etc.
;;
;; The unified primitives take (PTR OFFSET WIDTH-BYTES [SIGNED]) and
;; respect the same `nl-ffi-malloc' bounds-check + NULL guard as the
;; legacy per-width Rust entry points used to.

;;; Code:

(defun nl-ffi-read-u8  (buf off) (nl-ffi-read-int buf off 1 nil))
(defun nl-ffi-read-i16 (buf off) (nl-ffi-read-int buf off 2 t))
(defun nl-ffi-read-u16 (buf off) (nl-ffi-read-int buf off 2 nil))
(defun nl-ffi-read-i32 (buf off) (nl-ffi-read-int buf off 4 t))
(defun nl-ffi-read-u32 (buf off) (nl-ffi-read-int buf off 4 nil))
(defun nl-ffi-read-i64 (buf off) (nl-ffi-read-int buf off 8 t))

(defun nl-ffi-write-i16 (buf off val) (nl-ffi-write-int buf off 2 val))
(defun nl-ffi-write-i32 (buf off val) (nl-ffi-write-int buf off 4 val))
(defun nl-ffi-write-i64 (buf off val) (nl-ffi-write-int buf off 8 val))

;; Bytes read/write at offset are arity-dispatched at the primitive level
;; (`nl-ffi-read-bytes' / `nl-ffi-write-bytes' accept 2 OR 3 args).  These
;; wrappers exist for callers that prefer the explicit `-at' name.
(defun nl-ffi-read-bytes-at  (buf off n) (nl-ffi-read-bytes  buf off n))
(defun nl-ffi-write-bytes-at (buf off s) (nl-ffi-write-bytes buf off s))

(provide 'nelisp-cc-ffi-int-helpers)

;;; nelisp-cc-ffi-int-helpers.el ends here
