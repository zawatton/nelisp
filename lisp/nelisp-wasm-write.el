;;; nelisp-wasm-write.el --- wasm32 module writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 164 §2.2 / §6 P0 — binary writer for minimal self-contained wasm
;; modules produced by the AOT compiler.

;;; Code:

(require 'cl-lib)
(require 'nelisp-asm-wasm)

(defconst nelisp-wasm-write--magic
  (unibyte-string #x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))

(defun nelisp-wasm-write--payload-types (types)
  "Build the Type section payload for TYPES."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length types))
    (dolist (type types)
      (nelisp-asm-wasm-emit-func-type
       buf
       (plist-get type :params)
       (plist-get type :results)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-functions (functions)
  "Build the Function section payload for FUNCTIONS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length functions))
    (dolist (fn functions)
      (nelisp-asm-wasm-emit-uleb128 buf (plist-get fn :type-index)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-imports (imports)
  "Build the Import section payload for IMPORTS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length imports))
    (dolist (import imports)
      (nelisp-asm-wasm-emit-name buf (plist-get import :module))
      (nelisp-asm-wasm-emit-name buf (plist-get import :field))
      (nelisp-asm-wasm-emit-byte buf nelisp-asm-wasm--extern-func-kind)
      (nelisp-asm-wasm-emit-uleb128 buf (plist-get import :type-index)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-memory (unit)
  "Build the wasm Memory section payload for UNIT."
  (let ((min (or (plist-get unit :wasm-mem-min) 1))
        (max (plist-get unit :wasm-mem-max))
        (buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf 1)
    (if max
        (progn
          (nelisp-asm-wasm-emit-byte buf 1)
          (nelisp-asm-wasm-emit-uleb128 buf min)
          (nelisp-asm-wasm-emit-uleb128 buf max))
      (nelisp-asm-wasm-emit-byte buf 0)
      (nelisp-asm-wasm-emit-uleb128 buf min))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-table (unit)
  "Build the Table section payload for UNIT."
  (let ((table-size (or (plist-get unit :wasm-table-size) 0)))
    (when (> table-size 0)
      (let ((buf (nelisp-asm-wasm-make-buffer)))
        (nelisp-asm-wasm-emit-uleb128 buf 1)
        (nelisp-asm-wasm-emit-byte buf nelisp-asm-wasm--funcref)
        (nelisp-asm-wasm-emit-byte buf 0)
        (nelisp-asm-wasm-emit-uleb128 buf table-size)
        (nelisp-asm-wasm-buffer-bytes buf)))))

(defun nelisp-wasm-write--emit-const-i32-expr (buf value)
  "Append an `i32.const VALUE; end' init expr to BUF."
  (nelisp-asm-wasm-emit-byte buf #x41)
  (nelisp-asm-wasm-emit-sleb128 buf value)
  (nelisp-asm-wasm-op-end buf))

(defun nelisp-wasm-write--payload-element (unit)
  "Build the Element section payload for UNIT."
  (let ((elems (plist-get unit :wasm-element-indices)))
    (when elems
      (let ((buf (nelisp-asm-wasm-make-buffer)))
        (nelisp-asm-wasm-emit-uleb128 buf 1)
        (nelisp-asm-wasm-emit-byte buf 0)
        (nelisp-wasm-write--emit-const-i32-expr buf 0)
        (nelisp-asm-wasm-emit-uleb128 buf (length elems))
        (dolist (index elems)
          (nelisp-asm-wasm-emit-uleb128 buf index))
        (nelisp-asm-wasm-buffer-bytes buf)))))

(defun nelisp-wasm-write--payload-tags (tag-type-index)
  "Build the Tag section payload for TAG-TYPE-INDEX."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf 1)
    (nelisp-asm-wasm-emit-byte buf 0)
    (nelisp-asm-wasm-emit-uleb128 buf tag-type-index)
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-globals (globals)
  "Build the Global section payload for GLOBALS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length globals))
    (dolist (global globals)
      (nelisp-asm-wasm-emit-byte buf (plist-get global :type))
      (nelisp-asm-wasm-emit-byte buf (if (plist-get global :mut) 1 0))
      (nelisp-wasm-write--emit-const-i32-expr
       buf (plist-get global :init-i32)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-exports (exports)
  "Build the Export section payload for EXPORTS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length exports))
    (dolist (entry exports)
      (nelisp-asm-wasm-emit-name buf (plist-get entry :name))
      (nelisp-asm-wasm-emit-byte buf (plist-get entry :kind))
      (nelisp-asm-wasm-emit-uleb128 buf (plist-get entry :index)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-code (functions)
  "Build the Code section payload for FUNCTIONS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length functions))
      (dolist (fn functions)
      (nelisp-asm-wasm-emit-bytes buf (plist-get fn :body)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write--payload-data (segments)
  "Build the Data section payload for active SEGMENTS."
  (let ((buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-uleb128 buf (length segments))
    (dolist (segment segments)
      (nelisp-asm-wasm-emit-byte buf 0)
      (nelisp-wasm-write--emit-const-i32-expr buf (plist-get segment :addr))
      (let ((bytes (plist-get segment :bytes)))
        (nelisp-asm-wasm-emit-uleb128 buf (length bytes))
        (nelisp-asm-wasm-emit-bytes buf bytes)))
    (nelisp-asm-wasm-buffer-bytes buf)))

(defun nelisp-wasm-write-binary (file-path unit)
  "Write wasm UNIT to FILE-PATH and return FILE-PATH."
  (let* ((types (plist-get unit :wasm-types))
         (imports (plist-get unit :wasm-imports))
         (functions (plist-get unit :wasm-functions))
         (table (nelisp-wasm-write--payload-table unit))
         (element (nelisp-wasm-write--payload-element unit))
         (tag-type-index (plist-get unit :wasm-tag-type-index))
         (globals (plist-get unit :wasm-globals))
         (exports (or (plist-get unit :wasm-exports)
                      (cl-loop
                       for fn in functions
                       for index from (length imports)
                       collect (list :name (plist-get fn :name)
                                     :kind nelisp-asm-wasm--extern-func-kind
                                     :index index))))
         (data (plist-get unit :wasm-data))
         (buf (nelisp-asm-wasm-make-buffer)))
    (nelisp-asm-wasm-emit-bytes buf nelisp-wasm-write--magic)
    (nelisp-asm-wasm-emit-section
     buf 1 (nelisp-wasm-write--payload-types types))
    (when imports
      (nelisp-asm-wasm-emit-section
       buf 2 (nelisp-wasm-write--payload-imports imports)))
    (nelisp-asm-wasm-emit-section
     buf 3 (nelisp-wasm-write--payload-functions functions))
    (when table
      (nelisp-asm-wasm-emit-section buf 4 table))
    (nelisp-asm-wasm-emit-section
     buf 5 (nelisp-wasm-write--payload-memory unit))
    (when tag-type-index
      (nelisp-asm-wasm-emit-section
       buf 13 (nelisp-wasm-write--payload-tags tag-type-index)))
    (when globals
      (nelisp-asm-wasm-emit-section
       buf 6 (nelisp-wasm-write--payload-globals globals)))
    (nelisp-asm-wasm-emit-section
     buf 7 (nelisp-wasm-write--payload-exports exports))
    (when element
      (nelisp-asm-wasm-emit-section buf 9 element))
    (nelisp-asm-wasm-emit-section
     buf 10 (nelisp-wasm-write--payload-code functions))
    (when data
      (nelisp-asm-wasm-emit-section
       buf 11 (nelisp-wasm-write--payload-data data)))
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file file-path
        (set-buffer-multibyte nil)
        (insert (nelisp-asm-wasm-buffer-bytes buf))))
    file-path))

(provide 'nelisp-wasm-write)

;;; nelisp-wasm-write.el ends here
