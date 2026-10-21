;;; strings-image-a.el --- build the string fixture of the cold-image check  -*- lexical-binding:t -*-
;; Run in a process that started from the build's cold image, then dump:
;;   BIN --load strings-image-a.el -- OUT.img
;; The strings below are created in the pool; the dump moves them into the
;; image's header area, next to the base image's own strings.

(defvar strings-image--ascii "alpha")
(defvar strings-image--multibyte (string-to-multibyte "beta"))
(defvar strings-image--wide (string 26085 26412 35486))
(defvar strings-image--props (propertize "gamma" 'face 'bold))
(defvar strings-image--empty "")
(defvar strings-image--made (make-string 5 ?x))
(defvar strings-image--long (make-string 200000 ?q))
(defvar strings-image--list
  (list strings-image--ascii strings-image--multibyte strings-image--wide
        strings-image--props strings-image--empty strings-image--made
        strings-image--long))
(defvar strings-image--eq-table (make-hash-table :test 'eq))
(puthash strings-image--ascii 'ascii strings-image--eq-table)
(puthash strings-image--wide 'wide strings-image--eq-table)
(defvar strings-image--equal-table (make-hash-table :test 'equal))
(puthash "alpha" 1 strings-image--equal-table)
(puthash "日本語" 2 strings-image--equal-table)
(defvar strings-image--symbol (intern "strings-image--sym-κ"))
(defvar strings-image--shared (list strings-image--ascii strings-image--ascii))
(garbage-collect)
(nelisp--arena-dump-image-stream (getenv "STRINGS_IMAGE_OUT"))
