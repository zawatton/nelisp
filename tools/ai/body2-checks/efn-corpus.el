;;; -*- lexical-binding: t; -*-
;; expand-file-name parity corpus (GNU 31.1 is the oracle): ~, .., ///, trailing /, default-directory nil, relative dirs.
(load (expand-file-name "enc.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setenv "HOME" "/home/tester")
(setq default-directory "/srv/data/")
(defvar b2-names '("" "a" "a/b" "a/" "a//b" "a/./b" "a/../b" "../a" "./a" "." ".." "./" "../" "/" "///" "/a" "/a/" "/a//b/"
  "/a/./b/../c" "/a/b/../../c" "/a/b/../../../c" "/.." "/../a" "/../../a" "/a/.." "/a/../.." "~" "~/" "~/a" "~/a/../b" "~/x/../y"
  "~//a" "~/.." "~/../.." "a/b/c/../../.." "a/b/c/../../../.." "a//" "a/." "a/.." "a/b/." "/a/." "/a/b/.." "..." ".../a" "a...b" ".a" "a."
  "a b" "a/b c/d" "é" "é/日/.." "/日/a" "~/é" "foo/../../bar" "/foo/bar//baz/../qux/." "x/y/z/./../../w/"))
(defvar b2-bases '(nil "/" "/tmp" "/tmp/" "/tmp//x" "/a/b/../c" "/a/./b" "rel" "rel/dir/" "~" "~/sub" "." ".." "/日"))
(dolist (n b2-names)
  (b2-l (list "nobase" n) (expand-file-name n))
  (dolist (b b2-bases) (b2-l (list "base" n b) (expand-file-name n b)))
  (dolist (dd '("/var/log/" "/var/log" "/" "/a/./b/" "/a/b/.."))
    (b2-l (list "dd" n dd) (let ((default-directory dd)) (expand-file-name n)))))
(b2-c (let ((default-directory nil)) (expand-file-name "/abs")))
(b2-c (let ((default-directory nil)) (expand-file-name "~/q")))
(b2-c (expand-file-name nil))
(b2-c (expand-file-name 5))
(kill-emacs 0)
