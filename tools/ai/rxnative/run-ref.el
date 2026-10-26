;;; -*- lexical-binding: t; -*-
(load (expand-file-name "test-ref.el" (file-name-directory load-file-name)) nil t)
(defvar rt-pats
  '("a" "abc" "a\\|b" "foo\\|bar\\|baz" "o+" "o*" "o?" "\\(o\\)\\1" "\\(f\\)\\(o\\)*" "\\(a*\\)*b" "\\(a*\\)+" "\\(?:ab\\)+" "[0-9]+" "[^0-9]+" "[a-c]*d" "[[:alpha:]]+" "[[:digit:]]+" "[[:upper:]]+" "[[:lower:]]+" "[[:alnum:]_]+" "[[:punct:]]" "[[:blank:]]+" "^abc" "abc$" "\\`foo" "o\\'" "x*" "." ".*" ".*?b" ".+?" "a.c" "a\\{2\\}" "a\\{2,3\\}" "a\\{2,\\}" "\\(ab\\)\\{2\\}" "\\(ab\\)\\{1,3\\}" "\\(a\\|ab\\)\\(c\\|bcd\\)" "\\(a\\|ab\\)*c" "\\(a*\\)\\(b*\\)" "\\(\\(a\\)\\|b\\)*" "a*?" "a+?" "a??" "\\(a\\)?b" "\\(a\\)*\\1" "x\\(y\\)?z" "[]a]" "[^]a]" "[a-]" "\\(foo\\|\\(bar\\)\\)+" "^$" "^" "$" "\\(^a\\|b$\\)" "\\(?:a\\|b\\)*c" "[ \t]*$" "^[ \t]*" "foo.*bar" "\\([a-z]+\\) \\([a-z]+\\)" "\\([a-z]\\)\\1" "\\(.\\)\\1+" "[A-Z][a-z]+" "k+" "[ks]" "K" "S+" "\\(a\\|b\\)\\{3\\}"))
(defvar rt-subs
  '("" "a" "abc" "foo bar baz" "FOO Bar" "aaa" "ab" "aab" "abab" "ababab" "abcd" "abbcd" "abc\nabc" "xx\nabc\n" "  tab\there  " "hello World 123 foo_bar" "kkKK ss SS" "aoo" "foooo" "a.c" "a\nc" "123abc456" "Zebra Apple" "xyz" "xz" "ababc" "aaab" "bbb" "ac" "abcabc" "dd" "cabcd" "あいう foo えお" "foo あ bar" "é" "tèst"))
(dolist (re rt-pats)
  (dolist (s rt-subs)
    (dolist (fold '(nil t))
      (dolist (st '(0 1))
        (when (<= st (length s))
          (rt-one re s st fold))))))
(princ (format "RESULT ok=%d bad=%d decl=%d\n" rt-ok rt-bad rt-decl))
