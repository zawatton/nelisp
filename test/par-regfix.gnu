nconc none                 nil
nconc nil                  nil
nconc nil nil              nil
nconc atom only            5
nconc nil atom             5
nconc list atom            (1 . 2)
nconc dotted last          (1 2 . 3)
nconc dotted first         (1 3)
nconc nonlist middle       (ERR (wrong-type-argument consp 2))
nconc nil middle           (1 2)
nconc 3 lists              (1 2 3 4)
nconc shares last          ((1 9) t)
nconc mutates first        (t (1 2))
nconc same list twice      (circ 1 t)
nconc string arg           (ERR (wrong-type-argument consp "ab"))
nconc vector middle        (ERR (wrong-type-argument consp [2]))
funcall nconc              (1 2)
apply nconc                (1 2)
mapcan basic               (1 2 3)
mapcan nil seq             nil
mapcan nil results         nil
mapcan identity lists      (1 2 3)
mapcan atom result one     1
mapcan atom result two     (ERR (wrong-type-argument consp 1))
mapcan atom last           (1 . 7)
mapcan dotted result       (1 2 . 9)
mapcan string seq          (97 98)
mapcan vector seq          (1 2)
mapcan dotted seq          (ERR (wrong-type-argument listp 2))
mapcan non-seq             (ERR (wrong-type-argument sequencep 5))
mapcan shared result       ((1) t)
mapcan bool-vector seq     (t t)
scw empty                  ""
scw blank                  ""
scw plain                  "a"
scw spaces                 "a b"
scw tabs newlines          "a b c"
scw nbsp KNOWN-DIV         "a b"
scw unicode KNOWN-DIV      "あ い う"
scw formfeed vtab          "abc"
scw nil                    (ERR (wrong-type-argument arrayp nil))
scw symbol                 (ERR (wrong-type-argument sequencep a))
scw number                 (ERR (wrong-type-argument sequencep 5))
nconc circular first       circular-list
nconc circular last        returned
mapcan circular seq        circular-list
