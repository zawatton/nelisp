sub plain                    "bc"
sub no end                   "cdef"
sub no start                 "abcdef"
sub nil nil                  "abcdef"
sub neg start                "def"
sub neg end                  "bcde"
sub neg both                 "cd"
sub empty                    ""
sub whole                    "abc"
sub start>end                (ERR (args-out-of-range "abcdef" 4 2))
sub end>len                  (ERR (args-out-of-range "abc" 0 9))
sub start>len                (ERR (args-out-of-range "abc" 9 nil))
sub neg oob                  (ERR (args-out-of-range "abc" -9 nil))
sub neg end oob              (ERR (args-out-of-range "abc" 0 -9))
sub nonint start             (ERR (wrong-type-argument integerp a))
sub nonint end               (ERR (wrong-type-argument integerp "x"))
sub float                    (ERR (wrong-type-argument integerp 1.0))
sub nil seq                  (ERR (wrong-type-argument arrayp nil))
sub symbol                   (ERR (wrong-type-argument arrayp abc))
sub int                      (ERR (wrong-type-argument arrayp 5))
sub too many args            (ERR (wrong-number-of-arguments substring 4))
sub indirect too many args   (ERR (wrong-number-of-arguments #<subr substring> 4))
sub no args                  (ERR wrong-number-of-arguments)
sub vector                   [2 3]
sub vector neg               [3 4]
sub vector oob               (ERR (args-out-of-range [1 2 3] 5 nil))
sub vector no end            [1 2 3]
sub list                     (ERR (wrong-type-argument arrayp (1 2 3)))
sub multibyte                "いう"
sub multibyte neg            "えお"
sub multibyte len            2
sub multibyte p              t
sub unibyte p                nil
sub fresh                    nil
sub fresh whole              (nil t)
sub prop mid                 #("bc" 0 2 (face bold))
sub prop neg                 #("ef" 0 2 (face bold))
sub prop partial             #("bcde" 1 3 (k 1))
sub prop two                 #("bc" 0 1 (a 1) 1 2 (b 2))
sub prop none in range       "ab"
sub prop multibyte           #("いうえ" 0 3 (face x))
sub prop no end              #("cdef" 0 4 (face bold))
sub prop pos                 (nil 1 nil 1)
sub prop vector              [1 2]
cs plain                     "abc"
cs fresh                     (nil t)
cs empty                     ""
cs multibyte                 "あいう"
cs multibyte p               t
cs list                      ((1 2 3) nil)
cs list shares elts          t
cs nil                       nil
cs vector                    ([1 2 3] nil)
cs vector indep              [1 2]
cs bool-vector               #&3""
cs dotted                    (ERR (wrong-type-argument listp 3))
cs symbol                    (ERR (wrong-type-argument sequencep a))
cs int                       (ERR (wrong-type-argument sequencep 5))
cs no args                   (ERR wrong-number-of-arguments)
cs too many                  (ERR (wrong-number-of-arguments copy-sequence 2))
cs prop                      #("abc" 0 3 (face bold))
cs prop partial              #("abcd" 1 3 (k 1))
cs prop independent          (1 2)
cs prop multibyte            #("あい" 0 2 (face x))
funcall substring            "b"
apply substring              "b"
mapcar substring             ("a" "b" "c")
scw plain                    "a b"
scw tabs nl                  "a b"
scw empty                    ""
scw only ws                  ""
scw single                   "a"
scw prop                     #("a b" 0 1 (k 1) 2 3 (k 1))
scw multibyte                "あ い"
scw trim                     ("a b" "a" "a")
