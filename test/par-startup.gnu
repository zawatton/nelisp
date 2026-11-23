a dyn let in closure     (7 1)
a marker count special   0
a marker count nonspecial 1
a callee sees dynamic    (9 2)
b local special let      (5 1)
b special + local mix    ((3 4) (3 4) 1)
c eval alist special     8
c eval alist nonspecial  8
c eval alist pair+marker (1 8)
c uninterned same name   8
f env n special+ns       (st-sp1 st-ns)
f env n order            (st-ns st-sp1 st-ns7 st-sp2)
f env n dups             (st-ns st-sp1 st-ns st-sp1 st-sp1)
f env n mixed            (st-sp1 st-ns8 st-sp2 st-ns9 st-sp1 st-ns8)
f env raw special+ns     (st-sp1 st-ns t)
g env raw lexical pairs  (t)
g env raw mixed pairs    (t)
c special after          (1 2)
d eval-and-compile closure ((6 7) (6 7) 1)
e bundle closure         ((5 6 7) 1 1)
e bundle marker count    80
h call special dyn       ((9 5 1) (9 5 1) 1)
h view unchanged by call t
h view after call n      (st-cs1 st-cn1 st-cs2 st-cs1)
h late special           (3 3)
h big alist call         ((9 5 1) 401)
