R1 a a a                     (rl-a rl-a rl-a)
R2 a b a                     (rl-a rl-b rl-a)
R3 a a b b a a               (rl-a rl-a rl-b rl-b rl-a rl-a)
R4 loop 5 then closure       (5 (rl-a rl-a rl-a rl-a rl-a))
R5 loop 40 length            40
R6 loop then other name      (rl-b rl-a rl-a rl-a)
R7 snapshot unchanged        (rl-a rl-a)
R8 let after run is dynamic  1
R9 let other name lexical    nil
R10 closure sees run special 5
R11 two runs then closure    (rl-b rl-b rl-a rl-a rl-a)
R12 nested closure view      (rl-a rl-a rl-a)
