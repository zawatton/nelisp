E5 condcase b-warm         (unbound unbound)
E5a condcase a-warm        (unbound unbound)
E5g condcase global sp     0
E5c cold condcase decl     unbound
g1 global special          (0 (error 1))
g2 eval global special     0
g3 eval local decl         unbound
g4 eval decl+global        0
g5 body sees var           (error 1)
g8 success var             5
