Kikuchi:

P(y,x1,x2,x3) = P(y,x1,x2) * P(y,x2,x3) / P(y,x2) -- divide out 'over-counts'

Fitting:

y1 | y2
---+----
   |    x1
---+----
   |    x2
---+----
   |    x3
---+----

P : x,y |-> {0..1}

R_y = {y1,y2}
R_x = {x1,x2,x3}

0.) P(y1) = 0.5
1.) P(y1) = alpha; P(y2) = 1 - alpha
2.) P(y1,x1) = alpha
    P(y1,{x2 or x3}) = beta
    P(y2,x3) = gamma
