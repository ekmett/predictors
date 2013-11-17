Vagueness generators: Ways to penalize overfitting.

1. Bootstrap (to draw q from p)
2. Splitting (partition your data set to get p and q
3. Compression
       (Q = {}, P = {d1}) -- Q has to give a prediction given nothing, or a prior
       (Q = {d1}, P = {d2})
       (Q = {d1,d2}, P = {d3})
       (Q = {d1,d2,d3}, P = {d4})
       (Q = {d1,d2,d3,d4}, P = {d5})
       ...

