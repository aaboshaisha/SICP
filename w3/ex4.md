Give an algebraic formula relating the values of the parameters b, n, counter, and product of the expt and exp-iter procedures given near the top of page 45 of Abelson and Sussman.

## Answer

```
c -> counter
p-> product 

(exp-iter b c p)  n
(exp-iter 3 4 1)  4
(exp-iter 3 3 3)  4
(exp-iter 3 2 9)  4
(exp-iter 3 1 27) 4
(exp-iter 3 0 81) 4
```

We can see that $b{(n - c)} = p$
