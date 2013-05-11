Observe that our model of evaluation allows for combinations
whose operators are compound expressions. Use this
observation to describe the behavior of the following procedure:
(define (a-plus-abs-b a b)
((if (> b 0) + -) a b))


The if statement chooses between the "+" and "-" operators (or really the
procedures they reference, not the symbols themselves?)

If b is negative it is subtracted, if poitive it is added, which is 
equivalent to adding the abslute value of b to a.