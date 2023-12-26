
Exact solution for N() prior
# https://www.cs.toronto.edu/~rgrosse/courses/csc411_f18/slides/lec19-slides.pdf

# t|x∼N(w⊤ψ(x), σ2)

w ∼ N (0, S)
t|x,w ∼ N(w⊤ψ(x), σ2)

=============

y = w^t psi(x)
L(y,t) = 0.5(t-y)^2


=======> General solution
w|D ~ N(mu, SIGMA)
mu = 1/sigma^2  + sum_i PSI^t   t
SIGMA^-1 = 1/sigma^2  +  PSI^t PSI + SIGMA^-1

=======> Solution for one dimensional y



# Python people, check out:
# https://www.kaggle.com/code/sathi557/bayesian-linear-regression-demo






w = (Ψ⊤Ψ + λI)−1Ψ⊤t
if R ( w ) = λ2 ∥ w ∥ 2

==================

w ∼ N (m, S)
log rho(w) = − 12 (w − m)⊤ S−1 (w − m)



======================== wikipedia ======================================


https://en.wikipedia.org/wiki/Bayesian_linear_regression

============= other post


https://gregorygundersen.com/blog/2020/02/04/bayesian-linear-regression/

it depends on if we know mean and var; here, not


==================




book:
https://www.microsoft.com/en-us/research/uploads/prod/2006/01/Bishop-Pattern-Recognition-and-Machine-Learning-2006.pdf

#p152 / 172

noise parameter beta is a known constant

3.32 p(t|x,W, β) = N (t|WTφ(x), β−1I).
3.48  p(w) = N (w|m0, S0)

posterior then:
3.49 p(w|t) = N (w|mN , SN )
3.50 mN = SN ( S0^−1 m0 + β Φ^T t ) 
3.51 SN−1 = S0^−1 + β Φ^T Φ




================================================

Under/overfitting demo

y=ai sum_n sin(nx)


