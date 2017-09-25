## This is the maths behind the article Design and analysis of stepped
## wedge cluster randomized trials by Hussey and Hughes (2005)

## Declaring some variables to begin with
## No. of clusters
I = 10
i = 1:I

## No of timepoints
## I %/% (T-1) == 0
T = 6
j = 1:T

## Randomizing each cluster to a time, not sure how to do that
## but this is a guess
## Not sure if this is important or not??? Does it matter since they're
## all created the same way? Perhaps? Oh well, write the coding anyway
## just in case...
O = order(runif(I, 0, 1), i)

## No of individuals sampled in each cluster
N = 150 # I believe I need to make this run: N = round(runif(T, 20, 500))
k = 1:N

## Cluster effect
tau = 5
alpha = rnorm(I, 0, tau)

## Time effect
beta = c(runif(T-1, 1, 5), 0)

## Treatment effect
theta = -15

## Matrix of treatment indicator
## Each row (j) is treatment indicator for clusters
## randomized to treatment at time j.
X = matrix(0, nrow = T-1, ncol = T)
Cols = matrix(j, nrow = T-1, ncol = T, byrow = TRUE)
Rows = matrix(1:(T-1), nrow = T-1, ncol = T)
X[which(Rows < Cols)] = 1

## Mean
mu = 20

## Error term
sigmaSQError = 4

## For one person, with the error term
## i is the cluster number
## j+1 is the first time the cluster gets treatment
make.one.Yijk = function(i,j){
  mu + alpha[i] + beta + X[j,]*theta + rnorm(T, 0, sigmaSQError)
}

## One cluster without consideration of order, as
## I didn't think it was relevant in the end.
d = as.vector(cbind(replicate(N, make.one.Yijk(i = 1, j = 1)),
                    replicate(N, make.one.Yijk(i = 2, j = 1)),
                    replicate(N, make.one.Yijk(i = 3, j = 2)),
                    replicate(N, make.one.Yijk(i = 4, j = 2)),
                    replicate(N, make.one.Yijk(i = 5, j = 3)),
                    replicate(N, make.one.Yijk(i = 6, j = 3)),
                    replicate(N, make.one.Yijk(i = 7, j = 4)),
                    replicate(N, make.one.Yijk(i = 8, j = 4)),
                    replicate(N, make.one.Yijk(i = 9, j = 5)),
                    replicate(N, make.one.Yijk(i = 10, j = 5))))

## Used to make the data, indicates when individuals are
## being treated
x = as.vector(sapply(1:(T-1), function(i){rep(X[i,], 2*N)}))

## time effect to fit the model below
t = rep(j, N*I)

## Cluster effect to fit the model below
c = rep(i, each = N*T)

## Making the data to run the model
DT = data.frame(d,x,t,c)
              
## We'll begin by calculating power the most simple method from the
## text. Using weighted least squares, what the hell should the weights
## be?? Hmmmmm.... Method for tau and sigma know
WLSmod <- lm(d ~ x + factor(t) + factor(c), data = DT) #weights=1/SD^2)

## To get true vals of coefs of factors(c)
alpha - alpha[1]

## To get true vals of coefs of factors(t)
beta - beta[1]

## The following 3 are used to find the variance
## in equation 8 from article - pg 187
U = function(x){
  sum(x)
}

W = function(X, N){
  sum((rowSums(X)*N)^2)
}

V = function(X, N){
  sum(rowSums(X)^2*N)
}

## Variance function eqn 8  - pg 187
Var = function(I, sigmaSQError, T, tau){
  ((I*sigmaSQError)*(sigmaSQError + T*tau^2))/
    ((I*U(x = x) - W(X = X, N = N))*sigmaSQError +
       (U(x = x)^2 + I*T*U(x = x) - T*W(X = X, N = N) - I*V(X = X, N = N))*tau^2)
}

## Now to find the power
Pwr = function(theta.alt, alpha){
  pnorm((theta.alt)/ 
          sqrt(Var(I = I, sigmaSQError = sigmaSQError, T = T, tau = tau)) - 
          qnorm(alpha/2, mean = 0, sd = 1))
}

  


