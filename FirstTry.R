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
N = 150
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
WLSmod <- lm(d ~ x + t + factor(c), data = DT) #weights=1/SD^2)

