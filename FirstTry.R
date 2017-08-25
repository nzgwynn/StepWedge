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
make.one.Yijk = function(i,j){
  mu + alpha[i] + beta + X[j,]*theta + rnorm(T, 0, sigmaSQError)
}

## One cluster without consideration of order, as
## I didn't think it was relevant in the end.
replicate(N, make.one.Yijk(i = 1, j = 1))

## We'll begin by calculating power the most simple way stated in the
## text. Using weight least squares, what the hell should the weights
## be?? Hmmmmm....
model.2 <- lm(Progeny ~ Parent, weights=1/SD^2)

