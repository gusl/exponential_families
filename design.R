## Goal: design 2D exponential families

############
## 1D
############

pdf <- function(x) exp(sum(eta*T(x)))  ## unnormalized

## 'eta' is the natural parameter of the exponential family.
## to parametrize the family by the mean of the RV, you'd have to first compute the symbolic integral


###################
## Gaussian(mu, sigma^2)
## in this example
## eta1 = mu/sigma^2
## eta2 = -1/ (2 sigma^2)
## e.g. mu=1, sigma^2 = 1, we get eta=(1, -0.5)
##      mu=0, sigma^2 = 1, we get eta=(0, -0.5)
eta.fun <- function(mu, sigma) c(mu/sigma^2, -1/(2*sigma^2))
T <- function(x) c(x, x^2)
ranges <- c("Real", "Positive")
eta1 <- rnorm(1)
eta2 <- rgamma(1, shape=1)
eta <- c(eta1, eta2)

eta <- eta.fun(2,1)
XX <- makeGrid(-5,5,100)
plot(XX, sapply(XX,pdf), type="l")

for(i in 1:10){
    eta1 <- rnorm(1)
    eta2 <- -rgamma(1, shape=1)
    eta <- c(eta1, eta2)
    pplot(XX, sapply(XX,pdf), type="l")
}


## 1D: Gamma(k, theta)
## eta1 = k-1
## eta2 = -1/theta
eta.fun <- function(k, theta) c(k-1, -1/theta)
T <- function(x) c(log(x), x)
ranges <- c("Real", "Positive")
eta1 <- rnorm(1)
eta2 <- rgamma(1, shape=1)
eta <- c(eta1, eta2)

## Gamma(k=1, eta=1), aka exponential
eta <- eta.fun(1, 3)
XX <- makeGrid(0,10,100)
plot(XX, sapply(XX,pdf), type="l")

plot(0,0, type="n", xlim=c(0,10), ylim=c(0,10))
for(i in 1:10){
    eta1 <- rnorm(1)
    eta2 <- -rgamma(1, shape=1)
    eta <- c(eta1, eta2)
    pplot(XX, sapply(XX,pdf), type="l")    
}


## 1D: Beta(alpha, beta)
## eta1 = 
## eta2 = 
eta.fun <- function(alpha, beta) c(alpha-1, beta-1)
T <- function(x) c(log(x), log(1-x))
ranges <- c("Positive", "Positive")
eta <- eta.fun(1,1)
eta <- eta.fun(0.5,0.5)
eta <- eta.fun(2,5)
XX <- makeGrid(0,1,100)
plot(XX, sapply(XX,pdf), type="l")

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,2))
for (i in 1:10){
    alpha <- rgamma(1, shape=1)
    beta <- rgamma(1, shape=1)
    eta <- eta.fun(alpha, beta)
    pplot(XX, sapply(XX,pdf), type="l")
}


## 2D: Gaussian(muX, sigmaX, muY, sigmaY, rho)
eta.fun <- function(muX, sigmaX, muY, sigmaY, rho)
T <- function(x,y) c(x, x^2, y, y^2, x*y)

## 2D density: 3D plot
