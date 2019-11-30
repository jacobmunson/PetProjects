### Difference in Normal Distributions
# Almost nothing original here - but I want safe storage 
# Reference materials
# https://stats.stackexchange.com/questions/186463/distribution-of-difference-between-two-normal-distributions
# http://mathworld.wolfram.com/NormalDifferenceDistribution.html

# Specify parameters
mu <- c(0, 0)
sigma <- c(1, 1)
# Simulate data
n.sim <- 1e5; set.seed(17)
x.sim <- matrix(rnorm(n.sim*2, mu, sigma), nrow=2)
x <- abs(x.sim[2, ] - x.sim[1, ])
# Display the results
hist(x, freq=FALSE)
f <- function(x, mu, sigma) {sqrt(2 / pi) / sigma * cosh(x * mu / sigma^2) * exp(-(x^2 + mu^2)/(2*sigma^2))}
curve(f(x, abs(diff(mu)), sqrt(sum(sigma^2))), lwd=2, col="orange", add=TRUE, lty = 3)
lines(density(abs(rnorm(n = 1e5, mean = 0, sd = sqrt(2))), from = 0), lty = 2, col = 'green')

abline(v = dnorm(x = 0.05, mean = 0, sd = sqrt(2)), col = "orange")
abline(v = qnorm(p = 0.05, mean = 0, sd = sqrt(2), lower.tail = FALSE), col = "pink")

val = 0.05

abline(h = dnorm(x = 0.05, mean = 0, sd = sqrt(2)), col = "purple")
abline(v = val, col = "purple")
