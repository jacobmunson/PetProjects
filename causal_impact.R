#################################
### Causal Impact walkthrough ###
# 2/22/2020 #####################
# https://google.github.io/CausalImpact/CausalImpact.html
#################################

# Library
library(CausalImpact)

# Data
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

# Data structure
head(data)
str(data)

# Plotting
plot(x1, ylim = c(min(min(x1,y)),max(max(x1,y))), col = 'red')
lines(y, col = "green")

## their way
matplot(data, type = "l")

# Identifying period
pre.period <- c(1, 70)
post.period <- c(71, 100)

# Build and plot model
impact <- CausalImpact(data, pre.period, post.period)
plot(impact)

# Alternatively...
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), time.points)
head(data)

pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)

# Summaries
summary(impact)
summary(impact, "report")
impact$summary

# Alternatively with model arguments
impact <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

summary(impact)
summary(impact, "report")
impact$summary

plot(impact)