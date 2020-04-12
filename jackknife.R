##################################
### Jackknife for ratings data ###
# Mean, weighted mean ############
##################################

# Made up data
r = c(4,3,3,2,5) # ratings values 
sim = c(1,0.75,0.5,0.32,0.06) # similarity (weights)
df_x = data.frame(r = r, sim = sim)

# Using the library (mostly for verification of by-hand work)
library(bootstrap) # library version
theta = function(x){mean(x)} # estimate to compute
jk = jackknife(x = r, theta = theta) # function call
jk # includes se, bias, jk values

# I want weighted mean - doesn't work with library version
theta = function(df_x){sum(df_x$r * df_x$sim)/sum(abs(df_x$sim))}
theta_dot = theta(df_x)

mean(r) # mean of r = 3.4
theta_dot # weighted mean of r = 3.304

x = r
df_loc = t(combn(1:length(x), length(x)-1)) # n-1 elements for jackknife 

theta_i = rep(NA,nrow(df_loc)) # empty vector for jk estimates
for(i in 1:nrow(df_loc)){ # iterate through df_loc and grab the n-1 relevant values and computed estimate
  print(df_x[df_loc[i,],])
  print(theta(df_x[df_loc[i,],]))
  theta_i[i] = theta(df_x[df_loc[i,],])
} # probably re-write in C++ for large scale use

theta_dot # weighted mean estimate
theta_i # jk estimates of weighted mean
mean(theta_i) # jk estimate of weighted mean 
n = nrow(df_x)

### Bias ### 
bias = (n - 1)*(mean(jk$jack.values) - mean(r)) # 0
mean(r) - bias # doesn't change since mean is unbiased estimator

bias = (n - 1)*(mean(theta_i) - theta_dot) # 0
(theta_dot - bias) # changes for weighted-mean
n*theta_dot - (n-1)*mean(theta_i) # changes for weighted-mean
# this is new theta_jack = mean(theta_i) : replaces theta_dot in CI? I think


se_jack_weighted = sqrt(((n-1)/n)*(sum((theta_dot - theta_i       )^2)))
se_jack_weighted # 0.4347
se_jack_unweighted = sqrt(((n-1)/n)*(sum((mean(r) - jk$jack.values)^2)))
se_jack_unweighted # 0.5099


### Confidence intervals ###
multi = qt(p = 0.975, df = n-1)

c(mean(r) - multi*se_jack_unweighted, mean(r), mean(r) + multi*se_jack_unweighted)
c(theta_dot - multi*se_jack_weighted, theta_dot, theta_dot + multi*se_jack_weighted)

mean_val = (theta_dot - bias) # bias corrected value
c(mean_val - multi*se_jack_weighted, mean_val, mean_val + multi*se_jack_weighted)
# what does this actually mean? CI for prediction? For... user? For what? 

