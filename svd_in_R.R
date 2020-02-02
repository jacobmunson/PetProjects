### SVD in R ###
# 1/12/2020 ####
################

# Let's make a matrix of data 
set.seed(1)
M = matrix(data = rnorm(n = 30, mean = 5, sd = 3), nrow = 5, ncol = 6)
M

# SVD function
svd_m = svd(M) # produces d, u, v

# Rebuilding M
# Should be 5x6 

# svd_m$u - 5*5
# diag(svd_m$d) - 5*5
# product of those yields 5*5
# t(svd_m$v) - 5*6
# product of those yields 5*6
svd_m$u %*% diag(svd_m$d) %*% t(svd_m$v)

# compare to 
M
