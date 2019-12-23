### Function for lp-norm of two vectors
# A reference for inquiring minds:
# https://www.saedsayad.com/k_nearest_neighbors.htm 
# Built to require p >= 1 (to be a valid distance metric)
# 12/22/2019

# Minkowski Distance function
lp_norm = function(x, y, p){
  stopifnot(p >= 1)
  return((sum((abs(x - y))^p))^(1/p))
}

lp_norm(x = c(1,1), y = c(0,0), p = 1) # Manhattan distance = 2
lp_norm(x = c(1,1), y = c(0,0), p = 2) # Euclidean distance = 1.414
lp_norm(x = c(1,1), y = c(0,0), p = 0.5) # p < 1 is not a valid distance metric
