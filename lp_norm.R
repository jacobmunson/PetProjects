### Function for lp-norm of two vectors
# A reference for inquiring minds:
# https://www.saedsayad.com/k_nearest_neighbors.htm 
# Built to require p >= 1 (to be a valid distance metric)
# 12/22/2019

# 12/24/2019
# Updated to allow p < 1
# Special case for p = 0
# Also, examples of each as well as very large p for l_infinity norm

# Minkowski Distance function
lp_norm = function(x, y, p){
  if(p < 1){message("p less than 1 is not a valid distance metric.")}
  if(p != 0){return((sum((abs(x - y))^p))^(1/p))}else{
    return(length(which(x-y == 0)))
  }
}

lp_norm(x = c(1,0), y = c(0,0), p = 0) # l_0 norm = 1
lp_norm(x = c(1,1), y = c(0,0), p = 1) # Manhattan distance = 2
lp_norm(x = c(1,1), y = c(0,0), p = 2) # Euclidean distance = 1.414
lp_norm(x = c(5,2), y = c(9,6), p = 500) # as p -> Inf, l_Inf norm = 4.005549 = max{x - y}
lp_norm(x = c(1,1), y = c(0,0), p = 0.5) # 4 but, p < 1 is not a valid distance metric
