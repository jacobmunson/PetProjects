#######################
### Power Iteration ###
#######################

# not formalized - just storing here 

A <- matrix(data = c(0,0,1,0,1,
                     0,0,1,1,0,
                     1,1,0,0,1,
                     0,1,0,0,1,
                     1,0,1,1,0),ncol = 5, byrow = TRUE)
A

power_iter <- function(A, k_max){
  A <- as.matrix(A) #just in case
  k <- 0; i <- 0
  p_0 <- matrix(data = rep(1,nrow(A)), nrow = nrow(A))
  p_k <- p_0
  p_vec <- p_0
  lambdas <- c()
  for(j in 1:k_max){
    k <- k + 1
    p_k1 <- p_k
    p_k <- t(A) %*% p_k
    p_vec <- cbind(p_vec, p_k)
    p_vec <- as.matrix(p_vec)
    i <- which(p_k == max(p_k))[1] 
    lambda <- max(p_k[i]/max(p_k1[i]))
    lambdas[k] <- lambda 
    p_k <- p_k / lambda # p_vec[k,i-1]
    
  }
  print(lambdas)
  P <- p_k/(norm(x = p_k, type = "F"))
  return (P)
}


power_iter(A = A, k_max = 15)
eigen(A)
